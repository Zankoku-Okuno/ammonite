module Language.Ammonite.Syntax.Distfix
    ( Distfix
    , Assoc(..)
    , distfix
    , forceDistfix
    ) where

import Control.Applicative
import Control.Monad

import Language.Distfix hiding (distfix)
import qualified Language.Distfix as DF
import Language.Ammonite.Syntax.Concrete
import Language.Ammonite.Syntax.Parser


data Assoc = LeftAssoc | RightAssoc | NonAssoc
    deriving (Eq, Show)

forceDistfix :: String -> Assoc -> (Distfix Syntax)
forceDistfix funcname assoc = case distfix funcname (Just assoc) of
    Left _ -> error "forceDistfix"
    Right x -> x

distfix :: String -> Maybe Assoc -> Either String (Distfix Syntax)
distfix funcname m_assoc = do
    (shape, parts) <- check funcname m_assoc
    pure $ DF.distfix shape (mkPred <$> parts)
    where
    mkPred name (Name test, _) = test == name
    mkPred _ _ = False
    

check :: String -> Maybe Assoc -> Either String (Shape, [String])
check funcname m_assoc = do
    shape <- findShape m_assoc openl openr
    mapM_ checkPart parts
    pure (shape, parts)
    where
    (openl, parts, openr) = splitup funcname
    checkPart part = case parse name "" part of
        Left _ -> Left $ "invalid identifier in distfix: " ++ part
        Right _ -> Right ()
    findShape (Just LeftAssoc) True True = Right OpenLeft
    findShape (Just LeftAssoc) True False = Right HalfOpenLeft
    findShape (Just LeftAssoc) False _ = Left "left-associative distfix must be open to the left"
    findShape (Just RightAssoc) True True = Right OpenRight
    findShape (Just RightAssoc) False True = Right HalfOpenRight
    findShape (Just RightAssoc) False _ = Left "right-associative distfix must be open to the right"
    findShape (Just NonAssoc) True True = Right OpenNon
    findShape (Just NonAssoc) False False = Right Closed
    findShape (Just NonAssoc) _ _ = Left "non-associative distfix must be either closed or open on both sides"
    findShape Nothing True True = Left "open distfix must have associativity specified"
    findShape Nothing True False = Right HalfOpenLeft
    findShape Nothing False True = Right HalfOpenRight
    findShape Nothing False False = Right Closed

splitup :: String -> (Bool, [String], Bool)
splitup str = (openLeft, go str' [], openRight)
    where
    openLeft = head str == '_'
    openRight = last str == '_'
    str' = (if openLeft then tail else id) . (if openRight then init else id) $ str
    go str acc = case break (=='_') str of
        (rest, []) -> reverse (rest:acc)
        (part, '_':rest) -> go rest (part:acc)
        _ -> error "Something's rotten in Denmark. (Language.Ammonite.Distfix.splitup)"