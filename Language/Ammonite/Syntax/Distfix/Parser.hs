module Language.Ammonite.Syntax.Distfix.Parser
    ( Assoc(..)
    , DistfixDefs(..)
    , GroupName, GroupLine
    ) where

import Control.Monad (when, unless)

import Text.Luthor
import Text.Luthor.Indent hiding (parse, many) --FIXME luthor 0.0.2 doesn't export `many`
import Text.Luthor.Syntax
import Text.Parsec.Error (errorMessages, messageString)

import Language.Distfix
import Language.Ammonite.Syntax.Concrete
import Language.Ammonite.Syntax.Parser (name)


type Parser = ParsecI String ()
type GroupName = String
type GroupLine = Either GroupName [Distfix Syntax]



data Assoc = LeftAssoc | RightAssoc | NonAssoc
    deriving (Eq, Show)

data DistfixDefs = DFDefs {
      dfdefsImport :: [FilePath]
    , dfdefsGroups :: [(GroupName, [GroupLine])]
    , dfdefsUse :: Maybe (Either FilePath GroupName)
}

parseDistfixes :: String -> Either String DistfixDefs
parseDistfixes input = case runPI (allInput distfixDirective) (DontMix " ") [ilws] () "" input of
    Right val -> Right val
    Left err -> Left $ messageString . head $ errorMessages err

data Def =
      Group GroupName [GroupLine]
    | Imprt FilePath
    | Use (Either FilePath GroupName)
defsApart :: [Def] -> ([FilePath], [(GroupName, [GroupLine])], [Either FilePath GroupName])
defsApart = go ([], [], [])
    where
    go (is, gs, us) [] = (reverse is, reverse gs, reverse us)
    go (is, gs, us) (Imprt x : xs) = go (x:is, gs, us) xs
    go (is, gs, us) (Group a b : xs) = go (is, (a,b):gs, us) xs
    go (is, gs, us) (Use x : xs) = go (is, gs, x:us) xs
distfixDirective :: Parser DistfixDefs
distfixDirective = do
    (imprts, groups, uses) <- defsApart <$> anyDef `sepAroundBy` nextline
    use <- case uses of
        [] -> pure Nothing
        [x] -> pure $ Just x
        xs -> fail "you can only use one distfix group at a time" --TODO show usage statemetns
    pure $ DFDefs imprts groups use

anyDef :: Parser Def
anyDef = choice [
      uncurry Group <$> defgroup
    , Imprt <$> imprt
    , Use <$> use
    ]

defgroup :: Parser (GroupName, [GroupLine])
defgroup = do
    groupname <- string "group" *> ilws *> name
    liness <- between indent dedent $ do
        (groupline `sepBy1` between2 lws0 comma) `sepBy1` nextline
    pure (groupname, concat liness)

groupline :: Parser GroupLine
groupline = choice [
      do
        dfs <- (assoc <$$> (,) <*> (lws0 *> name)) `sepBy1` between2 ws0 comma
        Right <$> mapM (uncurry mkDistfix) dfs
    , do
        a <- assoc
        ops <- parens $ name `sepBy1` between2 ws0 comma
        Right <$> mapM (mkDistfix a) ops
    , Left <$> name
    ]

assoc :: Parser (Maybe Assoc)
assoc = optional $ choice [
      LeftAssoc <$ string "left"
    , RightAssoc <$ string "right"
    , NonAssoc <$ string "non"
    ]

imprt :: Parser FilePath
imprt = string "import" *> ilws *> filepath

use :: Parser (Either FilePath GroupName)
use = string "use" *> ilws *> choice [
      Left <$> filepath
    , Right <$> name
    ]

filepath :: Parser FilePath
filepath = dqString (('$', '$') : ('s', ' ') : cEscapes)


mkDistfix :: Maybe Assoc -> String -> Parser (Distfix Syntax)
mkDistfix m_assoc funcname = do
    (shape, parts) <- check m_assoc funcname
    pure $ distfix shape (mkPred <$> parts)
    where
    mkPred name (Name test, _) = test == name
    mkPred _ _ = False

check :: Maybe Assoc -> String -> Parser (Shape, [String])
check m_assoc funcname = do
    (openl, parts, openr) <- splitup funcname
    shape <- findShape m_assoc openl openr
    pure (shape, parts)
    where
    findShape (Just LeftAssoc) True True = pure OpenLeft
    findShape (Just LeftAssoc) True False = pure HalfOpenLeft
    findShape (Just LeftAssoc) False _ = fail $ "left-associative distfix must have a slot on the left: " ++ funcname
    findShape (Just RightAssoc) True True = pure OpenRight
    findShape (Just RightAssoc) False True = pure HalfOpenRight
    findShape (Just RightAssoc) False _ = fail $ "right-associative distfix must have a slot on the right: " ++ funcname
    findShape (Just NonAssoc) True True = pure OpenNon
    findShape (Just NonAssoc) False False = pure Closed
    findShape (Just NonAssoc) _ _ = fail $ "non-associative distfix must either have a slot on both sides or on neither: " ++ funcname
    findShape Nothing True True = fail $ "associativity must specified for distfixes with slots on both sides: " ++ funcname
    findShape Nothing True False = pure HalfOpenLeft
    findShape Nothing False True = pure HalfOpenRight
    findShape Nothing False False = pure Closed

splitup :: String -> Parser (Bool, [String], Bool)
splitup "" = error "Something's rotten in Denmark. (Language.Ammonite.Distfix.Parser.splitup got an empty str)"
splitup "_" = fail $ "invalid distfix (must have at least one part): _" -- this is here so I can form str' easily below
splitup str = do
    parts <- go str' []
    when (null parts) $ fail $ "invalid distfix (must have at least one part): " ++ str
    pure (openLeft, parts, openRight)
    where
    openLeft = head str == '_'
    openRight = last str == '_'
    str' = (if openLeft then tail else id) . (if openRight then init else id) $ str
    go str acc = case break (=='_') str of
        ("", _) -> fail $ "invalid distfix (can't have two slots in a row): " ++ str
        (rest, []) -> pure $ reverse (rest:acc)
        (part, '_':rest) -> go rest (part:acc)
        _ -> error "Something's rotten in Denmark. (Language.Ammonite.Distfix.Parser.splitup)"



--FIXME move this stuff (and stuff from normal Syntax.Parser into Syntax.Parser.Common)
ws :: Parser ()
ws = many1_ $ expect "whitespace" $ choice [
      void lws
    , newline
    , void $ nestingComment "#(" ")#"
    , void $ lineComment "#"
    ]

ilws :: Parser ()
ilws = many1_ $ expect "in-line whitespace" $ choice [
      void lws
    , void $ nestingComment "#(" ")#"
    , void $ lineComment "#"
    ]

ws0 :: Parser ()
ws0 = optional_ ws

lws0 :: Parser ()
lws0 = optional_ lws

parens = inParens . between2 ws0