module Language.Ammonite.Syntax.Sugar.Distfix
    ( deDistfix
    , defaultDistfixes
    ) where

import Data.List
import Control.Applicative
import Control.Monad
import Control.Monad.Writer
import Language.Distfix

import Language.Ammonite.Syntax.Concrete
import Language.Ammonite.Syntax.Sugar.Distfix.Defaults


deDistfix :: [[Distfix Syntax]] -> Syntax -> Either String Syntax
deDistfix dft x =
    let (x', errors) = runWriter (go (reverse dft) x)
    in if null errors
        then Right x' --FIXME check that no parts are left over
        else Left $ intercalate "\n" $ showError <$> errors
    where
    showError options =
           "ambiguous distfixes at "
        ++ show (snd $ head options)
        ++ ":\n"
        ++ (intercalate "\n    " $ show <$> options)


go :: [[Distfix Syntax]] -> Syntax -> Writer [[Syntax]] Syntax
go dft (Combine es, pos) = combine <$> (mapM (go dft) es >>= go1 dft)
go dft (e, pos) = do
    e' <- _go dft e
    pure (e', pos)

go1 :: [[Distfix Syntax]] -> [Syntax] -> Writer [[Syntax]] [Syntax]
go1 _ [] = error "internal error: go1 got empty expression list"
go1 _ [e] = pure [e]
go1 [] es = pure es
go1 dft@(dfs:rest) es = case findDistfixes dfs es of
    NoMatch -> go1 rest es
    OneMatch m -> rewrite (go1 dft) m
    Ambiguous ms -> do
        options <- map combine <$> mapM (rewrite pure) ms
        tell [options]
        pure es

rewrite :: ([Syntax] -> Writer [[Syntax]] [Syntax]) -> Detection Syntax -> Writer [[Syntax]] [Syntax]
rewrite recurse m = do
    let before = detectionBefore m
        after = detectionAfter m
    slots' <- recurse `mapM` detectionSlots m
    let m' = m { detectionSlots = slots' }
    if null before && null after
        then pure $ mkCall m'
        else recurse $ before++[combine $ mkCall m']++after

mkCall :: Detection Syntax -> [Syntax]
mkCall detect = (f, pos):args
    where
    f = mkName $ fromName <$> detectionParts detect
    args = case detectionSlots detect of
        [] -> error "precondition violation: no slots in distfix detection"
        xs -> mkArg <$> xs
    pos = snd . head $ detectionParts detect
    fromName (Name name, _) = name
    fromName _ = error "precondition violation: distfix part was not a Name"
    mkName parts = Name $ case detectionShape detect of
        Closed -> intercalate "_" parts
        HalfOpenLeft -> concat $ ('_':) <$> parts 
        HalfOpenRight -> concat $ (++"_") <$> parts
        _ -> concat $ (('_':) <$> parts) ++ ["_"]
    mkArg [] = error "precondition violation: empty slot in distfix detection"
    mkArg [e] = e
    mkArg es = (Combine es, snd $ head es)


combine :: [Syntax] -> Syntax
combine [] = error "precondition violation: combine must have at least two subexpressions"
combine [e] = e
combine (e@(_, pos):es) = (Combine (e:es), pos)

_go :: [[Distfix Syntax]] -> SyntaxCore -> Writer [[Syntax]] SyntaxCore
_go dft (Parens e) = Parens <$> go dft e
_go dft (AnonLambda xs e) = AnonLambda <$> mapM (go dft) xs <*> go dft e
_go dft e@(Name _) = pure e
_go dft e@AnonPoint = pure e
_go dft e@Unit = pure e
_go dft e@(Number _) = pure e
_go dft e@(Chr _) = pure e
_go dft (TextStr txt rest) = TextStr txt <$> doPair `mapM` rest
    where
    doPair (x, txt) = do
        x' <- go dft x
        pure (x', txt)
_go dft e@(ByteStr _) = pure e
_go dft (List es) = List <$> mapM (go dft) es
_go dft (Struct kvs) = Struct <$> doPair `mapM` kvs
    where
    doPair (name, x) = do
        x' <- go dft x
        pure (name, x')
_go dft (Record xs varpos kvs varkw) = do
    xs' <- go dft `mapM` xs
    varpos' <- maybe (pure Nothing) ((Just <$>) . go dft) varpos
    kvs' <- doPair `mapM` kvs
    varkw' <- maybe (pure Nothing) ((Just <$>) . go dft) varkw
    pure $ Record xs' varpos' kvs' varkw'
    where
    doPair (name, x) = do
        x' <- go dft x
        pure (name, x')
_go dft (Subvalue x route act) = do
    x' <- go dft x
    route' <- doRoute `mapM` route
    act' <- doAct act
    pure $ Subvalue x' route' act'
    where
    doRoute (Field x) = pure $ Field x
    doRoute (Index x) = Index <$> go dft x
    doRoute (Slice start stop) = do
        start' <- maybe (pure Nothing) ((Just <$>) . go dft) start
        stop' <- maybe (pure Nothing) ((Just <$>) . go dft) stop
        pure $ Slice start' stop'
    doAct (Update x) = Update <$> go dft x
    doAct x = pure x
_go dft (Quote e) = Quote <$> go dft e
_go dft (Unquote e) = Unquote <$> go dft e
_go dft (Combine es) = Combine <$> mapM (go dft) es
_go dft (DotExpr e) = DotExpr <$> go dft e
_go dft (Block es) = Block <$> mapM (go dft) es