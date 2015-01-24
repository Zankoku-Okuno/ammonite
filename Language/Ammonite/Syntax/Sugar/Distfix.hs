module Language.Ammonite.Syntax.Sugar.Distfix (deDistfix) where

import Data.List
import Control.Applicative
import Control.Monad
import Control.Monad.Writer
import Language.Distfix

import Language.Ammonite.Syntax.Concrete


deDistfix :: [[Distfix Syntax]] -> Syntax -> Either String Syntax
deDistfix dft x =
    let (x', errors) = runWriter (go dft x)
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
go dft (Combine es, pos) = go1 pos dft es
go dft (e, pos) = do
    e' <- _go dft e
    pure (e', pos)

go1 :: SourcePos -> [[Distfix Syntax]] -> [Syntax] -> Writer [[Syntax]] Syntax
go1 pos _ es | length es < 2 = error "precondition violation: combine must have at least two subexpressions"
go1 pos [] es = pure (Combine es, pos)
go1 pos dft@(dfs:rest) es = case findDistfixes dfs es of
    NoMatch -> go1 pos rest es
    OneMatch m -> rewrite (go1 pos dft) m
    Ambiguous ms -> do
        options <- mapM (rewrite wrap) ms
        tell [options]
        pure (Combine es, pos)
        where
        wrap [] = error "precondition violation: combine must have at least two subexpressions"
        wrap (e@(_, pos) : es) = pure (Combine $ e:es, pos)

rewrite :: ([Syntax] -> Writer [[Syntax]] Syntax) -> Detection Syntax -> Writer [[Syntax]] Syntax
rewrite recurse m =
    let before = detectionBefore m
        after = detectionAfter m
        call = mkCall m
    in if null before && null after
        then pure $ call
        else recurse $ before++[call]++after

mkCall :: Detection Syntax -> Syntax
mkCall detect = (Combine $ (f, pos):args, pos)
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
    mkArg [] = error "precondition violation: empty slot in distfix detection"
    mkArg [e] = e
    mkArg es = (Combine es, snd $ head es)


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