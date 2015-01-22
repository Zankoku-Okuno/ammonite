module Language.Ammonite.Syntax.Sugar.DotExpr
    ( deDot
    ) where

import Control.Applicative
import Control.Monad.Writer
import Text.Luthor (SourcePos)
import Language.Ammonite.Syntax.Concrete


deDot :: Syntax -> Either String Syntax
deDot x =
    let (x', errors) = runWriter (go x)
    in if null errors
        then Right x'
        else Left $ "misplaced dotted expressions:" ++ concatMap (("\n  "++) . show . snd) errors


go :: Syntax -> Writer [Syntax] Syntax
go x@(Combine xs, pos) = case findDot xs of
    Nothing -> do
        xs' <- mapM go xs
        pure (Combine xs', pos)
    Just ([], dot, _) -> tell [x] >> pure x
    Just ([obj], dot, args) -> do
        let call = [dot, obj]
        go (Combine $ call ++ args, pos)
    Just (before, dot, args) -> do
        let call = [dot, (Combine before, pos)]
        go (Combine $ call ++ args, pos)
go dot@(DotExpr inner, pos) = do
    inner' <- go inner
    tell [dot] >> pure dot
go (x, pos) = do
    x' <- _go x
    pure (x', pos)


isDotted :: Syntax -> Bool
isDotted (DotExpr _, pos) = True
isDotted _ = False

findDot :: [Syntax] -> Maybe ([Syntax], Syntax, [Syntax])
findDot xs | length xs < 2 = error "precondition violation: combine must have at least two subexpressions"
        | otherwise = case break isDotted xs of
    (_, []) -> Nothing
    (before, (DotExpr dot, pos):after) -> Just (before, dot, after)
    (before, _:after) -> error "internal error: isDotted failed to find only DotExpr"


_go :: SyntaxCore -> Writer [Syntax] SyntaxCore
_go (Parens x) = Parens <$> go x
_go (AnonLambda params body) = AnonLambda <$> mapM go params <*> go body
_go x@(Name _) = pure x
_go x@AnonPoint = pure x
_go x@Unit = pure x
_go x@(Number _) = pure x
_go x@(Chr _) = pure x
_go (TextStr txt rest) = 
    TextStr txt <$> doPair `mapM` rest
    where
    doPair (x, txt) = do
        x' <- go x
        pure (x', txt)
_go x@(ByteStr _) = pure x
_go (List xs) = List <$> mapM go xs
_go (Struct kvs) = Struct <$> doPair `mapM` kvs
    where
    doPair (name, x) = do
        x' <- go x
        pure (name, x')
_go (Record xs varpos kvs varkw) = do
    xs' <- go `mapM` xs
    varpos' <- maybe (pure Nothing) ((Just <$>) . go) varpos
    kvs' <- doPair `mapM` kvs
    varkw' <- maybe (pure Nothing) ((Just <$>) . go) varkw
    pure $ Record xs' varpos' kvs' varkw'
    where
    doPair (name, x) = do
        x' <- go x
        pure (name, x')
_go (Subvalue x route act) = do
    x' <- go x
    route' <- doRoute `mapM` route
    act' <- doAct act
    pure $ Subvalue x' route' act'
    where
    doRoute (Field x) = pure $ Field x
    doRoute (Index x) = Index <$> go x
    doRoute (Slice start stop) = do
        start' <- maybe (pure Nothing) ((Just <$>) . go) start
        stop' <- maybe (pure Nothing) ((Just <$>) . go) stop
        pure $ Slice start' stop'
    doAct (Update x) = Update <$> go x
    doAct x = pure x
_go (Quote x) = Quote <$> go x
_go (Unquote x) = Unquote <$> go x
_go (Block xs) = Block <$> mapM go xs




