module Language.Ammonite.Syntax.Sugar.AnonPoint
    ( deAnonPoint
    ) where

import Control.Applicative
import Control.Monad.State
import Text.Luthor (SourcePos)
import Language.Ammonite.Syntax.Concrete


deAnonPoint :: Syntax -> Either String Syntax
deAnonPoint x =
    let (x', (_, gather)) = runState (go x) (1, [])
    in if null gather
        then Right x'
        else Left $ "unbound anonymous points:" ++ concatMap (("\n  "++) . show . snd) gather


go :: Syntax -> M Syntax
go (AnonPoint, pos) = gensym pos
go (Parens x, pos) = do
    x' <- go x
    gather <- points
    pure (AnonLambda gather x', pos)
go (x, pos) = do
    x' <- _go x
    pure (x', pos)

_go :: SyntaxCore -> M SyntaxCore
_go (Parens x) = error "internal error: Syntax.Sugar.AnonPoint._go called on Syntax.Concrete.Parens"
_go (AnonLambda _ _) = error "precondition violated: syntax should not be run through deAnonPoint twice."
_go x@(Name _) = pure x
_go AnonPoint = error "internal error: Syntax.Sugar.AnonPoint._go called on Syntax.Concrete.AnonPoint"
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
_go (Quote x) = Quote <$> go x
_go (Unquote x) = Unquote <$> go x
_go (List xs) = List <$> go `mapM` xs
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
_go (Block stmts) = Block <$> go `mapM` stmts
_go (Combine xs) | length xs < 2 = error "precondition violation: combine must have at least two subexpressions"
_go (Combine xs) = Combine <$> go `mapM` xs
_go (DotExpr x) = DotExpr <$> go x
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


type M a = State (Int, [Syntax]) a

gensym :: SourcePos -> M Syntax
gensym pos = do
    (n, gather) <- get
    let name = (Name $ "**gensym_" ++ show n, pos)
    put (n+1, gather++[name])
    pure name

points :: M [Syntax]
points = do
    (n, gather) <- get
    put (n, [])
    pure gather



