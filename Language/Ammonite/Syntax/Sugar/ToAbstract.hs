module Language.Ammonite.Syntax.Sugar.ToAbstract
	( toAST
	) where

import Text.Parsec.Pos
import Data.Maybe
import Data.Symbol (intern)
import qualified Data.Text as T
import qualified Data.Sequence as Seq
import qualified Data.Map as Map
import Control.Applicative
import Control.Monad

import Language.Ammonite.Syntax.Concrete as CST
import Language.Ammonite.Syntax.Abstract as AST



toAST :: Syntax -> Expr sysval
toAST (Parens x, _) = toAST x
toAST (AnonLambda [] body, _) = toAST body
toAST (AnonLambda [arg] body, pos) =
    let lambda = (Lit $ Prim Lambda 2 [], pos')
        pos' = goPos pos
    in (Ap $ Seq.fromList [lambda, toAST arg, toAST body], pos')
toAST (AnonLambda args body, pos) =
    let lambda = (Lit $ Prim Lambda 2 [], pos')
        args' = (Ap $ Seq.fromList (toAST <$> args), pos')
        pos' = goPos pos
    in (Ap $ Seq.fromList [lambda, args', toAST body], pos')
toAST (x, pos) = (_go x, goPos pos)


_go :: SyntaxCore -> ExprCore sysval
_go (CST.Name x) = AST.Name (intern x)
_go AnonPoint = error "precondition violation: cannot create AST with anonymous points"
_go Unit = Lit UnitVal
_go (Number n) = Lit $ NumVal n
_go (Chr n) = Lit $ ChrVal n
_go (TextStr txt []) = Lit $ StrVal txt
_go (TextStr txt rest) = StrExpr txt (doPair <$> rest)
    where
    doPair (x, txt) = (toAST x, txt)
_go (ByteStr bytes) = Lit $ BytesVal bytes
_go (List xs) =
    let xs' = toAST <$> xs
    in if all isVal xs'
        then Lit . ListVal $ Seq.fromList (fromVal <$> xs')
        else ListExpr xs'
_go (Struct kvs) =
    let kvs' = doPair <$> kvs
    in if all (isVal . snd) kvs'
        then Lit . StructVal $ Map.fromList (fromKVal <$> kvs')
        else StructExpr (Map.fromList kvs')
    where
    doPair (k, x) = (intern k, toAST x)
_go (Record pos varpos kvs varkw) =
    let pos' = toAST <$> pos
        varpos' = toAST <$> varpos
        kvs' = doPair <$> kvs
        varkw' = toAST <$> varkw
    in if all isVal pos'
       && isNothing varpos'
       && all (isVal . snd) kvs'
       && isNothing varkw'
        then Lit $ RecordVal (Seq.fromList $ fromVal <$> pos') (Map.fromList $ fromKVal <$> kvs')
        else RecordExpr pos' varpos' (Map.fromList kvs') varkw'
    where
    doPair (k, x) = (intern k, toAST x)
_go (Quote x) = QuotedExpr (toAST x)
_go (Unquote x) = UnquotedExpr (toAST x)
_go (Combine args) | length args < 2 = error "precondition violation: combine must have at least two subexpressions"
_go (Combine args) = Ap $ Seq.fromList (toAST <$> args)
_go (CST.Block stmts) = AST.Block (toAST <$> stmts)
_go (DotExpr _) = error "precondition violation: cannot create AST with dotted expressions"
_go (Subvalue x r CST.Exists) = AST.Exists (toAST x) (goRoute <$> r)
_go (Subvalue x r CST.Access) = AST.Access (toAST x) (goRoute <$> r)
_go (Subvalue x r (CST.Update x')) = AST.Update (toAST x) (goRoute <$> r) (toAST x')
_go (Subvalue x r CST.Delete) = AST.Delete (toAST x) (goRoute <$> r)


goRoute :: CST.Route -> AST.Route sysval
goRoute (CST.Field name) = AST.Field (intern name)
goRoute (CST.Index x) = AST.Index (toAST x)
goRoute (CST.Slice start stop) = AST.Slice (toAST <$> start) (toAST <$> stop)


goPos :: SourcePos -> SourceLoc
goPos pos = (T.pack $ sourceName pos, sourceLine pos)


isVal (Lit _, _) = True
isVal _ = False
fromVal (Lit x, _) = x
fromKVal (k, (Lit x, _)) = (k, x)

