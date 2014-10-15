module Language.Ammonite.Syntax.Sugar
    ( desugar
    ) where

import Text.Parsec.Pos
import Data.Maybe
import Data.Symbol (intern)
import qualified Data.Text as T
import qualified Data.Sequence as Seq
import qualified Data.Map as Map
import Control.Applicative

import Language.Ammonite.Syntax.Concrete as CST
import Language.Ammonite.Syntax.Abstract as AST

import Language.Ammonite.Syntax.Sugar.AnonPoint


--FIXME other desugaring stages
desugar :: Syntax -> Either String (Expr sysval)
desugar = (xform <$>) . deAnonPoint

xform :: Syntax -> Expr sysval
xform (Parens x, _) = xform x
xform (AnonLambda [] body, _) = xform body
xform (AnonLambda [arg] body, pos) =
    let lambda = (Lit $ Prim Lambda, pos')
        pos' = xformPos pos
    in (Ap [lambda, xform arg, xform body], pos')
xform (AnonLambda args body, pos) =
    let lambda = (Lit $ Prim Lambda, pos')
        args' = (Ap $ xform <$> args, pos')
        pos' = xformPos pos
    in (Ap [lambda, args', xform body], pos')
xform (x, pos) = (_xform x, xformPos pos)

_xform :: SyntaxCore -> ExprCore sysval
_xform (CST.Name x) = AST.Name (intern x)
_xform AnonPoint = error "precondition violation: cannot create AST with anonymous points"
_xform Unit = Lit UnitVal
_xform (Number n) = Lit $ NumVal n
_xform (Chr n) = Lit $ ChrVal n
_xform (TextStr txt []) = Lit $ StrVal txt
_xform (TextStr txt rest) = StrExpr txt (doPair <$> rest)
    where
    doPair (x, txt) = (xform x, txt)
_xform (ByteStr bytes) = Lit $ BytesVal bytes
_xform (List xs) =
    let xs' = xform <$> xs
    in if all isVal xs'
        then Lit . ListVal $ Seq.fromList (fromVal <$> xs')
        else ListExpr xs'
_xform (Struct kvs) =
    let kvs' = doPair <$> kvs
    in if all (isVal . snd) kvs'
        then Lit . StructVal $ Map.fromList (fromKVal <$> kvs')
        else StructExpr (Map.fromList kvs')
    where
    doPair (k, x) = (intern k, xform x)
_xform (Record pos varpos kvs varkw) =
    let pos' = xform <$> pos
        varpos' = xform <$> varpos
        kvs' = doPair <$> kvs
        varkw' = xform <$> varkw
    in if all isVal pos'
       && isNothing varpos'
       && all (isVal . snd) kvs'
       && isNothing varkw'
        then Lit $ RecordVal (Seq.fromList $ fromVal <$> pos') (Map.fromList $ fromKVal <$> kvs')
        else RecordExpr pos' varpos' (Map.fromList kvs') varkw'
    where
    doPair (k, x) = (intern k, xform x)
_xform (Quote x) = QuotedExpr (xform x)
_xform (Unquote x) = UnquotedExpr (xform x)
_xform (Combine args) | length args < 2 = error "precondition violation: combine must have at least two subexpressions"
_xform (Combine args) = Ap (xform <$> args)
_xform (CST.Block stmts) = AST.Block (xform <$> stmts)
_xform (DotExpr _) = error "precondition violation: cannot create AST with dotted expressions"
_xform (Subvalue x r CST.Exists) = AST.Exists (xform x) (xformRoute <$> r)
_xform (Subvalue x r CST.Access) = AST.Access (xform x) (xformRoute <$> r)
_xform (Subvalue x r (CST.Update x')) = AST.Update (xform x) (xformRoute <$> r) (xform x')
_xform (Subvalue x r CST.Delete) = AST.Delete (xform x) (xformRoute <$> r)

xformRoute :: CST.Route -> AST.Route sysval
xformRoute (CST.Field name) = AST.Field (intern name)
xformRoute (CST.Index x) = AST.Index (xform x)
xformRoute (CST.Slice start stop) = AST.Slice (xform <$> start) (xform <$> stop)


xformPos :: SourcePos -> SourceLoc
xformPos pos = (T.pack $ sourceName pos, sourceLine pos)


isVal (Lit _, _) = True
isVal _ = False
fromVal (Lit x, _) = x
fromKVal (k, (Lit x, _)) = (k, x)


