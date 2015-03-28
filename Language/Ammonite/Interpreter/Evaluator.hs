{-#LANGUAGE ViewPatterns #-}
module Language.Ammonite.Interpreter.Evaluator where

import Data.Sequence (viewl, viewr, ViewL(..), ViewR(..))
import Data.IORef

import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class

import Language.Ammonite.Syntax.Abstract
import Language.Ammonite.Interpreter.Machine
import Language.Ammonite.Syntax.Printer


run :: Expr sysval -> Env sysval -> IO (Result sysval)
run prog env = Good <$> runMachine (eval prog) env

eval :: Expr sysval -> Machine sysval (Value sysval)
eval (Lit val, _) = reduce val
eval (Name x, _) = do
    m_val <- lookupCurrentEnv x
    case m_val of
        Nothing -> error "unimplemented: scope error"
        Just val -> reduce val
--TODO
eval (Ap (viewl -> f :< args), pos) = do
        pushArgs args
        eval f
    where
    pushArgs (viewr -> EmptyR) = pure ()
    pushArgs (viewr -> rest :> lastArg) = do
        pushCont (OpCont lastArg, pos)
        pushArgs rest
eval (Block es, pos) = seqExprs es pos
eval _ = error $ "eval unimplemented"


reduce :: Value sysval -> Machine sysval (Value sysval)
reduce v = maybe (pure v) (flip reduce' v) =<< popCont
    where
    reduce' :: Cont sysval -> Value sysval -> Machine sysval (Value sysval)
    reduce' (OpCont arg, pos) f@(PrimForm _ _ _) = form f arg pos
    --FIXME if it's an operative, don't move to ApCont, go straight to form
    reduce' (OpCont arg, pos) f = do
        pushCont (ApCont f, pos)
        eval arg
    reduce' k@(ApCont f@(PrimAp _ _ _), pos) (ThunkVal thunk_cell) = do
        thunk <- liftIO $ readIORef thunk_cell
        case thunk of
            Left v -> apply f v pos
            Right (e, env) -> do
                pushCont k
                pushCont (ThunkCont thunk_cell, pos)
                swapEnv env
                eval e
    reduce' (ApCont f, pos) v = apply f v pos
    reduce' (BlockCont es, pos) _ = seqExprs es pos
    reduce' (ThunkCont cell, _) v = do
        liftIO $ writeIORef cell $ Left v
        reduce v
    reduce' (MatchCont p, pos) v = match p v
    reduce' k v = error "reduce unimplemented"

form :: Value sysval -> Expr sysval -> SourceLoc -> Machine sysval (Value sysval)
form (PrimForm op n args) next _ | n > 1 = do
    env <- reifyEnv
    reduce (PrimForm op (n-1) (args ++ [(next, env)]))
form (PrimForm Define 1 [p]) e pos = do
    pushCont (MatchCont p, pos)
    eval e
    --evaluate e, then pattern match it against p, the resulting environment is then used to extend the current
    --if no match, that's an exception

apply :: Value sysval -> Value sysval -> SourceLoc -> Machine sysval (Value sysval)
apply (PrimAp op n args) next pos | n > 1 = reduce (PrimAp op (n-1) (args ++ [next]))
apply (PrimAp Add 1 [v1]) v2 pos = case (v1, v2) of
    (NumVal a, NumVal b) -> reduce $ NumVal (a+b)
    _ -> error "type error in add unimplemented"


seqExprs :: [Expr sysval] -> SourceLoc -> Machine sysval (Value sysval)
seqExprs [] _ = reduce UnitVal
seqExprs [e] _ = eval e
seqExprs (e:rest) pos = do
    pushCont (BlockCont rest, pos)
    eval e

match :: (Expr sysval, Env sysval) -> Value sysval -> Machine sysval (Value sysval)
match ((Name x, _), _) v = do
    bindCurrentEnv x v
    reduce v
match (dector, env) v = error "unimplemented: match"