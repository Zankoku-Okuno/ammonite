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
--TODO block
eval _ = error $ "eval unimplemented"

elaborate :: Expr sysval -> Machine sysval (Expr sysval)
elaborate = error "elaborate unimplemented"
    -- pushes cont, updates location, returns smaller expression

reduce :: Value sysval -> Machine sysval (Value sysval)
reduce v = maybe (pure v) (flip reduce' v) =<< popCont
    where
    reduce' :: Cont sysval -> Value sysval -> Machine sysval (Value sysval)
    --FIXME if it's an operative, don't move to ApCont, go straight to apply
    reduce' (OpCont arg, pos) f = do
        pushCont (ApCont f, pos)
        eval arg
    reduce' k@(ApCont f@(Prim _ _ _), pos) (ThunkVal thunk_cell) = do
        thunk <- liftIO $ readIORef thunk_cell
        case thunk of
            Left v -> apply f v pos
            Right (e, env) -> do
                pushCont k
                pushCont (ThunkCont thunk_cell, pos)
                swapEnv env
                eval e
    reduce' (ApCont f, pos) v = apply f v pos
    reduce' (ThunkCont cell, _) v = do
        liftIO $ writeIORef cell $ Left v
        reduce v
    reduce' k v = error "reduce unimplemented"
    -- pops cont, places val into context, then resolves that context
    -- which may involve tail-calling another elaborate or reduce


apply :: Value sysval -> Value sysval -> SourceLoc -> Machine sysval (Value sysval)
apply (Prim op n args) nextArg pos | n > 1 = reduce (Prim op (n-1) (args ++ [nextArg]))
apply (Prim Add 1 [v1]) v2 pos = case (v1, v2) of
    (NumVal a, NumVal b) -> reduce $ NumVal (a+b)
    _ -> error "type error in add unimplemented"