{-#LANGUAGE ViewPatterns #-}
module Language.Ammonite.Interpreter.Evaluator where

import Data.Sequence (viewl, viewr, ViewL(..), ViewR(..))
import Control.Applicative
import Control.Monad

import Language.Ammonite.Syntax.Abstract
import Language.Ammonite.Interpreter.Machine
import Language.Ammonite.Syntax.Printer


run :: Expr sysval -> Env sysval -> IO (Value sysval)
run prog env = runMachine (eval prog) env

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
reduce v = maybe (pure v) (reduce' v) =<< popCont
    where
    reduce' :: Value sysval -> Cont sysval -> Machine sysval (Value sysval)
    --FIXME if it's an operative, don't move to ApCont, go straight to apply
    reduce' f (OpCont arg, pos) = do
        pushCont (ApCont f, pos)
        eval arg
    reduce' v (ApCont f, pos) = apply f v pos
    reduce' v k = error "reduce unimplemented"
    -- pops cont, places val into context, then resolves that context
    -- which may involve tail-calling another elaborate or reduce


apply :: Value sysval -> Value sysval -> SourceLoc -> Machine sysval (Value sysval)
apply (Prim op n args) nextArg pos | n > 1 = reduce (Prim op (n-1) (args ++ [nextArg]))
apply (Prim Add 1 [v1]) v2 pos = case (v1, v2) of
    (NumVal a, NumVal b) -> reduce $ NumVal (a+b)
    _ -> error "type error in add unimplemented"