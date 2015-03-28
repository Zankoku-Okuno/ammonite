module Language.Ammonite.Interpreter.Evaluator where

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
eval _ = error $ "eval unimplemented"

elaborate :: Expr sysval -> Machine sysval (Expr sysval)
elaborate = error "elaborate unimplemented"
    -- pushes cont, updates location, returns smaller expression

reduce :: Value sysval -> Machine sysval (Value sysval)
reduce v = maybe (pure v) (reduce' v) =<< popCont
    where
    reduce' :: Value sysval -> Cont sysval -> Machine sysval (Value sysval)
    reduce' v k = error "reduce unimplemented"
    -- pops cont, places val into context, then resolves that context
    -- which may involve tail-calling another elaborate or reduce