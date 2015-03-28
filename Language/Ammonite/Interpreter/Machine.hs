{-#LANGUAGE GeneralizedNewtypeDeriving #-}
module Language.Ammonite.Interpreter.Machine
    ( Machine
    , runMachine
    , lookupCurrentEnv
    , swapEnv
    , reifyEnv
    , pushCont
    , popCont
    , Result(..)
    ) where

import Control.Applicative
import Language.Ammonite.Syntax.Abstract
import Text.Luthor (SourcePos)
import Control.Monad.State.Strict
import Control.Monad.IO.Class


newtype Machine sysval a = Machine { unMachine :: StateT (MachineState sysval) IO a }
    deriving (Functor, Applicative, Monad, MonadIO)

data Result sysval =
      Good (Value sysval)
    | Stuck --TODO what needs to be reported?

data MachineState sysval = S
    { msEnv :: Env sysval -- ^ the current environment
    , msControl :: Continuation sysval -- ^ the current continuation up to most recent environment swap or stack mark
    , msStack :: Stack sysval -- ^ the continuation for the whole thread
    --TODO any thread-local state, such as symbol generators and actual TLS
    --TODO a channel to send commands to the MultiMachine (which coordinates thread lifetimes)
    }

type Stack sysval = [Frame sysval]
data Frame sysval = 
      EnvFrame [(Continuation sysval, Env sysval)]
    | Mark (Cont sysval)


-- visible API --

runMachine :: Machine sysval a -> Env sysval -> IO a
runMachine action env = evalStateT (unMachine action) (startState env)

startState :: Env sysval -> MachineState sysval
startState env = S
    { msEnv = env
    , msControl = emptyCont
    , msStack = emptyStack
    }

lookupCurrentEnv :: Name -> Machine sysval (Maybe (Value sysval))
lookupCurrentEnv x = Machine $ do
    env <- gets msEnv
    liftIO $ lookupEnv x env

swapEnv :: Env sysval -> Machine sysval ()
swapEnv env' = do
    saveEnv
    Machine $ modify $ \s -> s { msEnv = env' }

reifyEnv :: Machine sysval (Env sysval)
reifyEnv = Machine $ gets msEnv

pushCont :: Cont sysval -> Machine sysval ()
pushCont cont
    | isStackMark cont = do
        saveEnv
        Machine $ modify $ \s -> s { msStack = Mark cont : msStack s }
    | otherwise = Machine $
        modify $ \s -> s { msControl = cont : msControl s }

popCont :: Machine sysval (Maybe (Cont sysval))
popCont = do
    k <- Machine $ gets msControl
    stack <- Machine $ gets msStack
    case k of
        top:rest -> do
            Machine $ modify $ \s -> s { msControl = rest }
            pure $ Just top
        [] -> case stack of
            (EnvFrame _ : _) -> do
                restoreEnv
                popCont
            (Mark cont : rest) -> do
                Machine $ modify $ \s -> s { msStack = rest }
                pure $ Just cont
            [] -> pure Nothing

            

-- abort, capture, abort+capture, restore, run guards


-- low-level (internal) API --

emptyCont :: Continuation sysval
emptyCont = []

emptyStack :: Stack sysval
emptyStack = []

saveEnv :: Machine sysval ()
saveEnv = Machine $ do
    stack <- gets msStack
    k <- gets msControl
    e <- gets msEnv
    when (not $ null k) $ do
        let stack' = case stack of {
            -- when we save the same env repeatedly, run in constant space
            (EnvFrame ((k0, e0):top) : rest) | e == e0 ->
                EnvFrame ((k++k0, e0):top) : rest;
            -- when we aren't introducing non-local control flow, just update the current frame
            (EnvFrame top : rest) ->
                EnvFrame ((k, e) : top) : rest;
            -- when non-local control flow is involved, we need to create a new frame
            rest ->
                EnvFrame [(k, e)] : rest;
        }
        modify $ \s -> s { msStack = stack', msControl = emptyCont }

restoreEnv :: Machine sysval ()
restoreEnv = Machine $ do
    stack <- gets msStack
    case stack of
        (EnvFrame [(k, e)] : rest) ->
            modify $ \s -> s { msStack = rest, msControl = k, msEnv = e }
        (EnvFrame ((k, e): top) : rest) ->
            modify $ \s -> s { msStack = EnvFrame top : rest, msControl = k, msEnv = e }
        _ -> error "internal error: Language.Ammonite.Interpreter.Machine.restoreEnv called when EnvFrame not on top of stack"

-- Helpers --

isStackMark :: Cont sysval -> Bool
--TODO once I add marks, I need to put True in here
isStackMark _ = False

-- split stack, cat stack, gather control guards
