{-#LANGUAGE GeneralizedNewtypeDeriving #-}
module Language.Ammonite.Interpreter.Machine
    ( Machine
    , StartState
    , runMachine
    , Result(..)
    , rts
    , lookupCurrentEnv
    , bindCurrentEnv
    , swapEnv
    , reifyEnv
    , pushCont
    , popCont
    , getStack
    , SplitStack
    , newCue
    , abort
    , capture
    ) where

import Data.IORef
import qualified Data.Map as Map
import Control.Applicative
import Language.Ammonite.Gensym
import Language.Ammonite.Syntax.Abstract
import Language.Ammonite.Interpreter.Data
import Language.Ammonite.Interpreter.RTS
import Text.Luthor (SourcePos)
import Control.Monad.State.Strict
import Control.Monad.IO.Class



--FIXME I need an EitherT ErrorReport
newtype Machine sysval a = Machine { unMachine :: StateT (MachineState sysval) IO a }
    deriving (Functor, Applicative, Monad, MonadIO)

data Result sysval =
      Good (Value sysval)
    | Stuck --TODO what needs to be reported?


type StartState sysval = (RTS sysval, Env sysval, GensymSource)

data MachineState sysval = S
    { msEnv :: Env sysval -- ^ the current environment
    , msControl :: [Cont sysval] -- ^ the current continuation up to most recent environment swap or stack mark
    , msStack :: Continuation sysval -- ^ the continuation for the whole thread
    , msGensym :: GensymSource
    , msRTS :: RTS sysval
    --TODO any thread-local state, such as actual TLS
    --TODO a channel to send commands to the MultiMachine (which coordinates thread lifetimes)
    }


-- visible API --

runMachine :: Machine sysval a -> StartState sysval -> IO a
runMachine action enter_state = evalStateT (unMachine action) (startState enter_state)

startState :: StartState sysval -> MachineState sysval
startState (rts, env, source) = S
    { msEnv = env
    , msControl = emptyCont
    , msStack = emptyStack
    , msGensym = source
    , msRTS = rts
    }

rts :: (RTS sysval -> a) -> Machine sysval a
rts field = Machine $ gets $ field . msRTS

lookupCurrentEnv :: Name -> Machine sysval (Maybe (Value sysval))
lookupCurrentEnv x = Machine $ do
    env <- gets msEnv
    liftIO $ lookupEnv x env

bindCurrentEnv :: Name -> Value sysval -> Machine sysval ()
bindCurrentEnv x v = Machine $
    liftIO . bindEnv x v =<< gets msEnv

swapEnv :: Env sysval -> Machine sysval ()
swapEnv env' = do
    saveEnv
    Machine $ modify $ \s -> s { msEnv = env' }

reifyEnv :: Machine sysval (Env sysval)
reifyEnv = Machine $ gets msEnv

getStack :: Machine sysval (Continuation sysval)
getStack = Machine $ do
    k <- gets msControl
    stack <- gets msStack
    e <- gets msEnv
    pure $ recombineStack (k, e) stack

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


type SplitStack sysval = Maybe (Continuation sysval, Cont sysval, Continuation sysval)

newCue :: Machine sysval Gensym
newCue = gensym

abort :: Gensym -> Machine sysval (SplitStack sysval)
abort cue = do
    split <- capture cue
    case split of
        Nothing -> pure Nothing
        it@(Just (_, _, below)) -> do
            Machine $ modify $ \s -> s { msStack = below, msControl = emptyCont }
            pure it

capture :: Gensym -> Machine sysval (SplitStack sysval)
capture cue = splitStack cue <$> getStack

-- restore, run guards


-- low-level (internal) API --

emptyCont :: [Cont sysval]
emptyCont = []

emptyStack :: Continuation sysval
emptyStack = []

saveEnv :: Machine sysval ()
saveEnv = do
    stack' <- getStack
    Machine $ modify $ \s -> s { msStack = stack', msControl = emptyCont }

restoreEnv :: Machine sysval ()
restoreEnv = Machine $ do
    stack <- gets msStack
    case stack of
        (EnvFrame [(k, e)] : rest) ->
            modify $ \s -> s { msStack = rest, msControl = k, msEnv = e }
        (EnvFrame ((k, e): top) : rest) ->
            modify $ \s -> s { msStack = EnvFrame top : rest, msControl = k, msEnv = e }
        _ -> error "internal error: Language.Ammonite.Interpreter.Machine.restoreEnv called when EnvFrame not on top of stack"

gensym :: Machine sysval Gensym
gensym = Machine $ do
    (next, source) <- gets $ step . msGensym
    modify $ \s -> s { msGensym = source }
    pure next



-- Helpers --

isStackMark :: Cont sysval -> Bool
isStackMark (Barrier _, _) = True
isStackMark (CueCont _ _, _) = True
--TODO once I add marks, I need to put True in here
isStackMark _ = False

recombineStack :: ([Cont sysval], Env sysval) -> Continuation sysval -> Continuation sysval
-- for sake of space, don't bother pushing empty continuations
recombineStack ([], _) stack = stack
-- when we save the same env repeatedly, run in constant space
recombineStack (k, e) (EnvFrame ((k0, e0):top) : rest) | e == e0 =
    EnvFrame ((k++k0, e0):top) : rest
-- when we aren't introducing non-local control flow, just update the current frame
recombineStack frame (EnvFrame top : rest) =
    EnvFrame (frame : top) : rest
-- when non-local control flow is involved, we need to create a new frame
recombineStack frame rest = EnvFrame [frame] : rest;

splitStack :: Gensym -> Continuation sysval -> SplitStack sysval
splitStack cue = go []
    where
    go above (Mark m@(CueCont mark _, _) : below) | cue == mark = Just (above, m, below)
    go above (Mark m@(Barrier thunk, _) : below) = Just (above, m, below)
    go above (next : below) = go (above ++ [next]) below
    go _ [] = Nothing

-- cat stack, gather control guards
