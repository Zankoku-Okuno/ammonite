module Language.Ammonite.Interpreter.Environment
    ( emptyEnv
    , stdEnv
    , startEnv
    ) where

import Data.Symbol
import Data.Map (Map)
import qualified Data.Map as Map
import Data.IORef
import Language.Ammonite.Syntax.Abstract


emptyEnv :: IO (Env sysval)
emptyEnv = do
    it <- newIORef $ Map.fromList stdBindings
    return Env
        { envBindings = it
        , envParent = Nothing
        }

stdEnv :: IO (Env sysval)
stdEnv = do
    it <- newIORef $ Map.fromList stdBindings
    return Env
        { envBindings = it
        , envParent = Nothing
        }

startEnv :: IO (Env sysval)
startEnv = do
    std <- stdEnv
    empty <- emptyEnv
    return $ empty { envParent = Just std }

stdBindings :: [(Name, Value sysval)]
stdBindings = map (\(x, v) -> (intern x, v))
    [ ("_+_", Prim Add 2 [])
    ]