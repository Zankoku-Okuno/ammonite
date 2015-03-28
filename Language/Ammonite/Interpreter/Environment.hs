module Language.Ammonite.Interpreter.Environment
    ( startEnv
    ) where

import Data.Symbol
import Data.Map (Map)
import qualified Data.Map as Map
import Data.IORef
import Language.Ammonite.Syntax.Abstract


startEnv :: IO (Env sysval)
startEnv = do
    it <- newIORef $ Map.fromList startBindings
    return Env
        { envBindings = it
        , envParent = Nothing
        }

startBindings :: [(Name, Value sysval)]
startBindings = map (\(x, v) -> (intern x, v))
    [ ("x", NumVal 3)
    ]