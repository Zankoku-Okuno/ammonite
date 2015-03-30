module Language.Ammonite.Interpreter.Environment
    ( emptyEnv
    , stdEnv
    , startEnv
    , childEnv
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

childEnv :: Env (sysval) -> IO (Env sysval)
childEnv parent = do
    empty <- emptyEnv
    return $ empty { envParent = Just parent }


stdBindings :: [(Name, Value sysval)]
stdBindings = map (\(x, v) -> (intern x, v))
    [ ("_is_", PrimForm Define 2 [])
    , ("λ", PrimForm Lambda 2 [])
    , ("lambda", PrimForm Lambda 2 [])
    , ("vau", PrimForm Vau 3 [])
    , ("eval", PrimAp Eval 2 [])
    
    , ("_+_", PrimAp Add 2 [])

    , ("newEnv", PrimAp NewEnv 2 [])

    , ("true", TrueVal)
    , ("false", FalseVal)
    ]