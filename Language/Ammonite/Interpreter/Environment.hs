{-#LANGUAGE OverloadedStrings #-}
module Language.Ammonite.Interpreter.Environment
    ( emptyEnv
    , stdEnv
    , startEnv
    , childEnv
    , copyEnv
    ) where

import Data.Symbol
import Data.Map (Map)
import qualified Data.Map as Map
import Data.IORef
import Language.Ammonite.Syntax.Abstract
import Language.Ammonite.Interpreter.RTS


emptyEnv :: IO (Env sysval)
emptyEnv = do
    it <- newIORef $ Map.empty
    return Env
        { envBindings = it
        , envParent = Nothing
        }

stdEnv :: RTS sysval -> IO (Env sysval)
stdEnv rts = do
    it <- newIORef $ Map.fromList $ stdBindings rts
    return Env
        { envBindings = it
        , envParent = Nothing
        }

startEnv :: RTS sysval -> IO (Env sysval)
startEnv rts = do
    std <- stdEnv rts
    empty <- emptyEnv
    return $ empty { envParent = Just std }

childEnv :: Env sysval -> IO (Env sysval)
childEnv parent = do
    empty <- emptyEnv
    return $ empty { envParent = Just parent }

copyEnv :: Env sysval -> IO (Env sysval)
copyEnv env = do
    cell <- newIORef =<< readIORef (envBindings env)
    return Env
        { envBindings = cell
        , envParent = envParent env
        }

stdBindings :: RTS sysval -> [(Name, Value sysval)]
stdBindings rts = map (\(x, v) -> (intern x, v))
    [ ("_is_", PrimForm Define 2 [])
    , ("λ", PrimForm Lambda 2 [])
    , ("lambda", PrimForm Lambda 2 [])
    , ("vau", PrimForm Vau 3 [])
    , ("eval", PrimAp Eval 2 [])

    , ("newCue", PrimAp NewCue 1 [])
    
    , ("_+_", PrimAp Add 2 [])

    , ("newEnv", PrimAp NewEnv 2 [])
    , ("handle", PrimAp Handle 2 [])

    , ("true", TrueVal)
    , ("false", FalseVal)
    , ("exn", rtsExnCue rts)

    , ("print", PrimAp DELME_Print 1 [])
    ]