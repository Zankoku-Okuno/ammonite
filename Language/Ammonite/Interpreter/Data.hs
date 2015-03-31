{-#LANGUAGE ViewPatterns #-}
module Language.Ammonite.Interpreter.Data
    ( isApplicative
    , bindEnv
    , lookupEnv
    , getField
    , setField
    , getIndex
    , setIndex
    ) where

import Control.Applicative

import Data.IORef
import Data.Ratio
import Data.Sequence (Seq, viewl, viewr, ViewL(..), ViewR(..), (<|), (|>))
import qualified Data.Sequence as Seq
import qualified Data.Map as Map

import Language.Ammonite.Syntax.Abstract



isApplicative :: Value sysval -> Bool
isApplicative f@(ClosureVal { opParameters = Left _ }) = False
isApplicative f@(ClosureVal { opParameters = Right _ }) = True
isApplicative (PrimAp {}) = True
isApplicative (PrimForm {}) = False
isApplicative (SysOp {}) = True
isApplicative _ = True

lookupEnv :: Name -> Env sysval -> IO (Maybe (Value sysval))
lookupEnv x env = do
    bindings <- readIORef $ envBindings env
    case Map.lookup x bindings of
        Just v -> pure $ Just v
        Nothing -> case envParent env of
            Nothing -> pure Nothing
            Just parent -> lookupEnv x parent

bindEnv :: Name -> Value sysval -> Env sysval -> IO ()
--FIXME I think changing an environment other than extending it could be very bad for compilation, even JIT compilation
--even extension could be bad, if it introduces shadowing
bindEnv x v env =
    modifyIORef (envBindings env) $ Map.insert x v

getField :: Value sysval -> Name -> IO (Maybe (Value sysval))
getField (StructVal s) x = pure $ Map.lookup x s
getField (RecordVal _ kw) x = pure $ Map.lookup x kw
getField (EnvVal cell) x = lookupEnv x cell
getField _ _ = pure Nothing

setField :: Value sysval -> Name -> Value sysval -> IO (Maybe (Value sysval))
setField (StructVal s) x subv = pure . Just . StructVal $ Map.insert x subv s
setField (RecordVal pos kw) x subv = pure . Just . RecordVal pos $ Map.insert x subv kw
setField (EnvVal cell) x subv = Just (EnvVal cell) <$ bindEnv x subv cell
setField _ _ _ = pure Nothing


toIndex :: Value sysval -> Maybe Int
--FIXME what if someone passes an integer that's too big for Int?
toIndex (NumVal r@(numerator -> i)) | denominator r == 1 = Just $ fromIntegral i
toIndex _ = Nothing

getIndex :: Value sysval -> Value sysval -> Either (Value sysval) (Maybe (Value sysval))
getIndex v (toIndex -> Just i) = Right $ go v i
    where
    go (ListVal xs) i = seqIx xs i
    go (RecordVal pos _) i = seqIx pos i
    go _ _ = Nothing
getIndex _ i = Left i

setIndex :: Value sysval -> Value sysval -> Value sysval -> Either (Value sysval) (Maybe (Value sysval))
setIndex v (toIndex -> Just i) subv = Right $ case v of
    ListVal xs -> ListVal <$> seqUpdate xs i subv
    RecordVal pos kw -> flip RecordVal kw <$> seqUpdate pos i subv
    _ -> Nothing
setIndex _ i _ = Left i

seqIx :: Seq (Value sysval) -> Int -> Maybe (Value sysval)
seqIx xs i | 0 <= i && i < Seq.length xs = Just $ Seq.index xs i
           | (-(Seq.length xs)) <= i && i < 0 = Just $ Seq.index xs (Seq.length xs + i)
           | otherwise = Nothing

seqUpdate :: Seq (Value sysval) -> Int -> Value sysval -> Maybe (Seq (Value sysval))
seqUpdate xs i v' | 0 <= i && i < Seq.length xs = Just $ Seq.update i v' xs
                  | (-(Seq.length xs)) <= i && i < 0 = Just $ Seq.update (Seq.length xs + i) v' xs
                  | otherwise = Nothing