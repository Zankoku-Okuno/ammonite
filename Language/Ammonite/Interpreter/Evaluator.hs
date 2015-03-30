{-#LANGUAGE ViewPatterns #-}
module Language.Ammonite.Interpreter.Evaluator where

import Data.Ratio
import Data.Sequence (viewl, viewr, ViewL(..), ViewR(..), (<|), (|>))
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import qualified Data.Map as Map
import Data.IORef

import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class

import Language.Ammonite.Syntax.Abstract
import Language.Ammonite.Interpreter.Machine
import Language.Ammonite.Interpreter.Environment
import Language.Ammonite.Syntax.Printer


run :: Expr sysval -> Env sysval -> IO (Result sysval)
run prog env = Good <$> runMachine (eval prog) env

eval :: Expr sysval -> Machine sysval (Value sysval)
eval (Lit val, _) = reduce val
eval (Name x, _) = do
    m_val <- lookupCurrentEnv x
    case m_val of
        Nothing -> error $ "unimplemented: scope error (" ++ show x ++ ")"
        Just val -> reduce val
eval (ListExpr [], _) = reduce $ ListVal Seq.empty
eval (ListExpr (e:es), pos) = do
    pushCont (ListCont Seq.empty es, pos)
    eval e
eval (StructExpr (Map.toList -> []), _) = reduce $ StructVal Map.empty
eval (StructExpr (Map.toList -> (x,e):fields), pos) = do
    pushCont (StructCont Map.empty x fields, pos)
    eval e
--TODO
eval (Exists e r, pos) = do
    pushCont (ExistsCont r, pos)
    eval e
eval (Access e r, pos) = do
    pushCont (AccessCont r, pos)
    eval e
eval (Update e r e', pos) = do
    pushCont (UpdateCont r e', pos)
    eval e
--TODO
eval (Ap (viewl -> f :< args), pos) = do
        pushArgs args
        eval f
    where
    pushArgs (viewr -> EmptyR) = pure ()
    pushArgs (viewr -> rest :> lastArg) = do
        pushCont (OpCont lastArg, pos)
        pushArgs rest
eval (Block es, pos) = seqExprs es pos
eval _ = error $ "eval unimplemented"


reduce :: Value sysval -> Machine sysval (Value sysval)
reduce v = maybe (pure v) (flip reduce' v) =<< popCont
    where
    -- Compound Data Contruction
    reduce' :: Cont sysval -> Value sysval -> Machine sysval (Value sysval)
    reduce' (ListCont vs [], pos) v = reduce $ ListVal (vs |> v)
    reduce' (ListCont vs (e:es), pos) v = do
        pushCont (ListCont (vs |> v) es, pos)
        eval e
    reduce' (StructCont s x [], pos) v = reduce $ StructVal (Map.insert x v s)
    reduce' (StructCont s x ((y,e):fields), pos) v = do
        pushCont (StructCont (Map.insert x v s) y fields, pos)
        eval e
    --TODO
    -- Structure Tranversal
    reduce' (ExistsCont [], pos) v = reduce TrueVal
    reduce' (ExistsCont (Field x:r), pos) v = do
        it <- liftIO $ v `getField` x
        case it of
            Nothing -> reduce FalseVal
            Just subv -> do
                pushCont (ExistsCont r, pos)
                reduce subv
    reduce' (ExistsCont (Index e:r), pos) v = do
        pushCont (ExistsIndexCont v r, pos)
        eval e
    reduce' (ExistsIndexCont v r, pos) i =
        case v `getIndex` i of
            Left i -> error "unimplemented: raise type error when index is not an int"
            Right Nothing -> reduce FalseVal
            Right (Just subv) -> do
                pushCont (ExistsCont r, pos)
                reduce subv
    reduce' (AccessCont [], pos) v = reduce v
    reduce' (AccessCont (Field x:r), pos) v = do
        it <- liftIO $ v `getField` x
        case it of
            Nothing -> error "unimplemented: raise an access error"
            Just subv -> do
                pushCont (AccessCont r, pos)
                reduce subv
    reduce' (AccessCont (Index e:r), pos) v = do
        pushCont (AccessIndexCont v r, pos)
        eval e
    reduce' (AccessIndexCont v r, pos) i =
        case v `getIndex` i of
            Left i -> error "unimplemented: raise type error when index is not an int"
            Right Nothing -> error "unimplemented: raise an access error"
            Right (Just subv) -> do
                pushCont (AccessCont r, pos)
                reduce subv
    reduce' (UpdateCont [] e', pos) _ = eval e'
    reduce' (UpdateCont [Field x] e', pos) v = do
        pushCont (UpdateFieldToCont v x, pos)
        eval e'
    reduce' (UpdateCont (Field x:r) e', pos) v = do
        it <- liftIO $ v `getField` x
        case it of
            Nothing -> error "unimplemented: raise an access error"
            Just subv -> do
                pushCont (UpdateFieldToCont v x, pos)
                pushCont (UpdateCont r e', pos)
                reduce subv
    reduce' (UpdateCont (Index i:r) e', pos) v = do
        pushCont (UpdateIndexCont v r e', pos)
        eval i
    reduce' (UpdateIndexCont v r e', pos) i = do
        case v `getIndex` i of
            Left i -> error "unimplemented: raise type error when index is not an int"
            Right Nothing -> error "unimplemented: raise an update error"
            Right (Just subv) -> do
                pushCont (UpdateIndexToCont v i, pos)
                pushCont (UpdateCont r e', pos)
                reduce subv
    reduce' (UpdateFieldToCont v x, pos) subv = do
        it <- liftIO $ (v `setField` x) subv
        case it of
            Nothing -> error "unimplemented: raise an update error"
            Just v' -> reduce v'
    reduce' (UpdateIndexToCont v i, pos) subv = do
        case (v `setIndex` i) subv of
            Left i -> error "unimplemented: raise type error when index is not an int"
            Right Nothing -> error "unimplemented: raise an access error"
            Right (Just v') -> reduce v'
    --TODO
    -- Application
    reduce' (OpCont arg, pos) f
        | isApplicative f = do
            pushCont (ApCont f, pos)
            eval arg
        | otherwise = form f arg pos
    reduce' (ApCont f, pos) v = apply f v pos
    -- Other
    reduce' (BlockCont es, pos) _ = seqExprs es pos
    reduce' (ThunkCont cell, _) v = do
        liftIO $ writeIORef cell $ Left v
        reduce v
    reduce' (BindCont p andthen, pos) v = do
        match p v
        case andthen of
            Left v -> reduce v
            Right e -> eval e
    reduce' (MatchCont p v andthen, pos) _ = do
        match p v
        case andthen of
            Left v -> reduce v
            Right e -> eval e
    reduce' k v = error "reduce unimplemented"

form :: Value sysval -> Expr sysval -> SourceLoc -> Machine sysval (Value sysval)
form f@(ClosureVal { opParameters = Left [(env_p, p)] }) e pos = do
    env <- reifyEnv
    swapEnv (opEnv f)
    pushCont (MatchCont p (ExprVal e) (Right $ opBody f), pos)
    pushCont (BindCont env_p (Left UnitVal), pos)
    reduce $ EnvVal env
form f@(ClosureVal { opParameters = Left ((env_p, p):rest) }) e pos = do
    env <- reifyEnv
    swapEnv (opEnv f)
    pushCont (MatchCont p (ExprVal e) (Left $ f { opParameters = Left rest }), pos)
    pushCont (BindCont env_p (Left UnitVal), pos)
    reduce $ EnvVal env
form (PrimForm op n args) next _ | n > 1 = do
    env <- reifyEnv
    reduce (PrimForm op (n-1) (args ++ [(next, env)]))
form (PrimForm Define 1 [p]) e pos = do
    pushCont (BindCont p (Left UnitVal), pos)
    eval e
form (PrimForm Lambda 1 [bind]) body pos = do
    let ps = case bind of
                ((Block ps, _), env) -> flip (,) env <$> ps
                p -> [p]
    env <- liftIO . childEnv =<< reifyEnv
    reduce ClosureVal
        { opParameters = Right ps
        , opEnv = env
        , opBody = body
        }
form (PrimForm Vau 1 [envBind, paramBind]) body pos = do
    let ps = [(envBind, paramBind)]
    env <- liftIO . childEnv =<< reifyEnv
    reduce ClosureVal
        { opParameters = Left ps
        , opEnv = env
        , opBody = body
        }


apply :: Value sysval -> Value sysval -> SourceLoc -> Machine sysval (Value sysval)
apply f@(ClosureVal { opParameters = Right [p] }) v pos = do
    swapEnv (opEnv f)
    pushCont (BindCont p (Right $ opBody f), pos)
    reduce v
apply f@(ClosureVal { opParameters = Right (p:ps) }) v pos = do
    swapEnv (opEnv f)
    pushCont (BindCont p (Left $ f { opParameters = Right ps }), pos)
    reduce v
apply f@(PrimAp _ _ _) (ThunkVal thunk_cell) pos = do
    thunk <- liftIO $ readIORef thunk_cell
    case thunk of
        Left v -> apply f v pos
        Right (e, env) -> do
            pushCont (ApCont f, pos)
            pushCont (ThunkCont thunk_cell, pos)
            swapEnv env
            eval e
apply (PrimAp op n args) next pos | n > 1 = reduce (PrimAp op (n-1) (args ++ [next]))
apply (PrimAp Add 1 [v1]) v2 pos = case (v1, v2) of
    (NumVal a, NumVal b) -> reduce $ NumVal (a+b)
    _ -> error "type error in add unimplemented"


seqExprs :: [Expr sysval] -> SourceLoc -> Machine sysval (Value sysval)
seqExprs [] _ = reduce UnitVal
seqExprs [e] _ = eval e
seqExprs (e:rest) pos = do
    pushCont (BlockCont rest, pos)
    eval e


match :: Pattern sysval -> Value sysval -> Machine sysval ()
match ((Name x, _), _) v = bindCurrentEnv x v
match (dector, env) v = error "unimplemented: match"


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
