{-#LANGUAGE ViewPatterns #-}
module Language.Ammonite.Interpreter.Evaluator (eval) where

import Data.IORef
import Data.Sequence (viewl, viewr, ViewL(..), ViewR(..), (<|), (|>))
import qualified Data.Sequence as Seq
import qualified Data.Map as Map

import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class

import Language.Ammonite.Syntax.Abstract
import Language.Ammonite.Interpreter.Data
import Language.Ammonite.Interpreter.Machine
import Language.Ammonite.Interpreter.Environment
import Language.Ammonite.Syntax.Printer


eval :: Expr sysval -> Env sysval -> IO (Result sysval)
eval prog env = Good <$> runMachine (elaborate prog) env

elaborate :: Expr sysval -> Machine sysval (Value sysval)
elaborate (Lit val, _) = reduce val
elaborate (Name x, _) = do
    m_val <- lookupCurrentEnv x
    case m_val of
        Nothing -> error $ "unimplemented: scope error (" ++ show x ++ ")"
        Just val -> reduce val
elaborate (ListExpr [], _) = reduce $ ListVal Seq.empty
elaborate (ListExpr (e:es), pos) = do
    pushCont (ListCont Seq.empty es, pos)
    elaborate e
elaborate (StructExpr (Map.toList -> []), _) = reduce $ StructVal Map.empty
elaborate (StructExpr (Map.toList -> (x,e):fields), pos) = do
    pushCont (StructCont Map.empty x fields, pos)
    elaborate e
--TODO
elaborate (Exists e r, pos) = do
    pushCont (ExistsCont r, pos)
    elaborate e
elaborate (Access e r, pos) = do
    pushCont (AccessCont r, pos)
    elaborate e
elaborate (Update e r e', pos) = do
    pushCont (UpdateCont r e', pos)
    elaborate e
--TODO
elaborate (Ap (viewl -> f :< args), pos) = do
        pushArgs args
        elaborate f
    where
    pushArgs (viewr -> EmptyR) = pure ()
    pushArgs (viewr -> rest :> lastArg) = do
        pushCont (OpCont lastArg, pos)
        pushArgs rest
elaborate (Block es, pos) = seqExprs es pos
elaborate _ = error $ "elaborate unimplemented"


reduce :: Value sysval -> Machine sysval (Value sysval)
reduce v = maybe (pure v) (flip reduce' v) =<< popCont
    where
    -- Compound Data Contruction
    reduce' :: Cont sysval -> Value sysval -> Machine sysval (Value sysval)
    reduce' (ListCont vs [], pos) v = reduce $ ListVal (vs |> v)
    reduce' (ListCont vs (e:es), pos) v = do
        pushCont (ListCont (vs |> v) es, pos)
        elaborate e
    reduce' (StructCont s x [], pos) v = reduce $ StructVal (Map.insert x v s)
    reduce' (StructCont s x ((y,e):fields), pos) v = do
        pushCont (StructCont (Map.insert x v s) y fields, pos)
        elaborate e
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
        elaborate e
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
        elaborate e
    reduce' (AccessIndexCont v r, pos) i =
        case v `getIndex` i of
            Left i -> error "unimplemented: raise type error when index is not an int"
            Right Nothing -> error "unimplemented: raise an access error"
            Right (Just subv) -> do
                pushCont (AccessCont r, pos)
                reduce subv
    reduce' (UpdateCont [] e', pos) _ = elaborate e'
    reduce' (UpdateCont [Field x] e', pos) v = do
        pushCont (UpdateFieldToCont v x, pos)
        elaborate e'
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
        elaborate i
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
            elaborate arg
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
            Right e -> elaborate e
    reduce' (MatchCont p v andthen, pos) _ = do
        match p v
        case andthen of
            Left v -> reduce v
            Right e -> elaborate e
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
form (PrimForm op 1 args) next pos = do
    env <- reifyEnv
    primForm op (args ++ [(next, env)]) pos
form (PrimForm op n args) next _ | n > 1 = do
    env <- reifyEnv
    reduce (PrimForm op (n-1) (args ++ [(next, env)]))


apply :: Value sysval -> Value sysval -> SourceLoc -> Machine sysval (Value sysval)
apply f@(ClosureVal { opParameters = Right [p] }) v pos = do
    env <- liftIO $ copyEnv (opEnv f)
    swapEnv env
    pushCont (BindCont p (Right $ opBody f), pos)
    reduce v
apply f@(ClosureVal { opParameters = Right (p:ps) }) v pos = do
    env <- liftIO $ copyEnv (opEnv f)
    swapEnv env
    pushCont (BindCont p (Left $ f { opEnv = env, opParameters = Right ps }), pos)
    reduce v
apply f@(PrimAp _ _ _) (ThunkVal thunk_cell) pos = do
    thunk <- liftIO $ readIORef thunk_cell
    case thunk of
        Left v -> apply f v pos
        Right (e, env) -> do
            pushCont (ApCont f, pos)
            pushCont (ThunkCont thunk_cell, pos)
            swapEnv env
            elaborate e
apply (PrimAp op 0 args) next pos = do
    applyPrim op (args ++ [next]) pos
apply (PrimAp op n args) next pos | n > 1 =
    reduce (PrimAp op (n-1) (args ++ [next]))


match :: Pattern sysval -> Value sysval -> Machine sysval ()
match ((Name x, _), _) v = bindCurrentEnv x v
match (dector, env) v = error "unimplemented: match"



primForm :: Prim -> [(Expr sysval, Env sysval)] -> SourceLoc -> Machine sysval (Value sysval)
primForm Define [p, (e, _)] pos = do
    pushCont (BindCont p (Left UnitVal), pos)
    elaborate e
primForm Lambda [bind, (body, env)] _ = do
    let ps = case bind of
                ((Block ps, _), pat_env) -> flip (,) pat_env <$> ps
                p -> [p]
    reduce ClosureVal
        { opParameters = Right ps
        , opEnv = env
        , opBody = body
        }
primForm Vau [envBind, paramBind, (body, env)] _ = do
    let ps = [(envBind, paramBind)]
    reduce ClosureVal
        { opParameters = Left ps
        , opEnv = env
        , opBody = body
        }


applyPrim :: Prim -> [Value sysval] -> SourceLoc -> Machine sysval (Value sysval) 

applyPrim Eval [ExprVal body, EnvVal env] _ = do
    swapEnv env
    elaborate body
applyPrim Eval _ _ = error "unimplemented: type error in elaborate"

applyPrim NewCue [UnitVal] pos = do
    c <- newCue
    reduce $ CueVal c (pos, Nothing)
applyPrim NewCue [StrVal desc] pos = do
    c <- newCue
    reduce $ CueVal c (pos, Just desc)
applyPrim NewCue _ pos = error "type error in newCue unimplemented"

applyPrim Handle [CueVal cue meta, handler] _ =
    --TODO I'd like to check that the handler is actually callable
    error "Handle primitive unimplemented" --probably should return a PrimOp of 1 arg
    --reduce (PrimOp PushCue cue handler, pos)
applyPrim Handle _ pos = error "type error in newCue unimplemented"

applyPrim Add [NumVal a, NumVal b] _ =
    reduce $ NumVal (a+b)
applyPrim Add _ pos = error "type error in add unimplemented"

applyPrim NewEnv [StructVal s, UnitVal] _ = do
    cell <- liftIO $ newIORef s
    reduce $ EnvVal $ Env cell Nothing
applyPrim NewEnv [StructVal s, EnvVal env] _ = do
    cell <- liftIO $ newIORef s
    reduce $ EnvVal $ Env cell (Just env)
applyPrim NewEnv _ pos = error "type error in NewEnv unimplemented"


seqExprs :: [Expr sysval] -> SourceLoc -> Machine sysval (Value sysval)
seqExprs [] _ = reduce UnitVal
seqExprs [e] _ = elaborate e
seqExprs (e:rest) pos = do
    pushCont (BlockCont rest, pos)
    elaborate e




