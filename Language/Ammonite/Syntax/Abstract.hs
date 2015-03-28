module Language.Ammonite.Syntax.Abstract where

import Data.Symbol
import Data.Ratio (Rational)
import Data.Text (Text)
import Data.ByteString (ByteString)
import Data.Sequence (Seq)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.IORef
import Language.Ammonite.Gensym (Gensym)

import Control.Applicative


type SourceFile = Text
type SourceLine = Int
type SourceLoc = (SourceFile, SourceLine)
type DefMetadata = (SourceLoc, Maybe Name, Text)

type Name = Symbol --FIXME this should be a newtype w/ smart constructors/deconstructors
type TypeTag = (Gensym, DefMetadata)
data Env sysval = Env
    { envBindings :: IORef (Map Name (Value sysval))
    , envParent :: Maybe (Env sysval)
    }
    deriving (Eq)

lookupEnv :: Name -> Env sysval -> IO (Maybe (Value sysval))
lookupEnv x env = do
    bindings <- readIORef $ envBindings env
    case Map.lookup x bindings of
        Just v -> pure $ Just v
        Nothing -> case envParent env of
            Nothing -> pure Nothing
            Just parent -> lookupEnv x parent


data Value sysval =
    -- Structured Types
      UnitVal
    | NumVal Rational
    | ChrVal Char
    | StrVal Text
    | BytesVal ByteString
    | ListVal (Seq (Value sysval))
    | StructVal (Map Name (Value sysval))
    | RecordVal
        { rvPos :: Seq (Value sysval) --FIXME this should be a Map Integer (Value sysval)
        , rvKw :: Map Name (Value sysval)
        }
    -- Reference Types
    -- TODO reference types: single cell vs. contiguous array, thread-safe vs. thread-local vs. non-threaded
    -- TODO ChannelVal (decide on a coherency mechanism)
    -- First-class Functions
    | ClosureVal
        { opIsApplicative :: Bool
        , opArguments :: [Value sysval]
        , opParameters :: [Expr sysval]
        , opEnv :: Env sysval
        , opBody :: Expr sysval
        , opMetadata :: DefMetadata
        }
    -- First-class Data Types
    | TypeVal TypeTag
    | AbsVal TypeTag (Value sysval)
    -- First-classs Modules
    | ModuleVal [ModuleItem sysval] DefMetadata
    -- First-class Control
    | CueVal Gensym DefMetadata
    | Subcont (Continuation sysval)
    -- First-class Environments
    | EnvVal (Env sysval)
    | ExprVal (Expr sysval)
    | ThunkVal --TODO (memoize result: decide on a thread-safe storage mechanism)
                -- I have the idea to force all thunks before primitives, so if passing through a channel is prim, then thunks will never be forced except in the thread in which they were created
    
    -- System Types
    | Prim
        { primOp :: Prim
        , primArity :: Int
        , primArgs :: [Value sysval]
        }
    | SysVal sysval
    | SysOp 
        { sysopArity :: Int
        , sysopArguments :: [Value sysval]
        , sysopExecute :: [Value sysval] -> IO (Either Text (Value sysval))
        }
    --TODO as the language matures, I expect some extensions to become built-in
        -- float vals, big decimal
        -- HandleVal (file handles)
        -- dynamic C library, Ctypes (incl. fixed-width ints and words)
        -- JStypes, call JS functions
        -- JITed functions
data ModuleItem sysval = Data Name (Value sysval)
                       | Docstr Text

data Prim =
      Lambda
    | Neg | Floor | Ceil
    | Add | Sub | Mul | Div | Exp | Log
      --TODO universal runtime (exn cue, special forms, halt cue, primitives)
    deriving (Eq, Show)

type Expr sysval = (ExprCore sysval, SourceLoc)
data ExprCore sysval =
      Lit (Value sysval)
    | Name Name
    | StrExpr Text [(Expr sysval, Text)]
    | ListExpr [Expr sysval]
    | StructExpr (Map Name (Expr sysval))
    | RecordExpr
        { rePos :: [Expr sysval]
        , reVarPos :: Maybe (Expr sysval)
        , reKw :: (Map Name (Expr sysval))
        , reVarKw :: Maybe (Expr sysval)
        }
    | QuotedExpr (Expr sysval)
    | UnquotedExpr (Expr sysval)
    | Exists (Expr sysval) [Route sysval]
    | Access (Expr sysval) [Route sysval]
    | Update (Expr sysval) [Route sysval] (Expr sysval)
    | Delete (Expr sysval) [Route sysval]
    | Ap (Seq (Expr sysval))
    | Block [Expr sysval]
data Route sysval =
      Field Name
    | Index (Expr sysval)
    | Slice (Maybe (Expr sysval)) (Maybe (Expr sysval))


type Continuation sysval = [Cont sysval]
type Cont sysval = (ContCore sysval, SourceLoc)
    -- TODO report function and module (contFunc, contMod)
data ContCore sysval =
    -- Compound Data Contruction
      StrCont Text {-hole-} Text [(Expr sysval, Text)]
    | ListCont (Seq (Value sysval)) {-hole-} (Seq (Expr sysval))
    | StructCont --TODO
    | RecordCont --TODO
    | ExprCont --TODO
    -- Application
    | OpCont {-hole-} (Expr sysval)
    | ApCont (Value sysval) {-hole-}
