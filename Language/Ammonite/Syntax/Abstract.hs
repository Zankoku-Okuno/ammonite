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
type DefMetadata = (Maybe SourceLoc, Maybe Text)

type Name = Symbol --FIXME this should be a newtype w/ smart constructors/deconstructors
type Tag = (Gensym, DefMetadata)
data Env sysval = Env
    { envBindings :: IORef (Map Name (Value sysval))
    , envParent :: Maybe (Env sysval)
    }
    deriving (Eq)    


data Value sysval =
    -- Primitive Types
      UnitVal
    | TrueVal | FalseVal
    | NumVal Rational
    | ChrVal Char
    | StrVal Text
    | BytesVal ByteString
    -- Structured Types
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
        { opParameters :: Either [(Pattern sysval, Pattern sysval)] [Pattern sysval]
        , opEnv :: Env sysval
        , opBody :: Expr sysval
        --, opMetadata :: DefMetadata --TODO this metadata really seems to be outside the lambda
        }
    -- First-class Data Types
    | TagVal Tag
    | AbsVal Tag (Value sysval)
    -- First-classs Modules
    | ModuleVal [ModuleItem sysval] DefMetadata
    -- First-class Control
    | CueVal Gensym DefMetadata
    | Within Prim [Value sysval]
    | Subcont (Continuation sysval)
    -- First-class Environments
    | EnvVal (Env sysval)
    | ExprVal (Expr sysval)
    | ThunkVal (ThunkCell sysval)
    
    -- System Types
    | PrimForm
        { primformOp :: Prim
        , primformArity :: Int
        , primformArgs :: [Suspension sysval]
        }
    | PrimAp
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

type ThunkCell sysval = IORef (Either (Value sysval) (Suspension sysval))
type Suspension sysval = (Expr sysval, Env sysval)

data Prim =
      Define | Vau | Lambda | Eval
    | NewEnv | Lazy | Force
    | NewCue | Handle
    | Neg | Floor | Ceil
    | Add | Sub | Mul | Div | Exp | Log
      --TODO universal runtime (exn cue, special forms, halt cue, primitives)
    | DELME_Print
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

type Pattern sysval = (Expr sysval, Env sysval)


type Continuation sysval = [Frame sysval]
data Frame sysval = 
      EnvFrame [([Cont sysval], Env sysval)]
    | Mark (Cont sysval)

type Cont sysval = (ContCore sysval, SourceLoc)
    -- TODO report function and module (contFunc, contMod)
data ContCore sysval =
    -- Compound Data Contruction
      StrCont Text {-hole-} Text [(Expr sysval, Text)]
    | ListCont (Seq (Value sysval)) {-hole-} [Expr sysval]
    | StructCont (Map Name (Value sysval)) Name {-hole-} [(Name, (Expr sysval))]
    | RecordCont --TODO
    | ExprCont --TODO
    -- Structure Tranversal
    | ExistsCont {-hole-} [Route sysval]
    | ExistsIndexCont (Value sysval) {-hole-} [Route sysval]
    | AccessCont {-hole-} [Route sysval]
    | AccessIndexCont (Value sysval) {-hole-} [Route sysval]
    | UpdateCont {-hole-} [Route sysval] (Expr sysval)
    | UpdateIndexCont (Value sysval) {-hole-} [Route sysval] (Expr sysval)
    | UpdateFieldToCont (Value sysval) Name {-hole-}
    | UpdateIndexToCont (Value sysval) (Value sysval) {-hole-}
    -- Application
    | OpCont {-hole-} (Expr sysval)
    | ApCont (Value sysval) {-hole-}
    -- Sequencing
    | BlockCont {-hole-} [Expr sysval]
    -- other
    | ThunkCont (ThunkCell sysval)
    | BindCont (Pattern sysval) {-hole-} (Either (Value sysval) (Expr sysval))
    | MatchCont (Pattern sysval) (Value sysval) (Either (Value sysval) (Expr sysval))
    -- Stack Marks
    | Barrier
    | CueCont Gensym (Value sysval)