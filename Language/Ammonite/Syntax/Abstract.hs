module Language.Ammonite.Syntax.Abstract where

import Data.Symbol
import Data.Ratio (Rational)
import Data.Text (Text)
import Data.Sequence (Seq)
import Data.Map (Map)
import Data.IORef (IORef)
import Language.Ammonite.Gensym (Gensym)


type SourceFile = Text
type SourceLine = Int
type SourceLoc = (SourceFile, SourceLine)
type DefMetadata = (SourceLoc, Maybe Name)

type Name = Symbol --FIXME this should be a newtype w/ smart constructors/deconstructors
type TypeTag = (Gensym, DefMetadata)
data Env sysval = Env
    { envBindings :: IORef (Map Name (Value sysval))
    , envParents :: [Env sysval]
    }


data Value sysval =
    -- Structured Types
      UnitVal
    | NumVal Rational
    | StrVal Text
    | ListVal (Seq (Value sysval))
    | StructVal (Map Name (Value sysval))
    | RecordVal
        { rvPos :: Seq (Value sysval)
        , rvKw :: (Map Name (Value sysval))
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
    | ModuleVal
        { modDocstr :: Maybe Text
        , modExport :: Map Name (Value sysval, Maybe Text)
        , modMetadata :: DefMetadata
        }
    -- First-class Control
    | CueVal Gensym DefMetadata
    | Subcont (Continuation sysval)
    -- First-class Environments
    | EnvVal (Env sysval)
    | ExprVal (Expr sysval)
    | ThunkVal --TODO (memoize result: decide on a thread-safe storage mechanism)
    
    -- System Types
    | Prim Prim
    | SysVal sysval
    | SysOp 
        { sysopArity :: Int
        , sysopArguments :: [Value sysval]
        , sysopExecute :: Value sysval -> IO (Either Text (Value sysval))
        }

    --TODO as the language matures, I expect some extensions to become built-in
        -- float vals, big decimal
        -- HandleVal (file handles)
        -- dynamic C library, Ctypes (incl. fixed-width ints and words)
data Prim =
      Lambda
      --TODO universal runtime (exn cue, special forms, halt cue, primitives)

type Expr sysval = (SourceLoc, ExprCore sysval)
data ExprCore sysval =
      Literal (Value sysval)
    | Name Name
    | StrExpr Text [(Expr sysval, Text)]
    | ListExpr (Seq (Expr sysval))
    | StructExpr (Map Name (Expr sysval))
    | RecordExpr
        { rePos :: Seq (Expr sysval)
        , reVarPos :: Expr sysval
        , reKw :: (Map Name (Expr sysval))
        , reVarKw :: Expr sysval
        }
    | Extant (Expr sysval) [SubValue sysval]
    | Access (Expr sysval) [SubValue sysval]
    | Update (Expr sysval) [SubValue sysval] (Expr sysval)
    | Delete (Expr sysval) [SubValue sysval]
    | QuotedExpr (Expr sysval)
    | UnquotedExpr (Expr sysval)
    | Ap [Expr sysval]
data SubValue sysval =
      Field Name
    | Index (Expr sysval)
    | Slice (Maybe (Expr sysval)) (Maybe (Expr sysval))

type Continuation sysval = [ContNode sysval]
data ContNode sysval = Cont
    { cont :: ContCore sysval
    , contEnv :: Env sysval
    , contLoc :: SourceLoc
    -- TODO report function and module (contFunc, contMod)
    }
data ContCore sysval =
      HaltCont
    | StrCont Text {-hole-} Text [(Expr sysval, Text)]
    | ListCont (Seq (Value sysval)) {-hole-} (Seq (Expr sysval))
    | StructCont --TODO
    | RecordCont --TODO
    | ExprCont --TODO
    | OpCont {-hole-} (Expr sysval)
    | ApCont (Value sysval) {-hole-}

