{-#LANGUAGE OverloadedStrings #-}
module Language.Ammonite.Interpreter.RTS
    ( RTS(..)
    , newRTS
    , mkExnVal
    , appendTrace
    ) where

import Language.Ammonite.Gensym
import Language.Ammonite.Syntax.Abstract
import Language.Ammonite.Syntax.Printer

import Data.Monoid
import Control.Applicative
import Control.Monad.State

import Data.Symbol
import Data.Maybe
import qualified Data.Sequence as Seq
import qualified Data.Map as Map


data RTS sysval = RTS
    { rtsExnCue :: Value sysval
    , rtsExnType :: Value sysval
    , rtsScopeError :: Value sysval
    , rtsAccessError :: Value sysval
    , rtsUpdateError :: Value sysval
    , rtsTypeError :: Value sysval
    , rtsUnhandledExn :: Value sysval
    }

newRTS :: GensymSource -> (RTS sysval, GensymSource)
newRTS source = flip runState source $ do
    exnCue <- gensym
    exnType <- gensym
    scopeError <- gensym
    accessError <- gensym
    updateError <- gensym
    typeError <- gensym
    unhandledExn <- gensym
    pure $ RTS
        { rtsExnCue = CueVal exnCue (Nothing, Just "EXN")
        , rtsExnType = TagVal (exnType, (Nothing, Just "Exn"))
        ,     rtsScopeError   = TagVal (scopeError, (Nothing, Just "ScopeError"))
        ,     rtsAccessError  = TagVal (accessError, (Nothing, Just "AccessError"))
        ,     rtsUpdateError  = TagVal (updateError, (Nothing, Just "UpdateError"))
        ,     rtsTypeError    = TagVal (typeError, (Nothing, Just "TypeError"))
        ,     rtsUnhandledExn = TagVal (unhandledExn, (Nothing, Just "UnhandledExn"))
        }



mkExnVal :: RTS sysval -> Value sysval -> Value sysval -> Value sysval
mkExnVal rts tag msg =
    let TagVal exnType = rtsExnType rts
    in AbsVal exnType RecordVal
        { rvPos = Seq.fromList [tag]
        , rvKw = Map.fromList [(intern "msg", msg)]
        }
-- TODO once I figure out how to do exists/access/update/delete/call on abstypes, stuff will look nice

unpackExn :: RTS sysval -> Value sysval -> Maybe (Tag, Value sysval)
unpackExn rts (AbsVal (tag, meta) v) =
    let TagVal (exnTag, _) = rtsExnType rts
    in if tag == exnTag
        then Just ((tag, meta), v)
        else Nothing

appendTrace :: (ReportValue sysval) => RTS sysval -> Value sysval -> Continuation sysval -> Maybe (Value sysval)
appendTrace rts it k = do
    (tag, v@(RecordVal { rvKw = kw })) <- unpackExn rts it
    trace <- case Map.lookup (intern "trace") kw of
                    Just (StrVal below) -> Just . StrVal $ stackTrace k <> "\n" <> below
                    Nothing -> Just . StrVal $ stackTrace k
                    Just _ -> Nothing
    Just $ AbsVal tag (v { rvKw = Map.insert (intern "trace") trace kw })




gensym :: State GensymSource Gensym
gensym = do
    (it, rest) <- step <$> get
    put rest
    pure it
