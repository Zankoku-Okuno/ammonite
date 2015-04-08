{-#LANGUAGE OverloadedStrings #-}
module Language.Ammonite.Interpreter.RTS
    ( RTS(..)
    , newRTS
    , mkExnVal
    ) where

import Language.Ammonite.Gensym
import Language.Ammonite.Syntax.Abstract

import Control.Applicative
import Control.Monad.State

import Data.Symbol
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



mkExnVal :: RTS sysval -> Value sysval -> Value sysval -> Value sysval -> Value sysval
mkExnVal rts tag trace msg =
    let TagVal exnType = rtsExnType rts
    in AbsVal exnType RecordVal
        { rvPos = Seq.fromList [tag]
        , rvKw = Map.fromList
            [ (intern "trace", trace)
            , (intern "msg", msg)
            ]
        }
-- TODO once I figure out how to to exists/access/update/delete on abstypes, stuff will look nice

gensym :: State GensymSource Gensym
gensym = do
    (it, rest) <- step <$> get
    put rest
    pure it
