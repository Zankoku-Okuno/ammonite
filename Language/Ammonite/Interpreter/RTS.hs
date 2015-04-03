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
    , rtsScopeExn :: Value sysval
    }

newRTS :: GensymSource -> (RTS sysval, GensymSource)
newRTS source = flip runState source $ do
    exnCue <- gensym
    exnType <- gensym
    scopeExn <- gensym
    pure $ RTS
        { rtsExnCue = CueVal exnCue (Nothing, Just "EXN")
        , rtsExnType = TypeVal (exnType, (Nothing, Just "Exn"))
        , rtsScopeExn = TypeVal (scopeExn, (Nothing, Just "ScopeExn"))
        }



mkExnVal :: RTS sysval -> Value sysval -> Value sysval -> Value sysval -> Value sysval
mkExnVal rts tag trace msg =
    let TypeVal exnType = rtsExnType rts
    in AbsVal exnType RecordVal
        { rvPos = Seq.fromList [tag] -- TODO ...and gensyms for each subtype of builtin exception
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
