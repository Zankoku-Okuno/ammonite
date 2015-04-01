{-#LANGUAGE OverloadedStrings #-}
module Language.Ammonite.Interpreter.RTS
    ( RTS(..)
    , newRTS
    ) where

import Language.Ammonite.Gensym
import Language.Ammonite.Syntax.Abstract

import Control.Applicative
import Control.Monad.State


data RTS sysval = RTS
    { rtsExnCue :: Value sysval
    }

newRTS :: GensymSource -> (RTS sysval, GensymSource)
newRTS source = flip runState source $ do
    exnCue <- gensym
    pure $ RTS
        { rtsExnCue = CueVal exnCue (Nothing, Just "EXN")
        }


gensym :: State GensymSource Gensym
gensym = do
    (it, rest) <- step <$> get
    put rest
    pure it
