module Language.Ammonite.Syntax.Sugar
    ( desugar
    ) where

import Control.Applicative

import Language.Ammonite.Syntax.Concrete
import Language.Ammonite.Syntax.Abstract

import Language.Ammonite.Syntax.Sugar.AnonPoint


--FIXME other desugaring stages
--FIXME xform to Abstract syntax
desugar = deAnonPoint
