module Language.Ammonite.Syntax.Sugar
    ( desugar
    ) where

import Control.Applicative
import Control.Monad

import Language.Ammonite.Syntax.Concrete (Syntax)
import Language.Ammonite.Syntax.Abstract (Expr)

import Language.Ammonite.Syntax.Sugar.DotExpr
import Language.Ammonite.Syntax.Sugar.AnonPoint
import Language.Ammonite.Syntax.Sugar.ToAbstract


deDistfix = pure --TODO


desugar :: Syntax -> Either String (Expr sysval)
desugar = (toAST <$>) . (deDistfix >=> deDot >=> deAnonPoint)
