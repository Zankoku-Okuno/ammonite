module Language.Ammonite.Syntax.Sugar
    ( desugar
    ) where

import Control.Applicative
import Control.Monad

import Language.Ammonite.Syntax.Concrete (Syntax)
import Language.Ammonite.Syntax.Abstract (Expr)
import Language.Distfix (Distfix)

import Language.Ammonite.Syntax.Distfix.Defaults
import Language.Ammonite.Syntax.Sugar.Distfix
import Language.Ammonite.Syntax.Sugar.DotExpr
import Language.Ammonite.Syntax.Sugar.AnonPoint
import Language.Ammonite.Syntax.Sugar.ToAbstract


desugar :: [[Distfix Syntax]] -> Syntax -> Either String (Expr sysval)
desugar distfixes = (toAST <$>) . (deDistfix distfixes >=> deDot >=> deAnonPoint)