module Language.Ammonite.Syntax.Concrete 
    ( TransUnit
    , Directive
    , Syntax
    , SourcePos
    , SyntaxCore(..)
    , Route(..)
    , SubvalueAction(..)
    ) where

import Data.Ratio (Rational, (%))
import Data.Text (Text)
import Data.ByteString (ByteString)
import Text.Luthor (SourcePos)


type TransUnit = ([Directive], [Syntax])
type Directive = String

type Syntax = (SyntaxCore, SourcePos)
data SyntaxCore =
      Parens Syntax
    | AnonLambda [Syntax] Syntax
    | Name String
    | AnonPoint
    | Unit
    | Number Rational
    | Chr Char
    | TextStr Text [(Syntax, Text)]
    | ByteStr ByteString
    | List [Syntax]
    | Struct [(String, Syntax)]
    | Record [Syntax] (Maybe Syntax) [(String, Syntax)] (Maybe Syntax)
    | Subvalue Syntax [Route] SubvalueAction
    | Quote Syntax
    | Unquote Syntax
    | Combine [Syntax]
    | DotExpr Syntax
    | Block [Syntax]
    deriving (Eq, Show)

data Route =
      Field String
    | Index Syntax
    | Slice (Maybe Syntax) (Maybe Syntax)
    deriving (Eq, Show)

data SubvalueAction =
      Exists
    | Access
    | Update Syntax
    | Delete
    deriving (Eq, Show)




