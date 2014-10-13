module Language.Ammonite.Syntax.Concrete 
	( TransUnit
	, Directive
	, Syntax
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
    | Name String
    | AnonPoint
    | Unit
    | Void
    | Number Rational
    | Chr Char
    | TextStr Text [(Syntax, Text)]
    | ByteStr ByteString
    | Quote Syntax
    | Unquote Syntax
    | List [Syntax]
    --TODO? range `[1 ... 3]` and `[1, 3 ... 30]`
    | Struct [(String, Syntax)]
    | Record [Syntax] (Maybe Syntax) [(String, Syntax)] (Maybe Syntax)
    --TODO? comprehensions
    | Block [Syntax]
    | Combine Syntax [Syntax]
    | DotExpr Syntax
    | Subvalue Syntax [Route] SubvalueAction
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




