module Language.Ammonite.Syntax.Sugar
    ( desugar
    ) where

import Control.Applicative
import Control.Monad

import Language.Ammonite.Syntax.Concrete (Syntax)
import Language.Ammonite.Syntax.Abstract (Expr)
import Language.Ammonite.Syntax.Distfix

import Language.Ammonite.Syntax.Sugar.Distfix
import Language.Ammonite.Syntax.Sugar.DotExpr
import Language.Ammonite.Syntax.Sugar.AnonPoint
import Language.Ammonite.Syntax.Sugar.ToAbstract


desugar :: Syntax -> Either String (Expr sysval)
desugar = (toAST <$>) . (deDistfix defaultDistfixes >=> deDot >=> deAnonPoint)

defaultDistfixes :: [[Distfix Syntax]]
defaultDistfixes =
    [   [ forceDistfix "|_|" NonAssoc
        , forceDistfix "⌊_⌋" NonAssoc
        , forceDistfix "⌈_⌉" NonAssoc
        ]
    ,   [ forceDistfix "_↑_" RightAssoc
        , forceDistfix "_^_" RightAssoc
        ]
    ,   [ forceDistfix "_*_" LeftAssoc
        , forceDistfix "_/_" LeftAssoc
        , forceDistfix "_%_" LeftAssoc
        ]
    ,   [ forceDistfix "_+_" LeftAssoc
        , forceDistfix "_-_" LeftAssoc
        ]
    ,   [ forceDistfix "_<|_" LeftAssoc
        , forceDistfix "_|>_" RightAssoc
        ]
    ,   [ forceDistfix "_++_" RightAssoc
        , forceDistfix "_--_" RightAssoc
        , forceDistfix "_∪_" LeftAssoc
        , forceDistfix "_∩_" LeftAssoc
        ]
    ,   [ forceDistfix "_∧_" LeftAssoc
        , forceDistfix "_⊼_" LeftAssoc
        
        , forceDistfix "_&&_" LeftAssoc
        , forceDistfix "_!&_" LeftAssoc
        ]
    ,   [ forceDistfix "_∨_" LeftAssoc
        , forceDistfix "_⊽_" LeftAssoc
        
        , forceDistfix "_||_" LeftAssoc
        , forceDistfix "_!|_" LeftAssoc
        ]
    --TODO other logical operators? (implies)
    ,   [ forceDistfix "_=_" NonAssoc
        , forceDistfix "_<_" NonAssoc
        , forceDistfix "_>_" NonAssoc
        , forceDistfix "_≠_" NonAssoc
        , forceDistfix "_≤_" NonAssoc
        , forceDistfix "_≥_" NonAssoc
        , forceDistfix "_≈_" NonAssoc
        , forceDistfix "_!=_" NonAssoc
        , forceDistfix "_<=_" NonAssoc
        , forceDistfix "_>=_" NonAssoc

        , forceDistfix "_∈_" NonAssoc
        , forceDistfix "_∉_" NonAssoc
        , forceDistfix "_∋_" NonAssoc
        , forceDistfix "_∌_" NonAssoc
        , forceDistfix "_⊆_" NonAssoc
        , forceDistfix "_⊇_" NonAssoc
        , forceDistfix "_⊂_" NonAssoc
        , forceDistfix "_⊃_" NonAssoc

        , forceDistfix "_<_<_" NonAssoc
        , forceDistfix "_<_≤_" NonAssoc
        , forceDistfix "_≤_<_" NonAssoc
        , forceDistfix "_≤_≤_" NonAssoc
        --TODO other trinary relational operators?
        ]
    ,   [ forceDistfix "_<$>_" LeftAssoc
        , forceDistfix "_<$$>_" LeftAssoc
        , forceDistfix "_<*>_" LeftAssoc
        , forceDistfix "_<**>_" LeftAssoc
        , forceDistfix "_<$_" LeftAssoc
        , forceDistfix "_$>_" LeftAssoc
        ]
    --TODO arrow operators
    ,   [ forceDistfix "_>=>_" LeftAssoc
        , forceDistfix "_>>=_" LeftAssoc
        , forceDistfix "_=<<_" LeftAssoc
        , forceDistfix "_>>_" LeftAssoc
        , forceDistfix "_<<_" LeftAssoc
        ]
    ,   [ forceDistfix "_$_" RightAssoc
        , forceDistfix "_$$_" RightAssoc
        , forceDistfix "_∘_" RightAssoc

        , forceDistfix "_of_" RightAssoc
        ]
    ,   [ forceDistfix "if_then_else_" RightAssoc
        , forceDistfix "case_of_" RightAssoc
        , forceDistfix "case_of_else_" RightAssoc
        ]
    ,   [ forceDistfix "_→_" RightAssoc
        , forceDistfix "_->_" RightAssoc
        ]
    ,   [ forceDistfix "_is_" NonAssoc
        ]
    ]