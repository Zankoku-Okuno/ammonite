module Language.Ammonite.Syntax.Sugar.Distfix.Defaults
    ( defaultDistfixes
    ) where

import Language.Ammonite.Syntax.Concrete (Syntax, SyntaxCore(..))
import Language.Distfix


parts :: [String] -> [Syntax -> Bool]
parts = map part
    where
    part x (Name y, _) = x == y
    part _ _ = False

defaultDistfixes :: [[Distfix Syntax]]
defaultDistfixes =
    [   map (distfix Closed . parts) [
          ["|", "|"]
        , ["⌊", "⌋"]
        , ["⌈", "⌉"]
        ]
    ,   map (distfix OpenRight . parts) [
          ["↑"]
        
        , ["^"]
        ]
    ,   map (distfix OpenLeft . parts) [
          ["*"], ["/"], ["%"]
        ]
    ,   map (distfix OpenLeft . parts) [
          ["+"], ["-"]
        ]
    ,   [ distfix OpenLeft $ parts ["<|"]
        , distfix OpenRight $ parts ["|>"]
        ]
    ,   [ distfix OpenRight $ parts ["++"]
        , distfix OpenRight $ parts ["--"]
        , distfix OpenLeft $ parts ["∪"]
        , distfix OpenLeft $ parts ["∩"]
        ]
    ,   map (distfix OpenLeft . parts) [ 
          ["∧"], ["⊼"]
        
        , ["&&"], ["!&"]
        ]
    ,   map (distfix OpenLeft . parts) [
          ["∨"], ["⊽"]

        , ["||"], ["!|"]
        ]
    --TODO other logical operators? (implies)
    ,   map (distfix OpenNon . parts) [
          ["="], ["≠"], ["≈"]
        , ["<"], [">"]
        , ["≤"], ["≥"]

        , ["∈"], ["∉"]
        , ["∋"], ["∌"]
        , ["⊆"], ["⊇"], ["⊂"], ["⊃"]

        , ["<", "<"]
        , ["<", "≤"]
        , ["≤", "<"]
        , ["≤", "≤"]
        --TODO other trinary relational operators?

        , ["!="], ["~="]
        , ["<="], [">="]
        ]
    ,   map (distfix OpenLeft . parts) [
          ["<$>"], ["<$$>"]
        , ["<*>"], ["<**>"]
        , ["<$"], ["$>"]
        ]
    --TODO arrow operators
    ,   map (distfix OpenLeft . parts) [
          [">=>"]
        , [">>="], ["=<<"]
        , [">>"], ["<<"]
        ]
    ,   map (distfix OpenRight . parts) [
          ["$"], ["$$"]
        , ["∘"]

        , ["of"]
        ]
    ,   map (distfix HalfOpenRight . parts) [
          ["if", "then", "else"]
        , ["case", "of"]
        , ["case", "of", "else"]
        ]
    ,   map (distfix OpenRight . parts) [
          ["→"]
        
        , ["->"]
        ]
    ,   [ distfix OpenNon $ parts ["is"] ]
    ]