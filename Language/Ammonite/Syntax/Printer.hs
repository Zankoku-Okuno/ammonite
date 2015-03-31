module Language.Ammonite.Syntax.Printer
    ( showAST --FIXME use a pretty-printer
    , ReportValue(..)
    ) where

import Data.Ratio
import Data.Symbol
import Data.List (intercalate)
import Data.Foldable (toList)
import qualified Data.Sequence as Seq
import qualified Data.Map as Map
import qualified Data.Text as T
import Control.Applicative
import Language.Ammonite.Syntax.Abstract as AST


class ReportValue a where
    report :: a -> String

instance ReportValue () where
    report () = "()"

instance (ReportValue sysval) => ReportValue (Value sysval) where
    report = showVal


showVal :: ReportValue sysval => Value sysval -> String
showVal UnitVal = "()"
showVal TrueVal = "true"
showVal FalseVal = "false"
showVal (NumVal n) | denom == 1 = show numer
                   | otherwise = show numer ++ "/" ++ show denom
    where (numer, denom) = (numerator n, denominator n)
showVal (ChrVal c) = show c --FIXME don't rely on Haskell
showVal (StrVal txt) = show txt --FIXME don't rely on Haskell
showVal (BytesVal bytes) = show bytes --FIXME don't rely on Haskell
showVal (ListVal xs) = bracks $ commas $ showVal <$> toList xs
showVal (StructVal kvs) = braces $ commas $ kv showVal <$> Map.toList kvs
showVal (RecordVal pos kw)
    | Seq.null pos && Map.null kw = "(,)" 
    | Seq.length pos == 1 && Map.null kw = parens $ showVal (head $ toList pos) ++ ","
    | otherwise = parens $ commas $ (showVal <$> toList pos) ++ (kv showVal <$> Map.toList kw)
showVal (ClosureVal {}) = angles $ "closure" --FIXME show metadata
--showVal (TypeVal _) = undefined -- TODO
--showVal (AbsVal _ _) = undefined -- TODO
--showVal (ModuleVal {}) = undefined -- TODO
--showVal (CueVal _ _) = undefined -- TODO
--showVal (Subcont _) = undefined -- TODO
showVal (EnvVal _) = angles "environment" -- TODO
showVal (ExprVal e) = '`': showAST e
--showVal (ThunkVal) = undefined -- TODO
showVal (PrimForm op arity args) = "<PrimForm: " ++ show op ++ " " ++ show arity ++ ">"
showVal (PrimAp op arity args) = "<PrimAp: " ++ show op ++ intercalate "," (map ((" "++) . showVal) args) ++ ">"
--showVal (SysVal _) = undefined -- TODO
--showVal (SysOp {}) = undefined -- TODO

showAST :: ReportValue sysval => Expr sysval -> String
showAST (Lit x, _) = showVal x
showAST (Name x, _) = unintern x
showAST (StrExpr txt rest, _) = dquote $ showStr txt ++ concatMap showPair rest
    where
    showPair (e, txt) = "$(" ++ showAST e ++ ")" ++ showStr txt
showAST (ListExpr xs, _) = bracks . commas $ showAST <$> xs
showAST (StructExpr kvs, _) = braces . commas $ kv showAST <$> Map.toList kvs
showAST (RecordExpr [] Nothing kw Nothing, _) | Map.null kw = parens ","
showAST (RecordExpr [e] Nothing kw Nothing, _) | Map.null kw =
    parens $ showAST e ++ ","
showAST (RecordExpr pos Nothing kw Nothing, _) | Map.null kw =
    parens . commas $ showAST <$> pos
showAST (RecordExpr pos (Just varpos) kw Nothing, _) | Map.null kw =
    let showPos = ((++", ") . showAST) `concatMap` pos
        showVarpos = showAST varpos ++ "..."
    in parens $ concat [showPos, showVarpos]
showAST (RecordExpr pos varpos kw Nothing, _) =
    let showPos = ((++", ") . showAST) `concatMap` pos
        showVarpos = maybe "" ((++"..., ") . showAST) varpos
        showKw = commas $ kv showAST <$> Map.toList kw
    in parens $ concat [showPos, showVarpos, showKw]
showAST (RecordExpr pos varpos kw (Just varkw), _) =
    let showPos = ((++", ") . showAST) `concatMap` pos
        showVarpos = maybe "" ((++"..., ") . showAST) varpos
        showKw = ((++", ") . kv showAST) `concatMap` Map.toList kw
        showVarkw = showAST varkw ++ ":..."
    in parens $ concat [showPos, showVarpos, showKw, showVarkw]
showAST (Exists e route, _) = showAST e ++ concatMap showRoute route ++ ":?"
showAST (Access e route, _) = showAST e ++ concatMap showRoute route
showAST (Update e route e', _) = showAST e ++ concatMap showRoute route ++ ":=" ++ showAST e'
showAST (Delete e route, _) = showAST e ++ concatMap showRoute route ++ ":="
showAST (QuotedExpr e, _) = ('`':) $ showAST e
showAST (UnquotedExpr e, _) = (',':) $ showAST e
showAST (Ap xs, _) = parens . spaces $ showAST <$> toList xs
showAST (Block stmts, _) = parens . semicolons $ showAST <$> stmts

showRoute :: ReportValue sysval => Route sysval -> String
showRoute (Field x) = '.' : unintern x
showRoute (Index e) = bracks $ showAST e
showRoute (Slice start stop) =
    let showStart = maybe "" ((++" ") . showAST) start
        showStop = maybe "" ((" "++) . showAST) stop
    in bracks $ concat [showStart, "...", showStop]

--FIXME do proper indentation
--FIXME elimiate redundant parens: Ap as element of ListExpr, StructExpr, RecordExpr, Block, interpoaltion into a string


showStr str = T.unpack str --FIXME escape characters that can't appear in a string literal

kv f (k, v) = unintern k ++ ": " ++ f v
parens = ("("++) . (++")")
bracks = ("["++) . (++"]")
braces = ("{"++) . (++"}")
angles = ("<"++) . (++">")
dquote = ("\""++) . (++"\"")
semicolons = intercalate "; "
commas = intercalate ", "
spaces = intercalate " "

