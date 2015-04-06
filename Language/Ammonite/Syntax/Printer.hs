{-#LANGUAGE OverloadedStrings #-}
module Language.Ammonite.Syntax.Printer
    ( showAST --FIXME use a pretty-printer
    , ReportValue(..)
    , stackTrace
    ) where

import Data.Ratio
import Data.Symbol
import Data.List (intercalate)
import Data.Foldable (toList)
import qualified Data.Sequence as Seq
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as T
import Data.Monoid
import Control.Applicative
import Language.Ammonite.Syntax.Abstract as AST


class ReportValue a where
    report :: a -> Text

instance ReportValue () where
    report () = "()"
instance ReportValue Symbol where
    report = T.pack . unintern

instance (ReportValue sysval) => ReportValue (Value sysval) where
    report = showVal

instance (ReportValue sysval) => ReportValue (ContCore sysval) where
    report = showCont


showVal :: ReportValue sysval => Value sysval -> Text
showVal UnitVal = "()"
showVal TrueVal = "true"
showVal FalseVal = "false"
showVal (NumVal n) | denom == 1 = T.pack $ show numer
                   | otherwise = (T.pack . show) numer <> "/" <> (T.pack . show) denom
    where (numer, denom) = (numerator n, denominator n)
showVal (ChrVal c) = T.pack $ show c --FIXME don't rely on Haskell
showVal (StrVal txt) = T.pack $ show txt --FIXME don't rely on Haskell
showVal (BytesVal bytes) = T.pack $ show bytes --FIXME don't rely on Haskell
showVal (ListVal xs) = bracks $ commas $ showVal <$> toList xs
showVal (StructVal kvs) = braces $ commas $ kv showVal <$> Map.toList kvs
showVal (RecordVal pos kw)
    | Seq.null pos && Map.null kw = "(,)" 
    | Seq.length pos == 1 && Map.null kw = parens $ showVal (head $ toList pos) <> ","
    | otherwise = parens $ commas $ (showVal <$> toList pos) <> (kv showVal <$> Map.toList kw)
showVal (ClosureVal {}) = angles $ "closure" --FIXME show metadata
showVal (TypeVal _) = error "unimplemented: show TypeVal"
showVal (AbsVal (_, (_, Nothing)) _) = angles $ "AbsType"
showVal (AbsVal (_, (_, Just name)) _) = angles $ "AbsType: " <> name
showVal (ModuleVal {}) = error "unimplemented: show module"
showVal (CueVal _ (_, Nothing)) = angles $ "Cue"
showVal (CueVal _ (_, Just name)) = angles $ "Cue: " <> name
showVal (Subcont _) = error "unimplemented: show subcont"
showVal (EnvVal _) = angles "environment" -- TODO
showVal (ExprVal e) = "`" <> showAST e
showVal (ThunkVal _) = error "unimplemented: show thunk"
showVal (PrimForm op arity args) = "<PrimForm: " <> (T.pack . show) op <> " " <> (T.pack . show) arity <> ">"
showVal (PrimAp op arity args) = "<PrimAp: " <> (T.pack . show) op <> T.intercalate "," (map ((" "<>) . showVal) args) <> ">"
showVal (SysVal _) = error "unimplemented: show sysval"
showVal (SysOp {}) = error "unimplemented: show sysop"


showCont :: ReportValue sysval => ContCore sysval -> Text
showCont (StrCont before next rest) = "TODO StrCont"
showCont (ListCont before rest) = "TODO ListCont"
showCont (StructCont before x rest) = "TODO StructCont"
showCont (RecordCont) = "TODO RecordCont"
showCont (ExprCont) = "TODO ExprCont"
showCont (ExistsCont route) = "_" <> (T.concat . map showRoute) route <> ":?"
showCont (ExistsIndexCont v route) = showVal v <> "[_]" <> (T.concat . map showRoute) route <> ":?"
showCont (AccessCont route) = "_" <> (T.concat . map showRoute) route
showCont (AccessIndexCont v route) = showVal v <> "[_]" <> (T.concat . map showRoute) route
showCont (UpdateCont route new) = "_" <> (T.concat . map showRoute) route <> ":=" <> showAST new
showCont (UpdateIndexCont v route new) = showVal v <> "[_]" <> (T.concat . map showRoute) route <> ":=" <> showAST new
showCont (UpdateFieldToCont v x) = showVal v <> "." <> report x <> ":=_"
showCont (UpdateIndexToCont v i) = showVal v <> "[" <> showVal i <> "]:=_"
showCont (OpCont e) = "(_ " <> showAST e <> ")"
showCont (ApCont v) = "(" <> showVal v <> " _)"
showCont (BlockCont es) = "(_; " <> T.intercalate "; " (map showAST es) <> ")"
showCont (ThunkCont _) = "update thunk"
showCont (BindCont p andthen) = "TODO BindCont"
showCont (MatchCont p v andthen) = "TODO MatchCont"
showCont Barrier = "TODO Barrier"
showCont (CueCont cue handler) = "TODO CueCont"


showAST :: ReportValue sysval => Expr sysval -> Text
showAST (Lit x, _) = showVal x
showAST (Name x, _) = report x
showAST (StrExpr txt rest, _) = dquote $ showStr txt <> (T.concat . map showPair) rest
    where
    showPair (e, txt) = "$(" <> showAST e <> ")" <> showStr txt
showAST (ListExpr xs, _) = bracks . commas $ showAST <$> xs
showAST (StructExpr kvs, _) = braces . commas $ kv showAST <$> Map.toList kvs
showAST (RecordExpr [] Nothing kw Nothing, _) | Map.null kw = parens ","
showAST (RecordExpr [e] Nothing kw Nothing, _) | Map.null kw =
    parens $ showAST e <> ","
showAST (RecordExpr pos Nothing kw Nothing, _) | Map.null kw =
    parens . commas $ showAST <$> pos
showAST (RecordExpr pos (Just varpos) kw Nothing, _) | Map.null kw =
    let showPos = (T.concat . map ((<>", ") . showAST)) pos
        showVarpos = showAST varpos <> "..."
    in parens $ T.concat [showPos, showVarpos]
showAST (RecordExpr pos varpos kw Nothing, _) =
    let showPos = (T.concat . map ((<>", ") . showAST)) pos
        showVarpos = maybe "" ((<>"..., ") . showAST) varpos
        showKw = commas $ kv showAST <$> Map.toList kw
    in parens $ T.concat [showPos, showVarpos, showKw]
showAST (RecordExpr pos varpos kw (Just varkw), _) =
    let showPos = (T.concat . map ((<>", ") . showAST)) pos
        showVarpos = maybe "" ((<>"..., ") . showAST) varpos
        showKw = (T.concat . map ((<>", ") . kv showAST)) (Map.toList kw)
        showVarkw = showAST varkw <> ":..."
    in parens $ T.concat [showPos, showVarpos, showKw, showVarkw]
showAST (Exists e route, _) = showAST e <> (T.concat . map showRoute) route <> ":?"
showAST (Access e route, _) = showAST e <> (T.concat . map showRoute) route
showAST (Update e route e', _) = showAST e <> (T.concat . map showRoute) route <> ":=" <> showAST e'
showAST (Delete e route, _) = showAST e <> (T.concat . map showRoute) route <> ":="
showAST (QuotedExpr e, _) = ("`"<>) $ showAST e
showAST (UnquotedExpr e, _) = (","<>) $ showAST e
showAST (Ap xs, _) = parens . spaces $ showAST <$> toList xs
showAST (Block stmts, _) = parens . semicolons $ showAST <$> stmts

showRoute :: ReportValue sysval => Route sysval -> Text
showRoute (Field x) = "." <> report x
showRoute (Index e) = bracks $ showAST e
showRoute (Slice start stop) =
    let showStart = maybe "" ((<>" ") . showAST) start
        showStop = maybe "" ((" "<>) . showAST) stop
    in bracks $ mconcat [showStart, "...", showStop]

--FIXME do proper indentation
--FIXME elimiate redundant parens: Ap as element of ListExpr, StructExpr, RecordExpr, Block, interpoaltion into a string
--FIXME add required parens, such as `({x: 1}.x:=3).x`


stackTrace :: ReportValue sysval => Continuation sysval -> Text
stackTrace = T.intercalate "\n" . reverse . map goFrame
    where
    goFrame (EnvFrame sections) = T.intercalate "\n" . reverse $ map (goEnv . fst) sections
    goFrame (Mark cont) = goPos cont
    goEnv = T.intercalate "\n" . reverse . map goPos
    goPos (core, (file, line)) =
           "At line " <> (T.pack . show) line
        <> " in " <> (T.pack . show) file <> ":\n"
        <> "    " <> showCont core



showStr str = str --FIXME escape characters that can't appear in a string literal

kv :: (a -> Text) -> (Symbol, a) -> Text
kv f (k, v) = report k <> ": " <> f v
parens :: Text -> Text
parens = ("("<>) . (<>")")
bracks :: Text -> Text
bracks = ("["<>) . (<>"]")
braces :: Text -> Text
braces = ("{"<>) . (<>"}")
angles :: Text -> Text
angles = ("<"<>) . (<>">")
dquote :: Text -> Text
dquote = ("\""<>) . (<>"\"")
semicolons = T.intercalate "; "
commas = T.intercalate ", "
spaces = T.intercalate " "

