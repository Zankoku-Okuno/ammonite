{-# LANGUAGE OverloadedStrings #-}
module Language.Ammonite.Syntax.Parser
    ( Parser
    , parse
    , file
    , expr
    , strip
    , name
    ) where

import Data.Maybe
import Data.List (intercalate)
import Control.Monad (when, unless)

import Data.Word (Word8)
import Data.Ratio (Rational, (%))
import qualified Data.Text as T
import qualified Data.ByteString as BS

import Text.Luthor hiding (parse)
import Text.Luthor.Indent hiding (parse, many) --FIXME luthor 0.0.2 doesn't export `many`
import Text.Luthor.Syntax

import Language.Ammonite.Syntax.Concrete


type Parser = ParsecI String ()
parse :: Parser a -> SourceName -> String -> Either ParseError a
parse parser = runParserI (allInput parser) (DontMix " ") [ilws] ()


file :: Parser TransUnit
file = do
    pos0 <- getPosition
    optional_ $ string "#!" *> anyChar `manyThru` lineBreak
    directives <- many $ do
        string "#(!" *> ws0
        anyChar `manyThru` (ws0 *> string ")#" *> lineBreak)
    syntax <- lineExpr `sepAroundBy` nextline
    pure $ (,) directives $ case syntax of
        [] -> (Unit, pos0)
        [e] -> e
        es -> (Block es, pos0)

expr :: Parser Syntax
expr = _expr False
lineExpr :: Parser Syntax
lineExpr = _expr True
_expr :: Bool -> Parser Syntax
_expr inline = do
    (stmts, pos) <- withPosition $
        applyExpr inline coreExpr `sepBy1` sepSemicolon
    return $ case stmts of
        [e] -> e
        _ -> (Block stmts, pos)

applyExpr :: Bool -> Parser Syntax -> Parser Syntax
applyExpr inline coreExpr = withPosition $ do
    es <- anExpr coreExpr `sepBy1` (if inline then ilws else ws)
    return $ case es of
        [e] -> fst e
        args -> Combine args

anExpr :: Parser Syntax -> Parser Syntax
anExpr coreExpr = withPosition $ choice
        [ Unquote <$ char ',' <*> anotherExpr
        , Quote <$ char '`' <*> anotherExpr
        , DotExpr <$ dot <*> anotherExpr
        , fst <$> do
            e0 <- coreExpr
            option e0 (subValue e0)
        ]
    where
    anotherExpr = anExpr coreExpr

coreExpr :: Parser Syntax
coreExpr = choice
    [ atom
    , compound
    , indentExpr
    , parenExpr
    ]

parenExpr :: Parser Syntax
parenExpr = withPosition $ Parens <$> parens expr


atom :: Parser Syntax
atom = expect "atom" $ withPosition $
    choice [
          Unit <$ string "()"
        , numLit
        , chrLit
        , strLit
        , rawStrLit
        , heredoc
        , bytestr
        , Name <$> name
        , AnonPoint <$ char '_'
        ]

numLit :: Parser SyntaxCore
numLit = Number <$> choice [rational, scientific, (%1) <$> integer]

chrLit :: Parser SyntaxCore
chrLit = between2 sq $ Chr <$> choice
    [ letterEsc cEscapes
    , decimalEsc
    , asciiEsc
    , uniEsc
    , aChar $ uniPrintMinus (charClass "\\'\"")
    ]

strLit :: Parser SyntaxCore
strLit = between2 dq $ do
        first <- option "" strPart
        rest <- many $ exprPart <$$> (,) <*> strPart
        return $ TextStr first rest
    where
    strPart = T.pack . catMaybes <$> manyOf
        [ Nothing <$ string "\\&"
        , Nothing <$ bsnlwsbs
        , Just <$> char '$' `notFollowedBy` char '('
        , Just <$> letterEsc (('$', '$') : ('s', ' ') : cEscapes)
        , Just <$> decimalEsc
        , Just <$> asciiEsc
        , Just <$> uniEsc
        , Just <$> aChar (uniPrintMinus $ charClass "\'\"\\$")
        ]
    exprPart = char '$' *> parens expr


rawStrLit :: Parser SyntaxCore
rawStrLit = do
    char 'r'
    content <- T.pack <$> sqString
    return $ TextStr content []

heredoc :: Parser SyntaxCore
heredoc = do
    count 3 sq
    premarker <- between2 (lws0) $ many1 (aChar uniPrint) <* newline
    let (strip, marker) = if head premarker == '-' then (True, tail premarker) else (False, premarker)
        endline = void $ do
            string marker
            between2 (lws0) (count 3 sq)
            lookAhead lineBreak
        preline = if strip then lws0 else pure ()
        line = preline *> (aChar uniPrint `manyThru` newline)
    lines <- line `manyThru` endline
    let content = T.intercalate "\n" (T.pack <$> lines)
    return $ TextStr content []

bytestr :: Parser SyntaxCore
bytestr = between (char 'b' *> sq) sq $
    ByteStr . BS.pack <$> hexOctet `sepAroundBy` ws0
{-
Conventions are:
    use dash to separate words in a multi-word identifer
    use ! at the end of an itentifier to signal that it is not referentially transparent (if the effects are encapsulated, it doesn't count)
    use ? at the end of an identifier to signal that it is a predicate (e.g. gt? bool?) (no need to but an `is` prefix, so: `foo?` in lieu of `isFoo?`)
    use ? at the beginning of an identifier to signal that it might be a good value but it might be an error signal
    use $ at the beginning of an identifier to signal that it is assignable (reference cell, fluidvar, mutable array, mutable eobject, &c)
    use / to separate an identifier from its arity, provided arity is significant
    use asterisk at the begining or end of an identifier to signal visibility/magic:
        one leading asterisk for private
        two leading and trailing asterisks for language-defined magic
        two leading asterisks, but no trailing asterisks for implementation-defined magic
    variations of functions have funcname_flags. for example, if `cp` copies a single file no clobber, `cp_f` copies a single file with clobber, `cp_r` copies a tree no clobber, `cp_rf` copies a tree with clobber
        distfixes use underscores where slots go (this is actually a rule)
-}
name :: Parser String
name = expect "identifier" $ choice
        [ uniIdMinus rChr `many1Not` rChr1
        , char '-' <$$> (:) <*> option [] (uniIdMinus rChr `many1Not` rChr2)
        , char '+' <$$> (:) <*> option [] (uniIdMinus rChr `many1Not` rChr2)
        , char '_' <$$> (:) <*> many1 (aChar $ uniIdMinus rChr)
        ]
    where
    rChr = charClass "#\\\"`()[]{}:;.,"
    rChr1 = charClass "-+'_0-9"
    rChr2 = charClass "0-9"

compound :: Parser Syntax
compound = withPosition $ choice [list, struct, record]
    where
    list = brackets $ do
        leadingComma <- optional sepComma
        xs <- expr `sepBy` sepComma
        when (notNull xs) $ optional_ sepComma
        return $ List xs
    struct = braces $ do
        leadingComma <- optional sepComma
        kvs <- kvPair `sepAroundBy` sepComma
        when (notNull kvs) $ optional_ sepComma
        return $ Struct kvs
    record = parens $ do
        leadingComma <- optional sepComma
        positionals <- exprSep `sepBy` sepComma
        vararg <- optional $ do
            when (notNull positionals) sepComma
            expr <* ellipsis3 <* sepc
        keywords <- option [] $ do
            when (isJust vararg) $ optional_ sepComma
            when (isNothing vararg && notNull positionals) $ sepComma
            kvPair `sepBy1` sepComma
        varkwarg <- optional $ do
            when (notNull keywords) sepComma
            when (null keywords && isJust vararg) $ optional_ sepComma
            when (null keywords && isNothing vararg && notNull positionals) sepComma
            expr <* colon <* ellipsis3 <* sepc
        case undefined of
            _ | null positionals && isNothing vararg && null keywords && isNothing varkwarg ->
                when (isNothing leadingComma) sepComma
            _ | length positionals == 1 && isNothing vararg && null keywords && isNothing varkwarg ->
                if isNothing leadingComma
                    then sepComma
                    else optional_ sepComma
            _ | otherwise -> optional_ sepComma
        return $ Record positionals vararg keywords varkwarg
    exprSep = expr <* sepc
    kvPair = name <* (colon >> ws) <$$> (,) <*> expr
    notNull = not . null

subValue :: Syntax -> Parser Syntax
subValue e0 = withPosition $ do
    route <- many1 $ attr <||> index <||> slice
    let exists = Exists <$ string ":?"
        access = pure Access
        update = Update <$> (string ":=" *> anExpr coreExpr)
        delete = Delete <$ string ":="
    Subvalue e0 route <$> choice [exists, update, delete, access]
    where
    attr = Field <$ dot <*> name
    index = Index <$> brackets expr
    slice = brackets $ Slice <$>
        optional (expr <* ws) <* (ellipsis3 <* sep) <*> optional expr
        --FIXME have all of `[e ... e]`, `[e ...]` and `[... e]`, even `[...]` though it's useless
    
indentExpr :: Parser Syntax
indentExpr = withPosition $ do
        colon
        operator <- optional $ anExpr $
            choice [atom, compound, parenExpr]
        preblock <- withPosition $ between indent dedent $
            lineExpr `sepBy1` nextline
        let block = case preblock of 
                ([e], _) -> e
                (stmts, pos) -> (Block stmts, pos)
        return $ case operator of
            Nothing -> fst block
            Just operator -> Combine [operator, block]
--in postorder: 
    --group from before colon to dedent,
    --group after the expresssion immediately following colon (if any) and at dedent
    --delete colon
    --group around each nextline-separated expression
    --place semicolon at each nextline



ws :: Parser ()
ws = many1_ $ expect "whitespace" $ choice [
      void lws
    , newline
    , void $ nestingComment "#(" ")#"
    , void $ lineComment "#"
    ]

ilws :: Parser ()
ilws = many1_ $ expect "in-line whitespace" $ choice [
      void lws
    , void $ nestingComment "#(" ")#"
    , void $ lineComment "#"
    ]

ws0 :: Parser ()
ws0 = optional_ ws

lws0 :: Parser ()
lws0 = optional_ lws

parens = inParens . between2 ws0
brackets = inBrackets . between2 ws0
braces = inBraces . between2 ws0


sep :: Parser ()
sep = ws <||> lookAhead_ (oneOf ")]}")

sepc :: Parser ()
sepc = ws <||> lookAhead_ (oneOf ",)]}")

sepComma :: Parser ()
sepComma = ws0 *> comma *> sep

sepSemicolon :: Parser ()
sepSemicolon = ws0 *> semicolon *> sep

strip :: Parser a -> Parser a
strip = between2 ws0