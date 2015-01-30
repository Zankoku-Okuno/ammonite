{-# LANGUAGE ScopedTypeVariables #-}
module Language.Ammonite.Load 
    ( AmmoniteFile(..)
    , FileExpr
    , FileVal
    , LoadError(..)
    , Loader
    , newLoader
    , parseFile
    , evalFile
    ) where

import System.IO
import Control.Exception (Exception, handleJust)
import System.IO.Error (IOError, isAlreadyInUseError, isDoesNotExistError, isPermissionError)
import qualified System.File.Load as L

import Control.Applicative
import Control.Monad
import Control.Monad.Trans.Either

import Language.Distfix
import Text.Parsec (ParseError)

import Language.Ammonite.Syntax.Concrete
import Language.Ammonite.Syntax.Sugar
import Language.Ammonite.Syntax.Abstract
import Language.Ammonite.Syntax.Parser

import Language.Ammonite.Syntax.Sugar.Distfix.Parser
import Language.Ammonite.Syntax.Sugar.Distfix.Defaults


type Loader sysval = (L.Loader (FileExpr sysval), L.Loader (FileVal sysval))

type FileExpr sysval = Either LoadError (AmmoniteFile sysval)
type FileVal sysval = Either LoadError (Value sysval)


data AmmoniteFile sysval = AF {
      afProgram :: Expr sysval
    , afDistfixGroups :: [(GroupName, [[Distfix Syntax]])]
    , afDistfixUse :: [[Distfix Syntax]] -- use the default set if none specified
}

data LoadError =
      SystemError FilePath -- ^When we can't even open the requested file.
    | SyntaxError ParseError -- ^When the file cannot be parsed.
    | DirectiveError FilePath String -- ^When any directive handling has a problem.
    | DesugarError FilePath String -- ^When there is an error desugaring the syntax.
    | ImportError String -- ^When there is an error evaluating the file body.

newLoader :: IO (Loader sysval)
newLoader = (,) <$> L.newLoader <*> L.newLoader


parseFile :: Loader sysval -> FilePath -> IO (FileExpr sysval)
parseFile (syntax, _) = L.load syntax go
    where
    go path = handleJust handler pure $ withFile path ReadMode $ \fp -> do
        input <- hGetContents fp
        runEitherT $ do
            (directives, concrete) <- lmapHoistEither SyntaxError $ parse file path input
            --TODO get distfix directives, send to loadDistfixes
            (groups, uses) <- loadDistfixes path []
            --TODO if any directives left over, die
            abstract <- lmapHoistEither (DesugarError path) $ desugar uses concrete
            pure $ AF {
                  afProgram = abstract
                , afDistfixGroups = groups
                , afDistfixUse = uses
                }
        where
        handler e | isAlreadyInUseError e
                  || isDoesNotExistError e
                  || isPermissionError e = Just . Left $ SystemError path
                  | otherwise = Nothing

evalFile :: Loader sysval -> FilePath -> IO (FileVal sysval)
evalFile loader@(syntax, val) path = do
    program <- (afProgram <$>) <$> parseFile loader path
    error "TODO Language.Ammonite.Load.evalFile"


loadDistfixes :: FilePath -> [String] -> EitherT LoadError IO ([(GroupName, [[Distfix Syntax]])], [[Distfix Syntax]])
loadDistfixes path [] = pure ([], defaultDistfixes)
loadDistfixes path dfDirectives = error "TODO Language.Ammonite.Load.loadDistfixes"
        --TODO merge the distfixes into one input and feed to distfix parser
        --TODO find all imports (incl from `use`) and load them
        --TODO resolve all group names to distfix tables in group defs
        --TODO resolve the `use` clause


lmapHoistEither f = bimapEitherT f id . hoistEither

instance Show LoadError where
    show (SystemError path) = "Import Error: cannot open file \"" ++ path ++ "\""
    show (SyntaxError msg) = "Syntax Error: " ++ show msg
    show (DirectiveError path msg) = "Syntax Error: \"" ++ path ++ "\"\n" ++ msg
    show (DesugarError path msg) = "Syntax Error: \"" ++ path ++ "\"\n" ++ msg
    show (ImportError msg) = "Import Error: " ++ error "TODO: Show LoadError (ImportError)"
