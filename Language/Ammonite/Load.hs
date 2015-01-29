{-# LANGUAGE ScopedTypeVariables #-}
module Language.Ammonite.Load 
    ( AmmoniteFile
    , Loader
    , newLoader
    , load
    , LoadUnit(..)
    , LoadError(..)
    ) where

import Control.Applicative
import Control.Monad
import Control.Monad.Trans.Either

import System.IO
import qualified System.File.Load as L
import Language.Distfix
import Text.Parsec (ParseError)

import Language.Ammonite.Syntax.Concrete
import Language.Ammonite.Syntax.Sugar
import Language.Ammonite.Syntax.Abstract
import Language.Ammonite.Syntax.Parser
import Language.Ammonite.Syntax.Distfix.Parser
import Language.Ammonite.Syntax.Distfix.Defaults


type Loader sysval = L.Loader (AmmoniteFile sysval)
type AmmoniteFile sysval = Either LoadError (LoadUnit sysval)

newLoader :: IO (Loader a)
newLoader = L.newLoader

load :: Loader sysval -> FilePath -> IO (AmmoniteFile sysval)
load loader path = L.loadOr (Left $ SystemError path) loader (loadAmmonite path) path


data LoadUnit sysval = LU {
      luProgram :: Expr sysval --FIXME this should really just be a module value
    , luDistfixGroups :: [(GroupName, [[Distfix Syntax]])]
    , luDistfixUse :: [[Distfix Syntax]] -- use the default set if none specified
}

data LoadError =
      SystemError FilePath -- ^When we can't even open the requested file.
    | SyntaxError ParseError -- ^When the file cannot be parsed.
    | DirectiveError FilePath String -- ^When any directive handling has a problem.
    | DesugarError FilePath String -- ^When there is an error desugaring the syntax.
    | ImportError String -- ^When there is an error evaluating the file body.


loadAmmonite :: FilePath -> L.Load (Either LoadError (LoadUnit sysval))
loadAmmonite path fp = do
    input <- hGetContents fp
    runEitherT $ do
        (directives, concrete) <- lmapHoistEither SyntaxError $ parse file path input
        --TODO get distfix directives, send to loadDistfixes
        (groups, uses) <- loadDistfixes path []
        --TODO if any directives left over, die
        abstract <- lmapHoistEither (DesugarError path) $ desugar uses concrete
        --TODO evaluate the file
        pure $ LU {
              luProgram = abstract
            , luDistfixGroups = groups
            , luDistfixUse = uses
            }

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