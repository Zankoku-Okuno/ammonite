{-# LANGUAGE ScopedTypeVariables #-}
module Language.Ammonite.Load where

import Control.Applicative
import Control.Monad

import System.IO
import System.File.Load
import Language.Distfix

import Language.Ammonite.Syntax.Concrete
import Language.Ammonite.Syntax.Sugar
import Language.Ammonite.Syntax.Abstract
import Language.Ammonite.Syntax.Parser
import Language.Ammonite.Syntax.Distfix.Parser

data LoadUnit sysval = LU {
      luProgram :: Value sysval
    , luDistfixGroups :: [(GroupName, [[Distfix Syntax]])]
    , luDistfixUse :: [[Distfix Syntax]] -- use the default set if none specified
}

data LoadError =
      SyntaxError String
    | DesugarError String
    --TODO directives error, desugaring error, eval error

--FIXME use an EitherT
loadAmmonite :: FilePath -> Load (Either LoadError (LoadUnit sysval))
loadAmmonite path fp = do
    input <- hGetContents fp
    case parse file path input of
        Left err -> pure . Left . SyntaxError $ show err
        Right (directives, concrete) -> do
            --TODO get distfix directives, send to loadDistfixes
            (groups, uses) <- loadDistfixes undefined
            --TODO if any directives left over, die
            --TODO desugar the syntax with the used distfixes
            case desugar concrete of
                Left err -> pure . Left $ DesugarError err
                Right abstract -> do
                    --TODO evaluate the file
                    pure . Right $ LU {
                          luProgram = undefined
                        , luDistfixGroups = groups
                        , luDistfixUse = uses
                        }
    where
    loadDistfixes dfDirectives = undefined
        --TODO merge the distfixes into one input and feed to distfix parser
        --TODO find all imports (incl from `use`) and load them
        --TODO resolve all group names to distfix tables in group defs
        --TODO resolve the `use` clause
