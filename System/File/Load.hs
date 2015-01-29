{-| Sometimes, we want to interpret the contents of a file on disk,
    but we don't want to interpret a file more than once. This module
    solves this task in a generic, thread-safe way.

    Let's say you have some process for loading a file of type 'Load'.
    Then, making a call to 'load' will either find the file already processed
    and immediately return the result, or else process it for the first time
    using your passed 'Load' procedure.

    Multiple files may be loading at once within the same 'Loader'.
    That is, if a file is requested while a different one is being processed,
    there is no blocking.
    This is because there are locks around each file in addition to the lock
    around the file index.
    On the other hand, if a request comes in for a file while another thread is
    already processing it, then that request blocks until the other thread is done
    processing the file.

    Multiple loaders can be in operation at once, but they are independent!
    A file can be loaded multiple times /using different /'Loader'/s/, but only
    once for the same 'Loader'.
    Thus, if you build a programming language that reads source files from disk
    during an import, an interpreter for that language should have a single 'Loader'
    to handle all import commands. But, if you have multiple interpreters, they may
    each have different 'Loader's, so each interpreter would independently import files.
-}
module System.File.Load (
      Loader
    , newLoader
    , Load
    , load
    , loadOr
    ) where

import Control.Exception (Exception, handleJust)
import System.IO.Error (IOError, isAlreadyInUseError, isDoesNotExistError, isPermissionError)
import Control.Applicative
import System.IO (FilePath, withFile, Handle, IOMode(ReadMode))
import Control.Concurrent.MVar
import Data.Map (Map)
import qualified Data.Map as Map

{-| A shortcut for actions that process a file. -}
type Load a = Handle -> IO a
{-| Tracks the results of file processing and manages requests for files. -}
newtype Loader a = C (MVar (Map FilePath
                                (CacheLine a)))
type CacheLine a = MVar (Maybe a)

--TODO unload and reload functions
--type Unload a = a -> IO ()
--unload :: Loader a -> Unload a -> FilePath -> IO ()
--reload :: Loader a -> (Load a, Unload a) -> FilePath -> IO a


{-| Obtain an index of files which loads and processes files
    independently of all others.
-}
newLoader :: IO (Loader a)
newLoader = C <$> newMVar Map.empty

{-| Within a 'Loader', either immediately obtain the result of a processed file,
    or else open, process and index the file obtaining the result of processing.

    FIXME offer a way to handle an error (like the file does not exist or lack of permissions)
-}
load :: Loader a -> Load a -> FilePath -> IO a
load index = _load index Nothing

loadOr :: a -> Loader a -> Load a -> FilePath -> IO a
loadOr errval index = _load index (Just errval)

_load :: Loader a -> Maybe a -> Load a -> FilePath -> IO a
_load index m_errval proc path = do
    --FIXME normalize the path to an abspath
    line <- fromIndex index path
    fromLine m_errval line proc path

fromIndex :: Loader a -> FilePath -> IO (CacheLine a)
fromIndex (C cell) path = modifyMVar cell $ \index ->
    case Map.lookup path index of
        Just cached -> pure (index, cached)
        Nothing -> do
            newLine <- newMVar Nothing
            let index' = Map.insert path newLine index
            pure (index', newLine)

fromLine :: Maybe a -> CacheLine a -> Load a -> FilePath -> IO a
fromLine m_errval cell proc path =
    modifyMVar cell $ \val -> case val of
        Just cached -> pure (val, cached)
        Nothing -> do
            res <- case m_errval of
                Nothing -> go
                Just errval -> handleJust (handler errval) pure go
            pure (Just res, res)
    where
    go = withFile path ReadMode proc
    handler errval e | isAlreadyInUseError e = Just errval
                     | isDoesNotExistError e = Just errval
                     | isPermissionError e = Just errval
                     | otherwise = Nothing

