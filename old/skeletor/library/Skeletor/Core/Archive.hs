--------------------------------------------------
--------------------------------------------------

{-| 

-}

module Skeletor.Core.Archive

  ( ArchiveConfig(..)
  , readArchive
  , readArchiveWith

  ) where

--------------------------------------------------
--------------------------------------------------

import Skeletor.Core.File.Types
import Skeletor.Core.File.Read

--------------------------------------------------
--------------------------------------------------

import qualified "tar" Codec.Archive.Tar       as Tar
import qualified "tar" Codec.Archive.Tar.Check as Tar

--------------------------------------------------

import qualified "temporary" System.IO.Temp as Temporary

--------------------------------------------------
--------------------------------------------------

import qualified "bytestring" Data.ByteString.Lazy as ByteString
import           "bytestring" Data.ByteString.Lazy (ByteString)

--------------------------------------------------
--------------------------------------------------

import qualified "base" Control.Exception as Exception
import           "base" Control.Exception (Exception(..), IOException)

--------------------------------------------------
--------------------------------------------------

import Prelude_skeletor

--------------------------------------------------
--------------------------------------------------

{-|  

-}

data ArchiveConfig = ArchiveConfig

  { extractionPath :: !FilePath  -- ^ a directory. 'readArchive' extracts the @.tar@'s files under 'extractionPath'.
--, temporaryPath :: !FilePath  -- ^ a directory. 'readArchive' extracts the @.tar@'s files under 'extractionPath'.
  }

  deriving stock    (Show,Read,Eq,Ord,Lift,Generic)
  deriving anyclass (NFData,Hashable)

--------------------------------------------------
--------------------------------------------------

getTemporaryArchiveConfig :: IO ArchiveConfig
getTemporaryArchiveConfig = do

  tmp <- Temporary.getCanonicalTemporaryDirectory -- systemTemporaryDirectory
  extractPath <- Temporary.createTempDirectory tmp template

  return ArchiveConfig{..}

  where
  template = "skeletor" -- theDirectoryNameTemplate

--------------------------------------------------
--------------------------------------------------

{-| De-archive the (given) archive;
into a directory (of files).

'readArchive' reads an archive file.

Rethrows these exceptions under 'MonadThrow':

* 'IOException'
* 'Tar.FileNameError'

(Other exceptions may be thrown under @IO@.)

-}

readArchiveWith :: (MonadThrow m, MonadIO m) => ArchiveConfig -> ArchiveFile -> m Bool
readArchiveWith ArchiveConfig{..} archiveFile = rethrowing $ liftIO $ do

  bytes <- readLazyByteStringFile archiveFile
  dearchive bytes

  return $ True                 -- TODO

  where

  dearchive :: {- FilePath -> -} ByteString -> IO ()
  dearchive

    = Tar.read
    > Tar.checkTarbomb extractPath
    > Tar.unpack       extractPath

  rethrowing = catches_throwM (Proxy :: Proxy Tar.FileNameError)

--------------------------------------------------

{-| @'readArchiveWith'@, with @'getTemporaryArchiveConfig'@.

Returns the (temporary, but undeleted) directory into which
the archive was extracted.

-}

readArchive :: (MonadThrow m, MonadIO m) => ArchiveFile -> m FilePath
readArchive archiveFile = do

  config@ArchiveConfig{..} <- getTemporaryArchiveConfig

  _ <- readArchive config archiveFile

  return extractPath

--------------------------------------------------
--------------------------------------------------

catches_throwM
  :: forall e m a proxy.
    ( MonadThrow m, MonadIO m
    , Exception e
    )
  => proxy e -> IO a -> m a

catches_throwM effect =

  effect `catches` handlers

  where

  handlers =
    [ Handler (\(e :: e)           -> throwM e)
    , Handler (\(e :: IOException) -> throwM e)
    ]

--------------------------------------------------
--------------------------------------------------
