{-# LANGUAGE OverloadedStrings #-}

{-# LANGUAGE ScopedTypeVariables #-}
--------------------------------------------------
--------------------------------------------------

{-| 

-}

module Skeletor.Core.Tarball

  ( readTarball
  , CompressionAlgorithm(..)
  , guessCompressionAlgorithmFromTarballFileExtension
  ) where

--------------------------------------------------
--------------------------------------------------

import Skeletor.Core.File.Types

--------------------------------------------------
--------------------------------------------------

import qualified "filepath" System.FilePath as File

--------------------------------------------------

import qualified "tar" Codec.Archive.Tar as Tar

--------------------------------------------------

import qualified "zlib" Codec.Compression.GZip as GZip
import qualified "zlib" Codec.Compression.Zlib as Zlib

import qualified "zlib" Codec.Compression.Zlib.Internal (DecompressError)

--------------------------------------------------

import qualified "bytestring" Data.ByteString.Lazy as LazyByteString

--------------------------------------------------
-- Definitions -----------------------------------
--------------------------------------------------

data CompressionAlgorithm

  = GZIP -- See 'Tarball_TAR_GZ'.
  | ZLIB -- See 'Tarball_TAR_Z'.

  deriving stock    (Enum,Bounded,Ix)
  deriving anyclass (GEnum)
  deriving stock    (Show,Read,Eq,Ord,Lift,Generic)
  deriving anyclass (NFData,Hashable)

--------------------------------------------------
--------------------------------------------------

tarballCompressionAlgorithm :: TarballExtension -> CompressionAlgorithm
tarballCompressionAlgorithm = \case

  Tarball_TAR_GZ -> GZIP
  Tarball_TAR_Z  -> ZLIB

--------------------------------------------------

guessCompressionAlgorithmFromTarballFileExtension :: FilePath -> CompressionAlgorithm
guessCompressionAlgorithmFromTarballFileExtension tarballPath =

  tarballCompressionAlgorithm tarballExtension

  where

  tarballExtension = File.takeExtensions tarballPath

--------------------------------------------------
--------------------------------------------------

data TarballExtension

  = Tarball_TAR_GZ              -- @".tar.gz"@ — i.e. a @GZip@ tarball.
  | Tarball_TAR_Z               -- @".tar.z"@  — i.e. a @ZLib@ tarball.

  deriving stock    (Enum,Bounded,Ix)
  deriving anyclass (GEnum)
  deriving stock    (Show,Read,Eq,Ord,Lift,Generic)
  deriving anyclass (NFData,Hashable)

--------------------------------------------------
--------------------------------------------------

renderTarballExtension :: (IsString s) => TarballExtension -> s
renderTarballExtension = \case

  Tarball_TAR_GZ -> ".tar.gz"
  Tarball_TAR_Z  -> ".tar.z"

--------------------------------------------------

parseTarballExtension :: String -> Maybe TarballExtension
parseTarballExtension = \case

  ".tar.gz" -> Just Tarball_TAR_GZ
  ".tar.z"  -> Just Tarball_TAR_Z
  _         -> Nothing

--------------------------------------------------
--------------------------------------------------

{-| 'readTarball' reads a "tarball".

Rethrows these exceptions under 'MonadThrow':

* 'IOException'
* 'DecompressError'

(Other exceptions may be thrown under @IO@.)

Dispatches to:

* 'readTarballGZip' — on @".tar.gz"@ files, or if 'GZIP' is given; and to
* 'readTarballZlib' — on @".tar.z"@ files, or if 'ZLIB' is given.

NOTE the 'CompressionAlgorithm', if given (i.e. @('Just' _)@),
overrides the @tarballPath@'s file-extension.

-}

readTarball
  :: (MonadThrow m)
  => Maybe CompressionAlgorithm
  -> FilePath                    -- ^ @directoryPath@ — output path.
  -> FilePath                    -- ^ @tarballPath@   — input path.
  -> m Bool

readTarball compression directoryPath tarballPath = do

  tarballReader directoryPath tarballPath

  where

  tarballReader = \case
    Tarball_TAR_GZ -> readTarballGZip
    Tarball_TAR_Z  -> readTarballZlib

  tarballKind
    = compression
    & maybe (tarballCompressionAlgorithm tarballExtension') id

  tarballExtension'
    = parseTarballExtension tarballExtension
    & maybe Tarball_TAR_GZ id

  tarballExtension
    = File.takeExtensions tarballPath

  -- NOTE:
  -- « File.takeExtensions "basename.tar.gz" »
  -- outputs the full « ".tar.gz" »)
  -- (not just « ".gz" »).

--------------------------------------------------

readTarballGZip :: (MonadThrow m) => FilePath -> FilePath -> m Bool
readTarballGZip directoryPath tarballPath = rethrowing $ do

  bytes <- BS.readFile tarballPath
  decompress bytes

  return $ True                 -- TODO

  where

  decompress
    = GZip.decompressWith GZip.defaultDecompressParams{ }
    > Tar.read
    > Tar.unpack directoryPath

  rethrowing = catches_throwM (Proxy :: Proxy DecompressError)

--------------------------------------------------

readTarballZlib :: (MonadThrow m) => FilePath -> FilePath -> m Bool
readTarballZlib directoryPath tarballPath = rethrowing $ do

  bytes <- BS.readFile tarballPath
  decompress bytes

  return $ True                 -- TODO

  where

  decompress
    = Zlib.decompressWith Zlib.defaultDecompressParams{ }
    > Tar.read
    > Tar.unpack directoryPath

  rethrowing = catches_throwM (Proxy :: Proxy DecompressError)

--------------------------------------------------
-- Utilities -------------------------------------
--------------------------------------------------

catches_throwM
  :: forall e m a.
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
-- EOF -------------------------------------------
--------------------------------------------------