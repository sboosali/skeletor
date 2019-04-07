{-# LANGUAGE BinaryLiterals #-}

--------------------------------------------------
--------------------------------------------------

{-| 

-}

module Skeletor.Core.File.Read

  ( readTarball
  , readArchive
  , readCompressed
  , readUTF8

  ) where

--------------------------------------------------
--------------------------------------------------

import Skeletor.Core.File.Types

import Skeletor.Core.Tarball    (readTarball)
import Skeletor.Core.Compressed (readCompressed)
import Skeletor.Core.Archive    (readArchive)

--------------------------------------------------
--------------------------------------------------

import           "modern-uri" Text.URI (URI)
import qualified "modern-uri" Text.URI as URI

--------------------------------------------------
--------------------------------------------------

import qualified "unordered-containers" Data.HashMap.Lazy as HashMap
import           "unordered-containers" Data.HashMap.Lazy (HashMap)

--------------------------------------------------

import qualified "containers" Data.Map as Map
import           "containers" Data.Map (Map)

--------------------------------------------------

import qualified "filepath"   System.FilePath as File

--------------------------------------------------

import qualified "text" Data.Text         as StrictText
import qualified "text" Data.Text.Lazy    as LazyText

import qualified "text" Data.Text.IO      as StrictText
import qualified "text" Data.Text.Lazy.IO as LazyText

--------------------------------------------------

import qualified "bytestring" Data.ByteString      as StrictByteString
import qualified "bytestring" Data.ByteString.Lazy as LazyByteString

--------------------------------------------------
--------------------------------------------------

import Prelude_location

--------------------------------------------------
--------------------------------------------------

{-| De-compress and de-archive the (given) tarball;
into a directory (of files).

-}

readTarball :: TarballFile -> IO (_)
readTarball = _

--------------------------------------------------

{-| De-code the (given, @UTF-8@-encoded) file.

-}

readUTF8 :: UTF8File -> IO (_)
readUTF8 = _

--------------------------------------------------
--------------------------------------------------

{-| Get the file contents.

Usage:

@contents <- readFileWith reader parser file
@

-}

readFileWith :: (FilePath -> IO a) -> (a -> b) -> File a -> IO b
readFileWith reader parser = \case

  FileContents contents -> go contents

  FilePath     filepath -> do
    contents <- reader filepath
    go contents

  where

  go contents = return (parser contents)

--------------------------------------------------

{-| Specializes 'readFileWith' to lazy @Text@.

@≡ 'readFileWith' 'LazyText.readFile' 'id'
@

-}

readLazyTextFile :: File LazyText.Text -> IO LazyText.Text
readLazyTextFile = readFileWith reader parser

  where

  reader = LazyText.readFile
  parser = id

{-# INLINE readLazyTextFile #-}

--------------------------------------------------

{-| Specializes 'readFileWith' to strict @Text@.

@≡ 'readFileWith' 'StrictText.readFile' 'id'
@

-}

readStrictTextFile :: File StrictText.Text -> IO StrictText.Text
readStrictTextFile = readFileWith reader parser

  where

  reader = StrictText.readFile
  parser = id

{-# INLINE readStrictTextFile #-}

--------------------------------------------------
--------------------------------------------------

{-| Specializes 'readFileWith' to lazy @ByteString@.

@≡ 'readFileWith' 'LazyByteString.readFile' 'id'
@

-}

readLazyByteStringFile :: File LazyByteString.ByteString -> IO LazyByteString.ByteString
readLazyByteStringFile = readFileWith reader parser

  where

  reader = LazyByteString.readFile
  parser = id

{-# INLINE readLazyByteStringFile #-}

--------------------------------------------------

{-| Specializes 'readFileWith' to strict @ByteString@.

@≡ 'readFileWith' 'StrictByteString.readFile' 'id'
@

-}

readStrictByteStringFile :: File StrictByteString.ByteString -> IO StrictByteString.ByteString
readStrictByteStringFile = readFileWith reader parser

  where

  reader = StrictByteString.readFile
  parser = id

{-# INLINE readStrictByteStringFile #-}

--------------------------------------------------
--------------------------------------------------
{-



{-| 

@'readFileWith' '' ''
@

-}

readFileText :: (FilePath -> IO a) -> (a -> b) -> File a -> IO b
readFileText = readFileWith reader parser

  where

  reader = _
  parser = _



-}
--------------------------------------------------
