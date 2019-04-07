{-# LANGUAGE BinaryLiterals #-}

--------------------------------------------------
--------------------------------------------------

{-| 

-}

module Skeletor.Core.File.Types where

--------------------------------------------------

import Skeletor.Core.EnvironmentVariable

--------------------------------------------------
--------------------------------------------------

import           "modern-uri" Text.URI (URI)
import qualified "modern-uri" Text.URI as URI

--------------------------------------------------

import qualified "unordered-containers" Data.HashMap.Lazy as HashMap
import           "unordered-containers" Data.HashMap.Lazy (HashMap)

--------------------------------------------------

import qualified "containers" Data.Map as Map
import           "containers" Data.Map (Map)

--------------------------------------------------

import qualified "filepath"   System.FilePath as File

--------------------------------------------------

import qualified "text"       Data.Text    as T
import qualified "text"       Data.Text.IO as T

--------------------------------------------------

import qualified "bytestring" Data.ByteString as B

--import qualified "bytestring" Data.ByteString.Lazy as B

--------------------------------------------------

--import qualified "base" System.IO as IO

import Prelude_location

--------------------------------------------------
--------------------------------------------------

{-|

-}


--------------------------------------------------
--------------------------------------------------

{-| a 'File' represents either:

* a file's location; or
* file contents.

See 'readFile'.

-}

data File a

  = FileLocation FilePath
  | FileContents !a

  deriving stock    (Functor)
  deriving stock    (Show,Eq,Ord,Generic)
  deriving anyclass (NFData)

--------------------------------------------------
--------------------------------------------------

{-| an Archive is an archive-file;
i.e. a directory of files, "linked" into a single file. 

Its file-extension should be @.tar@.

-}

type ArchiveFile = File ByteString

--------------------------------------------------

-- pattern Archiveocation  :: FilePath   -> ArchiveFile
-- pattern ArchiveContents :: ByteString -> ArchiveFile

--------------------------------------------------
--------------------------------------------------

{-| a Tarball is a compressed archive-file: 
i.e. a directory of files, "linked" into a single file,
then compressed (into a smaller file).

Its file-extension should be @.tar.gz@.

-}

type TarballFile = File ByteString

--------------------------------------------------

-- TarballFile
-- FileLocation FilePath
-- FileContents ByteString

--------------------------------------------------
--------------------------------------------------

{-| a @UTF8File@ is an regular-file;
its contents are @UTF-8@-encoded text.

Its file-extension can be anything; in particular, @.txt@.

-}

type UTF8File = File Text


--------------------------------------------------

-- UTF8File
-- FileLocation FilePath
-- FileContents Text

--------------------------------------------------
--------------------------------------------------



--------------------------------------------------
--------------------------------------------------
{- Notes / Old Code



{-| a _ is an _ file.

-}

type _ = File 





-}
--------------------------------------------------