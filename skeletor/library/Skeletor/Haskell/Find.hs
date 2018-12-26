--------------------------------------------------
--------------------------------------------------

{-| Finding Files on the Filesystem (or from other Locations).

e.g.

@
@

-}

module Skeletor.Haskell.Find where

--------------------------------------------------
--------------------------------------------------

import Skeletor.Haskell.Types
import Skeletor.Haskell.Core
import Skeletor.Haskell.Variable

--------------------------------------------------

import qualified "filemanip"  System.FilePath.Find as Find
import           "filemanip"  System.FilePath.Find (FindClause)

import           "filemanip"  System.FilePath.GlobPattern ((~~))

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

import Prelude_skeletor

--------------------------------------------------
--------------------------------------------------

{-|

Skips 'ignoredDirectories', when descending.

Skips 'ignoredFiles' and non-files, when collecting.

-}

findFiles :: FilePathFilters -> FilePath -> IO [FilePath]
findFiles = Find.find recursionPredicate filterPredicate

  where

  recursionPredicate :: FindClause Bool
  recursionPredicate = shouldRecurIntoSubdirectory <$> Find.directory

  filterPredicate :: FindClause Bool
  filterPredicate = isRegularFileM Find.&&? isGoodFilenameM
  
    where
    isRegularFileM  = (Find.fileType Find.==? Find.RegularFile)
    isGoodFilenameM = (shouldKeepFilename <$> Find.fileName)

  shouldRecurIntoSubdirectory :: FilePath -> Bool
  shouldRecurIntoSubdirectory = shouldIgnoreDirectory > not

  shouldKeepFilename :: FilePath -> Bool
  shouldKeepFilename = shouldIgnoreFilename > not

  shouldIgnoreDirectory :: FilePath -> Bool
  shouldIgnoreDirectory directory =
    any (directory ~~) ignoredDirectories

  shouldIgnoreFilename :: FilePath -> Bool
  shouldIgnoreFilename filename =
    any (filename ~~) ignoredFiles

--------------------------------------------------
--------------------------------------------------

filterFilePaths :: FilePathFilters -> FindClause Bool
filterFilePaths = \case

  This  bs    -> filterBlacklist bs
  That  ws    -> filterWhitelist ws
  These bs ws -> filterBlacklist bs Find.&&? filterWhitelist ws

--------------------------------------------------

filterBlacklist :: Blacklist -> FindClause Bool
filterBlacklist (Blacklist bs) =  bs

--------------------------------------------------

filterWhitelist :: Whitelist -> FindClause Bool
filterWhitelist (Whitelist ws) =  ws

--------------------------------------------------
--------------------------------------------------
{- Notes / Old Code

--------------------------------------------------



-}
--------------------------------------------------