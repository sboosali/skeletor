--------------------------------------------------
--------------------------------------------------

{-| Finding Files on the Filesystem (or from other Locations).

e.g.

@
@

Defines utilities: @is{File,Directory}{Unrestricted,Permitted}By*@

-}

module Skeletor.Haskell.Find where

--------------------------------------------------
-- Imports ---------------------------------------
--------------------------------------------------

import Internal.Skeletor.Haskell

--------------------------------------------------

-- import Skeletor.Haskell.Types
import Skeletor.Haskell.Find.Types

import Skeletor.Haskell.Chiaroscuro

--------------------------------------------------

import qualified "filemanip"  System.FilePath.Find as Find
import           "filemanip"  System.FilePath.Find (FindClause)
-- import           "filemanip"  System.FilePath.Find
--   ( (~~?), (/~?)
--   , (==?), (/=?)
--   , (>?), (<?), (>=?), (<=?)
--   , (&&?), (||?)
--   )

import           "filemanip"  System.FilePath.GlobPattern
  ( (~~), (/~)
  )

--------------------------------------------------

-- import qualified "filepath"   System.FilePath as File

--------------------------------------------------
--------------------------------------------------

import "base" Data.Functor.Contravariant (Predicate(..))

--------------------------------------------------

import Prelude_skeletor

--------------------------------------------------
-- Definitions -----------------------------------
--------------------------------------------------

{-|



For example, see 'defaultFileFilters', which:

* skips 'ignoredDirectories', when descending (into subdirectories); and
* skips 'ignoredFiles' and non-files, when collecting (files to return).

-}

findFilesWith
  :: FileFilters
  -> (FilePath -> IO [FilePath])

findFilesWith filters filepath =

  Find.find shouldDescendM shouldCollectM filepath

  where

  shouldDescendM :: FindClause Bool
  shouldDescendM = shouldDescend <$> Find.directory

  shouldCollectM :: FindClause Bool
  shouldCollectM = isRegularFileM Find.&&? (shouldCollect <$> Find.fileName)
    where
    isRegularFileM  = (Find.fileType Find.==? Find.RegularFile)

  shouldDescend :: FilePath -> Bool
  Predicate shouldDescend = (directoryPredicate filters)

  shouldCollect :: FilePath -> Bool
  shouldCollect = (directoryPredicate filters) & getPredicate

--------------------------------------------------
--------------------------------------------------

{-| Whether the (given) 'FilePath' is accepted by the 'FileFilters'.

-}

filePredicate :: FileFilters -> Predicate FilePath

filePredicate (ChiaroscuroFilters{ chiaroscuroBlacklist, chiaroscuroWhitelist }) =

  Predicate predicate

  where

  predicate :: FilePath -> Bool
  predicate filepath
    = isFileUnrestrictedByBlacklist chiaroscuroBlacklist filepath
   && isFilePermittedByWhitelist    chiaroscuroWhitelist filepath

--------------------------------------------------

{-| Whether the (given) 'FilePath' is not restricted by the 'Blacklist'.

-}

isFileUnrestrictedByBlacklist
  :: Blacklist FilePattern
  -> (FilePath -> Bool)

isFileUnrestrictedByBlacklist (Blacklist bs) = go bs
  where

  go blacklist filepath =

    all_Set (Predicate predicate) blacklist

    where

    predicate :: FilePattern -> Bool
    predicate = \case
      RegularPattern   glob -> (filepath /~ glob)
      DirectoryPattern _    -> False

--------------------------------------------------

{-| Whether the (given) regular 'FilePath' is permitted by the 'Whitelist'.

/NOTE/ an empty whitelist (i.e. @('mempty' :: 'Whitelist')@) permits anything.

-}

isFilePermittedByWhitelist
  :: Whitelist FilePattern
  -> (FilePath -> Bool)

isFilePermittedByWhitelist = \case

  Whitelist Nothing   -> const True
  Whitelist (Just ws) -> go ws

  where

  go whitelist filepath =

    any_Set (Predicate predicate) whitelist

    where

    predicate :: FilePattern -> Bool
    predicate = \case
      RegularPattern   _    -> False
      DirectoryPattern glob -> (filepath ~~ glob)

--------------------------------------------------
--------------------------------------------------

{-| Whether the (given) directory 'FilePath' is accepted by the 'FileFilters'.

-}

directoryPredicate :: FileFilters -> Predicate FilePath

directoryPredicate (ChiaroscuroFilters{ chiaroscuroBlacklist, chiaroscuroWhitelist }) =

  Predicate predicate

  where

  predicate :: FilePath -> Bool
  predicate filepath
    = isDirectoryUnrestrictedByBlacklist chiaroscuroBlacklist filepath
   && isDirectoryPermittedByWhitelist    chiaroscuroWhitelist filepath

--------------------------------------------------

{-| Whether the (given) 'FilePath' is not restricted by the 'Blacklist'.

-}

isDirectoryUnrestrictedByBlacklist
  :: Blacklist FilePattern
  -> (FilePath -> Bool)

isDirectoryUnrestrictedByBlacklist (Blacklist bs) = go bs
  where

  go blacklist filepath =

    all_Set (Predicate predicate) blacklist

    where

    predicate :: FilePattern -> Bool
    predicate = \case
      RegularPattern   glob -> (filepath /~ glob)
      DirectoryPattern _    -> False

--------------------------------------------------

{-| Whether the (given) regular 'FilePath' is permitted by the 'Whitelist'.

/NOTE/ an empty whitelist (i.e. @('mempty' :: 'Whitelist')@) permits anything.

-}

isDirectoryPermittedByWhitelist
  :: Whitelist FilePattern
  -> (FilePath -> Bool)

isDirectoryPermittedByWhitelist = \case

  Whitelist Nothing   -> const True
  Whitelist (Just ws) -> go ws

  where

  go whitelist filepath =

    any_Set (Predicate predicate) whitelist

    where

    predicate :: FilePattern -> Bool
    predicate = \case
      RegularPattern   _    -> False
      DirectoryPattern glob -> (filepath ~~ glob)

--------------------------------------------------
--------------------------------------------------
{- Notes / Old Code

infix 4 (~~?) :: FindClause FilePath -> GlobPattern -> FindClause Bool

  Return True if the current file's name matches the given GlobPattern.

infix 4 (/~?) :: FindClause FilePath -> GlobPattern -> FindClause Bool

  Return True if the current file's name does not match the given GlobPattern.

(.&.?) :: Bits a => FindClause a -> a -> FindClause a infixl 7

  This operator is useful to check if bits are set in a FileMode.

--------------------------------------------------

-}
--------------------------------------------------