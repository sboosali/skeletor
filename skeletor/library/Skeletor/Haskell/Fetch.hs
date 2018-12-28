--------------------------------------------------
--------------------------------------------------

{-| Fetch a directory tree

Possibly downloading a url, decompressing a tarball, or cloning a repository.

-}

module Skeletor.Haskell.Fetch where

--------------------------------------------------
--------------------------------------------------

import Skeletor.Haskell.Types

import Skeletor.Haskell.Core
import Skeletor.Haskell.Location

--------------------------------------------------
-- Imports ---------------------------------------
--------------------------------------------------

import qualified "filepath"   System.FilePath as File

--------------------------------------------------
--------------------------------------------------

import qualified "base" System.IO as IO

--------------------------------------------------

import Prelude_skeletor

--------------------------------------------------
-- Definitions -----------------------------------
--------------------------------------------------

{-|

-}

fetchProject :: String -> IO FilePath      -- TODO Managed or Cont
fetchProject = _

--------------------------------------------------

{-|

-}

resolveProject :: String -> Maybe Location
resolveProject = _

--------------------------------------------------

{-|

-}

  = LocationInlineFile  Text         -- ^ Write this string.
  | LocationFile        FilePath     -- ^ Copy this file.

  | LocationInlineDirectory FileTree -- ^ Write these strings.
  | LocationDirectory       FilePath -- ^ Copy this directory, recursively.

  | LocationURL         URI          -- ^ Download this URL.
  | LocationGit         URI          -- ^ Clone this Git repository.

  | LocationArchive     FilePath     -- ^ Un-Archive this file, then copy the 'LocationDirectory'.
  | LocationTarball     FilePath     -- ^ Un-Compress this file, un-archive it, then copy the 'LocationDirectory'.

  | LocationEnvironment EV           -- ^ Read (TODO: a path or URI?) from this environment variable.

--------------------------------------------------

{-|

-}

--------------------------------------------------

{-|

-}

--------------------------------------------------
--------------------------------------------------