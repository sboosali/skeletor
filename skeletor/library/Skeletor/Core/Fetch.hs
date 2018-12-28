--------------------------------------------------
--------------------------------------------------

{-| Fetch a directory tree

Possibly downloading a url, decompressing a tarball, or cloning a repository.

-}

module Skeletor.Core.Fetch where

--------------------------------------------------
--------------------------------------------------

-- import Skeletor.Core.

--------------------------------------------------
-- Imports ---------------------------------------
--------------------------------------------------

import qualified "filepath" System.FilePath as File

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

fetchLocation :: String -> IO FilePath      -- TODO Managed or Cont
fetchLocation = _

--------------------------------------------------

{-|

-}

resolveLocation :: String -> Maybe Location
resolveLocation = _

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

  der

--------------------------------------------------

{-|

-}

--------------------------------------------------

{-|

-}

--------------------------------------------------
--------------------------------------------------