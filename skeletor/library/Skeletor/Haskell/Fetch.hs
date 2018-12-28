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
--------------------------------------------------

{-|

-}

resolveProject :: String -> Maybe Location
resolveProject = _

--------------------------------------------------
--------------------------------------------------