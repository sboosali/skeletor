--------------------------------------------------
--------------------------------------------------

{-| Fetch a directory tree

Possibly downloading a url, decompressing a tarball, or cloning a repository.

-}

module Skeletor.Haskell.Fetch where

--------------------------------------------------
--------------------------------------------------

import Skeletor.Core.Location

import Skeletor.Haskell.Types
import Skeletor.Haskell.Core

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

{-# INLINABLE fetchProject #-}

--------------------------------------------------
--------------------------------------------------

{-|

-}

resolveProject :: String -> Maybe Location
resolveProject = _

{-# INLINEABLE resolveProject #-}

--------------------------------------------------
--------------------------------------------------