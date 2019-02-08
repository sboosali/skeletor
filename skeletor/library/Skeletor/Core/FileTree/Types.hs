{-# LANGUAGE BinaryLiterals #-}

--------------------------------------------------
--------------------------------------------------

{-| 

-}

module Skeletor.Core.FileTree.Types where

--------------------------------------------------

import Skeletor.Core.File

--------------------------------------------------
--------------------------------------------------

import qualified "unordered-containers" Data.HashMap.Lazy as HashMap
import           "unordered-containers" Data.HashMap.Lazy (HashMap)

--------------------------------------------------

import qualified "filepath" System.FileTreePath as FileTree

--------------------------------------------------

import qualified "text"       Data.Text    as T
import qualified "text"       Data.Text.IO as T

--------------------------------------------------

import qualified "bytestring" Data.ByteString as B

--import qualified "bytestring" Data.ByteString.Lazy as B

--------------------------------------------------

--import qualified "base" System.IO as IO

--------------------------------------------------

import Prelude_location

--------------------------------------------------
--------------------------------------------------

{-|

-}

--------------------------------------------------
--------------------------------------------------

{-| An in-memory directory (with all its children files).

-}

newtype FileTree a = FileTree

  (HashMap FilePath (Maybe (File a)))
  -- TODO value should be (Maybe Text) to represent an empty directory?

  

--------------------------------------------------
--------------------------------------------------
{- Notes / Old Code


-}
--------------------------------------------------