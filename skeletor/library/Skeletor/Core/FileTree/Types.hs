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

type FileTree = FileTree Text

--------------------------------------------------

{-| An on-disk directory.

-}

type FilePathTree = FileTree Void

--------------------------------------------------
--------------------------------------------------

{-| An in-memory directory (with all its children files).

-}

newtype FileTree a = FileTree

  (HashMap FilePath (File a))
  -- TODO value should be (Maybe Text) to represent an empty directory?

  deriving stock    (Show,Read,Generic)
  deriving newtype  (Eq,Ord)
  deriving newtype  (Semigroup,Monoid)
  deriving newtype  (NFData,Hashable)

--------------------------------------------------

instance IsList (FileTree a) where

  type Item (FileTree a) = (FilePath, a)

  fromList = HashMap.fromList > coerce
  toList   = coerce           > HashMap.toList

--------------------------------------------------

-- instance (Semigroup a) => Semigroup (FileTree a) where

--   (FileTree xs) <> (FileTree ys) = FileTree zs

--     where
--     zs = HashMap.unionWith (<>) xs ys

--------------------------------------------------
--------------------------------------------------

{-|

@
≡ 'HashMap.empty'
@

-}

emptyFileTree :: FileTree a
emptyFileTree = FileTree HashMap.empty

--------------------------------------------------
--------------------------------------------------
{- Notes / Old Code


-}
--------------------------------------------------