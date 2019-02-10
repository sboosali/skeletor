
--------------------------------------------------
--------------------------------------------------

{-| 

-}

module Skeletor.Core.Directory.Types where

--------------------------------------------------
--------------------------------------------------

import Skeletor.Core.File.Types

--------------------------------------------------
--------------------------------------------------

import qualified "unordered-containers" Data.HashMap.Lazy as HashMap
import           "unordered-containers" Data.HashMap.Lazy (HashMap)

--------------------------------------------------

import qualified "filepath" System.FilePath as FilePath

--------------------------------------------------

import qualified "directory" System.Directory as Directory

--------------------------------------------------
--------------------------------------------------

import qualified "text"       Data.Text    as T
import qualified "text"       Data.Text.IO as T

--------------------------------------------------

import qualified "bytestring" Data.ByteString as B

--import qualified "bytestring" Data.ByteString.Lazy as B

--------------------------------------------------

--import qualified "base" System.IO as IO

--------------------------------------------------
--------------------------------------------------

import Prelude_location

--------------------------------------------------
-- Declarations ----------------------------------
--------------------------------------------------

--------------------------------------------------
--------------------------------------------------

{-| A polymorphic directory.

(e.g. with paths as keys, and files as values).



-}

newtype Directory f a = Directory

  (HashMap FilePath (f a))

  -- TODO value should be (Maybe Text) to represent an empty directory?

  deriving stock    (Functor)
  deriving stock    (Show{-,Read-},Generic)
  deriving newtype  (Eq,Ord)
  deriving newtype  (Semigroup,Monoid)
  deriving newtype  (NFData{-,Hashable-})

--------------------------------------------------

instance IsList (Directory f a) where

  type Item (Directory f a) = (FilePath, a)

  fromList = HashMap.fromList > coerce
  toList   = coerce           > HashMap.toList

--------------------------------------------------

-- instance (Semigroup a) => Semigroup (Directory f a) where

--   (Directory xs) <> (Directory ys) = Directory zs

--     where
--     zs = HashMap.unionWith (<>) xs ys

--------------------------------------------------

-- | @= 'defaultDirectory'@

instance Default (Directory f a) where
  def = defaultDirectory

--------------------------------------------------
-- Definitions -----------------------------------
--------------------------------------------------

-- | @= 'emptyDirectory'@

defaultDirectory :: Directory f a
defaultDirectory = emptyDirectory

--------------------------------------------------
--------------------------------------------------

{-|

@
≡ 'HashMap.empty'
@

-}

emptyDirectory :: Directory f a
emptyDirectory = Directory HashMap.empty

--------------------------------------------------
--------------------------------------------------
{- Notes / Old Code



-}
--------------------------------------------------