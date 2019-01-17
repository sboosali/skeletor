{-# LANGUAGE BinaryLiterals #-}

--------------------------------------------------
--------------------------------------------------

{-|



-}

module Skeletor.Haskell.Types

  ( module Skeletor.Haskell.Types
  , module Skeletor.Haskell.Binding.Types
  , module Skeletor.Haskell.Variable.Types
  ) where

--------------------------------------------------
-- Exports ---------------------------------------
--------------------------------------------------

import Skeletor.Haskell.Binding.Types
import Skeletor.Haskell.Variable.Types

--------------------------------------------------
-- Imports (Project) -----------------------------
--------------------------------------------------

--------------------------------------------------
-- Imports (External) ----------------------------
--------------------------------------------------

import qualified "unordered-containers" Data.HashMap.Lazy as HashMap
import           "unordered-containers" Data.HashMap.Lazy (HashMap)

--------------------------------------------------
-- Imports (Standard Library) --------------------
--------------------------------------------------

import           "text" Data.Text (Text)

--------------------------------------------------
-- Imports (Custom Prelude) ----------------------
--------------------------------------------------

import Prelude_skeletor hiding (Text)

--------------------------------------------------
-- Types -----------------------------------------
--------------------------------------------------

type UnknownOr = Either String    -- TODO mv to own pkg.

--------------------------------------------------
--------------------------------------------------

{-| 

-}

type ProjectIdentifier = UnknownOr KnownProject

--------------------------------------------------

{-| 

-}

data KnownProject

  = DefaultHaskellProject
  
  deriving stock    (Enum,Bounded,Ix)
  deriving anyclass (GEnum)
  deriving stock    (Show,Read,Eq,Ord,Lift,Generic)
  deriving anyclass (NFData,Hashable)

--------------------------------------------------

-- | @= 'defaultKnownProject'@

instance Default KnownProject where
  def = defaultKnownProject

-- | @= 'DefaultHaskellProject'@

defaultKnownProject :: KnownProject
defaultKnownProject = DefaultHaskellProject

--------------------------------------------------
--------------------------------------------------

{-| 

@HashMap FilePath () ≡ HashSet FilePath
@

-}

type Files_ = Files ()

--------------------------------------------------

{-|

-}

type FileTemplates = Files TemplateFile

--------------------------------------------------

{-|

-}

type FilesBy = Files FileBy

--------------------------------------------------
--------------------------------------------------

{-|

-}

newtype Files a = Files

  (HashMap FilePath a)

  deriving stock    (Functor,Foldable,Traversable)
  deriving stock    (Show,Read)
  deriving stock    (Generic,Generic1)
  deriving newtype  (Eq,Ord)
--deriving newtype  (Semigroup,Monoid) --TODO

  deriving newtype  (NFData,Hashable)
  deriving newtype  (Hashable1)

--------------------------------------------------

-- | 
instance (Ord a) => IsList (Files a) where

  type Item (Files a) = (FilePath, a)
  fromList = HashMap.fromList > coerce
  toList   = coerce           > HashMap.toList

--------------------------------------------------

-- instance NFData1 Files where

--   liftRnf :: (a -> ()) -> (Files a -> ())
--   liftRnf rnf' = go

--     where
--     go _ = _

--------------------------------------------------

{-|

@
≡ 'Map.empty'
@

-}

emptyFiles :: Files a
emptyFiles = Files HashMap.empty

--------------------------------------------------
--------------------------------------------------

{-|

-}

data FileBy

  = FileByContents Text         -- ^ Write this string.
  | FileByPath     FilePath     -- ^ Copy this file.
--TODO | FileByUrl      URI          -- ^ Download this URL, then write it.

  deriving stock    (Show,Eq,Ord,Generic)
  deriving anyclass (NFData)

--------------------------------------------------

instance Hashable FileBy where

  hashWithSalt :: Int -> FileBy -> Int
  hashWithSalt salt = \case

    FileByContents text -> c1 `hashWithSalt` salt `hashWithSalt` text
    FileByPath     path -> c2 `hashWithSalt` salt `hashWithSalt` path
--TODO    FileByUrl      url  -> c3 `hashWithSalt` salt `hashWithSalt` url

    where

    c1 :: Int
    c1 = 0b0000000000000000000000000000000000000000000000000000000000000000

    c2 :: Int
    c2 = 0b0101010101010101010101010101010101010101010101010101010101010101

--TODO c3 = :: Int
--    c3 = 0b1010101010101010101010101010101010101010101010101010101010101010

--------------------------------------------------
--------------------------------------------------

{-|

-}

data TemplateFile = TemplateFile

  { 
  }

--------------------------------------------------
--------------------------------------------------

{-|

-}

data SrcDst = SrcDst

  { input  :: Src
  , output :: Dst
  }

--------------------------------------------------
--------------------------------------------------

{-|  

-}

data Src

  = SrcStdin
  | SrcFile  FilePath
  | SrcLines [Text]

  deriving stock    (Show,Read,Eq,Ord,Generic)
  deriving anyclass (NFData,Hashable)

--------------------------------------------------

{-|  

-}

data Dst

  = DstStdout
  | DstFile   FilePath

  deriving stock    (Show,Read,Eq,Ord,Lift,Generic)
  deriving anyclass (NFData,Hashable)

--------------------------------------------------
--------------------------------------------------
{- Notes / Old Code

--------------------------------------------------

  | FileVariable                -- Mod
  | DirectoryVariable           -- 

--------------------------------------------------



-}
--------------------------------------------------