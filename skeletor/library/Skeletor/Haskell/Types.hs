{-# LANGUAGE BinaryLiterals #-}

--------------------------------------------------
--------------------------------------------------

{-|



-}

module Skeletor.Haskell.Types where

--------------------------------------------------
-- Imports ---------------------------------------
--------------------------------------------------

import "these" Data.These (These(..))

--------------------------------------------------

import "filemanip" System.FilePath.GlobPattern (GlobPattern)

--------------------------------------------------

import qualified "unordered-containers" Data.HashMap.Lazy as HashMap
import           "unordered-containers" Data.HashMap.Lazy (HashMap)

--------------------------------------------------
--------------------------------------------------

import qualified "containers" Data.Map as Map
import           "containers" Data.Map (Map)

--------------------------------------------------

import qualified "containers" Data.Set as Set
import           "containers" Data.Set (Set)

--------------------------------------------------

import qualified "base" Data.Foldable as Fold

--------------------------------------------------
--------------------------------------------------

import Prelude_skeletor

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

newtype FileTree = FileTree

  (Map FilePath String)         -- TODO Text

  deriving stock    (Show,Read,Generic)
  deriving newtype  (Eq,Ord,Semigroup,Monoid)
  deriving newtype  (NFData)

instance IsList FileTree where
  type Item FileTree = (FilePath, String)
  fromList = Map.fromList > coerce
  toList   = coerce       > Map.toList

--------------------------------------------------

{-|

@
≡ 'Map.empty'
@

-}

emptyFileTree :: FileTree
emptyFileTree = FileTree Map.empty

--------------------------------------------------
--------------------------------------------------

{-|

'ChiaroscuroFilters' is a pair of filters (a blacklist and a whitelist)
of 'GlobPattern's.

@≡ 'These' 'emptyBlacklist' 'fullWhitelist'@

-}

data ChiaroscuroFilters = ChiaroscuroFilters

  { chiaroscuroBlacklist :: !Blacklist
  , chiaroscuroWhitelist :: !Whitelist
  }

  deriving stock    (Generic)
  deriving stock    (Show, Read)
  deriving stock    (Eq, Ord)

  deriving anyclass (NFData{-, Hashable-})

--------------------------------------------------

-- | @≡ 'noChiaroscuroFilters'@

instance Default ChiaroscuroFilters where

  def = noChiaroscuroFilters

--------------------------------------------------

-- | @≡ 'noChiaroscuroFilters'@

instance Monoid ChiaroscuroFilters where

  mempty = noChiaroscuroFilters

--------------------------------------------------

-- | @'(<>)'@ is pointwise.

instance Semigroup ChiaroscuroFilters where

  ChiaroscuroFilters{ chiaroscuroBlacklist = lb, chiaroscuroWhitelist = lw } <> ChiaroscuroFilters{ chiaroscuroBlacklist = rb, chiaroscuroWhitelist = rw } =

    ChiaroscuroFilters{chiaroscuroBlacklist,chiaroscuroWhitelist}

    where

    chiaroscuroBlacklist = lb <> rb
    chiaroscuroWhitelist = lw <> rw

--------------------------------------------------

{-|

@[ blacklist "A", whitelist "X", blacklist "B", whitelist "Y" ]
@

@ChiaroscuroFilters [ "A", "B" ] [ "X", "Y" ]
@

@[] :: ChiaroscuroFilters@ means "nothing is filtered".

-}

instance IsList ChiaroscuroFilters where

  type Item ChiaroscuroFilters = ChiaroscuroFilter

  fromList = fmap go > Fold.fold

    where
    go = either blacklistFilter whitelistFilter

  toList (ChiaroscuroFilters{chiaroscuroBlacklist,chiaroscuroWhitelist}) =

    fromBlacklist chiaroscuroBlacklist <> fromWhitelist chiaroscuroWhitelist

    where
    fromBlacklist (Blacklist bs) = bs & (Set.toList > fmap blacklist)

    fromWhitelist = \case
      Whitelist Nothing   -> []
      Whitelist (Just ws) -> ws & (Set.toList > fmap whitelist)

--------------------------------------------------

{-| Everything is permitted and nothing is restricted.

@≡ 'ChiaroscuroFilters' 'emptyBlacklist' 'fullWhitelist'@

-}

noChiaroscuroFilters :: ChiaroscuroFilters
noChiaroscuroFilters = ChiaroscuroFilters emptyBlacklist fullWhitelist

--------------------------------------------------

blacklistFilter :: GlobPattern -> ChiaroscuroFilters
blacklistFilter = Set.singleton > go
  where

  go x = ChiaroscuroFilters (Blacklist x) mempty

--------------------------------------------------

whitelistFilter :: GlobPattern -> ChiaroscuroFilters
whitelistFilter = Set.singleton >
  go
  where

  go x = ChiaroscuroFilters mempty (Whitelist (Just x))

--------------------------------------------------
--------------------------------------------------

{-| a 'ChiaroscuroFilter' is a filter (a 'GlobPattern'),
either blacklisted or whitelisted.

-}

type ChiaroscuroFilter = (Either GlobPattern GlobPattern)

--------------------------------------------------

{-| a blacklisted pattern.

= Implementation

@≡ 'Left'@

-}

blacklist :: GlobPattern -> ChiaroscuroFilter
blacklist = Left

--------------------------------------------------

{-| a whitelisted pattern.

= Implementation

@≡ 'Right'@

-}

whitelist :: GlobPattern -> ChiaroscuroFilter
whitelist = Right

--------------------------------------------------
--------------------------------------------------

{-|

Merging two whitelists takes their intersection. That is,
something is permitted only if it's permitted by both whitelists.

-}

newtype Whitelist = Whitelist

  (Maybe (Set GlobPattern))

  deriving stock    (Generic)
  deriving stock    (Show, Read)
  deriving newtype  (Eq, Ord)

  --deriving newtype  (Semigroup, Monoid)
  deriving newtype  (NFData{-, Hashable-})

--------------------------------------------------

-- | @≡ 'fullWhitelist'@

instance Monoid Whitelist where

  mempty = fullWhitelist

--------------------------------------------------

-- | @('<>') ≡ 'Set.intersection'@

instance Semigroup Whitelist where

  Whitelist Nothing <> ws = ws
  ws <> Whitelist Nothing = ws

  Whitelist (Just ws) <> Whitelist (Just vs) =

    Whitelist (Just (ws `Set.intersection` vs))

--------------------------------------------------

{-

@[] :: Whitelist@ means "nothing is permitted"
(N.B. it's not 'fullWhitelist').

-}

instance IsList Whitelist where

  type Item (Whitelist) = GlobPattern

  fromList = Set.fromList > Just > coerce
  toList   = coerce > maybe Set.empty id > Set.toList -- TODO «toList» is ill-defined; «fullWhitelist» "should" be all (infinite) possible strings.

--------------------------------------------------

-- | a 'Whitelist' representing everything being permitted.

fullWhitelist :: Whitelist
fullWhitelist = Whitelist Nothing

--------------------------------------------------
--------------------------------------------------

{-| 

Merging two blacklists takes their union. That is,
something is restricted if it's in either blacklist.

-}

newtype Blacklist = Blacklist

  (Set GlobPattern)

  deriving stock    (Generic)
  deriving stock    (Show, Read)
  deriving newtype  (Eq, Ord)

  --deriving newtype  (Semigroup, Monoid)
  deriving newtype  (NFData{-, Hashable-})

--------------------------------------------------

-- | @≡ 'emptyBlacklist'@

instance Monoid Blacklist where

  mempty = emptyBlacklist

--------------------------------------------------

-- | @('<>') ≡ 'Set.union'@

instance Semigroup Blacklist where

  (Blacklist ws) <> (Blacklist vs) = Blacklist (ws `Set.union` vs)

--------------------------------------------------

{-|
@([] :: Blacklist) == 'emptyBlacklist'@
-}

instance IsList Blacklist where

  type Item (Blacklist) = GlobPattern

  fromList = Set.fromList > coerce
  toList   = coerce > Set.toList

--------------------------------------------------

-- | a 'Blacklist' representing nothing being restricted.

emptyBlacklist :: Blacklist
emptyBlacklist = Blacklist Set.empty

--------------------------------------------------
--------------------------------------------------
{- Notes / Old Code

--------------------------------------------------

  | FileVariable                -- Mod
  | DirectoryVariable           -- 

--------------------------------------------------



-}
--------------------------------------------------