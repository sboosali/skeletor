{-# LANGUAGE BinaryLiterals #-}

--------------------------------------------------
--------------------------------------------------

{-|



-}

module Skeletor.Haskell.Chiaroscuro.Types where

--------------------------------------------------
-- Imports ---------------------------------------
--------------------------------------------------

--------------------------------------------------
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

--------------------------------------------------
--------------------------------------------------

{-|

'ChiaroscuroFilters' is a pair (i.e. a blacklist and a whitelist)
of filters on 'GlobPattern's.

@≡ 'These' 'emptyBlacklist' 'fullWhitelist'@

-}

data ChiaroscuroFilters a = ChiaroscuroFilters

  { chiaroscuroBlacklist :: !(Blacklist a)
  , chiaroscuroWhitelist :: !(Whitelist a)
  }

  deriving stock    (Generic)
  deriving stock    (Show, Read)
  deriving stock    (Eq, Ord)

  deriving anyclass (NFData{-, Hashable-})

--------------------------------------------------

-- | @≡ 'noChiaroscuroFilters'@

instance Default (ChiaroscuroFilters a) where

  def = noChiaroscuroFilters

--------------------------------------------------

-- | @≡ 'noChiaroscuroFilters'@

instance (Ord a) => Monoid (ChiaroscuroFilters a) where

  mempty = noChiaroscuroFilters

--------------------------------------------------

-- | @'(<>)'@ is pointwise.

instance (Ord a) => Semigroup (ChiaroscuroFilters a) where

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

instance (Ord a) => IsList (ChiaroscuroFilters a) where

  ----------------------------

  type Item (ChiaroscuroFilters a) = ChiaroscuroFilter a

  ----------------------------

  fromList = Fold.foldMap singletonChiaroscuroFilters

  ----------------------------

  toList (ChiaroscuroFilters{chiaroscuroBlacklist,chiaroscuroWhitelist}) =

    fromBlacklist chiaroscuroBlacklist <> fromWhitelist chiaroscuroWhitelist

    where

    fromBlacklist (Blacklist bs) =
      bs & (Set.toList > fmap Blacklisted)

    fromWhitelist = \case
      Whitelist Nothing   -> []
      Whitelist (Just ws) -> ws & (Set.toList > fmap Whitelisted)

--------------------------------------------------

{-| Everything is permitted and nothing is restricted.

@≡ 'ChiaroscuroFilters' 'emptyBlacklist' 'fullWhitelist'@

-}

noChiaroscuroFilters :: ChiaroscuroFilters a
noChiaroscuroFilters = ChiaroscuroFilters emptyBlacklist fullWhitelist

--------------------------------------------------

singletonChiaroscuroFilters :: (Ord a) => ChiaroscuroFilter a -> ChiaroscuroFilters a
singletonChiaroscuroFilters = \case

  Blacklisted x -> fromBlacklisted x
  Whitelisted x -> fromWhitelisted x

  where

  fromBlacklisted x = mempty { chiaroscuroBlacklist = Blacklist       (Set.singleton x)  }

  fromWhitelisted x = mempty { chiaroscuroWhitelist = Whitelist (Just (Set.singleton x)) }

--------------------------------------------------
--------------------------------------------------

{-| a 'ChiaroscuroFilter' is a single value marked as either:

* 'Blacklisted'; or
* 'Whitelisted'.

-}

data ChiaroscuroFilter a

  = Blacklisted !a
  | Whitelisted !a

  deriving stock    (Functor,Foldable,Traversable)

  deriving stock    (Generic,Lift)
  deriving stock    (Show,Read,Eq,Ord)
  deriving anyclass (NFData,Hashable)

--------------------------------------------------
--------------------------------------------------

{-|

Merging two whitelists takes their intersection. That is,
something is permitted only if it's permitted by both whitelists.

-}

newtype Whitelist a = Whitelist

  (Maybe (Set a))

  deriving stock    (Generic)
  deriving stock    (Show, Read)
  deriving newtype  (Eq, Ord)

  --deriving newtype  (Semigroup, Monoid)
  deriving newtype  (NFData{-, Hashable-})

--------------------------------------------------

-- | @≡ 'fullWhitelist'@

instance (Ord a) => Monoid (Whitelist a) where

  mempty = fullWhitelist

--------------------------------------------------

-- | @('<>') ≡ 'Set.intersection'@

instance (Ord a) => Semigroup (Whitelist a) where

  Whitelist Nothing <> ws = ws
  ws <> Whitelist Nothing = ws

  Whitelist (Just ws) <> Whitelist (Just vs) =

    Whitelist (Just (ws `Set.intersection` vs))

--------------------------------------------------

{-

@[] :: Whitelist@ means "nothing is permitted"
(N.B. it's not 'fullWhitelist').

-}

instance (Ord a) => IsList (Whitelist a) where

  type Item (Whitelist a) = a

  fromList = Set.fromList > Just > coerce
  toList   = coerce > maybe Set.empty id > Set.toList -- TODO «toList» is ill-defined; «fullWhitelist» "should" be all (infinite) possible strings.

--------------------------------------------------

-- | a 'Whitelist' representing everything being permitted.

fullWhitelist :: Whitelist a
fullWhitelist = Whitelist Nothing

--------------------------------------------------
--------------------------------------------------

{-| 

Merging two blacklists takes their union. That is,
something is restricted if it's in either blacklist.

-}

newtype Blacklist a = Blacklist

  (Set a)

  deriving stock    (Generic)
  deriving stock    (Show, Read)
  deriving newtype  (Eq, Ord)

  --deriving newtype  (Semigroup, Monoid)
  deriving newtype  (NFData{-, Hashable-})

--------------------------------------------------

-- | @≡ 'emptyBlacklist'@

instance (Ord a) => Monoid (Blacklist a) where

  mempty = emptyBlacklist

--------------------------------------------------

-- | @('<>') ≡ 'Set.union'@

instance (Ord a) => Semigroup (Blacklist a) where

  (Blacklist ws) <> (Blacklist vs) = Blacklist (ws `Set.union` vs)

--------------------------------------------------

{-|
@([] :: Blacklist) == 'emptyBlacklist'@
-}

instance (Ord a) => IsList (Blacklist a) where

  type Item (Blacklist a) = a

  fromList = Set.fromList > coerce
  toList   = coerce > Set.toList

--------------------------------------------------

-- | a 'Blacklist' representing nothing being restricted.

emptyBlacklist :: Blacklist a
emptyBlacklist = Blacklist Set.empty

--------------------------------------------------
--------------------------------------------------

{-|

-}


--------------------------------------------------
--------------------------------------------------