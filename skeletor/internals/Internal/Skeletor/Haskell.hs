--------------------------------------------------
--------------------------------------------------

{-| ("Internal modules" are prefixed with @Internal.@)

(Don't import them, they are invisible to the @PVP@ (Package Versioning Policy))

(They are exposed for testing and documentation.)

-}

module Internal.Skeletor.Haskell where

--------------------------------------------------
--------------------------------------------------

import qualified "containers" Data.Set as Set
import           "containers" Data.Set (Set)

--------------------------------------------------

import           "base" Data.Functor.Contravariant (Predicate(..))

--------------------------------------------------
--------------------------------------------------

import Prelude_skeletor

--------------------------------------------------
--------------------------------------------------

any_Set :: forall a. (Ord a) => Predicate a -> Set a -> Bool
any_Set (Predicate p) xs = Set.foldr' go False xs
  where

  go :: a -> Bool -> Bool
  go x b = b || p x

--------------------------------------------------

all_Set :: forall a. (Ord a) => Predicate a -> Set a -> Bool
all_Set (Predicate p) xs = Set.foldr' go True xs
  where

  go :: a -> Bool -> Bool
  go x b = b && p x

--------------------------------------------------
--------------------------------------------------

{-| 

-}

--------------------------------------------------

{-| 

-}

--------------------------------------------------
--------------------------------------------------