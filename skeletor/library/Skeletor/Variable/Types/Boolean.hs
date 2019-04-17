--------------------------------------------------
-- Extensions ------------------------------------
--------------------------------------------------

{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE InstanceSigs     #-}

--------------------------------------------------

{- | /Boolean-like/ template\/configuration variables.

Types:

* `Boolean`

Functions:

-}

module Skeletor.Variable.Types.Boolean

  ( module Skeletor.Variable.Types.Boolean

  , Bool
  , String

  ) where

--------------------------------------------------
-- Imports (Internal) -----------------------------
--------------------------------------------------

import Prelude_skeletor

--------------------------------------------------
-- Imports (External) ----------------------------
--------------------------------------------------

import qualified "lens" Control.Lens as L
import           "lens" Control.Lens ( Prism', Iso' )

--------------------------------------------------
-- Imports ---------------------------------------
--------------------------------------------------

import qualified "containers" Data.Map as Map

--------------------------------------------------

import qualified "text" Data.Text as Text

--------------------------------------------------

import           "base" Data.Ratio
  
--------------------------------------------------

import qualified "base" Prelude

--------------------------------------------------
-- Types -----------------------------------------
--------------------------------------------------

{- | A /“stringly-typed”/ boolean.

== Examples

>>> "f" :: Boolean
BooleanB False
>>> "t" :: Boolean
BooleanB True

>>> "0" :: Boolean
BooleanB False
>>> "1" :: Boolean
BooleanB True

>>> "false" :: Boolean
BooleanB False
>>> "true" :: Boolean
BooleanB True

>>> "no" :: Boolean
BooleanB False
>>> "yes" :: Boolean
BooleanB True

>>> "off" :: Boolean
BooleanB False
>>> "on" :: Boolean
BooleanB True

>>> "FALSE" :: Boolean
BooleanB False
>>> "TRUE" :: Boolean
BooleanB True

>>> "disabled" :: Boolean
BooleanB False
>>> "enabled" :: Boolean
BooleanB True

>>> "-1" :: Boolean
BooleanS "-1"
>>> "+1" :: Boolean
BooleanS "+1"

-}

data Boolean

  = BooleanB Bool
  | BooleanS String

  deriving stock    (Show,Read,Eq,Ord)
  deriving stock    (Lift,Data,Generic)
  deriving anyclass (NFData,Hashable)

--------------------------------------------------

-- | @`toBoolean` ≈ fromString@
instance IsString Boolean where

  fromString :: String -> Boolean
  fromString = toBoolean

--------------------------------------------------
-- Constants -------------------------------------
--------------------------------------------------

{- | Strings which alias booleans.

They are:

* @"0"@ — `False`
* @"1"@ — `True`

* @"f"@ — `False`
* @"t"@ — `True`

* @"false"@ — `False`
* @"true"@ — `True`

* @"no"@ — `False`
* @"yes"@ — `True`

* @"off"@ — `False`
* @"on"@ — `True`

* @"disabled"@ — `False`
* @"enabled"@ — `True`

-}

namedBools :: Map String Bool
namedBools = Map.fromList

  [ "0"-: False
  , "1"-: True

  , "f"-: False
  , "t"-: True

  , "false"-: False
  , "true"-: True

  , "no"-: False
  , "yes"-: True

  , "off"-: False
  , "on"-: True

  , "disabled"-: False
  , "enabled"-: True
{-
  , ""-: False
  , ""-: True

  , ""-: False
  , ""-: True

  , ""-: False
  , ""-: True
-}
  ]

--------------------------------------------------
-- Functions -------------------------------------
--------------------------------------------------

-- | Parse a `Boolean`.

toBoolean :: String -> Boolean
toBoolean s

  = boolByName s
  & maybe (BooleanS s) BooleanB

--------------------------------------------------

{- | Unpack a `Boolean`.

Output:

* @Just False@ represents `falsy`.
* @Just True@ represents `truthy`.
* @Nothing@ represents neither\/unknown.

-}

fromBoolean :: Boolean -> Maybe Bool
fromBoolean = \case

  BooleanB b -> Just b
  BooleanS s -> boolByName s

--------------------------------------------------

{- | Is it “falsy”.

== /NOTE/

A `Boolean` may be neither `falsy` nor `truthy`.

-}

falsy :: Boolean -> Bool
falsy = \case

  BooleanB b -> b == False
  BooleanS s -> boolByName s & maybe False (== False) 

--------------------------------------------------

{- | Is it “truthy”.

== /NOTE/

A `Boolean` may be neither `truthy` nor `falsy`.

-}

truthy :: Boolean -> Bool
truthy = \case

  BooleanB b -> b == True
  BooleanS s -> boolByName s & maybe False (== True) 

--------------------------------------------------
-- Utilities -------------------------------------
--------------------------------------------------

-- | Lookup `namedBools`.

boolByName :: String -> Maybe Bool
boolByName s = Map.lookup (lowercase s) namedBools
  where

  lowercase = fmap toLower

--------------------------------------------------
-- Optics ----------------------------------------
--------------------------------------------------

--------------------------------------------------
-- Doctest ---------------------------------------
--------------------------------------------------

{- $setup

>>> :set -XOverloadedStrings

-}

--------------------------------------------------
-- Notes -----------------------------------------
--------------------------------------------------
{-------------------------------------------------

TODO?

>>> 0 :: Boolean
BooleanB False
>>> 1 :: Boolean
BooleanB True

--------------------------------------------------

TODO?

Optics:

* `bBool`
* `bTrue`
* `bFalse`
* `bString`

--------------------------------------------------

-------------------------------------------------}
--------------------------------------------------
-- EOF -------------------------------------------
--------------------------------------------------