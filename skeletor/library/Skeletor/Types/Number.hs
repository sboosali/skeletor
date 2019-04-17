--------------------------------------------------
-- Extensions ------------------------------------
--------------------------------------------------

{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE InstanceSigs     #-}

--------------------------------------------------

{- |

Types:

* `Number`

Optics:

* `_Z`
* `_Q`

-}

module Skeletor.Types.Number

  ( module Skeletor.Types.Number

  , Integer
  , Rational
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

import           "base" Data.Ratio

  
--------------------------------------------------

import qualified "base" Prelude

--------------------------------------------------
-- Types -----------------------------------------
--------------------------------------------------

{- | A /“stringly-typed”/ number.

Naming:

* `Z` — @ℤ@ is the mathematical notation for “the integers” (abbreviates /"Zahlen"/, German for /"numbers"/).
* `Q` — @ℚ@ is the mathematical notation for “the rationals” (abbreviates /"quoziente"/, Italian for /"quotient"/). 
* `S` — for “String”.

== Examples

>>> 1 :: Number
Z 1
>>> 1.5 :: Number
Q (3 % 2)
>>> :set -XOverloadedStrings
>>> "one" :: Number
S "one"

-}

data Number

  = Z Integer
  | Q Rational
  | S String

  deriving stock    (Show,Read,Eq,Ord)
  deriving stock    (Lift,Data,Generic)
  deriving anyclass (NFData,Hashable)

--------------------------------------------------

-- | @`S` ≈ fromString@
instance IsString Number where

  fromString :: String -> Number
  fromString = (S . fromString)

--------------------------------------------------

-- | @`Z` ≈ `fromInteger`@
instance Num Number where

  fromInteger :: Integer -> Number
  fromInteger = Z

  -- (+)    = _
  -- (-)    = _
  -- (*)    = _

  -- negate = _
  -- abs    = _
  -- signum = _

--------------------------------------------------

-- | @`Q` ≈ `fromRational`@
instance Fractional Number where

  fromRational :: Rational -> Number
  fromRational = Q

  -- (/)   = _Q
  -- recip = _Q

--------------------------------------------------
-- Functions -------------------------------------
--------------------------------------------------

--------------------------------------------------
-- Optics ----------------------------------------
--------------------------------------------------

{- | Prism into 'Z'.

== Examples

>>> (Z 1) ^? _Z
Just 1

>>> (Q 1.0) ^? _Z
Just 1

>>> (Q 2.5) ^? _Z
Nothing

>>> (S "1") ^? _Z
Just 1

>>> (S "2.5") ^? _Z
Nothing

>>> (S "one") ^? _Z
Nothing

-}

_Z :: Prism' Number Integer
_Z = L.prism' loosen tighten
  where

  loosen :: Integer -> Number
  loosen = Z

  tighten :: Number -> Maybe Integer
  tighten = \case

    Z z -> Just z
    Q q -> fromQ q
    S s -> (readMay @Integer)                             s
      <|> (readMay @Double   >=> (toRational >>> fromQ)) s
      <|> (readMay @Rational >=> fromQ)                  s

  fromQ :: Rational -> Maybe Integer
  fromQ q =
    let
      (z, remainder') = properFraction q
    in
      if remainder' == 0 then Just z else Nothing

--------------------------------------------------

{- | Prism into 'Q'.

== Examples

>>> (Z 1) ^? _Q
Just (1 % 1)

>>> (Q 1.0) ^? _Q
Just (1 % 1)

>>> (Q 2.5) ^? _Q
Just (5 % 2)

>>> (S "1") ^? _Q
Just (1 % 1)

>>> (S "2.5") ^? _Q
Just (5 % 2)

>>> (S "one") ^? _Q
Nothing

-}

_Q :: Prism' Number Rational
_Q = L.prism' loosen tighten
  where

  loosen :: Rational -> Number
  loosen = Q

  tighten :: Number -> Maybe Rational
  tighten = \case

    Z z -> Just (fromZ z)
    Q q -> Just q
    S s -> (fromZ      <$> (readMay @Integer) s)
      <|> (toRational <$> (readMay @Double)  s)
      <|> (readMay @Rational s)

  fromZ :: Integer -> Rational
  fromZ z = (z % 1)

--------------------------------------------------

{- | Isomorphism via 'S' (all 'Iso's are 'Prism's).

== Examples

>>> (Z 1) ^. _S
"1"

>>> (Q 1.0) ^. _S
"1"

>>> (Q 1.5) ^. _S
"1.5"

>>> (S "1") ^. _S
"1"

>>> (S "1.5") ^. _S
"1.5"

>>> (S "one") ^. _S
"one"

== /NOTE/

Whenever possible, `_S` shows `Q`s via the `Integer` `Show`
or `Double`'s `Show` instances
(i.e. not necessarily via its own `Rational`'s).

-}

_S :: Iso' Number String
_S = L.iso from into
  where

  into :: String -> Number
  into = S

  from :: Number -> String
  from = \case

    Z z -> show z
    Q q -> (showAsInteger q <|> showAsDouble q) & maybe (showAsRational q) id
    S s -> s

  showAsRational = show @Rational

  showAsDouble :: Rational -> Maybe String
  showAsDouble q =
    let
      d = fromRational q
    in
      if isNaN d
      then Nothing
      else Just (show @Double d)

  -- NOTE if out-of-bounds, show as Rational.

  -- TODO is checking against « isNaN » enough?

  showAsInteger :: Rational -> Maybe String
  showAsInteger q =
    let
      (z, remainder') = properFraction q
    in
      if remainder' == 0
      then Just (show @Integer z)
      else Nothing

--------------------------------------------------
-- Doctest ---------------------------------------
--------------------------------------------------

{- $setup

>>> import Data.Ratio   ( (%) )
>>> import Control.Lens ( (^?), (^.) )

-}

--------------------------------------------------
-- Notes -----------------------------------------
--------------------------------------------------
{-------------------------------------------------

« Data.Ratio »:

`Fractional` instances:

* `Rational` — /arbitrary-precision/ fractions.
* `Double`  — /double-precision/ floating-point numbers.

--------------------------------------------------

« Control.Lens.Prism »:

prism'        :: (b -> s) -> (s -> Maybe a) -> Prism s s a b
prism' @(a~b) :: (a -> s) -> (s -> Maybe a) -> Prism' s a

--------------------------------------------------

-------------------------------------------------}
--------------------------------------------------
-- EOF -------------------------------------------
--------------------------------------------------