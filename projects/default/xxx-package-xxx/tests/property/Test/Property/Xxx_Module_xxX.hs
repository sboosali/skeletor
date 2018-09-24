{-# LANGUAGE OverloadedStrings #-}

--------------------------------------------------
--------------------------------------------------

{-|



-}

module Test.Property.Xxx_Module_xxX where

--------------------------------------------------
--------------------------------------------------

import Xxx_Module_xxX
--import Internal.Xxx_Module_xxX

--------------------------------------------------

-- import qualified "" _ as _
-- import           "base" _

--------------------------------------------------

import           "hedgehog" Hedgehog
import qualified "hedgehog" Hedgehog.Gen   as Gen
import qualified "hedgehog" Hedgehog.Range as Range

--------------------------------------------------

import Prelude_Xxx_PackageUnderscores_xxX

--------------------------------------------------
--------------------------------------------------

{-| 

-}

prop_reverse_involutive :: Property
prop_reverse_involutive = property $ do

    xs <- forAll $ Gen.list (Range.linear 0 100) Gen.alpha
    reverse (reverse xs) === xs

--------------------------------------------------

{-| 

-}

--------------------------------------------------
--------------------------------------------------
{- Notes



HEDGEHOG (PACKAGE)

.

http://hackage.haskell.org/package/hedgehog/

http://reddit.com/r/haskell/comments/646k3d/ann_hedgehog_property_testing/

hedgehog: a (modern) property-based testing system.

.

Unlike QuickCheck, Hedgehog uses IntegratedShrinking, so shrinks obey the invariants of generated values by construction.

.

# Features

- Integrated Shrinking

shrinks obey invariants, by construction.


- Monadic Generators 

generators allow monadic effects.

- rich Range Combinators

Range combinators give better control over the scope and shrinking of generated numbers and the size of generated collections.


- Diffing

Equality and roundtrip assertions show a diff, instead of the two unequal values.


- Source Mapping

Generated values and the source of failures are shown inline with the source code for a property. (GHC 8 only).


- Discovery Macro

Template Haskell test runner, which executes properties concurrently.


- No Instances

No Arbitrary instances, and therefore no orphans. Separate your test code from your production code.

.

e.g.

``haskell
newtype PropertyT m a ≈ TestT (GenT m) a

data Tree m a =
  Tree (m (Node m a)

data Node m a =
  Node a [Tree m a]

data Gen m a =
  Gen (Seed -> Tree m a)
```

.

# module Hedgehog.Gen


## Gen.integral

Generates a random integral number in the given [inclusive,inclusive] range.

When the generator tries to shrink, it will shrink towards the origin of the specified Range.


### type

```haskell
integral 
  :: (MonadGen m, Integral a)
  => Range a -> m a
```

### e.g.

For example, the following generator: will produce a number between 1970 and 2100; but will shrink towards 2000:


```haskell
integral (Range.constantFrom 2000 1970 2100) :: Gen Int
```


## Gen.enumBounded

Generates a random value from a bounded enumeration.

This generator shrinks towards minBound.


### type

```haskell
enumBounded
  :: (MonadGen m, Enum a, Bounded a) 
  => m a
```

### e.g.

```haskell
enumBounded :: Gen Bool
```

### Specializations

#### Gen.bool

```haskell
bool :: Gen Bool
bool = enumBounded
```


## Gen.<char>

### Gen.digit

Generates an ASCII digit: '0'..'9'


digit :: MonadGen m => m Char 


### Gen.hexit

Generates an ASCII hexit: '0'..'9', 'a'..'f', 'A'..'F'

hexit :: MonadGen m => m Char 


### Gen.alphaNum

Generates an ASCII letter or digit: 'a'..'z', 'A'..'Z', '0'..'9'


alphaNum :: MonadGen m => m Char


### Gen.ascii

Generates an ASCII character: '\0'..'\127'


ascii :: MonadGen m => m Char 


### Gen.latin1

Generates a Latin-1 character: '\0'..'\255'


latin1 :: MonadGen m => m Char 

### Gen.unicode

Generates a Unicode character, excluding noncharacters and invalid standalone surrogates: '\0'..'\1114111' (excluding '\55296'..'\57343')


unicode :: MonadGen m => m Char 




.

# module Hedgehog.Range


## Range.constantFrom

### type

```haskell
constantFrom :: a -> a -> a -> Range a
```

### value

```haskell
constantFrom z x y =
  Range z $ \_ -> (x, y)
```

### Arguments

* Origin (the value produced when the size parameter is 0).
* Lower bound (the bottom of the range when the size parameter is 99).
* Upper bound (the top of the range when the size parameter is 99).



## Range.linearFrom

Construct a range which scales the bounds (linearly) relative to the size parameter.

### type

```haskell
linearFrom :: a -> a -> a -> Range a
```


## Range.exponentialFrom

Construct a range which scales the bounds **exponentially** relative to the size parameter.

### type

```haskell
linearFrom :: (Integral a) => a -> a -> a -> Range a
```

.






-}