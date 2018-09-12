{-# LANGUAGE OverloadedStrings #-}

--------------------------------------------------
--------------------------------------------------

{-|



-}

module Test.Property.__MODULE__ where

--------------------------------------------------
--------------------------------------------------

import __MODULE__
--import Internal.__MODULE__

--------------------------------------------------

-- import qualified "" _ as _
-- import           "base" _

--------------------------------------------------

import           "hedgehog" Hedgehog
import qualified "hedgehog" Hedgehog.Gen   as Gen
import qualified "hedgehog" Hedgehog.Range as Range

--------------------------------------------------

import Prelude___PACKAGE_UNDERSCORES__

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