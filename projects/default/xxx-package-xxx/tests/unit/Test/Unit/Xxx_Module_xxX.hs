--------------------------------------------------
--------------------------------------------------

{-|

-}

module Test.Unit.Xxx_Module_xxX

  ( tests
  ) where

--------------------------------------------------
--------------------------------------------------

--import Xxx_Module_xxX
--import Internal.Xxx_Module_xxX

--------------------------------------------------

import Prelude_xxx_package_xxx

--------------------------------------------------
-- Imports ---------------------------------------
--------------------------------------------------

import qualified "tasty"       Test.Tasty       as Tasty
import qualified "tasty-hunit" Test.Tasty.HUnit as Tasty
import           "tasty-hunit" Test.Tasty.HUnit ((@=?), (@?))

--import           "HUnit" Test.HUnit ((~:), (~=?), (@?))

--------------------------------------------------

-- import qualified "" _ as _

--------------------------------------------------

import Prelude

--------------------------------------------------
-- Exports ---------------------------------------
--------------------------------------------------

{-| 

-}

tests :: Tasty.TestTree
tests = Tasty.testGroup "Unit Tests"

  [ Tasty.testCase "foo (symbolic)" $
  
      test1_symbolic
      
  , Tasty.testCase "foo (alphanumeric)" $
  
      test1_alphanumeric

  , Tasty.testCase "bar" $
  
      bar (bar True)  @?  "bar ∘ bar ≡ id"

  ]

  where
  bar = Prelude.not

--------------------------------------------------
-- Tests -----------------------------------------
--------------------------------------------------

test1_symbolic :: Tasty.Assertion
test1_symbolic =

  (1,2) @=? (foo 3)

  where
  foo x = (x - 2, x - 1)

--------------------------------------------------

test1_alphanumeric :: Tasty.Assertion
test1_alphanumeric = do

  Tasty.assertEqual "(foo 3)" (1,2) (foo 3)

  where
  foo x = (x - 2, x - 1)

--------------------------------------------------
--------------------------------------------------
{- NOTES

(@=?) ≡ assertEqual ""

(@?) ≡ flip assertBool

-}
--------------------------------------------------