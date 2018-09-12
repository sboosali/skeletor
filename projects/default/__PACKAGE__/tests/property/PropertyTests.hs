{-# LANGUAGE OverloadedStrings #-}

--------------------------------------------------
--------------------------------------------------

import Test.Property.__MODULE__

--------------------------------------------------

-- import "hedgehog" Hedgehog

--------------------------------------------------

import qualified "tasty"          Test.Tasty          as Tasty
import qualified "tasty-hedgehog" Test.Tasty.Hedgehog as Tasty

--------------------------------------------------

import "base" Prelude

--------------------------------------------------
--------------------------------------------------

main = do
  putStrLn "[TODO] test:__PACKAGE__:property"

  Tasty.defaultMain tests

--------------------------------------------------

tests :: Tasty.TestTree
tests =  Tasty.testGroup "Property Tests"

  [ Tasty.testProperty "reverse involutive" prop_reverse_involutive
  -- , Tasty.testProperty "sort idempotent"    prop_sort_idempotent
  ]

-- tests :: IO Bool
-- tests =
--   checkParallel $ Group "Test.Example" [
--       ("prop_reverse", prop_reverse)
--     ]

--------------------------------------------------
--------------------------------------------------
{-

testProperty :: TestName -> Property -> TestTree





-}
