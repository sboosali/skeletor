--------------------------------------------------
--------------------------------------------------

{-|



-}

module Test.Unit.HaskellProject where

--------------------------------------------------

--import HaskellProject
--import Internal.HaskellProject

--------------------------------------------------

import qualified "HUnit" Test.HUnit as HUnit
--import           "HUnit" Test.HUnit         ((~:), (~=?), (@?))

--------------------------------------------------

-- import qualified "" _ as _

--------------------------------------------------

import Prelude_haskell_project

--------------------------------------------------
--------------------------------------------------

tests :: HUnit.Test
tests = HUnit.TestList
  [ HUnit.TestLabel "test1" test1
  , HUnit.TestLabel "test2" test2
  ]

--------------------------------------------------

test1 :: HUnit.Test
test1 = HUnit.TestCase $ do
  HUnit.assertEqual "(1 + 2)" 3 (1 + 2)

test2 :: HUnit.Test
test2 = HUnit.TestCase $ do
  HUnit.assertEqual "(1 + 2)" 0 (1 + 2)

--------------------------------------------------
--------------------------------------------------
{- NOTES

------------------------------
[1] identifiers
------------------------------

   tests = TestList [TestLabel "test1" test1, TestLabel "test2" test2]

   test1 = TestCase (assertEqual "for (foo 3)," (1,2) (foo 3))

   test2 = TestCase (do (x,y) <- partA 3
                        assertEqual "for the first result of partA," 5 x
                        b <- partB y
                        assertBool ("(partB " ++ show y ++ ") failed") b)

------------------------------
[2] symbols
------------------------------

tests = HUnit.test
  [ "test1" ~: test1
  , "test2" ~: test2
  ]

test1 = "(foo 3)" ~: (1,2) ~=? (foo 3)

test2 = do
  (x, y) <- partA 3
  assertEqual "for the first result of partA," 5 x
  partB y @? "(partB " ++ show y ++ ") failed"

------------------------------
------------------------------

-}
--------------------------------------------------