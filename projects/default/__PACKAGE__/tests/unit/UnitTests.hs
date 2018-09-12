--------------------------------------------------
--------------------------------------------------

import Test.Unit.__MODULE__

--------------------------------------------------

import qualified "HUnit"       Test.HUnit       as HUnit

--------------------------------------------------

import qualified "tasty"       Test.Tasty       as Tasty
import qualified "tasty-hunit" Test.Tasty.HUnit as Tasty

--------------------------------------------------

import "base" Prelude

--------------------------------------------------

main = do
  putStrLn "[TODO] test:__PACKAGE__:unit"

  Tasty.defaultMain tests

--------------------------------------------------

tests :: Tasty.TestTree
tests = Tasty.testGroup "Unit Tests"

  [ Tasty.testCase "2+2=4" $
  
      2+2 HUnit.@?= 4
      
  , Tasty.testCase "7 is even" $
  
      HUnit.assertBool "Oops, 7 is odd" (even 7)
  ]

--------------------------------------------------
--------------------------------------------------