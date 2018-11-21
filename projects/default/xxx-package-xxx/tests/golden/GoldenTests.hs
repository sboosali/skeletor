--------------------------------------------------

import Test.Golden.Xxx_Module_xxX (goldenTests)

import qualified "tasty" Test.Tasty as Tasty

import Prelude

--------------------------------------------------

main :: IO ()
main = Tasty.defaultMain =<< goldenTests

--------------------------------------------------