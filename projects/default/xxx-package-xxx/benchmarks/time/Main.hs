--------------------------------------------------
--------------------------------------------------

import Bench.Xxx_Module_xxX (benchmarks)

--------------------------------------------------

import Prelude

--------------------------------------------------

import qualified "criterion" Criterion.Main  as Criterion
import qualified "criterion" Criterion.Types as Criterion

--------------------------------------------------
--------------------------------------------------

main :: IO ()
main = do

  putStrLn "----------------------------------------"
  putStrLn "[bench:xxx-package-xxx:time] TODO"
  putStrLn "----------------------------------------"

  Criterion.defaultMainWith config benchmarks

  where

  config :: Criterion.Config
  config = Criterion.defaultConfig
    { Criterion.timeLimit = 1
    }

--------------------------------------------------
--------------------------------------------------