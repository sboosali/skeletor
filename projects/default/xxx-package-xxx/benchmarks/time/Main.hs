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
  putStrLn "[TODO] bench:xxx-package-xxx:time"
  putStrLn "----------------------------------------"

  Criterion.defaultMainWith config benchmarks

  where

  config :: Criterion.Config
  config = Criterion.defaultConfig
    { Criterion.timeLimit = 1
    }

--------------------------------------------------
--------------------------------------------------