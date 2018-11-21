--------------------------------------------------
--------------------------------------------------

import Bench.Xxx_Module_xxX (benchmarks)

--------------------------------------------------

import qualified "weigh" Weigh as Weigh

--------------------------------------------------

import "base" Prelude

--------------------------------------------------
--------------------------------------------------

main :: IO ()
main = do

  putStrLn "----------------------------------------"
  putStrLn "[bench:xxx-package-xxx:space] TODO"
  putStrLn "----------------------------------------"

  Weigh.mainWith $ do
    Weigh.setConfig config
    benchmarks

  where

  config :: Weigh.Config
  config = Weigh.defaultConfig
    { Weigh.configFormat = Weigh.Markdown
    }

--------------------------------------------------
--------------------------------------------------