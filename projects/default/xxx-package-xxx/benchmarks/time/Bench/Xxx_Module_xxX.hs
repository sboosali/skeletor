-- {-# LANGUAGE RankNTypes       #-}
-- {-# LANGUAGE FlexibleContexts #-}
-- {-# LANGUAGE DeriveFunctor    #-}
-- {-# LANGUAGE BangPatterns     #-}

--------------------------------------------------
--------------------------------------------------

{-|



-}

module Bench.Xxx_Module_xxX

  ( benchmarks
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

-- import qualified Data.ByteString     as BS
-- import qualified Data.Foldable       as F
-- import qualified Data.HashMap.Lazy   as HM
-- import qualified Data.Map            as M
-- import qualified Data.Sequence       as S
-- import qualified Data.Vector         as V

--------------------------------------------------

--import qualified "deepseq" Control.DeepSeq as DeepSeq
--import           "deepseq" Control.DeepSeq ((<$!!>))

--------------------------------------------------

-- import qualified "" _ as _

--------------------------------------------------

-- import           "base" _

--------------------------------------------------

import qualified "criterion" Criterion as Bench

--------------------------------------------------
-- Exports ---------------------------------------
--------------------------------------------------

{-| 

-}

benchmarks :: [Bench.Benchmark]
benchmarks =

  [ Bench.bgroup "foo"

      [ Bench.bench "1234567"
                    (Bench.nf (fmap foo) l)
      ]

  ]

  where

  foo :: Int -> Int
  foo x = x + 1

  l = [0..1234567] :: [Int]

  -- b = BS.pack $ map fromIntegral l
  -- h = HM.fromList $ zip l l
  -- m = M.fromList $ zip l l
  -- s = S.fromList l
  -- u = U.fromList l
  -- v = V.fromList l

--------------------------------------------------
-- Benchmarks ------------------------------------
--------------------------------------------------

--------------------------------------------------
{- Notes -----------------------------------------

----------------------------------------

documentation:

  nf :: NFData b => (a -> b) -> a -> Benchmarkable

  ^ Apply an argument to a function, and evaluate the result to normal form (NF).

----------------------------------------

example output:

    $ time cabal new-bench "xxx-package-xxx:time"

    Running 1 benchmarks...
    Benchmark time: RUNNING...

    benchmarking foo/1234567
    time                 65.76 ms   (63.54 ms .. 67.03 ms)
                         1.000 R²   (0.999 R² .. 1.000 R²)
    mean                 64.50 ms   (63.82 ms .. 65.02 ms)
    std dev              753.2 μs   (422.0 μs .. 1.132 ms)

    variance introduced by outliers: 16% (moderately inflated)

    Benchmark time: FINISH
    
    real     0m8.097s
    user     0m11.212s
    sys      0m2.288s
    
----------------------------------------

-------------------------------------------------}
