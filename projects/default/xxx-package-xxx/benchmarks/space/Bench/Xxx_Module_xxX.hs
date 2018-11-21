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

-- import qualified "" _ as _

--------------------------------------------------

-- import           "base" _

--------------------------------------------------

import qualified "weigh" Weigh as Weigh

--------------------------------------------------
-- Exports ---------------------------------------
--------------------------------------------------

{-| 

-}

benchmarks :: Weigh.Weigh ()
benchmarks = do

  Weigh.func "integers count 0"   count 0
  Weigh.func "integers count 1"   count 1
  Weigh.func "integers count 2"   count 2
  Weigh.func "integers count 3"   count 3
  Weigh.func "integers count 10"  count 10
  Weigh.func "integers count 100" count 100

  Weigh.func "integers count 1,000,000" count 1000000

  where

  count :: Integer -> ()
  count 0 = ()
  count a = count (a - 1)

--------------------------------------------------
-- Benchmarks ------------------------------------
--------------------------------------------------

--------------------------------------------------
{- Notes -----------------------------------------

----------------------------------------

documentation:

  func
    :: NFData a

    => String	
    ^ Name of the case.

    -> (b -> a)	
    ^ Function that does some action to measure.

    -> b	
    ^ Argument to that function.

    -> Weigh ()	 
    ^ Weigh a function applied to an argument.

----------------------------------------

example output:

    $ time cabal new-bench "xxx-package-xxx:space"

    Running 1 benchmarks...
    Benchmark space: RUNNING...
    
    Case                Allocated  GCs
    integers count 0            0    0
    integers count 1           32    0
    integers count 2           64    0
    integers count 3           96    0
    integers count 10         320    0
    integers count 100      3,200    0

    Benchmark space: FINISH
    
    real    0m3.997s
    user    0m3.804s
    sys     0m0.372s

----------------------------------------

-------------------------------------------------}
