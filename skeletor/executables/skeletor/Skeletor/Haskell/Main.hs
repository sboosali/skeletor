--------------------------------------------------
--------------------------------------------------

{-|

'Skeletor.Haskell.Main' glues the other modules together:

* 'Skeletor.Haskell.Options': the command-line options (parsing, defaulting, etc).
* 'Skeletor.Haskell.Program': the @main@ program (the logic, filesystem interaction, etc).

-}

module Skeletor.Haskell.Main

  ( main
  , module Skeletor.Haskell.Options
  , module Skeletor.Haskell.Program
  ) where

--------------------------------------------------
--------------------------------------------------

import Skeletor.Haskell.Options
import Skeletor.Haskell.Program

--------------------------------------------------

import Prelude_exe

--------------------------------------------------
--------------------------------------------------

{-| 

-}

main :: IO ()
main = do
  config <- getConfig
  program config

--------------------------------------------------
--------------------------------------------------