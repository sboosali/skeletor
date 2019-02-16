--------------------------------------------------
--------------------------------------------------

{-|

'Skeletor.Haskell.Main' glues the other modules together:

* 'Program.Skeletor.Haskell.Options': the command-line options (parsing, defaulting, etc).
* 'Program.Skeletor.Haskell.Core': the @main@ program (the logic, filesystem interaction, etc).

-}

module Program.Skeletor.Haskell.Main

  ( main
  , module Program.Skeletor.Haskell.Options
  , module Program.Skeletor.Haskell.Core
  ) where

--------------------------------------------------
--------------------------------------------------

import Program.Skeletor.Haskell.Options
import Program.Skeletor.Haskell.Core

--------------------------------------------------

import Prelude_exe

--------------------------------------------------
--------------------------------------------------

{-| 

-}

main :: IO ()
main = do
  options <- getOptions
  program options

--------------------------------------------------
--------------------------------------------------