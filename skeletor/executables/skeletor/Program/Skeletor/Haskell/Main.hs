--------------------------------------------------
--------------------------------------------------

{-|

'Skeletor.Haskell.Main' glues the other modules together:

* 'Program.Skeletor.Haskell.Options': the command-line options (parsing, defaulting, etc).
* 'Program.Skeletor.Haskell.Core': the @main@ program (the logic, filesystem interaction, etc).

-}

module Program.Skeletor.Haskell.Main

  ( main
  , programWithOptions
  , programWithConfig
  ) where

--------------------------------------------------
--------------------------------------------------

import Program.Skeletor.Haskell.CLI (getCommand)
import Program.Skeletor.Haskell.IO  (runCommand)

--------------------------------------------------
--------------------------------------------------

import Prelude_exe

--------------------------------------------------
--------------------------------------------------

{-| the @main@ procedure of the @skeletor-haskell@ executable.

@≡ 'getCommand' >>= 'runCommand'
@

-}

main :: IO ()
main = do

  command <- getCommand

  runCommand command

--------------------------------------------------

-- cli :: IO ()
-- cli = do
--   nothing

--------------------------------------------------
--------------------------------------------------