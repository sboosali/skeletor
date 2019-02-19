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

import Program.Skeletor.Haskell.Options (getOptions)
import Program.Skeletor.Haskell.Core    (programWithOptions, programWithConfig)

--------------------------------------------------
--------------------------------------------------

import Prelude_exe

--------------------------------------------------
--------------------------------------------------

{-| the @main@ procedure @skeletor-haskell@ executable.

@≡ 'getOptions' >>= 'programWithOptions'
@

-}

main :: IO ()
main = do

  options <- getOptions

  programWithOptions options

--------------------------------------------------
--------------------------------------------------