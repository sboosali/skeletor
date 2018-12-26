
--------------------------------------------------
--------------------------------------------------

{-|

'Xxx_Module_xxX.Main' glues the other modules together:

* 'Xxx_Module_xxX.Options': 
the command-line options (parsing, defaulting, etc).
* 'Xxx_Module_xxX.Core': 
the @main@ program (the logic, data loading, filesystem interaction, etc).

-}

module Executable.Xxx_Module_xxX.Main

  ( main
  , module Executable.Xxx_Module_xxX.Options
  , module Executable.Xxx_Module_xxX.Core
  ) where

--------------------------------------------------

import Executable.Xxx_Module_xxX.Options
import Executable.Xxx_Module_xxX.Core

--------------------------------------------------
-- Imports: Internal -----------------------------
--------------------------------------------------

---import qualified "xxx-package-xxx" Executable.Xxx_Module_xxX as Xxx_ModuleAbbreviation_xxX

--------------------------------------------------

import Prelude_exe

--------------------------------------------------
-- Imports: External -----------------------------
--------------------------------------------------

-- import qualified "" _ as _

--------------------------------------------------

-- import           "base" _

--------------------------------------------------
--------------------------------------------------

{-| 

-}

main :: IO ()
main = do

  config <- getConfig
  mainWith config

--------------------------------------------------
--------------------------------------------------