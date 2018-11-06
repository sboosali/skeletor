
--------------------------------------------------
--------------------------------------------------

{-|

'Xxx_Module_xxX.Main' glues the other modules together:

* 'Xxx_Module_xxX.Options': 
the command-line options (parsing, defaulting, etc).
* 'Xxx_Module_xxX.Program': 
the @main@ program (the logic, data loading, filesystem interaction, etc).

-}

module Xxx_Module_xxX.Main

  ( main
  , module Xxx_Module_xxX.Options
  , module Xxx_Module_xxX.Program
  ) where

--------------------------------------------------

import Xxx_Module_xxX.Options
import Xxx_Module_xxX.Program

--------------------------------------------------
-- Imports: Internal -----------------------------
--------------------------------------------------

---import qualified "xxx-package-xxx" Xxx_Module_xxX as Xxx_ModuleAbbreviation_xxX

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