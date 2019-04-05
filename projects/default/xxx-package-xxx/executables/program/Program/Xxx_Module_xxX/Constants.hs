{-# LANGUAGE CPP #-}

--------------------------------------------------
--------------------------------------------------

{-| Program metadata:

* name
* version
* license

-}

module Program.Xxx_Module_xxX.Constants

  (

    xxx_program_xxx_name
  , xxx_program_xxx_version
  , xxx_program_xxx_executableName
  , xxx_program_xxx_license
  , xxx_program_xxx_licenseText

  ) where

--------------------------------------------------
-- Includes --------------------------------------
--------------------------------------------------

#include "cabal_macros.h"

--------------------------------------------------
-- Imports ---------------------------------------
--------------------------------------------------

import Program.Xxx_Module_xxX.Prelude

--------------------------------------------------

import qualified "base" Data.Version as Version
import           "base" Data.Version (Version (..))

--------------------------------------------------
-- Constants -------------------------------------
--------------------------------------------------

-- | The english name of this program.

xxx_program_xxx_name :: String
xxx_program_xxx_name = "__PROGRAM__"

--------------------------------------------------

-- | The current version of this program.

xxx_program_xxx_version :: String
#ifdef CURRENT_PACKAGE_VERSION
xxx_program_xxx_version = CURRENT_PACKAGE_VERSION
#else
xxx_program_xxx_version = "0.0.0"
#endif

--------------------------------------------------

-- | The executable @basename@ of this program (as invoked in a terminal).

xxx_program_xxx_executableName :: String
xxx_program_xxx_executableName = "xxx-program-xxx"

--------------------------------------------------

-- | The SPDX License Identifier of this program.

xxx_program_xxx_license :: String
xxx_program_xxx_license = "__LICENSE__"

--------------------------------------------------

-- | The license text of this program (for the user to read).

xxx_program_xxx_licenseText :: String
xxx_program_xxx_licenseText = "" -- TODO

--------------------------------------------------
-- EOF -------------------------------------------
--------------------------------------------------