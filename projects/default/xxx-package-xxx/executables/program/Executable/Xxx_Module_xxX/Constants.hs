--------------------------------------------------
--------------------------------------------------

{-| Program metadata:

* name
* version
* license

-}

module Executable.Xxx_Module_xxX.Constants

  ( programName
  , programVersion
 --, programVersionPatch
  , programLicenseIdentifier
  , programLicenseContents
  ) where

--------------------------------------------------
-- Imports ---------------------------------------
--------------------------------------------------

import Prelude_exe

--------------------------------------------------

import qualified "base" Data.Version as Version
import           "base" Data.Version (Version (..))

--------------------------------------------------
-- Constants -------------------------------------
--------------------------------------------------

-- | The name of this program (as invoked in a terminal).

programName :: String
programName = "xxx-program-xxx"

--------------------------------------------------

-- | The current version of this program.

programVersion :: Version
programVersion = Version.makeVersion (majorVersion ++ minorVersion)

  where

  majorVersion = [0,1]
  minorVersion = [0]

--------------------------------------------------

-- | The current build of this program (a Git commit).

programVersionPatch :: String
programVersionPatch = "" --TODO-- inject current git commit via preprocesser.

--------------------------------------------------

-- | The SPDX License Identifier of this program.

programLicenseIdentifier :: String
programLicenseIdentifier = "__LICENSE__"

--------------------------------------------------

-- | The license of this program (for the user to read).

programLicenseContents :: String
programLicenseContents = "" -- TODO

--------------------------------------------------
-- EOF -------------------------------------------
--------------------------------------------------