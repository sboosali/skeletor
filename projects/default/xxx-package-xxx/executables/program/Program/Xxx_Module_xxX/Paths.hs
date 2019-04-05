
--------------------------------------------------
--------------------------------------------------

{-|




-}

module Program.Xxx_Module_xxX.Paths

  ( -- * Application Metadata

    subdirectory_Xxx_Program_xxX
  , name_Xxx_Program_xxX
  , version_Xxx_Program_xxX

  , getMyApplicationDataDirectory
  , getMyApplicationConfigDirectory
  , getMyApplicationCacheDirectory

  ) where

--------------------------------------------------
-- Imports ---------------------------------------
--------------------------------------------------

import Program.Xxx_Module_xxX.Prelude

--------------------------------------------------

import "spiros" Prelude.Spiros.Application

--------------------------------------------------

import qualified "filepath"  System.FilePath  as File

--------------------------------------------------

import qualified "directory" System.Directory as Directory

--------------------------------------------------

import qualified "base" Data.Version as Version
import           "base" Data.Version (Version)

import qualified "base" System.Info  as IO

--------------------------------------------------
-- Definitions -----------------------------------
--------------------------------------------------

application :: ApplicationInformation
application = ApplicationInformation{..}
  where

  name                  = "xxx_program_xxx_name"
  version               = "xxx_program_xxx_version"
  license               = "xxx_program_xxx_license"
  vendor                = ""

  executable            = "xxx_program_xxx_executableName"
  interface             = ApplicationCLI
  platforms             = [ DesktopLinux, DesktopWindows, DesktopMacintosh ]

  posixSubDirectory     = "" -- "myapplication/"
  windowsSubDirectory   = "" -- "sboosali/My Application/"
  macintoshSubDirectory = "" -- "io.sboosali.My-Application/"

--------------------------------------------------

subdirectory_Xxx_Program_xxX :: IO FilePath
subdirectory_Xxx_Program_xxX = do

  platform <- getRuntimeApplicationPlatform

  let directory = subdirectoryOfMyApplicationFor platform

  return directory

--------------------------------------------------
--------------------------------------------------

-- | Return this application's (platform-specific, user-writeable) configuration directory.

getMyApplicationDataDirectory :: FilePath -> IO FilePath
getMyApplicationDataDirectory = getMyApplicationDirectoryFor Directory.XdgData

--------------------------------------------------

-- | Return this application's (platform-specific, user-writeable) data directory.

getMyApplicationConfigDirectory :: FilePath -> IO FilePath
getMyApplicationConfigDirectory = getMyApplicationDirectoryFor Directory.XdgConfig

--------------------------------------------------

-- | Return this application's (platform-specific, user-writeable) cache.

getMyApplicationCacheDirectory :: FilePath -> IO FilePath
getMyApplicationCacheDirectory = getMyApplicationDirectoryFor Directory.XdgCache

--------------------------------------------------
-- Utilities -------------------------------------
--------------------------------------------------

-- | 

getMyApplicationDirectoryFor :: Directory.XdgDirectory -> FilePath -> IO FilePath
getMyApplicationDirectoryFor xdg path = do

  subdirectory <- subdirectory_Xxx_Program_xxX

  let relativePath = subdirectory File.</> path

  xdgPath <- Directory.getXdgDirectory xdg relativePath

  absolutePath <- Directory.makeAbsolute xdgPath

  return absolutePath

--createDirectoryIfMissing

--------------------------------------------------
--------------------------------------------------

type ApplicationName = String

--------------------------------------------------

{- | Return this application's (platform-specific, user-writeable) subdirectory (a.k.a. namespace), given:

* the application name        — @"Foo App"@
* the application institution — @"Mondragon"@
* the application website     — @"Mondragon.com"@

For example, given an application named @Foo App@ by company @Mondragon@ with web address @Mondragon.co.uk@:

* @"fooapp/"@                on Linux   — i.e. lower-cased, no spaces
* @"MegaCrop\Foo App\"@      on Windows — i.e. two folders, @Mondragon.com@ and @Foo App\@.
* @"com.Mondragon.Foo-App/"@ on MacOS   — i.e. invalid characters are replaced with @-@.

See <https://stackoverflow.com/questions/43853548/xdg-basedir-directories-for-windows>.

-}

subdirectoryOfMyApplicationFor :: ApplicationPlatform -> FilePath
subdirectoryOfMyApplicationFor = \case

  PosixPlatform     -> thePosixApplicationDirectory
  WindowsPlatform   -> theWindowsApplicationDirectory
  MacintoshPlatform -> theMacintoshApplicationDirectory

  where

  thePosixApplicationDirectory     = "myapplication/"

  theWindowsApplicationDirectory   = "sboosali/My Application/"

  theMacintoshApplicationDirectory = "io.sboosali.My-Application/"

--------------------------------------------------
--------------------------------------------------

data ApplicationPlatform

  = PosixPlatform
  | WindowsPlatform
  | MacintoshPlatform

--------------------------------------------------

getRuntimeApplicationPlatform :: IO (Maybe ApplicationPlatform)
getRuntimeApplicationPlatform = do

  return IO.os >>= \case

    "linux"   -> Just PosixPlatform

    "mingw32" -> Just WindowsPlatform
    "mingw64" -> Just WindowsPlatform

    "darwin"  -> Just MacintoshPlatform

    _         -> Nothing

--------------------------------------------------
-- EOF -------------------------------------------
--------------------------------------------------