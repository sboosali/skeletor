
--------------------------------------------------
--------------------------------------------------

{-|




-}

module Program.Xxx_Module_xxX.Paths

  ( -- * Application Metadata

    application
  , currentApplicationSubdirectory

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

import qualified "base" System.Info  as IO

--------------------------------------------------
-- Definitions -----------------------------------
--------------------------------------------------


application :: ApplicationInformation
application = defaultApplicationInformation application0
  where

  application0 :: ApplicationInformation0
  application0 = def{ name0, license0, version0, vendor0, executable0, interface0, platforms0 }
    where

    name0        = "__PROGRAM__"
    license0     = "Apache-2.0"
    version0     = "0.0.0"
    vendor0      = "sboosali.io"
    executable0  = Just "xxx-program-xxx"
    interface0   = Just ApplicationCLI
    platforms0   = Just allDesktopPlatforms

--------------------------------------------------

currentApplicationSubdirectory :: IO FilePath
currentApplicationSubdirectory = do

  platform <- getRuntimeApplicationPlatform

  let directory = platform & maybe defaultDirectory subdirectoryOfMyApplicationFor

  return directory

  where

  defaultDirectory = "." <> (application & executable)

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

  subdirectory <- currentApplicationSubdirectory

  let relativePath = subdirectory File.</> path

  xdgPath <- Directory.getXdgDirectory xdg relativePath

  absolutePath <- Directory.makeAbsolute xdgPath

  return absolutePath

--createDirectoryIfMissing

--------------------------------------------------
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
getRuntimeApplicationPlatform = return platform
  where

  platform = IO.os & \case

    "linux"   -> Just PosixPlatform

    "mingw32" -> Just WindowsPlatform
    "mingw64" -> Just WindowsPlatform

    "darwin"  -> Just MacintoshPlatform

    _         -> Nothing

--------------------------------------------------
-- EOF -------------------------------------------
--------------------------------------------------