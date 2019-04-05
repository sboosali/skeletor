
--------------------------------------------------
--------------------------------------------------

{-|




-}

module Program.Xxx_Module_xxX.Paths

  ( -- * Application Metadata

    subdirectory_Xxx_Program_xxX
  , name_Xxx_Program_xxX
  , version_Xxx_Program_xxX

  -- * Application-Specific Directories

  {- |

The @getMyApplication{Config,Data,Cache}Directory@ operations
return this application's (platform-specific, user-writeable) directory
for @{configuration files, data files, caching}@.

The @getMyApplication{Config,Data,Cache}Directory@ operations may throw these exceptions (all 'System.IO.IOError's):

* @System.IO.HardwareFault@
A physical I\/O error has occurred.
@[EIO]@

* 'System.IO.isDoesNotExistError'
There is no path referring to the working directory.
@[EPERM, ENOENT, ESTALE...]@

* 'System.IO.isPermissionError'
The process has insufficient privileges to perform the operation.
@[EACCES]@

* 'System.IO.isFullError'
Insufficient resources are available to perform the operation.

* @UnsupportedOperation@
The operating system has no notion of current working directory.

  -}

  , getMyApplicationDataDirectory
  , getMyApplicationConfigDirectory
  , getMyApplicationCacheDirectory

  ) where

--------------------------------------------------
-- Imports ---------------------------------------
--------------------------------------------------

import Program.Xxx_Module_xxX.Prelude

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

name_Xxx_Program_xxX :: ApplicationName
name_Xxx_Program_xxX = "My Application"

--------------------------------------------------

version_Xxx_Program_xxX :: Version
version_Xxx_Program_xxX = "0.0.0"

--------------------------------------------------
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