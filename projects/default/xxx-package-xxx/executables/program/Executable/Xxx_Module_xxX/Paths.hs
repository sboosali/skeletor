
--------------------------------------------------
--------------------------------------------------

{-| 

-}

module MyApplication.Paths

  ( directoryMyApplication

  , nameOfMyApplication
  , versionOfMyApplication

  , getMyApplicationDataDirectory
  , getMyApplicationConfigDirectory
  , getMyApplicationCacheDirectory

  ) where

--------------------------------------------------
-- Imports ---------------------------------------
--------------------------------------------------

import qualified "directory" System.Directory as Directory

--------------------------------------------------

import qualified "base" Data.Version as Version
import           "base" Data.Version (Version)

import qualified "base" System.Info  as IO

--------------------------------------------------

import qualified "base" Prelude
import           "base" Prelude

--------------------------------------------------
-- Definitions -----------------------------------
--------------------------------------------------

nameOfMyApplication :: ApplicationName
nameOfMyApplication = Prelude.words "my application"

--------------------------------------------------

versionOfMyApplication :: Version
versionOfMyApplication = "0.0.0"

--------------------------------------------------
--------------------------------------------------

directoryMyApplication :: IO FilePath
directoryMyApplication = do

  platform <- getRuntimeApplicationPlatform

  let directory = renderApplicationDirectoryFor platform

  return directory

--------------------------------------------------
--------------------------------------------------

-- | 

getMyApplicationDataDirectory :: FilePath -> IO FilePath
getMyApplicationDataDirectory = getMyApplicationDirectoryFor Directory.XdgData

--------------------------------------------------

-- | 

getMyApplicationConfigDirectory :: FilePath -> IO FilePath
getMyApplicationConfigDirectory = getMyApplicationDirectoryFor Directory.XdgConfig

--------------------------------------------------

-- | 

getMyApplicationCacheDirectory :: FilePath -> IO FilePath
getMyApplicationCacheDirectory = getMyApplicationDirectoryFor Directory.XdgCache

--------------------------------------------------
-- Utilities -------------------------------------
--------------------------------------------------

-- | 

getMyApplicationDirectoryFor :: Directory.XdgDirectory -> FilePath -> IO FilePath
getMyApplicationDirectoryFor = \case

  Directory.XdgData   -> dataFile

  Directory.XdgConfig -> configFile

  Directory.XdgCache  -> cacheFile

  where

  dataFile file = do

    return file

  configFile file = do

    return file

  cacheFile file = do

    return file

--------------------------------------------------
--------------------------------------------------

type ApplicationName = [String]

--------------------------------------------------

{- | Return a platform-specific and application-specific directory for user-writeable data\/configs\/caches.

For example, given an application named @Foo App@ by company @MegaCorp@ with web address @MegaCorp.co.uk@:

* @"fooapp/"@                 on Linux   — i.e. lower-cased, no spaces
* @"MegaCrop\Foo App\"@       on Windows — i.e. two folders, @MegaCrop@ and @Foo App\@.
* @"uk.co.MegaCorp.Foo-App/"@ on MacOS   — i.e. invalid characters are replaced with @-@.

See <https://stackoverflow.com/questions/43853548/xdg-basedir-directories-for-windows>.

-}

renderApplicationDirectoryFor :: Platform -> ApplicationName -> FilePath
renderApplicationDirectoryFor = \case

  PosixPlatform     -> renderPosixApplicationDirectory
  WindowsPlatform   -> renderWindowsApplicationDirectory
  MacintoshPlatform -> renderMacintoshApplicationDirectory

  where

  renderPosixApplicationDirectory     name = name

  renderWindowsApplicationDirectory   name = name

  renderMacintoshApplicationDirectory name = name

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