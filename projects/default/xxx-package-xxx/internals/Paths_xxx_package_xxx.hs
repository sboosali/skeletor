{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}

{-# LANGUAGE ImplicitPrelude #-}

--------------------------------------------------

module Paths_xxx_package_xxx
  ( getDataDir
  , getDataFileName
  ) where

--------------------------------------------------

import qualified "base" Control.Exception as Exception
import           "base" System.Environment (getEnv)
--import           "base" Data.Version (Version(..))

--------------------------------------------------

#if defined(VERSION_base)

#if MIN_VERSION_base(4,0,0)
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#else
catchIO :: IO a -> (Exception.Exception -> IO a) -> IO a
#endif

#else
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#endif
catchIO = Exception.catch

--------------------------------------------------

datadir :: FilePath                                -- TODO make relative, or make TemplateVariable.
datadir = "./skeletor/data/golden/"

--------------------------------------------------

getDataDir :: IO FilePath
getDataDir = catchIO (getEnv "xxx_package_xxx_datadir") (\_ -> return datadir)

--------------------------------------------------

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)

--------------------------------------------------
