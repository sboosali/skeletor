{-# LANGUAGE OverloadedLists #-}

--------------------------------------------------
--------------------------------------------------

{-|



-}

module Program.Skeletor.Haskell.Action where

--------------------------------------------------
-- Imports (Internal) ----------------------------
--------------------------------------------------

import Program.Skeletor.Haskell.Types

import Program.Skeletor.Haskell.Version
import Program.Skeletor.Haskell.License

-- import Skeletor.Core.Types
-- import Skeletor.Haskell.Types

--------------------------------------------------
-- Imports (External) ----------------------------
--------------------------------------------------

--------------------------------------------------
-- Imports (Standard Library) --------------------
--------------------------------------------------

import qualified "base" Data.Version as Version

import qualified "base" System.IO    as IO

--------------------------------------------------
--------------------------------------------------

import Prelude_exe

--------------------------------------------------
--------------------------------------------------

{-| 

-}

toActions :: (MonadThrow m) => Options -> m Actions
toActions Options{..} = do

  let actions' = actions

  return actions'

  where

  actions :: Actions
  actions = mconcat
    [ (if printVersion then [ActionPrintVersion] else [])
    , (if printLicense then [ActionPrintLicense] else [])
    , []
    ]

--------------------------------------------------
--------------------------------------------------

{-| 

-}

runAction :: (MonadThrow m, MonadIO m) => Action -> m Status
runAction = \case

  ActionPrintVersion               -> do

    printVersionWith ()
    return Success

  ActionPrintLicense               -> do

    printLicenseWith ()
    return Success

  ActionCreateProject        input -> do

    result <- (createProjectWith input)
    return (toStatus result)

  ActionDownloadProject      input -> do

    result <- (downloadProjectWith input)
    return (toStatus result)

  ActionResolveConfiguration       -> do

    nothing
    return Success

--------------------------------------------------
--------------------------------------------------

{-| 

-}

createProjectWith :: (MonadThrow m, MonadIO m) => CreateProject -> m ProjectCreated
createProjectWith CreateProject{..} = do

  path <- fetchLocation location

  return ProjectCreated
    { status = Success
    }

--------------------------------------------------
--------------------------------------------------

{-| 

-}

downloadProjectWith :: (MonadThrow m, MonadIO m) => DownloadProject -> m ProjectDownloaded
downloadProjectWith DownloadProject{..} = do

  path <- fetchLocation location

  return ProjectDownloaded
    { status = Success
    , path
    }

--------------------------------------------------
--------------------------------------------------

{-| 

-}

fetchLocation
  :: (MonadThrow m, MonadIO m)
  => Location -> m FilePath

fetchLocation = \case

  LocationStdin    -> stdinLocation
  LocationPath fp  -> return fp
  LocationURL  uri -> fetchURI uri

  where
  fetchURI = return             -- TODO

  stdinLocation = liftIO $ do
  --IO.putStr "Enter ① a directory filepath or ② a project name: "
    IO.putStr "Enter a directory filepath: "
    IO.getLine

--------------------------------------------------
--------------------------------------------------

{-| 

-}

printVersionWith :: (MonadThrow m, MonadIO m) => () -> m ()
printVersionWith () = liftIO $ do

  putStrLn version

  where

  version = Version.showVersion programVersion

--------------------------------------------------
--------------------------------------------------

{-| 

-}

printLicenseWith :: (MonadThrow m, MonadIO m) => () -> m ()
printLicenseWith () = liftIO $ do

  putStrLn license

  where

  license = programLicense

--------------------------------------------------
--------------------------------------------------

{-| 

-}

--------------------------------------------------
--------------------------------------------------