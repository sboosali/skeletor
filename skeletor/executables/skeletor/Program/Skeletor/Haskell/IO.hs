
--------------------------------------------------
--------------------------------------------------

{-| 

-}

module Program.Skeletor.Haskell.IO where

--------------------------------------------------
-- Imports (Project) -----------------------------
--------------------------------------------------

import Program.Skeletor.Haskell.Prelude
import Program.Skeletor.Haskell.Types
import Program.Skeletor.Haskell.Utilities
import Program.Skeletor.Haskell.Constants

--------------------------------------------------
-- Imports (External) ----------------------------
--------------------------------------------------

import qualified "modern-uri" Text.URI as URI

--------------------------------------------------
-- Imports (StdLib) ------------------------------
--------------------------------------------------

import qualified "text" Data.Text as Text

import qualified "base" System.IO    as IO
import qualified "base" Data.Version as Version

--------------------------------------------------
-- Definitions -----------------------------------
--------------------------------------------------

{-| 

-}

runCommand :: (MonadThrow m, MonadIO m) => Command -> m Status
runCommand = \case

  CommandPrintVersion -> do

    printVersionWith ()
    return Success

  CommandPrintLicense -> do

    printLicenseWith ()
    return Success

  CommandCreateProject CreateProjectOptions{..} -> do

    result <- (createProjectWith CreateProject{..})
    return (toStatus result)

  CommandDownloadProject DownloadProjectOptions{..} -> do

    result <- (downloadProjectWith DownloadProject{..})
    return (toStatus result)

  CommandResolveConfiguration ResolveConfigurationOptions{..} -> do

    let extraConfig = mempty

    result <- (resolveConfigurationWith ResolveConfiguration{ extraConfig })
    return (toStatus result)

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

resolveConfigurationWith :: (MonadThrow m, MonadIO m) => ResolveConfiguration -> m ConfigurationResolved
resolveConfigurationWith ResolveConfiguration{..} = do

  let config = extraConfig

  return ConfigurationResolved
    { status = Success
    , config = config
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
  LocationURI  uri -> fetchURI uri

  where

  fetchURI x = return (go x)            -- TODO

     where
     go = URI.render > Text.unpack

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

  license = programLicenseIdentifier

--------------------------------------------------
-- Utilities -------------------------------------
--------------------------------------------------

-- 

-- :: _ -> _
-- = _

--------------------------------------------------
-- Definitions -----------------------------------
--------------------------------------------------



--------------------------------------------------
-- Notes -----------------------------------------
--------------------------------------------------
{-



-}