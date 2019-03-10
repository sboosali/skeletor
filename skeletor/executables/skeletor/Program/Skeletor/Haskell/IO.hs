
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

--------------------------------------------------
-- Imports (External) ----------------------------
--------------------------------------------------

-- import qualified "" _ as _
-- import           "" _ ()

--------------------------------------------------
-- Imports (StdLib) ------------------------------
--------------------------------------------------

-- import qualified "" _ as _
-- import           "" _ ()

--------------------------------------------------
-- Definitions -----------------------------------
--------------------------------------------------

{-| 

-}

runCommand :: (MonadThrow m, MonadIO m) => Command -> m Status
runCommand = \case

  CommandPrintVersion               -> do

    printVersionWith ()
    return Success

  CommandPrintLicense               -> do

    printLicenseWith ()
    return Success

  CommandCreateProject        input -> do

    result <- (createProjectWith input)
    return (toStatus result)

  CommandDownloadProject      input -> do

    result <- (downloadProjectWith input)
    return (toStatus result)

  CommandResolveConfiguration       -> do

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
--------------------------------------------------

{-| 

-}

printConfigWith :: (MonadThrow m, MonadIO m) => Config -> m ()
printConfigWith config = liftIO $ do

  print config

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