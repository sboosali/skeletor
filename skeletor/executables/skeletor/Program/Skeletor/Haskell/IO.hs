{-# LANGUAGE BlockArguments #-}

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

import qualified "time" Data.Time.Clock     as Time
import qualified "time" Data.Time.LocalTime as Time
import qualified "time" Data.Time.Format    as Time

--------------------------------------------------

import qualified "filepath" System.FilePath as File
import           "filepath" System.FilePath ((</>))

--------------------------------------------------

import qualified "directory" System.Directory as Directory

--------------------------------------------------

import qualified "unix-compat" System.PosixCompat.Files as UNIX

--------------------------------------------------
-- Imports (StdLib) ------------------------------
--------------------------------------------------

import qualified "text" Data.Text    as Text
import qualified "text" Data.Text.IO as Text

import qualified "base" System.IO    as IO
import qualified "base" Data.Version as Version

--------------------------------------------------
-- Definitions -----------------------------------
--------------------------------------------------

{-| 

-}

runCommand :: (MonadThrow m, MonadIO m) => Command -> m Status
runCommand = \case

  CommandPrintVersion options -> do

    printVersionWith options
    return Success

  CommandPrintLicense options -> do

    printLicenseWith options
    return Success

  CommandPrintExamples options -> do

    printExamplesWith options
    return Success

  CommandCreateProject CreateProjectOptions{..} -> do

    result <- (createProjectWith CreateProject{..})
    return (toStatus result)

  CommandDownloadProject DownloadProjectOptions{..} -> do

    result <- (downloadProjectWith DownloadProject{..})
    return (toStatus result)

  CommandResolveConfiguration ResolveConfigurationOptions{..} -> do

    let extraConfig = mempty

    ConfigurationResolved{ status, config } <- resolveConfigurationWith ResolveConfiguration{ extraConfig }

    io $ printAndSaveConfig config

    return status

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

printVersionWith :: (MonadThrow m, MonadIO m) => GlobalOptions -> m ()
printVersionWith GlobalOptions{..} = liftIO $ do

  go verbosity

  where

  go = \case

    Silent  -> printVersionConcise
    Concise -> printVersionConcise

    Verbose    -> printVersionVerbose
    Vociferous -> printVersionVerbose

  printVersionConcise = do

    putStrLn $ programVersionBranch

  printVersionVerbose = do

    let versionLine = mconcat [ programName, ", version ", programVersion ]

    putStrLn $ versionLine

--  let versionString = versionStringBranch ++ versionStringTags

{-# INLINEABLE printVersionWith #-}

--------------------------------------------------
--------------------------------------------------

{-| 

-}

printLicenseWith :: (MonadThrow m, MonadIO m) => GlobalOptions -> m ()
printLicenseWith GlobalOptions{..} = liftIO $ do

  go verbosity

  where

  go = \case

    Silent  -> printLicenseConcise
    Concise -> printLicenseConcise

    Verbose    -> printLicenseVerbose
    Vociferous -> printLicenseVerbose

  printLicenseConcise = do

    putStrLn $ programLicenseIdentifier

  printLicenseVerbose = do

    printLicenseConcise
    putStrLn ""

    putStrLn $ programLicenseContents

{-# INLINEABLE printLicenseWith #-}

--------------------------------------------------
--------------------------------------------------

{-| 

-}

printExamplesWith :: (MonadThrow m, MonadIO m) => GlobalOptions -> m ()
printExamplesWith GlobalOptions{..} = liftIO $ do

  go verbosity

  where

  go = \case

    Silent  -> printExamplesConcise
    Concise -> printExamplesConcise

    Verbose    -> printExamplesVerbose
    Vociferous -> printExamplesVerbose

  printExamplesConcise = do

    putStrLn `traverse_` programExamples

  printExamplesVerbose = do

    printExamplesConcise

{-# INLINEABLE printExamplesWith #-}

--------------------------------------------------

printAndSaveConfig :: Configuration -> IO ()
printAndSaveConfig config = do

   printDivider

   Text.putStrLn sConfig -- TODO

   printDivider

   fpConfig <- newTemporaryFilePath "config"
   Text.writeFile fpConfig sConfig

   putStrLn fpConfig

   printDivider

  where

  sConfig :: Text
  sConfig = show config & fromString

{-# INLINEABLE printAndSaveConfig #-}

--------------------------------------------------
-- Utilities -------------------------------------
--------------------------------------------------

{- | an application-specific filepath to a temporary file.

e.g.:

@
> newTemporaryFilePath "project.tar.gz"
"/tmp/haskell-skeletor/2019-04-06-21h-43m-51s-852ms_project.tar.gz"
@

-}

newTemporaryFilePath :: String -> IO FilePath
newTemporaryFilePath name = do

  directory <- Directory.getTemporaryDirectory
  time      <- Time.getZonedTime

  let timestamp = formatZonedTimeAsFilePath time

  let dirname  = directory </> programExecutable
  let basename = timestamp <> "_" <> name 

  let path = dirname </> basename

  return path

--------------------------------------------------

{- | Format a timestamp to be part of a filepath.

-}

formatUTCTimeAsFilePath :: (Maybe Time.TimeLocale) -> Time.UTCTime -> String
formatUTCTimeAsFilePath mLocale t =

  Time.formatTime locale timeFormatWithHyphensAndUnits t

  where

  locale = mLocale & maybe Time.defaultTimeLocale id

--------------------------------------------------

{- | Format a timestamp to be part of a filepath.

e.g.:

@
> formatZonedTimeAsFilePath _
"2019-04-06-21h-43m-51s-852ms"
@

-}

formatZonedTimeAsFilePath :: Time.ZonedTime -> String
formatZonedTimeAsFilePath t =

  Time.formatTime locale timeFormatWithHyphensAndUnits t

  where

  locale = Time.defaultTimeLocale
  -- NOTE even « Prelude.undefined » works as the locale for « ZonedTime » (it's ignored).

--------------------------------------------------

timeFormatWithHyphensAndUnits :: String
timeFormatWithHyphensAndUnits = "%Y-%m-%d-%Hh-%Mm-%Ss-%03qms"

-- NOTE given the meta-syntax « %<modifier><width><alternate><specifier> »,
--      the syntax « %03q » means (0-padded) 3-width picoseconds (i.e. milliseconds).

--------------------------------------------------
-- Notes -----------------------------------------
--------------------------------------------------
{-



-}
--------------------------------------------------
-- EOF -------------------------------------------
--------------------------------------------------