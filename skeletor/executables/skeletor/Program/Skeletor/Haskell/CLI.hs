--------------------------------------------------

{-# LANGUAGE BlockArguments        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE OverloadedLists       #-}
{-# LANGUAGE ApplicativeDo         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns        #-}

--------------------------------------------------
--------------------------------------------------

{-|



-}

module Program.Skeletor.Haskell.CLI

  ( cli
  , pCommand

  , piCreateProjectOptions
  , piDownloadProjectOptions
  , piResolveConfigurationOptions

  , pGlobalOptions

  , pLocation
  , pProject

  , pLicense
  , pOSILicense
  , pFLOSSLicense

  ) where

--------------------------------------------------
-- Imports (Internal) ----------------------------
--------------------------------------------------

import Program.Skeletor.Haskell.Types
import Program.Skeletor.Haskell.Parsers

--import Program.Skeletor.Haskell.Options
--import Program.Skeletor.Haskell.Config
--import Program.Skeletor.Haskell.Action
import Program.Skeletor.Haskell.Command

--------------------------------------------------

import Skeletor.Haskell
--import Skeletor.Haskell.License
--import Skeletor.Haskell.Variable.Binding

--------------------------------------------------
-- Imports (External) ----------------------------
--------------------------------------------------

--import qualified "attoparsec" Data.Attoparsec.Text as A

--------------------------------------------------
-- Imports (Standard Library) --------------------
--------------------------------------------------

--import qualified "text" Data.Text as T
--import           "text" Data.Text (Text)

-- NOTE « attoparsec » uses strict « Text ».

--------------------------------------------------
--------------------------------------------------

import qualified "optparse-applicative" Options.Applicative      as P
import qualified "optparse-applicative" Options.Applicative.Help as P hiding (fullDesc)

--------------------------------------------------

import           "base" Data.Maybe
--import           "base" Data.Semigroup

--------------------------------------------------

import Prelude_exe

--------------------------------------------------
-- Command-Line Interface ------------------------
--------------------------------------------------

cli :: P.ParserInfo Command
cli = piCommand
  where

  piCommand = info description pCommand

  description = ""

--------------------------------------------------
-- « ParserInfo »s -------------------------------
--------------------------------------------------

pCommand :: P.Parser Command
pCommand = P.hsubparser ps
  where

  ps = mconcat

    [ (P.command "create" (CommandCreateProject        <$> piCreateProjectOptions))
    , (P.command "fetch"  (CommandDownloadProject      <$> piDownloadProjectOptions))
    , (P.command "config" (CommandResolveConfiguration <$> piResolveConfigurationOptions))
    ]

-- command :: String -> ParserInfo a -> Mod CommandFields a

-- sample :: Parser Sample
-- sample = subparser
--        ( command "hello"
--          (info hello (progDesc "Print greeting")))

--------------------------------------------------

{-|

-}

piCreateProjectOptions :: P.ParserInfo CreateProjectOptions
piCreateProjectOptions = info "Create a project" do

  globals     <- pGlobalOptions

  location    <- pLocation

  destination <- pDestination

  license     <- pLicense

  return CreateProjectOptions{..}

  where

  description = ""

--------------------------------------------------

{-|

-}

piDownloadProjectOptions :: P.ParserInfo DownloadProjectOptions
piDownloadProjectOptions = info "Download a project" do

  globals     <- pGlobalOptions

  location    <- pLocation

  destination <- pDestination

  method      <- pFetchBy

  return DownloadProjectOptions{..}

  where

  description = ""

--------------------------------------------------

{-|

-}

piResolveConfigurationOptions :: P.ParserInfo ()  --TODO-- ResolveConfigurationOptions
piResolveConfigurationOptions = info description do

  empty

  where

  description = "Print an explicit (\"standalone\") configuration file merging the implicit configurations of: the current values of skeletor's environment variables; the user configuration in skeletor's XDG_CONFIG_DIR; skeletor's builtin projects and locations."

--------------------------------------------------
-- « Parser »s -----------------------------------
--------------------------------------------------

{-|

Options, Arguments, and Flags include:

* @-v@, @--verbose@.
* @--subdir@, @--no-subdir@.
* @-f@, @--project-filepath@, @-p@, @--project-name@,

-}

pGlobalOptions :: P.Parser GlobalOptions
pGlobalOptions = do

  verbosity <- (P.flag Concise Verbose) (mconcat

        [ P.long    "verbose"
        , P.short   'v'
        , P.help    "Enable verbose messages. (Includes network progress from downloading any resources. Includes printing the config that's derived from the invokation of this command: ①, parsing these command-line options; and ②, defaulting the values of any optional options.)"
        , P.style P.bold
        ])

  dryrun <- (P.flag TrueRun DryRun) (mconcat

        [ P.long    "dryrun"
        , P.short   'i'
        , embolden
        , P.help    "Whether the execution will just be a 'dry-run' (i.e. effects are disabled, instead they are printed out)."
        ])

  return GlobalOptions{..}

--------------------------------------------------

{-|

-}

pLocation :: P.Parser Location
pLocation = P.option rLocation (mconcat

        [ P.long    "location"
        , P.short   'l'
        , P.metavar "LOCATION"
        , P.completeWith printedKnownLocations
        , embolden
        , P.help    ""
        ])

--------------------------------------------------

{-|

-}

pProject :: P.Parser KnownProjectName
pProject = P.option rProject (mconcat

        [ P.long    "project-name"
        , P.metavar "n"
        , P.completeWith builtinProjectNames
        , embolden
        , P.help    "NAME of, or LOCATION of, a project." -- TODO -- 
        ])

--------------------------------------------------

{-|

-}

pDestination :: P.Parser FilePath
pDestination = P.option rDestination (mconcat

        [ P.long    "destination"
        , P.metavar "-d"
        , P.completeWith knownDestinations
        , embolden
        , P.help    "Destination in which to save the proejct being downloaded or being created."
        ])

  where

    rDestination = P.str

--------------------------------------------------

{-|

-}

pFetchBy :: P.Parser FetchBy
pFetchBy = defaulting defaultFetchBy (P.option rFetchBy (mconcat

        [ P.long    "fetch-method"
        , P.completeWith printedFetchBy
        , embolden
        , P.help    "How to download resources (in particular, projects)."
        ]))

--------------------------------------------------

{-|

-}

pLicense :: P.Parser License
pLicense = defaulting defaultLicense (P.option rLicense (mconcat

        [ P.long    "license"
        , P.metavar "SPDX_LICENSE"
        , P.completeWith knownLicenseIds
        , embolden
        , P.help    "The project's license (an SPDX license identifier). Examples include: « GPL-3.0-or-later », « GPL-3.0-only », « BSD-3-Clause », « CC-BY-SA-4.0 », « MIT ». Press <tab> (twice) to autocomplete all (~350) licenses."
        ]))

--------------------------------------------------

{-|

-}

pOSILicense :: P.Parser License
pOSILicense = defaulting defaultLicense (P.option rOSILicense (mconcat

        [ P.long    "license-osi"
        , P.metavar "SPDX_LICENSE"
        , P.completeWith knownOSILicenseIds
        , embolden
        , P.help    "Like « --license _», but only for Open Source Initiative licenses."
        , embolden
        ]))

--------------------------------------------------

{-|

-}

pFLOSSLicense :: P.Parser License
pFLOSSLicense = defaulting defaultFLOSSLicense (P.option rFLOSSLicense (mconcat

        [ P.long    "license-libre"  --[OLD] "license-floss"
        , P.metavar "SPDX_LICENSE"
        , P.completeWith knownFLOSSLicenseIds
        , embolden
        , P.help    "Like « --license _», but only for Free/Libre and Open-Source Software (a.k.a Copyleft) licenses."
        ]))

--------------------------------------------------
-- Utilities -------------------------------------
--------------------------------------------------

{-| 

-}

info
  :: forall a. String -> P.Parser a
  -> P.ParserInfo a

info description parser = P.info (P.helper <*> parser) information
  where

  information :: P.InfoMod a
  information = mconcat

      [ P.fullDesc
      , P.progDesc description
      ]

--------------------------------------------------

{-| 

-}

knownDestinations :: [FilePath]
knownDestinations = []

--------------------------------------------------

defaulting :: (Alternative f) => a -> f a -> f a
defaulting x = \p -> maybe x id <$> optional p

{-# INLINEABLE defaulting #-}

--------------------------------------------------

embolden :: P.Mod f a
embolden = P.style P.bold

{-# INLINEABLE embolden #-}

--------------------------------------------------
--------------------------------------------------