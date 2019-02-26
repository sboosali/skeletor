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

  ( pCommand
  , pCreateProjectOptions
  , pDownloadProjectOptions
  , pResolveConfigurationOptions

  , pGlobalOptions
  , pLicense
  , pLocation
  , pProject

  ) where

--------------------------------------------------
-- Imports (Internal) ----------------------------
--------------------------------------------------

import Program.Skeletor.Haskell.Types

import Program.Skeletor.Haskell.Options
import Program.Skeletor.Haskell.Config
import Program.Skeletor.Haskell.Action
import Program.Skeletor.Haskell.Command

--------------------------------------------------

import Skeletor.Haskell
import Skeletor.Haskell.License
import Skeletor.Haskell.Variable.Binding

--------------------------------------------------
-- Imports (External) ----------------------------
--------------------------------------------------

import qualified "attoparsec" Data.Attoparsec.Text as A

--------------------------------------------------
-- Imports (Standard Library) --------------------
--------------------------------------------------

import qualified "text" Data.Text as T
import           "text" Data.Text (Text)

-- NOTE « attoparsec » uses strict « Text ».

--------------------------------------------------
--------------------------------------------------

import qualified "optparse-applicative" Options.Applicative      as P
import qualified "optparse-applicative" Options.Applicative.Help as P hiding (fullDesc)

--------------------------------------------------

import           "base" Data.Maybe
import           "base" Data.Semigroup

--------------------------------------------------

import Prelude_exe

--------------------------------------------------
-- Command-Line Interface ------------------------
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
        , P.help    "Whether the execution will just be a 'dry-run' (i.e. effects are disabled, instead they are printed out)."
        , P.style P.bold
        ])

  return GlobalOptions{..}

--------------------------------------------------

pCommand :: P.Parser Command
pCommand = P.hsubparser ps
  where

  ps = mconcat

    [ (P.command "create" (CommandCreateProject        <$> pCreateProjectOptions))
    , (P.command "fetch"  (CommandDownloadProject      <$> pDownloadProjectOptions))
    , (P.command "config" (CommandResolveConfiguration <$> pResolveConfigurationOptions))
    ]


command :: String -> ParserInfo a -> Mod CommandFields a

sample :: Parser Sample
sample = subparser
       ( command "hello"
         (info hello (progDesc "Print greeting")))

--------------------------------------------------

{-|

-}

pCreateProjectOptions :: P.Parser CreateProjectOptions
pCreateProjectOptions = do

  globals     <- pGlobalOptions

  location    <- pLocation

  destination <- pDestination

  license     <- pLicense

  return CreateProjectOptions{..}

--------------------------------------------------

{-|

-}

pDownloadProjectOptions :: P.Parser DownloadProjectOptions
pDownloadProjectOptions = do

  globals     <- pGlobalOptions

  location    <- pLocation

  destination <- pDestination

  method      <- pFetchBy
 
  return DownloadProjectOptions{..}

--------------------------------------------------

{-|

-}

pResolveConfigurationOptions :: P.Parser ()  --TODO-- ResolveConfigurationOptions
pResolveConfigurationOptions = empty

--------------------------------------------------

{-|

-}


pLocation :: P.Parser Location
pLocation = (P.strOption (mconcat

        [ P.long    ""
        , P.metavar ""
        , P.completeWith knownLocations
        , P.help    ""
        , embolden
        ]))

--------------------------------------------------

{-|

-}

pDestination :: P.Parser FilePath
pDestination = (P.strOption (mconcat

        [ P.long    ""
        , P.metavar ""
        , P.completeWith knownDestinations
        , P.help    ""
        , embolden
        ]))

--------------------------------------------------

{-|

-}

pLicense :: P.Parser License
pLicense = defaulting defaultLicense (P.strOption (mconcat

        [ P.long    "license"
        , P.metavar "SPDX_LICENSE"
        , P.completeWith knownLicenseIds
        , P.help    "The project's license (an SPDX license identifier). Examples include: « GPL-3.0-or-later », « GPL-3.0-only », « BSD-3-Clause », « CC-BY-SA-4.0 », « MIT ». Press <tab> (twice) to autocomplete all (~350) licenses."
        , embolden
        ]))

--------------------------------------------------

{-|

-}

pOSILicense :: License
pOSILicense = defaulting defaultLicense (P.strOption (mconcat

        [ P.long    "license-osi"
        , P.metavar "SPDX_LICENSE"
        , P.completeWith knownOSILicenseIds
        , P.help    "Like « --license _», but only for Open Source Initiative licenses."
        , embolden
        ]))

--------------------------------------------------

{-|

-}

pFLOSSLicense :: License
pFLOSSLicense = defaulting defaultFLOSSLicense (P.strOption (mconcat

        [ P.long    "license-libre"  --[OLD] "license-floss"
        , P.metavar "SPDX_LICENSE"
        , P.completeWith knownFLOSSLicenseIds
        , P.help    "Like « --license _», but only for Free/Libre and Open-Source Software (a.k.a Copyleft) licenses."
        , embolden
        ]))

--------------------------------------------------

{-| 

-}

pFetchBy :: FetchBy
pFetchBy = _

--------------------------------------------------

{-| 

-}

knownLocations :: [Location]
knownLocations = []

--------------------------------------------------

{-| 

-}

knownDestinations :: [FilePath]
knownDestinations = []

--------------------------------------------------
-- Utilities -------------------------------------
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