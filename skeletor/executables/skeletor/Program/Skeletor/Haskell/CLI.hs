--------------------------------------------------

{-# LANGUAGE BlockArguments        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ApplicativeDo         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns        #-}

--------------------------------------------------
--------------------------------------------------

{-|



-}

module Program.Skeletor.Haskell.CLI

  ( getCommand
  , parseCommand
  , preferences

  , piCommand
  , piCreateProjectOptions
  , piDownloadProjectOptions
  , piResolveConfigurationOptions

  , pCommand
  , pGlobalOptions
  , pLocation
  , pProject
  , pLicense
  , pOSILicense
  , pFLOSSLicense

  ) where

--TODO-- =rules

--------------------------------------------------
-- Imports (Internal) ----------------------------
--------------------------------------------------

import Program.Skeletor.Haskell.Types
import Program.Skeletor.Haskell.Utilities
import Program.Skeletor.Haskell.Constants
import Program.Skeletor.Haskell.Parsers

--------------------------------------------------

import Skeletor.Haskell
--import Skeletor.Haskell.License
--import Skeletor.Haskell.Variable.Binding

--------------------------------------------------
-- Imports (External) ----------------------------
--------------------------------------------------

--import qualified "attoparsec" Data.Attoparsec.Text as A

--------------------------------------------------
--------------------------------------------------

import qualified "optparse-applicative" Options.Applicative      as P
import qualified "optparse-applicative" Options.Applicative.Help as P hiding (fullDesc)

--------------------------------------------------
-- Imports (Standard Library) --------------------
--------------------------------------------------

--import qualified "text" Data.Text as T
--import           "text" Data.Text (Text)

-- NOTE « attoparsec » uses strict « Text ».

--------------------------------------------------
--------------------------------------------------

--import           "base" Data.Semigroup
import           "base" Data.Maybe
import           "base" System.Exit

--------------------------------------------------

import Program.Skeletor.Haskell.Prelude

--------------------------------------------------
-- Command-Line Interface ------------------------
--------------------------------------------------

getCommand :: IO Command
getCommand = do

  P.customExecParser preferences piCommand

--------------------------------------------------

parseCommand :: (MonadThrow m) => [String] -> m Command
parseCommand

  = P.execParserPure preferences piCommand
  > fromParserResult
  > ( either throwM return )

--------------------------------------------------

{-|

-}

preferences :: P.ParserPrefs
preferences = config { P.prefShowHelpOnError = True, P.prefShowHelpOnEmpty = True }
  where

  config = P.prefs (mconcat

    [ P.showHelpOnError
    , P.showHelpOnEmpty
    ])

--------------------------------------------------

-- printHelpTextForCommand :: IO Command
-- printHelpTextForCommand = printParserHelpText preferences piCommand

--------------------------------------------------
-- « ParserInfo »s -------------------------------
--------------------------------------------------

{-|

-}

piCommand :: P.ParserInfo Command
piCommand = info description pCommand

  where
  description = ""

--------------------------------------------------

{-|

-}

piCreateProjectOptions :: P.ParserInfo CreateProjectOptions
piCreateProjectOptions = info description do

  globals     <- pGlobalOptions

  location    <- pLocation

  destination <- pDestination

  license     <- pLicense

  return CreateProjectOptions{..}

  where

  description = "Create a project"

--------------------------------------------------

{-|

-}

piDownloadProjectOptions :: P.ParserInfo DownloadProjectOptions
piDownloadProjectOptions = info description do

  globals     <- pGlobalOptions

  location    <- pLocation

  destination <- pDestination

  method      <- pFetchBy

  return DownloadProjectOptions{..}

  where

  description = "Download a project"

--------------------------------------------------

{-|

-}

piResolveConfigurationOptions :: P.ParserInfo ResolveConfigurationOptions
piResolveConfigurationOptions = info description do

  globals     <- pGlobalOptions

  return ResolveConfigurationOptions{..}

  where

  description = "Print an explicit (\"standalone\") configuration file merging the implicit configurations of: the current values of skeletor's environment variables; the user configuration in skeletor's XDG_CONFIG_DIR; skeletor's builtin projects and locations."

--------------------------------------------------
-- « Parser »s -----------------------------------
--------------------------------------------------

{-| The (top-level) program invocation; @main@ parses this.

If no subcommand can be parsed, real ('pCommand')
or fake ('pPseudoSubCommand'), the parser fails; and
@main@ throws an exception, exiting with exit code @2@.

Parses the program's (primary) subcommands.

-}

pCommand :: P.Parser Command
pCommand = do

  globals <- pGlobalOptions

  command <- pSubCommand

  return (fromResult (globals, command))

  where

  pSubCommand :: P.Parser Command
  pSubCommand = pFakeSubCommand <|> pRealSubCommand
    where

    pRealSubCommand = asum

      [ P.hsubparser ps
      , P.subparser  qs
      ]

    ps = mconcat

      [ (P.command "create" (CommandCreateProject        <$> piCreateProjectOptions))
      , (P.command "fetch"  (CommandDownloadProject      <$> piDownloadProjectOptions))
      , (P.command "config" (CommandResolveConfiguration <$> piResolveConfigurationOptions))
      ]

    qs = mconcat

      [ pVersionRealCommand
      , pLicenseRealCommand
      ]

    pFakeSubCommand :: P.Parser Command
    pFakeSubCommand = asum

      [ pVersionFakeCommand
      ]

    pVersionFakeCommand :: P.Parser Command
    pVersionFakeCommand = (P.flag' (CommandPrintVersion def)) $ mconcat

            [ P.long "version"
            , P.style P.bold
            , P.help "(The {{{ --version }}} option is an alias for the {{{ version }}} subcommand.)"
            ]

    pVersionRealCommand :: P.Mod P.CommandFields Command
    pVersionRealCommand = P.command "version" piVersionCommand

    pLicenseRealCommand :: P.Mod P.CommandFields Command
    pLicenseRealCommand = P.command "license" piLicenseCommand

    piVersionCommand :: P.ParserInfo Command
    piVersionCommand = info helpVersion do
      globals  <- pGlobalOptions
      return (CommandPrintVersion globals)

    piLicenseCommand :: P.ParserInfo Command
    piLicenseCommand = info helpLicense do
      globals  <- pGlobalOptions
      return (CommandPrintLicense globals)

    helpVersion :: String
    helpVersion = "Print the version of this program. The format is: dot-separated numerics. For example: {{{ 0.11.0 }}}. When the verbosity is {{{ 1 }}} (the default) or less, no other text is printed (also see option {{{ --verbose }}}); when {{{ 2 }}} or greater, also print the patch version (i.e. the git commit) and build information (the compiler version, and transitive dependencies' versions)."

    helpLicense :: String
    helpLicense = "Print the license of this program. The format is: an SPDX License Identifier (alphanumerics, plus hyphens and/or dots). For example: {{{ Apache-2.0 }}}. When the verbosity is {{{ 1 }}} (the default) or less, no other text is printed (also see option {{{ --verbose }}}); when {{{ 2 }}} or greater, also print the license contents."

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
        , embolden
        , P.help    "Enable verbose messages. (Includes network progress from downloading any resources. Includes printing the config that's derived from the invokation of this command: ①, parsing these command-line options; and ②, defaulting the values of any optional options.)"
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
        , P.action       "directory"

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
        , P.action       "directory"
-- TODO , P.completer (P.mkCompleter ... defaultTarballFileExtensions)

        , embolden
        , P.help    "NAME of, or LOCATION of, a project." -- TODO -- 
        ])

-- TODO make sure symlinks to directories work.

-- P.completer (P.mkCompleter (completeFilesWithExtension tarballExtensions))

-- tar
-- zip
-- json

-- completeFilesWithExtension :: [String] -> (String -> IO [String])
-- completeFilesWithExtension extensions prefix = do

-- Directory.
-- File.

--------------------------------------------------

{-|

-}

pDestination :: P.Parser FilePath
pDestination = P.option rDestination (mconcat

        [ P.long    "destination"
        , P.metavar "-d"

        , P.completeWith knownDestinations
        , P.action       "directory"

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
        , P.help    "The project's license (an SPDX license identifier). Examples include: {{{ GPL-3.0-or-later }}}, {{{ GPL-3.0-only }}}, {{{ BSD-3-Clause }}}, {{{ CC-BY-SA-4.0 }}}, {{{ MIT }}}. Press <tab> (twice) to autocomplete all (~350) licenses."
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
        , P.help    "Like {{{ --license _», but only for Open Source Initiative licenses."
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
        , P.help    "Like {{{ --license _», but only for Free/Libre and Open-Source Software (a.k.a Copyleft) licenses."
        ]))

--------------------------------------------------

options :: P.Parser Options
options = do

  projectpath <- optional (P.strOption (mconcat

        [ (P.long    "project-filepath")
        , (P.short   'f')
        , (P.metavar "PROJECT_PATH")
        , P.action  "directory"
        , P.help    "Which project skeleton, by path. (When both {{{ --project-filepath }}} and {{{ --project-name }}} are given, this option takes precedence. When neither are given, the default value equivalent to {{{ --project-name=default }}})."
        , P.style P.bold
        ]))

  projectname <- optional (P.strOption (mconcat

        [ (P.long    "project-name")
        , (P.short   'p')
        , (P.metavar "PROJECT_NAME")
        , P.completeWith builtinProjectNames
        , P.help    "Which project skeleton, by name. (Press the {{{ TAB }}} key for shell-completion of known projects)."
        , P.style P.bold
        ]))

  subdirectory <- optional (P.strOption (mconcat
        [ (P.long    "subdir")
        , P.action  "file"
        , P.help    "The subdirectory of the {{{ --location }}} (when unpacked)."
        , P.style P.bold
        ]))

  configpath <- optional (P.strOption (mconcat

        [ (P.long    "config")
        , (P.short   'c')
        , (P.metavar "CONFIG_FILE")
        , P.action  "file"
        , P.help    "Non-Command-Line Options & Arguments — most (but not all) options can be passed via an {{{ INI }}} file (c.f. a UNIX-style {{{ .conf }}} file). Relative filepaths are interpreted relative: to ① the current directory from which this command was invoked; ② to the XDG configuraton directories (both global and user). Absolute filepaths are accepted too. NOTE any explicit Command-Line options override any options written in CONFIG_FILE."
        , P.style P.bold
        ]))

  bindings <- many (P.option rBinding (mconcat

        [ (P.long    "binding")
        , (P.short   'b')
        , (P.metavar "VARIABLE_BINDING")
        , P.help    "A configuration variable binding. e.g. {{{ -b \"name=Sam Boosalis\" }}} (NOTE the quotes are stripped from the argument by the shell, they group the {{{ name=value }}} into a single argument, when the {{{ value }}} has whitespace.)."
        , P.style P.bold
        ]))

  environment <- defaulting (Bindings []) (P.option rBindings (mconcat

        [ (P.long    "bindings")
        , (P.short   'e')
        , (P.metavar "VARIABLE_BINDING...")
        , P.help    "A set of configuration variable bindings. e.g. {{{ -e 'user=sboosali:name=Sam Boosalis:' }}}. one {{{ --bindings _ }}} is equivalent to multiple {{{ --binding _ --binding _ ... }}}."
        , P.style P.bold
        ]))

  license <- defaulting "Apache-2.0" (P.strOption (mconcat

        [ (P.long    "license")
        , (P.metavar "LICENSE")
        , P.completeWith knownLicenseIds
        , P.help    "The PROJECTS's spdx license identifier."
        , P.style P.bold
        ]))

  return Options{..}

--------------------------------------------------
-- Utilities -------------------------------------
--------------------------------------------------

fromResult :: Result -> Command
fromResult (globals, command) = globals `addOptionsToCommand` command

--------------------------------------------------

addOptionsToCommand :: GlobalOptions -> Command -> Command
addOptionsToCommand globalOptions = \case

  CommandCreateProject localOptions ->

    CommandCreateProject (globalOptions `addGlobalOptionsTo` localOptions)

  CommandDownloadProject localOptions ->

    CommandDownloadProject (globalOptions `addGlobalOptionsTo` localOptions)

  CommandResolveConfiguration localOptions ->

    CommandResolveConfiguration (globalOptions `addGlobalOptionsTo` localOptions)

  command -> command

--------------------------------------------------

{-|


-}

info
  :: forall a.
    String -> P.Parser a
  -> P.ParserInfo a

info description parser = P.info (P.helper <*> parser) information
  where

  information :: P.InfoMod a
  information = mconcat

      [ P.fullDesc
      , P.progDesc description
      ]

--------------------------------------------------

fromParserResult :: P.ParserResult a -> Either CommandFailure a
fromParserResult = \case

    P.Success a           -> Right a
    P.Failure e           -> Left (toCommandFailure (P.renderFailure e programName))
    P.CompletionInvoked _ -> Left def

--------------------------------------------------

toCommandFailure :: (String, ExitCode) -> CommandFailure
toCommandFailure (stderr, exitcode) = CommandFailure{..}

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