{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ApplicativeDo     #-}

--------------------------------------------------
--------------------------------------------------

{-|



-}

module Program.Skeletor.Haskell.Options where

--------------------------------------------------
-- Imports (Internal) ----------------------------
--------------------------------------------------

import Program.Skeletor.Haskell.Types

--------------------------------------------------

import Skeletor.Haskell
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

import qualified "optparse-applicative" Options.Applicative as P

--------------------------------------------------

-- import           "base" _

--------------------------------------------------

import Prelude_exe

--------------------------------------------------
--------------------------------------------------

{-| Calls:

* 'preferences', 'parser'.
* 'mergeConfigs', 'defaultConfig'.

-}

getConfig :: IO Config
getConfig = do

  config_CommandLine <- P.customExecParser preferences parser
  let config_Default = defaultConfig
  
  let config = mergeConfigs config_CommandLine config_Default

  return config

--------------------------------------------------

{-| A pure 'getConfig'.

Calls:

* 'toConfig'.

@
>>> :set -XImplicitPrelude
>>> parseConfig [] == Right defaultConfig
True
>>> parseConfig (concat [ ["-v"],  ["--no-subdir"], ["-f", ".\/my-haskell-project-skeleton"] ])
Right (Config {verbosity = Verbose, filepath = Just ".\/my-haskell-project-skeleton", project = Just "default", subdirectory = PackageInRootDirectory})
@

-}

parseConfig :: [String] -> Either String Config
parseConfig args = mergedConfig

  where
  mergedConfig = parsedConfig <&> (\x -> mergeConfigs x defaultConfig)
  parsedConfig = toConfig args

--------------------------------------------------

{-| A pure parser of (mocked) command-line arguments, for @doctest@s (via 'execParserPure').

@
>>> :set -XImplicitPrelude
>>> toConfig []
Right (Config {verbosity = Concise, filepath = Nothing, project = Nothing, subdirectory = PackageInNamesakeSubdirectory})
>>> toConfig [] == toConfig ["--subdir"]
True
>>> toConfig (concat [ ["-v"],  ["--no-subdir"], ["-f", ".\/my-haskell-project-skeleton"] ])
Right (Config {verbosity = Verbose, filepath = Just "./my-haskell-project-skeleton", project = Nothing, subdirectory = PackageInRootDirectory})
@

See 'options'.

-}


toConfig :: [String] -> Either String Config
toConfig args = resultConfig

  where
  resultConfig = case go args of
    P.Success x -> Right x
    P.Failure k -> Left $ show k
    _           -> Left ""

  go = P.execParserPure preferences parser

--------------------------------------------------

{-| 

@
mergeConfigs extraConfig baseConfig
@

-}

mergeConfigs :: Config -> Config -> Config
mergeConfigs extraConfig@Config{ projectname = extraProject } Config{ projectname = baseProject } = updateConfig extraConfig

  where
  updateConfig = case extraProject of
    Nothing -> (\x -> x{ projectname = baseProject })
    Just{}  -> id

--------------------------------------------------

{-| Uses:

* 'P.fullDesc'
* 'options'
* 'P.info'

-}

parser :: P.ParserInfo Config
parser = P.info options information

--------------------------------------------------

{-| Uses:

* 'P.fullDesc'
* 'options'
* 'P.info'

-}

information :: P.InfoMod a
information = mconcat

  [ P.fullDesc
  , P.failureCode 2 -- exit code — when a parse error occurs.
  , P.headerDoc $ Just  "----------------------------------------"
  , P.footerDoc $ Just  "----------------------------------------"
  ]

--------------------------------------------------

{-| Uses:

* 'P.disambiguate'
* 'P.showHelpOnError'
* 'P.showHelpOnEmpty'

-}

preferences :: P.ParserPrefs
preferences = P.prefs (mconcat

  [ P.disambiguate
  , P.showHelpOnError
  , P.showHelpOnEmpty
  ])

--------------------------------------------------
--------------------------------------------------

rBindings :: P.ReadM Bindings
rBindings = P.eitherReader (A.parseOnly p . T.pack)
  where

  p = pBindings bindingSyntax

--------------------------------------------------

rBinding :: P.ReadM Binding
rBinding = P.eitherReader (A.parseOnly p . T.pack)
  where

  p = pBinding bindingSyntax

--------------------------------------------------

{- | a colon.

(while a « = » (the equals sign) is more natural,
on the cmdln, it would be gobbled under « optparse-applicative »'s syntax)

-}

bindingSyntax :: BindingSyntax
bindingSyntax = posixBindingSyntax

--------------------------------------------------
--------------------------------------------------

{-|

Options, Arguments, and Flags include:

* @-v@, @--verbose@.
* @--subdir@, @--no-subdir@.
* @-f@, @--project-filepath@, @-p@, @--project-name@,

-}

options :: P.Parser Config
options = do

  verbosity <- (P.flag Concise Verbose) (mconcat

        [ P.long    "verbose"
        , P.short   'v'
        , P.help    "Enable verbose messages. (Includes printing the config that's derived from the invokation of this command: [1] parsing these command-line options; and [2] defaulting the values of any optional options)."
        ])

  printVersion <- P.switch (mconcat

        [ P.long    "version"
        , P.help    "Print the version of this program. The format is, for example, « 0.0.0 ». No other text is printed."
        ])

  printLicense <- P.switch (mconcat   -- TODO -- subcommand, not option.

        [ P.long    "license"
        , P.help    "Print the SPDX license identifier of this program, then print out the license text."
        ])

  dryrun <- (P.flag TrueRun DryRun) (mconcat

        [ P.long    "dryrun"
        , P.short   'i'
        , P.help    "Whether the execution will just be a 'dry-run' (i.e. effects are disabled, instead they are printed out)."
        ])

  projectpath <- optional (P.strOption (mconcat

        [ P.long    "project-filepath"
        , P.short   'f'
        , P.metavar "PROJECT_PATH"
        , P.action  "directory"
        , P.help    "Which project skeleton, by path. (When both « --project-filepath » and « --project-name » are given, this option takes precedence. When neither are given, the default value equivalent to « --project-name=default »)."
        ]))

  projectname <- optional (P.strOption (mconcat

        [ P.long    "project-name"
        , P.short   'p'
        , P.metavar "PROJECT_NAME"
        , P.completeWith knownProjectNames
        , P.help    "Which project skeleton, by name. (Press the « TAB » key for shell-completion of known projects)."
        ]))

  subdirectory <- ((P.flag' PackageInRootDirectory
         (mconcat
              [ P.long    "no-subdir"
              --, P.metavar "SUBDIR(✓)"
              , P.help    "Whether the (singleton-package) project has its package in its root directory (When both « --no-subdir » and « --subdir » are given, this option takes precedence)."
              ]))
        <|>
        (P.flag' PackageInNamesakeSubdirectory
           (mconcat
              [ P.long    "subdir"
              --, P.metavar "SUBDIR(❌)"
              , P.help    "Whether the (singleton-package) project has a separate subdirectory for its package (the default)."
              ]))
        <|> pure def
      )

  configpath <- optional (P.strOption (mconcat

        [ P.long    "config"
        , P.short   'c'
        , P.metavar "CONFIG_FILE"
        , P.action  "file"
        , P.help    "Non-Command-Line Options & Arguments — most (but not all) options can be passed via an « INI » file (c.f. a UNIX-style « .conf » file). Relative filepaths are interpreted relative: to (1) the current directory from which this command was invoked; (2) to the XDG configuraton directories (both global and user). Absolute filepaths are accepted too. NOTE any explicit Command-Line options override any options written in CONFIG_FILE."
        ]))

  bindings <- many (P.option rBinding (mconcat

        [ P.long    "binding"
        , P.short   'b'
        , P.metavar "VARIABLE_BINDING"
        , P.help    "A configuration variable binding. e.g. « -b \"name=Sam Boosalis\" » (NOTE the quotes are stripped from the argument by the shell, they group the « name=value » into a single argument, when the « value » has whitespace.)."
        ]))

  environment <- (P.option rBindings (mconcat

        [ P.long    "binding."
        , P.short   'e'
        , P.metavar "VARIABLE_BINDING..."
        , P.help    "A set of configuration variable bindings. e.g. « -e 'user=sboosali:name=Sam Boosalis:' ». one « --bindings _ » is equivalent to multiple « --binding _ --binding _ ...»."
        ]))

  license <- P.switch (mconcat   -- TODO -- subcommand, not option.

        [ P.long    "license"
        , P.metavar "LICENSE"
        , P.completeWith (licenseId <$> allLicenses)
        , P.help    "Print the SPDX license identifier of this program, then print out the license text."
        ])

  return Config{..}

--------------------------------------------------
{- Notes -----------------------------------------

sample :: Parser Sample
sample = subparser

       ( command "hello" (info hello (progDesc "Print greeting"))
      <> command "goodbye" (info (pure Goodbye) (progDesc "Say goodbye"))
       )

      <|> subparser

       ( command "bonjour" (info hello (progDesc "Print greeting"))
      <> command "au-revoir" (info (pure Goodbye) (progDesc "Say goodbye"))
      <> commandGroup "French commands:"
      <> hidden
       )

--------------------------------------------------

(italics . parens) (progDescDoc "")

--------------------------------------------------



-------------------------------------------------}