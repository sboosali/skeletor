{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists   #-}
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

-- NOTE « attoparsec » uses strict « Text ».

--------------------------------------------------

import qualified "optparse-applicative" Options.Applicative      as P
import qualified "optparse-applicative" Options.Applicative.Help as P hiding (fullDesc)

--------------------------------------------------
-- Imports (Standard Library) --------------------
--------------------------------------------------

import qualified "text" Data.Text as T
import           "text" Data.Text (Text)

--------------------------------------------------
--------------------------------------------------

import Prelude_exe

--------------------------------------------------
-- Definitions -----------------------------------
--------------------------------------------------

{-| Calls:

* 'preferences', 'parser'.
* 'mergeOptions', 'defaultOptions'.

-}

getOptions :: IO Options
getOptions = do

  config_CommandLine <- P.customExecParser preferences parser
  let config_Default = defaultOptions
  
  let config = mergeOptions config_CommandLine config_Default

  return config

--------------------------------------------------

{-| A pure 'getOptions'.

Calls:

* 'toOptions'.

@
>>> :set -XImplicitPrelude
>>> parseOptions [] == Right defaultOptions
True
>>> parseOptions (concat [ ["-v"],  ["--no-subdir"], ["-f", ".\/my-haskell-project-skeleton"] ])
Right (Options {verbosity = Verbose, filepath = Just ".\/my-haskell-project-skeleton", project = Just "default", subdirectory = PackageInRootDirectory})
@

-}

parseOptions :: [String] -> Either String Options
parseOptions args = mergedOptions

  where
  mergedOptions = parsedOptions <&> (\x -> mergeOptions x defaultOptions)
  parsedOptions = toOptions args

--------------------------------------------------

{-| A pure parser of (mocked) command-line arguments, for @doctest@s (via 'execParserPure').

@
>>> :set -XImplicitPrelude
>>> toOptions []
Right (Options {verbosity = Concise, filepath = Nothing, project = Nothing, subdirectory = PackageInNamesakeSubdirectory})
>>> toOptions [] == toOptions ["--subdir"]
True
>>> toOptions (concat [ ["-v"],  ["--no-subdir"], ["-f", ".\/my-haskell-project-skeleton"] ])
Right (Options {verbosity = Verbose, filepath = Just "./my-haskell-project-skeleton", project = Nothing, subdirectory = PackageInRootDirectory})
@

See 'options'.

-}


toOptions :: [String] -> Either String Options
toOptions args = resultOptions

  where
  resultOptions = case go args of
    P.Success x -> Right x
    P.Failure k -> Left $ show k
    _           -> Left ""

  go = P.execParserPure preferences parser

--------------------------------------------------

{-| 

@
mergeOptions extraOptions baseOptions
@

-}

mergeOptions :: Options -> Options -> Options
mergeOptions extraOptions@Options{ projectname = extraProject } Options{ projectname = baseProject } = updateOptions extraOptions

  where
  updateOptions = case extraProject of
    Nothing -> (\x -> x{ projectname = baseProject })
    Just{}  -> id

--------------------------------------------------

{-| Uses:

* 'P.fullDesc'
* 'options'
* 'P.info'

-}

parser :: P.ParserInfo Options
parser = P.info (options <**> P.helper) information

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

rLicense :: P.ReadM License
rLicense = P.maybeReader parseLicense

--------------------------------------------------

{- | 'posixBindingSyntax'.

(a « = » (the equals sign) is more natural than a « : » (colon);
TODO ensure that, on the cmdln, it won't be gobbled under « optparse-applicative »'s syntax)

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

options :: P.Parser Options
options = do

  verbosity <- (P.flag Concise Verbose) (mconcat

        [ (P.long    "verbose")
        , (P.short   'v')
        , P.help    "Enable verbose messages. (Includes printing the config that's derived from the invokation of this command: ① parsing these command-line options; and ② defaulting the values of any optional options)."
        , P.style P.bold
        ])

  dryrun <- (P.flag TrueRun DryRun) (mconcat

        [ (P.long    "dryrun")
        , (P.short   'i')
        , P.help    "Whether the execution will just be a 'dry-run' (i.e. effects are disabled, instead they are printed out)."
        , P.style P.bold
        ])

  printVersion <- empty

     <|> (P.switch (mconcat

        [ (P.long    "print-version")
        , P.help    "Print the version of this program. The format is, for example, « 0.0.0 ». No other text is printed."
        , P.style P.bold
        ]))

     <|> (P.switch (mconcat

        [ (P.long    "version")
        , P.help    "Alias for « --print-version »."
        , P.style P.bold
        ]))

  printLicense <- P.switch (mconcat   -- TODO -- subcommand, not option.

        [ (P.long    "print-license")
        , P.help    "Print the SPDX license identifier of this program, then print out the license text."
        , P.style P.bold
        ])

  printConfig <- P.switch (mconcat
        [ (P.long    "print-config")
        , P.internal                  -- .hidden
        , P.help    "[INTERNAL] Print the internal configuration which the command-line options are parsed into."
        , P.style P.bold
        ])

  resolveConfiguration <- P.switch (mconcat [])

  projectpath <- optional (P.strOption (mconcat

        [ (P.long    "project-filepath")
        , (P.short   'f')
        , (P.metavar "PROJECT_PATH")
        , P.action  "directory"
        , P.help    "Which project skeleton, by path. (When both « --project-filepath » and « --project-name » are given, this option takes precedence. When neither are given, the default value equivalent to « --project-name=default »)."
        , P.style P.bold
        ]))

  projectname <- optional (P.strOption (mconcat

        [ (P.long    "project-name")
        , (P.short   'p')
        , (P.metavar "PROJECT_NAME")
        , P.completeWith builtinProjectNames
        , P.help    "Which project skeleton, by name. (Press the « TAB » key for shell-completion of known projects)."
        , P.style P.bold
        ]))

  subdirectory <- optional (P.strOption (mconcat
        [ (P.long    "subdir")
        , P.action  "file"
        , P.help    "The subdirectory of the « --location » (when unpacked)."
        , P.style P.bold
        ]))

  configpath <- optional (P.strOption (mconcat

        [ (P.long    "config")
        , (P.short   'c')
        , (P.metavar "CONFIG_FILE")
        , P.action  "file"
        , P.help    "Non-Command-Line Options & Arguments — most (but not all) options can be passed via an « INI » file (c.f. a UNIX-style « .conf » file). Relative filepaths are interpreted relative: to ① the current directory from which this command was invoked; ② to the XDG configuraton directories (both global and user). Absolute filepaths are accepted too. NOTE any explicit Command-Line options override any options written in CONFIG_FILE."
        , P.style P.bold
        ]))

  bindings <- many (P.option rBinding (mconcat

        [ (P.long    "binding")
        , (P.short   'b')
        , (P.metavar "VARIABLE_BINDING")
        , P.help    "A configuration variable binding. e.g. « -b \"name=Sam Boosalis\" » (NOTE the quotes are stripped from the argument by the shell, they group the « name=value » into a single argument, when the « value » has whitespace.)."
        , P.style P.bold
        ]))

  environment <- defaulting [] (P.option rBindings (mconcat

        [ (P.long    "bindings")
        , (P.short   'e')
        , (P.metavar "VARIABLE_BINDING...")
        , P.help    "A set of configuration variable bindings. e.g. « -e 'user=sboosali:name=Sam Boosalis:' ». one « --bindings _ » is equivalent to multiple « --binding _ --binding _ ...»."
        , P.style P.bold
        ]))

  license <- defaulting "BSD-3-Clause" (P.strOption (mconcat

        [ (P.long    "license")
        , (P.metavar "LICENSE")
        , P.completeWith knownLicenseIds
        , P.help    "The PROJECTS's spdx license identifier."
        , P.style P.bold
        ]))

  return Options{..}

  where

  defaulting x p = maybe x id <$> optional p

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