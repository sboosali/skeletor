--------------------------------------------------
--------------------------------------------------

{-|



-}

module Program.Skeletor.Haskell.Options where

--------------------------------------------------

import Program.Skeletor.Haskell.Types

--------------------------------------------------

import Skeletor.Haskell

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
mergeConfigs extraConfig@Config{ project = extraProject } Config{ project = baseProject } = updateConfig extraConfig

  where
  updateConfig = case extraProject of
    Nothing -> (\x -> x{ project = baseProject })
    Just{}  -> id

--------------------------------------------------

{-| Uses:

* 'P.fullDesc'
* 'options'
* 'P.info'

-}

parser :: P.ParserInfo Config
parser = P.info options P.fullDesc

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

{-|

Options, Arguments, and Flags include:

* @-v@, @--verbose@.
* @--subdir@, @--no-subdir@.
* @-f@, @--project-filepath@, @-p@, @--project-name@,

-}

options :: P.Parser Config
options = Config

  <$> (P.flag Concise Verbose) (mconcat

        [ P.long    "verbose"
        , P.short   'v'
        , P.help    "Enable verbose messages. (Includes printing the config that's derived from the invokation of this command: [1] parsing these command-line options; and [2] defaulting the values of any optional options)."
        ])

  <*> (P.flag TrueRun DryRun) (mconcat

        [ P.long    "dryrun"
        , P.short   'i'
        , P.help    "Whether the execution will just be a 'dry-run' (i.e. effects are disabled, instead they are printed out)."
        ])

  <*> optional (P.strOption (mconcat

        [ P.long    "project-filepath"
        , P.short   'f'
        , P.metavar "PROJECT_PATH"
        , P.help    "Which project skeleton, by path. (When both « --project-filepath » and « --project-name » are given, this option takes precedence. When neither are given, the default value equivalent to « --project-name=default »)."
        , P.action  "directory"
        ]))

  <*> optional (P.strOption (mconcat

        [ P.long    "project-name"
        , P.short   'p'
        , P.metavar "PROJECT_NAME"
        , P.help    "Which project skeleton, by name. (Press the « TAB » key for shell-completion of known projects)."
        , P.completeWith knownProjectNames
        ]))

  <*> ((P.flag' PackageInRootDirectory
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

--------------------------------------------------







--------------------------------------------------
--------------------------------------------------
{- OldCode

--------------------------------------------------



--------------------------------------------------



--------------------------------------------------



--------------------------------------------------

>>> :set -XImplicitPrelude
>>> toConfig [] == Right defaultConfig
True
>>> toConfig [] == toConfig ["--subdir"]
True
>>> toConfig (concat [ ["-v"],  ["--no-subdir"], ["-f", ".\/my-haskell-project-skeleton"] ])
Right (Config {verbosity = Verbose, filepath = Just ".\/my-haskell-project-skeleton", project = Nothing, subdirectory = PackageInRootDirectory})



:set -XPackageImports
:set -XImplicitPrelude
import qualified "optparse-applicative" Options.Applicative as P
parseConfig (concat [ ["-v"],  ["--no-subdir"], ["-f", "./my-haskell-project-skeleton"] ])





    P.Failure (P.ParserFailure k) -> Left $ show (k "")

--------------------------------------------------


-- P.mkCompleter getCompletions


--------------------------------------------------

  <*> (P.flag PackageInNamesakeSubdirectory PackageInRootDirectory)
        ( P.long    "--no-subdir"
       <> P.metavar "SUBDIR?"
       <> P.help    "Whether the (singleton-package) project has (the default, without any flag) or doesn't have (with this flag) a separate subdirectory for its package."
       <> P.value
        )

--------------------------------------------------



--------------------------------------------------






--------------------------------------------------
-}



--------------------------------------------------
--------------------------------------------------
{- Notes

--------------------------------------------------

http://hackage.haskell.org/package/optparse-applicative-0.14.2.0/docs/Options-Applicative.html 

    switch = flag False True


--------------------------------------------------


flag Source#

:: a	
default value

-> a	
active value

-> Mod FlagFields a	
option modifier

-> Parser a	 
Builder for a flag parser.

A flag that switches from a "default value" to an "active value" when encountered. For a simple boolean value, use switch instead.

Note: Because this parser will never fail, it can not be used with combinators such as some or many, as these combinators continue until a failure occurs. See flag'.




strOption :: IsString s => Mod OptionFields s -> Parser s Source#

Builder for an option taking a String argument.




option :: ReadM a -> Mod OptionFields a -> Parser a Source#

Builder for an option using the given reader.

This is a regular option, and should always have either a long or short name specified in the modifiers (or both).

nameParser = option str ( long "name" <> short 'n' )



--------------------------------------------------


prefs :: PrefsMod -> ParserPrefs Source#

Create a ParserPrefs given a modifier


ParserPrefs	 

prefMultiSuffix :: String	
metavar suffix for multiple options

prefDisambiguate :: Bool	
automatically disambiguate abbreviations (default: False)

prefShowHelpOnError :: Bool	
always show help text on parse errors (default: False)

prefShowHelpOnEmpty :: Bool	
show the help text for a command or subcommand if it fails with no input (default: False)

prefBacktrack :: Bool	
backtrack to parent parser when a subcommand fails (default: True)

prefColumns :: Int	
number of columns in the terminal, used to format the help page (default: 80)


--------------------------------------------------

disambiguate :: PrefsMod Source#

Turn on disambiguation.

See https://github.com/pcapriotti/optparse-applicative#disambiguation

showHelpOnError :: PrefsMod Source#

Show full help text on any error.

showHelpOnEmpty :: PrefsMod Source#

Show the help text if the user enters only the program name or subcommand.

This will suppress a Missing: error and show the full usage instead if a user just types the name of the program.



--------------------------------------------------


mkCompleter :: (String -> IO [String]) -> Completer Source#

Smart constructor for a Completer

listIOCompleter :: IO [String] -> Completer Source#

Create a Completer from an IO action

listCompleter :: [String] -> Completer Source#

Create a Completer from a constant list of strings.

bashCompleter :: String -> Completer Source#

Run a compgen completion action.

Common actions include file and directory. See http://www.gnu.org/software/bash/manual/html_node/Programmable-Completion-Builtins.html#Programmable-Completion-Builtins for a complete list.





--------------------------------------------------

-}
--------------------------------------------------