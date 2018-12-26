--------------------------------------------------
--------------------------------------------------

{-|



-}

module Executable.Xxx_Module_xxX.Options where

--------------------------------------------------
--------------------------------------------------

import Executable.Xxx_Module_xxX.Types

--------------------------------------------------
-- Imports: Internal -----------------------------
--------------------------------------------------

---import qualified "xxx-package-xxx" Executable.Xxx_Module_xxX as Xxx_ModuleAbbreviation_xxX

--------------------------------------------------

import Prelude_exe

--------------------------------------------------
-- Imports: External -----------------------------
--------------------------------------------------

import qualified "optparse-applicative" Options.Applicative as P

--------------------------------------------------
-- CLI -------------------------------------------
--------------------------------------------------

{-|

Options, Arguments, and Flags include:

* @--dry-run@.
* @-v@, @--verbose@.
* @-f@, @--filepath@.

-}

options :: P.Parser Config
options = Config

  <$> (P.flag Concise Verbose) (mconcat

        [ P.long    "verbose"
        , P.short   'v'
        , P.help    "Enable verbose messages. (Includes printing the config that's derived from the invokation of this command: [1] parsing these command-line options; and [2] defaulting the values of any optional options)."
        ])

  <*> optional (P.strOption (mconcat

        [ P.long    "filepath"
        , P.short   'f'
        , P.metavar "FILEPATH"
        , P.help    "Read FILEPATH for (additional) configuration options."
        , P.action  "directory"
        ]))

  <*> (P.flag TrueRun DryRun
         (mconcat
              [ P.long    "dry-run"
              , P.help    "Don't execute any actions, instead print them out."
              ]))

--------------------------------------------------

{-| Calls 'options' with:

* 'P.fullDesc'
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
--------------------------------------------------

{-| 

-}

getConfig :: IO Config
getConfig = do

  config_CommandLine <- P.customExecParser preferences parser
  let config_Default = defaultConfig

  let config         = mergeConfigs config_Default config_CommandLine

  return config

  where
  mergeConfigs :: Config -> Config -> Config
  mergeConfigs _ c = c
  --NOTE this just shadows the default config with the given one.

--------------------------------------------------

{-| 

-}

--------------------------------------------------
--------------------------------------------------
{- Notes

http://hackage.haskell.org/package/optparse-applicative-0.14.2.0/docs/Options-Applicative.html 

-}
--------------------------------------------------