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

import Program.Skeletor.Haskell.Prelude

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