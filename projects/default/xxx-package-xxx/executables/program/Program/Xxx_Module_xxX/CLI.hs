--------------------------------------------------
-- Extensions ------------------------------------
--------------------------------------------------

{-# LANGUAGE BlockArguments        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ApplicativeDo         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns        #-}

--------------------------------------------------

{-| Command-Line Interface for @xxx-program-xxx@.

-}

module Program.Xxx_Module_xxX.CLI where

--------------------------------------------------
-- Imports (Internal) ----------------------------
--------------------------------------------------

import Program.Xxx_Module_xxX.Types
import Program.Xxx_Module_xxX.Prelude

--------------------------------------------------
-- Imports (External) ----------------------------
--------------------------------------------------

import qualified "optparse-applicative" Options.Applicative      as P
import qualified "optparse-applicative" Options.Applicative.Help as P hiding (fullDesc)

-- NOTE « attoparsec » uses strict « Text ».

--------------------------------------------------
-- Imports ---------------------------------------
--------------------------------------------------

import           "base" Data.Maybe
import           "base" System.Exit

--------------------------------------------------
-- CLI -------------------------------------------
--------------------------------------------------

getCommand :: IO Command
getCommand = do

  P.customExecParser preferences piCommand

{-# INLINEABLE getCommand #-}

--------------------------------------------------

parseCommand :: (MonadThrow m) => [String] -> m Command
parseCommand

  = P.execParserPure preferences piCommand
  > fromParserResult
  > ( either throwM return )

{-# INLINEABLE parseCommand #-}

--------------------------------------------------

{-|

-}

preferences :: P.ParserPrefs
preferences = P.prefs (mconcat xs)
  where

  xs =
    [ P.showHelpOnError
    , P.showHelpOnEmpty
    ]

{-# INLINEABLE preferences #-}

--------------------------------------------------
-- « ParserInfo »s -------------------------------
--------------------------------------------------

-- | 

piCommand :: P.ParserInfo Command
piCommand = info description pCommand

  where
  description = "{{{ xxx-program-xxx }}} is a program for fetching and extending a list of {{{ Magic: The Gathering }} cards."

{-# INLINEABLE piCommand #-}

--------------------------------------------------

-- | 

piFetch :: P.ParserInfo Subcommand
piFetch = info description pFetch

  where
  description = "Fetch from SRC, save in DST."

{-# INLINEABLE piFetch #-}

--------------------------------------------------

-- | 

piPrint :: P.ParserInfo Subcommand
piPrint = info description pPrint

  where
  description = "Print out the program version or license, or its current configuration."

{-# INLINEABLE piPrint #-}

--------------------------------------------------
-- « Parser »s -----------------------------------
--------------------------------------------------

{- | Command-line (sub'Command's and 'Option's).

@xxx-program-xxx@'s @main@ parser.

-}

pCommand :: P.Parser Command
pCommand = do

  options    <- pOptions
  subcommand <- pSubcommand

  return Command{options,subcommand}

{-# INLINEABLE pCommand #-}

--------------------------------------------------

{- | @xxx-program-xxx@'s (global) options. -}

pOptions :: P.Parser Options
pOptions = do

  ------------------------------

  verbose <- (P.flag Concise Verbose) (mconcat

        [ P.long    "verbose"
        , P.short   'v'
        , embolden
        , P.help    "Enable verbose messages. (Includes network progress from downloading any resources. Includes printing the config that's derived from the invocation of this command: (1), parsing these command-line options; and (2), defaulting the values of any optional options.). {{{ -v }}} abbreviates \"verbose\"."
        ])

  ------------------------------

  dryrun <- (P.flag TrueRun DryRun) (mconcat

        [ P.long    "dryrun"
        , P.short   'i'
        , embolden
        , P.help    "Disable effects. Whether the execution will just be a 'dry-run' (i.e. most effects are disabled, instead they are printed out). {{{ -i }}} abbreviates \"information\"."
        ])

  ------------------------------

  force <- (P.flag RespectExisting OverwriteExisting) (mconcat

        [ P.long    "force"
        , P.short   'f'
        , P.style   P.bold
        , P.help    "Overwrite FILE. Whether existing files will be overwritten, or preserved (prompting for confirmation). {{{ -f }}} abbreviates \"forcefully overwrite\"."
        ])

  ------------------------------

  return Options{..}

{-# INLINEABLE pOptions #-}

--------------------------------------------------

{- | @xxx-program-xxx@'s 'Subcommand's. -}

pSubcommand :: P.Parser Subcommand
pSubcommand = P.hsubparser ps
  where

  ps =
      [ P.command "fetch" piFetch
      , P.command "print" piPrint
      ]

{-# INLINEABLE pSubcommand #-}

--------------------------------------------------

-- | 

pFetch :: P.Parser Subcommand
pFetch = do

  srcdst <- pSrcDst

  return (Fetch srcdst)

{-# INLINEABLE pFetch #-}

--------------------------------------------------
-- « Parser »s -----------------------------------
--------------------------------------------------

-- | 

pSrcDst :: P.Parser SrcDst
pSrcDst = do

  src <- pSrc
  dst <- pDst

  return SrcDst{ src, dst }

{-# INLINEABLE pSrcDst #-}

--------------------------------------------------
-- EOF -------------------------------------------
--------------------------------------------------




















