--------------------------------------------------
-- Extensions ------------------------------------
--------------------------------------------------

{-# LANGUAGE BlockArguments        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ApplicativeDo         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns        #-}

--------------------------------------------------

{-|

-}

module Program.Xxx_Module_xxX.IO where

--------------------------------------------------
-- Imports (Internal) ----------------------------
--------------------------------------------------

import Program.Xxx_Module_xxX.Types
import Program.Xxx_Module_xxX.Constants
import Program.Xxx_Module_xxX.Prelude

--------------------------------------------------
-- Imports (External) ----------------------------
--------------------------------------------------

import qualified "base" System.IO as IO

--------------------------------------------------
-- IO --------------------------------------------
--------------------------------------------------

{- | Any @xxx-program-xxx@ invocation executes @runCommand@.

== CLI

@
$ xxx-program-xxx ...
@

-}

runCommand :: Command -> IO ()
runCommand Command{ subcommand, options } = go subcommand
  where

  go :: Subcommand -> IO ()
  go = \case

      Fetch srcdst -> do
        fetch options srcdst

      PrintVersion -> do
        printVersion options

      PrintLicense -> do
        printLicense options

--------------------------------------------------

{- | 

== CLI

@
$ xxx-program-xxx fetch --input ... --output ... version
@

-}

fetch :: Options -> SrcDst -> IO ()
fetch Options{..} = \SrcDst{src,dst} -> do

  go src dst

  where

  ------------------------------

  go :: Src -> Dst -> IO ()
  go src dst = do

    putStdErr (show src)
    putStdErr (show dst)

    writeDst dst

  ------------------------------

  writeDst :: Dst -> IO ()
  writeDst dst = case force of

      RespectExisting -> do
        nothing

      OverwriteExisting -> do
        --TODO-- writeFile dst
        nothing

  ------------------------------

{-# INLINEABLE fetch #-}

--------------------------------------------------

{- | 

== CLI

@
$ xxx-program-xxx print version
@

-}

printVersion :: Options -> IO ()
printVersion Options{..} = do

  go verbose

  where

  go = \case

    Quiet   -> printVersionConcise
    Concise -> printVersionConcise

    Verbose -> printVersionVerbose
    Loud    -> printVersionVerbose

  printVersionConcise = do

    putStrLn $ programVersionBranch

  printVersionVerbose = do

    let versionLine = concat [ programName, ", version ", programVersion ]

    putStrLn $ versionLine

--  let versionString = versionStringBranch ++ versionStringTags

{-# INLINEABLE printVersion #-}

--------------------------------------------------

{- | 

== CLI

@
$ xxx-program-xxx print license
@

-}

printLicense :: Options -> IO ()
printLicense Options{..} = do

  go verbose

  where

  go = \case

    Quiet   -> printLicenseConcise
    Concise -> printLicenseConcise

    Verbose -> printLicenseVerbose
    Loud    -> printLicenseVerbose

  printLicenseConcise = do

    putStrLn $ programLicenseIdentifier

  printLicenseVerbose = do

    printLicenseConcise
    putStrLn ""

    putStrLn $ programLicenseContents

{-# INLINEABLE printLicense #-}

--------------------------------------------------
-- Utilities -------------------------------------
--------------------------------------------------

--------------------------------------------------
-- EOF -------------------------------------------
--------------------------------------------------