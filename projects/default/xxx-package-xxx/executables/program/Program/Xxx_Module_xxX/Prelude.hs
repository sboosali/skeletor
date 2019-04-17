--------------------------------------------------

{- |

(component-specific custom prelude.)

-}

module Program.Xxx_Module_xxX.Prelude

  ( module EXPORT
  ) where

--------------------------------------------------
--- Exports --------------------------------------
--------------------------------------------------

import "xxx-package-xxx" Prelude_xxx_package_xxx as EXPORT

--------------------------------------------------

import          "spiros" Prelude.Spiros          as EXPORT

--------------------------------------------------
-- Imports ---------------------------------------
--------------------------------------------------

import qualified "base" System.IO as IO

--------------------------------------------------
-- Definitions -----------------------------------
--------------------------------------------------

putStdErr :: String -> IO ()
putStdErr = IO.hPutStr IO.stderr

{-# INLINEABLE putStdErr #-}

--------------------------------------------------

putStdOut :: String -> IO ()
putStdOut = IO.hPutStr IO.stdout

{-# INLINEABLE putStdOut #-}

--------------------------------------------------
-- EOF -------------------------------------------
--------------------------------------------------