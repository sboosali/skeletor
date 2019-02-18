--------------------------------------------------
--------------------------------------------------

{-|



-}

module Program.Skeletor.Haskell.Version

  ( programVersion
  ) where

--------------------------------------------------
--------------------------------------------------

import qualified "base" Data.Version as Version
import           "base" Data.Version (Version (..))

--------------------------------------------------
--------------------------------------------------

import Prelude_exe

--------------------------------------------------
--------------------------------------------------

-- | 

programVersion :: Version
programVersion = Version.makeVersion (majorVersion ++ minorVersion)

  where

  majorVersion = [0,1]
  minorVersion = [0]

--------------------------------------------------
--------------------------------------------------
