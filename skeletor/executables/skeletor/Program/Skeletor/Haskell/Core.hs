--------------------------------------------------
--------------------------------------------------

{-|



-}

module Program.Skeletor.Haskell.Core

  ( programWithOptions
  , programWithConfig
  ) where

--------------------------------------------------

import Skeletor.Haskell

--------------------------------------------------

import Program.Skeletor.Haskell.Types
import Program.Skeletor.Haskell.Options
import Program.Skeletor.Haskell.Config

--------------------------------------------------

-- import qualified "" _ as _

--------------------------------------------------

-- import           "base" _

--------------------------------------------------

import Prelude_exe

--------------------------------------------------
--------------------------------------------------

{-| 

-}

programWithOptions :: Options -> IO ()
programWithOptions options@Options{..} = do

  config <- toConfig options

  programWithConfig config

--------------------------------------------------
--------------------------------------------------

{-| 

-}

programWithConfig :: Config -> IO ()
programWithConfig config@Config{..} = do

  print config

--------------------------------------------------
--------------------------------------------------