--------------------------------------------------
--------------------------------------------------

{-| 'Command' represents this program's subcommands.

-}

module Program.Skeletor.Haskell.Command
  (
    module Program.Skeletor.Haskell.Command.Types
  , module Program.Skeletor.Haskell.Command
  
  ) where

--------------------------------------------------
-- Exports ---------------------------------------
--------------------------------------------------

import Program.Skeletor.Haskell.Command.Types

--------------------------------------------------
-- Imports (Internal) ----------------------------
--------------------------------------------------

import Prelude_exe

--------------------------------------------------
-- Imports (External) ----------------------------
--------------------------------------------------

-- import qualified "" _ as _
-- import           "" _ ()

--------------------------------------------------
-- Imports (Standard Library) --------------------
--------------------------------------------------

-- import qualified "" _ as _
-- import           "" _ ()

--------------------------------------------------
-- Definitions -----------------------------------
--------------------------------------------------

--------------------------------------------------
-- Utilities -------------------------------------
--------------------------------------------------



--------------------------------------------------
-- Notes -----------------------------------------
--------------------------------------------------
{-

data Command

  = CommandCreateProject        CreateProjectOptions
  | CommandDownloadProject      DownloadProjectOptions
  | CommandResolveConfiguration ResolveConfigurationOptions

  deriving stock    (Show,Eq,Ord)
  deriving stock    (Generic)
  deriving anyclass (NFData)

--------------------------------------------------
--------------------------------------------------

{-|

-}

data CreateProjectOptions = CreateProjectOptions

  { globals     :: GlobalOptions
  , location    :: Location
  , destination :: FilePath
  , license     :: License
  }

  deriving stock    (Show,Eq,Ord)
  deriving stock    (Generic)
  deriving anyclass (NFData)

--------------------------------------------------
--------------------------------------------------

{-|

-}

data DownloadProjectOptions = DownloadProjectOptions

  { globals     :: GlobalOptions
  , location    :: Location
  , destination :: FilePath
  , method      :: FetchBy
  }

  deriving stock    (Show,Eq,Ord)
  deriving stock    (Generic)
  deriving anyclass (NFData)

--------------------------------------------------
--------------------------------------------------

{-|

-}

type ResolveConfigurationOptions = ()


-}