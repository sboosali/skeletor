{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TypeApplications #-}

--------------------------------------------------
--------------------------------------------------

{-| Core types, functions, and constants.

-}

module Skeletor.Haskell.Core where

--------------------------------------------------
--------------------------------------------------

import Skeletor.Haskell.Types
import Skeletor.Haskell.Find.Types

--import Skeletor.Haskell.Variable

--------------------------------------------------

--import           "filemanip" System.FilePath.GlobPattern (GlobPattern)

--------------------------------------------------

import Prelude_skeletor

--------------------------------------------------
-- Definitions -----------------------------------
--------------------------------------------------

{- | Default blacklists.

For haskell (@cabal@ & @stack@), emacs, and @nix@ files that are
local and\/or temporary.

-}

defaultFileFilters :: FileFilters
defaultFileFilters = ignoredDirectories <> ignoredFiles

--------------------------------------------------

-- | from my standard @.gitignore@.

ignoredDirectories :: FileFilters
ignoredDirectories =

  [ blacklistedDirectory "dist"
  , blacklistedDirectory "dist-newstyle"
  , blacklistedDirectory "dist-*"
  , blacklistedDirectory ".stack-work"
  , blacklistedDirectory ".cabal-sandbox"
  , blacklistedDirectory "result"
  , blacklistedDirectory "result-*"

  , blacklistedDirectory "/home/sboo/haskell/skeletor/projects/default/dist-newstyle" --TODO
  ]

--------------------------------------------------

-- | from my standard @.gitignore@.

ignoredFiles :: FileFilters
ignoredFiles = fromList (blacklistedFile <$> allFiles)

  where

  allFiles = concat @[]
    [ haskellFiles
    , emacsFiles
    , nixFiles
    ]

  haskellFiles =
    [ ".ghc.environment.*"
    , "cabal.project.local"
    , "cabal.sandbox.config"
    , "*.o"
    , "*.hi"
    , "*.chi"
    , "*.chs.h"
    ]

  emacsFiles =
    [ "*~"
    , "\\#*"
    , "\\.\\#*"
    , "\\#*\\#"
    , "TAGS"
    ]

  nixFiles =
    [ "result"
    ]

--------------------------------------------------
--------------------------------------------------