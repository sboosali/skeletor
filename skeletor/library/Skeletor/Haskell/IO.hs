--------------------------------------------------
--------------------------------------------------

{-| Filesystem Interaction & Manipulation.

e.g.

@
>> projectTree <- readProject (Right DefaultHaskellProject)
>> :i projectTree
TODO
@

e.g.

@
>> findProjectFiles (Right DefaultHaskellProject) >>= (traverse_ putStrLn)

.\/default
.\/default\/.gitignore
.\/default\/LICENSE.txt
.\/default\/Makefile
.\/default\/README.md
.\/default\/xxx-package-xxx
.\/default\/xxx-package-xxx\/LICENSE.txt
.\/default\/xxx-package-xxx\/xxx-package-xxx.cabal
.\/default\/xxx-package-xxx\/benches
.\/default\/xxx-package-xxx\/benches\/space
.\/default\/xxx-package-xxx\/benches\/space\/.dir-locals.el
.\/default\/xxx-package-xxx\/benches\/space\/Bench
.\/default\/xxx-package-xxx\/benches\/space\/Bench\/Xxx_Module_xxX.hs
.\/default\/xxx-package-xxx\/benches\/space\/Main.hs
.\/default\/xxx-package-xxx\/benches\/time
.\/default\/xxx-package-xxx\/benches\/time\/.dir-locals.el
.\/default\/xxx-package-xxx\/benches\/time\/Bench
.\/default\/xxx-package-xxx\/benches\/time\/Bench\/Xxx_Module_xxX.hs
.\/default\/xxx-package-xxx\/benches\/time\/Main.hs
.\/default\/xxx-package-xxx\/dist
.\/default\/xxx-package-xxx\/dist\/cabal-config-flags
.\/default\/xxx-package-xxx\/executables
.\/default\/xxx-package-xxx\/executables\/example
.\/default\/xxx-package-xxx\/executables\/example\/.dir-locals.el
.\/default\/xxx-package-xxx\/executables\/example\/Example
.\/default\/xxx-package-xxx\/executables\/example\/Example\/Xxx_Module_xxX.hs
.\/default\/xxx-package-xxx\/executables\/example\/Main.hs
.\/default\/xxx-package-xxx\/executables\/program
.\/default\/xxx-package-xxx\/executables\/program\/.dir-locals.el
.\/default\/xxx-package-xxx\/executables\/program\/Main.hs
.\/default\/xxx-package-xxx\/executables\/program\/Prelude_exe.hs
.\/default\/xxx-package-xxx\/executables\/program\/Xxx_Module_xxX
.\/default\/xxx-package-xxx\/executables\/program\/Xxx_Module_xxX\/Main.hs
.\/default\/xxx-package-xxx\/executables\/program\/Xxx_Module_xxX\/Options.hs
.\/default\/xxx-package-xxx\/internals
.\/default\/xxx-package-xxx\/internals\/.dir-locals.el
.\/default\/xxx-package-xxx\/internals\/Internal
.\/default\/xxx-package-xxx\/internals\/Internal\/Xxx_Module_xxX.hs
.\/default\/xxx-package-xxx\/internals\/Prelude_xxx_package_xxx.hs
.\/default\/xxx-package-xxx\/library
.\/default\/xxx-package-xxx\/library\/.dir-locals.el
.\/default\/xxx-package-xxx\/library\/Xxx_Module_xxX
.\/default\/xxx-package-xxx\/library\/Xxx_Module_xxX\/Core.hs
.\/default\/xxx-package-xxx\/library\/Xxx_Module_xxX\/Derived.hs
.\/default\/xxx-package-xxx\/library\/Xxx_Module_xxX\/Types.hs
.\/default\/xxx-package-xxx\/library\/Xxx_Module_xxX.hs
.\/default\/xxx-package-xxx\/tests
.\/default\/xxx-package-xxx\/tests\/golden
.\/default\/xxx-package-xxx\/tests\/golden\/.dir-locals.el
.\/default\/xxx-package-xxx\/tests\/golden\/GoldenTests.hs
.\/default\/xxx-package-xxx\/tests\/golden\/Test
.\/default\/xxx-package-xxx\/tests\/golden\/Test\/Golden
.\/default\/xxx-package-xxx\/tests\/golden\/Test\/Golden\/Xxx_Module_xxX.hs
.\/default\/xxx-package-xxx\/tests\/property
.\/default\/xxx-package-xxx\/tests\/property\/.dir-locals.el
.\/default\/xxx-package-xxx\/tests\/property\/PropertyTests.hs
.\/default\/xxx-package-xxx\/tests\/property\/Test
.\/default\/xxx-package-xxx\/tests\/property\/Test\/Property
.\/default\/xxx-package-xxx\/tests\/property\/Test\/Property\/Xxx_Module_xxX.hs
.\/default\/xxx-package-xxx\/tests\/unit
.\/default\/xxx-package-xxx\/tests\/unit\/.dir-locals.el
.\/default\/xxx-package-xxx\/tests\/unit\/Test
.\/default\/xxx-package-xxx\/tests\/unit\/Test\/Unit
.\/default\/xxx-package-xxx\/tests\/unit\/Test\/Unit\/Xxx_Module_xxX.hs
.\/default\/xxx-package-xxx\/tests\/unit\/UnitTests.hs
.\/default\/cabal.project
.\/default\/documents
.\/default\/documents\/LICENSE-GPL3-StandardLicenseHeader.txt
.\/default\/documents\/LICENSE-GPL3-TerminalLicenseNotice.txt
.\/default\/documents\/LICENSE-GPL3.txt
.\/default\/documents\/NOTES.md
.\/default\/nix
.\/default\/nix\/README.md
.\/default\/nix\/config
.\/default\/nix\/config\/default.nix
.\/default\/nix\/environment.nix
.\/default\/nix\/nixpkgs
.\/default\/nix\/nixpkgs\/default.nix
.\/default\/nix\/nixpkgs\/nixpkgs.json
.\/default\/nix\/nixpkgs\/update-nixpkgs.sh
.\/default\/nix\/overlays
.\/default\/nix\/overlays\/default.nix
.\/default\/nix\/overlays\/ghcs.nix
.\/default\/nix\/overlays\/overlay.nix
.\/default\/nix\/packages.nix
.\/default\/nix\/project.nix
.\/default\/nix\/shell.nix
.\/default\/scripts
.\/default\/scripts\/README.md
.\/default\/scripts\/sboo
.\/default\/scripts\/sboo\/ghcid.sh
.\/default\/shell.nix
@

-}

module Skeletor.Haskell.IO where

--------------------------------------------------
--------------------------------------------------

import Skeletor.Haskell.Types
import Skeletor.Haskell.Core
import Skeletor.Haskell.Variable

--------------------------------------------------

import qualified "filemanip" System.FilePath.Find as Find
import           "filemanip" System.FilePath.Find (FindClause)

import           "filemanip" System.FilePath.GlobPattern ((~~))

--------------------------------------------------

import qualified "containers" Data.Map as Map
import           "containers" Data.Map (Map)

--------------------------------------------------

import Prelude_skeletor

--------------------------------------------------
--------------------------------------------------

{-|

@
≡ 'readProjectByIdentifier'
@

-}

readProject :: ProjectIdentifier -> IO FileTree
readProject = readProjectByIdentifier

--------------------------------------------------

{-|

@
≡ 'locateProject' '>>>' 'findProjectFilesByPath'
@

-}

findProjectFilesByIdentifier :: ProjectIdentifier -> IO [FilePath]
findProjectFilesByIdentifier = locateProject > findProjectFilesByPath

--------------------------------------------------

{-|

Skips 'ignoredDirectories', when descending.

Skips 'ignoredFiles' and non-files, when collecting.

-}

findProjectFilesByPath :: FilePath -> IO [FilePath]
findProjectFilesByPath = Find.find recursionPredicate filterPredicate

  where

  recursionPredicate :: FindClause Bool
  recursionPredicate = shouldRecurIntoSubdirectory <$> Find.directory

  filterPredicate :: FindClause Bool
  filterPredicate = isRegularFileM Find.&&? isGoodFilenameM
  
    where
    isRegularFileM  = (Find.fileType Find.==? Find.RegularFile)
    isGoodFilenameM = (shouldKeepFilename <$> Find.fileName)

  shouldRecurIntoSubdirectory :: FilePath -> Bool
  shouldRecurIntoSubdirectory = shouldIgnoreDirectory > not

  shouldKeepFilename :: FilePath -> Bool
  shouldKeepFilename = shouldIgnoreFilename > not

  shouldIgnoreDirectory :: FilePath -> Bool
  shouldIgnoreDirectory directory =
    any (directory ~~) ignoredDirectories

  shouldIgnoreFilename :: FilePath -> Bool
  shouldIgnoreFilename filename =
    any (filename ~~) ignoredFiles

--------------------------------------------------
--------------------------------------------------

{-|

@
≡ 'locateProject' '>>>' 'readProjectByPath'
@

-}

readProjectByIdentifier :: ProjectIdentifier -> IO FileTree
readProjectByIdentifier = locateProject > readProjectByPath

--------------------------------------------------

{-|

-}

readProjectByPath :: FilePath -> IO FileTree
readProjectByPath project = do

  files <- findProjectFilesByPath project
  readFileTree files

--------------------------------------------------

{-|

-}

readFileTree :: [FilePath] -> IO FileTree
readFileTree paths = do

  filePathsAndContents <- go `traverse` paths  -- TODO error-handling

  forced <- forceIO filePathsAndContents  -- TODO error-handling

  let wrapped = fromList forced

  pure wrapped

  where

  go path = do
    body <- readFile path
    pure (path, body)

--------------------------------------------------
--------------------------------------------------

{-|

-}

--------------------------------------------------
--------------------------------------------------
{- Notes / Old Code




  shouldRecurIntoSubdirectory :: FindClause Bool
  shouldRecurIntoSubdirectory = do
  
      currentDirectory <- Find.directory 
  
      let shouldIgnore = shouldIgnoreDirectory currentDirectory
      let shouldRecur  = not shouldIgnore
  
      pure shouldRecur




UnknownOr KnownProject

Unknown Project 



{-|

-}

findProjectFilesByName :: UnknownOr KnownProject -> IO [FilePath]
findProjectFilesByName = findProjectFilesByPath

  where




-}
--------------------------------------------------