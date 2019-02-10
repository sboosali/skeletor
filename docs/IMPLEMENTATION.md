# Implementation Notes

```haskell
```


## Style

most imports are qualified:

```haskell
import qualified "" _ as _
import           "" _      ()
```



## `base`

### `Data.Monoid`

```haskell
newtype Alt f a

-- Monoid under <|>.
```

### `Control.Exception`

```haskell
catches :: IO a -> [Handler a] -> IO a 

Sometimes you want to catch two different sorts of exception. You could do something like

>>> f = expr `catch` \ (ex :: ArithException) -> handleArith ex
>>>          `catch` \ (ex :: IOException)    -> handleIO    ex

However, there are a couple of problems with this approach. The first is that having two exception handlers is inefficient. However, the more serious issue is that the second exception handler will catch exceptions in the first, e.g. in the example above, if handleArith throws an IOException then the second exception handler will catch it.

Instead, we provide a function catches, which would be used thus:

>>> f = expr `catches` [Handler (\ (ex :: ArithException) -> handleArith ex),
>>>                     Handler (\ (ex :: IOException)    -> handleIO    ex)]
```

```haskell
data Handler a =

  Exception e => Handler (e -> IO a)	 

You need this when using catches.

```



## `containers`

```haskell
foldMapWithKey :: Monoid m => (k -> a -> m) -> Map k a -> m 
```




## `zlib`

```haskell
module Codec.Compression.GZip
```

```haskell
decompress :: ByteString -> ByteString

Decompress a stream of data in the gzip format.

There are a number of errors that can occur. In each case an exception will be thrown. The possible error conditions are:

- if the stream does not start with a valid gzip header
- if the compressed stream is corrupted
- if the compressed stream ends permaturely

Note that the decompression is performed lazily. Errors in the data stream may not be detected until the end of the stream is demanded (since it is only at the end that the final checksum can be checked). If this is important to you, you must make sure to consume the whole decompressed stream before doing any IO action that depends on it.
```

```haskell
decompressWith :: DecompressParams -> ByteString -> ByteString

Like decompress but with the ability to specify various decompression parameters. Typical usage:

>>> decompressWith defaultCompressParams { ... }
```



## `filepath`

```haskell
takeExtensions :: FilePath -> String

Get all extensions.

>>> takeExtensions "/directory/path.ext" == ".ext"
>>> takeExtensions "file.tar.gz" == ".tar.gz"
```




## `turtle`

`Shell` ≡ `[] + IO + Managed`

the `Shell` Embeddings:

```haskell
select ::        [a] -> Shell a
liftIO ::      IO a  -> Shell a
using  :: Managed a  -> Shell a
```

`Shell` Embeddings obey these laws. Associativity Law:

```haskell
do { x <- select m; select (f x) } = select (do { x <- m; f x })
do { x <- liftIO m; liftIO (f x) } = liftIO (do { x <- m; f x })
do { x <- with   m; using  (f x) } = using  (do { x <- m; f x })
```

and Identity Law:

```haskell
select (return x) = return x
liftIO (return x) = return x
using  (return x) = return x
```

> ... and select obeys these additional laws:

```haskell
select xs <|> select ys = select (xs <|> ys)
select empty = empty
```




## `gitlib`

<http://hackage.haskell.org/package/gitlib-3.1.2/docs/Git-Tutorial.html>

## `hlibgit2`

Haskell Bindings to the @libgit@ C Library.

<https://github.com/jwiegley/gitlib/tree/master/hlibgit2>

```haskell
-- | type of a git object.

data ObjectType =
      TypeTree
    | TypeBlob
    | TypeCommit
    | TypeTag
    | TypeDeltaOff
    | TypeDeltaRef
```

```haskell
-- | Git object file type

data ObjectFileType =
      FileTypeDirectory
    | FileTypeRegularFile
    | FileTypeSymbolicLink
    | FileTypeGitLink
```

```haskell
-- | traditional unix permission for owner, group and permissions

data FilePermissions = FilePermissions
    { getOwnerPerm :: {-# UNPACK #-} !Perm
    , getGroupPerm :: {-# UNPACK #-} !Perm
    , getOtherPerm :: {-# UNPACK #-} !Perm
    }
    
-- | a bitfield representing a typical unix permission:
-- * bit 0 represents the read permission
-- * bit 1 represents the write permission
-- * bit 2 represents the execute permission

type Perm = Word8
```

```haskell
-- | an author or committer line
-- has the format: name <email> time timezone
-- FIXME: should be a string, but I don't know if the data is stored
-- consistantly in one encoding (UTF8)

data Person = Person
    { personName  :: !ByteString
    , personEmail :: !ByteString
    , personTime  :: !GitTime
    }
```

```haskell
-- | Represent a commit object.

data Commit hash = Commit
    { commitTreeish   :: !(Ref hash)
    , commitParents   :: [Ref hash]
    , commitAuthor    :: !Person
    , commitCommitter :: !Person
    , commitEncoding  :: Maybe ByteString
    , commitExtras    :: [CommitExtra]
    , commitMessage   :: !ByteString
    } deriving (Show,Eq)

data CommitExtra = CommitExtra
    { commitExtraKey   :: !ByteString
    , commitExtraValue :: !ByteString
    } deriving (Show,Eq)
```

```haskell
-- | Represent a signed tag.

data Tag hash = Tag
    { tagRef        :: !(Ref hash)
    , tagObjectType :: !ObjectType
    , tagBlob       :: !ByteString
    , tagName       :: !Person
    , tagS          :: !ByteString
    } deriving (Show,Eq)
```





## `network-uri`

### `Network.URI

```haskell
-- |Represents a general universal resource identifier using
--  its component parts.
--
--  For example, for the URI
--
--  >   foo://anonymous@www.haskell.org:42/ghc?query#frag
--
--  the components are:
--

data URI = URI

    { uriScheme     :: String           -- ^ @foo:@
    , uriAuthority  :: Maybe URIAuth    -- ^ @\/\/anonymous\@www.haskell.org:42@
    , uriPath       :: String           -- ^ @\/ghc@
    , uriQuery      :: String           -- ^ @?query@
    , uriFragment   :: String           -- ^ @#frag@
    }
```

```haskell
-- |Type for authority value within a URI

data URIAuth = URIAuth

    { uriUserInfo   :: String           -- ^ @anonymous\@@
    , uriRegName    :: String           -- ^ @www.haskell.org@
    , uriPort       :: String           -- ^ @:42@
    }
```

```haskell
-- |Blank URI

nullURI :: URI
nullURI = URI

    { uriScheme     = ""
    , uriAuthority  = Nothing
    , uriPath       = ""
    , uriQuery      = ""
    , uriFragment   = ""
    }
```






## `Cabal`

### module `Distribution.Simple.Program`

```haskell
-- | Represents a program which can be configured.
--
-- Note: rather than constructing this directly, start with 'simpleProgram' and
-- override any extra fields.
--

data Program = Program {

       -- | The simple name of the program, eg. ghc

       programName :: String,

       -- | A function to search for the program if its location was not
       -- specified by the user. Usually this will just be a call to
       -- 'findProgramOnSearchPath'.
       --
       -- It is supplied with the prevailing search path which will typically
       -- just be used as-is, but can be extended or ignored as needed.
       --
       -- For the purpose of change monitoring, in addition to the location
       -- where the program was found, it returns all the other places that
       -- were tried.
       --

       programFindLocation :: Verbosity -> ProgramSearchPath
                              -> IO (Maybe (FilePath, [FilePath])),

       -- | Try to find the version of the program. For many programs this is
       -- not possible or is not necessary so it's OK to return Nothing.

       programFindVersion :: Verbosity -> FilePath -> IO (Maybe Version),

       -- | A function to do any additional configuration after we have
       -- located the program (and perhaps identified its version). For example
       -- it could add args, or environment vars.

       programPostConf :: Verbosity -> ConfiguredProgram -> IO ConfiguredProgram,

       -- | A function that filters any arguments that don't impact the output
       -- from a commandline. Used to limit the volatility of dependency hashes
       -- when using new-build.

       programNormaliseArgs :: Maybe Version -> PackageDescription -> [String] -> [String]
     }
```


```haskell
-- | Represents a program which has been configured and is thus ready to be run.
--
-- These are usually made by configuring a 'Program', but if you have to
-- construct one directly then start with 'simpleConfiguredProgram' and
-- override any extra fields.
--

data ConfiguredProgram = ConfiguredProgram {

       -- | Just the name again
       --
       programId :: String,

       -- | The version of this program, if it is known.
       --
       programVersion :: Maybe Version,

       -- | Default command-line args for this program.
       -- These flags will appear first on the command line, so they can be
       -- overridden by subsequent flags.
       --
       programDefaultArgs :: [String],

       -- | Override command-line args for this program.
       -- These flags will appear last on the command line, so they override
       -- all earlier flags.
       --
       programOverrideArgs :: [String],

       -- | Override environment variables for this program.
       -- These env vars will extend\/override the prevailing environment of
       -- the current to form the environment for the new process.
       --
       programOverrideEnv :: [(String, Maybe String)],

       -- | A key-value map listing various properties of the program, useful
       -- for feature detection. Populated during the configuration step, key
       -- names depend on the specific program.
       --
       programProperties :: Map.Map String String,

       -- | Location of the program. eg. @\/usr\/bin\/ghc-6.4@
       --
       programLocation :: ProgramLocation,

       -- | In addition to the 'programLocation' where the program was found,
       -- these are additional locations that were looked at. The combination
       -- of ths found location and these not-found locations can be used to
       -- monitor to detect when the re-configuring the program might give a
       -- different result (e.g. found in a different location).
       --
       programMonitorFiles :: [FilePath]

     }
```

```haskell
-- | Where a program was found. Also tells us whether it's specified by user or
-- not.  This includes not just the path, but the program as well.

data ProgramLocation

    = UserSpecified { locationPath :: FilePath }
      -- ^The user gave the path to this program,
      -- eg. --ghc-path=\/usr\/bin\/ghc-6.6

    | FoundOnSystem { locationPath :: FilePath }
```

```haskell
simpleProgram :: String -> Program
simpleProgram name = Program

 {
    programName         = name,
    programFindLocation = \v p -> findProgramOnSearchPath v p name,
    programFindVersion  = \_ _ -> return Nothing,
    programPostConf     = \_ p -> return p,
    programNormaliseArgs   = \_ _ -> id
  }
```


### module `Distribution.Simple.Program.Find`

```haskell
import qualified System.Directory as Directory
         ( findExecutable )

import System.FilePath as FilePath
         ( (</>), (<.>), splitSearchPath, searchPathSeparator, getSearchPath
         , takeDirectory )

```


```haskell
-- | A search path to use when locating executables. This is analogous
-- to the unix @$PATH@ or win32 @%PATH%@ but with the ability to use
-- the system default method for finding executables ('findExecutable' which
-- on unix is simply looking on the @$PATH@ but on win32 is a bit more
-- complicated).
--
-- The default to use is @[ProgSearchPathDefault]@ but you can add extra dirs
-- either before, after or instead of the default, e.g. here we add an extra
-- dir to search after the usual ones.
--
-- > ['ProgramSearchPathDefault', 'ProgramSearchPathDir' dir]
--

type ProgramSearchPath = [ProgramSearchPathEntry]

data ProgramSearchPathEntry =
         ProgramSearchPathDir FilePath  -- ^ A specific dir
       | ProgramSearchPathDefault       -- ^ The system default

defaultProgramSearchPath :: ProgramSearchPath
defaultProgramSearchPath = [ProgramSearchPathDefault]
```


```haskell
findProgramOnSearchPath :: Verbosity -> ProgramSearchPath
                        -> FilePath -> IO (Maybe (FilePath, [FilePath]))

findProgramOnSearchPath verbosity searchpath prog = do

    debug verbosity $ "Searching for " ++ prog ++ " in path."
    res <- tryPathElems [] searchpath
    case res of
      Nothing   -> debug verbosity ("Cannot find " ++ prog ++ " on the path")
      Just (path, _) -> debug verbosity ("Found " ++ prog ++ " at "++ path)
    return res

  where

    tryPathElems :: [[FilePath]] -> [ProgramSearchPathEntry]
                 -> IO (Maybe (FilePath, [FilePath]))
    tryPathElems _     []       = return Nothing
    tryPathElems tried (pe:pes) = do
      res <- tryPathElem pe
      case res of
        (Nothing,      notfoundat) -> tryPathElems (notfoundat : tried) pes
        (Just foundat, notfoundat) -> return (Just (foundat, alltried))
          where
            alltried = concat (reverse (notfoundat : tried))

    tryPathElem :: ProgramSearchPathEntry -> NoCallStackIO (Maybe FilePath, [FilePath])
    tryPathElem (ProgramSearchPathDir dir) =
        findFirstExe [ dir </> prog <.> ext | ext <- exeExtensions ]

    -- On windows, getSystemSearchPath is not guaranteed 100% correct so we
    -- use findExecutable and then approximate the not-found-at locations.
    tryPathElem ProgramSearchPathDefault | buildOS == Windows = do
      mExe    <- firstJustM [ findExecutable (prog <.> ext) | ext <- exeExtensions ]
      syspath <- getSystemSearchPath
      case mExe of
        Nothing ->
          let notfoundat = [ dir </> prog | dir <- syspath ] in
          return (Nothing, notfoundat)

        Just foundat -> do
          let founddir   = takeDirectory foundat
              notfoundat = [ dir </> prog
                           | dir <- takeWhile (/= founddir) syspath ]
          return (Just foundat, notfoundat)

    -- On other OSs we can just do the simple thing
    tryPathElem ProgramSearchPathDefault = do
      dirs <- getSystemSearchPath
      findFirstExe [ dir </> prog <.> ext | dir <- dirs, ext <- exeExtensions ]

    findFirstExe :: [FilePath] -> NoCallStackIO (Maybe FilePath, [FilePath])
    findFirstExe = go []
      where
        go fs' []     = return (Nothing, reverse fs')
        go fs' (f:fs) = do
          isExe <- doesExecutableExist f
          if isExe
            then return (Just f, reverse fs')
            else go (f:fs') fs

    -- Helper for evaluating actions until the first one returns 'Just'
    firstJustM :: Monad m => [m (Maybe a)] -> m (Maybe a)
    firstJustM [] = return Nothing
    firstJustM (ma:mas) = do
      a <- ma
      case a of
        Just _  -> return a
        Nothing -> firstJustM mas
```


```haskell

```




## `cabal-install`

### module `Distribution.Client.VCS`

```haskell
-- | VCS driver for Git.
--
vcsGit :: VCS Program
vcsGit =
    VCS {
      vcsRepoType = Git,
      vcsProgram  = gitProgram,
      vcsCloneRepo,
      vcsSyncRepos
    }
  where
    vcsCloneRepo :: Verbosity
                 -> ConfiguredProgram
                 -> SourceRepo
                 -> FilePath
                 -> FilePath
                 -> [ProgramInvocation]
    vcsCloneRepo verbosity prog repo srcuri destdir =
        [ programInvocation prog cloneArgs ]
        -- And if there's a tag, we have to do that in a second step:
     ++ [ (programInvocation prog (checkoutArgs tag)) {
            progInvokeCwd = Just destdir
          }
        | tag <- maybeToList (repoTag repo) ]
      where
        cloneArgs  = ["clone", srcuri, destdir]
                     ++ branchArgs ++ verboseArg
        branchArgs = case repoBranch repo of
          Just b  -> ["--branch", b]
          Nothing -> []
        checkoutArgs tag = "checkout" : verboseArg ++ [tag, "--"]
        verboseArg = [ "--quiet" | verbosity < Verbosity.normal ]

    vcsSyncRepos :: Verbosity
                 -> ConfiguredProgram
                 -> [(SourceRepo, FilePath)]
                 -> IO [MonitorFilePath]
    vcsSyncRepos _ _ [] = return []
    vcsSyncRepos verbosity gitProg
                 ((primaryRepo, primaryLocalDir) : secondaryRepos) = do

      vcsSyncRepo verbosity gitProg primaryRepo primaryLocalDir Nothing
      sequence_
        [ vcsSyncRepo verbosity gitProg repo localDir (Just primaryLocalDir)
        | (repo, localDir) <- secondaryRepos ]
      return [ monitorDirectoryExistence dir 
             | dir <- (primaryLocalDir : map snd secondaryRepos) ]

    vcsSyncRepo verbosity gitProg SourceRepo{..} localDir peer = do
        exists <- doesDirectoryExist localDir
        if exists
          then git localDir                 ["fetch"]
          else git (takeDirectory localDir) cloneArgs
        git localDir checkoutArgs
      where
        git :: FilePath -> [String] -> IO ()
        git cwd args = runProgramInvocation verbosity $
                         (programInvocation gitProg args) {
                           progInvokeCwd = Just cwd
                         }

        cloneArgs      = ["clone", "--no-checkout", loc, localDir]
                      ++ case peer of
                           Nothing           -> []
                           Just peerLocalDir -> ["--reference", peerLocalDir]
                      ++ verboseArg
                         where Just loc = repoLocation
        checkoutArgs   = "checkout" : verboseArg ++ ["--detach", "--force"
                         , checkoutTarget, "--" ]
        checkoutTarget = fromMaybe "HEAD" (repoBranch `mplus` repoTag)
        verboseArg     = [ "--quiet" | verbosity < Verbosity.normal ]
```

```haskell
gitProgram :: Program
gitProgram = (simpleProgram "git") {
    programFindVersion = findProgramVersion "--version" $ \str ->
      case words str of
        -- "git version 2.5.5"
        (_:_:ver:_) | all isTypical ver -> ver

        -- or annoyingly "git version 2.17.1.windows.2" yes, really
        (_:_:ver:_) -> intercalate "."
                     . takeWhile (all isNum)
                     . split
                     $ ver
        _ -> ""
  }
  where
    isNum     c = c >= '0' && c <= '9'
    isTypical c = isNum c || c == '.'
    split    cs = case break (=='.') cs of
                    (chunk,[])     -> chunk : []
                    (chunk,_:rest) -> chunk : split rest
```




## `parsers`

### module `Text.Parser.Combinators`

```haskell

-- | @count n p@ parses @n@ occurrences of @p@. If @n@ is smaller or
-- equal to zero, the parser equals to @return []@. Returns a list of
-- @n@ values returned by @p@.

count :: Applicative m => Int -> m a -> m [a]

count n p | n <= 0    = pure []
          | otherwise = sequenceA (replicate n p)
```








## `tar` package

### Example

```haskell
import qualified "tar" Codec.Archive.Tar as Tar


```

### Modules

`Codec.Archive.Tar`:

> Reading, writing and manipulating ".tar" archive files.

### Functions

#### `extract`

    extract
        :: FilePath	-- Destination directory
        -> FilePath	-- Tarball
        -> IO ()	 


>Extract all the files contained in a ".tar" file.
>
>It is equivalent to calling the standard tar program like so:

    $ tar -x -f tarball.tar -C dir


>So for example if the tarball.tar file contains foo/bar.txt then this will extract it to dir/foo/bar.txt.
>
>This is a high level "all in one" operation. Since you may need variations on this function it is instructive to see how it is written. It is just:

    Tar.unpack dir . Tar.read =<< BS.readFile tar

#### `unpack`

    unpack
        :: Exception e
        => FilePath     -- ^ Destination directory
        -> Entries e
        -> IO ()

> Create local files and directories based on the entries of a tar archive.
> 
> This is a portable implementation of unpacking suitable for portable archives. It handles NormalFile and Directory entries and has simulated support for SymbolicLink and HardLink entries. Links are implemented by copying the target file. This therefore works on Windows as well as Unix. All other entry types are ignored, that is they are not unpacked and no exception is raised.
> 
> If the Entries ends in an error then it is raised an an exception. Any files or directories that have been unpacked before the error was encountered will not be deleted. For this reason you may want to unpack into an empty directory so that you can easily clean up if unpacking fails part-way.
> 
> On its own, this function only checks for security (using checkSecurity). You can do other checks by applying checking functions to the Entries that you pass to this function. For example:

    unpack dir (checkTarbomb expectedDir entries)
    
> If you care about the priority of the reported errors then you may want to use checkSecurity before checkTarbomb or other checks.

#### `read`

    read :: ByteString -> Entries FormatError

> Convert a data stream in the tar file format into an internal data structure. Decoding errors are reported by the Fail constructor of the Entries type

#### `check*`

checks:

    unpack dir (checkTarbomb expectedDir entries)

#### `checkTarbomb`

    checkTarbomb :: FilePath -> Entries e -> Entries (Either e TarBombError)

> This function checks a sequence of tar entries for being a "tar bomb". This means that the tar file does not follow the standard convention that all entries are within a single subdirectory, e.g. a file "foo.tar" would usually have all entries within the "foo/" subdirectory.
> 
> Given the expected subdirectory, this function checks all entries are within that subdirectroy.
> 
> Note: This check must be used in conjunction with checkSecurity (or checkPortability).

### Notes

#### Compression

Compressed tar archives, via `zlib` package:

>Tar files are commonly used in conjunction with gzip compression, as in ".tar.gz" or ".tar.bz2" files. This module does not directly handle compressed tar files however they can be handled easily by composing functions from this module and the modules Codec.Compression.GZip or Codec.Compression.BZip (see zlib or bzlib packages).
>
>Creating a compressed ".tar.gz" file is just a minor variation on the create function, but where throw compression into the pipeline:

    BS.writeFile tar . GZip.compress . Tar.write =<< Tar.pack base dir


>Similarly, extracting a compressed ".tar.gz" is just a minor variation on the extract function where we use decompression in the pipeline:

    Tar.unpack dir . Tar.read . GZip.decompress =<< BS.readFile tar








## `temporary`

```ghci
λ> getCanonicalTemporaryDirectory

   "/tmp"
   it :: FilePath

λ> createTempDirectory "/tmp" "xyz"

   "/tmp/xyz-52e768a90eb24cb6"
   it :: FilePath

λ> createTempDirectory "/tmp" "xyz"

   "/tmp/xyz-10a16da732fbd79d"
   it :: FilePath
```

```haskell
-- | Create a temporary directory.

createTempDirectory

  :: FilePath    -- ^ Parent directory to create the directory in

  -> String      -- ^ Directory name template

  -> IO FilePath

createTempDirectory dir template = findTempName

  where

    findTempName = do
      x :: Word <- randomIO
      let dirpath = dir </> template ++ printf "-%.*x" (wordSize `div` 4) x
      r <- MC.try $ mkPrivateDir dirpath
      case r of
        Right _ -> return dirpath
        Left  e | isAlreadyExistsError e -> findTempName
                | otherwise              -> ioError e
```

```haskell
-- | Create and use a temporary directory inside the given directory.
--
-- The directory is deleted after use.

withTempDirectory :: (MC.MonadMask m, MonadIO m) =>
                     FilePath -- ^ Parent directory to create the directory in
                  -> String   -- ^ Directory name template
                  -> (FilePath -> m a) -- ^ Callback that can use the directory
                  -> m a

withTempDirectory targetDir template =
  MC.bracket
    (liftIO (createTempDirectory targetDir template))
    (liftIO . ignoringIOErrors . removeDirectoryRecursive)
```




## ``

### module ``

```haskell
```

## ``

### module ``

```haskell
```

## ``

### module ``

```haskell
```

## ``

### module ``

```haskell
```

## ``

### module ``

```haskell
```

## ``

### module ``

```haskell
```

## ``

### module ``

```haskell
```

## ``

### module ``

```haskell
```

