# Notes

(N.B. this is an incoherent reference document for the library author).

## Templates

e.g.


```haskell
>>> :set -XOverloadedStrings -XOverloadedLists
>>> sedTemplate (TemplateBinding [ ("", ""), ("", "") ]) (SrcDst { input  = "", output = "" })
```

## Usage

e.g.


```haskell
sedTemplate

  (TemplateBinding [ ("", "")
                   , ("", "")
                   ])

  SrcDst { input  = ""
         , output = ""
         }
```

## The Filesystem




## APIs

```haskell
{-

--

instance (NFData k, NFData v) => NFData (HashMap k v)

--

readFile :: FilePath -> IO ByteString

writeFile :: FilePath -> ByteString -> IO ()

--

readFile :: FilePath -> IO Text

The readFile function reads a file and returns the contents of the file as a string. The entire file is read strictly, as with getContents.

writeFile :: FilePath -> Text -> IO ()

Write a string to a file. The file is truncated to zero length before writing begins.

--

  class Hashable a where
    hashWithSalt :: Int -> a -> Int 
    infixl 0

The class of types that can be converted to a hash value.

Minimal implementation: hashWithSalt.

Return a hash value for the argument, using the given salt.

--

18,446,744,073,709,551,615 ≡ 2^64 − 1

--

lookupEnv :: String -> IO (Maybe String)

  Return the value of the environment variable var, or Nothing if there is no such value.

--

splitOn

  :: Text	
     -- ^ String to split on. If this string is empty, an error will occur.

  -> Text	
     -- ^ Input text.

  -> [Text]	 
     -- ^ O(m+n) Break a Text into pieces separated by the first Text argument (which cannot be empty), consuming the delimiter. An empty delimiter is invalid, and will cause an error to be raised.

Examples:

    >>> splitOn "\r\n" "a\r\nb\r\nd\r\ne"
    ["a","b","d","e"]

    >>> splitOn "aaa"  "aaaXaaaXaaaXaaa"
    ["","X","X","X",""]

    >>> splitOn "x"    "x"
    ["",""]

Laws

    intercalate s . splitOn s         == id
    splitOn (singleton c)             == split (==c)

--

Cabal-2.4.1.0:Distribution.System.OS

  These are the known OS names: 
    Linux, Windows, OSX ,FreeBSD, OpenBSD, NetBSD, DragonFly ,Solaris, AIX, HPUX, IRIX ,HaLVM ,Hurd ,IOS, Android,Ghcjs

--

setEnv :: String -> String -> IO ()

  setEnv name value sets the specified environment variable to value.

  Throws IOException if name is the empty string or contains an equals sign.

--

getEnvironment :: IO [(String, String)]

  getEnvironment retrieves the entire environment as a list of (key,value) pairs.
  
  If an environment entry does not contain an '=' character, the key is the whole entry and the value is the empty string.

--

data FileType = BlockDevice
              | CharacterDevice
              | NamedPipe
              | RegularFile
              | Directory
              | SymbolicLink
              | Socket
              | Unknown

--


--

--

--

--

--

--

--

--

--

--

--

--

--

--

--

--

--

--

--

--

--

--

-}
```


## POSIX Exit Codes

> On POSIX systems the standard exit code is 0 for success and any number from 1 to 255 for anything else.

Reserved Exit Codes:

* `1`       — Catchall for general errors
* `2`       — Misuse of shell builtins (according to Bash documentation)
* `126`     — Command invoked cannot execute
* `127`     — “command not found”
* `128`     — Invalid argument to exit
* `128`+`n` — Fatal error signal “n”
* `130`     — Script terminated by Control-C




## Bash Completion

e.g.:

```sh
$ /home/sboo/haskell/skeletor/dist-newstyle/build/x86_64-linux/ghc-8.6.3/skeletor-0.0.0/x/skeletor-haskell/build/skeletor-haskell/skeletor-haskell --license BSD-<TAB>

BSD-1-Clause                          BSD-2-Clause-Patent                   BSD-3-Clause-LBNL                     BSD-4-Clause                          
BSD-2-Clause                          BSD-3-Clause                          BSD-3-Clause-No-Nuclear-License       BSD-4-Clause-UC                       
BSD-2-Clause-FreeBSD                  BSD-3-Clause-Attribution              BSD-3-Clause-No-Nuclear-License-2014  BSD-Protection                        
BSD-2-Clause-NetBSD                   BSD-3-Clause-Clear                    BSD-3-Clause-No-Nuclear-Warranty      BSD-Source-Code                       
```




























## Statically-Linked Haskell Executables

### Links

* `nh2 / static-haskell-nix`: <https://github.com/nh2/static-haskell-nix/blob/master/README.md>

### Examples

#### 

```haskell
executable _
  ...
  ld-options: -static
```

#### 

`example-server.cabal`:

```haskell
name: example-server
build-type: Simple

...

executable example-server
  build-depends: base >=4.9 && <5, scotty
  ...
  ld-options: -static

```

`example-server.nix`:


```haskell
{ pkgs }:

let
static = {

  gmp = pkgs.gmp6.override { withStatic = true; };

  zlib = pkgs.zlib.static;

};

tools = {

  ldd = pkgs.ldd;

  grep = pkgs.grep;

};
in

{ mkDerivation, base, scotty, stdenv }:

      mkDerivation {

        pname = "example-server";


        src = pkgs.lib.sourceByRegex ./. [
          ".*\.cabal$"
          "^Setup.hs$"
          "^Main.hs$"
        ];

        isLibrary = false;
        isExecutable = true;


        enableSharedExecutables = false;
        enableSharedLibraries = false;


        executableHaskellDepends = [ base scotty ];

        configureFlags = [
          "--ghc-option=-optl=-static"
          "--extra-lib-dirs=${static.gmp}/lib"
          "--extra-lib-dirs=${static.zlib}/lib"
        ] ++ pkgs.lib.optionals (!strip) [
          "--disable-executable-stripping"
        ] ;

        ...


        test = ''
        "${tools.ldd}/ldd" $out | "${tools.grep}/grep" "not a dynamic executable"
        '';

      };
```

`default.nix`:

```haskell
{ nixpkgs ? (import <nixpkgs> {}).pkgsMusl, compiler ? "ghc843", strip ? true }:

let

  pkgs = nixpkgs.pkgsMusl;

  example-server = import ./example-server.nix { inherit pkgs; };

  normalHaskellPackages = pkgs.haskell.packages.${compiler};

  haskellPackages = with pkgs.haskell.lib; normalHaskellPackages.override {
    overrides = self: super: {

      # Dependencies we need to patch
      hpc-coveralls = appendPatch super.hpc-coveralls (builtins.fetchurl https://github.com/guillaume-nargeot/hpc-coveralls/pull/73/commits/344217f513b7adfb9037f73026f5d928be98d07f.patch);

    };
  };

  drv = haskellPackages.callPackage example-server {};

in
  if pkgs.lib.inNixShell then drv.env else drv
```

`Main.hs`:

```haskell
--

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}

-- 

import qualified "scotty" Web.Scotty as W

--

import "base" Data.Monoid (mconcat)


-- 

main = W.scotty 3000 $ do

    W.get "/:word" $ do

        beam <- W.param "word"
        W.html $ mconcat ["<h1>Scotty, ", beam, " me up!</h1>"]


--
```

### 






















## 















## 










## 


