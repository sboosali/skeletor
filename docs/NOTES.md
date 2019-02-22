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


## Installation

```
$ git clone https://github.com/nh2/static-haskell-nix && cd static-haskell-nix

$ NIX_PATH=nixpkgs=https://github.com/NixOS/nixpkgs/archive/88ae8f7d.tar.gz time nix-build --no-link ./default.nix

/nix/store/*-example-scotty-app-0.1.0.0

$ /nix/store/*-example-scotty-app-0.1.0.0/bin/example-scotty-app 

Setting phasers to stun... (port 3000) (ctrl-c to quit)

$ ldd /nix/store/*-example-scotty-app-0.1.0.0/bin/example-scotty-app 

	not a dynamic executable
```





















## Nixpkgs

`pkgs` variants (/ alternative):

* `pkgsCross`
* `pkgsMusl`
* `pkgsStatic`

### `pkgsCross`

```nix

```

### `pkgsMusl`

```nix

```

### `pkgsStatic`

```nix

```

### 






















## `make`

### Usage

```sh
$ man make

...

       -C dir, --directory=dir

            Change  to directory dir before reading the makefiles or doing anything else.
            If multiple -C options are specified, each is  interpreted  relative  to  the
            previous  one:  -C / -C etc is equivalent to -C /etc.  This is typically used
            with recursive invocations of make.

       -f file, --file=file, --makefile=FILE
            Use file as a makefile.

       -k, --keep-going
            Continue as much as possible after an error.  While the target  that  failed,
            and  those  that  depend  on  it, cannot be remade, the other dependencies of
            these targets can be processed all the same.


```
































## nixpkgs & haskell

### `haskell.lib`

#### ``

```nix

```

#### `overrideCabal`

```nix
overrideCabal = drv: f:

  let
  
  go = args:
  
    args // {

      mkDerivation = drv:

        (args.mkDerivation drv).override f;

    };

  overrideScope = scope:
  
    overrideCabal (drv.overrideScope scope) f;

  in

  (drv.override go) // { inherit overrideScope; };
```

#### `add*`

```nix
addExtraLibrary = drv: x:

  addExtraLibraries drv [x];

addExtraLibraries = drv: xs:

  let
  
  go = drv:

    {
      extraLibraries = (drv.extraLibraries or []) ++ xs;
    };

  in

  overrideCabal drv go;
```

#### `haskell.lib.generateOptparseApplicativeCompletion`

```nix
  /*
    Modify a Haskell package to add shell completion scripts for the
    given executable produced by it. These completion scripts will be
    picked up automatically if the resulting derivation is installed,
    e.g. by `nix-env -i`.
    Invocation:
      generateOptparseApplicativeCompletions command pkg
      command: name of an executable
          pkg: Haskell package that builds the executables
  */

  generateOptparseApplicativeCompletion = exeName: pkg: overrideCabal pkg (drv: {
    postInstall = (drv.postInstall or "") + ''
      bashCompDir="$out/share/bash-completion/completions"
      zshCompDir="$out/share/zsh/vendor-completions"
      fishCompDir="$out/share/fish/vendor_completions.d"
      mkdir -p "$bashCompDir" "$zshCompDir" "$fishCompDir"
      "$out/bin/${exeName}" --bash-completion-script "$out/bin/${exeName}" >"$bashCompDir/${exeName}"
      "$out/bin/${exeName}" --zsh-completion-script "$out/bin/${exeName}" >"$zshCompDir/_${exeName}"
      "$out/bin/${exeName}" --fish-completion-script "$out/bin/${exeName}" >"$fishCompDir/${exeName}.fish"
      # Sanity check
      grep -F ${exeName} <$bashCompDir/${exeName} >/dev/null || {
        echo 'Could not find ${exeName} in completion script.'
        exit 1
      }
    '';
  });
```

#### ``

```nix

```

#### ``

```nix

```

#### ``

```nix

```



























## 