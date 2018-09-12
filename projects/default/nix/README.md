# `nix`

`sboosali`'s generic project-specific `nix`-configuration.

## TODO 

  * [ ] Haskell-specific projects.

## files

* `./project.nix`: An `attrset` of project information.
* `./packages.nix`: A `list` of system-packages. 
* `./environment.nix`: A `derivation` via `buildEnv`.
* `./overlays/`: A directory of nix-overlays.
* `./nixpkgs/`: A pinned `nixpkgs` repository.
* `./nix/shell.nix`: Called by `./shell.nix`.


## customization

* `./project.nix`: Name your project (`project.name`).
* `./packages.nix`: Add a system-package or two.
* `./nixpkgs/nixpkgs.json`: Pin your release (via `nix-prefetch-git`).
* `./overlays/*.nix`: Write an overlay file.

## `./packages.nix`

Either install, with:

```bash
$ nix-env -i -f ./nix/nixpkgs/ 
```

Or provision (an environment), with:

```bash
$ nix-shell
```

By default, when dependencies are installed, these paths are linked into your `~/.nix-profile/*`:

* `/bin`: for executables.
* `/lib`: for shared-objects / dynamically-linked-libraries (`.so` on Linux, `.dylib` on Apple).
* `/include`: for header files (`.h`).
* `/share`: for documentation, shell-completion, etc.


## `./overlays/*.nix`

Each overlay has this signature:

```nix
self: super:
{

 ...

}
```

* `super`: holds the `attrset` returned by the previous overlay or by the original `pkgs`.
* `self`: the final `attrset` (i.e. "package set") after all overlays have been applied,
including those after the current overlay (i.e. the one currenty being applied).

i.e. `./overlays/default.nix` is like `foldOverlays` in the following pseudo-Haskell:

```haskell

import qualified "containers" Data.Map.Lazy as Map

type P = Map.Map String Derivation

foldOverlays :: P -> [P -> P -> P] -> P
foldOverlays p overlays 

  = self

  where
  
  self :: P
  self = foldr applyOverlay overlays p

  applyOverlay :: (P -> P -> P) -> P -> P
  applyOverlay overlay super 
    
    = super `rightBiasedUnion` (overlay self super)
  
  rightBiasedUnion = Map.unionWith (l: r: r)

```

(this is a simplification, and possibly misleading, but that's is how I think about it).

`./overlays/overlay.nix` is a trivial overlay, to provide an example and a jumping-off point.


## `./nixpkgs/*`

The project-specific `nixpkgs`.

Pinned to the version in `nixpkgs.json`, which is generated from `nix-prefetch-git`.

Has optional `config`uration and `overlays`. The relevant directories are:

- `./nix/nixpkgs/` 
- `./nix/config/`
- `./nix/overlays/`

# Dependencies

Depends on no other `./nix` sub-directories (than the three listed above);
and, afaik, on no implicit settings (like the user-specific `~/.nixpkgs/config.nix` or system-specific `/nix/configuration.nix`).

### Updating

You can re-pin against the latest `master` of the official `nixpkgs` repository, i.e.

    https://github.com/NixOS/nixpkgs

by running this (very short) script:

    ./nix/nixpkgs/update-nixpkgs.sh

from the `${project.root}`.


## `./shell.nix`

`../shell.nix` just calls `./shell.nix`

the `nix`-version of a `main()` function, a "`main()`-like" `nix`-expression. by which I mean: it being top-level and requiring no inputs.

 `./shell.nix` uses `./nixpkgs/default.nix` 

