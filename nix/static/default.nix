##################################################
{ overlays ? []
, config   ? {}

, nixpkgs  ? <nixpkgs>

, pkgs     ? (import nixpkgs { inherit overlays config; }).pkgsMusl
           # ^ « musl » as C Library, not « glibc ».

, compiler ? "ghc844"
           # ^ GHC 8.4.4

, strip    ? true
}:

##################################################
let

inherit (pkgs) lib;

#------------------------------------------------#

haskellPackages =

    if   null == compiler
    then pkgs.haskellPackages
    else pkgs.haskell.packages.${compiler};

#------------------------------------------------#

haskellUtilities =

    pkgs.haskell.lib;

#------------------------------------------------#

# haskellPackages = with pkgs.haskell.lib;
#  pkgs.haskell.packages.${compiler}.override {
#     overrides = self: super: {
#       # Dependencies we need to patch
#       hpc-coveralls = appendPatch super.hpc-coveralls (builtins.fetchurl https://github.com/guillaume-nargeot/hpc-coveralls/pull/73/commits/344217f513b7adfb9037f73026f5d928be98d07f.patch);
#     };
# };

in
##################################################
let

the-package    = import ./skeletor.nix {
  inherit pkgs;
  inherit strip;
};

the-derivation = haskellPackages.callPackage the-package {
};

in
##################################################

haskellUtilities.shellAware the-derivation

##################################################