##################################################
{ pkgs

, compiler
, strip
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

linkStatically = import ./lib/link-statically.nix {
  inherit pkgs;
  inherit strip;
};

in
##################################################
let

the-cabal2nix-package = import ../cabal2nix/skeletor.nix;

#------------------------------------------------#

the-cabal2nix-derivation = haskellPackages.callPackage the-cabal2nix-package {

  spiros = haskellUtilities.callCabal2nix "spiros" ~/haskell/spiros/spiros;

};

#------------------------------------------------#

the-static-derivation = linkStatically the-cabal2nix-derivation;

in
##################################################

haskellUtilities.shellAware the-static-derivation

##################################################