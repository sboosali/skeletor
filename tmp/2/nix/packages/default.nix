##################################################
{ systemPackages
, haskellPackages
, haskellUtilities

, static
, strip
}:

##################################################
let
#------------------------------------------------#

haskellPackages' = haskellPackages;

# haskellPackages' = haskellPackages.override {

#   overrides = self: super:

#     packages;

# };

#------------------------------------------------#

overrides = import ./overrides {

  inherit systemPackages haskellUtilities;

  haskellPackages = haskellPackages';

  inherit static strip;

};

#------------------------------------------------#

cabal2nix = import ./cabal2nix {

  haskellPackages = haskellPackages';

};

##################################################

packages = {

  inherit skeletor;

};

#TODO# overriden « haskellPackages' » such that (project-)local packages depend on each other.

#------------------------------------------------#

programs = {

  inherit skeletor-haskell;
  # inherit skeletor-elisp;
  # inherit skeletor-nix;
  # inherit skeletor-python;
  # inherit skeletor-javascript;

};

#------------------------------------------------#

skeletor =

  haskellUtilities.overrideCabal (cabal2nix.skeletor) (overrides.skeletor);

#------------------------------------------------#
in
##################################################

packages

##################################################