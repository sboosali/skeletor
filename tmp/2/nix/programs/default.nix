##################################################
{ haskellUtilities

, projectPackages

, static
, strip
}:

##################################################
let
#------------------------------------------------#

programs = {

  inherit skeletor-haskell;
  # inherit skeletor-elisp;
  # inherit skeletor-nix;
  # inherit skeletor-python;
  # inherit skeletor-javascript;

};

#------------------------------------------------#

skeletor-haskell =

  haskellUtilities.justStaticExecutables (projectPackages.skeletor);

#------------------------------------------------#
in
##################################################

programs

##################################################