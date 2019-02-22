##################################################
{ systemPackages
, haskellPackages
, haskellUtilities

, static
, strip
}:

##################################################
let

skeletor = import ./skeletor.nix {

  inherit systemPackages haskellPackages haskellUtilities;
  inherit static strip;

};

#------------------------------------------------#

in
##################################################
{

  inherit skeletor;

}
##################################################