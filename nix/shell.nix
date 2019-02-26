##################################################
{ overlays ? []
, config   ? {}
, nixpkgs  ? <nixpkgs>
, pkgs     ? (import nixpkgs { inherit overlays config; }).pkgs
}:

##################################################
let

inherit (pkgs) lib;
inherit (pkgs) stdenv;

#------------------------------------------------#

theEnvironment = import ./static/libraries.nix { inherit pkgs; };

#------------------------------------------------#

theDerivation = stdenv.mkDerivation {

  name = "skeletor-haskell";

  buildInputs = [ theEnvironment ];

};

in
##################################################

theDerivation

##################################################