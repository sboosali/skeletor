##################################################
{ nixpkgs ? (import ./nixpkgs)
}:

##################################################
let

project     = import ./project.nix
 { inherit nixpkgs;
 };

packages    = import ./packages.nix
 { inherit nixpkgs;
 };

environment = import ./environment.nix
 { inherit nixpkgs;
   inherit project packages;
 };

in
##################################################

environment

##################################################