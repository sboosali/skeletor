##################################################
arguments@

{ nixpkgs  ? <nixpkgs>
, overlays ? []
, config   ? {}
           # ^ (these options (above) affect only « pkgs » (below).)

, pkgs     ? (import nixpkgs { inherit overlays config; }).pkgsMusl
           # ^ « musl » as C Library, not « glibc ».

, compiler ? "ghc863"
           # ^ the haskell compiler. GHC 8.6.3 (by default).

, strip    ? true
           # ^ enable "executable stripping".

}:

##################################################
let

#------------------------------------------------#

in
##################################################
let

static = import ./static {

};

#------------------------------------------------#

in
##################################################
{

  inherit static;

}
##################################################