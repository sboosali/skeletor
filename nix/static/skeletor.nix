##################################################
{ pkgs
, strip
}:

##################################################
let

inherit (pkgs) lib;

#------------------------------------------------#

skeletor-cabal2nix = import ../cabal2nix/skeletor.nix;

in
##################################################
let

static = {

  gmp = pkgs.gmp6.override { withStatic = true; };

  zlib = pkgs.zlib.static;

};

#------------------------------------------------#

configureFlags-forStaticLinking = [

          "--ghc-option=-optl=-static"

          "--extra-lib-dirs=${static.gmp}/lib"
          "--extra-lib-dirs=${static.zlib}/lib"

        ] ++ pkgs.lib.optionals (!strip) [

          "--disable-executable-stripping"

        ];

in
##################################################
let

override-forStaticLinking = attrs:

  {
    configureFlags = attrs.configureFlags ++ configureFlags-forStaticLinking;

    isLibrary    = true;
    isExecutable = true;

    enableSharedExecutables = false;
    enableSharedLibraries   = false;

  #      isLibrary    = true;
  #      configureFlags = configureFlags;
  };

#------------------------------------------------#

skeletor-static = skeletor-cabal2nix.overrideAttrs override-forStaticLinking;

#------------------------------------------------#

in
##################################################

skeletor-static

##################################################