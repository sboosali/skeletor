##################################################
{ pkgs
}:

##################################################
let

inherit (pkgs) lib;

in
##################################################
let

static = {

  gmp = pkgs.gmp6.override { withStatic = true; };

  zlib = pkgs.zlib.static;

};

tools = {

  # ldd = pkgs.ldd;

  # grep = pkgs.grep;

};

configureFlags = [

          "--ghc-option=-optl=-static"
          "--extra-lib-dirs=${static.gmp}/lib"
          "--extra-lib-dirs=${static.zlib}/lib"

        ] ++ pkgs.lib.optionals (!strip) [

          "--disable-executable-stripping"

        ] ;

in
##################################################

{ mkDerivation, stdenv
, base, scotty
}:

##################################################
mkDerivation {

        pname   = "skeletor-haskell";
        version = "0.0.0"

        src = lib.sourceByRegex ../../skeletor [
          ".*\.cabal$"
          "^Setup.hs$"
          "^Main.hs$"
        ];

        isLibrary    = false;
        isExecutable = true;

        enableSharedExecutables = false;
        enableSharedLibraries   = false;

        executableHaskellDepends = [ base ];

        inherit configureFlags;

        # test = ''
        # "${tools.ldd}/ldd" $out | "${tools.grep}/grep" "not a dynamic executable"
        # '';

};
##################################################