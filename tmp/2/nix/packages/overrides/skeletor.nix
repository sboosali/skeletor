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

inherit (systemPackages) lib;

#------------------------------------------------#

staticLibraries = {

  glibc = systemPackages.glibc.static;

  zlib  = systemPackages.zlib.static;

  gmp   = systemPackages.gmp6.override { withStatic = true; };

};

#------------------------------------------------#

newAttributes =

    (lib.optionalAttrs (static != null)
        {
            enableStaticLibraries = static;
            
            enableSharedExecutables = (! static);
            enableSharedLibraries   = (! static);
        }
    );

#------------------------------------------------#

configureFlags = (with staticLibraries; [

    "--extra-lib-dirs=${glibc}/lib"
    "--extra-lib-dirs=${zlib}/lib"
    "--extra-lib-dirs=${gmp}/lib"

  ]) ++ (lib.optionals (!strip) [

    "--disable-executable-stripping"

  ]);

#------------------------------------------------#
in
##################################################
oldAttributes:

  (newAttributes // {

        configureFlags = oldAttributes.configureFlags ++ configureFlags;

        isLibrary    = false;
        isExecutable = true;
  })

##################################################