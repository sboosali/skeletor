##################################################
{ nixpkgs ? <nixpkgs>
}:

##################################################
let

pkgs = (import <nixpkgs> {}).pkgs

in
##################################################
let

config = {

  verbose = 2;
  jobs    = 4;

  nix           = false;
  deterministic = true;
  relocatable   = true;


};

in
##################################################
''
-- Statically-Linked Executables
--------------------------------------------------


package bytestring

  ghc-options:
    -optl=-static
    -optl=-pthread

    -optl=-L${pkgs.gmp6.override { withStatic = true; }}/lib
    -optl=-L${pkgs.zlib.static}/lib
    -optl=-L${pkgs.glibc.static}/lib

--------------------------------------------------

static: True
shared: False

--------------------------------------------------

verbose: ${int config.verbose}
         -- « 1 » by default.

jobs: ${int config.jobs}
      -- « $ncpus » by default.

--------------------------------------------------

nix: ${bool config.nix}

extra-lib-dirs:     ${path ~/.nix-profile/lib}

extra-include-dirs: ${path ~/.nix-profile/include}

extra-prog-path:    ${path ~/.nix-profile/bin}
extra-prog-path:    ${path ~/.nix-profile/libexec}

--------------------------------------------------

deterministic: ${bool config.deterministic}
relocatable:   ${bool config.relocatable}

--------------------------------------------------

-- program-default-options
--   ld-options:

--------------------------------------------------
''