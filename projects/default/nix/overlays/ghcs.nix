self: super: 
########################################
let

defaultGHC = self.haskell.compiler.ghc843;

GHCs =
  [
    defaultGHC
    self.haskell.compiler.ghc7103
    self.haskell.compiler.ghc802
    self.haskell.compiler.ghc822 
    self.haskell.compiler.ghc861
    self.haskell.compiler.ghcjs
  ];

  # ^
  # first item in `paths` is the unqualified executable;
  # i.e. `ghc` is `ghc-8.4.3`.
  #
  # [problem] error "have the same priority ; use 'nix-env --set-flag priority NUMBER INSTALLED_PKGNAME' to change the priority of one of the conflicting packages"
  # even with `buildEnv.ignoreCollisions = true`.
  #
  # [solution] 
  #

environmentGHCs = super.buildEnv
 {
   name             = "GHCs";
   ignoreCollisions = true;
   paths            = GHCs;
 };

in
########################################
{

  ghcs = (super.ghcs or {})
    // environmentGHCs;

}
########################################
# Notes ################################
########################################

# ^ /u/ElvishJerricco:
# \`nix-env\` treats two packages with the same name but different version suffixes as an upgrade operation. You'll probably need to use \`buildEnv\` to get the both:

# Usage
# =====
#
#   $ cd "${ProjectDirectory}"/nix
#
#   $ nix repl
#
#   > nixpkgs = import ./nixpkgs
#
#   > :i nixpkgs.ghcs
#   installing 'GHCs.drv'
#   ...
#
#   > :q
#
#   $ compgen -c ghc | grep -E '^ghc-[0-9]'
#   ghc-8.6.0.20180810
#   ghc-8.4.3
#   ghc-8.2.2
#   ghc-8.0.2
#   ghc-7.10.3
#   ...
#
#   $ ghcjs --version
#   version 8.4.3
#

########################################