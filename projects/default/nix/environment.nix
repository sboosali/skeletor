##################################################
{ nixpkgs
, packages
, project
}:

##################################################

nixpkgs.pkgs.buildEnv
  {
      name                 = "${project.name}-project-environment";

      paths                 = packages;
      pathsToLink           = [ "/" "/bin" "/lib" "/include" ];

      buildInputs           = packages;
      extraOutputsToInstall = [ "out" "dev" "doc" ];

  }
##################################################