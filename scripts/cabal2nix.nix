#!/bin/bash
set -e
set -u
##################################################

echo ========================================

mkdir -p ./nix/cabal2nix

echo "# -*- buffer-read-only: t; -*-" > ./nix/cabal2nix/skeletor.nix

echo ========================================
echo

(cd nix/cabal2nix   &&   cabal2nix  --flag static  --no-check  file://../../skeletor   >>   ./skeletor.nix)

# ^ Nix PathLiterals are relative to their source file (not the directory of an invoked command).

echo
echo ========================================
echo

cat ./nix/cabal2nix/skeletor.nix

echo
echo ========================================

##################################################

#cabal2nix  --flag static  --no-check  file://./skeletor   >>   ./nix/cabal2nix/skeletor.nix

