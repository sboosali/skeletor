#!/bin/bash
set -e
set -u
##################################################

echo ========================================
echo

mkdir -p ./nix/cabal2nix

cabal2nix  --flag static  --no-check  ./skeletor   >   ./nix/cabal2nix/skeletor.nix

echo
echo ========================================
echo

cat ./nix/cabal2nix/skeletor.nix

echo
echo ========================================

##################################################
