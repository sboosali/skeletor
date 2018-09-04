#!/bin/bash
set -e

File="${1:-./nix/nixpkgs/nixpkgs.json}"

nix-prefetch-git https://github.com/NixOS/nixpkgs > "${File}"

cat "${File}"