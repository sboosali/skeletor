#!/bin/bash
set -o xtrace
##################################################
# Constants

CABAL_DEFAULT_TARGET="all" ##TODO

GHCID_DIRECTORY=./.sboo

##################################################
# Arguments

GHCID_TARGET="${1:-${CABAL_DEFAULT_TARGET}}"

# ^
# e.g.:
# - all
# - <package>
# - ""
#   ^ (i.e. blank, which defaults to a single <package> when unambiguous (WRONG it doesn't))

##################################################
# Variables

GHCID_COMMAND="cabal new-repl ${GHCID_TARGET}"

GHCID_FILE="${GHCID_TARGET}.ghcid"

GHCID_OUTPUT="${GHCID_DIRECTORY}/${GHCID_FILE}"

##################################################

# echo ----------------------------------------
# echo 
# echo "${GHCID_DIRECTORY}"

mkdir -p "${GHCID_DIRECTORY}"

echo
echo ----------------------------------------
echo 
echo "${GHCID_OUTPUT}"
echo 

ghcid --command="${GHCID_COMMAND}" --outputfile="${GHCID_OUTPUT}" ##>/dev/null

##################################################