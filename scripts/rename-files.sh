#!/bin/bash
set -e
##################################################
# Usage
# =====

# e.g.
# 
#   $ 
#

##################################################

MESSAGE="Usage: « $0 ORIGINAL RENAMED [ DIRECTORY ] [ --dry-run ] »".

ORIGINAL=${1:?"$MESSAGE"}
RENAMED=${2:?"$MESSAGE"}

DIRECTORY=${3:?.}
OPTIONS="" # ${@:3}

##################################################

find  "$DIRECTORY"  -mindepth 2  -maxdepth 2  -type d  -exec  ./rename-directory.sh "$ORIGINAL" "$RENAMED" {} "$OPTIONS"  \;
find  "$DIRECTORY"  -mindepth 3  -maxdepth 3  -type d  -exec  ./rename-directory.sh "$ORIGINAL" "$RENAMED" {} "$OPTIONS"  \;
find  "$DIRECTORY"  -mindepth 4  -maxdepth 4  -type d  -exec  ./rename-directory.sh "$ORIGINAL" "$RENAMED" {} "$OPTIONS"  \;
find  "$DIRECTORY"  -mindepth 5  -maxdepth 5  -type d  -exec  ./rename-directory.sh "$ORIGINAL" "$RENAMED" {} "$OPTIONS"  \;
find  "$DIRECTORY"  -mindepth 6  -maxdepth 6  -type d  -exec  ./rename-directory.sh "$ORIGINAL" "$RENAMED" {} "$OPTIONS"  \;
find  "$DIRECTORY"  -mindepth 7  -maxdepth 7  -type d  -exec  ./rename-directory.sh "$ORIGINAL" "$RENAMED" {} "$OPTIONS"  \;
find  "$DIRECTORY"  -mindepth 8  -maxdepth 8  -type d  -exec  ./rename-directory.sh "$ORIGINAL" "$RENAMED" {} "$OPTIONS"  \;
find  "$DIRECTORY"  -mindepth 9  -maxdepth 9  -type d  -exec  ./rename-directory.sh "$ORIGINAL" "$RENAMED" {} "$OPTIONS"  \;

find  "$DIRECTORY"  -mindepth 2  -type f  -exec  ./rename.sh           "$ORIGINAL" "$RENAMED" {} "$OPTIONS"  \;

##################################################

#find . -type f -name '*.hs' -exec ./rename.sh "$ORIGINAL" "$RENAMED" {} "$DRYRUN" \;

# find  "$DIRECTORY"  -mindepth 2  -maxdepth 2  -type d  -exec  ./rename.sh "$ORIGINAL" "$RENAMED" {} "$OPTIONS"  \;
# find  "$DIRECTORY"  -mindepth 3  -maxdepth 3  -type d  -exec  ./rename.sh "$ORIGINAL" "$RENAMED" {} "$OPTIONS"  \;
# find  "$DIRECTORY"  -mindepth 4  -maxdepth 4  -type d  -exec  ./rename.sh "$ORIGINAL" "$RENAMED" {} "$OPTIONS"  \;
# find  "$DIRECTORY"  -mindepth 5  -maxdepth 5  -type d  -exec  ./rename.sh "$ORIGINAL" "$RENAMED" {} "$OPTIONS"  \;
# find  "$DIRECTORY"  -mindepth 6  -maxdepth 6  -type d  -exec  ./rename.sh "$ORIGINAL" "$RENAMED" {} "$OPTIONS"  \;
