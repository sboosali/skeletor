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

MESSAGE="Usage: « $0 ORIGINAL RENAMED FILEPATH [ --dry-run ] »".

ORIGINAL=${1:?"$MESSAGE"}
RENAMED=${2:?"$MESSAGE"}
FILEPATH=${3:?"$MESSAGE"}
OPTIONS=${4:-""}

##################################################

OriginalFilepath="$FILEPATH"

RenamedFilepath=${FILEPATH//"$ORIGINAL"/"$RENAMED"}

##################################################

echo "mkdir -p « $RenamedFilepath »"

mkdir -p "$RenamedFilepath" 

if   [[ ! "$OriginalFilepath" = "$RenamedFilepath" ]];
     # ^ ensure: the filepaths are different.

then
    echo "« $OriginalFilepath »  →  « $RenamedFilepath »"
    
    if   [[ ! "$OPTIONS" =~ "--dry-run" ]];
         # ^ ensure: the dry-run option is not a substring of the options given (case sensitive).
    then
        mv "$OriginalFilepath" "$RenamedFilepath" 
    fi 

else
    echo "« $OriginalFilepath »  ≡  « $RenamedFilepath »"
fi
##################################################