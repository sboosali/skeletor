##################################################
let

#bootstrap = import <nixpkgs> {};

readPrefetchGitJSON = p:
  (builtins.fetchGit
    (builtins.intersectAttrs { url = null; rev = null; }
      (builtins.fromJSON
        (builtins.readFile p))));

in
##################################################
let

nixpkgs  = readPrefetchGitJSON ./nixpkgs.json;

config   = import ../config;
overlays = import ../overlays;

in
##################################################

import nixpkgs { inherit config overlays; }

##################################################