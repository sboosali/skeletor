{ pkg, exe ? null }:

# exit code is « 0 » if-and-only-if « pkg »'s « exe » is static executable.

##################################################
let

programPath =

 if   exe != null
 then ''${pkg}/bin/${exe}''
 else ''${pkg}'';

in
##################################################
''
#!/bin/bash

ldd ${programPath} | grep -F "not a dynamic executable"
''