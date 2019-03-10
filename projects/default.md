# the `default` project


## Project Files

### `Makefile`

the `Makefile` has these targets (among many, Haskell-specific and Nix-specific, others):

* `make`         — builds.
* `make build`   — builds.
* `make check`   — tests.
* `make install` — installs..
* `make dist`    — creates a tarball.

these are standard targets. in nixpkgs (`stdenv.mkDerivation`), by default:

* `buildPhase`   — invokes `make`.
* `checkPhase`   — invokes `make check`.
* `installPhase` — invokes `make install`.
* `distPhase`    — invokes `make dist`.

### `extra-source-files`

the package (i.e. `xxx-package-xxx`) directory has these subdirectories:

* `share/man/`  — has the Manpages for the program (i.e. `xxx-program-xxx`).
* `share/info/` — TODO.
* `share/doc/`  — TODO.

these are directories. in nixpkgs (`stdenv.mkDerivation`), by default:

* `fixupPhase` — moves `man/` and `doc/` and `info/` to `share/`.



## TODO my configuration

```ini
########################################
[default]
Name=Spiros Boosalis
LegalName=Sam Boosalis

User=sboo
HackageUser=sboo
GithubUser=sboosali

########################################
```


## core template-variables

- `xxx-package-xxx`: the package name. NOTE this template-variable has a **rigid** name (why? to support `xxx_package_xxx`), unlike other template-variables whose name can have aliases.
- `Xxx_Module_xxX`: the primary module (i.e. all other `exposed-modules` are its submodules).
- `__Project__`: the project name (a project has one-or-more packages).


## recommended template-variables

`__Name__`: your name.
`__LegalName__`: your legal name (for copyright).
`__Year__`" defaults to the current year.
`__Copyright__`: defaults to `__Year__ __LegalName__` (e.g. `"2018 Spiros Boosalis"`)
`__Synopsis__`: the `.cabal` `synopsis` field.
`__Description__`: the `.cabal` `description` field.
`__Categories__`: the `.cabal` `category` field.

## derived template-variables

- `xxx-package-directory-xxx`: defaults to `xxx-package-xxx`.
- `xxx-program-xxx`: defaults to `xxx-package-xxx`.
- `xxx_package_xxx`: the package name as a valid module name.
- `Xxx_ModuleAbbreviation_xxX`: defaults to (the constant) `"Z"`.
- `__Project__`: defaults to `xxx-package-directory-xxx`.
- `__GithubUser__`: defaults to `__User__`.
- `__GithubRepository__`: defaults to `__Project__`.
- `__ProjectDirectory__`: defaults to `~/haskell/__Project__`.


