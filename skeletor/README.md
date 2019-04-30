# `skeletor`


## Summary

`skeletor` is a program for project-scaffolding. 

`skeletor` templates can have valid syntax for any format, and thus `skeletor` projects can be built and tested (like normal projects).


## Usage

### Usage: Writing a `skeletor` project(-scaffold)

Data files, e.g. `./example/xxx-package-xxx.cabal`:

```cabal
package: xxx-package-xxx
...
library
  exposed-modules: 
    Xxx_Module_xxX
    Xxx_Module_xxX.Types
...
```

(where `example` will be the name of our project-scaffold).

Metadata file, e.g. `./example.json`:

```json
{ "variables": 

  [{ "variable": "xxx-package-xxx",
     "option":   "package",
     "type":     "string",
     "filenames": true,
     "files":     [".cabal", ".hs", ...]
   },

   { "variable": "Xxx_Module_xxX",
     "option":   "module",
     ...
   },

   ...
  ]
}
```

or, with the defaults for haskell project scaffolds:

```json
{}
```

Development (of the files above), e.g.:

```sh
$ cd ~/.config/skeletor/projects/example/

$ cabal new-build xxx-package-xxx
...
```

will build like any other package. Why? Because there are no (unsyntactic) `{package}`s or `$package`s.

c.f. a Mustache template:

```cabal
package: {package}
...
library
  exposed-modules: 
    {module}
    {module}.Types
...
```

which won't build (it's not a valid `.cabal` file).

### Usage: Invoking a `skeletor` project

Usage (given the files above) of creating a new project (given a scaffold, and some template-variable bindings):

```sh
$ cd ~/src

$ skeletor new --project=example --binding package=american-staffordshire-terrier --binding module=AmericanStaffordshireTerrier
...

$ pwd
~/src/american-staffordshire-terrier/

$ ls
american-staffordshire-terrier.cabal
...

$ cat american-staffordshire-terrier.cabal
package: american-staffordshire-terrier
...
library
  exposed-modules: 
    AmericanStaffordshireTerrier
    AmericanStaffordshireTerrier.Types
...
```

Or more concisely (with short-form options, and defaulting the subcommand):

```sh
$ skeletor -p example -b package=american-staffordshire-terrier -b module=AmericanStaffordshireTerrier
```

NOTE that tab-completion of arguments should work (as well as of commands/options):

```sh
$ skeletor <TAB>
...                                 # lists subcommands

$ skeletor new <TAB>                # lists options (of the subcommand)
...

$ skeletor new -p <TAB>             # lists known projects
...

$ skeletor new -p example -b <TAB>  # lists all template-variables (of the given project [TODO])
...

$ skeletor new -p example -b package=
```

Or more flexibly (using a remote template, downloading it if necessary):

```sh
$ skeletor new -t git -I  -p example -b package=american-staffordshire-terrier -b module=AmericanStaffordshireTerrier
```

[TODO] where `-I` abbreviates `--include`, and `-t` abbreviates `--type`.


## Description

Project-scaffolding with *lexically-valid* project-skeletons (via lexically-arbitrary template-variables).

## Motivation

the project skeletons can themselves be **buildable**. 

Your scaffold can a *valid* cabal project, 

## Implementation

How? All standard templating-engines have some fixed lexical-syntax for interpolation (e.g. `{var}`, `$var`, etc). `skeletor` templates have arbitrary lexical-syntax (but well-defined in metadata, or optionally inferred from the variable-name itself) for interpolation. 

Specifically, here are some contexts, with their syntax:

* Haskell packages — a string of:

    * upper-or-lower alphanumeric characters (as a regex, `[0-9a-zA-z]`), or
    * some few symbolic character (as a regex, `[_-]`) (in particular, no curly-braces or dollar-signs).

* Haskell modules — a string:

    * that must start with an upper-character, and
    * may have upper-or-lower alphanumeric characters (as a regex, `[0-9a-zA-z]`), or
    * may have one symbolic character (as a regex, `[_]`) (in particular, no curly-braces or dollar-signs).

* Filepaths (POSIX) — a string of:

    * any character except `/`.

* (and so on)

And here are some details that `skeletor` handles:

* [TODO] For example, to reference the package name in a module name (where he former is a superset of the latter, but with different conventions), we may need to convert the name by replacing `-`s with `_`s (as `Cabal` does when auto-generating `Paths_` modules).


## Features

**WORK IN PROGRESS**

Includes several **extensive** template-skeletons.


## Usage (as a library)

```haskell
import qualified "skeletor" Skeletor

...
```

## Resources

Resources (files, environment variables, websites, port numbers, etc) which this program depends on or makes use of.

Files:

* `~/.config/skeletor/haskell/skeletor-haskell.ini`
* `~/.local/share/skeletor/haskell/projects/simple.tar.gz`
* `~/.cache/skeletor/haskell/*.{tar,gz,zip,tar.gz}`

Environment Variables

* `$https_proxy`
* `$http_proxy`
* `$all_proxy`
* `$XDG_CONFIG_HOME` — Defaults to `~/.config/` on *Linux* and to `%UserProfile%\AppData\Roaming\` on *Windows*.
* `$XDG_DATA_HOME` — Defaults to `~/.local/share/` on *Linux* and to `%UserProfile%\AppData\Roaming\` on *Windows*.
* `$XDG_CACHE_HOME` — Defaults to `~/.cache/` on *Linux* and to `%UserProfile%\AppData\Local\` on *Windows*.

Websites:

* <https://github.com/sboosali/skeletor/tree/master/release/**.tar.gz>

Ports:

* `443` — for `HTTPS` connections.
* `80` — for `HTTP` connections. 

(The lists above are not necessarily exhaustive.)

## "Specification" (Work In Progress)

a project skeleton is a FileTree. it SHOULD be a valid project.

e.g. for haskell projects, a "valid project" is one that `cabal` can build (modulo other dependencies, like system libraries or inaccessible downloads).

a FileTree is a directory containing regular-files, directories, and (maybe?) symlinks.

a project identifier is a URI (or any other short string?) that identifies and locates a project skeleton. a.k.a. the Identified "can be uniquely identified as, and/or located at," the Identifier.

the supported project identifiers are:

* a filepath — the root of the FileTree, a directory.
* an archive — a `.tar`, is un-archived (into the above).
* a tarball — a `.tar.gz` is decompressed (into the above).
* a URI — download it, follow links (detect cycles, ditto with hardlinks of filepaths).
* a git repo — clone it.
* a project name — the named project must exist on a Projects Path (where "exists" is often "contains as a subdirectory", but is contextual, and includes (maybe) a git repository of a github organization).

a Project Path is a location that contains multiple project skeletons. there are several Projects Paths (as constants and as variables). namely (directly or indirectly):

* `$SKELETOR_PATH` — an environment variable, a colon-separated list of paths (like `$PATH`).
* `$XDG_DATA_HOME/skeletor/projects` — the standard location of data files for a user's application.
* <https://github.com/sboosali/skeletor/tree/master/projects> — official repository (extremely "official" lol)


## Roadmap

- [ ] personal/maximalist (`spiros`-based) project skeleton.
- [ ] minimalist (`base`-based) project skeleton.
- [ ] git-like tab-completion for arguments.
- [ ] 
- [ ] 
- [ ] 
- [ ] 
- [ ] 
- [ ] 
- [ ] 


## 