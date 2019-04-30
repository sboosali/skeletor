#------------------------------------------------#
# README ----------------------------------------#
#------------------------------------------------#

# Targets:
#
# • Standard targets — « make {build,check,dist,install} ».
# • Release targets  — « make static », « make man », « make bash-completion », « make release », « make {upload,publish}-{hackage,github} ».
#

#------------------------------------------------#

# « make man » creates a (compressed) Manpage file at:
#
# « .../share/man1/xxx-program-xxx.1.gz »
#
# which can be viewed with:
#
# « man xxx-program-xxx.1.gz »
#

#------------------------------------------------#

# « make bash-completion » creates a Bash Completion script at:
#
# « .../share/bash/completion/xxx-program-xxx.bash »
#
# which can be registered with:
#
# « source xxx-program-xxx.bash »
#

#------------------------------------------------#
# Makefile Settings -----------------------------#
#------------------------------------------------#

SHELL=bash

.EXPORT_ALL_VARIABLES:

#------------------------------------------------#
# Makefile Variables ----------------------------#
#------------------------------------------------#
# Metadata...

Version=0.0.0

Project ?=TODO
Package ?=TODO
Program ?=$(Package)

#------------------------------------------------#
# Components...

CabalTargets ?=all
CabalTarget  ?=lib:$(Package)
CabalProgram ?=exe:$(Program)

#------------------------------------------------#

CabalProject ?=./cabal.project

#------------------------------------------------#
# Makefile Variables: Programs ------------------#
#------------------------------------------------#

Cabal ?=cabal
Ghc   ?=ghc

#------------------------------------------------#

Open          ?=xdg-open
ClipboardCopy ?=xclip -selection clipboard

#------------------------------------------------#

Nix  ?=nix

#------------------------------------------------#
# Makefile Variables: Paths ---------------------#
#------------------------------------------------#

BuildDirectory   ?=./dist-newstyle
DataDirectory    ?=./data

#------------------------------------------------#

ShareDirectory          ?=./gitignored/share
HaddockDirectory        ?=$(ShareDirectory)/doc
ManpageDirectory        ?=$(ShareDirectory)/man
BashCompletionDirectory ?=$(ShareDirectory)/bash/completions
ZshCompletionDirectory  ?=$(ShareDirectory)/zsh/completions
FishCompletionDirectory ?=$(ShareDirectory)/fish/completions

#------------------------------------------------#
# Makefile Variables: Options -------------------#
#------------------------------------------------#

CabalOptions=--project-file $(CabalProject) --builddir $(BuildDirectory)

#------------------------------------------------#

HackageUser     =sboo
HackagePassword =pass hackage.haskell.org/user/$(HackageUser)

#------------------------------------------------#

GitHubOwner      =sboosali
GitHubRepository =$(Project)

#------------------------------------------------#
# Makefile Variables: Subcommands ---------------#
#------------------------------------------------#

CabalBuild   ?=$(Cabal) new-build $(CabalOptions)
CabalRun     ?=$(Cabal) new-run $(CabalOptions)

CabalTest    ?=$(Cabal) new-test --enable-tests $(CabalOptions)
CabalBench   ?=$(Cabal) new-bench --enable-benchmarks $(CabalOptions)
CabalDocs    ?=$(Cabal) new-haddock --enable-documentation $(CabalOptions)

CabalInstall ?=$(Cabal) new-install -v --overwrite-policy=always $(CabalOptions)
CabalDist    ?=$(Cabal) new-sdist -v

#------------------------------------------------#
# Makefile Variables: Environment Variables -----#
#------------------------------------------------#

LC_ALL=C.UTF-8

#------------------------------------------------#
# Makefile Targets: Standard --------------------#
#------------------------------------------------#

build:
	@printf "\n%s\n" "========================================"
	@printf "%s\n\n" "Building..."

	$(CabalBuild) $(CabalOptions) $(CabalTargets)

	@printf "\n%s\n" "========================================"

.PHONY: build

#------------------------------------------------#

check:
	@printf "\n%s\n" "========================================"
	@printf "%s\n\n" "Testing..."

	$(CabalTest) $(CabalOptions) $(CabalTargets)

	@printf "\n%s\n" "========================================"

.PHONY: check

#------------------------------------------------#

install: install-xxx-program-xxx bash-completion man

.PHONY: install

#------------------------------------------------#

dist:
	@printf "\n%s\n" "========================================"
	@printf "%s\n\n" "Archiving..."

	$(CabalDist) $(CabalOptions) $(CabalTargets)

	@printf "\n%s\n" "========================================"

.PHONY: dist

#------------------------------------------------#

all: build check install dist

.PHONY: all

#------------------------------------------------#
# Makefile Targets: Components ------------------#
#------------------------------------------------#

xxx-package-xxx:

	@printf "\n%s\n" "========================================"
	@printf "%s\n\n" "The Package..."

	$(CabalBuild) $(CabalOptions) "xxx-package-xxx:lib:xxx-package-xxx"

	@printf "\n%s\n" "========================================"

.PHONY: xxx-package-xxx

#------------------------------------------------#

xxx-program-xxx:

	@printf "\n%s\n" "========================================"
	@printf "%s\n\n" "The Program..."

	$(CabalRun) $(CabalOptions) "xxx-package-xxx:exe:xxx-program-xxx" -- --help

	@printf "\n%s\n" "========================================"

.PHONY: xxx-program-xxx

#------------------------------------------------#

install-xxx-program-xxx:
	@printf "\n%s\n" "========================================"
	@printf "%s\n\n" "Installing..."

	$(CabalInstall) $(CabalOptions) $(CabalProgram)

	@printf "\n%s\n" "========================================"

.PHONY: install-xxx-program-xxx

#------------------------------------------------#

bash-completion:

	@mkdir -p $(BashCompletionDirectory)

	@printf "\n%s\n" "========================================"
	@printf "%s\n\n" "Generating Bash Completion..."

	@mkdir -p "$(BashCompletionDirectory)"
	xxx-program-xxx --bash-completion-script `which xxx-program-xxx` > "$(BashCompletionDirectory)/xxx-program-xxx.bash"

	@printf "\n%s\n" "========================================"

	@$(ClipboardCopy) <(printf "%s\n" 'source "$(BashCompletionDirectory)/xxx-program-xxx.bash"')

.PHONY: bash-completion

# « make bash-completion » assumes the program has been installed (i.e. registered onto « $PATH »).
# To install it, run « make install-xxx-program-xxx ».

#------------------------------------------------#

man:

	@mkdir -p $(ManpageDirectory)/man1

	@printf "\n%s\n" "========================================"
	@printf "%s\n\n" "Generating Manual Pages..."

	pandoc --standalone --from=markdown --to=man ./xxx-package-xxx/docs/man/xxx-program-xxx.1.md | gzip > $(ManpageDirectory)/man1/xxx-program-xxx.1.gz

	@printf "\n%s\n" "========================================"

.PHONY: man

#------------------------------------------------#

docs:

	@printf "\n%s\n" "========================================"
	@printf "%s\n\n" "Building Documentation..."

	$(CabalHaddock) $(CabalOptions) $(CabalTargets)

	@printf "\n%s\n" "========================================"

.PHONY: docs

#------------------------------------------------#

bench:

	@printf "\n%s\n" "========================================"
	@printf "%s\n\n" "Benchmarking..."

	$(CabalBench) $(CabalOptions) $(CabalTargets)

	@printf "\n%s\n" "========================================"

.PHONY: bench

#------------------------------------------------#

checkdocs:

	@printf "\n%s\n" "========================================"
	@printf "%s\n\n" "Checking Documentation..."

	$(CabalTest) $(CabalOptions) "$(Package):test:doc"

	@printf "\n%s\n" "========================================"

	$(CabalHaddock) $(CabalOptions) $(CabalTargets)

	@printf "\n%s\n" "========================================"

.PHONY: checkdocs

#------------------------------------------------#
# Haskell Compilers -----------------------------#
#------------------------------------------------#

js:

	$(CabalBuild)  $(CabalTargets)  --project-file "./cabal-ghcjs.project"

.PHONY: js

#------------------------------------------------#

7.10:

	$(CabalBuild)  $(CabalTargets)  --with-compiler ghc-7.10.3

.PHONY: 7.10

#------------------------------------------------#

8.0:

	$(CabalBuild)  $(CabalTargets)  --with-compiler ghc-8.0.2

.PHONY: 8.0

#------------------------------------------------#

8.2:

	$(CabalBuild)  $(CabalTargets)  --with-compiler ghc-8.2.2

.PHONY: 8.2

#------------------------------------------------#

8.4:

	$(CabalBuild)  $(CabalTargets)  -with-compiler ghc-8.4.4

.PHONY: 8.4

#------------------------------------------------#

8.6:

	$(CabalBuild)  $(CabalTargets)  --with-compiler ghc-8.6.4

.PHONY: 8.6

#------------------------------------------------#

8.8:

	$(CabalBuild)  $(CabalTargets)  --with-compiler ghc-8.8.0

.PHONY: 8.8

#------------------------------------------------#
# Git -------------------------------------------#
#------------------------------------------------#

git-tag:

	git tag -a "$(Version)" -m "$(Version)"
	git push --tags origin master

.PHONY: git-tag

#------------------------------------------------#
# Makefile Targets: Development -----------------#
#------------------------------------------------#

update:

	$(Cabal) new-update "--project-file=./cabal.project"
	$(Cabal) new-update "--project-file=./cabal-ghcjs.project"
#	$(Cabal) new-update "--project-file=./cabal-static.project"

.PHONY: update

#------------------------------------------------#

typecheck:
	@printf "\n%s\n" "========================================"

	@printf "%s\n\n" "Typechecking..."

	$(Cabal) new-build $(CabalOptions) --ghc-options="-fno-code -fforce-recomp" $(CabalTargets)

	@printf "\n%s\n" "========================================"

.PHONY: typecheck

#------------------------------------------------#

tags:

	hasktags --etags  --follow-symlinks library/  --output TAGS  --tags-absolute

.PHONY: tags

#------------------------------------------------#

clean:

	$(Cabal) new-clean

	rm -rf dist/ dist-newstyle/ .sboo/
	rm -f *.project.local .ghc.environment.*

	rm -rf "result/" result-*/
	find .  -type f  -name 'TAGS'  -exec  rm -f '{}'  \+

#	rm -fr _
#	killall _

.PHONY: clean

#------------------------------------------------#
# Makefile Variables: Release -------------------#
#------------------------------------------------#

static:

.PHONY: static

#------------------------------------------------#
# Notes -----------------------------------------#
#------------------------------------------------#

# « cabal new-build » artefacts:
#
# e.g. « ./dist-newstyle/build/x86_64-linux/ghc-8.0.1/xxx-package-xxx-0.0.0/x/xxx-program-xxx/build/xxx-program-xxx/xxx-program-xxx »
#
# i.e. « ./dist-newstyle/build/$(Architecture)-$(Kernel)/$(CompilerFlavor)-$(CompilerVersion)/$(Package)-$(PackageVersion)/x/$(Executable)/build/$(Executable)/$(Executable) »
#

#------------------------------------------------#
# EOF -------------------------------------------#
#------------------------------------------------#




















































##################################################
# Makefile Settings
##################################################

SHELL=bash

.EXPORT_ALL_VARIABLES:

##################################################
# Makefile Customizeables:Customize
##################################################

DefaultPackageName ?=xxx-package-xxx
#                          ^ [Customize]

DefaultModule ?=Xxx_Module_xxX
#                          ^ [Customize]

DefaultProjectFile ?=./cabal.project
#                          ^ [Customize]

DefaultCompilerFlavor ?=ghc
#                          ^ [Customize]

DefaultCompilerVersion ?=8.6.3
#                          ^ [Customize]

Stackage ?=lts-13.9
#                          ^ [Customize]

##################################################
# Makefile Variables: Package
################################################## Customize:
# (i.e. Package-Specific / Component-Specific)

DefaultPackageVersion ?=0.0.0
#                          ^ [Customize]

DefaultLibraryTarget ?="lib:$(DefaultPackageName)"

DefaultExecutableTarget ?="exe:example-$(DefaultPackageName)"

DefaultTarget ?="all"
#                          ^ [Customize]
#        e.g.
#             "all"
#             $(DefaultTarget)
#             $(DefaultLibraryTarget)
#

DefaultPackage ?=$(DefaultPackageName)-$(DefaultPackageVersion)
#                          ^ [Derived]

##################################################
# Makefile Variables: Haskell Compiler.
################################################## Customize:

CompilerFlavor ?=$(DefaultCompilerFlavor)

CompilerVersion ?=$(DefaultCompilerVersion)

CompilerProgram ?=$(CompilerFlavor)-$(CompilerVersion)
#                          ^ [Derived]

##################################################
# Makefile Variables: Project / `cabal-new`
##################################################

Target ?=$(DefaultTarget)

ProjectFile ?=$(DefaultProjectFile)

EnableNix ?= # "--enable-nix"

CabalOptions ?=--project-file $(ProjectFile) -w $(CompilerProgram)
#                          ^ [Derived]

##################################################
# Makefile Variables: Programs.
##################################################

Cabal ?=cabal

#------------------------------------------------#

Ghc    ?=ghc
GhcPkg ?=ghc-pkg
Ghci   ?=ghci

#------------------------------------------------#

Nix      ?=nix
NixBuild ?=nix-build --show-trace
NixShell ?=nix-shell --show-trace

#------------------------------------------------#

Markdown ?=multimarkdown
 #TODO pandoc

Open ?=xdg-open

Cabal2nix ?=cabal2nix

#------------------------------------------------#

CheckCabal ?=$(Cabal) check
CheckTarball ?=tar -C /tmp -zxvf
CheckMarkdown ?=$(Markdown)
CheckJson ?=jsonlint
CheckBash ?=shellcheck

CheckNix ?=nix-instantiate

 # ^ nix-instantiate:
 # parse the given `.nix`, and return its `.drv` file.

CheckStaticExecutables ?=ldd

 # ^ TODO on OSX, otool -o

##################################################
# Makefile Variables: File/Directory Paths
##################################################

RootDirectory ?=$(CURDIR)
DefaultPackageDirectory ?=$(DefaultPackageName)

ReleaseDirectory ?=./ignore/release
#                          ^ [Customize]

# ReleaseDirectory ?=./release
# ^ change `ReleaseDirectory` to `./release` during a release
# to actually commit it.

BuildDirectory ?=./dist-newstyle
BuildTarballDirectory ?=$(BuildDirectory)/sdist/

NixDirectory ?=./nix
ScriptDirectory ?=./scripts
DocumentDirectory ?=./docs

HaddockDirectory ?=$(ReleaseDirectory)/documentation
TarballDirectory ?=$(ReleaseDirectory)/tarballs
BinaryDirectory ?=$(ReleaseDirectory)/bin
InstallDirectory ?=$(ReleaseDirectory)/dist-newstyle/ #TODO

##################################################
# Makefile Variables: (Miscellaneous) Strings
##################################################

Tags ?=etags
# ^ [Customize]
#
# Values:
#
# Tags=etags make tags
# Tags=ctags make tags
#

##################################################
# the `default` and `all` targets
##################################################







build:
	@printf "\n%s\n" "========================================"

	@printf "%s\n\n" "Building..."

	$(Cabal) new-build $(CabalOptions) $(CabalTargets)

	@printf "\n%s\n" "========================================"

.PHONY: build

#------------------------------------------------#

check:
	@printf "\n%s\n" "========================================"

	@printf "%s\n\n" "Testing..."

	$(Cabal) new-test $(CabalOptions) $(CabalTargets)

	@printf "\n%s\n" "========================================"

.PHONY: check

#------------------------------------------------#

install:
	@printf "\n%s\n" "========================================"

	@printf "%s\n\n" "Installing..."

	$(Cabal) new-install $(CabalOptions) $(CabalProgram)

	@printf "\n%s\n" "========================================"

.PHONY: install

#------------------------------------------------#

dist:
	@printf "\n%s\n" "========================================"

	@printf "%s\n\n" "Archiving..."

	$(Cabal) new-sdist $(CabalOptions) $(CabalTargets)

	@printf "\n%s\n" "========================================"

.PHONY: dist

#------------------------------------------------#

all: build check install dist

.PHONY: all

#------------------------------------------------#
















default: build-all

.PHONY: default

#------------------------------------------------#

all: info build-all test-all docs-all check-all tarball-all
	@echo '=================================================='
	@echo '[All (build/test/docs)] SUCCESS =================='
	@echo '=================================================='

.PHONY: all

#------------------------------------------------#

info:
	@echo '=================================================='
	@echo 'target'
	@echo $(Target)

	@echo '=================================================='
	$(Cabal) --version

	@echo '=================================================='
	ghc --version

	@echo '=================================================='

.PHONY: info

##################################################
# `cabal`-wrapper for target `all`
##################################################

check-haskell:
	$(Cabal) new-build -fno-code -O0 all

.PHONY: check-haskell

#------------------------------------------------#

build-all:
	$(Cabal) new-build all

.PHONY: build-all

#------------------------------------------------#

test-all:
	$(Cabal) new-test all

.PHONY: test-all

#------------------------------------------------#

bench-all:
	$(Cabal) new-bench all

.PHONY: bench-all

##################################################
# Default Component: build, test, repl.
##################################################

build-default:
	$(Cabal) new-build $(DefaultTarget)

.PHONY: build-default

##################################################

test-default:
	$(Cabal) new-test $(DefaultTarget)

.PHONY: test-default

##################################################

bench-default:
	$(Cabal) new-bench $(DefaultTarget)

.PHONY: bench-default

##################################################

repl-default:
	$(Cabal) new-repl $(DefaultLibraryTarget)

.PHONY: repl-default

##################################################
# Configuring: `cabal configure`
##################################################

configure: configure-8-6

.PHONY: configure

#------------------------------------------------#

configure-8-6:
	$(Cabal) $(EnableNix) new-configure -w ghc-8.6.1

.PHONY: configure-8-6

#------------------------------------------------#

configure-8-4:
	$(Cabal) $(EnableNix) new-configure -w ghc-8.4.3

.PHONY: configure-8-4

#------------------------------------------------#

configure-8-2:
	$(Cabal) $(EnableNix) new-configure -w ghc-8.2.2

.PHONY: configure-8-2

#------------------------------------------------#

configure-7-10:
	$(Cabal) $(EnableNix) new-configure -w ghc-7.10.3

.PHONY: configure-7-10

##################################################
# Nix: generate and validate `.nix` files
##################################################

shell.nix: $(DefaultPackageName)/*.cabal
	$(Cabal2nix) --shell $(DefaultPackageName)/*.cabal > $@

#------------------------------------------------#

##################################################
# Configuring: install non-Haskell dependencies.
##################################################

install: install-dependencies #TODO is "install" the right name.

.PHONY: install

#------------------------------------------------#

install-dependencies:
	nix-env -f ./nix/shell.nix -i

#TODO? ./nix/environment/shell.nix

.PHONY: install-dependencies

##################################################
# Building: different targets, compilers, build-tools.
##################################################

build: build-default
	@echo '=================================================='
	@echo '[Build] SUCCESS =================================='
	@echo '=================================================='

.PHONY: build

#------------------------------------------------#

cabal-compile: build-all

.PHONY: cabal-compile

#------------------------------------------------#

stack-compile:
	stack --nix build

.PHONY: stack-compile

##################################################
# Executables: building/running/registering them.
##################################################

build-example:
	$(Cabal) new-build xxx-package-xxx-example

.PHONY: build-example

##################################################
# Development: developing this package
##################################################

#------------------------------------------------#

tags:

	hasktags --etags  --follow-symlinks library/  --output TAGS  --tags-absolute

#	$(Cabal) new-repl $(Target) < <(echo -e $(TagsScript))
#	$(Ghci) -e ":$(Tags)"

        # ^ NOTE:
        # * the « <(...) » is a Process Substitution, while
        # * the « ... < ... » is a Redirection.

        # ^ NOTE:
        # « ghci -e » (« -e » means "evaluate") is for non-interactive usage.

.PHONY: tags

#------------------------------------------------#

##################################################
# Documentation: building/copying/opening
##################################################

docs: docs-all

.PHONY: docs

#------------------------------------------------#

docs-all: docs-markdown docs-haskell

.PHONY: docs-all

#------------------------------------------------#

docs-markdown:
	find . -name '*.md'   -print0 | xargs -n 1 -0 $(Markdown)
	find . -name '*.html' -print0

#TODO $(Markdown) 1.md > 1.html
#
# currently, it only checks the `md`.
#
#	find . -name '*.md' -exec sh -c '"$(Markdown)" "$1"' _  \{\} \;
#
#	find . -name '*.md' -print0 | xargs -n 1 -0 $(Markdown)

.PHONY: docs-markdown

# ^ NOTE:
# https://stackoverflow.com/questions/15030563/redirecting-stdout-with-find-exec-and-without-creating-new-shell

#------------------------------------------------#

docs-haskell: build-docs-haskell copy-docs-haskell
#TODO docs-haskell: build-docs-haskell copy-docs-haskell open-docs-haskell
	@echo '=================================================='
	@echo '[Haddocks] SUCCESS ==============================='
	@echo '=================================================='

.PHONY: docs-haskell

#------------------------------------------------#

build-docs-haskell: build-default
	@echo '=================================================='
	$(Cabal) new-haddock $(LibraryTarget) --enable-documentation
	@echo '=================================================='
	find $(BuildDirectory) -name "index.html" -print

.PHONY: build-docs-haskell

#------------------------------------------------#

copy-docs-haskell: build-docs-haskell
	rm    -fr $(HaddockDirectory)
	mkdir -p  $(HaddockDirectory)
	@echo '=================================================='
	cp -aRv  ./dist-newstyle/build/*-*/ghc-*/$(Package)/doc/html/$(PackageName)/src/* $(HaddockDirectory)

.PHONY: copy-docs-haskell

#------------------------------------------------#

open-docs-haskell: 
	@echo '=================================================='
	find $(HaddockDirectory) -name "$(Module).html" -print
	@echo '=================================================='
	find $(HaddockDirectory) -name "$(Module).html" -exec $(Open) \{\} \; #TODO open with `&`

.PHONY: open-docs-haskell

##################################################
# Verify different files (by extension) with different tools.
##################################################

check: check-all

.PHONY: check

#------------------------------------------------#

check-all: check-files   # check-haskell
	@echo '=================================================='
	@echo '[Check Everything] SUCCESS ======================='
	@echo '=================================================='
	@echo 

.PHONY: check-all

#------------------------------------------------#

check-files: check-markdown check-json check-cabal check-bash check-nix
	@echo '=================================================='
	@echo '[Check Files/Tools (non-Code)] SUCCESS ==========='
	@echo '=================================================='
	@echo 
        # ^ check all (non-code) tools and files.

.PHONY: check-files

#------------------------------------------------#

check-markdown:
	@echo '=================================================='
	find $(DocumentDirectory)/ -name '*.md'  -print0 | xargs -n 1 -0 $(CheckMarkdown)
	find $(PackageDirectory)/$(DocumentDirectory)/ -name '*.md'  -print0 | xargs -n 1 -0 $(CheckMarkdown)

.PHONY: check-markdown

#------------------------------------------------#

check-json:
	@echo '=================================================='
	find $(RootDirectory) -name "*.json" -print0 -exec $(CheckJson) \{\} \;

.PHONY: check-json

#------------------------------------------------#

check-cabal:
	@echo '=================================================='
	(cd $(PackageDirectory) && $(CheckCabal))

.PHONY: check-cabal

#------------------------------------------------#

check-bash:
	@echo '=================================================='
	find $(ScriptDirectory) -name "*.sh" -print0 -exec $(CheckBash) \{\} \;

.PHONY: check-bash

#------------------------------------------------#

check-nix:
	@echo '=================================================='
	find $(NixDirectory) -name "*.nix" -print0 -exec $(CheckNix) \{\} \;

.PHONY: check-nix

#------------------------------------------------#

# check-text:
# 	@echo '=================================================='
# 	find */$(DocumentDirectory)/ -name "*" -type f -print0 -exec $(CheckText) \{\} \;

# .PHONY: check-text

#	find */$(DocumentDirectory)/ -name "*" -type f -print0 -exec $(CheckText) \{\} \;
        # ^ [TODO] ( '*.txt' | '*.md' | '*.html' | '*.org' )

#------------------------------------------------#

# check-cabal:
# 	find $(Directory) -name "*.cabal" -print0 -exec $(CheckCabal) \{\} \;
# .PHONY: check-cabal

##################################################
# Information: print out m isc info, dry-runs.
##################################################

print-platform:
	@$(Ghci) -e ":unset +t" -e "putStrLn System.Info.arch" -e "putStrLn System.Info.os" 2>/dev/null

# e.g.
#
# $ make print-platform
#
#   x86_64
#   linux

.PHONY: print-platform

#------------------------------------------------#

print-architecture:
	@$(Ghci) -e ":unset +t" -e "putStrLn System.Info.arch" 2>/dev/null

.PHONY: print-architecture

#------------------------------------------------#

print-operating-system:
	@$(Ghci) -e ":unset +t" -e "putStrLn System.Info.os" 2>/dev/null

.PHONY: print-operating-system

##################################################
# Tarballs: zip them and unzip them.
##################################################

tarball: tarball-all

.PHONY: tarball

#------------------------------------------------#

tarball-all: copy-tarball
	@echo '=================================================='
	@echo '[Tarballs (.tar.gz)] SUCCESS ====================='
	@echo '=================================================='
	@echo 
	@find $(TarballDirectory) -name "*.tar.gz"
	@echo 
	@echo '=================================================='

.PHONY: tarball-all

#------------------------------------------------#

cabal-sdist:
	$(Cabal) new-sdist all

.PHONY: cabal-sdist

#------------------------------------------------#

copy-tarball: cabal-sdist
	@echo '=================================================='
	rm    -fr $(TarballDirectory)
	mkdir -p  $(TarballDirectory)
	find $(BuildTarballDirectory) -name "*.tar.gz" -exec mv \{\} $(TarballDirectory) \;

.PHONY: copy-tarball

# #------------------------------------------------#

# check-tarball: copy-tarball
# 	@echo '=================================================='
# 	find $(TarballDirectory) -name "*.tar.gz" -print0 -exec $(CheckTarball) \{\} \;
#         # ^ verifies tarball, by unpacking it.
# 	find /tmp/$(Package) -name "*.tar.gz"    		# TODO

# .PHONY: check-tarball

# #------------------------------------------------#

# copy-tarball: build-tarball
# 	@echo '=================================================='
# 	rm    -fr $(TarballDirectory)
# 	mkdir -p  $(TarballDirectory)
# 	find $(PackageDirectory) -name "*.tar.gz" -print0 -exec mv \{\} $(TarballDirectory) \;      		# TODO

# .PHONY: copy-tarball

# #------------------------------------------------#

# build-tarball: build-default cabal-sdist
# 	@echo '=================================================='

# .PHONY: build-tarball


##################################################
# Fetching: files, resources
##################################################

#------------------------------------------------#

fetch-stackage: fetch-stackage--stackage.project

.PHONY: fetch-stackage

#------------------------------------------------#

fetch-stackage--stackage.project:

	@mkdir -p "./cabal"

	@echo '=================================================='
	@echo

	curl -sL "https://www.stackage.org/$(Stackage)/cabal.config?global=false"  >  "./cabal/stackage-$(Stackage).project"

	@echo '=================================================='
	@echo

	@cat  "./cabal/stackage-$(Stackage).project"

	@echo
	@echo '=================================================='

.PHONY: fetch-stackage--stackage.project

#------------------------------------------------#

fetch-stackage--cabal.config:

	@echo '=================================================='
	@echo

	curl -sL "https://www.stackage.org/$(Stackage)/cabal.config?global=true"  >  "./cabal.config"

	@echo '=================================================='
	@echo

	@cat  "./cabal.config"

	@echo
	@echo '=================================================='

.PHONY: fetch-stackage--cabal.config

#------------------------------------------------#

##################################################
# Cleaning
##################################################

clean:

	$(Cabal) new-clean

	rm -rf "./dist" "./dist-newstyle" ./dist-* ".stack-work"
	rm -rf ./*/dist/ ./*/dist-*/

	rm -f *.project.local .ghc*.environment.*
	rm -rf TAGS ./*/TAGS

.PHONY: clean

#------------------------------------------------#

cabal-clean:
	$(Cabal) new-clean

.PHONY: cabal-clean

##################################################
# Release: tarball and documentation into `./realease/`
##################################################

#------------------------------------------------#

release: release-all

.PHONY: release

#------------------------------------------------#

release-all: docs-haskell tarball
	find $(ReleaseDirectory) -type f
	@echo '=================================================='
	@echo '[Release] SUCCESS ================================'
	@echo '=================================================='

.PHONY: release-all

#------------------------------------------------#

install:
	$(Cabal) new-build all --prefix=$(InstallDirectory)

.PHONY: install

#------------------------------------------------#

upload: tarball
	$(Cabal) upload
	@echo '=================================================='
	@echo '[Upload] SUCCESS ================================='
	@echo '=================================================='

.PHONY: upload

#------------------------------------------------#










##################################################
# Building #######################################
##################################################

#------------------------------------------------#

build:

	@echo "=================================================="
	@echo ""

	$(Cabal) new-build $(CabalOptions) $(CabalTargets)

	@echo ""
	@echo "=================================================="


.PHONY: build

#------------------------------------------------#

build-ghcjs:

	$(Cabal) new-build --project-file="./cabal-ghcjs.project" all

.PHONY: build-ghcjs

#------------------------------------------------#


#------------------------------------------------#

##################################################
# Testing ########################################
##################################################

#------------------------------------------------#

test:

	@echo "=================================================="
	@echo ""

	$(Cabal) new-test $(CabalOptions) --enable-tests $(CabalTargets)

	@echo ""
	@echo "=================================================="

.PHONY: test

#------------------------------------------------#

bench:

	@echo "=================================================="
	@echo ""

	$(Cabal) new-bench $(CabalOptions) --enable-benchmarks $(CabalTargets)

	@echo ""
	@echo "=================================================="

.PHONY: bench

#------------------------------------------------#



#------------------------------------------------#

##################################################
# Documentation ##################################
##################################################

#------------------------------------------------#

docs:

	@echo "=================================================="
	@echo ""

	$(Cabal) new-haddock $(CabalOptions) --enable-documentation $(CabalTargets)

	@echo ""
	@echo "=================================================="
	@echo ""

	find $(BuildDirectory) -name "index.html" -print

	@echo ""
	@echo "=================================================="

.PHONY: docs

#------------------------------------------------#

##################################################
# Release ########################################
##################################################

#------------------------------------------------#

sdist:

	$(Cabal) new-build $(CabalTargets)
	$(Cabal) new-sdist $(CabalTargets)

#	(cd ./spiros  &&  cabal sdist)

.PHONY: sdist

#------------------------------------------------#

static:

	$(Cabal) new-build --enable-executable-static $(CabalTargets)
	$(Cabal) new-sdist $(CabalTargets)

.PHONY: static

# TODO: test the program: ① whether static and ② how large.
#
# $ ldd ./bin/cabal
# 	not a dynamic executable
#
# $ du -h ./bin/cabal
# 18M	./bin/cabal
#
#

#------------------------------------------------#

##################################################

















##################################################
# (Miscellaneous)
##################################################