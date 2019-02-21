##################################################
# Makefile Settings
##################################################

SHELL=bash

.EXPORT_ALL_VARIABLES:

##################################################
# Makefile Customizeables:Customize
##################################################

DefaultPackageName?=xxx-package-xxx
#                          ^ [Customize]

DefaultModule?=Xxx_Module_xxX
#                          ^ [Customize]

DefaultProjectFile?=./cabal.project
#                          ^ [Customize]

DefaultCompilerFlavor?=ghc
#                          ^ [Customize]

DefaultCompilerVersion?=8.6.3
#                          ^ [Customize]

##################################################
# Makefile Variables: Package
################################################## Customize:
# (i.e. Package-Specific / Component-Specific)

DefaultPackageVersion?=0.0.0
#                          ^ [Customize]

DefaultLibraryTarget?="lib:$(DefaultPackageName)"
#                          ^ [Customize]
#                          ^  e.g. "lib:xxx-package-xxx"

DefaultTarget?="all"
#                          ^ [Customize]
#        e.g.
#             "all"
#             $(DefaultTarget)
#             $(DefaultLibraryTarget)
#

DefaultPackage?=$(DefaultPackageName)-$(DefaultPackageVersion)
#                          ^ [Derived]

##################################################
# Makefile Variables: Haskell Compiler.
################################################## Customize:

CompilerFlavor?=$(DefaultCompilerFlavor)

CompilerVersion?=$(DefaultCompilerVersion)

CompilerProgram?=$(CompilerFlavor)-$(CompilerVersion)
#                          ^ [Derived]

##################################################
# Makefile Variables: Project / `cabal-new`
##################################################

Target?=$(DefaultTarget)

ProjectFile?=$(DefaultProjectFile)

EnableNix?= # "--enable-nix"

CabalOptions?=--project-file $(ProjectFile) -w $(CompilerProgram)
#                          ^ [Derived]

##################################################
# Makefile Variables: Programs.
##################################################

Cabal ?=cabal

Ghc ?=ghc
GhcPkg ?=ghc-pkg
Ghci ?=ghci

Markdown?=multimarkdown
 #TODO pandoc

Open?=xdg-open

CheckCabal?=$(Cabal) check
CheckTarball?=tar -C /tmp -zxvf
CheckMarkdown?=$(Markdown)
CheckJson?=jsonlint
CheckBash?=shellcheck
CheckNix?=nix-instantiate
 # ^ nix-instantiate:
 # parse the given `.nix`, and return its `.drv` file.

Cabal2nix ?=cabal2nix

##################################################
# Makefile Variables: File/Directory Paths
##################################################

RootDirectory?=$(CURDIR)
DefaultPackageDirectory?=$(DefaultPackageName)

ReleaseDirectory?=./ignore/release
#                          ^ [Customize]

# ReleaseDirectory?=./release
# ^ change `ReleaseDirectory` to `./release` during a release
# to actually commit it.

BuildDirectory?=./dist-newstyle
BuildTarballDirectory?=$(BuildDirectory)/sdist/

NixDirectory?=./nix
ScriptDirectory?=./scripts
DocumentDirectory?=./docs

HaddockDirectory?=$(ReleaseDirectory)/documentation
TarballDirectory?=$(ReleaseDirectory)/tarballs
BinaryDirectory?=$(ReleaseDirectory)/bin
InstallDirectory?=$(ReleaseDirectory)/dist-newstyle/ #TODO

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
# Testing:
##################################################

test: test-default
	@echo '=================================================='
	@echo '[Test] SUCCESS ==================================='
	@echo '=================================================='

.PHONY: test

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
#	$(Cabal) new-repl $(Target) < <(echo -e $(TagsScript))
	$(Ghci) -e ":$(Tags)"

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
# Cleaning
##################################################

clean: cabal-clean
	rm -rf "./dist/" "./dist-newstyle/" ./dist-* ".stack-work"
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
# (Miscellaneous)
##################################################