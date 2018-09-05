##################################################
# Makefile Variables (Package-Specific / Component-Specific)
##################################################

DefaultPackageName=xmlrpc
DefaultPackageVersion=0.0.0

DefaultModule=XmlRpc
DefaultLibraryTarget="lib:$(DefaultPackageName)" # e.g. "lib:xmlrpc"

##################################################
# Makefile Directives
##################################################

.EXPORT_ALL_VARIABLES:

##################################################
# Makefile Variables (Package-Independent)
##################################################

DefaultPackage=$(DefaultPackageName)-$(DefaultPackageVersion)

##################################################

CompilerFlavor=ghc

CompilerVersion=8.4.3

CompilerProgram=$(CompilerFlavor)-$(CompilerVersion)

##################################################

Cabal=cabal

Markdown=multimarkdown
#TODO pandoc

Open=xdg-open

CheckCabal=$(Cabal) check
CheckTarball=tar -C /tmp -zxvf
CheckBash=shellcheck
CheckNix=nix-instantiate
CheckJson=jsonlint
CheckMarkdown=$(Markdown)

# ^ nix-instantiate:
# parse the given `.nix`, and return its `.drv` file.

##################################################

RootDirectory=$(CURDIR)

BuildDirectory=./dist-newstyle
NixDirectory=./nix
ScriptDirectory=./scripts
DocumentDirectory=./docs

ReleaseDirectory=./release
HaddockDirectory=$(ReleaseDirectory)/documentation
TarballDirectory=$(ReleaseDirectory)/tarballs

DefaultPackageDirectory=$(DefaultPackageName)

##################################################
# the `default` and `all` targets
##################################################

default: build

.PHONY: default

##################################################

all: build-all test-all docs-all check-all tarball-all 
	@echo '=================================================='
	@echo '[All (build/test/docs)] SUCCESS =================='
	@echo '=================================================='

.PHONY: all

##################################################
# `cabal` wrapper targets
##################################################

check-haskell:
	$(Cabal) new-build -fno-code -O0 all

.PHONY: check-haskell

##################################################

build-all:
	$(Cabal) new-build all

.PHONY: build-all

##################################################

test-all:
	$(Cabal) new-test all

.PHONY: test-all

##################################################
# Default Component: build, test, repl.
##################################################

build-default:
	$(Cabal) new-build $(DefaultLibraryTarget)

.PHONY: build-default


##################################################

test-default:
	$(Cabal) new-test $(DefaultLibraryTarget)

.PHONY: test-default

##################################################

repl-default:
	$(Cabal) new-repl $(DefaultLibraryTarget)

.PHONY: repl

##################################################
# Building: different targets, compilers, build-tools.
##################################################

build: build-default
	@echo '=================================================='
	@echo '[Build] SUCCESS =================================='
	@echo '=================================================='

.PHONY: build

##################################################

cabal-compile:
	$(Cabal) new-build all

.PHONY: cabal-compile

##################################################

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

build-examples:
	$(Cabal) new-build xmlrpc-examples

.PHONY: build-examples

##################################################

examples: build-examples
	@echo '=================================================='
	$(Cabal) new-run xmlrpc-example-time
	@echo '=================================================='
	$(Cabal) new-run xmlrpc-example-validator
	@echo '=================================================='
	$(Cabal) new-run xmlrpc-example-introspect
	@echo '=================================================='
	$(Cabal) new-run xmlrpc-example-simple
	@echo '=================================================='
	$(Cabal) new-run xmlrpc-example-person
	@echo '=================================================='

.PHONY: examples

##################################################
# Documentation: building/copying/opening
##################################################

docs: docs-all

.PHONY: docs

##################################################

docs-all: docs-markdown docs-haskell

.PHONY: docs-all

##################################################

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

# ^ 
# https://stackoverflow.com/questions/15030563/redirecting-stdout-with-find-exec-and-without-creating-new-shell

##################################################

docs-haskell: build-docs-haskell copy-docs-haskell
#TODO docs-haskell: build-docs-haskell copy-docs-haskell open-docs-haskell
	@echo '=================================================='
	@echo '[Haddocks] SUCCESS ==============================='
	@echo '=================================================='

.PHONY: docs-haskell

##################################################

build-docs-haskell: build-default
	@echo '=================================================='
	$(Cabal) new-haddock $(DefaultLibraryTarget) --enable-documentation
	@echo '=================================================='
	find $(BuildDirectory) -name "index.html" -print

.PHONY: build-docs-haskell

##################################################

copy-docs-haskell: build-docs-haskell
	rm    -fr $(HaddockDirectory)
	mkdir -p  $(HaddockDirectory)
	@echo '=================================================='
	cp -aRv  ./dist-newstyle/build/*-*/ghc-*/$(DefaultPackage)/doc/html/$(DefaultPackageName)/src/* $(HaddockDirectory)

.PHONY: copy-docs-haskell

##################################################

open-docs-haskell: 
	@echo '=================================================='
	find $(HaddockDirectory) -name "$(DefaultModule).html" -print
	@echo '=================================================='
	find $(HaddockDirectory) -name "$(DefaultModule).html" -exec $(Open) \{\} \; #TODO open with `&`

.PHONY: open-docs-haskell

##################################################
# Verify different files (by extension) with different tools.
##################################################

check: check-all

.PHONY: check

##################################################

check-all: check-files   # check-haskell
	@echo '=================================================='
	@echo '[Check Everything] SUCCESS ======================='
	@echo '=================================================='
	@echo 

.PHONY: check-all

##################################################

check-files: check-markdown check-json check-cabal check-bash check-nix
	@echo '=================================================='
	@echo '[Check Files/Tools (non-Code)] SUCCESS ==========='
	@echo '=================================================='
	@echo 
        # ^ check all (non-code) tools and files.

.PHONY: check-files

##################################################

check-markdown:
	@echo '=================================================='
	find $(DocumentDirectory)/ -name '*.md'  -print0 | xargs -n 1 -0 $(CheckMarkdown)
	find $(DefaultPackageDirectory)/$(DocumentDirectory)/ -name '*.md'  -print0 | xargs -n 1 -0 $(CheckMarkdown)

.PHONY: check-markdown

##################################################

check-json:
	@echo '=================================================='
	find $(RootDirectory) -name "*.json" -print0 -exec $(CheckJson) \{\} \;

.PHONY: check-json

##################################################

check-cabal:
	@echo '=================================================='
	(cd $(DefaultPackageDirectory) && $(CheckCabal))

.PHONY: check-cabal

##################################################

check-bash:
	@echo '=================================================='
	find $(ScriptDirectory) -name "*.sh" -print0 -exec $(CheckBash) \{\} \;

.PHONY: check-bash

##################################################

check-nix:
	@echo '=================================================='
	find $(NixDirectory) -name "*.nix" -print0 -exec $(CheckNix) \{\} \;

.PHONY: check-nix

##################################################

# check-text:
# 	@echo '=================================================='
# 	find */$(DocumentDirectory)/ -name "*" -type f -print0 -exec $(CheckText) \{\} \;

# .PHONY: check-text

#	find */$(DocumentDirectory)/ -name "*" -type f -print0 -exec $(CheckText) \{\} \;
        # ^ [TODO] ( '*.txt' | '*.md' | '*.html' | '*.org' ) 

##################################################

# check-cabal:
# 	find $(Directory) -name "*.cabal" -print0 -exec $(CheckCabal) \{\} \;
# .PHONY: check-cabal

##################################################
# Tarballs: zip them and unzip them.
##################################################

tarball-all: tarball 		# TODO

.PHONY: tarball-all

##################################################

tarball: build-tarball check-tarball
	@echo '=================================================='
	@echo '[Tarballs (.tar.gz)] SUCCESS ====================='
	@echo '=================================================='

.PHONY: tarball

##################################################

check-tarball: copy-tarball
	@echo '=================================================='
	find $(TarballDirectory) -name "*.tar.gz" -print0 -exec $(CheckTarball) \{\} \;
        # ^ verifies tarball, by unpacking it.
	find /tmp/$(DefaultPackage) -name "*.tar.gz"

.PHONY: check-tarball

##################################################

copy-tarball: build-tarball
	@echo '=================================================='
	rm    -fr $(TarballDirectory)
	mkdir -p  $(TarballDirectory)
	find $(DefaultPackageDirectory) -name "*.tar.gz" -print0 -exec mv \{\} $(TarballDirectory) \;

.PHONY: copy-tarball

##################################################

build-tarball: build-default
	@echo '=================================================='
	(cd  $(DefaultPackageDirectory) && $(Cabal) sdist)

.PHONY: build-tarball

##################################################
# (Miscellaneous)
##################################################

clean:
	rm -rf "./dist/" "./dist-newstyle/" "./dist-newdante/" "./dist-ghcjs/"
	rm -f *.project.local .ghc*.environment.*
	rm -rf ./*/dist/ ./*/dist-newstyle/

.PHONY: clean

##################################################
# Release: 
##################################################

release: release-all

.PHONY: release

##################################################

release-all: docs-haskell tarball
	find $(ReleaseDirectory) -type f
	@echo '=================================================='
	@echo '[Release] SUCCESS ================================'
	@echo '=================================================='

.PHONY: release-all

##################################################

upload: tarball
	$(Cabal) upload
	@echo '=================================================='
	@echo '[Upload] SUCCESS ================================='
	@echo '=================================================='

.PHONY: upload

##################################################
##################################################
##################################################