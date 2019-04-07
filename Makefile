##################################################
# Makefile Settings ##############################
##################################################

SHELL=bash

.EXPORT_ALL_VARIABLES:

##################################################
# Makefile Variables #############################
################################################## Haskell

#------------------------------------------------#

PackageName ?=skeletor

ProjectFile ?=./cabal.project

ModuleName ?=Skeletor.Haskell

Target ?=skeletor

CabalTargets ?=all

#------------------------------------------------#

CompilerFlavor ?=ghc

CompilerVersion ?=8.6.3

Stackage ?=lts-13.9

##################################################

# (i.e. Package-Specific / Component-Specific)

PackageVersion ?=0.0.0
#                          ^ [Customize]

LibraryTarget ?=lib:$(PackageName)
#                          ^ [Customize]
#                          ^  e.g. "lib:skeletor"

ExecutableProgram=skeletor-haskell
#                          ^ [Customize]

ExecutableTarget ?=skeletor:exe:$(ExecutableProgram)
#                          ^ [Customize]

Component ?=skeletor
#                          ^ [Customize]

Target ?=all #TODO
#                             e.g. "all"

##################################################

Package=$(PackageName)-$(PackageVersion)

#------------------------------------------------#

CompilerProgram=$(CompilerFlavor)-$(CompilerVersion)

################################################## Cabal

CabalOptions=--project-file $(ProjectFile) -w $(CompilerProgram)

#------------------------------------------------#

Cabal		?=cabal

CabalBuild	?=$(Cabal) new-build $(CabalOptions)

CabalTest	?=$(Cabal) new-test $(CabalOptions) --enable-tests

CabalBench	?=$(Cabal) new-bench $(CabalOptions) --enable-benchmarks

CabalHaddock	?=$(Cabal) new-haddock $(CabalOptions) --enable-documentation

################################################## Programs

Nix      ?=nix
NixBuild ?=nix-build --show-trace
NixShell ?=nix-shell --show-trace

#------------------------------------------------#

Pandoc ?=pandoc

Markdown ?=multimarkdown
 #TODO pandoc

Open ?=xdg-open

Cabal2nix ?=cabal2nix

MakeETags ?=hasktags --etags  --tags-absolute --follow-symlinks
MakeCTags ?=hasktags --ctags  --tags-absolute --follow-symlinks
#MakeGTags ?=hasktags --gtags  --tags-absolute --follow-symlinks

#------------------------------------------------#

CheckCabal ?=$(Cabal) check
CheckTarball ?=tar -C /tmp -zxvf
CheckMarkdown ?=$(Markdown)
CheckJson ?=jsonlint
CheckBash ?=shellcheck

CheckNix ?=nix-instantiate

 # ^ nix-instantiate:
 # parse the given `.nix`, and return its `.drv` file.

CheckStaticExecutables ?=lld    #TODO# grep too, return non-zero-exit-code when any dynamic dependencies exist

 # ^ TODO on OSX, otool -o

################################################## Paths

RootDirectory ?=$(CURDIR)
PackageDirectory ?=$(PackageName)

#------------------------------------------------#

BuildDirectory ?=./dist-newstyle

BashCompletionDirectory ?=./etc/bash_completion.d

NixDirectory      ?=./nix
ScriptDirectory   ?=./scripts
DocumentDirectory ?=./docs

#------------------------------------------------#

ReleaseDirectory ?=./ignore/release
#                          ^ [Customize]

# ReleaseDirectory?=./release
# ^ change `ReleaseDirectory` to `./release` during a release
# to actually commit it.

HaddockDirectory	?=$(ReleaseDirectory)/documentation
TarballDirectory	?=$(ReleaseDirectory)/tarballs
BinaryDirectory		?=$(ReleaseDirectory)/bin
InstallDirectory	?=$(ReleaseDirectory)/dist-newstyle/ #TODO

################################################## Miscellaneous

ETagsFile      ?=TAGS
CTagsFile      ?=tags

TagsDirectory ?=$(PackageDirectory)

##################################################
# Makefile Variables #############################
##################################################

# Project-Specific Variables...

#------------------------------------------------#

TemplateName ?=default

#------------------------------------------------#

##################################################
# the `default` and `all` targets
##################################################

default: all

.PHONY: default

#------------------------------------------------#

all: build-all test-all docs-all check-all tarball-all
	@echo '=================================================='
	@echo '[All (build/test/docs)] SUCCESS =================='
	@echo '=================================================='

.PHONY: all

#------------------------------------------------#

develop:
	@echo '=================================================='
	@echo '[Build] SUCCESS =================================='
	@echo '=================================================='

	$(Cabal) new-build -fdevelop $(Component)

.PHONY: develop

##################################################
# Components: by name ############################
##################################################

lib:
	$(Cabal) new-run $(LibraryTarget)

.PHONY: lib

#------------------------------------------------#

skeletor-haskell:

	LC_ALL=C.UTF-8 $(Cabal) new-run skeletor-haskell -- --help

.PHONY: skeletor-haskell

#------------------------------------------------#

# skeletor-elisp:
# 	$(Cabal) new-run skeletor-elisp

# .PHONY: skeletor-elisp

# #------------------------------------------------#

# skeletor-nix:
# 	$(Cabal) new-run skeletor-nix

# .PHONY: skeletor-nix

# #------------------------------------------------#

# skeletor-python:
# 	$(Cabal) new-run skeletor-python

# .PHONY: skeletor-python

#------------------------------------------------#

unittest:
	$(Cabal) new-run "$(Component):test:unit"

.PHONY: unittest

#------------------------------------------------#

doctest:
	$(Cabal) new-run "$(Component):test:doc"

.PHONY: doctest

#------------------------------------------------#

quickcheck:
	$(Cabal) new-run "$(Component):test:property"

.PHONY: quickcheck

##################################################
# Components: project packages ###################
##################################################

# haskell-project:
# 	find -L "./projects/$(TemplateName)" -type f -name "*.cabal" -prune -o -wholename '.stack-work' -o -wholename 'dist' -o -wholename 'dist-*' -o -wholename 'result' -o -wholename 'result-*' -exec $(Cabal) new-build '{}' \;
#TODO#
# .PHONY: haskell-project

#------------------------------------------------#

haskell-project-default:
	$(Cabal) new-build ./projects/default/xxx-package-xxx/xxx-package-xxx.cabal

.PHONY: haskell-project-default

#------------------------------------------------#

haskell-project-simple:
	$(Cabal) new-build ./projects/simple/xxx-package-xxx.cabal

.PHONY: haskell-project-simple

##################################################
# `cabal` wrapper targets
##################################################

check-haskell:
	$(Cabal) new-build -fno-code -O0 all

.PHONY: check-haskell

#------------------------------------------------#

build-all:
	$(Cabal) new-build all

.PHONY: build-all

#------------------------------------------------#

run-skeletor-haskell:

	@echo -e "\n========================================\n"

	@$(Cabal) new-build "exe:skeletor-haskell"

	@echo -e "\n========================================\n"

	@$(Cabal) new-exec -- ldd `which skeletor-haskell`

	@echo -e "\n========================================\n"
	@echo ""

	$(Cabal) new-run exe:skeletor-haskell -- create --help || true

	@echo ""
	@echo -e "\n========================================\n"
	@echo ""

	$(Cabal) new-run exe:skeletor-haskell --        --help || true

	@echo ""
	@echo -e "\n========================================\n"

	@$(Cabal) new-exec -- which skeletor-haskell

	@echo -e "\n========================================\n"

.PHONY: run-skeletor-haskell

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
	$(Cabal) new-build $(Target)

.PHONY: build-default

#------------------------------------------------#

test-default:
	$(Cabal) new-test $(Target)

.PHONY: test-default

#------------------------------------------------#

bench-default:
	$(Cabal) new-bench $(Target)

.PHONY: bench-default

#------------------------------------------------#

repl-default:
	$(Cabal) new-repl $(LibraryTarget)

.PHONY: repl-default

##################################################
# Executables.
##################################################

build-exe:
	$(Cabal) new-build $(ExecutableTarget)

.PHONY: build-exe

#------------------------------------------------#

repl-exe:
	$(Cabal) new-repl $(ExecutableTarget)

.PHONY: repl-exe

##################################################
# Interpreter.
##################################################

repl: repl-default

.PHONY: repl

#------------------------------------------------#

cabal-compile: build-all

.PHONY: cabal-compile

#------------------------------------------------#

# stack-compile:
# 	stack --nix build

# .PHONY: stack-compile

##################################################
# Executables: building/running/registering them.
##################################################

build-executable:
	$(Cabal) new-build $(ExecutableTarget)

.PHONY: build-executable

#------------------------------------------------#

run:
	$(Cabal) new-build $(ExecutableTarget)

.PHONY: run

#------------------------------------------------#

locate-executable:
	@$(Cabal) new-exec which -- $(ExecutableProgram)

.PHONY: locate-executable

# e.g. « /home/sboo/haskell/skeletor/dist-newstyle/build/x86_64-linux/ghc-8.6.3/skeletor-0.0.0/x/skeletor-haskell/build/skeletor-haskell/skeletor-haskell »

# Usage (Command-Line):
#
#   $(make locate-executable 2>/dev/null)

#------------------------------------------------#

# install-skeletor-haskell: static-build-skeletor-haskell

# 	@echo '=================================================='
# 	@echo

# 	time $(Cabal) new-install -overwrite-policy=always "skeletor:exe:skeletor-haskell"

# 	@echo
# 	@echo '=================================================='
# 	@echo

# 	@mkdir -p "$(BashCompletionDirectory)"

# 	skeletor-haskell --bash-completion-script skeletor-haskell > "$(BashCompletionDirectory)/skeletor-haskell.bash"

# 	@echo
# 	@echo '=================================================='
# 	@echo

# 	@echo $(Lld) `which skeletor-haskell`

# 	@echo
# 	@echo '=================================================='
# 	@echo

# 	@echo source `readlink -f "$(BashCompletionDirectory)/skeletor-haskell.bash"`

# 	@echo
# 	@echo '=================================================='

# .PHONY: install-skeletor-haskell

#------------------------------------------------#

uninstall-skeletor-haskell:

	if [ -L "~/.cabal/bin/skeletor-haskell" ]; then rm "~/.cabal/bin/skeletor-haskell"; fi

.PHONY: uninstall-skeletor-haskell

#------------------------------------------------#
# Nix -------------------------------------------#
#------------------------------------------------#

default.nix: skeletor/*.cabal

	$(Cabal2nix) $^ > $@

#------------------------------------------------#

shell.nix: skeletor/*.cabal

	$(Cabal2nix) --shell $^ > $@

#TODO# cabal2nix file://$(readlink -f skeletor.cabal)

##################################################
# Static-Linking
##################################################

# static: cabal-static.project static--skeletor-haskell--via-nix
# .PHONY: static

#------------------------------------------------#

cabal-static.project:

	@echo '=================================================='
	@echo

	$(NixBuild)  "./nix/static"  -A "cabal-project-file"  -o "./cabal-static.project"

	@echo '=================================================='
	@echo

	@cat "./cabal-static.project"

	@echo
	@echo '=================================================='

.PHONY: cabal-static.project

#------------------------------------------------#

static--skeletor-haskell--via-nix: cabal-static.project

	@echo '=================================================='
	@echo

	time  $(NixBuild)  "./nix/static"  -A "skeletor-static"  -o "./result-static"

	@echo
	@echo '=================================================='

.PHONY: static--skeletor-haskell--via-nix

#------------------------------------------------#

static--skeletor-haskell--via-cabal-new: cabal-static.project

	@echo '=================================================='
	@echo

	time  $(Cabal)  new-build  "--project-file=./cabal-static.project"  "skeletor:exe:skeletor-haskell"

	@echo
	@echo '=================================================='

.PHONY: static--skeletor-haskell--via-cabal-new

#------------------------------------------------#

install-static-skeletor-haskell: cabal-static.project

	@echo '=================================================='
	@echo

	time  $(Cabal)  new-install  --overwrite-policy=always  "--project-file=./cabal-static.project"  "skeletor:exe:skeletor-haskell"

	@echo
	@echo '=================================================='
	@echo

	@mkdir -p "$(BashCompletionDirectory)"

	skeletor-haskell --bash-completion-script skeletor-haskell > "$(BashCompletionDirectory)/skeletor-haskell.bash"

	@echo
	@echo '=================================================='
	@echo

	@echo `which skeletor-haskell`

	@echo
	@echo '=================================================='
	@echo

	@echo source `readlink -f "$(BashCompletionDirectory)/skeletor-haskell.bash"`

	@echo
	@echo '=================================================='
	@echo

#	@echo ! ldd `which skeletor-haskell`
#	@echo ldd `which skeletor-haskell` | grep -F "not a dynamic executable"

	! ldd `which skeletor-haskell`

	@echo
	@echo '=================================================='

.PHONY: install-static-skeletor-haskell

# NOTE « ldd »:
#
#      fails (i.e. the exit code is non-zero) on statically-linked executables.

# NOTE « ! ... »:
#
#      negates the exit code.

# NOTE in « ldd `which skeletor-haskell` | grep -F "not a dynamic executable"  »:
#
#      the exit code is 0 if-and-only-if skeletor-haskell is a statically-linked executable.

##################################################
# Documentation: building/copying/opening
##################################################

#------------------------------------------------#

docs-all: docs-markdown docs-haskell

.PHONY: docs-all

#------------------------------------------------#

markdown:
	$(Pandoc) README.md -o README.html

.PHONY: markdown

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
	find $(HaddockDirectory) -name "$(ModuleName).html" -print
	@echo '=================================================='
	find $(HaddockDirectory) -name "$(ModuleName).html" -exec $(Open) \{\} \; #TODO open with `&`

.PHONY: open-docs-haskell

##################################################
# Nix: parsing, evaluating, building.
##################################################

nix:

	@echo '=================================================='
	@echo

	time  $(NixBuild)  "./nix"  -A "static.skeletor-static"  -o "./result"

	@echo
	@echo '=================================================='

.PHONY: nix

#------------------------------------------------#

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
# Tarballs: zip them and unzip them.
##################################################

tarball-all: tarball 		# TODO

.PHONY: tarball-all

#------------------------------------------------#

tarball: build-tarball check-tarball
	@echo '=================================================='
	@echo '[Tarballs (.tar.gz)] SUCCESS ====================='
	@echo '=================================================='

.PHONY: tarball

#------------------------------------------------#

check-tarball: copy-tarball
	@echo '=================================================='
	find $(TarballDirectory) -name "*.tar.gz" -print0 -exec $(CheckTarball) \{\} \;
        # ^ verifies tarball, by unpacking it.
	find /tmp/$(Package) -name "*.tar.gz"

.PHONY: check-tarball

#------------------------------------------------#

copy-tarball: build-tarball
	@echo '=================================================='
	rm    -fr $(TarballDirectory)
	mkdir -p  $(TarballDirectory)
	find $(PackageDirectory) -name "*.tar.gz" -print0 -exec mv \{\} $(TarballDirectory) \;

.PHONY: copy-tarball

#------------------------------------------------#

build-tarball: build-default
	@echo '=================================================='
	(cd  $(PackageDirectory) && $(Cabal) sdist)

.PHONY: build-tarball

##################################################
# (Miscellaneous)
##################################################

#------------------------------------------------#

list-projects:

	@find -L projects/ -maxdepth 1 -type f

.PHONY: list-projects

#------------------------------------------------#

list-project-default:

	@find -L projects/default -type f

.PHONY: list-project-default

#------------------------------------------------#

clean:
	rm -rf "./dist/" "./dist-newstyle/" "./dist-newdante/" "./dist-ghcjs/"
	rm -f *.project.local .ghc*.environment.*
	rm -rf ./*/dist/ ./*/dist-newstyle/

.PHONY: clean

##################################################
# Installation: executables, tab-completion.
##################################################

install: #TODO
	$(Cabal) new-build "skeletor:exe:skeletor-haskell"
	source <($(Cabal) new-exec -- skeletor-haskell --bash-completion-script `cabal new-exec -- which skeletor-haskell`)

#TODO the « source » is run in a subshell, so it won't take for your terminal.

.PHONY: install

# source <(cabal new-exec -- /home/sboo/haskell/skeletor/dist-newstyle/build/x86_64-linux/ghc-8.6.3/skeletor-0.0.0/x/skeletor-haskell/build/skeletor-haskell/skeletor-haskell --bash-completion-script `which /home/sboo/haskell/skeletor/dist-newstyle/build/x86_64-linux/ghc-8.6.3/skeletor-0.0.0/x/skeletor-haskell/build/skeletor-haskell/skeletor-haskell`)

#------------------------------------------------#

##################################################
# Fetching: files, resources
##################################################

#------------------------------------------------#

fetch-stackage: fetch-stackage--cabal.config

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
# Development ####################################
##################################################

# developing this package...

#------------------------------------------------#

tags: etags ctags

.PHONY: tags

#------------------------------------------------#

etags:

	@echo '=================================================='
	@echo

	$(MakeETags)  "$(TagsDirectory)"  --output "$(ETagsFile)"

	@echo '=================================================='
	@echo

	@cat $(ETagsFile)

	@echo
	@echo '=================================================='

.PHONY: etags

#------------------------------------------------#

ctags:

	@echo '=================================================='
	@echo

	$(MakeCTags)  "$(TagsDirectory)"  --output "$(CTagsFile)"

	@echo '=================================================='
	@echo

	@cat $(CTagsFile)

	@echo
	@echo '=================================================='

.PHONY: ctags

#------------------------------------------------#

# install:
# 	$(CabalBuild) --prefix=$(InstallDirectory) $(ExecutableTarget)
# .PHONY: install

#------------------------------------------------#

################################################################################

##################################################
# Building #######################################
##################################################

#------------------------------------------------#

build:

	@echo "=================================================="
	@echo ""

	$(CabalBuild) $(CabalTargets)

	@echo ""
	@echo "=================================================="

.PHONY: build

#------------------------------------------------#

build-ghcjs:

	@echo "=================================================="
	@echo ""

#	$(Cabal) new-build --project-file="./cabal-ghcjs.project" $(CabalTargets)
	$(Cabal) new-build -w ghcjs $(CabalTargets)

	@echo ""
	@echo "=================================================="

.PHONY: build-ghcjs

#------------------------------------------------#

build-ghc-8.4:

	@echo "=================================================="
	@echo ""

	$(Cabal) new-build $(CabalTargets)

	@echo ""
	@echo "=================================================="

.PHONY: build-ghc-8.4

#------------------------------------------------#


#------------------------------------------------#

##################################################
# Testing ########################################
##################################################

#------------------------------------------------#

test:

	@echo "=================================================="
	@echo ""

	$(CabalTest) $(CabalTargets)

	@echo ""
	@echo "=================================================="

.PHONY: test

#------------------------------------------------#

bench:

	@echo "=================================================="
	@echo ""

	$(CabalBench) $(CabalTargets)

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

	$(CabalHaddock) $(CabalTargets)

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

	$(Cabal) new-build $(CabalOptions) $(CabalTargets)
	$(Cabal) new-sdist $(CabalOptions) $(CabalTargets)

.PHONY: sdist

#------------------------------------------------#

static:

	$(Cabal) new-build $(CabalOptions) --enable-executable-static $(CabalTargets)
	$(Cabal) new-sdist $(CabalOptions) $(CabalTargets)

.PHONY: static

#------------------------------------------------#

release:

	find $(ReleaseDirectory) -type f

.PHONY: release

#------------------------------------------------#

upload: sdist

	$(Cabal) upload

.PHONY: upload

#------------------------------------------------#

##################################################

################################################################################