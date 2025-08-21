# Determine this makefile's path.
# Be sure to place this BEFORE `include` directives, if any.
# Source: https://stackoverflow.com/a/27132934/323874
THIS_FILE := $(lastword $(MAKEFILE_LIST))

EMACS=emacs
CASK ?= cask

INIT_PACKAGE_EL="(progn  \
  (require 'package)  \
  (push '(\"melpa\" . \"http://melpa.org/packages/\") package-archives)  \
  (package-initialize)  \
  (unless package-archive-contents \
     (package-refresh-contents)))"

PROJECT_FILES=`${CASK} files`

help:
	@echo "clean - remove all build artifacts"
	@echo "install - install package dependencies in .cask/"
	@echo "lint - check style with package-lint"
	@echo "lint+less - lint piped to less"
	@echo "lint-no-noise - lint with typically noisy warnings filtered out"
	@echo "checkdoc - check docstrings"
	@echo "build - byte compile the package"
	@echo "test - run tests"

clean :
	${CASK} clean-elc

install:
	${CASK} install

lint:
	${CASK} exec $(EMACS) -Q --batch  \
	                      --eval $(INIT_PACKAGE_EL)  \
	                      -l "package-lint.el"  \
	                      -f "package-lint-batch-and-exit"  \
	                      ${PROJECT_FILES}

lint+less:
	@$(MAKE) -f $(THIS_FILE) lint 2>&1 | less

lint-no-noise:
	@$(MAKE) -f $(THIS_FILE) lint 2>&1 | grep -v "start with.*prefix" |grep -v "lexical-binding" |grep -v "non-snapshot.*racket" |grep -v "non-snapshot.*clever" |grep -v "Version.*header is missing" |grep -v "Package-Version"

checkdoc:
	${CASK} exec $(EMACS) -Q --batch  \
	                      -l "dev/build-utils.el"  \
	                      --eval '(flycheck/batch-checkdoc ".")'

build :
	${CASK} build

test: build
	${CASK} exec ert-runner

cover-coveralls:
	${CASK} exec ert-runner -l test/undercover-helper.el

cover-local:
	UNDERCOVER_FORCE=true UNDERCOVER_CONFIG='("*.el" (:report-file "coverage/local-report.json") (:report-format text) (:send-report nil))' ${CASK} exec ert-runner

.PHONY:	help lint lint+less lint-no-noise checkdoc build clean install test cover-local cover-coveralls
