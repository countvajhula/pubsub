# Determine this makefile's path.
# Be sure to place this BEFORE `include` directives, if any.
# Source: https://stackoverflow.com/a/27132934/323874
THIS_FILE := $(lastword $(MAKEFILE_LIST))

EMACS=emacs
CASK ?= cask

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
	                      -l "package-lint.el"  \
	                      --eval "(setq package-lint-main-file \"pubsub.el\")" \
	                      -f "package-lint-batch-and-exit"  \
	                      ${PROJECT_FILES}

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

.PHONY:	help lint checkdoc build clean install test cover-local cover-coveralls
