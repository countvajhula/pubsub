EMACS=emacs

# For a single-package repo, CI_PROJECT is optional.
# CI_PACKAGES contains the single package name.
export CI_PACKAGES=pubsub

help:
	@echo "Run common development actions."
	@echo
	@echo "setup-ci         - Clone the emacs-ci repo."
	@echo "bootstrap        - Install Straight.el."
	@echo "install          - Install package dependencies."
	@echo "byte-compile     - Byte compile the package."
	@echo "native-compile   - Native compile the package."
	@echo "lint             - Check style with package-lint."
	@echo "checkdoc         - Check docstrings."
	@echo "test             - Run ERT tests."
	@echo "cover-local      - Run tests and generate a local coverage report."
	@echo "cover-coveralls  - Run tests and report coverage to Coveralls (for CI)."

setup-ci:
	@if [ -d ".emacs-ci" ]; then \
		echo "--> Updating existing emacs-ci repository..."; \
		cd .emacs-ci && git pull; \
	else \
		echo "--> Cloning emacs-ci repository..."; \
		git clone https://github.com/countvajhula/emacs-ci.git .emacs-ci; \
	fi

clean:
	cd .emacs-ci && rm -rf init

bootstrap:
	cd .emacs-ci && emacs --batch --quick --load bootstrap.el

install:
	cd .emacs-ci && emacs --batch --quick --load install.el

byte-compile:
	cd .emacs-ci && emacs --batch --quick --load byte-compile.el

native-compile:
	cd .emacs-ci && emacs --batch --quick --load native-compile.el

lint:
	cd .emacs-ci && emacs --batch --quick --load lint.el

checkdoc:
	cd .emacs-ci && emacs --batch --quick --load checkdoc.el

test: byte-compile
	cd .emacs-ci && emacs --batch --quick --load test.el

cover-local: install
	# Ensure the target directory exists at the project root.
	mkdir -p coverage
	# Export the variables to ensure they are reliably passed to the subprocess.
	# Force undercover to run
	export UNDERCOVER_FORCE=true && \
	export UNDERCOVER_CONFIG='("*.el" (:report-file "coverage/local-report.json") (:report-format text) (:send-report nil))' && \
	cd .emacs-ci && emacs --batch --quick --load coverage.el

cover-coveralls: install
	cd .emacs-ci && emacs --batch --quick --load coverage.el

.PHONY: help setup-ci clean bootstrap install byte-compile native-compile lint checkdoc test cover-local cover-coveralls
