EMACS ?= emacs

EMACS_BATCH=${EMACS} -Q -batch

all: clean-elc compile test

compile: clean-elc
	${EMACS} -Q -L . -batch -f batch-byte-compile helix.el

clean-elc:
	rm -f *.elc

TEST_SELECTOR ?= t
test:
	@echo "---- Run unit tests"
	@${EMACS_BATCH} \
		 -l helix-core.el \
		 -l helix-multiple-cursors.el \
		 -l helix-jj.el \
		 -l helix.el \
		 -l helix-test.el \
		 --eval "(ert-run-tests-batch-and-exit '${TEST_SELECTOR})" \
		 && echo "OK"

FILES = helix-core.el helix-multiple-cursors.el helix-jj.el helix.el

CHECKDOC="(dolist (file '(${FILES})) \
	(checkdoc-file (symbol-name file)))"

checkdoc:
	@${EMACS_BATCH} --eval ${CHECKDOC}

lint: compile checkdoc

.PHONY:	all compile clean-elc test lint checkdoc
