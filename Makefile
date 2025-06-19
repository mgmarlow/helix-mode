EMACS ?= emacs

EMACS_BATCH=${EMACS} -Q -batch

all: clean-elc compile test

compile: clean-elc
	${EMACS} -Q -L . -batch -f batch-byte-compile *.el

clean-elc:
	rm -f *.elc

TEST_SELECTOR ?= t
test:
	@echo "---- Run unit tests"
	@${EMACS_BATCH} \
		 -l helix-multiple-cursors.el \
		 -l helix-jj.el \
		 -l helix.el \
		 -l helix-test.el \
		 --eval "(ert-run-tests-batch-and-exit '${TEST_SELECTOR})" \
		 && echo "OK"

.PHONY:	all compile clean-elc test
