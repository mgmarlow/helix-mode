EMACS ?= emacs

FILES = helix-core.el helix-multiple-cursors.el helix-jj.el helix.el

DEPS = package-lint

INIT_PACKAGES="(progn \
  (require 'package) \
  (push '(\"melpa\" . \"https://melpa.org/packages/\") package-archives) \
  (package-initialize) \
  (dolist (pkg '(${DEPS})) \
    (unless (package-installed-p pkg) \
      (unless (assoc pkg package-archive-contents) \
	(package-refresh-contents)) \
      (package-install pkg))) \
  (unless package-archive-contents (package-refresh-contents)) \
  )"

EMACS_BATCH=${EMACS} -Q -batch --eval ${INIT_PACKAGES}

all: clean-elc compile test

compile: clean-elc
	${EMACS} -Q -L . -batch -f batch-byte-compile ${FILES}

clean-elc:
	rm -f *.elc

clean: clean-elc

TEST_SELECTOR ?= t
test:
	@echo "---- Run unit tests"
	@${EMACS_BATCH} \
		$(addprefix -l ,$(FILES)) \
		-l helix-test.el \
		--eval "(ert-run-tests-batch-and-exit '${TEST_SELECTOR})" \
		&& echo "OK"

CHECKDOC="(dolist (file '(${FILES})) \
	(checkdoc-file (symbol-name file)))"

checkdoc:
	@${EMACS_BATCH} --eval ${CHECKDOC}

package-lint:
	@${EMACS_BATCH} --eval "(setq package-lint-main-file \"helix.el\")" -f package-lint-batch-and-exit ${FILES}

lint: compile checkdoc package-lint

.PHONY:	all compile clean-elc test lint checkdoc
