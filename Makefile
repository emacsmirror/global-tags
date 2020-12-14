# taken from https://github.com/abo-abo/tiny/blob/master/Makefile
# see http://sachachua.com/blog/2015/02/continuous-integration-code-coverage-emacs-packages-travis-coveralls/
EMACS ?= emacs
BEMACS = $(EMACS) -Q -batch

all: bytec test

# Use LC_ALL=C to avoid locale dependencies in the dates!
test:
	LC_ALL=C $(BEMACS) \
              -l tests/setup.el \
              -l global-tags.el \
              -l tests/global-tags-tests.el \
              -f buttercup-run
bytec:
	LC_ALL=C $(BEMACS) \
	    --eval '(setq byte-compile-error-on-warn t)' \
	    --eval '(byte-recompile-directory "./")'


.PHONY:	all test
