# taken from https://github.com/abo-abo/tiny/blob/master/Makefile
# see http://sachachua.com/blog/2015/02/continuous-integration-code-coverage-emacs-packages-travis-coveralls/
EMACS ?= emacs
BEMACS = $(EMACS) -Q -batch

all: test

# Use LC_ALL=C to avoid locale dependencies in the dates!
test: clean
	LC_ALL=C $(BEMACS) -l tests/setup.el \
	       -l global.el \
	       -l tests/global-tests.el \
	       -f buttercup-run-discover

compile:
	$(EMACS) -Q -batch -f batch-byte-compile counsel-gtags.el

clean:
	rm -f f.elc

.PHONY:	all test
