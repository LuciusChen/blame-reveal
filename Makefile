EMACS ?= emacs
EMACS_BATCH = $(EMACS) -Q --batch --eval "(setq load-prefer-newer t)" -L .

.PHONY: test compile

test:
	$(EMACS_BATCH) -l test/blame-reveal-test.el -f ert-run-tests-batch-and-exit

compile:
	$(EMACS_BATCH) -f batch-byte-compile blame-reveal*.el
