EMACS ?= emacs

.PHONY: test compile clean
test:
	$(EMACS) --batch -L . -l test/jb-kotlin-test.el -f ert-run-tests-batch-and-exit

compile:
	$(EMACS) --batch -L . --eval '(setq byte-compile-error-on-warn t)' -f batch-byte-compile jb-kotlin-navigation.el jb-kotlin-reload.el jb-kotlin-lsp-dap.el

clean:
	rm -f jb-kotlin-lsp-dap.elc jb-kotlin-navigation.elc jb-kotlin-reload.elc
