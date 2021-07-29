# for a guide: https://nullprogram.com/blog/2020/01/22/
.POSIX:
.SUFFIXES: .el .elc
EMACS	= emacs
EL   	= secretary.el
ELC  	= $(EL:.el=.elc)

compile: $(ELC)

check: secretary-tests.elc
	$(EMACS) -Q --batch -L . -l secretary-tests.elc -f ert-run-tests-batch

clean:
	rm -f $(ELC)

run: $(ELC)
    $(EMACS) -Q -L . -l secretary.elc -f secretary-mode

secretary-test.elc: $(ELC)

.el.elc:
	$(EMACS) -Q --batch -L . -f batch-byte-compile $<
