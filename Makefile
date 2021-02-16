# https://nullprogram.com/blog/2020/01/22/ for a guide
.POSIX:
.SUFFIXES: .el .elc
EMACS	= emacs
EL   	= secretary.el
ELC  	= $(EL:.el=.elc)

compile: $(ELC)

check: secretary-test.elc
	$(EMACS) -Q --batch -L . -l secretary-test.elc -f ert-run-tests-batch

clean:
	rm -f $(ELC)

run: $(ELC)
    $(EMACS) -Q -L . -l secretary.elc -f secretary-mode

secretary-test.elc: $(ELC)

.el.elc:
	$(EMACS) -Q --batch -L . -f batch-byte-compile $<
