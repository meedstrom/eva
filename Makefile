# https://nullprogram.com/blog/2020/01/22/ for a guide
.POSIX:
.SUFFIXES: .el .elc
EMACS	= emacs
EL   	= secretary.el secretary-nlp.el secretary-modeler.el \
          secretary-data-collector.el secretary-presenter.el \
          secretary-common.el secretary-test.el
ELC  	= $(EL:.el=.elc)

compile: $(ELC)

check: secretary-test.elc
	$(EMACS) -Q --batch -L . -l secretary-test.elc -f ert-run-tests-batch

clean:
	rm -f $(ELC)

run: $(ELC)
    $(EMACS) -Q -L . -l foo.elc -f foo-mode

secretary.elc: secretary-common.elc secretary-nlp.elc secretary-modeler.elc \
               secretary-data-collector.elc secretary-presenter.elc
secretary-nlp.elc: secretary-common.elc
secretary-data-collector.elc: secretary-common.elc
secretary-modeler.elc: secretary-common.elc
secretary-presenter.elc: secretary-common.elc
secretary-test.elc: $(ELC)

.el.elc:
	$(EMACS) -Q --batch -L . -f batch-byte-compile $<
