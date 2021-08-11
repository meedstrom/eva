# for a guide: https://nullprogram.com/blog/2020/01/22/
.POSIX:
.SUFFIXES: .el .elc
EMACS	= emacs
EL   	= secretary.el secretary-config.el secretary-builtin.el secretary-activity.el
ELC  	= $(EL:.el=.elc)
LDFLAGS = -L deps/ts -L deps/dash -L deps/s -L deps/ess/lisp -L deps/f -L deps/named-timer -L deps/pfuture -L deps/transient/lisp

# default make action just to give us static analysis for now
compile-and-clean: $(ELC)
	rm -f $(ELC)

# $ELC expands to .elc versions of all files listed in $EL.
compile: $(ELC)

check: secretary-tests.elc
	$(EMACS) -Q -L . $(LDFLAGS) --batch -l secretary-tests.elc -f ert-run-tests-batch

test: check

clean:
	rm -f $(ELC)

# Dependencies
# State that secretary-test depends on all other elc files being built first.
secretary-tests.elc:  $(ELC)
secretary-config.elc: secretary.elc

# Tell make how to compile an .el into an .elc.
.el.elc:
	$(EMACS) -Q -L . $(LDFLAGS) --batch -f batch-byte-compile $<

# Clone the dependencies of this package:
# mkdir deps
# git clone --depth=1 https://github.com/alphapapa/ts.el deps/ts
# git clone --depth=1 https://github.com/magnars/dash.el deps/dash
# git clone --depth=1 https://github.com/magnars/s.el deps/s
# git clone --depth=1 https://github.com/emacs-ess/ess deps/ess
# git clone --depth=1 https://github.com/rejeep/f.el  deps/f
# git clone --depth=1 https://github.com/DarwinAwardWinner/emacs-named-timer  deps/named-timer
# git clone --depth=1 https://github.com/Alexander-Miller/pfuture  deps/pfuture
# git clone --depth=1 https://github.com/magit/transient deps/transient
# git clone --depth=1 https://github.com/ledger/ledger-mode deps/ledger-mode

# Not (yet) needed at compilation
# git clone --depth=1 https://github.com/ch11ng/exwm deps/exwm
# git clone --depth=1 https://github.com/bastibe/org-journal deps/org-journal
