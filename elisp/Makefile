SRCS = hhp.el hhp-func.el hhp-doc.el hhp-comp.el hhp-check.el hhp-process.el \
       hhp-command.el hhp-info.el hhp-ins-mod.el hhp-indent.el
EMACS = emacs
DETECT = xemacs

TEMPFILE  = temp.el

all: $(TEMPFILE) hhp.el
	$(EMACS) -batch -q -no-site-file -l ./$(TEMPFILE) -f hhp-compile
	rm -f $(TEMPFILE)

detect: $(TEMPFILE) hhp.el
	$(EMACS) -batch -q -no-site-file -l ./$(TEMPFILE) -f hhp-compile
	rm -f $(DETECT)

$(TEMPFILE):
	@echo '(setq load-path (cons "." load-path))' >> $(TEMPFILE)
	@echo '(defun hhp-compile () (mapcar (lambda (x) (byte-compile-file x)) (list ' >> $(TEMPFILE)
	@echo $(SRCS)| sed -e 's/\(hhp[^ ]*\.el\)/"\1"/g' >> $(TEMPFILE)
	@echo ')))' >> $(TEMPFILE)

clean:
	rm -f *.elc $(TEMPFILE)

VERSION = `grep version hhp.el | sed -e 's/[^0-9\.]//g'`

bump:
	echo "(define-package\n  \"hhp\"\n  $(VERSION)\n  \"Sub mode for Haskell mode\"\n  nil)" > hhp-pkg.el

archive:
	git archive master -o ~/hhp-$(VERSION).tar --prefix=hhp-$(VERSION)/
