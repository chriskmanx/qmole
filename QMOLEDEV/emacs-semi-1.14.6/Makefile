#
# Makefile for SEMI kernel.
#

PACKAGE = semi
API	= 1.14
RELEASE = 6

FLIM_API= 1.14

TAR	= tar
RM	= /bin/rm -f
CP	= /bin/cp -p

EMACS	= emacs
XEMACS	= xemacs
FLAGS   = -batch -q -no-site-file -l SEMI-MK

PREFIX	= NONE
LISPDIR = NONE
PACKAGEDIR = NONE
VERSION_SPECIFIC_LISPDIR = NONE

GOMI	= *.elc

VERSION	= $(API).$(RELEASE)
ARC_DIR_PREFIX = /home/kanji/tomo/public_html/comp/emacsen/lisp
ARC_DIR = $(ARC_DIR_PREFIX)/semi/semi-$(API)-for-flim-$(FLIM_API)


elc:
	$(EMACS) $(FLAGS) -f compile-semi \
		$(PREFIX) $(LISPDIR) $(VERSION_SPECIFIC_LISPDIR)

install-elc:	elc
	$(EMACS) $(FLAGS) -f install-semi \
		$(PREFIX) $(LISPDIR) $(VERSION_SPECIFIC_LISPDIR)

install:	install-elc


package:
	$(XEMACS) $(FLAGS) -f compile-semi-package $(PACKAGEDIR)

install-package:	package
	$(XEMACS) $(FLAGS) -f install-semi-package $(PACKAGEDIR)


clean:
	-$(RM) $(GOMI)


tar:
	cvs commit
	sh -c 'cvs tag -R $(PACKAGE)-`echo $(VERSION) | tr . _`; \
	cd /tmp; \
	cvs -d :pserver:anonymous@cvs.m17n.org:/cvs/root \
		export -d $(PACKAGE)-$(VERSION) \
		-r $(PACKAGE)-`echo $(VERSION) | tr . _` \
		semi'
	$(RM) /tmp/$(PACKAGE)-$(VERSION)/ftp.in
	cd /tmp; $(TAR) cvzf $(PACKAGE)-$(VERSION).tar.gz $(PACKAGE)-$(VERSION)
	cd /tmp; $(RM) -r $(PACKAGE)-$(VERSION)
	sed "s/VERSION/$(VERSION)/" < ftp.in | sed "s/API/$(API)/" \
		| sed "s/PACKAGE/$(PACKAGE)/" \
		| sed "s/FLIM_API/$(FLIM_API)/" > ftp

release:
	-$(RM) $(ARC_DIR)/$(PACKAGE)-$(VERSION).tar.gz
	mv /tmp/$(PACKAGE)-$(VERSION).tar.gz $(ARC_DIR)
