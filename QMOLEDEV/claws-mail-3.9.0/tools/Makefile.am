EXTRA_TOOLS = \
	OOo2claws-mail.pl \
	acroread2claws-mail.pl \
	calypso_convert.pl \
	claws-mail-compose-insert-files.pl \
	convert_mbox.pl \
	csv2addressbook.pl \
	eud2gc.py \
	filter_conv.pl \
	filter_conv_new.pl \
	fix_date.sh \
	freshmeat_search.pl \
	gif2xface.pl \
	google_msgid.pl \
	google_search.pl \
	kmail2claws-mail.pl \
	kmail2claws-mail_v2.pl \
	kmail-mailbox2claws-mail.pl \
	mairix.sh \
	mew2claws-mail.pl \
	multiwebsearch.pl \
	nautilus2claws-mail.sh \
	outlook2claws-mail.pl \
	popfile-link.sh \
	tb2claws-mail \
	tbird2claws.py \
	textviewer.pl \
	textviewer.sh \
	thunderbird-filters-convertor.pl \
	update-po \
	uudec \
	uuooffice \
	vcard2xml.py \
	kdeservicemenu/install.sh \
	kdeservicemenu/claws-mail-kdeservicemenu.pl

EXTRA_DIST = \
	README \
	ca-certificates.crt \
	multiwebsearch.conf \
	kdeservicemenu/README \
	kdeservicemenu/claws-mail-attach-files.desktop.template \
	kdeservicemenu/claws-mail-attach-files.desktop.kde4template \
	$(EXTRA_TOOLS)

MAKE_EXE = chmod u+x $(EXTRA_TOOLS)

all-local:
	if [ ! -d kdeservicemenu -a ! -e kdeservicemenu ]; then \
	mkdir kdeservicemenu; \
	fi; \
	for file in $(EXTRA_TOOLS); do \
	if [ ! -e ${top_builddir}/tools/$$file ]; then \
	todir=${top_builddir}/tools; \
	dir=$$(dirname $$file); \
	if [ ! $$dir = . ]; then \
	todir=$$todir/$$dir; \
	fi; \
	cp ${top_srcdir}/tools/$$file $$todir; \
	fi; \
	done;
	$(MAKE_EXE)

distclean-local:
	if [ ! ${top_builddir} = ${top_srcdir} ]; then \
	for file in $(EXTRA_TOOLS); do \
	rm -f $$file; \
	done; \
	if [ -d kdeservicemenu ]; then \
	rmdir --ignore-fail-on-non-empty kdeservicemenu; \
	fi; \
	fi

if PLATFORM_WIN32
install-data-local:
	mkdir -p ${pkgdatadir}
	cp ${top_srcdir}/tools/ca-certificates.crt ${pkgdatadir}/
endif
