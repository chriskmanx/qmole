
ACLOCAL_AMFLAGS = -I .

DIST_SUBDIRS = goffice-bits src plugins user

if WITH_GOFFICE_BUILTIN
SUBDIRS = goffice-bits
else 
SUBDIRS =
endif
SUBDIRS += src plugins user

if TOOLKIT_GTK
desktopdir = @ABIWORD_DATADIR@/applications
desktop_DATA = abiword.desktop

mimedir = @ABIWORD_DATADIR@/mime-info
mime_DATA = abiword.keys

pkgconfigdir = $(libdir)/pkgconfig
pkgconfig_DATA = abiword-$(ABIWORD_SERIES).pc
endif

icondir= @ABIWORD_DATADIR@/icons
icon_DATA = abiword_48.png

EXTRA_DIST = \
	AUTHORS \
	BiDiReadme.txt \
	BUILD.TXT \
	COPYING	\
	COPYRIGHT.TXT \
	INSTALL \
	NEWS \
	README \
	ChangeLog \
	abiword.desktop \
	abiword.keys \
	abiword_48.png \
	libabiword.pc.in

DISTCLEANFILES = \
	abiword-$(ABIWORD_SERIES).pc \
	plugin-builtin.m4 \
	plugin-configure.m4 \
	plugin-list.m4 \
	plugin-makefiles.m4 \
	doltcompile \
	doltlibtool

abiword-$(ABIWORD_SERIES).pc: libabiword.pc
	cp $< $@

if TOOLKIT_WIN
zip:
	rm -rf $(PACKAGE)-$(VERSION)
	make DESTDIR=$(abs_builddir)/zip install
	mv zip$(prefix) $(PACKAGE)-$(VERSION)
	cd $(PACKAGE)-$(VERSION)
	zip -r $(PACKAGE)-$(VERSION).zip $(PACKAGE)-$(VERSION) -x \*.a \*.la
	rm -rf $(PACKAGE)-$(VERSION) ./zip
endif

