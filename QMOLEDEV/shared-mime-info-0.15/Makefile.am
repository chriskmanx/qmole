## Process this file with automake to produce Makefile.in

SUBDIRS=po

AUTOMAKE_OPTIONS = foreign

packagesdir = $(mimedir)/packages
packages_DATA = freedesktop.org.xml

bin_PROGRAMS = update-mime-database
update_mime_database_SOURCES = update-mime-database.c

man_MANS = update-mime-database.1

EXTRA_DIST = \
	shared-mime-info-spec.xml	\
	freedesktop.org.xml.in		\
	$(packages_DATA) 		\
	README				\
	$(man_MANS)			\
	intltool-extract.in		\
	intltool-merge.in		\
	intltool-update.in		\
	shared-mime-info.pc.in

CLEANFILES = intltool-extract intltool-merge intltool-update freedesktop.org.xml

CFLAGS = $(ALL_CFLAGS)
LDFLAGS = $(ALL_LIBS)

pkgconfigdir = $(libdir)/pkgconfig
pkgconfig_DATA = shared-mime-info.pc

$(pkgconfig_DATA): config.status

@INTLTOOL_XML_RULE@
@INTLTOOL_DESKTOP_RULE@

install-data-hook: 
if ENABLE_UPDATE_MIMEDB
	$(DESTDIR)"$(bindir)/update-mime-database" "$(DESTDIR)${mimedir}"
endif

uninstall-hook:
	for media in text application image audio inode video message model multipart; do rm -f "$(DESTDIR)${mimedir}/$${media}/"*.xml; done
	rm -f "$(DESTDIR)${mimedir}/globs"
	rm -f "$(DESTDIR)${mimedir}/magic"
	rm -f "$(DESTDIR)${mimedir}/XMLnamespaces"

check:
	xmllint --noout --valid $(srcdir)/freedesktop.org.xml

