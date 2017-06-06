INCLUDES= -I$(top_srcdir) -I$(top_builddir) -I$(top_builddir)/gconf \
	$(DEPENDENT_WITH_XML_CFLAGS) \
	$(DEPENDENT_ORBIT_CFLAGS) \
	-DGCONF_ENABLE_INTERNALS=1 -DG_LOG_DOMAIN=\"GConf-Backends\"

backenddir = $(pkglibdir)/$(MAJOR_VERSION)

if LDAP_SUPPORT
EVOLDAP_BACKEND = libgconfbackend-evoldap.la
endif

backend_LTLIBRARIES = libgconfbackend-xml.la libgconfbackend-oldxml.la $(EVOLDAP_BACKEND)

libgconfbackend_oldxml_la_SOURCES = \
	xml-cache.h		\
	xml-cache.c		\
	xml-dir.h		\
	xml-dir.c		\
	xml-entry.h		\
	xml-entry.c		\
	xml-backend.c

libgconfbackend_oldxml_la_LDFLAGS = -avoid-version -module -no-undefined
libgconfbackend_oldxml_la_LIBADD  = $(DEPENDENT_WITH_XML_LIBS) $(top_builddir)/gconf/libgconf-$(MAJOR_VERSION).la $(INTLLIBS)

libgconfbackend_xml_la_SOURCES = 	\
	markup-backend.c		\
	markup-tree.h			\
	markup-tree.c

libgconfbackend_xml_la_LDFLAGS = -avoid-version -module -no-undefined
libgconfbackend_xml_la_LIBADD  = $(DEPENDENT_LIBS) $(top_builddir)/gconf/libgconf-$(MAJOR_VERSION).la $(INTLLIBS)

noinst_PROGRAMS = xml-test

xml_test_SOURCES= xml-test.c
xml_test_LDADD = \
	$(DEPENDENT_WITH_XML_LIBS) \
	$(top_builddir)/gconf/libgconf-$(MAJOR_VERSION).la \
	libgconfbackend-oldxml.la

bin_PROGRAMS = gconf-merge-tree
gconf_merge_tree_SOURCES = gconf-merge-tree.c
gconf_merge_tree_LDADD = $(DEPENDENT_LIBS) $(top_builddir)/gconf/libgconf-$(MAJOR_VERSION).la

if LDAP_SUPPORT
libgconfbackend_evoldap_la_SOURCES = evoldap-backend.c
libgconfbackend_evoldap_la_LDFLAGS = -avoid-version -module -no-undefined
libgconfbackend_evoldap_la_LIBADD  = \
	$(DEPENDENT_WITH_XML_LIBS) \
	$(top_builddir)/gconf/libgconf-$(MAJOR_VERSION).la \
	$(INTLLIBS) \
	$(LDAP_LIBS)

backendconfdir   = $(sysconfdir)/gconf/2
backendconf_DATA = evoldap.conf

schemadir   = $(pkgdatadir)/schema
schema_DATA = evoldap.schema
endif

EXTRA_DIST =		\
	README.evoldap	\
	evoldap.conf	\
	evoldap.schema
