
EFENCE=

INCLUDES = -I$(top_srcdir) -I$(top_builddir) \
	 $(DEPENDENT_CFLAGS) \
	 -DG_LOG_DOMAIN=\"GConf-Tests\" -DGCONF_ENABLE_INTERNALS=1

noinst_PROGRAMS=testgconf testlisteners testschemas testchangeset testencode testunique testpersistence testdirlist testaddress testbackend

TESTLIBS= $(INTLLIBS) $(DEPENDENT_LIBS) $(top_builddir)/gconf/libgconf-$(MAJOR_VERSION).la  $(EFENCE)

testunique_SOURCES=testunique.c

testunique_LDADD = $(TESTLIBS)

testgconf_SOURCES=testgconf.c

testgconf_LDADD = $(TESTLIBS)

testpersistence_SOURCES=testpersistence.c

testpersistence_LDADD = $(TESTLIBS)

testlisteners_SOURCES=testlisteners.c

testlisteners_LDADD = $(TESTLIBS)

testschemas_SOURCES=testschemas.c

testschemas_LDADD = $(TESTLIBS)

testchangeset_SOURCES=testchangeset.c

testchangeset_LDADD = $(TESTLIBS)

testencode_SOURCES=testencode.c

testencode_LDADD = $(TESTLIBS)

testdirlist_SOURCES=testdirlist.c

testdirlist_LDADD = $(TESTLIBS)


testaddress_SOURCES=testaddress.c

testaddress_LDADD = $(TESTLIBS)

testbackend_SOURCES=testbackend.c

testbackend_LDADD = $(TESTLIBS)









