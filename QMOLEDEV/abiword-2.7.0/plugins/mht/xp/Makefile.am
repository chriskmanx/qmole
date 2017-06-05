
noinst_LTLIBRARIES = libxp.la

AM_CPPFLAGS = \
	$(MHT_CFLAGS) \
	-DABI_BUILD_VERSION=\"$(VERSION)\"

conditional_src = \
	ie_imp_MHT.cpp \
	ie_imp_MHT.h \
	tidyImporter.cpp \
	tidyImporter.h \
	tidyReader.cpp \
	tidyReader.h

if ABI_XHTML_MHT
mht_src = \
	ie_imp_MHT.cpp \
	ie_imp_MHT.h
endif

if ABI_XHTML_TIDY
tidy_src = \
	tidyImporter.cpp \
	tidyImporter.h \
	tidyReader.cpp \
	tidyReader.h

endif

libxp_la_SOURCES = \
	$(mht_src) \
	$(tidy_src) \
	register.cpp

EXTRA_DIST = \
	$(conditional_src)

