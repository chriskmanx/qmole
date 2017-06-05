## Process this file with automake to produce Makefile.in

###############################################################################
##                                                                           ##
## Copyright (c) 2001 Morgan Stanley Dean Witter & Co. All rights reserved.     ##
## See .../src/LICENSE for terms of distribution.                           ##
##                                                                           ##
##                                                                           ##
###############################################################################

EXTRA_DIST = version.h.template

CXXLD = $(CXX)

bin_PROGRAMS = aplus

aplus_SOURCES = \
	mainC.C \
	aplus_main.c \
	aplus_uext.c \
	matherr.c \
	version.h

CFLAGS = @CFLAGS@ -DBSTUB

INCLUDES = -I.. $(X_INCLUDES)

aplus_LDADD = \
	$(top_srcdir)/src/IPC/libIPC.la \
	$(top_srcdir)/src/cxs/libcxs.la \
	$(top_srcdir)/src/cxb/libcxb.la \
	$(top_srcdir)/src/AplusGUI/libAplusGUI.la \
	$(top_srcdir)/src/cxsys/libcxsys.la \
	$(top_srcdir)/src/cxc/libcxc.la \
	$(top_srcdir)/src/esf/libesf.la \
	$(top_srcdir)/src/a/liba.la \
	$(top_srcdir)/src/dap/libdap.la \
	$(top_srcdir)/src/MSGUI/libMSGUI.la \
	$(top_srcdir)/src/MSIPC/libMSIPC.la \
	$(top_srcdir)/src/MSTypes/libMSTypes.la 

aplus_LDFLAGS = \
	-L$(top_srcdir)/src/IPC \
	-L$(top_srcdir)/src/cxs \
	-L$(top_srcdir)/src/cxb \
	-L$(top_srcdir)/src/AplusGUI \
	-L$(top_srcdir)/src/cxsys \
	-L$(top_srcdir)/src/cxc \
	-L$(top_srcdir)/src/esf \
	-L$(top_srcdir)/src/a \
	-L$(top_srcdir)/src/dap \
	-L$(top_srcdir)/src/MSGUI \
	-L$(top_srcdir)/src/MSIPC \
	-L$(top_srcdir)/src/MSTypes \
	$(X_LIBS) $(LIBS)

install-exec-hook:
	mv $(DESTDIR)$(bindir)/aplus $(DESTDIR)$(bindir)/a+

# Additional Targets

aplus_main.c: version.h

version.h: FORCE
	echo "Making $@";\
	rm -f $@;\
	cat version.h.template | sed -e s/%RCVAL%/$(VERSION)/ \
		-e s/%FNVAL%/$(OWNER_FULLNAME)/ \
		-e s@%IMDIRVAL%@$(prefix)@ >$@;

FORCE:
