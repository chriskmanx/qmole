
# Before making a release, the LTVERSION string should be modified.
# The string is of the form C:R:A.
# - If interfaces have been changed or added, but binary compatibility has
#   been preserved, change to C+1:0:A+1
# - If binary compatibility has been broken (eg removed or changed interfaces)
#   change to C+1:0:0
# - If the interface is the same as the previous version, change to C:R+1:A

LTVERSION = 0:7:0

if OS_WIN32
export_symbols = -export-symbols glade.def

install-libtool-import-lib:
	$(INSTALL) .libs/libglade-2.0.dll.a $(DESTDIR)$(libdir)

uninstall-libtool-import-lib:
	-rm $(DESTDIR)$(libdir)/libglade-2.0.dll.a
else

export_symbols = -export-symbols-regex "^[^_].*"

install-libtool-import-lib:
uninstall-libtool-import-lib:
endif

INCLUDES = \
  -DG_LOG_DOMAIN=\"libglade\" \
  -I$(top_srcdir) \
  $(LIBGLADE_CFLAGS) \
  -DGLADE_LIBDIR=\""$(libdir)"\" \
  -DGLADE_PREFIX=\""$(prefix)"\" \
  -DG_DISABLE_DEPRECATED \
  -DGDK_DISABLE_DEPRECATED \
  -DGDK_PIXBUF_DISABLE_DEPRECATED \
  -DGNOME_DISABLE_DEPRECATED

glademoduledir = $(libdir)/libglade/2.0

lib_LTLIBRARIES = libglade-2.0.la

gladeincludedir      = $(includedir)/libglade-2.0/glade
gladeinclude_HEADERS = \
  glade.h \
  glade-init.h \
  glade-xml.h \
  glade-build.h \
  glade-parser.h

libglade_2_0_la_SOURCES  = \
  glade-init.c \
  glade-xml.c \
  glade-parser.c \
  glade-gtk.c \
  glade-private.h

libglade_2_0_la_LIBADD   = $(LIBGLADE_LIBS)
libglade_2_0_la_LDFLAGS  = -version-info $(LTVERSION) $(export_symbols) -no-undefined

EXTRA_DIST = $(gladeinclude_HEADERS) \
	glade.def

install-data-local: install-libtool-import-lib

uninstall-local: uninstall-libtool-import-lib
