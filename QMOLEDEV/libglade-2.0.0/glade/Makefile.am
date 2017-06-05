
# Before making a release, the LTVERSION string should be modified.
# The string is of the form C:R:A.
# - If interfaces have been changed or added, but binary compatibility has
#   been preserved, change to C+1:0:A+1
# - If binary compatibility has been broken (eg removed or changed interfaces)
#   change to C+1:0:0
# - If the interface is the same as the previous version, change to C:R+1:A

LTVERSION = 0:0:0

INCLUDES = \
	-DG_LOG_DOMAIN=\"libglade\"	\
	-I$(top_srcdir)			\
	$(LIBGLADE_CFLAGS) 		\
	-DGLADE_LIBDIR=\""$(libdir)"\"	\
	-DG_DISABLE_DEPRECATED		\
	-DGDK_DISABLE_DEPRECATED	\
	-DGDK_PIXBUF_DISABLE_DEPRECATED	\
	-DGTK_DISABLE_DEPRECATED	\
	-DGNOME_DISABLE_DEPRECATED

glademoduledir = $(libdir)/libglade/2.0

lib_LTLIBRARIES = libglade-2.0.la

gladeincludedir      = $(includedir)/libglade-2.0/glade
gladeinclude_HEADERS = \
  glade.h 	\
  glade-init.h	\
  glade-xml.h	\
  glade-build.h	\
  glade-parser.h

libglade_2_0_la_SOURCES  = \
  glade-init.c 	 \
  glade-xml.c 	 \
  glade-parser.c \
  glade-gtk.c 	 \
  glade-private.h \
  $(gladeinclude_HEADERS)

libglade_2_0_la_LIBADD   = $(LIBGLADE_LIBS)
libglade_2_0_la_LDFLAGS  = -version-info $(LTVERSION)

EXTRA_DIST = $(gladeinclude_HEADERS) \
	makefile.mingw	\
	glade.def
