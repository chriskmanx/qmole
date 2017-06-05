AUTOMAKE_OPTIONS = 1.7

PLATFORM_VERSION = 2.0

CLEANFILES =
EXTRA_DIST = 
INCLUDES = \
	$(PYTHON_INCLUDES) \
	$(PYGOBJECT_CFLAGS) \
	-I$(srcdir)/gtk

# defs files
defsdir = $(pkgdatadir)/$(PLATFORM_VERSION)/defs
defs_DATA =

# python scripts & extension modules
pkgpythondir = $(pyexecdir)/gtk-2.0
pkgpyexecdir = $(pyexecdir)/gtk-2.0

# gtk python extension modules
pygtkexecdir = $(pkgpyexecdir)/gtk
pygtkexec_LTLIBRARIES =

# gtk python scripts
pygtkdir = $(pkgpythondir)/gtk
pygtk_PYTHON = 			\
	__init__.py 		\
	_lazyutils.py		\
	compat.py		\
	deprecation.py		\
	keysyms.py

# gtk headers
pkgincludedir = $(includedir)/pygtk-$(PLATFORM_VERSION)/pygtk
pkginclude_HEADERS = pygtk.h

# linker flags
common_ldflags = -module -avoid-version
if PLATFORM_WIN32
common_ldflags += -no-undefined
endif

# gtk module
GTK_DEFS = gtk-base.defs
GDK_DEFS = gdk-base.defs
GTKUNIXPRINT_DEFS = gtkunixprint-base.defs
GTK_TYPES_DEFS = gtk-base-types.defs
GDK_TYPES_DEFS = gdk-base-types.defs
GTKUNIXPRINT_TYPES_DEFS = gtkunixprint-base-types.defs

GTK_2_10_DEFS = gdk-2.10.defs gtk-2.10.defs gtk-2.10-types.defs

GTK_2_12_DEFS =
	gdk-2.12.defs		\
	gtk-2.12.defs		\
	gtk-2.12-types.defs	\
	gtkunixprint-2.12.defs

GTK_2_14_DEFS =
	gdk-2.14.defs		\
	gtk-2.14.defs		\
	gtk-2.14-types.defs 	\
	gtkunixprint-2.14.defs

GTK_2_16_DEFS =
	gdk-2.16.defs		\
	gtk-2.16.defs		\
	gtk-2.16-types.defs

GTK_2_18_DEFS =
	gdk-2.18.defs		\
	gtk-2.18.defs		\
	gtk-2.18-types.defs

GTK_2_20_DEFS =
	gdk-2.20.defs		\
	gtk-2.20.defs		\
	gtk-2.20-types.defs	\
	gtkunixprint-2.20.defs

GTK_2_22_DEFS =
	gdk-2.22.defs		\
	gtk-2.22.defs		\
	gtk-2.22-types.defs

GTK_2_24_DEFS =
	gdk-2.24.defs		\
	gtk-2.24.defs		\
	gtk-2.24-types.defs

if HAVE_GTK_2_10
GDK_DEFS += gdk-2.10.defs 
GTK_DEFS += gtk-2.10.defs 
GTK_TYPES_DEFS += gtk-2.10-types.defs
endif

if HAVE_GTK_2_12
GDK_DEFS += gdk-2.12.defs 
GTK_DEFS += gtk-2.12.defs 
GTK_TYPES_DEFS += gtk-2.12-types.defs
GTKUNIXPRINT_DEFS += gtkunixprint-2.12.defs
endif


if HAVE_GTK_2_14
GDK_DEFS += gdk-2.14.defs 
GTK_DEFS += gtk-2.14.defs 
GTK_TYPES_DEFS += gtk-2.14-types.defs
GTKUNIXPRINT_DEFS += gtkunixprint-2.14.defs
endif

if HAVE_GTK_2_16
GDK_DEFS += gdk-2.16.defs 
GTK_DEFS += gtk-2.16.defs 
GTK_TYPES_DEFS += gtk-2.16-types.defs
endif

if HAVE_GTK_2_18
GDK_DEFS += gdk-2.18.defs 
GTK_DEFS += gtk-2.18.defs 
GTK_TYPES_DEFS += gtk-2.18-types.defs
endif

if HAVE_GTK_2_20
GDK_DEFS += gdk-2.20.defs 
GTK_DEFS += gtk-2.20.defs 
GTK_TYPES_DEFS += gtk-2.20-types.defs
endif

if HAVE_GTK_2_22
GDK_DEFS += gdk-2.22.defs
GTK_DEFS += gtk-2.22.defs
GTK_TYPES_DEFS += gtk-2.22-types.defs
endif

if HAVE_GTK_2_24
GDK_DEFS += gdk-2.24.defs
GTK_DEFS += gtk-2.24.defs
GTK_TYPES_DEFS += gtk-2.24-types.defs
endif

CREATEDEFS = $(PYTHON) $(CODEGENDIR)/createdefs.py

GDK_OVERRIDES = \
	gdk.override 		\
	gdkcairo.override	\
	gdkcolor.override	\
	gdkdraw.override	\
	gdkevent.override	\
	gdkgc.override		\
	gdkpixbuf.override	\
	gdkrectangle.override	\
	gdkwindow.override

GTK_OVERRIDES = 		\
	gtk.override 		\
	gtkborder.override	\
	gtkbuilder.override 	\
	gtkclist.override 	\
	gtkcontainer.override 	\
	gtkctree.override 	\
	gtkiconview.override 	\
	gtkrcstyle.override 	\
	gtkstatusbar.override	\
	gtkstyle.override 	\
	gtktextview.override 	\
	gtktoolbar.override 	\
	gtktreeview.override 	\
	gtkwidget.override 	\
	gtkwindow.override

CLEANFILES +=			\
	gtk.c			\
	gdk.c			\
	gtk.defs		\
	gtk-types.defs		\
	gdk.defs		\
	gdk-types.defs		\
	gen-gtk.c

EXTRA_DIST += 			\
	gtk-extrafuncs.defs	\
	$(GDK_DEFS) 		\
	$(GDK_TYPES_DEFS) 	\
	$(GDK_OVERRIDES) 	\
	$(GTK_DEFS) 		\
	$(GTK_TYPES_DEFS)	\
	$(GTK_2_10_DEFS)	\
	$(GTK_2_12_DEFS)	\
	$(GTK_2_14_DEFS)	\
	$(GTK_2_16_DEFS)	\
	$(GTK_2_18_DEFS)	\
	$(GTK_2_20_DEFS)	\
	$(GTK_2_22_DEFS)	\
	$(GTK_2_24_DEFS)	\
	$(GTK_OVERRIDES)


gdk.defs: $(GDK_DEFS) Makefile
	$(CREATEDEFS) $@ $(GDK_DEFS)

gtk.defs: $(GTK_DEFS) Makefile
	$(CREATEDEFS) $@ $(GTK_DEFS)

gdk-types.defs: $(GDK_TYPES_DEFS) Makefile
	$(CREATEDEFS) $@ $(GDK_TYPES_DEFS)

gtk-types.defs: $(GTK_TYPES_DEFS) Makefile
	$(CREATEDEFS) $@ $(GTK_TYPES_DEFS)

gtkunixprint.defs: $(GTKUNIXPRINT_DEFS) Makefile
	$(CREATEDEFS) $@ $(GTKUNIXPRINT_DEFS)

gtkunixprint-types.defs: $(GTKUNIXPRINT_TYPES_DEFS) Makefile
	$(CREATEDEFS) $@ $(GTKUNIXPRINT_TYPES_DEFS)

gdk.c: gdk-types.defs gdk.defs $(GDK_OVERRIDES)
gtk.c: gdk-types.defs gtk-types.defs gtk.defs gdk-types.defs $(GTK_OVERRIDES)
_gtk_la_CFLAGS = $(PYCAIRO_CFLAGS) $(GTK_CFLAGS)
_gtk_la_LDFLAGS = $(common_ldflags) -export-symbols-regex init_gtk
_gtk_la_LIBADD = $(GTK_LIBS)
_gtk_la_SOURCES = 		\
	gtk-types.c 		\
	gtkmodule.c 		\
	gtkobject-support.c 	\
	pygtk.h 		\
	pygtk-private.h 	\
	pygtkcellrenderer.h 	\
	pygtkcellrenderer.c	\
	pygtktreemodel.h 	\
	pygtktreemodel.c
nodist__gtk_la_SOURCES = gtk.c gdk.c
if BUILD_GTK
pygtkexec_LTLIBRARIES += _gtk.la
defs_DATA += $(GDK_DEFS) $(GDK_TYPES_DEFS) $(GTK_DEFS) $(GTK_TYPES_DEFS) \
	gdk.defs gtk.defs gdk-types.defs gtk-types.defs gtk-extrafuncs.defs
endif

# libglade module
LIBGLADE_DEFS = libglade.defs
CLEANFILES += libglade.c
EXTRA_DIST += $(LIBGLADE_DEFS) libglade.override
libglade.c: $(LIBGLADE_DEFS) libglade.override gdk-types.defs gtk-types.defs
glade_la_CFLAGS = $(LIBGLADE_CFLAGS)
glade_la_LIBADD = $(LIBGLADE_LIBS)
glade_la_LDFLAGS = $(common_ldflags) -export-symbols-regex initglade
glade_la_SOURCES = libglademodule.c
nodist_glade_la_SOURCES = libglade.c
if BUILD_LIBGLADE
pygtkexec_LTLIBRARIES += glade.la
defs_DATA += $(LIBGLADE_DEFS)
endif

# gtkunixprint module
CLEANFILES +=			\
	gtkunixprint.c 		\
	gtkunixprint.defs 	\
	gtkunixprint-types.defs


EXTRA_DIST +=				\
	$(GTKUNIXPRINT_DEFS)		\
	$(GTKUNIXPRINT_TYPES_DEFS)	\
	gtkunixprint.override

gtkunixprint.c:						\
	 gtkunixprint-types.defs gtkunixprint.defs	\
		gtkunixprint.override gdk-types.defs	\
		gtk-types.defs

gtkunixprint_la_CFLAGS = $(PYCAIRO_CFLAGS) $(GTKUNIXPRINT_CFLAGS)
gtkunixprint_la_LIBADD = $(GTKUNIXPRINT_LIBS)
gtkunixprint_la_LDFLAGS = $(common_ldflags) -export-symbols-regex initgtkunixprint
gtkunixprint_la_SOURCES = gtkunixprintmodule.c
nodist_gtkunixprint_la_SOURCES = gtkunixprint.c
if BUILD_GTKUNIXPRINT
pkgpyexec_LTLIBRARIES = gtkunixprint.la
defs_DATA += $(GTKUNIXPRINT_DEFS) $(GTKUNIXPRINT_TYPES_DEFS)
endif

if PLATFORM_WIN32
_gtk_la_CFLAGS  += -DPLATFORM_WIN32
endif


extra_codegen_args =

if HAVE_GIO_TYPES_DEFS
extra_codegen_args += --register $(PYGOBJECT_DEFSDIR)/gio-types.defs
endif

.defs.c: 
	($(PYTHON) $(CODEGENDIR)/codegen.py \
	   $(PYGTK_CODEGEN_DEFINES) \
	    -I $(srcdir) \
	    --py_ssize_t-clean $(extra_codegen_args) \
	    --register $(top_srcdir)/pango-types.defs \
	    --register $(top_srcdir)/atk-types.defs \
	    --register gdk-types.defs \
	    --register gtk-types.defs \
	    --override $(srcdir)/$*.override \
	    --prefix py$* $<) > gen-$*.c \
	&& cp gen-$*.c $*.c \
	&& rm -f gen-$*.c

# prevent the .defs.c rule from trying to generate gtk-types.c because
# there is gtk-types.defs.  This is basically a hack and I really wish I
# could use pattern rules.  Perhaps we should rename gtk-types.c.
gtk-types.c:
	@:


all: $(pygtkexec_LTLIBRARIES:.la=.so) $(pkgpyexec_LTLIBRARIES:.la=.so)
clean-local:
	rm -f $(pygtkexec_LTLIBRARIES:.la=.so) $(pkgpyexec_LTLIBRARIES:.la=.so)
.la.so:
	$(LN_S) .libs/$@ $@ || true
