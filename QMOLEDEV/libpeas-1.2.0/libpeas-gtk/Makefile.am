lib_LTLIBRARIES = libpeas-gtk-1.0.la

INCLUDES = \
	-I$(top_srcdir)			\
	-I$(srcdir)			\
	$(PEAS_CFLAGS)			\
	$(PEAS_GTK_CFLAGS)		\
	$(GCOV_CFLAGS)			\
	$(WARN_CFLAGS)			\
	$(DISABLE_DEPRECATED)

libpeas_gtk_1_0_la_LDFLAGS = \
	$(GCOV_LDFLAGS) \
	-version-info $(LT_CURRENT):$(LT_REVISION):$(LT_AGE) \
	-export-dynamic -no-undefined -export-symbols-regex "^[^_].*"

libpeas_gtk_1_0_la_LIBADD = \
	$(PEAS_LIBS)							\
	$(PEAS_GTK_LIBS)						\
	$(top_builddir)/libpeas/libpeas-1.0.la

INST_H_FILES = \
	peas-gtk-configurable.h		\
	peas-gtk-plugin-manager.h	\
	peas-gtk-plugin-manager-view.h	\
	peas-gtk.h

NOINST_H_FILES = \
	peas-gtk-disable-plugins-dialog.h	\
	peas-gtk-plugin-manager-store.h

C_FILES = \
	peas-gtk-configurable.c			\
	peas-gtk-disable-plugins-dialog.c	\
	peas-gtk-plugin-manager.c		\
	peas-gtk-plugin-manager-store.c		\
	peas-gtk-plugin-manager-view.c

libpeas_gtk_1_0_la_SOURCES = \
	$(INST_H_FILES)			\
	$(NOINST_H_FILES)		\
	$(C_FILES)

headerdir = $(prefix)/include/libpeas-1.0/libpeas-gtk
header_DATA = $(INST_H_FILES)

if HAVE_INTROSPECTION
  -include $(INTROSPECTION_MAKEFILE)
  INTROSPECTION_GIRS = PeasGtk-1.0.gir
  INTROSPECTION_SCANNER_ARGS = \
	--add-include-path=$(top_builddir)/libpeas	\
	--c-include=libpeas-gtk/peas-gtk.h		\
	--warn-all
  INTROSPECTION_COMPILER_ARGS = \
	--includedir=$(top_srcdir)/libpeas		\
	--includedir=$(top_builddir)/libpeas

  introspection_sources = $(INST_H_FILES) $(C_FILES)

  PeasGtk-1.0.gir: libpeas-gtk-1.0.la
  PeasGtk_1_0_gir_INCLUDES = GObject-2.0 Gtk-3.0
  PeasGtk_1_0_gir_CFLAGS = $(PEAS_CFLAGS) $(PEAS_GTK_CFLAGS) -I$(top_srcdir)
  PeasGtk_1_0_gir_LIBS = $(top_builddir)/libpeas/libpeas-1.0.la libpeas-gtk-1.0.la
  PeasGtk_1_0_gir_FILES = $(addprefix $(srcdir)/,$(introspection_sources))
  PeasGtk_1_0_gir_EXPORT_PACKAGES = libpeas-gtk-1.0
  PeasGtk_1_0_gir_SCANNERFLAGS = --include-uninstalled=$(top_builddir)/libpeas/Peas-1.0.gir

  girdir = $(datadir)/gir-1.0
  gir_DATA = $(INTROSPECTION_GIRS)

  typelibdir = $(libdir)/girepository-1.0
  typelib_DATA = $(INTROSPECTION_GIRS:.gir=.typelib)

  CLEANFILES = $(gir_DATA) $(typelib_DATA)
endif

gcov_sources = $(C_FILES)
include $(top_srcdir)/Makefile.gcov
