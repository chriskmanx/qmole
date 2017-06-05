
noinst_LTLIBRARIES = libgoffice.la

AM_CPPFLAGS = \
	$(DEPS_CFLAGS)

libgoffice_la_SOURCES = \
	goffice/goffice-config.h \
	goffice/goffice-features.h \
	goffice/goffice-priv.h \
	goffice/goffice.h \
	goffice/goffice.c \
	goffice/app/goffice-app.h \
	goffice/app/go-cmd-context.h \
	goffice/app/go-cmd-context.c \
	goffice/app/go-cmd-context-impl.h \
	goffice/gtk/goffice-gtk.h \
	goffice/gtk/goffice-gtk.c \
	goffice/gtk/go-color-palette.h \
	goffice/gtk/go-color-palette.h \
	goffice/gtk/go-color-palette.c \
	goffice/gtk/go-color-group.h \
	goffice/gtk/go-color-group.c \
	goffice/gtk/go-combo-box.h \
	goffice/gtk/go-combo-box.c \
	goffice/gtk/go-combo-color.h \
	goffice/gtk/go-combo-color.c \
	goffice/utils/goffice-utils.h \
	goffice/utils/go-glib-extras.h \
	goffice/utils/go-glib-extras.c \
	goffice/utils/go-image.h \
	goffice/utils/go-image.c \
	goffice/utils/go-marshalers.h \
	goffice/utils/go-marshalers.c \
	goffice/utils/go-file.h \
	goffice/utils/go-file.c \
	goffice/utils/go-color.h \
	goffice/utils/go-color.c

BUILT_SOURCES =	goffice-paths.h

CLEANFILES = $(BUILT_SOURCES)

EXTRA_DIST = \
	AUTHORS \
	ChangeLog \
	COPYING \
	README \
	README.abiword

# Depends on this Makefile, because it uses make variables.
goffice-paths.h: Makefile
	@echo 'creating $@'
	@( \
	echo '/* This file has been automatically generated.  Do not edit. */'; \
	echo ''; \
	echo '#ifndef GOFFICE_PATHS_H'; \
	echo '#define GOFFICE_PATHS_H'; \
	echo ''; \
	echo '#define GOFFICE_DATADIR "$(goffice_datadir)"'; \
	echo '#define GOFFICE_LIBDIR "$(goffice_libdir)"'; \
	echo '#define GOFFICE_ICONDIR "$(goffice_icondir)"'; \
	echo '#define GOFFICE_LOCALEDIR "$(goffice_localedir)"'; \
	echo ''; \
	echo '#endif /* GOFFICE_PATHS_H */'; \
	) >$@

