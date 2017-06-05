
AM_CFLAGS = -Wall
AM_CPPFLAGS = @X_CFLAGS@ @GLIB_CFLAGS@
LDADD = @X_LIBS@ @X_PRE_LIBS@ -lX11 -lXmu @X_EXTRA_LIBS@ @GLIB_LIBS@

bin_PROGRAMS = wmctrl
wmctrl_SOURCES = main.c
man_MANS = wmctrl.1
EXTRA_DIST = $(man_MANS)
