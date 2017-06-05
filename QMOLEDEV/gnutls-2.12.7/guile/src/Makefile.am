#  GnuTLS --- Guile bindings for GnuTLS.
#  Copyright (C) 2007, 2008, 2009, 2010 Free Software Foundation, Inc.
#
#  GnuTLS is free software; you can redistribute it and/or
#  modify it under the terms of the GNU Lesser General Public
#  License as published by the Free Software Foundation; either
#  version 2.1 of the License, or (at your option) any later version.
#
#  GnuTLS is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
#  Lesser General Public License for more details.
#
#  You should have received a copy of the GNU Lesser General Public
#  License along with GnuTLS; if not, write to the Free Software
#  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA

GUILE_FOR_BUILD =						\
  GUILE_AUTO_COMPILE=0 $(GUILE) -L $(top_srcdir)/guile/modules

noinst_HEADERS = errors.h utils.h

EXTRA_DIST =					\
  make-enum-map.scm make-smob-types.scm		\
  make-enum-header.scm make-smob-header.scm	\
  make-session-priorities.scm

BUILT_SOURCES = enum-map.i.c smob-types.i.c enums.h smobs.h	\
		priorities.i.c					\
		extra-smobs.h extra-enums.h			\
		extra-enum-map.i.c extra-smob-types.i.c		\
                core.x errors.x extra.x

CLEANFILES = $(BUILT_SOURCES)

lib_LTLIBRARIES = libguile-gnutls-v-1.la libguile-gnutls-extra-v-1.la

# Linking against GnuTLS.
GNUTLS_CORE_LIBS  = $(top_builddir)/lib/libgnutls.la
GNUTLS_EXTRA_LIBS = $(top_builddir)/libextra/libgnutls-extra.la

# Linking against Gnulib modules.
GNULIB_LIBS    = $(top_builddir)/lib/gl/liblgnu.la
GNULIB_CFLAGS  = -I$(top_builddir)/lib/gl -I$(top_srcdir)/lib/gl


libguile_gnutls_v_1_la_SOURCES = core.c errors.c utils.c
libguile_gnutls_v_1_la_CFLAGS =			\
  $(AM_CFLAGS) $(GNULIB_CFLAGS) $(GUILE_CFLAGS)
libguile_gnutls_v_1_la_LIBADD = \
	$(GNUTLS_CORE_LIBS) $(GNULIB_LIBS) \
	$(GUILE_LDFLAGS)

libguile_gnutls_extra_v_1_la_SOURCES = extra.c
libguile_gnutls_extra_v_1_la_CFLAGS =		\
  $(AM_CFLAGS) $(GNULIB_CFLAGS) $(GUILE_CFLAGS)
libguile_gnutls_extra_v_1_la_LIBADD =		\
  $(GNUTLS_CORE_LIBS) $(GNUTLS_EXTRA_LIBS)	\
  $(builddir)/libguile-gnutls-v-1.la		\
  $(GNULIB_LIBS) $(GUILE_LDFLAGS)

AM_CPPFLAGS = \
	-I$(top_srcdir)/lib/includes \
	-I$(top_builddir)/lib/includes	\
	-I$(top_srcdir)/libextra/includes \
	-I$(top_builddir) \
	-I$(builddir)

if HAVE_GCC

# Generated `.x' files and Guile's `scm_c_define_gsubr ()' require
# `-Wno-strict-prototypes'.  This trick makes sure `-Wno-s-p' appears
# after `-Ws-p'.
AM_CFLAGS = -Wno-strict-prototypes

# The `-fgnu89-inline' option appeared in GCC 4.1.3.
if HAVE_GCC_GNU89_INLINE_OPTION

# Guile and GMP currently rely on GNU inline semantics, not C99 inline.
AM_CFLAGS += -fgnu89-inline

endif HAVE_GCC_GNU89_INLINE_OPTION

endif HAVE_GCC


enums.h: $(srcdir)/make-enum-header.scm
	$(GUILE_FOR_BUILD) $^ > $@

enum-map.i.c: $(srcdir)/make-enum-map.scm
	$(GUILE_FOR_BUILD) $^ > $@

smobs.h: $(srcdir)/make-smob-header.scm
	$(GUILE_FOR_BUILD) $^ > $@

smob-types.i.c: $(srcdir)/make-smob-types.scm
	$(GUILE_FOR_BUILD) $^ > $@

priorities.i.c: $(srcdir)/make-session-priorities.scm
	$(GUILE_FOR_BUILD) $^ > $@


# GnuTLS-extra

extra-enums.h: $(srcdir)/make-enum-header.scm
	$(GUILE_FOR_BUILD) $^ extra > $@

extra-enum-map.i.c: $(srcdir)/make-enum-map.scm
	$(GUILE_FOR_BUILD) $^ extra > $@

extra-smobs.h: $(srcdir)/make-smob-header.scm
	$(GUILE_FOR_BUILD) $^ extra > $@

extra-smob-types.i.c: $(srcdir)/make-smob-types.scm
	$(GUILE_FOR_BUILD) $^ extra > $@


# C file snarfing.

# `$(GUILE_CFLAGS)' may contain a series of `-I' switches so it must be
# included here, even though we'd really want `$(GUILE_CPPFLAGS)'.
snarfcppopts = $(DEFS) $(INCLUDES) $(AM_CPPFLAGS) $(CPPFLAGS) \
	       $(CFLAGS) $(AM_CFLAGS) $(GUILE_CFLAGS)

.c.x: $(BUILT_SOURCES)
	$(guile_snarf) -o $@ $< $(snarfcppopts)

# Target used by doc/Makefile, to create all built sources necessary
# for generating the manual.

.PHONY: built-sources
built-sources: $(BUILT_SOURCES)
