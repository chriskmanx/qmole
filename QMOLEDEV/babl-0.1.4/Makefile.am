ACLOCAL_AMFLAGS = -I m4
DISTCHECK_CONFIGURE_FLAGS = --enable-introspection
AUTOMAKE_OPTIONS = foreign dist-bzip2

SUBDIRS = \
	babl		\
	extensions	\
	tests		\
	docs

pkgconfigdir = $(libdir)/pkgconfig
pkgconfig_DATA = babl.pc

built_dist_files = README

EXTRA_DIST = \
	$(built_dist_files)	\
	ChangeLog.pre-0-1	\
	AUTHORS			\
	COPYING			\
	INSTALL			\
	NEWS			\
	TODO			\
	ChangeLog		\
	autogen.sh		\
	babl.pc.in		\
	m4/introspection.m4

DISTCLEANFILES = \
	$(built_dist_files)	\
	babl.pc

.PHONY: snapshot


if HAVE_W3M
README: all docs/index.html
	$(W3M) -cols 72 -dump docs/index.html > $@
else
README:
	@echo "*** w3m must be available in order to make dist"
	@false
endif

snapshot:
	$(MAKE) dist distdir=$(PACKAGE)-`date +"%Y%m%d"`
snapcheck:
	$(MAKE) distcheck distdir=$(PACKAGE)-`date +"%Y%m%d"`

distclean-local:
	if test $(srcdir) = .; then :; else 	\
		rm -f $(BUILT_EXTRA_DIST);	\
	fi



### ChangeLog generation

CHANGELOG_START = BABL_0_1_0

ChangeLog: $(srcdir)/ChangeLog
$(srcdir)/ChangeLog:
	@echo Creating $@
	@if test -d "$(srcdir)/.git"; then \
	  (GIT_DIR=$(top_srcdir)/.git ./missing --run \
	   git log $(CHANGELOG_START)^.. --stat) | fmt --split-only > $@.tmp \
	  && mv -f $@.tmp $@ \
	  || ($(RM) $@.tmp; \
	      echo Failed to generate ChangeLog, your ChangeLog may be outdated >&2; \
	      (test -f $@ || echo git-log is required to generate this file >> $@)); \
	else \
	  test -f $@ || \
	  (echo A git checkout and git-log is required to generate ChangeLog >&2 && \
	  echo A git checkout and git-log is required to generate this file >> $@); \
	fi
.PHONY: $(srcdir)/ChangeLog
