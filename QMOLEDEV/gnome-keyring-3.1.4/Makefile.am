if WITH_PAM
PAM_DIR = pam
else
PAM_DIR = 
endif

if WITH_TESTS
TESTS_DIR = testing
else
TESTS_DIR = 
endif

SUBDIRS = \
	. \
	egg \
	gck \
	gcr \
	ui \
	pkcs11 \
	schema \
	daemon \
	tool \
	$(TESTS_DIR) \
	$(PAM_DIR) \
	po \
	docs

ACLOCAL_AMFLAGS = ${ACLOCAL_FLAGS}

EXTRA_DIST = \
	intltool-extract.in \
	intltool-merge.in \
	intltool-update.in \
	ChangeLog.old \
	HACKING

DISTCHECK_CONFIGURE_FLAGS = \
	--enable-gtk-doc \
	--enable-tests=full \
	--enable-debug=yes \
	--disable-gcov

DISTCLEANFILES = \
	intltool-extract \
	intltool-merge \
	intltool-update

CHANGELOG_START = \
	efb922c92fae5929ecc8c702770ebb6c390d38a4

dist-hook:
	@if test -d "$(srcdir)/.git"; \
	then \
		echo Creating ChangeLog && \
		( cd "$(top_srcdir)" && \
		  echo '# Generate automatically. Do not edit.'; echo; \
		  $(top_srcdir)/missing --run git log $(CHANGELOG_START).. --stat --date=short ) > ChangeLog.tmp \
		&& mv -f ChangeLog.tmp $(top_distdir)/ChangeLog \
		|| ( rm -f ChangeLog.tmp ; \
		     echo Failed to generate ChangeLog >&2 ); \
	else \
		echo A git clone is required to generate a ChangeLog >&2; \
	fi

install-pam:
	@echo "WARNING: install-pam is no longer used, use the --with-pam-dir configure option instead"

if WITH_COVERAGE
coverage:
	mkdir -p testing/coverage
	$(LCOV) --directory docs/ --zerocounters
	$(LCOV) --directory . --capture --output-file testing/coverage.info
	$(GENHTML) --output-directory testing/coverage testing/coverage.info
	$(LCOV) --directory . --zerocounters
	@echo "file://$(abs_top_builddir)/testing/coverage/index.html"

clear-coverage:
	$(LCOV) --directory . --zerocounters

endif

if WITH_GNU_MAKE
# Include this local file if it exists
-include Makefile.local
endif
