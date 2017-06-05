
SUBDIRS = \
	. \
	egg \
	library \
	po \
	docs

EXTRA_DIST = \
	intltool-extract.in \
	intltool-merge.in \
	intltool-update.in \
	HACKING \
	COPYING.GPL

DISTCHECK_CONFIGURE_FLAGS = \
	--enable-gtk-doc \
	--enable-tests=yes \
	--enable-debug=yes

DISTCLEANFILES = \
	intltool-extract \
	intltool-merge \
	intltool-update

dist-hook:
	@if test -d "$(srcdir)/.git"; \
	then \
		echo Creating ChangeLog && \
		( cd "$(top_srcdir)" && \
		  echo '# Generate automatically. Do not edit.'; echo; \
		  $(top_srcdir)/missing --run git log --stat --date=short ) > ChangeLog.tmp \
		&& mv -f ChangeLog.tmp $(top_distdir)/ChangeLog \
		|| ( rm -f ChangeLog.tmp ; \
		     echo Failed to generate ChangeLog >&2 ); \
	else \
		echo A git clone is required to generate a ChangeLog >&2; \
	fi
