if WITH_WIN32

noinst_DATA = lib.def

lib.def: stamp-lib.def
	@true

stamp-lib.def: $(LIB_PUBLIC_HDRS) Makefile $(top_srcdir)/dumpdef.pl
	hdrs='$(LIB_PUBLIC_HDRS)'; \
	hdrs_list=''; \
	for hdr in $$hdrs; do \
	  if test -f $(srcdir)/$$hdr; then \
	    hdrs_list="$$hdrs_list $(srcdir)/$$hdr"; \
	  else \
	    hdrs_list="$$hdrs_list $$hdr"; \
	  fi; \
	done; \
	cat $(top_builddir)/gsf-config.h $$hdrs_list | \
		sed -e 's/^#[ \t]*include[ \t]\+.*$$//g' | \
		$(CPP) $(AM_CPPFLAGS) $(CPP_CFLAGS) -P - > xgen-libdef.1 && \
	echo EXPORTS> xgen-libdef.2 && \
	perl $(top_srcdir)/dumpdef.pl \
		xgen-libdef.1 >> xgen-libdef.2 \
	&& (cmp -s xgen-libdef.2 lib.def || \
		cp xgen-libdef.2 lib.def) \
	&& rm -f xgen-libdef.1 xgen-libdef.2 \
	&& echo timestamp > $@	

CLEANFILES = lib.def stamp-lib.def

endif
