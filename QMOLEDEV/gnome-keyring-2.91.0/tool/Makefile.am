bin_PROGRAMS= \
	gnome-keyring@GCR_VERSION_SUFFIX@

INCLUDES=	\
	-I$(top_srcdir) \
	-I$(top_builddir) \
	$(GTK_CFLAGS) \
	$(GOBJECT_CFLAGS) \
	$(GLIB_CFLAGS) \
	-DGCK_API_SUBJECT_TO_CHANGE

gnome_keyring@GCR_VERSION_SUFFIX@_SOURCES = \
	gkr-tool.c gkr-tool.h \
	gkr-tool-import.c
	
gnome_keyring@GCR_VERSION_SUFFIX@_CFLAGS = \
	-DGCR_API_SUBJECT_TO_CHANGE \
	-DGNOMELOCALEDIR=\""$(datadir)/locale"\"

gnome_keyring@GCR_VERSION_SUFFIX@_LDADD = \
	$(top_builddir)/gck/libgck.la \
	$(top_builddir)/gcr/libgcr@GCR_VERSION_SUFFIX@.la \
	$(top_builddir)/egg/libegg.la \
	$(GTHREAD_LIBS) \
	$(GTK_LIBS) \
	$(GCRYPT_LIBS) \
	$(DAEMON_LIBS)

# Install a link from gnome-keyring → gnome-keyring@GCR_VERSION_SUFFIX@ to maintain compatibility
if WITH_GTK3
install-exec-hook:
	@test -e "$(DESTDIR)$(bindir)/gnome-keyring" || (cd $(DESTDIR)$(bindir) && $(LN_S) gnome-keyring@GCR_VERSION_SUFFIX@ gnome-keyring)
endif
