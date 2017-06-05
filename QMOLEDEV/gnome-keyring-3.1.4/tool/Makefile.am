bin_PROGRAMS= \
	gnome-keyring-@GCR_MAJOR@

INCLUDES=	\
	-I$(top_srcdir) \
	-I$(top_builddir) \
	$(GTK_CFLAGS) \
	$(GOBJECT_CFLAGS) \
	$(GLIB_CFLAGS) \
	-DGCK_API_SUBJECT_TO_CHANGE

gnome_keyring_@GCR_MAJOR@_SOURCES = \
	gkr-tool.c gkr-tool.h \
	gkr-tool-import.c \
	gkr-tool-trust.c \
	gkr-tool-version.c

gnome_keyring_@GCR_MAJOR@_CFLAGS = \
	-DGCR_API_SUBJECT_TO_CHANGE \
	-DGNOMELOCALEDIR=\""$(datadir)/locale"\"

gnome_keyring_@GCR_MAJOR@_LDADD = \
	$(top_builddir)/gcr/libgcr-@GCR_MAJOR@.la \
	$(top_builddir)/egg/libegg.la \
	$(top_builddir)/gck/libgck-@GCK_MAJOR@.la \
	$(GTHREAD_LIBS) \
	$(GTK_LIBS) \
	$(GCRYPT_LIBS) \
	$(DAEMON_LIBS)

# Install a link from gnome-keyring > gnome-keyring-@GCR_MAJOR@ to maintain compatibility
install-exec-hook:
	@test -e "$(DESTDIR)$(bindir)/gnome-keyring" || (cd $(DESTDIR)$(bindir) && $(LN_S) gnome-keyring-@GCR_MAJOR@ gnome-keyring)
