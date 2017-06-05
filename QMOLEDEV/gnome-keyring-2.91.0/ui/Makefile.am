
if WITH_TESTS
TESTS_DIR = tests
else
TESTS_DIR =
endif

SUBDIRS = . $(TESTS_DIR)

INCLUDES= \
	-DPREFIX=\""$(prefix)"\" \
	-DBINDIR=\""$(bindir)"\" \
	-DLIBEXECDIR=\""$(libexecdir)"\" \
	-DGNOMELOCALEDIR=\""$(datadir)/locale"\" \
	-I$(top_srcdir) \
	-I$(top_builddir) \
	$(GOBJECT_CFLAGS) \
	$(GLIB_CFLAGS)

# ------------------------------------------------------------------
# UI BUILDER
#

uidir = $(datadir)/gnome-keyring@GCR_VERSION_SUFFIX@/ui/

ui_DATA = \
	gku-prompt.ui

# ------------------------------------------------------------------
# DAEMON CODE

noinst_LTLIBRARIES = libgku-prompt.la

BUILT_SOURCES = \
	gku-prompt-marshal.c gku-prompt-marshal.h

libgku_prompt_la_SOURCES = \
	gku-prompt.c gku-prompt.h \
	gku-prompt-util.c gku-prompt-util.h \
	$(BUILT_SOURCES)

libgku_prompt_la_LIBADD = \
	$(GLIB_LIBS) \
	$(GOBJECT_LIBS)

gku-prompt-marshal.h: gku-prompt-marshal.list $(GLIB_GENMARSHAL)
	$(GLIB_GENMARSHAL) $< --header --prefix=gku_prompt_marshal > $@

gku-prompt-marshal.c: gku-prompt-marshal.list $(GLIB_GENMARSHAL)
	echo "#include \"gku-prompt-marshal.h\"" > $@ && \
	$(GLIB_GENMARSHAL) $< --body --prefix=gku_prompt_marshal >> $@

EXTRA_DIST = \
	gku-prompt-marshal.list \
	$(ui_DATA)

# ------------------------------------------------------------------
# PROMPT TOOL
#

libexec_PROGRAMS= \
	gnome-keyring-prompt@GCR_VERSION_SUFFIX@

gnome_keyring_prompt@GCR_VERSION_SUFFIX@_SOURCES = \
	gku-prompt-tool.c \
	gku-prompt-util.c gku-prompt-util.h

gnome_keyring_prompt@GCR_VERSION_SUFFIX@_LDADD = \
	$(top_builddir)/egg/libegg-prompt.la \
	$(top_builddir)/egg/libegg-entry-buffer.la \
	$(top_builddir)/gcr/libgcr@GCR_VERSION_SUFFIX@.la \
	$(LIBGCRYPT_LIBS) \
	$(GTK_LIBS)

gnome_keyring_prompt@GCR_VERSION_SUFFIX@_CFLAGS = \
	-DUIDIR=\""$(uidir)"\" \
	-DGCR_API_SUBJECT_TO_CHANGE \
	$(LIBGCRYPT_CFLAGS) \
	$(GTK_CFLAGS)

# Install a link from gnome-keyring-prompt → gnome-keyring-prompt@GCR_VERSION_SUFFIX@ to maintain compatibility
if WITH_GTK3
install-exec-hook:
	@test -e "$(DESTDIR)$(libexecdir)/gnome-keyring-prompt" || (cd $(DESTDIR)$(libexecdir) && $(LN_S) gnome-keyring-prompt@GCR_VERSION_SUFFIX@ gnome-keyring-prompt)
endif
