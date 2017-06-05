
if WITH_TESTS
TESTS_DIR = tests
TESTABLE_LIB = libgku-prompt-testable.la
else
TESTS_DIR =
TESTABLE_LIB =
endif

SUBDIRS = . $(TESTS_DIR)

INCLUDES= \
	-DGNOMELOCALEDIR=\""$(datadir)/locale"\" \
	-DPROMPTEXEC=\""$(libexecdir)/gnome-keyring-prompt-@GCR_MAJOR@"\" \
	-I$(top_srcdir) \
	-I$(top_builddir) \
	$(GOBJECT_CFLAGS) \
	$(GLIB_CFLAGS)

# ------------------------------------------------------------------
# UI BUILDER
#

uidir = $(datadir)/gnome-keyring-@GCR_MAJOR@/ui/

ui_DATA = \
	gku-prompt.ui

# ------------------------------------------------------------------
# DAEMON CODE

noinst_LTLIBRARIES = libgku-prompt.la $(TESTABLE_LIB)

BUILT_SOURCES = \
	gku-prompt-marshal.c gku-prompt-marshal.h

libgku_prompt_la_SOURCES = \
	gku-prompt.c gku-prompt.h \
	gku-prompt-util.c gku-prompt-util.h \
	$(BUILT_SOURCES)

libgku_prompt_la_LIBADD = \
	$(GLIB_LIBS) \
	$(GOBJECT_LIBS)

libgku_prompt_testable_la_SOURCES = $(libgku_prompt_la_SOURCES)
libgku_prompt_testable_la_CFLAGS = -DWITH_TESTABLE

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
	gnome-keyring-prompt-@GCR_MAJOR@

gnome_keyring_prompt_@GCR_MAJOR@_SOURCES = \
	gku-prompt-tool.c \
	gku-prompt-util.c gku-prompt-util.h

gnome_keyring_prompt_@GCR_MAJOR@_LDADD = \
	$(top_builddir)/egg/libegg-prompt.la \
	$(top_builddir)/egg/libegg-entry-buffer.la \
	$(top_builddir)/gcr/libgcr-@GCR_MAJOR@.la \
	$(top_builddir)/gck/libgck-@GCK_MAJOR@.la \
	$(LIBGCRYPT_LIBS) \
	$(GTK_LIBS)

gnome_keyring_prompt_@GCR_MAJOR@_CFLAGS = \
	-DUIDIR=\""$(uidir)"\" \
	-DGCR_API_SUBJECT_TO_CHANGE \
	$(LIBGCRYPT_CFLAGS) \
	$(GTK_CFLAGS)

# Install a link from gnome-keyring-prompt -> gnome-keyring-prompt-@GCR_MAJOR@ to maintain compatibility
install-exec-hook:
	@test -e "$(DESTDIR)$(libexecdir)/gnome-keyring-prompt" || (cd $(DESTDIR)$(libexecdir) && $(LN_S) gnome-keyring-prompt-@GCR_MAJOR@ gnome-keyring-prompt)

@INTLTOOL_DESKTOP_RULE@
desktopdir= $(datadir)/applications
desktop_in_in_files = gnome-keyring-prompt.desktop.in.in
desktop_in_files = gnome-keyring-prompt.desktop.in
desktop_DATA = $(desktop_in_files:.desktop.in=.desktop)

EXTRA_DIST += \
	$(desktop_in_in_files)

CLEANFILES = \
	$(desktop_DATA)

DISTCLEANFILES = \
	$(desktop_in_files)
