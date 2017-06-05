
INCLUDES = \
	-I$(top_srcdir) \
	-DGNOME_KEYRING_DAEMON=\""$(bindir)/gnome-keyring-daemon"\" 

pamlibdir = $(PAM_DEST_DIR)
pamlib_LTLIBRARIES = pam_gnome_keyring.la

pam_gnome_keyring_la_SOURCES = \
	gkr-pam.h \
	gkr-pam-client.c \
	gkr-pam-module.c \
	gkr-pam-stubs.c

pam_gnome_keyring_la_LIBADD = \
	$(top_builddir)/egg/libegg-buffer.la \
	$(top_builddir)/egg/libegg-creds.la \
	$(top_builddir)/egg/libegg-secure.la \
	$(LIBSELINUX) \
	-lpam
	
pam_gnome_keyring_la_LDFLAGS = \
	-module -avoid-version \
	-export-symbols-regex 'pam_sm_'

install-pam: install
	@echo "WARNING: install-pam is no longer used, use the --with-pam-dir configure option instead"
