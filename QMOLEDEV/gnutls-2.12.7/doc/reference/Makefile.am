## Process this file with automake to produce Makefile.in

# We require automake 1.6 at least.
AUTOMAKE_OPTIONS = 1.6

# This is a blank Makefile.am for using gtk-doc.
# Copy this to your project's API docs directory and modify the variables to
# suit your project. See the GTK+ Makefiles in gtk+/docs/reference for examples
# of using the various options.

# The name of the module, e.g. 'glib'.
DOC_MODULE=$(PACKAGE)

# Uncomment for versioned docs and specify the version of the module, e.g. '2'.
#DOC_MODULE_VERSION=2


# The top-level SGML file. You can change this if you want to.
DOC_MAIN_SGML_FILE=$(DOC_MODULE)-docs.sgml

# Directories containing the source code, relative to $(srcdir).
# gtk-doc will search all .c and .h files beneath these paths
# for inline comments documenting functions and macros.
# e.g. DOC_SOURCE_DIR=../../../gtk ../../../gdk
DOC_SOURCE_DIR=../../lib ../../libextra

# Extra options to pass to gtkdoc-scangobj. Not normally needed.
SCANGOBJ_OPTIONS=

# Extra options to supply to gtkdoc-scan.
# e.g. SCAN_OPTIONS=--deprecated-guards="GTK_DISABLE_DEPRECATED"
SCAN_OPTIONS=--ignore-decorators=ASN1_API \
	--deprecated-guards=ASN1_DISABLE_DEPRECATED

# Extra options to supply to gtkdoc-mkdb.
# e.g. MKDB_OPTIONS=--sgml-mode --output-format=xml
MKDB_OPTIONS=--sgml-mode --output-format=xml

# Extra options to supply to gtkdoc-mktmpl
# e.g. MKTMPL_OPTIONS=--only-section-tmpl
MKTMPL_OPTIONS=

# Extra options to supply to gtkdoc-mkhtml
MKHTML_OPTIONS=

# Extra options to supply to gtkdoc-fixref. Not normally needed.
# e.g. FIXXREF_OPTIONS=--extra-dir=../gdk-pixbuf/html --extra-dir=../gdk/html
FIXXREF_OPTIONS=

# Used for dependencies. The docs will be rebuilt if any of these change.
# e.g. HFILE_GLOB=$(top_srcdir)/gtk/*.h
# e.g. CFILE_GLOB=$(top_srcdir)/gtk/*.c
HFILE_GLOB=$(top_srcdir)/lib/includes/gnutls/*.h \
	$(top_builddir)/lib/includes/gnutls/*.h \
	$(top_srcdir)/libextra/includes/gnutls/*.h
CFILE_GLOB=$(top_srcdir)/lib/*.c \
	$(top_srcdir)/lib/x509/*.c \
	$(top_srcdir)/lib/openpgp/*.c \
	$(top_srcdir)/libextra/*.c \
	$(top_srcdir)/lib/minitasn1/*.c \
	$(top_srcdir)/libextra/*.c

# Extra header to include when scanning, which are not under DOC_SOURCE_DIR
# e.g. EXTRA_HFILES=$(top_srcdir}/contrib/extra.h
EXTRA_HFILES=

# Header files to ignore when scanning. Use base file name, no paths
# e.g. IGNORE_HFILES=gtkdebug.h gtkintl.h
IGNORE_HFILES=compat.h gnutlsxx.h

# cd lib/ && echo *.h | sed 's/crypto.h//'
IGNORE_HFILES += auth_anon.h auth_cert.h auth_dh_common.h auth_psk.h	\
	auth_psk_passwd.h auth_srp.h auth_srp_passwd.h config.h		\
	debug.h ext_cert_type.h ext_max_record.h ext_oprfi.h	\
	ext_safe_renegotiation.h ext_server_name.h			\
	ext_session_ticket.h ext_signature.h ext_srp.h			\
	gnutls_algorithms.h gnutls_auth.h gnutls_buffers.h		\
	gnutls_cert.h gnutls_cipher.h gnutls_cipher_int.h		\
	gnutls_compress.h gnutls_constate.h gnutls_cryptodev.h		\
	gnutls_datum.h gnutls_db.h gnutls_dh.h gnutls_errors.h		\
	gnutls_extensions.h gnutls_global.h gnutls_handshake.h		\
	gnutls_hash_int.h gnutls_helper.h gnutls_int.h gnutls_kx.h	\
	gnutls_mem.h gnutls_mpi.h gnutls_num.h gnutls_pk.h		\
	gnutls_record.h gnutls_rsa_export.h gnutls_session_pack.h	\
	gnutls_sig.h gnutls_srp.h gnutls_state.h gnutls_str.h		\
	gnutls_supplemental.h gnutls_v2_compat.h gnutls_x509.h		\
	random.h x509_b64.h

# cd lib/gl/ && echo *.h
IGNORE_HFILES += alignof.h alloca.h alloca.in.h arg-nonnull.h		\
	byteswap.in.h c-ctype.h close-hook.h errno.in.h float+.h	\
	float.in.h gettext.h minmax.h netdb.in.h printf-args.h		\
	printf-parse.h read-file.h size_max.h sockets.h stdbool.in.h	\
	stddef.in.h stdint.in.h stdio.h stdio-impl.h stdio.in.h		\
	stdlib.h stdlib.in.h string.h string.in.h strings.h		\
	strings.in.h str-two-way.h sys_socket.in.h sys_stat.in.h	\
	time.h time.in.h unistd.h unistd.in.h w32sock.h warn-on-use.h	\
	vasnprintf.h wchar.h wchar.in.h xsize.h

# cd lib/gl/sys/ && echo *.h
IGNORE_HFILES += socket.h stat.h

# cd lib/minitasn1/ && echo *.h
IGNORE_HFILES += element.h gstr.h int.h libtasn1.h parser_aux.h structure.h

# cd lib/opencdk/ && echo *.h
IGNORE_HFILES += context.h filters.h keydb.h main.h opencdk.h packet.h	\
	stream.h types.h

# cd lib/openpgp/ && echo *.h
IGNORE_HFILES += gnutls_openpgp.h openpgp_int.h

# cd lib/x509/ && echo *.h
IGNORE_HFILES += common.h pbkdf2-sha1.h x509_int.h

# cd lib/gl/tests/ && echo *.h
IGNORE_HFILES += arg-nonnull.h c++defs.h intprops.h macros.h		\
	signature.h warn-on-use.h verify.h zerosize-ptr.h

# cd libextra/ && echo *.h
IGNORE_HFILES += config.h ext_inner_application.h openssl_compat.h

# cd libextra/gl/ && echo *.h
IGNORE_HFILES += hmac.h md5.h memxor.h

# Images to copy into HTML directory.
# e.g. HTML_IMAGES=$(top_srcdir)/gtk/stock-icons/stock_about_24.png
HTML_IMAGES=

# Extra SGML files that are included by $(DOC_MAIN_SGML_FILE).
# e.g. content_files=running.sgml building.sgml changes-2.0.sgml
content_files=

# SGML files where gtk-doc abbrevations (#GtkWidget) are expanded
# These files must be listed here *and* in content_files
# e.g. expand_content_files=running.sgml
expand_content_files=

# CFLAGS and LDFLAGS for compiling gtkdoc-scangobj with your library.
# Only needed if you are using gtkdoc-scangobj to dynamically query widget
# signals and properties.
# e.g. GTKDOC_CFLAGS=-I$(top_srcdir) -I$(top_builddir) $(GTK_DEBUG_FLAGS)
# e.g. GTKDOC_LIBS=$(top_builddir)/gtk/$(gtktargetlib)
GTKDOC_CFLAGS=
GTKDOC_LIBS=

# This includes the standard gtk-doc make rules, copied by gtkdocize.
include $(top_srcdir)/gtk-doc.make

# Other files to distribute
# e.g. EXTRA_DIST += version.xml.in
EXTRA_DIST +=

# Files not to distribute
# for --rebuild-types in $(SCAN_OPTIONS), e.g. $(DOC_MODULE).types
# for --rebuild-sections in $(SCAN_OPTIONS) e.g. $(DOC_MODULE)-sections.txt
#DISTCLEANFILES +=

# Comment this out if you want your docs-status tested during 'make check'
if ENABLE_GTK_DOC
#TESTS_ENVIRONMENT = cd $(srcsrc) &&
#TESTS = $(GTKDOC_CHECK)
endif
