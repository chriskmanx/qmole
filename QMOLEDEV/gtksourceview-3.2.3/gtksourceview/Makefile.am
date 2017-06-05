if ENABLE_PROVIDERS
SUBDIRS = completion-providers
endif

INCLUDES = 				\
	-DDATADIR=\""$(datadir)"\" 	\
	-DG_LOG_DOMAIN=\"GtkSourceView\"\
	-I$(top_srcdir) -I$(srcdir) 	\
	$(DISABLE_DEPRECATED_CFLAGS)	\
	$(WARN_CFLAGS) 			\
	$(IGE_MAC_CFLAGS)		\
	$(DEP_CFLAGS)

BUILT_SOURCES = 			\
	gtksourceview-marshal.c 	\
	gtksourceview-marshal.h		\
	gtksourceview-typebuiltins.c	\
	gtksourceview-typebuiltins.h

# libs
lib_LTLIBRARIES = libgtksourceview-3.0.la

libgtksourceview_headers =			\
	gtksource.h				\
	gtksourcebuffer.h			\
	gtksourcecompletioncontext.h		\
	gtksourcecompletion.h			\
	gtksourcecompletioninfo.h		\
	gtksourcecompletionitem.h		\
	gtksourcecompletionproposal.h		\
	gtksourcecompletionprovider.h		\
	gtksourcegutter.h			\
	gtksourcegutterrenderer.h		\
	gtksourcegutterrenderertext.h		\
	gtksourcegutterrendererpixbuf.h		\
	gtksourcelanguage.h			\
	gtksourcelanguagemanager.h		\
	gtksourcemark.h				\
	gtksourcemarkattributes.h		\
	gtksourceprintcompositor.h		\
	gtksourcestyle.h			\
	gtksourcestylescheme.h			\
	gtksourcestyleschememanager.h		\
	gtksourceundomanager.h			\
	gtksourceview.h

NOINST_H_FILES = \
	gtksourcecompletionmodel.h		\
	gtksourcecompletion-private.h		\
	gtksourcecompletionui.h			\
	gtksourcecompletionutils.h		\
	gtksourcecontextengine.h		\
	gtksourceengine.h			\
	gtksourcegutter-private.h		\
	gtksourcelanguage-private.h		\
	gtksourcestyle-private.h		\
	gtksourceundomanagerdefault.h		\
	gtksourceview-i18n.h			\
	gtksourceview-utils.h			\
	gtktextregion.h				\
	gtksourcepixbufhelper.h			\
	gtksourcegutterrendererlines.h		\
	gtksourcegutterrenderermarks.h		\
	gtksourcegutterrenderer-private.h

libgtksourceview_c_files = \
	gtksourcebuffer.c 		\
	gtksourcecompletion.c		\
	gtksourcecompletioncontext.c	\
	gtksourcecompletioninfo.c	\
	gtksourcecompletionitem.c	\
	gtksourcecompletionmodel.c	\
	gtksourcecompletionproposal.c	\
	gtksourcecompletionprovider.c	\
	gtksourcecompletionutils.c	\
	gtksourcecontextengine.c	\
	gtksourceengine.c		\
	gtksourcegutter.c		\
	gtksourcegutterrenderer.c	\
	gtksourcegutterrenderertext.c	\
	gtksourcegutterrendererpixbuf.c	\
	gtksourcegutterrendererlines.c	\
	gtksourcegutterrenderermarks.c	\
	gtksourcepixbufhelper.c		\
	gtksourcelanguage.c 		\
	gtksourcelanguagemanager.c 	\
	gtksourcelanguage-parser-1.c	\
	gtksourcelanguage-parser-2.c	\
	gtksourcemark.c			\
	gtksourcemarkattributes.c	\
	gtksourceprintcompositor.c	\
	gtksourcestyle.c		\
	gtksourcestylescheme.c		\
	gtksourcestyleschememanager.c	\
	gtksourceundomanager.c 		\
	gtksourceundomanagerdefault.c	\
	gtksourceview.c 		\
	gtksourceview-i18n.c		\
	gtksourceview-utils.c 		\
	gtktextregion.c

libgtksourceview_3_0_la_SOURCES = 	\
	$(libgtksourceview_c_files)	\
	$(libgtksourceview_headers)	\
	$(NOINST_H_FILES)

# do not distribute generated files
nodist_libgtksourceview_3_0_la_SOURCES =\
	$(BUILT_SOURCES)

completion_providers = 							\
	completion-providers/words/libgtksourcecompletionwords.la

libgtksourceview_3_0_la_LIBADD = $(DEP_LIBS) $(IGE_MAC_LIBS) $(completion_providers)
libgtksourceview_3_0_la_LDFLAGS = -no-undefined -export-symbols-regex "^gtk_source_.*"
libgtksourceview_3_0_includedir = $(includedir)/gtksourceview-3.0/gtksourceview

libgtksourceview_3_0_include_HEADERS =		\
	$(libgtksourceview_headers)		\
	gtksourceview-typebuiltins.h

gtksourceview-marshal.h: gtksourceview-marshal.list $(GLIB_GENMARSHAL)
	$(AM_V_GEN) $(GLIB_GENMARSHAL) $< --header --prefix=_gtksourceview_marshal > $@

gtksourceview-marshal.c: gtksourceview-marshal.list $(GLIB_GENMARSHAL)
	$(AM_V_GEN) echo "#include \"gtksourceview-marshal.h\"" > $@ && \
	$(GLIB_GENMARSHAL) $< --body --prefix=_gtksourceview_marshal >> $@

gtksourceview-typebuiltins.c: stamp-gtksourceview-typebuiltins.c
	@true
stamp-gtksourceview-typebuiltins.c: Makefile.am $(libgtksourceview_headers)
	$(AM_V_GEN) (cd $(srcdir) \
	 && $(GLIB_MKENUMS) \
			--fhead "#include <glib-object.h>\n" \
			--fhead "#include \"gtksourceview-typebuiltins.h\"\n\n" \
			--fprod "\n/* enumerations from \"@filename@\" */" \
			--vhead "static const G@Type@Value _@enum_name@_values[] = {" \
			--vprod "  { @VALUENAME@, \"@VALUENAME@\", \"@valuenick@\" }," \
			--vtail "  { 0, NULL, NULL }\n};\n\n" \
			--vtail "GType\n@enum_name@_get_type (void)\n{\n" \
			--vtail "  static GType type = 0;\n\n" \
			--vtail "  if (!type)\n" \
			--vtail "    type = g_@type@_register_static (\"@EnumName@\", _@enum_name@_values);\n\n" \
			--vtail "  return type;\n}\n\n" \
		$(libgtksourceview_headers) ) > xgen-gtbec \
	&& (cmp -s xgen-gtbec gtksourceview-typebuiltins.c || cp xgen-gtbec gtksourceview-typebuiltins.c) \
	&& rm -f xgen-gtbec && echo stamp > $(@F)

gtksourceview-typebuiltins.h: stamp-gtksourceview-typebuiltins.h
	@true

# FIXME: we have to sed the file because glib-mkenums does
#        not recognize GTK_SOURCE as the prefix but instead GTK
stamp-gtksourceview-typebuiltins.h: Makefile.am $(libgtksourceview_headers)
	$(AM_V_GEN) (cd $(srcdir) \
	 && $(GLIB_MKENUMS) \
			--fhead "#ifndef __GTKSOURCEVIEW_TYPEBUILTINS_H__\n" \
			--fhead "#define __GTKSOURCEVIEW_TYPEBUILTINS_H__\n\n" \
			--ftail "#endif /* __GTKSOURCEVIEW_TYPEBUILTINS_H__ */\n" \
			--fprod "#include <gtksourceview/@filename@>\n" \
			--eprod "G_BEGIN_DECLS\n" \
			--eprod "#define GTK_TYPE_@ENUMSHORT@ @enum_name@_get_type()\n" \
			--eprod "GType @enum_name@_get_type (void);\n" \
			--eprod "G_END_DECLS\n\n" \
		$(libgtksourceview_headers) ) > xgen-gth \
	&& $(SED) -i -e 's/GTK_TYPE_SOURCE_/GTK_SOURCE_TYPE_/' xgen-gth \
	&& (cmp -s xgen-gth gtksourceview-typebuiltins.h || cp xgen-gth gtksourceview-typebuiltins.h) \
	&& rm -f xgen-gth && echo stamp > $(@F)

EXTRA_DIST = 					\
	gtksourceview-marshal.list

CLEANFILES = 					\
	$(BUILT_SOURCES)			\
	stamp-gtksourceview-typebuiltins.h	\
	stamp-gtksourceview-typebuiltins.c

libgtksourcecompletionwords_files =	\
	./completion-providers/words/gtksourcecompletionwords.h	\
	./completion-providers/words/gtksourcecompletionwords.c

if HAVE_INTROSPECTION
-include $(INTROSPECTION_MAKEFILE)
INTROSPECTION_GIRS = GtkSource-3.0.gir

GtkSource-3.0.gir: libgtksourceview-3.0.la
INTROSPECTION_SCANNER_ARGS =			\
	-I$(top_srcdir)				\
	--c-include=gtksourceview/gtksource.h	\
	--warn-all
GtkSource_3_0_gir_NAMESPACE = GtkSource
GtkSource_3_0_gir_VERSION = 3.0
GtkSource_3_0_gir_LIBS = libgtksourceview-3.0.la
GtkSource_3_0_gir_FILES =			\
	$(libgtksourcecompletionwords_files)	\
	$(libgtksourceview_headers)		\
	$(libgtksourceview_c_files)		\
	$(BUILT_SOURCES)

GtkSource_3_0_gir_INCLUDES = Gtk-3.0 Gdk-3.0
GtkSource_3_0_gir_PACKAGES = gtk+-3.0 gdk-3.0
GtkSource_3_0_gir_EXPORT_PACKAGES = gtksourceview-3.0

girdir = $(INTROSPECTION_GIRDIR)
gir_DATA = $(INTROSPECTION_GIRS)

typelibdir = $(INTROSPECTION_TYPELIBDIR)
typelib_DATA = $(INTROSPECTION_GIRS:.gir=.typelib)

CLEANFILES +=			\
	$(gir_DATA)	\
	$(typelib_DATA)
endif

-include $(top_srcdir)/git.mk
