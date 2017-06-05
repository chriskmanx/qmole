SUBDIRS = language-specs

if ENABLE_PROVIDERS
SUBDIRS += completion-providers
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
lib_LTLIBRARIES = libgtksourceview-2.0.la

libgtksourceview_headers =			\
	gtksourcebuffer.h			\
	gtksourceiter.h				\
	gtksourceview.h				\
	gtksourcelanguage.h			\
	gtksourcelanguagemanager.h		\
	gtksourcestyle.h			\
	gtksourcestyleschememanager.h		\
	gtksourcestylescheme.h			\
	gtksourcemark.h				\
	gtksourceprintcompositor.h		\
	gtksourcegutter.h			\
	gtksourceundomanager.h			\
	gtksourcecompletion.h			\
	gtksourcecompletioninfo.h		\
	gtksourcecompletionitem.h		\
	gtksourcecompletionproposal.h		\
	gtksourcecompletionprovider.h		\
	gtksourcecompletioncontext.h

NOINST_H_FILES = \
	gtksourcecompletionmodel.h	\
	gtksourcecompletion-private.h	\
	gtksourcecompletionutils.h	\
	gtksourcecompletionui.h		\
	gtksourcecontextengine.h	\
	gtksourceengine.h		\
	gtksourcegutter-private.h	\
	gtksourcelanguage-private.h	\
	gtksourcestyle-private.h	\
	gtksourceundomanagerdefault.h	\
	gtksourceview-i18n.h		\
	gtksourceview-utils.h		\
	gtktextregion.h

libgtksourceview_2_0_la_SOURCES = 	\
	gtksourcebuffer.c 		\
	gtksourceiter.c			\
	gtksourceview.c 		\
	gtksourceundomanager.c 		\
	gtksourceundomanagerdefault.c	\
	gtktextregion.c 		\
	gtksourcelanguage.c 		\
	gtksourcelanguagemanager.c 	\
	gtksourcelanguage-parser-1.c	\
	gtksourcelanguage-parser-2.c	\
	gtksourceview-i18n.c		\
	gtksourceview-utils.c 		\
	gtksourcestyle.c		\
	gtksourcestyleschememanager.c	\
	gtksourcestylescheme.c		\
	gtksourceengine.c		\
	gtksourcecontextengine.c	\
	gtksourcemark.c			\
	gtksourceprintcompositor.c	\
	gtksourcegutter.c		\
	gtksourcecompletion.c		\
	gtksourcecompletioninfo.c	\
	gtksourcecompletionitem.c	\
	gtksourcecompletionproposal.c	\
	gtksourcecompletionprovider.c	\
	gtksourcecompletionmodel.c	\
	gtksourcecompletionutils.c	\
	gtksourcecompletioncontext.c 	\
	$(libgtksourceview_headers)	\
	$(NOINST_H_FILES)

# do not distribute generated files
nodist_libgtksourceview_2_0_la_SOURCES =\
	$(BUILT_SOURCES)

completion_providers = 							\
	completion-providers/words/libgtksourcecompletionwords.la

libgtksourceview_2_0_la_LIBADD = $(DEP_LIBS) $(IGE_MAC_LIBS) $(completion_providers)
libgtksourceview_2_0_la_LDFLAGS = -no-undefined -export-symbols-regex "^gtk_source_.*"
libgtksourceview_2_0_includedir = $(includedir)/gtksourceview-2.0/gtksourceview

libgtksourceview_2_0_include_HEADERS =		\
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
stamp-gtksourceview-typebuiltins.h: Makefile.am $(libgtksourceview_headers)
	$(AM_V_GEN) (cd $(srcdir) \
	 && $(GLIB_MKENUMS) \
			--fhead "#ifndef __GTKSOURCEVIEW_TYPEBUILTINS_H__\n" \
			--fhead "#define __GTKSOURCEVIEW_TYPEBUILTINS_H__ 1\n\n" \
			--ftail "#endif /* __GTKSOURCEVIEW_TYPEBUILTINS_H__ */\n" \
			--fprod "#include <gtksourceview/@filename@>\n" \
			--eprod "G_BEGIN_DECLS\n" \
			--eprod "#define GTK_TYPE_@ENUMSHORT@ @enum_name@_get_type()\n" \
			--eprod "GType @enum_name@_get_type (void);\n" \
			--eprod "G_END_DECLS\n\n" \
		$(libgtksourceview_headers) ) > xgen-gth \
	&& (cmp -s xgen-gth gtksourceview-typebuiltins.h || cp xgen-gth gtksourceview-typebuiltins.h) \
	&& rm -f xgen-gth && echo stamp > $(@F)

EXTRA_DIST = \
	gtksourceview-marshal.list \
	$(ui_DATA)

CLEANFILES = $(BUILT_SOURCES) stamp-gtksourceview-typebuiltins.h stamp-gtksourceview-typebuiltins.c

-include $(top_srcdir)/git.mk
