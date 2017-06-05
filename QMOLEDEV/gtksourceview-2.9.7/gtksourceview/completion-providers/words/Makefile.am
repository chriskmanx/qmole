INCLUDES = 				\
	-DDATADIR=\""$(datadir)"\" 	\
	-DG_LOG_DOMAIN=\"GtkSourceView\"\
	-I$(top_srcdir) -I$(srcdir) 	\
	$(DISABLE_DEPRECATED_CFLAGS)	\
	$(WARN_CFLAGS) 			\
	$(DEP_CFLAGS)

noinst_LTLIBRARIES = libgtksourcecompletionwords.la

NOINST_H_FILES =				\
	gtksourcecompletionwordslibrary.h	\
	gtksourcecompletionwordsproposal.h	\
	gtksourcecompletionwordsbuffer.h	\
	gtksourcecompletionwordsutils.h

libgtksourcecompletionwords_headers = 		\
	gtksourcecompletionwords.h

libgtksourcecompletionwords_la_SOURCES =	\
	gtksourcecompletionwords.c		\
	gtksourcecompletionwordslibrary.c	\
	gtksourcecompletionwordsproposal.c	\
	gtksourcecompletionwordsbuffer.c	\
	gtksourcecompletionwordsutils.c		\
	$(libgtksourcecompletionwords_headers)	\
	$(NOINST_H_FILES)

libgtksourcecompletionwords_includedir = \
	$(includedir)/gtksourceview-2.0/gtksourceview/completion-providers/words

libgtksourcecompletionwords_include_HEADERS =		\
	$(libgtksourcecompletionwords_headers)

-include $(top_srcdir)/git.mk
