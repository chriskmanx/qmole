FONTDIR=${libdir}/X11/fonts
FONTENCDEFS = -DFONT_ENCODINGS_DIRECTORY=\"$(FONTDIR)/encodings/encodings.dir\"

INCLUDES =			\
	-I${top_srcdir}/include \
	-I${top_srcdir}/include/X11/fonts \
	$(FONTENCDEFS)		

noinst_LTLIBRARIES = libfontfile.la

libfontfile_la_LIBADD = 	\
	$(Z_LIBS)

libfontfile_la_SOURCES = 	\
	bitsource.c		\
	bufio.c			\
	defaults.c		\
	dirfile.c		\
	encparse.c		\
	ffcheck.c		\
	fileio.c		\
	filewr.c		\
	fontdir.c		\
	fontenc.c		\
	fontencI.h		\
	fontencc.c		\
	fontfile.c		\
	fontscale.c		\
	gunzip.c		\
	printerfont.c		\
	register.c		\
	renderers.c
