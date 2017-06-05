
h_sources =  			\
	babl-base.h

c_sources =			\
	babl-base.c		\
	formats.c		\
	type-float.c		\
	type-u8.c		\
	type-u16.c		\
	type-u32.c		\
	model-rgb.c		\
	model-gray.c		\
	model-ycbcr.c

AM_CPPFLAGS = -I$(top_srcdir) -I$(top_srcdir)/babl

noinst_LTLIBRARIES= libbase.la
libbase_la_SOURCES= $(h_sources) $(c_sources) 
libbase_la_LIBADD = $(MATH_LIB)

EXTRA_DIST = 		\
	rgb-constants.h	\
	util.h

