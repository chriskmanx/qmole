AM_CPPFLAGS = $(LIBGSF_CFLAGS) $(PYTHON_INCLUDES) $(PYGTK_CFLAGS)

pygsfdir = $(pythondir)/gsf
pygsf_PYTHON = __init__.py

pygsfexecdir = $(pyexecdir)/gsf

CLEANFILES =
EXTRA_DIST = gsf-arg-types.py
pygsfexec_LTLIBRARIES =

pygsfexec_LTLIBRARIES += _gsfmodule.la

_gsfmodule_la_CFLAGS = 		\
	-I$(top_srcdir)		\
	$(LIBGSF_CFLAGS)
_gsfmodule_la_LDFLAGS = -module -avoid-version -export-symbols-regex init_gsf
_gsfmodule_la_LIBADD = $(top_builddir)/gsf/libgsf-1.la $(LIBGSF_LIBS)
_gsfmodule_la_SOURCES = gsfmodule.c
nodist__gsfmodule_la_SOURCES = gsf.c
CLEANFILES += gsf.c
EXTRA_DIST += gsf.defs gsf.override  
gsf.c: gsf.defs gsf.override gsf-arg-types.py

.defs.c:
	(cd $(srcdir) \
	 && $(PYGTK_CODEGEN) \
	    --load-types gsf-arg-types.py \
	    --override $*.override \
	    --prefix py$* $*.defs) > gen-$*.c \
	&& cp gen-$*.c $*.c \
	&& rm -f gen-$*.c
