NULL=

idldir = $(datadir)/idl/bonobo-2.0

idl_DATA =				\
	Bonobo.idl			\
	Bonobo_Canvas.idl		\
	Bonobo_Context.idl		\
	Bonobo_Control.idl		\
	Bonobo_Embeddable.idl		\
	Bonobo_Gdk.idl			\
	Bonobo_ItemContainer.idl	\
	Bonobo_Listener.idl		\
	Bonobo_Moniker.idl		\
	Bonobo_Persist.idl		\
	Bonobo_Print.idl		\
	Bonobo_Property.idl		\
	Bonobo_Storage.idl		\
	Bonobo_UI.idl			\
	Bonobo_Zoomable.idl		\
	Bonobo_Exception.idl		\
	Bonobo_Clipboard.idl		\
	$(NULL)

EXTRA_DIST = $(idl_DATA)

orbittypelib_LTLIBRARIES = Bonobo_module.la
orbittypelibdir = $(libdir)/orbit-2.0

IMODULE_SOURCE =	\
	Bonobo.h	\
	Bonobo-common.c	\
	Bonobo-imodule.c

idl_flags = \
	-I$(top_srcdir)/idl \
	-I $(BONOBO_ACTIVATION_IDL_DIR) \
	-D__Bonobo_COMPILATION \
	-D__Bonobo_Unknown_COMPILATION \
	-D__Bonobo_GenericFactory_COMPILATION \
	-D__Bonobo_Activation_types_COMPILATION

INCLUDES =  \
	-I. \
	$(LIBBONOBO_CFLAGS)

Bonobo_module_la_SOURCES = \
	Bonobo-imodule.c

Bonobo_module_la_LIBADD = \
	$(LIBBONOBO_LIBS)

Bonobo_module_la_LDFLAGS = \
	-export-dynamic -module -avoid-version

$(IMODULE_SOURCE): $(idl_DATA) $(ORBIT_IDL)
	$(ORBIT_IDL) $(idl_flags) --imodule $(top_srcdir)/idl/Bonobo.idl

DONT_DIST_SOURCE = $(IMODULE_SOURCE)

dist-hook:
	for file in $(DONT_DIST_SOURCE) ; do \
	    rm -f $(distdir)/$$file ; \
	done
