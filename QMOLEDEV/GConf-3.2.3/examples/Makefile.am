INCLUDES=-I$(top_srcdir) -I$(top_builddir) \
	$(DEPENDENT_WITH_GTK_CFLAGS) \
        -DGCONF_SRCDIR=\""$(absolute_top_srcdir)"\" -DGCONF_SYSCONFDIR=\""$(sysgconfdir)"\"

EFENCE=

if GTK
GTK_EXAMPLES=basic-gconf-app simple-view simple-controller complex-gconf-app
else
GTK_EXAMPLES=
endif

noinst_PROGRAMS=$(GTK_EXAMPLES)

GTK_EXAMPLES_LINK= $(EFENCE) $(INTLLIBS) $(DEPENDENT_WITH_GTK_LIBS) $(top_builddir)/gconf/libgconf-$(MAJOR_VERSION).la $(EFENCE)

basic_gconf_app_SOURCES = basic-gconf-app.c

basic_gconf_app_LDADD = $(GTK_EXAMPLES_LINK)

simple_view_SOURCES = simple-view.c

simple_view_LDADD = $(GTK_EXAMPLES_LINK)

simple_controller_SOURCES = simple-controller.c

simple_controller_LDADD = $(GTK_EXAMPLES_LINK)

complex_gconf_app_SOURCES = basic-gconf-app.c

complex_gconf_app_LDADD = $(GTK_EXAMPLES_LINK)

EXTRA_DIST = basic-gconf-app.schemas

