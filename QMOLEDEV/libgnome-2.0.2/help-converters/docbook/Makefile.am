SUBDIRS= gnome-customization

INCLUDES = \
        -DGNOMELOCALEDIR=\""$(datadir)/locale"\"        \
	-DPREFIXDIR=\""$(prefix)"\" 			\
	$(HELP_DOCBOOK_MODULE_CFLAGS)			

libexec_PROGRAMS = gnome2-db2html

gnome2_db2html_SOURCES = \
	gnome-db2html3.c

gnome2_db2html_LDADD = \
	$(HELP_DOCBOOK_MODULE_LIBS)		
