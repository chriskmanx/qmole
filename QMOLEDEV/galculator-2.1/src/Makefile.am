## Process this file with automake to produce Makefile.in

AM_CPPFLAGS = \
	-DPACKAGE_LOCALE_DIR=\""$(datadir)/locale"\" \
	-DPACKAGE_UI_DIR=\""$(datadir)/galculator/ui"\" \
	-DPACKAGE_PIXMAPS_DIR=\""$(datadir)/pixmaps"\" \
	-DGDK_DISABLE_DEPRECATED \
	-DGTK_DISABLE_DEPRECATED \
	-DGSEAL_ENABLE

bin_PROGRAMS = galculator

galculator_SOURCES = \
	main.c galculator.h \
	math_functions.c math_functions.h \
	display.c display.h \
	general_functions.c general_functions.h \
	calc_basic.c calc_basic.h \
	config_file.c config_file.h \
	callbacks.c callbacks.h \
	ui.c ui.h \
	flex_parser.l flex_parser.h \
	g_real.h

galculator_CFLAGS = \
	$(PACKAGE_CFLAGS)

galculator_LDADD = \
	$(PACKAGE_LIBS) $(LIBM)

galculator_LDFLAGS = -export-dynamic

