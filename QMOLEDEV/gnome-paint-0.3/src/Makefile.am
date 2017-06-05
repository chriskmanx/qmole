## Process this file with automake to produce Makefile.in

## Created by Anjuta

AM_CPPFLAGS = \
	-DPACKAGE_LOCALE_DIR=\""$(prefix)/$(DATADIRNAME)/locale"\" \
	-DPACKAGE_SRC_DIR=\""$(srcdir)"\" \
	-DPACKAGE_DATA_DIR=\""$(datadir)"\" \
	$(GNOME_PAINT_CFLAGS)

AM_CFLAGS =\
	 -Wall\
	 -g

bin_PROGRAMS = \
	gnome-paint

EXTRA_DIST = $(glade_DATA)

gnome_paint_SOURCES = \
	color.c  \
	color.h  \
	main.c  \
	toolbar.c  \
	toolbar.h  \
	common.h  \
	cv_resize.c  \
	cv_resize.h  \
	cv_drawing.c  \
	cv_drawing.h  \
	cv_line_tool.c  \
	cv_line_tool.h  \
	file.c  \
	file.h  \
	pixbuf-file-chooser.c  \
	pixbuf-file-chooser.h  \
	pixmaps/line_0.xpm  \
	pixmaps/line_1.xpm  \
	pixmaps/line_2.xpm  \
	pixmaps/line_3.xpm  \
	pixmaps/line_4.xpm  \
	pixmaps/rect0.xpm  \
	pixmaps/rect1.xpm  \
	pixmaps/rect2.xpm  \
	pixmaps/sel1.xpm  \
	pixmaps/sel2.xpm  \
	cv_rectangle_tool.c  \
	cv_rectangle_tool.h  \
	cv_pencil_tool.c  \
	cv_pencil_tool.h  \
	cv_ellipse_tool.c  \
	cv_ellipse_tool.h  \
	cv_polygon_tool.c  \
	cv_polygon_tool.h  \
	undo.c  \
	undo.h  \
	gp-image.h  \
	gp-image.c  \
	cv_color_pick_tool.c  \
	cv_color_pick_tool.h  \
	cv_flood_fill_tool.c  \
	cv_flood_fill_tool.h  \
	pixbuf_util.c  \
	pixbuf_util.h  \
	gp_point_array.c  \
	gp_point_array.h  \
	cv_paintbrush_tool.c  \
	cv_paintbrush_tool.h  \
	cv_rounded_rectangle_tool.c  \
	cv_rounded_rectangle_tool.h  \
	cv_airbrush_tool.c  \
	cv_airbrush_tool.h  \
	cv_curve_tool.c  \
	cv_curve_tool.h  \
	color-picker.c  \
	color-picker.h  \
	clipboard.c  \
	clipboard.h  \
	cv_rect_select.c  \
	cv_rect_select.h

gnome_paint_CFLAGS = \
	-DG_DISABLE_DEPRECATED\
	-DG_DISABLE_SINGLE_INCLUDES\
	-DGDK_DISABLE_DEPRECATED\
	-DGTK_DISABLE_DEPRECATED\
	-DGDK_DISABLE_SINGLE_INCLUDES\
	-DGTK_DISABLE_SINGLE_INCLUDES

gnome_paint_LDFLAGS = \
	-Wl,--export-dynamic

gnome_paint_LDADD = \
	$(GNOME_PAINT_LIBS)

# Remove glade directory on uninstall
uninstall-local:
	-rm -r $(gladedir)
	-rm -r $(datadir)/gnome_paint
