/* -*- Mode: C; c-basic-offset: 4 -*-
 * Copyright (C) 1998-2001  James Henstridge <james@daa.com.au>
 * GNOME option parsing code by Miguel de Icaza.
 *
 * test-libglade.c: a test program for the libglade library
 *
 *   This program is free software; you can redistribute it and/or
 *   modify it under the terms of the GNU General Public License as
 *   published by the Free Software Foundation; either version 2 of
 *   the License, or (at your option) any later version.
 *
 *   This program is distributed in the hope that it will be useful,
 *   but WITHOUT ANY WARRANTY; without even the implied warranty of
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *   General Public License for more details.
 *
 *   You should have received a copy of the GNU General Public License
 *   along with this program; if not, write to the Free Software
 *   Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
 *   02111-1307 USA
 *
 *
 * At your option, you may use this alternative X style licence:
 *
 *   Permission is hereby granted, free of charge, to any person
 *   obtaining a copy of this software and associated documentation
 *   files (the "Software"), to deal in the Software without
 *   restriction, including without limitation the rights to use,
 *   copy, modify, merge, publish, distribute, sublicense, and/or sell
 *   copies of the Software, and to permit persons to whom the
 *   Software is furnished to do so, subject to the following
 *   conditions:
 *
 *   The above copyright notice and this permission notice shall be
 *   included in all copies or substantial portions of the Software.
 *
 *   THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
 *   EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
 *   OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
 *   NONINFRINGEMENT.  IN NO EVENT SHALL THE AUTHORS BE LIABLE FOR ANY
 *   CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF
 *   CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
 *   CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 *   SOFTWARE.
 */

#ifdef HAVE_CONFIG_H
#  include <config.h>
#endif
#include <string.h>
#include <stdlib.h>
#include <gtk/gtk.h>
#include <glade/glade.h>
#include <glade/glade-build.h>

#ifdef WITH_GNOME
#  include <libgnomeui/libgnomeui.h>
#endif

int
main (int argc, char **argv)
{
    const char *filename = NULL;
    const char *rootnode = NULL;
    gboolean no_connect = FALSE;

    int i;
    GladeXML *xml;

    /* initialise libraries */
    gtk_init(&argc, &argv);

#ifdef WITH_GNOME
    if (!gnome_program_init ("Terminal", VERSION,
			     LIBGNOMEUI_MODULE,
			     argc, argv,
			     NULL))
	g_error ("Cannot gnome_program_init ()");
#endif

    /* argument parsing */
    for (i = 1; i < argc; i++) {
	if (!strcmp(argv[i], "--no-connect"))
	    no_connect = TRUE;
	else if (filename == NULL)
	    filename = argv[i];
	else if (rootnode == NULL)
	    rootnode = argv[i];
	else {
	    g_print("Usage: %s [--no-connect] filename [rootnode]\n", argv[0]);
	    return 1;
	}
    }
    if (filename == NULL) {
	g_print("Usage: %s [--no-connect] filename [rootnode]\n", argv[0]);
	return 1;
    }

    /* construct the interface */
    xml = glade_xml_new(filename, rootnode, NULL);
    if (!xml) {
	g_warning("something bad happened while creating the interface");
	return 1;
    }

    /* if you pass the rootnode argument to libglade, it will only
     * construct widgets below that point. This shows how you might
     * want to integrate the constructed interface in your program. */
    if (rootnode) {
	GtkWidget *wid = glade_xml_get_widget(xml, rootnode);
	if (!GTK_IS_WINDOW(wid)) {
	    GtkWidget *win, *frame;

	    win = gtk_window_new(GTK_WINDOW_TOPLEVEL);
	    g_signal_connect(win, "destroy",
			     G_CALLBACK(gtk_main_quit), NULL);
	    gtk_container_set_border_width(GTK_CONTAINER(win), 5);
	    frame = gtk_frame_new(NULL);
	    gtk_container_set_border_width(GTK_CONTAINER(frame), 5);
	    gtk_container_add(GTK_CONTAINER(win), frame);
	    gtk_widget_show(frame);
	    gtk_container_add(GTK_CONTAINER(frame), wid);
	    gtk_widget_show(win);
	}
    }

    /* look up handler names in the symbol table and automatically
     * connect the signals. This requires that your program export
     * dynamic symbols.  If you link using libtool, passing the
     * -export-dynamic link flag will do this in a cross platform
     * manner.  For GNU ld, the --export-dynamic argument should
     * work.
     *
     * If you don't want to do symbol lookups like this, the
     * glade_xml_signal_connect() function allows you to connect
     * individual handlers up. */
    if (!no_connect)
	glade_xml_signal_autoconnect(xml);

    /* we have finished with the GladeXML object, so release our
     * reference to it.  This does not destroy all the widgets created
     * as part of the interface. */
    g_object_unref(G_OBJECT(xml));

    /* start the event loop */
    gtk_main();

    return 0;
}
