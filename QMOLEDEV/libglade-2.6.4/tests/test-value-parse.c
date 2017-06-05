/* -*- mode: C; c-basic-offset: 4 -*- */
#include <gtk/gtk.h>
#include <glade/glade.h>
#include <glade/glade-build.h>

int
main(int argc, char **argv)
{
    if (!gtk_init_check(&argc, &argv)) {
	g_message("Could not init gtk.  Skipping test");
	return 77; /* magic value to ignore test */
    }

    g_print ("Testing string to enum... ");
    if (glade_enum_from_string(GTK_TYPE_ANCHOR_TYPE, "GTK_ANCHOR_NORTH_WEST")
	!= GTK_ANCHOR_NORTH_WEST) {
	g_print("failed\n");
	return 1;
    }
    g_print("passed\n");

    g_print ("Testing nickname string to enum... ");
    if (glade_enum_from_string(GTK_TYPE_ANCHOR_TYPE, "north-west")
	!= GTK_ANCHOR_NORTH_WEST) {
	g_print("failed\n");
	return 1;
    }
    g_print("passed\n");

    g_print ("Testing string to enum (single component)... ");
    if (glade_flags_from_string(GTK_TYPE_ATTACH_OPTIONS, "GTK_EXPAND")
	!= GTK_EXPAND) {
	g_print("failed\n");
	return 1;
    }
    g_print("passed\n");

    g_print ("Testing nicknamestring to enum (single component)... ");
    if (glade_flags_from_string(GTK_TYPE_ATTACH_OPTIONS, "expand")
	!= GTK_EXPAND) {
	g_print("failed\n");
	return 1;
    }
    g_print("passed\n");

    g_print ("Testing string to flag component ordering... ");
    if ((glade_flags_from_string(GTK_TYPE_ATTACH_OPTIONS, "expand|fill") !=
	 (GTK_EXPAND | GTK_FILL)) ||
	(glade_flags_from_string(GTK_TYPE_ATTACH_OPTIONS, "fill|expand") !=
	 (GTK_EXPAND | GTK_FILL))) {
	g_print("failed\n");
	return 1;
    }
    g_print("passed\n");

    g_print ("All tests passed\n");

    return 0;
}
