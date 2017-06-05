/*
 *  ui.c - general user interface code.
 *	part of galculator
 *  	(c) 2002-2013 Simon Flöry (simon.floery@rechenraum.com)
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU Library General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
 */
 
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "galculator.h"
#include "ui.h"
#include "display.h"
#include "math_functions.h"
#include "config_file.h"
#include "general_functions.h"
#include "callbacks.h"

#include <gtk/gtk.h>

GtkBuilder	*main_window_xml, *dispctrl_xml, *button_box_xml, *prefs_xml, *view_xml,
            *classic_view_xml, *paper_view_xml;
char		dec_point[2];
GtkListStore	*prefs_constant_store, *prefs_user_function_store;

static void set_disp_ctrl_object_data ();
static void set_all_buttons_property (GtkCallback func, gpointer data);
static void set_all_dispctrl_buttons_property (GtkCallback func, gpointer data);
static void set_all_normal_buttons_property (GtkCallback func, gpointer data);
static void set_table_child_callback (GtkWidget	*table_child, gpointer user_data);

/* active_buttons. bit mask, in which modes the corresponding button is active.
 * assume TRUE for all other bases/modes!
 */

s_active_buttons active_buttons[] = {\
	{"button_2", ~(AB_BIN)}, \
	{"button_3", ~(AB_BIN)}, \
	{"button_4", ~(AB_BIN)}, \
	{"button_5", ~(AB_BIN)}, \
	{"button_6", ~(AB_BIN)}, \
	{"button_7", ~(AB_BIN)}, \
	{"button_8", ~(AB_BIN | AB_OCT)}, \
	{"button_9", ~(AB_BIN | AB_OCT)}, \
	{"button_a", ~(AB_DEC | AB_BIN | AB_OCT)}, \
	{"button_b", ~(AB_DEC | AB_BIN | AB_OCT)}, \
	{"button_c", ~(AB_DEC | AB_BIN | AB_OCT)}, \
	{"button_d", ~(AB_DEC | AB_BIN | AB_OCT)}, \
	{"button_e", ~(AB_DEC | AB_BIN | AB_OCT)}, \
	{"button_f", ~(AB_DEC | AB_BIN | AB_OCT)}, \
	{"button_ee", AB_DEC}, \
	{"button_sin", AB_DEC}, \
	{"button_cos", AB_DEC}, \
	{"button_tan", AB_DEC}, \
	{"button_point", ~(AB_BIN | AB_OCT | AB_HEX)}, \
	{"button_sign", ~(AB_BIN | AB_OCT | AB_HEX)}, \
	{NULL}\
};

/* gtk_builder_file_open. opens a new .ui file, checks if this open was successful
 * and returns *GtkBuilder.
 */

static GtkBuilder *gtk_builder_file_open (char *filename, gboolean fatal)
{
	GtkBuilder	*xml = gtk_builder_new();
    GError* error = NULL;

    /* gtk_builder_set_translation_domain(xml, NULL); */
	if(!gtk_builder_add_from_file(xml, filename, &error))
    {
        /* if errors happen */
        g_object_unref(xml);
        xml = NULL;
        fprintf (stderr, _("[%s] Couldn't load %s. This file is necessary \
    to build galculator's user interface. Make sure you did a make install and the file \
    is accessible!\n"), PACKAGE, filename);
        if(error)
        {
            fprintf (stderr, "%s\n", error->message);
            g_error_free(error);
        }
        if (fatal == TRUE) exit(EXIT_FAILURE);
    }
	return xml;
}

/* apply_object_data. with gtk (better gobject) we can store additional
 * information for a widget (resp gobject). This information could be the
 * operation sign for src/calc_basic.c. This function sets such object data.
 * Is called from set_scientific_object_data resp set_basic_object_data.
 */

static void apply_object_data (s_operation_map operation_map[],
			s_gfunc_map gfunc_map[],
			s_function_map function_map[])
{
	int 		counter;
	gpointer	*func;
	GObject		*object;
	
	counter = 0;
	while (operation_map[counter].button_name != NULL) {
		object = G_OBJECT (gtk_builder_get_object (button_box_xml, 
			operation_map[counter].button_name));
		g_object_set_data (object, "operation",
			GINT_TO_POINTER(operation_map[counter].operation));
		g_object_set_data (object, "display_string",
			operation_map[counter].display_string);
		counter++;
	}
	
	counter = 0;
	while (gfunc_map[counter].button_name != NULL) {
		object = G_OBJECT (gtk_builder_get_object (button_box_xml, 
			gfunc_map[counter].button_name));
 		g_object_set_data (object, "display_string", gfunc_map[counter].display_string);
		g_object_set_data (object, "func", gfunc_map[counter].func);
		counter++;
	};
	
	counter = 0;
	while (function_map[counter].button_name != NULL) {
		func = (void *) malloc (sizeof (function_map[counter].func));
		memcpy (func, function_map[counter].func, sizeof (function_map[counter].func));
		object = G_OBJECT (gtk_builder_get_object (button_box_xml, 
			function_map[counter].button_name));
		g_object_set_data (object, "display_names", function_map[counter].display_names);
		g_object_set_data (object, "func", func);	
		counter++;
	};
}

/* set_scientific_object_data. Here, we set the information that is saved in
 * apply_object_data. For scientific mode.
 */

static void set_scientific_object_data ()
{
	s_operation_map	operation_map[] = {
		{"button_pow", "^", '^'},
		{"button_lsh", "<<", '<'},
		{"button_mod", "MOD", 'm'},
		{"button_and", "AND", '&'},
		{"button_or", "OR", '|'},
		{"button_xor", "XOR", 'x'},
		{"button_enter", "=", '='},
		{"button_plus", "+", '+'},
		{"button_minus", "-", '-'},
		{"button_mult", "*", '*'},
		{"button_div", "/", '/'},
		{"button_percent", "%", '%'},
		{"button_f1", "(", '('},	/* paropen or swapxy */
		{"button_f2", ")", ')'},	/* parclose or rolldn */
		{NULL}
	};
	
	s_gfunc_map gfunc_map[] = {
		{"button_sign", "-", display_result_toggle_sign},
		{"button_ee", "e", display_append_e},
		{"button_f1", "(", gfunc_f1},	/* paropen or swapxy */
		{"button_f2", ")", gfunc_f2},	/* parclose or rolldn */
		{NULL}
	};
	
	/* declare this one static as we need the display_names throughout */
	
	static s_function_map function_map[] = {
		{"button_sin", {"sin(", "asin(", "sinh(", "asinh("}, {sin_wrapper, asin_wrapper, G_SINH, G_ASINH}},
		{"button_cos", {"cos(", "acos(", "cosh(", "acosh("}, {cos_wrapper, acos_wrapper, G_COSH, G_ACOSH}},
		{"button_tan", {"tan(", "atan(", "tanh(", "atanh("}, {tan_wrapper, atan_wrapper, G_TANH, G_ATANH}},
		{"button_log", {"log(", "10^", "log(", "log("}, {G_LOG10, pow10y, G_LOG10, G_LOG10}},
		{"button_ln", {"ln(", "e^", "ln(", "ln("}, {G_LOG, G_EXP, G_LOG, G_LOG}},
		{"button_sq", {"^2", "sqrt(", "^2", "^2"}, {powx2, G_SQRT, powx2, powx2}},
		{"button_sqrt", {"sqrt(", "^2", "sqrt(", "sqrt("}, {G_SQRT, powx2, G_SQRT, G_SQRT}},
		{"button_fac", {"!", "!", "!", "!"}, {factorial, factorial, factorial, factorial}},
		{"button_cmp", {"~", "~", "~", "~"}, {cmp, cmp, cmp, cmp}},
		{NULL}
	};

	apply_object_data (operation_map, gfunc_map, function_map);
}

/* set_basic_object_data. Here, we set the information that is saved in
 * apply_object_data. For basic mode.
 */

static void set_basic_object_data ()
{
	s_operation_map	operation_map[] = {
		{"button_enter", "=", '='},
		{"button_plus", "+", '+'},
		{"button_minus", "-", '-'},
		{"button_mult", "*", '*'},
		{"button_div", "/", '/'},
		{"button_percent", "%", '%'},
		{"button_f1", "(", '('},	/* paropen or swapxy */
		{"button_f2", ")", ')'},	/* parclose or rolldn */
		{NULL}
	};
	
	s_gfunc_map gfunc_map[] = {
		{"button_sign", "-", display_result_toggle_sign},
		{"button_f1", "(", gfunc_f1},	/* paropen or swapxy */
		{"button_f2", ")", gfunc_f2},	/* parclose or rolldn */
		{NULL}
	};
	
	s_function_map function_map[] = {
		{"button_sqrt", {"sqrt", "^2", "sqrt", "sqrt"}, {G_SQRT, powx2, G_SQRT, G_SQRT}},
		{NULL}
	};
	
	apply_object_data (operation_map, gfunc_map, function_map);
}

/* set_disp_ctrl_object_data. Here, we set the information that is saved in
 * apply_object_data. For display control buttons.
 */

static void set_disp_ctrl_object_data ()
{
	int	counter=0;
	
	s_gfunc_map map[] = {\
		{"button_clr", NULL, clear},\
		{"button_backspace", NULL, backspace},\
		{"button_allclr", NULL, all_clear},\
		{NULL}\
	};

	while (map[counter].button_name != NULL) {
		g_object_set_data (G_OBJECT (gtk_builder_get_object (
			dispctrl_xml, map[counter].button_name)),
			"display_string", map[counter].display_string);
		g_object_set_data (G_OBJECT (gtk_builder_get_object (
			dispctrl_xml, map[counter].button_name)),
			"func", map[counter].func);
		counter++;
	};
}


static void free_accel_group(GtkWidget* widget, gpointer user_data)
{
    GtkAccelGroup* accel_group = GTK_ACCEL_GROUP(user_data);
    if(main_window_xml)
    {
        GtkWidget* toplevel = GTK_WIDGET(gtk_builder_get_object(main_window_xml, "main_window"));
        gtk_window_remove_accel_group(GTK_WINDOW(toplevel), accel_group);
    }
}

/* ui_pack_from_xml. This is a very special function. But we need it at least
 * three times. takes child_name from child_xml and adds it to box at index.
 * signals are connected and accel_group of accel_child_name is attached to
 * box's toplevel window.
 */

static void ui_pack_from_xml (GtkWidget *box, 
				int index, 
				GtkBuilder *child_xml, 
				char *child_name,
				gboolean expand,
				gboolean fill)
{
    GtkWidget	*child_widget=NULL;
    GtkWidget  *parent, *child_toplevel;
	GtkAccelGroup	*accel_group=NULL;
	GSList		*accel_list=NULL;
	
	/* at first connect signal handlers */
	gtk_builder_connect_signals (child_xml, NULL);
	/* next, get the "root" child */
    child_widget = GTK_WIDGET(gtk_builder_get_object (child_xml, child_name));

	/* we have to add the accel_group of child to the main_window in order
	 * to get working accelerators.
	 */
    parent = gtk_widget_get_parent(child_widget);
    g_assert(parent != NULL);
    child_toplevel = gtk_widget_get_toplevel(parent);
    accel_list = gtk_accel_groups_from_object(G_OBJECT(child_toplevel));
	if (accel_list)
    {
        accel_group = GTK_ACCEL_GROUP(accel_list->data);
        if (accel_group)
        {
            gtk_window_add_accel_group ((GtkWindow *) gtk_widget_get_toplevel (box), accel_group);
            /* remove the accel group when the child widget is destroyed */
            g_signal_connect(child_widget, "destroy", G_CALLBACK(free_accel_group), accel_group);
        }
    }

    /* reparent the child widget */
    g_object_ref(child_widget);
    gtk_container_remove(GTK_CONTAINER(parent), child_widget);
	gtk_box_pack_start ((GtkBox *) box, child_widget, expand, fill, 0);
    g_object_unref(child_widget);

    /* Destroy the toplevel dummy window so it won't be leaked. */
    if(parent)
        gtk_widget_destroy(child_toplevel);

	gtk_box_reorder_child ((GtkBox *) box, child_widget, index);

	gtk_widget_show (box);
}


/* ui_main_window_create. creates the main_window, containing menu toolbar and
 * the display skeleton. display control buttons and the calculator's buttons
 * are added by the callbacks for scientific resp basic mode.
 */

GtkWidget *ui_main_window_create ()
{
    GtkWidget* main_win;
	main_window_xml = gtk_builder_file_open (MAIN_GLADE_FILE, TRUE);
	/* connect the signals in the interface */
	gtk_builder_connect_signals(main_window_xml, NULL);
    main_win = GTK_WIDGET(gtk_builder_get_object (main_window_xml, "main_window"));
    /* the gtk builder xml object is freed in on_main_window_destroy() */
	return main_win;
}

#ifdef WITH_HILDON
void create_hildon_menu (HildonWindow *main_window)
{
    GtkWidget *main_menu;
    
    main_menu = GTK_WIDGET(gtk_builder_get_object (main_window_xml, "main_menu"));
    hildon_window_set_menu(HILDON_WINDOW(main_window), GTK_MENU(main_menu));
    gtk_widget_show_all(GTK_WIDGET(main_menu));
}
#endif

/* ui_main_window_set_dispctrl. we can't (un-)hide the dispctrl buttons as they
 * have the same key accelerators and thus only one button group would get
 * activated.
 */

void ui_main_window_set_dispctrl (int location)
{
	GtkWidget	*table_dispctrl, *box;
	s_signal_cb	signal_cb;
	/* destroy any existing display controls */
	if (dispctrl_xml) {
        table_dispctrl = GTK_WIDGET(gtk_builder_get_object (dispctrl_xml, "table_dispctrl"));
		if (table_dispctrl) gtk_widget_destroy (table_dispctrl); 
		g_object_unref (G_OBJECT(dispctrl_xml));
		dispctrl_xml = NULL;
	}
	/* now create the new one at location */
	switch(location) {
		case DISPCTRL_BOTTOM:
            box = GTK_WIDGET(gtk_builder_get_object (view_xml, "display_vbox"));
			dispctrl_xml = gtk_builder_file_open (DISPCTRL_BOTTOM_GLADE_FILE, TRUE);
			ui_pack_from_xml (box, 1, dispctrl_xml, "table_dispctrl", TRUE, TRUE);
			break;
		case DISPCTRL_RIGHT:
            box = GTK_WIDGET(gtk_builder_get_object (view_xml, "display_hbox"));
			dispctrl_xml = gtk_builder_file_open (DISPCTRL_RIGHT_GLADE_FILE, TRUE);
			ui_pack_from_xml (box, 1, dispctrl_xml, "table_dispctrl", FALSE, FALSE);
			break;
		case DISPCTRL_RIGHTV:
            box = GTK_WIDGET(gtk_builder_get_object (view_xml, "display_hbox"));
			dispctrl_xml = gtk_builder_file_open (DISPCTRL_RIGHTV_GLADE_FILE, TRUE);
			ui_pack_from_xml (box, 1, dispctrl_xml, "table_dispctrl", FALSE, FALSE);
			break;
		default:
			error_message ("Unknown location %i in \"ui_main_window_set_dispctrl\"", location);
	}
	set_disp_ctrl_object_data ();

	/* finally we connect that signal handler */
	
	signal_cb.detailed_signal = g_strdup ("button_press_event");
	signal_cb.callback = (GCallback) on_button_press_event;
	set_all_dispctrl_buttons_property (set_table_child_callback, (gpointer) &signal_cb);
	set_all_dispctrl_buttons_tip();
}

/* ui_main_window_buttons_destroy. removes the scientific resp basic mode 
 * buttons. display control buttons are not touched here!
 */

void ui_main_window_buttons_destroy ()
{
	GtkWidget	*box;
	
	if (!button_box_xml) return;
    box = GTK_WIDGET(gtk_builder_get_object (button_box_xml, "button_box"));
	if (box) gtk_widget_destroy (box);
    g_object_unref(button_box_xml);
    button_box_xml = NULL;
}

/* ui_main_window_buttons_create. fills main_window with calculator's buttons,
 * paying respect to current mode. dispctrl buttons need to be done extra.
 */

void ui_main_window_buttons_create (int mode)
{
	GtkWidget	*box, *button;
	s_signal_cb	signal_cb;

	switch (mode) {
	case BASIC_MODE:
		if (button_box_xml) g_object_unref (G_OBJECT(button_box_xml));
		button_box_xml = gtk_builder_file_open (BASIC_GLADE_FILE, TRUE);
        box = GTK_WIDGET(gtk_builder_get_object (view_xml, "classic_view_vbox"));
		ui_pack_from_xml (box, 2, button_box_xml, "button_box", TRUE, TRUE);
		set_basic_object_data (button_box_xml);
		break;
	case SCIENTIFIC_MODE:
		if (button_box_xml) g_object_unref (G_OBJECT(button_box_xml));
		button_box_xml = gtk_builder_file_open (SCIENTIFIC_GLADE_FILE, TRUE);
        box = GTK_WIDGET(gtk_builder_get_object (view_xml, "classic_view_vbox"));
		ui_pack_from_xml (box, 2, button_box_xml, "button_box", TRUE, TRUE);
		set_scientific_object_data (button_box_xml);
		break;
	case PAPER_MODE:
		return;
	default:
		error_message ("Unknown mode in \"ui_main_window_buttons_create\"");
	}

	signal_cb.detailed_signal = g_strdup ("button_press_event");
	signal_cb.callback = (GCallback) on_button_press_event;
	set_all_normal_buttons_property (set_table_child_callback, (gpointer) &signal_cb);
	
	/* update "decimal point" button to locale's decimal point */
	dec_point[0] = getDecPoint();
	dec_point[1] = '\0';
    gtk_button_set_label ((GtkButton *) GTK_WIDGET(gtk_builder_get_object (
        button_box_xml, "button_point")), dec_point);
        
	/* disable mr and m+ button if there is nothing to display */
    button = GTK_WIDGET(gtk_builder_get_object (button_box_xml, "button_MR"));
	gtk_widget_set_sensitive (button, memory.len > 0);
    button = GTK_WIDGET(gtk_builder_get_object (button_box_xml, "button_Mplus"));
	gtk_widget_set_sensitive (button, memory.len > 0);
	
	/* apply button specific prefs */
	set_all_normal_buttons_size (prefs.button_width, prefs.button_height);
	set_all_normal_buttons_font (prefs.custom_button_font ? prefs.button_font : "");
	set_all_normal_buttons_tip ();
}

/* set_table_child_callback. Function argument for set_all_*_buttons_property.
 * sets the size.
 */

static void set_table_child_callback (GtkWidget	*table_child, gpointer user_data)
{
	s_signal_cb	*signal_cb;
	signal_cb = user_data;
	g_signal_connect (table_child, signal_cb->detailed_signal, 
		signal_cb->callback, NULL);
}

/* set_table_child_size. Function argument for set_all_buttons_property.
 * sets the size.
 */

static void set_table_child_size (GtkWidget	*table_child, gpointer user_data)
{
	GtkRequisition	*size;
	size = user_data;				/* dereference */
	gtk_widget_set_size_request (table_child, size->width, size->height);
}

/* set_table_child_font. Function argument for set_all_buttons_property.
 * sets the font of buttons. We have to set the label of the button!
 */

static void set_table_child_font (GtkWidget		*w, gpointer user_data)
{
	PangoFontDescription	*font;

	font = user_data;				/* dereference */
	
	if (GTK_IS_BIN (w))
		w = gtk_bin_get_child (GTK_BIN(w));
	/* if it's a normal button, w is now the label we want to font-change.
	 * if it's a popup button, we have to get the most left child first.
	 */
	if (GTK_IS_BOX (w))
    {
        GList* children = gtk_container_get_children(GTK_CONTAINER(w));
        if(children)
        {
            w = GTK_WIDGET(children->data);
            g_list_free(children);
        }
        else
            w = NULL;
    }
	if (GTK_IS_LABEL(w))
    {
#if GTK_CHECK_VERSION(3, 0, 0)
        gtk_widget_override_font(w, font);
#else
        gtk_widget_modify_font (w, font);
#endif
    }
	/* else do nothing */
}

gboolean set_table_child_tip_accel_finder (GtkAccelKey *key, GClosure *closure, gpointer data)
{
	gpointer	*d;
	GClosure	*c;
	GtkWidget* button;
	gchar		*tt, *al, *new_tt;

	d = (gpointer *) data;
	c = (GClosure *) d[0];
	button = GTK_WIDGET(d[1]);
	if (closure == c) {
		tt = gtk_widget_get_tooltip_text(button);
		al = gtk_accelerator_get_label(key->accel_key, key->accel_mods);
		new_tt = g_strdup_printf("%s    %s", tt, al);
        gtk_widget_set_tooltip_text(button, new_tt);
        g_free(new_tt);
		g_free(tt);
		g_free(al);
		return TRUE;
	}
	return FALSE;
}


static void set_table_child_tip_accel (GtkWidget* button, gpointer user_data)
{
	GList 		*closure_list;
	GtkAccelGroup	*accel_group;
	gpointer	d[2];
	
	/* get all accelerators (== closures) connected to this button */
	closure_list = gtk_widget_list_accel_closures (button);
	if (!closure_list) return;
	if (!closure_list->data) return;
	/* we head for first closure */
	accel_group = gtk_accel_group_from_accel_closure(closure_list->data);
	if (!accel_group) return;
	d[0] = (gpointer) closure_list->data;
	d[1] = (gpointer) button;
	gtk_accel_group_find(accel_group, (GtkAccelGroupFindFunc)set_table_child_tip_accel_finder, d);
}

/* set_all_dispctrl_buttons_property. calls func with argument data for 
 * every button of the display control section.
 */

static void set_all_dispctrl_buttons_property (GtkCallback func, gpointer data)
{
	GtkTable	*table;
    GList* table_children;

	if (!dispctrl_xml) return;
	/* at first the display control table. always there; somehow */
    table = (GtkTable *) GTK_WIDGET(gtk_builder_get_object (dispctrl_xml, "table_dispctrl"));
	if (!table) return;
    
    table_children = gtk_container_get_children(GTK_CONTAINER(table));
	/* dispctrl_right has an extra table for cosmetic reasons. */
	if (GTK_IS_TABLE (table_children->data))
		table = GTK_TABLE(table_children->data);
    g_list_free(table_children);
	gtk_container_foreach (GTK_CONTAINER(table), (GtkCallback)func, data);
}

/* set_all_dispctrl_buttons_property. calls func with argument data for 
 * every button of the "calculator" section.
 */

static void set_all_normal_buttons_property (GtkCallback func, gpointer data)
{
	GtkTable	*table;
	
	/* now depending on mode the remaining buttons */
	switch (prefs.mode) {
	case BASIC_MODE:
        table = (GtkTable *) GTK_WIDGET(gtk_builder_get_object (button_box_xml, "table_buttons"));
		gtk_container_foreach (GTK_CONTAINER(table), (GtkCallback)func, data);
		break;
	case SCIENTIFIC_MODE:
        table = (GtkTable *) GTK_WIDGET(gtk_builder_get_object (button_box_xml, "table_standard_buttons"));
		gtk_container_foreach (GTK_CONTAINER(table), (GtkCallback)func, data);
        table = (GtkTable *) GTK_WIDGET(gtk_builder_get_object (button_box_xml, "table_bin_buttons"));
		gtk_container_foreach (GTK_CONTAINER(table), (GtkCallback)func, data);
        table = (GtkTable *) GTK_WIDGET(gtk_builder_get_object (button_box_xml, "table_func_buttons"));
		gtk_container_foreach (GTK_CONTAINER(table), (GtkCallback)func, data);
		break;
	case PAPER_MODE:
		/* do nothing - no buttons */
		break;
	default:
		error_message ("Unknown mode %i in \"set_all_normal_buttons_property\"", prefs.mode);
	}
}

/* set_all_buttons_property. calls func with argument data for every button.
 */

static void set_all_buttons_property (GtkCallback func, gpointer data)
{
	set_all_dispctrl_buttons_property (func, data);
	set_all_normal_buttons_property (func, data);
}

/* set_all_buttons_size. gateway for set_all_buttons_property.
 */

void set_all_buttons_size (int width, int height)
{
	GtkRequisition	size;
	
	size.width = width;
	size.height = height;
		
	set_all_buttons_property (set_table_child_size, (gpointer) &size);
		
	gtk_window_resize ((GtkWindow *) gtk_widget_get_toplevel(
            GTK_WIDGET(gtk_builder_get_object (main_window_xml, "main_window"))), 1, 1);
}

void set_all_normal_buttons_tip ()
{
	set_all_normal_buttons_property (set_table_child_tip_accel, NULL);
}

void set_all_dispctrl_buttons_tip ()
{
	set_all_dispctrl_buttons_property (set_table_child_tip_accel, NULL);
}

/* set_all_nomral_buttons_size. gateway for set_all_normal_buttons_property.
 */

void set_all_normal_buttons_size (int width, int height)
{
	GtkRequisition	size;
	
	size.width = width;
	size.height = height;
	set_all_normal_buttons_property (set_table_child_size, (gpointer) &size);
}

/* set_all_dispctrl_buttons_size. gateway for set_all_dispctrl_buttons_property.
 */

void set_all_dispctrl_buttons_size (int width, int height)
{
	GtkRequisition	size;
	
	size.width = width;
	size.height = height;
	set_all_dispctrl_buttons_property (set_table_child_size, (gpointer) &size);
}

/* set_all_buttons_font. gateway for set_all_buttons_property.
 */

void set_all_buttons_font (char *font_string)
{
	PangoFontDescription	*pango_font;

	pango_font = pango_font_description_from_string (font_string);
	set_all_buttons_property (set_table_child_font, pango_font);
}

/* set_all_normal_buttons_font. gateway for set_all_buttons_property.
 */

void set_all_normal_buttons_font (char *font_string)
{
	PangoFontDescription	*pango_font;

	pango_font = pango_font_description_from_string (font_string);
	set_all_normal_buttons_property (set_table_child_font, pango_font);
}

/* set_all_dispctrl_buttons_font. gateway for set_all_buttons_property.
 */

void set_all_dispctrl_buttons_font (char *font_string)
{
	PangoFontDescription	*pango_font;

	pango_font = pango_font_description_from_string (font_string);
	set_all_dispctrl_buttons_property (set_table_child_font, pango_font);
}

gboolean button_deactivation (gpointer data)
{
	GtkToggleButton 	*b;
	
	b = (GtkToggleButton*) data;
	gtk_toggle_button_set_active (b, FALSE);
	return FALSE;	
}

void button_activation (GtkToggleButton *b)
{
	g_timeout_add (100, button_deactivation, (gpointer) b);
}

void update_active_buttons (int number_base, int notation_mode)
{
	int		counter=0;
	GtkWidget	*current_button;
	unsigned int	state;
	
	/* state = (1 << number_base) | (1 << (notation_mode + NR_NUMBER_BASES)); */
	state = 1 << number_base;
	while (active_buttons[counter].button_name != NULL) {
        current_button = GTK_WIDGET(gtk_builder_get_object (button_box_xml, active_buttons[counter].button_name));
		if (current_button == NULL) {
			counter++;
			continue;
		}
		gtk_widget_set_sensitive (current_button, 
			(active_buttons[counter].mask & state) == state);
		counter++;
	}
}

void update_dispctrl()
{
	/* just put one here and hide it afterwards. we need the button
			for working key accelerators. */
	if (prefs.mode == BASIC_MODE) 
		ui_main_window_set_dispctrl (DISPCTRL_BOTTOM);
	else if (current_status.notation == CS_RPN)
		ui_main_window_set_dispctrl (DISPCTRL_RIGHTV);
	else ui_main_window_set_dispctrl (DISPCTRL_RIGHT);
	set_widget_visibility (dispctrl_xml, "table_dispctrl", 
		prefs.vis_dispctrl);
	set_all_dispctrl_buttons_size (prefs.button_width, prefs.button_height);
	set_all_dispctrl_buttons_font (prefs.custom_button_font ? prefs.button_font : "");

}

/*
 * (un) hide a widget
 */ 

void set_widget_visibility (GtkBuilder *xml, char *widget_name, gboolean visible)
{
	GtkWidget	*widget;
	/* g_print("set visible: %s: %d\n", widget_name, visible); */
    widget = GTK_WIDGET(gtk_builder_get_object (xml, widget_name));
	if (!widget) {
		error_message ("Couldn't find widget \"%s\" in \"set_widget_visibility\"", widget_name);
		return;
	}
	if (visible) gtk_widget_show (widget);
	else gtk_widget_hide (widget);
}

/* menu code - e.g. used for the constant popup menu */

#if !GTK_CHECK_VERSION(2, 18, 0)
#define gtk_widget_get_visible(widget)  GTK_WIDGET_VISIBLE(widget)
#endif

void position_menu (GtkMenu *menu, 
		gint *x, 
		gint *y, 
		gboolean *push_in, 
		gpointer user_data)
{
	/* this code is taken from GTK 2.2.1 source, therefore credits go there.
	 *  gtk+-2.0.6/gtk/gtkoptionmenu.c (function gtk_option_menu_position)
	 * modified to fit our button menu widget.
	 */
	GtkWidget *child;
	GtkWidget *widget;
    GtkAllocation allocation;
	GList *children, *l;
	gint screen_width;
	gint menu_xpos;
	gint menu_ypos;
	gint menu_width;
	
	g_return_if_fail (GTK_IS_BUTTON (user_data));
	
	widget = GTK_WIDGET (user_data);
	
    GtkRequisition requisition;
#if GTK_CHECK_VERSION(3, 0, 0)
        /* see this patch: http://osdir.com/ml/general/2010-10/msg06642.html */
        gtk_widget_get_preferred_size (GTK_WIDGET (menu), &requisition, NULL);
#else
        gtk_widget_get_child_requisition (GTK_WIDGET (menu), &requisition);
#endif
    menu_width = requisition.width;

	/* i guess we don't need the "active" stuff from the original positioning
		code. we don't have any active items
	 */
	 
	gdk_window_get_origin (gtk_widget_get_window(widget), &menu_xpos, &menu_ypos);
    gtk_widget_get_allocation(widget, &allocation);

	menu_xpos += allocation.x;
	menu_ypos += allocation.y + allocation.height / 2 - 2;
	
	children = gtk_container_get_children(GTK_CONTAINER(menu));
	for(l = children; l; l=l->next) {
		child = GTK_WIDGET(l->data);
		if (gtk_widget_get_visible (child))	{
#if GTK_CHECK_VERSION(3, 0, 0)
            /* see above */
            gtk_widget_get_preferred_size (child, &requisition, NULL);
#else
            gtk_widget_get_child_requisition (child, &requisition);
#endif
			menu_ypos -= requisition.height;
		}
	}
    g_list_free(children);
	
	/*screen_width = gdk_screen_get_width (gtk_widget_get_screen (widget));*/
	screen_width = gdk_screen_width ();
	
	if (menu_xpos < 0) menu_xpos = 0;
	else if ((menu_xpos + menu_width) > screen_width)
		menu_xpos -= ((menu_xpos + menu_width) - screen_width);
	
	*x = menu_xpos;
	*y = menu_ypos;
	*push_in = TRUE;
}

GtkWidget *ui_user_functions_menu_create (s_user_function *user_function, GCallback user_function_handler)
{
	GtkWidget	*menu, *child;
	int		counter=0;
	char		*label;
	
	menu = gtk_menu_new();
	while (user_function[counter].name != NULL) {
		label = g_strdup_printf ("%s(%s) = %s", user_function[counter].name, 
			user_function[counter].variable, 
			user_function[counter].expression);
		child = gtk_menu_item_new_with_label(label);
		g_free (label);
		gtk_menu_shell_append ((GtkMenuShell *) menu, child);
		gtk_widget_show (child);
		g_signal_connect (G_OBJECT (child), "activate", user_function_handler, GINT_TO_POINTER(counter));
		counter++;
	}
	return menu;
}

GtkWidget *ui_constants_menu_create (s_constant *constant, GCallback const_handler)
{
	GtkWidget	*menu, *child;
	int		counter=0;
	char		*label;
	
	menu = gtk_menu_new();
	while (constant[counter].name != NULL) {
		label = g_strdup_printf ("%s: %s (%s)", constant[counter].name, constant[counter].value, constant[counter].desc);
		child = gtk_menu_item_new_with_label(label);
		g_free (label);
		gtk_menu_shell_append ((GtkMenuShell *) menu, child);
		gtk_widget_show (child);
		g_signal_connect (G_OBJECT (child), "activate", const_handler, constant[counter].value);
		counter++;
	}
	return menu;
}

GtkWidget *ui_memory_menu_create (s_array memory, GCallback memory_handler, char *last_item)
{
	GtkWidget	*menu, *child;
	int		counter=0;
	char		*label;
	
	menu = gtk_menu_new();
	for (counter = 0; counter < memory.len; counter++) {
		label = float2string("%"G_LMOD"f", memory.data[counter]);
		child = gtk_menu_item_new_with_label(label);
		g_free (label);
		gtk_menu_shell_append ((GtkMenuShell *) menu, child);
		gtk_widget_show (child);
		g_signal_connect (G_OBJECT (child), "activate", memory_handler, GINT_TO_POINTER(counter));
	}
	if (last_item != NULL) {
		label = g_strdup (last_item);
		child = gtk_menu_item_new_with_label(label);
		g_free (label);
		gtk_menu_shell_append ((GtkMenuShell *) menu, child);
		gtk_widget_show (child);
		g_signal_connect (G_OBJECT (child), "activate", memory_handler, GINT_TO_POINTER(counter));
	}
	return menu;
}

GtkWidget *ui_right_mouse_menu_create ()
{
	GtkWidget	*menu, *menu_item;
	
	menu = gtk_menu_new();
	
	menu_item = gtk_check_menu_item_new_with_mnemonic (_("Show _menu bar"));
	gtk_check_menu_item_set_active ((GtkCheckMenuItem *) menu_item, prefs.show_menu);
	gtk_menu_shell_append ((GtkMenuShell *) menu, menu_item);
	gtk_widget_show (menu_item);
	g_signal_connect (G_OBJECT (menu_item), "toggled", (GCallback) on_show_menubar1_toggled, NULL);
	
	return menu;
}

GtkWidget *ui_pref_dialog_create ()
{
	int			counter=0;
	GtkWidget		*w, *prefs_dialog;
	GtkTreeIter   		iter;
	GtkWidget		*tree_view;
	GtkCellRenderer 	*renderer;
	GtkTreeViewColumn 	*column;
	GtkTreeSelection	*select;
	GtkSizeGroup		*sgroup;
	s_prefs_entry		*prefs_list;
	
	prefs_xml = gtk_builder_file_open (PREFS_GLADE_FILE, FALSE);
	gtk_builder_connect_signals(prefs_xml, NULL);
	
    prefs_dialog = GTK_WIDGET(gtk_builder_get_object (prefs_xml, "prefs_dialog"));
    
    /* Set maximal number of bits for the integer types depending on whether
	 * float128 is available or not.
	 */
	
#if HAVE_LIBQUADMATH
	gdouble upperBound = 112;
#else
	gdouble upperBound = 32;
#endif

	gtk_adjustment_set_upper(gtk_spin_button_get_adjustment(GTK_SPIN_BUTTON(gtk_builder_get_object (prefs_xml, "prefs_hex_bits"))), upperBound);
	gtk_adjustment_set_upper(gtk_spin_button_get_adjustment(GTK_SPIN_BUTTON(gtk_builder_get_object (prefs_xml, "prefs_oct_bits"))), upperBound);
	gtk_adjustment_set_upper(gtk_spin_button_get_adjustment(GTK_SPIN_BUTTON(gtk_builder_get_object (prefs_xml, "prefs_bin_bits"))), upperBound);
	
	gtk_window_set_title ((GtkWindow *)prefs_dialog, g_strdup_printf (_("%s Preferences"), PACKAGE));
	
	/* 
	 * fill gui with current preferences settings 
	 */
	 
	prefs_list = config_file_get_prefs_list();
	while (prefs_list[counter].key != NULL) {
		if (prefs_list[counter].set_handler != NULL) {
			prefs_list[counter].set_handler (
				prefs_xml,
				prefs_list[counter].widget_name,
				prefs_list[counter].variable);
		}
		counter++;
	}

    w = GTK_WIDGET(gtk_builder_get_object (prefs_xml, "prefs_button_font_label"));
	gtk_widget_set_sensitive (w, prefs.custom_button_font);
	
    w = GTK_WIDGET(gtk_builder_get_object (prefs_xml, "prefs_button_font"));
	gtk_widget_set_sensitive (w, prefs.custom_button_font);
	
    w = GTK_WIDGET(gtk_builder_get_object (prefs_xml, "prefs_bin_length"));
	gtk_widget_set_sensitive (w, prefs.bin_fixed);
	
	/* make user defined constants list. */
	
	prefs_constant_store = gtk_list_store_new (NR_CONST_COLUMNS, 
		G_TYPE_STRING, G_TYPE_STRING, G_TYPE_STRING);
	counter = 0;
	while (constant[counter].name != NULL) {
		gtk_list_store_append (prefs_constant_store, &iter);
		gtk_list_store_set (prefs_constant_store, &iter, 
			CONST_NAME_COLUMN, constant[counter].name, 
			CONST_VALUE_COLUMN, constant[counter].value, 
			CONST_DESC_COLUMN, constant[counter].desc,
			-1);
		counter++;
	}
    tree_view = GTK_WIDGET(gtk_builder_get_object (prefs_xml, "constant_treeview"));
	gtk_tree_view_set_model ((GtkTreeView *) tree_view, 
		GTK_TREE_MODEL (prefs_constant_store));
	renderer = gtk_cell_renderer_text_new ();
	column = gtk_tree_view_column_new_with_attributes (_("Name"), renderer, 
		"text", CONST_NAME_COLUMN, NULL);
	gtk_tree_view_append_column (GTK_TREE_VIEW (tree_view), column);
	column = gtk_tree_view_column_new_with_attributes (_("Value"), renderer, 
		"text", CONST_VALUE_COLUMN, NULL);
	gtk_tree_view_append_column (GTK_TREE_VIEW (tree_view), column);
	column = gtk_tree_view_column_new_with_attributes (_("Description"), renderer, 
		"text", CONST_DESC_COLUMN, NULL);
	gtk_tree_view_append_column (GTK_TREE_VIEW (tree_view), column);
	select = gtk_tree_view_get_selection (GTK_TREE_VIEW (tree_view));
	gtk_tree_selection_set_mode (select, GTK_SELECTION_SINGLE);
	g_signal_connect (G_OBJECT (select), "changed",
                  G_CALLBACK (const_list_selection_changed_cb),
                  NULL);
	
	/* make user defined function list */

	prefs_user_function_store = gtk_list_store_new (NR_UFUNC_COLUMNS, G_TYPE_STRING, G_TYPE_STRING, G_TYPE_STRING);
	counter = 0;
	while (user_function[counter].name != NULL) {
		gtk_list_store_append (prefs_user_function_store, &iter);
		gtk_list_store_set (prefs_user_function_store, &iter, 
			UFUNC_NAME_COLUMN, user_function[counter].name, 
			UFUNC_VARIABLE_COLUMN, user_function[counter].variable, 
			UFUNC_EXPRESSION_COLUMN, user_function[counter].expression,
			-1);
		counter++;
	}
    tree_view = GTK_WIDGET(gtk_builder_get_object (prefs_xml, "user_function_treeview"));
	gtk_tree_view_set_model ((GtkTreeView *) tree_view, 
		GTK_TREE_MODEL (prefs_user_function_store));
	renderer = gtk_cell_renderer_text_new ();
	column = gtk_tree_view_column_new_with_attributes (_("Name"), renderer, 
		"text", UFUNC_NAME_COLUMN, NULL);
	gtk_tree_view_append_column (GTK_TREE_VIEW (tree_view), column);
	column = gtk_tree_view_column_new_with_attributes (_("Variable"), renderer, 
		"text", UFUNC_VARIABLE_COLUMN, NULL);
	gtk_tree_view_append_column (GTK_TREE_VIEW (tree_view), column);
	column = gtk_tree_view_column_new_with_attributes (_("Expression"), renderer, 
		"text", UFUNC_EXPRESSION_COLUMN, NULL);
	gtk_tree_view_append_column (GTK_TREE_VIEW (tree_view), column);
	select = gtk_tree_view_get_selection (GTK_TREE_VIEW (tree_view));
	gtk_tree_selection_set_mode (select, GTK_SELECTION_SINGLE);
	g_signal_connect (G_OBJECT (select), "changed",
                  G_CALLBACK (user_function_list_selection_changed_cb),
                  NULL);	
	
	/* pack widget of same size into GtkSizeGroup */

	sgroup = gtk_size_group_new (GTK_SIZE_GROUP_HORIZONTAL);
    gtk_size_group_add_widget (sgroup, GTK_WIDGET(
        gtk_builder_get_object (prefs_xml, "prefs_button_font_label")));
    gtk_size_group_add_widget (sgroup, GTK_WIDGET(
        gtk_builder_get_object (prefs_xml, "prefs_button_width_label")));
    gtk_size_group_add_widget (sgroup, GTK_WIDGET(
        gtk_builder_get_object (prefs_xml, "prefs_button_height_label")));
	
	sgroup = gtk_size_group_new (GTK_SIZE_GROUP_HORIZONTAL);
    gtk_size_group_add_widget (sgroup, GTK_WIDGET(
        gtk_builder_get_object (prefs_xml, "prefs_const_add_button")));
    gtk_size_group_add_widget (sgroup, GTK_WIDGET(
        gtk_builder_get_object (prefs_xml, "prefs_const_update_button")));
    gtk_size_group_add_widget (sgroup, GTK_WIDGET(
        gtk_builder_get_object (prefs_xml, "prefs_const_delete_button")));
    gtk_size_group_add_widget (sgroup, GTK_WIDGET(
        gtk_builder_get_object (prefs_xml, "prefs_const_clear_button")));
	
	sgroup = gtk_size_group_new (GTK_SIZE_GROUP_HORIZONTAL);
    gtk_size_group_add_widget (sgroup, GTK_WIDGET(
        gtk_builder_get_object (prefs_xml, "prefs_func_add_button")));
    gtk_size_group_add_widget (sgroup, GTK_WIDGET(
        gtk_builder_get_object (prefs_xml, "prefs_func_update_button")));
    gtk_size_group_add_widget (sgroup, GTK_WIDGET(
        gtk_builder_get_object (prefs_xml, "prefs_func_delete_button")));
    gtk_size_group_add_widget (sgroup, GTK_WIDGET(
        gtk_builder_get_object (prefs_xml, "prefs_func_clear_button")));

	gtk_widget_show (prefs_dialog);
	return prefs_dialog;
}

GtkWidget *ui_about_dialog_create()
{
    GtkWidget* dialog;
	GtkBuilder* about_dialog_xml = gtk_builder_file_open (ABOUT_GLADE_FILE, FALSE);
	gtk_builder_connect_signals(about_dialog_xml, NULL);
	dialog = GTK_WIDGET(gtk_builder_get_object (about_dialog_xml, "about_dialog"));
    g_object_unref(about_dialog_xml);
    return dialog;
}

void ui_formula_entry_activate ()
{
	GtkWidget	*formula_entry;
	
    formula_entry = GTK_WIDGET(gtk_builder_get_object (view_xml, "formula_entry"));
	gtk_widget_activate(formula_entry);
}

void ui_formula_entry_set (G_CONST_RETURN gchar *text)
{
	GtkWidget	*formula_entry;

	if (text == NULL) return;
    formula_entry = GTK_WIDGET(gtk_builder_get_object (view_xml, "formula_entry"));
	gtk_entry_set_text ((GtkEntry *) formula_entry, text);
}

void ui_formula_entry_insert (G_CONST_RETURN gchar *text)
{
	GtkWidget	*formula_entry;
	int		position;
	
	if (text == NULL) return;
    formula_entry = GTK_WIDGET(gtk_builder_get_object (view_xml, "formula_entry"));
	position = gtk_editable_get_position ((GtkEditable *) formula_entry);
	gtk_editable_insert_text ((GtkEditable *) formula_entry, text, -1,
                                             &position);
	gtk_editable_set_position ((GtkEditable *) formula_entry, position);
}

void ui_formula_entry_backspace ()
{
	GtkWidget	*formula_entry;
	
    formula_entry = GTK_WIDGET(gtk_builder_get_object (view_xml, "formula_entry"));
	gtk_editable_delete_text ((GtkEditable *) formula_entry, 
		strlen(gtk_entry_get_text((GtkEntry *) formula_entry)) - 1, -1);
}

/* ui_formula_entry_state. if color == NULL looks like we get default. this is
 * what we want.
 */

void ui_formula_entry_state (gboolean error)
{
	GtkWidget		*formula_entry;
#if GTK_CHECK_VERSION(3, 0, 0)
    GdkRGBA color, *pcolor = NULL;
    formula_entry = GTK_WIDGET(gtk_builder_get_object (view_xml, "formula_entry"));
	if (error) {
		gdk_rgba_parse (&color, "red");
        pcolor = &color;
	}
	gtk_widget_override_color (formula_entry, GTK_STATE_FLAG_NORMAL, pcolor);
    gtk_widget_override_color (formula_entry, GTK_STATE_FLAG_ACTIVE, pcolor);
    gtk_widget_override_color (formula_entry, GTK_STATE_FLAG_PRELIGHT, pcolor);
    gtk_widget_override_color (formula_entry, GTK_STATE_FLAG_INSENSITIVE, pcolor);

#else
	GdkColor color, *pcolor = NULL;
    formula_entry = GTK_WIDGET(gtk_builder_get_object (view_xml, "formula_entry"));
	if (error) {
		gdk_color_parse ("red", &color);
        pcolor = &color;
	}
	gtk_widget_modify_text (formula_entry, 0, pcolor);
	gtk_widget_modify_text (formula_entry, 1, pcolor);
	gtk_widget_modify_text (formula_entry, 2, pcolor);
	gtk_widget_modify_text (formula_entry, 3, pcolor);
	gtk_widget_modify_text (formula_entry, 4, pcolor);
#endif

}

void ui_button_set_pan ()
{
	set_button_label_and_tooltip (button_box_xml, "button_enter", 
		_("="), _("Enter"));
	set_button_label_and_tooltip (button_box_xml, "button_pow", 
		_("x^y"), _("Power"));
	set_button_label_and_tooltip (button_box_xml, "button_f1", 
		_("("), _("Open Bracket"));
	set_button_label_and_tooltip (button_box_xml, "button_f2", 
		_(")"), _("Close Bracket"));
}

void ui_button_set_rpn ()
{
	set_button_label_and_tooltip (button_box_xml, "button_enter", 
		_("ENT"), _("Enter"));
	set_button_label_and_tooltip (button_box_xml, "button_pow", 
		_("y^x"), _("Power"));
	set_button_label_and_tooltip (button_box_xml, "button_f1", 
		_("x<>y"), _("swap current number with top of stack"));
	set_button_label_and_tooltip (button_box_xml, "button_f2", 
		_("roll"), _("roll down stack"));	
}

void ui_relax_fmod_buttons ()
{
	GtkWidget	*tbutton;
	
    tbutton = GTK_WIDGET(gtk_builder_get_object (button_box_xml, "button_inv"));
	gtk_toggle_button_set_active ((GtkToggleButton *) tbutton, FALSE);
    tbutton = GTK_WIDGET(gtk_builder_get_object (button_box_xml, "button_hyp"));
	gtk_toggle_button_set_active ((GtkToggleButton *) tbutton, FALSE);
}

void ui_classic_view_create()
{
	GtkWidget	*classic_view_vbox, *box;

	/* at first, check if there is already a classic view */
	if (classic_view_xml) {
        classic_view_vbox = GTK_WIDGET(gtk_builder_get_object (classic_view_xml, "classic_view_vbox"));
        g_assert(classic_view_xml != NULL);
		if (classic_view_vbox) return;
	}

	/* if not, build one */
	classic_view_xml = gtk_builder_file_open (CLASSIC_VIEW_GLADE_FILE, TRUE);
    box = GTK_WIDGET(gtk_builder_get_object (main_window_xml, "window_vbox"));
	ui_pack_from_xml (box, 1, classic_view_xml, "classic_view_vbox", TRUE, TRUE);
    view_xml = classic_view_xml;
	
	display_init ();
}

void ui_classic_view_destroy()
{
	GtkWidget	*classic_view_vbox;
	
	if (!classic_view_xml) return;
    classic_view_vbox = GTK_WIDGET(gtk_builder_get_object (classic_view_xml, "classic_view_vbox"));
	if (classic_view_vbox) gtk_widget_destroy (classic_view_vbox);
    g_object_unref(classic_view_xml);
    classic_view_xml = NULL;
}

void ui_paper_view_create()
{
	GtkWidget		*paper_view_vbox, *box, *tree_view;
	GtkCellRenderer 	*renderer;
	GtkTreeViewColumn 	*column;
	GtkListStore		*paper_store;
	GtkTreeSelection	*select;

	/* at first, check if there is already a ng view */
	if (paper_view_xml) {
        paper_view_vbox = GTK_WIDGET(gtk_builder_get_object (paper_view_xml, "paper_view_vbox"));
        g_assert(paper_view_vbox != NULL);
		if (paper_view_vbox) return;
	}
	
	/* if not, build one */
	paper_view_xml = gtk_builder_file_open (PAPER_VIEW_GLADE_FILE, TRUE);
    box = GTK_WIDGET(gtk_builder_get_object (main_window_xml, "window_vbox"));
	ui_pack_from_xml (box, 1, paper_view_xml, "paper_view_vbox", TRUE, TRUE);
    view_xml = paper_view_xml;
	
	/* markup / xalign / foreground */
	paper_store = gtk_list_store_new (3, G_TYPE_STRING, G_TYPE_FLOAT, G_TYPE_STRING);
    tree_view = GTK_WIDGET(gtk_builder_get_object (view_xml, "paper_treeview"));
	gtk_tree_view_set_model ((GtkTreeView *) tree_view, GTK_TREE_MODEL (paper_store));
	
	renderer = gtk_cell_renderer_text_new ();
	column = gtk_tree_view_column_new_with_attributes (_("Result Display"), renderer, "markup", 0, "xalign", 1, "foreground", 2, NULL);
	gtk_tree_view_append_column (GTK_TREE_VIEW (tree_view), column);
	
	select = gtk_tree_view_get_selection (GTK_TREE_VIEW (tree_view));
	gtk_tree_selection_set_mode (select, GTK_SELECTION_SINGLE);
	g_signal_connect (G_OBJECT (tree_view), "button-press-event",
                  G_CALLBACK (paper_tree_view_selection_changed_cb),
                  NULL);
                  
    gtk_widget_grab_focus(GTK_WIDGET(gtk_builder_get_object (view_xml, "paper_entry")));
}

void ui_paper_view_destroy()
{
	GtkWidget	*paper_view_vbox;
	
	if (!paper_view_xml) return;
    paper_view_vbox = GTK_WIDGET(gtk_builder_get_object (paper_view_xml, "paper_view_vbox"));
	if (paper_view_vbox) gtk_widget_destroy (paper_view_vbox);
    g_object_unref(paper_view_xml);
    paper_view_xml = NULL;
}
