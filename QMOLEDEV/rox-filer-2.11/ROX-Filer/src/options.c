/*
 * ROX-Filer, filer for the ROX desktop project
 * Copyright (C) 2006, Thomas Leonard and others (see changelog for details).
 *
 * This program is free software; you can redistribute it and/or modify it
 * under the terms of the GNU General Public License as published by the Free
 * Software Foundation; either version 2 of the License, or (at your option)
 * any later version.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
 * FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
 * more details.
 *
 * You should have received a copy of the GNU General Public License along with
 * this program; if not, write to the Free Software Foundation, Inc., 59 Temple
 * Place, Suite 330, Boston, MA  02111-1307  USA
 */

/* options.c - code for handling user choices */

/* How it works:
 *
 * On startup:
 *
 * - The <Choices>/PROJECT/Options file is read in, which contains a list of
 *   name/value pairs, and these are stored in the 'loading' hash table.
 *
 * - Each part of the filer then calls option_add_int(), or a related function,
 *   supplying the name for each option and a default value. Once an option is
 *   registered, it is removed from the loading table.
 *
 * - If things need to happen when values change, modules register with
 *   option_add_notify().
 *
 * - option_register_widget() can be used during initialisation (any time
 *   before the Options box is displayed) to tell the system how to render a
 *   particular type of option.
 *
 * - Finally, all notify callbacks are called. Use the Option->has_changed
 *   field to work out what has changed from the defaults.
 *
 * When the user opens the Options box:
 *
 * - The Options.xml file is read and used to create the Options dialog box.
 *   Each element in the file has a key corresponding to an option named
 *   above.
 *
 * - For each widget in the box, the current value of the option is used to
 *   set the widget's state.
 *
 * - All current values are saved for a possible Revert later.
 *
 * When the user changes an option or clicks on Revert:
 *
 * - The option values are updated.
 *
 * - All notify callbacks are called. Use the Option->has_changed field
 *   to see what changed.
 *
 * When OK is clicked:
 *
 * - If anything changed then:
 *   - All the options are written to the filesystem
 *   - The saver_callbacks are called.
 */

#include "config.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>
#include <ctype.h>
#include <gtk/gtk.h>
#include <libxml/parser.h>

#include "global.h"

#include "choices.h"
#include "options.h"
#include "main.h"
#include "gui_support.h"
#include "support.h"

/* Add all option tooltips to this group */
static GtkTooltips *option_tooltips = NULL;
#define OPTION_TIP(widget, tip)	\
	gtk_tooltips_set_tip(option_tooltips, widget, tip, NULL)

/* The Options window. NULL if not yet created. */
static GtkWidget *window = NULL;

/* "filer_unique" -> (Option *) */
static GHashTable *option_hash = NULL;

/* A mapping (name -> value) for options which have been loaded by not
 * yet registered. The options in this table cannot be used until
 * option_add_*() is called to move them into option_hash.
 */
static GHashTable *loading = NULL;

/* A mapping (XML name -> OptionBuildFn). When reading the Options.xml
 * file, this table gives the function used to create the widgets.
 */
static GHashTable *widget_builder = NULL;

/* A mapping (name -> GtkSizeGroup) of size groups used by the widgets
 * in the options box. This hash table is created/destroyed every time
 * the box is opened/destroyed.
 */
static GHashTable *size_groups = NULL;

/* List of functions to call after all option values are updated */
static GList *notify_callbacks = NULL;

/* List of functions to call after all options are saved */
static GList *saver_callbacks = NULL;

static int updating_widgets = 0;	/* Ignore change signals when set */

static GtkWidget *revert_widget = NULL;

/* Static prototypes */
static void save_options(void);
static void revert_options(GtkWidget *widget, gpointer data);
static void build_options_window(void);
static GtkWidget *build_window_frame(GtkTreeView **tree_view);
static void update_option_widgets(void);
static void button_patch_set_colour(GtkWidget *button, GdkColor *color);
static void option_add(Option *option, const gchar *key);
static void set_not_changed(gpointer key, gpointer value, gpointer data);
static void load_options(xmlDoc *doc);
static gboolean check_anything_changed(void);
static int get_int(xmlNode *node, guchar *attr);
static void may_add_tip(GtkWidget *widget, xmlNode *element);
static void add_to_size_group(xmlNode *node, GtkWidget *widget);

static const char *process_option_line(gchar *line);

static GList *build_label(Option *option, xmlNode *node, guchar *label);
static GList *build_spacer(Option *option, xmlNode *node, guchar *label);
static GList *build_frame(Option *option, xmlNode *node, guchar *label);

static GList *build_toggle(Option *option, xmlNode *node, guchar *label);
static GList *build_slider(Option *option, xmlNode *node, guchar *label);
static GList *build_entry(Option *option, xmlNode *node, guchar *label);
static GList *build_radio_group(Option *option, xmlNode *node, guchar *label);
static GList *build_colour(Option *option, xmlNode *node, guchar *label);
static GList *build_menu(Option *option, xmlNode *node, guchar *label);
static GList *build_font(Option *option, xmlNode *node, guchar *label);
static GList *build_numentry(Option *option, xmlNode *node, guchar *label);
static void update_numentry(Option *option);
static guchar *read_numentry(Option *option);

static gboolean updating_file_format = FALSE;

/****************************************************************
 *			EXTERNAL INTERFACE			*
 ****************************************************************/

void options_init(void)
{
	char	*path;
	xmlDoc	*doc;

	loading = g_hash_table_new(g_str_hash, g_str_equal);
	option_hash = g_hash_table_new(g_str_hash, g_str_equal);
	widget_builder = g_hash_table_new(g_str_hash, g_str_equal);

	path = choices_find_xdg_path_load("Options", PROJECT, SITE);
	if (path)
	{
		/* Load in all the options set in the filer, storing them
		 * temporarily in the loading hash table.
		 * They get moved to option_hash when they're registered.
		 */
		doc = xmlParseFile(path);
		if (doc)
		{
			load_options(doc);
			xmlFreeDoc(doc);
		}
		else
		{
			parse_file(path, process_option_line);
			updating_file_format = TRUE;
		}

		g_free(path);
	}

	option_register_widget("label", build_label);
	option_register_widget("spacer", build_spacer);
	option_register_widget("frame", build_frame);

	option_register_widget("toggle", build_toggle);
	option_register_widget("slider", build_slider);
	option_register_widget("entry", build_entry);
	option_register_widget("numentry", build_numentry);
	option_register_widget("radio-group", build_radio_group);
	option_register_widget("colour", build_colour);
	option_register_widget("menu", build_menu);
	option_register_widget("font", build_font);
}

/* When parsing the XML file, process an element named 'name' by
 * calling 'builder(option, xml_node, label)'.
 * builder returns the new widgets to add to the options box.
 * 'name' should be a static string. Call 'option_check_widget' when
 * the widget's value is modified.
 *
 * Functions to set or get the widget's state can be stored in 'option'.
 * If the option doesn't have a name attribute in Options.xml then
 * ui will be NULL on entry (this is used for buttons).
 */
void option_register_widget(char *name, OptionBuildFn builder)
{
	g_hash_table_insert(widget_builder, name, builder);
}

/* This is called when the widget's value is modified by the user.
 * Reads the new value of the widget into the option and calls
 * the notify callbacks.
 */
void option_check_widget(Option *option)
{
	guchar		*new = NULL;

	if (updating_widgets)
		return;		/* Not caused by the user... */

	g_return_if_fail(option->read_widget != NULL);

	new = option->read_widget(option);

	g_return_if_fail(new != NULL);

	g_hash_table_foreach(option_hash, set_not_changed, NULL);

	option->has_changed = strcmp(option->value, new) != 0;

	if (!option->has_changed)
	{
		g_free(new);
		return;
	}

	g_free(option->value);
	option->value = new;
	option->int_value = atoi(new);

	options_notify();
}

/* Call all the notify callbacks. This should happen after any options
 * have their values changed.
 * Set each option->has_changed flag before calling this function.
 */
void options_notify(void)
{
	GList	*next;

	for (next = notify_callbacks; next; next = next->next)
	{
		OptionNotify *cb = (OptionNotify *) next->data;

		cb();
	}

	if (updating_file_format)
	{
		updating_file_format = FALSE;
		save_options();
		info_message(_("ROX-Filer has converted your Options file "
				"to the new XML format"));
	}

	if (revert_widget)
		gtk_widget_set_sensitive(revert_widget,
					 check_anything_changed());
}

/* Store values used by Revert */
static void store_backup(gpointer key, gpointer value, gpointer data)
{
	Option *option = (Option *) value;

	g_free(option->backup);
	option->backup = g_strdup(option->value);
}

/* Allow the user to edit the options. Returns the window widget (you don't
 * normally need this). NULL if already open.
 */
GtkWidget *options_show(void)
{
	if (!option_tooltips)
		option_tooltips = gtk_tooltips_new();

	/* For debugging
	if (g_hash_table_size(loading) != 0)
	{
		g_print(PROJECT ": Some options loaded but not used:\n");
		g_hash_table_foreach(loading, (GHFunc) puts, NULL);
	}
	*/

	if (window)
	{
		gtk_window_present(GTK_WINDOW(window));
		return NULL;
	}

	g_hash_table_foreach(option_hash, store_backup, NULL);
			
	build_options_window();

	update_option_widgets();
	
	gtk_widget_show_all(window);

	return window;
}

/* Initialise and register a new integer option */
void option_add_int(Option *option, const gchar *key, int value)
{
	option->value = g_strdup_printf("%d", value);
	option->int_value = value;
	option_add(option, key);
}

void option_add_string(Option *option, const gchar *key, const gchar *value)
{
	option->value = g_strdup(value);
	option->int_value = atoi(value);
	option_add(option, key);
}

/* Add a callback which will be called after any options have changed their
 * values. If several options change at once, this is called after all
 * changes.
 */
void option_add_notify(OptionNotify *callback)
{
	g_return_if_fail(callback != NULL);

	notify_callbacks = g_list_append(notify_callbacks, callback);
}

/* Call 'callback' after all the options have been saved */
void option_add_saver(OptionNotify *callback)
{
	g_return_if_fail(callback != NULL);

	saver_callbacks = g_list_append(saver_callbacks, callback);
}

/* Base class for building numentry widgets with particular ranges */
GList *build_numentry_base(Option *option, xmlNode *node,
				  guchar *label, GtkAdjustment *adj)
{
	GtkWidget	*hbox;
	GtkWidget	*spin;
	GtkWidget	*label_wid;
	guchar		*unit;
	int		width;

	width = get_int(node, "width");
	unit = xmlGetProp(node, "unit");

	hbox = gtk_hbox_new(FALSE, 4);

	if (label)
	{
		label_wid = gtk_label_new(_(label));
		gtk_misc_set_alignment(GTK_MISC(label_wid), 1.0, 0.5);
		gtk_box_pack_start(GTK_BOX(hbox), label_wid, FALSE, TRUE, 0);
		add_to_size_group(node, label_wid);
	}

	spin = gtk_spin_button_new(adj, adj->step_increment, 0);
	gtk_entry_set_width_chars(GTK_ENTRY(spin),
			width > 1 ? width + 1 : 2);
	gtk_box_pack_start(GTK_BOX(hbox), spin, FALSE, TRUE, 0);
	may_add_tip(spin, node);

	if (unit)
	{
		gtk_box_pack_start(GTK_BOX(hbox), gtk_label_new(_(unit)),
				FALSE, TRUE, 0);
		g_free(unit);
	}

	option->update_widget = update_numentry;
	option->read_widget = read_numentry;
	option->widget = spin;

	g_signal_connect_swapped(spin, "value-changed",
			G_CALLBACK(option_check_widget), option);

	return g_list_append(NULL, hbox);
}

/****************************************************************
 *                      INTERNAL FUNCTIONS                      *
 ****************************************************************/

/* Option should contain the default value.
 * It must never be destroyed after being registered (Options are typically
 * statically allocated).
 * The key corresponds to the option's name in Options.xml, and to the key
 * in the saved options file.
 *
 * On exit, the value will have been updated to the loaded value, if
 * different to the default.
 */
static void option_add(Option *option, const gchar *key)
{
	gpointer okey, value;

	g_return_if_fail(option_hash != NULL);
	g_return_if_fail(g_hash_table_lookup(option_hash, key) == NULL);
	g_return_if_fail(option->value != NULL);
	
	option->has_changed = FALSE;

	option->widget = NULL;
	option->update_widget = NULL;
	option->read_widget = NULL;
	option->backup = NULL;

	g_hash_table_insert(option_hash, (gchar *) key, option);

	/* Use the value loaded from the file, if any */
	if (g_hash_table_lookup_extended(loading, key, &okey, &value))
	{
		option->has_changed = strcmp(option->value, value) != 0;
			
		g_free(option->value);
		option->value = value;
		option->int_value = atoi(value);
		g_hash_table_remove(loading, key);
		g_free(okey);
	}
}

static GtkColorSelectionDialog *current_csel_box = NULL;
static GtkFontSelectionDialog *current_fontsel_box = NULL;

static void get_new_colour(GtkWidget *ok, Option *option)
{
	GtkWidget	*csel;
	GdkColor	c;

	g_return_if_fail(current_csel_box != NULL);

	csel = current_csel_box->colorsel;

	gtk_color_selection_get_current_color(GTK_COLOR_SELECTION(csel), &c);

	button_patch_set_colour(option->widget, &c);
	
	gtk_widget_destroy(GTK_WIDGET(current_csel_box));

	option_check_widget(option);
}

static void open_coloursel(GtkWidget *button, Option *option)
{
	GtkColorSelectionDialog	*csel;
	GtkWidget		*dialog, *patch;

	if (current_csel_box)
		gtk_widget_destroy(GTK_WIDGET(current_csel_box));

	dialog = gtk_color_selection_dialog_new(NULL);
	csel = GTK_COLOR_SELECTION_DIALOG(dialog);
	current_csel_box = csel;
	gtk_window_set_position(GTK_WINDOW(csel), GTK_WIN_POS_MOUSE);

	g_signal_connect(dialog, "destroy",
			G_CALLBACK(gtk_widget_destroyed), &current_csel_box);
	gtk_widget_hide(csel->help_button);
	g_signal_connect_swapped(csel->cancel_button, "clicked",
			G_CALLBACK(gtk_widget_destroy), dialog);
	g_signal_connect(csel->ok_button, "clicked",
			G_CALLBACK(get_new_colour), option);

	patch = GTK_BIN(button)->child;

	gtk_color_selection_set_current_color(
			GTK_COLOR_SELECTION(csel->colorsel),
			&patch->style->bg[GTK_STATE_NORMAL]);

	gtk_widget_show(dialog);
}

static void font_chosen(GtkWidget *dialog, gint response, Option *option)
{
	gchar *font;

	if (response != GTK_RESPONSE_OK)
		goto out;

	font = gtk_font_selection_dialog_get_font_name(
					GTK_FONT_SELECTION_DIALOG(dialog));

	gtk_label_set_text(GTK_LABEL(option->widget), font);

	g_free(font);

	option_check_widget(option);

out:
	gtk_widget_destroy(dialog);

}

static void toggle_active_font(GtkToggleButton *toggle, Option *option)
{
	if (current_fontsel_box)
		gtk_widget_destroy(GTK_WIDGET(current_fontsel_box));

	if (gtk_toggle_button_get_active(toggle))
	{
		gtk_widget_set_sensitive(option->widget->parent, TRUE);
		gtk_label_set_text(GTK_LABEL(option->widget), "Sans 12");
	}
	else
	{
		gtk_widget_set_sensitive(option->widget->parent, FALSE);
		gtk_label_set_text(GTK_LABEL(option->widget),
				   _("(use default)"));
	}

	option_check_widget(option);
}

static void open_fontsel(GtkWidget *button, Option *option)
{
	if (current_fontsel_box)
		gtk_widget_destroy(GTK_WIDGET(current_fontsel_box));

	current_fontsel_box = GTK_FONT_SELECTION_DIALOG(
				gtk_font_selection_dialog_new(PROJECT));

	gtk_window_set_position(GTK_WINDOW(current_fontsel_box),
				GTK_WIN_POS_MOUSE);

	g_signal_connect(current_fontsel_box, "destroy",
			G_CALLBACK(gtk_widget_destroyed), &current_fontsel_box);

	gtk_font_selection_dialog_set_font_name(current_fontsel_box,
						option->value);

	g_signal_connect(current_fontsel_box, "response",
			G_CALLBACK(font_chosen), option);

	gtk_widget_show(GTK_WIDGET(current_fontsel_box));
}

/* These are used during parsing... */
static xmlDocPtr options_doc = NULL;

#define DATA(node) (xmlNodeListGetString(options_doc, node->xmlChildrenNode, 1))

static void may_add_tip(GtkWidget *widget, xmlNode *element)
{
	guchar	*data, *tip;

	data = DATA(element);
	if (!data)
		return;

	tip = g_strstrip(g_strdup(data));
	g_free(data);
	if (*tip)
		OPTION_TIP(widget, _(tip));
	g_free(tip);
}

/* Returns zero if attribute is not present */
static int get_int(xmlNode *node, guchar *attr)
{
	guchar *txt;
	int	retval;

	txt = xmlGetProp(node, attr);
	if (!txt)
		return 0;

	retval = atoi(txt);
	g_free(txt);

	return retval;
}

/* Adds 'widget' to the GtkSizeGroup selected by 'index'.  This function
 * does nothing if 'node' has no "sizegroup" attribute.
 * The value of "sizegroup" is either a key. All widgets with the same
 * key request the same size.
 * Size groups are created on the fly and get destroyed when the options
 * box is closed.
 */
static void add_to_size_group(xmlNode *node, GtkWidget *widget)
{
	GtkSizeGroup	*sg;
	guchar		*name;

	g_return_if_fail(node != NULL);
	g_return_if_fail(widget != NULL);

	name = xmlGetProp(node, "sizegroup");
	if (!name)
		return;

	if (size_groups == NULL)
		size_groups = g_hash_table_new_full(g_str_hash, g_str_equal,
				g_free, NULL);

	sg = (GtkSizeGroup *) g_hash_table_lookup(size_groups, name);
	if (sg == NULL)
	{

		sg = (GtkSizeGroup *) gtk_size_group_new(
				GTK_SIZE_GROUP_HORIZONTAL);
		g_hash_table_insert(size_groups, name, sg);
		gtk_size_group_add_widget(sg, widget);
		g_object_unref(G_OBJECT(sg));
	}
	else
	{
		gtk_size_group_add_widget(sg, widget);
		g_free(name);
	}
}

static GtkWidget *build_radio(xmlNode *radio, GtkWidget *prev)
{
	GtkWidget	*button;
	GtkRadioButton	*prev_button = (GtkRadioButton *) prev;
	guchar		*label;

	label = xmlGetProp(radio, "label");

	button = gtk_radio_button_new_with_label(
			prev_button ? gtk_radio_button_get_group(prev_button)
				    : NULL,
			_(label));
	g_free(label);

	may_add_tip(button, radio);

	g_object_set_data(G_OBJECT(button), "value",
			  xmlGetProp(radio, "value"));

	return button;
}

static void build_menu_item(xmlNode *node, GtkWidget *menu)
{
	GtkWidget	*item;
	guchar		*label;

	g_return_if_fail(strcmp(node->name, "item") == 0);

	label = xmlGetProp(node, "label");
	item = gtk_menu_item_new_with_label(_(label));
	g_free(label);

	gtk_widget_show(item);
	gtk_menu_shell_append(GTK_MENU_SHELL(menu), item);

	g_object_set_data(G_OBJECT(item), "value", xmlGetProp(node, "value"));
}

static void build_widget(xmlNode *widget, GtkWidget *box)
{
	const char *name = widget->name;
	OptionBuildFn builder;
	guchar	*oname;
	Option	*option;
	guchar	*label;

	label = xmlGetProp(widget, "label");

	if (strcmp(name, "hbox") == 0 || strcmp(name, "vbox") == 0)
	{
		GtkWidget *nbox;
		xmlNode	  *hw;

		if (name[0] == 'h')
			nbox = gtk_hbox_new(FALSE, 4);
		else
			nbox = gtk_vbox_new(FALSE, 0);

		if (label)
			gtk_box_pack_start(GTK_BOX(nbox),
				gtk_label_new(_(label)), FALSE, TRUE, 4);
		gtk_box_pack_start(GTK_BOX(box), nbox, FALSE, TRUE, 0);

		for (hw = widget->xmlChildrenNode; hw; hw = hw->next)
		{
			if (hw->type == XML_ELEMENT_NODE)
				build_widget(hw, nbox);
		}

		g_free(label);
		return;
	}

	oname = xmlGetProp(widget, "name");

	if (oname)
	{
		option = g_hash_table_lookup(option_hash, oname);

		if (!option)
		{
			g_warning("No Option for '%s'!\n", oname);
			g_free(oname);
			return;
		}

		g_free(oname);
	}
	else
		option = NULL;

	builder = g_hash_table_lookup(widget_builder, name);
	if (builder)
	{
		GList *widgets, *next;

		if (option && option->widget)
			g_warning("Widget for option already exists!");

		widgets = builder(option, widget, label);

		for (next = widgets; next; next = next->next)
		{
			GtkWidget *w = (GtkWidget *) next->data;
			gtk_box_pack_start(GTK_BOX(box), w, FALSE, TRUE, 0);
		}
		g_list_free(widgets);
	}
	else
		g_warning("Unknown option type '%s'\n", name);

	g_free(label);
}

static void build_section(xmlNode *section, GtkWidget *notebook,
			  GtkTreeStore *tree_store, GtkTreeIter *parent)
{
	guchar 		*title = NULL;
	GtkWidget   	*page;
	GtkTreeIter	iter;
	xmlNode		*widget;

	title = xmlGetProp(section, "title");
	page = gtk_vbox_new(FALSE, 4);
	gtk_container_set_border_width(GTK_CONTAINER(page), 4);
	gtk_notebook_append_page(GTK_NOTEBOOK(notebook), page, NULL);

	gtk_tree_store_append(tree_store, &iter, parent);
	gtk_tree_store_set(tree_store, &iter, 0, _(title), 1, page, -1);
	g_free(title);
		
	widget = section->xmlChildrenNode;
	for (; widget; widget = widget->next)
	{
		if (widget->type == XML_ELEMENT_NODE)
		{
			if (strcmp(widget->name, "section") == 0)
				build_section(widget, notebook,
						tree_store, &iter);
			else
				build_widget(widget, page);
		}
	}
}

/* Parse <app_dir>/Options.xml to create the options window.
 * Sets the global 'window' variable.
 */
static void build_options_window(void)
{
	GtkTreeView	*tree;
	GtkTreeStore	*store;
	GtkWidget	*notebook;
	xmlDocPtr	options_doc;
	xmlNode		*options, *section;
	gchar		*path;

	notebook = build_window_frame(&tree);
	
	path = g_strconcat(app_dir, "/Options.xml", NULL);
	options_doc = xmlParseFile(path);

	if (!options_doc)
	{
		report_error(_("Internal error: %s unreadable"), path);
		g_free(path);
		return;
	}

	g_free(path);

	options = xmlDocGetRootElement(options_doc);
	if (strcmp(options->name, "options") == 0)
	{
		GtkTreePath *treepath;

		store = (GtkTreeStore *) gtk_tree_view_get_model(tree);
		section = options->xmlChildrenNode;
		for (; section; section = section->next)
			if (section->type == XML_ELEMENT_NODE)
				build_section(section, notebook, store, NULL);

		gtk_tree_view_expand_all(tree);
		treepath = gtk_tree_path_new_first();
		if (treepath)
		{
			gtk_tree_view_set_cursor(tree, treepath, NULL, FALSE);
			gtk_tree_path_free(treepath);
		}
	}

	xmlFreeDoc(options_doc);
	options_doc = NULL;
}

static void null_widget(gpointer key, gpointer value, gpointer data)
{
	Option	*option = (Option *) value;

	option->widget = NULL;
}

static void options_destroyed(GtkWidget *widget, gpointer data)
{
	if (current_csel_box)
		gtk_widget_destroy(GTK_WIDGET(current_csel_box));
	if (current_fontsel_box)
		gtk_widget_destroy(GTK_WIDGET(current_fontsel_box));

	revert_widget = NULL;

	if (check_anything_changed())
		save_options();
	
	if (widget == window)
	{
		window = NULL;

		g_hash_table_foreach(option_hash, null_widget, NULL);

		if (size_groups)
		{
			g_hash_table_destroy(size_groups);
			size_groups = NULL;
			
		}
	}
}

/* The cursor has been changed in the tree view, so switch to the new
 * page in the notebook.
 */
static void tree_cursor_changed(GtkTreeView *tv, gpointer data)
{
	GtkTreePath	*path = NULL;
	GtkNotebook	*nbook = GTK_NOTEBOOK(data);
	GtkTreeModel	*model;
	GtkWidget	*page = NULL;
	GtkTreeIter	iter;

	gtk_tree_view_get_cursor(tv, &path, NULL);
	if (!path)
		return;

	model = gtk_tree_view_get_model(tv);
	gtk_tree_model_get_iter(model, &iter, path);
	gtk_tree_path_free(path);
	gtk_tree_model_get(model, &iter, 1, &page, -1);

	if (page)
	{
		gtk_notebook_set_current_page(nbook,
				gtk_notebook_page_num(nbook, page));
		g_object_unref(page);
	}
}

/* Creates the window and adds the various buttons to it.
 * Returns the notebook to add sections to and sets the global
 * 'window'. If 'tree_view' is non-NULL, it stores the address
 * of the tree view widget there.
 */
static GtkWidget *build_window_frame(GtkTreeView **tree_view)
{
	GtkWidget	*notebook;
	GtkWidget	*tl_vbox, *hbox, *frame, *tv;
	GtkWidget	*actions, *button;
	GtkTreeStore	*model;
	char		*string, *save_path;

	window = gtk_window_new(GTK_WINDOW_TOPLEVEL);

	gtk_window_set_position(GTK_WINDOW(window), GTK_WIN_POS_CENTER);
	gtk_window_set_title(GTK_WINDOW(window), _("Options"));
	g_signal_connect(window, "destroy",
			G_CALLBACK(options_destroyed), NULL);
	gtk_container_set_border_width(GTK_CONTAINER(window), 4);

	tl_vbox = gtk_vbox_new(FALSE, 4);
	gtk_container_add(GTK_CONTAINER(window), tl_vbox);

	hbox = gtk_hbox_new(FALSE, 4);
	gtk_box_pack_start(GTK_BOX(tl_vbox), hbox, TRUE, TRUE, 0);

	frame = gtk_frame_new(NULL);
	gtk_frame_set_shadow_type(GTK_FRAME(frame), GTK_SHADOW_IN);
	gtk_box_pack_end(GTK_BOX(hbox), frame, TRUE, TRUE, 0);

	notebook = gtk_notebook_new();
	gtk_notebook_set_show_tabs(GTK_NOTEBOOK(notebook), FALSE);
	gtk_notebook_set_show_border(GTK_NOTEBOOK(notebook), FALSE);
	gtk_container_add(GTK_CONTAINER(frame), notebook);

	frame = gtk_frame_new(NULL);
	gtk_frame_set_shadow_type(GTK_FRAME(frame), GTK_SHADOW_IN);
	gtk_box_pack_start(GTK_BOX(hbox), frame, FALSE, TRUE, 0);

	/* tree view */
	model = gtk_tree_store_new(2, G_TYPE_STRING, GTK_TYPE_WIDGET);
	tv = gtk_tree_view_new_with_model(GTK_TREE_MODEL(model));
	g_object_unref(model);
	gtk_tree_selection_set_mode(
			gtk_tree_view_get_selection(GTK_TREE_VIEW(tv)),
			GTK_SELECTION_BROWSE);
	gtk_tree_view_set_headers_visible(GTK_TREE_VIEW(tv), FALSE);
	gtk_tree_view_insert_column_with_attributes(GTK_TREE_VIEW(tv), -1,
			NULL, gtk_cell_renderer_text_new(), "text", 0, NULL);
	gtk_container_add(GTK_CONTAINER(frame), tv);
	g_signal_connect(tv, "cursor_changed",
			G_CALLBACK(tree_cursor_changed), notebook);

	actions = gtk_hbutton_box_new();
	gtk_button_box_set_layout(GTK_BUTTON_BOX(actions),
				  GTK_BUTTONBOX_END);
	gtk_box_set_spacing(GTK_BOX(actions), 10);

	gtk_box_pack_start(GTK_BOX(tl_vbox), actions, FALSE, TRUE, 0);
	
	revert_widget = button_new_mixed(GTK_STOCK_UNDO, _("_Revert"));
	GTK_WIDGET_SET_FLAGS(revert_widget, GTK_CAN_DEFAULT);
	gtk_box_pack_start(GTK_BOX(actions), revert_widget, FALSE, TRUE, 0);
	g_signal_connect(revert_widget, "clicked",
			 G_CALLBACK(revert_options), NULL);
	gtk_tooltips_set_tip(option_tooltips, revert_widget,
			_("Restore all choices to how they were when the "
			  "Options box was opened."), NULL);
	gtk_widget_set_sensitive(revert_widget, check_anything_changed());

	button = gtk_button_new_from_stock(GTK_STOCK_OK);
	GTK_WIDGET_SET_FLAGS(button, GTK_CAN_DEFAULT);
	gtk_box_pack_start(GTK_BOX(actions), button, FALSE, TRUE, 0);
	g_signal_connect_swapped(button, "clicked",
				G_CALLBACK(gtk_widget_destroy), window);
	gtk_widget_grab_default(button);
	gtk_widget_grab_focus(button);

	save_path = choices_find_xdg_path_save("...", PROJECT, SITE, FALSE);
	if (save_path)
	{
		string = g_strdup_printf(_("Choices will be saved as:\n%s"),
					save_path);
		gtk_tooltips_set_tip(option_tooltips, button, string, NULL);
		g_free(string);
		g_free(save_path);
	}
	else
		gtk_tooltips_set_tip(option_tooltips, button,
				_("(saving disabled by CHOICESPATH)"), NULL);

	if (tree_view)
		*tree_view = GTK_TREE_VIEW(tv);

	return notebook;
}

/* Given the last radio button in the group, select whichever
 * radio button matches the given value.
 */
static void radio_group_set_value(GtkRadioButton *last, guchar *value)
{	
	GSList	*next;

	for (next = gtk_radio_button_get_group(last); next; next = next->next)
	{
		GtkToggleButton *button = (GtkToggleButton *) next->data;
		guchar	*val;

		val = g_object_get_data(G_OBJECT(button), "value");
		g_return_if_fail(val != NULL);

		if (strcmp(val, value) == 0)
		{
			gtk_toggle_button_set_active(button, TRUE);
			return;
		}
	}

	g_warning("Can't find radio button with value %s\n", value);
}

/* Given the last radio button in the group, return a copy of the
 * value for the selected radio item.
 */
static guchar *radio_group_get_value(GtkRadioButton *last)
{
	GSList	*next;

	for (next = gtk_radio_button_get_group(last); next; next = next->next)
	{
		GtkToggleButton *button = (GtkToggleButton *) next->data;

		if (gtk_toggle_button_get_active(button))
		{
			guchar	*val;

			val = g_object_get_data(G_OBJECT(button), "value");
			g_return_val_if_fail(val != NULL, NULL);

			return g_strdup(val);
		}
	}

	return NULL;
}

/* Select this item with this value */
static void option_menu_set(GtkOptionMenu *om, guchar *value)
{
	GtkWidget *menu;
	GList	  *list, *next;
	int	  i = 0;

	menu = gtk_option_menu_get_menu(om);
	list = gtk_container_get_children(GTK_CONTAINER(menu));

	for (next = list; next; next = next->next)
	{
		GObject	*item = (GObject *) next->data;
		guchar	*data;

		data = g_object_get_data(item, "value");
		g_return_if_fail(data != NULL);

		if (strcmp(data, value) == 0)
		{
			gtk_option_menu_set_history(om, i);
			break;
		}
		
		i++;
	}

	g_list_free(list);
}

/* Get the value (static) of the selected item */
static guchar *option_menu_get(GtkOptionMenu *om)
{
	GtkWidget *menu, *item;

	menu = gtk_option_menu_get_menu(om);
	item = gtk_menu_get_active(GTK_MENU(menu));

	return g_object_get_data(G_OBJECT(item), "value");
}

static void restore_backup(gpointer key, gpointer value, gpointer data)
{
	Option *option = (Option *) value;

	g_return_if_fail(option->backup != NULL);

	option->has_changed = strcmp(option->value, option->backup) != 0;
	if (!option->has_changed)
		return;

	g_free(option->value);
	option->value = g_strdup(option->backup);
	option->int_value = atoi(option->value);
}

static void revert_options(GtkWidget *widget, gpointer data)
{
	g_hash_table_foreach(option_hash, restore_backup, NULL);
	options_notify();
	update_option_widgets();
}

static void check_changed_cb(gpointer key, gpointer value, gpointer data)
{
	Option *option = (Option *) value;
	gboolean *changed = (gboolean *) data;

	g_return_if_fail(option->backup != NULL);

	if (*changed)
		return;

	if (strcmp(option->value, option->backup) != 0)
		*changed = TRUE;
}

static gboolean check_anything_changed(void)
{
	gboolean retval = FALSE;
	
	g_hash_table_foreach(option_hash, check_changed_cb, &retval);

	return retval;
}

static void write_option(gpointer key, gpointer value, gpointer data)
{
	xmlNodePtr doc = (xmlNodePtr) data;
	Option *option = (Option *) value;
	xmlNodePtr tree;

	tree = xmlNewTextChild(doc, NULL, "Option", option->value);
	xmlSetProp(tree, "name", (gchar *) key);
}

static void save_options(void)
{
	xmlDoc	*doc;
	GList	*next;
	guchar	*save, *save_new;

	save = choices_find_xdg_path_save("Options", PROJECT, SITE, TRUE);
	if (!save)
		goto out;

	save_new = g_strconcat(save, ".new", NULL);

	doc = xmlNewDoc("1.0");
	xmlDocSetRootElement(doc, xmlNewDocNode(doc, NULL, "Options", NULL));

	g_hash_table_foreach(option_hash, write_option,
				xmlDocGetRootElement(doc));

	if (save_xml_file(doc, save_new) || rename(save_new, save))
		report_error(_("Error saving %s: %s"), save, g_strerror(errno));

	g_free(save_new);
	g_free(save);
	xmlFreeDoc(doc);

	for (next = saver_callbacks; next; next = next->next)
	{
		OptionNotify *cb = (OptionNotify *) next->data;
		cb();
	}

out:
	if (window)
		gtk_widget_destroy(window);
}

/* Make the widget reflect the current value of the option */
static void update_cb(gpointer key, gpointer value, gpointer data)
{
	Option *option = (Option *) value;

	g_return_if_fail(option != NULL);
	if (option->widget == NULL)
		return;

	updating_widgets++;
	
	if (option->update_widget)
		option->update_widget(option);

	updating_widgets--;
}

/* Reflect the values in the Option structures by changing the widgets
 * in the Options window.
 */
static void update_option_widgets(void)
{
	g_hash_table_foreach(option_hash, update_cb, NULL);
}

/* Each of the following update the widget to make it show the current
 * value of the option.
 */

static void update_toggle(Option *option)
{
	gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(option->widget),
			option->int_value);
}

static void update_entry(Option *option)
{
	gtk_entry_set_text(GTK_ENTRY(option->widget), option->value);
}

static void update_numentry(Option *option)
{
	gtk_spin_button_set_value(GTK_SPIN_BUTTON(option->widget),
			option->int_value);
}

static void update_radio_group(Option *option)
{
	radio_group_set_value(GTK_RADIO_BUTTON(option->widget), option->value);
}

static void update_slider(Option *option)
{
	gtk_adjustment_set_value(
			gtk_range_get_adjustment(GTK_RANGE(option->widget)),
			option->int_value);
}

static void update_menu(Option *option)
{
	option_menu_set(GTK_OPTION_MENU(option->widget), option->value);
}

static void update_font(Option *option)
{
	GtkToggleButton *active;
	gboolean have_font = option->value[0] != '\0';

	active = g_object_get_data(G_OBJECT(option->widget), "rox_override");

	if (active)
	{
		gtk_toggle_button_set_active(active, have_font);
		gtk_widget_set_sensitive(option->widget->parent, have_font);
	}
	
	gtk_label_set_text(GTK_LABEL(option->widget),
			   have_font ? option->value
				     : (guchar *) _("(use default)"));
}

static void update_colour(Option *option)
{
	GdkColor colour;

	gdk_color_parse(option->value, &colour);
	button_patch_set_colour(option->widget, &colour);
}

/* Each of these read_* calls get the new (string) value of an option
 * from the widget.
 */

static guchar *read_toggle(Option *option)
{
	GtkToggleButton *toggle = GTK_TOGGLE_BUTTON(option->widget);

	return g_strdup_printf("%d", gtk_toggle_button_get_active(toggle));
}

static guchar *read_entry(Option *option)
{
	return gtk_editable_get_chars(GTK_EDITABLE(option->widget), 0, -1);
}

static guchar *read_numentry(Option *option)
{
	return g_strdup_printf("%d", (int)
		gtk_spin_button_get_value(GTK_SPIN_BUTTON(option->widget)));
}

static guchar *read_slider(Option *option)
{
	return g_strdup_printf("%d", (int)
		gtk_range_get_adjustment(GTK_RANGE(option->widget))->value);
}

static guchar *read_radio_group(Option *option)
{
	return radio_group_get_value(GTK_RADIO_BUTTON(option->widget));
}

static guchar *read_menu(Option *option)
{
	return g_strdup(option_menu_get(GTK_OPTION_MENU(option->widget)));
}

static guchar *read_font(Option *option)
{
	GtkToggleButton *active;

	active = g_object_get_data(G_OBJECT(option->widget), "rox_override");
	if (active && !gtk_toggle_button_get_active(active))
		return g_strdup("");
	
	return g_strdup(gtk_label_get_text(GTK_LABEL(option->widget)));
}

static guchar *read_colour(Option *option)
{
	GtkStyle *style = GTK_BIN(option->widget)->child->style;

	return g_strdup_printf("#%04x%04x%04x",
			style->bg[GTK_STATE_NORMAL].red,
			style->bg[GTK_STATE_NORMAL].green,
			style->bg[GTK_STATE_NORMAL].blue);
}

static void set_not_changed(gpointer key, gpointer value, gpointer data)
{
	Option	*option = (Option *) value;

	option->has_changed = FALSE;
}

/* Builders for decorations (no corresponding option) */

static GList *build_label(Option *option, xmlNode *node, guchar *label)
{
	GtkWidget *widget;
	guchar *text;
	int help;

	g_return_val_if_fail(option == NULL, NULL);
	g_return_val_if_fail(label == NULL, NULL);

	text = DATA(node);
	widget = gtk_label_new(_(text));
	g_free(text);

	help = get_int(node, "help");

	gtk_misc_set_alignment(GTK_MISC(widget), 0, help ? 0.5 : 1);
	gtk_label_set_justify(GTK_LABEL(widget), GTK_JUSTIFY_LEFT);
	gtk_label_set_line_wrap(GTK_LABEL(widget), TRUE);

	if (help)
	{
		GtkWidget *hbox, *image, *align, *spacer;

		hbox = gtk_hbox_new(FALSE, 4);
		image = gtk_image_new_from_stock(GTK_STOCK_DIALOG_INFO,
				GTK_ICON_SIZE_BUTTON);
		align = gtk_alignment_new(0, 0, 0, 0);

		gtk_container_add(GTK_CONTAINER(align), image);
		gtk_box_pack_start(GTK_BOX(hbox), align, FALSE, TRUE, 0);
		gtk_box_pack_start(GTK_BOX(hbox), widget, FALSE, TRUE, 0);

		spacer = gtk_event_box_new();
		gtk_widget_set_size_request(spacer, 6, 6);

		return g_list_append(g_list_append(NULL, hbox), spacer);
	}

	return g_list_append(NULL, widget);
}

static GList *build_spacer(Option *option, xmlNode *node, guchar *label)
{
	GtkWidget *eb;

	g_return_val_if_fail(option == NULL, NULL);
	g_return_val_if_fail(label == NULL, NULL);

	eb = gtk_event_box_new();
	gtk_widget_set_size_request(eb, 12, 12);

	return g_list_append(NULL, eb);
}

static GList *build_frame(Option *option, xmlNode *node, guchar *label)
{
	GtkWidget *nbox, *frame, *label_widget;
	xmlNode	  *hw;
	PangoAttrList *list;
	PangoAttribute *attr;

	g_return_val_if_fail(option == NULL, NULL);
	g_return_val_if_fail(label != NULL, NULL);

	frame = gtk_frame_new(_(label));
	gtk_frame_set_shadow_type(GTK_FRAME(frame), GTK_SHADOW_NONE);

	/* Make the title bold */
	label_widget = gtk_frame_get_label_widget(GTK_FRAME(frame));
	attr = pango_attr_weight_new(PANGO_WEIGHT_BOLD);

	attr->start_index = 0;
	attr->end_index = -1;
	list = pango_attr_list_new();
	pango_attr_list_insert(list, attr);
	gtk_label_set_attributes(GTK_LABEL(label_widget), list);

	nbox = gtk_vbox_new(FALSE, 0);
	gtk_container_set_border_width(GTK_CONTAINER(nbox), 12);
	gtk_container_add(GTK_CONTAINER(frame), nbox);

	for (hw = node->xmlChildrenNode; hw; hw = hw->next)
		if (hw->type == XML_ELEMENT_NODE)
			build_widget(hw, nbox);

	return g_list_append(NULL, frame);
}

/* These create new widgets in the options window and set the appropriate
 * callbacks.
 */

static GList *build_toggle(Option *option, xmlNode *node, guchar *label)
{
	GtkWidget	*toggle;

	g_return_val_if_fail(option != NULL, NULL);

	toggle = gtk_check_button_new_with_label(_(label));

	may_add_tip(toggle, node);

	option->update_widget = update_toggle;
	option->read_widget = read_toggle;
	option->widget = toggle;

	g_signal_connect_swapped(toggle, "toggled",
			G_CALLBACK(option_check_widget), option);

	return g_list_append(NULL, toggle);
}

static GList *build_slider(Option *option, xmlNode *node, guchar *label)
{
	GtkAdjustment *adj;
	GtkWidget *hbox, *slide, *label_wid;
	int	min, max;
	int	fixed;
	int	showvalue;
	guchar	*end;

	g_return_val_if_fail(option != NULL, NULL);

	min = get_int(node, "min");
	max = get_int(node, "max");
	fixed = get_int(node, "fixed");
	showvalue = get_int(node, "showvalue");

	adj = GTK_ADJUSTMENT(gtk_adjustment_new(0,
				min, max, 1, 10, 0));

	hbox = gtk_hbox_new(FALSE, 4);

	if (label)
	{
		label_wid = gtk_label_new(_(label));
		gtk_misc_set_alignment(GTK_MISC(label_wid), 0, 0.5);
		gtk_box_pack_start(GTK_BOX(hbox), label_wid, FALSE, TRUE, 0);
		add_to_size_group(node, label_wid);
	}

	end = xmlGetProp(node, "end");
	if (end)
	{
		gtk_box_pack_end(GTK_BOX(hbox), gtk_label_new(_(end)),
				 FALSE, TRUE, 0);
		g_free(end);
	}

	slide = gtk_hscale_new(adj);

	if (fixed)
		gtk_widget_set_size_request(slide, adj->upper, 24);
	if (showvalue)
	{
		gtk_scale_set_draw_value(GTK_SCALE(slide), TRUE);
		gtk_scale_set_value_pos(GTK_SCALE(slide),
				GTK_POS_LEFT);
		gtk_scale_set_digits(GTK_SCALE(slide), 0);
	}
	else 
		gtk_scale_set_draw_value(GTK_SCALE(slide), FALSE);
	GTK_WIDGET_UNSET_FLAGS(slide, GTK_CAN_FOCUS);

	may_add_tip(slide, node);

	gtk_box_pack_start(GTK_BOX(hbox), slide, !fixed, TRUE, 0);

	option->update_widget = update_slider;
	option->read_widget = read_slider;
	option->widget = slide;

	g_signal_connect_swapped(adj, "value-changed",
			G_CALLBACK(option_check_widget), option);

	return g_list_append(NULL, hbox);
}

static GList *build_entry(Option *option, xmlNode *node, guchar *label)
{
	GtkWidget	*hbox;
	GtkWidget	*entry;
	GtkWidget	*label_wid;

	g_return_val_if_fail(option != NULL, NULL);

	hbox = gtk_hbox_new(FALSE, 4);

	if (label)
	{
		label_wid = gtk_label_new(_(label));
		gtk_misc_set_alignment(GTK_MISC(label_wid), 1.0, 0.5);
		gtk_box_pack_start(GTK_BOX(hbox), label_wid, FALSE, TRUE, 0);
	}

	entry = gtk_entry_new();
	gtk_box_pack_start(GTK_BOX(hbox), entry, TRUE, TRUE, 0);
	add_to_size_group(node, entry);
	may_add_tip(entry, node);

	option->update_widget = update_entry;
	option->read_widget = read_entry;
	option->widget = entry;

	g_signal_connect_data(entry, "changed",
			G_CALLBACK(option_check_widget), option,
			NULL, G_CONNECT_AFTER | G_CONNECT_SWAPPED);

	return g_list_append(NULL, hbox);
}

static GList *build_numentry(Option *option, xmlNode *node, guchar *label)
{
	GtkObject *adj;
	int	min, max, step;

	g_return_val_if_fail(option != NULL, NULL);

	min = get_int(node, "min");
	max = get_int(node, "max");
	step = MAX(1, get_int(node, "step"));

	adj = gtk_adjustment_new(min, min, max, step, step * 10, 0);

	return build_numentry_base(option, node, label, GTK_ADJUSTMENT(adj));
}
	
static GList *build_radio_group(Option *option, xmlNode *node, guchar *label)
{
	GList		*list = NULL;
	GtkWidget	*button = NULL;
	xmlNode		*rn;
	int		cols;

	g_return_val_if_fail(option != NULL, NULL);

	for (rn = node->xmlChildrenNode; rn; rn = rn->next)
	{
		if (rn->type == XML_ELEMENT_NODE)
		{
			button = build_radio(rn, button);
			g_signal_connect_swapped(button, "toggled",
				G_CALLBACK(option_check_widget), option);
			list = g_list_append(list, button);
		}
	}

	option->update_widget = update_radio_group;
	option->read_widget = read_radio_group;
	option->widget = button;

	cols = get_int(node, "columns");
	if (cols > 1)
	{
		GtkWidget *table;
		GList	*next;
		int	i, n;
		int	rows;

		n = g_list_length(list);
		rows = (n + cols - 1) / cols;

		table = gtk_table_new(rows, cols, FALSE);

		i = 0;
		for (next = list; next; next = next->next)
		{
			GtkWidget *button = GTK_WIDGET(next->data);
			int left = i / rows;
			int top = i % rows;

			gtk_table_attach_defaults(GTK_TABLE(table), button,
					left, left + 1, top, top + 1);

			i++;
		}

		g_list_free(list);
		list = g_list_prepend(NULL, table);
	}

	return list;
}

static GList *build_colour(Option *option, xmlNode *node, guchar *label)
{
	GtkWidget	*hbox, *da, *button, *label_wid;
	
	g_return_val_if_fail(option != NULL, NULL);

	hbox = gtk_hbox_new(FALSE, 4);

	if (label)
	{
		label_wid = gtk_label_new(_(label));
		gtk_misc_set_alignment(GTK_MISC(label_wid), 1.0, 0.5);
		gtk_box_pack_start(GTK_BOX(hbox), label_wid, TRUE, TRUE, 0);
	}

	button = gtk_button_new();
	da = gtk_drawing_area_new();
	gtk_widget_set_size_request(da, 64, 12);
	gtk_container_add(GTK_CONTAINER(button), da);
	g_signal_connect(button, "clicked", G_CALLBACK(open_coloursel), option);

	may_add_tip(button, node);
	
	gtk_box_pack_start(GTK_BOX(hbox), button, FALSE, TRUE, 0);

	option->update_widget = update_colour;
	option->read_widget = read_colour;
	option->widget = button;

	return g_list_append(NULL, hbox);
}

static GList *build_menu(Option *option, xmlNode *node, guchar *label)
{
	GtkWidget	*hbox, *om, *option_menu, *label_wid;
	xmlNode		*item;

	g_return_val_if_fail(option != NULL, NULL);

	hbox = gtk_hbox_new(FALSE, 4);

	label_wid = gtk_label_new(_(label));
	gtk_misc_set_alignment(GTK_MISC(label_wid), 1.0, 0.5);
	gtk_box_pack_start(GTK_BOX(hbox), label_wid, TRUE, TRUE, 0);

	option_menu = gtk_option_menu_new();
	gtk_box_pack_start(GTK_BOX(hbox), option_menu, FALSE, TRUE, 0);

	om = gtk_menu_new();

	for (item = node->xmlChildrenNode; item; item = item->next)
	{
		if (item->type == XML_ELEMENT_NODE)
			build_menu_item(item, om);
	}

	gtk_widget_show(om);
	gtk_option_menu_set_menu(GTK_OPTION_MENU(option_menu), om);
	add_to_size_group(node, option_menu);

	option->update_widget = update_menu;
	option->read_widget = read_menu;
	option->widget = option_menu;

	g_signal_connect_data(option_menu, "changed",
			G_CALLBACK(option_check_widget), option,
			NULL, G_CONNECT_AFTER | G_CONNECT_SWAPPED);

	return g_list_append(NULL, hbox);
}

static GList *build_font(Option *option, xmlNode *node, guchar *label)
{
	GtkWidget	*hbox, *button;
	GtkWidget	*active = NULL;
	int		override;

	g_return_val_if_fail(option != NULL, NULL);

	override = get_int(node, "override");

	hbox = gtk_hbox_new(FALSE, 4);

	if (override)
	{
		/* Add a check button to enable the font chooser. If off,
		 * the option's value is "".
		 */
		active = gtk_check_button_new_with_label(_(label));
		gtk_box_pack_start(GTK_BOX(hbox), active, FALSE, TRUE, 0);
		g_signal_connect(active, "toggled",
				 G_CALLBACK(toggle_active_font), option);
	}
	else
		gtk_box_pack_start(GTK_BOX(hbox), gtk_label_new(_(label)),
				FALSE, TRUE, 0);

	button = gtk_button_new_with_label("");
	gtk_box_pack_start(GTK_BOX(hbox), button, FALSE, TRUE, 0);

	option->update_widget = update_font;
	option->read_widget = read_font;
	option->widget = GTK_BIN(button)->child;
	may_add_tip(button, node);

	g_object_set_data(G_OBJECT(option->widget), "rox_override", active);

	g_signal_connect(button, "clicked", G_CALLBACK(open_fontsel), option);

	return g_list_append(NULL, hbox);
}

static void button_patch_set_colour(GtkWidget *button, GdkColor *colour)
{
	GtkStyle   	*style;
	GtkWidget	*patch;

	patch = GTK_BIN(button)->child;

	style = gtk_style_copy(GTK_WIDGET(patch)->style);
	style->bg[GTK_STATE_NORMAL].red = colour->red;
	style->bg[GTK_STATE_NORMAL].green = colour->green;
	style->bg[GTK_STATE_NORMAL].blue = colour->blue;
	gtk_widget_set_style(patch, style);
	g_object_unref(G_OBJECT(style));

	if (GTK_WIDGET_REALIZED(patch))
		gdk_window_clear(patch->window);
}

static void load_options(xmlDoc *doc)
{
	xmlNode *root, *node;

	root = xmlDocGetRootElement(doc);
	
	g_return_if_fail(strcmp(root->name, "Options") == 0);

	for (node = root->xmlChildrenNode; node; node = node->next)
	{
		gchar *value, *name;

		if (node->type != XML_ELEMENT_NODE)
			continue;
		if (strcmp(node->name, "Option") != 0)
			continue;
		name = xmlGetProp(node, "name");
		if (!name)
			continue;

		value = xmlNodeGetContent(node);

		if (g_hash_table_lookup(loading, name))
			g_warning("Duplicate option found!");

		g_hash_table_insert(loading, name, value);

		/* (don't need to free name or value) */
	}
}

/* Process one line from the options file (\0 term'd).
 * Returns NULL on success, or a pointer to an error message.
 * The line is modified.
 */
static const char *process_option_line(gchar *line)
{
	gchar 		*eq, *c;
	gchar		*name = line;

	g_return_val_if_fail(option_hash != NULL, "No registered options!");

	eq = strchr(line, '=');
	if (!eq)
		return _("Missing '='");

	c = eq - 1;
	while (c > line && (*c == ' ' || *c == '\t'))
		c--;
	c[1] = '\0';
	c = eq + 1;
	while (*c == ' ' || *c == '\t')
		c++;

	if (g_hash_table_lookup(loading, name))
		return "Duplicate option found!";

	g_hash_table_insert(loading, g_strdup(name), g_strdup(g_strstrip(c)));

	return NULL;
}
