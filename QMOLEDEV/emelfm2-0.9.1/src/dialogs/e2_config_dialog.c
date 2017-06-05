/* $Id: e2_config_dialog.c 2988 2013-12-03 07:38:31Z tpgww $

Copyright (C) 2003-2013 tooar <tooar@emelfm2.net>
Portions copyright (C) 1999 Michael Clark

This file is part of emelFM2.
emelFM2 is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

emelFM2 is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
See the GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with emelFM2; see the file GPL. If not, see http://www.gnu.org/licenses.
*/

#include "emelfm2.h"
#include <string.h>
#include "e2_dialog.h"
#include "e2_config_dialog.h"
#include "e2_option_tree.h"
#include "e2_filelist.h"
#include "e2_task.h"
#include "e2_plugins.h"
#include "e2_filetype.h"
#ifdef E2_MOUSECUSTOM
# include "e2_mousebinding.h"
#endif
#include "e2_icons.h"

//define to apply scrolledwindow to each page of the config notebeok, allowing
//omission of bars for whole book, which cause nested bars for tree-set data
#define NEWSCROLL

typedef struct _E2_TreeCfgData
{
	E2_OptionSet *set;
	GtkTreePath *tpath;
} E2_TreeCfgData;

//common vars we need in several functions
GtkWidget *config_dialog = NULL;	//this is accessed by other files
static gchar *page_last_name = NULL;	//the group name for the notebook page last opened
extern time_t last_work_time;

// =========== functions supporting single-page config dialogs ===============

/**
@brief dummy 'apply' function used when none is supplied by caller
*/
static void _e2_cd1_no_application (void)
{}
/**
@brief clean up at the end of a one-page config dialog

This relates to tree-options only

@param rt pointer to data struct for the dialog
@param flushtype TRUE to restore backed-up data to the tree, FALSE to cleanup only

@return
*/
static void _e2_cd1_cancel (E2_SpecificConfDialogRuntime *rt, gboolean flushtype)
{
	//restore or abandon the default tree data
	e2_option_tree_unbackup (rt->set, flushtype);
	gtk_widget_destroy (rt->dialog);
	//cleanup any copy/cut buffer data
	g_hash_table_destroy (tree_view_buffer_hash);
	DEALLOCATE (E2_SpecificConfDialogRuntime, rt);
	config_dialog = NULL;
}

  /*********************/
 /***** callbacks *****/
/*********************/

/**
@brief callback for dialog "delete-event" signal

@param widget UNUSED pointer to the widget that was 'deleted'
@param rt pointer to data struct for the dialog

@return
*/
/* handled in default response
static void _e2_cd1_close_cb (GtkWidget *widget, E2_SpecificConfDialogRuntime *rt)
{
	NEEDCLOSEBGL
	_e2_cd1_cancel (rt, TRUE);
	NEEDOPENBGL
} */
/**
@brief close the dialog when esc key is pressed

This is the callback for @a widget keypress signal

@param widget UNUSED pointer to the widget that received the keypress
@param event event data struct
@param rt pointer to data struct for the dialog

@return TRUE if esc key pressed, otherwise FALSE
*/
static gboolean _e2_cd1_key_press_cb (GtkWidget *widget, GdkEventKey *event,
	E2_SpecificConfDialogRuntime *rt)
{
	if (event->keyval == GDK_Escape)
	{  //restore tree data and clean up
		NEEDCLOSEBGL
		_e2_cd1_cancel (rt, TRUE);
		NEEDOPENBGL
		return TRUE;
	}
	return FALSE;
}
/**
@brief handle button click in a one-page config dialog

This is the callback for a buttonclick or other response signal
Note: applies only to tree options

@param dialog the dialog where the response was generated
@param response the response assigned to @a button
@param rt pointer to data struct for the dialog

@return
*/
static void _e2_cd1_response_cb (GtkDialog *dialog, gint response,
	E2_SpecificConfDialogRuntime *rt)
{
	NEEDCLOSEBGL
	switch (response)
	{
		case E2_RESPONSE_APPLY:
		{
			void (*apply_new) () = rt->apply_function; //keep this thru the cleanup
			//clean up without restoring tree data
			_e2_cd1_cancel (rt, FALSE);
			//perform post-dialog data processing, via caller-specified 'apply'
			apply_new ();
//			e2_option_disable_config_checks (); in write func
			//update stored config data
			e2_option_file_write (NULL);
			//update stamp, to prevent auto-refresh
			e2_fs_touch_config_dir ();
//			e2_option_enable_config_checks ();
		}
			break;
		case E2_RESPONSE_MORE:
			(*rt->apply_function) ();
/*			//clear old backup data
			e2_option_tree_unbackup (rt->set, FALSE);
			//renew backup data, in case we cancel later
			e2_option_tree_backup (rt->set); */
			break;
		default:
			_e2_cd1_cancel (rt, TRUE);
			break;
	}
	NEEDOPENBGL
}
/**
@brief create single-page config dialog for a tree-type option set

This creates and optionally shows a dialog similar to a page in the overall config
dialog. It applies only to tree-type option sets.
Clicking 'apply' or 'ok' performs a caller-specified fn (if any) and updates the config file

@param set_name string, non-translated 'internal' name of the tree optionset to be worked on
@param apply_function ptr to caller's 'apply changes' function, or NULL if nothing to be done
@param showit boolean TRUE to show the dialog immediately, FALSE if not

@return E2_SpecificConfDialogRuntime created, with dialog data, or NULL if error occurred
*/
E2_SpecificConfDialogRuntime *e2_config_dialog_single (gchar *set_name,
	void (*apply_function)(void), gboolean showit)
{
	E2_SpecificConfDialogRuntime *rt = ALLOCATE (E2_SpecificConfDialogRuntime);
	CHECKALLOCATEDWARN (rt, return NULL;)
	rt->set = e2_option_get_simple (set_name);
	rt->apply_function = (apply_function != NULL) ? apply_function : _e2_cd1_no_application;
  //set config_dialog too, so that list-cell-renderer can access the dialog
	config_dialog =
	rt->dialog = e2_dialog_create (NULL, NULL, _("configuration"),
		(ResponseFunc)_e2_cd1_response_cb, rt);
	gtk_widget_set_size_request (rt->dialog, 150, 50);
	GtkWidget *dialog_vbox =
#ifdef USE_GTK2_14
		gtk_dialog_get_content_area (GTK_DIALOG (rt->dialog));
#else
		GTK_DIALOG (rt->dialog)->vbox;
#endif
	gtk_container_set_border_width (GTK_CONTAINER (dialog_vbox), E2_PADDING);
	GtkWidget *frame = e2_widget_add_frame (dialog_vbox, FALSE, 0, NULL, FALSE);
	gchar *label_text2 = g_strconcat ("<b>", rt->set->group, "</b>", NULL);
	GtkWidget *label = e2_widget_add_mid_label (NULL, label_text2, 0.5, FALSE, 0);
	g_free (label_text2);
	gtk_misc_set_padding (GTK_MISC (label), E2_PADDING_XSMALL, E2_PADDING_XSMALL);
	gtk_container_add (GTK_CONTAINER (frame), label);

	e2_option_connect (rt->dialog, FALSE);
	g_object_set_data (G_OBJECT (rt->dialog), "dialog-form", GINT_TO_POINTER (E2_CFGDLG_SINGLE));

	//receptacle for cut/copied row(s) data generated from context menu
	//the keys are constant strings, not to be replaced or freed
	tree_view_buffer_hash = g_hash_table_new_full (g_str_hash, g_str_equal,
		NULL, (GDestroyNotify) e2_option_tree_menu_hash_clean);
	//add tree config data
	//(this also arranges backup of tree data)
	e2_option_tree_add_widget (rt->dialog, TRUE, dialog_vbox, rt->set);

//	g_signal_connect (G_OBJECT (rt->dialog), "delete-event",
//		G_CALLBACK (_e2_cd1_close_cb), rt);
	g_signal_connect_after (G_OBJECT (rt->dialog), "key-press-event",
		G_CALLBACK (_e2_cd1_key_press_cb), rt);
	//prepare dialog with buttons

	E2_Button discard_btn;
	discard_btn = E2_BUTTON_DISCARD;
	discard_btn.showflags |= E2_BTN_DEFAULT;

	gtk_window_set_default_size (GTK_WINDOW (rt->dialog), 450, 350);
	E2_BUTTON_APPLY.showflags &= ~E2_BTN_DEFAULT;	//CHECKME local copy ?
	e2_dialog_show (rt->dialog, app.main_window, 0,
		&E2_BUTTON_MORE, &discard_btn, &E2_BUTTON_APPLY, NULL);
	if (showit)
		gtk_widget_show_all (rt->dialog);

	return rt;
}

// ================== end of single-page config things ==================

/**
@brief set popup menu position

This function is supplied when calling gtk_menu_popup(), to position
the displayed menu.
set @a push_in to TRUE for menu completely inside the screen,
FALSE for menu clamped to screen size

@param menu UNUSED the GtkMenu to be positioned
@param x place to store gint representing the menu left
@param y place to store gint representing the menu top
@param push_in place to store pushin flag
@param treeview where the menu is to be shown

@return
*/
void e2_confdlg_menu_set_position (GtkMenu *menu,
	gint *x, gint *y, gboolean *push_in, GtkWidget *treeview)
{
	GtkWidget *window = gtk_widget_get_toplevel (treeview);
#ifdef USE_GTK2_18
	if (gtk_widget_is_toplevel (window))
#else
	if (GTK_WIDGET_TOPLEVEL (window))
#endif
	{
		gint left, top;
		gtk_window_get_position (GTK_WINDOW (window), &left, &top);
		GtkAllocation alloc;
#ifdef USE_GTK2_18
		gtk_widget_get_allocation (treeview, &alloc);
#else
		alloc = treeview->allocation;
#endif
		*x = left + alloc.x + alloc.width/3;
		*y = top + alloc.y + alloc.height/3;
		*push_in = FALSE;
	}
	else
	{
		//FIXME
		*x = 50;
		*y = 50;
		*push_in = TRUE;
	}
}
/**
@brief construct and pop up destroyable context-menu for categories treeview
@param treeview the widget to which the menu belongs
@param event_button which mouse button was clicked (0 for a menu key)
@param event_time time that the event happened (0 for a menu key)@param time
@param data UNUSED data

@return
*/
static void _e2_confdlg_show_context_menu (GtkWidget *treeview, guint event_button,
	guint32 event_time, gpointer data)
{
	GtkWidget *menu = e2_menu_get ();
	e2_menu_add (menu, _("_Expand"), STOCK_NAME_ZOOM_IN,
		_("Expand all rows"), e2_tree_expand_all_cb, treeview);
	e2_menu_add (menu, _("C_ollapse"), STOCK_NAME_ZOOM_OUT,
		_("Collapse all rows"), e2_tree_collapse_all_cb, treeview);
	g_signal_connect (G_OBJECT (menu), "selection-done",
		G_CALLBACK (e2_menu_selection_done_cb), NULL);
	if (event_button == 0)
		gtk_menu_popup (GTK_MENU (menu), NULL, NULL,
			(GtkMenuPositionFunc) e2_confdlg_menu_set_position,
			treeview, 0, event_time);
	else
		//this was a button-3 click
		gtk_menu_popup (GTK_MENU (menu), NULL, NULL,
			NULL, NULL, event_button, event_time);
}

  /*********************/
 /***** callbacks *****/
/*********************/

/**
@brief callback for options hash cleanup
Size-group opjects in the hash need to be unreffed so that they will be cleared
when all widgets in the group are destroyed
@param data pointer to data item to be cleaned

@return
*/
static void _e2_hashfree (gpointer data)
{
	if (GTK_IS_SIZE_GROUP (data))
		g_object_unref (data);
}

#ifndef NEWSCROLL
/**
@brief callback for show dialog

@param dialog UNUSED the dialog widget being shown
@param sw scrolled window containing the categories treeview

@return
*/
static void _e2_confdlg_show_cb (GtkWidget *dialog, GtkWidget *sw)
{
	NEEDCLOSEBGL
	gtk_scrolled_window_set_policy (GTK_SCROLLED_WINDOW (sw),
		GTK_POLICY_AUTOMATIC, GTK_POLICY_AUTOMATIC);
	NEEDOPENBGL
}
#endif
/**
@brief callback for show dialog

@param dialog the config dialog widget
@param rt pointer to data struct for the dialog

@return
*/
static void _e2_confdlg_show_cb2 (GtkWidget *dialog, E2_ConfigDialogRuntime *rt)
{
	gtk_notebook_set_current_page (rt->notebook, rt->openpage);
	gtk_widget_grab_focus (rt->treeview);
}
/**
@brief change dependent widget sensitivity, when the dependency is negated

@param button the activated option toggle-button
@param dependant the dependant option-widget

@return TRUE
*/
static gboolean _e2_confdlg_negdepends_cb (GtkWidget *button,
	GtkWidget *dependant)
{
	NEEDCLOSEBGL
	gtk_widget_set_sensitive (dependant,
#ifdef USE_GTK2_14
		!gtk_toggle_button_get_active (GTK_TOGGLE_BUTTON (button)));
#else
		!GTK_TOGGLE_BUTTON (button)->active);
#endif
	NEEDOPENBGL
	return TRUE;
}
/**
@brief change dependent widget sensitivity, when the dependency is not negated

@param button the activated option toggle-button
@param dependant the dependant option-widget

@return TRUE
*/
static gboolean _e2_confdlg_posdepends_cb_ (GtkWidget *button,
	GtkWidget *dependant)
{
	gtk_widget_set_sensitive (dependant,
#ifdef USE_GTK2_14
		gtk_toggle_button_get_active (GTK_TOGGLE_BUTTON (button)));
#else
		GTK_TOGGLE_BUTTON (button)->active);
#endif
	return TRUE;
}
/**
@brief cleanup after config dialog is finished

@return
*/
static void _e2_confdlg_clean (void)
{
	if (config_dialog != NULL)
	{
		printd (DEBUG, "config dialog widget destroy");
#ifdef USE_GTK2_18
		gtk_widget_get_allocation (config_dialog, &app.cfg_alloc);
#else
		app.cfg_alloc = config_dialog->allocation;
#endif
		gtk_widget_destroy (config_dialog);
		config_dialog = NULL;
	}
	//cleanup any copy/cut buffer data
	g_hash_table_destroy (tree_view_buffer_hash);
}
/**

@brief process commit or apply button-click

@param close TRUE if 'commit' button was pressed, FALSE if 'apply'
@param rt pointer to runtime data for the current dialog

@return
*/
static gboolean _e2_confdlg_apply (gboolean close, E2_ConfigDialogRuntime *rt)
{
	gboolean bchoice;
	gint ichoice;
	guint i;
	E2_OptionFlags buildflags;
	gchar *cchoice, *ccurrent;
	gpointer *walker;
	E2_OptionSet *set;

	printd (DEBUG, "config dialog callback: %s", (close) ? "commit" : "apply");
	//prevent interruptions - disable all refreshing
	e2_filelist_disable_refresh ();
	e2_option_disable_config_checks ();

	buildflags = 0;

	//update all options that were included in the dialog
	for (i = 0, walker = options_array->pdata; i < options_array->len; i++, walker++)
	{
		set = *walker;
		if (set->widget != NULL)	//this is the flag for included-in-dialog
		{
			switch (set->type)
			{
				case E2_OPTION_TYPE_BOOL:
					bchoice = gtk_toggle_button_get_active (GTK_TOGGLE_BUTTON (set->widget));
					if (e2_option_bool_get_direct (set) != bchoice)
					{
						e2_option_bool_set_direct (set, bchoice);
						buildflags |= set->flags;
					}
					break;
				case E2_OPTION_TYPE_INT:
					ichoice = gtk_spin_button_get_value_as_int (GTK_SPIN_BUTTON (set->widget));
					if (e2_option_int_get_direct (set) != ichoice)
					{
						e2_option_int_set_direct (set, ichoice);
						buildflags |= set->flags;
					}
					break;
				case E2_OPTION_TYPE_FONT:
				case E2_OPTION_TYPE_STR:
				case E2_OPTION_TYPE_COLOR:
					cchoice = gtk_editable_get_chars (GTK_EDITABLE (set->widget), 0, -1);
					ccurrent = e2_option_str_get_direct (set);
					if (strcmp (cchoice, ccurrent))
					{
						if (set->type == E2_OPTION_TYPE_COLOR)
							e2_option_color_set_str_direct (set, cchoice);
						else
							e2_option_str_set_direct (set, cchoice);
						buildflags |= set->flags;
					}
					g_free (cchoice);
					break;
				case E2_OPTION_TYPE_SEL:
					ichoice = gtk_combo_box_get_active (GTK_COMBO_BOX (set->widget));
					if (e2_option_sel_get_direct (set) != ichoice)
					{
						e2_option_sel_set_direct (set, ichoice);
						buildflags |= set->flags;
					}
					break;
				case E2_OPTION_TYPE_TREE:
					if (set->ex.tree.flags & E2_OPTION_TREE_SET_EDITED)
					{
						buildflags |= set->flags;
						set->ex.tree.flags &= ~E2_OPTION_TREE_SET_EDITED;
					}
					//abandon backed-up tree-option data
					e2_option_tree_unbackup (set, FALSE);
					//if the config dialog is not finished, get ready for next ok/apply
					//FIXME this not needed if backup triggered by dirty tree
					if (!close)
						e2_option_tree_backup (set);
					break;
				default:
					break;
			}
		}
	}

	//if this is a 'commit' button response, close the dialog etc
	if (close)
		_e2_confdlg_clean ();

	e2_utils_update_gtk_settings ();
	//rebuild only what is needed ...
	//the 'special' (non-window-specific) rebuilds are handled in the
	//same order as for a session-start
	if (buildflags & E2_OPTION_FLAG_BUILDKEYS)
		e2_keybinding_clean ();
#ifdef E2_MOUSECUSTOM
	if (buildflags & E2_OPTION_FLAG_BUILDBUTTONS)
	{
		e2_mousebinding_clean ();
#ifdef E2_PTRGESTURES
		e2_mousegesture_clean ();
#endif
	}
#endif
#ifdef E2_ICONCACHE
	if (buildflags & E2_OPTION_FLAG_BUILDICONS)
	{
		e2_icons_cache_clear ();
		e2_icons_cache_init ();
	}
#endif
	if (buildflags & E2_OPTION_FLAG_BUILDPLUGS)
		e2_plugins_update_configured ();

	if (buildflags & E2_OPTION_FLAG_BUILDSAMEBARS)
	{	//user wants toolbar buttons conformed
		//FIXME distinguish between style & size
		gint index1 = e2_option_sel_get ("allbars-style");
		gint index2 = e2_option_sel_get ("allbars-isize");
		E2_ToolbarData **thisbar;
		for (thisbar = app.bars; *thisbar != NULL; thisbar++)
		{
			e2_option_sel_set_direct ((*thisbar)->rt->style, index1);
			e2_option_sel_set_direct ((*thisbar)->rt->isize, index2);
		}
	}

#ifdef E2_RAINBOW
	if (buildflags & E2_OPTION_FLAG_BUILDFILES)
		e2_option_color_filetypes_sync (); //may be a color-data change
#endif

	if (buildflags & E2_OPTION_FLAG_BUILDPANES)
	{	//check if we want the panes to be the same
		const gchar *src_basename, *dest_basename;
		gchar *opt_name;
		gint srcnum, destnum, intvalue;
		//decide whether pane 1 or 2 or neither is the driver
		gboolean boolvalue = e2_option_bool_get ("pane1-uses-other");
		if (boolvalue)
		{
			srcnum = 2;
			destnum = 1;
		}
		else
		{
			boolvalue = e2_option_bool_get ("pane2-uses-other");
			if (boolvalue)
			{
				srcnum = 1;
				destnum = 2;
			}
		}
		if (boolvalue)
		{	//the change wasn't merely to cancel the same-panes option
			//conform columns
			for (i = 1; i < MAX_COLUMNS; i++)	//column 0 never changes
			{
				opt_name = g_strdup_printf ("pane%d-show-column%d", srcnum, i);
				boolvalue = e2_option_bool_get (opt_name);
				g_free (opt_name);
				opt_name = g_strdup_printf ("pane%d-show-column%d", destnum, i);
				e2_option_bool_set (opt_name, boolvalue);
				g_free (opt_name);
			}
			//CHECKME also conform the column order ?

			if (e2_option_bool_get ("advanced-config"))
			{	//these are also shown in advancee mode
				//conform most other options set in e2_toolbar_options_register ()
				E2_BarType barnum = (srcnum == 1) ? E2_BAR_PANE1 : E2_BAR_PANE2;
				src_basename  = app.bars[barnum]->name;
				barnum = (destnum == 1) ? E2_BAR_PANE1 : E2_BAR_PANE2;
				dest_basename = app.bars[barnum]->name;
				//booleans
				gchar *bnames[5] = {"-show", "-tooltips", "-hori", "-relief", "-same"};  //option names, no translation
				for (i = 0; i < 5; i++)
				{
					opt_name = g_strconcat (src_basename, bnames[i], NULL);
					boolvalue = e2_option_bool_get (opt_name);
					g_free (opt_name);
					opt_name = g_strconcat (dest_basename, bnames[i], NULL);
					e2_option_bool_set (opt_name, boolvalue);
					g_free (opt_name);
				}
				//sels (except "-type" which can't automatically be conformed
				gchar *snames[3] = {"-space", "-style", "-isize"};  //option names, no translation
				for (i = 0; i < 3; i++)
				{
					opt_name = g_strconcat (src_basename, snames[i], NULL);
					intvalue = e2_option_sel_get (opt_name);
					g_free (opt_name);
					opt_name = g_strconcat (dest_basename, snames[i], NULL);
					e2_option_sel_set (opt_name, intvalue);
					g_free (opt_name);
				}
				//ints - can't validly set priority either
	/*			opt_name = g_strconcat (src_basename, "-priority", NULL);
				intvalue = e2_option_int_get (opt_name);
				g_free (opt_name);
				opt_name = g_strconcat (dest_basename, "-priority", NULL);
				e2_option_int_set (opt_name, intvalue);
				g_free (opt_name); */
			}
		}
	}

	if (buildflags & E2_OPTION_FLAG_BUILDALL)
		e2_window_recreate (&app.window);	//also registers all key-bindings
	else if (buildflags & E2_OPTION_FLAG_BUILDPANES)
	{
		//rebuild filepane treeviews, & toolbars (in case they are destroyed with the panes)
		e2_window_recreate (&app.window);
/*		//FIXME more intelligent rebuild for this e.g no need to do output pane
		e2_pane_recreate (&app.pane1);
		e2_pane_recreate (&app.pane2);
//		e2_toolbar_create (&app.toolbar);	//this might be inside a destroyed pane
//		e2_toolbar_create (&app.commandbar);
		e2_pane_flag_active ();

		WAIT_FOR_EVENTS; //make sure pane parameters are set, before connecting cb's
		e2_window_pane_cb_setup (&app.window);

		e2_pane_change_dir (&app.pane1, app.pane1.path);
		e2_pane_change_dir (&app.pane2, app.pane2.path);
*/
	}
	else
	{
		if (buildflags & E2_OPTION_FLAG_BUILDBARS)
			e2_toolbar_recreate_all ();

		if (buildflags & E2_OPTION_FLAG_BUILDOUT)
			e2_output_update_style ();

		if (buildflags & E2_OPTION_FLAG_BUILDKEYS)
//CHECKME ok to re-register command-line key bindings, if bars re-created ?
//CHECKME maybe fileview bindings should be registered before the general
//button-press callback for the respective treeviews
			e2_keybinding_register_all ();
#ifdef E2_MOUSECUSTOM
		if (buildflags & E2_OPTION_FLAG_BUILDBUTTONS)
			e2_mousebinding_register_all (); //includes gestures if relevant
#endif
		if (buildflags & E2_OPTION_FLAG_BUILDLISTS)
		{
			e2_fileview_set_font ();	//FIXME do this only if needed
			//filelists will be refreshed only if dirty, so cd instead, but we
			//must trick the cd manager into thinking that it's a real change
			gchar *e = curr_view->dir + strlen (curr_view->dir) - 1;
			*e = '\0'; //strip trailing /
			e2_pane_change_dir (curr_pane, curr_pane->path);
			e = other_view->dir + strlen (other_view->dir) - 1;
			*e = '\0'; //strip trailing /
			e2_pane_change_dir (other_pane, other_pane->path);
		}
	}

	//aliases
	if ((buildflags & E2_OPTION_FLAG_BUILDALIAS)
		&& e2_option_bool_get ("command-use-aliases"))
			e2_alias_sync (&app.aliases);

	//filetypes
	if (buildflags & E2_OPTION_FLAG_BUILDFILES)
		e2_filetype_apply_allnew ();

	if (close)	//if this is a 'commit' button response
	{
		//update the stored config data
		e2_option_file_write (NULL);
		//update stamp, to prevent auto-refresh here
		e2_fs_touch_config_dir ();
		//may need to show the revised config file data
/*	FIXME this may cause crash
		if (!(buildflags & (E2_OPTION_FLAG_BUILDALL | E2_OPTION_FLAG_BUILDPANES)))
		{
#ifdef E2_FAM
			e2_filelist_request_refresh (curr_view->dir, FALSE);
			e2_filelist_request_refresh (other_view->dir, TRUE);
#else
			e2_filelist_check_dirty (GINT_TO_POINTER(1));
#endif
		}
*/
	}
	//set filelist monitoring according to current option
	if (e2_option_bool_get ("auto-refresh"))
	{
		if (!rt->refresh_files)
		{
			//option was not set before
			last_work_time = time (NULL);
			e2_filelist_start_refresh_checks ();
			rt->refresh_files = TRUE;	//in case we're continuing
		}
		else
			e2_filelist_enable_refresh ();
	}
	else if (rt->refresh_files)
	{
		//option was set before
		e2_filelist_stop_refresh_checks ();
		rt->refresh_files = FALSE;	//in case we're continuing
	}
	//after the config file update, set config monitoring according to current option
	if (e2_option_bool_get ("auto-refresh-config"))
	{
		if (!rt->refresh_cfg)
		{
			//option was not set before
			last_work_time = time (NULL);
			e2_option_start_config_checks ();
			rt->refresh_cfg = TRUE;	//in case we're continuing
		}
		else
			e2_option_enable_config_checks ();
	}
	else if (rt->refresh_cfg)
	{
		//option was set before
		e2_option_stop_config_checks ();
		rt->refresh_cfg = FALSE;	//in case we're continuing
	}

	return TRUE;
}
/**
@brief process a cancellation request

@return TRUE always
*/
static void _e2_confdlg_cancel_cb (void)
{
	printd (DEBUG, "config dialog cancel cb");
	if (config_dialog != NULL)
	{
		NEEDCLOSEBGL
		//NOTE this triggers an "edited" callback for any
		//treeview cell that is being edited at this time !
		_e2_confdlg_clean ();
		//revert all option trees to their last-saved state,
		//after the clean
		e2_option_tree_restore_all ();
//		gtk_widget_grab_focus (curr_view->treeview);
		NEEDOPENBGL
	}
}
/**
@brief determine what action to take after a dialog button click

@param widget the dialog where the response was generated
@param response the response assigned to @a button
@param rt pointer to data struct for the dialog

@return
*/
static void _e2_confdlg_response_cb (GtkDialog *dialog, gint response,
	E2_ConfigDialogRuntime *rt)
{
	NEEDCLOSEBGL
	switch (response)
	{
		case E2_RESPONSE_APPLY:
			_e2_confdlg_apply (TRUE, rt);	//does some cleanup
			DEALLOCATE (E2_ConfigDialogRuntime, rt);//CHECKME more cleanup ?
			break;
		case E2_RESPONSE_MORE:
			_e2_confdlg_apply (FALSE, rt);
			break;
		case E2_RESPONSE_USER1:	//revert to default options
		{
			DialogButtons choice = e2_dialog_warning (
			_("Reverting to default configuration cannot be undone"), NULL);
			if (choice == OK)
			{
				_e2_confdlg_clean ();	//MUST kill dialog before clearing option data
//				app.keytrans = TRUE;	//translate the re-created keybindings, later on
				printd (DEBUG, "At cfgdlg1, goto e2_option_refresh");
				e2_option_refresh (FALSE, TRUE);
				DEALLOCATE (E2_ConfigDialogRuntime, rt);
				WAIT_FOR_EVENTS
				return e2_config_dialog_create (NULL);
			}
		}
			break;
		case E2_RESPONSE_USER2:	//toggle between advanced & basic
			//same response for basic & advanced buttons - only 1 is displayed
			e2_option_bool_toggle ("advanced-config");
			_e2_confdlg_clean ();
			DEALLOCATE (E2_ConfigDialogRuntime, rt);
			e2_option_date_style (); //this option depends on advanced/basic
			e2_config_dialog_create (NULL);
			break;
		default:
			NEEDOPENBGL
			_e2_confdlg_cancel_cb ();
			NEEDCLOSEBGL
			DEALLOCATE (E2_ConfigDialogRuntime, rt);
			break;
	}
	NEEDOPENBGL
}
/**
@brief close the dialog if the esc key is pressed

@param widget UNUSED the focused widget when key was pressed
@param event ptr to event data struct
@param data UNUSED ptr to data specified when callback was connected

@return TRUE if esc key was pressed
*/
/* <Esc> key processing is done in the dialog's "negative response" process
gboolean e2_confdlg_key_press_cb (GtkWidget *widget, GdkEventKey *event,
	gpointer data)
{
/ *	if (event->keyval == GDK_Return
	 || event->keyval == GDK_KP_Enter
	 || event->keyval == GDK_ISO_Enter
	 || event->keyval == GDK_3270_Enter)  return activates any focused button
	{
//		NEEDOPENBGL
		gboolean ret = e2_confdlg_ok_cb (FALSE);
//		NEEDCLOSEBGL
		return ret;
	}
* /
	if (event->keyval == GDK_Escape)
	{
//		NEEDOPENBGL
		_e2_confdlg_cancel_cb ();
//		NEEDCLOSEBGL
		return TRUE;
	}
	return FALSE;
} */

//this static flag is ok because only 1 config dialog, and only 1 edit, at any time
static gint cancel_blocked = 0;
/**
@brief allow <Esc> press to cancel editing of the entry text without closing the dialog

@param widget UNUSED the focused widget when key was pressed
@param event ptr to event data struct
@param set ptr to option data

@return TRUE if <Esc> key was pressed
*/
static gboolean _e2_confdlg_key_press_cb (GtkWidget *widget, GdkEventKey *event,
	E2_OptionSet *set)
{
	if (g_atomic_int_get (&cancel_blocked) == 0)
	{
		g_signal_handlers_block_by_func (G_OBJECT (config_dialog),
			e2_dialog_key_neg_cb, config_dialog);
		g_atomic_int_set (&cancel_blocked, 1);
	}
	else if (event->keyval == GDK_Escape
		  || event->keyval == GDK_Return
		  || event->keyval == GDK_KP_Enter
		  || event->keyval == GDK_ISO_Enter
		  || event->keyval == GDK_3270_Enter)
	{
		if (event->keyval == GDK_Escape)
		{
			NEEDCLOSEBGL
			gtk_entry_set_text (GTK_ENTRY (set->widget), set->sval);
			gtk_editable_set_position (GTK_EDITABLE (set->widget), -1);
			NEEDOPENBGL
		}
		//revert <Esc> key handling when cell editing is finsished
		g_signal_handlers_unblock_by_func (G_OBJECT (config_dialog),
			e2_dialog_key_neg_cb, config_dialog);
		g_atomic_int_set (&cancel_blocked, 0);
		return (event->keyval == GDK_Escape);
	}
	return FALSE;
}
/**
@brief cancel localised <Esc> blocking

This is a callback for entry "focus-out-event",

@param widget UNUSED newly-departed widget or NULL
@param event UNUSED pointer to event data struct
@param data UNUSED data specified when callback was connected

@return FALSE to propagate the event to other handlers
*/
static gboolean _e2_confdlg_focus_out_cb (GtkWidget *widget, GdkEventFocus *event,
	gpointer data)
{
	if (g_atomic_int_get (&cancel_blocked) != 0)
	{
		g_signal_handlers_unblock_by_func (G_OBJECT (config_dialog),
			e2_dialog_key_neg_cb, config_dialog);
		g_atomic_int_set (&cancel_blocked, 0);
	}
	return FALSE;
}
/**
@brief show categories context menu if right-button pressed

@param treeview the categories treeview widget
@param event ptr to event data struct
@param data UNUSED ptr to data specified when callback was connected

@return TRUE for a right-button press
*/
static gboolean _e2_confdlg_button_press_cb (GtkWidget *treeview,
	GdkEventButton *event, gpointer data)
{
	if (event->button == 3
#ifdef E2_MOUSECUSTOM
		&& (event->state & E2_MODIFIER_MASK) == 0
#endif
		)
	{
		NEEDCLOSEBGL
		_e2_confdlg_show_context_menu (treeview, 3, event->time, data);
		NEEDOPENBGL
		return TRUE;
	}
	return FALSE;
}
/**
@brief menu-button press callback

@param treeview the widget where the press happened
@param data UNUSED ptr to data specified when callback was connected

@return TRUE always
*/
static gboolean _e2p_confdlg_popup_menu_cb (GtkWidget *treeview, gpointer data)
{
	guint32 event_time = gtk_get_current_event_time ();
	NEEDCLOSEBGL
	_e2_confdlg_show_context_menu (treeview, 0, event_time, data);
	NEEDOPENBGL
	return TRUE;
}
/**
@brief response callback for plugin selection dialog

@param dialog the dialog where the response was triggered
@param response the response assigned to the activated button widget
@param set ptr to plugins option set

@return
*/
static void _e2_confdlg_plugpick_response_cb (GtkDialog *dialog, gint response,
	E2_OptionSet *set)
{
	GSList *selected;
	NEEDCLOSEBGL
	switch (response)
	{
		case E2_RESPONSE_USER1:	//toggle hidden items display
			break;
		case GTK_RESPONSE_OK:
		case E2_RESPONSE_MORE:
			//returns localized (not UTF8) strings
			selected = gtk_file_chooser_get_filenames (GTK_FILE_CHOOSER (dialog));
			if (selected)
			{
				GSList *member;
				GtkTreeIter first, sibling;
				GtkTreeIter *pf = NULL, *ps = NULL;

				if (gtk_tree_model_get_iter_first (set->ex.tree.model, &first))
				{
					pf = &first;

					GtkTreeSelection *sel = gtk_tree_view_get_selection
						(GTK_TREE_VIEW (set->widget));
					if (gtk_tree_selection_get_selected (sel, NULL, &sibling))
						ps = &sibling;
					else
					{
						gint count = gtk_tree_model_iter_n_children
							(set->ex.tree.model, NULL);
						if (count > 1)
						{
							gtk_tree_model_iter_nth_child (set->ex.tree.model,
								&sibling, NULL, count-1);
							ps = &sibling;
						}
						else
							ps = pf;
					}
				}

				for (member = selected; member != NULL; member = member->next)
				{
					E2P_InitData pdata;
					gchar *plocal = member->data;
					if (e2_plugins_open_module (plocal, &pdata))
					{
						Plugin *p = (*pdata.init) (E2P_UIDATA);
						if ((p->flags & E2P_NOCFG) == 0)
						{
							if (pf != NULL)
							{
								GtkTreeIter work = first;
								//absolute filepath will always have at least 1 separator
								gchar *sep = strrchr (plocal, G_DIR_SEPARATOR);
								if (e2_tree_find_iter_from_str_simple (set->ex.tree.model,
									FILE_COL, sep + 1, &work, FALSE))
								{
									//duplicate entry, dump it
									p->module = pdata.module;
									e2_plugins_unload1 (p, TRUE);
									continue;
								}
							}
							e2_plugins_store_config_data (set->ex.tree.model,
								ps, p, TRUE, plocal);
							//if we started with an empty store, can't be duplication, so don't start matching
							e2_option_tree_flag_change (set);
							p->module = pdata.module;
							e2_plugins_unload1 (p, TRUE); //cleanup then dump
						}
					}
					g_free (plocal);
				}
				g_slist_free (selected);
			}
			else
			{	//conversion error
				//FIXME warn the user
			}
			if (response == E2_RESPONSE_MORE)
				break;
		default:
			gtk_widget_destroy (GTK_WIDGET (dialog));
			gtk_widget_grab_focus (set->widget);
			break;
	}
	NEEDOPENBGL
}
/**
@brief bring up a system find-file window to choose a plugin

@param button UNUSED clicked widget
@param set ptr to plugins option data struct

@return
*/
void e2_confdlg_choose_plugins_cb (GtkButton *button, E2_OptionSet *set)
{
	gchar *ppath;
	GtkTreeView *tvw = GTK_TREE_VIEW (set->widget);
	GtkTreePath *tpath;

	NEEDCLOSEBGL
	gtk_tree_view_get_cursor (tvw, &tpath, NULL); //NULL tpath (no plugins) is unlikely

	if (tpath != NULL)
	{
		GtkTreeIter iter;
		gchar *pname;

		gtk_tree_model_get_iter (set->ex.tree.model, &iter, tpath);
		gtk_tree_model_get (set->ex.tree.model, &iter, 5, &pname, 6, &ppath, -1);
		if (*pname == '\0')
		{
			g_free (ppath);
			ppath = NULL;	//we'll just set the dir in the opened dialog
		}
		else
		{
			if (*ppath == '\0')
			{
				g_free (ppath);
				ppath = g_strconcat (PLUGINS_DIR G_DIR_SEPARATOR_S, pname, NULL);	//mixed ascii & utf
			}
			else
			{
				gchar *freeme = ppath;
				ppath = g_build_filename (ppath, pname, NULL);
				g_free (freeme);
			}
		}
		g_free (pname);
	}
	else
		ppath = NULL;

	//no need for vfs support, local plugins only
	GtkWidget *dialog = gtk_file_chooser_dialog_new (NULL,
		GTK_WINDOW (config_dialog), GTK_FILE_CHOOSER_ACTION_OPEN, NULL, NULL);

	E2_Button more_btn = E2_BUTTON_MORE;
	more_btn.tip = _("Select and continue");
	more_btn.showflags |= E2_BTN_TIPPED;
	e2_dialog_add_defined_button (dialog, &more_btn);

	e2_dialog_setup_chooser (dialog,
		_("choose plugin"),
		ppath,	//full path or NULL
		GTK_FILE_CHOOSER_ACTION_OPEN,
		TRUE,	//show hidden
		TRUE,	//multi-selection
		GTK_RESPONSE_OK,	//default response
		STOCK_NAME_CANCEL, GTK_RESPONSE_CANCEL,
		STOCK_NAME_OPEN, GTK_RESPONSE_OK,
		NULL);

	if (ppath != NULL)
		g_free (ppath);
	else
		gtk_file_chooser_set_current_folder (GTK_FILE_CHOOSER (dialog), PLUGINS_DIR);//localised

	GtkFileFilter *filter = gtk_file_filter_new ();
	gtk_file_filter_set_name (GTK_FILE_FILTER (filter), _("plugin"));
	gtk_file_filter_add_pattern (filter, "e2p*.so");
	gtk_file_chooser_add_filter (GTK_FILE_CHOOSER (dialog), filter);

	g_signal_connect (G_OBJECT (dialog), "response",
		G_CALLBACK (_e2_confdlg_plugpick_response_cb), set);

	e2_dialog_setup (dialog, config_dialog);
	gtk_widget_show (dialog);
	NEEDOPENBGL
}

  /**************************/
 /***** font callbacks *****/
/**************************/

#ifdef USE_GTK3_2
/**
@brief callback for signal issued when displayed font is activated

@param chooser UNUSED interface object
@param font_str the name of the activated font
@param entry config dialog set-widget

@return
*/
static void _e2_confdlg_font_activate_cb (GtkFontChooser *chooser, gchar *font_str,
	GtkWidget *entry)
{
	if (font_str == NULL)
		font_str = "";
	NEEDCLOSEBGL
	gtk_entry_set_text (GTK_ENTRY (entry), font_str);
	NEEDOPENBGL
}
/**
@brief font selection dialog response callback

@param dialog the dialog
@param response enumerator of type of re
@param entry config dialog set-widget

@return
*/
static void _e2_confdlg_font_response_cb (GtkDialog *dialog, gint response,
	GtkWidget *entry)
{
	NEEDCLOSEBGL
	gtk_widget_hide (GTK_WIDGET (dialog));
	if (response == GTK_RESPONSE_OK)
	{
		gchar *font_str = gtk_font_chooser_get_font (GTK_FONT_CHOOSER (dialog));
		if (font_str != NULL)
		{
			gtk_entry_set_text (GTK_ENTRY (entry), font_str);
			g_free (font_str);
		}
	}
	else if (response == GTK_RESPONSE_DELETE_EVENT)
	{
		const gchar *title = gtk_window_get_title (GTK_WINDOW (dialog));
		g_object_set_data (G_OBJECT (config_dialog), title, NULL);
	}
	NEEDOPENBGL
}
#else
/**
@brief accept font selection

@param button
@param entry config dialog set-widget

@return
*/
static void _e2_confdlg_font_select_ok_cb (GtkWidget *button, GtkWidget *entry)
{
	GtkWidget *dialog = g_object_get_data (G_OBJECT (button), "dialog_widget");
	NEEDCLOSEBGL
	const gchar *title = gtk_window_get_title (GTK_WINDOW (dialog));
	gchar *font_str = gtk_font_selection_dialog_get_font_name
		(GTK_FONT_SELECTION_DIALOG (dialog));
	gtk_entry_set_text (GTK_ENTRY (entry), font_str);
	NEEDOPENBGL
	g_free (font_str);
	g_object_set_data (G_OBJECT (config_dialog), title, NULL);
}
#endif
/**
@brief cancel font selection

@param button UNUSED the activated button widget
@param dialog the font-selection dialog widget

@return
*/
static void _e2_confdlg_font_select_cancel_cb (GtkWidget *button, GtkWidget *dialog)
{
	NEEDCLOSEBGL
	const gchar *title = gtk_window_get_title (GTK_WINDOW (dialog));
	NEEDOPENBGL
	g_object_set_data (G_OBJECT (config_dialog), title, NULL);
}
/**
@brief create font-selection dialog

@param button the clicked button
@param set pointer to data struct for the option to be altered

@return
*/
static void _e2_confdlg_font_select_cb (GtkButton *button, E2_OptionSet *set)
{
#ifdef USE_GTK3_2
	GtkFontChooserDialog *dialog;
#else
	GtkFontSelectionDialog *dialog;
#endif
	gchar *title = g_strdup_printf (_("Choose font: %s"), set->desc);
	dialog = g_object_get_data (G_OBJECT (config_dialog), title);
	NEEDCLOSEBGL
	if (dialog != NULL)
		gtk_window_present (GTK_WINDOW (dialog));
	else
	{
#ifdef USE_GTK3_2
		dialog = (GtkFontChooserDialog*) gtk_font_chooser_dialog_new (title, GTK_WINDOW (config_dialog));
		gtk_font_chooser_set_font (GTK_FONT_CHOOSER (dialog), gtk_entry_get_text (GTK_ENTRY (set->widget)));
		gtk_font_chooser_set_show_preview_entry (GTK_FONT_CHOOSER (dialog), FALSE);
		g_signal_connect (G_OBJECT (dialog), "font-activated",
			G_CALLBACK (_e2_confdlg_font_activate_cb), set->widget);
		g_signal_connect (G_OBJECT (dialog), "response",
			G_CALLBACK (_e2_confdlg_font_response_cb), set->widget);
#else
		dialog = (GtkFontSelectionDialog*) gtk_font_selection_dialog_new (title);
# ifndef USE_GTK3_0
		gtk_widget_destroy (
#  ifdef USE_GTK2_14
			gtk_font_selection_dialog_get_apply_button (dialog)
#  else
			dialog->apply_button
#  endif
		);
# endif
		g_object_set_data (
# ifdef USE_GTK2_14
			G_OBJECT (gtk_font_selection_dialog_get_ok_button (dialog)),
# else
			G_OBJECT (dialog->ok_button),
# endif
			"dialog_widget", GTK_WIDGET (dialog));
		gtk_font_selection_dialog_set_font_name (dialog,
			gtk_entry_get_text (GTK_ENTRY (set->widget)));
# ifdef E2_COMPOSIT
		e2_window_set_opacity (GTK_WIDGET (dialog), DIALOG_OPACITY_LEVEL);	//constant opacity for dialogs
# endif
		g_signal_connect (
# ifdef USE_GTK2_14
			G_OBJECT (gtk_font_selection_dialog_get_ok_button (dialog)),
# else
			G_OBJECT (dialog->ok_button),
# endif
			"clicked",
			G_CALLBACK (_e2_confdlg_font_select_ok_cb), set->widget);
		g_signal_connect (
# ifdef USE_GTK2_14
			G_OBJECT (gtk_font_selection_dialog_get_cancel_button (dialog)),
# else
			G_OBJECT (dialog->cancel_button),
# endif
			"clicked",
			G_CALLBACK (_e2_confdlg_font_select_cancel_cb), dialog);
#endif //use GTK3_2
		g_signal_connect (G_OBJECT (dialog), "delete-event",
			G_CALLBACK (_e2_confdlg_font_select_cancel_cb), NULL);
		g_object_set_data_full (G_OBJECT (config_dialog), title, dialog,
			(gpointer) gtk_widget_destroy);
		//CHECKME the normal dialog properties like close on Esc press ?
		e2_dialog_show (GTK_WIDGET (dialog), config_dialog, 0, NULL);
	}
	NEEDOPENBGL
	g_free (title);
}
/**
@brief update font example

@param entry widget containing name of font
@param label label widget to be updated with example of the named font

@return
*/
static void _e2_confdlg_font_entry_cb (GtkEditable *entry, GtkWidget *label)
{
	NEEDCLOSEBGL
	gchar *font_str = gtk_editable_get_chars (entry, 0, -1);
	if ((font_str != NULL) && (*font_str != '\0') && (strlen (font_str) > 2))
	{
		gchar *label_text = g_strdup_printf ("\t<small><i>%s</i></small> <span font_desc=\"%s\">%s</span>",
			_("example:"), font_str, _("abcd efgh ABCD EFGH"));
		gtk_label_set_markup (GTK_LABEL (label), label_text);
		g_free (label_text);
	}
	NEEDOPENBGL
	g_free (font_str);
}

  /*************************/
 /**** color callbacks ****/
/*************************/

#ifdef E2_RAINBOW
/**
@brief response callback for filetype-color selection dialog

@param dialog the dialog where the response was triggered
@param response the response assigned to the activated button widget
@param set ptr to plugins option data struct

@return
*/
static void _e2_confdlg_colorpick_response_cb (GtkDialog *dialog, gint response,
	E2_TreeCfgData *data)
{
#ifdef USE_GTK3_4
	GdkRGBA color;
	gchar color_str[8];
#else
	GdkColor color;
#endif
	GtkTreeIter iter;

	NEEDCLOSEBGL
	switch (response)
	{
#ifdef USE_GTK3_4
		case GTK_RESPONSE_OK:
			gtk_color_chooser_get_rgba (GTK_COLOR_CHOOSER (dialog), &color);
			snprintf (color_str, sizeof(color_str), "#%.2X%.2X%.2X",
				(gint)(color.red * 255), (gint)(color.green * 255), (gint)(color.blue * 255));
#else
		case E2_RESPONSE_APPLY:
			gtk_color_selection_get_current_color (
# ifdef USE_GTK2_14
				GTK_COLOR_SELECTION (gtk_color_selection_dialog_get_color_selection
					(GTK_COLOR_SELECTION_DIALOG (dialog))),
# else
				GTK_COLOR_SELECTION (GTK_COLOR_SELECTION_DIALOG (dialog)->colorsel),
# endif
				&color);
			gchar *color_str = e2_utils_color2str (&color);
#endif
			gtk_tree_model_get_iter (data->set->ex.tree.model, &iter, data->tpath);
			gtk_tree_store_set (GTK_TREE_STORE (data->set->ex.tree.model),
				&iter, 2, color_str, -1);
			e2_option_tree_flag_change (data->set);
#ifndef USE_GTK3_4
			g_free (color_str);
#endif
		default:
			gtk_widget_destroy (GTK_WIDGET (dialog));
			gtk_tree_path_free (data->tpath);
			g_free (data);
			break;
	}
	NEEDOPENBGL
}
/**
@brief callback for color-selection button in filetypes page
Color strings are stored in col 3 of rows with extensions title
(path depth 2) or a row with a specific extension (depth 3)
@param button UNUSED the clicked color button
@param set pointer to filetypes option data

@return
*/
void e2_confdlg_extcolorpick_cb (GtkButton *button, E2_OptionSet *set)
{
	GtkTreeView *tvw = GTK_TREE_VIEW (set->widget);
	GtkTreePath *path;
	gtk_tree_view_get_cursor (tvw, &path, NULL);
	if (path == NULL)
		return;

	GtkTreeIter iter;
	gchar *current;
	gboolean valid = FALSE;
	gint depth = gtk_tree_path_get_depth (path);
	switch (depth)
	{
		case 2:	//we're at an extensions/commands heading line
			gtk_tree_model_get_iter (set->ex.tree.model, &iter, path);
			gtk_tree_model_get (set->ex.tree.model, &iter, 1, &current, -1);
			//extensions node is ok
			if (!strcmp (current, _C(13)))
				valid = TRUE;
			g_free (current);
			break;
		case 3:	//we're at an extension or command line
		{
			GtkTreePath *nodepath = gtk_tree_path_copy (path);
			gtk_tree_path_up (nodepath);
			gtk_tree_model_get_iter (set->ex.tree.model, &iter, nodepath);
			gtk_tree_path_free (nodepath);
			gtk_tree_model_get (set->ex.tree.model, &iter, 1, &current, -1);
			if (!strcmp (current, _C(13)))
				valid = TRUE;
			g_free (current);
		}
		break;
		default:
		break;
	}
	NEEDCLOSEBGL
	if (!valid)
	{
		e2_output_print_error (_("Color data are not stored there"), FALSE);
		NEEDOPENBGL
		gtk_tree_path_free (path);
		return;
	}
//FIXME use ref in case user edits model
//	GtkTreeRowReference *ref = gtk_tree_row_reference_new (set->ex.tree.model, path);

	E2_TreeCfgData *data = MALLOCATE (E2_TreeCfgData);	//too small for slice
	CHECKALLOCATEDWARN (data, return;)
	data->set = set;
	data->tpath = path;
	gtk_tree_model_get_iter (set->ex.tree.model, &iter, path);
	gtk_tree_model_get (set->ex.tree.model, &iter, 2, &current, -1);
#ifdef USE_GTK3_4
	GdkRGBA color;
	if (*current == '\0')
		gdk_rgba_parse (&color, "black");
	else if (!gdk_rgba_parse (&color, current))
	{
		e2_output_print_error (_("The current color descriptor is not valid"), FALSE);
		gdk_rgba_parse (&color, "black");
	}
#else
	GdkColor color;
	if (*current == '\0')
		gdk_color_parse ("black", &color);
	else if (!gdk_color_parse (current, &color))
	{
		e2_output_print_error (_("The current color descriptor is not valid"), FALSE);
		gdk_color_parse ("black", &color);
	}
#endif
	g_free (current);

#ifdef USE_GTK3_4
	GtkWidget *dialog = gtk_color_chooser_dialog_new (_("Set filetype color"),
      GTK_WINDOW(config_dialog));
	GtkColorChooser *gcc = GTK_COLOR_CHOOSER (dialog);
	gtk_color_chooser_set_use_alpha (gcc, FALSE);
	gtk_color_chooser_set_rgba (gcc, &color);
	g_object_set (G_OBJECT(dialog), "show-editor", TRUE, NULL);
#else
	GtkWidget *dialog = gtk_color_selection_dialog_new
		(_("Set filetype color"));
	gtk_color_selection_set_current_color (
# ifdef USE_GTK2_14
		GTK_COLOR_SELECTION (gtk_color_selection_dialog_get_color_selection
				(GTK_COLOR_SELECTION_DIALOG (dialog))),
# else
		GTK_COLOR_SELECTION (GTK_COLOR_SELECTION_DIALOG (dialog)->colorsel),
# endif
		&color);
# ifdef E2_COMPOSIT
	e2_window_set_opacity (dialog, DIALOG_OPACITY_LEVEL);	//constant opacity for dialogs
# endif
	//substitute our own button
	E2_Button yes_btn;
	e2_button_derive (&yes_btn, &E2_BUTTON_APPLY, BTN_YES_CONTINUE);
	GtkWidget *bbox =
# ifdef USE_GTK2_14
		gtk_dialog_get_action_area (GTK_DIALOG (dialog));
# else
		GTK_DIALOG (dialog)->action_area;
# endif

	GList *btns = gtk_container_get_children (GTK_CONTAINER (bbox));
	gtk_container_remove (GTK_CONTAINER (bbox), btns->next->data);
	g_list_free (btns);
	GtkWidget *btn = e2_dialog_add_defined_button (dialog, &yes_btn);
#endif
	g_signal_connect (G_OBJECT (dialog), "response",
		G_CALLBACK (_e2_confdlg_colorpick_response_cb), data);

	e2_dialog_setup (dialog, config_dialog);
#ifndef USE_GTK3_4
	gtk_widget_show_all (btn);
#endif
	gtk_widget_show (dialog);
	NEEDOPENBGL
}
#endif

#ifdef USE_GTK3_4
/**
@brief non-filetypes color-selection-dialog response callback
@param dialog the dialog where the response was generated
@param response the generated response number
@param set pointer to option data struct
@return
*/
static void _e2_confdlg_color_response_cb (GtkDialog *dialog, gint response, E2_OptionSet *set)
{
	if (GTK_IS_WIDGET (dialog))
	{
		NEEDCLOSEBGL
		switch (response)
		{
			case GTK_RESPONSE_OK:
			{
				GdkRGBA color;
				gchar color_str[8];

				gtk_color_chooser_get_rgba (GTK_COLOR_CHOOSER (dialog), &color);
				snprintf (color_str, sizeof(color_str), "#%.2X%.2X%.2X",
					(gint)(color.red * 255), (gint)(color.green * 255), (gint)(color.blue * 255));
				gtk_entry_set_text (GTK_ENTRY (set->widget), color_str);
			}
			//no break here
			default:
			{
				//kill reference to the dialog
				const gchar *title = gtk_window_get_title (GTK_WINDOW (dialog));
				g_object_set_data (G_OBJECT (config_dialog), title, NULL);
				if (response != GTK_RESPONSE_DELETE_EVENT)
					gtk_widget_destroy ((GtkWidget*)dialog);
			}
			break;
		}
		NEEDOPENBGL
	}
}
#else //not USE_GTK3_4
/**
@brief

@param button the clicked button widget
@param entry

@return
*/
static void _e2_confdlg_color_select_ok_cb (GtkWidget *button, GtkWidget *entry)
{
	GdkColor color;
	NEEDCLOSEBGL
	GtkWidget *dialog = gtk_widget_get_toplevel (button);
	gtk_color_selection_get_current_color (
# ifdef USE_GTK2_14
		GTK_COLOR_SELECTION (gtk_color_selection_dialog_get_color_selection (GTK_COLOR_SELECTION_DIALOG (dialog))),
# else
		GTK_COLOR_SELECTION (GTK_COLOR_SELECTION_DIALOG (dialog)->colorsel),
# endif
		&color);
	gchar *color_str = e2_utils_color2str (&color);

	gtk_entry_set_text (GTK_ENTRY (entry), color_str);
	g_free (color_str);

	const gchar *title = gtk_window_get_title (GTK_WINDOW (dialog));
	NEEDOPENBGL
	g_object_set_data (G_OBJECT (config_dialog), title, NULL);
}
/**
@brief

@param button the clicked button widget
@param dialog

@return
*/
static void _e2_confdlg_color_select_cancel_cb (GtkWidget *button, GtkWidget *dialog)
{
	NEEDCLOSEBGL
	const gchar *title = gtk_window_get_title (GTK_WINDOW (dialog));
	NEEDOPENBGL
	g_object_set_data (G_OBJECT (config_dialog), title, NULL);
}
#endif //def USE_GTK3_4
/**
@brief callback for color-selection button in non-filetypes page

@param button the clicked color-button
@param set pointer to data for the relevant config option

@return
*/
static void _e2_confdlg_color_select_cb (GtkButton *button, E2_OptionSet *set)
{
	gchar *title = g_strdup_printf (_("Choose color: %s"), set->desc);
	GtkWidget *dialog = g_object_get_data (G_OBJECT (config_dialog), title);
	NEEDCLOSEBGL
	if (dialog != NULL)
		gtk_window_present (GTK_WINDOW (dialog));
	else
	{
		//CHECKME close on Esc press ?
#ifdef USE_GTK3_4
		GdkRGBA color;
		dialog = gtk_color_chooser_dialog_new (title, GTK_WINDOW(config_dialog));
		GtkColorChooser *gcc = GTK_COLOR_CHOOSER (dialog);
		gtk_color_chooser_set_use_alpha (gcc, FALSE);
		gchar *color_str = gtk_editable_get_chars (GTK_EDITABLE (set->widget), 0, -1);
		gdk_rgba_parse (&color, color_str);
		g_free (color_str);
		gtk_color_chooser_set_rgba (gcc, &color);
		g_object_set (G_OBJECT (dialog), "show-editor", TRUE, NULL);
		g_signal_connect (G_OBJECT (dialog), "response",
			G_CALLBACK(_e2_confdlg_color_response_cb), set);
#else
		GdkColor color;
		dialog = gtk_color_selection_dialog_new (title);
# ifdef USE_GTK2_20
		GtkWidget *btn;
		g_object_get (G_OBJECT (dialog), "help-button", &btn, NULL);
		gtk_widget_destroy (btn);
# else
		gtk_widget_destroy ((GTK_COLOR_SELECTION_DIALOG(dialog))->help_button);
# endif
		gchar *color_str = gtk_editable_get_chars (GTK_EDITABLE (set->widget), 0, -1);
		gdk_color_parse (color_str, &color);
		g_free (color_str);
		gtk_color_selection_set_current_color (GTK_COLOR_SELECTION (
# ifdef USE_GTK2_14
			gtk_color_selection_dialog_get_color_selection (GTK_COLOR_SELECTION_DIALOG(dialog))
# else
			(GTK_COLOR_SELECTION_DIALOG(dialog))->colorsel
# endif
			), &color);

# ifdef USE_GTK2_20
		g_object_get (G_OBJECT (dialog), "ok-button", &btn, NULL);
		g_signal_connect (G_OBJECT (btn),
# else
		g_signal_connect (G_OBJECT ((GTK_COLOR_SELECTION_DIALOG(dialog))->ok_button),
# endif
			"clicked",
			G_CALLBACK (_e2_confdlg_color_select_ok_cb), set->widget);

# ifdef USE_GTK2_20
		g_object_get (G_OBJECT (dialog), "cancel-button", &btn, NULL);
		g_signal_connect (G_OBJECT (btn),
# else
		g_signal_connect (G_OBJECT ((GTK_COLOR_SELECTION_DIALOG(dialog))->cancel_button),
# endif
			"clicked",
			G_CALLBACK (_e2_confdlg_color_select_cancel_cb), dialog);
		g_signal_connect (G_OBJECT (dialog),
			"delete-event", G_CALLBACK (_e2_confdlg_color_select_cancel_cb), dialog);
#endif //def USE_GTK3_4
		//make the dialog discoverable
		g_object_set_data_full (G_OBJECT (config_dialog), title, dialog,
			(void *) gtk_widget_destroy);
		gtk_window_set_resizable (GTK_WINDOW (dialog), TRUE);
		e2_dialog_show (GTK_WIDGET (dialog), config_dialog, 0, NULL);
	}
	NEEDOPENBGL
	g_free (title);
}
/**
@brief

@param entry
@param label

@return
*/
static void _e2_confdlg_color_entry_cb (GtkEditable *entry, GtkWidget *label)
{
	NEEDCLOSEBGL
	gchar *color_str = gtk_editable_get_chars (entry, 0, -1);
	GdkColor color;
	if (gdk_color_parse (color_str, &color))
	{
		gchar *label_text = g_strdup_printf ("\t<small><i>%s</i></small>  "
		"<span foreground=\"%s\" background=\"%s\">%s</span> "
		"<span foreground=\"%s\">%s</span>  %s",
		_("currently:"), color_str, color_str, _("abCD"), color_str, _("abCD"), color_str);
		gtk_label_set_markup (GTK_LABEL (label), label_text);
		g_free (label_text);
	}
	NEEDOPENBGL
	g_free (color_str);
}
/**
@brief process a click on an item in the categories treeview

@param selection selection for the categories treeview
@param rt pointer to data struct for the dialog

@return TRUE if there is a selected category
*/
static gboolean _e2_confdlg_category_selected_cb (
	GtkTreeSelection *selection, E2_ConfigDialogRuntime *rt)
{
	printd (DEBUG, "callback: category selected");
	GtkTreeIter iter;
	GtkTreeModel *model;

	NEEDCLOSEBGL
	if (gtk_tree_selection_get_selected (selection, &model, &iter))
	{
		g_free (page_last_name);
		gint page;
		gtk_tree_model_get (model, &iter, 0, &page_last_name, 1, &page, -1);

		gtk_notebook_set_current_page (rt->notebook, page);
		gtk_widget_grab_focus (rt->treeview);
		NEEDOPENBGL
		return TRUE;
	}
	NEEDOPENBGL
	return FALSE;
}
/**
@brief setup an entry widget for a set on a dialog page

@param box box widget to hold the entry
@param set pointer to set data

@return
*/
static void _e2_confdlg_set_entry (GtkWidget *box, E2_OptionSet *set)
{
	set->widget = e2_widget_add_entry (box, set->sval, TRUE, FALSE);
	e2_widget_set_safetip (set->widget, set->tip);
	//arrange localised handling of <Esc> key presses
	g_signal_connect (G_OBJECT (GTK_ENTRY (set->widget)), "key-press-event",
		G_CALLBACK (_e2_confdlg_key_press_cb), set);
	g_signal_connect (G_OBJECT (GTK_ENTRY (set->widget)), "focus-out-event",
		G_CALLBACK (_e2_confdlg_focus_out_cb), NULL);
}
/**
@brief add a line to the categories treeview, and a page to the options notebook
Expects page_last_name to be non-NULL
(even if wrong, when changing to basic config)
@param parent_iter pointer to iter in categories treeview, can be NULL for a new category
@param label_text name of the category
@param with_sw TRUE to put the data into a scrolled window
@param rt pointer to data struct for the dialog

@return a vbox widget
*/
static GtkWidget *_e2_confdlg_add_page (GtkTreeIter *parent_iter,
	gchar *label_text,
#ifdef NEWSCROLL
	gboolean with_sw,
#endif
	E2_ConfigDialogRuntime *rt)
{
	//add page to the treeview model
	GtkTreeIter child_iter;
	gint page_num = gtk_notebook_get_n_pages (rt->notebook);
#ifdef USE_GTK2_10
	gtk_tree_store_insert_with_values (rt->store, &child_iter, parent_iter, -1,
#else
	gtk_tree_store_append (rt->store, &child_iter, parent_iter);
	gtk_tree_store_set (rt->store, &child_iter,
#endif
		 0, label_text, 1, page_num, -1);
	//is this one the one nominated to open at startup?
	if (!strcmp (page_last_name, label_text))
	{
		rt->openpath = gtk_tree_model_get_path
			(GTK_TREE_MODEL (rt->store), &child_iter);
		rt->openpage = page_num;
	}

	GtkWidget *vbox;
#ifdef NEWSCROLL
	if (with_sw)
	{
		GtkWidget *sw = e2_widget_get_sw (GTK_POLICY_AUTOMATIC,
			GTK_POLICY_AUTOMATIC, GTK_SHADOW_NONE);
#ifdef USE_GTK3_0
		vbox = gtk_box_new (GTK_ORIENTATION_VERTICAL, E2_PADDING);
#else
		vbox = gtk_vbox_new (FALSE, E2_PADDING);
#endif
		e2_widget_sw_add_with_viewport (sw, vbox);
		gtk_widget_show_all (sw);
		gtk_notebook_append_page (rt->notebook, sw, NULL);
	}
	else
	{
#endif
		//page vbox containing the headline and config widgets
#ifdef USE_GTK3_0
		vbox = gtk_box_new (GTK_ORIENTATION_VERTICAL, E2_PADDING);
#else
		vbox = gtk_vbox_new (FALSE, E2_PADDING);
#endif
		gtk_widget_show (vbox);
		gtk_notebook_append_page (rt->notebook, vbox, NULL);
#ifdef NEWSCROLL
	}
#endif

	g_object_set_data_full (G_OBJECT (vbox), "iter", gtk_tree_iter_copy (&child_iter),
		(void *) gtk_tree_iter_free);
	gtk_container_set_border_width (GTK_CONTAINER (vbox), E2_PADDING);

	//page headline
	GtkWidget *frame = e2_widget_add_frame (vbox, FALSE, 0, NULL, FALSE);
	gchar *label_text2 = g_strconcat ("<b>", label_text, "</b>", NULL);
	GtkWidget *label = e2_widget_add_mid_label (NULL, label_text2, 0.5, FALSE, 0);
	g_free (label_text2);
	gtk_misc_set_padding (GTK_MISC (label), E2_PADDING_XSMALL, E2_PADDING_XSMALL);
	gtk_container_add (GTK_CONTAINER (frame), label);

	return vbox;
}
/**
@brief get notebook page for option group @a group, after creating it if necessary
Recursive if @a group is a child (name includes '.') or is a grandchild (name includes ':')
(and in those cases, it alters the set->group temporarily)
This also sets page_last_name, if it was NULL
@param set pointer to set data struct
@param rt pointer to data struct for the dialog

@return a vbox widget into which option data can be packed
*/
static GtkWidget *_e2_confdlg_get_page (E2_OptionSet *set, E2_ConfigDialogRuntime *rt)
{
	GtkWidget *box;
	if ((box = (GtkWidget *) g_hash_table_lookup (rt->opthash, set->group)) == NULL)
	{
		gchar *frame = strrchr (set->group, ':');	//always ascii :, don't need g_utf8_strrchr()
		if (frame != NULL)
		{
			*frame = '\0';
			GtkWidget *frame_box;
			if ((frame_box = (GtkWidget *) g_hash_table_lookup (rt->opthash, set->group)) == NULL)
				frame_box = _e2_confdlg_get_page (set, rt); //recurse with altered set->group
			gchar *frame_title = e2_utils_str_stretch (frame + 1);
			GtkWidget *frame_widget = gtk_frame_new (frame_title);
			g_free (frame_title);
#ifdef USE_GTK3_0
			GtkWidget *vbox = gtk_box_new (GTK_ORIENTATION_VERTICAL, 0);
#else
			GtkWidget *vbox = gtk_vbox_new (FALSE, 0);
#endif
			gtk_container_add (GTK_CONTAINER (frame_widget), vbox);
			gtk_container_set_border_width (GTK_CONTAINER (vbox), E2_PADDING_SMALL);
			gtk_box_pack_start (GTK_BOX (frame_box), frame_widget, FALSE, FALSE, 0);
			box = vbox;
			*frame = ':';
		}
		else
		{
			gchar *child = strrchr (set->group, '.');	//always ascii '.', don't need g_utf8_strrchr()
			if (child != NULL)
			{
				*child = '\0';
				box = _e2_confdlg_get_page (set, rt); //recurse with altered set->group
				if (page_last_name == NULL)
				//we didn't have a page-name from last time or earlier this time
					page_last_name = g_strdup (set->group);
				GtkTreeIter *iter2;
				iter2 = g_object_get_data (G_OBJECT (box), "iter");
				if (iter2)
					box = _e2_confdlg_add_page (iter2, child + 1,
#ifdef NEWSCROLL
						set->type != E2_OPTION_TYPE_TREE,
#endif
						rt);
				*child = '.';
			}
			else	//this is a group without a descendant
			{
				if (page_last_name == NULL)
				//we didn't have a page-name from last time or earlier this time
					page_last_name = g_strdup (set->group);
				box = _e2_confdlg_add_page (NULL, set->group,
#ifdef NEWSCROLL
					set->type != E2_OPTION_TYPE_TREE,
#endif
					rt);
			}
		}
		g_hash_table_insert (rt->opthash, g_strdup(set->group), box);
	}
	return box;
}
/**
@brief create and show configuration dialog
Expects BGL closed
@param page name of page to show when dialog is started, or "" or NULL to use the last-used page

@return
*/
void e2_config_dialog_create (gchar *page)
{
	printd (DEBUG, "create config dialog (%s)", page);
	//check if there is already a config dialog opened
	if (config_dialog != NULL)
	{
		gtk_window_present (GTK_WINDOW (config_dialog));
		return;
	}

	E2_ConfigDialogRuntime *rt = ALLOCATE (E2_ConfigDialogRuntime);
	CHECKALLOCATEDWARN (rt, return);
	//ensure we open at the start, if there's no matching page now
	//i.e. after conversion to basic config, when on a page now hidden
	rt->openpage = 0;
	rt->openpath = NULL;

	config_dialog = e2_dialog_create (NULL, NULL, _("configuration"),
		(ResponseFunc)_e2_confdlg_response_cb, rt);

	e2_option_connect (config_dialog, FALSE);

	GtkWidget *dialog_vbox =
#ifdef USE_GTK2_14
		gtk_dialog_get_content_area (GTK_DIALOG (config_dialog));
#else
		GTK_DIALOG (config_dialog)->vbox;
#endif
	gtk_container_set_border_width (GTK_CONTAINER (dialog_vbox), E2_PADDING);

	//the main hpane - contains the categories treeview and the frame
#ifdef USE_GTK3_2
	GtkWidget *hpane = gtk_paned_new (GTK_ORIENTATION_HORIZONTAL);
#else
	GtkWidget *hpane = gtk_hpaned_new ();
#endif
	gtk_box_pack_start (GTK_BOX (dialog_vbox), hpane, TRUE, TRUE, 0);

	//scrolled window for the categories treeview
	GtkWidget *sw = e2_widget_get_sw (GTK_POLICY_AUTOMATIC, GTK_POLICY_AUTOMATIC,
		GTK_SHADOW_IN);
	gtk_widget_set_size_request (sw, 150, 50);
	gtk_paned_pack1 (GTK_PANED (hpane), sw, FALSE, TRUE);

	//categories treestore has single node, 2 columns
	//for (displayed) name and (hidden) enumerator/index
	rt->store = gtk_tree_store_new (2, G_TYPE_STRING, G_TYPE_INT);
	//categories tree view
	GtkWidget *catsview;	//the categories treeview
	rt->treeview = catsview =
		gtk_tree_view_new_with_model (GTK_TREE_MODEL (rt->store));
	g_object_unref (rt->store);
	gtk_tree_view_set_rules_hint (GTK_TREE_VIEW (catsview), TRUE);
//#ifdef USE_GTK3_0
//CHECKME	gtk_scrollable_set_vscroll_policy (GTK_SCROLLABLE (catsview), GTK_SCROLL_NATURAL);
//#endif
//#ifdef USE_GTK2_10
//	gtk_tree_view_set_enable_tree_lines (GTK_TREE_VIEW (catstore), TRUE);
//#endif
	GtkTreeSelection *selection =
		gtk_tree_view_get_selection ((GtkTreeView*)catsview);
	gtk_tree_selection_set_mode (selection, GTK_SELECTION_SINGLE);

	GtkCellRenderer *renderer = gtk_cell_renderer_text_new ();
	gtk_tree_view_insert_column_with_attributes ((GtkTreeView*)catsview, -1,
		_("Categories"), renderer, "text", 0, NULL);
	GtkTreeViewColumn *col = gtk_tree_view_get_column ((GtkTreeView*)catsview, 0);
	gtk_tree_view_column_set_sizing (col, GTK_TREE_VIEW_COLUMN_AUTOSIZE);
//	gtk_tree_view_column_set_clickable (col, FALSE);
	gtk_container_add (GTK_CONTAINER (sw), catsview);

	//the notebook
	rt->notebook = GTK_NOTEBOOK (gtk_notebook_new ());
	gtk_notebook_set_show_tabs (rt->notebook, FALSE);
	gtk_notebook_set_show_border (rt->notebook, FALSE);
#ifdef NEWSCROLL
	//in this case, each page has its own sw
	gtk_paned_pack2 (GTK_PANED (hpane), GTK_WIDGET(rt->notebook), TRUE, TRUE);
	gtk_widget_set_size_request (GTK_WIDGET(rt->notebook), 10, 10);
#else
	sw = e2_widget_get_sw_plain (GTK_POLICY_NEVER, GTK_POLICY_NEVER);
	e2_widget_sw_add_with_viewport (sw, GTK_WIDGET (rt->notebook));
	gtk_paned_pack2 (GTK_PANED (hpane), sw, TRUE, TRUE);
#endif
	//note which page we want to open at
	if (page != NULL && *page != '\0')
	{
		if (page_last_name != NULL)
			g_free (page_last_name);
		page_last_name = g_strdup (page);
	}
	/*if page_last_name is still NULL (i.e. when dialog is first run)
	 that string is set when the first page is added to the notebook*/

	//iterate through the options, setting up category tree and page contents
	E2_OptionFlags includemask = (e2_option_bool_get ("advanced-config")) ?
	E2_OPTION_FLAG_BASIC | E2_OPTION_FLAG_ADVANCED :
	E2_OPTION_FLAG_BASIC | E2_OPTION_FLAG_BASICONLY ;

	rt->opthash = g_hash_table_new_full (g_str_hash, g_str_equal, g_free,
		(GDestroyNotify)_e2_hashfree);
	//log these in case they're changed via the dialog
	rt->refresh_files = e2_option_bool_get ("auto-refresh");
	rt->refresh_cfg = e2_option_bool_get ("auto-refresh-config");

	guint i;
	gpointer *walker;
	E2_OptionSet *set;
	for (i = 0, walker = options_array->pdata; i < options_array->len; i++, walker++)
	{
		GtkSizeGroup *size_group;
		set = *walker;
		if (!(set->flags & includemask))
		{
			set->widget = NULL;	//make sure that this is ignored when updating after 'ok' or 'apply'
			continue;
		}
		if (set->type != E2_OPTION_TYPE_TREE)
		{
			//make sure there's a size group for the page
			gchar *size_group_key = g_strconcat (set->group, "_sizegroup", NULL);	//no translation
			size_group = (GtkSizeGroup *) g_hash_table_lookup (rt->opthash, size_group_key);
			if (GTK_IS_SIZE_GROUP (size_group))
				g_free (size_group_key);
			else
			{
				size_group = gtk_size_group_new (GTK_SIZE_GROUP_HORIZONTAL);
				g_hash_table_insert (rt->opthash, size_group_key, size_group);
			}
		}
		else
			size_group = NULL;	//warning prevention only

		//get page for this set
		//and if necessary, create categories view item(s) and
		//notebook page(s) for parent group and any child(ren)
		GtkWidget *box = _e2_confdlg_get_page (set, rt);
		//FIXME create only the the startup page now, rest after display
		GtkWidget *hbox, *label;	//, *button;
		gchar *label_text;
		switch (set->type)
		{
			case E2_OPTION_TYPE_TREE:
				e2_option_tree_add_widget (config_dialog, FALSE, box, set);
				break;
			case E2_OPTION_TYPE_BOOL:
				//button =
				e2_widget_add_tied_check_button (box, set, config_dialog);
				//CHECKME size_group needed ?
				//gtk_size_group_add_widget (size_group, button);
				break;
			case E2_OPTION_TYPE_STR:
#ifdef USE_GTK3_0
				hbox = gtk_box_new (GTK_ORIENTATION_HORIZONTAL, E2_PADDING);
#else
				hbox = gtk_hbox_new (FALSE, E2_PADDING);
#endif
				gtk_box_pack_start (GTK_BOX (box), hbox, TRUE, TRUE, E2_PADDING);
				label = e2_widget_add_mid_label (hbox, set->desc, 0.0, FALSE, 0);
				gtk_size_group_add_widget (size_group, label);
				_e2_confdlg_set_entry (hbox, set);
#ifdef E2_ASSISTED
				e2_widget_set_label_relations (label, set->widget);
#endif
				break;
			case E2_OPTION_TYPE_INT:
				e2_option_int_add_widget (config_dialog, box, size_group, set);
				break;
			case E2_OPTION_TYPE_SEL:
				e2_option_sel_add_widget (config_dialog, box, size_group, set);
				break;
			case E2_OPTION_TYPE_FONT:
				//hbox for option label, font entry and font dialog button
#ifdef USE_GTK3_0
				hbox = gtk_box_new (GTK_ORIENTATION_HORIZONTAL, E2_PADDING_SMALL);
#else
				hbox = gtk_hbox_new (FALSE, E2_PADDING_SMALL);
#endif
				gtk_box_pack_start (GTK_BOX (box), hbox, TRUE, TRUE, E2_PADDING_SMALL);
				//option label
				label_text = g_strconcat (set->desc, ": ", NULL);
				label = e2_widget_add_mid_label (hbox, label_text, 0.0, FALSE, E2_PADDING_SMALL);
				g_free (label_text);
				gtk_size_group_add_widget (size_group, label);
				//font entry for font name
				_e2_confdlg_set_entry (hbox, set);
#ifdef E2_ASSISTED
				e2_widget_set_label_relations (label, set->widget);
#endif
				gtk_entry_set_text (GTK_ENTRY (set->widget), set->sval);
				//button for font select dialog
				e2_button_add (hbox, FALSE, 0, _("change"), STOCK_NAME_SELECT_FONT,
					_("Click to open a font select dialog"), _e2_confdlg_font_select_cb, set);
				//label for font example
				label = e2_widget_add_mid_label (box, "", 0.0, FALSE, 0);
				gtk_misc_set_alignment (GTK_MISC (label), 0.0, 0.0);
				//set label contents
				NEEDOPENBGL
				_e2_confdlg_font_entry_cb (GTK_EDITABLE (set->widget), label);
				NEEDCLOSEBGL
				//react on changes to the font name
				g_signal_connect (GTK_EDITABLE (set->widget), "changed",
					G_CALLBACK (_e2_confdlg_font_entry_cb), label);
				break;
			case E2_OPTION_TYPE_COLOR:
				//hbox for option label, color entry and color dialog button
#ifdef USE_GTK3_0
				hbox = gtk_box_new (GTK_ORIENTATION_HORIZONTAL, E2_PADDING_SMALL);
#else
				hbox = gtk_hbox_new (FALSE, E2_PADDING_SMALL);
#endif
				gtk_box_pack_start (GTK_BOX (box), hbox, TRUE, TRUE, E2_PADDING_SMALL);
				//option label
				label_text = g_strconcat (set->desc, ": ", NULL);
				label = e2_widget_add_mid_label (hbox, label_text, 0.0, FALSE, E2_PADDING_SMALL);
				g_free (label_text);
				gtk_size_group_add_widget (size_group, label);
				//color entry for color names/color hex values
				_e2_confdlg_set_entry (hbox, set);
#ifdef E2_ASSISTED
				e2_widget_set_label_relations (label, set->widget);
#endif
				gtk_entry_set_text (GTK_ENTRY (set->widget), set->sval);
				//button for color select dialog
				e2_button_add (hbox, FALSE, 0, _("change"), STOCK_NAME_SELECT_COLOR,
					_("Click to open a color selection dialog"),
					_e2_confdlg_color_select_cb, set);
				//label for color example
				label = e2_widget_add_mid_label (box, "", 0.0, FALSE, 0);
				gtk_misc_set_alignment (GTK_MISC (label), 0.0, 0.0);
				//set label contents
				NEEDOPENBGL
				_e2_confdlg_color_entry_cb (GTK_EDITABLE (set->widget), label);
				NEEDCLOSEBGL
				//react on changes to the color name/color hex value
				g_signal_connect (GTK_EDITABLE (set->widget), "changed",
					G_CALLBACK (_e2_confdlg_color_entry_cb), label);
				break;
			default:
				break;
		}
	}
	/*a set's dependency (if any) may be 'forward' in the array, in which
	case the relevant widget not known when the set was processed in the previous
	walk, so need to setup dependencies after all options are setup i.e. iterate again ...
	FIXME in pass 1, log the items with un-resolved dependencies, then
	just do those here */
	for (i = 0, walker = options_array->pdata; i < options_array->len; i++, walker++)
	{
		set = *walker;
		if (!(set->flags & includemask))
			continue;
		if (set->depends != NULL)
		{
			if (set->depends[0] == '!')
			{	//negated dependency
				E2_OptionSet *dep = e2_option_get (set->depends + 1);
				if ((dep != NULL) && (dep->type == E2_OPTION_TYPE_BOOL))
				{
					if (dep->widget != NULL)	//dep is in this dialog
						g_signal_connect (G_OBJECT (dep->widget), "toggled",
							G_CALLBACK (_e2_confdlg_negdepends_cb), set->widget);
					gtk_widget_set_sensitive (set->widget,
						! e2_option_bool_get_direct (dep));	//if dep is not in the dialog, it will remain in its current state
				}
			}
			else	//not negated
			{
				E2_OptionSet *dep = e2_option_get (set->depends);
				if ((dep != NULL) && (dep->type == E2_OPTION_TYPE_BOOL))
				{
					if (dep->widget != NULL)	//dep is in this dialog
						g_signal_connect (G_OBJECT (dep->widget), "toggled",
							G_CALLBACK (_e2_confdlg_posdepends_cb_), set->widget);
					gtk_widget_set_sensitive (set->widget,
						e2_option_bool_get_direct (dep));
				}
				else	//always insensitive if dep not boolean
					if (dep->type != E2_OPTION_TYPE_BOOL)
				{
					gtk_widget_set_sensitive (set->widget, FALSE);
				}
				//ignore instruction if dep set  doesn't exist
			}
		}
	}
	//clean up
	g_hash_table_destroy (rt->opthash);

	//expand the category tree view
	gtk_tree_view_expand_all (GTK_TREE_VIEW (catsview));

	//show and select the startup row
	GtkTreePath *path = rt->openpath;
	//when converting from advanced to basic config, we can't
	//be sure the current page will be displayed
	if (path != NULL)
	{
		gtk_tree_view_set_cursor (GTK_TREE_VIEW (catsview), path, NULL, TRUE);
		gtk_tree_view_scroll_to_cell (GTK_TREE_VIEW (catsview), path, NULL, FALSE, 0, 0);
		gtk_tree_path_free (path);
	}
	gtk_window_resize (GTK_WINDOW(config_dialog), app.cfg_alloc.width,
		app.cfg_alloc.height);	//revert to last size if not too small

	//receptacle for cut/copied row(s) data generated from context menu
	//the keys are constant strings, not to be replaced or freed
	tree_view_buffer_hash = g_hash_table_new_full (g_str_hash, g_str_equal,
		NULL, (GDestroyNotify) e2_option_tree_menu_hash_clean);

//	g_signal_connect (G_OBJECT (config_dialog), "delete-event", handled in response process
//		G_CALLBACK (_e2_confdlg_cancel_cb), NULL);

#ifdef E2_TRANSIENTBINDINGS
	//FIXME setup dialog-specific stuff
#endif

//	g_signal_connect_after (G_OBJECT (config_dialog), "key-press-event",
//		G_CALLBACK (e2_confdlg_key_press_cb), NULL);
//	g_signal_connect (G_OBJECT (catsview), "key-press-event",
//		G_CALLBACK (e2_confdlg_key_press_cb), NULL);
	g_signal_connect (G_OBJECT (catsview), "button-press-event",
		G_CALLBACK (_e2_confdlg_button_press_cb), NULL);
	g_signal_connect (G_OBJECT (catsview), "popup-menu",
		G_CALLBACK (_e2p_confdlg_popup_menu_cb), NULL);
	g_signal_connect (G_OBJECT (selection), "changed",
		G_CALLBACK (_e2_confdlg_category_selected_cb), rt);
	//make the notebook open at the correct page
	//(can't get it to work, if set sooner than in the 'show' cb ...)
	g_signal_connect (G_OBJECT (config_dialog), "show",
		G_CALLBACK (_e2_confdlg_show_cb2), rt);
//#ifdef NEWSCROLL
//	g_signal_connect (G_OBJECT (config_dialog), "show",
//		G_CALLBACK (_e2_confdlg_show_cb), rt->notebook);
//#else
#ifndef NEWSCROLL
	g_signal_connect (G_OBJECT (config_dialog), "show",
		G_CALLBACK (_e2_confdlg_show_cb), sw);
#endif
	e2_dialog_add_custom_button_full (config_dialog, FALSE,
		E2_RESPONSE_USER1, _("De_fault"), STOCK_NAME_CLEAR,
		_("Revert all options to their default settings"),
		NULL, NULL);
	if (e2_option_bool_get ("advanced-config"))
	{
		e2_dialog_add_custom_button_full (config_dialog, FALSE,
			E2_RESPONSE_USER2, _("_Basic"), STOCK_NAME_REDO,
			_("Display only the basic configuration options"),
			NULL, NULL);
	}
	else
	{
		e2_dialog_add_custom_button_full (config_dialog, FALSE,
			E2_RESPONSE_USER2, _("Ad_vanced"), STOCK_NAME_REDO,
			_("Display all configuration options"),
			NULL, NULL);
	}

	E2_BUTTON_APPLY.showflags &= ~E2_BTN_DEFAULT;	//CHECKME modify local copy ?
	e2_dialog_show (config_dialog, app.main_window, 0,
		&E2_BUTTON_MORE, &E2_BUTTON_DISCARD, &E2_BUTTON_APPLY, NULL);

	//prepare all the buttons for mnemonic-blocking during any keybinding config
	e2_option_tree_connect_mnemonics (
#ifdef USE_GTK2_14
		gtk_dialog_get_action_area (GTK_DIALOG(config_dialog))
#else
		GTK_DIALOG(config_dialog)->action_area
#endif
	);

//	app.keytrans = FALSE;	//cancel any keys-translation-flag from earlier in this session
}
