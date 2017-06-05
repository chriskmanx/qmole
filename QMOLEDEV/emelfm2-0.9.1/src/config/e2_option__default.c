/* $Id: e2_option__default.c 3058 2014-02-14 23:38:00Z tpgww $

Copyright (C) 2003-2013 tooar <tooar@emelfm2.net>

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

/**
@file src/config/e2_option__default.c
@brief default place to register all config options

Registration of all configuration options, at session start
The order of registration determines the order of the config dialog

*/

#include "emelfm2.h"
#include "e2_option.h"
#include "e2_dialog.h"
#include "e2_mkdir_dialog.h"
#include "e2_toolbar.h"
#include "e2_context_menu.h"
#include "e2_plugins.h"
#include "e2_filetype.h"
#ifdef E2_MOUSECUSTOM
#include "e2_mousebinding.h"
#endif

const gchar *custom_date_label = N_("strftime() format provided below");
gboolean custom_date_label_translated = FALSE;

/**
@brief setup and register all options, plus a few other initialisation tasks

Option values, tips etc are established.
Each option has flags which indicate the way it should be presented
in a config dialog, what things (esp. part(s) of the main window)
need to be rebuilt if the option value is changed in a config
dialog, and what cleanups are needed if the config data are dumped.
Tree option data is not stored yet, but only prepared for storage.
After the config file (if any) is parsed, and relevant tree option data
in that file are processed, the 'prepared' tree data are either stored
or abandoned, as need be. See e2_option_trees_confirm().
The order of the entries in this function determines the order of
options shown in a config dialog.
As a convenience, some of the included functions involve a few
initialisation tasks which are not strictly option-related.

@return
*/
void e2_option_default_register (void)
{
	printd (DEBUG, "registering all default (non-tree) options");
//NOTE: each group with a non-constant group_name must include one and only one
//option with E2_OPTION_FLAG_FREEGROUP

	  /*******************/
	 /***** general *****/
	/*******************/

	gchar *group_name = g_strconcat(_C(17),":",_C(26),NULL); //_("general:miscellaneous"
	e2_option_bool_register ("advanced-config", group_name,
		_("show all options in config dialogs"), NULL,
		NULL, FALSE,
		E2_OPTION_FLAG_HIDDEN | E2_OPTION_FLAG_FREEGROUP);	//no rebuild
	e2_option_bool_register ("auto-refresh-config", group_name,
		_("reload config on external change"),
		_("This enables automatic reloading of the configuration data for this program, if that data is changed by another program instance"),
		NULL, FALSE,
		E2_OPTION_FLAG_ADVANCED);	//no rebuild
/*	e2_option_int_register ("sort-before-show-count", group_name, _("threshold number of directory items processed before display"),
		_("Directories with more than this many entries will be displayed in an interim state, until loading and sorting are completed"),
		NULL, 100, 0, 1<<(sizeof (gint)*4,
		E2_OPTION_FLAG_ADVANCED);
*/
	gchar *local = DOC_DIR G_DIR_SEPARATOR_S MAIN_HELP;
	gchar *utf = F_FILENAME_FROM_LOCALE (local);
	e2_option_str_register ("usage-help-doc", group_name,
		_("document containing usage advice"),
		_("This document is opened from the help dialog usage page"),
		NULL, utf,
		E2_OPTION_FLAG_ADVANCED);	//no rebuild
	F_FREE (utf, local);
	local = DOC_DIR G_DIR_SEPARATOR_S CFG_HELP;
	utf = F_FILENAME_FROM_LOCALE (local);
	e2_option_str_register ("config-help-doc", group_name,
		_("document containing configuration advice"),
		_("This document is opened from the help dialog configuration page"),
		NULL, utf,
		E2_OPTION_FLAG_ADVANCED);	//no rebuild
	F_FREE (utf, local);
	e2_option_bool_register ("session-end-warning", group_name,
		_("warn about running processes when shutting down"),
		_("This enables a reminder about incomplete commands and actions"),
		NULL,TRUE,
		E2_OPTION_FLAG_ADVANCED);	//no rebuild
	e2_option_bool_register ("command-persist", group_name, _("running commands survive shutdown"),
		_("If activated, commands that are still running will not be terminated at end of emelFM2 session"),
		NULL, TRUE,
		E2_OPTION_FLAG_ADVANCED);

	e2_bookmark_options_register ();
	e2_command_options_register ();
	e2_filetype_options_register ();
	e2_plugins_options_register ();

	  /*********************/
	 /***** interface *****/
	/*********************/

	group_name = g_strconcat(_C(20) ,":",_C(26),NULL);  //_("interface:miscellaneous"
#ifdef E2_COMPOSIT
	e2_option_int_register ("window-opacity", group_name, _("opacity"),
		_("Window translucence, 30 (faint) to 100 (opaque)"),
		NULL, 100, 30, 100,
		E2_OPTION_FLAG_BASIC | E2_OPTION_FLAG_BUILDALL);
#endif
#ifndef E2_MOUSECUSTOM
	e2_option_bool_register ("button2-updir", group_name, _("'go up' on middle-button click"),
		_("This is a faster alternative to double clicking '..' or clicking the 'go up' button"), NULL, TRUE,
		E2_OPTION_FLAG_BASIC | E2_OPTION_FLAG_BUILDPANES);
#endif
	e2_option_bool_register ("windows-right-click", group_name, _("match windows (TM) right-click behaviour"),
		_("If activated, clicking the right mouse button will also select the row where the mouse cursor is"),
		NULL, FALSE,
		E2_OPTION_FLAG_ADVANCED | E2_OPTION_FLAG_FREEGROUP); //no rebuild
	e2_option_bool_register ("windows-right-click-extra", group_name, _("advanced windows (TM) right-click behaviour"),
		_("If activated, clicking on a free area will not clear the current selection"),
		"windows-right-click", FALSE,
		E2_OPTION_FLAG_ADVANCED); //no rebuild

	e2_keybinding_options_register ();	//here to make nicer order in tab

#ifndef USE_GTK3_10
	e2_option_int_register ("submenu-up-delay", group_name, _("menu popup delay (ms)"),
		_("The delay (in milliseconds) after the mouse pointer arrives at a menu item, before any submenu will pop up"),
		NULL, 50, 0, 10000,
		E2_OPTION_FLAG_ADVANCED); //no rebuild
	e2_option_int_register ("submenu-down-delay", group_name, _("menu popdown delay (ms)"),
		_("The delay (in milliseconds) after the mouse pointer leaves a menu item, before popping down an activated submenu"),
		NULL, 400, 0, 10000,
		E2_OPTION_FLAG_ADVANCED); //no rebuild
#endif
	const gchar *opt_window_title[] =
		{_("fixed"), _("pane1 directory"), _("pane2 directory"), _("active pane directory"), NULL};
	e2_option_sel_register ("title-type", group_name, _("main window title"),
		_("Show the application name only or append a pane directory"),
		NULL, 0, opt_window_title,
		E2_OPTION_FLAG_ADVANCED | E2_OPTION_FLAG_BUILDALL);
    //option-values for this must conform to GtkCornerType enum, but for scrolled
	//windows, the enum determines where the child is positioned, the opposite of
	//where the bar(s) are positioned
	const gchar *opt_scrollbar_position[] =
		{_("bottom-right"), _("top-right"), _("bottom-left"), _("top-left"), NULL};
	e2_option_sel_register ("scrollbar-position", group_name, _("scrollbar position"),
		_("The default (bottom-right) should be ok for most users"), NULL, 0, opt_scrollbar_position,   //make this more informative
		E2_OPTION_FLAG_ADVANCED | E2_OPTION_FLAG_BUILDALL);

	group_name = g_strconcat(_C(20) ,":",_C(19),NULL);  //_("interface:icons"
	e2_option_bool_register ("use-icon-dir", group_name, _("use icons directory"),
		_("If activated, icon files in the directory shown below will be used for toolbars etc"),
		NULL, FALSE,
		E2_OPTION_FLAG_ADVANCED | E2_OPTION_FLAG_BUILDICONS |
		E2_OPTION_FLAG_BUILDBARS | E2_OPTION_FLAG_FREEGROUP);
	e2_option_str_register ("icon-dir", group_name, _("icons directory"),
		_("The directory from which icon files will be retrieved"),
			"use-icon-dir", "/YOURHOME/."BINNAME, E2_OPTION_FLAG_BUILDICONS |
			E2_OPTION_FLAG_BUILDBARS | E2_OPTION_FLAG_ADVANCED);
	const gchar *opt_icons_show[] =
		{_("theme"),_("yes"), _("no"),NULL};
	e2_option_sel_register ("dialog-button-icons", group_name, _("show icons in dialog buttons"),
		_("Show an icon as well as a label"),
		NULL, 1, opt_icons_show,
		E2_OPTION_FLAG_ADVANCED); //no rebuild
	e2_option_sel_register ("menu-show-icons", group_name, _("show icons in menus"),
		_("Show an icon as well as a label"),
		NULL, 1, opt_icons_show,
		E2_OPTION_FLAG_ADVANCED);
/*	const gchar *opt_menu_isize[] =
		{_("menu"), _("toolbar small"), _("toolbar large"), _("button"), _("dnd"), _("dialog"), NULL};
	e2_option_sel_register ("menu-isize", group_name, _("menu icon size"),
		_("This sets the icon size for ALL menus"),
		NULL/ *"menu-show-icons"* /, 0, opt_menu_isize,
		E2_OPTION_FLAG_ADVANCED); //no rebuild
*/
#ifdef E2_MOUSECUSTOM
	e2_mousebinding_options_register ();
# ifdef E2_PTRGESTURES
	e2_mousegesture_options_register ();
# endif
#endif
	e2_menu_custom_option_register ();

	  /*****************/
	 /***** panes *****/
	/*****************/

	group_name = g_strconcat(_C(33) ,":",_C(26),NULL);  //_("panes:miscellaneous"
	e2_option_bool_register ("auto-refresh", group_name, _("auto refresh"),
		_("Automatically check for and display file-list changes"), NULL, TRUE,
		E2_OPTION_FLAG_ADVANCED | E2_OPTION_FLAG_FREEGROUP ); //no rebuild
	e2_option_bool_register ("dir-line-focus-after-activate",
		group_name, _("focus the pane after completing a directory entry"),
		_("After a directory-line is activated, make the pane containing it the active one"),
		NULL, TRUE,
		E2_OPTION_FLAG_ADVANCED);	//no rebuild
	e2_option_bool_register ("select-first-item", group_name,
		_("select first item in newly-opened directories"),
		_("Select the first item in a file-list when a directory is first displayed in a session"),
		NULL, FALSE,
		E2_OPTION_FLAG_ADVANCED);	//no rebuild
	e2_option_bool_register ("cache-history", group_name,
		_("persistent goto history"),
		_("Save \"goto buttons\" opened-directories data for use in the next session"),
		NULL, TRUE,
		E2_OPTION_FLAG_ADVANCED);	//no rebuild, and if changed in-session,
									//will not affect cacheing of that session

	group_name = g_strconcat(_C(33) ,":",_C(39),NULL); //_("panes:style"
	e2_option_bool_register ("panes-horizontal", group_name, _("use horizontal panes"),
		_("Horizontal (vertical-stacked) panes present more columns at once"), NULL,
#ifdef E2_PANES_HORIZONTAL
		TRUE,
#else
		FALSE,
#endif
		E2_OPTION_FLAG_BASIC  | E2_OPTION_FLAG_BUILDALL | E2_OPTION_FLAG_FREEGROUP);
	const gchar *opt_menu_activeflag[] =
		{_("colored headers"), _("bold name header"),_("sensitivity"),NULL};
	e2_option_sel_register ("active-pane-signal", group_name, _("type of indicator for active pane"),
		_("This determines how the active pane is indicated on-screen. Some GTK themes do not allow column-header re-coloring"),
#ifdef E2_SMALLSCREEN
		"!active-pane-tools",
#else
		NULL,
#endif
		0, opt_menu_activeflag,
		E2_OPTION_FLAG_ADVANCED | E2_OPTION_FLAG_BUILDPANES);

#ifdef E2_SMALLSCREEN
		e2_option_bool_register ("active-pane-tools", group_name, _("single toolbar"),
		_("Show toolbar in active pane only"),
		NULL, FALSE,
		E2_OPTION_FLAG_BASIC | E2_OPTION_FLAG_BUILDPANES);
#endif

	e2_option_bool_register ("panes-hinted", group_name, _("banded background"),
		_("If activated, lines in file lists will alternate background color"),
		NULL, FALSE,
		E2_OPTION_FLAG_BASIC | E2_OPTION_FLAG_BUILDPANES);
	e2_option_bool_register ("custom-list-font",
		group_name, _("use custom font"),
		_("If activated, the font specified below will be used, instead of the theme default"),
		NULL, FALSE,
		E2_OPTION_FLAG_BASIC | E2_OPTION_FLAG_BUILDLISTS);
	e2_option_font_register ("list-font", group_name, _("custom font"),
		_("This is the font used for flle pane text"), "custom-list-font", "Sans 10",//_I( font name
		E2_OPTION_FLAG_BASIC | E2_OPTION_FLAG_BUILDLISTS);

	if (!custom_date_label_translated)
	{
		custom_date_label = _(custom_date_label);
		custom_date_label_translated = TRUE;
	}
	//HACK - custom/last format-string is set to its real value in e2_option_date_style()
	const gchar *opt_filelist_date_string[] = {_("Default: May 20 09:11"),
		_("Standard: 20/05/04 09:11"), _("American: 05/20/04 09:11"),
		"ISO8601: 2004-05-20T09:11", _("LC_TIME locale specified"), custom_date_label, NULL};	//no need to translate ISO string
	e2_option_sel_register ("date-string", group_name, _("date format"),
		_("This determines the format of all dates displayed in the panes"),
			NULL, 0, opt_filelist_date_string,
			E2_OPTION_FLAG_BASIC | E2_OPTION_FLAG_BUILDLISTS);

	e2_option_str_register ("custom-date-format", group_name, _("custom date/time format string:"),
		_("Format string interpreted by strftime() to present file date/time"),
			NULL, "%c",
			E2_OPTION_FLAG_ADVANCED | E2_OPTION_FLAG_BUILDLISTS);

	const gchar *opt_filelist_size_string[] = {_("condensed"), _("exact"), NULL};
	e2_option_sel_register ("size-string", group_name, _("size format"),
		_("Displayed item-sizes can be 'condensed' to show kB, MB where appropriate"),
			NULL, 1, opt_filelist_size_string,
			E2_OPTION_FLAG_BASIC | E2_OPTION_FLAG_BUILDLISTS);

	e2_option_bool_register ("show-updir-entry", group_name, _("show parent directory entry '..' in file lists"),
		_("This slows status-line updates"), NULL, FALSE,
		E2_OPTION_FLAG_BASIC | E2_OPTION_FLAG_BUILDLISTS);
	e2_option_bool_register ("sort-dirs-first", group_name, _("directories first"),
		_("This places directories ahead of other items in file lists"),
		NULL, TRUE,
		E2_OPTION_FLAG_BASIC | E2_OPTION_FLAG_BUILDLISTS);
	e2_option_bool_register ("namesort-case-sensitive", group_name, _("filename sort is case sensitive"),
		_("This places all the capitalised file/directory names ahead of the others, if your LANG_C suports that"),
		NULL, FALSE,
		E2_OPTION_FLAG_BASIC | E2_OPTION_FLAG_BUILDLISTS);

	  /******************/
	 /***** colors *****/
	/******************/

	group_name = g_strconcat(_C(33),".",_C(2),":",_C(21),NULL); //_("panes.colors:filetypes"
	e2_option_color_register ("color-ft-exec", group_name, _("executable"),
		_("Executable file names are listed in this color"), NULL, "forest green",
		E2_OPTION_FLAG_ADVANCED | E2_OPTION_FLAG_BUILDLISTS | E2_OPTION_FLAG_FREEGROUP);
	e2_option_color_register ("color-ft-dir", group_name, _("directory"),
		_("Directory names are listed in this color"), NULL, "blue",
		E2_OPTION_FLAG_ADVANCED | E2_OPTION_FLAG_BUILDLISTS);
	e2_option_color_register ("color-ft-link", group_name, _("symbolic link"),
		_("Symbolic link names are listed in this color"), NULL, "sky blue",
		E2_OPTION_FLAG_ADVANCED | E2_OPTION_FLAG_BUILDLISTS);
	e2_option_color_register ("color-ft-dev", group_name, _("device"),
		_("Device names are listed in this color"), NULL, "orange",
		E2_OPTION_FLAG_ADVANCED | E2_OPTION_FLAG_BUILDLISTS);
	e2_option_color_register ("color-ft-socket", group_name, _("socket"),
		_("Sockets are listed in this color"), NULL, "purple",
		E2_OPTION_FLAG_ADVANCED | E2_OPTION_FLAG_BUILDLISTS);

	group_name = g_strconcat(_C(33),".",_C(2),":",_C(26),NULL); //_("panes.colors:miscellaneous"
#ifdef E2_ASSISTED
	e2_option_bool_register ("color-background-set", group_name, _("custom background color"),
		_("If enabled, the color specified below will be used for background"),
		NULL, FALSE,
		E2_OPTION_FLAG_BASIC | E2_OPTION_FLAG_BUILDLISTS);
	e2_option_color_register ("color-background", group_name, _("background color"),
		_("Background color used instead of theme color"),
		"color-background-set", "#2E3436",
		E2_OPTION_FLAG_BASIC | E2_OPTION_FLAG_BUILDLISTS);
#endif
	e2_option_color_register ("color-active-pane", group_name, _("active pane header color"),
		_("This color is used for the background of column headers in the active pane"), NULL, "dark grey",
		E2_OPTION_FLAG_ADVANCED | E2_OPTION_FLAG_BUILDPANES | E2_OPTION_FLAG_FREEGROUP);
	e2_option_color_register ("color-highlight", group_name, _("highlight color"),
		_("This color is used for the background of highlighed item-names and other text"), NULL, "wheat",
		E2_OPTION_FLAG_ADVANCED); //no rebuild
/*just use theme color now
	e2_option_color_register ("color-inactive-selected", group_name, _("inactive selected item color"),
		_("This is the background color used for selected files and directories in the inactive pane"), "light grey",
		E2_OPTION_FLAG_ADVANCED | E2_OPTION_FLAG_BUILDLISTS);
tags not supported now
	e2_option_color_register ("color-tag", group_name, _("tagged item color"),
		_("This is the background color used for tagged files and directories"), NULL, "yellow",
		E2_OPTION_FLAG_ADVANCED);
*/

	e2_pane_options_register (1);
	e2_pane_options_register (2);

	  /********************/
	 /***** toolbars *****/
	/********************/

	group_name = g_strconcat(_C(20) ,":",_C(39),NULL);
	const gchar *opt_toolbar_style[] =
		{_("theme"),_("icon only"), _("label only"), _("icon above label"), _("icon beside label"), NULL};
	gchar *tip = g_strdup_printf (_("'%s' uses the Gtk default, '%s' leaves most space for other things"),
		opt_toolbar_style[0], opt_toolbar_style[1]);
	e2_option_sel_register ("allbars-style", group_name, _("toolbars button style"), tip, NULL,
		1, opt_toolbar_style,
		E2_OPTION_FLAG_BASICONLY | E2_OPTION_FLAG_BUILDBARS
		| E2_OPTION_FLAG_BUILDSAMEBARS | E2_OPTION_FLAG_FREEGROUP
		| E2_OPTION_FLAG_FREETIP);
	const gchar *opt_toolbar_isize[] =
		{_("theme"),_("menu"), _("toolbar small"), _("toolbar large"), _("button"), _("dnd"), _("dialog"),NULL};
	tip = g_strdup_printf (_("'%s' uses the Gtk default, '%s' is smallest, '%s' is largest"),
		opt_toolbar_isize[0], opt_toolbar_isize[1], opt_toolbar_isize[6]);
	e2_option_sel_register ("allbars-isize", group_name, _("toolbars icon size"), tip,
		NULL, 2, opt_toolbar_isize,
		E2_OPTION_FLAG_BASICONLY | E2_OPTION_FLAG_BUILDBARS
		| E2_OPTION_FLAG_BUILDSAMEBARS | E2_OPTION_FLAG_FREETIP);

	e2_toolbar_toolbaroptions_register ();

	  /**********************/
	 /***** commandbar *****/
	/**********************/
	e2_toolbar_commandbaroptions_register ();

// this is a consolidated list of all the 'externals' here
//above	e2_pane_options_register (_C(33), _C(30), 1);  //_(panes: panebar 1
//above	e2_pane_options_register (_C(33), _C(32), 2); //_(panes: panebar 2
//above	e2_toolbar_toolbaroptions_register ();
//above	e2_toolbar_commandbaroptions_register ()
//above	e2_context_menu_options_register ();
//above	e2_plugins_options_register ();
	e2_alias_init(); //this also does some other init stuff
	e2_command_line_options_register ();
//above	e2_bookmark_options_register ();
//above	e2_command_options_register ();

	  /************************/
	 /***** context menu *****/
	/************************/
	e2_context_menu_options_register ();

	e2_output_options_register ();
	e2_dialog_options_register ();
	e2_mkdir_dialog_options_register ();
//	e2_search_dialog_options_register ();
	e2_view_dialog_options_register ();
	e2_edit_dialog_options_register ();
	e2_file_info_dialog_options_register ();
// keybindings have a separate data structure too
//above	e2_keybinding_init(); //this also does some other stuff
//above	e2_filetype_options_register();
}
