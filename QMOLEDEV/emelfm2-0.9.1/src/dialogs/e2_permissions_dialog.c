/* $Id: e2_permissions_dialog.c 2815 2013-10-13 07:00:55Z tpgww $

Copyright (C) 2003-2013 tooar <tooar@emelfm2.net>
Portions copyright (C) 1999 Michael Clark.

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
#include <pwd.h>
#include <grp.h>
#include "e2_dialog.h"
#include "e2_permissions_dialog.h"
#include "e2_ownership_dialog.h"
#include "e2_filelist.h"

//order of these determines order of chmod_buttons[] in dialog data
enum { USR_READ, USR_WRITE, USR_EXEC, GRP_READ, GRP_WRITE, GRP_EXEC,
	   OTH_READ, OTH_WRITE, OTH_EXEC, SETUID, SETGID, STICKY, PERM_COUNT };

typedef struct _E2_PermsDldRuntime
{
	GtkWidget *chmod_buttons[PERM_COUNT];
	GtkWidget *set_perms_button;
	GtkWidget *add_perms_button;
	GtkWidget *remove_perms_button;
	GtkWidget *recurse_button;
	GtkWidget *recurse_dirs_button;
	GtkWidget *recurse_other_button;
	gboolean permission;
	mode_t mode;	//current-permissions of item, logged during dialog creation
} E2_PermsDldRuntime;

//these flags must be in the same order as the enum above
static mode_t mask[PERM_COUNT] =
	{ S_IRUSR, S_IWUSR, S_IXUSR, S_IRGRP, S_IWGRP, S_IXGRP,
      S_IROTH, S_IWOTH, S_IXOTH, S_ISUID, S_ISGID, S_ISVTX };

/**
@brief construct mode-string corresponding to widget settings
The returned string will begin with mode-indicator char 's', 'a' or 'r',
followed by digits representing octal mode (e.g. "4755")
@param rt pointer to dialog data struct

@return newly-allocated string, or NULL in case of error
*/
static void _e2_permissions_dialog_get_mode_string (E2_ChmodType *op_ret,
	mode_t *mode_ret, E2_PermsDldRuntime *rt)
{
	if (
#ifdef USE_GTK2_14
		gtk_toggle_button_get_active (GTK_TOGGLE_BUTTON (rt->set_perms_button))
#else
		GTK_TOGGLE_BUTTON (rt->set_perms_button)->active
#endif
	)
		*op_ret = E2_CHMOD_SET;
	else if (
#ifdef USE_GTK2_14
			gtk_toggle_button_get_active (GTK_TOGGLE_BUTTON (rt->add_perms_button))
#else
			GTK_TOGGLE_BUTTON (rt->add_perms_button)->active
#endif
	)
		*op_ret = E2_CHMOD_ADD;
	else if (
#ifdef USE_GTK2_14
			gtk_toggle_button_get_active (GTK_TOGGLE_BUTTON (rt->remove_perms_button))
#else
			GTK_TOGGLE_BUTTON (rt->remove_perms_button)->active
#endif
	)
		*op_ret = E2_CHMOD_REM;
	else //should never happen
	{
		printd (WARN, "Strange error in permissions dialog task");
		return;	//default parameters set upsteam
	}

	mode_t mode = 0;
	gint i;
	for (i = 0; i < PERM_COUNT; i++)
	{
		if (
#ifdef USE_GTK2_14
			gtk_toggle_button_get_active (GTK_TOGGLE_BUTTON (rt->chmod_buttons[i]))
#else
			GTK_TOGGLE_BUTTON (rt->chmod_buttons[i])->active
#endif
			)
			mode |= mask[i];
	}
	*mode_ret = mode; //redundant & ALLPERMS);
}
/**
@brief add a check button for a current permission

@param table the table to which the button will be added
@param left index of column to left of attach-position in @a table
@param right index of column to right of attach-position in @a table
@param top index or row above attach-position in @a table
@param bottom index of row below attach-position in @a table
@param label label to add to the button, or NULL
@param state state of the button
@param i buttons-array index of added button
@param rt pointer to dialog data struct

@return
*/
static void _e2_permissions_dialog_add_chmod_button (
				GtkWidget *table,
				gint left, gint right, gint top, gint bottom,
				gint i,
				gchar *label,
				gint state,
				E2_PermsDldRuntime *rt)
{
	if (label == NULL)
	{    //align the button reasonably
		GtkWidget *aligner =  gtk_alignment_new (0.1, 0.5, 0, 0 );
		GtkWidget *check_button = gtk_check_button_new ();
		gtk_container_add (GTK_CONTAINER (aligner), check_button);
#ifdef USE_GTK3_2
		gtk_grid_attach (GTK_GRID(table), aligner, left, top, right-left, bottom-top);
#else
		gtk_table_attach (GTK_TABLE (table), aligner,
			left, right, top, bottom,
			GTK_FILL,  //GtkAttachOptions xoptions
			GTK_FILL, //GtkAttachOptions yoptions
			0, //xpadding
			0 //ypadding
			);
#endif
		gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON (check_button), state);
		rt->chmod_buttons[i] = check_button;
	}
	else  //there is a label
		rt->chmod_buttons[i] = e2_button_add_toggle_to_table (table,
			label, state, NULL, NULL, left, right, top, bottom);
	//prevent changes if relevant
	if (!rt->permission)
		gtk_widget_set_sensitive (rt->chmod_buttons[i], FALSE);
}
/**
@brief "toggled" signal callback for add_perms_button and remove_perms_button
This sets all permission-button states to FALSE
@param button the toggled button
@param rt pointer to dialog data struct

@return
*/
static void _e2_permissions_dialog_clear_chmod_buttons_cb (GtkToggleButton *button,
	E2_PermsDldRuntime *rt)
{
	gint i;

	NEEDCLOSEBGL
	for (i = 0; i < PERM_COUNT; i++)
	{
		gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON (rt->chmod_buttons[i]),
			FALSE);
	}
	NEEDOPENBGL
}
/**
@brief "toggled" signal callback for set_perms_button
This sets all permission-button states to conform to mode stored in @a rt
@param button UNUSED the toggled button
@param rt pointer to dialog data struct

@return
*/
static void _e2_permissions_dialog_reset_chmod_buttons_cb (GtkToggleButton *button,
	E2_PermsDldRuntime *rt)
{
	mode_t mode = rt->mode;
	gint i;

	NEEDCLOSEBGL
	for (i = 0; i < PERM_COUNT; i++)
	{
		gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON (rt->chmod_buttons[i]),
			(mode & mask[i]));
	}
	NEEDOPENBGL
}
/**
@brief "toggled" signal callback for recurse_button

@param button the toggled button
@param rt pointer to dialog data struct

@return
*/
static void _e2_permissions_dialog_toggle_recurse_button_cb (GtkToggleButton *button,
	E2_PermsDldRuntime *rt)
{
	if (rt->permission)
	{
		gboolean flag =
#ifdef USE_GTK2_14
			gtk_toggle_button_get_active (button);
#else
			button->active;
#endif
		NEEDCLOSEBGL
		gtk_widget_set_sensitive (rt->recurse_dirs_button, flag);
		gtk_widget_set_sensitive (rt->recurse_other_button, flag);
		NEEDOPENBGL
	}
}
/**
@brief "toggled" signal callback for recurse_dirs and recurs_others buttons

@param button the toggled button
@param rt pointer to dialog data struct

@return
*/
static void _e2_permissions_dialog_toggle_recurse_type_cb (GtkToggleButton *button,
	E2_PermsDldRuntime *rt)
{
	if (rt->permission &&
			//don't care about choice to turn recursion on
#ifdef USE_GTK2_14
			!gtk_toggle_button_get_active (button)
#else
			!button->active
#endif
	)
	{
		NEEDCLOSEBGL
		if (button == GTK_TOGGLE_BUTTON (rt->recurse_dirs_button))
		{
			if (!
#ifdef USE_GTK2_14
				gtk_toggle_button_get_active (GTK_TOGGLE_BUTTON (rt->recurse_other_button))
#else
				GTK_TOGGLE_BUTTON (rt->recurse_other_button)->active
#endif
				)
				gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON (rt->recurse_other_button),TRUE);
		}
		else //button == rt->recurse_other_button
		{
			if (!
#ifdef USE_GTK2_14
				gtk_toggle_button_get_active (GTK_TOGGLE_BUTTON (rt->recurse_dirs_button))
#else
				GTK_TOGGLE_BUTTON (rt->recurse_dirs_button)->active
#endif
			)
				gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON (rt->recurse_dirs_button),TRUE);
		}
		NEEDOPENBGL
	}
}
/**
@brief create and run a dialog for changing item permissions

@param localpath virtual path of item to be processed, with localised string
@param multi TRUE if this dialog is part of a series for multiple items
@param permitted TRUE if change is authorised
@param mode_ret store for mode string to be used in the backend chmod function
@param recurse_ret store for returning whether to recurse the changes
@param winx_ret store for popup window horiz boundary (may hold -1)
@param winy_ret store for popup window vert boundary (may hold -1)

@return enumerator corresponing to the clicked dialog button
*/
DialogButtons e2_permissions_dialog_run (VPATH *localpath,
	gboolean multi, gboolean permitted,
	E2_ChmodType *op_ret, mode_t *mode_ret,
	E2_RecurseType *recurse_ret, gint *winx_ret, gint *winy_ret)
{
	GtkWidget *permissions_dialog;
	GtkWidget *dialog_vbox, *sub_vbox;
	GtkWidget *hbox;
	GtkWidget *frame;
	GtkWidget *table;

	struct stat statbuf;
	struct passwd *pw_buf;
	struct group *grp_buf;
	E2_PermsDldRuntime rt;
	DialogButtons result;

	if (e2_fs_lstat (localpath, &statbuf E2_ERR_NONE()))
		return CANCEL;

//tag DEBUGfreeze;
	//maybe this helps gtk 2.16 crashes ?
//	CLOSEBGL

	rt.permission = permitted;

	GString *label_text = g_string_sized_new (NAME_MAX+20);
	gboolean thisis_dir = e2_fs_is_dir3 (localpath E2_ERR_NONE());
	CLOSEBGL
	permissions_dialog = e2_dialog_create (NULL, NULL, _("permissions"),
		DUMMY_RESPONSE_CB, NULL);
	OPENBGL

	dialog_vbox =
#ifdef USE_GTK2_14
		gtk_dialog_get_content_area (GTK_DIALOG (permissions_dialog));
#else
		GTK_DIALOG (permissions_dialog)->vbox;
#endif
	gtk_container_set_border_width (GTK_CONTAINER (dialog_vbox), E2_PADDING);
#ifndef USE_GTK3_0
	gtk_dialog_set_has_separator (GTK_DIALOG (permissions_dialog), FALSE);  //all info in frames, no need
#endif
	gchar *label = (thisis_dir) ? _("Directory name") : _("Filename") ;
	gchar *name = g_filename_display_basename (VPSTR (localpath));
	g_string_printf (label_text, "%s: <b>%s</b>", label, name);
	g_free (name);

#ifdef USE_GTK3_0
	hbox = gtk_box_new (GTK_ORIENTATION_HORIZONTAL, 0);
#else
	hbox = gtk_hbox_new (FALSE, 0);
#endif
	gtk_box_pack_start (GTK_BOX(dialog_vbox), hbox, TRUE, TRUE, E2_PADDING); //top, bottom padding
	e2_widget_add_mid_label (hbox, label_text->str, 0, TRUE, E2_PADDING); //L, R padding

	label = _("User");
	if ((pw_buf = getpwuid (statbuf.st_uid)) != NULL)
		name = e2_utf8_from_locale (pw_buf->pw_name);
	else
		name = NULL;
	if (name != NULL)
	{
		g_string_printf (label_text, "%s: %s", label, name);
		g_free (name);
	}
	else
		g_string_printf (label_text, "%s: %d", label, (guint) statbuf.st_uid);

#ifdef USE_GTK3_0
	hbox = gtk_box_new (GTK_ORIENTATION_HORIZONTAL, 0);
#else
	hbox = gtk_hbox_new (FALSE, 0);
#endif
	e2_widget_add_mid_label (hbox, label_text->str, 0, TRUE, E2_PADDING);
	gtk_box_pack_start (GTK_BOX(dialog_vbox), hbox, TRUE, TRUE, 0);

	label = _("Group");
	if ((grp_buf = getgrgid (statbuf.st_gid)) != NULL)
		name = e2_utf8_from_locale (grp_buf->gr_name);
	else
		name = NULL;
	if (name != NULL)
	{
		g_string_printf (label_text, "%s: %s", label, name);
		g_free (name);
	}
	else
		g_string_printf (label_text, "%s: %d", label, (guint) statbuf.st_gid);

#ifdef USE_GTK3_0
	hbox = gtk_box_new (GTK_ORIENTATION_HORIZONTAL, 0);
#else
	hbox = gtk_hbox_new (FALSE, 0);
#endif
	e2_widget_add_mid_label (hbox, label_text->str, 0, TRUE, E2_PADDING);
	gtk_box_pack_start (GTK_BOX(dialog_vbox), hbox, TRUE, TRUE, 0);

	gchar *s = e2_fs_get_perm_string (statbuf.st_mode);
	label_text = g_string_assign (label_text, g_utf8_next_char (s));	//skip the "type code" in 1st byte of string
	g_free (s);
	g_string_insert_c (label_text, e2_utils_get_byte_position (label_text->str, 6), ' ');
	g_string_insert_c (label_text, e2_utils_get_byte_position (label_text->str, 3), ' ');
	g_string_insert_c (label_text, 0, ' ');
	label_text = g_string_prepend (label_text, _("currently"));

#ifdef USE_GTK3_0
	hbox = gtk_box_new (GTK_ORIENTATION_HORIZONTAL, 0);
#else
	hbox = gtk_hbox_new (FALSE, 0);
#endif
	e2_widget_add_mid_label (hbox, label_text->str, 0, TRUE, E2_PADDING);
	gtk_box_pack_start (GTK_BOX(dialog_vbox), hbox, TRUE, TRUE, 0);

	frame = gtk_frame_new (_("Permissions"));
	gtk_box_pack_start (GTK_BOX (dialog_vbox), frame, TRUE, TRUE, E2_PADDING);

#ifdef USE_GTK3_0
	sub_vbox = gtk_box_new (GTK_ORIENTATION_VERTICAL, 0);
#else
	sub_vbox = gtk_vbox_new (FALSE, 0);
#endif
	gtk_container_add (GTK_CONTAINER (frame), sub_vbox);

	table = e2_widget_add_table (sub_vbox, 4, 5, TRUE, TRUE, 0);  //4 rows, 5 cols, homogen, fill, no pad
	e2_widget_add_mid_label_to_table (table, _("Read"), 0.0,	1,2,0,1);  //gint left, gint right, gint top, gint bottom)
	e2_widget_add_mid_label_to_table (table, _("Write"), 0.0,	2,3,0,1);
	e2_widget_add_mid_label_to_table (table, _("Exec"), 0.05,	3,4,0,1);
	e2_widget_add_mid_label_to_table (table, _("Special"), 0.2,	4,5,0,1);

	e2_widget_add_mid_label_to_table (table, _("User"), 0.1,	0,1,1,2);
	_e2_permissions_dialog_add_chmod_button (table, 1,2,1,2, USR_READ, NULL,
		statbuf.st_mode & S_IRUSR, &rt);
	_e2_permissions_dialog_add_chmod_button (table, 2,3,1,2, USR_WRITE, NULL,
		statbuf.st_mode & S_IWUSR, &rt);
	_e2_permissions_dialog_add_chmod_button (table, 3,4,1,2, USR_EXEC, NULL,
		statbuf.st_mode & S_IXUSR,	&rt);
	_e2_permissions_dialog_add_chmod_button (table, 4,5,1,2, SETUID, _("Set UID"),
		statbuf.st_mode & S_ISUID, &rt);

	e2_widget_add_mid_label_to_table (table, _("Group"), 0.1,	0,1,2,3);
	_e2_permissions_dialog_add_chmod_button (table, 1,2,2,3, GRP_READ, NULL,
		statbuf.st_mode & S_IRGRP, &rt);
	_e2_permissions_dialog_add_chmod_button (table, 2,3,2,3, GRP_WRITE, NULL,
		statbuf.st_mode & S_IWGRP, &rt);
	_e2_permissions_dialog_add_chmod_button (table, 3,4,2,3, GRP_EXEC, NULL,
		statbuf.st_mode & S_IXGRP, &rt);
	_e2_permissions_dialog_add_chmod_button (table, 4,5,2,3, SETGID, _("Set GID"),
		statbuf.st_mode & S_ISGID, &rt);

	e2_widget_add_mid_label_to_table (table, _("Other"), 0.1,	0,1,3,4);
	_e2_permissions_dialog_add_chmod_button (table, 1,2,3,4, OTH_READ, NULL,
		statbuf.st_mode & S_IROTH, &rt);
	_e2_permissions_dialog_add_chmod_button (table, 2,3,3,4, OTH_WRITE, NULL,
		statbuf.st_mode & S_IWOTH, &rt);
	_e2_permissions_dialog_add_chmod_button (table, 3,4,3,4, OTH_EXEC, NULL,
		statbuf.st_mode & S_IXOTH, &rt);
	_e2_permissions_dialog_add_chmod_button (table, 4,5,3,4, STICKY, _("Sticky"),
		statbuf.st_mode & S_ISVTX, &rt);

	if (!permitted || !thisis_dir)
	{
		//prevent testing of missing widgets in the response cb
		rt.recurse_button = NULL;
		rt.recurse_dirs_button = NULL;
		rt.recurse_other_button = NULL;
	}

	if (permitted)
	{
		frame = gtk_frame_new (_("Action"));
		gtk_box_pack_start (GTK_BOX (dialog_vbox), frame, TRUE, TRUE, E2_PADDING);

#ifdef USE_GTK3_0
		sub_vbox = gtk_box_new (GTK_ORIENTATION_VERTICAL, 0);
#else
		sub_vbox = gtk_vbox_new (FALSE, 0);
#endif
		gtk_container_add (GTK_CONTAINER (frame), sub_vbox);

		table = e2_widget_add_table (sub_vbox, 1, 5, TRUE, TRUE, 0);  //1 rows, 5 cols, homogen, fill, no pad
		rt.mode = statbuf.st_mode;
		rt.set_perms_button = e2_button_add_radio_to_table (table, _("_Set"),
			NULL, TRUE, _e2_permissions_dialog_reset_chmod_buttons_cb,
			&rt,
			1, 2, 0, 1 ); //gint left, gint right, gint top, gint bottom
		rt.add_perms_button = e2_button_add_radio_to_table (table, _("_Add"),
			gtk_radio_button_get_group (GTK_RADIO_BUTTON (rt.set_perms_button)),
			FALSE, _e2_permissions_dialog_clear_chmod_buttons_cb, &rt,
			2, 3, 0, 1);
		rt.remove_perms_button = e2_button_add_radio_to_table (table, _("_Remove"),
			gtk_radio_button_get_group (GTK_RADIO_BUTTON (rt.add_perms_button)),
			FALSE, _e2_permissions_dialog_clear_chmod_buttons_cb, &rt,
			3, 4, 0, 1);

		if (thisis_dir)  //no longer do this for 'multi'
		{
			table = e2_widget_add_table (sub_vbox, 1, 5, TRUE, TRUE, 0);  //1 row, 5 cols, homogen, fill, no pad
			rt.recurse_button = e2_button_add_toggle_to_table (table, _("R_ecurse:"),
				FALSE, _e2_permissions_dialog_toggle_recurse_button_cb, &rt,
				1,2,0,1 );
			e2_widget_set_safetip (rt.recurse_button,
				_("If activated, changes will be applied to selected items and"
					" also their descendents, if such items match your choice of"
					" \"directories\" and/or \"others\" (anything not a directory)"));
			rt.recurse_dirs_button = e2_button_add_toggle_to_table (table, _("d_irectories"),
			   TRUE, _e2_permissions_dialog_toggle_recurse_type_cb, &rt, 2,3,0,1) ;
			gtk_widget_set_sensitive (rt.recurse_dirs_button, FALSE);
			rt.recurse_other_button = e2_button_add_toggle_to_table (table, _("o_thers"),
			   TRUE, _e2_permissions_dialog_toggle_recurse_type_cb, &rt, 3,4,0,1) ;
			gtk_widget_set_sensitive (rt.recurse_other_button, FALSE);
		}
	}
	else //no right to change object permissions
		//show message
		e2_dialog_setup_auth (dialog_vbox);

	g_string_free (label_text, TRUE);
	result = NO_TO_ALL;	//default in case of abort

	E2_DialogFlags flags = 0;

	//add buttons in the order that they will appear
	GtkWidget *btn;
	E2_Button no_btn;
	if (multi)
	{
		e2_dialog_set_negative_response (permissions_dialog, E2_RESPONSE_NOTOALL);
		e2_dialog_add_defined_button (permissions_dialog, &E2_BUTTON_CANCEL);
		btn = e2_dialog_add_defined_button (permissions_dialog, &E2_BUTTON_APPLYTOALL);
		if (!permitted)
			gtk_widget_set_sensitive (btn, FALSE);
		e2_button_derive (&no_btn, &E2_BUTTON_NO, BTN_NO_SKIP);
		flags |= E2_DIALOG_MULTI;
	}
	else
	{
		e2_dialog_set_negative_response (permissions_dialog, GTK_RESPONSE_NO);
		e2_button_derive (&no_btn, &E2_BUTTON_NO, BTN_NO_CANCEL);
	}

	e2_dialog_add_defined_button (permissions_dialog, &no_btn);

	E2_BUTTON_APPLY.showflags |= E2_BTN_DEFAULT;	//CHECKME local copy ?
	btn = e2_dialog_add_defined_button (permissions_dialog, &E2_BUTTON_APPLY);
	if (!permitted)
		gtk_widget_set_sensitive (btn, FALSE);

	CLOSEBGL
	e2_dialog_setup (permissions_dialog, app.main_window);
	e2_dialog_run (permissions_dialog, NULL, flags);
	if (*winx_ret >= 0 && *winy_ret >= 0)
		gtk_window_move (GTK_WINDOW (permissions_dialog), *winx_ret, *winy_ret);
	//block until the user selects
	//refreshing is enabled downstream, while waiting
	result = e2_dialog_wait (permissions_dialog, TRUE, TRUE, multi, TRUE);//CHECKME more reliable if TRUE maincontext ?
	OPENBGL

	//defaults
	*op_ret = E2_CHMOD_NONE;
	*mode_ret = 0;
	*recurse_ret = E2_RECURSE_NONE;
	if (result == OK || result == YES_TO_ALL)
	{
		if (permitted)
		{
			_e2_permissions_dialog_get_mode_string (op_ret, mode_ret, &rt);
			//some widgets only exist if permission is TRUE;
			if (rt.recurse_button != NULL
			  //the recurse button has been added to the dialog
#ifdef USE_GTK2_14
				&& gtk_toggle_button_get_active (GTK_TOGGLE_BUTTON (rt.recurse_button))
#else
				&& GTK_TOGGLE_BUTTON (rt.recurse_button)->active
#endif
			)
			{
				if (
#ifdef USE_GTK2_14
					gtk_toggle_button_get_active (GTK_TOGGLE_BUTTON (rt.recurse_dirs_button))
#else
					GTK_TOGGLE_BUTTON (rt.recurse_dirs_button)->active
#endif
				)
					*recurse_ret = E2_RECURSE_DIRS;
				if (
#ifdef USE_GTK2_14
					gtk_toggle_button_get_active (GTK_TOGGLE_BUTTON (rt.recurse_other_button))
#else
					GTK_TOGGLE_BUTTON (rt.recurse_other_button)->active
#endif
				)
					*recurse_ret |= E2_RECURSE_OTHER;
			}
		}
	}

	if (GTK_IS_DIALOG (permissions_dialog)) //not explicitly closed by the user
	{
		CLOSEBGL
		gtk_window_get_position (GTK_WINDOW (permissions_dialog), winx_ret, winy_ret);
		//now it's safe to destroy the window
		gtk_widget_destroy (permissions_dialog);
		OPENBGL
	}

	return result;
}
