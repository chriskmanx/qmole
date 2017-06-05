/* $Id: e2_ownership_dialog.c 2934 2013-11-15 08:08:22Z tpgww $

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
//#include <unistd.h>
#include <pwd.h>
#include <grp.h>
#include "e2_dialog.h"
#include "e2_ownership_dialog.h"
#include "e2_filelist.h"

typedef struct _E2_OwnersDldRuntime
{
	GtkWidget *user_combo;
	GtkWidget *group_combo;
	GtkWidget *recurse_dirs_button;
	uid_t owner;
	gid_t group;
	gboolean recurse;
	gboolean permission;
} E2_OwnersDldRuntime;

/**
@brief get user id corresponing to text entered into the dialog

@param rt pointer to dialog data struct

@return the id
*/
static uid_t _e2_ownership_dialog_get_user_id (E2_OwnersDldRuntime *rt)
{
	gchar *user;
	uid_t user_id;

	user = (gchar *) gtk_entry_get_text (
#ifdef USE_GTK2_14
		GTK_ENTRY (gtk_bin_get_child (GTK_BIN (rt->user_combo)))
#else
		GTK_ENTRY (GTK_BIN (rt->user_combo)->child)
#endif
		);
	user = e2_utf8_to_locale (user);
	if (user != NULL)
	{
		struct passwd *pw_buf;

		if ((pw_buf = getpwnam (user)) != NULL)
			user_id = pw_buf->pw_uid;
		else
		{
			user_id = (uid_t) strtol (user, NULL, 10);
			if (errno == EINVAL)
			{
				//FIXME user 0 probably not relevant
			}
		}
		g_free (user);
	}
	else
		user_id = 0;	//FIXME

	return user_id;
}
/**
@brief get group id corresponing to text entered into the dialog

@param rt pointer to dialog data struct

@return the id
*/
static gid_t _e2_ownership_dialog_get_group_id (E2_OwnersDldRuntime *rt)
{
	gchar *group;
	struct group *grp_buf;
	gid_t group_id;

	group = (gchar *) gtk_entry_get_text (
#ifdef USE_GTK2_14
		GTK_ENTRY (gtk_bin_get_child (GTK_BIN (rt->group_combo)))
#else
		GTK_ENTRY (GTK_BIN (rt->group_combo)->child)
#endif
	);
	group = e2_utf8_to_locale (group);
	if (group != NULL)
	{
		if ((grp_buf = getgrnam (group)) != NULL)
			group_id = grp_buf->gr_gid;
		else
		{
			group_id = (gid_t) strtol (group, NULL, 10);
			if (errno == EINVAL)
			{
				//FIXME group 0 probably not relevant
			}
		}
		g_free (group);
	}
	else
		group_id = 0;

	return group_id;
}
/**
@brief update recurse flag

This only applies if change is permitted.

@param togglebutton the clicked button
@param rt pointer to dialog data struct

@return
*/
static void _e2_ownership_dialog_toggle_recurse_button_cb (
	GtkToggleButton *togglebutton, E2_OwnersDldRuntime *rt)
{
//	NEEDCLOSEBGL
	rt->recurse =
#ifdef USE_GTK2_14
		gtk_toggle_button_get_active (GTK_TOGGLE_BUTTON (rt->recurse_dirs_button));
#else
		GTK_TOGGLE_BUTTON (rt->recurse_dirs_button)->active;
#endif
//	NEEDOPENBGL
}
/**
@brief change widget focus after the user-combobox entry, or possibly group-entry, is activated

@param entry the activated widget
@param focus the widget to be focused next

@return
*/
static void _e2_ownership_dialog_activate_cb (GtkEntry *entry, GtkWidget *focus)
{
	//prevent activation of any newly-focused widget too
	g_signal_stop_emission_by_name ((gpointer)entry, "activate");
	NEEDCLOSEBGL
	gtk_widget_grab_focus (focus);
	NEEDOPENBGL
}
/**
@brief apply current settings after the group-combobox entry is activated for a non-directory item

@param entry the activated widget
@param dialog the ownership-dialog widget

@return
*/
static void _e2_ownership_dialog_activate_cb2 (GtkEntry *entry, GtkWidget *dialog)
{
	g_signal_stop_emission_by_name ((gpointer)entry, "activate");
//	NEEDOPENBGL
	g_signal_emit_by_name (G_OBJECT (dialog), "response", E2_RESPONSE_APPLY, NULL);
//	NEEDCLOSEBGL
}
/**
@brief create and run a dialog for changing item permissions
This is run from inside a queued thread-function
@param localpath path of item to be processed
@param multi TRUE if this dialog is part of a series for multiple items
@param permitted TRUE if change is authorised
@param owner_ret store for returning the new owner
@param group_ret store for returning the new group
@param recurse_ret store for returning whether to recurse the changes
@param winx_ret store for popup window horiz boundary (may hold -1)
@param winy_ret store for popup window vert boundary (may hold -1)

@return enumerator corresponing to the clicked dialog button
*/
DialogButtons e2_ownership_dialog_run (VPATH *localpath,
	gboolean multi, gboolean permitted,
	uid_t *owner_ret, gid_t *group_ret, gboolean *recurse_ret,
	gint *winx_ret, gint *winy_ret)
{
	GtkWidget *ownership_dialog;
	GtkWidget *dialog_vbox;
	GtkWidget *table, *hbox;

//	E2_OwnersDldRuntime *rt = ALLOCATE (E2_OwnersDldRuntime);
//	CHECKALLOCATEDWARN (rt, return GTK_RESPONSE_CANCEL);
	E2_OwnersDldRuntime rt;
	DialogButtons result;
    struct passwd *pw_buf;
   	struct group *grp_buf;
	gchar *utf, *current, *tmp;
	struct stat statbuf;

	if (e2_fs_lstat (localpath, &statbuf E2_ERR_NONE()))
		return CANCEL;

	//setup the combobox history data
	current = NULL;
	gint user_index = 0, grp_index = 0;  //default values for combobox display (= 1st entry)
	GList *userlist = NULL, *grouplist = NULL;

	gint myuid = getuid ();

	uid_t src_uid = statbuf.st_uid;
	if (permitted)
    {
		rt.permission = TRUE;	//confirm ok to proceed
		setpwent ();
		while ((pw_buf = getpwent ()) != NULL)
		{	//screen-out superfluous names
			if (
				(myuid > 0) //user is not root
				&& ((guint) pw_buf->pw_uid > 0)  //buffer entry is not root's
				&& ((guint) pw_buf->pw_uid < UID_LOWLIMIT)  //but is less than the system-user threshold
			)
				continue;
			utf = e2_utf8_from_locale (pw_buf->pw_name);
			if (utf == NULL)
				utf = g_strdup_printf ("%u", (guint) pw_buf->pw_uid);
			userlist = g_list_prepend (userlist, utf);
			//remember the combo index for the owner
			if ((uid_t) pw_buf->pw_uid == src_uid)
				current = utf;
		}
	    endpwent();

		if ((pw_buf = getpwuid (src_uid)) == NULL)
		{
			userlist = g_list_prepend (userlist,
				g_strdup_printf ("%u", (guint) src_uid));
			current = userlist->data;
		}
		if (userlist->next != NULL)
		{
			userlist = g_list_sort (userlist, (GCompareFunc) g_utf8_collate);
			user_index = g_list_index (userlist, current);
		}

		gid_t src_gid = statbuf.st_gid;
		if (myuid == 0)
		{
		    setgrent ();
			while ((grp_buf = getgrent ()) != NULL)
			{
				utf = e2_utf8_from_locale (grp_buf->gr_name);
				if (utf == NULL)
					utf = g_strdup_printf ("%u", (guint) grp_buf->gr_gid);
				grouplist = g_list_prepend (grouplist, utf);
				if ((gid_t) grp_buf->gr_gid == src_gid)
					current = utf;
			}
			endgrent();
		}
		else //not root
		{
			gid_t grp_ids[REPORTABLE_GROUPS];
			gint n = getgroups (REPORTABLE_GROUPS, grp_ids);
			gint ctr;
			for (ctr = 0; ctr < n; ctr++)
			{
				if ((grp_buf = getgrgid (grp_ids[ctr])) != NULL)
				{
					utf = e2_utf8_from_locale (grp_buf->gr_name);
					if (utf == NULL)
						utf = g_strdup_printf ("%u", (guint) grp_buf->gr_gid);
					grouplist = g_list_prepend (grouplist, utf);
					if ((gid_t) grp_buf->gr_gid == src_gid)
						current = utf;
				}
			}
		}
		if ((grp_buf = getgrgid (src_gid)) == NULL)
		{
			grouplist = g_list_prepend (grouplist,
				g_strdup_printf ("%u", (guint) src_gid));
			current = grouplist->data;
		}
		if (grouplist->next != NULL)
		{
			grouplist = g_list_sort (grouplist, (GCompareFunc) g_utf8_collate);
			grp_index = g_list_index (grouplist, current);
		}
	}
	else  //no changes allowed, just show current values
    {
		rt.permission = FALSE;	//signal not ok to proceed, even if ok is clicked
    	if ((pw_buf = getpwuid (statbuf.st_uid)) != NULL)
		{
			utf = e2_utf8_from_locale (pw_buf->pw_name);
			if (utf == NULL)
				utf = g_strdup_printf ("%u", (guint) pw_buf->pw_uid);
			userlist = g_list_prepend (userlist, utf);
		}
		else
			userlist = g_list_prepend (userlist,
				g_strdup_printf ("%u", (guint) statbuf.st_uid));

    	if ((grp_buf = getgrgid (statbuf.st_gid)) != NULL)
		{
			utf = e2_utf8_from_locale (grp_buf->gr_name);
			if (utf == NULL)
				utf = g_strdup_printf ("%u", (guint) statbuf.st_gid);
			grouplist = g_list_prepend (grouplist, utf);
		}
		else
			grouplist = g_list_prepend (grouplist,
				g_strdup_printf ("%u", (guint) statbuf.st_gid));
	}

//tag DEBUGfreeze;
	//maybe this helps gtk 2.16 crashes ?
//	CLOSEBGL

	GString *label_text = g_string_sized_new (NAME_MAX+20);
	gboolean thisis_dir = e2_fs_is_dir3 (localpath E2_ERR_NONE());

	CLOSEBGL
	ownership_dialog = e2_dialog_create (NULL, NULL, _("ownership"),
		DUMMY_RESPONSE_CB, NULL);
	OPENBGL

	dialog_vbox =
#ifdef USE_GTK2_14
		gtk_dialog_get_content_area (GTK_DIALOG (ownership_dialog));
#else
		GTK_DIALOG (ownership_dialog)->vbox;
#endif
	gtk_container_set_border_width (GTK_CONTAINER (dialog_vbox), E2_PADDING);

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
	gtk_widget_show (hbox);

	gchar *s = e2_fs_get_perm_string (statbuf.st_mode);
	label_text = g_string_assign (label_text, s);
	g_free (s);
	g_string_insert_c (label_text, e2_utils_get_byte_position (label_text->str, 7), ' ');
	g_string_insert_c (label_text, e2_utils_get_byte_position (label_text->str, 4), ' ');
	g_string_insert_c (label_text, e2_utils_get_byte_position (label_text->str, 1), ' ');
	g_string_prepend (label_text, ": ");
	g_string_prepend (label_text, _("Permissions"));
#ifdef USE_GTK3_0
	hbox = gtk_box_new (GTK_ORIENTATION_HORIZONTAL, 0);
#else
	hbox = gtk_hbox_new (FALSE, 0);
#endif
	e2_widget_add_mid_label (hbox, label_text->str, 0, TRUE, E2_PADDING);
	gtk_box_pack_start (GTK_BOX(dialog_vbox), hbox, TRUE, TRUE, 0);
	gtk_widget_show (hbox);

	g_string_free (label_text, TRUE);

	table = e2_widget_add_table (dialog_vbox, 2, 3, TRUE, TRUE, 0);  //2 rows, 3 cols, homogen, fill, no pad
	gtk_container_set_border_width (GTK_CONTAINER (table), E2_PADDING);
#ifdef USE_GTK3_2
	gtk_grid_set_row_spacing (GTK_GRID (table), E2_PADDING);
#else
	gtk_table_set_row_spacings (GTK_TABLE (table), E2_PADDING);
#endif
//	CLOSEBGL //block entry-cursor blinking for now, to avoid sporadic bad keypress-latency

	//comboboxes for user & group
	tmp = e2_utils_strcat (_("User"), ": ");
	e2_widget_add_mid_label_to_table (table, tmp, 0.1, 0,1,0,1);
	g_free (tmp);
#ifndef USE_GTK3_0
	CLOSEBGL
#endif
	//on gtk2 at least, combobox seems to work better if this func is called with BGL closed
	rt.user_combo = e2_combobox_get ((ActivateFunc)NULL, NULL,
		(userlist == NULL) ? NULL : &userlist,
		E2_COMBOBOX_HAS_ENTRY | E2_COMBOBOX_NO_AUTO_HISTORY | E2_COMBOBOX_CYCLE_HISTORY);
#ifdef GTK3_COMBO_FIX
	CLOSEBGL
#endif
	e2_combobox_set_active (rt.user_combo, user_index);
	e2_combobox_block_changed (rt.user_combo); //prevent "activate" signal emission upon change
#ifdef USE_GTK3_2
	gtk_grid_attach (GTK_GRID (table), rt.user_combo, 1, 0, 2, 1);
#else
	gtk_table_attach_defaults (GTK_TABLE (table), rt.user_combo, 1, 3, 0, 1);
#endif
	e2_list_free_with_data (&userlist);

	tmp = e2_utils_strcat (_("Group"), ": ");
	e2_widget_add_mid_label_to_table (table, tmp, 0.1, 0,1,1,2);
	g_free (tmp);
	rt.group_combo = e2_combobox_get ((ActivateFunc)NULL, NULL,
		(grouplist == NULL) ? NULL : &grouplist,
		E2_COMBOBOX_HAS_ENTRY | E2_COMBOBOX_NO_AUTO_HISTORY | E2_COMBOBOX_CYCLE_HISTORY);
	e2_combobox_set_active (rt.group_combo, grp_index);
	e2_combobox_block_changed (rt.group_combo); //prevent "activate" signal emission upon change
#ifdef USE_GTK3_2
	gtk_grid_attach (GTK_GRID (table), rt.group_combo, 1, 1, 2, 1);
#else
	gtk_table_attach_defaults (GTK_TABLE (table), rt.group_combo, 1, 3, 1, 2);
#endif
	e2_list_free_with_data (&grouplist);

	if (permitted)
	{
		rt.recurse = *recurse_ret;
		if (thisis_dir)  //note: no recurse button now if multiple-selection
		{
			table = e2_widget_add_table (dialog_vbox, 1, 3, TRUE, TRUE, 0); //1 row, 3 cols, homogen, fill, no pad
#ifdef USE_GTK3_2
			gtk_grid_set_row_spacing (GTK_GRID (table), E2_PADDING);
#else
			gtk_table_set_row_spacings (GTK_TABLE(table), E2_PADDING);
#endif
			rt.recurse_dirs_button = e2_button_add_toggle_to_table (table, _("_Recurse"),
				rt.recurse, _e2_ownership_dialog_toggle_recurse_button_cb, &rt, //rt,
				1, 2, 0, 1 ); // gint left, gint right, gint top, gint bottom
		}
	}
	else
	{
		gtk_widget_set_sensitive (rt.user_combo, FALSE);
		gtk_widget_set_sensitive (rt.group_combo, FALSE);
		e2_dialog_setup_auth (dialog_vbox);	//frame, makes separator redundant
#ifndef USE_GTK3_0
		gtk_dialog_set_has_separator (GTK_DIALOG (ownership_dialog), FALSE);
#endif
	}

	result = NO_TO_ALL;	//default in case of abort

	E2_DialogFlags flags = 0;

	//add buttons in the order that they will appear
	GtkWidget *btn;
	E2_Button no_btn;
	if (multi)
	{
		e2_dialog_set_negative_response (ownership_dialog, E2_RESPONSE_NOTOALL);
		e2_dialog_add_defined_button (ownership_dialog, &E2_BUTTON_CANCEL);
		btn = e2_dialog_add_defined_button (ownership_dialog, &E2_BUTTON_APPLYTOALL);
		if (!rt.permission)
			gtk_widget_set_sensitive (btn, FALSE);
		e2_button_derive (&no_btn, &E2_BUTTON_NO, BTN_NO_SKIP);
		flags |= E2_DIALOG_MULTI;
	}
	else
	{
		e2_dialog_set_negative_response (ownership_dialog, GTK_RESPONSE_NO);
		e2_button_derive (&no_btn, &E2_BUTTON_NO, BTN_NO_CANCEL);
	}

	e2_dialog_add_defined_button (ownership_dialog, &no_btn);
	E2_BUTTON_APPLY.showflags |= E2_BTN_DEFAULT;	//CHECKME local copy ?
	btn = e2_dialog_add_defined_button (ownership_dialog, &E2_BUTTON_APPLY);
	if (!rt.permission)
		gtk_widget_set_sensitive (btn, FALSE);

	if (permitted)
	{
		g_signal_connect (
#ifdef USE_GTK2_14
			G_OBJECT (gtk_bin_get_child (GTK_BIN(rt.user_combo))),
#else
			G_OBJECT (GTK_BIN (rt.user_combo)->child),
#endif
			"activate",
			G_CALLBACK(_e2_ownership_dialog_activate_cb), rt.group_combo);
		if (thisis_dir)
			//don't automatically apply the current values, in case user wants to check the recurse button
			g_signal_connect (
#ifdef USE_GTK2_14
				G_OBJECT (gtk_bin_get_child (GTK_BIN(rt.group_combo))),
#else
				G_OBJECT (GTK_BIN (rt.group_combo)->child),
#endif
				"activate",
				G_CALLBACK(_e2_ownership_dialog_activate_cb), btn);
		else
			g_signal_connect (
#ifdef USE_GTK2_14
				G_OBJECT (gtk_bin_get_child (GTK_BIN(rt.group_combo))),
#else
				G_OBJECT (GTK_BIN (rt.group_combo)->child),
#endif
				"activate",
				G_CALLBACK(_e2_ownership_dialog_activate_cb2), ownership_dialog);
	}

#ifdef GTK3_COMBO_FIX
	gtk_widget_grab_focus (gtk_bin_get_child (GTK_BIN(rt.user_combo)));
#endif
	e2_dialog_setup (ownership_dialog, app.main_window);
	e2_dialog_run (ownership_dialog, NULL, flags);
	if (*winx_ret >= 0 && *winy_ret >= 0)
		gtk_window_move (GTK_WINDOW (ownership_dialog), *winx_ret, *winy_ret);
	//refreshing is enabled downstream, while waiting
	//block until the user selects
	result = e2_dialog_wait (ownership_dialog, TRUE, TRUE, multi, TRUE);//CHECKME more reliable if TRUE maincontext ?
	OPENBGL

	if (result == OK || result == YES_TO_ALL)
	{
		if (permitted)
		{
			*owner_ret = _e2_ownership_dialog_get_user_id (&rt);
			*group_ret = _e2_ownership_dialog_get_group_id (&rt);
			*recurse_ret = rt.recurse;
		}
	}
	if (GTK_IS_DIALOG(ownership_dialog)) //not explicitly closed by the user
	{
		CLOSEBGL
		gtk_window_get_position (GTK_WINDOW (ownership_dialog), winx_ret, winy_ret); //irrelevant if closed
		//now it's safe to destroy widgets
		gtk_widget_destroy (ownership_dialog);
		OPENBGL
	}

	return result;
}
