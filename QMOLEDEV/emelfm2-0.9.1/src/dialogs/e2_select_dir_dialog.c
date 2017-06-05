/* $Id: e2_select_dir_dialog.c 2790 2013-10-09 13:00:04Z tpgww $

Copyright (C) 2005-2013 tooar <tooar@emelfm2.net>

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
#include "e2_dialog.h"
#include "e2_icons.h"

/**
@brief display directory-chooser dialog
Assumes BGL closed
@a newdir is set to a /-terminated string, newly-allocated,
or else NULL, if no or invalid choice is made

@param view pointer to view data for the pane to which this dialog will apply
@param startdir path of dir to show in chooser when it opens, utf8 string
@param newdir pointer to store for chosen new dir-path

@return
*/
void e2_opendir_dialog_create (ViewInfo *view, gchar *startdir, gchar **newdir)
{
	GtkWidget *dialog = gtk_file_chooser_dialog_new (NULL,
		GTK_WINDOW (app.main_window), GTK_FILE_CHOOSER_ACTION_SELECT_FOLDER, NULL, NULL);
	e2_dialog_setup_chooser (dialog,
		_("open"),
		startdir,
		GTK_FILE_CHOOSER_ACTION_SELECT_FOLDER,
		!view->show_hidden,	//show- or hide-hidden
		FALSE,	//single-selection
		GTK_RESPONSE_OK,	//default response
		STOCK_NAME_CANCEL, GTK_RESPONSE_CANCEL,
		STOCK_NAME_OPEN, GTK_RESPONSE_OK,
		NULL);

	e2_dialog_setup (dialog, app.main_window);
//	gtk_widget_show (dialog);

	*newdir = NULL;
	gint response;
	while ((response = e2_dialog_run_simple (dialog, app.main_window)) == E2_RESPONSE_USER1)
	{}

	if (response == GTK_RESPONSE_OK)
	{
		gchar *dir = gtk_file_chooser_get_filename (GTK_FILE_CHOOSER (dialog));
//		if (dir != NULL)
//		{
			gchar *utf = F_FILENAME_FROM_LOCALE (dir);	//not DISPLAY
			*newdir = g_strconcat (utf, G_DIR_SEPARATOR_S, NULL);
			g_free (dir);
			F_FREE (utf, dir);
//		}
	}
	if (GTK_IS_WIDGET (dialog))	//did not close using window-manager close-button
		gtk_widget_destroy (dialog);
}
