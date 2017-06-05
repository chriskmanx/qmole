/* $Id: e2_password_dialog.h 2832 2013-10-23 08:02:31Z tpgww $

Copyright (C) 2007-2013 tooar <tooar@emelfm2.net>

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
@file src/dialogs/e2_password_dialog.h
@brief header file for password dialog
*/

#include "emelfm2.h"
#include "e2_dialog.h"

/*
typedef struct _E2_PWDialogRuntime
{
	GtkWidget *dialog;		//the displayed dialog
	DialogButtons result;	//enumerator of what the user chose
	gboolean destroy;		//TRUE to destroy dialog and data after user's choice
	GtkWidget *pwentry1;
	GtkWidget *pwentry2;
//	GtkWidget *table;		//pointer to tablular layuout, if that's used
	gboolean confirm;		//TRUE when requiring matching contents in pwentry1 and pwentr2
	gboolean hint;			//TRUE when the last char in either used entry is displayed as plaintext
	gboolean hide;			//TRUE when no entered char is to be echoed
	gboolean plain;			//TRUE when the entered text is displayed as is
	gchar **passwd;			//place to store entered (and if relevant matched) plaintext password
} E2_PWDialogRuntime;

UNUSED DialogButtons e2_password_dialog_run (gboolean threaded);
*/
typedef struct _E2_PWDataRuntime
{
	GtkWidget *dialog;	//widget to be "responsed" in entry-activate cb, or NULL
	GtkWidget *focus;	//widget to be focused in entry-activate cb, or NULL
	GtkWidget *pwentry1;
	GtkWidget *pwentry2;
	gboolean confirm;	//TRUE when requiring matching contents in pwentry1 and pwentry2
	gboolean hint;		//TRUE when the last char in either used entry is displayed as plaintext
	gboolean hide;		//TRUE when no entered char is to be echoed
	gboolean plain;		//TRUE when the entered text is displayed as is
	gchar **passwd;		//place to store entered (and if relevant matched) plaintext password, or NULL
} E2_PWDataRuntime;

E2_PWDataRuntime *e2_password_dialog_setup (GtkWidget *box, gboolean confirm,
	const gchar *mainprompt, gchar **pw) G_GNUC_MALLOC;
gboolean e2_password_dialog_confirm (E2_PWDataRuntime *rt);
void e2_password_dialog_backup (E2_PWDataRuntime *rt);
