////////////////////////////////////////////////////////////////////////////
// Atol file manager project <http://atol.sf.net>
//
// This code is licensed under BSD license.See "license.txt" for more details.
//
// File: object of this class should be alive as long as program lives
//		 so that instance can be detected (borowed from gnome-volume-manager)
////////////////////////////////////////////////////////////////////////////
/*
 * src/clipboard.c - X clipboard hack to detect if daemon is running
 *
 * Elliot Lee <sopwith@redhat.com>
 *
 * (C) Copyright 1999 Red Hat, Inc.
 *
 * Licensed under the GNU GPL v2.  See COPYING.
 */

#include "SingleInstance.h"
#include <errno.h>
#include <gtk/gtk.h>
#include <gdk/gdkx.h>

//pair of dummy functions
static void clipboard_get_func (GtkClipboard *clipboard, GtkSelectionData *selection_data, guint info, gpointer user_data_or_owner){}
static void clipboard_clear_func (GtkClipboard *clipboard, gpointer user_data_or_owner){}

#define CLIPBOARD_NAME "_ATOL"

/*
 * atol_get_clipboard - try and get the CLIPBOARD_NAME clipboard
 *
 * Returns TRUE if successfully retrieved and FALSE otherwise.
 */
gboolean atol_get_clipboard ()
{
	return 1;
	static const GtkTargetEntry targets[] = { {CLIPBOARD_NAME, 0, 0} };
	Atom atom = gdk_x11_get_xatom_by_name (CLIPBOARD_NAME);
	gboolean retval = FALSE;
	GtkClipboard *clipboard;

	XGrabServer (GDK_DISPLAY ());

	if (XGetSelectionOwner (GDK_DISPLAY (), atom) != None)
		goto out;

	clipboard = gtk_clipboard_get (gdk_atom_intern (CLIPBOARD_NAME, FALSE));

	if (gtk_clipboard_set_with_data (clipboard, targets,
					 G_N_ELEMENTS (targets),
					 clipboard_get_func,
					 clipboard_clear_func, NULL))
		retval = TRUE;

out:
	XUngrabServer (GDK_DISPLAY ());
	gdk_flush ();

	return retval;
}

CSingleInstance::CSingleInstance(const char *szName)
{
	m_bAlreadyExists = !atol_get_clipboard();
}

CSingleInstance::~CSingleInstance()
{
}

bool CSingleInstance::ProgramAlreadyStarted()
{
	return m_bAlreadyExists;
}


