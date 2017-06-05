/* $Id: e2_dialog.c 3003 2014-01-17 23:17:53Z tpgww $

Copyright (C) 2003-2014 tooar <tooar@emelfm2.net>

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
@file src/dialogs/e2_dialog.c
@brief dialog utility functions

This file contains dialog utility functions.
*/
/**
\page dialogs dialogs

ToDo - describe how dialogs work
*/

#include "emelfm2.h"
#include <string.h>
#include "e2_dialog.h"
#include "e2_option.h"
#include "e2_filelist.h"
#ifdef E2_MOUSECUSTOM
# include "e2_mousebinding.h"
#endif
#include "e2_icons.h"

/**
@brief remove dialog's bindings when it's being closed
@param dialog the widget being closed

@return
*/
static void _e2_dialog_close (gpointer dialog)
{
	e2_keybinding_disrol (GTK_WIDGET (dialog), NULL);
#ifdef E2_MOUSECUSTOM
	e2_mousebinding_disrol (GTK_WIDGET (dialog), NULL);
#  ifdef E2_PTRGESTURES
	e2_mousegesture_disrol (GTK_WIDGET (dialog), NULL);
#  endif
#endif
}

  /*********************/
 /***** callbacks *****/
/*********************/

/**
@brief "show" callback for a dialog with scrolled window
Turns on automatic scrollbars for @a scrolled
@param dialog UNUSED the dialog widget containing @a scrolled
@param scrolled scrolled window to be updated
@return
*/
void e2_dialog_show_cb (GtkWidget *dialog, GtkScrolledWindow *scrolled)
{
	NEEDCLOSEBGL
	gtk_scrolled_window_set_policy (scrolled, GTK_POLICY_AUTOMATIC,
		GTK_POLICY_AUTOMATIC);
	NEEDOPENBGL
}
/**
@brief "show" callback for a dialog with notebook
Turns on automatic scrollbars for each page of @a book
@param dialog UNUSED the dialog widget containing @a book
@param book notebook to be updated
@return
*/
void e2_dialog_show_notebook_cb (GtkWidget *dialog, GtkNotebook *book)
{
	gint i, pagecount = gtk_notebook_get_n_pages (book);
	for (i = 0; i < pagecount; i++)
	{
		GtkWidget *page;
		GtkScrolledWindow *sw;
		page = gtk_notebook_get_nth_page (book, i);
		sw = (GtkScrolledWindow *)g_object_get_data (G_OBJECT (page),
			"e2-tab-scrolled-window");
		if (sw != NULL)
			gtk_scrolled_window_set_policy (sw, GTK_POLICY_AUTOMATIC, GTK_POLICY_AUTOMATIC);
	}
}
/**
@brief handle <Enter> keypresses in a line-input dialog
This is convenient because the buttons in the dialog are typically not focused
@param entry the entry widget for the combo box
@param dialog the dialog where the keypress happened
@return
*/
static void _e2_dialog_activated_cb (GtkEntry *entry, GtkWidget *dialog)
{
	//mimic yes or apply button
//	NEEDOPENBGL
	g_signal_emit_by_name (G_OBJECT(dialog), "response", GTK_RESPONSE_YES);
//	NEEDCLOSEBGL
}
/**
@brief default callback to handle responses from @a dialog
This is to handle cancellation requests when there's no dialog-specific
response callback (which excludes cases where there is a new mainloop)
@param dialog the dialog where the response was triggered
@param response the response enumerator
@param data UNUSED data specified when the callback was connected
@return
*/
static void _e2_dialog_response_cb (GtkDialog *dialog, gint response,
	gpointer data)
{
	gint negresponse = GPOINTER_TO_INT (g_object_get_data (G_OBJECT (dialog),
		"e2-negative-response"));
	if (response == negresponse) // || response == GTK_RESPONSE_DELETE_EVENT)
	{
//		NEEDOPENBGL
		e2_dialog_cancel_cb (NULL, GTK_WIDGET (dialog));
//		NEEDCLOSEBGL
	}
}

static void _e2_dialog_wait_response2_cb (GtkDialog *dialog, gint response, E2_Wait *w)
{
	w->choice = response;
	NEEDCLOSEBGL
	e2_main_loop_quit (w->loopdata);
	NEEDOPENBGL
}
/**
@brief blocked-dialog response callback
Optionally converts the actual dialog response to the interface standard
@param dialog UNUSED the dialog where the response was generated
@param response the generated response number
@param w pointer to wait-data struct
@return
*/
static void _e2_dialog_wait_response_cb (GtkDialog *dialog, gint response, E2_Wait *w)
{
	if (w->choice != IGNORE)
		w->choice = e2_dialog_response_decode (response);
	NEEDCLOSEBGL
	e2_main_loop_quit (w->loopdata);
	NEEDOPENBGL
}

  /******************/
 /***** public *****/
/******************/

/**
@brief initiate a mainloop pending the user's choice in @a dialog
This is a replacement for a nested gtk_main(), to allow interruption
@param dialog the dialog where the response will be generated
@param lockednow TRUE if we arrive here with BGL closed
@param maincontext TRUE to create mainloop in default context
@param multi TRUE when this dialog applies to multiple selected items
@param getresult TRUE to decode the original response from @a dialog, FALSE just returns a default value (suitable to cancel any operation)
@return enumerator corresponding to the user's choice
*/
DialogButtons e2_dialog_wait (GtkWidget *dialog,
	gboolean lockednow, gboolean maincontext, gboolean multi, gboolean getresult)
{
	E2_Wait wdata;

	wdata.loopdata = e2_main_loop_new (maincontext);
	if (wdata.loopdata != NULL)
	{
		//setup default result in case of abort
		if (getresult)
			wdata.choice = (multi) ? NO_TO_ALL : CANCEL;
		else
			wdata.choice = IGNORE;
		//replace the default response handler, which fails to clean up after cancellation
		g_signal_handlers_disconnect_matched ((gpointer)dialog,
			G_SIGNAL_MATCH_FUNC, 0, 0, NULL, _e2_dialog_response_cb, NULL);
		g_signal_connect (G_OBJECT (dialog), "response",
			G_CALLBACK (_e2_dialog_wait_response_cb), &wdata);

#if 1
		GET_EVENTS_CONTEXT
#endif

		if (lockednow)
		{
#if 1
			gtk_widget_queue_draw (dialog); //FIXME doesn't always work
			EXTRA_WAIT_FOR_EVENTS
#else
			OPENBGL
			gtk_widget_queue_draw (dialog); //refresh dialog at idle-time
			EXTRA_WAIT_FOR_EVENTS_UNLOCKED
			CLOSEBGL
#endif
//			gdk_flush (); //also in e2_main_loop_run();
			e2_main_loop_run (wdata.loopdata);
		}
		else
		{
			//allow for aborting the process using this loop
			//this is a macro, it can't be conditional by itself
			pthread_cleanup_push ((gpointer)OPENBGL_NAME,
#ifdef DEBUG_MESSAGES
				NULL
#else
				&display_mutex
#endif
			);
#if 1
			CLOSEBGL
			gtk_widget_queue_draw (dialog);
			EXTRA_WAIT_FOR_EVENTS
#else
			gtk_widget_queue_draw (dialog); //refresh view at idle-time
			EXTRA_WAIT_FOR_EVENTS_UNLOCKED
			CLOSEBGL
#endif
//			gdk_flush (); //also in e2_main_loop_run();
			e2_main_loop_run (wdata.loopdata);
			OPENBGL
			pthread_cleanup_pop (0); //another macro
		}

		//prevent fatal multiple-callbacks if this func is repeatedly called
		//on the same dialog e.g. inside a loop
		if (GTK_IS_DIALOG(dialog))
			g_signal_handlers_disconnect_matched (dialog,
				G_SIGNAL_MATCH_FUNC | G_SIGNAL_MATCH_DATA,
				0, 0, NULL, _e2_dialog_wait_response_cb, &wdata);
	}
	else
		wdata.choice = (multi) ? NO_TO_ALL : CANCEL;

	return wdata.choice;
}
/**
@brief convert a dialog response to the interface standard
@param response the generated response number
@return the converted response
*/
DialogButtons e2_dialog_response_decode (gint response)
{
	switch (response)
	{
		case GTK_RESPONSE_OK:
		case GTK_RESPONSE_YES:
		case GTK_RESPONSE_APPLY:
			return OK;
		case GTK_RESPONSE_CANCEL:
		case GTK_RESPONSE_NO:
		case GTK_RESPONSE_CLOSE:
			return CANCEL;
		case E2_RESPONSE_APPLYTOALL:
			return YES_TO_ALL;
		default:
			return NO_TO_ALL;
	}
}
//these callbacks used for 3 filter dialogs & others
/**
@brief close dialog @a dialog
@param widget the activated button, or NULL after a keypress
@param dialog the dialog widget
@return TRUE always
*/
gboolean e2_dialog_cancel_cb (GtkWidget *widget, GtkWidget *dialog)
{
	NEEDCLOSEBGL
	gtk_widget_destroy (dialog);
	gtk_widget_set_sensitive (app.main_window, TRUE);
//	gtk_widget_grab_focus (curr_view->treeview);
	NEEDOPENBGL
	return TRUE;
}
/* *
@brief close dialog @a dialog when Esc key is pressed
@param widget the focused widget when the key was pressed
@param event pointer to event data struct
@param dialog the dialog widget
@return TRUE if Esc key was pressed
*/
/*gboolean e2_dialog_key_press_cb
	(GtkWidget *widget, GdkEventKey *event, GtkWidget *dialog)
{
	if (event->keyval == GDK_Escape)
		return e2_dialog_cancel_cb (NULL, dialog);
	return FALSE;
}
*/
/**
@brief initiate the assigned 'negative response' for dialog @a data or otherwise translated keycode
@a data is unused here but is needed when [un]blocking this callback
@param widget the dialog widget which received the keypress
@param event pointer to event data struct
@param data pointer to dialog widget (= widget)
@return TRUE if Esc key was pressed
*/
gboolean e2_dialog_key_neg_cb (GtkWidget *widget, GdkEventKey *event, gpointer data)
{
//	printd (DEBUG, "e2_dialog_key_neg_cb (widget:_,event:_,data_)");
	if (event->keyval == GDK_Escape)
	{
		gint response = GPOINTER_TO_INT (g_object_get_data (G_OBJECT (widget),
			"e2-negative-response"));
		gtk_dialog_response (GTK_DIALOG (widget), response);
		return TRUE;
	}
	e2_utils_translate_key_event (event);
	return FALSE;
}

/* BAD use what is currently focused, not always the positive response
gboolean e2_dialog_key_pos_cb (GtkWidget *widget, GdkEventKey *event, gpointer data)
{
	printd (DEBUG, "e2_dialog_key_pos_cb (widget:_,event:_,data_)");
	if (event->keyval == GDK_Return
	 || event->keyval == GDK_KP_Enter
	 || event->keyval == GDK_ISO_Enter
	 || event->keyval == GDK_3270_Enter)
	{
		gint response = GPOINTER_TO_INT (g_object_get_data (G_OBJECT (data),
				"e2-positive-response"));
		gtk_dialog_response (GTK_DIALOG (data), response);
		return TRUE;
	}
	e2_utils_translate_key_event (event); again ? see neg
	return FALSE;
}
*/
/**
@brief setup various dialog window parameters
Expects BGL closed.
@param dialog the widget to be processed
@param parent the parent dialog widget of @a dialog
@return
*/
void e2_dialog_setup (GtkWidget *dialog, GtkWidget *parent)
{
	//some funcs here go down to xlib etc CHECKME problem if re-entrant ?
	GtkWindow *thiswindow = GTK_WINDOW (dialog);
	if (parent != NULL)
		gtk_window_set_transient_for (thiswindow, GTK_WINDOW (parent));
	gtk_window_set_destroy_with_parent (thiswindow, TRUE);
	gtk_window_set_role (thiswindow, "dialog");
	gtk_window_set_position (thiswindow, e2_option_int_get ("dialog-position"));
	gtk_window_set_resizable (thiswindow, TRUE);
}
/**
@brief complete setup of, then run, @a dialog
@param dialog the widget to be processed
@param parent the parent dialog widget of @a dialog, or NULL
@param flags bitflags indicating how the dialog is to be processed
@param button start of NULL-terminated sequence of button data-structs
@return the value returned from the dialog
*/
DialogButtons e2_dialog_show (GtkWidget *dialog, GtkWidget *parent,
	E2_DialogFlags flags, E2_Button *button, ...)
{
//	GtkWidget *lastbutton;  //just in case we want a pointer to the last 1
	//some of the calls here have no buttons
	//but we get warned if the flags button is made the last computsory one
	if (button != NULL)
	{
		va_list args;
		va_start (args, button);
		while (button != NULL)
		{
			//lastbutton =
			e2_dialog_add_defined_button (dialog, button);
			button = va_arg (args, E2_Button *);
		}
		va_end (args);
	}
	if (flags & E2_DIALOG_CLOSELOCK) //or else BGL is assumed already closed
		CLOSEBGL
	e2_dialog_setup (dialog, parent);
	if (flags & E2_DIALOG_CLOSELOCK)
		OPENBGL
	return e2_dialog_run (dialog, parent, flags);
}
/**
@brief run dialog
This supports out-of-loop dialog single-setup
If modal, it's intended to be used for filelist-related dialogs, as the active-
pane filelist is focused when the run is completed
@param dialog the widget to be processed
@param parent (for modal dialogs) the parent dialog widget of @a dialog that's to be desensitized, or NULL
@param flags bitflags indicating how the dialog is to be run
@return the value returned from the dialog, 0 if the dialog is not run
*/
DialogButtons e2_dialog_run (GtkWidget *dialog, GtkWidget *parent, E2_DialogFlags flags)
{
//FIXME if single-setup used, need to re-select default button */
	DialogButtons ret = IGNORE;

	gboolean lock = flags & E2_DIALOG_CLOSELOCK;

	if (lock) //or else BGL is assumed already closed
		CLOSEBGL

	if (flags & E2_DIALOG_DONT_SHOW_ALL)
		gtk_widget_show (dialog);	//this duplicates effect in gtk_dialog_run()
	else
		gtk_widget_show_all (dialog);

	if (flags & E2_DIALOG_BLOCKED)
	{
		gtk_window_present (GTK_WINDOW (dialog));	//sometimes, dialog is not focused
		printd (DEBUG, "start local wait for %s", (lock) ? "other context" : "main context" );
		ret = e2_dialog_wait (dialog, TRUE, !lock, flags & E2_DIALOG_MULTI, TRUE);  //CHECKME TRUE maincontext
		if (flags & E2_DIALOG_FREE)
		{
			if (GTK_IS_WIDGET (dialog))
				gtk_widget_destroy (dialog);
			gtk_widget_grab_focus (curr_view->treeview);
		}
	}
	else if (flags & E2_DIALOG_MODAL)
	{
		gtk_window_set_modal (GTK_WINDOW (dialog), TRUE);
		if (parent != NULL)
			gtk_widget_set_sensitive (parent, FALSE);
		ret = e2_dialog_wait (dialog, TRUE, !lock, flags & E2_DIALOG_MULTI, TRUE);  //CHECKME TRUE maincontext
		if (parent != NULL)
			gtk_widget_set_sensitive (parent, TRUE);
		//ret could be GTK_RESPONSE_NONE or GTK_RESPONSE_DELETE_EVENT !!
		if (flags & E2_DIALOG_FREE)
		{
			if (GTK_IS_WIDGET (dialog))
				gtk_widget_destroy (dialog);
			gtk_widget_grab_focus (curr_view->treeview);
		}
	}

	if (lock)
		OPENBGL

	return ret;
}

/**
@brief replacement for gtk_dialog_run using a local mainloop
Assumes BGL is closed
@return the response from the dialog
*/
gint e2_dialog_run_simple (GtkWidget *dialog, GtkWidget *parent)
{
	e2_dialog_setup (dialog, parent);
	gtk_window_set_modal (GTK_WINDOW (dialog), TRUE);
//	gtk_widget_show (dialog);	//this duplicates effect in gtk_dialog_run()
	gtk_window_present (GTK_WINDOW (dialog));	//sometimes, dialog is not focused

	E2_Wait wdata;
	//setup default result in case of abort
	wdata.choice = IGNORE;

	wdata.loopdata = e2_main_loop_new (TRUE);
	if (wdata.loopdata != NULL)
	{
		g_signal_connect (G_OBJECT (dialog), "response",
			G_CALLBACK (_e2_dialog_wait_response2_cb), &wdata);
		e2_main_loop_run (wdata.loopdata);
	}
	return (gint) wdata.choice;
}
/**
@brief create a generic dialog
If @a callback is NULL, the caller must locally handle "delete-event" signals
if anything special is needed for that.
If @a label contains any text which looks like "bad" pango-markup, then the
label must have been escaped before calling here.
Expects BGL closed.
@param stock name of gtk-stock-icon, or absolute localised path of icon file, to be shown near the top of the dialog, or NULL
@param label_text text to be shown near the top of the dialog, or NULL
@param title title string for the dialog, or NULL for PROGNAME
@param callback response function for the dialog,
  or DEFAULT_RESPONSE_CB (to apply _e2_dialog_response_cb)
  or DUMMY_RESPONSE_CB (to apply no response cb at all)
@param data pointer to data to be provided to @a callback

@return the dialog widget
*/
GtkWidget *e2_dialog_create (const gchar *stock, const gchar *label_text,
	const gchar *title, void (*callback) (GtkDialog*,gint,gpointer), gpointer data)
{
	GtkWidget *dialog = gtk_dialog_new ();
	e2_window_set_title (dialog, title);
//	gtk_window_set_wmclass (GTK_WINDOW (dialog),
//		gtk_window_get_title (GTK_WINDOW (dialog)), PROGNAME);
	//fullscreening only works with NORMAL hint
	gtk_window_set_type_hint (GTK_WINDOW (dialog), GDK_WINDOW_TYPE_HINT_NORMAL);
//	gint eflags = gtk_widget_get_events (dialog); 0
//	printd (DEBUG, "dialog event flags %x", eflags);
	gtk_widget_set_events (dialog,
#if 0
		GDK_ALL_EVENTS_MASK //CHECKME getting everything slows UI ?
#else
	    GDK_EXPOSURE_MASK
//	  | GDK_POINTER_MOTION_MASK
	  | GDK_POINTER_MOTION_HINT_MASK
//	  | GDK_BUTTON_MOTION_MASK
//	  | GDK_BUTTON1_MOTION_MASK
//	  | GDK_BUTTON2_MOTION_MASK
//	  | GDK_BUTTON3_MOTION_MASK
	  | GDK_BUTTON_PRESS_MASK
	  | GDK_BUTTON_RELEASE_MASK
	  | GDK_KEY_PRESS_MASK
	  | GDK_KEY_RELEASE_MASK
//	  | GDK_ENTER_NOTIFY_MASK
//	  | GDK_LEAVE_NOTIFY_MASK
//	  | GDK_FOCUS_CHANGE_MASK
	  | GDK_STRUCTURE_MASK
//	  | GDK_PROPERTY_CHANGE_MASK
//	  | GDK_VISIBILITY_NOTIFY_MASK
//	  | GDK_PROXIMITY_IN_MASK
//	  | GDK_PROXIMITY_OUT_MASK
//	  | GDK_SUBSTRUCTURE_MASK
	  | GDK_SCROLL_MASK
#endif
	);
	GtkWidget *img = (stock != NULL ) ?
		e2_widget_get_icon (stock, GTK_ICON_SIZE_DIALOG) : NULL;

	if (img != NULL || label_text != NULL)
	{
#ifdef USE_GTK3_0
		GtkWidget *hbox = gtk_box_new (GTK_ORIENTATION_HORIZONTAL, E2_PADDING);
#else
		GtkWidget *hbox = gtk_hbox_new (FALSE, E2_PADDING);
#endif
		gtk_container_set_border_width (GTK_CONTAINER (hbox), E2_PADDING);
		g_object_set_data (G_OBJECT (dialog), "e2-dialog-hbox", hbox);
		if (img != NULL)
		{
//			gtk_misc_set_alignment (GTK_MISC (img), 0.5, 0.0);	//no effect
//			gtk_box_pack_start (GTK_BOX (hbox), img, FALSE, FALSE, E2_PADDING);
			gint isize = e2_icons_get_pixsize (GTK_ICON_SIZE_DIALOG);
			GtkWidget *align = gtk_alignment_new (1.0, 0.0, 0.0, 0.0);
			gtk_widget_set_size_request (align, isize*1.2, isize);
			gtk_container_add (GTK_CONTAINER (align), img);
			gtk_box_pack_start (GTK_BOX (hbox), align, FALSE, FALSE, 0);
		}
		if (label_text != NULL)
		{
			GtkWidget *label = e2_widget_add_mid_label (hbox, label_text, 0.5, TRUE,
				(img != NULL) ? 2 : E2_PADDING);
			g_object_set_data (G_OBJECT (dialog), "e2-dialog-label", label);
		}
		gtk_box_pack_start (GTK_BOX (
#ifdef USE_GTK2_14
			gtk_dialog_get_content_area (GTK_DIALOG (dialog))
#else
			GTK_DIALOG (dialog)->vbox
#endif
			), hbox, FALSE, FALSE, 0);
	}

#ifdef E2_COMPOSIT
	//dialog not yet REALIZED, so no downstream call to xlib from this
	e2_window_set_opacity (dialog, DIALOG_OPACITY_LEVEL);	//constant opacity for dialogs
#endif

	//the dialog may not have a CANCEL button, but the response will be decoded anyway
	e2_dialog_set_negative_response (dialog, GTK_RESPONSE_CANCEL);

	if (callback != DEFAULT_RESPONSE_CB)
	{
		if (callback != DUMMY_RESPONSE_CB)
			g_signal_connect (G_OBJECT (dialog), "response",
				G_CALLBACK (callback), data);
	}
	else
		g_signal_connect (G_OBJECT (dialog), "response",
			G_CALLBACK (_e2_dialog_response_cb), NULL);

	//don't need a "delete-event" callback, that event generates a reponse GTK_RESPONSE_DELETE_EVENT

	//highest priority - arrange for key-translations from locale
#ifdef USE_GTK3_0
	g_signal_connect (G_OBJECT (dialog), "key-press-event",
		G_CALLBACK (e2_window_key_cb), GINT_TO_POINTER(1));	//includes translation, non-NULL data to prevent disconnection by bindings
	g_signal_connect (G_OBJECT (dialog), "key-release-event",
		G_CALLBACK (e2_window_key_cb), NULL);
#else
	g_signal_connect (G_OBJECT (dialog), "key-press-event",
		G_CALLBACK (e2_utils_key_translate_cb), GINT_TO_POINTER(1));
	g_signal_connect (G_OBJECT (dialog), "key-release-event",
		G_CALLBACK (e2_utils_key_translate_cb), NULL);
#endif
	gchar *category = g_strconcat (_C(17), ".", _C(11), NULL);	//general.dialogs
	e2_keybinding_enrol (dialog, category, (void(*)(E2_OptionSet*))NULL);
#ifdef E2_MOUSECUSTOM
	e2_mousebinding_enrol (dialog, category, (void(*)(E2_OptionSet*))NULL);
# ifdef E2_PTRGESTURES
	e2_mousegesture_enrol (dialog, category, (void(*)(E2_OptionSet*))NULL);
# endif
#endif
	g_free (category);
	//arrange to remove the bindings when dialog is closed
	g_object_set_data_full (G_OBJECT (dialog), "e2-clean-dialog", dialog,
		(GDestroyNotify)_e2_dialog_close);
	//_after_ the bindings connection (tho' that may be in idle callback), support keypress-cancellation
	g_signal_connect (G_OBJECT (dialog), "key-press-event",
		G_CALLBACK (e2_dialog_key_neg_cb), dialog);

	return dialog;
}
/**
@brief create a vbox with scrolled window, in @a dialog
@param dialog the widget to hold the box
@return the vbox widget
*/
GtkWidget *e2_dialog_add_sw (GtkWidget *dialog)
{
	//start without scrollbars, so that the dialog is first displayed with the
	//size of the things packed into the sw
	GtkWidget *scrolled = e2_widget_add_sw (
#ifdef USE_GTK2_14
		gtk_dialog_get_content_area (GTK_DIALOG (dialog)),
#else
		GTK_DIALOG (dialog)->vbox,
#endif
		GTK_POLICY_NEVER, GTK_POLICY_NEVER, TRUE, 0);
	//arrange for automatic scrollbars when the dialog is shown, after sizing
	//and after any local "show" callback processing
	g_signal_connect_after (G_OBJECT (GTK_DIALOG (dialog)), "show",
		G_CALLBACK (e2_dialog_show_cb), scrolled);
	GtkWidget *vbox = e2_widget_get_box (TRUE, FALSE, 0);
#ifdef USE_GTK3_8
	gtk_container_add (GTK_CONTAINER (scrolled), vbox);
#else
	gtk_scrolled_window_add_with_viewport (GTK_SCROLLED_WINDOW (scrolled), vbox);
#endif
	gtk_viewport_set_shadow_type (
#ifdef USE_GTK2_14
		GTK_VIEWPORT (gtk_bin_get_child (GTK_BIN (scrolled))),
#else
		GTK_VIEWPORT (GTK_BIN (scrolled)->child),
#endif
		GTK_SHADOW_NONE);
	GtkWidget *hbox = g_object_get_data (G_OBJECT (dialog), "e2-dialog-hbox");
	if (hbox != NULL)
	{
		gtk_widget_reparent (hbox, vbox);
		gtk_box_set_child_packing (GTK_BOX (vbox), hbox, FALSE, FALSE, 0,
			GTK_PACK_START);
	}
	return vbox;
}
/**
@brief setup to show/hide dialog button icon
This is a helper for the various add-button functions
@param stock string with name of icon that was requested
@return stock or NULL if no icon is to be shown
*/
static const gchar *_e2_dialog_button_image_check (const gchar *stock)
{
	gint choice = e2_option_sel_get ("dialog-button-icons");
	if (choice == 0)	//theme
	{
#ifdef USE_GTK2_18
		gboolean show;
		GtkSettings* defs = gtk_settings_get_default ();
		g_object_get (G_OBJECT (defs), "gtk-button-images", &show, NULL);
		if (show)
#endif
			choice = 1;
	}
	return ((choice == 1) ? stock : NULL);
}
/**
@brief pack a pre-defined button at the end of the 'action-area' of @a dialog
Any custom tooltip will be cleared (and maybe freed) after button is added
@param dialog the widget to hold the button
@param button_data ptr to data struct holding dialog-button parameters
@return the (unshown) new button widget
*/
GtkWidget *e2_dialog_add_defined_button (GtkWidget *dialog, E2_Button *button_data)
{
	gchar *tip = (button_data->showflags & E2_BTN_TIPPED) ? button_data->tip : NULL;
	GtkWidget *button = e2_button_get (button_data->label,
		_e2_dialog_button_image_check (button_data->name), tip, NULL, NULL);
	gtk_dialog_add_action_widget (GTK_DIALOG (dialog), button, button_data->response);
	if (button_data->showflags & E2_BTN_DEFAULT)
		gtk_dialog_set_default_response (GTK_DIALOG (dialog), button_data->response);
	else if (button_data->showflags & E2_BTN_GREY)
		gtk_widget_set_sensitive (button, FALSE);
	//revert to default button parameters, ready for next usage
	button_data->showflags = button_data->default_flags;
	return button;
}
/**
@brief pack a simple custom button at the end of the 'action-area' of @a dialog
@param dialog the widget to hold the button
@param stock string with icon (file)name, or NULL
@param label string with button label
@param response the dialog response to be assigned to the button
@return the created button widget
*/
GtkWidget *e2_dialog_add_simple_button
	(GtkWidget *dialog, const gchar *stock, const gchar *label, gint response)
{
	GtkWidget *button = e2_button_get_full (label,
		_e2_dialog_button_image_check (stock), GTK_ICON_SIZE_BUTTON,
		NULL, NULL, NULL, E2_BUTTON_CAN_DEFAULT | E2_BUTTON_CAN_FOCUS);
	gtk_dialog_add_action_widget (GTK_DIALOG (dialog), button, response);
	return button;
}
/**
@brief pack a custom button at the end of the 'action-area' of @a dialog
@param dialog the widget to hold the button
@param button pointer to populated button data struct
@param is_default TRUE if the created button will be the default
@param tip	string with button tooltip, if non-NULL it prevails over any tip in @a button
@param callback callback func when button is clicked
@param data ptr to data supplied to @a callback
@return the created button widget
*/
GtkWidget *e2_dialog_add_custom_button
	(GtkWidget *dialog, E2_Button *button, gboolean is_default,
		const gchar *tip, void *callback, void *data)
{
	if (tip == NULL)
		tip = button->tip;
	return e2_dialog_add_custom_button_full
		(dialog, is_default, button->response, button->label,
		_e2_dialog_button_image_check (button->name), tip,
		callback, data);
}
/**
@brief add a custom 'unstructured' button to the 'action-area' of @a dialog
@param dialog the widget to hold the button
@param is_default TRUE if the created button will be the default
@param response the dialog response to be assigned to the button
@param label string with button label
@param stock string with icon name, or NULL
@param tip	string with button tooltip
@param callback callback func for button's "clicked" signal, or NULL
@param data ptr to data supplied to @a callback
@return the created button widget
*/
GtkWidget *e2_dialog_add_custom_button_full
	( GtkWidget *dialog, gboolean is_default, guint response,
	  const gchar *label, const gchar *stock, const gchar *tip,
	  void (*callback)(/*GtkButton*,gpointer*/), gpointer data )
{
	E2_ButtonFlags myflags = E2_BUTTON_CAN_FOCUS;
	if (is_default)
	{
		myflags |= E2_BUTTON_CAN_DEFAULT;
		gtk_dialog_set_default_response (GTK_DIALOG (dialog), response);
	}
	GtkWidget *button = e2_button_get_full (label,
		_e2_dialog_button_image_check (stock),
		GTK_ICON_SIZE_BUTTON, tip, callback, data, myflags);
	gtk_dialog_add_action_widget (GTK_DIALOG (dialog), button, response);
	return button;
}
/**
@brief pack a (gtk) toggle button at the end of the 'action-area' of @a dialog
@param dialog the widget to hold the button
@param value T/F the initial setting of the toggle
@param label string with button label
@param tip string with button tooltip, or NULL
@param response the dialog response to be assigned to the button
@return the created button widget
*/
GtkWidget *e2_dialog_add_toggle_button
	(GtkWidget *dialog, gboolean value, gchar *label, gchar *tip, gint response)
{
	GtkWidget *button = e2_button_get_toggle (FALSE, value, label, tip, NULL, NULL);
	gtk_dialog_add_action_widget (GTK_DIALOG (dialog), button, response);
	return button;
}
/**
@brief pack a check button at the end of the 'action-area' of @a dialog
@param dialog the widget to hold the button
@param value T/F the initial setting of the toggle
@param label string with button label
@param tip string with button tooltip, or NULL
@param response the dialog response to be assigned to the button
@return the created button widget
*/
GtkWidget *e2_dialog_add_check_button
	(GtkWidget *dialog, gboolean value, gchar *label, gchar *tip, gint response)
{
	GtkWidget *button = e2_button_get_toggle (TRUE, value, label, tip, NULL, NULL);
	gtk_dialog_add_action_widget (GTK_DIALOG (dialog), button, response);
	return button;
}
/* UNUSED
void e2_dialog_set_positive_response (GtkWidget *dialog, gint response)
{
	g_object_set_data (G_OBJECT (dialog), "e2-positive-response",
		GINT_TO_POINTER (response));
//	gtk_dialog_set_default_response (GTK_DIALOG (dialog), response);
} */
/**
@brief assign the 'negative' response for @a dialog
@param dialog the dialog widget to be processed
@param response the number of the "negative" response
@return
*/
void e2_dialog_set_negative_response (GtkWidget *dialog, gint response)
{
	g_object_set_data (G_OBJECT (dialog), "e2-negative-response",
		GINT_TO_POINTER (response));
}

/* UNUSED as we only use the -ve response, now
void e2_dialog_set_responses (GtkWidget *dialog, gint pos, gint neg)
{
	e2_dialog_set_positive_response (dialog, pos);
	e2_dialog_set_negative_response (dialog, neg);
} */
/**
@brief proportionally change size of @a dialog
@param dialog the dialog widget to be resized
@param factor multiple to apply to width and height of @a dialog
@return
*/
void e2_dialog_resize (GtkWidget *dialog, gfloat factor)
{
	gint width, height;
	gtk_window_get_size (GTK_WINDOW (dialog), &width, &height);
	width *= factor;
	height *= factor;
	gtk_window_resize (GTK_WINDOW (dialog), width, height);
}
/**
@brief set dialog window cursor to @a type
Expects BGL on/active
@param dialog the affected dialog widget
@param type enumerator of desired cursor type

@return
*/
void e2_dialog_set_cursor (GtkWidget *dialog, GdkCursorType type)
{
	GdkCursor *cursor = gdk_cursor_new (type);
	gdk_window_set_cursor (
#ifdef USE_GTK2_14
		gtk_widget_get_window (dialog),
#else
		dialog->window,
#endif
		cursor);
#ifdef USE_GTK3_0
	g_object_unref (G_OBJECT (cursor));
#else
	gdk_cursor_unref (cursor);
#endif
	gdk_flush ();
}

/**
@brief "clicked" signal callback for file-chooser dialog toggle-hidden button

@param button the clicked button widget
@param chooser the chooser interface for the dialog

@return
*/
static void _e2_dialog_toggglehidden_cb (GtkButton *button, GtkFileChooser *chooser)
{
	NEEDCLOSEBGL
	//toggle hidden-items display & button-image
	gboolean nowhidden = gtk_file_chooser_get_show_hidden (chooser);
	gtk_file_chooser_set_show_hidden (chooser, ! nowhidden);
	e2_button_set_image (GTK_WIDGET (button), (nowhidden) ?
		"hidden_show"E2ICONTB : "hidden_noshow"E2ICONTB);
	NEEDOPENBGL
}
/**
@brief setup a file-chooser dialog
The actual dialog supplied, not created here, in the absence of passthrough of
variadic args for button-icons and -signals
Expects BGL closed, probably
@param dialog the newly-created file-chooser-dialog
@param title title string for the dialog, or NULL
@param startpath initial item to select in the dialog (absolute utf8 path), or NULL
@param action enumerator of type of chooser dialog
@param showhidden TRUE to start with hidden items displayed
@param multi TRUE if selection of multiple items is allowed
@param defresponse the default response for the dialog
@param first_button stock-item name of first button to include
@param varargs NULL-terminated series of args: response, stockname, response ....

@return the filechooser widget that's part of the dialog
*/
void e2_dialog_setup_chooser (GtkWidget *dialog, const gchar *title,
	const gchar *startpath, GtkFileChooserAction action, gboolean showhidden,
	gboolean multi, gint defresponse, const gchar *first_button, ...)
{
	va_list btnargs;
	const gchar *name;
	gchar *local, *s;
	GtkFileChooser *chooser = GTK_FILE_CHOOSER (dialog);

	e2_dialog_add_custom_button_full
		(dialog, FALSE, E2_RESPONSE_USER1, _("_Hidden"),
		(showhidden) ? "hidden_noshow"E2ICONTB : "hidden_show"E2ICONTB,
		_("Toggle display of hidden items"),
		_e2_dialog_toggglehidden_cb, chooser);

	if (first_button != NULL)
	{
		name = first_button;
		va_start (btnargs, first_button);
		while (name)
		{
			GtkWidget *btn;
			const gchar *label = e2_icons_get_stock_label (name);
			gint response = va_arg (btnargs, gint);
			if (label != NULL)
				btn = e2_dialog_add_simple_button (dialog, name, label, response);
			else
			{
				//const gchar *stock = STOCK_NAME_MISSING_IMAGE; TODO
				if (strncmp (name, "gtk-", 4) == 0)
					name += 4;
				btn = e2_dialog_add_simple_button (dialog, /*stock*/NULL, name, response);
			}
			gtk_widget_show_all (btn);
			name = va_arg (btnargs, const gchar *);
		}
		va_end (btnargs);

		gtk_dialog_set_default_response (GTK_DIALOG (dialog), defresponse);
	}
	//the dialog may not have a CANCEL button, but the response will be decoded anyway
	e2_dialog_set_negative_response (dialog, GTK_RESPONSE_CANCEL);

	e2_window_set_title (dialog, title);
#ifdef E2_COMPOSIT
	e2_window_set_opacity (dialog, DIALOG_OPACITY_LEVEL);	//constant opacity for dialogs
#endif

#ifdef USE_GTK2_8
	if ((action == GTK_FILE_CHOOSER_ACTION_SAVE
		|| action == GTK_FILE_CHOOSER_ACTION_CREATE_FOLDER
		|| action == GTK_FILE_CHOOSER_ACTION_OPEN)
		&& e2_option_bool_get ("confirm-overwrite"))
			gtk_file_chooser_set_do_overwrite_confirmation (chooser, TRUE);
#endif

	gtk_file_chooser_set_select_multiple (chooser, multi);
	if (showhidden)
		gtk_file_chooser_set_show_hidden (chooser, TRUE);

	if (startpath != NULL)
	{
		local = D_FILENAME_TO_LOCALE (startpath);
		s = local + strlen (local) - 1;
		if (*s == G_DIR_SEPARATOR)
		{	//no file named, need to trick selector into opening correct dir
			s = local;
			local = e2_utils_strcat (local, " ");
			g_free (s);
		}
		gtk_file_chooser_set_filename (chooser, local);
		g_free (local);
	}
	else
	{
#ifdef E2_VFSTMP
		FIXME
#else
		local = D_FILENAME_TO_LOCALE (curr_view->dir);
		s = local + strlen (local) - sizeof(gchar);
		if (s > local && *s == G_DIR_SEPARATOR)
			*s = '\0';
	    gtk_file_chooser_set_current_folder (chooser, local);
		g_free (local);
#endif
//    	gtk_file_chooser_set_current_name (chooser,
//			"Untitled document");	//this must be utf8
	}

//FIXME change size: here or after mapping or showing does not work
//	gint window_width = MIN(400, app.window.panes_paned->allocation.width*2/3);
//		gtk_window_resize (GTK_WINDOW (dialog), window_width, -1);
//	gint width, height;
//	gtk_window_get_size (GTK_WINDOW (dialog), &width, &height);
//	width = MIN(width, app.window.panes_paned->allocation.width*2/3);
//	gtk_window_resize (GTK_WINDOW (dialog), width*2, height*6);
//	gtk_window_resize (GTK_WINDOW (dialog), width, -1);
//	g_signal_connect (G_OBJECT (dialog), "map", //"show",
//		G_CALLBACK (_e2_dialog_show_cb), NULL);

	//highest priority - arrange for key-translations from locale
#ifdef USE_GTK3_0
	g_signal_connect (G_OBJECT (dialog), "key-press-event",
		G_CALLBACK (e2_window_key_cb), GINT_TO_POINTER(1));	//includes translation, non-NULL data to prevent disconnection by bindings
	g_signal_connect (G_OBJECT (dialog), "key-release-event",
		G_CALLBACK (e2_window_key_cb), NULL);
#else
	g_signal_connect (G_OBJECT (dialog), "key-press-event",
		G_CALLBACK (e2_utils_key_translate_cb), GINT_TO_POINTER(1));
	g_signal_connect (G_OBJECT (dialog), "key-release-event",
		G_CALLBACK (e2_utils_key_translate_cb), NULL);
#endif
	gchar *category = g_strconcat (_C(17), ".", _C(11), NULL);	//general.dialogs
	e2_keybinding_enrol (dialog, category, (void(*)(E2_OptionSet*))NULL);
#ifdef E2_MOUSECUSTOM
	e2_mousebinding_enrol (dialog, category, (void(*)(E2_OptionSet*))NULL);
# ifdef E2_PTRGESTURES
	e2_mousegesture_enrol (dialog, category, (void(*)(E2_OptionSet*))NULL);
# endif
#endif
	g_free (category);
	//arrange to remove all bindings when dialog is closed
	g_object_set_data_full (G_OBJECT (dialog), "e2-clean-dialog", dialog,
		(GDestroyNotify)_e2_dialog_close);
	//_after_ the bindings, support keypress-cancellation
	g_signal_connect (G_OBJECT (dialog), "key-press-event",
		G_CALLBACK (e2_dialog_key_neg_cb), dialog);
//	e2_dialog_setup (dialog, parent);
}
/**
@brief setup generic "question" dialog for line-input by user
Assumes BGL closed
@param window_title window title string
@param prompt prompt string
@param suggestion suggested answer string
@param extras button-related flags
@param select_text TRUE to select the displayed @a suggestion
@param history TRUE to make the input a combobox with history
@param history_list the history list for a combo entry
@param retflags store for flags to use when running dialog, may be updated here
@param retentry store for entry wigget which will hold the result

@return the dialog
*/
static GtkWidget *_e2_dialog_setup_line (gchar* window_title, gchar *prompt,
	gchar *suggestion, OW_ButtonFlags extras, gboolean select_text, // tag PASSWORDINPUT gboolean hide_text,
	gboolean history, GList **history_list,
	E2_DialogFlags *retflags, GtkWidget **retentry)
{
	GtkWidget *dialogentry, *dialogcombo;

/*	GtkWidget *dialog = get_dialog (app.main_window, GTK_MESSAGE_QUESTION,
#ifdef LATESHOW
	"");
#else
	prompt);
#endif
	gchar *title = (window_title != NULL) ? window_title : _("user input");
	e2_dialog_set_title (dialog, title);
*/
	gchar *title = (window_title != NULL) ? window_title : _("user input");
	GtkWidget *dialog = e2_dialog_create (STOCK_NAME_DIALOG_QUESTION,
#ifdef LATESHOW
		"",
#else
		prompt,
#endif
		title, DUMMY_RESPONSE_CB, NULL);

	GtkWidget *dialog_vbox =
#ifdef USE_GTK2_14
		gtk_dialog_get_content_area (GTK_DIALOG (dialog));
#else
		GTK_DIALOG (dialog)->vbox;
#endif
	if (history)
	{
		//cuz E2_COMBOBOX_MENU_STYLE flag is not set, on gtk2, downstream calls
		//gtk_widget_set_name() which MAY? need BGL closed
		dialogcombo = e2_combobox_add (dialog_vbox, FALSE, E2_PADDING,
			NULL, NULL, history_list,
			E2_COMBOBOX_HAS_ENTRY | E2_COMBOBOX_FOCUS_ON_CHANGE);
		dialogentry =
#ifdef USE_GTK2_14
			gtk_bin_get_child (GTK_BIN (dialogcombo));
#else
			GTK_BIN (dialogcombo)->child;
#endif
		if (suggestion != NULL)
		{
			gtk_entry_set_text (GTK_ENTRY (dialogentry), suggestion);
			//select_text is TRUE
			gtk_editable_select_region (GTK_EDITABLE (dialogentry), 0, -1);
		}
	}
	else
	{
		dialogcombo = NULL; 	//assignment for compiler-warning prevention only
		dialogentry = e2_widget_add_entry (dialog_vbox,
#ifdef LATESHOW
			NULL, FALSE, FALSE);
#else
			suggestion, FALSE, select_text);
#endif
	}
	*retentry = dialogentry;

	OPENBGL

#ifdef E2_ASSISTED
	GtkWidget *label = (GtkWidget *) g_object_get_data (G_OBJECT (dialog),
		"e2-dialog-label");
	e2_widget_set_label_relations (label, dialogentry);
#endif

	//handle Return-key presses when the entry is focused
	g_signal_connect (G_OBJECT (dialogentry), "activate",
//		G_CALLBACK (_e2_dialog_activated_cb), &retval);	//FIXME need a way to close local mainloop
		G_CALLBACK (_e2_dialog_activated_cb), dialog);

/* tag PASSWORDINPUT
	if (hide_text)
		gtk_entry_set_visibility (GTK_ENTRY (dialogentry), FALSE); */

	//these separated, to do them once for re-run dialogs
	E2_Button local_btn;
	if (extras)
	{
		E2_Button all_btn;
		E2_Button stop_btn;
		all_btn = E2_BUTTON_APPLYTOALL;
		stop_btn = E2_BUTTON_CANCEL;
		if (extras & NOALL)
		{
			all_btn.showflags |= E2_BTN_GREY;
			e2_dialog_set_negative_response (dialog, E2_RESPONSE_NOTOALL);
		}
		else if (!(extras & BOTHALL))
		{
			all_btn.showflags |= E2_BTN_GREY;
			stop_btn.showflags |= E2_BTN_GREY;
		}
		e2_dialog_add_defined_button (dialog, &stop_btn);
		e2_dialog_add_defined_button (dialog, &all_btn);
		e2_button_derive (&local_btn, &E2_BUTTON_NO, BTN_NO_SKIP);
		if (!(all_btn.showflags & E2_BTN_GREY))
			*retflags |= E2_DIALOG_MULTI;
	}
	else
		e2_button_derive (&local_btn, &E2_BUTTON_NO, BTN_NO_CANCEL);

	e2_dialog_add_defined_button (dialog, &local_btn);
	e2_button_derive (&local_btn, &E2_BUTTON_APPLY, BTN_YES_CONTINUE);
	e2_dialog_add_defined_button (dialog, &local_btn);

#ifdef LATESHOW
// FIXME figure out how to do all the above once-only (if needed) instead of each dialog re-run
// -------------------------------------------------------------------------------------------
// add prompt to dialog now (eg in re-run)instead of at the start (1st-time)
//	GtkLabel *label = GTK_LABEL (GTK_MESSAGE_DIALOG (dialog)->label);
	GtkLabel *label = GTK_LABEL (GTK_DIALOG (dialog)->label);
	gtk_label_set_text (label, prompt);
	gtk_label_set_use_markup (label, TRUE);
#ifdef E2_ASSISTED
	e2_widget_set_label_relations (GTK_WIDGET(label), dialogentry);
#endif

//  & add default/suggestion now
	if (history)
	{
		//CHECKME set to last-used entry?
//		if (history_list != NULL)
//			e2_combobox_set_active (dialogcombo, 0)
#ifdef GTK3_COMBO_FIX
		gtk_widget_grab_focus (gtk_bin_get_child (GTK_BIN(dialogcombo)));
#endif
	}
	else
	{
		gtk_entry_set_text (GTK_ENTRY (dialogentry), suggestion);
		if (select_text)
			gtk_editable_select_region (GTK_EDITABLE (dialogentry), 0, -1);
	}
#endif

	CLOSEBGL
	return dialog;
}
/**
@brief add framed "not authorised" message to @a box

@param box the box to hold the widgets

@return
*/
void e2_dialog_setup_auth (GtkWidget *box)
{
	GtkWidget *label = gtk_label_new (NULL);
	gchar *s = g_strconcat ("<b>",_("You don't have authority to change anything"), "</b>", NULL);
	gtk_label_set_markup (GTK_LABEL (label), s);
	g_free (s);
	gtk_misc_set_alignment (GTK_MISC (label), 0.5, 0.5);
	gtk_misc_set_padding (GTK_MISC (label), E2_PADDING_LARGE, E2_PADDING_LARGE);

	GtkWidget *frame = gtk_frame_new (NULL);
    gtk_container_add (GTK_CONTAINER (frame), label);

	gtk_box_pack_start (GTK_BOX (box), frame, FALSE, FALSE, 0);
}
/**
@brief add "not authorised" message to dialog
Button sensitivities in @a dialog are also adjusted, so @a dialog must be 'complete'
@param dialog the dialog to update
@param multi TRUE to disable YES_TO_ALL signal too

@return
*/
static void _e2_dialog_setup_auth (GtkWidget *dialog, gboolean multi)
{
#ifdef USE_GTK3_0
	GtkWidget *box = gtk_box_new (GTK_ORIENTATION_HORIZONTAL, 0);
#else
	GtkWidget *box = gtk_hbox_new (FALSE, 0);
#endif
	gtk_container_set_border_width (GTK_CONTAINER (box), E2_PADDING);
	gchar *s = g_strconcat ("<b>",_("You don't have authority to do this"), "</b>", NULL);
	e2_widget_add_mid_label (box, s, 0.5, TRUE, 0);
	g_free (s);
	GtkWidget *vbox =
#ifdef USE_GTK2_14
		gtk_dialog_get_content_area (GTK_DIALOG (dialog));
#else
		GTK_DIALOG (dialog)->vbox;
#endif
	gtk_box_pack_end (GTK_BOX (vbox), box, FALSE, FALSE, 0);
	gtk_box_reorder_child (GTK_BOX(vbox), box, 2);

	gtk_dialog_set_response_sensitive (GTK_DIALOG (dialog), GTK_RESPONSE_YES, FALSE);
	if (multi)
		gtk_dialog_set_response_sensitive (GTK_DIALOG (dialog), E2_RESPONSE_APPLYTOALL, FALSE);
}

  /****************************/
 /***** specific dialogs *****/
/****************************/

/* *
@brief display dialog to get user input with obfuscated text display, for passwords
@param window_title window title string
@param prompt prompt string
@param input pointer to store for the inputted password string
@return button code returned by the dialog
*/
/* tag PASSWORDINPUT
DialogButtons e2_dialog_password_input (gchar* window_title, gchar *prompt,
	gchar **input)
{
	gchar *realprompt = (prompt != NULL) ? prompt : _("Enter password:");
	return _e2_dialog_line_input (window_title, realprompt,
		NULL, FALSE, TRUE, input, 0, FALSE, NULL);
} */
/**
@brief display blocked but non-modal dialog to get user input
Expects BGL closed
@param window_title window title string
@param prompt prompt string
@param suggestion suggested answer string
@param extras button-related flags
@param select_text TRUE to select the displayed @a suggestion
@param input pointer to store for the entered string, will be newly-allocated UTF8
@return button code returned by the dialog
*/
DialogButtons e2_dialog_line_input (gchar* window_title, gchar *prompt,
	gchar *suggestion, OW_ButtonFlags extras, gboolean select_text,
	gchar **input)
{
	//block until user selects something
	E2_DialogFlags flags = E2_DIALOG_BLOCKED;
	GtkWidget *dialogentry;

	GtkWidget *dialog = _e2_dialog_setup_line (window_title, prompt, suggestion,
		extras,	select_text,// tag PASSWORDINPUT FALSE,
		FALSE, NULL, &flags, &dialogentry);
	e2_dialog_setup (dialog, app.main_window);
	DialogButtons retval = e2_dialog_run (dialog, NULL, flags);
	if (retval == OK)
	{
		*input = gtk_editable_get_chars (GTK_EDITABLE (dialogentry), 0, -1 ); //free this after return
		if (**input == '\0')
		{	//dopey user supplied a blank name
			g_free (*input);
			retval = CANCEL;
		}
	}

	gtk_widget_destroy (dialog);

	return retval;
}
/**
@brief display blocked but non-modal dialog at a specific position, to get user input for a file operation
Expects BGL open/off. Filelist refreshing is enabled after the dialog is popped up
@param window_title window title string
@param prompt prompt string
@param suggestion suggested answer string
@param extras button-related flags
@param select_text TRUE to select the displayed @a suggestion
@param permitted TRUE if change is authorised
@param horz store for horizontal window position
@param vert store for vertical window position
@param input pointer to store for the entered string, will be newly-allocated UTF8
@return button code returned by the dialog
*/
DialogButtons e2_dialog_positioned_input (gchar* window_title, gchar *prompt,
	gchar *suggestion, OW_ButtonFlags extras, gboolean select_text, gboolean permitted,
	gint *horz, gint *vert, gchar **input)
{
	E2_DialogFlags flags = 0;
	GtkWidget *dialogentry;

	CLOSEBGL

	GtkWidget *dialog = _e2_dialog_setup_line (window_title, prompt,
		suggestion, extras,	select_text,// tag PASSWORDINPUT FALSE,
		FALSE, NULL, &flags, &dialogentry);

	if (!permitted)
		_e2_dialog_setup_auth (dialog, (flags & E2_DIALOG_MULTI));

	e2_dialog_setup (dialog, app.main_window);
	gtk_widget_show_all (dialog);
	if (*horz >= 0 && *vert >= 0)
		gtk_window_move (GTK_WINDOW (dialog), *horz, *vert);
	//refreshing is always temporarily enabled downstream, while waiting
	//block until the user selects
	//CHECKME more reliable if assume default maincontext ?
	DialogButtons retval = e2_dialog_wait (dialog, TRUE, TRUE, (flags & E2_DIALOG_MULTI), TRUE);

	if (retval == OK)
	{
		*input = gtk_editable_get_chars (GTK_EDITABLE (dialogentry), 0, -1 ); //free this after return
		if (**input == '\0')
		{	//dopey user supplied a blank name
			g_free (*input);
			retval = CANCEL;
		}
	}

	if (GTK_IS_DIALOG (dialog)) //not explicitly closed by the user
	{
		gtk_window_get_position (GTK_WINDOW (dialog), horz, vert); //FIXME position if closed
		gtk_widget_destroy (dialog);
	}
	OPENBGL

	return retval;
}
/**
@brief display blocked but non-modal dialog to get user input, via a combobox
Expects BGL closed
@param window_title window title string
@param prompt prompt string
@param suggestion string to show in combobox entry, or NULL
@param extras button-related flags
@param history_list the history list for a combo entry
@param input pointer to store for the entered string, newly-allocated UTF8
@return button code returned by the dialog
*/
DialogButtons e2_dialog_combo_input (gchar* window_title, gchar *prompt,
	gchar *suggestion, OW_ButtonFlags extras, GList **history_list,
	gchar **input)
{
	//block until user selects something
	E2_DialogFlags flags = E2_DIALOG_BLOCKED;
	GtkWidget *dialogentry;

	GtkWidget *dialog = _e2_dialog_setup_line (window_title, prompt, suggestion,
		extras,	TRUE, // tag PASSWORDINPUT FALSE,
		TRUE, history_list, &flags, &dialogentry);

	e2_dialog_setup (dialog, app.main_window);

	DialogButtons retval = e2_dialog_run (dialog, NULL, flags);
	if (retval == OK)
	{
		*input = gtk_editable_get_chars (GTK_EDITABLE (dialogentry), 0, -1 ); //free this after return
		if (**input == '\0')
		{	//dopey user supplied a blank name
			g_free (*input);
			retval = CANCEL;
		}
		else
			e2_list_update_history (history_list, *input, NULL, 10, FALSE);
	}

	gtk_widget_destroy (dialog);

	return retval;
}
/**
@brief create and run dialog for delete confirmation
BGL is assumed open/off on arrival
@param localpath virtual path with delete-item path as localised string
@param multi TRUE if this is part of a multi-item deletion
@param permitted TRUE if this deletion is authorised
@param horz store for dialog horizontal position
@param vert store for dialog vertical position

@return button code corresponding to user's choice
*/
DialogButtons e2_dialog_delete_check (VPATH *localpath, gboolean multi,
	gboolean permitted,	gint *horz, gint *vert)
{
	gchar *s1, *s2, *s3;

	s1 = g_path_get_basename (VPSTR(localpath));
	s2 = F_DISPLAYNAME_FROM_LOCALE (s1);
	s3 = g_markup_escape_text (s2, -1);
	g_free (s1);
	F_FREE (s2, s1);
	s1 = g_strdup_printf(_("Are you sure you want to delete <b>%s</b>?"), s3);
	g_free (s3);
	CLOSEBGL
	GtkWidget *dialog = e2_dialog_create (STOCK_NAME_DIALOG_QUESTION, s1,
		_("confirm"), DUMMY_RESPONSE_CB, NULL);
	OPENBGL
	g_free (s1);

	E2_DialogFlags flags = 0;

	E2_Button stop_btn;
	stop_btn = E2_BUTTON_CANCEL;
	E2_Button all_btn;
	all_btn = E2_BUTTON_APPLYTOALL;
	all_btn.name = STOCK_NAME_DELETE;

	if (multi)
	{
		e2_dialog_set_negative_response (dialog, E2_RESPONSE_NOTOALL);
		all_btn.tip = _("Delete items without further confirmation");
		all_btn.showflags |= E2_BTN_TIPPED;
		flags |= E2_DIALOG_MULTI;
	}
	else
	{
		if (permitted)
		{
			e2_dialog_set_negative_response (dialog, GTK_RESPONSE_NO);
			stop_btn.showflags |= E2_BTN_GREY;
		}
		else //cancel-button will abort a single-non-permitted deletion
			e2_dialog_set_negative_response (dialog, E2_RESPONSE_NOTOALL);
		all_btn.showflags |= E2_BTN_GREY;
	}

	e2_dialog_add_defined_button (dialog, &stop_btn);
	e2_dialog_add_defined_button (dialog, &all_btn);

	e2_button_derive (&stop_btn, &E2_BUTTON_NO, BTN_NO_KEEP);
	if (multi || permitted)
	{
		stop_btn.tip = _("Retain this item");
		stop_btn.showflags |= (E2_BTN_DEFAULT | E2_BTN_TIPPED);
	}
	else
		stop_btn.showflags |= E2_BTN_GREY;
	e2_dialog_add_defined_button (dialog, &stop_btn);

	e2_button_derive (&stop_btn, &E2_BUTTON_YES, BTN_YES_DELETE);
	stop_btn.name = STOCK_NAME_DELETE;
	stop_btn.showflags &= ~E2_BTN_DEFAULT;
	e2_dialog_add_defined_button (dialog, &stop_btn);

	if (!permitted)
		_e2_dialog_setup_auth (dialog, multi);

//tag DEBUGfreeze2;
	//maybe this helps gtk 2.16 crashes ?
//	OPENBGL

	CLOSEBGL
	e2_dialog_setup (dialog, app.main_window);
	e2_dialog_run (dialog, NULL, flags);
	if (*horz >= 0 && *vert >= 0)
		gtk_window_move (GTK_WINDOW (dialog), *horz, *vert);

	DialogButtons choice = e2_dialog_wait (dialog, TRUE, FALSE, multi, TRUE);

	if (GTK_IS_DIALOG (dialog)) //not explicitly closed by the user
	{
		gtk_window_get_position (GTK_WINDOW (dialog), horz, vert);
		gtk_widget_destroy (dialog);
	}
	OPENBGL

	return choice;
}
/**
@brief dialog for overwrite check
Assumes that @a dlocal (at least) exists, expects BGL open/off.
@param slocal virtual path with localised string for source-item, or NULL for no date-check
@param dlocal virtual path with localised string for destination-item
@param extras flags for which buttons to show
@return button code returned by the dialog
*/
DialogButtons e2_dialog_ow_check (VPATH *slocal, VPATH *dlocal, OW_ButtonFlags extras)
{
//	printd (DEBUG, "overwrite check");
	struct stat sb;

//tag gboolean DEBUGfreeze;
	//maybe this helps with gtk 2.16 flakiness
//	CLOSEBGL

	GString *dialog_prompt = g_string_sized_new (NAME_MAX + 64);	//pick a reasonable initial size
	gchar *utf = F_DISPLAYNAME_FROM_LOCALE (VPSTR(dlocal));
	//before applying bold markup, escape any pango-annoying component of the item name
	gchar *public = g_markup_escape_text (utf, -1);
	gchar *type = NULL;

	if (slocal != NULL)
	{
//		E2_ERR_DECLARE
		if (!e2_fs_stat (slocal, &sb E2_ERR_NONE()))	//should never fail
		{
			gboolean sdir = S_ISDIR (sb.st_mode);
			time_t stime = sb.st_mtime;
			if (e2_fs_stat (dlocal, &sb E2_ERR_NONE()))
			{
				//FIXME handle error properly - maybe nothing to overwrite now
				//E2_ERR_CLEAR
			}
			else if (sdir || !S_ISDIR (sb.st_mode))	//no special treatment for file-to-dir
			{
				if (stime > sb.st_mtime)
					type = _("older");	//dest is older
				else if (stime < sb.st_mtime)
					type = _("newer");	//source is older
			}
		}
//#ifdef E2_VFS
//		else
//			E2_ERR_CLEAR
//#endif
	}
	if (type == NULL)
		type = _("existing");

	if (!e2_fs_lstat (dlocal, &sb E2_ERR_NONE())	//should never fail
		&& S_ISDIR (sb.st_mode)
		&& !S_ISLNK (sb.st_mode))
			g_string_printf (dialog_prompt,
				_("Remove all contents of %s\n<b>%s</b> ?"), type, public);
	else
	{
		gchar *dbase = g_path_get_basename (public);
		gchar *dpath = g_path_get_dirname (public);
		g_string_printf (dialog_prompt, _("Overwrite %s <b>%s</b>\nin %s ?"),
			type, dbase, dpath);
		g_free (dbase);
		g_free (dpath);
	}

	CLOSEBGL
	GtkWidget *dialog = e2_dialog_create (STOCK_NAME_DIALOG_WARNING,
		dialog_prompt->str, _("confirm"), DUMMY_RESPONSE_CB, NULL);
	OPENBGL

	F_FREE (utf, VPSTR(dlocal));
	g_free (public);
	g_string_free (dialog_prompt, TRUE);
//FIXME adjust this to suit single-setup when multi is TRUE
	E2_Button all_btn;
	E2_Button stop_btn;
	all_btn = E2_BUTTON_APPLYTOALL;
	all_btn.name = STOCK_NAME_OK;
	stop_btn = E2_BUTTON_CANCEL;
	if (extras & NOALL)
	{
		all_btn.showflags |= E2_BTN_GREY;
		e2_dialog_set_negative_response (dialog, E2_RESPONSE_NOTOALL);
	}
	else if (!(extras & BOTHALL))
	{
		all_btn.showflags |= E2_BTN_GREY;
		stop_btn.showflags |= E2_BTN_GREY;
	}
	else
	{
		all_btn.tip = _("Overwrite any existing item without further confirmation");
		all_btn.showflags |= E2_BTN_TIPPED;
	}

	e2_dialog_add_defined_button (dialog, &stop_btn);
	e2_dialog_add_defined_button (dialog, &all_btn);

	e2_button_derive (&stop_btn, &E2_BUTTON_NO, BTN_NO_KEEP);
	//set default button to 'no'
	stop_btn.showflags |= E2_BTN_DEFAULT;
	e2_dialog_add_defined_button (dialog, &stop_btn);

	stop_btn = E2_BUTTON_YES;
	stop_btn.label = _("_Replace");
	stop_btn.showflags &= ~E2_BTN_DEFAULT;
	stop_btn.name = STOCK_NAME_OK;
	e2_dialog_add_defined_button (dialog, &stop_btn);

	//block until user responds
	// FIXME just hide the window, if so specified
	E2_DialogFlags flags = E2_DIALOG_BLOCKED | E2_DIALOG_FREE | E2_DIALOG_CLOSELOCK;

	if (!(all_btn.showflags & E2_BTN_GREY))
		flags |= E2_DIALOG_MULTI;

	if (e2_fs_access3 (dlocal, W_OK E2_ERR_NONE()))
		_e2_dialog_setup_auth (dialog, (flags & E2_DIALOG_MULTI));

//tag DEBUGfreeze2;
	//maybe this helps
//	OPENBGL

	return (e2_dialog_show (dialog, app.main_window, flags, NULL));
}
/**
@brief run warning dialog for confirmation
Assumes BGL is on/closed
@param prompt prompt string
@param yes_label translated label to use for yes-button, or NULL for "Commi_t"

@return button code returned by the dialog
*/
DialogButtons e2_dialog_warning (gchar *prompt, const gchar *yes_label)
{
	GtkWidget *dialog = e2_dialog_create (STOCK_NAME_DIALOG_WARNING, prompt,
		_("confirm"), DUMMY_RESPONSE_CB, NULL);

	E2_Button no_btn;
	e2_button_derive (&no_btn, &E2_BUTTON_NO, BTN_NO_CANCEL);
	//set default button to 'no'
	no_btn.showflags |= E2_BTN_DEFAULT;

	E2_Button yes_btn;
	if (yes_label != NULL)
	{
		yes_btn = E2_BUTTON_YES;
		yes_btn.label = yes_label;
	}
	else
	{
		e2_button_derive (&yes_btn, &E2_BUTTON_YES, BTN_YES_COMMIT);
		yes_btn.name = E2_BUTTON_APPLY.name;
	}
	yes_btn.showflags &= ~E2_BTN_DEFAULT;
	//block until user responds
	return (e2_dialog_show (dialog, app.main_window,
			E2_DIALOG_BLOCKED | E2_DIALOG_FREE,
			&no_btn, &yes_btn, NULL));
}
/**
@brief create "too slow" dialog
Assumes BGL is on.
@a response_func must handle: GTK_RESPONSE_YES (wait again), GTK_RESPONSE_NO (stop waiting),
and E2_RESPONSE_USER1 (stop asking)
Note: when juggling dialog between threads, avoid gtk_main[_quit] usage in
dialog display code and response cb.
@param prompt_type context-specific part of prompt string
@param tip_type context-specific part of cancel-button tip
@param response_func pointer to function to use for dialog's "response" callback, or NULL
@param data data to provide to the dialog's response callback

@return the shown dialog widget
*/
GtkWidget *e2_dialog_slow (gchar *prompt_type, gchar *tip_type,
	void(*response_func) (GtkDialog*,gint,gpointer), gpointer data)
{
	gchar *prompt = g_strdup_printf (
		_("%s is taking a long time. Continue waiting ?"), prompt_type);
	GtkWidget *dialog = e2_dialog_create (STOCK_NAME_DIALOG_QUESTION, prompt,
		_("confirm"), response_func, data);
	g_free (prompt);

	E2_Button local_button;
	local_button.label = _("_Quiet");
	local_button.tip = _("Don't ask any more");
	local_button.showflags = E2_BTN_TIPPED;
	local_button.response = E2_RESPONSE_USER1;
	e2_dialog_add_defined_button (dialog, &local_button);

	local_button.label = _("_Stop");
	local_button.tip = g_strdup_printf (_("Cancel the %s"), tip_type);
	local_button.response = GTK_RESPONSE_NO;
	e2_dialog_add_defined_button (dialog, &local_button);
	g_free (local_button.tip);

	local_button = E2_BUTTON_YES;
	local_button.label = _("_Wait");
	local_button.tip = _("Wait some more");
	local_button.showflags = E2_BTN_TIPPED;
	e2_dialog_add_defined_button (dialog, &local_button);

	e2_dialog_set_negative_response (dialog, GTK_RESPONSE_YES);

	e2_dialog_setup (dialog, app.main_window);
	gtk_widget_show_all (dialog);

	return dialog;
}

/**
@brief register dialog-related options
@return
*/
void e2_dialog_options_register (void)
{
	const gchar *opt_dialog_pos_position[] = {_("none"), _("center"), _("mouse"),
		_("always center"), _("center on parent"), NULL};
	gchar *group_name = _C(11);  //_("dialogs"
	e2_option_sel_register ("dialog-position", group_name, _("dialog position"),
		_("This determines the position where dialog windows will pop up"), NULL, 1,
		opt_dialog_pos_position,
		E2_OPTION_FLAG_ADVANCED);
}
