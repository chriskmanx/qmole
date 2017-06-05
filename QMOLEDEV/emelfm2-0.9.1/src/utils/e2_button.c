/* $Id: e2_button.c 2790 2013-10-09 13:00:04Z tpgww $

Copyright (C) 2004-2013 tooar <tooar@emelfm2.net>

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
#include "e2_button.h"
#include "e2_icons.h"

E2_Button E2_BUTTON_YES =
	{ NULL,STOCK_NAME_YES, NULL,	//label is always context-dependent
		E2_BTN_DEFAULT,E2_BTN_DEFAULT,GTK_RESPONSE_YES};
E2_Button E2_BUTTON_NO =
	{ NULL,STOCK_NAME_NO,NULL,0,0,GTK_RESPONSE_NO};	//label is always context-dependent
E2_Button E2_BUTTON_CANCEL =
	{ N_("_Cancel"),STOCK_NAME_CANCEL,NULL,0,0,E2_RESPONSE_NOTOALL};
E2_Button E2_BUTTON_APPLY =
	{ N_("Commi_t"),STOCK_NAME_OK,NULL,0,0,E2_RESPONSE_APPLY}; //often co-used with _Cancel, _All
E2_Button E2_BUTTON_APPLYTOALL =
	{ N_("_All"),"apply_all"E2ICOND,N_("Change all unprocessed items"),
		E2_BTN_TIPPED,E2_BTN_TIPPED,E2_RESPONSE_APPLYTOALL};
E2_Button E2_BUTTON_DISCARD =
	{ N_("_Discard"),STOCK_NAME_DISCARD,N_("Abandon any changes"),
		E2_BTN_TIPPED,E2_BTN_TIPPED,E2_RESPONSE_DISCARD};
E2_Button E2_BUTTON_REFRESH =
	{ N_("_Refresh"),STOCK_NAME_REFRESH,NULL,0,0,E2_RESPONSE_REFRESH};
E2_Button E2_BUTTON_CLOSE =
	{ N_("_Close"),STOCK_NAME_CLOSE,NULL,0,0,GTK_RESPONSE_CLOSE};
E2_Button E2_BUTTON_CREATE =
	{ N_("C_reate"),STOCK_NAME_YES,NULL,0,0,E2_RESPONSE_CREATE};  //co-use with close/cancel
E2_Button E2_BUTTON_REMOVE =
	{ N_("_Remove"),STOCK_NAME_REMOVE,NULL,0,0,E2_RESPONSE_REMOVE}; //check no co-use with refresh
E2_Button E2_BUTTON_MORE =
	{ N_("_Apply"),"proceed"E2ICOND,NULL,0,0,E2_RESPONSE_MORE};

//array of labels which may be applied to pending or existing dialog-buttons
//(usually yes or no) indexed by E2_ButtonText enum
static const gchar *defined_button_labels[] =
{
	//+ve choices
	N_("Commi_t"),	//might appear with _All or _Cancel
	N_("_Proceed"),
	N_("_Delete"),
	//-ve choices
	N_("_Cancel"),
	N_("_Omit"),	//often appears with "_Stop"
	N_("_Keep"),	//typically with "Delete"
};
//array of tips corresponding to above labels, NULL when no tip applies
static const gchar *defined_button_tips[] =
{
	//+ve choices
	NULL,
	NULL,
	NULL,
	//-ve choices
	N_("Do not do anything"),
	N_("Do not change this item"),
	N_("Do not change this"),
};

/**
@brief initialize translated labels and tips for defined buttons

@return
*/
void e2_button_setup_labels (void)
{
	guint i, count;
	count = sizeof(defined_button_labels)/sizeof(defined_button_labels[0]);
	for (i = 0; i < count; i++)
	{
		defined_button_labels[i] = gettext (defined_button_labels[i]);
		if (defined_button_tips[i] != NULL)
			defined_button_tips[i] = gettext (defined_button_tips[i]);
	}

	E2_BUTTON_YES.label = defined_button_labels[BTN_YES_COMMIT];
	E2_BUTTON_NO.label = defined_button_labels[BTN_NO_CANCEL];
	E2_BUTTON_CANCEL.label = gettext (E2_BUTTON_CANCEL.label);
	E2_BUTTON_APPLY.label = gettext (E2_BUTTON_APPLY.label);
	E2_BUTTON_APPLYTOALL.label = gettext (E2_BUTTON_APPLYTOALL.label);
	E2_BUTTON_DISCARD.label = gettext (E2_BUTTON_DISCARD.label);
	E2_BUTTON_REFRESH.label = gettext (E2_BUTTON_REFRESH.label);
	E2_BUTTON_CLOSE.label = gettext (E2_BUTTON_CLOSE.label);
	E2_BUTTON_CREATE.label = gettext (E2_BUTTON_CREATE.label);
	E2_BUTTON_REMOVE.label = gettext (E2_BUTTON_REMOVE.label);
	E2_BUTTON_MORE.label = gettext (E2_BUTTON_MORE.label);

	E2_BUTTON_APPLYTOALL.tip = gettext (E2_BUTTON_APPLYTOALL.tip);
	E2_BUTTON_DISCARD.tip = gettext (E2_BUTTON_DISCARD.tip);
	E2_BUTTON_MORE.tip = gettext (E2_BUTTON_MORE.tip);
}
/**
@brief set label and tip for pending button @a button, and maybe the icon
This does not affect the "default" label
@param button pointer to button data struct
@param base pointer to static defined-button data struct, or NULL
@param type 0-based index into labels array

@return
*/
void e2_button_derive (E2_Button *button, E2_Button *base, E2_ButtonText type)
{
	if (base != NULL)	//populate struct if wanted
		*button = *base;
	if (type >= 0 && type < (sizeof(defined_button_labels)/sizeof(defined_button_labels[0])))
	{
		button->label = defined_button_labels[type];
		button->tip = (gchar *)defined_button_tips[type];
		if (button->tip != NULL)
			button->showflags |= E2_BTN_TIPPED;
		else
			button->showflags &= ~E2_BTN_TIPPED;
		if (type == BTN_NO_CANCEL)
			button->name = STOCK_NAME_CANCEL;
	}
}
/**
@brief set label and tip for existing button @a button
This does not affect the button's "default" label
@param button the button widget to be updated
@param type 0-based index into labels array

@return
*/
void e2_button_set_indexed_text (GtkWidget *button, E2_ButtonText type)
{
	if (type >= 0 && type < (sizeof(defined_button_labels)/sizeof(defined_button_labels[0])))
	{
		e2_button_set_label (button, defined_button_labels[type]);
		if (defined_button_tips[type] != NULL)
		{
			e2_widget_set_safetip (button, defined_button_tips[type]);
		}
		else
		{
#ifdef USE_GTK2_12TIPS
			gtk_widget_set_has_tooltip (button, FALSE);
#else
			GtkTooltipsData *tdata = gtk_tooltips_data_get (button);
			if (tdata != NULL)
			{
				g_free (tdata->tip_text);
				tdata->tip_text = NULL;
				g_free (tdata->tip_private);
				tdata->tip_private = NULL;
			}
#endif
		}
	}
}
/**
@brief set new label for existing widget @a button

e2 uses non-standard dialog buttons, containing alignment
containing h/vbox, the latter with label packed after icon (if any)

@param button the button widget to be updated
@param label string optionally with mnemonic

@return
*/
void e2_button_set_label (GtkWidget *button, const gchar *label)
{
	if (label != NULL && *label != '\0')
	{
		GtkWidget *bbox =
#ifdef USE_GTK2_14
			gtk_bin_get_child (GTK_BIN (button));
		bbox = gtk_bin_get_child (GTK_BIN (bbox));
		GList *children = gtk_container_get_children (GTK_CONTAINER (bbox));
		GtkWidget *child = (GtkWidget*) children->data;
		if (!GTK_IS_LABEL (child))
			child = (GtkWidget*) children->next->data;
		gtk_label_set_markup_with_mnemonic (GTK_LABEL (child), label);
		g_list_free (children);
#else
			GTK_BIN (GTK_BIN (button)->child)->child;
		GtkBoxChild *child = (GtkBoxChild *) GTK_BOX (bbox)->children->data;
		if (!GTK_IS_LABEL (child->widget))
			child = (GtkBoxChild *) GTK_BOX (bbox)->children->next->data;
		gtk_label_set_markup_with_mnemonic (GTK_LABEL (child->widget), label);
#endif
	}
}
/**
@brief set new (or no) icon for existing button @a button

e2 uses non-standard dialog buttons, containing alignment containing h/vbox,
the latter with image (if any) packed first. @a stock may be a gtk stock name
or a custom icon filename. The new image is shown immediately, so this func is
not intended for buttons which are ultimately included in a "show all".

@param button the button widget to be updated
@param stock string describing icon or NULL to clear the icon

@return
*/
void e2_button_set_image (GtkWidget *button, gchar *stock)
{
	gint choice = e2_option_sel_get ("dialog-button-icons");
	if (choice == 0)
	{
#ifdef USE_GTK2_18
		gboolean show;
		GtkSettings* defs = gtk_settings_get_default ();
		g_object_get (G_OBJECT (defs), "gtk-button-images", &show, NULL);
		if (show)
#endif
			choice = 1;
	}
	if (choice == 1)
	{
		GtkWidget *image = (stock != NULL) ?
			e2_widget_get_icon (stock, GTK_ICON_SIZE_BUTTON) : NULL;
		if (stock == NULL || image != NULL)
		{
			GtkWidget *bbox =
#ifdef USE_GTK2_14
				gtk_bin_get_child (GTK_BIN (button));
			bbox = gtk_bin_get_child (GTK_BIN (bbox));
			GList *children = gtk_container_get_children (GTK_CONTAINER (bbox));
			GtkWidget *child1 = (GtkWidget *) children->data;
			if (GTK_IS_IMAGE (child1))
				gtk_container_remove (GTK_CONTAINER (bbox), child1);
			g_list_free (children);
#else
				GTK_BIN (GTK_BIN (button)->child)->child;
			GtkBoxChild *child1 = (GtkBoxChild *) GTK_BOX (bbox)->children->data;
			if (GTK_IS_IMAGE (child1->widget))
				gtk_container_remove (GTK_CONTAINER (bbox), child1->widget);
#endif
			if (image != NULL)
			{
				gtk_box_pack_start (GTK_BOX (bbox), image, FALSE, FALSE, 0);
				gtk_box_reorder_child (GTK_BOX (bbox), image, 0);
				gtk_widget_show (image);
			}
		}
	}
}
/**
@brief create button
This is used for toolbar buttons, and (maybe indirectly) for dialog buttons
@param label button label which may contain markup and/or mnemonic, or NULL
@param stock string describing icon, or NULL
@param size code for size of button-icon
@param tip tooltip for the button, or NULL
@param callback callback function for button's "clicked" signal, or NULL
@param data data specified for the callback
@param flags flags indicating button properties

@return the (unshown) button widget
*/
GtkWidget *e2_button_get_full (const gchar *label, const gchar *stock, GtkIconSize size,
	const gchar *tip, void (*callback)(/*GtkButton*,gpointer*/), gpointer data,
	E2_ButtonFlags flags)
{
	GtkWidget *button = gtk_button_new ();
#ifdef USE_GTK2_18
	gtk_widget_set_can_default (button, (flags & E2_BUTTON_CAN_DEFAULT) > 0);
	gtk_widget_set_can_focus (button, (flags & E2_BUTTON_CAN_FOCUS) > 0);
#else
	if (flags & E2_BUTTON_CAN_DEFAULT)
		GTK_WIDGET_SET_FLAGS (button, GTK_CAN_DEFAULT);
	else
		GTK_WIDGET_UNSET_FLAGS (button, GTK_CAN_DEFAULT);
	if (flags & E2_BUTTON_CAN_FOCUS)
		GTK_WIDGET_SET_FLAGS (button, GTK_CAN_FOCUS);
	else
		GTK_WIDGET_UNSET_FLAGS (button, GTK_CAN_FOCUS);
#endif

	if (tip != NULL)
		e2_widget_set_safetip (button, tip);
	if (callback != NULL)
		g_signal_connect_data (G_OBJECT (button), "clicked",
			G_CALLBACK (callback), data,
			(flags & E2_BUTTON_FREE_DATA) ? (GClosureNotify) g_free : NULL, 0);

	GtkWidget *bbox;
#ifdef USE_GTK3_0
	if (flags & E2_BUTTON_ICON_ABOVE_LABEL)
		bbox = gtk_box_new (GTK_ORIENTATION_VERTICAL, 2);	//constant gap, not E2_PADDING_XSMALL
	else
		bbox = gtk_box_new (GTK_ORIENTATION_HORIZONTAL, 2);
#else
	if (flags & E2_BUTTON_ICON_ABOVE_LABEL)
		bbox = gtk_vbox_new (FALSE, 2);
	else
		bbox = gtk_hbox_new (FALSE, 2);
#endif

	gboolean need_align = FALSE;

	if (stock != NULL)
	{
		GtkWidget *img = e2_widget_get_icon (stock, size);
		if (img == NULL && (flags & E2_BUTTON_SHOW_MISSING_ICON))
			img = e2_widget_get_icon (STOCK_NAME_MISSING_IMAGE, size);
		if (img != NULL)
		{
			gtk_box_pack_start (GTK_BOX (bbox),
				img, FALSE, FALSE, 0);
			need_align = TRUE;
		}
	}

	if ((label != NULL) && (*label != '\0'))
	{
		GtkWidget *wlab = gtk_label_new ("");
		gtk_label_set_markup_with_mnemonic (GTK_LABEL (wlab), label);
		gtk_label_set_mnemonic_widget ((GtkLabel*)wlab, button); //NEEDED? label not a child of button
		gtk_box_pack_start (GTK_BOX (bbox), wlab, TRUE, TRUE, 0);
		need_align = TRUE;
	}

	if (need_align)
	{
		GtkWidget *align = gtk_alignment_new (0.5, 0.5, 0.0, 0.0);
		gtk_container_add (GTK_CONTAINER (align), bbox);
		gtk_container_add (GTK_CONTAINER (button), align);
	}
	else	//should never happen
		gtk_container_add (GTK_CONTAINER (button), bbox);
	return button;
}
/**
@brief get button-widget which can be default and focused
Downstream code requires BGL closed
@param label button label which may contain markup and/or mnemonic
@param stock string describing icon, or NULL
@param tip tooltip for the button, or NULL
@param callback callback function for button's "clicked" signal, or NULL
@param data data to send to the callback

@return the (unshown) new button widget
*/
GtkWidget *e2_button_get (const gchar *label, const gchar *stock, const gchar *tip,
	void (*callback)(/*GtkButton*,gpointer*/), gpointer data)
{
	return (e2_button_get_full (label, stock, GTK_ICON_SIZE_BUTTON,
		tip, callback, data, E2_BUTTON_CAN_DEFAULT | E2_BUTTON_CAN_FOCUS));
}
/**
@brief add button-sized button to the start of @a box

@param box widget to hold the button
@param exp TRUE to make the button expand with @a box
@param pad padding between button and @a box
@param label button label which may contain markup and/or mnemonic
@param stock string describing icon, or NULL
@param tip tooltip for the button, or NULL
@param callback callback function for "clicked" signal, or NULL
@param data data specified for the callback

@return the button widget
*/
GtkWidget *e2_button_add (GtkWidget *box, gboolean exp, guint pad, gchar *label,
	gchar *stock, gchar *tip, void (*callback)(/*GtkButton*,gpointer*/), gpointer data)
{
	GtkWidget *button = e2_button_get (label, stock, tip, callback, data);
	gtk_box_pack_start (GTK_BOX (box), button, exp, TRUE, pad);
	return button;
}
/**
@brief add button-sized button to the end of @a box

@param box widget to hold the button
@param exp TRUE to make the button expand with @a box
@param pad padding between button and @a box
@param label button label which may contain markup and/or mnemonic
@param stock string describing icon, or NULL
@param tip tooltip for the button, or NULL
@param callback callback function for "clicked" signal, or NULL
@param data data specified for the callback

@return the (unshown) new button widget
*/
GtkWidget *e2_button_add_end (GtkWidget *box, gboolean exp, guint pad, gchar *label,
	gchar *stock, gchar *tip, void (*callback)(/*GtkButton*,gpointer*/), gpointer data)
{
	GtkWidget *button = e2_button_get (label, stock, tip, callback, data);
	gtk_box_pack_end (GTK_BOX (box), button, exp, TRUE, pad);
	return button;
}
/**
@brief create check or toggle button with mmemonic label

@param check TRUE for check button, FALSE for toggle button
@param state initial state of the button
@param label button label which may contain markup and/or mnemonic
@param tip tooltip for the button, or NULL
@param callback callback function for "toggled" signal, or NULL
@param data data specified for @a callback

@return the button widget
*/
GtkWidget *e2_button_get_toggle (gboolean check, gboolean state, gchar *label,
	gchar *tip, void (*callback)(GtkToggleButton*,gpointer), gpointer data)
{
	GtkWidget *button;
	if (check)
		button = gtk_check_button_new_with_mnemonic (label);
	else
		button = gtk_toggle_button_new_with_mnemonic (label);
#ifdef USE_GTK2_18
	gtk_widget_set_can_default (button, TRUE);
	gtk_widget_set_can_focus (button, TRUE);
#else
	GTK_WIDGET_SET_FLAGS (button, GTK_CAN_DEFAULT);
	GTK_WIDGET_SET_FLAGS (button, GTK_CAN_FOCUS);
#endif
	if (state)
		gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON (button), TRUE);
	if (tip != NULL)
		e2_widget_set_safetip (button, tip);
	if (callback != NULL)
		g_signal_connect (G_OBJECT (button), "toggled", G_CALLBACK (callback), data);
	return button;
}
/**
@brief add check or toggle button with mmemonic label to @a box

@param box widget to hold the button
@param check TRUE for check button, FALSE for toggle button
@param state initial state of the button
@param label button label which may contain mnemonic
@param tip tooltip for the button, or NULL
@param exp TRUE to make the button expand with @a box
@param pad padding between button and @a box
@param callback callback function for "toggled" signal, or NULL
@param data data specified for the callback

@return the button widget
*/
GtkWidget *e2_button_add_toggle (GtkWidget *box, gboolean check, gboolean state,
	gchar *label, gchar *tip, gboolean exp, guint pad,
	void (*callback)(GtkToggleButton*,gpointer), gpointer data)
{
	GtkWidget *button = e2_button_get_toggle (check, state, label, tip, callback, data);
	gtk_box_pack_start (GTK_BOX (box), button, exp, TRUE, pad);
	return button;
}
/**
@brief add radio button with mmemonic label to @a box

@param box widget to hold the button
@param label button label which may contain mnemonic, or NULL if no label
@param group the radio group, or NULL to start a group
@param state the intitial state of the button
@param exp TRUE to make the button expand with @a box
@param pad padding between button and @a box
@param callback callback function for "toggled" signal, or NULL
@param data data specified for the callback

@return the button widget
*/
GtkWidget *e2_button_add_radio (GtkWidget *box, gchar *label, GSList *group,
	gboolean state, gboolean exp, guint pad,
	void (*callback)(GtkToggleButton*,gpointer), gpointer data)
{
	GtkWidget *radio_button = (label != NULL) ?
		gtk_radio_button_new_with_mnemonic (group, label):
		gtk_radio_button_new (group);
	if (state)
		gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON (radio_button), TRUE);
	if (callback != NULL)
		g_signal_connect (G_OBJECT (radio_button), "toggled",
			G_CALLBACK (callback), data);
	gtk_box_pack_start (GTK_BOX (box), radio_button, exp, TRUE, pad);
	return radio_button;
}
/**
@brief add button with mmemonic label to @a table

@param table table to hold the button
@param label button label which may contain mnemonic
@param callback callback function for "clicked" signal, or NULL
@param data data specified for the callback
@param left index of table column left of attachment-position
@param right index of table column right of attachment-position
@param top index of table row above attachment-position
@param bottom index of table row below attachment-position

@return the button widget
*/
GtkWidget *e2_button_add_to_table (
	GtkWidget *table,
	gchar *label,
	void (*callback)(GtkButton*,gpointer),
	gpointer data,
	gint left, gint right, gint top, gint bottom)
{
	GtkWidget *button;
	button = gtk_button_new_with_mnemonic (label);
	if (callback != NULL)
		g_signal_connect (G_OBJECT (button), "clicked",
			G_CALLBACK (callback), data);
#ifdef USE_GTK3_2
	gtk_grid_attach (GTK_GRID (table), button, left, top, right-left, bottom-top);
#else
	gtk_table_attach_defaults (GTK_TABLE (table), button,
								left, right, top, bottom);
#endif
	return button;
}
/**
@brief add check button with optional mmemonic label to @a table

@param table table to hold the button
@param label button label which may contain mnemonic, or NULL
@param state the intitial state of the button
@param callback callback function for "toggled" signal, or NULL
@param data data specified for the callback
@param left index of table column left of attachment-position
@param right index of table column right of attachment-position
@param top index of table row above attachment-position
@param bottom index of table row below attachment-position

@return the button widget
*/
GtkWidget *e2_button_add_toggle_to_table (
	GtkWidget *table,
	gchar *label,
	gboolean state,
	void (*callback)(/*GtkToggleButton*,gpointer*/),
	gpointer data,
	gint left, gint right, gint top, gint bottom)
{
	GtkWidget *check_button;
	check_button = (label == NULL) ? gtk_check_button_new () :
 		gtk_check_button_new_with_mnemonic (label);
	if (state)
		gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON (check_button), TRUE);
	if (callback != NULL)
		g_signal_connect (G_OBJECT (check_button), "toggled",
			G_CALLBACK (callback), data);
#ifdef USE_GTK3_2
	gtk_grid_attach (GTK_GRID (table), check_button, left, top, right-left, bottom-top);
#else
	gtk_table_attach_defaults (GTK_TABLE (table), check_button,
		                        left, right, top, bottom);
#endif
	return check_button;
}
/**
@brief add check button with optional mmemonic label to @a table

@param table table to hold the button
@param label button label which may contain mnemonic
@param group radio group, or NULL to start a group
@param state the intitial state of the button
@param callback callback function for "toggled" signal, or NULL
@param data data specified for the callback
@param left index of table column left of attachment-position
@param right index of table column right of attachment-position
@param top index of table row above attachment-position
@param bottom index of table row below attachment-position

@return the button widget
*/
GtkWidget *e2_button_add_radio_to_table (
	GtkWidget *table,
	gchar *label,
	GSList *group,
	gboolean state,
	void (*callback)(/*GtkToggleButton*,gpointer*/),
	gpointer data,
	gint left, gint right, gint top, gint bottom)
{
	GtkWidget *radio_button = gtk_radio_button_new_with_mnemonic (group, label);
	if (state)
		gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON (radio_button), TRUE);
	if (callback != NULL)
		g_signal_connect (G_OBJECT (radio_button), "toggled",
			G_CALLBACK (callback), data);
#ifdef USE_GTK3_2
	gtk_grid_attach (GTK_GRID (table), radio_button, left, top, right-left, bottom-top);
#else
	gtk_table_attach_defaults (GTK_TABLE (table), radio_button,
		                        left, right, top, bottom);
#endif
	return radio_button;
}
