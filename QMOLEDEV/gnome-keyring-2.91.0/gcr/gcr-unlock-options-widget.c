/*
 * Copyright (C) 2010 Stefan Walter
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as
 * published by the Free Software Foundation; either version 2.1 of
 * the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
 * 02111-1307, USA.
 */

#include "config.h"

#include "gcr-unlock-options-widget.h"

#include <glib/gi18n-lib.h>

enum {
	PROP_0,
	PROP_CHOICE,
	PROP_TTL
};

struct _GcrUnlockOptionsWidgetPrivate {
	GtkBuilder *builder;
	gchar *choice;
};

G_DEFINE_TYPE (GcrUnlockOptionsWidget, gcr_unlock_options_widget, GTK_TYPE_ALIGNMENT);

/* -----------------------------------------------------------------------------
 * INTERNAL
 */

static GtkToggleButton*
builder_get_toggle_button (GtkBuilder *builder, const gchar *name)
{
	GObject *object = gtk_builder_get_object (builder, name);
	g_return_val_if_fail (GTK_IS_TOGGLE_BUTTON (object), NULL);
	return GTK_TOGGLE_BUTTON (object);
}

static GtkSpinButton*
builder_get_spin_button (GtkBuilder *builder, const gchar *name)
{
	GObject *object = gtk_builder_get_object (builder, name);
	g_return_val_if_fail (GTK_IS_SPIN_BUTTON (object), NULL);
	return GTK_SPIN_BUTTON (object);
}

static const gchar*
widget_name_for_option (const gchar *option)
{
	g_return_val_if_fail (option, NULL);
	if (g_str_equal (option, GCR_UNLOCK_OPTION_ALWAYS))
		return "lock_always_choice";
	else if (g_str_equal (option, GCR_UNLOCK_OPTION_SESSION))
		return "lock_session_choice";
	else if (g_str_equal (option, GCR_UNLOCK_OPTION_TIMEOUT))
		return "lock_timeout_choice";
	else if (g_str_equal (option, GCR_UNLOCK_OPTION_IDLE))
		return "lock_idle_choice";
	else
		return NULL;
}

static GtkToggleButton*
widget_button_for_option (GcrUnlockOptionsWidget *self, const gchar *option)
{
	const gchar *name = widget_name_for_option (option);
	g_return_val_if_fail (name, NULL);
	return builder_get_toggle_button (self->pv->builder, name);
}

static const gchar*
widget_button_to_option (GcrUnlockOptionsWidget *self, GtkToggleButton *button)
{
	const gchar *option;
	g_return_val_if_fail (button, NULL);
	option = g_object_get_data (G_OBJECT (button), "unlock-choice");
	g_return_val_if_fail (option, NULL);
	return option;
}

static void
on_choice_toggled (GtkToggleButton *button, GcrUnlockOptionsWidget *self)
{
	GtkWidget *spin;
	GtkToggleButton *after, *idle;

	spin = GTK_WIDGET (gtk_builder_get_object (self->pv->builder, "lock_minutes_spin"));
	after = builder_get_toggle_button (self->pv->builder, "lock_timeout_choice");
	idle = builder_get_toggle_button (self->pv->builder, "lock_idle_choice");
	gtk_widget_set_sensitive (spin, gtk_toggle_button_get_active (after) ||
	                                gtk_toggle_button_get_active (idle));

	if (gtk_toggle_button_get_active (button)) {
		g_free (self->pv->choice);
		self->pv->choice = g_strdup (widget_button_to_option (self, button));
	}
}

/* -----------------------------------------------------------------------------
 * OBJECT
 */


static GObject*
gcr_unlock_options_widget_constructor (GType type, guint n_props, GObjectConstructParam *props)
{
	GObject *obj = G_OBJECT_CLASS (gcr_unlock_options_widget_parent_class)->constructor (type, n_props, props);
	GcrUnlockOptionsWidget *self = NULL;
	GtkToggleButton *button;
	GtkWidget *widget;

	if (obj) {
		self = GCR_UNLOCK_OPTIONS_WIDGET (obj);

		if (!gtk_builder_add_from_file (self->pv->builder, UIDIR "gcr-unlock-options-widget.ui", NULL))
			g_return_val_if_reached (obj);

		widget = GTK_WIDGET (gtk_builder_get_object (self->pv->builder, "unlock-options-widget"));
		g_return_val_if_fail (GTK_IS_WIDGET (widget), obj);
		gtk_container_add (GTK_CONTAINER (self), widget);
		gtk_widget_show (widget);

		button = builder_get_toggle_button (self->pv->builder, "lock_always_choice");
		g_signal_connect (button, "toggled", G_CALLBACK (on_choice_toggled), self);
		g_object_set_data (G_OBJECT (button), "unlock-choice", GCR_UNLOCK_OPTION_ALWAYS);

		button = builder_get_toggle_button (self->pv->builder, "lock_session_choice");
		g_signal_connect (button, "toggled", G_CALLBACK (on_choice_toggled), self);
		g_object_set_data (G_OBJECT (button), "unlock-choice", GCR_UNLOCK_OPTION_SESSION);
		on_choice_toggled (button, self);

		button = builder_get_toggle_button (self->pv->builder, "lock_timeout_choice");
		g_signal_connect (button, "toggled", G_CALLBACK (on_choice_toggled), self);
		g_object_set_data (G_OBJECT (button), "unlock-choice", GCR_UNLOCK_OPTION_TIMEOUT);

		button = builder_get_toggle_button (self->pv->builder, "lock_idle_choice");
		g_signal_connect (button, "toggled", G_CALLBACK (on_choice_toggled), self);
		g_object_set_data (G_OBJECT (button), "unlock-choice", GCR_UNLOCK_OPTION_IDLE);
	}

	return obj;
}

static void
gcr_unlock_options_widget_init (GcrUnlockOptionsWidget *self)
{
	self->pv = (G_TYPE_INSTANCE_GET_PRIVATE (self, GCR_TYPE_UNLOCK_OPTIONS_WIDGET, GcrUnlockOptionsWidgetPrivate));
	self->pv->builder = gtk_builder_new ();
}

static void
gcr_unlock_options_widget_dispose (GObject *obj)
{
	GcrUnlockOptionsWidget *self = GCR_UNLOCK_OPTIONS_WIDGET (obj);

	if (self->pv->builder)
		g_object_unref (self->pv->builder);
	self->pv->builder = NULL;

	G_OBJECT_CLASS (gcr_unlock_options_widget_parent_class)->dispose (obj);
}

static void
gcr_unlock_options_widget_finalize (GObject *obj)
{
	GcrUnlockOptionsWidget *self = GCR_UNLOCK_OPTIONS_WIDGET (obj);

	g_assert (!self->pv->builder);
	g_free (self->pv->choice);
	self->pv->choice = NULL;

	G_OBJECT_CLASS (gcr_unlock_options_widget_parent_class)->finalize (obj);
}

static void
gcr_unlock_options_widget_set_property (GObject *obj, guint prop_id, const GValue *value,
                                        GParamSpec *pspec)
{
	GcrUnlockOptionsWidget *self = GCR_UNLOCK_OPTIONS_WIDGET (obj);

	switch (prop_id) {
	case PROP_CHOICE:
		gcr_unlock_options_widget_set_choice (self, g_value_get_string (value));
		break;
	case PROP_TTL:
		gcr_unlock_options_widget_set_ttl (self, g_value_get_uint (value));
		break;
	default:
		G_OBJECT_WARN_INVALID_PROPERTY_ID (obj, prop_id, pspec);
		break;
	}
}

static void
gcr_unlock_options_widget_get_property (GObject *obj, guint prop_id, GValue *value,
                                        GParamSpec *pspec)
{
	GcrUnlockOptionsWidget *self = GCR_UNLOCK_OPTIONS_WIDGET (obj);

	switch (prop_id) {
	case PROP_CHOICE:
		g_value_set_string (value, gcr_unlock_options_widget_get_choice (self));
		break;
	case PROP_TTL:
		g_value_set_uint (value, gcr_unlock_options_widget_get_ttl (self));
		break;
	default:
		G_OBJECT_WARN_INVALID_PROPERTY_ID (obj, prop_id, pspec);
		break;
	}
}

static void
gcr_unlock_options_widget_class_init (GcrUnlockOptionsWidgetClass *klass)
{
	GObjectClass *gobject_class = G_OBJECT_CLASS (klass);

	gcr_unlock_options_widget_parent_class = g_type_class_peek_parent (klass);
	g_type_class_add_private (klass, sizeof (GcrUnlockOptionsWidgetPrivate));

	gobject_class->constructor = gcr_unlock_options_widget_constructor;
	gobject_class->dispose = gcr_unlock_options_widget_dispose;
	gobject_class->finalize = gcr_unlock_options_widget_finalize;
	gobject_class->set_property = gcr_unlock_options_widget_set_property;
	gobject_class->get_property = gcr_unlock_options_widget_get_property;

	g_object_class_install_property (gobject_class, PROP_CHOICE,
	               g_param_spec_string ("choice", "Choice", "Unlock Option Choice",
	                                    NULL, G_PARAM_READWRITE));

	g_object_class_install_property (gobject_class, PROP_TTL,
	               g_param_spec_uint ("ttl", "TTL", "Unlock Option Timeout in Seconds",
	                                  0, G_MAXUINT, 0, G_PARAM_READWRITE));
}

/* -----------------------------------------------------------------------------
 * PUBLIC
 */

GtkWidget*
gcr_unlock_options_widget_new (void)
{
	return g_object_new (GCR_TYPE_UNLOCK_OPTIONS_WIDGET, NULL);
}

const gchar*
gcr_unlock_options_widget_get_choice (GcrUnlockOptionsWidget *self)
{
	g_return_val_if_fail (GCR_IS_UNLOCK_OPTIONS_WIDGET (self), NULL);
	return self->pv->choice;
}

void
gcr_unlock_options_widget_set_choice (GcrUnlockOptionsWidget *self, const gchar *option)
{
	GtkToggleButton *button;

	g_return_if_fail (GCR_IS_UNLOCK_OPTIONS_WIDGET (self));
	g_return_if_fail (option);

	button = widget_button_for_option (self, option);
	gtk_toggle_button_set_active (button, TRUE);
}

guint
gcr_unlock_options_widget_get_ttl (GcrUnlockOptionsWidget *self)
{
	GtkSpinButton *spin;
	gint amount;

	g_return_val_if_fail (GCR_IS_UNLOCK_OPTIONS_WIDGET (self), 0);

	spin = builder_get_spin_button (self->pv->builder, "lock_minutes_spin");
	amount = gtk_spin_button_get_value_as_int (spin);
	return amount * 60;
}

void
gcr_unlock_options_widget_set_ttl (GcrUnlockOptionsWidget *self, guint ttl)
{
	GtkSpinButton *spin;
	guint amount;

	g_return_if_fail (GCR_IS_UNLOCK_OPTIONS_WIDGET (self));

	amount = ttl / 60;
	if (!amount || ttl % 60)
		amount += 1;

	spin = builder_get_spin_button (self->pv->builder, "lock_minutes_spin");
	gtk_spin_button_set_value (spin, amount);
}

const gchar*
gcr_unlock_options_widget_get_label (GcrUnlockOptionsWidget *self, const gchar *option)
{
	GtkToggleButton *button;
	const gchar *name;

	g_return_val_if_fail (GCR_IS_UNLOCK_OPTIONS_WIDGET (self), NULL);
	g_return_val_if_fail (option, NULL);

	name = widget_name_for_option (option);
	g_return_val_if_fail (name, NULL);

	button = builder_get_toggle_button (self->pv->builder, name);
	g_return_val_if_fail (button, NULL);

	return gtk_button_get_label (GTK_BUTTON (button));
}

void
gcr_unlock_options_widget_set_label (GcrUnlockOptionsWidget *self, const gchar *option,
                                     const gchar *text)
{
	GtkToggleButton *button;
	const gchar *name;

	g_return_if_fail (GCR_IS_UNLOCK_OPTIONS_WIDGET (self));
	g_return_if_fail (option);
	g_return_if_fail (text);

	name = widget_name_for_option (option);
	g_return_if_fail (name);

	button = builder_get_toggle_button (self->pv->builder, name);
	g_return_if_fail (button);

	gtk_button_set_label (GTK_BUTTON (button), text);
}

gboolean
gcr_unlock_options_widget_get_sensitive (GcrUnlockOptionsWidget *self, const gchar *option)
{
	GtkToggleButton *button;
	GtkStateType state;

	g_return_val_if_fail (GCR_IS_UNLOCK_OPTIONS_WIDGET (self), FALSE);
	g_return_val_if_fail (option, FALSE);

	button = widget_button_for_option (self, option);
	state = gtk_widget_get_state (GTK_WIDGET (button));
	return (state & GTK_STATE_INSENSITIVE) != GTK_STATE_INSENSITIVE;
}

void
gcr_unlock_options_widget_set_sensitive (GcrUnlockOptionsWidget *self, const gchar *option,
                                         gboolean sensitive, const gchar *reason)
{
	GtkToggleButton *button;

	g_return_if_fail (GCR_IS_UNLOCK_OPTIONS_WIDGET (self));
	g_return_if_fail (option);

	button = widget_button_for_option (self, option);
	gtk_widget_set_sensitive (GTK_WIDGET (button), sensitive);

	if (!sensitive && reason)
		gtk_widget_set_tooltip_text (GTK_WIDGET (button), reason);
	else if (sensitive)
		gtk_widget_set_has_tooltip (GTK_WIDGET (button), FALSE);
}
