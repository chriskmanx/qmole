/*
 * gnome-keyring
 *
 * Copyright (C) 2008 Stefan Walter
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

#include "gkm-secret.h"

#include "egg/egg-secure-memory.h"

#include <string.h>

struct _GkmSecret {
	GObject parent;
	guchar *memory;
	gsize n_memory;
};

G_DEFINE_TYPE (GkmSecret, gkm_secret, G_TYPE_OBJECT);

/* -----------------------------------------------------------------------------
 * OBJECT
 */

static void
gkm_secret_init (GkmSecret *self)
{

}

static void
gkm_secret_dispose (GObject *obj)
{
	GkmSecret *self = GKM_SECRET (obj);

	egg_secure_clear (self->memory, self->n_memory);

	G_OBJECT_CLASS (gkm_secret_parent_class)->dispose (obj);
}

static void
gkm_secret_finalize (GObject *obj)
{
	GkmSecret *self = GKM_SECRET (obj);

	egg_secure_free (self->memory);
	self->memory = NULL;
	self->n_memory = 0;

	G_OBJECT_CLASS (gkm_secret_parent_class)->finalize (obj);
}

static void
gkm_secret_class_init (GkmSecretClass *klass)
{
	GObjectClass *gobject_class = G_OBJECT_CLASS (klass);
	gobject_class->dispose = gkm_secret_dispose;
	gobject_class->finalize = gkm_secret_finalize;
}

/* -----------------------------------------------------------------------------
 * PUBLIC
 */

GkmSecret*
gkm_secret_new (const guchar *data, gssize n_data)
{
	GkmSecret *secret = g_object_new (GKM_TYPE_SECRET, NULL);

	if (data) {
		if (n_data == -1) {
			secret->memory = (guchar*)egg_secure_strdup ((const gchar*)data);
			secret->n_memory = strlen ((const gchar*)data);
		} else {
			secret->memory = egg_secure_alloc (n_data + 1);
			memcpy (secret->memory, data, n_data);
			secret->n_memory = n_data;
		}
	} else {
		secret->memory = NULL;
		secret->n_memory = 0;
	}

	return secret;
}

GkmSecret*
gkm_secret_new_from_login (CK_UTF8CHAR_PTR pin, CK_ULONG n_pin)
{
	if (n_pin == (CK_ULONG)-1)
		return gkm_secret_new ((const guchar*)pin, -1);
	else
		return gkm_secret_new ((const guchar*)pin, (gssize)n_pin);
}

GkmSecret*
gkm_secret_new_from_password (const gchar *password)
{
	return gkm_secret_new ((const guchar*)password, -1);
}

const guchar*
gkm_secret_get (GkmSecret *self, gsize *n_data)
{
	g_return_val_if_fail (GKM_IS_SECRET (self), NULL);
	g_return_val_if_fail (n_data, NULL);
	*n_data = self->n_memory;
	return self->memory;
}

const gchar*
gkm_secret_get_password (GkmSecret *self, gsize *n_data)
{
	g_return_val_if_fail (GKM_IS_SECRET (self), NULL);
	g_return_val_if_fail (n_data, NULL);
	*n_data = self->n_memory;
	return (gchar*)self->memory;
}

gboolean
gkm_secret_equal (GkmSecret *self, GkmSecret *other)
{
	g_return_val_if_fail (GKM_IS_SECRET (self), FALSE);
	g_return_val_if_fail (GKM_IS_SECRET (other), FALSE);
	if (self == other)
		return TRUE;
	return gkm_secret_equals (self, other->memory, other->n_memory);
}

gboolean
gkm_secret_equals (GkmSecret *self, const guchar* pin, gssize n_pin)
{
	g_return_val_if_fail (GKM_IS_SECRET (self), FALSE);

	/* In case they're different somewhere */
	if (n_pin == (CK_ULONG)-1)
		n_pin = -1;

	if (n_pin == -1 && pin != NULL)
		n_pin = strlen ((const gchar*)pin);

	/* The same length */
	if (n_pin != self->n_memory)
		return FALSE;

	/* Two null passwords */
	if (!pin && !self->memory)
		return TRUE;

	/* For our purposes a null password equals an empty password */
	if (n_pin == 0)
		return TRUE;

	/* One null, one not null */
	if (!pin || !self->memory)
		return FALSE;

	/* Compare actual memory */
	return memcmp (pin, self->memory, n_pin) == 0;
}

gboolean
gkm_secret_is_trivially_weak (GkmSecret *self)
{
	return gkm_secret_equals (self, NULL, 0) ||
	       gkm_secret_equals (self, (const guchar*)"", 0);
}
