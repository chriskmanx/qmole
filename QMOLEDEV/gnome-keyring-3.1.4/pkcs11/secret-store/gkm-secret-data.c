/*
 * gnome-keyring
 *
 * Copyright (C) 2009 Stefan Walter
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

#include "gkm-secret-data.h"

#include "gkm/gkm-secret.h"
#include "gkm/gkm-transaction.h"
#include "gkm/gkm-util.h"

#include "egg/egg-secure-memory.h"

#include <glib/gi18n.h>

struct _GkmSecretData {
	GObject parent;
	GHashTable *secrets;
	GkmSecret *master;
};

G_DEFINE_TYPE (GkmSecretData, gkm_secret_data, G_TYPE_OBJECT);

/* -----------------------------------------------------------------------------
 * INTERNAL
 */

typedef struct _set_secret_args {
	gchar *identifier;
	GkmSecret *old_secret;
} set_secret_args;

static gboolean
complete_set_secret (GkmTransaction *transaction, GObject *obj, gpointer user_data)
{
	GkmSecretData *self = GKM_SECRET_DATA (obj);
	set_secret_args *args = user_data;

	/* If the transaction failed, revert */
	if (gkm_transaction_get_failed (transaction)) {
		if (!args->old_secret) {
			g_hash_table_remove (self->secrets, args->identifier);
		} else {
			g_hash_table_replace (self->secrets, args->identifier, args->old_secret);
			args->identifier = NULL; /* hash table took ownership */
			args->old_secret = NULL; /* ditto */
		}
	}

	/* Free up transaction data */
	g_free (args->identifier);
	if (args->old_secret)
		g_object_unref (args->old_secret);
	g_slice_free (set_secret_args, args);

	return TRUE;
}

static void
begin_set_secret (GkmSecretData *self, GkmTransaction *transaction,
                  const gchar *identifier, GkmSecret *secret)
{
	set_secret_args *args;

	g_assert (GKM_IS_SECRET_DATA (self));
	g_assert (!gkm_transaction_get_failed (transaction));
	g_assert (identifier);
	g_assert (GKM_IS_SECRET (secret));

	args = g_slice_new0 (set_secret_args);

	/* Take ownership of the old data, if present */
	if (g_hash_table_lookup_extended (self->secrets, identifier,
	                                  (gpointer*)&args->identifier,
	                                  (gpointer*)&args->old_secret)) {
		if (!g_hash_table_steal (self->secrets, args->identifier))
			g_assert_not_reached ();
	} else {
		args->identifier = g_strdup (identifier);
	}

	/* Replace with our new data */
	g_hash_table_replace (self->secrets, g_strdup (identifier),
	                      g_object_ref (secret));

	/* Track in the transaction */
	gkm_transaction_add (transaction, self, complete_set_secret, args);
}

/* -----------------------------------------------------------------------------
 * OBJECT
 */

static void
gkm_secret_data_init (GkmSecretData *self)
{
	self->secrets = g_hash_table_new_full (g_str_hash, g_str_equal, g_free, g_object_unref);
}

static void
gkm_secret_data_finalize (GObject *obj)
{
	GkmSecretData *self = GKM_SECRET_DATA (obj);

	if (self->secrets)
		g_hash_table_destroy (self->secrets);
	self->secrets = NULL;

	if (self->master)
		g_object_unref (self->master);
	self->master = NULL;

	G_OBJECT_CLASS (gkm_secret_data_parent_class)->finalize (obj);
}

static void
gkm_secret_data_class_init (GkmSecretDataClass *klass)
{
	GObjectClass *gobject_class = G_OBJECT_CLASS (klass);
	gkm_secret_data_parent_class = g_type_class_peek_parent (klass);
	gobject_class->finalize = gkm_secret_data_finalize;
}

/* -----------------------------------------------------------------------------
 * PUBLIC
 */

GkmSecret*
gkm_secret_data_get_secret (GkmSecretData *self, const gchar *identifier)
{
	g_return_val_if_fail (GKM_IS_SECRET_DATA (self), NULL);
	g_return_val_if_fail (identifier, NULL);
	return g_hash_table_lookup (self->secrets, identifier);
}

const guchar*
gkm_secret_data_get_raw (GkmSecretData *self, const gchar *identifier,
                         gsize *n_result)
{
	GkmSecret *secret;

	g_return_val_if_fail (GKM_IS_SECRET_DATA (self), NULL);
	g_return_val_if_fail (identifier, NULL);
	g_return_val_if_fail (n_result, NULL);

	secret = gkm_secret_data_get_secret (self, identifier);
	if (secret == NULL)
		return NULL;

	return gkm_secret_get (secret, n_result);
}


void
gkm_secret_data_set_secret (GkmSecretData *self, const gchar *identifier,
                            GkmSecret *secret)
{
	g_return_if_fail (GKM_IS_SECRET_DATA (self));
	g_return_if_fail (identifier);
	g_return_if_fail (GKM_IS_SECRET (secret));
	g_hash_table_replace (self->secrets, g_strdup (identifier),
	                      g_object_ref (secret));
}

void
gkm_secret_data_set_transacted  (GkmSecretData *self, GkmTransaction *transaction,
                                 const gchar *identifier, GkmSecret *secret)
{
	g_return_if_fail (GKM_IS_SECRET_DATA (self));
	g_return_if_fail (GKM_IS_TRANSACTION (transaction));
	g_return_if_fail (!gkm_transaction_get_failed (transaction));
	g_return_if_fail (identifier);
	g_return_if_fail (GKM_IS_SECRET (secret));

	begin_set_secret (self, transaction, identifier, secret);
}

void
gkm_secret_data_remove_secret (GkmSecretData *self, const gchar *identifier)
{
	g_return_if_fail (GKM_IS_SECRET_DATA (self));
	g_return_if_fail (identifier);
	g_hash_table_remove (self->secrets, identifier);
}

GkmSecret*
gkm_secret_data_get_master (GkmSecretData *self)
{
	g_return_val_if_fail (GKM_IS_SECRET_DATA (self), NULL);
	return self->master;
}

void
gkm_secret_data_set_master (GkmSecretData *self, GkmSecret *master)
{
	g_return_if_fail (GKM_IS_SECRET_DATA (self));
	g_return_if_fail (!master || GKM_IS_SECRET (master));

	if (master)
		g_object_ref (master);
	if (self->master)
		g_object_unref (self->master);
	self->master = master;
}
