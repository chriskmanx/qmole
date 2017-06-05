/*
 * gnome-keyring
 *
 * Copyright (C) 2011 Collabora Ltd.
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
 *
 * Author: Stef Walter <stefw@collabora.co.uk>
 */

#include "config.h"

#include "gcr-record.h"
#include "gcr-collection.h"
#define DEBUG_FLAG GCR_DEBUG_GNUPG
#include "gcr-debug.h"
#include "gcr-gnupg-collection.h"
#include "gcr-gnupg-key.h"
#include "gcr-gnupg-process.h"
#include "gcr-gnupg-util.h"
#include "gcr-internal.h"
#include "gcr-util.h"

#include <sys/wait.h>
#include <string.h>

enum {
	PROP_0,
	PROP_DIRECTORY
};

struct _GcrGnupgCollectionPrivate {
	GHashTable *items;          /* char *keyid -> GcrGnupgKey* */
	gchar *directory;
};

/* Forward declarations */
static void _gcr_collection_iface (GcrCollectionIface *iface);

G_DEFINE_TYPE_WITH_CODE (GcrGnupgCollection, _gcr_gnupg_collection, G_TYPE_OBJECT,
	G_IMPLEMENT_INTERFACE (GCR_TYPE_COLLECTION, _gcr_collection_iface)
);

/* -----------------------------------------------------------------------------
 * OBJECT
 */

static void
_gcr_gnupg_collection_init (GcrGnupgCollection *self)
{
	self->pv = G_TYPE_INSTANCE_GET_PRIVATE (self, GCR_TYPE_GNUPG_COLLECTION,
	                                        GcrGnupgCollectionPrivate);

	self->pv->items = g_hash_table_new_full (g_str_hash, g_str_equal,
	                                         g_free, g_object_unref);
}

static void
_gcr_gnupg_collection_set_property (GObject *obj, guint prop_id, const GValue *value,
                                    GParamSpec *pspec)
{
	GcrGnupgCollection *self = GCR_GNUPG_COLLECTION (obj);

	switch (prop_id) {
	case PROP_DIRECTORY:
		g_return_if_fail (!self->pv->directory);
		self->pv->directory = g_value_dup_string (value);
		if (self->pv->directory && !g_path_is_absolute (self->pv->directory)) {
			g_warning ("gnupg collection directory path should be absolute: %s",
			           self->pv->directory);
		}
		break;
	default:
		G_OBJECT_WARN_INVALID_PROPERTY_ID (obj, prop_id, pspec);
		break;
	}
}

static void
_gcr_gnupg_collection_get_property (GObject *obj, guint prop_id, GValue *value,
                                    GParamSpec *pspec)
{
	GcrGnupgCollection *self = GCR_GNUPG_COLLECTION (obj);

	switch (prop_id) {
	case PROP_DIRECTORY:
		g_value_set_string (value, self->pv->directory);
		break;
	default:
		G_OBJECT_WARN_INVALID_PROPERTY_ID (obj, prop_id, pspec);
		break;
	}
}

static void
_gcr_gnupg_collection_dispose (GObject *obj)
{
	GcrGnupgCollection *self = GCR_GNUPG_COLLECTION (obj);

	g_hash_table_remove_all (self->pv->items);

	G_OBJECT_CLASS (_gcr_gnupg_collection_parent_class)->dispose (obj);
}

static void
_gcr_gnupg_collection_finalize (GObject *obj)
{
	GcrGnupgCollection *self = GCR_GNUPG_COLLECTION (obj);

	g_assert (self->pv->items);
	g_assert (g_hash_table_size (self->pv->items) == 0);
	g_hash_table_destroy (self->pv->items);
	self->pv->items = NULL;

	g_free (self->pv->directory);
	self->pv->directory = NULL;

	G_OBJECT_CLASS (_gcr_gnupg_collection_parent_class)->finalize (obj);
}

static void
_gcr_gnupg_collection_class_init (GcrGnupgCollectionClass *klass)
{
	GObjectClass *gobject_class = G_OBJECT_CLASS (klass);

	gobject_class->get_property = _gcr_gnupg_collection_get_property;
	gobject_class->set_property = _gcr_gnupg_collection_set_property;
	gobject_class->dispose = _gcr_gnupg_collection_dispose;
	gobject_class->finalize = _gcr_gnupg_collection_finalize;

	/**
	 * GcrGnupgCollection:directory:
	 *
	 * Directory to load the gnupg keys from, or %NULL for default
	 * ~/.gnupg/ directory.
	 */
	g_object_class_install_property (gobject_class, PROP_DIRECTORY,
	           g_param_spec_string ("directory", "Directory", "Gnupg Directory",
	                                NULL, G_PARAM_READWRITE | G_PARAM_CONSTRUCT_ONLY));

	g_type_class_add_private (gobject_class, sizeof (GcrGnupgCollectionPrivate));
	_gcr_initialize ();
}

static guint
gcr_gnupg_collection_real_get_length (GcrCollection *coll)
{
	GcrGnupgCollection *self = GCR_GNUPG_COLLECTION (coll);
	return g_hash_table_size (self->pv->items);
}

static GList*
gcr_gnupg_collection_real_get_objects (GcrCollection *coll)
{
	GcrGnupgCollection *self = GCR_GNUPG_COLLECTION (coll);
	return g_hash_table_get_values (self->pv->items);
}

static void
_gcr_collection_iface (GcrCollectionIface *iface)
{
	iface->get_length = gcr_gnupg_collection_real_get_length;
	iface->get_objects = gcr_gnupg_collection_real_get_objects;
}

/**
 * _gcr_gnupg_collection_new:
 * @directory: (allow-none): The gnupg home directory.
 *
 * Create a new GcrGnupgCollection.
 *
 * The gnupg home directory is where the keyring files live. If directory is
 * %NULL then the default gnupg home directory is used.
 *
 * Returns: (transfer full): A newly allocated collection.
 */
GcrCollection*
_gcr_gnupg_collection_new (const gchar *directory)
{
	return g_object_new (GCR_TYPE_GNUPG_COLLECTION,
	                     "directory", directory,
	                     NULL);
}

/*
 * We have to run the gnupg process twice to list the public and then the
 * secret keys. These phases are tracked by GcrLoadingPhase. If the first
 * phase completes successfully (using gpg --list-keys) then we move on to
 * the second phase where the secret keys are loaded (using gpg --list-secret-keys)
 *
 * If a key is loaded as a public key by the public phase, it can be updated by
 * the secret phase. A key discovered in the secret phase must have a public
 * counterpart already loaded by the public phase.
 */

typedef enum {
	GCR_LOADING_PHASE_PUBLIC = 1,
	GCR_LOADING_PHASE_SECRET = 2,
} GcrLoadingPhase;

/*
 * We use @difference to track the keys that were in the collection before
 * the load process, and then remove any not found, at the end of the load
 * process. Strings are directly used from collection->pv->items keys.
 */

typedef struct {
	GcrGnupgCollection *collection;       /* reffed pointer back to collection */
	GcrLoadingPhase loading_phase;        /* Whether loading public or private */
	GPtrArray *records;                   /* GcrRecord* not yet made into a key */
	GcrGnupgProcess *process;             /* The gnupg process itself */
	GCancellable *cancel;                 /* Cancellation for process */
	GString *out_data;                    /* Pending output not yet parsed into colons */
	GHashTable *difference;               /* Hashset gchar *keyid -> gchar *keyid */

	guint output_sig;
	guint error_sig;
	guint status_sig;
	guint attribute_sig;

	GQueue *attribute_queue;              /* Queue of unprocessed GcrRecord* status records */
	GByteArray *attribute_buf;            /* Buffer of unprocessed attribute data received */
	GHashTable *attributes;               /* Processed attributes waiting for a matching key */
} GcrGnupgCollectionLoad;

/* Forward declarations */
static void spawn_gnupg_list_process (GcrGnupgCollectionLoad *load, GSimpleAsyncResult *res);

static void
_gcr_gnupg_collection_load_free (gpointer data)
{
	GcrGnupgCollectionLoad *load = data;
	g_assert (load);

	g_ptr_array_unref (load->records);
	g_string_free (load->out_data, TRUE);
	g_hash_table_destroy (load->difference);
	g_object_unref (load->collection);

	if (load->process) {
		if (load->output_sig)
			g_signal_handler_disconnect (load->process, load->output_sig);
		if (load->error_sig)
			g_signal_handler_disconnect (load->process, load->error_sig);
		if (load->status_sig)
			g_signal_handler_disconnect (load->process, load->status_sig);
		if (load->attribute_sig)
			g_signal_handler_disconnect (load->process, load->attribute_sig);
		g_object_unref (load->process);
	}

	if (load->cancel)
		g_object_unref (load->cancel);

	if (load->attribute_queue) {
		while (!g_queue_is_empty (load->attribute_queue))
			_gcr_record_free (g_queue_pop_head (load->attribute_queue));
		g_queue_free (load->attribute_queue);
	}
	if (load->attribute_buf)
		g_byte_array_unref (load->attribute_buf);
	if (load->attributes)
		g_hash_table_destroy (load->attributes);

	g_slice_free (GcrGnupgCollectionLoad, load);
}

static void
process_records_as_public_key (GcrGnupgCollectionLoad *load, GPtrArray *records,
                               const gchar *keyid)
{
	GPtrArray *attr_records = NULL;
	const gchar *fingerprint;
	gchar *orig_fingerprint;
	GcrGnupgKey *key;
	guint i;

	/* Add in any attributes we have loaded */
	fingerprint = _gcr_gnupg_key_get_fingerprint_for_records (records);
	if (fingerprint && load->attributes)
		attr_records = g_hash_table_lookup (load->attributes, fingerprint);
	if (attr_records) {
		_gcr_debug ("adding %d user id attribute(s) to key/fingerprint: %s/%s",
		            (gint)attr_records->len, keyid, fingerprint);

		if (!g_hash_table_lookup_extended (load->attributes, fingerprint,
		                                   (gpointer*)&orig_fingerprint, NULL))
			g_assert_not_reached ();
		if (!g_hash_table_steal (load->attributes, fingerprint))
			g_assert_not_reached ();
		g_free (orig_fingerprint);

		/* Move all the attribute records over to main records set */
		for (i = 0; i < attr_records->len; i++)
			g_ptr_array_add (records, attr_records->pdata[i]);

		/* Shallow free of attr_records array */
		g_free (g_ptr_array_free (attr_records, FALSE));
	}

	/* Note that we've seen this keyid */
	g_hash_table_remove (load->difference, keyid);

	key = g_hash_table_lookup (load->collection->pv->items, keyid);

	/* Already have this key, just update */
	if (key) {
		_gcr_debug ("updating public key: %s", keyid);
		_gcr_gnupg_key_set_public_records (key, records);

	/* Add a new key */
	} else {
		key = _gcr_gnupg_key_new (records, NULL);
		_gcr_debug ("creating public key: %s", keyid);
		g_hash_table_insert (load->collection->pv->items, g_strdup (keyid), key);
		gcr_collection_emit_added (GCR_COLLECTION (load->collection), G_OBJECT (key));
	}
}

static void
process_records_as_secret_key (GcrGnupgCollectionLoad *load, GPtrArray *records,
                               const gchar *keyid)
{
	GcrGnupgKey *key;

	key = g_hash_table_lookup (load->collection->pv->items, keyid);

	/* Don't have this key */
	if (key == NULL) {
		g_message ("Secret key seen but no public key for: %s", keyid);

	/* Tell the private key that it's a secret one */
	} else {
		_gcr_debug ("adding secret records to key: %s", keyid);
		_gcr_gnupg_key_set_secret_records (key, records);
	}
}

static void
process_records_as_key (GcrGnupgCollectionLoad *load)
{
	GPtrArray *records;
	const gchar *keyid;
	GQuark schema;

	g_assert (load->records->len);

	records = load->records;
	load->records = g_ptr_array_new_with_free_func (_gcr_record_free);

	keyid = _gcr_gnupg_key_get_keyid_for_records (records);
	if (keyid) {
		schema = _gcr_record_get_schema (records->pdata[0]);

		/* A public key */
		if (schema == GCR_RECORD_SCHEMA_PUB)
			process_records_as_public_key (load, records, keyid);

		/* A secret key */
		else if (schema == GCR_RECORD_SCHEMA_SEC)
			process_records_as_secret_key (load, records, keyid);

		else
			g_assert_not_reached ();

	} else {
		g_warning ("parsed gnupg data had no keyid");
	}

	g_ptr_array_unref (records);
}

static gboolean
process_outstanding_attribute (GcrGnupgCollectionLoad *load, GcrRecord *record)
{
	const gchar *fingerprint;
	GPtrArray *records;
	GcrRecord *xa1;
	guint length;

	if (!_gcr_record_get_uint (record, GCR_RECORD_ATTRIBUTE_LENGTH, &length))
		g_return_val_if_reached (FALSE);
	fingerprint = _gcr_record_get_raw (record, GCR_RECORD_ATTRIBUTE_FINGERPRINT);
	g_return_val_if_fail (fingerprint != NULL, FALSE);

	/* Do we have enough data for this attribute? */
	if (!load->attribute_buf || load->attribute_buf->len < length) {
		_gcr_debug ("not enough attribute data in buffer: %u", length);
		return FALSE;
	}

	if (!load->attributes)
		load->attributes = g_hash_table_new_full (g_str_hash, g_str_equal,
							  g_free, (GDestroyNotify)g_ptr_array_unref);

	records = g_hash_table_lookup (load->attributes, fingerprint);
	if (!records) {
		records = g_ptr_array_new_with_free_func (_gcr_record_free);
		g_hash_table_insert (load->attributes, g_strdup (fingerprint), records);
	}

	_gcr_debug ("new attribute of length %d for key with fingerprint %s",
		    length, fingerprint);

	xa1 = _gcr_gnupg_build_xa1_record (record, load->attribute_buf->data, length);
	g_ptr_array_add (records, xa1);

	/* Did we use up all the attribute data? Get rid of the buffer */
	if (length == load->attribute_buf->len) {
		g_byte_array_unref (load->attribute_buf);
		load->attribute_buf = NULL;

	/* Otherwise clear out the used data from buffer */
	} else {
		g_byte_array_remove_range (load->attribute_buf, 0, length);
	}

	return TRUE;
}

static void
process_outstanding_attributes (GcrGnupgCollectionLoad *load)
{
	GcrRecord *record;

	if (load->attribute_queue == NULL)
		return;

	_gcr_debug ("%d outstanding attribute records",
	            (gint)g_queue_get_length (load->attribute_queue));

	for (;;) {
		record = g_queue_peek_head (load->attribute_queue);
		if (record == NULL)
			break;
		if (!process_outstanding_attribute (load, record))
			break;
		g_queue_pop_head (load->attribute_queue);
		_gcr_record_free (record);
	}
}

static void
on_line_parse_output (const gchar *line, gpointer user_data)
{
	GcrGnupgCollectionLoad *load = user_data;
	GcrRecord *record;
	GQuark schema;

	_gcr_debug ("output: %s", line);

	record = _gcr_record_parse_colons (line, -1);
	if (!record) {
		g_warning ("invalid gnupg output line: %s", line);
		return;
	}

	schema = _gcr_record_get_schema (record);

	/*
	 * Each time we see a line with 'pub' or 'sec' schema we assume that
	 * it's a new key being listed.
	 */
	if (schema == GCR_RECORD_SCHEMA_PUB || schema == GCR_RECORD_SCHEMA_SEC) {
		_gcr_debug ("start of new key");
		if (load->records->len)
			process_records_as_key (load);
		g_assert (!load->records->len);
		g_ptr_array_add (load->records, record);
		record = NULL;

	/*
	 * 'uid' and 'fpr' schema lines get added to the key that came before.
	 */
	} else if (schema == GCR_RECORD_SCHEMA_UID ||
	           schema == GCR_RECORD_SCHEMA_FPR) {
		if (load->records->len) {
			g_ptr_array_add (load->records, record);
			record = NULL;
		}
	}

	if (record != NULL)
		_gcr_record_free (record);
}


static void
on_gnupg_process_output_data (GcrGnupgProcess *process, GByteArray *buffer,
                              gpointer user_data)
{
	GSimpleAsyncResult *res = G_SIMPLE_ASYNC_RESULT (user_data);
	GcrGnupgCollectionLoad *load = g_simple_async_result_get_op_res_gpointer (res);

	g_string_append_len (load->out_data, (gchar*)buffer->data, buffer->len);
	_gcr_util_parse_lines (load->out_data, FALSE, on_line_parse_output, load);
}

static void
on_gnupg_process_error_line (GcrGnupgProcess *process, const gchar *line,
                             gpointer user_data)
{
	g_printerr ("%s\n", line);
}

static void
on_gnupg_process_status_record (GcrGnupgProcess *process, GcrRecord *record,
                                gpointer user_data)
{
	GSimpleAsyncResult *res = G_SIMPLE_ASYNC_RESULT (user_data);
	GcrGnupgCollectionLoad *load = g_simple_async_result_get_op_res_gpointer (res);

	if (GCR_RECORD_SCHEMA_ATTRIBUTE != _gcr_record_get_schema (record))
		return;

	if (!load->attribute_queue)
		load->attribute_queue = g_queue_new ();

	g_queue_push_tail (load->attribute_queue, _gcr_record_copy (record));
	process_outstanding_attributes (load);
}

static void
on_gnupg_process_attribute_data (GcrGnupgProcess *process, GByteArray *buffer,
                                 gpointer user_data)
{
	GSimpleAsyncResult *res = G_SIMPLE_ASYNC_RESULT (user_data);
	GcrGnupgCollectionLoad *load = g_simple_async_result_get_op_res_gpointer (res);

	/* If we don't have a buffer, just claim this one */
	if (!load->attribute_buf)
		load->attribute_buf = g_byte_array_ref (buffer);

	/* If we have data remaining over, add it to our buffer */
	else
		g_byte_array_append (load->attribute_buf, buffer->data, buffer->len);

	process_outstanding_attributes (load);
}

static void
on_gnupg_process_completed (GObject *source, GAsyncResult *result, gpointer user_data)
{
	GSimpleAsyncResult *res = G_SIMPLE_ASYNC_RESULT (user_data);
	GcrGnupgCollectionLoad *load = g_simple_async_result_get_op_res_gpointer (res);
	GHashTableIter iter;
	GError *error = NULL;
	GObject *object;
	gpointer keyid;

	if (!_gcr_gnupg_process_run_finish (GCR_GNUPG_PROCESS (source), result, &error)) {
		g_simple_async_result_set_from_error (res, error);
		g_simple_async_result_complete (res);
		g_object_unref (res);
		g_clear_error (&error);
		return;
	}

	/* Process any remaining output */
	_gcr_util_parse_lines (load->out_data, TRUE, on_line_parse_output, load);

	/* Process last bit as a key, if any */
	if (load->records->len)
		process_records_as_key (load);

	/* If we completed loading public keys, then go and load secret */
	switch (load->loading_phase) {
	case GCR_LOADING_PHASE_PUBLIC:
		_gcr_debug ("public load phase completed");
		load->loading_phase = GCR_LOADING_PHASE_SECRET;
		spawn_gnupg_list_process (load, res);
		g_object_unref (res);
		return;
	case GCR_LOADING_PHASE_SECRET:
		_gcr_debug ("secret load phase completed");
		/* continue below */
		break;
	default:
		g_assert_not_reached ();
	}

	/* Remove any keys that we still have in the difference */
	g_hash_table_iter_init (&iter, load->difference);
	while (g_hash_table_iter_next (&iter, &keyid, NULL)) {
		object = g_hash_table_lookup (load->collection->pv->items, keyid);
		if (object != NULL) {
			g_object_ref (object);
			_gcr_debug ("removing key no longer present in keyring: %s", (gchar*)keyid);
			g_hash_table_remove (load->collection->pv->items, keyid);
			gcr_collection_emit_removed (GCR_COLLECTION (load->collection), object);
			g_object_unref (object);
		}
	}

	g_simple_async_result_complete (res);
	g_object_unref (res);
}

static void
spawn_gnupg_list_process (GcrGnupgCollectionLoad *load, GSimpleAsyncResult *res)
{
	GcrGnupgProcessFlags flags = 0;
	GPtrArray *argv;

	argv = g_ptr_array_new ();

	switch (load->loading_phase) {
	case GCR_LOADING_PHASE_PUBLIC:
		_gcr_debug ("starting public load phase");
		g_ptr_array_add (argv, (gpointer)"--list-keys");
		/* Load photos in public phase */
		flags = GCR_GNUPG_PROCESS_WITH_ATTRIBUTES |
		        GCR_GNUPG_PROCESS_WITH_STATUS;
		break;
	case GCR_LOADING_PHASE_SECRET:
		_gcr_debug ("starting secret load phase");
		g_ptr_array_add (argv, (gpointer)"--list-secret-keys");
		break;
	default:
		g_assert_not_reached ();
	}

	g_ptr_array_add (argv, (gpointer)"--fixed-list-mode");
	g_ptr_array_add (argv, (gpointer)"--with-colons");
	g_ptr_array_add (argv, (gpointer)"--with-fingerprint");
	g_ptr_array_add (argv, NULL);

	/* res is unreffed in on_gnupg_process_completed */
	_gcr_gnupg_process_run_async (load->process, (const gchar**)argv->pdata, NULL, flags,
	                              load->cancel, on_gnupg_process_completed,
	                              g_object_ref (res));

	g_ptr_array_unref (argv);
}

/**
 * _gcr_gnupg_collection_load_async:
 * @self: The collection
 * @cancellable: Cancellation object or %NULL
 * @callback: Callback to call when result is ready
 * @user_data: Data for callback
 *
 * Start an operation to load or reload the list of gnupg keys in this
 * collection.
 */
void
_gcr_gnupg_collection_load_async (GcrGnupgCollection *self, GCancellable *cancellable,
                                  GAsyncReadyCallback callback, gpointer user_data)
{
	GSimpleAsyncResult *res;
	GcrGnupgCollectionLoad *load;
	GHashTableIter iter;
	gpointer keyid;

	g_return_if_fail (GCR_IS_GNUPG_COLLECTION (self));

	/* TODO: Cancellation not yet implemented */

	res = g_simple_async_result_new (G_OBJECT (self), callback, user_data,
	                                 _gcr_gnupg_collection_load_async);

	load = g_slice_new0 (GcrGnupgCollectionLoad);
	load->records = g_ptr_array_new_with_free_func (_gcr_record_free);
	load->out_data = g_string_sized_new (1024);
	load->collection = g_object_ref (self);
	load->cancel = cancellable ? g_object_ref (cancellable) : cancellable;

	load->process = _gcr_gnupg_process_new (self->pv->directory, NULL);
	load->output_sig = g_signal_connect (load->process, "output-data", G_CALLBACK (on_gnupg_process_output_data), res);
	load->error_sig = g_signal_connect (load->process, "error-line", G_CALLBACK (on_gnupg_process_error_line), res);
	load->status_sig = g_signal_connect (load->process, "status-record", G_CALLBACK (on_gnupg_process_status_record), res);
	load->attribute_sig = g_signal_connect (load->process, "attribute-data", G_CALLBACK (on_gnupg_process_attribute_data), res);

	/*
	 * Track all the keys we currently have, at end remove those that
	 * didn't get listed by the gpg process.
	 */
	load->difference = g_hash_table_new (g_str_hash, g_str_equal);
	g_hash_table_iter_init (&iter, self->pv->items);
	while (g_hash_table_iter_next (&iter, &keyid, NULL))
		g_hash_table_insert (load->difference, keyid, keyid);

	g_simple_async_result_set_op_res_gpointer (res, load,
	                                           _gcr_gnupg_collection_load_free);

	load->loading_phase = GCR_LOADING_PHASE_PUBLIC;
	spawn_gnupg_list_process (load, res);

	g_object_unref (res);
}

/**
 * _gcr_gnupg_collection_load_finish:
 * @self: The collection
 * @result: The result passed to the callback
 * @error: Location to raise an error on failure.
 *
 * Get the result of an operation to load or reload the list of gnupg keys
 * in this collection.
 */
gboolean
_gcr_gnupg_collection_load_finish (GcrGnupgCollection *self, GAsyncResult *result,
                                   GError **error)
{
	g_return_val_if_fail (GCR_IS_GNUPG_COLLECTION (self), FALSE);
	g_return_val_if_fail (!error || !*error, FALSE);

	g_return_val_if_fail (g_simple_async_result_is_valid (result, G_OBJECT (self),
	                      _gcr_gnupg_collection_load_async), FALSE);

	if (g_simple_async_result_propagate_error (G_SIMPLE_ASYNC_RESULT (result), error))
		return FALSE;

	return TRUE;
}
