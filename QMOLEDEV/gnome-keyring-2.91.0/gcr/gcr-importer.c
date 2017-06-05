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

#include "gcr-import-dialog.h"
#include "gcr-importer.h"
#include "gcr-internal.h"
#include "gcr-parser.h"

#include <glib/gi18n-lib.h>

enum {
	PROP_0,
	PROP_SLOT,
	PROP_PARSER,
	PROP_PROMPT_BEHAVIOR
};

enum {
	IMPORTED,
	LAST_SIGNAL
};

static guint signals[LAST_SIGNAL] = { 0 };

struct _GcrImporterPrivate {
	GckSlot *slot;
	GcrParser *parser;
	GcrImporterPromptBehavior behavior;
	
	/* Information about last import */
	GError *error;
	gboolean succeeded;
	
	/* State data during import */
	gboolean processing;
	GCancellable *cancel;
	GInputStream *input;
	gboolean prompted;
	gboolean async;
	GByteArray *buffer;
	GckSession *session;
	GQueue queue;
	
	/* Extra async stuff */
	GAsyncReadyCallback callback;
	gpointer user_data;
};

/* State forward declarations */
static void state_cancelled (GcrImporter *self, gboolean async);
static void state_complete (GcrImporter *self, gboolean async);
static void state_create_object (GcrImporter *self, gboolean async);
static void state_open_session (GcrImporter *self, gboolean async);
static void state_initialize_pin (GcrImporter *self, gboolean async);
static void state_parse_buffer (GcrImporter *self, gboolean async);
static void state_read_buffer (GcrImporter *self, gboolean async);

static void gcr_importer_async_result (GAsyncResultIface *iface);
G_DEFINE_TYPE_WITH_CODE (GcrImporter, gcr_importer, G_TYPE_OBJECT,
                         G_IMPLEMENT_INTERFACE (G_TYPE_ASYNC_RESULT, gcr_importer_async_result));

#define BLOCK 4096

/* -----------------------------------------------------------------------------
 * INTERNAL 
 */

static void
cleanup_state_data (GcrImporter *self)
{
	GckAttributes *attrs;
	
	if (self->pv->buffer)
		g_byte_array_free (self->pv->buffer, TRUE);
	self->pv->buffer = NULL;
	
	if (self->pv->session)
		g_object_unref (self->pv->session);
	self->pv->session = NULL;
	
	while ((attrs = g_queue_pop_head (&self->pv->queue)) != NULL)
		gck_attributes_unref (attrs);
	g_assert (g_queue_is_empty (&self->pv->queue));
	
	if (self->pv->input)
		g_object_unref (self->pv->input);
	self->pv->input = NULL;
	
	if (self->pv->cancel)
		g_object_unref (self->pv->cancel);
	self->pv->cancel = NULL;
}

static void
cleanup_import_data (GcrImporter *self)
{
	if (self->pv->error)
		g_clear_error (&self->pv->error);
	self->pv->succeeded = TRUE;
}

static void
next_state (GcrImporter *self, void (*state) (GcrImporter*, gboolean))
{
	g_assert (GCR_IS_IMPORTER (self));
	g_assert (self->pv->processing);
	g_assert (state);
	
	if (self->pv->cancel && g_cancellable_is_cancelled (self->pv->cancel))
		state = state_cancelled;
	
	(state) (self, self->pv->async);
}

/* ---------------------------------------------------------------------------------
 * COMPLETE
 */

static void
state_complete (GcrImporter *self, gboolean async)
{
	if (async && self->pv->callback != NULL)
		(self->pv->callback) (G_OBJECT (self), G_ASYNC_RESULT (self), self->pv->user_data);
	
	cleanup_state_data (self);
	self->pv->processing = FALSE;
}

static void
state_failure (GcrImporter *self, gboolean async)
{
	self->pv->succeeded = FALSE;
	next_state (self, state_complete);
}

static void
state_cancelled (GcrImporter *self, gboolean async)
{
	if (self->pv->cancel && g_cancellable_is_cancelled (self->pv->cancel))
		g_cancellable_cancel (self->pv->cancel);
	if (self->pv->error)
		g_error_free (self->pv->error);
	self->pv->error = g_error_new_literal (GCR_DATA_ERROR, GCR_ERROR_CANCELLED, _("The operation was cancelled"));
	next_state (self, state_failure);
}

/* ---------------------------------------------------------------------------------
 * CREATE OBJECTS
 */

static void
complete_create_object (GcrImporter *self, GckObject *object, GError *error)
{
	if (object == NULL) {
		g_propagate_error (&self->pv->error, error);
		next_state (self, state_failure);
		
	} else {
		g_signal_emit (self, signals[IMPORTED], 0, object);
		g_object_unref (object);
		next_state (self, state_create_object);
	}
}

static void
on_create_object (GObject *obj, GAsyncResult *res, gpointer user_data)
{
	GError *error = NULL;
	GckObject *object = gck_session_create_object_finish (GCK_SESSION (obj), res, &error);
	complete_create_object (GCR_IMPORTER (user_data), object, error);
}

static void
state_create_object (GcrImporter *self, gboolean async)
{
	GckAttributes *attrs;
	GckObject *object;
	GError *error = NULL;
	
	/* No more objects */
	if (g_queue_is_empty (&self->pv->queue)) {
		next_state (self, state_complete);
		
	} else {
		
		/* Pop first one off the list */
		attrs = g_queue_pop_head (&self->pv->queue);
		g_assert (attrs);

		gck_attributes_add_boolean (attrs, CKA_TOKEN, CK_TRUE);

		if (async) {
			gck_session_create_object_async (self->pv->session, attrs, self->pv->cancel,
			                                  on_create_object, self);
		} else {
			object = gck_session_create_object (self->pv->session, attrs, self->pv->cancel, &error);
			complete_create_object (self, object, error);
		}
	
		gck_attributes_unref (attrs);
	}
}

/* ---------------------------------------------------------------------------------
 * OPEN SESSION
 */

static void
complete_open_session (GcrImporter *self, GckSession *session, GError *error)
{
	if (!session) {
		g_propagate_error (&self->pv->error, error);
		next_state (self, state_failure);
	} else {
		self->pv->session = session;
		next_state (self, state_create_object);
	}
}

static void
on_open_session (GObject *obj, GAsyncResult *res, gpointer user_data)
{
	GError *error = NULL;
	GckSession *session = gck_slot_open_session_finish (GCK_SLOT (obj), res, &error);
	complete_open_session (GCR_IMPORTER (user_data), session, error);
}

static void
state_open_session (GcrImporter *self, gboolean async)
{
	GckSession *session;
	GError *error = NULL;
	
	if (!self->pv->slot) {
		g_set_error (&self->pv->error, GCR_DATA_ERROR, GCR_ERROR_FAILURE, _("No location available to import to"));
		next_state (self, state_failure);
		
	} else {
		
		if (async) {
			gck_slot_open_session_async (self->pv->slot, GCK_SESSION_READ_WRITE, self->pv->cancel,
			                             on_open_session, self);
		} else {
			session = gck_slot_open_session_full (self->pv->slot, GCK_SESSION_READ_WRITE, 0, NULL, NULL,
			                                      self->pv->cancel, &error);
			complete_open_session (self, session, error);
		}
	}
}

/* ---------------------------------------------------------------------------------
 * INITIALIZE TOKEN
 * 
 * HACK: This is a big temporary hack to get, until the next version 
 * when we can fix this correctly.  
 */

static CK_RV
hacky_perform_initialize_pin (GckSlot *slot)
{
	CK_FUNCTION_LIST_PTR funcs;
	CK_SESSION_HANDLE session;
	CK_SLOT_ID slot_id;
	CK_RV rv;
	
	/* 
	 * This hack only works when:
	 *  
	 *  - Module is protected authentication path
	 *  - No other sessions are open.
	 *  
	 *  Thankfully this is the case with gnome-keyring-daemon and 
	 *  the gnome-keyring tool. 
	 */
	
	funcs = gck_module_get_functions (gck_slot_get_module (slot));
	g_return_val_if_fail (funcs, CKR_GENERAL_ERROR);
	slot_id = gck_slot_get_handle (slot);
	
	rv = funcs->C_OpenSession (slot_id, CKF_RW_SESSION | CKF_SERIAL_SESSION, NULL, NULL, &session);
	if (rv != CKR_OK)
		return rv;
	
	rv = funcs->C_Login (session, CKU_SO, NULL, 0);
	if (rv == CKR_OK) {
		rv = funcs->C_InitPIN (session, NULL, 0);
		funcs->C_Logout (session);
	}
	
	funcs->C_CloseSession (session);
	
	return rv;
}

static void
state_initialize_pin (GcrImporter *self, gboolean async)
{
	GckTokenInfo *info;
	gboolean initialize;
	CK_RV rv;
	
	g_assert (GCR_IS_IMPORTER (self));
	
	/* HACK: Doesn't function when async */
	if (!async) {
		g_return_if_fail (self->pv->slot);
		info = gck_slot_get_token_info (self->pv->slot);
		g_return_if_fail (info);
	
		initialize = !(info->flags & CKF_USER_PIN_INITIALIZED);
		gck_token_info_free (info);

		if (initialize) {
			rv = hacky_perform_initialize_pin (self->pv->slot);
			if (rv != CKR_OK) {
				g_propagate_error (&self->pv->error, g_error_new (GCK_ERROR, rv, "%s", gck_message_from_rv (rv)));
				next_state (self, state_failure);
				return;
			} 
		}
	}

	next_state (self, state_open_session);
}

/* ---------------------------------------------------------------------------------
 * IMPORT PROMPT
 */

static void
complete_import_prompt (GcrImporter *self, GcrImportDialog *dialog, gint response)
{
	GckSlot *slot;

	gtk_widget_hide (GTK_WIDGET (dialog));
	self->pv->prompted = TRUE;

	/* No dialog or dialog completed */
	if (response == GTK_RESPONSE_OK) {

		slot = _gcr_import_dialog_get_selected_slot (dialog);
		gcr_importer_set_slot (self, slot);
		next_state (self, state_initialize_pin);
		
	/* The dialog was cancelled or closed */
	} else {
		next_state (self, state_cancelled);
	}
}

static void
on_prompt_response (GtkDialog *dialog, gint response, gpointer user_data)
{
	complete_import_prompt (GCR_IMPORTER (user_data), GCR_IMPORT_DIALOG (dialog), response);
	g_object_unref (dialog);
}

static void 
state_import_prompt (GcrImporter *self, gboolean async)
{
	GcrImportDialog *dialog;
	gboolean prompt;
	gint response;
	
	g_assert (GCR_IS_IMPORTER (self));
	
	/* No need to prompt */
	if (self->pv->prompted == TRUE)
		prompt = FALSE;
	else if (self->pv->behavior == GCR_IMPORTER_PROMPT_ALWAYS)
		prompt = TRUE;
	else if (self->pv->behavior == GCR_IMPORTER_PROMPT_NEVER)
		prompt = FALSE;
	else 
		prompt = self->pv->slot ? FALSE : TRUE;
	
	if (prompt == FALSE) {
		next_state (self, state_initialize_pin);
		
	} else {
		
		dialog = _gcr_import_dialog_new ();

		_gcr_import_dialog_set_primary_text (dialog, _("Import Certificates/Keys"));
		_gcr_import_dialog_hide_password (dialog);
		
		if (self->pv->slot) {
			_gcr_import_dialog_set_selected_slot (dialog, self->pv->slot);
			_gcr_import_dialog_hide_selected_slot (dialog);
		} else {
			_gcr_import_dialog_set_secondary_text (dialog, _("Choose a location to store the imported certificates/keys."));
		}
			
		/* Prompt without blocking main loop */
		if (async) {
			g_signal_connect (dialog, "response", G_CALLBACK (on_prompt_response), self);
			gtk_widget_show (GTK_WIDGET (dialog));
			
		/* Block mainloop */
		} else {
			response = gtk_dialog_run (GTK_DIALOG (dialog));
			complete_import_prompt (self, dialog, response);
			g_object_unref (dialog);
		}
	}
}

/* ---------------------------------------------------------------------------------
 * PARSING
 */

static const gchar*
prepare_auth_primary (CK_OBJECT_CLASS klass)
{
	if (klass == CKO_PRIVATE_KEY)
		return _("Enter password to unlock the private key");
	else if (klass == CKO_CERTIFICATE)
		return _("Enter password to unlock the certificate");
	else 
		return _("Enter password to unlock");
}

static gchar*
prepare_auth_secondary (CK_OBJECT_CLASS klass, const gchar *label)
{
	if (label == NULL) {
		if (klass == CKO_PRIVATE_KEY) {
			/* TRANSLATORS: The key is locked. */
			return g_strdup (_("In order to import the private key, it must be unlocked"));
		} else if (klass == CKO_CERTIFICATE) {
			/* TRANSLATORS: The certificate is locked. */
			return g_strdup (_("In order to import the certificate, it must be unlocked"));
		} else {
			/* TRANSLATORS: The data is locked. */
			return g_strdup (_("In order to import the data, it must be unlocked"));
		}
	} else {
		if (klass == CKO_PRIVATE_KEY) {
			/* TRANSLATORS: The key is locked. */
			return g_strdup_printf (_("In order to import the private key '%s', it must be unlocked"), label);
		} else if (klass == CKO_CERTIFICATE) {
			/* TRANSLATORS: The certificate is locked. */
			return g_strdup_printf (_("In order to import the certificate '%s', it must be unlocked"), label);
		} else {
			/* TRANSLATORS: The object '%s' is locked. */
			return g_strdup_printf (_("In order to import '%s', it must be unlocked"), label);
		}
	}
}

static void
on_parser_parsed (GcrParser *parser, GcrImporter *self)
{
	GckAttributes *attrs;
	
	g_return_if_fail (GCR_IS_PARSER (parser));
	g_return_if_fail (GCR_IS_IMPORTER (self));
	
	attrs = gcr_parser_get_parsed_attributes (parser);
	g_return_if_fail (attrs);
	g_queue_push_tail (&self->pv->queue, gck_attributes_ref (attrs));
}

static gboolean
on_parser_authenticate (GcrParser *parser, gint count, GcrImporter *self)
{
	GcrImportDialog *dialog;
	GckAttributes *attrs;
	const gchar *password;
	gchar *text, *label;
	GckSlot *slot;
	gulong klass;
	
	dialog = _gcr_import_dialog_new ();
	
	if (self->pv->slot)
		_gcr_import_dialog_set_selected_slot (dialog, self->pv->slot);
	
	/* Figure out the text for the dialog */
	attrs = gcr_parser_get_parsed_attributes (parser);
	g_return_val_if_fail (attrs, FALSE);

	if (!gck_attributes_find_ulong (attrs, CKA_CLASS, &klass))
		klass = (gulong)-1;
	if (!gck_attributes_find_string (attrs, CKA_LABEL, &label))
		label = NULL;
	
	text = prepare_auth_secondary (klass, label);
	_gcr_import_dialog_set_primary_text (dialog, prepare_auth_primary (klass));
	_gcr_import_dialog_set_secondary_text (dialog, text);
	g_free (label);
	g_free (text);
	
	if (!_gcr_import_dialog_run (dialog, NULL))
		return FALSE;

	slot = _gcr_import_dialog_get_selected_slot (dialog);
	gcr_importer_set_slot (self, slot);
	
	password = _gcr_import_dialog_get_password (dialog);
	gcr_parser_add_password (parser, password);
	
	g_object_unref (dialog);
	self->pv->prompted = TRUE;
	return TRUE;
}

static void 
state_parse_buffer (GcrImporter *self, gboolean async)
{
	GError *error = NULL;
	GcrParser *parser;
	gulong parsed_conn;
	gulong auth_conn;
	gboolean ret;
	
	g_assert (GCR_IS_IMPORTER (self));
	g_assert (self->pv->buffer);
	
	parser = gcr_importer_get_parser (self);
	g_object_ref (parser);

	/* Listen in to the parser */
	parsed_conn = g_signal_connect (parser, "parsed", G_CALLBACK (on_parser_parsed), self);
	auth_conn = g_signal_connect (parser, "authenticate", G_CALLBACK (on_parser_authenticate), self);

	ret = gcr_parser_parse_data (parser, self->pv->buffer->data, self->pv->buffer->len, &error);
	
	/* An optimization to free data early as possible */
	g_byte_array_free (self->pv->buffer, TRUE);
	self->pv->buffer = NULL;
	
	g_signal_handler_disconnect (parser, parsed_conn);
	g_signal_handler_disconnect (parser, auth_conn);
	g_object_unref (parser);
	
	if (ret == TRUE) {
		next_state (self, state_import_prompt);
	} else {
		g_propagate_error (&self->pv->error, error);
		next_state (self, state_failure);
	}
}

/* ---------------------------------------------------------------------------------
 * BUFFER READING
 */

static void
complete_read_buffer (GcrImporter *self, gssize count, GError *error)
{
	g_assert (GCR_IS_IMPORTER (self));
	g_assert (self->pv->buffer);
	
	/* A failure */
	if (count == -1) {
		g_propagate_error (&self->pv->error, error);
		next_state (self, state_failure);
	} else {
	
		g_return_if_fail (count >= 0 && count <= BLOCK);
		g_byte_array_set_size (self->pv->buffer, self->pv->buffer->len - (BLOCK - count));
		
		/* Finished reading */
		if (count == 0) {
			
			/* Optimization, unref input early */
			g_object_unref (self->pv->input);
			self->pv->input = NULL;
			
			next_state (self, state_parse_buffer);
			
		/* Read the next block */
		} else {
			next_state (self, state_read_buffer);
		}
	}
	
}

static void
on_read_buffer (GObject *obj, GAsyncResult *res, gpointer user_data)
{
	GError *error = NULL;
	gssize count;
	
	count = g_input_stream_read_finish (G_INPUT_STREAM (obj), res, &error);
	complete_read_buffer (user_data, count, error);
}

static void 
state_read_buffer (GcrImporter *self, gboolean async)
{
	GError *error = NULL;
	gssize count;
	gsize at;
	
	g_assert (GCR_IS_IMPORTER (self));
	g_assert (G_IS_INPUT_STREAM (self->pv->input));
	
	if (!self->pv->buffer)
		self->pv->buffer = g_byte_array_sized_new (BLOCK);

	at = self->pv->buffer->len;
	g_byte_array_set_size (self->pv->buffer, at + BLOCK);
	
	if (async) {
		g_input_stream_read_async (self->pv->input, self->pv->buffer->data + at, 
		                           BLOCK, G_PRIORITY_DEFAULT, self->pv->cancel,
		                           on_read_buffer, self);
	} else {
		count = g_input_stream_read (self->pv->input, self->pv->buffer->data + at, 
		                             BLOCK, self->pv->cancel, &error);
		complete_read_buffer (self, count, error);
	}
}

/* -----------------------------------------------------------------------------
 * OBJECT 
 */

static GObject* 
gcr_importer_constructor (GType type, guint n_props, GObjectConstructParam *props) 
{
	GcrImporter *self = GCR_IMPORTER (G_OBJECT_CLASS (gcr_importer_parent_class)->constructor(type, n_props, props));
	g_return_val_if_fail (self, NULL);	
	
	return G_OBJECT (self);
}

static void
gcr_importer_init (GcrImporter *self)
{
	self->pv = G_TYPE_INSTANCE_GET_PRIVATE (self, GCR_TYPE_IMPORTER, GcrImporterPrivate);
	self->pv->behavior = GCR_IMPORTER_PROMPT_NEEDED;
	g_queue_init (&self->pv->queue);
}

static void
gcr_importer_dispose (GObject *obj)
{
	GcrImporter *self = GCR_IMPORTER (obj);
	
	cleanup_state_data (self);
	cleanup_import_data (self);
	
	if (self->pv->parser)
		g_object_unref (self->pv->parser);
	self->pv->parser = NULL;
	
	if (self->pv->slot)
		g_object_unref (self->pv->slot);
	self->pv->slot = NULL;

	G_OBJECT_CLASS (gcr_importer_parent_class)->dispose (obj);
}

static void
gcr_importer_finalize (GObject *obj)
{
	GcrImporter *self = GCR_IMPORTER (obj);
	
	g_assert (!self->pv->parser);
	g_assert (!self->pv->slot);
	
	G_OBJECT_CLASS (gcr_importer_parent_class)->finalize (obj);
}

static void
gcr_importer_set_property (GObject *obj, guint prop_id, const GValue *value, 
                           GParamSpec *pspec)
{
	GcrImporter *self = GCR_IMPORTER (obj);
	
	switch (prop_id) {
	case PROP_PARSER:
		gcr_importer_set_parser (self, g_value_get_object (value));
		break;
	case PROP_SLOT:
		gcr_importer_set_slot (self, g_value_get_object (value));
		break;
	case PROP_PROMPT_BEHAVIOR:
		gcr_importer_set_prompt_behavior (self, (GcrImporterPromptBehavior)g_value_get_int (value));
		break;
	default:
		G_OBJECT_WARN_INVALID_PROPERTY_ID (obj, prop_id, pspec);
		break;
	}
}

static void
gcr_importer_get_property (GObject *obj, guint prop_id, GValue *value, 
                           GParamSpec *pspec)
{
	GcrImporter *self = GCR_IMPORTER (obj);
	
	switch (prop_id) {
	case PROP_PARSER:
		g_value_set_object (value, gcr_importer_get_parser (self));
		break;
	case PROP_SLOT:
		g_value_set_object (value, gcr_importer_get_slot (self));
		break;
	case PROP_PROMPT_BEHAVIOR:
		g_value_set_int (value, gcr_importer_get_prompt_behavior (self));
		break;
	default:
		G_OBJECT_WARN_INVALID_PROPERTY_ID (obj, prop_id, pspec);
		break;
	}
}

static void
gcr_importer_class_init (GcrImporterClass *klass)
{
	GObjectClass *gobject_class = G_OBJECT_CLASS (klass);
    
	gobject_class->constructor = gcr_importer_constructor;
	gobject_class->dispose = gcr_importer_dispose;
	gobject_class->finalize = gcr_importer_finalize;
	gobject_class->set_property = gcr_importer_set_property;
	gobject_class->get_property = gcr_importer_get_property;
    
	g_type_class_add_private (gobject_class, sizeof (GcrImporterPrivate));
	
	g_object_class_install_property (gobject_class, PROP_PARSER,
	           g_param_spec_object ("parser", "Parser", "Parser used to parse imported data",
	                                GCR_TYPE_PARSER, G_PARAM_READWRITE));
	
	g_object_class_install_property (gobject_class, PROP_PARSER,
	           g_param_spec_object ("slot", "Slot", "PKCS#11 slot to import data into",
	                                GCK_TYPE_SLOT, G_PARAM_READWRITE));

	g_object_class_install_property (gobject_class, PROP_PROMPT_BEHAVIOR,
	           g_param_spec_int ("prompt-behavior", "Prompt Behavior", "Import Prompt Behavior",
	                             0, G_MAXINT, GCR_IMPORTER_PROMPT_NEEDED, G_PARAM_READWRITE));
	
	signals[IMPORTED] = g_signal_new ("imported", GCR_TYPE_IMPORTER, 
	                                G_SIGNAL_RUN_FIRST, G_STRUCT_OFFSET (GcrImporterClass, imported),
	                                NULL, NULL, g_cclosure_marshal_VOID__OBJECT, 
	                                G_TYPE_NONE, 1, GCK_TYPE_OBJECT);

	_gcr_initialize ();
}

static gpointer
gcr_importer_real_get_user_data (GAsyncResult *base)
{
	g_return_val_if_fail (GCR_IS_IMPORTER (base), NULL);
	return GCR_IMPORTER (base)->pv->user_data;
}

static GObject* 
gcr_importer_real_get_source_object (GAsyncResult *base)
{
	g_return_val_if_fail (GCR_IS_IMPORTER (base), NULL);
	return G_OBJECT (base);
}

static void 
gcr_importer_async_result (GAsyncResultIface *iface)
{
	iface->get_source_object = gcr_importer_real_get_source_object;
	iface->get_user_data = gcr_importer_real_get_user_data;
}

/* -----------------------------------------------------------------------------
 * PUBLIC 
 */

GcrImporter*
gcr_importer_new (void)
{
	return g_object_new (GCR_TYPE_IMPORTER, NULL);
}

GcrParser*
gcr_importer_get_parser (GcrImporter *self)
{
	g_return_val_if_fail (GCR_IS_IMPORTER (self), NULL);
	if (!self->pv->parser) 
		self->pv->parser = gcr_parser_new ();
	return self->pv->parser;
}

void
gcr_importer_set_parser (GcrImporter *self, GcrParser *parser)
{
	g_return_if_fail (GCR_IS_IMPORTER (self));
	
	if (parser)
		g_object_ref (parser);
	if (self->pv->parser)
		g_object_unref (self->pv->parser);
	self->pv->parser = parser;
	g_object_notify (G_OBJECT (self), "parser");
}

GckSlot*
gcr_importer_get_slot (GcrImporter *self)
{
	g_return_val_if_fail (GCR_IS_IMPORTER (self), NULL);
	return self->pv->slot;
}

void 
gcr_importer_set_slot (GcrImporter *self, GckSlot *slot)
{
	g_return_if_fail (GCR_IS_IMPORTER (self));
	
	if (slot)
		g_object_ref (slot);
	if (self->pv->slot)
		g_object_unref (self->pv->slot);
	self->pv->slot = slot;
	g_object_notify (G_OBJECT (self), "slot");
}

GcrImporterPromptBehavior
gcr_importer_get_prompt_behavior (GcrImporter *self)
{
	g_return_val_if_fail (GCR_IS_IMPORTER (self), GCR_IMPORTER_PROMPT_NEEDED);
	return self->pv->behavior;
}

void
gcr_importer_set_prompt_behavior (GcrImporter *self, GcrImporterPromptBehavior behavior)
{
	g_return_if_fail (GCR_IMPORTER (self));
	self->pv->behavior = behavior;
	g_object_notify (G_OBJECT (self), "prompt-behavior");
}

gboolean
gcr_importer_import (GcrImporter *self, GInputStream *input,
                     GCancellable *cancel, GError **error)
{
	g_return_val_if_fail (GCR_IS_IMPORTER (self), FALSE);
	g_return_val_if_fail (G_IS_INPUT_STREAM (input), FALSE);
	g_return_val_if_fail (!error || !*error, FALSE);
	g_return_val_if_fail (!self->pv->processing, FALSE);
	
	cleanup_import_data (self);
	
	self->pv->input = g_object_ref (input);
	if (cancel)
		self->pv->cancel = g_object_ref (cancel);
	self->pv->processing = TRUE;
	self->pv->async = FALSE;
	
	next_state (self, state_read_buffer);
	
	g_assert (!self->pv->processing);
	g_assert (!self->pv->input);
	g_assert (!self->pv->cancel);
	
	if (!self->pv->succeeded) {
		g_propagate_error (error, self->pv->error);
		self->pv->error = NULL;
		return FALSE;
	}
	
	return TRUE;
}

void
gcr_importer_import_async (GcrImporter *self, GInputStream *input, GCancellable *cancel,
                           GAsyncReadyCallback callback, gpointer user_data)
{
	g_return_if_fail (GCR_IS_IMPORTER (self));
	g_return_if_fail (G_IS_INPUT_STREAM (input));
	g_return_if_fail (!self->pv->processing);
	
	cleanup_import_data (self);
	
	self->pv->input = g_object_ref (input);
	if (cancel)
		self->pv->cancel = g_object_ref (cancel);
	self->pv->processing = TRUE;
	self->pv->async = TRUE;
	self->pv->callback = callback;
	self->pv->user_data = user_data;
	
	next_state (self, state_read_buffer);
	g_assert (self->pv->processing);
}

gboolean
gcr_importer_import_finish (GcrImporter *self, GAsyncResult *res, GError **error)
{
	g_return_val_if_fail (GCR_IS_IMPORTER (self), FALSE);
	g_return_val_if_fail (GCR_IMPORTER (res) == self, FALSE);
	g_return_val_if_fail (!error || !*error, FALSE);
	g_return_val_if_fail (!self->pv->processing, FALSE);

	g_assert (!self->pv->input);
	g_assert (!self->pv->cancel);
	
	if (!self->pv->succeeded) {
		g_propagate_error (error, self->pv->error);
		self->pv->error = NULL;
		return FALSE;
	}
	
	return TRUE;
}
