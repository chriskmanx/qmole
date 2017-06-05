/* -*- Mode: C; indent-tabs-mode: t; c-basic-offset: 8; tab-width: 8 -*- */
/* gck-enumerator.c - the GObject PKCS#11 wrapper library

   Copyright (C) 2010, Stefan Walter

   The Gnome Keyring Library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Library General Public License as
   published by the Free Software Foundation; either version 2 of the
   License, or (at your option) any later version.

   The Gnome Keyring Library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Library General Public License for more details.

   You should have received a copy of the GNU Library General Public
   License along with the Gnome Library; see the file COPYING.LIB.  If not,
   write to the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
   Boston, MA 02111-1307, USA.

   Author: Stef Walter <nielsen@memberwebs.com>
*/

#include "config.h"

#include "gck.h"
#include "gck-private.h"

#include <string.h>

/**
 * SECTION:gck-enumerator
 * @title: GckEnumerator
 * @short_description: Enumerates through PKCS#11 objects.
 *
 * Xxxxxx
 */

/**
 * GckEnumerator:
 *
 * Xxxxxx
 */

typedef struct _GckEnumeratorState GckEnumeratorState;

typedef gpointer (*GckEnumeratorFunc) (GckEnumeratorState *args, gboolean forward);

struct _GckEnumeratorState {
	/* For the current call */
	gint want_objects;
	gboolean want_password;

	/* The state we're currently in */
	GckEnumeratorFunc handler;

	/* Input to enumerator */
	GList *modules;
	GckTokenInfo *match_token;
	GckAttributes *match_attrs;
	guint session_options;
	gboolean authenticate;
	gchar *password;

	/* state_module */
	GckModule *module;

	/* state_slots */
	GList *slots;

	/* state_slot */
	GckSlot *slot;
	GckTokenInfo *token_info;

	/* state_session */
	GckSession *session;

	/* state_results */
	GArray *objects;

	/* Output from enumerator */
	GList *results;
};

struct _GckEnumeratorPrivate {
	/* Data here is set atomically */
	gpointer state;
	gint mode;
};

G_DEFINE_TYPE (GckEnumerator, gck_enumerator, G_TYPE_OBJECT);

static gpointer state_start          (GckEnumeratorState *args, gboolean forward);
static gpointer state_module         (GckEnumeratorState *args, gboolean forward);
static gpointer state_slots          (GckEnumeratorState *args, gboolean forward);
static gpointer state_slot           (GckEnumeratorState *args, gboolean forward);
static gpointer state_session        (GckEnumeratorState *args, gboolean forward);
static gpointer state_authenticated  (GckEnumeratorState *args, gboolean forward);
static gpointer state_results        (GckEnumeratorState *args, gboolean forward);

/* ----------------------------------------------------------------------------
 * INTERNAL
 */

static gpointer
rewind_state (GckEnumeratorState *args, GckEnumeratorFunc handler)
{
	g_assert (args);
	g_assert (handler);
	g_assert (args->handler);

	while (handler != args->handler) {
		args->handler = (args->handler) (args, FALSE);
		g_assert (args->handler);
	}

	return handler;
}

static void
cleanup_state (GckEnumeratorState *args)
{
	g_assert (args);

	/* Have each state cleanup */
	rewind_state (args, state_start);

	/* state_module */
	g_assert (!args->module);

	/* state_slots */
	g_assert (!args->slots);

	/* state_slot */
	g_assert (!args->slot);
	g_assert (!args->token_info);

	/* state_session */
	g_assert (!args->session);

	/* state_results */
	if (args->objects)
		g_array_free (args->objects, TRUE);
	args->objects = NULL;

	/* Other cleanup */
	gck_list_unref_free (args->results);
	args->results = NULL;

	gck_list_unref_free (args->modules);
	args->modules = NULL;

	/* TODO: Can we use secure memory here? */
	if (args->password) {
		g_free (args->password);
		args->password  = NULL;
	}

	gck_token_info_free (args->match_token);
	args->match_token = NULL;

	if (args->match_attrs) {
		_gck_attributes_unlock (args->match_attrs);
		gck_attributes_unref (args->match_attrs);
	}
}

static gpointer
state_start (GckEnumeratorState *args, gboolean forward)
{
	g_assert (args->module == NULL);

	if (forward) {

		/* There is no no more modules? */
		if (!args->modules)
			return NULL;

		/* Pop off the current module */
		args->module = args->modules->data;
		g_assert (GCK_IS_MODULE (args->module));
		args->modules = g_list_delete_link (args->modules, args->modules);
		return state_module;
	}

	/* Should never be asked to go backward from start state */
	g_assert_not_reached ();
}

static gpointer
state_module (GckEnumeratorState *args, gboolean forward)
{
	g_assert (args->module);
	g_assert (!args->slots);

	/* module to slots state */
	if (forward) {

		args->slots = gck_module_get_slots (args->module, TRUE);
		return state_slots;

	/* module to start state */
	} else {
		g_object_unref (args->module);
		args->module = NULL;
		return state_start;
	}
}

static gpointer
state_slots (GckEnumeratorState *args, gboolean forward)
{
	GckSlot *slot;
	GckTokenInfo *token_info;

	g_assert (args->slot == NULL);

	/* slots to slot state */
	if (forward) {

		/* If there are no more slots go back to start state */
		if (!args->slots)
			return rewind_state (args, state_start);

		/* Pop the next slot off the stack */
		slot = args->slots->data;
		args->slots = g_list_delete_link (args->slots, args->slots);

		token_info = gck_slot_get_token_info (slot);
		if (!token_info) {
			g_message ("couldn't get token info while enumerating");
			g_object_unref (slot);
			return rewind_state (args, state_start);
		}

		/* Are we trying to match the slot? */
		if (args->match_token) {

			/* No match? Go to next slot */
			if (!_gck_token_info_match (args->match_token, token_info)) {
				g_object_unref (slot);
				gck_token_info_free (token_info);
				return state_slots;
			}
		}

		/* We have a slot */
		args->slot = slot;
		args->token_info = token_info;
		return state_slot;

	/* slots state to module state */
	} else {

		gck_list_unref_free (args->slots);
		args->slots = NULL;
		return state_module;
	}
}

static gpointer
state_slot (GckEnumeratorState *args, gboolean forward)
{
	CK_FUNCTION_LIST_PTR funcs;
	CK_SESSION_HANDLE session;
	CK_FLAGS flags;
	CK_RV rv;

	g_assert (args->slot);
	g_assert (args->module);
	g_assert (args->session == NULL);

	/* slot to session state */
	if (forward) {
		funcs = gck_module_get_functions (args->module);
		g_return_val_if_fail (funcs, NULL);

		flags = CKF_SERIAL_SESSION;
		if ((args->session_options & GCK_SESSION_READ_WRITE) == GCK_SESSION_READ_WRITE)
			flags |= CKF_RW_SESSION;

		rv = (funcs->C_OpenSession) (gck_slot_get_handle (args->slot),
		                             flags, NULL, NULL, &session);

		if (rv != CKR_OK) {
			g_message ("couldn't open session on module while enumerating objects: %s",
			           gck_message_from_rv (rv));
			return rewind_state (args, state_slots);
		}

		args->session = gck_session_from_handle (args->slot, session, args->session_options);
		return state_session;

	/* slot to slots state */
	} else {
		g_object_unref (args->slot);
		args->slot = NULL;

		gck_token_info_free (args->token_info);
		args->token_info = NULL;

		return state_slots;
	}
}

static gpointer
state_session (GckEnumeratorState *args, gboolean forward)
{
	CK_FUNCTION_LIST_PTR funcs;
	GckSessionInfo *sinfo;
	CK_ULONG n_pin;
	CK_RV rv;

	g_assert (args->session);
	g_assert (args->module);
	g_assert (!args->want_password);
	g_assert (args->token_info);

	/* session to authenticated state */
	if (forward) {

		/* Don't want to authenticate? */
		if (!args->authenticate)
			return state_authenticated;

		/* No login necessary */
		if ((args->token_info->flags & CKF_LOGIN_REQUIRED) == 0)
			return state_authenticated;

		/* Next check if session is logged in */
		sinfo = gck_session_get_info (args->session);
		if (sinfo == NULL) {
			g_message ("couldn't get session info when enumerating");
			return rewind_state (args, state_slots);
		}

		/* Already logged in? */
		if (sinfo->state == CKS_RW_USER_FUNCTIONS ||
		    sinfo->state == CKS_RO_USER_FUNCTIONS ||
		    sinfo->state == CKS_RW_SO_FUNCTIONS) {
			gck_session_info_free (sinfo);
			return state_authenticated;
		}

		gck_session_info_free (sinfo);

		funcs = gck_module_get_functions (args->module);
		g_return_val_if_fail (funcs, NULL);

		/* Try to log in */
		n_pin = args->password ? strlen (args->password) : 0;
		rv = (funcs->C_Login) (gck_session_get_handle (args->session), CKU_USER,
		                       (CK_BYTE_PTR)args->password, n_pin);

		/* Authentication failed, ask for a password */
		if (rv == CKR_PIN_INCORRECT) {
			args->want_password = TRUE;
			return NULL;

		/* Any other failure continue without authentication */
		} else if (rv != CKR_OK) {
			g_message ("couldn't authenticate when enumerating: %s", gck_message_from_rv (rv));
		}

		return state_authenticated;

	/* Session to slot state */
	} else {
		g_object_unref (args->session);
		args->session = NULL;
		return state_slot;
	}
}

static gpointer
state_authenticated (GckEnumeratorState *args, gboolean forward)
{
	CK_FUNCTION_LIST_PTR funcs;
	CK_OBJECT_HANDLE objects[128];
	CK_SESSION_HANDLE session;
	CK_ATTRIBUTE_PTR attrs;
	CK_ULONG n_attrs, count;
	CK_RV rv;

	/* Just go back, no logout */
	if (!forward)
		return state_session;

	/* This is where we do the actual searching */

	g_assert (args->session);
	g_assert (!args->want_password);
	g_assert (args->want_objects);

	if (args->match_attrs) {
		attrs = _gck_attributes_commit_out (args->match_attrs, &n_attrs);
	} else {
		attrs = NULL;
		n_attrs = 0;
	}

	funcs = gck_module_get_functions (args->module);
	g_return_val_if_fail (funcs, NULL);

	session = gck_session_get_handle (args->session);
	g_return_val_if_fail (session, NULL);

	/* Get all the objects */
	rv = (funcs->C_FindObjectsInit) (session, attrs, n_attrs);

	if (rv == CKR_OK) {
		while (rv == CKR_OK) {
			rv = (funcs->C_FindObjects) (session, objects, G_N_ELEMENTS (objects), &count);

			if (count == 0)
				break;

			if (!args->objects)
				args->objects = g_array_new (FALSE, TRUE, sizeof (CK_OBJECT_HANDLE));
			g_array_append_vals (args->objects, objects, count);
		}

		(funcs->C_FindObjectsFinal) (session);
	}

	return state_results;
}

static GckObject*
extract_result (GckEnumeratorState *args)
{
	CK_OBJECT_HANDLE handle;

	if (!args->objects || !args->objects->len)
		return NULL;

	g_assert (args->session);

	handle = g_array_index (args->objects, CK_OBJECT_HANDLE, 0);
	g_array_remove_index_fast (args->objects, 0);

	return gck_object_from_handle (args->session, handle);
}

static gpointer
state_results (GckEnumeratorState *args, gboolean forward)
{
	GckObject *object;
	guint have;

	g_assert (args->session);

	/* No cleanup, just unwind */
	if (!forward)
		return state_authenticated;

	/* Create result objects from what we have */
	have = g_list_length (args->results);

	while (have < args->want_objects) {

		object = extract_result (args);
		if (!object)
			return rewind_state (args, state_slots);

		args->results = g_list_append (args->results, object);
		++have;
	}

	/* We got all the results we wanted */
	return NULL;
}

/* ----------------------------------------------------------------------------
 * OBJECT
 */

static void
gck_enumerator_init (GckEnumerator *self)
{
	GckEnumeratorState *args;

	self->pv = G_TYPE_INSTANCE_GET_PRIVATE (self, GCK_TYPE_ENUMERATOR, GckEnumeratorPrivate);
	args = g_new0 (GckEnumeratorState, 1);
	args->handler = state_start;
	g_atomic_pointer_set (&self->pv->state, args);
}

static void
gck_enumerator_finalize (GObject *obj)
{
	GckEnumerator *self = GCK_ENUMERATOR (obj);
	GckEnumeratorState *state = g_atomic_pointer_get (&self->pv->state);

	if (!g_atomic_pointer_compare_and_exchange (&self->pv->state, state, NULL))
		g_assert_not_reached ();

	g_assert (state);
	cleanup_state (state);
	g_free (state);

	G_OBJECT_CLASS (gck_enumerator_parent_class)->finalize (obj);
}


static void
gck_enumerator_class_init (GckEnumeratorClass *klass)
{
	GObjectClass *gobject_class = (GObjectClass*)klass;
	gck_enumerator_parent_class = g_type_class_peek_parent (klass);

	gobject_class->finalize = gck_enumerator_finalize;
	g_type_class_add_private (klass, sizeof (GckEnumeratorPrivate));
}

/* ----------------------------------------------------------------------------
 * PUBLIC
 */

GckEnumerator*
_gck_enumerator_new (GList *modules, guint session_options, GckTokenInfo *match_token, GckAttributes *match_attrs)
{
	GckEnumerator *self;
	GckEnumeratorState *state;

	self = g_object_new (GCK_TYPE_ENUMERATOR, NULL);
	state = g_atomic_pointer_get (&self->pv->state);

	state->session_options = session_options;
	state->modules = gck_list_ref_copy (modules);

	if (match_attrs) {
		state->match_attrs = gck_attributes_ref (match_attrs);
		_gck_attributes_lock (state->match_attrs);
	}
	state->match_token = match_token;

	return self;
}

typedef struct _EnumerateNext {
	GckArguments base;
	GckEnumeratorState *state;
} EnumerateNext;

static CK_RV
perform_enumerate_next (EnumerateNext *args)
{
	GckEnumeratorFunc handler;
	GckEnumeratorState *state;

	g_assert (args->state);
	state = args->state;

	g_assert (!state->want_password);
	g_assert (state->handler);

	for (;;) {
		handler = (state->handler) (state, TRUE);
		if (!handler)
			break;
		state->handler = handler;
	}

	/* TODO: In some modes, errors */
	return CKR_OK;
}

static gboolean
complete_enumerate_next (EnumerateNext *args, CK_RV result)
{
	GckEnumeratorState *state;
	gboolean ret = TRUE;

	g_assert (args->state);
	state = args->state;

	if (state->want_password) {
		g_assert (state->module);
		g_assert (state->slot);

		/* TODO: Should we be using secure memory here? */
		g_free (state->password);
		state->password = NULL;

		ret = _gck_module_fire_authenticate_slot (state->module, state->slot, NULL, &state->password);

		/* If authenticate returns TRUE then call is not complete */
		ret = !ret;
	}

	return ret;
}

static void
free_enumerate_next (EnumerateNext *args)
{
	/* Should have been assigned back to enumerator */
	g_assert (!args->state);

	g_free (args);
}

GckObject*
gck_enumerator_next (GckEnumerator *self, GCancellable *cancellable, GError **error)
{
	EnumerateNext args = { GCK_ARGUMENTS_INIT, NULL, };
	GckObject *result = NULL;

	g_return_val_if_fail (GCK_IS_ENUMERATOR (self), NULL);
	g_return_val_if_fail (!error || !*error, NULL);

	/* Remove the state and own it ourselves */
	args.state = g_atomic_pointer_get (&self->pv->state);
	if (!args.state || !g_atomic_pointer_compare_and_exchange (&self->pv->state, args.state, NULL)) {
		g_warning ("this enumerator is already running a next operation");
		return NULL;
	}

	/* A result from a previous run? */
	result = extract_result (args.state);
	if (!result) {
		args.state->want_objects = 1;

		/* Run the operation and steal away the results */
		if (_gck_call_sync (NULL, perform_enumerate_next, complete_enumerate_next, &args, cancellable, error)) {
			if (args.state->results) {
				g_assert (g_list_length (args.state->results) == 1);
				result = g_object_ref (args.state->results->data);
				gck_list_unref_free (args.state->results);
				args.state->results = NULL;
			}
		}

		args.state->want_objects = 0;
	}

	/* Put the state back */
	if (!g_atomic_pointer_compare_and_exchange (&self->pv->state, NULL, args.state))
		g_assert_not_reached ();

	return result;
}

GList*
gck_enumerator_next_n (GckEnumerator *self, gint max_objects, GCancellable *cancellable,
                       GError **error)
{
	EnumerateNext args = { GCK_ARGUMENTS_INIT, NULL, };
	GList *results = NULL;

	g_return_val_if_fail (GCK_IS_ENUMERATOR (self), NULL);
	g_return_val_if_fail (max_objects == -1 || max_objects > 0, NULL);
	g_return_val_if_fail (!error || !*error, NULL);

	/* Remove the state and own it ourselves */
	args.state = g_atomic_pointer_get (&self->pv->state);
	if (!args.state || !g_atomic_pointer_compare_and_exchange (&self->pv->state, args.state, NULL)) {
		g_warning ("this enumerator is already running a next operation");
		return NULL;
	}

	args.state->want_objects = max_objects <= 0 ? G_MAXINT : max_objects;

	/* Run the operation and steal away the results */
	if (_gck_call_sync (NULL, perform_enumerate_next, complete_enumerate_next, &args, cancellable, error)) {
		results = args.state->results;
		args.state->results = NULL;
	}

	args.state->want_objects = 0;

	/* Put the state back */
	if (!g_atomic_pointer_compare_and_exchange (&self->pv->state, NULL, args.state))
		g_assert_not_reached ();

	return results;
}

void
gck_enumerator_next_async (GckEnumerator *self, gint max_objects, GCancellable *cancellable,
                           GAsyncReadyCallback callback, gpointer user_data)
{
	GckEnumeratorState *state;
	EnumerateNext *args;

	g_return_if_fail (GCK_IS_ENUMERATOR (self));
	g_return_if_fail (max_objects == -1 || max_objects > 0);

	g_object_ref (self);

	/* Remove the state and own it ourselves */
	state = g_atomic_pointer_get (&self->pv->state);
	if (!state || !g_atomic_pointer_compare_and_exchange (&self->pv->state, state, NULL)) {
		g_warning ("this enumerator is already running a next operation");
		return;
	}

	state->want_objects = max_objects <= 0 ? G_MAXINT : max_objects;
	args =  _gck_call_async_prep (NULL, self, perform_enumerate_next, complete_enumerate_next,
	                               sizeof (*args), free_enumerate_next);

	args->state = state;
	_gck_call_async_ready_go (args, cancellable, callback, user_data);
	g_object_unref (self);
}

GList*
gck_enumerator_next_finish (GckEnumerator *self, GAsyncResult *result, GError **error)
{
	EnumerateNext *args;
	GckEnumeratorState *state;
	GList *results = NULL;

	g_object_ref (self);

	args = _gck_call_arguments (result, EnumerateNext);
	state = args->state;
	args->state = NULL;
	state->want_objects = 0;

	if (_gck_call_basic_finish (result, error)) {
		results = state->results;
		state->results = NULL;
	}

	/* Put the state back */
	if (!g_atomic_pointer_compare_and_exchange (&self->pv->state, NULL, state))
		g_assert_not_reached ();

	g_object_unref (self);

	return results;
}
