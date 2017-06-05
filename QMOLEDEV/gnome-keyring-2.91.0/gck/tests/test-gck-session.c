#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include "test-suite.h"

#include <glib.h>

#include "gck-test.h"

static GckModule *module = NULL;
static GckSlot *slot = NULL;
static GckSession *session = NULL;

DEFINE_SETUP(load_session)
{
	GError *err = NULL;
	GList *slots;

	/* Successful load */
	module = gck_module_initialize (".libs/libmock-test-module.so", NULL, 0, &err);
	SUCCESS_RES (module, err);

	slots = gck_module_get_slots (module, TRUE);
	g_assert (slots != NULL);

	slot = GCK_SLOT (slots->data);
	g_object_ref (slot);
	gck_list_unref_free (slots);

	session = gck_slot_open_session (slot, 0, NULL, &err);
	SUCCESS_RES(session, err);
}

DEFINE_TEARDOWN(load_session)
{
	g_object_unref (session);
	g_object_unref (slot);
	g_object_unref (module);
}

DEFINE_TEST(session_props)
{
	GckModule *mod;
	GckSlot *sl;
	gulong handle;

	g_object_get (session, "module", &mod, "handle", &handle, "slot", &sl, NULL);
	g_assert (mod == module);
	g_assert (sl == slot);
	g_object_unref (mod);
	g_object_unref (sl);

	g_assert (handle != 0);
	g_assert (gck_session_get_handle (session) == handle);
}

DEFINE_TEST(session_info)
{
	GckSessionInfo *info;

	info = gck_session_get_info (session);
	g_assert (info != NULL && "no session info");

	g_assert (info->slot_id == gck_slot_get_handle (slot));
	g_assert ((info->flags & CKF_SERIAL_SESSION) == CKF_SERIAL_SESSION);
	g_assert (info->device_error == 1414);
	gck_session_info_free (info);
}

static void
fetch_async_result (GObject *source, GAsyncResult *result, gpointer user_data)
{
	*((GAsyncResult**)user_data) = result;
	g_object_ref (result);
	testing_wait_stop ();
}

DEFINE_TEST(open_close_session)
{
	GckSession *sess;
	GAsyncResult *result = NULL;
	GError *err = NULL;

	sess = gck_slot_open_session (slot, 0, NULL, &err);
	SUCCESS_RES (sess, err);

	g_object_unref (sess);

	/* Test opening async */
	gck_slot_open_session_async (slot, 0, NULL, fetch_async_result, &result);

	testing_wait_until (500);
	g_assert (result != NULL);

	/* Get the result */
	sess = gck_slot_open_session_finish (slot, result, &err);
	SUCCESS_RES (sess, err);

	g_object_unref (result);
	g_object_unref (sess);
}

DEFINE_TEST(init_set_pin)
{
	GAsyncResult *result = NULL;
	GError *err = NULL;
	gboolean ret;

	/* init pin */
	ret = gck_session_init_pin (session, (guchar*)"booo", 4, NULL, &err);
	SUCCESS_RES (ret, err);

	/* set pin */
	ret = gck_session_set_pin (session, (guchar*)"booo", 4, (guchar*)"tooo", 4, NULL, &err);
	SUCCESS_RES (ret, err);

	/* init pin async */
	gck_session_init_pin_async (session, (guchar*)"booo", 4, NULL, fetch_async_result, &result);
	testing_wait_until (500);
	g_assert (result != NULL);
	ret = gck_session_init_pin_finish (session, result, &err);
	SUCCESS_RES (ret, err);
	g_object_unref (result);
	result = NULL;

	/* set pin async */
	gck_session_set_pin_async (session, (guchar*)"booo", 4, (guchar*)"tooo", 4, NULL, fetch_async_result, &result);
	testing_wait_until (500);
	g_assert (result != NULL);
	ret = gck_session_set_pin_finish (session, result, &err);
	SUCCESS_RES (ret, err);
	g_object_unref (result);
	result = NULL;
}


DEFINE_TEST(login_logout)
{
	GAsyncResult *result = NULL;
	GError *err = NULL;
	gboolean ret;

	/* login/logout */
	ret = gck_session_login (session, CKU_USER, (guchar*)"booo", 4, NULL, &err);
	SUCCESS_RES (ret, err);

	ret = gck_session_logout (session, NULL, &err);
	SUCCESS_RES (ret, err);

	/* login async */
	gck_session_login_async (session, CKU_USER, (guchar*)"booo", 4, NULL, fetch_async_result, &result);
	testing_wait_until (500);
	g_assert (result != NULL);

	ret = gck_session_login_finish (session, result, &err);
	SUCCESS_RES (ret, err);

	g_object_unref (result);
	result = NULL;

	/* logout async */
	gck_session_logout_async (session, NULL, fetch_async_result, &result);
	testing_wait_until (500);
	g_assert (result != NULL);

	ret = gck_session_logout_finish (session, result, &err);
	SUCCESS_RES (ret, err);

	g_object_unref (result);
	result = NULL;

}

static gboolean
authenticate_token (GckModule *module, GckSlot *slot, gchar *label, gchar **password, gpointer unused)
{
	g_assert (unused == GUINT_TO_POINTER (35));
	g_assert (password != NULL);
	g_assert (*password == NULL);
	g_assert (GCK_IS_MODULE (module));
	g_assert (GCK_IS_SLOT (slot));

	*password = g_strdup ("booo");
	return TRUE;
}

DEFINE_TEST(auto_login)
{
	GckObject *object;
	GckSession *new_session;
	GAsyncResult *result = NULL;
	GError *err = NULL;
	GckAttributes *attrs;
	gboolean ret;

	attrs = gck_attributes_new ();
	gck_attributes_add_ulong (attrs, CKA_CLASS, CKO_DATA);
	gck_attributes_add_string (attrs, CKA_LABEL, "TEST OBJECT");
	gck_attributes_add_boolean (attrs, CKA_PRIVATE, CK_TRUE);

	/* Try to do something that requires a login */
	object = gck_session_create_object (session, attrs, NULL, &err);
	g_assert (!object);
	g_assert (err && err->code == CKR_USER_NOT_LOGGED_IN);
	g_clear_error (&err);

	/* Setup for auto login */
	g_signal_connect (module, "authenticate-slot", G_CALLBACK (authenticate_token), GUINT_TO_POINTER (35));
	new_session = gck_slot_open_session (slot, GCK_SESSION_READ_WRITE | GCK_SESSION_LOGIN_USER, NULL, &err);
	SUCCESS_RES (new_session, err);

	/* Try again to do something that requires a login */
	object = gck_session_create_object (new_session, attrs, NULL, &err);
	SUCCESS_RES (object, err);
	g_object_unref (object);

	/* We should now be logged in, try to log out */
	ret = gck_session_logout (new_session, NULL, &err);
	SUCCESS_RES (ret, err);

	g_object_unref (new_session);

	/* Now try the same thing, but asyncronously */
	gck_slot_open_session_async (slot, GCK_SESSION_READ_WRITE | GCK_SESSION_LOGIN_USER, NULL, fetch_async_result, &result);
	testing_wait_until (500);
	g_assert (result != NULL);
	new_session = gck_slot_open_session_finish (slot, result, &err);
	SUCCESS_RES (new_session, err);
	g_object_unref (result);

	result = NULL;
	gck_session_create_object_async (new_session, attrs, NULL, fetch_async_result, &result);
	testing_wait_until (500);
	g_assert (result != NULL);
	object = gck_session_create_object_finish (new_session, result, &err);
	SUCCESS_RES (object, err);
	g_object_unref (result);
	g_object_unref (object);

	/* We should now be logged in, try to log out */
	ret = gck_session_logout (new_session, NULL, &err);
	SUCCESS_RES (ret, err);

	g_object_unref (new_session);
}
