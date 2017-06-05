/*
 * gnome-keyring
 *
 * Copyright (C) 2008 Stefan Walter
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General  License as
 * published by the Free Software Foundation; either version 2.1 of
 * the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General  License for more details.
 *
 * You should have received a copy of the GNU Lesser General
 * License along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
 * 02111-1307, USA.
 */

#include "config.h"

#include "gkd-util.h"
#include "gkd-pkcs11.h"

#include "egg/egg-cleanup.h"

#include "pkcs11/wrap-layer/gkm-wrap-layer.h"
#include "pkcs11/roots-store/gkm-roots-store.h"
#include "pkcs11/rpc-layer/gkm-rpc-layer.h"
#include "pkcs11/secret-store/gkm-secret-store.h"
#include "pkcs11/ssh-store/gkm-ssh-store.h"
#include "pkcs11/user-store/gkm-user-store.h"

#include "gpg-agent/gkd-gpg-agent.h"
#include "ssh-agent/gkd-ssh-agent.h"

#include <string.h>

/* The top level of our internal PKCS#11 module stack */
static CK_FUNCTION_LIST_PTR pkcs11_roof = NULL;

/* The top level of our internal PKCS#11 module stack, but below prompting */
static CK_FUNCTION_LIST_PTR pkcs11_base = NULL;

static void
pkcs11_daemon_cleanup (gpointer unused)
{
	CK_RV rv;

	g_assert (pkcs11_roof);

	gkd_ssh_agent_uninitialize ();
	gkm_rpc_layer_uninitialize ();
	gkd_gpg_agent_uninitialize ();
	rv = (pkcs11_roof->C_Finalize) (NULL);

	if (rv != CKR_OK)
		g_warning ("couldn't finalize internal PKCS#11 stack (code: %d)", (gint)rv);

	pkcs11_roof = NULL;
}

gboolean
gkd_pkcs11_initialize (void)
{
	CK_FUNCTION_LIST_PTR roots_store;
	CK_FUNCTION_LIST_PTR secret_store;
	CK_FUNCTION_LIST_PTR ssh_store;
	CK_FUNCTION_LIST_PTR user_store;
	CK_C_INITIALIZE_ARGS init_args;
	gboolean ret;
	CK_RV rv;

	/* Secrets */
	secret_store = gkm_secret_store_get_functions ();

	/* SSH storage */
	ssh_store = gkm_ssh_store_get_functions ();

	/* Root certificates */
	roots_store = gkm_roots_store_get_functions ();

	/* User certificates */
	user_store = gkm_user_store_get_functions ();

	/* Add all of those into the wrapper layer */
	gkm_wrap_layer_add_module (ssh_store);
#ifdef ROOT_CERTIFICATES
	gkm_wrap_layer_add_module (roots_store);
#endif
	gkm_wrap_layer_add_module (secret_store);
	gkm_wrap_layer_add_module (user_store);

	pkcs11_roof = gkm_wrap_layer_get_functions ();
	pkcs11_base = gkm_wrap_layer_get_functions_no_prompts ();

	memset (&init_args, 0, sizeof (init_args));
	init_args.flags = CKF_OS_LOCKING_OK;

#if WITH_TESTS
	{
		const gchar *path = g_getenv ("GNOME_KEYRING_TEST_PATH");
		if (path && path[0])
			init_args.pReserved = g_strdup_printf ("directory=\"%s\"", path);
	}
#endif

	/* Initialize the whole caboodle */
	rv = (pkcs11_roof->C_Initialize) (&init_args);
	g_free (init_args.pReserved);

	if (rv != CKR_OK) {
		g_warning ("couldn't initialize internal PKCS#11 stack (code: %d)", (gint)rv);
		return FALSE;
	}

	egg_cleanup_register (pkcs11_daemon_cleanup, NULL);

	ret = gkd_gpg_agent_initialize (pkcs11_roof) &&
	      gkd_ssh_agent_initialize (pkcs11_roof) &&
	      gkm_rpc_layer_initialize (pkcs11_roof);

	return ret;
}

static void
pkcs11_rpc_cleanup (gpointer unused)
{
	gkm_rpc_layer_shutdown ();
}

static gboolean
accept_rpc_client (GIOChannel *channel, GIOCondition cond, gpointer unused)
{
	if (cond == G_IO_IN)
		gkm_rpc_layer_accept ();

	return TRUE;
}

gboolean
gkd_pkcs11_startup_pkcs11 (void)
{
	GIOChannel *channel;
	const gchar *base_dir;
	int sock;

	base_dir = gkd_util_get_master_directory ();
	g_return_val_if_fail (base_dir, FALSE);

	sock = gkm_rpc_layer_startup (base_dir);
	if (sock == -1)
		return FALSE;

	channel = g_io_channel_unix_new (sock);
	g_io_add_watch (channel, G_IO_IN | G_IO_HUP, accept_rpc_client, NULL);
	g_io_channel_unref (channel);

	egg_cleanup_register (pkcs11_rpc_cleanup, NULL);

	return TRUE;
}

static void
pkcs11_ssh_cleanup (gpointer unused)
{
	gkd_ssh_agent_shutdown ();
}

static gboolean
accept_ssh_client (GIOChannel *channel, GIOCondition cond, gpointer unused)
{
	if (cond == G_IO_IN)
		gkd_ssh_agent_accept ();
	return TRUE;
}

gboolean
gkd_pkcs11_startup_ssh (void)
{
	GIOChannel *channel;
	const gchar *base_dir;
	int sock;

	base_dir = gkd_util_get_master_directory ();
	g_return_val_if_fail (base_dir, FALSE);

	sock = gkd_ssh_agent_startup (base_dir);
	if (sock == -1)
		return FALSE;

	channel = g_io_channel_unix_new (sock);
	g_io_add_watch (channel, G_IO_IN | G_IO_HUP, accept_ssh_client, NULL);
	g_io_channel_unref (channel);

	/* gkm-ssh-agent sets the environment variable */
	gkd_util_push_environment ("SSH_AUTH_SOCK", g_getenv ("SSH_AUTH_SOCK"));

	egg_cleanup_register (pkcs11_ssh_cleanup, NULL);

	return TRUE;
}

CK_FUNCTION_LIST_PTR
gkd_pkcs11_get_functions (void)
{
	return pkcs11_roof;
}

CK_FUNCTION_LIST_PTR
gkd_pkcs11_get_base_functions (void)
{
	return pkcs11_base;
}
