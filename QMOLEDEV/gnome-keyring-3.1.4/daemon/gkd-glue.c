/*
 * gnome-keyring
 *
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

#include "gkd-glue.h"
#include "gkd-util.h"

#include "gpg-agent/gkd-gpg-agent.h"
#include "ssh-agent/gkd-ssh-agent.h"

#include "egg/egg-cleanup.h"

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
gkd_daemon_startup_ssh (void)
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

	/* gck-ssh-agent sets the environment variable */
	gkd_util_push_environment ("SSH_AUTH_SOCK", g_getenv ("SSH_AUTH_SOCK"));

	egg_cleanup_register (pkcs11_ssh_cleanup, NULL);

	return TRUE;
}

static void
pkcs11_gpg_cleanup (gpointer unused)
{
	gkd_gpg_agent_shutdown ();
}

static gboolean
accept_gpg_client (GIOChannel *channel, GIOCondition cond, gpointer unused)
{
	if (cond == G_IO_IN)
		gkd_gpg_agent_accept ();
	return TRUE;
}

gboolean
gkd_daemon_startup_gpg (void)
{
	GIOChannel *channel;
	const gchar *base_dir;
	int sock;

	base_dir = gkd_util_get_master_directory ();
	g_return_val_if_fail (base_dir, FALSE);

	sock = gkd_gpg_agent_startup (base_dir);
	if (sock == -1)
		return FALSE;

	channel = g_io_channel_unix_new (sock);
	g_io_add_watch (channel, G_IO_IN | G_IO_HUP, accept_gpg_client, NULL);
	g_io_channel_unref (channel);

	/* gck-gpg-agent sets the environment variable */
	gkd_util_push_environment ("GPG_AGENT_INFO", g_getenv ("GPG_AGENT_INFO"));

	egg_cleanup_register (pkcs11_gpg_cleanup, NULL);

	return TRUE;
}
