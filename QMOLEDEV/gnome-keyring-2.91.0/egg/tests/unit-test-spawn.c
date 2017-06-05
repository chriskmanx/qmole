/* -*- Mode: C; indent-tabs-mode: t; c-basic-offset: 8; tab-width: 8 -*- */
/* unit-test-dh.c: Test egg-spawn.c

   Copyright (C) 2009 Stefan Walter

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

   Author: Stef Walter <stef@memberwebs.com>
*/

#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include "test-suite.h"

#include "egg-spawn.h"

#include <sys/wait.h>

typedef struct _EchoData {
	gint index;
	gchar *output;
	gchar *error;
	gboolean finalized;
	gboolean completed;
	gboolean is_async;
	pid_t parent_pid;
} EchoData;

static gboolean
want_input (gint fd, gpointer user_data)
{
	EchoData *data = user_data;
	gchar *buffer;

	g_assert (data);
	if (data->index == 85)
		return FALSE;

	g_assert (data->index >= 80);
	g_assert (data->index < 85);

	buffer = g_strdup_printf ("%d\n", data->index);
	if (egg_spawn_write_input (fd, (guchar*)buffer, strlen (buffer)) < 0)
		g_assert_not_reached ();
	g_free (buffer);
	++data->index;
	return TRUE;
}

static gboolean
have_output (gint fd, gpointer user_data)
{
	EchoData *data = user_data;
	gchar buffer[1024];
	gssize length;
	gchar *output;

	g_assert (data);

	length = egg_spawn_read_output (fd, (guchar*)buffer, 1023);
	g_assert (length >= 0);

	buffer[length] = 0;
	output = g_strconcat (data->output ? data->output : "", buffer, NULL);
	g_free (data->output);
	data->output = output;

	return (length > 0);
}

static gboolean
have_error (gint fd, gpointer user_data)
{
	EchoData *data = user_data;
	gchar buffer[1024];
	gssize length;
	gchar *error;

	g_assert (data);

	length = egg_spawn_read_output (fd, (guchar*)buffer, 1023);
	g_assert (length >= 0);

	buffer[length] = 0;
	error = g_strconcat (data->error ? data->error : "", buffer, NULL);
	g_free (data->error);
	data->error = error;

	return (length > 0);
}

static void
completed_func (gpointer user_data)
{
	EchoData *data = user_data;
	g_assert (data);
	g_assert (!data->finalized);
	g_assert (!data->completed);
	data->completed = TRUE;
	if (data->is_async)
		testing_wait_stop ();
}

static void
finalize_func (gpointer user_data)
{
	EchoData *data = user_data;
	g_assert (!data->finalized);
	data->finalized = 1;
}

static void
child_setup (gpointer user_data)
{
	EchoData *data = user_data;
	g_assert (data->parent_pid != getpid ());
}

static EggSpawnCallbacks echo_callbacks = {
	want_input,
	have_output,
	have_error,
	completed_func,
	finalize_func,
	child_setup,
};

static char* echo_argv[] = {
	"/bin/sh",
	"./echo-script.sh",
	NULL
};

static char* error_argv[] = {
	"/nonexistent",
	NULL
};

static EggSpawnCallbacks null_callbacks = {
	NULL,
	NULL,
	NULL,
	completed_func,
	finalize_func,
	child_setup,
};

DEFINE_TEST(test_spawn_sync)
{
	GError *error = NULL;
	gboolean ret;
	gint exit_status;
	EchoData data;
	GPid pid = 0;

	memset (&data, 0, sizeof (data));
	data.parent_pid = getpid();
	data.index = 80;

	ret = egg_spawn_sync_with_callbacks (testing_data_directory (),
	                                     echo_argv, NULL, 0, &pid,
	                                     &echo_callbacks, &data,
	                                     &exit_status, &error);
	g_assert (ret == TRUE);
	g_assert (pid != 0);
	g_assert (WIFEXITED (exit_status));
	g_assert_cmpint (WEXITSTATUS(exit_status), ==, 3);
	g_assert (error == NULL);
	g_assert (data.finalized);
	g_assert (data.completed);
	g_assert_cmpstr (data.output, ==, "80 81 82 83 84\n");
	g_assert_cmpstr (data.error, ==, "1\n2\n3\n4\n5\n");
}

DEFINE_TEST(test_spawn_sync_error)
{
	GError *error = NULL;
	gboolean ret;

	ret = egg_spawn_sync_with_callbacks (testing_data_directory (),
	                                     error_argv, NULL, 0, NULL,
	                                     NULL, NULL,
	                                     NULL, &error);
	g_assert (ret == FALSE);
	g_assert (error != NULL);
	g_clear_error (&error);
}


DEFINE_TEST(test_spawn_async)
{
	GError *error = NULL;
	EchoData data;
	guint ret;
	GPid pid;

	memset (&data, 0, sizeof (data));
	data.parent_pid = getpid();
	data.index = 80;
	data.is_async = TRUE;

	ret = egg_spawn_async_with_callbacks (testing_data_directory (),
	                                     echo_argv, NULL, 0, &pid,
	                                     &echo_callbacks, &data,
	                                     NULL, &error);
	g_assert (ret != 0);
	g_assert (error == NULL);
	g_assert (!data.finalized);
	g_assert (!data.output);
	g_assert (!data.completed);

	testing_wait_until (2000);

	g_assert (data.finalized);
	g_assert (data.completed);
	g_assert_cmpstr (data.output, ==, "80 81 82 83 84\n");
	g_assert_cmpstr (data.error, ==, "1\n2\n3\n4\n5\n");
}

DEFINE_TEST(test_spawn_async_none)
{
	GError *error = NULL;
	EchoData data;
	guint ret;

	memset (&data, 0, sizeof (data));
	data.parent_pid = getpid();
	data.is_async = TRUE;

	ret = egg_spawn_async_with_callbacks (testing_data_directory (),
	                                     echo_argv, NULL, 0, NULL,
	                                     &null_callbacks, &data,
	                                     NULL, &error);
	g_assert (ret != 0);
	g_assert (error == NULL);
	g_assert (!data.finalized);
	g_assert (!data.completed);
	g_assert (!data.output);

	testing_wait_until (2000);

	g_assert (data.finalized);
	g_assert (data.completed);
	g_assert (!data.output);
}

DEFINE_TEST(test_spawn_async_error)
{
	GError *error = NULL;
	guint ret;

	ret = egg_spawn_async_with_callbacks (testing_data_directory (),
	                                     error_argv, NULL, 0, NULL,
	                                     NULL, NULL,
	                                     NULL, &error);
	g_assert (ret == 0);
	g_assert (error != NULL);
	g_clear_error (&error);
}
