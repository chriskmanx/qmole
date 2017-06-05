/* -*- Mode: C; indent-tabs-mode: t; c-basic-offset: 8; tab-width: 8 -*- */
/* testing.h: Declarations for common functions called from gtest unit tests

   Copyright (C) 2008 Stefan Walter

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

#ifndef TESTING_H_
#define TESTING_H_

#include "config.h"

#include <glib.h>
#include <glib-object.h>
#include <glib/gstdio.h>

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <unistd.h>

gboolean         testing_wait_until               (gint timeout);

void             testing_wait_stop                (void);

const gchar*     testing_data_directory           (void);

const gchar*     testing_scratch_directory        (void);

guchar*          testing_data_read                (const gchar *basename,
                                                   gsize *n_data);

gchar*           testing_scratch_filename         (const gchar *basename);

gchar*           testing_data_filename            (const gchar *basename);

#ifdef CRYPTOKI_VERSION_MAJOR

void             testing_test_p11_module          (CK_FUNCTION_LIST_PTR module,
                                                   const gchar *config);

#endif

typedef void     (*TestingExternalFunc)           (void);

void             testing_external_run             (const gchar *name,
                                                   TestingExternalFunc func,
                                                   int *result);

const gchar*     testing_external_name            (void);

void             testing_external_fail            (void);

#define DECLARE_SETUP(x) \
	void setup_##x(int *v, gconstpointer d)
#define DEFINE_SETUP(x) \
	void setup_##x(int *__unused G_GNUC_UNUSED, gconstpointer __data G_GNUC_UNUSED)

#define DECLARE_TEARDOWN(x) \
	void teardown_##x(int *v, gconstpointer d)
#define DEFINE_TEARDOWN(x) \
	void teardown_##x(int *__unused G_GNUC_UNUSED, gconstpointer __data G_GNUC_UNUSED)

#define DECLARE_TEST(x) \
	void test_##x(int *v, gconstpointer d)
#define DEFINE_TEST(x) \
	void test_##x(int *__unused G_GNUC_UNUSED, gconstpointer __data G_GNUC_UNUSED)

#define DECLARE_START(x) \
	void start_##x(void)
#define DEFINE_START(x) \
	void start_##x(void)

#define DECLARE_STOP(x) \
	void stop_##x(void)
#define DEFINE_STOP(x) \
	void stop_##x(void)

#define DECLARE_EXTERNAL(x) \
	void external_##x(void)
#define DEFINE_EXTERNAL(x) \
	void external_##x(void)

#ifndef g_assert_cmpsize
#define g_assert_cmpsize(a, o, b) \
	g_assert_cmpuint ((guint)(a), o, (guint)(b))
#endif

#endif /* TESTING_H_ */
