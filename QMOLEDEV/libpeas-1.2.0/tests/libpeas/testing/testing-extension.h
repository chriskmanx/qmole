/*
 * testing-extension.h
 * This file is part of libpeas
 *
 * Copyright (C) 2011 - Garrett Regier
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU Library General Public License as published by
 *  the Free Software Foundation; either version 2 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU Library General Public License for more details.
 *
 *  You should have received a copy of the GNU Library General Public License
 *  along with this program; if not, write to the Free Software
 *  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
 */

#ifndef __TESTING_EXTENSION_H__
#define __TESTING_EXTENSION_H__

#include <libpeas/peas-engine.h>

#include "testing.h"

G_BEGIN_DECLS

typedef struct _TestingExtensionFixture_ TestingExtensionFixture_;

struct _TestingExtensionFixture_ {
  PeasEngine *engine;
};

void testing_extension_set_plugin_    (const gchar              *plugin);

void testing_extension_test_setup_    (TestingExtensionFixture_ *fixture,
                                       gconstpointer             data);
void testing_extension_test_teardown_ (TestingExtensionFixture_ *fixture,
                                       gconstpointer             data);
void testing_extension_test_runner_   (TestingExtensionFixture_ *fixture,
                                       gconstpointer             data);

void testing_extension_garbage_collect_           (PeasEngine *engine);
void testing_extension_provides_valid_            (PeasEngine *engine);
void testing_extension_provides_invalid_          (PeasEngine *engine);
void testing_extension_create_valid_              (PeasEngine *engine);
void testing_extension_create_invalid_            (PeasEngine *engine);
void testing_extension_reload_                    (PeasEngine *engine);
void testing_extension_call_no_args_              (PeasEngine *engine);
void testing_extension_call_with_return_          (PeasEngine *engine);
void testing_extension_call_single_arg_           (PeasEngine *engine);
void testing_extension_call_multi_args_           (PeasEngine *engine);
void testing_extension_properties_construct_only_ (PeasEngine *engine);
void testing_extension_properties_read_only_      (PeasEngine *engine);
void testing_extension_properties_write_only_     (PeasEngine *engine);
void testing_extension_properties_readwrite_      (PeasEngine *engine);

#define _EXTENSION_TEST(loader, path, ftest) \
  g_test_add ("/extension/" #loader "/" path, TestingExtensionFixture_, \
              (gpointer) testing_extension_##ftest##_, \
              testing_extension_test_setup_, \
              testing_extension_test_runner_, \
              testing_extension_test_teardown_)

/* This also tests that loaders are loaded lazily */
#define EXTENSION_TESTS_INIT(loader) \
  testing_init (); \
  peas_engine_enable_loader (peas_engine_get_default (), #loader); \
  g_assert (g_type_from_name ("PeasPluginLoader") == G_TYPE_INVALID); \
  g_object_unref (peas_engine_get_default ()); \
  testing_extension_set_plugin_ ("extension-" #loader); \
\
  _EXTENSION_TEST (loader, "garbage-collect", garbage_collect); \
\
  _EXTENSION_TEST (loader, "provides-valid", provides_valid); \
  _EXTENSION_TEST (loader, "provides-invalid", provides_invalid); \
\
  _EXTENSION_TEST (loader, "create-valid", create_valid); \
  _EXTENSION_TEST (loader, "create-invalid", create_invalid); \
\
  _EXTENSION_TEST (loader, "reload", reload)

#define EXTENSION_TESTS_CALLABLE(loader) \
  _EXTENSION_TEST (loader, "call-no-args", call_no_args); \
  _EXTENSION_TEST (loader, "call-with-return", call_with_return); \
  _EXTENSION_TEST (loader, "call-single-arg", call_single_arg); \
  _EXTENSION_TEST (loader, "call-multi-args", call_multi_args)

#define EXTENSION_TESTS_PROPERTIES(loader) \
  _EXTENSION_TEST (loader, "properties-construct-only", properties_construct_only); \
  _EXTENSION_TEST (loader, "properties-read-only", properties_read_only); \
  _EXTENSION_TEST (loader, "properties-write-only", properties_write_only); \
  _EXTENSION_TEST (loader, "properties-readwrite", properties_readwrite)

#define EXTENSION_TESTS(loader) \
  EXTENSION_TESTS_INIT (loader); \
  EXTENSION_TESTS_CALLABLE (loader); \
  EXTENSION_TESTS_PROPERTIES (loader)

/* This macro is there to add loader-specific tests. */
#define EXTENSION_TEST(loader, path, func) \
  g_test_add ("/extension/" #loader "/" path, TestingExtensionFixture_, \
              (gpointer) test_extension_##loader##_##func, \
              testing_extension_test_setup_, \
              testing_extension_test_runner_, \
              testing_extension_test_teardown_)

G_END_DECLS

#endif /* __TESTING__EXTENSION_H__ */
