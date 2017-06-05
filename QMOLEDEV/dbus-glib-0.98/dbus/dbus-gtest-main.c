/* -*- mode: C; c-file-style: "gnu" -*- */
/* dbus-gtest-main.c  Program to run all libdbus-glib tests
 *
 * Copyright (C) 2003  Red Hat Inc.
 *
 * Licensed under the Academic Free License version 2.1
 * 
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
 *
 */
#include <config.h>

#ifdef DBUS_BUILD_TESTS

#include "dbus-gtest.h"
#include <stdio.h>
#include <stdlib.h>
#include <locale.h>

int
main (int    argc,
      char **argv)
{
  const char *test_data_dir;

  setlocale(LC_ALL, "");

  
  if (argc > 1)
    test_data_dir = argv[1];
  else
    test_data_dir = NULL;
  
  dbus_glib_internal_do_not_use_run_tests (test_data_dir);
  
  return 0;
}

#endif
