/*
 * Copyright (C) 2010
 * Free Software Foundation, Inc.
 *
 * Author: Nikos Mavrogiannopoulos
 *
 * This file is part of GnuTLS.
 *
 * The GnuTLS is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public License
 * as published by the Free Software Foundation; either version 2.1 of
 * the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301,
 * USA
 *
 */

#include "dlopen.h"

#ifdef _WIN32

#include <windows.h>

void *
dlopen (const char *filename, int flag)
{
  return LoadLibrary (filename);
}


void *
dlsym (void *handle, const char *symbol)
{
  return GetProcAddress ((HINSTANCE) handle, symbol);
}

int
dlclose (void *handle)
{
  return !FreeLibrary ((HINSTANCE) handle);
}

#endif
