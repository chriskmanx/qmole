/* -*- mode: C; c-file-style: "gnu" -*- */
/* dbus-gutils.c Utils shared between convenience lib and installed lib
 *
 * Copyright (C) 2003  Red Hat, Inc.
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
#include "dbus-gutils.h"
#include "dbus-gtest.h"
#include <string.h>

char**
_dbus_gutils_split_path (const char *path)
{
  int len;
  char **split;
  int n_components;
  int i, j, comp;

  len = strlen (path);

  n_components = 0;
  if (path[1] != '\0') /* if not "/" */
    {
      i = 0;
      while (i < len)
        {
          if (path[i] == '/')
            n_components += 1;
          ++i;
        }
    }

  split = g_new0 (char*, n_components + 1);

  comp = 0;
  if (n_components == 0)
    i = 1;
  else
    i = 0;
  while (comp < n_components)
    {
      if (path[i] == '/')
        ++i;
      j = i;

      while (j < len && path[j] != '/')
        ++j;

      /* Now [i, j) is the path component */
      g_assert (i < j);
      g_assert (path[i] != '/');
      g_assert (j == len || path[j] == '/');

      split[comp] = g_strndup (&path[i], j - i + 1);

      split[comp][j-i] = '\0';

      ++comp;
      i = j;
    }
  g_assert (i == len);

  return split;
}

char*
_dbus_gutils_wincaps_to_uscore (const char *caps)
{
  const char *p;
  GString *str;

  str = g_string_new (NULL);
  p = caps;
  while (*p)
    {
      if (g_ascii_isupper (*p))
        {
          if (str->len > 0 &&
              (str->len < 2 || str->str[str->len-2] != '_'))
            g_string_append_c (str, '_');
          g_string_append_c (str, g_ascii_tolower (*p));
        }
      else
        {
          g_string_append_c (str, *p);
        }
      ++p;
    }

  return g_string_free (str, FALSE);
}
