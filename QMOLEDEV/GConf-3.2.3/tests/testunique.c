/* GConf
 * Copyright (C) 1999, 2000 Red Hat Inc.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Library General Public License for more details.
 *
 * You should have received a copy of the GNU Library General Public
 * License along with this library; if not, write to the
 * Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
 * Boston, MA 02110-1301, USA.
 */

/* This isn't an automated test */

#include <gconf/gconf.h>
#include <stdio.h>
#include <unistd.h>
#include <math.h>
#include <locale.h>

int 
main (int argc, char** argv)
{
  guint i = 0;

  while (i < 50)
    {
      gchar* key;

      key = gconf_unique_key();

      printf("key: %s\n", key);

      g_free(key);

      ++i;
    }
  
  return 0;
}
