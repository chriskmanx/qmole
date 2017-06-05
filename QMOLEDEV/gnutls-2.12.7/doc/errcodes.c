/*
 * Copyright (C) 2004, 2005, 2006, 2007, 2008, 2009, 2010 Free Software
 * Foundation, Inc.
 * Author: Nikos Mavrogiannopoulos, Simon Josefsson
 *
 * This file is part of GnuTLS.
 *
 * GnuTLS is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * GnuTLS is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <gnutls/gnutls.h>

typedef struct
{
  char name[128];
  int error_index;
} error_name;


static int
compar (const void *_n1, const void *_n2)
{
  const error_name *n1 = (const error_name *) _n1,
    *n2 = (const error_name *) _n2;
  return strcmp (n1->name, n2->name);
}

int
main (int argc, char *argv[])
{
  int i, j;
  const char *desc;
  const char *_name;
  error_name names_to_sort[400];        /* up to 400 names  */

  printf ("@table @code\n");

  memset (names_to_sort, 0, sizeof (names_to_sort));
  j = 0;
  for (i = 0; i > -400; i--)
    {
      _name = gnutls_strerror_name (i);
      if (_name == NULL)
        continue;

      strcpy (names_to_sort[j].name, _name);
      names_to_sort[j].error_index = i;
      j++;
    }

  qsort (names_to_sort, j, sizeof (error_name), compar);

  for (i = 0; i < j; i++)
    {
      _name = names_to_sort[i].name;
      desc = gnutls_strerror (names_to_sort[i].error_index);
      if (desc == NULL || _name == NULL)
        continue;

      printf ("@item %s:\n%s\n\n", _name, desc);
    }

  printf ("@end table\n");

  return 0;
}
