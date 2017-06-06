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










/*
 *
 *
 * DO NOT USE THESE CRAPPY TESTS AS EXAMPLE CODE. USE THE DOCS AND examples
 *
 *
 *
 */









#include <gconf/gconf.h>
#include <stdio.h>
#include <unistd.h>
#include <string.h>
#include <stdlib.h>
#include <math.h>
#include <locale.h>
#include <gconf/gconf-internals.h>
#include <gconf/gconf-backend.h>

static void
check(gboolean condition, const gchar* fmt, ...)
{
  va_list args;
  gchar* description;
  
  va_start (args, fmt);
  description = g_strdup_vprintf(fmt, args);
  va_end (args);
  
  if (condition)
    {
      printf(".");
      fflush(stdout);
    }
  else
    {
      fprintf(stderr, "\n*** FAILED: %s\n", description);
      exit(1);
    }
  
  g_free(description);
}

typedef struct {
  const gchar* protocol;
  const gchar** flags;
  const gchar* resource;
} AddressComponents;

static const gchar* flags1[] = { "readwrite", "someflag", "otherflag", NULL };
static const gchar* flags2[] = { "tuptuptup", "boop", "soop", NULL };

static const AddressComponents
addresses[] = {
  { "xml", flags1, "sdjf0q45093485093jta/s///;;;;,,,::: : : sdu1p" },
  { "foobar", flags2, ":::s;dkfljal;kjk; ;,,,,;;skdj;j" },
  { NULL, NULL, NULL }
};

int 
main (int argc, char** argv)
{
  const AddressComponents* iter;
  
  iter = addresses;

  while (iter->protocol)
    {
      gchar* address;
      gchar* flat_flags;
      gchar** flags;
      gchar*  protocol;
      gchar*  resource;
      
      flat_flags = g_strjoinv (",", iter->flags);

      address = g_strconcat(iter->protocol, ":", flat_flags, ":", iter->resource, NULL);

      g_free(flat_flags);
      
      protocol = gconf_address_backend(address);

      check(protocol != NULL,
            "protocol wasn't extracted from `%s'",
            address);
      check (strcmp(protocol, iter->protocol) == 0,
             "protocol extracted was wrong: `%s' vs. `%s' from `%s'",
             protocol, iter->protocol, address);
      
      resource = gconf_address_resource(address);

      check(resource != NULL,
            "resource wasn't extracted from `%s'",
            address);      

      check (strcmp(resource, iter->resource) == 0,
             "resource extracted was wrong: `%s' vs. `%s' from `%s'",
             resource, iter->resource, address);
      
      flags = gconf_address_flags(address);

      check(flags != NULL,
            "flags weren't extracted from `%s'",
            address);      
      
      {
        const gchar** correct_flag;
        gchar** our_flag;

        correct_flag = iter->flags;
        our_flag = flags;

        while (*correct_flag && *our_flag)
          {
            check(strcmp(*correct_flag, *our_flag) == 0,
                  "retrieved bad flag, `%s' vs. `%s' from `%s'",
                  *correct_flag, *our_flag, address);
            

            ++correct_flag;
            ++our_flag;
          }

        check(*correct_flag == NULL,
              "too few flags retrieved from `%s'",
              address);

        check(*our_flag == NULL,
              "too many flags retrieved from `%s'",
              address);
      }

      g_free(address);
      g_free(resource);
      g_free(protocol);
      g_strfreev(flags);
      
      ++iter;
    }

  {
    gchar** flags;

    flags = gconf_address_flags("xml::foobar");

    check(flags == NULL,
          "flags were not NULL despite their absence");

    g_strfreev(flags);
  }
  
  printf("\n");
  
  return 0;
}
