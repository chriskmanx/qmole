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

#include <gconf/gconf.h>
#include <gconf/gconf-internals.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <unistd.h>
#include <math.h>
#include <locale.h>

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

static const gchar*
keys[] = {
  "/testing/foo/tar",
  "/testing/foo/bar",
  "/testing/quad",
  "/testing/blah",
  "/testing/q/a/b/c/z/w/x/y/z",
  "/testing/foo/baz",
  "/testing/oops/bloo",
  "/testing/oops/snoo",
  "/testing/oops/kwoo",
  "/testing/foo/quaz",
  NULL
};

static const gchar*
quote_success_tests[] = {
  "sd;kjapoirgupoi4ut349u3940 u09ujt l;k234jtop ub0[t9\"\"\"\"\"\"\"\"\"\"\"\"\"\"\" \"\"\" \"\" \\\\\\d \\sdf \\d \\\\\f \\\\\\f\\\\g a|\"\\|'|'|'|\"|\"|\"|\"|\"|\"|\"|\"|\"|\"|'|'|\"|\"|\"|\"|\"|\"\\""\\\"|\\||||||||\\\\\\\\\\||\"|\"|\"|\"|\"\"\"\"\"\"\"\"\"\\||||||||||||\"\"\"\"\"\"|\"|\"\\\"|\"|\"|\\\"\"\"\"\"\"\"\"\"\"\"\"\"\"\"\"||\"\"\"\"\"\"\"\"|||\\\\\\\\\\\\\\\\\\||\\\\\"\"\"||\\\\|\"|\"",
  "\"\"\"\"\"\"\"\"\"\"\"\"\"\"\"\"\"\"\"\"\"",
  "\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\",
  "''''''''''''''''''''''''''''''''''",
  ",,,,,,,,,,,,,,,,,,,,,,,,,",
  ",\"\\dfkljs;kldjg",
  "",
  " ",
  "\"",
  "\\",
  "\\\\",
  "\"\"",
  NULL
};

static const gchar*
unquote_shouldfail_tests[] = {
  "blah blah blah blah \"", /* no start quote */
  "\"blah blah blah blah", /* no end quote */
  NULL
};

static gint ints[] = { -1, -2, -3, 0, 1, 2, 3, 4000, 0xfffff, -0xfffff, G_MININT, G_MAXINT, 0, 0, 57, 83, 95 };
static const guint n_ints = sizeof(ints)/sizeof(ints[0]);

static int
null_safe_strcmp(const char* lhs, const char* rhs)
{
  if (lhs == NULL && rhs == NULL)
    return 0;
  else if (lhs == NULL)
    return 1;
  else if (rhs == NULL)
    return -1;
  else
    return strcmp(lhs, rhs);
}

static void
print_parallel(const gchar* r, const gchar* l)
{
  while (*r && *l)
    {
      if (*r != *l)
        printf("[%c %c]", *r, *l);
      else
        printf("(%c %c)", *r, *l);

      ++r;
      ++l;
    }

  if (*r)
    printf("[... %s]", r);

  if (*l)
    printf("[... %s]", l);
}

static void
check_quoting(void)
{
  const gchar** testp;

  testp = quote_success_tests;

  while (*testp)
    {
      gchar* quoted;
      gchar* unquoted;
      const gchar* end;
      GError* error = NULL;
      
      quoted = gconf_quote_string(*testp);

      unquoted = gconf_unquote_string(quoted, &end, &error);

      /*       print_parallel(*testp, unquoted); */

      check (error == NULL,
             "error parsing quoted string: %s",
             error ? error->message : "<NULL>");
      
      check (strcmp(*testp, unquoted) == 0,
             "Original: `%s'\nUnquoted: `%s'\nNot equal?",
             *testp, unquoted);

      check (*end == '\0',
             "Entire string not unquoted?\nOriginal: `%s'\nQuoted: `%s'\nUnquoted: `%s'\nLeftover: `%s\n",
             *testp, quoted, unquoted, end);
      
      g_free(quoted);
      g_free(unquoted);

      ++testp;
    }

}

int 
main (int argc, char** argv)
{
  GConfEngine* conf;
  GError* err = NULL;
  
  printf("\nChecking string quoting:");
  
  check_quoting();

  printf("\n\n");
  
  return 0;
}
