/* GLIB - Library of useful routines for C programming
 * Copyright (C) 1995-1997  Peter Mattis, Spencer Kimball and Josh MacDonald
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the
 * Free Software Foundation, Inc., 59 Temple Place - Suite 330,
 * Boston, MA 02111-1307, USA.
 */

/*
 * Modified by the GLib Team and others 1997-2000.  See the AUTHORS
 * file for a list of people on the GLib Team.  See the ChangeLog
 * files for a list of changes.  These files are distributed with
 * GLib at ftp://ftp.gtk.org/pub/gtk/.
 */

/*
 * Code taken from glib-2.30.2, truncated, stripped & crippled down
 * to the minimum as required for use with pkg-config-lite.
 * For the full/complete glib code, refer to ftp://ftp.gtk.org/pub/gtk/.
 *
 * This snippet was created by Oliver Lange.
 * All changes from the original code fall under the GLib copyright.
 */

#include "glib-snippet.h"

#include <stdlib.h>
#include <stdarg.h>
#include <stdio.h>
#include <string.h>
#include <errno.h>
#ifdef G_OS_WIN32
#include <windows.h>
#else
#include <sys/stat.h>
#endif

#if defined __OpenBSD__
// only required by the goption.c snippet; not sure if any of these are
// still required, as some OpenBSD-related code from glib's goption.c is gone.
#include <sys/types.h>
#include <unistd.h>
#include <sys/param.h>
#endif

//
// BEGIN gquark.c
//
static gchar const *  glib_snippet_quark_strings[] =
{
  // glib-snippet for pkg-config-lite defines the following quarks:
  "(unknown-quark)",              // 0
  "g-shell-error-quark",          // 1
  "g-option-context-error-quark"  // 2
};

GQuark
g_quark_from_static_string(const gchar *  string)
{
  GQuark  q;

  for(q=0; q<(sizeof(glib_snippet_quark_strings)/sizeof(*glib_snippet_quark_strings)); q++)
  {
    if(!strcmp(string, glib_snippet_quark_strings[q]))
    {
      return(q);
    }
  }
  // unknown quark string
  return(0);
}

const gchar *
g_quark_to_string(GQuark  quark)
{
  if(quark >= (sizeof(glib_snippet_quark_strings)/sizeof(*glib_snippet_quark_strings)))
  {
    quark = 0;
  }
  return(glib_snippet_quark_strings[quark]);
}
//
// END gquark.c
//

//
// BEGIN gerror.c
//
GError*
g_error_new_valist (GQuark       domain,
                    gint         code,
                    const gchar *format,
                    va_list      args)
{
  GError *error;

  if(!domain)
    fprintf(stderr, "WARNING: g_error_new_valist: error domain quark is zero.\n");
  if(!format)
    fprintf(stderr, "WARNING: g_error_new_valist: error format string is NULL.\n");

  error = g_malloc(sizeof(GError));

  error->domain = domain;
  error->code = code;
  error->message = g_strdup_vprintf (format, args);

  return error;
}

// currently unused
//GError*
//g_error_new (GQuark       domain,
//             gint         code,
//             const gchar *format,
//             ...)
//{
//  GError* error;
//  va_list args;
//
//  g_return_val_if_fail (format != NULL, NULL);
//  g_return_val_if_fail (domain != 0, NULL);
//
//  va_start (args, format);
//  error = g_error_new_valist (domain, code, format, args);
//  va_end (args);
//
//  return error;
//}

GError*
g_error_new_literal (GQuark         domain,
                     gint           code,
                     const gchar   *message)
{
  GError* err;

  if(!message
  || !domain)
    return(NULL);

  err = g_malloc(sizeof(GError));

  err->domain = domain;
  err->code = code;
  err->message = g_strdup (message);

  return err;
}

void
g_error_free (GError *error)
{
  if(error)
  {
    g_free (error->message);
    g_free (error);
  }
}

#define ERROR_OVERWRITTEN_WARNING "WARNING: GError set over the top of a previous GError or uninitialized memory.\n" \
               "This indicates a bug in someone's code. You must ensure an error is NULL before it's set.\n" \
               "The overwriting error message was: %s\n"

void
g_set_error (GError      **err,
             GQuark        domain,
             gint          code,
             const gchar  *format,
             ...)
{
  GError *new;

  va_list args;

  if (err == NULL)
    return;

  va_start (args, format);
  new = g_error_new_valist (domain, code, format, args);
  va_end (args);

  if (*err == NULL)
    *err = new;
  else
    fprintf(stderr, ERROR_OVERWRITTEN_WARNING, new->message);
}

void
g_set_error_literal (GError      **err,
                     GQuark        domain,
                     gint          code,
                     const gchar  *message)
{
  GError *new;

  if (err == NULL)
    return;

  new = g_error_new_literal (domain, code, message);
  if (*err == NULL)
    *err = new;
  else
    fprintf(stderr, ERROR_OVERWRITTEN_WARNING, new->message);
}
//
// END gerror.c
//

//
// BEGIN gmem.c
//
//#include "gmem.h"

//#include <stdlib.h>
//#include <stdarg.h>
//#include <stdio.h>

//
// g_error() replacement function for use without g_logv()
//
static void
g_error (const gchar *format,
         ...)
{
  va_list args;
  va_start (args, format);
  fprintf(stderr, format, args);
  va_end (args);
  // shutdown now
  abort();
}

gpointer
g_malloc (gsize n_bytes)
{
  if (n_bytes)
  {
    gpointer mem;

    mem = malloc (n_bytes);
    if (mem)
      return mem;

    g_error ("%s: out of memory", G_STRLOC);
  }
  return NULL;
}

gpointer
g_malloc0 (gsize n_bytes)
{
  if (n_bytes)
  {
    gpointer mem;

    mem = calloc (1, n_bytes);
    if (mem)
      return mem;

    g_error ("%s: out of memory", G_STRLOC);
  }
  return NULL;
}

gpointer
g_realloc (gpointer mem,
           gsize    n_bytes)
{
  gpointer newmem;

  if (n_bytes)
  {
    newmem = realloc (mem, n_bytes);
    if (newmem)
      return newmem;

    g_error ("%s: out of memory", G_STRLOC);
  }

  if (mem)
    free (mem);
  return NULL;
}

void
g_free (gpointer mem)
{
  if (mem)
    free (mem);
}
//
// END gmem.c
//

//
// BEGIN gstrfuncs.c
//
//#include "gstrfuncs.h"

//#include "gmem.h"
//#include "gslist.h"

//#include <stdio.h>
//#include <stdlib.h>
//#include <string.h>

static const guint16 ascii_table_data[256] = {
  0x004, 0x004, 0x004, 0x004, 0x004, 0x004, 0x004, 0x004,
  0x004, 0x104, 0x104, 0x004, 0x104, 0x104, 0x004, 0x004,
  0x004, 0x004, 0x004, 0x004, 0x004, 0x004, 0x004, 0x004,
  0x004, 0x004, 0x004, 0x004, 0x004, 0x004, 0x004, 0x004,
  0x140, 0x0d0, 0x0d0, 0x0d0, 0x0d0, 0x0d0, 0x0d0, 0x0d0,
  0x0d0, 0x0d0, 0x0d0, 0x0d0, 0x0d0, 0x0d0, 0x0d0, 0x0d0,
  0x459, 0x459, 0x459, 0x459, 0x459, 0x459, 0x459, 0x459,
  0x459, 0x459, 0x0d0, 0x0d0, 0x0d0, 0x0d0, 0x0d0, 0x0d0,
  0x0d0, 0x653, 0x653, 0x653, 0x653, 0x653, 0x653, 0x253,
  0x253, 0x253, 0x253, 0x253, 0x253, 0x253, 0x253, 0x253,
  0x253, 0x253, 0x253, 0x253, 0x253, 0x253, 0x253, 0x253,
  0x253, 0x253, 0x253, 0x0d0, 0x0d0, 0x0d0, 0x0d0, 0x0d0,
  0x0d0, 0x473, 0x473, 0x473, 0x473, 0x473, 0x473, 0x073,
  0x073, 0x073, 0x073, 0x073, 0x073, 0x073, 0x073, 0x073,
  0x073, 0x073, 0x073, 0x073, 0x073, 0x073, 0x073, 0x073,
  0x073, 0x073, 0x073, 0x0d0, 0x0d0, 0x0d0, 0x0d0, 0x004
  /* the upper 128 are all zeroes */
};

const guint16 * const g_ascii_table = ascii_table_data;

gchar*
g_strdup (const gchar *str)
{
  gchar *new_str;
  gsize length;

  if (str)
  {
    length = strlen (str) + 1;
    new_str = g_new (char, length);
    memcpy (new_str, str, length);
  }
  else
    new_str = NULL;

  return new_str;
}

gpointer
g_memdup (gconstpointer mem,
          guint         byte_size)
{
  gpointer new_mem;

  if (mem)
  {
    new_mem = g_malloc (byte_size);
    memcpy (new_mem, mem, byte_size);
  }
  else
    new_mem = NULL;

  return new_mem;
}

gchar*
g_strndup (const gchar *str,
           gsize        n)
{
  gchar *new_str;

  if (str)
  {
    new_str = g_new (gchar, n + 1);
    strncpy (new_str, str, n);
    new_str[n] = '\0';
  }
  else
    new_str = NULL;

  return new_str;
}

gchar*
g_strnfill (gsize length,
            gchar fill_char)
{
  gchar *str;

  str = g_new (gchar, length + 1);
  memset (str, (guchar)fill_char, length);
  str[length] = '\0';

  return str;
}

gchar *
g_stpcpy (gchar       *dest,
          const gchar *src)
{
  register gchar *d = dest;
  register const gchar *s = src;
  if(!src || !dest)
  {
    return(NULL);
  }
  do
    *d++ = *s;
  while (*s++ != '\0');

  return d - 1;
}

//
// g_strdup_vprintf(): replacement function (simplified)
//
gchar *
g_strdup_vprintf (const gchar *  format,
                   va_list        args)
{
  size_t  bufSize = 64;
  gchar * result;
  int     len = 0;
  va_list tmp_args;

  do
  {
    if(!(result = g_malloc(bufSize)))
    {
      fprintf(stderr, "ERROR: out of memory.\n");
      abort();
    }
    *result = 0;
    if(format && *format)
    {
      va_copy(tmp_args, args);
      len = vsnprintf(result, bufSize, format, tmp_args);
      va_end(tmp_args);
    }
    // Win32 (and old glibc) return -1 on failure instead of the required size
    if(len >= 0 && (size_t)(len) < bufSize)
    {
      // success
      return(result);
    }
    // need bigger buffer
    bufSize = len >= 0 ? len+1 : bufSize<<1;
    g_free(result);
  }while(1);
  return(NULL);
}

gchar*
g_strdup_printf (const gchar *format,
                 ...)
{
  gchar *buffer;
  va_list args;

  va_start (args, format);
  buffer = g_strdup_vprintf (format, args);
  va_end (args);

  return buffer;
}

gchar*
g_strconcat (const gchar *string1, ...)
{
  gsize   l;
  va_list args;
  gchar   *s;
  gchar   *concat;
  gchar   *ptr;

  if (!string1)
    return NULL;

  l = 1 + strlen (string1);
  va_start (args, string1);
  s = va_arg (args, gchar*);
  while (s)
  {
    l += strlen (s);
    s = va_arg (args, gchar*);
  }
  va_end (args);

  concat = g_new (gchar, l);
  ptr = concat;

  ptr = g_stpcpy (ptr, string1);
  va_start (args, string1);
  s = va_arg (args, gchar*);
  while (s)
  {
    ptr = g_stpcpy (ptr, s);
    s = va_arg (args, gchar*);
  }
  va_end (args);

  return concat;
}

gchar
g_ascii_tolower (gchar c)
{
  return g_ascii_isupper (c) ? c - 'A' + 'a' : c;
}

gchar
g_ascii_toupper (gchar c)
{
  return g_ascii_islower (c) ? c - 'a' + 'A' : c;
}

#define ISUPPER(c)              ((c) >= 'A' && (c) <= 'Z')
#define TOLOWER(c)              (ISUPPER (c) ? (c) - 'A' + 'a' : (c))

gint
g_ascii_strcasecmp (const gchar *s1,
                    const gchar *s2)
{
  gint c1, c2;

  if(!s1 || !s2)
  {
    return(0);
  }
  while (*s1 && *s2)
  {
    c1 = (gint)(guchar) TOLOWER (*s1);
    c2 = (gint)(guchar) TOLOWER (*s2);
    if (c1 != c2)
      return (c1 - c2);
    s1++; s2++;
  }

  return (((gint)(guchar) *s1) - ((gint)(guchar) *s2));
}

gchar*
g_strchug (gchar *string)
{
  guchar *start;

  if(!string)
  {
    return(NULL);
  }
  for (start = (guchar*) string; *start && g_ascii_isspace (*start); start++)
  ;

  memmove (string, start, strlen ((gchar *) start) + 1);

  return string;
}

gchar*
g_strchomp (gchar *string)
{
  gsize len;

  if(!string)
  {
    return(NULL);
  }
  len = strlen (string);
  while (len--)
  {
    if (g_ascii_isspace ((guchar) string[len]))
      string[len] = '\0';
    else
      break;
  }

  return string;
}

//
// g_strsplit: in contrast to the original glib function, this function is
// using guint instead of gint for max_tokens, where 0 still means no limit.
// This is used to get rid of G_MAXINT and it also makes sense to use guint
// for max_tokens anyhow.
//
gchar**
g_strsplit (const gchar *string,
            const gchar *delimiter,
            guint        max_tokens)
{
  GSList *string_list = NULL, *slist;
  gchar **str_array, *s;
  guint n = 0;
  const gchar *remainder;

  if(!string || !delimiter || !*delimiter)
  {
    return(NULL);
  }

  if (max_tokens < 1)
    max_tokens = ~((guint)0); //G_MAXINT;

  remainder = string;
  s = strstr (remainder, delimiter);
  if (s)
  {
    gsize delimiter_len = strlen (delimiter);

    while (--max_tokens && s)
    {
      gsize len;

      len = s - remainder;
      string_list = g_slist_prepend (string_list,
                                     g_strndup (remainder, len));
      n++;
      remainder = s + delimiter_len;
      s = strstr (remainder, delimiter);
    }
  }
  if (*string)
  {
    n++;
    string_list = g_slist_prepend (string_list, g_strdup (remainder));
  }

  str_array = g_new (gchar*, n + 1);

  str_array[n--] = NULL;
  for (slist = string_list; slist; slist = slist->next)
    str_array[n--] = slist->data;

  g_slist_free (string_list);

  return str_array;
}

void
g_strfreev (gchar **str_array)
{
  if (str_array)
  {
    int i;

    for (i = 0; str_array[i] != NULL; i++)
      g_free (str_array[i]);

    g_free (str_array);
  }
}

gchar**
g_strdupv (gchar **str_array)
{
  if (str_array)
  {
    gint i;
    gchar **retval;

    i = 0;
    while (str_array[i])
      ++i;

    retval = g_new (gchar*, i + 1);

    i = 0;
    while (str_array[i])
    {
      retval[i] = g_strdup (str_array[i]);
      ++i;
    }
    retval[i] = NULL;

    return retval;
  }
  else
    return NULL;
}
//
// END gstrfuncs.c
//

//
// BEGIN gshell.c
//
GQuark
g_shell_error_quark (void)
{
  return g_quark_from_static_string ("g-shell-error-quark");
}

static gboolean
unquote_string_inplace (gchar* str, gchar** end, GError** err)
{
  gchar* dest;
  gchar* s;
  gchar quote_char;

  if(!end
  || (err != NULL && *err != NULL)
  || !str)
    return(FALSE);

  dest = s = str;

  quote_char = *s;

  if (!(*s == '"' || *s == '\''))
    {
      g_set_error_literal (err,
                           G_SHELL_ERROR,
                           G_SHELL_ERROR_BAD_QUOTING,
                           "Quoted text doesn't begin with a quotation mark");
      *end = str;
      return FALSE;
    }

  /* Skip the initial quote mark */
  ++s;

  if (quote_char == '"')
    {
      while (*s)
        {
          g_assert(s > dest); /* loop invariant */

          switch (*s)
            {
            case '"':
              /* End of the string, return now */
              *dest = '\0';
              ++s;
              *end = s;
              return TRUE;
              break;

            case '\\':
              /* Possible escaped quote or \ */
              ++s;
              switch (*s)
                {
                case '"':
                case '\\':
                case '`':
                case '$':
                case '\n':
                  *dest = *s;
                  ++s;
                  ++dest;
                  break;

                default:
                  /* not an escaped char */
                  *dest = '\\';
                  ++dest;
                  /* ++s already done. */
                  break;
                }
              break;

            default:
              *dest = *s;
              ++dest;
              ++s;
              break;
            }

          g_assert(s > dest); /* loop invariant */
        }
    }
  else
    {
      while (*s)
        {
          g_assert(s > dest); /* loop invariant */

          if (*s == '\'')
            {
              /* End of the string, return now */
              *dest = '\0';
              ++s;
              *end = s;
              return TRUE;
            }
          else
            {
              *dest = *s;
              ++dest;
              ++s;
            }

          g_assert(s > dest); /* loop invariant */
        }
    }

  /* If we reach here this means the close quote was never encountered */

  *dest = '\0';

  g_set_error_literal (err,
                       G_SHELL_ERROR,
                       G_SHELL_ERROR_BAD_QUOTING,
                       "Unmatched quotation mark in command line or other shell-quoted text");
  *end = s;
  return FALSE;
}

gchar*
g_shell_unquote (const gchar *quoted_string,
                 GError     **error)
{
  gchar *unquoted;
  gchar *end;
  gchar *start;
  GString *retval;

  if(!quoted_string)
    return(NULL);

  unquoted = g_strdup (quoted_string);

  start = unquoted;
  end = unquoted;
  retval = g_string_new (NULL);

  /* The loop allows cases such as
   * "foo"blah blah'bar'woo foo"baz"la la la\'\''foo'
   */
  while (*start)
    {
      /* Append all non-quoted chars, honoring backslash escape
       */

      while (*start && !(*start == '"' || *start == '\''))
        {
          if (*start == '\\')
            {
              /* all characters can get escaped by backslash,
               * except newline, which is removed if it follows
               * a backslash outside of quotes
               */

              ++start;
              if (*start)
                {
                  if (*start != '\n')
                    g_string_append_c (retval, *start);
                  ++start;
                }
            }
          else
            {
              g_string_append_c (retval, *start);
              ++start;
            }
        }

      if (*start)
        {
          if (!unquote_string_inplace (start, &end, error))
            {
              goto error;
            }
          else
            {
              g_string_append (retval, start);
              start = end;
            }
        }
    }

  g_free (unquoted);
  return g_string_free (retval, FALSE);

 error:
  g_assert (error == NULL || *error != NULL);

  g_free (unquoted);
  g_string_free (retval, TRUE);
  return NULL;
}

static inline void
ensure_token (GString **token)
{
  if (*token == NULL)
    *token = g_string_new (NULL);
}

static void
delimit_token (GString **token,
               GSList **retval)
{
  if (*token == NULL)
    return;

  *retval = g_slist_prepend (*retval, g_string_free (*token, FALSE));

  *token = NULL;
}

static GSList*
tokenize_command_line (const gchar *command_line,
                       GError **error)
{
  gchar current_quote;
  const gchar *p;
  GString *current_token = NULL;
  GSList *retval = NULL;
  gboolean quoted;

  current_quote = '\0';
  quoted = FALSE;
  p = command_line;

  while (*p)
    {
      if (current_quote == '\\')
        {
          if (*p == '\n')
            {
              /* we append nothing; backslash-newline become nothing */
            }
          else
            {
              /* we append the backslash and the current char,
               * to be interpreted later after tokenization
               */
              ensure_token (&current_token);
              g_string_append_c (current_token, '\\');
              g_string_append_c (current_token, *p);
            }

          current_quote = '\0';
        }
      else if (current_quote == '#')
        {
          /* Discard up to and including next newline */
          while (*p && *p != '\n')
            ++p;

          current_quote = '\0';

          if (*p == '\0')
            break;
        }
      else if (current_quote)
        {
          if (*p == current_quote &&
              /* check that it isn't an escaped double quote */
              !(current_quote == '"' && quoted))
            {
              /* close the quote */
              current_quote = '\0';
            }

          /* Everything inside quotes, and the close quote,
           * gets appended literally.
           */

          ensure_token (&current_token);
          g_string_append_c (current_token, *p);
        }
      else
        {
          switch (*p)
            {
            case '\n':
              delimit_token (&current_token, &retval);
              break;

            case ' ':
            case '\t':
              /* If the current token contains the previous char, delimit
               * the current token. A nonzero length
               * token should always contain the previous char.
               */
              if (current_token &&
                  current_token->len > 0)
                {
                  delimit_token (&current_token, &retval);
                }

              /* discard all unquoted blanks (don't add them to a token) */
              break;


              /* single/double quotes are appended to the token,
               * escapes are maybe appended next time through the loop,
               * comment chars are never appended.
               */

            case '\'':
            case '"':
              ensure_token (&current_token);
              g_string_append_c (current_token, *p);

              /* FALL THRU */

            case '#':
            case '\\':
              current_quote = *p;
              break;

            default:
              /* Combines rules 4) and 6) - if we have a token, append to it,
               * otherwise create a new token.
               */
              ensure_token (&current_token);
              g_string_append_c (current_token, *p);
              break;
            }
        }

      /* We need to count consecutive backslashes mod 2,
       * to detect escaped doublequotes.
       */
      if (*p != '\\')
	quoted = FALSE;
      else
	quoted = !quoted;

      ++p;
    }

  delimit_token (&current_token, &retval);

  if (current_quote)
    {
      if (current_quote == '\\')
        g_set_error (error,
                     G_SHELL_ERROR,
                     G_SHELL_ERROR_BAD_QUOTING,
                     "Text ended just after a '\\' character."
                     " (The text was '%s')",
                     command_line);
      else
        g_set_error (error,
                     G_SHELL_ERROR,
                     G_SHELL_ERROR_BAD_QUOTING,
                     "Text ended before matching quote was found for %c."
                     " (The text was '%s')",
                     current_quote, command_line);

      goto error;
    }

  if (retval == NULL)
    {
      g_set_error_literal (error,
                           G_SHELL_ERROR,
                           G_SHELL_ERROR_EMPTY_STRING,
                           "Text was empty (or contained only whitespace)");

      goto error;
    }

  /* we appended backward */
  retval = g_slist_reverse (retval);

  return retval;

 error:
  g_assert (error == NULL || *error != NULL);

  g_slist_free_full (retval, g_free);

  return NULL;
}

gboolean
g_shell_parse_argv (const gchar *command_line,
                    gint        *argcp,
                    gchar     ***argvp,
                    GError     **error)
{
  /* Code based on poptParseArgvString() from libpopt */
  gint argc = 0;
  gchar **argv = NULL;
  GSList *tokens = NULL;
  gint i;
  GSList *tmp_list;

  if(!command_line)
    return(FALSE);

  tokens = tokenize_command_line (command_line, error);
  if (tokens == NULL)
    return FALSE;

  argc = g_slist_length (tokens);
  argv = g_new0 (gchar*, argc + 1);
  i = 0;
  tmp_list = tokens;
  while (tmp_list)
    {
      argv[i] = g_shell_unquote (tmp_list->data, error);

      /* Since we already checked that quotes matched up in the
       * tokenizer, this shouldn't be possible to reach I guess.
       */
      if (argv[i] == NULL)
        goto failed;

      tmp_list = g_slist_next (tmp_list);
      ++i;
    }

  g_slist_free_full (tokens, g_free);

  if (argcp)
    *argcp = argc;

  if (argvp)
    *argvp = argv;
  else
    g_strfreev (argv);

  return TRUE;

 failed:

  g_assert (error == NULL || *error != NULL);
  g_strfreev (argv);
  g_slist_free_full (tokens, g_free);

  return FALSE;
}
//
// END gshell.c
//

//
// BEGIN goption.c
//
//#include <string.h>
//#include <stdlib.h>
//#include <stdio.h>
//#include <errno.h>

//#if defined __OpenBSD__
//#include <sys/types.h>
//#include <unistd.h>
//#include <sys/param.h>
//#endif

#define TRANSLATE(group, str) (str)

#define NO_ARG(entry) ((entry)->arg == G_OPTION_ARG_NONE ||       \
                       ((entry)->arg == G_OPTION_ARG_CALLBACK &&  \
                        ((entry)->flags & G_OPTION_FLAG_NO_ARG)))

#define OPTIONAL_ARG(entry) ((entry)->arg == G_OPTION_ARG_CALLBACK &&  \
                       (entry)->flags & G_OPTION_FLAG_OPTIONAL_ARG)

typedef struct
{
  GOptionArg arg_type;
  gpointer arg_data;
  union
  {
    gboolean bool;
    gint integer;
    gchar *str;
    gchar **array;
    gdouble dbl;
    gint64 int64;
  } prev;
  union
  {
    gchar *str;
    struct
    {
      gint len;
      gchar **data;
    } array;
  } allocated;
} Change;

typedef struct
{
  gchar **ptr;
  gchar *value;
} PendingNull;

struct _GOptionContext
{
  GList           *groups;

  gchar           *parameter_string;
  gchar           *summary;
  gchar           *description;

  guint            help_enabled   : 1;
  guint            ignore_unknown : 1;

  GOptionGroup    *main_group;

  /* We keep a list of change so we can revert them */
  GList           *changes;

  /* We also keep track of all argv elements
   * that should be NULLed or modified.
   */
  GList           *pending_nulls;
};

struct _GOptionGroup
{
  gchar           *name;
  gchar           *description;
  gchar           *help_description;

  GDestroyNotify   destroy_notify;
  gpointer         user_data;

  GOptionEntry    *entries;
  gint             n_entries;

  GOptionParseFunc pre_parse_func;
  GOptionParseFunc post_parse_func;
  GOptionErrorFunc error_func;
};

//
// prgname (simplified for use with pkg-config-lite)
//
static char const * local_prgname = "<command>";

static void
set_prgname_from_arg0(char const * arg0)
{
  char const *  result;
  char const *  new_name;

  if((new_name = arg0))
  {
  #ifdef _WIN32
    if((result = strrchr(arg0, ':')))
    {
      arg0 = ++result;
    }
    if((result = strrchr(arg0, '\\')))
    {
      if(strrchr(arg0, '/') > result)
      {
        new_name = strrchr(arg0, '/')+1;
      }
      else
      {
        new_name = ++result;
      }
    }
    else
  #endif // _WIN32
    if((result = strrchr(arg0, '/')))
    {
      new_name = ++result;
    }
  }
  if(new_name && *new_name)
  {
    // success: set name
    local_prgname = new_name;
  }
}

static void free_changes_list (GOptionContext *context,
                               gboolean        revert);
static void free_pending_nulls (GOptionContext *context,
                                gboolean        perform_nulls);


#define _g_utf8_strwidth strlen

GQuark
g_option_error_quark (void)
{
  return g_quark_from_static_string ("g-option-context-error-quark");
}

GOptionContext *
g_option_context_new (const gchar *parameter_string)

{
  GOptionContext *context;

  context = g_new0 (GOptionContext, 1);

  context->parameter_string = g_strdup (parameter_string);
  context->help_enabled = TRUE;
  context->ignore_unknown = FALSE;

  return context;
}

void g_option_context_free (GOptionContext *context)
{
  if(!context)
    return;

  g_list_free_full (context->groups, (GDestroyNotify) g_option_group_free);

  if (context->main_group)
    g_option_group_free (context->main_group);

  free_changes_list (context, FALSE);
  free_pending_nulls (context, FALSE);

  g_free (context->parameter_string);
  g_free (context->summary);
  g_free (context->description);

  g_free (context);
}

void g_option_context_set_help_enabled (GOptionContext *context,
                                        gboolean        help_enabled)
{
  if(!context)
    return;

  context->help_enabled = help_enabled;
}

gboolean
g_option_context_get_help_enabled (GOptionContext *context)
{
  if(!context)
    return(FALSE);

  return context->help_enabled;
}

void
g_option_context_set_ignore_unknown_options (GOptionContext *context,
                                             gboolean        ignore_unknown)
{
  if(!context)
    return;

  context->ignore_unknown = ignore_unknown;
}

gboolean
g_option_context_get_ignore_unknown_options (GOptionContext *context)
{
  if(!context)
    return(FALSE);

  return context->ignore_unknown;
}

void
g_option_context_add_group (GOptionContext *context,
                            GOptionGroup   *group)
{
  GList *list;

  if(!context
  || !group
  || !group->name
  || !group->description
  || !group->help_description)
    return;

  for (list = context->groups; list; list = list->next)
    {
      GOptionGroup *g = (GOptionGroup *)list->data;

      if ((group->name == NULL && g->name == NULL) ||
          (group->name && g->name && strcmp (group->name, g->name) == 0))
            fprintf(stderr,
                    "WARNING: A group named \"%s\" is already part of this GOptionContext",
                    group->name);
    }

  context->groups = g_list_append (context->groups, group);
}

void
g_option_context_set_main_group (GOptionContext *context,
                                 GOptionGroup   *group)
{
  if(!context
  || !group)
    return;

  if (context->main_group)
    {
      fprintf(stderr, "WARNING: This GOptionContext already has a main group");
      return;
    }

  context->main_group = group;
}

GOptionGroup *
g_option_context_get_main_group (GOptionContext *context)
{
  if(!context)
    return(NULL);

  return context->main_group;
}

void
g_option_context_add_main_entries (GOptionContext      *context,
                                   const GOptionEntry  *entries,
                                   const gchar         *OBSOLETE_IGNORED_translation_domain)
{
  if(!entries)
    return;

  if (!context->main_group)
    context->main_group = g_option_group_new (NULL, NULL, NULL, NULL, NULL);

  g_option_group_add_entries (context->main_group, entries);
}

static gint
calculate_max_length (GOptionGroup *group)
{
  GOptionEntry *entry;
  gint i, len, max_length;

  max_length = 0;

  for (i = 0; i < group->n_entries; i++)
    {
      entry = &group->entries[i];

      if (entry->flags & G_OPTION_FLAG_HIDDEN)
        continue;

      len = _g_utf8_strwidth (entry->long_name);

      if (entry->short_name)
        len += 4;

      if (!NO_ARG (entry) && entry->arg_description)
        len += 1 + _g_utf8_strwidth (TRANSLATE (group, entry->arg_description));

      max_length = MAX (max_length, len);
    }

  return max_length;
}

static void
print_entry (GOptionGroup       *group,
             gint                max_length,
             const GOptionEntry *entry,
             GString            *string)
{
  GString *str;

  if (entry->flags & G_OPTION_FLAG_HIDDEN)
    return;

  if (entry->long_name[0] == 0)
    return;

  str = g_string_new (NULL);

  if (entry->short_name)
    g_string_append_printf (str, "  -%c, --%s", entry->short_name, entry->long_name);
  else
    g_string_append_printf (str, "  --%s", entry->long_name);

  if (entry->arg_description)
    g_string_append_printf (str, "=%s", TRANSLATE (group, entry->arg_description));

  g_string_append_printf (string, "%s%*s %s\n", str->str,
                          (int) (max_length + 4 - _g_utf8_strwidth (str->str)), "",
                          entry->description ? TRANSLATE (group, entry->description) : "");
  g_string_free (str, TRUE);
}

static gboolean
group_has_visible_entries (GOptionContext *context,
                           GOptionGroup *group,
                           gboolean      main_entries)
{
  GOptionFlags reject_filter = G_OPTION_FLAG_HIDDEN;
  GOptionEntry *entry;
  gint i, l;
  gboolean main_group = group == context->main_group;

  if (!main_entries)
    reject_filter |= G_OPTION_FLAG_IN_MAIN;

  for (i = 0, l = (group ? group->n_entries : 0); i < l; i++)
    {
      entry = &group->entries[i];

      if (main_entries && !main_group && !(entry->flags & G_OPTION_FLAG_IN_MAIN))
        continue;
      if (!(entry->flags & reject_filter))
        return TRUE;
    }

  return FALSE;
}

static gboolean
group_list_has_visible_entires (GOptionContext *context,
                                GList          *group_list,
                                gboolean       main_entries)
{
  while (group_list)
    {
      if (group_has_visible_entries (context, group_list->data, main_entries))
        return TRUE;

      group_list = group_list->next;
    }

  return FALSE;
}

static gboolean
context_has_h_entry (GOptionContext *context)
{
  gsize i;
  GList *list;

  if (context->main_group)
    {
      for (i = 0; i < context->main_group->n_entries; i++)
        {
          if (context->main_group->entries[i].short_name == 'h')
            return TRUE;
        }
    }

  for (list = context->groups; list != NULL; list = list->next)
    {
     GOptionGroup *group;

      group = (GOptionGroup*)list->data;
      for (i = 0; i < group->n_entries; i++)
        {
          if (group->entries[i].short_name == 'h')
            return TRUE;
        }
    }
  return FALSE;
}

gchar *
g_option_context_get_help (GOptionContext *context,
                           gboolean        main_help,
                           GOptionGroup   *group)
{
  GList *list;
  gint max_length, len;
  gint i;
  GOptionEntry *entry;
  GHashTable *shadow_map;
  gboolean seen[256];
  const gchar *rest_description;
  GString *string;
  guchar token;

  string = g_string_sized_new (1024);

  rest_description = NULL;
  if (context->main_group)
    {

      for (i = 0; i < context->main_group->n_entries; i++)
        {
          entry = &context->main_group->entries[i];
          if (entry->long_name[0] == 0)
            {
              rest_description = TRANSLATE (context->main_group, entry->arg_description);
              break;
            }
        }
    }

  g_string_append_printf (string, "%s\n  %s %s",
                          "Usage:", local_prgname, "[OPTION...]");

  if (rest_description)
    {
      g_string_append (string, " ");
      g_string_append (string, rest_description);
    }

  if (context->parameter_string)
    {
      g_string_append (string, " ");
      g_string_append (string, TRANSLATE (context, context->parameter_string));
    }

  g_string_append (string, "\n\n");

  if (context->summary)
    {
      g_string_append (string, TRANSLATE (context, context->summary));
      g_string_append (string, "\n\n");
    }

  memset (seen, 0, sizeof (gboolean) * 256);
  shadow_map = g_hash_table_new (g_str_hash, g_str_equal);

  if (context->main_group)
    {
      for (i = 0; i < context->main_group->n_entries; i++)
        {
          entry = &context->main_group->entries[i];
          g_hash_table_insert (shadow_map,
                               (gpointer)entry->long_name,
                               entry);

          if (seen[(guchar)entry->short_name])
            entry->short_name = 0;
          else
            seen[(guchar)entry->short_name] = TRUE;
        }
    }

  list = context->groups;
  while (list != NULL)
    {
      GOptionGroup *g = list->data;
      for (i = 0; i < g->n_entries; i++)
        {
          entry = &g->entries[i];
          if (g_hash_table_lookup (shadow_map, entry->long_name) &&
              !(entry->flags & G_OPTION_FLAG_NOALIAS))
            entry->long_name = g_strdup_printf ("%s-%s", g->name, entry->long_name);
          else
            g_hash_table_insert (shadow_map, (gpointer)entry->long_name, entry);

          if (seen[(guchar)entry->short_name] &&
              !(entry->flags & G_OPTION_FLAG_NOALIAS))
            entry->short_name = 0;
          else
            seen[(guchar)entry->short_name] = TRUE;
        }
      list = list->next;
    }

  g_hash_table_destroy (shadow_map);

  list = context->groups;

  max_length = _g_utf8_strwidth ("-?, --help");

  if (list)
    {
      len = _g_utf8_strwidth ("--help-all");
      max_length = MAX (max_length, len);
    }

  if (context->main_group)
    {
      len = calculate_max_length (context->main_group);
      max_length = MAX (max_length, len);
    }

  while (list != NULL)
    {
      GOptionGroup *g = list->data;

      /* First, we check the --help-<groupname> options */
      len = _g_utf8_strwidth ("--help-") + _g_utf8_strwidth (g->name);
      max_length = MAX (max_length, len);

      /* Then we go through the entries */
      len = calculate_max_length (g);
      max_length = MAX (max_length, len);

      list = list->next;
    }

  /* Add a bit of padding */
  max_length += 4;

  if (!group)
    {
      list = context->groups;

      token = context_has_h_entry (context) ? '?' : 'h';

      g_string_append_printf (string, "%s\n  -%c, --%-*s %s\n",
                              "Help Options:", token, max_length - 4, "help",
                              "Show help options");

      /* We only want --help-all when there are groups */
      if (list)
        g_string_append_printf (string, "  --%-*s %s\n",
                                max_length, "help-all",
                                "Show all help options");

      while (list)
        {
          GOptionGroup *g = list->data;

          if (group_has_visible_entries (context, g, FALSE))
            g_string_append_printf (string, "  --help-%-*s %s\n",
                                    max_length - 5, g->name,
                                    TRANSLATE (g, g->help_description));

          list = list->next;
        }

      g_string_append (string, "\n");
    }

  if (group)
    {
      /* Print a certain group */

      if (group_has_visible_entries (context, group, FALSE))
        {
          g_string_append (string, TRANSLATE (group, group->description));
          g_string_append (string, "\n");
          for (i = 0; i < group->n_entries; i++)
            print_entry (group, max_length, &group->entries[i], string);
          g_string_append (string, "\n");
        }
    }
  else if (!main_help)
    {
      /* Print all groups */

      list = context->groups;

      while (list)
        {
          GOptionGroup *g = list->data;

          if (group_has_visible_entries (context, g, FALSE))
            {
              g_string_append (string, g->description);
              g_string_append (string, "\n");
              for (i = 0; i < g->n_entries; i++)
                if (!(g->entries[i].flags & G_OPTION_FLAG_IN_MAIN))
                  print_entry (g, max_length, &g->entries[i], string);

              g_string_append (string, "\n");
            }

          list = list->next;
        }
    }

  /* Print application options if --help or --help-all has been specified */
  if ((main_help || !group) &&
      (group_has_visible_entries (context, context->main_group, TRUE) ||
       group_list_has_visible_entires (context, context->groups, TRUE)))
    {
      list = context->groups;

      g_string_append (string,  "Application Options:");
      g_string_append (string, "\n");
      if (context->main_group)
        for (i = 0; i < context->main_group->n_entries; i++)
          print_entry (context->main_group, max_length,
                       &context->main_group->entries[i], string);

      while (list != NULL)
        {
          GOptionGroup *g = list->data;

          /* Print main entries from other groups */
          for (i = 0; i < g->n_entries; i++)
            if (g->entries[i].flags & G_OPTION_FLAG_IN_MAIN)
              print_entry (g, max_length, &g->entries[i], string);

          list = list->next;
        }

      g_string_append (string, "\n");
    }

  if (context->description)
    {
      g_string_append (string, TRANSLATE (context, context->description));
      g_string_append (string, "\n");
    }

  return g_string_free (string, FALSE);
}

static void
print_help (GOptionContext *context,
            gboolean        main_help,
            GOptionGroup   *group)
{
  gchar *help;

  help = g_option_context_get_help (context, main_help, group);
  printf ("%s", help);
  g_free (help);

  exit (0);
}

static gboolean
parse_int (const gchar *arg_name,
           const gchar *arg,
           gint        *result,
           GError     **error)
{
  gchar *end;
  glong tmp;

  errno = 0;
  tmp = strtol (arg, &end, 0);

  if (*arg == '\0' || *end != '\0')
    {
      g_set_error (error,
                   G_OPTION_ERROR, G_OPTION_ERROR_BAD_VALUE,
                   "Cannot parse integer value '%s' for %s",
                   arg, arg_name);
      return FALSE;
    }

  *result = tmp;
  if (*result != tmp || errno == ERANGE)
    {
      g_set_error (error,
                   G_OPTION_ERROR, G_OPTION_ERROR_BAD_VALUE,
                   "Integer value '%s' for %s out of range",
                   arg, arg_name);
      return FALSE;
    }

  return TRUE;
}


static gboolean
parse_double (const gchar *arg_name,
           const gchar *arg,
           gdouble        *result,
           GError     **error)
{
  gchar *end;
  gdouble tmp;

  errno = 0;
  tmp = g_strtod (arg, &end);

  if (*arg == '\0' || *end != '\0')
    {
      g_set_error (error,
                   G_OPTION_ERROR, G_OPTION_ERROR_BAD_VALUE,
                   "Cannot parse double value '%s' for %s",
                   arg, arg_name);
      return FALSE;
    }
  if (errno == ERANGE)
    {
      g_set_error (error,
                   G_OPTION_ERROR, G_OPTION_ERROR_BAD_VALUE,
                   "Double value '%s' for %s out of range",
                   arg, arg_name);
      return FALSE;
    }

  *result = tmp;

  return TRUE;
}


static gboolean
parse_int64 (const gchar *arg_name,
             const gchar *arg,
             gint64      *result,
             GError     **error)
{
  gchar *end;
  gint64 tmp;

  errno = 0;
  tmp = g_ascii_strtoll (arg, &end, 0);

  if (*arg == '\0' || *end != '\0')
    {
      g_set_error (error,
                   G_OPTION_ERROR, G_OPTION_ERROR_BAD_VALUE,
                   "Cannot parse integer value '%s' for %s",
                   arg, arg_name);
      return FALSE;
    }
  if (errno == ERANGE)
    {
      g_set_error (error,
                   G_OPTION_ERROR, G_OPTION_ERROR_BAD_VALUE,
                   "Integer value '%s' for %s out of range",
                   arg, arg_name);
      return FALSE;
    }

  *result = tmp;

  return TRUE;
}


static Change *
get_change (GOptionContext *context,
            GOptionArg      arg_type,
            gpointer        arg_data)
{
  GList *list;
  Change *change = NULL;

  for (list = context->changes; list != NULL; list = list->next)
    {
      change = list->data;

      if (change->arg_data == arg_data)
        goto found;
    }

  change = g_new0 (Change, 1);
  change->arg_type = arg_type;
  change->arg_data = arg_data;

  context->changes = g_list_prepend (context->changes, change);

 found:

  return change;
}

static void
add_pending_null (GOptionContext *context,
                  gchar         **ptr,
                  gchar          *value)
{
  PendingNull *n;

  n = g_new0 (PendingNull, 1);
  n->ptr = ptr;
  n->value = value;

  context->pending_nulls = g_list_prepend (context->pending_nulls, n);
}

static gboolean
parse_arg (GOptionContext *context,
           GOptionGroup   *group,
           GOptionEntry   *entry,
           const gchar    *value,
           const gchar    *option_name,
           GError        **error)

{
  Change *change;

  g_assert (value || OPTIONAL_ARG (entry) || NO_ARG (entry));

  switch (entry->arg)
    {
    case G_OPTION_ARG_NONE:
      {
        change = get_change (context, G_OPTION_ARG_NONE,
                             entry->arg_data);

        *(gboolean *)entry->arg_data = !(entry->flags & G_OPTION_FLAG_REVERSE);
        break;
      }
    case G_OPTION_ARG_STRING:
      {
        gchar *data;

//        data = g_locale_to_utf8 (value, -1, NULL, NULL, error);
        data = g_strdup(value);

        if (!data)
          return FALSE;

        change = get_change (context, G_OPTION_ARG_STRING,
                             entry->arg_data);
        g_free (change->allocated.str);

        change->prev.str = *(gchar **)entry->arg_data;
        change->allocated.str = data;

        *(gchar **)entry->arg_data = data;
        break;
      }
    case G_OPTION_ARG_STRING_ARRAY:
      {
        gchar *data;

//        data = g_locale_to_utf8 (value, -1, NULL, NULL, error);
        data = g_strdup(value);

        if (!data)
          return FALSE;

        change = get_change (context, G_OPTION_ARG_STRING_ARRAY,
                             entry->arg_data);

        if (change->allocated.array.len == 0)
          {
            change->prev.array = *(gchar ***)entry->arg_data;
            change->allocated.array.data = g_new (gchar *, 2);
          }
        else
          change->allocated.array.data =
//            g_renew (gchar *, change->allocated.array.data,
//                     change->allocated.array.len + 2);
            realloc (change->allocated.array.data, sizeof(gchar *)*(change->allocated.array.len + 2));

        change->allocated.array.data[change->allocated.array.len] = data;
        change->allocated.array.data[change->allocated.array.len + 1] = NULL;

        change->allocated.array.len ++;

        *(gchar ***)entry->arg_data = change->allocated.array.data;

        break;
      }

    case G_OPTION_ARG_FILENAME:
      {
        gchar *data;

#ifdef G_OS_WIN32
//        data = g_locale_to_utf8 (value, -1, NULL, NULL, error);
        data = g_strdup(value);

        if (!data)
          return FALSE;
#else
        data = g_strdup (value);
#endif
        change = get_change (context, G_OPTION_ARG_FILENAME,
                             entry->arg_data);
        g_free (change->allocated.str);

        change->prev.str = *(gchar **)entry->arg_data;
        change->allocated.str = data;

        *(gchar **)entry->arg_data = data;
        break;
      }

    case G_OPTION_ARG_FILENAME_ARRAY:
      {
        gchar *data;

#ifdef G_OS_WIN32
//        data = g_locale_to_utf8 (value, -1, NULL, NULL, error);
        data = g_strdup(value);

        if (!data)
          return FALSE;
#else
        data = g_strdup (value);
#endif
        change = get_change (context, G_OPTION_ARG_STRING_ARRAY,
                             entry->arg_data);

        if (change->allocated.array.len == 0)
          {
            change->prev.array = *(gchar ***)entry->arg_data;
            change->allocated.array.data = g_new (gchar *, 2);
          }
        else
          change->allocated.array.data =
//            g_renew (gchar *, change->allocated.array.data,
//                     change->allocated.array.len + 2);
            realloc (change->allocated.array.data, sizeof(gchar *)*(change->allocated.array.len + 2));

        change->allocated.array.data[change->allocated.array.len] = data;
        change->allocated.array.data[change->allocated.array.len + 1] = NULL;

        change->allocated.array.len ++;

        *(gchar ***)entry->arg_data = change->allocated.array.data;

        break;
      }

    case G_OPTION_ARG_INT:
      {
        gint data;

        if (!parse_int (option_name, value,
                        &data,
                        error))
          return FALSE;

        change = get_change (context, G_OPTION_ARG_INT,
                             entry->arg_data);
        change->prev.integer = *(gint *)entry->arg_data;
        *(gint *)entry->arg_data = data;
        break;
      }
    case G_OPTION_ARG_CALLBACK:
      {
        gchar *data;
        gboolean retval;

        if (!value && entry->flags & G_OPTION_FLAG_OPTIONAL_ARG)
          data = NULL;
        else if (entry->flags & G_OPTION_FLAG_NO_ARG)
          data = NULL;
        else if (entry->flags & G_OPTION_FLAG_FILENAME)
          {
#ifdef G_OS_WIN32
//            data = g_locale_to_utf8 (value, -1, NULL, NULL, error);
            data = g_strdup(value);
#else
            data = g_strdup (value);
#endif
          }
        else
//          data = g_locale_to_utf8 (value, -1, NULL, NULL, error);
          data = g_strdup(value);

        if (!(entry->flags & (G_OPTION_FLAG_NO_ARG|G_OPTION_FLAG_OPTIONAL_ARG)) &&
            !data)
          return FALSE;

        retval = (* (GOptionArgFunc) entry->arg_data) (option_name, data, group->user_data, error);

        if (!retval && error != NULL && *error == NULL)
          g_set_error (error,
                       G_OPTION_ERROR, G_OPTION_ERROR_FAILED,
                       "Error parsing option %s", option_name);

        g_free (data);

        return retval;

        break;
      }
    case G_OPTION_ARG_DOUBLE:
      {
        gdouble data;

        if (!parse_double (option_name, value,
                        &data,
                        error))
          {
            return FALSE;
          }

        change = get_change (context, G_OPTION_ARG_DOUBLE,
                             entry->arg_data);
        change->prev.dbl = *(gdouble *)entry->arg_data;
        *(gdouble *)entry->arg_data = data;
        break;
      }
    case G_OPTION_ARG_INT64:
      {
        gint64 data;

        if (!parse_int64 (option_name, value,
                         &data,
                         error))
          {
            return FALSE;
          }

        change = get_change (context, G_OPTION_ARG_INT64,
                             entry->arg_data);
        change->prev.int64 = *(gint64 *)entry->arg_data;
        *(gint64 *)entry->arg_data = data;
        break;
      }
    default:
      g_assert_not_reached ();
    }

  return TRUE;
}

static gboolean
parse_short_option (GOptionContext *context,
                    GOptionGroup   *group,
                    gint            idx,
                    gint           *new_idx,
                    gchar           arg,
                    gint           *argc,
                    gchar        ***argv,
                    GError        **error,
                    gboolean       *parsed)
{
  gint j;

  for (j = 0; j < group->n_entries; j++)
    {
      if (arg == group->entries[j].short_name)
        {
          gchar *option_name;
          gchar *value = NULL;

          option_name = g_strdup_printf ("-%c", group->entries[j].short_name);

          if (NO_ARG (&group->entries[j]))
            value = NULL;
          else
            {
              if (*new_idx > idx)
                {
                  g_set_error (error,
                               G_OPTION_ERROR, G_OPTION_ERROR_FAILED,
                               "Error parsing option %s", option_name);
                  g_free (option_name);
                  return FALSE;
                }

              if (idx < *argc - 1)
                {
                  if (!OPTIONAL_ARG (&group->entries[j]))
                    {
                      value = (*argv)[idx + 1];
                      add_pending_null (context, &((*argv)[idx + 1]), NULL);
                      *new_idx = idx + 1;
                    }
                  else
                    {
                      if ((*argv)[idx + 1][0] == '-')
                        value = NULL;
                      else
                        {
                          value = (*argv)[idx + 1];
                          add_pending_null (context, &((*argv)[idx + 1]), NULL);
                          *new_idx = idx + 1;
                        }
                    }
                }
              else if (idx >= *argc - 1 && OPTIONAL_ARG (&group->entries[j]))
                value = NULL;
              else
                {
                  g_set_error (error,
                               G_OPTION_ERROR, G_OPTION_ERROR_BAD_VALUE,
                               "Missing argument for %s", option_name);
                  g_free (option_name);
                  return FALSE;
                }
            }

          if (!parse_arg (context, group, &group->entries[j],
                          value, option_name, error))
            {
              g_free (option_name);
              return FALSE;
            }

          g_free (option_name);
          *parsed = TRUE;
        }
    }

  return TRUE;
}

static gboolean
parse_long_option (GOptionContext *context,
                   GOptionGroup   *group,
                   gint           *idx,
                   gchar          *arg,
                   gboolean        aliased,
                   gint           *argc,
                   gchar        ***argv,
                   GError        **error,
                   gboolean       *parsed)
{
  gint j;

  for (j = 0; j < group->n_entries; j++)
    {
      if (*idx >= *argc)
        return TRUE;

      if (aliased && (group->entries[j].flags & G_OPTION_FLAG_NOALIAS))
        continue;

      if (NO_ARG (&group->entries[j]) &&
          strcmp (arg, group->entries[j].long_name) == 0)
        {
          gchar *option_name;
          gboolean retval;

          option_name = g_strconcat ("--", group->entries[j].long_name, NULL);
          retval = parse_arg (context, group, &group->entries[j],
                              NULL, option_name, error);
          g_free (option_name);

          add_pending_null (context, &((*argv)[*idx]), NULL);
          *parsed = TRUE;

          return retval;
        }
      else
        {
          gint len = strlen (group->entries[j].long_name);

          if (strncmp (arg, group->entries[j].long_name, len) == 0 &&
              (arg[len] == '=' || arg[len] == 0))
            {
              gchar *value = NULL;
              gchar *option_name;

              add_pending_null (context, &((*argv)[*idx]), NULL);
              option_name = g_strconcat ("--", group->entries[j].long_name, NULL);

              if (arg[len] == '=')
                value = arg + len + 1;
              else if (*idx < *argc - 1)
                {
                  if (!OPTIONAL_ARG (&group->entries[j]))
                    {
                      value = (*argv)[*idx + 1];
                      add_pending_null (context, &((*argv)[*idx + 1]), NULL);
                      (*idx)++;
                    }
                  else
                    {
                      if ((*argv)[*idx + 1][0] == '-')
                        {
                          gboolean retval;
                          retval = parse_arg (context, group, &group->entries[j],
                                              NULL, option_name, error);
                          *parsed = TRUE;
                          g_free (option_name);
                          return retval;
                        }
                      else
                        {
                          value = (*argv)[*idx + 1];
                          add_pending_null (context, &((*argv)[*idx + 1]), NULL);
                          (*idx)++;
                        }
                    }
                }
              else if (*idx >= *argc - 1 && OPTIONAL_ARG (&group->entries[j]))
                {
                    gboolean retval;
                    retval = parse_arg (context, group, &group->entries[j],
                                        NULL, option_name, error);
                    *parsed = TRUE;
                    g_free (option_name);
                    return retval;
                }
              else
                {
                  g_set_error (error,
                               G_OPTION_ERROR, G_OPTION_ERROR_BAD_VALUE,
                               "Missing argument for %s", option_name);
                  g_free (option_name);
                  return FALSE;
                }

              if (!parse_arg (context, group, &group->entries[j],
                              value, option_name, error))
                {
                  g_free (option_name);
                  return FALSE;
                }

              g_free (option_name);
              *parsed = TRUE;
            }
        }
    }

  return TRUE;
}

static gboolean
parse_remaining_arg (GOptionContext *context,
                     GOptionGroup   *group,
                     gint           *idx,
                     gint           *argc,
                     gchar        ***argv,
                     GError        **error,
                     gboolean       *parsed)
{
  gint j;

  for (j = 0; j < group->n_entries; j++)
    {
      if (*idx >= *argc)
        return TRUE;

      if (group->entries[j].long_name[0])
        continue;

      if(group->entries[j].arg != G_OPTION_ARG_CALLBACK
      && group->entries[j].arg != G_OPTION_ARG_STRING_ARRAY
      && group->entries[j].arg != G_OPTION_ARG_FILENAME_ARRAY)
        return(FALSE);

      add_pending_null (context, &((*argv)[*idx]), NULL);

      if (!parse_arg (context, group, &group->entries[j], (*argv)[*idx], "", error))
        return FALSE;

      *parsed = TRUE;
      return TRUE;
    }

  return TRUE;
}

static void
free_changes_list (GOptionContext *context,
                   gboolean        revert)
{
  GList *list;

  for (list = context->changes; list != NULL; list = list->next)
    {
      Change *change = list->data;

      if (revert)
        {
          switch (change->arg_type)
            {
            case G_OPTION_ARG_NONE:
              *(gboolean *)change->arg_data = change->prev.bool;
              break;
            case G_OPTION_ARG_INT:
              *(gint *)change->arg_data = change->prev.integer;
              break;
            case G_OPTION_ARG_STRING:
            case G_OPTION_ARG_FILENAME:
              g_free (change->allocated.str);
              *(gchar **)change->arg_data = change->prev.str;
              break;
            case G_OPTION_ARG_STRING_ARRAY:
            case G_OPTION_ARG_FILENAME_ARRAY:
              g_strfreev (change->allocated.array.data);
              *(gchar ***)change->arg_data = change->prev.array;
              break;
            case G_OPTION_ARG_DOUBLE:
              *(gdouble *)change->arg_data = change->prev.dbl;
              break;
            case G_OPTION_ARG_INT64:
              *(gint64 *)change->arg_data = change->prev.int64;
              break;
            default:
              g_assert_not_reached ();
            }
        }

      g_free (change);
    }

  g_list_free (context->changes);
  context->changes = NULL;
}

static void
free_pending_nulls (GOptionContext *context,
                    gboolean        perform_nulls)
{
  GList *list;

  for (list = context->pending_nulls; list != NULL; list = list->next)
    {
      PendingNull *n = list->data;

      if (perform_nulls)
        {
          if (n->value)
            {
              /* Copy back the short options */
              *(n->ptr)[0] = '-';
              strcpy (*n->ptr + 1, n->value);
            }
          else
            *n->ptr = NULL;
        }

      g_free (n->value);
      g_free (n);
    }

  g_list_free (context->pending_nulls);
  context->pending_nulls = NULL;
}

gboolean
g_option_context_parse (GOptionContext   *context,
                        gint             *argc,
                        gchar          ***argv,
                        GError          **error)
{
  gint i, j, k;
  GList *list;

  /* Set program name */
  set_prgname_from_arg0((*argv)[0]);

  /* Call pre-parse hooks */
  list = context->groups;
  while (list)
    {
      GOptionGroup *group = list->data;

      if (group->pre_parse_func)
        {
          if (!(* group->pre_parse_func) (context, group,
                                          group->user_data, error))
            goto fail;
        }

      list = list->next;
    }

  if (context->main_group && context->main_group->pre_parse_func)
    {
      if (!(* context->main_group->pre_parse_func) (context, context->main_group,
                                                    context->main_group->user_data, error))
        goto fail;
    }

  if (argc && argv)
    {
      gboolean stop_parsing = FALSE;
      gboolean has_unknown = FALSE;
      gint separator_pos = 0;

      for (i = 1; i < *argc; i++)
        {
          gchar *arg, *dash;
          gboolean parsed = FALSE;

          if ((*argv)[i][0] == '-' && (*argv)[i][1] != '\0' && !stop_parsing)
            {
              if ((*argv)[i][1] == '-')
                {
                  /* -- option */

                  arg = (*argv)[i] + 2;

                  /* '--' terminates list of arguments */
                  if (*arg == 0)
                    {
                      separator_pos = i;
                      stop_parsing = TRUE;
                      continue;
                    }

                  /* Handle help options */
                  if (context->help_enabled)
                    {
                      if (strcmp (arg, "help") == 0)
                        print_help (context, TRUE, NULL);
                      else if (strcmp (arg, "help-all") == 0)
                        print_help (context, FALSE, NULL);
                      else if (strncmp (arg, "help-", 5) == 0)
                        {
                          list = context->groups;

                          while (list)
                            {
                              GOptionGroup *group = list->data;

                              if (strcmp (arg + 5, group->name) == 0)
                                print_help (context, FALSE, group);

                              list = list->next;
                            }
                        }
                    }

                  if (context->main_group &&
                      !parse_long_option (context, context->main_group, &i, arg,
                                          FALSE, argc, argv, error, &parsed))
                    goto fail;

                  if (parsed)
                    continue;

                  /* Try the groups */
                  list = context->groups;
                  while (list)
                    {
                      GOptionGroup *group = list->data;

                      if (!parse_long_option (context, group, &i, arg,
                                              FALSE, argc, argv, error, &parsed))
                        goto fail;

                      if (parsed)
                        break;

                      list = list->next;
                    }

                  if (parsed)
                    continue;

                  /* Now look for --<group>-<option> */
                  dash = strchr (arg, '-');
                  if (dash)
                    {
                      /* Try the groups */
                      list = context->groups;
                      while (list)
                        {
                          GOptionGroup *group = list->data;

                          if (strncmp (group->name, arg, dash - arg) == 0)
                            {
                              if (!parse_long_option (context, group, &i, dash + 1,
                                                      TRUE, argc, argv, error, &parsed))
                                goto fail;

                              if (parsed)
                                break;
                            }

                          list = list->next;
                        }
                    }

                  if (context->ignore_unknown)
                    continue;
                }
              else
                { /* short option */
                  gint new_i = i, arg_length;
                  gboolean *nulled_out = NULL;
                  gboolean has_h_entry = context_has_h_entry (context);
                  arg = (*argv)[i] + 1;
                  arg_length = strlen (arg);
                  nulled_out = g_newa (gboolean, arg_length);
                  memset (nulled_out, 0, arg_length * sizeof (gboolean));
                  for (j = 0; j < arg_length; j++)
                    {
                      if (context->help_enabled && (arg[j] == '?' ||
                        (arg[j] == 'h' && !has_h_entry)))
                        print_help (context, TRUE, NULL);
                      parsed = FALSE;
                      if (context->main_group &&
                          !parse_short_option (context, context->main_group,
                                               i, &new_i, arg[j],
                                               argc, argv, error, &parsed))
                        goto fail;
                      if (!parsed)
                        {
                          /* Try the groups */
                          list = context->groups;
                          while (list)
                            {
                              GOptionGroup *group = list->data;
                              if (!parse_short_option (context, group, i, &new_i, arg[j],
                                                       argc, argv, error, &parsed))
                                goto fail;
                              if (parsed)
                                break;
                              list = list->next;
                            }
                        }

                      if (context->ignore_unknown && parsed)
                        nulled_out[j] = TRUE;
                      else if (context->ignore_unknown)
                        continue;
                      else if (!parsed)
                        break;
                      /* !context->ignore_unknown && parsed */
                    }
                  if (context->ignore_unknown)
                    {
                      gchar *new_arg = NULL;
                      gint arg_index = 0;
                      for (j = 0; j < arg_length; j++)
                        {
                          if (!nulled_out[j])
                            {
                              if (!new_arg)
                                new_arg = g_malloc (arg_length + 1);
                              new_arg[arg_index++] = arg[j];
                            }
                        }
                      if (new_arg)
                        new_arg[arg_index] = '\0';
                      add_pending_null (context, &((*argv)[i]), new_arg);
                    }
                  else if (parsed)
                    {
                      add_pending_null (context, &((*argv)[i]), NULL);
                      i = new_i;
                    }
                }

              if (!parsed)
                has_unknown = TRUE;

              if (!parsed && !context->ignore_unknown)
                {
                  g_set_error (error,
                               G_OPTION_ERROR, G_OPTION_ERROR_UNKNOWN_OPTION,
                                   "Unknown option %s", (*argv)[i]);
                  goto fail;
                }
            }
          else
            {
              /* Collect remaining args */
              if (context->main_group &&
                  !parse_remaining_arg (context, context->main_group, &i,
                                        argc, argv, error, &parsed))
                goto fail;

              if (!parsed && (has_unknown || (*argv)[i][0] == '-'))
                separator_pos = 0;
            }
        }

      if (separator_pos > 0)
        add_pending_null (context, &((*argv)[separator_pos]), NULL);

    }

  /* Call post-parse hooks */
  list = context->groups;
  while (list)
    {
      GOptionGroup *group = list->data;

      if (group->post_parse_func)
        {
          if (!(* group->post_parse_func) (context, group,
                                           group->user_data, error))
            goto fail;
        }

      list = list->next;
    }

  if (context->main_group && context->main_group->post_parse_func)
    {
      if (!(* context->main_group->post_parse_func) (context, context->main_group,
                                                     context->main_group->user_data, error))
        goto fail;
    }

  if (argc && argv)
    {
      free_pending_nulls (context, TRUE);

      for (i = 1; i < *argc; i++)
        {
          for (k = i; k < *argc; k++)
            if ((*argv)[k] != NULL)
              break;

          if (k > i)
            {
              k -= i;
              for (j = i + k; j < *argc; j++)
                {
                  (*argv)[j-k] = (*argv)[j];
                  (*argv)[j] = NULL;
                }
              *argc -= k;
            }
        }
    }

  return TRUE;

 fail:

  /* Call error hooks */
  list = context->groups;
  while (list)
    {
      GOptionGroup *group = list->data;

      if (group->error_func)
        (* group->error_func) (context, group,
                               group->user_data, error);

      list = list->next;
    }

  if (context->main_group && context->main_group->error_func)
    (* context->main_group->error_func) (context, context->main_group,
                                         context->main_group->user_data, error);

  free_changes_list (context, TRUE);
  free_pending_nulls (context, FALSE);

  return FALSE;
}

GOptionGroup *
g_option_group_new (const gchar    *name,
                    const gchar    *description,
                    const gchar    *help_description,
                    gpointer        user_data,
                    GDestroyNotify  destroy)

{
  GOptionGroup *group;

  group = g_new0 (GOptionGroup, 1);
  group->name = g_strdup (name);
  group->description = g_strdup (description);
  group->help_description = g_strdup (help_description);
  group->user_data = user_data;
  group->destroy_notify = destroy;

  return group;
}


void
g_option_group_free (GOptionGroup *group)
{
  if(!group)
    return;

  g_free (group->name);
  g_free (group->description);
  g_free (group->help_description);

  g_free (group->entries);

  if (group->destroy_notify)
    (* group->destroy_notify) (group->user_data);

  g_free (group);
}

void
g_option_group_add_entries (GOptionGroup       *group,
                            const GOptionEntry *entries)
{
  gint i, n_entries;

  if(!entries)
    return;

  for (n_entries = 0; entries[n_entries].long_name != NULL; n_entries++) ;

//  group->entries = g_renew (GOptionEntry, group->entries, group->n_entries + n_entries);
  group->entries = realloc (group->entries, sizeof(GOptionEntry)*(group->n_entries + n_entries));

  memcpy (group->entries + group->n_entries, entries, sizeof (GOptionEntry) * n_entries);

  for (i = group->n_entries; i < group->n_entries + n_entries; i++)
    {
      gchar c = group->entries[i].short_name;

      if (c == '-' || (c != 0 && !g_ascii_isprint (c)))
        {
          fprintf(stderr, "WARNING:" G_STRLOC ": ignoring invalid short option '%c' (%d) in entry %s:%s",
              c, c, group->name, group->entries[i].long_name);
          group->entries[i].short_name = '\0';
        }

      if (group->entries[i].arg != G_OPTION_ARG_NONE &&
          (group->entries[i].flags & G_OPTION_FLAG_REVERSE) != 0)
        {
          fprintf(stderr, "WARNING:" G_STRLOC ": ignoring reverse flag on option of arg-type %d in entry %s:%s",
              group->entries[i].arg, group->name, group->entries[i].long_name);

          group->entries[i].flags &= ~G_OPTION_FLAG_REVERSE;
        }

      if (group->entries[i].arg != G_OPTION_ARG_CALLBACK &&
          (group->entries[i].flags & (G_OPTION_FLAG_NO_ARG|G_OPTION_FLAG_OPTIONAL_ARG|G_OPTION_FLAG_FILENAME)) != 0)
        {
          fprintf(stderr, "WARNING:" G_STRLOC ": ignoring no-arg, optional-arg or filename flags (%d) on option of arg-type %d in entry %s:%s",
              group->entries[i].flags, group->entries[i].arg, group->name, group->entries[i].long_name);

          group->entries[i].flags &= ~(G_OPTION_FLAG_NO_ARG|G_OPTION_FLAG_OPTIONAL_ARG|G_OPTION_FLAG_FILENAME);
        }
    }

  group->n_entries += n_entries;
}

// currently unused
//void
//g_option_group_set_parse_hooks (GOptionGroup     *group,
//                                GOptionParseFunc  pre_parse_func,
//                                GOptionParseFunc  post_parse_func)
//{
//  g_return_if_fail (group != NULL);
//
//  group->pre_parse_func = pre_parse_func;
//  group->post_parse_func = post_parse_func;
//}

// currently unused
//void
//g_option_group_set_error_hook (GOptionGroup     *group,
//                               GOptionErrorFunc  error_func)
//{
//  g_return_if_fail (group != NULL);
//
//  group->error_func = error_func;
//}

// currently unused
//void
//g_option_context_set_summary (GOptionContext *context,
//                              const gchar    *summary)
//{
//  g_return_if_fail (context != NULL);
//
//  g_free (context->summary);
//  context->summary = g_strdup (summary);
//}

// currently unused
//const gchar *
//g_option_context_get_summary (GOptionContext *context)
//{
//  g_return_val_if_fail (context != NULL, NULL);
//
//  return context->summary;
//}

void
g_option_context_set_description (GOptionContext *context,
                                  const gchar    *description)
{
  if(!context)
    return;

  g_free (context->description);
  context->description = g_strdup (description);
}

const gchar *
g_option_context_get_description (GOptionContext *context)
{
  if(!context)
    return(NULL);

  return context->description;
}
//
// END goption.c
//

//
// BEGIN gutils.c
//
//#include "gutils.h"

//#include "gmem.h"
//#include "gstrfuncs.h"

//#include <string.h>

gchar*
g_path_get_dirname (const gchar    *file_name)
{
  register gchar *base;
  register gsize len;

  if(!file_name)
  {
    return(NULL);
  }

  base = strrchr (file_name, G_DIR_SEPARATOR);
#ifdef G_OS_WIN32
  {
    gchar *q = strrchr (file_name, '/');
    if (base == NULL || (q != NULL && q > base))
        base = q;
  }
#endif
  if (!base)
  {
#ifdef G_OS_WIN32
    if (g_ascii_isalpha (file_name[0]) && file_name[1] == ':')
    {
      gchar drive_colon_dot[4];

      drive_colon_dot[0] = file_name[0];
      drive_colon_dot[1] = ':';
      drive_colon_dot[2] = '.';
      drive_colon_dot[3] = '\0';

      return g_strdup (drive_colon_dot);
    }
#endif
    return g_strdup (".");
  }

  while (base > file_name && G_IS_DIR_SEPARATOR (*base))
    base--;

#ifdef G_OS_WIN32
  if (base == file_name + 1 && g_ascii_isalpha (file_name[0]) && file_name[1] == ':')
    base++;
  else if (G_IS_DIR_SEPARATOR (file_name[0]) &&
           G_IS_DIR_SEPARATOR (file_name[1]) &&
           file_name[2] &&
           !G_IS_DIR_SEPARATOR (file_name[2]) &&
           base >= file_name + 2)
  {
    const gchar *p = file_name + 2;
    while (*p && !G_IS_DIR_SEPARATOR (*p))
      p++;
    if (p == base + 1)
    {
      len = (guint) strlen (file_name) + 1;
      base = g_new (gchar, len + 1);
      strcpy (base, file_name);
      base[len-1] = G_DIR_SEPARATOR;
      base[len] = 0;
      return base;
    }
    if (G_IS_DIR_SEPARATOR (*p))
    {
      p++;
      while (*p && !G_IS_DIR_SEPARATOR (*p))
        p++;
      if (p == base + 1)
        base++;
    }
  }
#endif

  len = (guint) 1 + base - file_name;

  base = g_new (gchar, len + 1);
  memmove (base, file_name, len);
  base[len] = 0;

  return base;
}
//
// END gutils.c
//

//
// BEGIN gfileutils.c
//
//#include "gfileutils.h"

//#ifdef G_OS_WIN32
//#include <windows.h>
//#else
//#include <sys/stat.h>
//#endif

//
// g_file_test: strongly crippled.
//
gboolean
g_file_test (const gchar *filename,
             GFileTest    test)
{
#ifdef G_OS_WIN32
#if defined(UNICODE) || defined (_UNICODE)
  #error This application must not be compiled in UNICODE mode.
#endif
/* stuff missing in std vc6 api */
#  ifndef INVALID_FILE_ATTRIBUTES
#    define INVALID_FILE_ATTRIBUTES -1
#  endif
#  ifndef FILE_ATTRIBUTE_DEVICE
#    define FILE_ATTRIBUTE_DEVICE 64
#  endif
  int attributes = GetFileAttributes (filename);

  if (attributes == INVALID_FILE_ATTRIBUTES)
    return FALSE;

  if (test & G_FILE_TEST_IS_REGULAR)
  {
    if ((attributes & (FILE_ATTRIBUTE_DIRECTORY | FILE_ATTRIBUTE_DEVICE)) == 0)
      return TRUE;
  }

  if (test & G_FILE_TEST_IS_DIR)
  {
    if ((attributes & FILE_ATTRIBUTE_DIRECTORY) != 0)
      return TRUE;
  }

  return FALSE;
#else
  if (test & (G_FILE_TEST_IS_REGULAR | G_FILE_TEST_IS_DIR))
  {
    struct stat s;

    if (stat (filename, &s) == 0)
    {
      if ((test & G_FILE_TEST_IS_REGULAR) && S_ISREG (s.st_mode))
        return TRUE;

      if ((test & G_FILE_TEST_IS_DIR) && S_ISDIR (s.st_mode))
        return TRUE;

    }
  }

  return FALSE;
#endif
}
//
// END gfileutils.c
//

//
// BEGIN glist.c
//
#define _g_list_alloc()         g_malloc(sizeof(GList))
#define _g_list_alloc0()        g_malloc0(sizeof(GList))
#define _g_list_free1(list)     g_free(list)

GList*
g_list_alloc (void)
{
  return _g_list_alloc0 ();
}

void
g_list_free (GList *list)
{
  GList * next;

  while(list)
  {
    next = list->next;
    g_free(list);
    list = next;
  }
}

void
g_list_free_1 (GList *list)
{
  _g_list_free1 (list);
}

void
g_list_free_full (GList          *list,
		  GDestroyNotify  free_func)
{
  g_list_foreach (list, (GFunc) free_func, NULL);
  g_list_free (list);
}

GList*
g_list_append (GList	*list,
	       gpointer	 data)
{
  GList *new_list;
  GList *last;

  new_list = _g_list_alloc ();
  new_list->data = data;
  new_list->next = NULL;

  if (list)
    {
      last = g_list_last (list);
      /* g_assert (last != NULL); */
      last->next = new_list;
      new_list->prev = last;

      return list;
    }
  else
    {
      new_list->prev = NULL;
      return new_list;
    }
}

GList*
g_list_prepend (GList	 *list,
		gpointer  data)
{
  GList *new_list;

  new_list = _g_list_alloc ();
  new_list->data = data;
  new_list->next = list;

  if (list)
    {
      new_list->prev = list->prev;
      if (list->prev)
	list->prev->next = new_list;
      list->prev = new_list;
    }
  else
    new_list->prev = NULL;

  return new_list;
}

GList*
g_list_last (GList *list)
{
  if (list)
    {
      while (list->next)
	list = list->next;
    }

  return list;
}

void
g_list_foreach (GList	 *list,
		GFunc	  func,
		gpointer  user_data)
{
  while (list)
    {
      GList *next = list->next;
      (*func) (list->data, user_data);
      list = next;
    }
}
//
// END glist.c
//

//
// BEGIN gslist.c
//
//#include "gslist.h"

//#include "gmem.h"

#define _g_slist_alloc0()       g_malloc0(sizeof(GSList)) //g_slice_new0 (GSList)
#define _g_slist_alloc()        g_malloc(sizeof(GSList))  //g_slice_new (GSList)
#define _g_slist_free1(slist)   g_free(slist)             //g_slice_free (GSList, slist)

GSList*
g_slist_alloc (void)
{
  return _g_slist_alloc0 ();
}

void
g_slist_free (GSList *list)
{
  while(list)
  {
    GSList *next = list->next;
    _g_slist_free1(list);
    list = next;
  }
}

void
g_slist_free_1 (GSList *list)
{
  _g_slist_free1 (list);
}

void
g_slist_free_full (GSList *       list,
                   GDestroyNotify free_func)
{
  g_slist_foreach (list, (GFunc) free_func, NULL);
  g_slist_free (list);
}

GSList*
g_slist_append (GSList   *list,
                gpointer  data)
{
  GSList *new_list;
  GSList *last;

  new_list = _g_slist_alloc ();
  new_list->data = data;
  new_list->next = NULL;

  if (list)
  {
    last = g_slist_last (list);
    /* g_assert (last != NULL); */
    last->next = new_list;

    return list;
  }
  else
    return new_list;
}

GSList*
g_slist_prepend (GSList   *list,
                 gpointer  data)
{
  GSList *new_list;

  new_list = _g_slist_alloc ();
  new_list->data = data;
  new_list->next = list;

  return new_list;
}

GSList *
g_slist_concat (GSList *list1, GSList *list2)
{
  if (list2)
  {
    if (list1)
      g_slist_last (list1)->next = list2;
    else
      list1 = list2;
  }

  return list1;
}

GSList*
g_slist_remove (GSList        *list,
                gconstpointer  data)
{
  GSList *tmp, *prev = NULL;

  tmp = list;
  while (tmp)
  {
    if (tmp->data == data)
    {
      if (prev)
        prev->next = tmp->next;
      else
        list = tmp->next;

      g_slist_free_1 (tmp);
      break;
    }
    prev = tmp;
    tmp = prev->next;
  }

  return list;
}

GSList*
g_slist_copy (GSList *list)
{
  GSList *new_list = NULL;

  if (list)
  {
    GSList *last;

    new_list = _g_slist_alloc ();
    new_list->data = list->data;
    last = new_list;
    list = list->next;
    while (list)
    {
      last->next = _g_slist_alloc ();
      last = last->next;
      last->data = list->data;
      list = list->next;
    }
    last->next = NULL;
  }

  return new_list;
}

GSList*
g_slist_reverse (GSList *list)
{
  GSList *prev = NULL;

  while (list)
  {
    GSList *next = list->next;

    list->next = prev;

    prev = list;
    list = next;
  }

  return prev;
}

GSList*
g_slist_find (GSList        *list,
              gconstpointer  data)
{
  while (list)
  {
    if (list->data == data)
      break;
    list = list->next;
  }

  return list;
}

GSList*
g_slist_last (GSList *list)
{
  if (list)
  {
    while (list->next)
      list = list->next;
  }

  return list;
}

guint
g_slist_length (GSList *list)
{
  guint length;

  length = 0;
  while (list)
    {
      length++;
      list = list->next;
    }

  return length;
}

void
g_slist_foreach (GSList   *list,
                 GFunc     func,
                 gpointer  user_data)
{
  while (list)
  {
    GSList *next = list->next;
    (*func) (list->data, user_data);
    list = next;
  }
}

static GSList *
g_slist_sort_merge (GSList   *l1,
                    GSList   *l2,
                    GFunc     compare_func,
                    gpointer  user_data)
{
  GSList list, *l;
  gint cmp;

  l=&list;

  while (l1 && l2)
  {
    cmp = ((GCompareDataFunc) compare_func) (l1->data, l2->data, user_data);

    if (cmp <= 0)
    {
      l=l->next=l1;
      l1=l1->next;
    }
    else
    {
      l=l->next=l2;
      l2=l2->next;
    }
  }
  l->next= l1 ? l1 : l2;

  return list.next;
}

static GSList *
g_slist_sort_real (GSList   *list,
                   GFunc     compare_func,
                   gpointer  user_data)
{
  GSList *l1, *l2;

  if (!list)
    return NULL;
  if (!list->next)
    return list;

  l1 = list;
  l2 = list->next;

  while ((l2 = l2->next) != NULL)
  {
    if ((l2 = l2->next) == NULL)
      break;
    l1=l1->next;
  }
  l2 = l1->next;
  l1->next = NULL;

  return g_slist_sort_merge (g_slist_sort_real (list, compare_func, user_data),
                             g_slist_sort_real (l2, compare_func, user_data),
                             compare_func,
                             user_data);
}

GSList *
g_slist_sort (GSList       *list,
              GCompareFunc  compare_func)
{
  return g_slist_sort_real (list, (GFunc) compare_func, NULL);
}
//
// END gslist.c
//

//
// BEGIN ghash.c
//
//#include "ghash.h"

//#include "gmem.h"
//#include "gstrfuncs.h"

//#include <string.h>

#define HASH_TABLE_MIN_SHIFT 3  /* 1 << 3 == 8 buckets */

#define UNUSED_HASH_VALUE 0
#define TOMBSTONE_HASH_VALUE 1
#define HASH_IS_UNUSED(h_) ((h_) == UNUSED_HASH_VALUE)
#define HASH_IS_TOMBSTONE(h_) ((h_) == TOMBSTONE_HASH_VALUE)
#define HASH_IS_REAL(h_) ((h_) >= 2)

struct _GHashTable
{
  gint             size;
  gint             mod;
  guint            mask;
  gint             nnodes;
  gint             noccupied;  /* nnodes + tombstones */

  gpointer        *keys;
  guint           *hashes;
  gpointer        *values;

  GHashFunc        hash_func;
  GEqualFunc       key_equal_func;
  gint             ref_count;
  GDestroyNotify   key_destroy_func;
  GDestroyNotify   value_destroy_func;
};

static const gint prime_mod [] =
{
  1,          /* For 1 << 0 */
  2,
  3,
  7,
  13,
  31,
  61,
  127,
  251,
  509,
  1021,
  2039,
  4093,
  8191,
  16381,
  32749,
  65521,      /* For 1 << 16 */
  131071,
  262139,
  524287,
  1048573,
  2097143,
  4194301,
  8388593,
  16777213,
  33554393,
  67108859,
  134217689,
  268435399,
  536870909,
  1073741789,
  2147483647  /* For 1 << 31 */
};

static void
g_hash_table_set_shift (GHashTable *hash_table, gint shift)
{
  gint i;
  guint mask = 0;

  hash_table->size = 1 << shift;
  hash_table->mod  = prime_mod [shift];

  for (i = 0; i < shift; i++)
  {
    mask <<= 1;
    mask |= 1;
  }

  hash_table->mask = mask;
}

static gint
g_hash_table_find_closest_shift (gint n)
{
  gint i;

  for (i = 0; n; i++)
    n >>= 1;

  return i;
}

static void
g_hash_table_set_shift_from_size (GHashTable *hash_table, gint size)
{
  gint shift;

  shift = g_hash_table_find_closest_shift (size);
  shift = MAX (shift, HASH_TABLE_MIN_SHIFT);

  g_hash_table_set_shift (hash_table, shift);
}

static guint
g_hash_table_lookup_node (GHashTable    *hash_table,
                          gconstpointer  key,
                          guint         *hash_return)
{
  guint node_index;
  guint node_hash;
  guint hash_value;
  guint first_tombstone = 0;
  gboolean have_tombstone = FALSE;
  guint step = 0;

  hash_value = hash_table->hash_func (key);
  if (!HASH_IS_REAL (hash_value))
    hash_value = 2;

  *hash_return = hash_value;

  node_index = hash_value % hash_table->mod;
  node_hash = hash_table->hashes[node_index];

  while (!HASH_IS_UNUSED (node_hash))
  {
    if (node_hash == hash_value)
    {
      gpointer node_key = hash_table->keys[node_index];

      if (hash_table->key_equal_func)
      {
        if (hash_table->key_equal_func (node_key, key))
          return node_index;
      }
      else if (node_key == key)
      {
        return node_index;
      }
    }
    else if (HASH_IS_TOMBSTONE (node_hash) && !have_tombstone)
    {
      first_tombstone = node_index;
      have_tombstone = TRUE;
    }

    step++;
    node_index += step;
    node_index &= hash_table->mask;
    node_hash = hash_table->hashes[node_index];
  }

  if (have_tombstone)
    return first_tombstone;

  return node_index;
}

//currently unused..
//static void
//g_hash_table_remove_node (GHashTable   *hash_table,
//                          int           i,
//                          gboolean      notify)
//{
//  gpointer key;
//  gpointer value;
//
//  key = hash_table->keys[i];
//  value = hash_table->values[i];
//
//  /* Erect tombstone */
//  hash_table->hashes[i] = TOMBSTONE_HASH_VALUE;
//
//  /* Be GC friendly */
//  hash_table->keys[i] = NULL;
//  hash_table->values[i] = NULL;
//
//  hash_table->nnodes--;
//
//  if (notify && hash_table->key_destroy_func)
//    hash_table->key_destroy_func (key);
//
//  if (notify && hash_table->value_destroy_func)
//    hash_table->value_destroy_func (value);
//
//}

static void
g_hash_table_remove_all_nodes (GHashTable *hash_table,
                               gboolean    notify)
{
  int i;
  gpointer key;
  gpointer value;

  hash_table->nnodes = 0;
  hash_table->noccupied = 0;

  if (!notify ||
      (hash_table->key_destroy_func == NULL &&
       hash_table->value_destroy_func == NULL))
  {
    memset (hash_table->hashes, 0, hash_table->size * sizeof (guint));
    memset (hash_table->keys, 0, hash_table->size * sizeof (gpointer));
    memset (hash_table->values, 0, hash_table->size * sizeof (gpointer));

    return;
  }

  for (i = 0; i < hash_table->size; i++)
  {
    if (HASH_IS_REAL (hash_table->hashes[i]))
    {
      key = hash_table->keys[i];
      value = hash_table->values[i];

      hash_table->hashes[i] = UNUSED_HASH_VALUE;
      hash_table->keys[i] = NULL;
      hash_table->values[i] = NULL;

      if (hash_table->key_destroy_func != NULL)
        hash_table->key_destroy_func (key);

      if (hash_table->value_destroy_func != NULL)
        hash_table->value_destroy_func (value);
    }
    else if (HASH_IS_TOMBSTONE (hash_table->hashes[i]))
    {
      hash_table->hashes[i] = UNUSED_HASH_VALUE;
    }
  }
}

static void
g_hash_table_resize (GHashTable *hash_table)
{
  gpointer *new_keys;
  gpointer *new_values;
  guint *new_hashes;
  gint old_size;
  gint i;

  old_size = hash_table->size;
  g_hash_table_set_shift_from_size (hash_table, hash_table->nnodes * 2);

  new_keys = g_new0 (gpointer, hash_table->size);
  if (hash_table->keys == hash_table->values)
    new_values = new_keys;
  else
    new_values = g_new0 (gpointer, hash_table->size);
  new_hashes = g_new0 (guint, hash_table->size);

  for (i = 0; i < old_size; i++)
  {
    guint node_hash = hash_table->hashes[i];
    guint hash_val;
    guint step = 0;

    if (!HASH_IS_REAL (node_hash))
      continue;

    hash_val = node_hash % hash_table->mod;

    while (!HASH_IS_UNUSED (new_hashes[hash_val]))
    {
      step++;
      hash_val += step;
      hash_val &= hash_table->mask;
    }

    new_hashes[hash_val] = hash_table->hashes[i];
    new_keys[hash_val] = hash_table->keys[i];
    new_values[hash_val] = hash_table->values[i];
  }

  if (hash_table->keys != hash_table->values)
    g_free (hash_table->values);

  g_free (hash_table->keys);
  g_free (hash_table->hashes);

  hash_table->keys = new_keys;
  hash_table->values = new_values;
  hash_table->hashes = new_hashes;

  hash_table->noccupied = hash_table->nnodes;
}

static void
g_hash_table_maybe_resize (GHashTable *hash_table)
{
  gint noccupied = hash_table->noccupied;
  gint size = hash_table->size;

  if ((size > hash_table->nnodes * 4 && size > 1 << HASH_TABLE_MIN_SHIFT) ||
      (size <= noccupied + (noccupied / 16)))
    g_hash_table_resize (hash_table);
}

GHashTable*
g_hash_table_new (GHashFunc    hash_func,
                  GEqualFunc   key_equal_func)
{
  return g_hash_table_new_full (hash_func, key_equal_func, NULL, NULL);
}

GHashTable*
g_hash_table_new_full (GHashFunc       hash_func,
                       GEqualFunc      key_equal_func,
                       GDestroyNotify  key_destroy_func,
                       GDestroyNotify  value_destroy_func)
{
  GHashTable *hash_table;

  hash_table = g_malloc(sizeof(GHashTable));  //g_slice_new (GHashTable);
  g_hash_table_set_shift (hash_table, HASH_TABLE_MIN_SHIFT);
  hash_table->nnodes             = 0;
  hash_table->noccupied          = 0;
  hash_table->hash_func          = hash_func ? hash_func : g_direct_hash;
  hash_table->key_equal_func     = key_equal_func;
  hash_table->ref_count          = 1;
  hash_table->key_destroy_func   = key_destroy_func;
  hash_table->value_destroy_func = value_destroy_func;
  hash_table->keys               = g_new0 (gpointer, hash_table->size);
  hash_table->values             = hash_table->keys;
  hash_table->hashes             = g_new0 (guint, hash_table->size);

  return hash_table;
}

static void
g_hash_table_insert_node (GHashTable *hash_table,
                          guint       node_index,
                          guint       key_hash,
                          gpointer    key,
                          gpointer    value,
                          gboolean    keep_new_key,
                          gboolean    reusing_key)
{
  guint old_hash;
  gpointer old_key;
  gpointer old_value;

  if (hash_table->keys == hash_table->values && key != value)
    hash_table->values = g_memdup (hash_table->keys, sizeof (gpointer) * hash_table->size);

  old_hash = hash_table->hashes[node_index];
  old_key = hash_table->keys[node_index];
  old_value = hash_table->values[node_index];

  if (HASH_IS_REAL (old_hash))
  {
    if (keep_new_key)
      hash_table->keys[node_index] = key;
    hash_table->values[node_index] = value;
  }
  else
  {
    hash_table->keys[node_index] = key;
    hash_table->values[node_index] = value;
    hash_table->hashes[node_index] = key_hash;

    hash_table->nnodes++;

    if (HASH_IS_UNUSED (old_hash))
    {
      /* We replaced an empty node, and not a tombstone */
      hash_table->noccupied++;
      g_hash_table_maybe_resize (hash_table);
    }
  }

  if (HASH_IS_REAL (old_hash))
  {
    if (hash_table->key_destroy_func && !reusing_key)
      hash_table->key_destroy_func (keep_new_key ? old_key : key);
    if (hash_table->value_destroy_func)
      hash_table->value_destroy_func (old_value);
  }
}

void
g_hash_table_unref (GHashTable *hash_table)
{
  if(hash_table)
  {
    if(!--hash_table->ref_count)
    {
      g_hash_table_remove_all_nodes (hash_table, TRUE);
      if (hash_table->keys != hash_table->values)
        g_free (hash_table->values);
      g_free (hash_table->keys);
      g_free (hash_table->hashes);
      g_free(hash_table);
    }
  }
}

void
g_hash_table_destroy (GHashTable *hash_table)
{
  if(hash_table)
  {
    g_hash_table_remove_all (hash_table);
    g_hash_table_unref (hash_table);
  }
}

gpointer
g_hash_table_lookup (GHashTable   *hash_table,
                     gconstpointer key)
{
  guint node_index;
  guint node_hash;

//  g_return_val_if_fail (hash_table != NULL, NULL);

  node_index = g_hash_table_lookup_node (hash_table, key, &node_hash);

  return HASH_IS_REAL (hash_table->hashes[node_index])
    ? hash_table->values[node_index]
    : NULL;
}

static void
g_hash_table_insert_internal (GHashTable *hash_table,
                              gpointer    key,
                              gpointer    value,
                              gboolean    keep_new_key)
{
  guint key_hash;
  guint node_index;

  if(hash_table)
  {
    node_index = g_hash_table_lookup_node (hash_table, key, &key_hash);
    g_hash_table_insert_node (hash_table, node_index, key_hash, key, value, keep_new_key, FALSE);
  }
}

void
g_hash_table_insert (GHashTable *hash_table,
                     gpointer    key,
                     gpointer    value)
{
  g_hash_table_insert_internal (hash_table, key, value, FALSE);
}

//currently unused..
//static gboolean
//g_hash_table_remove_internal (GHashTable    *hash_table,
//                              gconstpointer  key,
//                              gboolean       notify)
//{
//  guint node_index;
//  guint node_hash;
//
////  g_return_val_if_fail (hash_table != NULL, FALSE);
//
//  node_index = g_hash_table_lookup_node (hash_table, key, &node_hash);
//
//  if (!HASH_IS_REAL (hash_table->hashes[node_index]))
//    return FALSE;
//
//  g_hash_table_remove_node (hash_table, node_index, notify);
//  g_hash_table_maybe_resize (hash_table);
//
//  return TRUE;
//}

//currently unused..
//gboolean
//g_hash_table_remove (GHashTable    *hash_table,
//                     gconstpointer  key)
//{
//  return g_hash_table_remove_internal (hash_table, key, TRUE);
//}

void
g_hash_table_remove_all (GHashTable *hash_table)
{
  if(!hash_table)
  {
    return;
  }
  g_hash_table_remove_all_nodes (hash_table, TRUE);
  g_hash_table_maybe_resize (hash_table);
}

void
g_hash_table_foreach (GHashTable *hash_table,
                      GHFunc      func,
                      gpointer    user_data)
{
  gint i;

  if(!hash_table || !func)
  {
    return;
  }
  for (i = 0; i < hash_table->size; i++)
  {
    guint node_hash = hash_table->hashes[i];
    gpointer node_key = hash_table->keys[i];
    gpointer node_value = hash_table->values[i];

    if (HASH_IS_REAL (node_hash))
      (* func) (node_key, node_value, user_data);
  }
}

guint
g_direct_hash (gconstpointer v)
{
  return GPOINTER_TO_UINT (v);
}
//
// END ghash.c
//

//
// BEGIN gstring.c
//
//#include "gstring.h"

//#include "gmem.h"

//#include <string.h>

gboolean
g_str_equal (gconstpointer v1,
             gconstpointer v2)
{
  const gchar *string1 = v1;
  const gchar *string2 = v2;

  return strcmp (string1, string2) == 0;
}

guint
g_str_hash (gconstpointer v)
{
  const signed char *p;
  guint32 h = 5381;

  for (p = v; *p != '\0'; p++)
    h = (h << 5) + h + *p;

  return h;
}

#define MY_MAXSIZE ((gsize)-1)

static gsize
nearest_power (gsize base, gsize num)
{
  if (num > MY_MAXSIZE / 2)
  {
    return MY_MAXSIZE;
  }
  else
  {
    gsize n = base;

    while (n < num)
      n <<= 1;

    return n;
  }
}

static void
g_string_maybe_expand (GString* string,
                       gsize    len)
{
  if (string->len + len >= string->allocated_len)
  {
    string->allocated_len = nearest_power (1, string->len + len + 1);
    string->str = g_realloc (string->str, string->allocated_len);
  }
}

GString*
g_string_sized_new (gsize dfl_size)
{
  GString *string = g_malloc(sizeof(GString));

  string->allocated_len = 0;
  string->len   = 0;
  string->str   = NULL;

  g_string_maybe_expand (string, MAX (dfl_size, 2));
  string->str[0] = 0;

  return string;
}

GString*
g_string_new (const gchar *init)
{
  GString *string;

  if (init == NULL || *init == '\0')
    string = g_string_sized_new (2);
  else
  {
    gint len;

    len = strlen (init);
    string = g_string_sized_new (len + 2);

    g_string_append_len (string, init, len);
  }

  return string;
}

GString*
g_string_new_len (const gchar *init,
                  gssize       len)
{
  GString *string;

  if (len < 0)
    return g_string_new (init);
  else
  {
    string = g_string_sized_new (len);

    if (init)
      g_string_append_len (string, init, len);

    return string;
  }
}

gchar*
g_string_free (GString *string,
               gboolean free_segment)
{
  gchar *segment;

  if(!string)
  {
    return(NULL);
  }
  if (free_segment)
  {
    g_free (string->str);
    segment = NULL;
  }
  else
    segment = string->str;

  g_free (string);

  return segment;
}

GString*
g_string_truncate (GString *string,
                   gsize    len)
{
  if(!string)
  {
    return(NULL);
  }
  string->len = MIN (len, string->len);
  string->str[string->len] = 0;

  return string;
}

GString*
g_string_insert_len (GString     *string,
                     gssize       pos,
                     const gchar *val,
                     gssize       len)
{
  if(!string || !(len == 0 || val != NULL))
  {
    return(string);
  }
  if (len == 0)
    return string;

  if (len < 0)
    len = strlen (val);

  if (pos < 0)
    pos = string->len;
  else if(!(pos <= string->len))
    return(string);

  if (val >= string->str && val <= string->str + string->len)
  {
    gsize offset = val - string->str;
    gsize precount = 0;

    g_string_maybe_expand (string, len);
    val = string->str + offset;
    /* At this point, val is valid again.  */

    /* Open up space where we are going to insert.  */
    if (pos < string->len)
      memmove (string->str + pos + len, string->str + pos, string->len - pos);

    /* Move the source part before the gap, if any.  */
    if (offset < pos)
    {
      precount = MIN (len, pos - offset);
      memcpy (string->str + pos, val, precount);
    }

    /* Move the source part after the gap, if any.  */
    if (len > precount)
      memcpy (string->str + pos + precount,
              val + /* Already moved: */ precount + /* Space opened up: */ len,
              len - precount);
  }
  else
  {
    g_string_maybe_expand (string, len);

    /* If we aren't appending at the end, move a hunk
     * of the old string to the end, opening up space
     */
    if (pos < string->len)
      memmove (string->str + pos + len, string->str + pos, string->len - pos);

    /* insert the new string */
    if (len == 1)
      string->str[pos] = *val;
    else
      memcpy (string->str + pos, val, len);
  }

  string->len += len;

  string->str[string->len] = 0;

  return string;
}

GString*
g_string_append (GString     *string,
                 const gchar *val)
{
  if(!string || !val)
  {
    return(string);
  }
  return g_string_insert_len (string, -1, val, -1);
}

GString*
g_string_append_len (GString     *string,
                     const gchar *val,
                     gssize       len)
{
  if(!string || !(len == 0 || val != NULL))
  {
    return(string);
  }
  return g_string_insert_len (string, -1, val, len);
}

GString*
g_string_append_c (GString *string,
                   gchar    c)
{
  if(!string)
  {
    return(NULL);
  }
  return g_string_insert_c (string, -1, c);
}

//
// replacement function, using g_strdup_vprintf() instead of g_vasprintf()
//
void
g_string_append_vprintf (GString     *string,
                         const gchar *format,
                         va_list      args)
{
  gchar * buf;
  size_t  len;

  if(!string
  || !format)
    return;

  buf = g_strdup_vprintf(format, args);
  if (buf
  && (len = strlen(buf)))
  {
    g_string_maybe_expand (string, len);
    memcpy (string->str + string->len, buf, len + 1);
    string->len += len;
    g_free (buf);
  }
}

void
g_string_append_printf (GString     *string,
                        const gchar *format,
                        ...)
{
  va_list args;

  va_start (args, format);
  g_string_append_vprintf (string, format, args);
  va_end (args);
}

GString*
g_string_insert (GString     *string,
                 gssize       pos,
                 const gchar *val)
{
  if(!string || !val || !(pos <= string->len))
  {
    return(string);
  }
  return g_string_insert_len (string, pos, val, -1);
}

GString*
g_string_insert_c (GString *string,
                   gssize   pos,
                   gchar    c)
{
  if(!string)
  {
    return(NULL);
  }
  g_string_maybe_expand (string, 1);

  if (pos < 0)
    pos = string->len;
  else if(!(pos <= string->len))
    return(string);

  /* If not just an append, move the old stuff */
  if (pos < string->len)
    memmove (string->str + pos + 1, string->str + pos, string->len - pos);

  string->str[pos] = c;

  string->len += 1;

  string->str[string->len] = 0;

  return string;
}

GString*
g_string_erase (GString *string,
                gssize   pos,
                gssize   len)
{
  if(!string || !(pos >= 0) || !(pos <= string->len))
  {
    return(string);
  }

  if (len < 0)
    len = string->len - pos;
  else
  {
    if(!(pos + len <= string->len))
    {
      return(string);
    }

    if (pos + len < string->len)
      memmove (string->str + pos, string->str + pos + len, string->len - (pos + len));
  }

  string->len -= len;

  string->str[string->len] = 0;

  return string;
}
//
// END gstring.c
//

//
// BEGIN gwin32.c
//
#ifdef G_OS_WIN32

//#include "gwin32.h"

//#include "gmem.h"

//#include <string.h>
//#include <windows.h>

//
// g_win32_get_package_installation_subdir()
//
// Custom function, similar to the deprecated glib funtion named
// g_win32_get_package_installation_subdirectory().
//
#if defined(UNICODE) || defined (_UNICODE)
  #error This application must not be compiled in UNICODE mode.
#endif

gchar *
g_win32_get_package_installation_subdir(const gchar *  subdir)
{
  TCHAR   buffer[MAXPATHLEN];
  DWORD   len;
  gchar * result;

  len = GetModuleFileName(NULL, buffer, sizeof(buffer));
  if(!len || len >= sizeof(buffer)
  || strlen(buffer) + 1 + strlen(subdir) >= sizeof(buffer))
  {
    *buffer = 0;
  }
  else if((result = strrchr(buffer, '\\')))
  {
    *result = 0;
  }
  if(strlen(buffer) >= 4
  && (!stricmp(buffer + strlen(buffer) - 4, "\\bin")
   || !stricmp(buffer + strlen(buffer) - 4, "\\lib")))
  {
    buffer[strlen(buffer) - 4] = 0;
  }
  strcat(buffer, "\\");
  if(strlen(buffer) + strlen(subdir) < sizeof(buffer))
  {
    strcat(buffer, subdir);
  }
  result = g_malloc(strlen(buffer) + 1);
  strcpy(result, buffer);
  return(result);
}

#endif // G_OS_WIN32
//
// END gwin32.c
//
