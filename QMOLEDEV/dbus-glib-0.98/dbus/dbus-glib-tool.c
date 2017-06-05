/* -*- mode: C; c-file-style: "gnu" -*- */
/* dbus-glib-tool.c Tool used by apps using glib bindings
 *
 * Copyright (C) 2003, 2004 Red Hat, Inc.
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
#include "dbus-gidl.h"
#include "dbus-gparser.h"
#include "dbus-gutils.h"
#include "dbus-glib-tool.h"
#include "dbus-binding-tool-glib.h"
#include <glib/gstdio.h>
#include <stdio.h>
#include <stdlib.h>
#include <errno.h>
#include <sys/stat.h>
#include <string.h>
#include <time.h>

typedef enum {
  DBUS_BINDING_OUTPUT_NONE,
  DBUS_BINDING_OUTPUT_PRETTY,
  DBUS_BINDING_OUTPUT_GLIB_SERVER,
  DBUS_BINDING_OUTPUT_GLIB_CLIENT
} DBusBindingOutputMode;

static void
indent (int depth)
{
  depth *= 2; /* 2-space indent */
  
  while (depth > 0)
    {
      putc (' ', stdout);
      --depth;
    }
}

static void pretty_print (BaseInfo *base,
                          int       depth);

static void
pretty_print_list (GSList *list,
                   int     depth)
{
  GSList *tmp;
  
  tmp = list;
  while (tmp != NULL)
    {
      pretty_print (tmp->data, depth);
      tmp = tmp->next;
    }
}

static void
pretty_print (BaseInfo *base,
              int       depth)
{
  InfoType t;
  const char *name;

  t = base_info_get_type (base);
  name = base_info_get_name (base);

  indent (depth);
  
  switch (t)
    {
    case INFO_TYPE_NODE:
      {
        NodeInfo *n = (NodeInfo*) base;
        
        if (name == NULL)
          printf ("<anonymous node> {\n");
        else
          printf ("node \"%s\" {\n", name);

        pretty_print_list (node_info_get_interfaces (n), depth + 1);
        pretty_print_list (node_info_get_nodes (n), depth + 1);

        indent (depth);
        printf ("}\n");
      }
      break;
    case INFO_TYPE_INTERFACE:
      {
        InterfaceInfo *i = (InterfaceInfo*) base;
	GSList *annotations, *elt;

        g_assert (name != NULL);

        printf ("interface \"%s\" {\n", name);

	annotations = interface_info_get_annotations (i);
	for (elt = annotations; elt; elt = elt->next)
	  {
	    const char *name = elt->data;
	    const char *value = interface_info_get_annotation (i, name);

	    printf (" (binding \"%s\": \"%s\") ",
		    name, value);
	  }
	g_slist_free (annotations);

        pretty_print_list (interface_info_get_methods (i), depth + 1);
        pretty_print_list (interface_info_get_signals (i), depth + 1);
        pretty_print_list (interface_info_get_properties (i), depth + 1);

        indent (depth);
        printf ("}\n");
      }
      break;
    case INFO_TYPE_METHOD:
      {
        MethodInfo *m = (MethodInfo*) base;
	GSList *annotations, *elt;

        g_assert (name != NULL);

	annotations = method_info_get_annotations (m);
        printf ("method \"%s\" (\n", name);
	for (elt = annotations; elt; elt = elt->next)
	  {
	    const char *name = elt->data;
	    const char *value = method_info_get_annotation (m, name);

	    printf (" (annotation \"%s\": \"%s\") ",
		    name, value);
	  }
	g_slist_free (annotations);

        pretty_print_list (method_info_get_args (m), depth + 1);

        indent (depth);
        printf (")\n");
      }
      break;
    case INFO_TYPE_SIGNAL:
      {
        SignalInfo *s = (SignalInfo*) base;

        g_assert (name != NULL);

        printf ("signal \"%s\" (\n", name);

        pretty_print_list (signal_info_get_args (s), depth + 1);

        indent (depth);
        printf (")\n");
      }
      break;
    case INFO_TYPE_PROPERTY:
      {
        PropertyInfo *a = (PropertyInfo*) base;
        const char *pt = property_info_get_type (a);
        PropertyAccessFlags acc = property_info_get_access (a);

        printf ("%s%s %s",
                acc & PROPERTY_READ ? "read" : "",
                acc & PROPERTY_WRITE ? "write" : "",
                pt);
        if (name)
          printf (" %s\n", name);
        else
          printf ("\n");
      }
      break;
    case INFO_TYPE_ARG:
      {
        ArgInfo *a = (ArgInfo*) base;
        const char *at = arg_info_get_type (a);
        ArgDirection d = arg_info_get_direction (a);

        printf ("%s %s",
                d == ARG_IN ? "in" : "out",
                at);
        if (name)
          printf (" %s\n", name);
        else
          printf ("\n");
      }
      break;
    }
}

GQuark
dbus_binding_tool_error_quark (void)
{
  static GQuark quark = 0;
  if (!quark)
    quark = g_quark_from_static_string ("dbus_binding_tool_error");

  return quark;
}

static void warn (const char *fmt, ...) G_GNUC_PRINTF (1, 2);
static void warn_gerror (const char *prefix, GError *error);

static void
warn (const char *str, ...)
{
  va_list args;

  va_start (args, str);

  vfprintf (stderr, str, args);
  fputc ('\n', stderr);

  va_end (args);
}

static void
warn_gerror (const char *prefix, GError *error) 
{
  warn ("%s: %s", prefix, error->message);
}

static void
usage (int ecode)
{
  fprintf (stderr, "dbus-binding-tool [--version] [--help]\n");
  fprintf (stderr, "dbus-binding-tool --mode=[pretty|glib-server|glib-client] [--prefix=SYMBOL_PREFIX] [--ignore-unsupported] [--force] [--output=FILE]\n");
  fprintf (stderr, "dbus-binding-tool --mode=glib-server --prefix=SYMBOL_PREFIX [--ignore-unsupported] [--force] [--output=FILE]\n");
  exit (ecode);
}

static void
version (void)
{
  printf ("D-BUS Binding Tool %s\n"
          "Copyright (C) 2003-2005 Red Hat, Inc.\n"
          "This is free software; see the source for copying conditions.\n"
          "There is NO warranty; not even for MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.\n",
          VERSION);
  exit (0);
}

int
main (int argc, char **argv)
{
  const char *output_file;
  const char *prefix;
  char *output_file_tmp = NULL;
  int i;
  GSList *files;
  DBusBindingOutputMode outputmode;
  gboolean end_of_args;
  GSList *tmp;
  GIOChannel *channel = NULL;
  GError *error;
  time_t newest_src;
  struct stat srcbuf;
  struct stat targetbuf;
  gboolean force;
  gboolean ignore_unsupported;
  gboolean has_prefix = FALSE;

  g_type_init ();

  outputmode = DBUS_BINDING_OUTPUT_NONE;
  end_of_args = FALSE;
  files = NULL;
  output_file = NULL;
  prefix = "";
  ignore_unsupported = FALSE;
  force = FALSE;
  i = 1;
  while (i < argc)
    {
      const char *arg = argv[i];

      if (!end_of_args)
        {
          if (strcmp (arg, "--help") == 0 ||
              strcmp (arg, "-h") == 0 ||
              strcmp (arg, "-?") == 0)
            usage (0);
          else if (strcmp (arg, "--version") == 0)
            version ();
          else if (strcmp (arg, "--force") == 0)
            force = TRUE;
          else if (strncmp (arg, "--mode=", 7) == 0)
            {
	      const char *mode = arg + 7;
	      if (!strcmp (mode, "pretty"))
		outputmode = DBUS_BINDING_OUTPUT_PRETTY;
	      else if (!strcmp (mode, "glib-server"))
		outputmode = DBUS_BINDING_OUTPUT_GLIB_SERVER;
	      else if (!strcmp (mode, "glib-client"))
		outputmode = DBUS_BINDING_OUTPUT_GLIB_CLIENT;
	      else
		usage (1);
	    }
          else if (strcmp (arg, "--ignore-unsupported") == 0)
            ignore_unsupported = TRUE;
	  else if (strncmp (arg, "--output=", 9) == 0)
	    {
	      output_file = arg + 9;
	    }
          else if (strncmp (arg, "--prefix=", 9) == 0)
            {
              has_prefix = TRUE;
              prefix = arg + 9;
            }
          else if (arg[0] == '-' &&
                   arg[1] == '-' &&
                   arg[2] == '\0')
            end_of_args = TRUE;
          else if (arg[0] == '-')
            {
              usage (1);
            }
          else
            {
              files = g_slist_prepend (files, (char*) arg);
            }
        }
      else
        files = g_slist_prepend (files, (char*) arg);
      
      ++i;
    }

  if (outputmode == DBUS_BINDING_OUTPUT_GLIB_SERVER && !has_prefix)
    usage (1);

  error = NULL;

  files = g_slist_reverse (files);

  if (output_file && !force)
    {
      newest_src = 0;
      for (tmp = files; tmp != NULL; tmp = tmp->next)
	{
	  const char *filename;

	  filename = tmp->data;
	  if (stat (filename, &srcbuf) < 0)
            {
              warn ("Couldn't stat %s: %s", filename, g_strerror (errno));
              goto lose;
            }

	  if (srcbuf.st_mtime > newest_src)
	    newest_src = srcbuf.st_mtime;
	}

      if (stat (output_file, &targetbuf) > 0
	  && targetbuf.st_mtime >= newest_src)
	exit (0);
    }
  
  if (output_file)
    {
      output_file_tmp = g_strconcat (output_file, ".tmp", NULL);

      if (!(channel = g_io_channel_new_file (output_file_tmp, "w", &error)))
        {
          warn_gerror ("Couldn't open temporary file", error);
          goto lose;
        }
    }
  else
    {
      channel = g_io_channel_unix_new (fileno (stdout));
    }

  if (!g_io_channel_set_encoding (channel, NULL, &error))
    {
      warn_gerror ("Couldn't set channel encoding to NULL", error);
      goto lose;
    }


  for (tmp = files; tmp != NULL; tmp = tmp->next)
    {
      NodeInfo *node;
      GError *error;
      const char *filename;

      filename = tmp->data;

      error = NULL;
      node = description_load_from_file (filename,
                                         &error);
      if (node == NULL)
        {
          warn ("Unable to load \"%s\": %s", filename, error->message);
          goto lose;
        }
      else
	{
	  switch (outputmode)
	    {
	    case DBUS_BINDING_OUTPUT_PRETTY:
	      pretty_print ((BaseInfo*) node, 0);
	      break;
	    case DBUS_BINDING_OUTPUT_GLIB_SERVER:
	      if (!dbus_binding_tool_output_glib_server ((BaseInfo *) node, channel, prefix, &error))
                {
                  warn_gerror ("Compilation failed", error);
                  node_info_unref (node);
                  goto lose;
                }
	      break;
	    case DBUS_BINDING_OUTPUT_GLIB_CLIENT:
	      if (!dbus_binding_tool_output_glib_client ((BaseInfo *) node, channel, ignore_unsupported, &error))
                {
                  warn_gerror ("Compilation failed", error);
                  node_info_unref (node);
                  goto lose;
                }
	      break;
	    case DBUS_BINDING_OUTPUT_NONE:
	      break;
	    }
	}

      if (node)
        node_info_unref (node);
    }

  if (g_io_channel_shutdown (channel, TRUE, &error) != G_IO_STATUS_NORMAL)
    {
      warn_gerror ("Failed to shutdown IO channel", error);
      goto lose;
    }

  g_io_channel_unref (channel);
  channel = NULL;

  if (output_file)
    {
      if (rename (output_file_tmp, output_file) < 0)
        {
          warn ("Failed to rename %s to %s: %s", output_file_tmp, output_file,
              g_strerror (errno));
          goto lose;
        }
      g_free (output_file_tmp);
    }

  return 0;

lose:
  /* ignore errors, we're already handling an error */
  if (channel != NULL)
    {
      (void) g_io_channel_shutdown (channel, FALSE, NULL);
      g_io_channel_unref (channel);
    }

  if (output_file_tmp != NULL)
    (void) g_remove (output_file_tmp);

  g_free (output_file_tmp);
  return 1;
}
