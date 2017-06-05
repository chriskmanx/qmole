/* -*- mode: C; c-file-style: "gnu" -*- */
/* dbus-gloader-expat.c  expat XML loader
 *
 * Copyright (C) 2003 Red Hat, Inc.
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

#include "dbus-gparser.h"
#include <expat.h>
#include <string.h>

static void*
expat_g_malloc (size_t sz)
{
  return g_malloc (sz);
}

static void*
expat_g_realloc (void *mem, size_t sz)
{
  return g_realloc (mem, sz);
}

static XML_Memory_Handling_Suite memsuite =
{
  expat_g_malloc,
  expat_g_realloc,
  g_free
};

/*
 * Context for Expat parser for introspection data.
 */
typedef struct
{
  Parser *parser;       /**< The parser for the introspection data */
  const char *filename; /**< The filename being loaded */
  GString *content;     /**< The content of the current element */
  GError **error;       /**< Error return location */
  gboolean failed;      /**< True if parse has failed */
} ExpatParseContext;

static dbus_bool_t
process_content (ExpatParseContext *context)
{
  if (context->failed)
    return FALSE;

  if (context->content->len > 0)
    {
      if (!parser_content (context->parser,
                           context->content->str,
                           context->content->len,
                           context->error))
        {
          context->failed = TRUE;
          return FALSE;
        }
      g_string_set_size (context->content, 0);
    }

  return TRUE;
}

static void
expat_StartElementHandler (void            *userData,
                           const XML_Char  *name,
                           const XML_Char **atts)
{
  ExpatParseContext *context = userData;
  int i;
  char **names;
  char **values;

  /* Expat seems to suck and can't abort the parse if we
   * throw an error. Expat 2.0 is supposed to fix this.
   */
  if (context->failed)
    return;

  if (!process_content (context))
    return;

  /* "atts" is key, value, key, value, NULL */
  for (i = 0; atts[i] != NULL; ++i)
    ; /* nothing */

  g_assert (i % 2 == 0);
  names = g_new0 (char *, i / 2 + 1);
  values = g_new0 (char *, i / 2 + 1);

  i = 0;
  while (atts[i] != NULL)
    {
      g_assert (i % 2 == 0);
      names [i / 2] = (char*) atts[i];
      values[i / 2] = (char*) atts[i+1];

      i += 2;
    }

  if (!parser_start_element (context->parser,
                             name,
                             (const char **) names,
                             (const char **) values,
                             context->error))
    {
      g_free (names);
      g_free (values);
      context->failed = TRUE;
      return;
    }

  g_free (names);
  g_free (values);
}

static void
expat_EndElementHandler (void           *userData,
                         const XML_Char *name)
{
  ExpatParseContext *context = userData;

  if (!process_content (context))
    return;

  if (!parser_end_element (context->parser,
                           name,
                           context->error))
    {
      context->failed = TRUE;
      return;
    }
}

/* s is not 0 terminated. */
static void
expat_CharacterDataHandler (void           *userData,
                            const XML_Char *s,
                            int             len)
{
  ExpatParseContext *context = userData;

  if (context->failed)
    return;

  g_string_append_len (context->content,
                       s, len);
}

NodeInfo*
description_load_from_file (const char       *filename,
                            GError          **error)
{
  char *contents;
  gsize len;
  NodeInfo *nodes;
  
  contents = NULL;
  if (!g_file_get_contents (filename, &contents, &len, error))
    return NULL;

  nodes = description_load_from_string (contents, len, error);
  g_free (contents);

  return nodes;
}

NodeInfo*
description_load_from_string (const char  *str,
                              int          len,
                              GError     **error)
{
  XML_Parser expat;
  ExpatParseContext context;
  NodeInfo *nodes;
  
  g_return_val_if_fail (error == NULL || *error == NULL, NULL);

  if (len < 0)
    len = strlen (str);
  
  expat = NULL;
  context.parser = NULL;
  context.error = error;
  context.failed = FALSE;
  
  expat = XML_ParserCreate_MM ("UTF-8", &memsuite, NULL);
  if (expat == NULL)
    g_error ("No memory to create XML parser\n");

  context.parser = parser_new ();
  context.content = g_string_new (NULL);
  
  XML_SetUserData (expat, &context);
  XML_SetElementHandler (expat,
                         expat_StartElementHandler,
                         expat_EndElementHandler);
  XML_SetCharacterDataHandler (expat,
                               expat_CharacterDataHandler);
  
  if (!XML_Parse (expat, str, len, TRUE))
    {
      if (context.error != NULL &&
          *context.error == NULL)
        {
            enum XML_Error e;

            e = XML_GetErrorCode (expat);
            if (e == XML_ERROR_NO_MEMORY)
              g_error ("Not enough memory to parse XML document");
            else
              g_set_error (error,
                           G_MARKUP_ERROR,
                           G_MARKUP_ERROR_PARSE,
                           "Error in D-BUS description XML, line %ld, column %ld: %s\n",
                           (gulong)XML_GetCurrentLineNumber (expat),
                           (gulong)XML_GetCurrentColumnNumber (expat),
                           XML_ErrorString (e));
        }
      
        goto failed;
    }
  
  if (context.failed)
    goto failed;

  if (!parser_finished (context.parser, error))
    goto failed;

  XML_ParserFree (expat);
  g_string_free (context.content, TRUE);

  g_return_val_if_fail (error == NULL || *error == NULL, NULL);
  nodes = parser_get_nodes (context.parser);
  node_info_ref (nodes);
  parser_unref (context.parser);
  return nodes;

 failed:
  g_return_val_if_fail (error == NULL || *error != NULL, NULL);

  g_string_free (context.content, TRUE);
  if (expat)
    XML_ParserFree (expat);
  if (context.parser)
    parser_unref (context.parser);
  return NULL;
}

