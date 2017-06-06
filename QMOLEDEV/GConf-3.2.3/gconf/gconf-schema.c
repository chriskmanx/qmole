/* GConf
 * Copyright (C) 1999, 2000, 2002 Red Hat Inc.
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

#include <config.h>
#include "gconf-schema.h"
#include "gconf-internals.h"

/* FIXME clean this up, obviously RealSchema isn't needed. */
struct _GConfSchema {
  int dummy;
};

typedef struct {
  GConfValueType type; /* Type of the described entry */
  GConfValueType list_type; /* List type of the described entry */
  GConfValueType car_type; /* Pair car type of the described entry */
  GConfValueType cdr_type; /* Pair cdr type of the described entry */
  gchar* locale;       /* Schema locale */
  gchar* owner;        /* Name of creating application */
  gchar* short_desc;   /* 40 char or less description, no newlines */
  gchar* long_desc;    /* could be a paragraph or so */
  GConfValue* default_value; /* Default value of the key */
} GConfRealSchema;

#define REAL_SCHEMA(schema) ((GConfRealSchema*)(schema))

GConfSchema*  
gconf_schema_new (void)
{
  GConfRealSchema *real;

  real = g_new0 (GConfRealSchema, 1);

  real->type = GCONF_VALUE_INVALID;
  real->list_type = GCONF_VALUE_INVALID;
  real->car_type = GCONF_VALUE_INVALID;
  real->cdr_type = GCONF_VALUE_INVALID;

  return (GConfSchema*) real;
}

void          
gconf_schema_free (GConfSchema* sc)
{
  GConfRealSchema *real = REAL_SCHEMA (sc);
  
  g_free (real->locale);
  g_free (real->short_desc);
  g_free (real->long_desc);
  g_free (real->owner);

  if (real->default_value)
    gconf_value_free (real->default_value);
  
  g_free (sc);
}

GConfSchema*  
gconf_schema_copy (const GConfSchema* sc)
{
  GConfRealSchema *dest;
  const GConfRealSchema *real;

  real = REAL_SCHEMA (sc);
  dest = (GConfRealSchema*) gconf_schema_new ();

  dest->type = real->type;
  dest->list_type = real->list_type;
  dest->car_type = real->car_type;
  dest->cdr_type = real->cdr_type;

  dest->locale = g_strdup (real->locale);
  
  dest->short_desc = g_strdup (real->short_desc);

  dest->long_desc = g_strdup (real->long_desc);

  dest->owner = g_strdup (real->owner);

  dest->default_value = real->default_value ? gconf_value_copy (real->default_value) : NULL;
  
  return (GConfSchema*) dest;
}

void          
gconf_schema_set_type (GConfSchema* sc, GConfValueType type)
{
  REAL_SCHEMA (sc)->type = type;
}

void          
gconf_schema_set_list_type (GConfSchema* sc, GConfValueType type)
{
  REAL_SCHEMA (sc)->list_type = type;
}

void          
gconf_schema_set_car_type (GConfSchema* sc, GConfValueType type)
{
  REAL_SCHEMA (sc)->car_type = type;
}

void          
gconf_schema_set_cdr_type (GConfSchema* sc, GConfValueType type)
{
  REAL_SCHEMA (sc)->cdr_type = type;
}

void
gconf_schema_set_locale (GConfSchema* sc, const gchar* locale)
{
  g_return_if_fail (locale == NULL || g_utf8_validate (locale, -1, NULL));
  
  if (REAL_SCHEMA (sc)->locale)
    g_free (REAL_SCHEMA (sc)->locale);

  if (locale)
    REAL_SCHEMA (sc)->locale = g_strdup (locale);
  else 
    REAL_SCHEMA (sc)->locale = NULL;
}

void          
gconf_schema_set_short_desc (GConfSchema* sc, const gchar* desc)
{
  g_return_if_fail (desc == NULL || g_utf8_validate (desc, -1, NULL));
  
  if (REAL_SCHEMA (sc)->short_desc)
    g_free (REAL_SCHEMA (sc)->short_desc);

  if (desc)
    REAL_SCHEMA (sc)->short_desc = g_strdup (desc);
  else 
    REAL_SCHEMA (sc)->short_desc = NULL;
}

void          
gconf_schema_set_long_desc (GConfSchema* sc, const gchar* desc)
{
  g_return_if_fail (desc == NULL || g_utf8_validate (desc, -1, NULL));
  
  if (REAL_SCHEMA (sc)->long_desc)
    g_free (REAL_SCHEMA (sc)->long_desc);

  if (desc)
    REAL_SCHEMA (sc)->long_desc = g_strdup (desc);
  else 
    REAL_SCHEMA (sc)->long_desc = NULL;
}

void          
gconf_schema_set_owner (GConfSchema* sc, const gchar* owner)
{
  g_return_if_fail (owner == NULL || g_utf8_validate (owner, -1, NULL));
  
  if (REAL_SCHEMA (sc)->owner)
    g_free (REAL_SCHEMA (sc)->owner);

  if (owner)
    REAL_SCHEMA (sc)->owner = g_strdup (owner);
  else
    REAL_SCHEMA (sc)->owner = NULL;
}

void
gconf_schema_set_default_value (GConfSchema* sc, const GConfValue* val)
{
  if (REAL_SCHEMA (sc)->default_value != NULL)
    gconf_value_free (REAL_SCHEMA (sc)->default_value);

  REAL_SCHEMA (sc)->default_value = gconf_value_copy (val);
}

void
gconf_schema_set_default_value_nocopy (GConfSchema* sc, GConfValue* val)
{
  if (REAL_SCHEMA (sc)->default_value != NULL)
    gconf_value_free (REAL_SCHEMA (sc)->default_value);

  REAL_SCHEMA (sc)->default_value = val;
}

gboolean
gconf_schema_validate (const GConfSchema *sc,
                       GError           **err)
{
  GConfRealSchema *real;

  real = REAL_SCHEMA (sc);
  
  if (real->locale && !g_utf8_validate (real->locale, -1, NULL))
    {
      g_set_error (err, GCONF_ERROR,
                   GCONF_ERROR_FAILED,
                   _("Schema contains invalid UTF-8"));
      return FALSE;
    }

  if (real->short_desc && !g_utf8_validate (real->short_desc, -1, NULL))
    {
      g_set_error (err, GCONF_ERROR,
                   GCONF_ERROR_FAILED,
                   _("Schema contains invalid UTF-8"));
      return FALSE;
    }

  if (real->long_desc && !g_utf8_validate (real->long_desc, -1, NULL))
    {
      g_set_error (err, GCONF_ERROR,
                   GCONF_ERROR_FAILED,
                   _("Schema contains invalid UTF-8"));
      return FALSE;
    }

  if (real->owner && !g_utf8_validate (real->owner, -1, NULL))
    {
      g_set_error (err, GCONF_ERROR,
                   GCONF_ERROR_FAILED,
                   _("Schema contains invalid UTF-8"));
      return FALSE;
    }

  if (real->type == GCONF_VALUE_LIST &&
      real->list_type == GCONF_VALUE_INVALID)
    {
      g_set_error (err, GCONF_ERROR,
                   GCONF_ERROR_FAILED,
                   _("Schema specifies type list but doesn't specify the type of the list elements"));
      return FALSE;
    }
  
  if (real->type == GCONF_VALUE_PAIR &&
      (real->car_type == GCONF_VALUE_INVALID ||
       real->cdr_type == GCONF_VALUE_INVALID))
    {
      g_set_error (err, GCONF_ERROR,
                   GCONF_ERROR_FAILED,
                   _("Schema specifies type pair but doesn't specify the type of the car/cdr elements"));
      return FALSE;
    }
  
  return TRUE;
}

GConfValueType
gconf_schema_get_type (const GConfSchema *schema)
{
  g_return_val_if_fail (schema != NULL, GCONF_VALUE_INVALID);

  return REAL_SCHEMA (schema)->type;
}

GConfValueType
gconf_schema_get_list_type (const GConfSchema *schema)
{
  g_return_val_if_fail (schema != NULL, GCONF_VALUE_INVALID);

  return REAL_SCHEMA (schema)->list_type;
}

GConfValueType
gconf_schema_get_car_type (const GConfSchema *schema)
{
  g_return_val_if_fail (schema != NULL, GCONF_VALUE_INVALID);

  return REAL_SCHEMA (schema)->car_type;
}

GConfValueType
gconf_schema_get_cdr_type (const GConfSchema *schema)
{
  g_return_val_if_fail (schema != NULL, GCONF_VALUE_INVALID);

  return REAL_SCHEMA (schema)->cdr_type;
}

const char*
gconf_schema_get_locale (const GConfSchema *schema)
{
  g_return_val_if_fail (schema != NULL, NULL);

  return REAL_SCHEMA (schema)->locale;
}

const char*
gconf_schema_get_short_desc (const GConfSchema *schema)
{
  g_return_val_if_fail (schema != NULL, NULL);

  return REAL_SCHEMA (schema)->short_desc;
}

const char*
gconf_schema_get_long_desc (const GConfSchema *schema)
{
  g_return_val_if_fail (schema != NULL, NULL);

  return REAL_SCHEMA (schema)->long_desc;
}

const char*
gconf_schema_get_owner (const GConfSchema *schema)
{
  g_return_val_if_fail (schema != NULL, NULL);

  return REAL_SCHEMA (schema)->owner;
}

GConfValue*
gconf_schema_get_default_value (const GConfSchema *schema)
{
  g_return_val_if_fail (schema != NULL, NULL);

  return REAL_SCHEMA (schema)->default_value;
}

GConfValue*
gconf_schema_steal_default_value (GConfSchema *schema)
{
  GConfValue *val;
  
  g_return_val_if_fail (schema != NULL, NULL);

  val = REAL_SCHEMA (schema)->default_value;
  REAL_SCHEMA (schema)->default_value = NULL;

  return val;
}
