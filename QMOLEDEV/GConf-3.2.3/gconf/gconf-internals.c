
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

#include <config.h>
#include "gconf-internals.h"
#include "gconf-backend.h"
#include "gconf-schema.h"
#include "gconf.h"
#include <string.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <unistd.h>
#include <stdlib.h>
#include <stdio.h>
#include <fcntl.h>
#include <errno.h>
#include <ctype.h>
#include <locale.h>
#include <time.h>
#include <math.h>

#include <gio/gio.h>

#ifdef G_OS_WIN32
#include <windows.h>
#include <share.h>
#define pipe(fds) _pipe(fds, 4096, _O_BINARY)
#endif

gboolean gconf_log_debug_messages = FALSE;

static gboolean gconf_daemon_mode = FALSE;
static gchar* daemon_ior = NULL;

void
gconf_set_daemon_mode(gboolean setting)
{
  gconf_daemon_mode = setting;
}

gboolean
gconf_in_daemon_mode(void)
{
  return gconf_daemon_mode;
}

void
gconf_set_daemon_ior(const gchar* ior)
{
  if (daemon_ior != NULL)
    {
      g_free(daemon_ior);
      daemon_ior = NULL;
    }
      
  if (ior != NULL)
    daemon_ior = g_strdup(ior);
}

const gchar*
gconf_get_daemon_ior(void)
{
  return daemon_ior;
}

gchar*
gconf_key_directory  (const gchar* key)
{
  const gchar* end;
  gchar* retval;
  int len;

  end = strrchr(key, '/');

  if (end == NULL)
    {
      gconf_log(GCL_ERR, _("No '/' in key \"%s\""), key);
      return NULL;
    }

  len = end-key+1;

  if (len == 1)
    {
      /* Root directory */
      retval = g_strdup("/");
    }
  else 
    {
      retval = g_malloc(len);
      
      strncpy(retval, key, len);
      
      retval[len-1] = '\0';
    }

  return retval;
}

const gchar*
gconf_key_key        (const gchar* key)
{
  const gchar* end;
  
  end = strrchr(key, '/');

  if (end != NULL)
    ++end;

  return end;
}

/*
 *  Random stuff 
 */

#if HAVE_CORBA
GConfValue* 
gconf_value_from_corba_value(const ConfigValue* value)
{
  GConfValue* gval;
  GConfValueType type = GCONF_VALUE_INVALID;
  
  switch (value->_d)
    {
    case InvalidVal:
      return NULL;
    case IntVal:
      type = GCONF_VALUE_INT;
      break;
    case StringVal:
      type = GCONF_VALUE_STRING;
      break;
    case FloatVal:
      type = GCONF_VALUE_FLOAT;
      break;
    case BoolVal:
      type = GCONF_VALUE_BOOL;
      break;
    case SchemaVal:
      type = GCONF_VALUE_SCHEMA;
      break;
    case ListVal:
      type = GCONF_VALUE_LIST;
      break;
    case PairVal:
      type = GCONF_VALUE_PAIR;
      break;
    default:
      gconf_log(GCL_DEBUG, "Invalid type in %s", G_STRFUNC);
      return NULL;
    }

  g_assert(GCONF_VALUE_TYPE_VALID(type));
  
  gval = gconf_value_new(type);

  switch (gval->type)
    {
    case GCONF_VALUE_INT:
      gconf_value_set_int(gval, value->_u.int_value);
      break;
    case GCONF_VALUE_STRING:
      if (!g_utf8_validate (value->_u.string_value, -1, NULL))
        {
          gconf_log (GCL_ERR, _("Invalid UTF-8 in string value in '%s'"),
                     value->_u.string_value); 
        }
      else
        {
          gconf_value_set_string(gval, value->_u.string_value);
        }
      break;
    case GCONF_VALUE_FLOAT:
      gconf_value_set_float(gval, value->_u.float_value);
      break;
    case GCONF_VALUE_BOOL:
      gconf_value_set_bool(gval, value->_u.bool_value);
      break;
    case GCONF_VALUE_SCHEMA:
      gconf_value_set_schema_nocopy(gval, 
                                    gconf_schema_from_corba_schema(&(value->_u.schema_value)));
      break;
    case GCONF_VALUE_LIST:
      {
        GSList* list = NULL;
        guint i = 0;
        
        switch (value->_u.list_value.list_type)
          {
          case BIntVal:
            gconf_value_set_list_type(gval, GCONF_VALUE_INT);
            break;
          case BBoolVal:
            gconf_value_set_list_type(gval, GCONF_VALUE_BOOL);
            break;
          case BFloatVal:
            gconf_value_set_list_type(gval, GCONF_VALUE_FLOAT);
            break;
          case BStringVal:
            gconf_value_set_list_type(gval, GCONF_VALUE_STRING);
            break;
          case BInvalidVal:
            break;
          default:
            g_warning("Bizarre list type in %s", G_STRFUNC);
            break;
          }

        if (gconf_value_get_list_type(gval) != GCONF_VALUE_INVALID)
          {
            i = 0;
            while (i < value->_u.list_value.seq._length)
              {
                GConfValue* val;
                
                /* This is a bit dubious; we cast a ConfigBasicValue to ConfigValue
                   because they have the same initial members, but by the time
                   the CORBA and C specs kick in, not sure we are guaranteed
                   to be able to do this.
                */
                val = gconf_value_from_corba_value((ConfigValue*)&value->_u.list_value.seq._buffer[i]);
                
                if (val == NULL)
                  gconf_log(GCL_ERR, _("Couldn't interpret CORBA value for list element"));
                else if (val->type != gconf_value_get_list_type(gval))
                  gconf_log(GCL_ERR, _("Incorrect type for list element in %s"), G_STRFUNC);
                else
                  list = g_slist_prepend(list, val);
                
                ++i;
              }
        
            list = g_slist_reverse(list);
            
            gconf_value_set_list_nocopy(gval, list);
          }
        else
          {
            gconf_log(GCL_ERR, _("Received list from gconfd with a bad list type"));
          }
      }
      break;
    case GCONF_VALUE_PAIR:
      {
        g_return_val_if_fail(value->_u.pair_value._length == 2, gval);
        
        gconf_value_set_car_nocopy(gval,
                                   gconf_value_from_corba_value((ConfigValue*)&value->_u.list_value.seq._buffer[0]));

        gconf_value_set_cdr_nocopy(gval,
                                   gconf_value_from_corba_value((ConfigValue*)&value->_u.list_value.seq._buffer[1]));
      }
      break;
    default:
      g_assert_not_reached();
      break;
    }
  
  return gval;
}

void          
gconf_fill_corba_value_from_gconf_value(const GConfValue *value, 
                                        ConfigValue      *cv)
{
  if (value == NULL)
    {
      cv->_d = InvalidVal;
      return;
    }

  switch (value->type)
    {
    case GCONF_VALUE_INT:
      cv->_d = IntVal;
      cv->_u.int_value = gconf_value_get_int(value);
      break;
    case GCONF_VALUE_STRING:
      cv->_d = StringVal;
      cv->_u.string_value = CORBA_string_dup((char*)gconf_value_get_string(value));
      break;
    case GCONF_VALUE_FLOAT:
      cv->_d = FloatVal;
      cv->_u.float_value = gconf_value_get_float(value);
      break;
    case GCONF_VALUE_BOOL:
      cv->_d = BoolVal;
      cv->_u.bool_value = gconf_value_get_bool(value);
      break;
    case GCONF_VALUE_SCHEMA:
      cv->_d = SchemaVal;
      gconf_fill_corba_schema_from_gconf_schema (gconf_value_get_schema(value),
                                                 &cv->_u.schema_value);
      break;
    case GCONF_VALUE_LIST:
      {
        guint n, i;
        GSList* list;
        
        cv->_d = ListVal;

        list = gconf_value_get_list(value);

        n = g_slist_length(list);

        cv->_u.list_value.seq._buffer =
          CORBA_sequence_ConfigBasicValue_allocbuf(n);
        cv->_u.list_value.seq._length = n;
        cv->_u.list_value.seq._maximum = n;
        CORBA_sequence_set_release(&cv->_u.list_value.seq, TRUE);
        
        switch (gconf_value_get_list_type(value))
          {
          case GCONF_VALUE_INT:
            cv->_u.list_value.list_type = BIntVal;
            break;

          case GCONF_VALUE_BOOL:
            cv->_u.list_value.list_type = BBoolVal;
            break;
            
          case GCONF_VALUE_STRING:
            cv->_u.list_value.list_type = BStringVal;
            break;

          case GCONF_VALUE_FLOAT:
            cv->_u.list_value.list_type = BFloatVal;
            break;

          case GCONF_VALUE_SCHEMA:
            cv->_u.list_value.list_type = BSchemaVal;
            break;
            
          default:
            cv->_u.list_value.list_type = BInvalidVal;
            gconf_log(GCL_DEBUG, "Invalid list type in %s", G_STRFUNC);
            break;
          }
        
        i= 0;
        while (list != NULL)
          {
            /* That dubious ConfigBasicValue->ConfigValue cast again */
            gconf_fill_corba_value_from_gconf_value((GConfValue*)list->data,
                                                    (ConfigValue*)&cv->_u.list_value.seq._buffer[i]);

            list = g_slist_next(list);
            ++i;
          }
      }
      break;
    case GCONF_VALUE_PAIR:
      {
        cv->_d = PairVal;

        cv->_u.pair_value._buffer =
          CORBA_sequence_ConfigBasicValue_allocbuf(2);
        cv->_u.pair_value._length = 2;
        cv->_u.pair_value._maximum = 2;
        CORBA_sequence_set_release(&cv->_u.pair_value, TRUE);
        
        /* dubious cast */
        gconf_fill_corba_value_from_gconf_value (gconf_value_get_car(value),
                                                 (ConfigValue*)&cv->_u.pair_value._buffer[0]);
        gconf_fill_corba_value_from_gconf_value(gconf_value_get_cdr(value),
                                                (ConfigValue*)&cv->_u.pair_value._buffer[1]);
      }
      break;
      
    case GCONF_VALUE_INVALID:
      cv->_d = InvalidVal;
      break;
    default:
      cv->_d = InvalidVal;
      gconf_log(GCL_DEBUG, "Unknown type in %s", G_STRFUNC);
      break;
    }
}

ConfigValue*  
gconf_corba_value_from_gconf_value (const GConfValue* value)
{
  ConfigValue* cv;

  cv = ConfigValue__alloc();

  gconf_fill_corba_value_from_gconf_value(value, cv);

  return cv;
}

ConfigValue*  
gconf_invalid_corba_value (void)
{
  ConfigValue* cv;

  cv = ConfigValue__alloc();

  cv->_d = InvalidVal;

  return cv;
}

gchar*
gconf_object_to_string (CORBA_Object obj,
                        GError **err)
{
  CORBA_Environment ev;
  gchar *ior;
  gchar *retval;
  
  CORBA_exception_init (&ev);

  ior = CORBA_ORB_object_to_string (gconf_orb_get (), obj, &ev);

  if (ior == NULL)
    {
      gconf_set_error (err,
                       GCONF_ERROR_FAILED,
                       _("Failed to convert object to IOR"));

      return NULL;
    }

  retval = g_strdup (ior);

  CORBA_free (ior);

  return retval;
}

static ConfigValueType
corba_type_from_gconf_type(GConfValueType type)
{
  switch (type)
    {
    case GCONF_VALUE_INT:
      return IntVal;
    case GCONF_VALUE_BOOL:
      return BoolVal;
    case GCONF_VALUE_FLOAT:
      return FloatVal;
    case GCONF_VALUE_INVALID:
      return InvalidVal;
    case GCONF_VALUE_STRING:
      return StringVal;
    case GCONF_VALUE_SCHEMA:
      return SchemaVal;
    case GCONF_VALUE_LIST:
      return ListVal;
    case GCONF_VALUE_PAIR:
      return PairVal;
    default:
      g_assert_not_reached();
      return InvalidVal;
    }
}

static GConfValueType
gconf_type_from_corba_type(ConfigValueType type)
{
  switch (type)
    {
    case InvalidVal:
      return GCONF_VALUE_INVALID;
    case StringVal:
      return GCONF_VALUE_STRING;
    case IntVal:
      return GCONF_VALUE_INT;
    case FloatVal:
      return GCONF_VALUE_FLOAT;
    case SchemaVal:
      return GCONF_VALUE_SCHEMA;
    case BoolVal:
      return GCONF_VALUE_BOOL;
    case ListVal:
      return GCONF_VALUE_LIST;
    case PairVal:
      return GCONF_VALUE_PAIR;
    default:
      g_assert_not_reached();
      return GCONF_VALUE_INVALID;
    }
}

void          
gconf_fill_corba_schema_from_gconf_schema(const GConfSchema *sc, 
                                          ConfigSchema      *cs)
{
  cs->value_type = corba_type_from_gconf_type (gconf_schema_get_type (sc));
  cs->value_list_type = corba_type_from_gconf_type (gconf_schema_get_list_type (sc));
  cs->value_car_type = corba_type_from_gconf_type (gconf_schema_get_car_type (sc));
  cs->value_cdr_type = corba_type_from_gconf_type (gconf_schema_get_cdr_type (sc));

  cs->locale = CORBA_string_dup (gconf_schema_get_locale (sc) ? gconf_schema_get_locale (sc) : "");
  cs->short_desc = CORBA_string_dup (gconf_schema_get_short_desc (sc) ? gconf_schema_get_short_desc (sc) : "");
  cs->long_desc = CORBA_string_dup (gconf_schema_get_long_desc (sc) ? gconf_schema_get_long_desc (sc) : "");
  cs->owner = CORBA_string_dup (gconf_schema_get_owner (sc) ? gconf_schema_get_owner (sc) : "");

  {
    gchar* encoded;
    GConfValue* default_val;

    default_val = gconf_schema_get_default_value (sc);

    if (default_val)
      {
        encoded = gconf_value_encode (default_val);

        g_assert (encoded != NULL);

        cs->encoded_default_value = CORBA_string_dup (encoded);

        g_free (encoded);
      }
    else
      cs->encoded_default_value = CORBA_string_dup ("");
  }
}

ConfigSchema* 
gconf_corba_schema_from_gconf_schema (const GConfSchema* sc)
{
  ConfigSchema* cs;

  cs = ConfigSchema__alloc ();

  gconf_fill_corba_schema_from_gconf_schema (sc, cs);

  return cs;
}

GConfSchema*  
gconf_schema_from_corba_schema(const ConfigSchema* cs)
{
  GConfSchema* sc;
  GConfValueType type = GCONF_VALUE_INVALID;
  GConfValueType list_type = GCONF_VALUE_INVALID;
  GConfValueType car_type = GCONF_VALUE_INVALID;
  GConfValueType cdr_type = GCONF_VALUE_INVALID;

  type = gconf_type_from_corba_type(cs->value_type);
  list_type = gconf_type_from_corba_type(cs->value_list_type);
  car_type = gconf_type_from_corba_type(cs->value_car_type);
  cdr_type = gconf_type_from_corba_type(cs->value_cdr_type);

  sc = gconf_schema_new();

  gconf_schema_set_type(sc, type);
  gconf_schema_set_list_type(sc, list_type);
  gconf_schema_set_car_type(sc, car_type);
  gconf_schema_set_cdr_type(sc, cdr_type);

  if (*cs->locale != '\0')
    {
      if (!g_utf8_validate (cs->locale, -1, NULL))
        gconf_log (GCL_ERR, _("Invalid UTF-8 in locale for schema"));
      else
        gconf_schema_set_locale(sc, cs->locale);
    }

  if (*cs->short_desc != '\0')
    {
      if (!g_utf8_validate (cs->short_desc, -1, NULL))
        gconf_log (GCL_ERR, _("Invalid UTF-8 in short description for schema"));
      else
        gconf_schema_set_short_desc(sc, cs->short_desc);
    }

  if (*cs->long_desc != '\0')
    {
      if (!g_utf8_validate (cs->long_desc, -1, NULL))
        gconf_log (GCL_ERR, _("Invalid UTF-8 in long description for schema"));
      else
        gconf_schema_set_long_desc(sc, cs->long_desc);
    }

  if (*cs->owner != '\0')
    {
      if (!g_utf8_validate (cs->owner, -1, NULL))
        gconf_log (GCL_ERR, _("Invalid UTF-8 in owner for schema"));
      else
        gconf_schema_set_owner(sc, cs->owner);
    }
      
  {
    GConfValue* val;

    val = gconf_value_decode(cs->encoded_default_value);

    if (val)
      gconf_schema_set_default_value_nocopy(sc, val);
  }
  
  return sc;
}
#endif /* HAVE_CORBA */

const gchar* 
gconf_value_type_to_string(GConfValueType type)
{
  switch (type)
    {
    case GCONF_VALUE_INT:
      return "int";
    case GCONF_VALUE_STRING:
      return "string";
    case GCONF_VALUE_FLOAT:
      return "float";
    case GCONF_VALUE_BOOL:
      return "bool";
    case GCONF_VALUE_SCHEMA:
      return "schema";
    case GCONF_VALUE_LIST:
      return "list";
    case GCONF_VALUE_PAIR:
      return "pair";
    case GCONF_VALUE_INVALID:
      return "*invalid*";
    default:
      g_assert_not_reached();
      return NULL; /* for warnings */
    }
}

GConfValueType 
gconf_value_type_from_string(const gchar* type_str)
{
  if (strcmp(type_str, "int") == 0)
    return GCONF_VALUE_INT;
  else if (strcmp(type_str, "float") == 0)
    return GCONF_VALUE_FLOAT;
  else if (strcmp(type_str, "string") == 0)
    return GCONF_VALUE_STRING;
  else if (strcmp(type_str, "bool") == 0)
    return GCONF_VALUE_BOOL;
  else if (strcmp(type_str, "schema") == 0)
    return GCONF_VALUE_SCHEMA;
  else if (strcmp(type_str, "list") == 0)
    return GCONF_VALUE_LIST;
  else if (strcmp(type_str, "pair") == 0)
    return GCONF_VALUE_PAIR;
  else
    return GCONF_VALUE_INVALID;
}

/*
 * Configuration files (yikes! we can't store our configuration in GConf!)
 */

static gchar*
unquote_string(gchar* s)
{
  gchar* end;

  /* Strip whitespace and first quote from front of string */
  while (*s && (g_ascii_isspace(*s) || (*s == '"')))
    ++s;

  end = s;
  while (*end)
    ++end;

  --end; /* one back from '\0' */

  /* Strip whitespace and last quote from end of string */
  while ((end > s) && (g_ascii_isspace(*end) || (*end == '"')))
    {
      *end = '\0';
      --end;
    }

  return s;
}

#ifdef G_OS_WIN32

/* Return the home directory with forward slashes instead of backslashes */

const char*
_gconf_win32_get_home_dir (void)
{
  static GQuark quark = 0;
  char *home_copy, *p;

  if (quark != 0)
    return g_quark_to_string (quark);
  
  home_copy = g_strdup (g_get_home_dir ());

  /* Replace backslashes with forward slashes */
  for (p = home_copy; *p; p++)
    if (*p == '\\')
      *p = '/';
  
  quark = g_quark_from_string (home_copy);
  g_free (home_copy);

  return g_quark_to_string (quark);
}

#endif

static const gchar*
get_variable(const gchar* varname)
{
  /* These first two DO NOT use environment variables, which
     makes things a bit more "secure" in some sense
  */
  if (strcmp(varname, "HOME") == 0)
    {
#ifndef G_OS_WIN32
      return g_get_home_dir ();
#else
      return _gconf_win32_get_home_dir ();
#endif
    }
  else if (strcmp(varname, "USER") == 0)
    {
      return g_get_user_name();
    }
  else if (varname[0] == 'E' &&
           varname[1] == 'N' &&
           varname[2] == 'V' &&
           varname[3] == '_')
    {
      /* This is magic: if a variable called ENV_FOO is used,
         then the environment variable FOO is checked */
      const gchar* envvar = g_getenv(&varname[4]);

      if (envvar)
        return envvar;
      else
        return "";
    }
  else
    return "";
}

static gchar*
subst_variables(const gchar* src)
{
  const gchar* iter;
  gchar* retval;
  guint retval_len;
  guint pos;
  
  g_return_val_if_fail(src != NULL, NULL);

  retval_len = strlen(src) + 1;
  pos = 0;
  
  retval = g_malloc0(retval_len+3); /* add 3 just to avoid off-by-one
                                       segvs - yeah I know it bugs
                                       you, but C sucks */
  
  iter = src;

  while (*iter)
    {
      gboolean performed_subst = FALSE;
      
      if (pos >= retval_len)
        {
          retval_len *= 2;
          retval = g_realloc(retval, retval_len+3); /* add 3 for luck */
        }
      
      if (*iter == '$' && *(iter+1) == '(')
        {
          const gchar* varstart = iter + 2;
          const gchar* varend   = strchr(varstart, ')');

          if (varend != NULL)
            {
              char* varname;
              const gchar* varval;
              guint varval_len;

              performed_subst = TRUE;

              varname = g_strndup(varstart, varend - varstart);
              
              varval = get_variable(varname);
              g_free(varname);

              varval_len = strlen(varval);

              if ((retval_len - pos) < varval_len)
                {
                  retval_len = pos + varval_len;
                  retval = g_realloc(retval, retval_len+3);
                }
              
              strcpy(&retval[pos], varval);
              pos += varval_len;

              iter = varend + 1;
            }
        }

      if (!performed_subst)
        {
          retval[pos] = *iter;
          ++pos;
          ++iter;
        }
    }
  retval[pos] = '\0';

  return retval;
}

GSList *
gconf_load_source_path(const gchar* filename, GError** err)
{
  FILE* f;
  GSList *l = NULL;
  gchar buf[512];

  f = g_fopen(filename, "r");

  if (f == NULL)
    {
      if (err)
        *err = gconf_error_new(GCONF_ERROR_FAILED,
                               _("Couldn't open path file `%s': %s\n"), 
                               filename, 
                               g_strerror(errno));
      return NULL;
    }

  while (fgets(buf, 512, f) != NULL)
    {
      gchar* s = buf;
      
      while (*s && g_ascii_isspace(*s))
        ++s;

      if (*s == '#')
        {
          /* Allow comments, why not */
        }
      else if (*s == '\0')
        {
          /* Blank line */
        }
      else if (strncmp("include", s, 7) == 0)
        {
          gchar* unq;
          GSList* included;
          gchar *varsubst;
          
          s += 7;
          while (g_ascii_isspace(*s))
            s++;
          unq = unquote_string(s);

          varsubst = subst_variables (unq);
#ifdef G_OS_WIN32
	  {
	    gchar *tem = varsubst;
	    varsubst = _gconf_win32_replace_prefix (varsubst);
	    g_free (tem);
	  }
#endif
          included = gconf_load_source_path (varsubst, NULL);
          g_free (varsubst);
          
          if (included != NULL)
            l = g_slist_concat (l, included);
        }
      else 
        {
          gchar* unq;
          gchar* varsubst;
          
          unq = unquote_string(buf);
          varsubst = subst_variables(unq);
          
          if (*varsubst != '\0') /* Drop lines with just two quote marks or something */
            {
              gconf_log(GCL_DEBUG, _("Adding source `%s'\n"), varsubst);
              l = g_slist_append (l, varsubst);
            }
	  else
	    {
	      g_free (varsubst);
	    }
        }
    }

  if (ferror(f))
    {
      /* This should basically never happen */
      if (err)
        *err = gconf_error_new(GCONF_ERROR_FAILED,
                               _("Read error on file `%s': %s\n"), 
                               filename,
                               g_strerror(errno));
      /* don't return, we want to go ahead and return any 
         addresses we already loaded. */
    }

  fclose(f);  

  return l;
}

char *
gconf_address_list_get_persistent_name (GSList *addresses)
{
  GSList  *tmp;
  GString *str = NULL;

  if (!addresses)
    {
      return g_strdup ("empty");
    }

  tmp = addresses;
  while (tmp != NULL)
    {
      const char *address = tmp->data;

      if (str == NULL)
	{
	  str = g_string_new (address);
	}
      else
        {
          g_string_append_c (str, GCONF_DATABASE_LIST_DELIM);
          g_string_append (str, address);
        }

      tmp = tmp->next;
    }

  return g_string_free (str, FALSE);
}

GSList *
gconf_persistent_name_get_address_list (const char *persistent_name)
{
  char   delim [2] = { GCONF_DATABASE_LIST_DELIM, '\0' };
  char **address_vector;

  address_vector = g_strsplit (persistent_name, delim, -1);
  if (address_vector != NULL)
    {
      GSList  *retval = NULL;
      int      i;

      i = 0;
      while (address_vector [i] != NULL)
        {
          retval = g_slist_append (retval, g_strdup (address_vector [i]));
          ++i;
        }

      g_strfreev (address_vector);

      return retval;
    }
  else
    {
      return g_slist_append (NULL, g_strdup (persistent_name));
    }
}

void
gconf_address_list_free (GSList *addresses)
{
  GSList *tmp;

  tmp = addresses;
  while (tmp != NULL)
    {
      g_free (tmp->data);
      tmp = tmp->next;
    }

  g_slist_free (addresses);
}

/* This should also support concatting filesystem dirs and keys, 
   or dir and subdir.
*/
gchar*        
gconf_concat_dir_and_key(const gchar* dir, const gchar* key)
{
  guint dirlen;
  guint keylen;
  gchar* retval;

  g_return_val_if_fail(dir != NULL, NULL);
  g_return_val_if_fail(key != NULL, NULL);
  g_return_val_if_fail(*dir == '/', NULL);

  dirlen = strlen(dir);
  keylen = strlen(key);

  retval = g_malloc0(dirlen+keylen+3); /* auto-null-terminate */

  strcpy(retval, dir);

  if (dir[dirlen-1] == '/')
    {
      /* dir ends in slash, strip key slash if needed */
      if (*key == '/')
        ++key;

      strcpy((retval+dirlen), key);
    }
  else 
    {
      /* Dir doesn't end in slash, add slash if key lacks one. */
      gchar* dest = retval + dirlen;

      if (*key != '/')
        {
          *dest = '/';
          ++dest;
        }
      
      strcpy(dest, key);
    }
  
  return retval;
}

gulong
gconf_string_to_gulong(const gchar* str)
{
  gulong retval;
  gchar *end;
  errno = 0;
  retval = strtoul(str, &end, 10);
  if (end == str || errno != 0)
    retval = 0;

  return retval;
}

gboolean
gconf_string_to_double(const gchar* str,
                       gdouble*     retloc)
{
  char *end;

  errno = 0;
  *retloc = g_ascii_strtod (str, &end);
  if (end == str || errno != 0)
    {
      *retloc = 0.0;
      return FALSE;
    }
  else
    return TRUE;
}

gchar*
gconf_double_to_string (gdouble val)
{
  char str[G_ASCII_DTOSTR_BUF_SIZE];

  g_ascii_dtostr (str, G_ASCII_DTOSTR_BUF_SIZE, val);
  
  return g_strdup (str);
}

const gchar*
gconf_current_locale(void)
{
#ifdef HAVE_LC_MESSAGES
  return setlocale(LC_MESSAGES, NULL);
#else
  return setlocale(LC_CTYPE, NULL);
#endif
}

/*
 * Log
 */

void
gconf_log(GConfLogPriority pri, const gchar* fmt, ...)
{
  va_list args;
  GLogLevelFlags loglevel;

  if (!gconf_log_debug_messages && 
      pri == GCL_DEBUG)
    return;

  switch (pri)
    {
    case GCL_EMERG:
    case GCL_ALERT:
    case GCL_CRIT:
      loglevel = G_LOG_LEVEL_ERROR;
      break;
    case GCL_ERR:
      loglevel = G_LOG_LEVEL_CRITICAL;
      break;
    case GCL_WARNING:
      loglevel = G_LOG_LEVEL_WARNING;
      break;
    case GCL_NOTICE:
      loglevel = G_LOG_LEVEL_MESSAGE;
      break;
    case GCL_INFO:
      loglevel = G_LOG_LEVEL_INFO;
      break;
    case GCL_DEBUG:
      loglevel = G_LOG_LEVEL_DEBUG;
      break;
    default:
      g_assert_not_reached();
      break;
    }

  va_start (args, fmt);
  g_logv (G_LOG_DOMAIN, loglevel, fmt, args);
  va_end (args);
}

/*
 * List/pair conversion
 */

GConfValue*
gconf_value_list_from_primitive_list(GConfValueType list_type, GSList* list,
                                     GError **err)
{
  GSList* value_list;
  GSList* tmp;

  g_return_val_if_fail(list_type != GCONF_VALUE_INVALID, NULL);
  g_return_val_if_fail(list_type != GCONF_VALUE_LIST, NULL);
  g_return_val_if_fail(list_type != GCONF_VALUE_PAIR, NULL);
  
  value_list = NULL;

  tmp = list;

  while (tmp != NULL)
    {
      GConfValue* val;

      val = gconf_value_new(list_type);

      switch (list_type)
        {
        case GCONF_VALUE_INT:
          gconf_value_set_int(val, GPOINTER_TO_INT(tmp->data));
          break;

        case GCONF_VALUE_BOOL:
          gconf_value_set_bool(val, GPOINTER_TO_INT(tmp->data));
          break;

        case GCONF_VALUE_FLOAT:
          gconf_value_set_float(val, *((gdouble*)tmp->data));
          break;

        case GCONF_VALUE_STRING:
          if (!g_utf8_validate (tmp->data, -1, NULL))
            {
              g_set_error (err, GCONF_ERROR,
                           GCONF_ERROR_FAILED,
                           _("Text contains invalid UTF-8"));
              goto error;
            }
                     
          gconf_value_set_string(val, tmp->data);
          break;

        case GCONF_VALUE_SCHEMA:
          if (!gconf_schema_validate (tmp->data, err))
            goto error;
          gconf_value_set_schema(val, tmp->data);
          break;
          
        default:
          g_assert_not_reached();
          break;
        }

      value_list = g_slist_prepend(value_list, val);

      tmp = g_slist_next(tmp);
    }

  /* Get it in the right order. */
  value_list = g_slist_reverse(value_list);

  {
    GConfValue* value_with_list;
    
    value_with_list = gconf_value_new(GCONF_VALUE_LIST);
    gconf_value_set_list_type(value_with_list, list_type);
    gconf_value_set_list_nocopy(value_with_list, value_list);

    return value_with_list;
  }

 error:
  g_slist_foreach (value_list, (GFunc)gconf_value_free, NULL);
  g_slist_free (value_list);
  return NULL;
}


static GConfValue*
from_primitive(GConfValueType type, gconstpointer address,
               GError **err)
{
  GConfValue* val;

  val = gconf_value_new(type);

  switch (type)
    {
    case GCONF_VALUE_INT:
      gconf_value_set_int(val, *((const gint*)address));
      break;

    case GCONF_VALUE_BOOL:
      gconf_value_set_bool(val, *((const gboolean*)address));
      break;

    case GCONF_VALUE_STRING:
      if (!g_utf8_validate (*((const gchar**)address), -1, NULL))
        {
          g_set_error (err, GCONF_ERROR,
                       GCONF_ERROR_FAILED,
                       _("Text contains invalid UTF-8"));
          gconf_value_free (val);
          return NULL;
        }

      gconf_value_set_string(val, *((const gchar**)address));
      break;

    case GCONF_VALUE_FLOAT:
      gconf_value_set_float(val, *((const gdouble*)address));
      break;

    case GCONF_VALUE_SCHEMA:
      if (!gconf_schema_validate (*((GConfSchema**)address), err))
        {
          gconf_value_free (val);
          return NULL;
        }
      
      gconf_value_set_schema(val, *((GConfSchema**)address));
      break;
      
    default:
      g_assert_not_reached();
      break;
    }

  return val;
}

GConfValue*
gconf_value_pair_from_primitive_pair(GConfValueType car_type,
                                     GConfValueType cdr_type,
                                     gconstpointer address_of_car,
                                     gconstpointer address_of_cdr,
                                     GError       **err)
{
  GConfValue* pair;
  GConfValue* car;
  GConfValue* cdr;
  
  g_return_val_if_fail(car_type != GCONF_VALUE_INVALID, NULL);
  g_return_val_if_fail(car_type != GCONF_VALUE_LIST, NULL);
  g_return_val_if_fail(car_type != GCONF_VALUE_PAIR, NULL);
  g_return_val_if_fail(cdr_type != GCONF_VALUE_INVALID, NULL);
  g_return_val_if_fail(cdr_type != GCONF_VALUE_LIST, NULL);
  g_return_val_if_fail(cdr_type != GCONF_VALUE_PAIR, NULL);
  g_return_val_if_fail(address_of_car != NULL, NULL);
  g_return_val_if_fail(address_of_cdr != NULL, NULL);
  
  car = from_primitive(car_type, address_of_car, err);
  if (car == NULL)
    return NULL;
  cdr = from_primitive(cdr_type, address_of_cdr, err);
  if (cdr == NULL)
    {
      gconf_value_free (car);
      return NULL;
    }
  
  pair = gconf_value_new(GCONF_VALUE_PAIR);
  gconf_value_set_car_nocopy(pair, car);
  gconf_value_set_cdr_nocopy(pair, cdr);

  return pair;
}


GSList*
gconf_value_list_to_primitive_list_destructive(GConfValue* val,
                                               GConfValueType list_type,
                                               GError** err)
{
  GSList* retval;

  g_return_val_if_fail(val != NULL, NULL);
  g_return_val_if_fail(list_type != GCONF_VALUE_INVALID, NULL);
  g_return_val_if_fail(list_type != GCONF_VALUE_LIST, NULL);
  g_return_val_if_fail(list_type != GCONF_VALUE_PAIR, NULL);
  g_return_val_if_fail(err == NULL || *err == NULL, NULL);
  
  if (val->type != GCONF_VALUE_LIST)
    {
      if (err)
        *err = gconf_error_new(GCONF_ERROR_TYPE_MISMATCH,
                               _("Expected list, got %s"),
                               gconf_value_type_to_string(val->type));
      gconf_value_free(val);
      return NULL;
    }

  if (gconf_value_get_list_type(val) != list_type)
    {
      if (err)
        *err = gconf_error_new(GCONF_ERROR_TYPE_MISMATCH,
                               _("Expected list of %s, got list of %s"),
                               gconf_value_type_to_string(list_type),
                               gconf_value_type_to_string(gconf_value_get_list_type (val)));
      gconf_value_free(val);
      return NULL;
    }

  g_assert(gconf_value_get_list_type(val) == list_type);
      
  retval = gconf_value_steal_list (val);
      
  gconf_value_free (val);
  val = NULL;
      
  {
    /* map (typeChange, retval) */
    GSList* tmp;

    tmp = retval;

    while (tmp != NULL)
      {
        GConfValue* elem = tmp->data;

        g_assert(elem != NULL);
        g_assert(elem->type == list_type);
            
        switch (list_type)
          {
          case GCONF_VALUE_INT:
            tmp->data = GINT_TO_POINTER(gconf_value_get_int(elem));
            break;

          case GCONF_VALUE_BOOL:
            tmp->data = GINT_TO_POINTER(gconf_value_get_bool(elem));
            break;
                
          case GCONF_VALUE_FLOAT:
            {
              gdouble* d = g_new(gdouble, 1);
              *d = gconf_value_get_float(elem);
              tmp->data = d;
            }
            break;

          case GCONF_VALUE_STRING:
            tmp->data = gconf_value_steal_string (elem);
            break;

          case GCONF_VALUE_SCHEMA:
            tmp->data = gconf_value_steal_schema (elem);
            break;
                
          default:
            g_assert_not_reached();
            break;
          }

        /* Clean up the value */
        gconf_value_free(elem);
            
        tmp = g_slist_next(tmp);
      }
  } /* list conversion block */
      
  return retval;
}


static void
primitive_value(gpointer retloc, GConfValue* val)
{
  switch (val->type)
    {
    case GCONF_VALUE_INT:
      *((gint*)retloc) = gconf_value_get_int(val);
      break;

    case GCONF_VALUE_FLOAT:
      *((gdouble*)retloc) = gconf_value_get_float(val);
      break;

    case GCONF_VALUE_STRING:
      {
        *((gchar**)retloc) = gconf_value_steal_string (val);
      }
      break;

    case GCONF_VALUE_BOOL:
      *((gboolean*)retloc) = gconf_value_get_bool(val);
      break;

    case GCONF_VALUE_SCHEMA:
      *((GConfSchema**)retloc) = gconf_value_steal_schema(val);
      break;
      
    default:
      g_assert_not_reached();
      break;
    }
}

gboolean
gconf_value_pair_to_primitive_pair_destructive(GConfValue* val,
                                               GConfValueType car_type,
                                               GConfValueType cdr_type,
                                               gpointer car_retloc,
                                               gpointer cdr_retloc,
                                               GError** err)
{
  GConfValue* car;
  GConfValue* cdr;

  g_return_val_if_fail(val != NULL, FALSE);
  g_return_val_if_fail(car_type != GCONF_VALUE_INVALID, FALSE);
  g_return_val_if_fail(car_type != GCONF_VALUE_LIST, FALSE);
  g_return_val_if_fail(car_type != GCONF_VALUE_PAIR, FALSE);
  g_return_val_if_fail(cdr_type != GCONF_VALUE_INVALID, FALSE);
  g_return_val_if_fail(cdr_type != GCONF_VALUE_LIST, FALSE);
  g_return_val_if_fail(cdr_type != GCONF_VALUE_PAIR, FALSE);
  g_return_val_if_fail(car_retloc != NULL, FALSE);
  g_return_val_if_fail(cdr_retloc != NULL, FALSE);
  g_return_val_if_fail(err == NULL || *err == NULL, FALSE);  
      
  if (val->type != GCONF_VALUE_PAIR)
    {
      if (err)
        *err = gconf_error_new(GCONF_ERROR_TYPE_MISMATCH,
                               _("Expected pair, got %s"),
                               gconf_value_type_to_string(val->type));
      gconf_value_free(val);
      return FALSE;
    }

  car = gconf_value_get_car(val);
  cdr = gconf_value_get_cdr(val);
      
  if (car == NULL ||
      cdr == NULL)
    {
      if (err)
        *err = gconf_error_new(GCONF_ERROR_TYPE_MISMATCH, 
                               _("Expected (%s,%s) pair, got a pair with one or both values missing"),
                               gconf_value_type_to_string(car_type),
                               gconf_value_type_to_string(cdr_type));

      gconf_value_free(val);
      return FALSE;
    }

  g_assert(car != NULL);
  g_assert(cdr != NULL);
      
  if (car->type != car_type ||
      cdr->type != cdr_type)
    {
      if (err)
        *err = gconf_error_new(GCONF_ERROR_TYPE_MISMATCH,
                               _("Expected pair of type (%s,%s) got type (%s,%s)"),
                               gconf_value_type_to_string(car_type),
                               gconf_value_type_to_string(cdr_type),
                               gconf_value_type_to_string(car->type),
                               gconf_value_type_to_string(cdr->type));
      gconf_value_free(val);
      return FALSE;
    }

  primitive_value(car_retloc, car);
  primitive_value(cdr_retloc, cdr);

  gconf_value_free(val);

  return TRUE;
}



/*
 * Encode/decode
 */

gchar*
gconf_quote_string   (const gchar* src)
{
  gchar* dest;
  const gchar* s;
  gchar* d;

  g_return_val_if_fail(src != NULL, NULL);
  
  /* waste memory! woo-hoo! */
  dest = g_malloc0(strlen(src)*2+4);
  
  d = dest;

  *d = '"';
  ++d;
  
  s = src;
  while (*s)
    {
      switch (*s)
        {
        case '"':
          {
            *d = '\\';
            ++d;
            *d = '"';
            ++d;
          }
          break;
          
        case '\\':
          {
            *d = '\\';
            ++d;
            *d = '\\';
            ++d;
          }
          break;
          
        default:
          {
            *d = *s;
            ++d;
          }
          break;
        }
      ++s;
    }

  /* End with quote mark and NULL */
  *d = '"';
  ++d;
  *d = '\0';
  
  return dest;
}

gchar*
gconf_unquote_string (const gchar* str, const gchar** end, GError** err)
{
  gchar* unq;
  gchar* unq_end = NULL;

  g_return_val_if_fail(end != NULL, NULL);
  g_return_val_if_fail(err == NULL || *err == NULL, NULL);
  g_return_val_if_fail(str != NULL, NULL);
  
  unq = g_strdup(str);

  gconf_unquote_string_inplace(unq, &unq_end, err);

  *end = (str + (unq_end - unq));

  return unq;
}

void
gconf_unquote_string_inplace (gchar* str, gchar** end, GError** err)
{
  gchar* dest;
  gchar* s;

  g_return_if_fail(end != NULL);
  g_return_if_fail(err == NULL || *err == NULL);
  g_return_if_fail(str != NULL);
  
  dest = s = str;

  if (*s != '"')
    {
      if (err)
        *err = gconf_error_new(GCONF_ERROR_PARSE_ERROR,
                               _("Quoted string doesn't begin with a quotation mark"));
      *end = str;
      return;
    }

  /* Skip the initial quote mark */
  ++s;
  
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
          return;

        case '\\':
          /* Possible escaped quote or \ */
          ++s;
          if (*s == '"')
            {
              *dest = *s;
              ++s;
              ++dest;
            }
          else if (*s == '\\')
            {
              *dest = *s;
              ++s;
              ++dest;
            }
          else
            {
              /* not an escaped char */
              *dest = '\\';
              ++dest;
              /* ++s already done. */
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
  
  /* If we reach here this means the close quote was never encountered */

  *dest = '\0';
  
  if (err)
    *err = gconf_error_new(GCONF_ERROR_PARSE_ERROR,
                           _("Quoted string doesn't end with a quotation mark"));
  *end = s;
  return;
}

/* The encoding format

   The first byte of the encoded string is the type of the value:

    i  int
    b  bool
    f  float
    s  string
    c  schema
    p  pair
    l  list
    v  invalid

    For int, the rest of the encoded value is the integer to be parsed with atoi()
    For bool, the rest is 't' or 'f'
    For float, the rest is a float to parse with g_strtod()
    For string, the rest is the string (not quoted)
    For schema, the encoding is complicated; see below.
    For pair, the rest is two primitive encodings (ibcfs), quoted, separated by a comma,
              car before cdr
    For list, first character is type, the rest is primitive encodings, quoted,
              separated by commas

    Schema:

    After the 'c' indicating schema, the second character is a byte indicating
    the type the schema expects. Then a comma, and the quoted locale, or "" for none.
    comma, and quoted short description; comma, quoted long description; comma, default
    value in the encoded format given above, quoted.
*/

static gchar type_byte(GConfValueType type)
{
  switch (type)
    {
    case GCONF_VALUE_INT:
      return 'i';
        
    case GCONF_VALUE_BOOL:
      return 'b';

    case GCONF_VALUE_FLOAT:
      return 'f';

    case GCONF_VALUE_STRING:
      return 's';

    case GCONF_VALUE_SCHEMA:
      return 'c';

    case GCONF_VALUE_LIST:
      return 'l';

    case GCONF_VALUE_PAIR:
      return 'p';

    case GCONF_VALUE_INVALID:
      return 'v';
      
    default:
      g_assert_not_reached();
      return '\0';
    }
}

static GConfValueType
byte_type(gchar byte)
{
  switch (byte)
    {
    case 'i':
      return GCONF_VALUE_INT;

    case 'b':
      return GCONF_VALUE_BOOL;

    case 's':
      return GCONF_VALUE_STRING;

    case 'c':
      return GCONF_VALUE_SCHEMA;

    case 'f':
      return GCONF_VALUE_FLOAT;

    case 'l':
      return GCONF_VALUE_LIST;

    case 'p':
      return GCONF_VALUE_PAIR;
      
    case 'v':
      return GCONF_VALUE_INVALID;

    default:
      return GCONF_VALUE_INVALID;
    }
}

GConfValue*
gconf_value_decode (const gchar* encoded)
{
  GConfValueType type;
  GConfValue* val;
  const gchar* s;
  
  type = byte_type(*encoded);

  if (type == GCONF_VALUE_INVALID)
    return NULL;

  if (!g_utf8_validate (encoded, -1, NULL))
    {
      gconf_log (GCL_ERR, _("Encoded value is not valid UTF-8"));
      return NULL;
    }
  
  val = gconf_value_new(type);

  s = encoded + 1;
  
  switch (val->type)
    {
    case GCONF_VALUE_INT:
      gconf_value_set_int(val, atoi(s));
      break;
        
    case GCONF_VALUE_BOOL:
      gconf_value_set_bool(val, *s == 't' ? TRUE : FALSE);
      break;

    case GCONF_VALUE_FLOAT:
      {
        double d;
        gchar* endptr = NULL;
        
        d = g_strtod(s, &endptr);
        if (endptr == s)
          g_warning("Failure converting string to double in %s", G_STRFUNC);
        gconf_value_set_float(val, d);
      }
      break;

    case GCONF_VALUE_STRING:
      {
        gconf_value_set_string(val, s);
      }
      break;

    case GCONF_VALUE_SCHEMA:
      {
        GConfSchema* sc = gconf_schema_new();
        const gchar* end = NULL;
        gchar* unquoted;
        
        gconf_value_set_schema_nocopy(val, sc);

        gconf_schema_set_type(sc, byte_type(*s));
        ++s;
        gconf_schema_set_list_type(sc, byte_type(*s));
        ++s;
        gconf_schema_set_car_type(sc, byte_type(*s));
        ++s;
        gconf_schema_set_cdr_type(sc, byte_type(*s));
        ++s;

        if (*s != ',')
          g_warning("no comma after types in schema");

	++s;

        /* locale */
        unquoted = gconf_unquote_string(s, &end, NULL);

        gconf_schema_set_locale(sc, unquoted);

        g_free(unquoted);
        
        if (*end != ',')
          g_warning("no comma after locale in schema");

        ++end;
        s = end;

        /* short */
        unquoted = gconf_unquote_string(s, &end, NULL);

        gconf_schema_set_short_desc(sc, unquoted);

        g_free(unquoted);
        
        if (*end != ',')
          g_warning("no comma after short desc in schema");

        ++end;
        s = end;

        /* long */
        unquoted = gconf_unquote_string(s, &end, NULL);

        gconf_schema_set_long_desc(sc, unquoted);

        g_free(unquoted);
        
        if (*end != ',')
          g_warning("no comma after long desc in schema");

        ++end;
        s = end;
        
        /* default value */
        unquoted = gconf_unquote_string(s, &end, NULL);

        gconf_schema_set_default_value_nocopy(sc, gconf_value_decode(unquoted));

        g_free(unquoted);
        
        if (*end != '\0')
          g_warning("trailing junk after encoded schema");
      }
      break;

    case GCONF_VALUE_LIST:
      {
        GSList* value_list = NULL;

        gconf_value_set_list_type(val, byte_type(*s));
	++s;

        while (*s)
          {
            gchar* unquoted;
            const gchar* end;
            
            GConfValue* elem;
            
            unquoted = gconf_unquote_string(s, &end, NULL);            

            elem = gconf_value_decode(unquoted);

            g_free(unquoted);
            
            if (elem)
              value_list = g_slist_prepend(value_list, elem);
            
            s = end;
            if (*s == ',')
              ++s;
            else if (*s != '\0')
              {
                g_warning("weird character in encoded list");
                break; /* error */
              }
          }

        value_list = g_slist_reverse(value_list);

        gconf_value_set_list_nocopy(val, value_list);
      }
      break;

    case GCONF_VALUE_PAIR:
      {
        gchar* unquoted;
        const gchar* end;
        
        GConfValue* car;
        GConfValue* cdr;
        
        unquoted = gconf_unquote_string(s, &end, NULL);            
        
        car = gconf_value_decode(unquoted);

        g_free(unquoted);
        
        s = end;
        if (*s == ',')
          ++s;
        else
          {
            g_warning("weird character in encoded pair");
          }
        
        unquoted = gconf_unquote_string(s, &end, NULL);
        
        cdr = gconf_value_decode(unquoted);
        g_free(unquoted);


        gconf_value_set_car_nocopy(val, car);
        gconf_value_set_cdr_nocopy(val, cdr);
      }
      break;

    default:
      g_assert_not_reached();
      break;
    }

  return val;
}

gchar*
gconf_value_encode (GConfValue* val)
{
  gchar* retval = NULL;
  
  g_return_val_if_fail(val != NULL, NULL);

  switch (val->type)
    {
    case GCONF_VALUE_INT:
      retval = g_strdup_printf("i%d", gconf_value_get_int(val));
      break;
        
    case GCONF_VALUE_BOOL:
      retval = g_strdup_printf("b%c", gconf_value_get_bool(val) ? 't' : 'f');
      break;

    case GCONF_VALUE_FLOAT:
      retval = g_strdup_printf("f%g", gconf_value_get_float(val));
      break;

    case GCONF_VALUE_STRING:
      retval = g_strdup_printf("s%s", gconf_value_get_string(val));
      break;

    case GCONF_VALUE_SCHEMA:
      {
        gchar* tmp;
        gchar* quoted;
        gchar* encoded;
        GConfSchema* sc;

        sc = gconf_value_get_schema(val);
        
        tmp = g_strdup_printf("c%c%c%c%c,",
			      type_byte(gconf_schema_get_type(sc)),
			      type_byte(gconf_schema_get_list_type(sc)),
			      type_byte(gconf_schema_get_car_type(sc)),
			      type_byte(gconf_schema_get_cdr_type(sc)));

        quoted = gconf_quote_string(gconf_schema_get_locale(sc) ?
                                    gconf_schema_get_locale(sc) : "");
        retval = g_strconcat(tmp, quoted, ",", NULL);

        g_free(tmp);
        g_free(quoted);

        tmp = retval;
        quoted = gconf_quote_string(gconf_schema_get_short_desc(sc) ?
                                    gconf_schema_get_short_desc(sc) : "");

        retval = g_strconcat(tmp, quoted, ",", NULL);

        g_free(tmp);
        g_free(quoted);


        tmp = retval;
        quoted = gconf_quote_string(gconf_schema_get_long_desc(sc) ?
                                    gconf_schema_get_long_desc(sc) : "");

        retval = g_strconcat(tmp, quoted, ",", NULL);

        g_free(tmp);
        g_free(quoted);
        

        if (gconf_schema_get_default_value(sc) != NULL)
          encoded = gconf_value_encode(gconf_schema_get_default_value(sc));
        else
          encoded = g_strdup("");

        tmp = retval;
          
        quoted = gconf_quote_string(encoded);

        retval = g_strconcat(tmp, quoted, NULL);

        g_free(tmp);
        g_free(quoted);
        g_free(encoded);
      }
      break;

    case GCONF_VALUE_LIST:
      {
        GSList* tmp;

        retval = g_strdup_printf("l%c", type_byte(gconf_value_get_list_type(val)));
        
        tmp = gconf_value_get_list(val);

        while (tmp != NULL)
          {
            GConfValue* elem = tmp->data;
            gchar* encoded;
            gchar* quoted;
            
            g_assert(elem != NULL);

            encoded = gconf_value_encode(elem);

            quoted = gconf_quote_string(encoded);

            g_free(encoded);

            {
              gchar* free_me;
              free_me = retval;
              
              retval = g_strconcat(retval, ",", quoted, NULL);
              
              g_free(quoted);
              g_free(free_me);
            }
            
            tmp = g_slist_next(tmp);
          }
      }
      break;

    case GCONF_VALUE_PAIR:
      {
        gchar* car_encoded;
        gchar* cdr_encoded;
        gchar* car_quoted;
        gchar* cdr_quoted;

        car_encoded = gconf_value_encode(gconf_value_get_car(val));
        cdr_encoded = gconf_value_encode(gconf_value_get_cdr(val));

        car_quoted = gconf_quote_string(car_encoded);
        cdr_quoted = gconf_quote_string(cdr_encoded);

        retval = g_strconcat("p", car_quoted, ",", cdr_quoted, NULL);

        g_free(car_encoded);
        g_free(cdr_encoded);
        g_free(car_quoted);
        g_free(cdr_quoted);
      }
      break;

    default:
      g_assert_not_reached();
      break;
      
    }

  return retval;
}

#ifdef HAVE_CORBA

/*
 * Locks
 */

/*
 * Locks works as follows. We have a lock directory to hold the locking
 * mess, and we have an IOR file inside the lock directory with the
 * gconfd IOR, and we have an fcntl() lock on the IOR file. The IOR
 * file is created atomically using a temporary file, then link()
 */

struct _GConfLock {
  gchar *lock_directory;
  gchar *iorfile;
  int    lock_fd;
};

static void
gconf_lock_destroy (GConfLock* lock)
{
  if (lock->lock_fd >= 0)
    close (lock->lock_fd);
  g_free (lock->iorfile);
  g_free (lock->lock_directory);
  g_free (lock);
}

#ifndef G_OS_WIN32

static void
set_close_on_exec (int fd)
{
#if defined (F_GETFD) && defined (FD_CLOEXEC)
  int val;

  val = fcntl (fd, F_GETFD, 0);
  if (val < 0)
    {
      gconf_log (GCL_DEBUG, "couldn't F_GETFD: %s\n", g_strerror (errno));
      return;
    }

  val |= FD_CLOEXEC;

  if (fcntl (fd, F_SETFD, val) < 0)
    gconf_log (GCL_DEBUG, "couldn't F_SETFD: %s\n", g_strerror (errno));
#endif
}

#endif

#ifdef F_SETLK
/* Your basic Stevens cut-and-paste */
static int
lock_reg (int fd, int cmd, int type, off_t offset, int whence, off_t len)
{
  struct flock lock;

  lock.l_type = type; /* F_RDLCK, F_WRLCK, F_UNLCK */
  lock.l_start = offset; /* byte offset relative to whence */
  lock.l_whence = whence; /* SEEK_SET, SEEK_CUR, SEEK_END */
  lock.l_len = len; /* #bytes, 0 for eof */

  return fcntl (fd, cmd, &lock);
}
#endif

#ifdef F_SETLK
#define lock_entire_file(fd) \
  lock_reg ((fd), F_SETLK, F_WRLCK, 0, SEEK_SET, 0)
#define unlock_entire_file(fd) \
  lock_reg ((fd), F_SETLK, F_UNLCK, 0, SEEK_SET, 0)
#elif defined (G_OS_WIN32)
/* We don't use these macros */
#else
#warning Please implement proper locking
#define lock_entire_file(fd) 0
#define unlock_entire_file(fd) 0
#endif

static gboolean
file_locked_by_someone_else (int fd)
{
#ifdef F_SETLK
  struct flock lock;

  lock.l_type = F_WRLCK;
  lock.l_start = 0;
  lock.l_whence = SEEK_SET;
  lock.l_len = 0;

  if (fcntl (fd, F_GETLK, &lock) < 0)
    return TRUE; /* pretend it's locked */

  if (lock.l_type == F_UNLCK)
    return FALSE; /* we have the lock */
  else
    return TRUE; /* someone else has it */
#else
  return FALSE;
#endif
}

#ifndef G_OS_WIN32

static char*
unique_filename (const char *directory)
{
  char *guid;
  char *uniquefile;
  
  guid = gconf_unique_key ();
  uniquefile = g_strconcat (directory, "/", guid, NULL);
  g_free (guid);

  return uniquefile;
}

#endif

static int
create_new_locked_file (const gchar *directory,
                        const gchar *filename,
                        GError     **err)
{
  int fd;
  gboolean got_lock = FALSE;

#ifndef G_OS_WIN32

  char *uniquefile = unique_filename (directory);

  fd = open (uniquefile, O_WRONLY | O_CREAT, 0700);

  /* Lock our temporary file, lock hopefully applies to the
   * inode and so also counts once we link it to the new name
   */
  if (lock_entire_file (fd) < 0)
    {
      g_set_error (err,
                   GCONF_ERROR,
                   GCONF_ERROR_LOCK_FAILED,
                   _("Could not lock temporary file '%s': %s"),
                   uniquefile, g_strerror (errno));
      goto out;
    }
  
  /* Create lockfile as a link to unique file */
  if (link (uniquefile, filename) == 0)
    {
      /* filename didn't exist before, and open succeeded, and we have the lock */
      got_lock = TRUE;
      goto out;
    }
  else
    {
      /* see if the link really succeeded */
      struct stat sb;
      if (stat (uniquefile, &sb) == 0 &&
          sb.st_nlink == 2)
        {
          got_lock = TRUE;
          goto out;
        }
      else
        {
          g_set_error (err,
                       GCONF_ERROR,
                       GCONF_ERROR_LOCK_FAILED,
                       _("Could not create file '%s', probably because it already exists"),
                       filename);
          goto out;
        }
    }
  
 out:
  if (got_lock)
    set_close_on_exec (fd);
  
  unlink (uniquefile);
  g_free (uniquefile);

#else

  if (G_WIN32_HAVE_WIDECHAR_API ())
    {
      wchar_t *wfilename = g_utf8_to_utf16 (filename, -1, NULL, NULL, NULL);
      fd = _wsopen (wfilename, O_WRONLY|O_CREAT|O_EXCL, SH_DENYWR, 0700);
      g_free (wfilename);
    }
  else
    {
      char *cpfilename = g_locale_from_utf8 (filename, -1, NULL, NULL, NULL);
      fd = _sopen (cpfilename, O_WRONLY|O_CREAT|O_EXCL, SH_DENYWR, 0700);
      g_free (cpfilename);
    }

  got_lock = (fd >= 0);

#endif

  if (!got_lock)
    {
      if (fd >= 0)
        close (fd);
      fd = -1;
    }
  
  return fd;
}

static int
open_empty_locked_file (const gchar *directory,
                        const gchar *filename,
                        GError     **err)
{
  int fd;

  fd = create_new_locked_file (directory, filename, NULL);

  if (fd >= 0)
    return fd;
  
  /* We failed to create the file, most likely because it already
   * existed; try to get the lock on the existing file, and if we can
   * get that lock, delete it, then start over.
   */

#ifndef G_OS_WIN32

  fd = open (filename, O_RDWR, 0700);
  if (fd < 0)
    {
      /* File has gone away? */
      g_set_error (err,
                   GCONF_ERROR,
                   GCONF_ERROR_LOCK_FAILED,
                   _("Failed to create or open '%s'"),
                   filename);
      return -1;
    }

  if (lock_entire_file (fd) < 0)
    {
      g_set_error (err,
                   GCONF_ERROR,
                   GCONF_ERROR_LOCK_FAILED,
                   _("Failed to lock '%s': probably another process has the lock, or your operating system has NFS file locking misconfigured (%s)"),
                   filename, g_strerror (errno));
      close (fd);
      return -1;
    }

  /* We have the lock on filename, so delete it */
  /* FIXME this leaves .nfs32423432 cruft */
  unlink (filename);
  close (fd);
  fd = -1;

#else
  /* Unlinking open files fail on Win32 */

  if (g_remove (filename) == -1)
    {
      g_set_error (err,
		   GCONF_ERROR,
		   GCONF_ERROR_LOCK_FAILED,
		   _("Failed to remove '%s': %s"),
		   filename, g_strerror (errno));
      return -1;
    }
#endif

  /* Now retry creating our file */
  fd = create_new_locked_file (directory, filename, err);
  
  return fd;
}

static char *
get_ior (gboolean start_if_not_found,
         GString  *failure_log)
{
        GDBusConnection *connection;
        GVariant *value;
        char *ior;
        GError *error = NULL;

        /* if the bus isn't running and we don't want to start gconfd then
         * we don't want to autolaunch the bus either, so bail early.
         */
        if (g_getenv ("DBUS_SESSION_BUS_ADDRESS") == NULL &&
           (!start_if_not_found || g_getenv ("DISPLAY") == NULL)) {
                if (failure_log)
                    g_string_append_printf (failure_log,
                                            _("Not running within active session"));
                return NULL;
        }

        g_type_init ();

        connection = g_bus_get_sync (G_BUS_TYPE_SESSION, NULL, &error);
        if (connection == NULL) {
                if (failure_log)
                    g_string_append_printf (failure_log,
                                            _("Failed to get connection to session: %s"),
                                            error->message);
                g_error_free (error);
                return NULL;
        }

        value = g_dbus_connection_call_sync (connection,
                                             "org.gnome.GConf",
                                             "/org/gnome/GConf",
                                             "org.gnome.GConf",
                                             "GetIOR",
                                             g_variant_new ("()"),
                                             G_VARIANT_TYPE ("(s)"),
                                             start_if_not_found ? G_DBUS_CALL_FLAGS_NONE 
                                                                : G_DBUS_CALL_FLAGS_NO_AUTO_START,
                                             -1,
                                             NULL,
                                             &error);
        g_object_unref (connection);

        if (value == NULL) {
                if (failure_log)
                    g_string_append_printf (failure_log,
                                            _("GetIOR failed: %s"),
                                            error->message);

                g_error_free (error);
                return NULL;
        }

        g_variant_get (value, "(s)", &ior, NULL);
        g_variant_unref (value);

        return ior;
}

static ConfigServer
gconf_get_server (gboolean  start_if_not_found,
                  GString  *failure_log)
{
  ConfigServer server;
  char *ior;
  CORBA_ORB orb;
  CORBA_Environment ev;

  ior = get_ior (start_if_not_found, failure_log);

  if (ior == NULL)
    {
      return CORBA_OBJECT_NIL;
    }

  CORBA_exception_init (&ev);
  orb = gconf_orb_get ();

  if (orb == NULL)
    {
      if (failure_log)
        {
          g_string_append_printf (failure_log,
                                _("couldn't contact ORB to resolve existing gconfd object reference"));
          g_free (ior);
        }
      return CORBA_OBJECT_NIL;
    }

  server = CORBA_ORB_string_to_object (orb, (char*) ior, &ev);
  CORBA_exception_free (&ev);

  if (server == CORBA_OBJECT_NIL &&
      failure_log)
    g_string_append_printf (failure_log,
                            _("Failed to convert IOR '%s' to an object reference"),
                            ior);

  g_free (ior);

  return server;
}

static GConfLock *
gconf_get_lock_or_current_holder (const gchar  *lock_directory,
                                  ConfigServer *current_server,
                                  GError      **err)
{
  GConfLock* lock;
  
  g_return_val_if_fail(lock_directory != NULL, NULL);

  if (current_server)
    *current_server = CORBA_OBJECT_NIL;
  
  if (g_mkdir (lock_directory, 0700) < 0 &&
      errno != EEXIST)
    {
      int errsv = errno;

      gconf_set_error (err,
                       GCONF_ERROR_LOCK_FAILED,
                       _("couldn't create directory `%s': %s"),
                       lock_directory, g_strerror (errsv));

      return NULL;
    }

  lock = g_new0 (GConfLock, 1);

  lock->lock_directory = g_strdup (lock_directory);

  lock->iorfile = g_strconcat (lock->lock_directory, "/ior", NULL);

  /* Check the current IOR file and ping its daemon */
  
  lock->lock_fd = open_empty_locked_file (lock->lock_directory,
                                          lock->iorfile,
                                          err);
  
  if (lock->lock_fd < 0)
    {
      /* We didn't get the lock. Read the old server, and provide
       * it to the caller. Error is already set.
       */
      if (current_server)
        *current_server = gconf_get_server (FALSE, NULL);

      gconf_lock_destroy (lock);
      
      return NULL;
    }
  else
    {
      /* Write IOR to lockfile */
      const gchar* ior;
      int retval;
      gchar* s;
      
      s = g_strdup_printf ("%u:", (guint) getpid ());
        
      retval = write (lock->lock_fd, s, strlen (s));

      g_free (s);
        
      if (retval >= 0)
        {
          ior = gconf_get_daemon_ior();
            
          if (ior == NULL)
            retval = write (lock->lock_fd, "none", 4);
          else
            retval = write (lock->lock_fd, ior, strlen (ior));
        }

      if (retval < 0)
        {
          gconf_set_error (err,
                           GCONF_ERROR_LOCK_FAILED,
                           _("Can't write to file `%s': %s"),
                           lock->iorfile, g_strerror (errno));

          g_unlink (lock->iorfile);
          gconf_lock_destroy (lock);

          return NULL;
        }
    }

  return lock;
}

GConfLock*
gconf_get_lock (const gchar *lock_directory,
                GError     **err)
{
  return gconf_get_lock_or_current_holder (lock_directory, NULL, err);
}

gboolean
gconf_release_lock (GConfLock *lock,
                    GError   **err)
{
  gboolean retval;
  char *uniquefile;
  
  retval = FALSE;
  uniquefile = NULL;
  
  /* A paranoia check to avoid disaster if e.g.
   * some random client code opened and closed the
   * lockfile (maybe Nautilus checking its MIME type or
   * something)
   */
  if (lock->lock_fd < 0 ||
      file_locked_by_someone_else (lock->lock_fd))
    {
      g_set_error (err,
                   GCONF_ERROR,
                   GCONF_ERROR_FAILED,
                   _("We didn't have the lock on file `%s', but we should have"),
                   lock->iorfile);
      goto out;
    }

#ifndef G_OS_WIN32

  /* To avoid annoying .nfs3435314513453145 files on unlink, which keep us
   * from removing the lock directory, we don't want to hold the
   * lockfile open after removing all links to it. But we can't
   * close it then unlink, because then we would be unlinking without
   * holding the lock. So, we create a unique filename and link it too
   * the locked file, then unlink the locked file, then drop our locks
   * and close file descriptors, then unlink the unique filename
   */
  
  uniquefile = unique_filename (lock->lock_directory);

  if (link (lock->iorfile, uniquefile) < 0)
    {
      g_set_error (err,
                   GCONF_ERROR,
                   GCONF_ERROR_FAILED,
                   _("Failed to link '%s' to '%s': %s"),
                   uniquefile, lock->iorfile, g_strerror (errno));

      goto out;
    }
  
  /* Note that we unlink while still holding the lock to avoid races */
  if (unlink (lock->iorfile) < 0)
    {
      g_set_error (err,
                   GCONF_ERROR,
                   GCONF_ERROR_FAILED,
                   _("Failed to remove lock file `%s': %s"),
                   lock->iorfile,
                   g_strerror (errno));
      goto out;
    }

#endif

  /* Now drop our lock */
  if (lock->lock_fd >= 0)
    {
      close (lock->lock_fd);
      lock->lock_fd = -1;
    }

#ifndef G_OS_WIN32

  /* Now remove the temporary link we used to avoid .nfs351453 garbage */
  if (unlink (uniquefile) < 0)
    {
      g_set_error (err,
                   GCONF_ERROR,
                   GCONF_ERROR_FAILED,
                   _("Failed to clean up file '%s': %s"),
                   uniquefile, g_strerror (errno));

      goto out;
    }

#endif

  /* And finally clean up the directory - this would have failed if
   * we had .nfs323423423 junk
   */
  if (g_rmdir (lock->lock_directory) < 0)
    {
      g_set_error (err,
                   GCONF_ERROR,
                   GCONF_ERROR_FAILED,
                   _("Failed to remove lock directory `%s': %s"),
                   lock->lock_directory,
                   g_strerror (errno));
      goto out;
    }

  retval = TRUE;
  
 out:

  g_free (uniquefile);
  gconf_lock_destroy (lock);
  return retval;
}

static CORBA_ORB gconf_orb = CORBA_OBJECT_NIL;      

CORBA_ORB
gconf_orb_get (void)
{
  if (gconf_orb == CORBA_OBJECT_NIL)
    {
      CORBA_Environment ev;
      int argc = 1;
      char *argv[] = { "gconf", NULL };

      _gconf_init_i18n ();
      
      CORBA_exception_init (&ev);
      
      gconf_orb = CORBA_ORB_init (&argc, argv, "orbit-local-orb", &ev);
      g_assert (ev._major == CORBA_NO_EXCEPTION);

      CORBA_exception_free (&ev);
    }

  return gconf_orb;
}

int
gconf_orb_release (void)
{
  int ret = 0;

  if (gconf_orb != CORBA_OBJECT_NIL)
    {
      CORBA_ORB orb = gconf_orb;
      CORBA_Environment ev;

      gconf_orb = CORBA_OBJECT_NIL;

      CORBA_exception_init (&ev);

      CORBA_ORB_destroy (orb, &ev);
      CORBA_Object_release ((CORBA_Object)orb, &ev);

      if (ev._major != CORBA_NO_EXCEPTION)
        {
          ret = 1;
        }
      CORBA_exception_free (&ev);
    }

  return ret;
}

char*
gconf_get_daemon_dir (void)
{  
  if (gconf_use_local_locks ())
    {
      char *s;
      char *subdir;
      const char *tmpdir;

      subdir = g_strconcat ("gconfd-", g_get_user_name (), NULL);
      
      if (g_getenv ("GCONF_TMPDIR")) {
	tmpdir = g_getenv ("GCONF_TMPDIR");
      } else {
	tmpdir = g_get_tmp_dir ();
      }
      
      s = g_build_filename (tmpdir, subdir, NULL);

      g_free (subdir);

      return s;
    }
  else
    {
#ifndef G_OS_WIN32
      const char *home = g_get_home_dir ();
#else
      const char *home = _gconf_win32_get_home_dir ();
#endif
      return g_strconcat (home, "/.gconfd", NULL);
    }
}

char*
gconf_get_lock_dir (void)
{
  char *gconfd_dir;
  char *lock_dir;
  
  gconfd_dir = gconf_get_daemon_dir ();
  lock_dir = g_strconcat (gconfd_dir, "/lock", NULL);

  g_free (gconfd_dir);
  return lock_dir;
}

ConfigServer
gconf_activate_server (gboolean  start_if_not_found,
                       GError  **error)
{
  ConfigServer server = CORBA_OBJECT_NIL;
  GString *failure_log;
  CORBA_Environment ev;

  failure_log = g_string_new (NULL);
  
  g_string_append (failure_log, " 1: ");
  server = gconf_get_server (start_if_not_found, failure_log);

  /* Confirm server exists */
  CORBA_exception_init (&ev);

  if (!CORBA_Object_is_nil (server, &ev))
    {
      ConfigServer_ping (server, &ev);
  
      if (ev._major != CORBA_NO_EXCEPTION)
        {
          server = CORBA_OBJECT_NIL;

          g_string_append_printf (failure_log,
                                  _("Server ping error: %s"),
                                  CORBA_exception_id (&ev));
        }
    }

  CORBA_exception_free (&ev);

  if (server != CORBA_OBJECT_NIL)
    {
      g_string_free (failure_log, TRUE);
      return server;
    }

  if (server == CORBA_OBJECT_NIL &&
      error &&
      *error == NULL)
    g_set_error (error,
                 GCONF_ERROR,
                 GCONF_ERROR_NO_SERVER,
                 _("Failed to contact configuration server; the most common cause is a missing or misconfigured D-Bus session bus daemon. See http://projects.gnome.org/gconf/ for information. (Details - %s)"),
                 failure_log->len > 0 ? failure_log->str : _("none"));

  g_string_free (failure_log, TRUE);
  
  return server;
}

gboolean
gconf_CORBA_Object_equal (gconstpointer a, gconstpointer b)
{
  CORBA_Environment ev;
  CORBA_Object _obj_a = (gpointer)a;
  CORBA_Object _obj_b = (gpointer)b;
  gboolean retval;

  CORBA_exception_init (&ev);
  retval = CORBA_Object_is_equivalent(_obj_a, _obj_b, &ev);
  CORBA_exception_free (&ev);

  return retval;
}

guint
gconf_CORBA_Object_hash (gconstpointer key)
{
  CORBA_Environment ev;
  CORBA_Object _obj = (gpointer)key;
  CORBA_unsigned_long retval;

  CORBA_exception_init (&ev);
  retval = CORBA_Object_hash(_obj, G_MAXUINT, &ev);
  CORBA_exception_free (&ev);

  return retval;
}

#endif /* HAVE_CORBA */

void
_gconf_init_i18n (void)
{
  static gboolean done = FALSE;

  if (!done)
    {
      bindtextdomain (GETTEXT_PACKAGE, GCONF_LOCALE_DIR);
#ifdef HAVE_BIND_TEXTDOMAIN_CODESET
      bind_textdomain_codeset (GETTEXT_PACKAGE, "UTF-8");
#endif
      done = TRUE;
    }
}

enum { UNKNOWN, LOCAL, NORMAL };

gboolean
gconf_use_local_locks (void)
{
  static int local_locks = UNKNOWN;
  
  if (local_locks == UNKNOWN)
    {
      const char *l =
        g_getenv ("GCONF_GLOBAL_LOCKS");

      if (l && atoi (l) == 1)
        local_locks = NORMAL;
      else
        local_locks = LOCAL;
    }

  return local_locks == LOCAL;
}
