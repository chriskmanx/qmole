
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
#include "gconf-backend.h"
#include "gconf-sources.h"
#include "gconf-internals.h"
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

static const char * get_address_resource (const char *address);

/* 
 *  Sources
 */

GConfSource* 
gconf_resolve_address(const gchar* address, GError** err)
{
  GConfBackend* backend;

  backend = gconf_get_backend(address, err);

  if (backend == NULL)
    return NULL;
  else
    {
      GConfSource* retval;

      retval = gconf_backend_resolve_address(backend, address, err);

      if (retval == NULL)
        {
          gconf_backend_unref(backend);
          return NULL;
        }
      else
        {
          retval->backend = backend;
          retval->address = g_strdup(address);
          
          /* Leave a ref on the backend, now held by the GConfSource */
          
          return retval;
        }
    }
}

void         
gconf_source_free (GConfSource* source)
{
  GConfBackend* backend;
  gchar* address;
  
  g_return_if_fail(source != NULL);

  backend = source->backend;
  address = source->address;

  (*source->backend->vtable.destroy_source)(source);
  
  /* Remove ref held by the source. */
  gconf_backend_unref(backend);
  g_free(address);
}

#define SOURCE_READABLE(source, key, err)                  \
     ( ((source)->flags & GCONF_SOURCE_ALL_READABLE) ||    \
       ((source)->backend->vtable.readable != NULL &&     \
        (*(source)->backend->vtable.readable)((source), (key), (err))) )

static gboolean
source_is_writable(GConfSource* source, const gchar* key, GError** err)
{
  if ((source->flags & GCONF_SOURCE_NEVER_WRITEABLE) != 0)
    return FALSE;
  else if ((source->flags & GCONF_SOURCE_ALL_WRITEABLE) != 0)
    return TRUE;
  else if ((source->backend->vtable.writable != NULL) &&
           (*source->backend->vtable.writable)(source, key, err))
    return TRUE;
  else
    return FALSE;
}

static GConfValue*
gconf_source_query_value      (GConfSource* source,
                               const gchar* key,
                               const gchar** locales,
                               gchar** schema_name,
                               GError** err)
{
  g_return_val_if_fail(source != NULL, NULL);
  g_return_val_if_fail(key != NULL, NULL);
  g_return_val_if_fail(err == NULL || *err == NULL, NULL);
  
  /* note that key validity is unchecked */

  if ( SOURCE_READABLE(source, key, err) )
    {
      g_return_val_if_fail(err == NULL || *err == NULL, NULL);
      return (*source->backend->vtable.query_value)(source, key, locales, schema_name, err);
    }
  else
    return NULL;
}

static GConfMetaInfo*
gconf_source_query_metainfo      (GConfSource* source,
                                  const gchar* key,
                                  GError** err)
{
  g_return_val_if_fail(source != NULL, NULL);
  g_return_val_if_fail(key != NULL, NULL);
  g_return_val_if_fail(err == NULL || *err == NULL, NULL);
  
  /* note that key validity is unchecked */

  if ( SOURCE_READABLE(source, key, err) )
    {
      g_return_val_if_fail(err == NULL || *err == NULL, NULL);
      return (*source->backend->vtable.query_metainfo)(source, key, err);
    }
  else
    return NULL;
}


/* return value indicates whether the key was writable */
static gboolean
gconf_source_set_value        (GConfSource* source,
                               const gchar* key,
                               const GConfValue* value,
                               GError** err)
{
  g_return_val_if_fail(source != NULL, FALSE);
  g_return_val_if_fail(value != NULL, FALSE);
  g_return_val_if_fail(key != NULL, FALSE);
  g_return_val_if_fail(err == NULL || *err == NULL, FALSE);
  
  /* don't check key validity */

  if ( source_is_writable(source, key, err) )
    {
      g_return_val_if_fail(err == NULL || *err == NULL, FALSE);
      (*source->backend->vtable.set_value)(source, key, value, err);
      return TRUE;
    }
  else
    return FALSE;
}

static gboolean
gconf_source_unset_value      (GConfSource* source,
                               const gchar* key,
                               const gchar* locale,
                               GError** err)
{
  g_return_val_if_fail (source != NULL, FALSE);
  g_return_val_if_fail (key != NULL, FALSE);
  g_return_val_if_fail (err == NULL || *err == NULL, FALSE);
  
  if ( source_is_writable(source, key, err) )
    {
      g_return_val_if_fail(err == NULL || *err == NULL, FALSE);

      (*source->backend->vtable.unset_value)(source, key, locale, err);
      return TRUE;
    }
  else
    return FALSE;
}

static GSList*      
gconf_source_all_entries         (GConfSource* source,
                                  const gchar* dir,
                                  const gchar** locales,
                                  GError** err)
{
  g_return_val_if_fail(source != NULL, NULL);
  g_return_val_if_fail(dir != NULL, NULL);
  g_return_val_if_fail(err == NULL || *err == NULL, NULL);
  
  if ( SOURCE_READABLE(source, dir, err) )
    {
      g_return_val_if_fail(err == NULL || *err == NULL, NULL);
      return (*source->backend->vtable.all_entries)(source, dir, locales, err);
    }
  else
    return NULL;
}

static GSList*      
gconf_source_all_dirs          (GConfSource* source,
                                const gchar* dir,
                                GError** err)
{
  g_return_val_if_fail(source != NULL, NULL);
  g_return_val_if_fail(dir != NULL, NULL);  
  g_return_val_if_fail(err == NULL || *err == NULL, NULL);
  
  if ( SOURCE_READABLE(source, dir, err) )
    {
      g_return_val_if_fail(err == NULL || *err == NULL, NULL);
      return (*source->backend->vtable.all_subdirs)(source, dir, err);
    }
  else
    return NULL;
}

static gboolean
gconf_source_dir_exists        (GConfSource* source,
                                const gchar* dir,
                                GError** err)
{
  g_return_val_if_fail(source != NULL, FALSE);
  g_return_val_if_fail(dir != NULL, FALSE);
  g_return_val_if_fail(err == NULL || *err == NULL, FALSE);
  
  if ( SOURCE_READABLE(source, dir, err) )
    {
      g_return_val_if_fail(err == NULL || *err == NULL, FALSE);
      return (*source->backend->vtable.dir_exists)(source, dir, err);
    }
  else
    return FALSE;
}

static void         
gconf_source_remove_dir        (GConfSource* source,
                                const gchar* dir,
                                GError** err)
{
  g_return_if_fail(source != NULL);
  g_return_if_fail(dir != NULL);
  g_return_if_fail(err == NULL || *err == NULL);
  
  if ( source_is_writable(source, dir, err) )
    {
      g_return_if_fail(err == NULL || *err == NULL);
      (*source->backend->vtable.remove_dir)(source, dir, err);
    }
}

static gboolean    
gconf_source_set_schema        (GConfSource* source,
                                const gchar* key,
                                const gchar* schema_key,
                                GError** err)
{
  g_return_val_if_fail (source != NULL, FALSE);
  g_return_val_if_fail (key != NULL, FALSE);
  g_return_val_if_fail (err == NULL || *err == NULL, FALSE);
  
  if ( source_is_writable(source, key, err) )
    {
      g_return_val_if_fail(err == NULL || *err == NULL, FALSE);
      (*source->backend->vtable.set_schema)(source, key, schema_key, err);
      return TRUE;
    }
  else
    return FALSE;
}

static gboolean
gconf_source_sync_all         (GConfSource* source, GError** err)
{
  return (*source->backend->vtable.sync_all)(source, err);
}

static void
gconf_source_set_notify_func (GConfSource           *source,
			      GConfSourceNotifyFunc  notify_func,
			      gpointer               user_data)
{
  g_return_if_fail (source != NULL);

  if (source->backend->vtable.set_notify_func)
    {
      (*source->backend->vtable.set_notify_func) (source, notify_func, user_data);
    }
}

static void
gconf_source_add_listener (GConfSource *source,
			   guint        id,
			   const gchar *namespace_section)
{
  g_return_if_fail (source != NULL);
  g_return_if_fail (id > 0);

  if (source->backend->vtable.add_listener)
    {
      (*source->backend->vtable.add_listener) (source, id, namespace_section);
    }
}

static void
gconf_source_remove_listener (GConfSource *source,
			      guint        id)
{
  g_return_if_fail (source != NULL);
  g_return_if_fail (id > 0);

  if (source->backend->vtable.remove_listener)
    {
      (*source->backend->vtable.remove_listener) (source, id);
    }
}

/*
 *   Source stacks
 */

GConfSources* 
gconf_sources_new_from_addresses(GSList * addresses, GError** err)
{
  GConfSources *sources;
  GList        *sources_list;

  g_return_val_if_fail( (err == NULL) || (*err == NULL), NULL);

  sources_list = NULL;
  if (addresses != NULL)
    {
      GError *last_error = NULL;

      while (addresses != NULL)
	{
	  GConfSource* source;

	  if (last_error)
	    {
	      g_error_free (last_error);
	      last_error = NULL;
	    }
      
	  source = gconf_resolve_address ((const gchar*)addresses->data, &last_error);

	  if (source != NULL)
	    {
	      sources_list = g_list_prepend(sources_list, source);          
	      g_return_val_if_fail(last_error == NULL, NULL);
	    }
	  else
	    {
	      g_assert(last_error != NULL);
	      gconf_log(GCL_WARNING, _("Failed to load source \"%s\": %s"),
			(const gchar*)addresses->data, last_error->message);
	    }
          
	  addresses = g_slist_next(addresses);
	}

      if (sources_list == NULL)
	{
	  g_assert (last_error != NULL);
	  g_propagate_error (err, last_error);
	  return NULL;
	}

      if (last_error)
	{
	  g_error_free(last_error);
	  last_error = NULL;
	}
    }

  sources          = g_new0 (GConfSources, 1);
  sources->sources = g_list_reverse (sources_list);

  {
    GList *tmp;
    int i;
    gboolean some_writable;

    some_writable = FALSE;
    i = 0;
    tmp = sources->sources;
    while (tmp != NULL)
      {
        GConfSource *source = tmp->data;

        if (source->flags & GCONF_SOURCE_ALL_WRITEABLE)
          {
            some_writable = TRUE;
            gconf_log (GCL_DEBUG,
                       _("Resolved address \"%s\" to a writable configuration source at position %d"),
                       source->address, i);
          }
        else if (source->flags & GCONF_SOURCE_NEVER_WRITEABLE)
          {
            gconf_log (GCL_DEBUG,
                       _("Resolved address \"%s\" to a read-only configuration source at position %d"),
                       source->address, i);
          }
        else
          {
            some_writable = TRUE;
            gconf_log (GCL_DEBUG,
                       _("Resolved address \"%s\" to a partially writable configuration source at position %d"),
                       source->address, i);
          }

        ++i;
        tmp = tmp->next;
      }

    if (!some_writable)
      gconf_log (GCL_WARNING, _("None of the resolved addresses are writable; saving configuration settings will not be possible"));
  }
  
  return sources;
}

GConfSources*
gconf_sources_new_from_source       (GConfSource* source)
{
  GConfSources* sources;
  
  sources = g_new0(GConfSources, 1);

  if (source)
    sources->sources = g_list_append(NULL, source);

  return sources;
}

void
gconf_sources_free(GConfSources* sources)
{
  GList* tmp;

  tmp = sources->sources;

  while (tmp != NULL)
    {
      gconf_source_free(tmp->data);
      
      tmp = g_list_next(tmp);
    }

  g_list_free(sources->sources);

  g_free(sources);
}

void
gconf_sources_clear_cache        (GConfSources  *sources)
{
  GList* tmp;

  tmp = sources->sources;

  while (tmp != NULL)
    {
      GConfSource* source = tmp->data;

      if (source->backend->vtable.clear_cache)
        (*source->backend->vtable.clear_cache)(source);
      
      tmp = g_list_next(tmp);
    }
}

void
gconf_sources_clear_cache_for_sources (GConfSources  *sources,
				       GConfSources  *affected)
{
  GList* tmp;

  tmp = sources->sources;

  while (tmp != NULL)
    {
      GConfSource* source = tmp->data;
      const char *source_resource = get_address_resource (source->address);

      GList* tmp2;

      if (source->backend->vtable.clear_cache == NULL)
        continue;

      tmp2 = affected->sources;

      while (tmp2 != NULL)
	{
	  GConfSource* affected_source = tmp2->data;

	  if (source->backend == affected_source->backend &&
	      strcmp (source_resource, get_address_resource (affected_source->address)) == 0)
	    {
	      if (source->backend->vtable.clear_cache)
		(*source->backend->vtable.clear_cache)(source);
	    }

	  tmp2 = g_list_next(tmp2);
	}
      
      tmp = g_list_next(tmp);
    }
}

GConfValue*   
gconf_sources_query_value (GConfSources* sources, 
                           const gchar* key,
                           const gchar** locales,
                           gboolean use_schema_default,
                           gboolean* value_is_default,
                           gboolean* value_is_writable,
                           gchar   **schema_namep,
                           GError** err)
{
  GList* tmp;
  gchar* schema_name;
  GError* error;
  GConfValue* val;
  
  g_return_val_if_fail (sources != NULL, NULL);
  g_return_val_if_fail (key != NULL, NULL);
  g_return_val_if_fail ((err == NULL) || (*err == NULL), NULL);

  /* A value is writable if it is unset and a writable source exists,
   * or if it's set and the setting is within or after a writable source.
   * So basically if we see a writable source before we get the value,
   * or get the value from a writable source, the value is writable.
   */
  
  if (!gconf_key_check(key, err))
    return NULL;

  if (value_is_default)
    *value_is_default = FALSE;

  if (value_is_writable)
    *value_is_writable = FALSE;

  if (schema_namep)
    *schema_namep = NULL;

  val = NULL;
  schema_name = NULL;
  error = NULL;
  
  tmp = sources->sources;

  while (tmp != NULL)
    {
      GConfSource* source;      
      gchar** schema_name_retloc;

      schema_name_retloc = &schema_name;
      if (schema_name != NULL ||                          /* already have the schema name */
          (schema_namep == NULL && !use_schema_default))  /* don't need to get the schema name */
        schema_name_retloc = NULL;
      
      source = tmp->data;

      if (val == NULL)
        {
          /* A key is writable if the source containing its value or
           * an earlier source is writable
           */          
          if (value_is_writable &&
              source_is_writable (source, key, NULL)) /* ignore errors */
            *value_is_writable = TRUE;
          
          val = gconf_source_query_value (source, key, locales,
                                          schema_name_retloc, &error);

        }
      else if (schema_name_retloc != NULL)
        {
          GConfMetaInfo *mi;
          
          mi = gconf_source_query_metainfo (source, key, &error);
          
          if (mi)
            {
              *schema_name_retloc = mi->schema;
              mi->schema = NULL;
              gconf_meta_info_free (mi);
            }
        }
          
      if (error != NULL)
        {
          if (err)
            *err = error;
          else
            g_error_free (error);

          error = NULL;

          if (val)
            gconf_value_free (val);

          g_free (schema_name);
          
          return NULL;
        }

      /* schema_name_retloc == NULL means we aren't still looking for schema name,
       * tmp->next == NULL means we aren't going to get a schema name
       */
      if (val != NULL && (schema_name_retloc == NULL || schema_name != NULL || tmp->next == NULL))
        {
          if (schema_namep)
            *schema_namep = schema_name;
          else
            g_free (schema_name);
          
          return val;
        }
      else
        {
          ; /* Keep looking for either val or schema name */
        }
      
      tmp = g_list_next (tmp);
    }

  g_return_val_if_fail (error == NULL, NULL);
  g_return_val_if_fail (val == NULL, NULL);
  
  /* If we got here, there was no value; we try to look up the
   * schema for this key if we have one, and use the default
   * value.
   */
  
  if (schema_name != NULL)
    {
      /* Note that if the value isn't found, then it's always the
       * default value - even if there is no default value, NULL is
       * the default.  This makes things more sane (I think) because
       * is_default basically means "was set by user" - however we
       * also want to say that if use_schema_default is FALSE then
       * value_is_default will be FALSE so we put this inside the
       * schema_name != NULL conditional
      */
      if (value_is_default)
        *value_is_default = TRUE;

      if (use_schema_default)
        {
          val = gconf_sources_query_value (sources, schema_name, locales,
                                           FALSE, NULL, NULL, NULL, &error);
        }
      
      if (error != NULL)
        {
          if (err)
            *err = error;
          else
            g_error_free(error);

          g_free(schema_name);
          return NULL;
        }
      else if (val != NULL &&
               val->type != GCONF_VALUE_SCHEMA)
        {
          gconf_set_error (err, GCONF_ERROR_FAILED,
                           _("Schema `%s' specified for `%s' stores a non-schema value"), schema_name, key);

          if (schema_namep)
            *schema_namep = schema_name;
          else
            g_free (schema_name);

          return NULL;
        }
      else if (val != NULL)
        {
          GConfValue* retval;

          retval = gconf_schema_steal_default_value (gconf_value_get_schema (val));          

          gconf_value_free (val);

          if (schema_namep)
            *schema_namep = schema_name;
          else
            g_free (schema_name);
          
          return retval;
        }
      else
        {
          if (schema_namep)
            *schema_namep = schema_name;
          else
            g_free (schema_name);
          
          return NULL;
        }
    }
  
  return NULL;
}

void
gconf_sources_set_value   (GConfSources* sources,
                           const gchar* key,
                           const GConfValue* value,
			   GConfSources **modified_sources,
                           GError** err)
{
  GList* tmp;

  g_return_if_fail(sources != NULL);
  g_return_if_fail(key != NULL);
  g_return_if_fail((err == NULL) || (*err == NULL));

  if (modified_sources)
    *modified_sources = NULL;
  
  if (!gconf_key_check(key, err))
    return;
  
  g_assert(*key != '\0');
  
  if (key[1] == '\0')
    {
      gconf_set_error(err, GCONF_ERROR_IS_DIR,
                      _("The '/' name can only be a directory, not a key"));
      return;
    }
  
  tmp = sources->sources;

  while (tmp != NULL)
    {
      GConfSource* src = tmp->data;

      gconf_log (GCL_DEBUG, "Setting %s in %s",
                 key, src->address);
      
      /* gconf_source_set_value return value is whether source
       * was writable at key, not whether err was set.
       */
      if (gconf_source_set_value (src, key, value, err))
        {
          /* source was writable, err may be set */
          gconf_log (GCL_DEBUG, "%s was writable in %s", key, src->address);
	  if (modified_sources)
	    {
	      *modified_sources = gconf_sources_new_from_source (src);
	    }
	  return;
        }
      else
        {
          /* check whether the value is set; if it is, then
             we return an error since setting an overridden value
             would have no effect
          */
          GConfValue* val;

          val = gconf_source_query_value(tmp->data, key, NULL, NULL, NULL);
          
          if (val != NULL)
            {
              gconf_log (GCL_DEBUG, "%s was already set in %s", key, src->address);
              
              gconf_value_free(val);
              gconf_set_error(err, GCONF_ERROR_OVERRIDDEN,
                              _("Value for `%s' set in a read-only source at the front of your configuration path"), key);
              return;
            }
        }

      tmp = g_list_next(tmp);
    }

  /* If we arrived here, then there was nowhere to write a value */
  g_set_error (err,
               GCONF_ERROR,
               GCONF_ERROR_NO_WRITABLE_DATABASE,
               _("Unable to store a value at key '%s', as the configuration server has no writable databases. There are some common causes of this problem: 1) your configuration path file %s/path doesn't contain any databases or wasn't found 2) somehow we mistakenly created two gconfd processes 3) your operating system is misconfigured so NFS file locking doesn't work in your home directory or 4) your NFS client machine crashed and didn't properly notify the server on reboot that file locks should be dropped. If you have two gconfd processes (or had two at the time the second was launched), logging out, killing all copies of gconfd, and logging back in may help. If you have stale locks, remove ~/.gconf*/*lock. Perhaps the problem is that you attempted to use GConf from two machines at once, and ORBit still has its default configuration that prevents remote CORBA connections - put \"ORBIIOPIPv4=1\" in /etc/orbitrc. As always, check the user.* syslog for details on problems gconfd encountered. There can only be one gconfd per home directory, and it must own a lockfile in ~/.gconfd and also lockfiles in individual storage locations such as ~/.gconf"),           
               key, GCONF_CONFDIR);
}

void
gconf_sources_unset_value   (GConfSources* sources,
                             const gchar* key,
                             const gchar* locale,
			     GConfSources **modified_sources,
                             GError** err)
{
  /* We unset in every layer we can write to... */
  GList* tmp;
  GError* error = NULL;
  
  tmp = sources->sources;

  while (tmp != NULL)
    {
      GConfSource* src = tmp->data;

      if (gconf_source_unset_value(src, key, locale, &error))
        {
          /* it was writable */

          /* On error, set error and bail */
          if (error != NULL)
            {
              if (err)
                {
                  g_return_if_fail(*err == NULL);
                  *err = error;
                  return;
                }
              else
                {
                  g_error_free(error);
                  return;
                }
            }

	  if (modified_sources)
	    {
	      if (*modified_sources == NULL)
		{
		  *modified_sources = gconf_sources_new_from_source (src);
		}
	      else
		{
		  (*modified_sources)->sources =
		    g_list_prepend ((*modified_sources)->sources, src);
		}
	    }
        }
      
      tmp = g_list_next(tmp);
    }
}

static GSList *
prepend_unset_notify (GSList       *notifies,
		      GConfSources *modified_sources,
		      char         *key)
{
  GConfUnsetNotify *notify;

  g_assert (modified_sources != NULL);
  g_assert (key != NULL);

  notify = g_new0 (GConfUnsetNotify, 1);

  notify->modified_sources = modified_sources;
  notify->key              = key;

  return g_slist_append (notifies, notify);
}

static void
recursive_unset_helper (GConfSources   *sources,
                        const char     *key,
                        const char     *locale,
                        GConfUnsetFlags flags,
                        GSList        **notifies,
                        GError        **first_error)
{
  GError* err = NULL;
  GSList* subdirs;
  GSList* entries;
  GSList* tmp;
  const char *locales[2] = { NULL, NULL };
  GConfSources* modified_sources;
  GConfSources** modifiedp = NULL;

  if (notifies)
    {
      modified_sources = NULL;
      modifiedp = &modified_sources;
    }
  
  err = NULL;
  
  subdirs = gconf_sources_all_dirs (sources, key, &err);
          
  if (subdirs != NULL)
    {
      tmp = subdirs;

      while (tmp != NULL)
        {
          char *s = tmp->data;
          char *full = gconf_concat_dir_and_key (key, s);
          
          recursive_unset_helper (sources, full, locale, flags,
                                  notifies, first_error);
          
          g_free (s);
          g_free (full);

          tmp = g_slist_next (tmp);
        }

      g_slist_free (subdirs);
    }
  else
    {
      if (err != NULL)
        {
          gconf_log (GCL_DEBUG, "Error listing subdirs of '%s': %s\n",
                     key, err->message);
          if (*first_error)
            g_error_free (err);
          else
            *first_error = err;
          err = NULL;
        }
    }

  locales[0] = locale;
  entries = gconf_sources_all_entries (sources, key,
                                       locale ? locales : NULL,
                                       &err);
          
  if (err != NULL)
    {
      gconf_log (GCL_DEBUG, "Failure listing entries in '%s': %s\n",
                 key, err->message);
      if (*first_error)
        g_error_free (err);
      else
        *first_error = err;
      err = NULL;
    }

  if (entries != NULL)
    {
      tmp = entries;

      while (tmp != NULL)
        {
          GConfEntry* entry = tmp->data;
          char *full, *freeme;

	  full = freeme = gconf_concat_dir_and_key (key,
						    gconf_entry_get_key (entry));
          
          
          gconf_sources_unset_value (sources, full, locale, modifiedp, &err);
          if (notifies)
	    {
	      *notifies = prepend_unset_notify (*notifies, modified_sources, full);
	      modified_sources = NULL;
	      freeme = NULL;
	    }

          if (err != NULL)
            {
              gconf_log (GCL_DEBUG, "Error unsetting '%s': %s\n",
                         full, err->message);

              if (*first_error)
                g_error_free (err);
              else
                *first_error = err;
              err = NULL;
            }

          if (flags & GCONF_UNSET_INCLUDING_SCHEMA_NAMES)
            {
              gconf_sources_set_schema (sources,
                                        full, NULL,
                                        &err);
              if (err != NULL)
                {
                  gconf_log (GCL_DEBUG, "Error unsetting schema on '%s': %s\n",
                             full, err->message);
                  
                  if (*first_error)
                    g_error_free (err);
                  else
                    *first_error = err;
                  err = NULL;
                }
            }
          
          gconf_entry_free (entry);
          g_free (freeme);
          
          tmp = g_slist_next (tmp);
        }

      g_slist_free (entries);
    }

  gconf_sources_unset_value (sources, key, locale, modifiedp, &err);
  if (notifies)
    {
      *notifies = prepend_unset_notify (*notifies,
					modified_sources,
					g_strdup (key));
      modified_sources = NULL;
    }
  
  if (err != NULL)
    {
      gconf_log (GCL_DEBUG, "Error unsetting '%s': %s\n",
                 key, err->message);

      if (*first_error)
        g_error_free (err);
      else
        *first_error = err;
      err = NULL;
    }
}

void
gconf_sources_recursive_unset (GConfSources   *sources,
                               const gchar    *key,
                               const gchar    *locale,
                               GConfUnsetFlags flags,
                               GSList        **notifies,
                               GError        **err)
{
  GError *first_error;
  
  g_return_if_fail (sources != NULL);
  g_return_if_fail (key != NULL);
  g_return_if_fail (err == NULL || *err == NULL);

  first_error = NULL;
  recursive_unset_helper (sources, key, locale, flags,
                          notifies, &first_error);

  if (first_error)
    {
      if (notifies != NULL && *notifies != NULL)
	{
	  GSList *tmp;

	  tmp = *notifies;
	  while (tmp != NULL)
	    {
	      GConfUnsetNotify *notify = tmp->data;

	      g_free (notify->key);
	      g_free (notify);

	      tmp = tmp->next;
	    }

	  g_slist_free (*notifies);
	  *notifies = NULL;
	}

      g_propagate_error (err, first_error);
    }
}

gboolean
gconf_sources_dir_exists (GConfSources* sources,
                          const gchar* dir,
                          GError** err)
{
  GList *tmp;

  if (!gconf_key_check(dir, err))
    return FALSE;
  
  tmp = sources->sources;
  
  while (tmp != NULL) 
    {
      GConfSource* src = tmp->data;
      
      if (gconf_source_dir_exists (src, dir, err)) 
        return TRUE;

      tmp = g_list_next(tmp);
    }
  
  return FALSE;
}
          
void          
gconf_sources_remove_dir (GConfSources* sources,
                          const gchar* dir,
                          GError** err)
{
  /* We remove in every layer we can write to... */
  GList* tmp;
  
  if (!gconf_key_check(dir, err))
    return;
  
  tmp = sources->sources;

  while (tmp != NULL)
    {
      GConfSource* src = tmp->data;
      GError* error = NULL;
      
      gconf_source_remove_dir(src, dir, &error);

      /* On error, set error and bail */
      if (error != NULL)
        {
          if (err)
            {
              g_return_if_fail(*err == NULL);
              *err = error;
              return;
            }
          else
            {
              g_error_free(error);
              return;
            }
        }
      
      tmp = g_list_next(tmp);
    }
}

void         
gconf_sources_set_schema (GConfSources *sources,
                          const gchar  *key,
                          const gchar  *schema_key,
                          GError      **err)
{
  GList* tmp;

  if (!gconf_key_check (key, err))
    return;

  if (schema_key && !gconf_key_check (schema_key, err))
    return;
  
  tmp = sources->sources;

  while (tmp != NULL)
    {
      GConfSource* src = tmp->data;

      /* may set error, we just leave its setting */
      /* returns TRUE if the source was writable */
      if (gconf_source_set_schema (src, key, schema_key, err))
        return;

      tmp = g_list_next(tmp);
    }
}

/* God, this is depressingly inefficient. Maybe there's a nicer way to
   implement it... */
/* Then we have to ship it all to the app via CORBA... */
/* Anyway, we use a hash to be sure we have a single value for 
   each key in the directory, and we always take that value from
   the first source that had one set. When we're done we flatten
   the hash.
*/
static void
hash_listify_func(gpointer key, gpointer value, gpointer user_data)
{
  GSList** list_p = user_data;

  *list_p = g_slist_prepend(*list_p, value);
}

static void
hash_destroy_entries_func(gpointer key, gpointer value, gpointer user_data)
{
  GConfEntry* entry;

  entry = value;

  gconf_entry_free(entry);
}

static void
hash_destroy_pointers_func(gpointer key, gpointer value, gpointer user_data)
{
  g_free(value);
}

struct DefaultsLookupData {
  GConfSources* sources;
  const gchar** locales;
};

static void
hash_lookup_defaults_func(gpointer key, gpointer value, gpointer user_data)
{
  GConfEntry *entry = value;
  struct DefaultsLookupData* dld = user_data;
  GConfSources *sources = dld->sources;
  const gchar** locales = dld->locales;
  
  if (gconf_entry_get_value(entry) == NULL)
    {
      if (gconf_entry_get_schema_name(entry) != NULL)
        {
          GConfValue *val;


          val = gconf_sources_query_value(sources,
                                          gconf_entry_get_schema_name(entry),
                                          locales,
                                          TRUE,
                                          NULL,
                                          NULL,
                                          NULL,
                                          NULL);

          if (val != NULL &&
              val->type == GCONF_VALUE_SCHEMA)
            {
              GConfValue* defval;

              defval = gconf_schema_steal_default_value (gconf_value_get_schema(val));

              gconf_entry_set_value_nocopy (entry, defval);
              gconf_entry_set_is_default (entry, TRUE);
            }

          if (val)
            gconf_value_free(val);
        }
    }
}


static gboolean
key_is_writable (GConfSources *sources,
                 GConfSource  *value_in_src,
                 const gchar *key,
                 GError **err)
{
  GList *tmp;
  
  tmp = sources->sources;

  while (tmp != NULL)
    {
      GConfSource* src;
      
      src = tmp->data;
      
      if (source_is_writable (src, key, NULL))
        return TRUE;

      if (src == value_in_src)
        return FALSE; /* didn't find a writable source before value-containing
                         source.
                      */
      
      tmp = g_list_next (tmp);
    }

  /* This shouldn't be reached actually */
  return FALSE;
}

GSList*       
gconf_sources_all_entries   (GConfSources* sources,
                             const gchar* dir,
                             const gchar** locales,
                             GError** err)
{
  GList* tmp;
  GHashTable* hash;
  GSList* flattened;
  gboolean first_pass = TRUE; /* as an optimization, don't bother
                                 doing hash lookups on first source
                              */
  struct DefaultsLookupData dld = { NULL, NULL };
  
  dld.sources = sources;
  dld.locales = locales;

  /* Empty GConfSources, skip it */
  if (sources->sources == NULL)
    return NULL;

  hash = g_hash_table_new(g_str_hash, g_str_equal);

  tmp = sources->sources;

  while (tmp != NULL)
    {
      GConfSource* src;
      GSList* pairs;
      GSList* iter;
      GError* error = NULL;
      
      src   = tmp->data;
      pairs = gconf_source_all_entries(src, dir, locales, &error);
      iter  = pairs;
      
      /* On error, set error and bail */
      if (error != NULL)
        {
          g_hash_table_foreach(hash, hash_destroy_entries_func, NULL);
          
          g_hash_table_destroy(hash);
          
          if (err)
            {
              g_return_val_if_fail(*err == NULL, NULL);
              *err = error;
              return NULL;
            }
          else
            {
              g_error_free(error);
              return NULL;
            }
        }

      /* Iterate over the list of entries, stuffing them in the hash
         and setting their writability flag if they're new
      */
      
      while (iter != NULL)
        {
          GConfEntry* pair = iter->data;
          GConfEntry* previous;
          gchar *full;
          
          if (first_pass)
            previous = NULL; /* Can't possibly be there. */
          else
            previous = g_hash_table_lookup(hash, pair->key);
          
          if (previous != NULL)
            {
              if (gconf_entry_get_value (previous) != NULL)
                /* Discard this latest one */
                ;
              else
                {
                  /* Save the new value, previously we had an entry but no value */
                  gconf_entry_set_value_nocopy (previous,
                                                gconf_entry_steal_value(pair));

                  /* As an efficiency hack, remember that
                   * entry->key is relative not absolute on the
                   * gconfd side
                   */
                  full = gconf_concat_dir_and_key (dir, previous->key);

                  gconf_entry_set_is_writable (previous,
                                               key_is_writable (sources,
                                                                src,
                                                                full,
                                                                NULL));

                  g_free (full);
                }
              
              if (gconf_entry_get_schema_name (previous) != NULL)
                /* Discard this latest one */
                ;
              else
                {
                  /* Save the new schema name, previously we had an entry but no schema name*/
                  if (gconf_entry_get_schema_name (pair) != NULL)
                    {
                      gconf_entry_set_schema_name (previous,
                                                   gconf_entry_get_schema_name (pair));
                    }
                }

              gconf_entry_free(pair);
            }
          else
            {
              /* Save */
              g_hash_table_insert(hash, pair->key, pair);
              
              /* As an efficiency hack, remember that
               * entry->key is relative not absolute on the
               * gconfd side
               */
              full = gconf_concat_dir_and_key (dir, pair->key);

              gconf_entry_set_is_writable (pair,
                                           key_is_writable (sources,
                                                            src,
                                                            full,
                                                            NULL));
              
              g_free (full);
            }

          iter = g_slist_next(iter);
        }
      
      /* All pairs are either stored or destroyed. */
      g_slist_free(pairs);

      first_pass = FALSE;

      tmp = g_list_next(tmp);
    }

  flattened = NULL;

  g_hash_table_foreach(hash, hash_lookup_defaults_func, &dld);
  
  g_hash_table_foreach(hash, hash_listify_func, &flattened);

  g_hash_table_destroy(hash);
  
  return flattened;
}

GSList*       
gconf_sources_all_dirs   (GConfSources* sources,
                          const gchar* dir,
                          GError** err)
{
  GList* tmp = NULL;
  GHashTable* hash = NULL;
  GSList* flattened = NULL;
  gboolean first_pass = TRUE; /* as an optimization, don't bother
                                 doing hash lookups on first source
                              */

  g_return_val_if_fail(sources != NULL, NULL);
  g_return_val_if_fail(dir != NULL, NULL);

  /* As another optimization, skip the whole 
     hash thing if there's only zero or one sources
  */
  if (sources->sources == NULL)
    return NULL;

  if (sources->sources->next == NULL)
    {
      return gconf_source_all_dirs (sources->sources->data, dir, err);
    }

  /* 2 or more sources */
  g_assert(g_list_length(sources->sources) > 1);

  hash = g_hash_table_new(g_str_hash, g_str_equal);

  tmp = sources->sources;

  while (tmp != NULL)
    {
      GConfSource* src;
      GSList* subdirs;
      GSList* iter;
      GError* error = NULL;
      
      src     = tmp->data;
      subdirs = gconf_source_all_dirs(src, dir, &error);
      iter    = subdirs;

      /* On error, set error and bail */
      if (error != NULL)
        {
          g_hash_table_foreach (hash, hash_destroy_pointers_func, NULL);
          
          g_hash_table_destroy (hash);
          
          if (err)
            {
              g_return_val_if_fail(*err == NULL, NULL);
              *err = error;
              return NULL;
            }
          else
            {
              g_error_free(error);
              return NULL;
            }
        }
      
      while (iter != NULL)
        {
          gchar* subdir = iter->data;
          gchar* previous;
          
          if (first_pass)
            previous = NULL; /* Can't possibly be there. */
          else
            previous = g_hash_table_lookup(hash, subdir);
          
          if (previous != NULL)
            {
              /* Discard */
              g_free(subdir);
            }
          else
            {
              /* Save */
              g_hash_table_insert(hash, subdir, subdir);
            }

          iter = g_slist_next(iter);
        }
      
      /* All pairs are either stored or destroyed. */
      g_slist_free(subdirs);

      first_pass = FALSE;

      tmp = g_list_next(tmp);
    }

  flattened = NULL;

  g_hash_table_foreach(hash, hash_listify_func, &flattened);

  g_hash_table_destroy(hash);

  return flattened;
}

gboolean
gconf_sources_sync_all    (GConfSources* sources, GError** err)
{
  GList* tmp;
  gboolean failed = FALSE;
  GError* all_errors = NULL;
  
  tmp = sources->sources;

  while (tmp != NULL)
    {
      GConfSource* src = tmp->data;
      GError* error = NULL;
      
      if (!gconf_source_sync_all(src, &error))
        {
          failed = TRUE;
          g_assert(error != NULL);
        }
          
      /* On error, set error and bail */
      if (error != NULL)
        {
          if (err)
            all_errors = gconf_compose_errors(all_errors, error);

          g_error_free(error);
        }
          
      tmp = g_list_next(tmp);
    }

  if (err)
    {
      g_return_val_if_fail(*err == NULL, !failed);
      *err = all_errors;
    }
  
  return !failed;
}

GConfMetaInfo*
gconf_sources_query_metainfo (GConfSources* sources,
                              const gchar* key,
                              GError** err)
{
  GList* tmp;
  GConfMetaInfo* mi = NULL;
  
  tmp = sources->sources;

  while (tmp != NULL)
    {
      GConfSource* src = tmp->data;
      GError* error = NULL;
      GConfMetaInfo* this_mi;

      this_mi = gconf_source_query_metainfo(src, key, &error);
      
      /* On error, just keep going, log the error maybe. */
      if (error != NULL)
        {
          g_assert(this_mi == NULL);
          gconf_log(GCL_ERR, _("Error finding metainfo: %s"), error->message);
          g_error_free(error);
          error = NULL;
        }

      if (this_mi != NULL)
        {
          if (mi == NULL)
            mi = this_mi;
          else
            {
              /* Fill in any missing fields of "mi" found in "this_mi",
                 and pick the most recent mod time */
              if (gconf_meta_info_get_schema(mi) == NULL &&
                  gconf_meta_info_get_schema(this_mi) != NULL)
                {
                  gconf_meta_info_set_schema(mi,
                                             gconf_meta_info_get_schema(this_mi));
                }

              if (gconf_meta_info_get_mod_user(mi) == NULL &&
                  gconf_meta_info_get_mod_user(this_mi) != NULL)
                {
                  gconf_meta_info_set_mod_user(mi,
                                               gconf_meta_info_get_mod_user(this_mi));
                }
              
              if (gconf_meta_info_mod_time(mi) < gconf_meta_info_mod_time(this_mi))
                {
                  gconf_meta_info_set_mod_time(mi,
                                               gconf_meta_info_mod_time(this_mi));
                }

              gconf_meta_info_free(this_mi);
            }
        }
      
      tmp = g_list_next(tmp);
    }
  
  return mi;
}

GConfValue*
gconf_sources_query_default_value(GConfSources* sources,
                                  const gchar* key,
                                  const gchar** locales,
                                  gboolean* is_writable,
                                  GError** err)
{
  GError* error = NULL;
  GConfValue* val;
  GConfMetaInfo* mi;
  
  g_return_val_if_fail(err == NULL || *err == NULL, NULL);

  if (is_writable)
    *is_writable = key_is_writable (sources, NULL, key, NULL);
  
  mi = gconf_sources_query_metainfo(sources, key,
                                    &error);
  if (mi == NULL)
    {
      if (error != NULL)
        {
          if (err)
            *err = error;
          else
            {
              gconf_log(GCL_ERR, _("Error getting metainfo: %s"), error->message);
              g_error_free(error);
            }
        }
      return NULL;
    }

  if (gconf_meta_info_get_schema(mi) == NULL)
    {
      gconf_meta_info_free(mi);
      return NULL;
    }
      
  val = gconf_sources_query_value(sources,
                                  gconf_meta_info_get_schema(mi), locales,
                                  TRUE, NULL, NULL, NULL, &error);
  
  if (val != NULL)
    {
      GConfSchema* schema;

      if (val->type != GCONF_VALUE_SCHEMA)
        {
          gconf_log(GCL_WARNING,
                    _("Key `%s' listed as schema for key `%s' actually stores type `%s'"),
                    gconf_meta_info_get_schema(mi),
                    key,
                    gconf_value_type_to_string(val->type));

          gconf_meta_info_free(mi);
          return NULL;
        }

      gconf_meta_info_free(mi);

      schema = gconf_value_steal_schema (val);
      gconf_value_free (val);
      
      if (schema != NULL)
        {
          GConfValue* retval;

          retval = gconf_schema_steal_default_value (schema);

          gconf_schema_free (schema);
          
          return retval;
        }
      return NULL;
    }
  else
    {
      if (error != NULL)
        {
          if (err)
            *err = error;
          else
            {
              gconf_log(GCL_ERR, _("Error getting value for `%s': %s"),
                        gconf_meta_info_get_schema(mi),
                        error->message);
              g_error_free(error);
            }
        }
      
      gconf_meta_info_free(mi);
      
      return NULL;
    }
}

void
gconf_sources_set_notify_func (GConfSources          *sources,
			       GConfSourceNotifyFunc  notify_func,
			       gpointer               user_data)
{
  GList *tmp;

  tmp = sources->sources;
  while (tmp != NULL)
    {
      gconf_source_set_notify_func (tmp->data, notify_func, user_data);

      tmp = tmp->next;
    }
}

void
gconf_sources_add_listener (GConfSources *sources,
			    guint         id,
			    const gchar  *namespace_section)
{
  GList *tmp;

  tmp = sources->sources;
  while (tmp != NULL)
    {
      gconf_source_add_listener (tmp->data, id, namespace_section);

      tmp = tmp->next;
    }
}

void
gconf_sources_remove_listener (GConfSources *sources,
			       guint         id)
{
  GList *tmp;

  tmp = sources->sources;
  while (tmp != NULL)
    {
      gconf_source_remove_listener (tmp->data, id);

      tmp = tmp->next;
    }
}

/* Non-allocating variant of gconf_address_resource()
 */
static const char *
get_address_resource (const char *address)
{
  const char *start;

  g_return_val_if_fail (address != NULL, NULL);

  start = strchr (address, ':');
  if (start != NULL)
    {
      start = strchr (++start, ':');

      if (start != NULL)
        start++;
    }

  return start;
}

/* Return TRUE if
 *  1. @sources contains @modified_src and
 *  2. @key is not set in any source above @modified_src.
 */
gboolean
gconf_sources_is_affected (GConfSources *sources,
                           GConfSource  *modified_src,
                           const char   *key)
{
  const char *modified_resource;
  GList      *tmp;

  modified_resource = get_address_resource (modified_src->address);

  tmp = sources->sources;
  while (tmp != NULL)
    {
      GConfSource *source = tmp->data;

      if (source->backend == modified_src->backend &&
          strcmp (modified_resource, get_address_resource (source->address)) == 0)
        break;

      tmp = tmp->next;
    }

  if (tmp == NULL)
    return FALSE;
  
  tmp = tmp->prev;
  while (tmp != NULL)
    {
      GConfValue *val;
      
      val = gconf_source_query_value (tmp->data, key, NULL, NULL, NULL);
      if (val != NULL)
	{
	  gconf_value_free (val);
	  return FALSE;
	}
      
      tmp = tmp->prev;
    }
  
  return TRUE;
}
