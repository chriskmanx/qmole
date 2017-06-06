
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
#include "gconf-internals.h"
#include <stdio.h>
#include <string.h>
#include <sys/stat.h>
#include <unistd.h>


/* Don't allow special characters in configuration source addresses.
 * The important one here is not to allow ';' because we use that
 * internally as a list delimiter. See GCONF_DATABASE_LIST_DELIM
 */
static const char invalid_chars[] = 
#ifndef G_OS_WIN32
  /* Space is common in user names (and thus home directories) on Windows */
  " "
#endif
  "\t\r\n\"$&<>,+=#!()'|{}[]?~`;%\\";

static gboolean
gconf_address_valid (const char  *address,
		     char      **why_invalid)
{
  const char *s;

  g_return_val_if_fail (address != NULL, FALSE);

  if (why_invalid)
    *why_invalid = NULL;

  s = address;
  while (*s)
    {
      const char *inv = invalid_chars;

      while (*inv)
	{
	  if (*inv == *s)
	    {
	      if (why_invalid)
		*why_invalid = g_strdup_printf(_("`%c' is an invalid character in a configuration storage address"), *s);
	      return FALSE;
	    }
	  ++inv;
	}

      ++s;
    }

  return TRUE;
}

gchar* 
gconf_address_backend(const gchar* address)
{
  const gchar* end;

  g_return_val_if_fail(address != NULL, NULL);

  end = strchr(address, ':');

  if (end == NULL)
    {
      return NULL;
    }
  else
    {
      int len = end-address+1;
      gchar* retval = g_malloc(len);
      strncpy(retval, address, len-1);
      retval[len-1] = '\0';
      return retval;
    }
}

gchar* 
gconf_address_resource(const gchar* address)
{
  const gchar* start;

  g_return_val_if_fail(address != NULL, NULL);

  start = strchr(address, ':');

  if (start == NULL)
    return NULL;
  else
    {
      ++start;
      start = strchr(start, ':');

      if (start == NULL)
        return NULL;
      else
        {
          ++start;
#ifdef G_OS_WIN32
	  return _gconf_win32_replace_prefix (start);
#else
          return g_strdup(start);
#endif
        }
    }
}

gchar**
gconf_address_flags(const gchar* address)
{
  const gchar* start;
  const gchar* end;
  gchar* csv_flags;
  gchar** split_flags;
  
  g_return_val_if_fail(address != NULL, NULL);

  start = strchr(address, ':');

  if (start == NULL)
    return NULL;

  ++start;

  end = strchr(start, ':');

  if (end == NULL)
    return NULL;

  if (start == end)
    return NULL;
  
  csv_flags = g_strndup(start, end - start);
  
  split_flags = g_strsplit(csv_flags, ",", 0);

  g_free(csv_flags);

  if (*split_flags == NULL)
    {
      /* don't return an empty vector */
      g_strfreev(split_flags);
      return NULL;
    }
  else
    return split_flags;
}

gchar*       
gconf_backend_file(const gchar* address)
{
  gchar* back;
  gchar* file;
  gchar* retval;

  g_return_val_if_fail(address != NULL, NULL);

  back = gconf_address_backend(address);

  if (back == NULL)
    return NULL;

  file = g_strconcat("gconfbackend-", back, NULL);
  
  retval = g_module_build_path(GCONF_BACKEND_DIR, file);

  g_free(back);

  if (g_file_test(retval, G_FILE_TEST_EXISTS))
    {
      g_free(file);

      return retval;
    }
  else
    {
      /* -- debug only */
#ifdef GCONF_ENABLE_DEBUG      
      gchar* dir;

      g_free(retval);
      dir = g_strconcat(GCONF_SRCDIR, "/gconf/",
                        GCONF_BUILDDIR, "/backends/.libs", NULL);

      retval = g_module_build_path(dir, file);

      g_free(dir);
      
      if (g_file_test(retval, G_FILE_TEST_EXISTS))
        {
          g_free(file);
          return retval;
        }
#endif
      /* -- end debug only */

      gconf_log(GCL_ERR, _("No such file `%s'\n"), retval);

      g_free(file);
      g_free(retval);
      return NULL;
    }
}

/*
 * Backend Cache 
 */

static GHashTable* loaded_backends = NULL;

static gboolean
gconf_backend_verify_vtable (GConfBackendVTable  *vtable,
			     GConfBackendVTable  *vtable_copy,
			     const char          *backend_name,			    
			     GError             **err)
{
  int i;
  struct
  {
    char  *name;
    gsize  offset;
  } required_vtable_functions[] = {
    { "shutdown",        G_STRUCT_OFFSET(GConfBackendVTable, shutdown)        },
    { "resolve_address", G_STRUCT_OFFSET(GConfBackendVTable, resolve_address) },
    { "query_value",     G_STRUCT_OFFSET(GConfBackendVTable, query_value)     },
    { "query_metainfo",  G_STRUCT_OFFSET(GConfBackendVTable, query_metainfo)  },
    { "set_value",       G_STRUCT_OFFSET(GConfBackendVTable, set_value)       },
    { "all_entries",     G_STRUCT_OFFSET(GConfBackendVTable, all_entries)     },
    { "all_subdirs",     G_STRUCT_OFFSET(GConfBackendVTable, all_subdirs)     },
    { "unset_value",     G_STRUCT_OFFSET(GConfBackendVTable, unset_value)     },
    { "dir_exists",      G_STRUCT_OFFSET(GConfBackendVTable, dir_exists)      },
    { "remove_dir",      G_STRUCT_OFFSET(GConfBackendVTable, remove_dir)      },
    { "set_schema",      G_STRUCT_OFFSET(GConfBackendVTable, set_schema)      },
    { "sync_all",        G_STRUCT_OFFSET(GConfBackendVTable, sync_all)        },
    { "destroy_source",  G_STRUCT_OFFSET(GConfBackendVTable, destroy_source)  },
    { "blow_away_locks", G_STRUCT_OFFSET(GConfBackendVTable, blow_away_locks) }
  };

  if (!vtable)
    {
      gconf_set_error(err,
		      GCONF_ERROR_FAILED, _("Backend `%s' failed to return a vtable\n"),
		      backend_name);
      return FALSE;
    }

  /* Create a copy in case vtable size doesn't match */
  memcpy(vtable_copy, vtable, MIN(vtable->vtable_size, sizeof(GConfBackendVTable)));

  vtable_copy->vtable_size = sizeof(GConfBackendVTable);

  for (i = 0; i < G_N_ELEMENTS(required_vtable_functions); i++)
    {
      if (G_STRUCT_MEMBER_P(vtable_copy, required_vtable_functions[i].offset) == NULL)
	{
	  gconf_set_error(err,
			  GCONF_ERROR_FAILED, _("Backend `%s' missing required vtable member `%s'\n"),
			  backend_name,
			  required_vtable_functions[i].name);
	  return FALSE;
	}
    }

  return TRUE;
}

GConfBackend* 
gconf_get_backend(const gchar* address, GError** err)
{
  GConfBackend* backend;
  gchar* name;
  gchar* why_invalid;

  if (loaded_backends == NULL)
    {
      loaded_backends = g_hash_table_new(g_str_hash, g_str_equal);
    }

  why_invalid = NULL;
  if (!gconf_address_valid (address, &why_invalid))
    {
      g_assert (why_invalid != NULL);
      gconf_set_error (err, GCONF_ERROR_BAD_ADDRESS, _("Bad address `%s': %s"),
		       address, why_invalid);
      g_free (why_invalid);
      return NULL;
    }

  name = gconf_address_backend(address);
      
  if (name == NULL)
    {
      gconf_set_error(err, GCONF_ERROR_BAD_ADDRESS, _("Bad address `%s'"), address);
      return NULL;
    }

  backend = g_hash_table_lookup(loaded_backends, name);

  if (backend != NULL)
    {
      /* Returning a "copy" */
      gconf_backend_ref(backend);
      g_free(name);
      return backend;
    }
  else
    {
      gchar* file;
          
      file = gconf_backend_file(address);
          
      if (file != NULL)
        {
          GModule* module;
          GConfBackendVTable* (*get_vtable)(void);

          if (!g_module_supported())
            g_error(_("GConf won't work without dynamic module support (gmodule)"));
              
          module = g_module_open(file, G_MODULE_BIND_LAZY);
              
          g_free(file);
          
          if (module == NULL)
            {
              gconf_set_error(err,
                              GCONF_ERROR_FAILED, _("Error opening module `%s': %s\n"),
                              name, g_module_error());
              g_free(name);
              return NULL;
            }

          if (!g_module_symbol(module, 
                               "gconf_backend_get_vtable", 
                               (gpointer*)&get_vtable))
            {
              gconf_set_error(err,
                              GCONF_ERROR_FAILED, _("Error initializing module `%s': %s\n"),
                              name, g_module_error());
              g_module_close(module);
              g_free(name);
              return NULL;
            }
          
          backend = g_new0(GConfBackend, 1);

          backend->module = module;

	  if (!gconf_backend_verify_vtable((*get_vtable)(), &backend->vtable, name, err))
	    {
	      g_module_close(module);
	      g_free(name);
	      g_free(backend);
	      return NULL;
	    }
              
          backend->name = name;

          g_hash_table_insert(loaded_backends, (gchar*)backend->name, backend);
          
          /* Returning a "copy" */
          gconf_backend_ref(backend);

          return backend;
        }
      else
        {
          gconf_set_error(err, GCONF_ERROR_FAILED,
                          _("Couldn't locate backend module for `%s'"), address);
          return NULL;
        }
    }
}

void          
gconf_backend_ref(GConfBackend* backend)
{
  g_return_if_fail(backend != NULL);

  backend->refcount += 1;
}

void          
gconf_backend_unref(GConfBackend* backend)
{
  g_return_if_fail(backend != NULL);
  g_return_if_fail(backend->refcount > 0);

  if (backend->refcount > 1)
    {
      backend->refcount -= 1;
    }
  else
    {
      GError* error = NULL;
      
      (*backend->vtable.shutdown)(&error);

      if (error != NULL)
        {
          g_warning("%s", error->message);
          g_error_free(error);
        }
          
      if (!g_module_close(backend->module))
        g_warning(_("Failed to shut down backend"));

      g_hash_table_remove(loaded_backends, backend->name);
      
      g_free((gchar*)backend->name); /* cast off const */

      g_free(backend);
    }  
}

/*
 * Backend vtable wrappers
 */

GConfSource*  
gconf_backend_resolve_address (GConfBackend* backend, 
                               const gchar* address,
                               GError** err)
{
  gchar** flags;
  gchar** iter;
  GConfSource* retval;

  retval = (*backend->vtable.resolve_address)(address, err);

  if (retval == NULL)
    return NULL;

  flags = gconf_address_flags(address);

  if (flags == NULL)
    return retval;

  /* FIXME this is now already done in the XML backend,
   * so the following code is pointless, except that I think
   * the BDB backend isn't fixed. The reason it needs to go in
   * the backend is so that we don't try to get a lock on read-only sources.
   */
  iter = flags;
  while (*iter)
    {
      if (strcmp(*iter, "readonly") == 0)
        {
          retval->flags &= ~(GCONF_SOURCE_ALL_WRITEABLE);
          retval->flags |= GCONF_SOURCE_NEVER_WRITEABLE;
        }
      /* no need to handle readwrite because backends should always
         default to "maximum read/write privileges"
      */
      ++iter;
    }

  g_strfreev(flags);

  return retval;
}

void
gconf_blow_away_locks (const gchar* address)
{
  GConfBackend* backend;

  backend = gconf_get_backend (address, NULL);

  if (backend != NULL)
    {
      (*backend->vtable.blow_away_locks) (address);
    }
}
