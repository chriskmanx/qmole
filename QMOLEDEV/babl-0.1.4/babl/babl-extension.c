/* babl - dynamically extendable universal pixel conversion library.
 * Copyright (C) 2005, Øyvind Kolås.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 3 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General
 * Public License along with this library; if not, see
 * <http://www.gnu.org/licenses/>.
 */

#define BABL_DYNAMIC_EXTENSIONS

#include "config.h"


#ifdef BABL_DYNAMIC_EXTENSIONS
/* must be defined before inclusion of babl-internal.h */
#undef  BABL_INIT_HOOK
#define BABL_INIT_HOOK    init_hook (); dynamic_init_hook ();
#endif

#include "babl-internal.h"
#include "babl-db.h"
#include "babl-base.h"
#include <string.h>
#include <stdarg.h>

static Babl *babl_extension_current_extender = NULL;

Babl *
babl_extender (void)
{
  if (babl_extension_current_extender)
    return babl_extension_current_extender;
  return NULL;
}

void
babl_set_extender (Babl *new_extender)
{
  babl_extension_current_extender = new_extender;
}

static int
babl_extension_destroy (void *data);

static Babl *
extension_new (const char *path,
               void       *dl_handle,
               void        (*destroy)(void))
{
  Babl *babl;

  babl                = babl_malloc (sizeof (BablExtension) + strlen (path) + 1);
  babl_set_destructor (babl, babl_extension_destroy);
  babl->instance.name = (char *) babl + sizeof (BablExtension);
  strcpy (babl->instance.name, path);
  babl->instance.id         = 0;
  babl->class_type          = BABL_EXTENSION;
  babl->extension.dl_handle = dl_handle;
  babl->extension.destroy   = destroy;

  return babl;
}

static Babl *babl_quiet = NULL;

Babl *
babl_extension_quiet_log (void)
{
  if (babl_quiet)
    return babl_quiet;
  babl_quiet = extension_new ("", NULL, NULL);
  return babl_quiet;
}

Babl *
babl_extension_base (void)
{
  Babl *babl;
  void *dl_handle = NULL;

  void  (*destroy)(void) = NULL;

  if (!db)
    {
      babl_extension_quiet_log ();
      babl_set_extender (NULL);
      db = babl_db_init ();
    }
  babl = extension_new ("BablBase",
                        dl_handle,
                        destroy);
  babl_set_extender (babl);

  {
    Babl *ret = babl_db_insert (db, babl);
    if (ret != babl)
      babl_free (babl);
    else
      babl_base_init ();
    babl = ret;
  }
  babl_set_extender (NULL);
  return babl;
}

void babl_extension_deinit (void)
{
  babl_free (babl_quiet);
  babl_quiet = NULL;
}

#ifdef BABL_DYNAMIC_EXTENSIONS

#include <dirent.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>

#ifdef HAVE_DLFCN_H
#ifndef WIN32

#include <dlfcn.h>
#define HLIB    void *

#endif /* WIN32 */
#elif HAVE_DL_H


#include <dl.h>
#include <errno.h>
#if !defined(DYNAMIC_PATH)
#  define DYNAMIC_PATH          0
#endif
#if !defined(BIND_RESTRICTED)
#  define BIND_RESTRICTED       0
#endif
#define RTLD_NOW		(BIND_IMMEDIATE|BIND_NONFATAL|DYNAMIC_PATH)
#define HLIB   shl_t
#define dlopen(path, flags)    shl_load (path, flags, 0L)
#define dlclose(handle)                shl_unload (handle)
#define dlerror()              strerror (errno)
static void *dlsym (HLIB handle, const char *name) {
  void *address = 0;
  shl_findsym(&handle, name, TYPE_UNDEFINED, &address);
  return address;
}

#endif

#ifndef RTLD_NOW
#define RTLD_NOW    0
#endif

#ifdef WIN32
#define WIN32_LEAN_AND_MEAN
#include <windows.h>
#define HLIB    HINSTANCE

#define dlopen(a, b)    LoadLibrary (a)
#define dlsym(l, s)     GetProcAddress (l, s)
#define dlclose(l)      FreeLibrary (l)
#define dlerror()       GetLastError ()
#endif

typedef int  (*BablExtensionInitFunc)   (void);
typedef void (*BablExtensionDestroyFunc)(void);


static Babl *
load_failed (Babl *babl)
{
  if (babl)
    {
      babl_free (babl);
    }
  babl_set_extender (NULL);
  return NULL;
}

static Babl *
babl_extension_load (const char *path)
{
  Babl *babl = NULL;
  /* do the actual loading thing */
  HLIB  dl_handle = NULL;

  BablExtensionInitFunc init = NULL;
  BablExtensionDestroyFunc destroy = NULL;

  dl_handle = dlopen (path, RTLD_NOW);
  if (!dl_handle)
    {
      babl_log ("dlopen() failed:\n\t%s", dlerror ());
      return load_failed (babl);
    }
  init = (BablExtensionInitFunc) dlsym (dl_handle, "init");
  if (!init)
    {
      babl_log ("\n\tint babl_extension_init() function not found in extension '%s'", path);
      dlclose (dl_handle);
      return load_failed (babl);
    }

  destroy = (BablExtensionDestroyFunc) dlsym (dl_handle, "destroy");
  babl    = extension_new (path,
                           dl_handle,
                           destroy);

  babl_set_extender (babl);
  if (init ())
    {
      babl_log ("babl_extension_init() in extension '%s' failed (return!=0)", path);
      dlclose (dl_handle);
      return load_failed (babl);
    }

  babl_db_insert (db, babl);
  if (babl == babl_db_exist_by_name (db, path))
    {
      babl_set_extender (NULL);
      return babl;
    }
  else
    {
      return load_failed (babl);
    }
}

static void
babl_extension_load_dir (const char *base_path)
{
  DIR *dir;

  if ((dir = opendir (base_path)))
    {
      struct  dirent *dentry;

      while ((dentry = readdir (dir)) != NULL)
        {
          if (dentry->d_name[0] != '.')
            {
              char       *path = NULL;
              struct stat st;
              char       *extension;

              path = babl_strcat (path, base_path);
              path = babl_strcat (path, BABL_DIR_SEPARATOR);
              path = babl_strcat (path, dentry->d_name);

              stat (path, &st);

              if ((extension = strrchr (dentry->d_name, '.')) != NULL &&
                  !strcmp (extension, SHREXT))
                {
                  babl_extension_load (path);
                }

              babl_free (path);
            }
        }
      closedir (dir);
    }
}

static char *
expand_path (char *path)
{
  char *src;
  char *dst;

  dst = NULL;

  src = path;

  while (*src)
    {
      char *home;
      switch (*src)
        {
          case '~':
            home = getenv ("HOME");
            if (NULL != home)
              dst = babl_strcat (dst, home);
            break;

          default:
          {
            char tmp[2] = "?";
            tmp[0] = *src;
            dst    = babl_strcat (dst, tmp);
          }
        }
      src++;
    }
  return dst;
}


/*  parse the provided colon seperated list of paths to search
 */
void
babl_extension_load_dir_list (const char *dir_list)
{
  int         eos = 0;
  const char *src;
  char       *path, *dst;


  path = babl_strdup (dir_list);
  src  = dir_list;
  dst  = path;

  while (!eos)
    {
      switch (*src)
        {
          case '\0':
            eos = 1;

          case BABL_PATH_SEPARATOR:
          {
            char *expanded_path = expand_path (path);
            babl_extension_load_dir (expanded_path);
            babl_free (expanded_path);
          }
            dst = path;
            src++;
            *dst = '\0';
            break;

          default:
            *(dst++) = *(src++);
            *dst     = '\0';
            break;
        }
    }
  babl_free (path);
}

#endif


static int
babl_extension_destroy (void *data)
{
  Babl *babl = data;
  if (babl->extension.destroy)
    babl->extension.destroy ();
  if (babl->extension.dl_handle)
    dlclose (babl->extension.dl_handle);
  return 0;
}

BABL_CLASS_IMPLEMENT (extension)
