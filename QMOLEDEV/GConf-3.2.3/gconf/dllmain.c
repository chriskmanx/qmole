/* GConf - dllmain.c
 * Copyright (C) 2005 Novell, Inc
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

#include <windows.h>
#include <string.h>
#include <mbstring.h>
#include <glib.h>

/* locale_dir uses system codepage as it is passed to the non-UTF8ified
 * gettext library
 */
static const char *locale_dir;

/* The others are in UTF-8 */
static const char *runtime_prefix;
static const char *confdir;
static const char *etcdir;
static const char *serverdir;
static const char *backend_dir;

static HMODULE hmodule;
G_LOCK_DEFINE_STATIC (mutex);

char *
_gconf_win32_replace_prefix (const char *configure_time_path)
{
  if (strncmp (configure_time_path, PREFIX "/", strlen (PREFIX) + 1) == 0)
    {
      return g_strconcat (runtime_prefix,
			  configure_time_path + strlen (PREFIX),
			  NULL);
    }
  else
    return g_strdup (configure_time_path);
}

/* Minimal DllMain used to only tuck away the libgconf DLL's HMODULE */
BOOL WINAPI
DllMain (HINSTANCE hinstDLL,
	 DWORD     fdwReason,
	 LPVOID    lpvReserved)
{
  switch (fdwReason)
    {
    case DLL_PROCESS_ATTACH:
      hmodule = hinstDLL;
      break;
    }
  return TRUE;
}

static void
setup (void)
{
  char *full_prefix = NULL;
  char *cp_prefix; 

  wchar_t wcbfr[1000];
  
  G_LOCK (mutex);
  if (locale_dir != NULL)
    {
      G_UNLOCK (mutex);
      return;
    }

  if (GetModuleFileNameW (hmodule, wcbfr, G_N_ELEMENTS (wcbfr)))
    {
      full_prefix = g_utf16_to_utf8 (wcbfr, -1, NULL, NULL, NULL);
      if (GetShortPathNameW (wcbfr, wcbfr, G_N_ELEMENTS (wcbfr)))
	cp_prefix = g_utf16_to_utf8 (wcbfr, -1, NULL, NULL, NULL);
      else if (full_prefix)
	cp_prefix = g_locale_from_utf8 (full_prefix, -1, NULL, NULL, NULL);
    }

  if (full_prefix != NULL)
    {
      gchar *p = strrchr (full_prefix, '\\');
      if (p != NULL)
	*p = '\0';

      p = strrchr (full_prefix, '\\');
      if (p && (g_ascii_strcasecmp (p + 1, "bin") == 0))
	*p = '\0';
		  
      /* Replace backslashes with forward slashes to avoid
       * problems for instance in makefiles that use
       * gconftool-2 --get-default-source.
       */
      while ((p = strchr (full_prefix, '\\')) != NULL)
	*p = '/';

      runtime_prefix = full_prefix;
    }
  else
    {
      runtime_prefix = g_strdup ("");
    }

  if (cp_prefix != NULL)
    {
      gchar *p = _mbsrchr (cp_prefix, '\\');
      if (p != NULL)
	*p = '\0';
      
      p = _mbsrchr (cp_prefix, '\\');
      if (p && (g_ascii_strcasecmp (p + 1, "bin") == 0))
	*p = '\0';
    }
  else
    {
      cp_prefix = g_strdup ("");
    }

  locale_dir = g_strconcat (cp_prefix, GCONF_LOCALE_DIR + strlen (PREFIX), NULL);

  confdir = _gconf_win32_replace_prefix (GCONF_CONFDIR);
  etcdir = _gconf_win32_replace_prefix (GCONF_ETCDIR);
  serverdir = _gconf_win32_replace_prefix (GCONF_SERVERDIR);
  backend_dir = _gconf_win32_replace_prefix (GCONF_BACKEND_DIR);

  G_UNLOCK (mutex);
}

#define GETTER(varbl)				\
const char *					\
_gconf_win32_get_##varbl (void)		\
{						\
	setup ();				\
        return varbl;				\
}

GETTER (locale_dir)
GETTER (confdir)
GETTER (etcdir)
GETTER (serverdir)
GETTER (backend_dir)
