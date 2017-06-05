/* -*- Mode: C; c-basic-offset: 4 -*-
 * libglade - a library for building interfaces from XML files at runtime
 * Copyright (C) 1998-2002  James Henstridge <james@daa.com.au>
 *
 * glade-init.c: initialisation functions for libglade
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
 * Free Software Foundation, Inc., 59 Temple Place - Suite 330,
 * Boston, MA  02111-1307, USA.
 */
#ifdef HAVE_CONFIG_H
#  include <config.h>
#endif

#include <string.h>
#include <glib.h>
#include <gmodule.h>

#include <pango/pango-utils.h>

#include "glade-init.h"
#include "glade-build.h"
#include "glade-private.h"

#ifdef DEBUG
guint _glade_debug_flags = 0;
static const  GDebugKey libglade_debug_keys[] = {
    { "parser", GLADE_DEBUG_PARSER },
    { "build",  GLADE_DEBUG_BUILD },
};
static const guint libglade_ndebug_keys = G_N_ELEMENTS(libglade_debug_keys);
#endif


void _glade_init_gtk_widgets (void);

/**
 * glade_init:
 * 
 * It used to be necessary to call glade_init() before creating
 * GladeXML objects.  This is now no longer the case, as libglade will
 * be initialised on demand now.  Calling glade_init() manually will
 * not cause any problems though.
 */
void
glade_init(void)
{
    static gboolean initialised = FALSE;
#ifdef DEBUG
    const gchar *env_string;
#endif

    if (initialised) return;
    initialised = TRUE;
    _glade_init_gtk_widgets();

#ifdef DEBUG
    env_string = g_getenv("LIBGLADE_DEBUG");
    if (env_string != NULL) {
	_glade_debug_flags = g_parse_debug_string (env_string,
						   libglade_debug_keys,
						   libglade_ndebug_keys);
	env_string = NULL;
    }
#endif

}

gchar *
glade_module_check_version(gint version)
{
  if (version != GLADE_MODULE_API_VERSION)
    return "Wrong plugin API version";
  else
    return NULL;
}

static GPtrArray *loaded_packages = NULL;

static gchar **
get_module_path (void)
{
    const gchar *module_path_env = g_getenv ("LIBGLADE_MODULE_PATH");
    const gchar *exe_prefix = g_getenv("LIBGLADE_EXE_PREFIX");
    gchar **result;
    gchar *module_path;
    gchar *default_dir;
    
    if (exe_prefix)
	default_dir = g_build_filename (exe_prefix, "lib", NULL);
    else
	default_dir = g_strdup (GLADE_LIBDIR);
    
    module_path = g_strconcat (module_path_env ? module_path_env : "",
			       module_path_env ? G_SEARCHPATH_SEPARATOR_S : "",
			       default_dir, NULL);
    
    result = pango_split_file_list (module_path);
    
    g_free (default_dir);
    g_free (module_path);
    
    return result;
}

static GModule *
find_module (gchar      **module_path,
	     const gchar *subdir,
	     const gchar *name)
{
    GModule *module;
    gchar *module_name;
    gint i;
    
    if (g_path_is_absolute (name))
	return g_module_open (name, G_MODULE_BIND_LAZY);
    
    for (i = 0; module_path[i]; i++) {
	gchar *version_directory;
	
#ifndef G_OS_WIN32 /* ignoring GTK_BINARY_VERSION elsewhere too */
	version_directory = g_build_filename (module_path[i], subdir, NULL);
	module_name = g_module_build_path (version_directory, name);
	g_free (version_directory);
	
	/*g_print ("trying: %s\n", module_name);*/

	if (g_file_test (module_name, G_FILE_TEST_EXISTS)) {
	    module = g_module_open (module_name, G_MODULE_BIND_LAZY);
	    g_free (module_name);
	    return module;
	}
      
	g_free (module_name);
#endif
	
	module_name = g_module_build_path (module_path[i], name);
	
	if (g_file_test (module_name, G_FILE_TEST_EXISTS)) {
	    module = g_module_open (module_name, G_MODULE_BIND_LAZY);
	    g_free (module_name);
	    return module;
	}
	
	g_free (module_name);
    }
    
    /* As last resort, try loading without an absolute path (using system
     * library path)
     */
    module_name = g_module_build_path (NULL, name);
    module = g_module_open (module_name, G_MODULE_BIND_LAZY);
    g_free(module_name);
    
    return module;
}

/**
 * glade_require:
 * @library: the required library
 *
 * Ensure that a required library is available.  If it is not already
 * available, libglade will attempt to dynamically load a module that
 * contains the handlers for that library.
 */

void
glade_require(const gchar *library)
{
    gboolean already_loaded = FALSE;
    GModule *module;
    void (* init_func)(void);
    static char **module_path = NULL;

    /* a call to glade_init here to make sure libglade is initialised */
    glade_init();

    if (loaded_packages) {
	gint i;

	for (i = 0; i < loaded_packages->len; i++)
	    if (!strcmp(library, g_ptr_array_index(loaded_packages, i))) {
		already_loaded = TRUE;
		break;
	    }
    }

    if (already_loaded)
	return;

    if (!module_path)
	module_path = get_module_path ();

    module = find_module (module_path, "libglade/2.0", library);

    if (!module) {
	g_warning("Could not load support for `%s': %s", library,
		  g_module_error());
	return;
    }

    if (!g_module_symbol(module, "glade_module_register_widgets",
			 (gpointer)&init_func)) {
	g_warning("could not find `%s' init function: %s", library,
		  g_module_error());
	g_module_close(module);
	return;
    }

    init_func();
    g_module_make_resident(module);
}

/**
 * glade_provide:
 * @library: the provided library
 *
 * This function should be called by a module to assert that it
 * provides wrappers for a particular library.  This should be called
 * by the register_widgets() function of a libglade module so that it
 * isn't loaded twice, for instance.
 */

void
glade_provide(const gchar *library)
{
    gboolean already_loaded = FALSE;
    gint i;

    if (!loaded_packages)
	loaded_packages = g_ptr_array_new();

    for (i = 0; i < loaded_packages->len; i++)
	if (!strcmp(library, g_ptr_array_index(loaded_packages, i))) {
	    already_loaded = TRUE;
	    break;
	}

    if (!already_loaded)
	g_ptr_array_add(loaded_packages, g_strdup(library));
}

/**
 * GLADE_MODULE_CHECK_INIT:
 *
 * This macro will insert a suitable version check function into a
 * libglade loadable module.
 */
