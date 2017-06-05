/**************************************************************************

    orbit-idl-main.c (Driver program for the IDL parser & backend)

    Copyright (C) 1999 Elliot Lee

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

    $Id$

***************************************************************************/

#include "config.h"
#include <stdio.h>
#include <stdlib.h>
#include <errno.h>
#include <string.h>
#include <glib.h>
#include <libIDL/IDL.h>
#include <glib/gi18n.h>

#include "orbit-idl2.h"

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

/* FIXME: this program doesn't seem to support i18n? */
#ifndef GETTEXT_PACKAGE
#define GETTEXT_PACKAGE NULL
#endif

/* Settings made from the command line (prefaced with cl_) */
static gboolean cl_disable_stubs = FALSE,
  cl_disable_skels = FALSE,
  cl_disable_common = FALSE,
  cl_disable_headers = FALSE,
  cl_enable_skeleton_impl = FALSE;
static int cl_idlwarnlevel = 2;
static int cl_debuglevel = 0;
static int cl_is_pidl = 0;
static int cl_disable_idata = 0;
static int cl_enable_imodule = 0;
static int cl_add_imodule = 0;
static gboolean cl_disable_defs_skels = FALSE;
static gboolean cl_showcpperrors = FALSE;
static char *cl_output_lang = "c";
static char *cl_header_guard_prefix = "";
static char *cl_backend_dir = NULL;
static gboolean cl_onlytop = FALSE;
static char *cl_deps_file = NULL;
static char *cl_output_directory = "";
static char **idl_files = NULL;

#define BASE_CPP_ARGS "-D__ORBIT_IDL__ "
static GString *cl_cpp_args;

/* Callbacks for goption */

static gboolean
cl_libIDL_version_callback (const char *option_name,
			    const char *value,
			    gpointer data,
			    GError **error)
{
  g_print("libIDL %s (CORBA %s)\n",
	  IDL_get_libver_string(),
	  IDL_get_IDLver_string());
  exit(0);
}

static gboolean
cl_cpp_define_callback (const char *option_name,
			const char *value,
			gpointer data,
			GError **error)
{
  g_string_append_printf (cl_cpp_args, "-D%s ", value);
  return TRUE;
}

static gboolean
cl_cpp_include_callback (const char *option_name,
			 const char *value,
			 gpointer data,
			 GError **error)
{
  g_string_append_printf (cl_cpp_args, "-I%s ", value);
  return TRUE;
}

static gboolean
cl_version_callback (const char *option_name,
		     const char *value,
		     gpointer data,
		     GError **error)
{
  g_print ("orbit-idl-2 %s - serial %d\n",
           VERSION, ORBIT_CONFIG_SERIAL);
  exit(0);
}

static gboolean
cl_c_output_formatter_callback (const char *option_name,
				const char *value,
				gpointer data,
				GError **error)
{
  g_warning ("Do not use the 'c-output-formatter' option. It is ignored and will soon go away.");
  return TRUE;
}

static const GOptionEntry cl_libIDL_goptions[] = {
  { "libIDL-version", 0, 0, G_OPTION_ARG_CALLBACK, cl_libIDL_version_callback, N_("Show version of libIDL used"), NULL },
  { NULL }
};

static const GOptionEntry cl_cpp_goptions[] = {
  { "define", 'D', G_OPTION_FLAG_FILENAME, G_OPTION_ARG_CALLBACK, cl_cpp_define_callback, N_("Define value in preprocessor"), N_("DEFINE") },
  { "include", 'I', G_OPTION_FLAG_FILENAME, G_OPTION_ARG_CALLBACK, cl_cpp_include_callback, N_("Add search path for include files"), N_("DIR") },
  { NULL }
};

static const GOptionEntry goptions[] = {
  { "version", 'v', 0, G_OPTION_ARG_CALLBACK, cl_version_callback, N_("Output compiler version and serial"), NULL },
  { "lang", 'l', 0, G_OPTION_ARG_STRING, &cl_output_lang, N_("Output language (default is C)"), N_("LANG") },
  { "debug", 'd', 0, G_OPTION_ARG_INT, &cl_debuglevel, N_("Debug level (0 to 4)"), N_("LEVEL") },
  { "idlwarnlevel", 0, 0, G_OPTION_ARG_INT, &cl_idlwarnlevel, N_("IDL warning level (0 to 4, default is 2)"), N_("LEVEL") },
  { "showcpperrors", 0, 0, G_OPTION_ARG_NONE, &cl_showcpperrors, N_("Show CPP errors"), NULL },
  { "nostubs", 0, 0, G_OPTION_ARG_NONE, &cl_disable_stubs, N_("Don't output stubs"), NULL },
  { "noskels", 0, 0, G_OPTION_ARG_NONE, &cl_disable_skels, N_("Don't output skels"), NULL },
  { "nocommon", 0, 0, G_OPTION_ARG_NONE, &cl_disable_common, N_("Don't output common"), NULL },
  { "noheaders", 0, 0, G_OPTION_ARG_NONE, &cl_disable_headers, N_("Don't output headers"), NULL },
  { "noidata", 0, 0, G_OPTION_ARG_NONE, &cl_disable_idata, N_("Don't generate Interface type data"), NULL },
  { "imodule", 'i', 0, G_OPTION_ARG_NONE, &cl_enable_imodule, N_("Output only an imodule file"), NULL },
  { "add-imodule", 0, 0, G_OPTION_ARG_NONE, &cl_add_imodule, N_("Output an imodule file"), NULL },
  { "skeleton-impl", 0, 0, G_OPTION_ARG_NONE, &cl_enable_skeleton_impl, N_("Output skeleton implementation"), NULL },
  { "backenddir", 0, 0, G_OPTION_ARG_FILENAME, &cl_backend_dir, N_("Override IDL backend library directory"), N_("DIR") },
  { "c-output-formatter", 0, G_OPTION_FLAG_HIDDEN, G_OPTION_ARG_CALLBACK, &cl_c_output_formatter_callback, "DEPRECATED and IGNORED", "PROGRAM" },
  { "onlytop", 0, 0, G_OPTION_ARG_NONE, &cl_onlytop, N_("Inhibit includes"), NULL },
  { "pidl", 0, 0, G_OPTION_ARG_NONE, &cl_is_pidl, N_("Treat as Pseudo IDL"), NULL },
  { "nodefskels", 0, 0, G_OPTION_ARG_NONE, &cl_disable_defs_skels, N_("Don't output defs for skels in header"), NULL },
  { "deps", 0, 0,  G_OPTION_ARG_FILENAME, &cl_deps_file, N_("Generate dependency info suitable for inclusion in Makefile"), N_("FILENAME") },
  { "headerguardprefix", 0, 0, G_OPTION_ARG_STRING, &cl_header_guard_prefix, N_("Prefix for #ifdef header guards. Sometimes useful to avoid conflicts."), N_("PREFIX") },
  { "output-dir", 0, 0, G_OPTION_ARG_FILENAME, &cl_output_directory, N_("Where to put generated files. This directory must exist."), N_("DIR") },
  { G_OPTION_REMAINING, 0, 0, G_OPTION_ARG_FILENAME_ARRAY, &idl_files, NULL, N_("<IDL files>") },
  { NULL }
};

/********** main routines **********/
int main(int argc, char *argv[])
{
  GOptionContext *context;
  GOptionGroup *group;
  GError *error = NULL;
  OIDL_Run_Info rinfo;
  GPtrArray *args;
  int retval = 0;
  gboolean result;
  gpointer freeme;
  guint i;

  /* Argument parsing, etc. */
  cl_cpp_args = g_string_new("-D__ORBIT_IDL__ ");

  /* GOption cannot parse single-letter options without space
   * between option and argument (-I../dir) or single-dash options.
   * So fix those up before parsing, to retain command line compatibility
   * with previous popt based versions of orbit-idl-2
   */
  args = g_ptr_array_sized_new (2 * argc);
  g_ptr_array_add (args, g_strdup (argv[0]));

  for (i = 1; i < argc; ++i) {
    if (argv[i][0] == '-' &&
	(argv[i][1] == 'D' || argv[i][1] == 'I') &&
	argv[i][2] != '\0') {
      g_ptr_array_add (args, g_strndup (argv[i], 2));
      g_ptr_array_add (args, g_strdup (argv[i] + 2));
    } else if (strcmp (argv[i], "-define") == 0 ||
               strcmp (argv[i], "-include") == 0) {
      g_ptr_array_add (args, g_strdup_printf ("-%s", argv[i]));
    } else {
      g_ptr_array_add (args, g_strdup (argv[i]));
    }
  }

  /* Parsing will modify the array; memdup it beforehand
   * so we can free it correctly
   */
  argc = args->len;
  argv = freeme = g_memdup (args->pdata, argc * sizeof (gpointer));
  g_ptr_array_add (args, NULL); /* so we can use g_strfreev */

  /* Now parse the options */
  context = g_option_context_new ("");
  g_option_context_add_main_entries (context, goptions, GETTEXT_PACKAGE);

  group = g_option_group_new ("libIDL", N_("libIDL options"), N_("Show libIDL options"), NULL, NULL);
  g_option_group_set_translation_domain (group, GETTEXT_PACKAGE);
  g_option_group_add_entries (group, cl_libIDL_goptions);
  g_option_context_add_group (context, group);

  group = g_option_group_new ("cpp", N_("Preprocessor options"), N_("Show preprocessor options"), NULL, NULL);
  g_option_group_set_translation_domain (group, GETTEXT_PACKAGE);
  g_option_group_add_entries (group, cl_cpp_goptions);
  g_option_context_add_group (context, group);

  result = g_option_context_parse (context, &argc, &argv, &error);
 
  g_option_context_free (context);
  g_strfreev ((char **) g_ptr_array_free (args, FALSE));
  g_free (freeme);

  if (!result) {
    g_print ("orbit-idl-2: %s\n", error->message);
    g_error_free (error);
    exit (1);
  }

  if (!idl_files) {
    g_print ("No input files given!\n");
    exit (1);
  }

  /* Prep our run info for the backend */
  rinfo.cpp_args = cl_cpp_args->str;
  rinfo.debug_level = cl_debuglevel;
  rinfo.idl_warn_level = cl_idlwarnlevel;
  rinfo.show_cpp_errors = cl_showcpperrors;
  rinfo.is_pidl = cl_is_pidl;
  rinfo.do_skel_defs = !cl_disable_defs_skels;
  rinfo.enabled_passes =
     (cl_disable_stubs?0:OUTPUT_STUBS)
    |(cl_disable_skels?0:OUTPUT_SKELS)
    |(cl_disable_common?0:OUTPUT_COMMON)
    |(cl_disable_headers?0:OUTPUT_HEADERS)
    |(cl_enable_skeleton_impl?OUTPUT_SKELIMPL:0)
    |(cl_add_imodule?OUTPUT_IMODULE:0)
    |(cl_deps_file != NULL?OUTPUT_DEPS:0);

  rinfo.deps_file = cl_deps_file;

  if (cl_enable_imodule) /* clobber */
    rinfo.enabled_passes =
      OUTPUT_COMMON | OUTPUT_HEADERS | OUTPUT_IMODULE;

  rinfo.output_language = cl_output_lang;
  rinfo.header_guard_prefix = cl_header_guard_prefix;
  rinfo.output_directory = cl_output_directory;
  rinfo.backend_directory = cl_backend_dir;
  rinfo.onlytop = cl_onlytop;
  rinfo.idata = !cl_disable_idata;
  
  printf ("orbit-idl-2 " VERSION " compiling\n");
  printf (" %s mode, %s preprocessor errors, passes: %s%s%s%s%s%s\n\n",
	  rinfo.is_pidl ? "pidl" : "",
	  rinfo.show_cpp_errors ? "show" : "hide",
	  cl_disable_stubs ? "" : "stubs ",
	  cl_disable_skels ? "" : "skels ",
	  cl_disable_common ? "" : "common ",
	  cl_disable_headers ? "" : "headers ",
	  cl_enable_skeleton_impl ? "skel_impl " : "",
	  cl_enable_imodule ? "imodule" : "");
	   
  /* Do it */
  for (i = 0; idl_files[i]; ++i) {
    g_print ("Processing file %s\n", idl_files[i]);
    rinfo.input_filename = idl_files[i];
    if (!orbit_idl_to_backend(idl_files[i], &rinfo)) {
      g_warning("%s compilation failed", idl_files[i]);
      retval = 1;
    }
  }

  exit (retval);
}
