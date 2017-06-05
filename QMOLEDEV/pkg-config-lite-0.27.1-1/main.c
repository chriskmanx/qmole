/*
 * Copyright (C) 2001, 2002 Red Hat Inc.
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License as
 * published by the Free Software Foundation; either version 2 of the
 * License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
 * 02111-1307, USA.
 */

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include "pkg.h"
#include "parse.h"

#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <stdio.h>

#ifdef G_OS_WIN32
#define STRICT
#include <windows.h>
#undef STRICT
#endif

static int want_debug_spew = 0;
static int want_verbose_errors = 0;
static int want_stdout_errors = 0;
char *pcsysrootdir = NULL;
char *pkg_config_pc_path = NULL;

void
debug_spew (const char *format, ...)
{
  va_list args;
  gchar *str;
  FILE* stream;

//  g_return_if_fail (format != NULL);
  if(!format)
    return;

  if (!want_debug_spew)
    return;

  va_start (args, format);
  str = g_strdup_vprintf (format, args);
  va_end (args);

  if (want_stdout_errors)
    stream = stdout;
  else
    stream = stderr;

  fputs (str, stream);
  fflush (stream);

  g_free (str);
}

void
verbose_error (const char *format, ...)
{
  va_list args;
  gchar *str;
  FILE* stream;

//  g_return_if_fail (format != NULL);
  if(!format)
    return;

  if (!want_verbose_errors)
    return;

  va_start (args, format);
  str = g_strdup_vprintf (format, args);
  va_end (args);

  if (want_stdout_errors)
    stream = stdout;
  else
    stream = stderr;

  fputs (str, stream);
  fflush (stream);

  g_free (str);
}

static gboolean
define_variable_cb (const char *opt, const char *arg, gpointer data,
                    GError **error)
{
  char *varname;
  char *varval;
  char *tmp;

  tmp = g_strdup (arg);

  varname = tmp;
  while (*varname && isspace ((guchar)*varname))
    ++varname;

  varval = varname;
  while (*varval && *varval != '=' && *varval != ' ')
    ++varval;

  while (*varval && (*varval == '=' || *varval == ' '))
    {
      *varval = '\0';
      ++varval;
    }

  if (*varval == '\0')
    {
      fprintf (stderr, "--define-variable argument does not have a value "
               "for the variable\n");
      exit (1);
    }

  define_global_variable (varname, varval);

  g_free (tmp);
  return TRUE;
}

static gboolean
pkg_uninstalled (Package *pkg)
{
  /* See if > 0 pkgs were uninstalled */
  GSList *tmp;

  if (pkg->uninstalled)
    return TRUE;

  tmp = pkg->requires;
  while (tmp != NULL)
    {
      Package *pkg = tmp->data;

      if (pkg_uninstalled (pkg))
        return TRUE;

      tmp = g_slist_next (tmp);
    }

  return FALSE;
}

void
print_hashtable_key (gpointer key,
                     gpointer value,
                     gpointer user_data)
{
  printf("%s\n", (gchar*)key);
}

static void
init_pc_path (void)
{
#ifdef G_OS_WIN32
//  char *instdir, *lpath, *shpath;
//
//  instdir = g_win32_get_package_installation_directory_of_module (NULL);
//  if (instdir == NULL)
//    {
//      /* This only happens when GetModuleFilename() fails. If it does, that
//       * failure should be investigated and fixed.
//       */
//      debug_spew ("g_win32_get_package_installation_directory_of_module failed\n");
//      return;
//    }
//
//  lpath = g_build_filename (instdir, "lib", "pkgconfig", NULL);
//  shpath = g_build_filename (instdir, "share", "pkgconfig", NULL);
//  pkg_config_pc_path = g_strconcat (lpath, G_SEARCHPATH_SEPARATOR_S, shpath,
//                                    NULL);
//  g_free (instdir);
//  g_free (lpath);
//  g_free (shpath);
  char *lpath, *shpath;

  lpath   = g_win32_get_package_installation_subdir("lib/pkgconfig");
  shpath  = g_win32_get_package_installation_subdir("share/pkgconfig");
  pkg_config_pc_path = g_strconcat (lpath, G_SEARCHPATH_SEPARATOR_S, shpath,
                                    NULL);
  g_free (lpath);
  g_free (shpath);
#else
  pkg_config_pc_path = PKG_CONFIG_PC_PATH;
#endif
}

int
main (int argc, char **argv)
{
  static int want_my_version = 0;
  static int want_version = 0;
  static int want_libs = 0;
  static int want_cflags = 0;
  static int want_l_libs = 0;
  static int want_L_libs = 0;
  static int want_other_libs = 0;
  static int want_I_cflags = 0;
  static int want_other_cflags = 0;
  static int want_list = 0;
  static int want_static_lib_list = ENABLE_INDIRECT_DEPS;
  static int want_short_errors = 0;
  static int want_uninstalled = 0;
  static char *variable_name = NULL;
  static int want_exists = 0;
  static int want_provides = 0;
  static int want_requires = 0;
  static int want_requires_private = 0;
  static char *required_atleast_version = NULL;
  static char *required_exact_version = NULL;
  static char *required_max_version = NULL;
  static char *required_pkgconfig_version = NULL;
  static int want_silence_errors = 0;
  static int want_variable_list = 0;
  GString *str;
  GSList *packages = NULL;
  char *search_path;
  char *pcbuilddir;
  gboolean need_newline;
  FILE *log = NULL;
  GError *error = NULL;
  GOptionContext *opt_context;

  GOptionEntry options_table[] = {
    { "version", 0, 0, G_OPTION_ARG_NONE, &want_my_version,
      "output version of pkg-config", NULL },
    { "modversion", 0, 0, G_OPTION_ARG_NONE, &want_version,
      "output version for package", NULL },
    { "atleast-pkgconfig-version", 0, 0, G_OPTION_ARG_STRING,
      &required_pkgconfig_version,
      "require given version of pkg-config", "VERSION" },
    { "libs", 0, 0, G_OPTION_ARG_NONE, &want_libs,
      "output all linker flags", NULL },
    { "static", 0, 0, G_OPTION_ARG_NONE, &want_static_lib_list,
      "output linker flags for static linking", NULL },
    { "short-errors", 0, 0, G_OPTION_ARG_NONE, &want_short_errors,
      "print short errors", NULL },
    { "libs-only-l", 0, 0, G_OPTION_ARG_NONE, &want_l_libs,
      "output -l flags", NULL },
    { "libs-only-other", 0, 0, G_OPTION_ARG_NONE, &want_other_libs,
      "output other libs (e.g. -pthread)", NULL },
    { "libs-only-L", 0, 0, G_OPTION_ARG_NONE, &want_L_libs,
      "output -L flags", NULL },
    { "cflags", 0, 0, G_OPTION_ARG_NONE, &want_cflags,
      "output all pre-processor and compiler flags", NULL },
    { "cflags-only-I", 0, 0, G_OPTION_ARG_NONE, &want_I_cflags,
      "output -I flags", NULL },
    { "cflags-only-other", 0, 0, G_OPTION_ARG_NONE, &want_other_cflags,
      "output cflags not covered by the cflags-only-I option", NULL },
    { "variable", 0, 0, G_OPTION_ARG_STRING, &variable_name,
      "get the value of variable named NAME", "NAME" },
    { "define-variable", 0, 0, G_OPTION_ARG_CALLBACK, &define_variable_cb,
      "set variable NAME to VALUE", "NAME=VALUE" },
    { "exists", 0, 0, G_OPTION_ARG_NONE, &want_exists,
      "return 0 if the module(s) exist", NULL },
    { "print-variables", 0, 0, G_OPTION_ARG_NONE, &want_variable_list,
      "output list of variables defined by the module", NULL },
    { "uninstalled", 0, 0, G_OPTION_ARG_NONE, &want_uninstalled,
      "return 0 if the uninstalled version of one or more module(s) "
      "or their dependencies will be used", NULL },
    { "atleast-version", 0, 0, G_OPTION_ARG_STRING, &required_atleast_version,
      "return 0 if the module is at least version VERSION", "VERSION" },
    { "exact-version", 0, 0, G_OPTION_ARG_STRING, &required_exact_version,
      "return 0 if the module is at exactly version VERSION", "VERSION" },
    { "max-version", 0, 0, G_OPTION_ARG_STRING, &required_max_version,
      "return 0 if the module is at no newer than version VERSION", "VERSION" },
    { "list-all", 0, 0, G_OPTION_ARG_NONE, &want_list,
      "list all known packages", NULL },
    { "debug", 0, 0, G_OPTION_ARG_NONE, &want_debug_spew,
      "show verbose debug information", NULL },
    { "print-errors", 0, 0, G_OPTION_ARG_NONE, &want_verbose_errors,
      "show verbose information about missing or conflicting packages,"
      "default if --cflags or --libs given on the command line", NULL },
    { "silence-errors", 0, 0, G_OPTION_ARG_NONE, &want_silence_errors,
      "be silent about errors (default unless --cflags or --libs"
      "given on the command line)", NULL },
    { "errors-to-stdout", 0, 0, G_OPTION_ARG_NONE, &want_stdout_errors,
      "print errors from --print-errors to stdout not stderr", NULL },
    { "print-provides", 0, 0, G_OPTION_ARG_NONE, &want_provides,
      "print which packages the package provides", NULL },
    { "print-requires", 0, 0, G_OPTION_ARG_NONE, &want_requires,
      "print which packages the package requires", NULL },
    { "print-requires-private", 0, 0, G_OPTION_ARG_NONE, &want_requires_private,
      "print which packages the package requires for static linking", NULL },
#ifdef G_OS_WIN32
    { "dont-define-prefix", 0, 0, G_OPTION_ARG_NONE, &dont_define_prefix,
      "don't try to override the value of prefix for each .pc file found with "
      "a guesstimated value based on the location of the .pc file", NULL },
    { "prefix-variable", 0, 0, G_OPTION_ARG_STRING, &prefix_variable,
      "set the name of the variable that pkg-config automatically sets",
      "PREFIX" },
    { "msvc-syntax", 0, 0, G_OPTION_ARG_NONE, &msvc_syntax,
      "output -l and -L flags for the Microsoft compiler (cl)", NULL },
#endif
    { NULL, 0, 0, 0, NULL, NULL, NULL }
  };

  /* This is here so that we get debug spew from the start,
   * during arg parsing
   */
  if (getenv ("PKG_CONFIG_DEBUG_SPEW"))
    {
      want_debug_spew = TRUE;
      want_verbose_errors = TRUE;
      want_silence_errors = FALSE;
      debug_spew ("PKG_CONFIG_DEBUG_SPEW variable enabling debug spew\n");
    }


  /* Get the built-in search path */
  init_pc_path ();
  if (pkg_config_pc_path == NULL)
    {
      /* Even when we override the built-in search path, we still use it later
       * to add pc_path to the virtual pkg-config package.
       */
      verbose_error ("Failed to get default search path\n");
      exit (1);
    }

  search_path = getenv ("PKG_CONFIG_PATH");
  if (search_path)
    {
      add_search_dirs(search_path, G_SEARCHPATH_SEPARATOR_S);
    }
  if (getenv("PKG_CONFIG_LIBDIR") != NULL)
    {
      add_search_dirs(getenv("PKG_CONFIG_LIBDIR"), G_SEARCHPATH_SEPARATOR_S);
    }
  else
    {
      add_search_dirs(pkg_config_pc_path, G_SEARCHPATH_SEPARATOR_S);
    }

  pcsysrootdir = getenv ("PKG_CONFIG_SYSROOT_DIR");
  if (pcsysrootdir)
    {
      define_global_variable ("pc_sysrootdir", pcsysrootdir);
    }
  else
    {
      define_global_variable ("pc_sysrootdir", "/");
    }

  pcbuilddir = getenv ("PKG_CONFIG_TOP_BUILD_DIR");
  if (pcbuilddir)
    {
      define_global_variable ("pc_top_builddir", pcbuilddir);
    }
  else
    {
      /* Default appropriate for automake */
      define_global_variable ("pc_top_builddir", "$(top_builddir)");
    }

  if (getenv ("PKG_CONFIG_DISABLE_UNINSTALLED"))
    {
      debug_spew ("disabling auto-preference for uninstalled packages\n");
      disable_uninstalled = TRUE;
    }

  /* Parse options */
  opt_context = g_option_context_new (NULL);
  g_option_context_add_main_entries (opt_context, options_table, NULL);
  if (!g_option_context_parse(opt_context, &argc, &argv, &error))
    {
      fprintf (stderr, "%s\n", error->message);
      return 1;
    }


  /* Error printing is determined as follows:
   *     - for --cflags, --libs, etc. it's on by default
   *       and --silence-errors can turn it off
   *     - for --exists, --max-version, etc. and no options
   *       at all, it's off by default and --print-errors
   *       will turn it on
   */

  if (want_my_version ||
      want_version ||
      want_libs ||
      want_cflags ||
      want_l_libs ||
      want_L_libs ||
      want_other_libs ||
      want_I_cflags ||
      want_other_cflags ||
      want_list ||
      want_variable_list)
    {
      debug_spew ("Error printing enabled by default due to use of --version, --libs, --cflags, --libs-only-l, --libs-only-L, --libs-only-other, --cflags-only-I, --cflags-only-other or --list. Value of --silence-errors: %d\n", want_silence_errors);

      if (want_silence_errors && getenv ("PKG_CONFIG_DEBUG_SPEW") == NULL)
        want_verbose_errors = FALSE;
      else
        want_verbose_errors = TRUE;
    }
  else
    {
      debug_spew ("Error printing disabled by default, value of --print-errors: %d\n",
                  want_verbose_errors);

      /* Leave want_verbose_errors unchanged, reflecting --print-errors */
    }

  if (want_verbose_errors)
    debug_spew ("Error printing enabled\n");
  else
    debug_spew ("Error printing disabled\n");

  if (want_static_lib_list)
    enable_private_libs();
  else
    disable_private_libs();

  /* honor Requires.private if any Cflags are requested or any static
   * libs are requested */

  if (want_I_cflags || want_other_cflags || want_cflags ||
      want_requires_private || want_exists ||
      (want_static_lib_list && (want_libs || want_l_libs || want_L_libs)))
    enable_requires_private();

  /* ignore Requires if no Cflags or Libs are requested */

  if (!want_I_cflags && !want_other_cflags && !want_cflags &&
      !want_libs && !want_l_libs && !want_L_libs && !want_requires &&
      !want_exists)
    disable_requires();

  if (want_my_version)
    {
      printf ("%s\n", VERSION);
      return 0;
    }

  if (required_pkgconfig_version)
    {
      if (compare_versions (VERSION, required_pkgconfig_version) >= 0)
        return 0;
      else
        return 1;
    }

  package_init ();

  if (want_list)
    {
      print_package_list ();
      return 0;
    }

  /* Collect packages from remaining args */
  str = g_string_new ("");
  while (argc > 1)
    {
      argc--;
      argv++;

      g_string_append (str, *argv);
      g_string_append (str, " ");
    }

  g_option_context_free (opt_context);

  g_strstrip (str->str);

  if (getenv("PKG_CONFIG_LOG") != NULL)
    {
      log = fopen (getenv ("PKG_CONFIG_LOG"), "a");
      if (log == NULL)
	{
	  fprintf (stderr, "Cannot open log file: %s\n",
		   getenv ("PKG_CONFIG_LOG"));
	  exit (1);
	}
    }

  {
    gboolean failed = FALSE;
    GSList *reqs;
    GSList *iter;

    reqs = parse_module_list (NULL, str->str,
                              "(command line arguments)");

    iter = reqs;

    while (iter != NULL)
      {
        Package *req;
        RequiredVersion *ver = iter->data;

	/* override requested versions with cmdline options */
	if (required_exact_version)
	  {
	    g_free (ver->version);
	    ver->comparison = EQUAL;
	    ver->version = g_strdup (required_exact_version);
	  }
	else if (required_atleast_version)
	  {
	    g_free (ver->version);
	    ver->comparison = GREATER_THAN_EQUAL;
	    ver->version = g_strdup (required_atleast_version);
	  }
	else if (required_max_version)
	  {
	    g_free (ver->version);
	    ver->comparison = LESS_THAN_EQUAL;
	    ver->version = g_strdup (required_max_version);
	  }

        if (want_short_errors)
          req = get_package_quiet (ver->name);
        else
          req = get_package (ver->name);

	if (log != NULL)
	  {
	    if (req == NULL)
	      fprintf (log, "%s NOT-FOUND", ver->name);
	    else
	      fprintf (log, "%s %s %s", ver->name,
		       comparison_to_str (ver->comparison),
		       (ver->version == NULL) ? "(null)" : ver->version);
	    fprintf (log, "\n");
	  }

        if (req == NULL)
          {
            failed = TRUE;
            verbose_error ("No package '%s' found\n", ver->name);
            goto nextiter;
          }

        if (!version_test (ver->comparison, req->version, ver->version))
          {
            failed = TRUE;
            verbose_error ("Requested '%s %s %s' but version of %s is %s\n",
                           ver->name,
                           comparison_to_str (ver->comparison),
                           ver->version,
                           req->name,
                           req->version);

	    if (req->url)
	      verbose_error ("You may find new versions of %s at %s\n",
			     req->name, req->url);

            goto nextiter;
          }

        packages = g_slist_prepend (packages, req);

      nextiter:
        iter = g_slist_next (iter);
      }

    if (log != NULL)
      {
	fclose (log);
      }

    if (failed) {
      return 1;
    }

  if (want_variable_list)
    {
      GSList *tmp;
      tmp = packages;
      while (tmp != NULL)
        {
          Package *pkg = tmp->data;
          g_hash_table_foreach(pkg->vars,
                               &print_hashtable_key,
                               NULL);
          tmp = g_slist_next (tmp);
          if (tmp) printf ("\n");
        }
      need_newline = FALSE;
    }

  }

  g_string_free (str, TRUE);

  packages = g_slist_reverse (packages);

  if (packages == NULL)
    {
      fprintf (stderr, "Must specify package names on the command line\n");

      exit (1);
    }

  if (want_exists)
    return 0; /* if we got here, all the packages existed. */

  if (want_uninstalled)
    {
      /* See if > 0 pkgs (including dependencies recursively) were uninstalled */
      GSList *tmp;
      tmp = packages;
      while (tmp != NULL)
        {
          Package *pkg = tmp->data;

          if (pkg_uninstalled (pkg))
            return 0;

          tmp = g_slist_next (tmp);
        }

      return 1;
    }

  if (want_version)
    {
      GSList *tmp;
      tmp = packages;
      while (tmp != NULL)
        {
          Package *pkg = tmp->data;

          printf ("%s\n", pkg->version);

          tmp = g_slist_next (tmp);
        }
    }

 if (want_provides)
   {
     GSList *tmp;
     tmp = packages;
     while (tmp != NULL)
       {
         Package *pkg = tmp->data;
         char *key;
         key = pkg->key;
         while (*key == '/')
           key++;
         if (strlen(key) > 0)
           printf ("%s = %s\n", key, pkg->version);
         tmp = g_slist_next (tmp);
       }
   }

  if (want_requires)
    {
      GSList *pkgtmp;
      for (pkgtmp = packages; pkgtmp != NULL; pkgtmp = g_slist_next (pkgtmp))
        {
          Package *pkg = pkgtmp->data;
          GSList *reqtmp;

          /* process Requires: */
          for (reqtmp = pkg->requires; reqtmp != NULL; reqtmp = g_slist_next (reqtmp))
            {
              Package *deppkg = reqtmp->data;
              RequiredVersion *req;
              req = g_hash_table_lookup(pkg->required_versions, deppkg->key);
              if ((req == NULL) || (req->comparison == ALWAYS_MATCH))
                printf ("%s\n", deppkg->key);
              else
                printf ("%s %s %s\n", deppkg->key,
                  comparison_to_str(req->comparison),
                  req->version);
            }
        }
    }
  if (want_requires_private)
    {
      GSList *pkgtmp;
      for (pkgtmp = packages; pkgtmp != NULL; pkgtmp = g_slist_next (pkgtmp))
        {
          Package *pkg = pkgtmp->data;
          GSList *reqtmp;
          /* process Requires.private: */
          for (reqtmp = pkg->requires_private; reqtmp != NULL; reqtmp = g_slist_next (reqtmp))
            {

              Package *deppkg = reqtmp->data;
              RequiredVersion *req;

              if (g_slist_find (pkg->requires, reqtmp->data))
                continue;

              req = g_hash_table_lookup(pkg->required_versions, deppkg->key);
              if ((req == NULL) || (req->comparison == ALWAYS_MATCH))
                printf ("%s\n", deppkg->key);
              else
                printf ("%s %s %s\n", deppkg->key,
                  comparison_to_str(req->comparison),
                  req->version);
            }
        }
    }

  /* Print all flags; then print a newline at the end. */
  need_newline = FALSE;

  if (variable_name)
    {
      char *str = packages_get_var (packages, variable_name);
      printf ("%s", str);
      g_free (str);
      need_newline = TRUE;
    }

  if (want_I_cflags)
    {
      char *str = packages_get_I_cflags (packages);
      printf ("%s ", str);
      g_free (str);
      need_newline = TRUE;
    }
  else if (want_other_cflags)
    {
      char *str = packages_get_other_cflags (packages);
      printf ("%s ", str);
      g_free (str);
      need_newline = TRUE;
    }
  else if (want_cflags)
    {
      char *str = packages_get_all_cflags (packages);
      printf ("%s ", str);
      g_free (str);
      need_newline = TRUE;
    }

  if (want_l_libs)
    {
      char *str = packages_get_l_libs (packages);
      printf ("%s ", str);
      g_free (str);
      need_newline = TRUE;
    }
  else if (want_L_libs)
    {
      char *str = packages_get_L_libs (packages);
      printf ("%s ", str);
      g_free (str);
      need_newline = TRUE;
    }
  else if (want_other_libs)
    {
      char *str = packages_get_other_libs (packages);
      printf ("%s ", str);
      g_free (str);
      need_newline = TRUE;
    }
  else if (want_libs)
    {
      char *str = packages_get_all_libs (packages);
      printf ("%s ", str);
      g_free (str);
      need_newline = TRUE;
    }

  if (need_newline)
    printf ("\n");

  return 0;
}
