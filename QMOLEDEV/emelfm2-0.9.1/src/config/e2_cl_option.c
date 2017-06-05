/* $Id: e2_cl_option.c 3061 2014-02-15 00:45:35Z tpgww $

Copyright (C) 2003-2014 tooar <tooar@emelfm2.net>

This file is part of emelFM2, which is free software. You can redistribute it
and/or modify it under the terms of the GNU General Public License as published
by the Free Software Foundation; either version 3, or (at your option) any
later version.

emelFM2 is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
See the GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with emelFM2; see the file GPL. If not, see http://www.gnu.org/licenses.
*/

/*
@file src/config/e2_cl_option.c
@brief session-start command-line option processing, and help messages display

Glib's option parsing mechanism - GOptionContext and friends - ia not used
because several emelFM2 options may have whitespace inside an argument string -
and that is not supported by GOptionContext (nor is -h for help)
*/

#include "emelfm2.h"
#include <getopt.h>
#include <string.h>
#include "e2_cl_option.h"

static gchar *cwd = NULL;

/**
@brief get copy of @a original, made absolute if needed
Minimal interpretation of @a original - leading ~ or relative directory
@return allocated absolute path string, or copy of @a original, with trailing '/'
*/
static gchar *_e2_cl_option_get_path (const gchar *original)
{
	gchar *ret;
	gchar *path = g_strdup (original);
	g_strstrip (path);
	if (*path != '~' || *(path+1) != G_DIR_SEPARATOR)
	{
		if (cwd == NULL)
			cwd = g_get_current_dir ();
		ret = e2_utils_translate_relative_path	(cwd, path);
	}
	else //*path == '~' || *(path+1) == G_DIR_SEPARATOR
		if (*path == '~')
	{
		ret = e2_utils_translate_relative_path (g_get_home_dir(), path + 2);
	}
	else //*path != '~', *(path+1) == G_DIR_SEPARATOR
	{
		ret = path;
		gint len = strlen (path);
		if (len > 1)
		{
			if (*(path + len - 1) != G_DIR_SEPARATOR)
				ret = e2_utils_strcat (path, G_DIR_SEPARATOR_S);
		}
	}
	if (path != ret)
		g_free (path);
	return ret;
}
/**
@brief print usage/help message on console

@return
*/
static void _e2_cl_option_print_help (void)
{
	guint i, c;
	//NOTE for translators - ensure this contains %s - the application name will be substituted
	const gchar *usage = N_("Usage: %s [option ...]");
	const gchar *help_lines [] =
	{
		"  "                     , N_("Options:"),
		"-1,--one=DIR           ", N_("set 1st pane's start directory to DIR"),
		"-2,--two=DIR           ", N_("set 2nd pane's start directory to DIR"),
		"-a,--share-config      ", N_("also load (but not save) config data in DIR"),
		"-b,--build             ", N_("display build-time parameters"),
		"-c,--config=DIR        ", N_("set config directory to DIR (default: ~/.config/emelfm2)"),
#ifdef DEBUG_MESSAGES
	    "-d,--debug=[1-5]       ", N_("set debug level from 1 (low) to 5 (high)"),
#endif
		"-e,--encoding=TYPE     ", N_("set filesystem character encoding to TYPE"),
		"-f,--fallback-encoding ", N_("set fallback encoding (default: ISO-8859-1)"),
		"-h,--help              ", N_("show this help message"),
		"-i,--ignore-problems   ", N_("ignore encoding/locale problems (at your own risk!)"),
		"-l,--log-all           ", N_("maximise scope of error logging"),
		"-m,--daemon            ", N_("run program as daemon"),
		"-o,--original          ", N_("run program with default settings"),
		"-r,--run-at-start=CMD  ", N_("run command CMD at session start"),
		"-s,--set-option=OPT    ", N_("set one-line gui option using config-file formatted OPT"),
		"-t,--trash=DIR         ", N_("set trash directory to DIR (default: ~/.local/share/Trash/files)"),
		"-u,--user              ", N_("highlight the displayed session user"),
		"-v,--version           ", N_("display version and build info"),
#ifdef DEBUG_MESSAGES
		"-x,--verbose           ", N_("display time/location info in debug messages"),
#endif
		"DIR                    ", N_("same as -2 DIR, except pane 2 is focused"),
	};

	printf (_(usage), BINNAME);
	puts("");
	c = sizeof (help_lines) / sizeof (help_lines[0]);
	for (i = 0; i < c; i += 2)
		printf ("%s%s\n", help_lines[i], _(help_lines[i+1]));
}
/**
@brief print build-related message on console

@return
*/
static void _e2_cl_option_print_version (void)
{
	printf (
		_("%s v. %s\n"
		"Licensed under the GPL\n"
		"Copyright (C) %s\n"	//&#169; doesn't work
		"Build date: %s\n"
		"Build platform: GTK+ %d.%d.%d %s\n"),
		PROGNAME, VERSION RELEASE, COPYRIGHT, BUILDDATE,	//BUILDDATE may be non-utf-8
		GTK_MAJOR_VERSION, GTK_MINOR_VERSION, GTK_MICRO_VERSION, BUILDINFO
	);
}
/**
@brief print build-time options and parameters on console

@return
*/
static void _e2_cl_option_print_build (void)
{
	gchar **split, **tmp;
	printf (PROGNAME" %s:\n", _("build parameters"));
	split = g_strsplit (BUILDOPTS, "|", -1);
	for (tmp = split; *tmp != NULL; tmp++)
	{
		if (**tmp != '\0')
			printf (" %s\n", *tmp);
	}
	g_strfreev (split);
}
/**
@brief parse commandline options and store results for later use

@param argc no. of commandline arguments, as supplied to main()
@param argv array of commanline arguments, as supplied to main()

@return
*/
void e2_cl_option_process (gint argc, gchar *argv[])
{
	gchar *freeme;
	gint c;
#ifdef DEBUG_MESSAGES
	gint d = 0;
#endif

	static struct option long_options[] =
	{
		{"one", required_argument, NULL, '1'},
		{"two", required_argument, NULL, '2'},
		{"share-config", required_argument, NULL, 'a'},
		{"config", required_argument, NULL, 'c'},
#ifdef DEBUG_MESSAGES
		{"debug", required_argument, NULL, 'd'},
#endif
		{"encoding", required_argument, NULL, 'e'},
		{"fallback-encoding", required_argument, NULL, 'f'},
		{"trash", required_argument, NULL, 't'},
		{"ignore-problems", no_argument, NULL, 'i'},
		{"log-all", no_argument, NULL, 'l'},
		{"daemon", no_argument, NULL, 'm'},
		{"original", no_argument, NULL, 'o'},
		{"run-at-start", required_argument, NULL, 'r'},
		{"set-option", required_argument, NULL, 's'},
		{"user", no_argument, NULL, 'u'},
#ifdef DEBUG_MESSAGES
		{"verbose", no_argument, NULL, 'x'},
#endif
		{"build", no_argument, NULL, 'b'},
		{"help", no_argument, NULL, 'h'},
		{"version", no_argument, NULL, 'v'},
		{NULL, no_argument, NULL, 0}
	};

	opterr = 0; //no option-parse error messages

	//non-string non-zero-default values need to be set before the options are parsed
#ifdef DEBUG_MESSAGES
	e2_cl_options.debug_level = E2_DEBUG_LEVEL;
#endif
	//default to no logging when detailed messaging happening
	e2_cl_options.suppress_gtk_log = TRUE;	//(E2_DEBUG_LEVEL > 2);
#ifdef DEBUG_MESSAGES
# define CHAR_D "d:"
# define CHAR_X "x"
#else
# define CHAR_D
# define CHAR_X
#endif
	const gchar *pattern = "1:2:a:c:"CHAR_D"e:f:t:ilmor:s:u"CHAR_X"bhv";
	while ((c = getopt_long (argc, argv, pattern, long_options, NULL)) != -1)
	{
#ifdef DEBUG_MESSAGES
		d++;
#endif
		if (optarg != NULL && *optarg != '\0')
		{
			switch (c)
			{
				case '1':
					printd (DEBUG, "setting pane 1 startup path '%s'", optarg);
					//no encoding conversion until encoding choices are certain
					e2_cl_options.pane1_path = _e2_cl_option_get_path (optarg);
					break;
				case '2':
					g_free (e2_cl_options.pane2_path);
					printd (DEBUG, "setting pane 2 startup path '%s'", optarg);
					//no encoding conversion yet
					e2_cl_options.pane2_path = _e2_cl_option_get_path (optarg);
					break;
				case 'a':
					g_free (e2_cl_options.sharedconfig_dir);
					//no encoding conversion yet
					e2_cl_options.sharedconfig_dir = _e2_cl_option_get_path (optarg);
					break;
				case 'c':
					g_free (e2_cl_options.config_dir);
					//no encoding conversion yet
					e2_cl_options.config_dir = _e2_cl_option_get_path (optarg);
					break;
#ifdef DEBUG_MESSAGES
				case 'd':
					printd (DEBUG, "setting debug level '%s'", optarg);
					{
						gchar *end;
						e2_cl_options.debug_level = g_ascii_strtoull (optarg, &end, 10);
						if (end == optarg)
						{
							e2_cl_options.debug_level = E2_DEBUG_LEVEL;
							printd (WARN, "failed setting debug level '%s'", optarg);
						}
//						e2_cl_options.suppress_gtk_log =
//							(e2_cl_options.debug_level > 2);
					}
					break;
#endif
				case 'e':
					g_free (e2_cl_options.encoding);
					e2_cl_options.encoding = g_strdup (optarg);
					break;
				case 'f':
					g_free (e2_cl_options.fallback_encoding);
					e2_cl_options.fallback_encoding = g_strdup (optarg);
					break;
				case 'r':
					e2_cl_options.startup_commands = g_slist_append
						(e2_cl_options.startup_commands, g_strdup (optarg));
					break;
				case 's':
					e2_cl_options.option_overrides = g_list_append
						(e2_cl_options.option_overrides, g_strdup (optarg));
					break;
				case 't':
					g_free (e2_cl_options.trash_dir);
					//no encoding conversion yet
					e2_cl_options.trash_dir = _e2_cl_option_get_path (optarg);
					break;
				default:
					printd (ERROR, "unknown option with code: 0%o and value: %s", c, optarg);
					break;
			}
		}
		else	//no argument, or blank
		{
			switch (c)
			{
				case 'i':
					e2_cl_options.ignore_problems = TRUE;
					break;
				case 'l':
					e2_cl_options.suppress_gtk_log = FALSE;
				case 'm':
					e2_cl_options.detached = TRUE;
					break;
				case 'o':
					e2_cl_options.original = TRUE;
					break;
				case 'u':
					e2_cl_options.session_user = TRUE;
					break;
#ifdef DEBUG_MESSAGES
				case 'x':
					e2_cl_options.verbose = TRUE;
					break;
#endif
				case 'b':
					_e2_cl_option_print_build ();
					exit (0);
					break;
				case 'h':
					_e2_cl_option_print_help ();
					exit (0);
					break;
				case 'v':
					_e2_cl_option_print_version ();
					exit (0);
					break;
				case '?':
					printd (ERROR, "unknown option");
//ignore			exit (0);
					break;
				default:
					printd (ERROR, "unknown option with code: 0%o", c);
					break;
			}
		}
	}

	if (optind < argc)
	{
		gchar *path = _e2_cl_option_get_path (argv[optind]);
#ifdef E2_VFS
		VPATH ddata = { path, NULL };
		if (e2_fs_path_exists (&ddata))
#else
		if (e2_fs_path_exists (path))
#endif
		{
			e2_cl_options.force_path = TRUE;
		}
		else
		{
			//maybe the path applies to a file, not a directory
			freeme = path;
			path = g_path_get_dirname (path);
			g_free (freeme);
#ifdef E2_VFS
			ddata.path = path;
			if (e2_fs_path_exists (&ddata))
#else
			if (e2_fs_path_exists (path))
#endif
			{
				e2_cl_options.force_path = TRUE;
			}
		}

		if (e2_cl_options.force_path)
		{
#ifdef DEBUG_MESSAGES
			d++;
#endif
			optind++;
			g_free (e2_cl_options.pane2_path);
			e2_cl_options.pane2_path = path;
		}

		if (optind < argc)
		{
			g_free (cwd);
			g_free (path);
			e2_cl_option_clear ();
			printf (_("Startup options must begin with \"-\" or \"--\", or be a valid file-system path\n"));
			exit (1);	//FIXME do a full clean exit
		}
	}
	printd (DEBUG, "parsed %d command line options", d);
	g_free (cwd);

	//encodings may be needed for other-option conversions
	const gchar **encodings;
	g_get_filename_charsets (&encodings);
	if (e2_cl_options.encoding == NULL)
	{
		if (encodings[0] != NULL && *encodings[0] != '\0')
			e2_cl_options.encoding = g_strdup (encodings[0]);
		else
			e2_cl_options.encoding = g_strdup ("ISO-8859-15");
	}
	if (e2_cl_options.fallback_encoding == NULL)
	{
		if (encodings[1] != NULL && *encodings[1] != '\0')
			e2_cl_options.fallback_encoding = g_strdup (encodings[1]);
		else if (strcmp (e2_cl_options.encoding,"ISO-8859-1") == 0
			  || strcmp (e2_cl_options.encoding,"ISO-8859-15") == 0)
			e2_cl_options.fallback_encoding = g_strdup ("C");
		else
			e2_cl_options.fallback_encoding = g_strdup ("ISO-8859-15");
	}

	//now we setup any missing default strings
	gboolean subdir;
	const gchar *usedir;
	const gchar *homedir = g_getenv ("HOME");

	if (e2_cl_options.config_dir != NULL && *e2_cl_options.config_dir != '\0')
	{
		usedir = e2_cl_options.config_dir;
		subdir = FALSE;
	}
	else
	{
		usedir = g_getenv ("XDG_CONFIG_HOME");
		if (usedir == NULL || *usedir == '\0')
			usedir = g_get_user_config_dir ();
		subdir = TRUE;
	}

	//check dir is suitable (su etc may not adjust variable)
	if (usedir != NULL && *usedir != '\0')
	{
		if (homedir != NULL && *homedir != '\0')
		{
			if (!g_str_has_prefix (usedir, homedir))
				usedir = NULL;
		}
		else
			usedir = NULL;
	}
	if (usedir == NULL || *usedir == '\0')
		freeme = g_build_filename (g_get_home_dir (), ".config", BINNAME, NULL);
	else if (subdir)
		freeme = g_build_filename (usedir, BINNAME, NULL);
	else
	{
		freeme = (gchar*)usedir;
		printd (DEBUG, "setting config directory '%s'", freeme);
	}

	g_free (e2_cl_options.config_dir); //if any
//#ifdef E2_FILES_UTF8ONLY
//	e2_cl_options.config_dir = freeme;
//#else
	e2_cl_options.config_dir = e2_utf8_filename_from_locale (freeme);
	if (freeme != usedir)
		g_free (freeme);
//#endif

	if (e2_cl_options.sharedconfig_dir != NULL)
	{
		//minimal sanity check
#ifdef E2_VFS
		VPATH data = { e2_cl_options.sharedconfig_dir, NULL }; //local config data only
		if (e2_fs_is_dir3 (&data E2_ERR_NONE())
			&& (!e2_fs_access (&data, R_OK | X_OK E2_ERR_NONE())))
#else
		if (e2_fs_is_dir3 (e2_cl_options.sharedconfig_dir E2_ERR_NONE())
			&& (!e2_fs_access (e2_cl_options.sharedconfig_dir, R_OK E2_ERR_NONE())))
#endif
		{
			freeme = e2_cl_options.config_dir;
			e2_cl_options.config_dir = e2_utf8_filename_from_locale (freeme);
			g_free (freeme);
		}
		else
		{
			g_free (e2_cl_options.config_dir);
			e2_cl_options.config_dir = NULL;
		}
	}

	//setup local default trash dir (any others done elsewhere)
	if (e2_cl_options.trash_dir != NULL && *e2_cl_options.trash_dir != '\0')
		usedir = e2_cl_options.trash_dir;
	else
	{
		usedir = g_getenv ("XDG_DATA_HOME");
		if (usedir == NULL || *usedir == '\0')
			usedir = g_get_user_data_dir ();
	}
	if (usedir != NULL && *usedir != '\0')
	{
		if (homedir != NULL && *homedir != '\0')
		{
			if (!g_str_has_prefix (usedir, homedir))
				usedir = NULL;
		}
		else
			usedir = NULL;
	}
	if (usedir == NULL || *usedir == '\0')
		freeme = g_build_filename (g_get_home_dir (), ".local", "share", "Trash", NULL);
	else
	{
		freeme = g_build_filename (usedir, "Trash", NULL);
		printd (DEBUG, "setting trash directory '%s'", freeme);
	}

	g_free (e2_cl_options.trash_dir); //if any
//#ifdef E2_FILES_UTF8ONLY
//	e2_cl_options.trash_dir = freeme;
//#else
	e2_cl_options.trash_dir = e2_utf8_filename_from_locale (freeme);
	g_free (freeme);
//#endif
}
/**
@brief end-of-session cleanup for startup options data
*/
void e2_cl_option_clear (void)
{
	g_free (e2_cl_options.pane1_path);
	g_free (e2_cl_options.pane2_path);
	g_free (e2_cl_options.config_dir);
	g_free (e2_cl_options.sharedconfig_dir);
	g_free (e2_cl_options.trash_dir);
	g_free (e2_cl_options.encoding);
	g_free (e2_cl_options.fallback_encoding);
	if (e2_cl_options.option_overrides)
	{
		g_list_foreach (e2_cl_options.option_overrides, (GFunc)g_free, NULL);
		g_list_free (e2_cl_options.option_overrides);
	}
	if (e2_cl_options.startup_commands)
	{
		g_slist_foreach (e2_cl_options.startup_commands, (GFunc)g_free, NULL);
		g_slist_free (e2_cl_options.startup_commands);
	}
}
