/*
 * Claws Mail -- a GTK+ based, lightweight, and fast e-mail client
 * Copyright (C) 1999-2012  the Claws Mail team
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 * 
 */

#ifdef HAVE_CONFIG_H
#  include "config.h"
#include "claws-features.h"
#endif

#include "defs.h"
#include <stdlib.h>
#include <glib.h>
#ifdef ENABLE_NLS
#include <glib/gi18n.h>
#else
#define _(a) (a)
#define N_(a) (a)
#endif

#if HAVE_LOCALE_H
#  include <locale.h>
#endif

#include "claws.h"
#include "utils.h"
#include "ssl.h"
#include "version.h"

static gboolean claws_initialized = FALSE;
static gchar *startup_dir;
static void (*claws_idle_function)(void) = NULL;

/**
 * Parse program parameters and remove all parameters
 * that have been processed. Arguments are pointers to
 * original passed programm arguments and these will
 * be modified leaving only unknown parameters for
 * further processing
 *
 * \param argc pointer to number of parameters
 * \param argv pointer to array of parameter strings
 */
static void parse_parameter(int *argc, char ***argv)
{
	gint i, j, k;

	cm_return_if_fail(argc != NULL);
	cm_return_if_fail(argv != NULL);

	for (i = 1; i < *argc;) {
		if (strcmp("--debug", (*argv)[i]) == 0) {
			debug_set_mode(TRUE);

			(*argv)[i] = NULL;
		}

		i += 1;
	}

	/* Remove NULL args from argv[] for further processing */
	for (i = 1; i < *argc; i++) {
		for (k = i; k < *argc; k++)
			if ((*argv)[k] != NULL)
				break;

		if (k > i) {
			k -= i;
			for (j = i + k; j < *argc; j++)
				(*argv)[j - k] = (*argv)[j];
			*argc -= k;
		}
	}
}

gboolean claws_init(int *argc, char ***argv)
{
	if (claws_initialized)
		return TRUE;

#ifdef USE_GNUTLS
	ssl_init();
#endif
	startup_dir = g_get_current_dir();

	parse_parameter(argc, argv);

	debug_print("Starting Claws Mail version %s\n", PROG_VERSION);
	
	setlocale(LC_ALL, "");
#ifdef ENABLE_NLS
	bindtextdomain(PACKAGE, get_locale_dir () );
	bind_textdomain_codeset (PACKAGE, "UTF-8");
	textdomain(PACKAGE);
#endif /*ENABLE_NLS*/
	putenv("G_BROKEN_FILENAMES=1");

	/* backup if old rc file exists */
	if (is_file_exist(RC_DIR)) {
		if (g_rename(RC_DIR, RC_DIR ".bak") < 0) {
			FILE_OP_ERROR(RC_DIR, "rename");
			return FALSE;
		}
	}

	srand((gint) time(NULL));

	claws_initialized = TRUE;

	return TRUE;
}

void claws_done(void)
{

#ifdef USE_GNUTLS
	ssl_done();
#endif
}

const gchar *claws_get_startup_dir(void)
{
	return startup_dir;
}

guint claws_get_version(void)
{
	return VERSION_NUMERIC;
}

void claws_register_idle_function	(void (*idle_func)(void))
{
	claws_idle_function = idle_func;
}

void claws_do_idle(void)
{
	if (claws_idle_function != NULL)
		claws_idle_function();
}
