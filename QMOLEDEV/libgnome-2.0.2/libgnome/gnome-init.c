/* gnomelib-init.c - Implement libgnome module
   Copyright (C) 1997, 1998, 1999 Free Software Foundation
                 1999, 2000 Red Hat, Inc.
		 2001 SuSE Linux AG.
   All rights reserved.

   This file is part of GNOME 2.0.

   The Gnome Library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Library General Public License as
   published by the Free Software Foundation; either version 2 of the
   License, or (at your option) any later version.

   The Gnome Library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Library General Public License for more details.

   You should have received a copy of the GNU Library General Public
   License along with the Gnome Library; see the file COPYING.LIB.  If not,
   write to the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
   Boston, MA 02111-1307, USA.  */
/*
  @NOTATION@
 */

#include <config.h>
#include <errno.h>
#include <locale.h>
#include <stdarg.h>
#include <string.h>
#include <stdlib.h>
#include <sys/types.h>
#include <fcntl.h>
#include <unistd.h>
#include <pwd.h>
#include <sys/stat.h>

#include <glib.h>
#include "gnome-i18nP.h"

#include "gnome-init.h"
#include "gnome-gconfP.h"
#include "gnome-util.h"
#include "gnome-sound.h"
#include "gnome-triggers.h"

#include <errno.h>

#include <bonobo-activation/bonobo-activation.h>
#include <bonobo-activation/bonobo-activation-version.h>
#include <libbonobo.h>

#include <libgnomevfs/gnome-vfs-init.h>

/*****************************************************************************
 * bonobo
 *****************************************************************************/

static void
bonobo_post_args_parse (GnomeProgram *program, GnomeModuleInfo *mod_info)
{
	int dumb_argc = 1;
	char *dumb_argv[] = {NULL};

	dumb_argv[0] = g_get_prgname ();

	bonobo_init (&dumb_argc, dumb_argv);
}

/**
* gnome_bonobo_module_info_get:
*
* Retrieves the bonobo module version and indicate that it requires the current
* libgnome and its dependencies (although libbonobo does not depend on
* libgnome, libbonoboui does and this will also be initialised when
* initialising a GNOME app).
*
* Returns: a new #GnomeModuleInfo structure describing the version of the
* bonobo modules and its dependents.
*/
const GnomeModuleInfo *
gnome_bonobo_module_info_get (void)
{
	static GnomeModuleInfo module_info = {
		"bonobo",
		/* FIXME: get this from bonobo */"1.101.2",
		N_("Bonobo Support"),
		NULL, NULL,
		NULL, bonobo_post_args_parse,
		NULL, NULL, NULL, NULL
	};

	if (module_info.requirements == NULL) {
		static GnomeModuleRequirement req[2];

		req[0].required_version = VERSION;
		req[0].module_info = LIBGNOME_MODULE;

		req[1].required_version = NULL;
		req[1].module_info = NULL;

		module_info.requirements = req;
	}

	return &module_info;
}

/*****************************************************************************
 * bonobo-activation
 *****************************************************************************/

static void
bonobo_activation_pre_args_parse (GnomeProgram *program, GnomeModuleInfo *mod_info)
{
        if (!g_thread_supported ())
		g_thread_init (NULL);

	if (!bonobo_activation_is_initialized ())
		bonobo_activation_preinit (program, mod_info);
}

static void
bonobo_activation_post_args_parse (GnomeProgram *program, GnomeModuleInfo *mod_info)
{
	if (!bonobo_activation_is_initialized ()) {
		int dumb_argc = 1;
		char *dumb_argv[] = {NULL};

		dumb_argv[0] = g_get_prgname ();
		(void) bonobo_activation_orb_init (&dumb_argc, dumb_argv);
		
		bonobo_activation_postinit (program, mod_info);
	}
}

/* No need to make this public, always pulled in */
static const GnomeModuleInfo *
gnome_bonobo_activation_module_info_get (void)
{
	static GnomeModuleInfo module_info = {
		"bonobo-activation", NULL, N_("Bonobo activation Support"),
		NULL, NULL,
		bonobo_activation_pre_args_parse, bonobo_activation_post_args_parse,
		bonobo_activation_popt_options
	};
	if (module_info.version == NULL) {
		module_info.version = g_strdup_printf
			("%d.%d.%d",
			 BONOBO_ACTIVATION_MAJOR_VERSION,
			 BONOBO_ACTIVATION_MINOR_VERSION,
			 BONOBO_ACTIVATION_MICRO_VERSION);
	}
	return &module_info;
}

/*****************************************************************************
 * libgnome
 *****************************************************************************/

enum { ARG_DISABLE_SOUND = 1, ARG_ENABLE_SOUND, ARG_ESPEAKER, ARG_VERSION };

static char *gnome_user_dir = NULL;
static char *gnome_user_private_dir = NULL;
static char *gnome_user_accels_dir = NULL;

/**
* gnome_user_dir_get:
*
* Retrieves the user-specific directory for GNOME apps to use ($HOME/.gnome2
* is the usual GNOME 2 value).
*
* Returns: An absolute path to the directory.
*/
const char *
gnome_user_dir_get (void)
{
	return gnome_user_dir;
}

/**
* gnome_user_private_dir_get:
*
* Differs from gnome_user_dir_get() in that the directory returned here will
* have had permissions of 0700 (rwx------) enforced when it was created.  Of
* course, the permissions may have been altered since creation, so care still
* needs to be taken.
*
* Returns: An absolute path to the user-specific private directory that GNOME
* apps can use.
*/
const char *
gnome_user_private_dir_get (void)
{
	return gnome_user_private_dir;
}

/**
* gnome_user_accels_dir_get:
*
* Retrieves the user-specific directory that stores the keyboard shortcut files
* for each GNOME app. Note that most applications should be using GConf for
* storing this information, but it may be necessary to use the
* gnome_user_accels_dir_get() directory for legacy applications.
*
* Returns: The absolute path to the directory.
*/
const char *
gnome_user_accels_dir_get (void)
{
	return gnome_user_accels_dir;
}

static void
libgnome_option_cb (poptContext ctx, enum poptCallbackReason reason,
		    const struct poptOption *opt, const char *arg,
		    void *data)
{
	GnomeProgram *program;
	GValue value = { 0 };

	program = gnome_program_get ();
	
	switch(reason) {
	case POPT_CALLBACK_REASON_OPTION:
		switch(opt->val) {
		case ARG_ESPEAKER:
			g_value_init (&value, G_TYPE_STRING);
			g_value_set_string (&value, opt->arg);
			g_object_set_property (G_OBJECT (program),
					       GNOME_PARAM_ESPEAKER, &value);
			g_value_unset (&value);
			break;

		case ARG_DISABLE_SOUND:
			g_value_init (&value, G_TYPE_BOOLEAN);
			g_value_set_boolean (&value, FALSE);
			g_object_set_property (G_OBJECT (program),
					       GNOME_PARAM_ENABLE_SOUND, &value);
			g_value_unset (&value);
			break;

		case ARG_ENABLE_SOUND:
			g_value_init (&value, G_TYPE_BOOLEAN);
			g_value_set_boolean (&value, TRUE);
			g_object_set_property (G_OBJECT (program),
					       GNOME_PARAM_ENABLE_SOUND, &value);
			g_value_unset (&value);
			break;

		case ARG_VERSION:
			g_print ("Gnome %s %s\n",
				 gnome_program_get_app_id (program),
				 gnome_program_get_app_version (program));
			exit(0);
			break;
		}
	default:
		/* do nothing */
		break;
	}
}

static void
libgnome_userdir_setup (gboolean create_dirs)
{
	if(!gnome_user_dir) {
		gnome_user_dir = g_build_filename (g_get_home_dir(), GNOME_DOT_GNOME, NULL);
		gnome_user_private_dir = g_build_filename (g_get_home_dir(),
							   GNOME_DOT_GNOME_PRIVATE, NULL);
		gnome_user_accels_dir = g_build_filename (gnome_user_dir,
							  "accels", NULL);
	}

	if (!create_dirs)
		return;
	
	if (mkdir (gnome_user_dir, 0700) < 0) { /* private permissions, but we
						   don't check that we got them */
		if (errno != EEXIST) {
			fprintf(stderr, _("Could not create per-user gnome configuration directory `%s': %s\n"),
				gnome_user_dir, strerror(errno));
			exit(1);
		}
	}
    
	if (mkdir (gnome_user_private_dir, 0700) < 0) { /* This is private
							   per-user info mode
							   700 will be
							   enforced!  maybe
							   even other security
							   meassures will be
							   taken */
		if (errno != EEXIST) {
			fprintf (stderr, _("Could not create per-user gnome configuration directory `%s': %s\n"),
				 gnome_user_private_dir, strerror(errno));
			exit(1);
		}
	}


	/* change mode to 0700 on the private directory */
	if (chmod (gnome_user_private_dir, 0700) < 0) {
		fprintf(stderr, _("Could not set mode 0700 on private per-user gnome configuration directory `%s': %s\n"),
			gnome_user_private_dir, strerror(errno));
		exit(1);
	}
  
	if (mkdir (gnome_user_accels_dir, 0700) < 0) {
		if (errno != EEXIST) {
			fprintf(stderr, _("Could not create gnome accelerators directory `%s': %s\n"),
				gnome_user_accels_dir, strerror(errno));
			exit(1);
		}
	}
}

static void
libgnome_post_args_parse (GnomeProgram *program,
			  GnomeModuleInfo *mod_info)
{
	GValue value = { 0 };
	gboolean enable_val = TRUE, create_dirs_val = TRUE;                           
	char *espeaker_val = NULL;                                                    

	g_value_init (&value, G_TYPE_BOOLEAN);
	g_object_get_property (G_OBJECT (program),
			       GNOME_PARAM_CREATE_DIRECTORIES,
			       &value);
	create_dirs_val = g_value_get_boolean (&value);
	g_value_unset (&value);

	g_value_init (&value, G_TYPE_BOOLEAN);
	g_object_get_property (G_OBJECT (program), 
			       GNOME_PARAM_ENABLE_SOUND, &value);
	enable_val = g_value_get_boolean (&value);
	g_value_unset (&value);

	g_value_init (&value, G_TYPE_STRING);
	g_object_get_property (G_OBJECT (program), 
			       GNOME_PARAM_ESPEAKER, &value);
	espeaker_val = g_value_dup_string (&value);
	g_value_unset (&value);


	if (enable_val) {
		gnome_sound_init(espeaker_val);
	}

	libgnome_userdir_setup (create_dirs_val);

	setlocale (LC_ALL, "");
	/* XXX todo - handle multiple installation dirs */
	bindtextdomain (GETTEXT_PACKAGE, LIBGNOME_LOCALEDIR);
#ifdef HAVE_BIND_TEXTDOMAIN_CODESET
	bind_textdomain_codeset (GETTEXT_PACKAGE, "UTF-8");
#endif

}

static struct poptOption gnomelib_options [] = {
	{ NULL, '\0', POPT_ARG_INTL_DOMAIN, GETTEXT_PACKAGE, 0, NULL, NULL},

	{ NULL, '\0', POPT_ARG_CALLBACK, (void *) libgnome_option_cb, 0, NULL, NULL},

	{ "disable-sound", '\0', POPT_ARG_NONE,                                 
	  NULL, ARG_DISABLE_SOUND, N_("Disable sound server usage"), NULL},     

	{ "enable-sound", '\0', POPT_ARG_NONE,                                  
	  NULL, ARG_ENABLE_SOUND, N_("Enable sound server usage"), NULL},       

	{ "espeaker", '\0', POPT_ARG_STRING,                                    
	  NULL, ARG_ESPEAKER, N_("Host:port on which the sound server to use is"
				 " running"),
	  N_("HOSTNAME:PORT")},                                                 

	{"version", '\0', POPT_ARG_NONE, NULL, ARG_VERSION },

	{ NULL, '\0', 0, NULL, 0 }
};

static void
gnome_vfs_post_args_parse (GnomeProgram *program, GnomeModuleInfo *mod_info)
{
	gnome_vfs_init ();
}

/* No need for this to be public */
static const GnomeModuleInfo *
gnome_vfs_module_info_get (void)
{
	static GnomeModuleInfo module_info = {
		"gnome-vfs", GNOMEVFSVERSION, N_("GNOME Virtual Filesystem"),
		NULL, NULL,
		NULL, gnome_vfs_post_args_parse,
		NULL,
		NULL,
		NULL
	};
	return &module_info;
}
/**
* libgnome_module_info_get:
* 
* Retrieves the current libgnome version and the modules it depends on.
*
* Returns: a new #GnomeModuleInfo structure describing the version and
* the versions of the dependents.
*/
const GnomeModuleInfo *
libgnome_module_info_get (void)
{
	static GnomeModuleInfo module_info = {
		"libgnome", VERSION, N_("GNOME Library"),
		NULL, NULL,
		NULL, libgnome_post_args_parse,
		gnomelib_options,
		NULL, NULL, NULL, NULL
	};

	if (module_info.requirements == NULL) {
		static GnomeModuleRequirement req[4];

		req[0].required_version = "0.9.1";
		req[0].module_info = gnome_bonobo_activation_module_info_get ();

		req[1].required_version = "0.3.0";
		req[1].module_info = gnome_vfs_module_info_get ();

		req[2].required_version = "1.1.1";
		req[2].module_info = gnome_gconf_module_info_get ();

		req[3].required_version = NULL;
		req[3].module_info = NULL;

		module_info.requirements = req;
	}

	return &module_info;
}
