/*  -*- Mode: C; c-set-style: linux; indent-tabs-mode: nil; c-basic-offset: 8 -*- */
/* GNOME Library - gnome-gconf.c
 * Copyright (C) 2000  Red Hat Inc.,
 * All rights reserved.
 *
 * Author: Jonathan Blandford  <jrb@redhat.com>
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
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Cambridge, MA 02139, USA.
 */
/*
  @NOTATION@
*/


#include <config.h>
#include <stdlib.h>

#define GCONF_ENABLE_INTERNALS
#include <gconf/gconf.h>
#include <gconf/gconf-client.h>
extern struct poptOption gconf_options[];

#include "gnome-i18nP.h"

#include <libgnome/libgnome.h>

#include "gnome-gconf.h"
#include "gnome-gconfP.h"

/**
 * gnome_gconf_get_gnome_libs_settings_relative:
 * @subkey: key part below the gnome desktop settings directory
 *
 * Description:  Gets the full key name for a GNOME desktop specific
 * setting for a specific application. Those keys are used to store application-specific
 * configuration, for example the history of a GnomeEntry. This
 * config space should only be used by libraries.
 *
 * Returns:  A newly allocated string
 **/
gchar*
gnome_gconf_get_gnome_libs_settings_relative (const gchar *subkey)
{
        gchar *dir;
        gchar *key;

        dir = g_strconcat("/apps/gnome-settings/",
                          gnome_program_get_app_id (gnome_program_get()),
                          NULL);

        if (subkey && *subkey) {
                key = gconf_concat_dir_and_key(dir, subkey);
                g_free(dir);
        } else {
                /* subkey == "" */
                key = dir;
        }

        return key;
}

/**
 * gnome_gconf_get_app_settings_relative:
 * @program: #GnomeProgram pointer or %NULL for the default
 * @subkey: key part below the gnome desktop settings directory
 *
 * Description:  Gets the full key name for an application specific
 * setting.  That is "/apps/&lt;application_id&gt;/@subkey".
 *
 * Returns:  A newly allocated string
 **/
gchar*
gnome_gconf_get_app_settings_relative (GnomeProgram *program, const gchar *subkey)
{
        gchar *dir;
        gchar *key;

	if (program == NULL)
		program = gnome_program_get ();

        dir = g_strconcat ("/apps/",
			   gnome_program_get_app_id  (program),
			   NULL);

        if (subkey && *subkey) {
                key = gconf_concat_dir_and_key (dir, subkey);
                g_free (dir);
        } else {
                /* subkey == "" */
                key = dir;
        }

        return key;
}

/**
 * gnome_gconf_lazy_init
 *
 * Description:  Internal libgnome/ui routine.  You never have
 * to do this from your code.  But all places in libgnome/ui
 * that need gconf should call this before calling any gconf
 * calls.
 **/
void
_gnome_gconf_lazy_init (void)
{
	/* Note this is the same as in libgnomeui/libgnomeui/gnome-gconf-ui.c,
	 * keep this in sync (it's named gnomeui_gconf_lazy_init) */
        gchar *settings_dir;
	GConfClient* client = NULL;
	static gboolean initialized = FALSE;

	if (initialized)
		return;

	initialized = TRUE;

        client = gconf_client_get_default ();

        gconf_client_add_dir (client,
			      "/desktop/gnome",
			      GCONF_CLIENT_PRELOAD_NONE, NULL);

        settings_dir = gnome_gconf_get_gnome_libs_settings_relative ("");

        gconf_client_add_dir (client,
			      settings_dir,
			      /* Possibly we should turn preload on for this */
			      GCONF_CLIENT_PRELOAD_NONE,
			      NULL);
        g_free (settings_dir);

        /* Leak the GConfClient reference, we want to keep
         * the client alive forever.
         */
}

/**
 * gnome_gconf_module_info_get
 *
 * An internal libgnome/ui routine. This will never be needed in public code.
 *
 * Returns: A #GnomeModuleInfo instance representing the GConf module.
 */
const GnomeModuleInfo *
_gnome_gconf_module_info_get (void)
{
	static GnomeModuleInfo module_info = {
		"gnome-gconf",
		gconf_version,
		NULL /* description */,
		NULL /* requirements */,
		NULL /* instance init */,
		NULL /* pre_args_parse */,
		NULL /* post_args_parse */,
		gconf_options,
		NULL /* init_pass */,
		NULL /* class_init */,
		NULL, NULL /* expansions */
	};

	module_info.description = _("GNOME GConf Support");

	return &module_info;
}
