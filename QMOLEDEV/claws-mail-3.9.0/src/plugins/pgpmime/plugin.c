/*
 * Claws Mail -- a GTK+ based, lightweight, and fast e-mail client
 * Copyright (C) 1999-2012 the Claws Mail team
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

#include <stddef.h>
#include <glib.h>
#include <glib/gi18n.h>

#include "version.h"
#include "common/claws.h"
#include "pgpmime.h"
#include "plugin.h"

#define PLUGIN_NAME (_("PGP/MIME"))

gint plugin_init(gchar **error)
{
	if (!check_plugin_version(MAKE_NUMERIC_VERSION(2,9,2,72),
				VERSION_NUMERIC, PLUGIN_NAME, error))
		return -1;

	pgpmime_init();

	return 0;	
}

gboolean plugin_done(void)
{
	pgpmime_done();
	return TRUE;
}

const gchar *plugin_name(void)
{
	return PLUGIN_NAME;
}

const gchar *plugin_desc(void)
{
	return _("This plugin handles PGP/MIME signed and/or encrypted "
		 "mails. You can decrypt mails, verify signatures or "
                 "sign and encrypt your own mails.\n"
		 "\n"
		 "It can be selected as the Default Privacy System in "
		 "/Configuration/[Account Preferences]/Privacy and when "
		 "composing a message from /Options/Privacy System\n"
		 "\n"
		 "The plugin uses the GPGME library as a wrapper for GnuPG.\n"
		 "\n"
		 "GPGME is copyright 2001 by Werner Koch <dd9jn@gnu.org>");
}

const gchar *plugin_type(void)
{
	return "GTK2";
}

const gchar *plugin_licence(void)
{
	return "GPL3+";
}

const gchar *plugin_version(void)
{
	return VERSION;
}
