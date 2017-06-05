/*
 * Claws Mail -- a GTK+ based, lightweight, and fast e-mail client
 * Copyright (C) 1999-2012 Colin Leroy <colin@colino.net> and 
 * the Claws Mail team
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
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
 */

#ifdef HAVE_CONFIG_H
#  include "config.h"
#include "claws-features.h"
#endif

#include <stddef.h>
#include <glib.h>
#include <glib/gi18n.h>

#include "plugin.h"
#include "version.h"
#include "common/claws.h"
#include "smime.h"

gint plugin_init(gchar **error)
{
	if (!check_plugin_version(MAKE_NUMERIC_VERSION(2,9,2,72),
				VERSION_NUMERIC, _("S/MIME"), error))
		return -1;

	smime_init();

	return 0;	
}

gboolean plugin_done(void)
{
	smime_done();
	return TRUE;
}

const gchar *plugin_name(void)
{
	return _("S/MIME");
}

const gchar *plugin_desc(void)
{
	return _("This plugin handles S/MIME signed and/or encrypted "
		 "mails. You can decrypt mails, verify signatures or "
                 "sign and encrypt your own mails.\n"
		 "\n"
		 "It can be selected as the Default Privacy System in "
		 "/Configuration/[Account Preferences]/Privacy and when "
		 "composing a message from /Options/Privacy System\n"
		 "\n"
		 "This plugin uses the GPGME library as a wrapper for GnuPG.\n"
		 "This plugin also needs gpgsm, gnupg-agent and dirmngr "
		 "installed and configured.\n"
		 "\n"
		 "Information about how to get S/MIME certificates working "
		 "with GPGSM can be found at:\n"
		 "http://www.claws-mail.org/faq/index.php/S/MIME_howto\n"
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
