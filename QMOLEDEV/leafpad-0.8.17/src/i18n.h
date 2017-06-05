/*
 *  Leafpad - GTK+ based simple text editor
 *  Copyright (C) 2004-2005 Tarot Osuji
 *  
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2 of the License, or
 *  (at your option) any later version.
 *  
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *  
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 */

#ifndef _I18N_H
#define _I18N_H

#if GLIB_CHECK_VERSION(2, 4, 0)
#	include <glib/gi18n.h>
#else

#ifdef ENABLE_NLS
#	include <libintl.h>
#	define _(String) gettext(String)
#	ifdef gettext_noop
#		define N_(String) gettext_noop(String)
#	else
#		define N_(String) (String)
#	endif
#else
#	define _(String) (String)
#	define N_(String) (String)
#	define bindtextdomain(Domain, Directory)
#	define bind_textdomain_codeset(Domain, Codeset)
#	define textdomain(Domain)
#endif

#endif

#endif /* _I18N_H */
