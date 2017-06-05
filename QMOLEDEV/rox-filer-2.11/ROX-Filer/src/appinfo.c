/*
 * ROX-Filer, filer for the ROX desktop project
 * Copyright (C) 2006, Thomas Leonard and others (see changelog for details).
 *
 * This program is free software; you can redistribute it and/or modify it
 * under the terms of the GNU General Public License as published by the Free
 * Software Foundation; either version 2 of the License, or (at your option)
 * any later version.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
 * FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
 * more details.
 *
 * You should have received a copy of the GNU General Public License along with
 * this program; if not, write to the Free Software Foundation, Inc., 59 Temple
 * Place, Suite 330, Boston, MA  02111-1307  USA
 */

/* appinfo.c - querying the XMLwrapper.xml files */

/* Any valid application directory may contain a file called XMLwrapper.xml.
 * The format is:
 *
 * <?xml version="1.0"?>
 * <AppInfo>
 *   <Summary>Tooltip text</Summary>
 *   <About>
 *     <Purpose>...</Purpose>
 *     <Version>...</Version>
 *     <Authors>...</Authors>
 *     <License>...</License>
 *     <Homepage>...</Homepage>
 *     ...
 *   </About>
 *   <AppMenu>
 *     <Item option="...">
 *       <Label xml:lang='en'>...</Label>
 *     </Item>
 *     ...
 *   </AppMenu>
 *   <ROX:CanSetBackdrop/>
 * </AppInfo>
 */

#include "config.h"

#include <string.h>

#include "global.h"

#include "appinfo.h"
#include "fscache.h"
#include "type.h"
#include "diritem.h"
#include "support.h"
#include "xml.h"

/****************************************************************
 *			EXTERNAL INTERFACE			*
 ****************************************************************/

/* Load the XMLwrapper file for this application.
 *
 * Returns a pointer to the XMLwrapper structure, or NULL if this isn't
 * an application with a valid XMLwrapper file.
 *
 * g_object_unref() the result.
 */
XMLwrapper *appinfo_get(const gchar *app_dir, DirItem *item)
{
	XMLwrapper	*ai;
	guchar	*tmp;

	/* Is it even an application directory? */
	if (item->base_type != TYPE_DIRECTORY ||
			!(item->flags & ITEM_FLAG_APPDIR))
		return NULL;	/* Not an application */

	tmp = g_strconcat(app_dir, "/" APPINFO_FILENAME, NULL);
	ai = xml_cache_load(tmp);
	g_free(tmp);

	return ai;
}
