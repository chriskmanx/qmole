/*
 * Copyright 2009 Mark Benjamin <MarkBenjamin@dfgh.net>
 *
 * This file is part of NetSurf, http://www.netsurf-browser.org/
 *
 * NetSurf is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; version 2 of the License.
 *
 * NetSurf is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

#include <gtk/gtk.h>

#include "utils/utils.h"
#include "utils/utf8.h"
#include "utils/nsurl.h"
#include "utils/messages.h"
#include "desktop/browser.h"
#include "content/content.h"

#include "gtk/viewdata.h"
#include "gtk/viewsource.h"

void nsgtk_viewsource(GtkWindow *parent, struct browser_window *bw)
{
	nserror ret;
	struct hlcache_handle *hlcontent;
	const char *source_data;
	unsigned long source_size;
	char *ndata = NULL;
	size_t ndata_len;
	char *filename;
	char *title;

	hlcontent = browser_window_get_content(bw);
	if (hlcontent == NULL) {
		return;
	}

	if (content_get_type(hlcontent) != CONTENT_HTML) {
		return;
	}

	source_data = content_get_source_data(hlcontent, &source_size);

	ret = nsurl_nice(browser_window_get_url(bw), &filename, false);
	if (ret != NSERROR_OK) {
		filename = strdup(messages_get("SaveSource"));
		if (filename == NULL) {
			return;
		}
	}

	title = malloc(strlen(nsurl_access(browser_window_get_url(bw))) + SLEN("Source of  - NetSurf") + 1);
	sprintf(title, "Source of %s - NetSurf", nsurl_access(browser_window_get_url(bw)));

	ret = utf8_from_enc(source_data,
			    content_get_encoding(hlcontent, CONTENT_ENCODING_NORMAL),
			    source_size,
			    &ndata,
			    &ndata_len);
	if (ret == NSERROR_OK) {
		ret = nsgtk_viewdata(title, filename, ndata, ndata_len);
	}

	free(filename);
	free(title);
}
