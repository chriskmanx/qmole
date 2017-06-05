/*
 * Copyright 2011 Sven Weidauer <sven.weidauer@gmail.com>
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

#import <Cocoa/Cocoa.h>

#import "utils/log.h"
#import "utils/nsurl.h"
#import "desktop/gui_fetch.h"

#import "cocoa/fetch.h"

static char cocoafiletype[200];

static const struct mimemap_s {
	const char const *extension;
	const char const *mimetype;
} cocoamimemap[] = {
	{ "css", "text/css" },
	{ "f79", "text/css" },
	{ "jpg", "image/jpeg" },
	{ "jpeg", "image/jpeg" },
	{ "gif", "image/gif" },
	{ "png", "image/png" },
	{ "b60", "image/png" },
	{ "jng", "image/jng" },
	{ "svg", "image/svg" },
	{ NULL, "text/html" }
};


static const char *fetch_filetype(const char *unix_path)
{
	NSString *uti;
	NSString *mimeType = nil;
	NSError *utiError = nil;

	uti = [[NSWorkspace sharedWorkspace] typeOfFile: [NSString stringWithUTF8String: unix_path] error:&utiError];
	if (nil != uti) {
		LOG(("Looking for mimetype from uti \"%s\"", [uti UTF8String] ));
		mimeType = (NSString *)UTTypeCopyPreferredTagWithClass( (CFStringRef)uti, kUTTagClassMIMEType );
	} else {
		NSAlert *utiAlert = [NSAlert alertWithError:utiError];
		[utiAlert runModal]; // Ignore return value.

		LOG(("uti call failed"));

		strncpy(cocoafiletype, "text/html", sizeof(cocoafiletype));
		return cocoafiletype;
	}

	if (nil != mimeType) {
		strncpy(cocoafiletype, [mimeType UTF8String], sizeof(cocoafiletype));
		[mimeType release];
	} else {
		const char *extension;

		LOG(("mimetype from uti failed"));

		extension = [(NSString *)UTTypeCopyPreferredTagWithClass( (CFStringRef)uti, kUTTagClassFilenameExtension) UTF8String];

		if (extension == NULL) {
			/* give up and go with default */
			LOG(("No extension going with default type"));
			strncpy(cocoafiletype, "text/html", sizeof(cocoafiletype));		} else {
			int eidx = 0; /* index of extension entry */

			while ((cocoamimemap[eidx].extension != NULL) &&
			       (strcmp(cocoamimemap[eidx].extension, extension) != 0)) {
				eidx++;
			}

			strncpy(cocoafiletype,
				cocoamimemap[eidx].mimetype,
				sizeof(cocoafiletype));
		}
	}

	LOG(( "\tMIME type for '%s' is '%s'", unix_path, cocoafiletype ));

	return cocoafiletype;
}

static nsurl *gui_get_resource_url(const char *path)
{
	nsurl *url = NULL;
	NSString *nspath = [[NSBundle mainBundle] pathForResource: [NSString stringWithUTF8String: path] ofType: @""];
	if (nspath == nil) return NULL;
	nsurl_create([[[NSURL fileURLWithPath: nspath] absoluteString] UTF8String], &url);
	return url;
}

static struct gui_fetch_table fetch_table = {
	.filetype = fetch_filetype,

	.get_resource_url = gui_get_resource_url,
};

struct gui_fetch_table *cocoa_fetch_table = &fetch_table;
