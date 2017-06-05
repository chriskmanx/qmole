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

#import "cocoa/gui.h"
#import "cocoa/plotter.h"
#import "cocoa/BrowserView.h"
#import "cocoa/BrowserViewController.h"
#import "cocoa/BrowserWindowController.h"
#import "cocoa/FormSelectMenu.h"
#import "cocoa/fetch.h"
#import "cocoa/schedule.h"

#import "utils/nsoption.h"
#import "utils/utils.h"
#import "utils/log.h"
#import "desktop/mouse.h"
#import "desktop/gui_window.h"
#import "desktop/gui_misc.h"
#import "desktop/browser.h"
#import "desktop/textinput.h"
#import "image/ico.h"
#import "content/fetchers/resource.h"
#import "content/hlcache.h"
#import "content/content.h"

NSString * const kCookiesFileOption = @"CookiesFile";
NSString * const kURLsFileOption = @"URLsFile";
NSString * const kHotlistFileOption = @"Hotlist";
NSString * const kHomepageURLOption = @"HomepageURL";
NSString * const kOptionsFileOption = @"ClassicOptionsFile";
NSString * const kAlwaysCancelDownload = @"AlwaysCancelDownload";
NSString * const kAlwaysCloseMultipleTabs = @"AlwaysCloseMultipleTabs";

#define UNIMPL() NSLog( @"Function '%s' unimplemented", __func__ )

struct browser_window;

static struct gui_window *
gui_window_create(struct browser_window *bw,
                  struct gui_window *existing,
                  gui_window_create_flags flags)
{
	BrowserWindowController *window = nil;

	browser_window_set_scale(bw, (float)nsoption_int(scale) / 100, false);
	if (existing != NULL) {
		window = [(BrowserViewController *)(existing) windowController];
	}

	BrowserViewController *result = [[BrowserViewController alloc] initWithBrowser: bw];

	if (!(flags & GW_CREATE_TAB) || nil == window) {
		window = [[[BrowserWindowController alloc] init] autorelease];
		[[window window] makeKeyAndOrderFront: nil];
	}
	[window addTab: result];
	
	return (struct gui_window *)result;
}

static void gui_window_destroy(struct gui_window *g)
{
	BrowserViewController *vc = (BrowserViewController *)g;
	[vc release];
}

static void gui_window_set_title(struct gui_window *g, const char *title)
{
	[(BrowserViewController *)g setTitle: [NSString stringWithUTF8String: title]];
}

static void gui_window_redraw_window(struct gui_window *g)
{
	[[(BrowserViewController *)g browserView] setNeedsDisplay: YES];
}

static void gui_window_update_box(struct gui_window *g, const struct rect *rect)
{
	const NSRect nsrect = cocoa_scaled_rect_wh( 
		browser_window_get_scale([(BrowserViewController *)g browser]),
		rect->x0, rect->y0, 
		rect->x1 - rect->x0, rect->y1 - rect->y0 );
	[[(BrowserViewController *)g browserView] setNeedsDisplayInRect: nsrect];
}

static bool gui_window_get_scroll(struct gui_window *g, int *sx, int *sy)
{
	NSCParameterAssert( g != NULL && sx != NULL && sy != NULL );
	
	NSRect visible = [[(BrowserViewController *)g browserView] visibleRect];
	*sx = cocoa_pt_to_px( NSMinX( visible ) );
	*sy = cocoa_pt_to_px( NSMinY( visible ) );
	return true;
}

static void gui_window_set_scroll(struct gui_window *g, int sx, int sy)
{
	[[(BrowserViewController *)g browserView] scrollPoint: cocoa_point( sx, sy )];
}

/**
 * callback from core to reformat a window.
 */
static void cocoa_window_reformat(struct gui_window *gw)
{
	if (gw != NULL) {
                [[(BrowserViewController *)gw browserView] reformat ];
	}
}


static void gui_window_get_dimensions(struct gui_window *g,
                                      int *width, int *height,
                                      bool scaled)
{
	NSCParameterAssert( width != NULL && height != NULL );
	
	NSRect frame = [[[(BrowserViewController *)g browserView] superview] frame];
	if (scaled) {
        	const CGFloat scale = browser_window_get_scale([(BrowserViewController *)g browser]);
		frame.size.width /= scale;
		frame.size.height /= scale;
	}
	*width = cocoa_pt_to_px( NSWidth( frame ) );
	*height = cocoa_pt_to_px( NSHeight( frame ) );
}

static void gui_window_update_extent(struct gui_window *g)
{
	BrowserViewController * const window = (BrowserViewController *)g;
        int width;
        int height;
	struct browser_window *browser = [window browser];

        browser_window_get_extents(browser, false, &width, &height);
	
	[[window browserView] setMinimumSize: cocoa_scaled_size( browser_window_get_scale(browser), width, height )];
}

static void gui_window_set_status(struct gui_window *g, const char *text)
{
	[(BrowserViewController *)g setStatus: [NSString stringWithUTF8String: text]];
}

static void gui_window_set_pointer(struct gui_window *g, gui_pointer_shape shape)
{
	switch (shape) {
		case GUI_POINTER_DEFAULT:
		case GUI_POINTER_WAIT:
		case GUI_POINTER_PROGRESS:
			[[NSCursor arrowCursor] set];
			break;
			
		case GUI_POINTER_CROSS:
			[[NSCursor crosshairCursor] set];
			break;
			
		case GUI_POINTER_POINT:
		case GUI_POINTER_MENU:
			[[NSCursor pointingHandCursor] set];
			break;
			
		case GUI_POINTER_CARET:
			[[NSCursor IBeamCursor] set];
			break;
			
		case GUI_POINTER_MOVE:
			[[NSCursor closedHandCursor] set];
			break;

		default:
			NSLog( @"Other cursor %d requested", shape );
			[[NSCursor arrowCursor] set];
			break;
	}
}

static nserror gui_window_set_url(struct gui_window *g, struct nsurl *url)
{
  [(BrowserViewController *)g setUrl: [NSString stringWithUTF8String: nsurl_access(url)]];
        return NSERROR_OK;
}

static void gui_window_start_throbber(struct gui_window *g)
{
	[(BrowserViewController *)g setIsProcessing: YES];
	[(BrowserViewController *)g updateBackForward];
}

static void gui_window_stop_throbber(struct gui_window *g)
{
	[(BrowserViewController *)g setIsProcessing: NO];
	[(BrowserViewController *)g updateBackForward];
}

static void gui_window_set_icon(struct gui_window *g, hlcache_handle *icon)
{
	NSBitmapImageRep *bmp = icon != NULL ? (NSBitmapImageRep *)content_get_bitmap( icon ) : NULL;

	NSImage *image = nil;
	if (bmp != nil) {
		image = [[NSImage alloc] initWithSize: NSMakeSize( 32, 32 )];
		[image addRepresentation: bmp];
	} else {
		image = [[NSImage imageNamed: @"NetSurf"] copy];
	}
	[image setFlipped: YES];

	[(BrowserViewController *)g setFavicon: image];
	[image release];
}

static void gui_window_place_caret(struct gui_window *g, int x, int y, int height,
		const struct rect *clip)
{
	[[(BrowserViewController *)g browserView] addCaretAt: cocoa_point( x, y ) 
												  height: cocoa_px_to_pt( height )];
}

static void gui_window_remove_caret(struct gui_window *g)
{
	[[(BrowserViewController *)g browserView] removeCaret];
}

static void gui_window_new_content(struct gui_window *g)
{
	[(BrowserViewController *)g contentUpdated];
}


static void gui_create_form_select_menu(struct gui_window *g,
								 struct form_control *control)
{
	BrowserViewController * const window = (BrowserViewController *)g;
	FormSelectMenu  *menu = [[FormSelectMenu alloc] initWithControl: control forWindow: window->browser];
	[menu runInView: [window browserView]];
	[menu release];
}

static nserror gui_launch_url(nsurl *url)
{
	[[NSWorkspace sharedWorkspace] openURL: [NSURL URLWithString: [NSString stringWithUTF8String: nsurl_access(url)]]];
    return NSERROR_OK;
}

struct ssl_cert_info;

static void gui_cert_verify(nsurl *url, const struct ssl_cert_info *certs, 
					 unsigned long num, nserror (*cb)(bool proceed, void *pw),
					 void *cbpw)
{
	cb( false, cbpw );
}


static struct gui_window_table window_table = {
	.create = gui_window_create,
	.destroy = gui_window_destroy,
	.redraw = gui_window_redraw_window,
	.update = gui_window_update_box,
	.get_scroll = gui_window_get_scroll,
	.set_scroll = gui_window_set_scroll,
	.get_dimensions = gui_window_get_dimensions,
	.update_extent = gui_window_update_extent,
    .reformat = cocoa_window_reformat,

	.set_title = gui_window_set_title,
	.set_url = gui_window_set_url,
	.set_icon = gui_window_set_icon,
	.set_status = gui_window_set_status,
	.set_pointer = gui_window_set_pointer,
	.place_caret = gui_window_place_caret,
	.remove_caret = gui_window_remove_caret,
    .new_content = gui_window_new_content,
	.start_throbber = gui_window_start_throbber,
	.stop_throbber = gui_window_stop_throbber,
	.create_form_select_menu = gui_create_form_select_menu,
};

struct gui_window_table *cocoa_window_table = &window_table;


static struct gui_browser_table browser_table = {
    .schedule = cocoa_schedule,

	.launch_url = gui_launch_url,
	.cert_verify = gui_cert_verify,
};

struct gui_browser_table *cocoa_browser_table = &browser_table;
