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

#import "cocoa/FormSelectMenu.h"
#import "cocoa/coordinates.h"

#import "desktop/browser.h"
#import "render/form.h"

static inline NSRect cocoa_rect_for_control( struct browser_window *bw, struct form_control *control)
{
	struct rect r;
	form_control_bounding_rect(control, &r);
	return cocoa_scaled_rect(browser_window_get_scale(bw), r.x0, r.y0, r.x1, r.y1 );
}

@interface FormSelectMenu ()

- (void) itemSelected: (id) sender;

@end


@implementation FormSelectMenu


- initWithControl: (struct form_control *) c forWindow: (struct browser_window *) w;
{
	if ((self = [super init]) == nil) return nil;
	
	control = c;
	browser = w;
	
	menu = [[NSMenu alloc] initWithTitle: @"Select"];
	if (menu == nil) {
		[self release];
		return nil;
	}
	
	[menu addItemWithTitle: @"" action: NULL keyEquivalent: @""];
	
	NSInteger currentItemIndex = 0;
        struct form_option *opt;
	for (opt = form_select_get_option(control, 0); opt != NULL; opt = opt->next) {
		NSMenuItem *item = [[NSMenuItem alloc] initWithTitle: [NSString stringWithUTF8String: opt->text] 
													  action: @selector( itemSelected: ) 
											   keyEquivalent: @""];
		if (opt->selected) [item setState: NSOnState];
		[item setTarget: self];
		[item setTag: currentItemIndex++];
		[menu addItem: item];
		[item release];
	}
	
	[menu setDelegate: self];
	
	return self;
}

- (void) dealloc;
{
	[cell release];
	[menu release];
	
	[super dealloc];
}

- (void) runInView: (NSView *) view;
{
	[self retain];
	
	cell = [[NSPopUpButtonCell alloc] initTextCell: @"" pullsDown: YES];
	[cell setMenu: menu];

	const NSRect rect = cocoa_rect_for_control(browser, control);
								   
	[cell attachPopUpWithFrame: rect inView: view];
	[cell performClickWithFrame: rect inView: view];
}

- (void) itemSelected: (id) sender;
{
	form_select_process_selection( control, [sender tag] );
}

- (void) menuDidClose: (NSMenu *) sender;
{
	[self release];
}

@end
