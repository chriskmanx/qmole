/*
 * ROX-Filer, filer for the ROX desktop project
 * By Thomas Leonard, <tal197@users.sourceforge.net>.
 */

#ifndef _TOOLBAR_H
#define _TOOLBAR_H

#include <gtk/gtk.h>

/* The values correspond to the menu indexes in the option widget */
typedef enum {
	TOOLBAR_NONE 		= 0,
	TOOLBAR_NORMAL 		= 1,
	TOOLBAR_LARGE 		= 2,
	TOOLBAR_HORIZONTAL 	= 3,
} ToolbarType;

extern Option o_toolbar, o_toolbar_info;

/* Prototypes */
void toolbar_init(void);
void toolbar_update_info(FilerWindow *filer_window);
void toolbar_update_toolbar(FilerWindow *filer_window);

#endif /* _TOOLBAR_H */
