/*
 * ROX-Filer, filer for the ROX desktop project
 * By Thomas Leonard, <tal197@users.sourceforge.net>.
 */

#ifndef __VIEW_COLLECTION_H__
#define __VIEW_COLLECTION_H__

#include <gtk/gtk.h>

typedef struct _ViewCollectionClass ViewCollectionClass;

#define VIEW_COLLECTION(obj) \
	(GTK_CHECK_CAST((obj), view_collection_get_type(), ViewCollection))

GtkWidget *view_collection_new(FilerWindow *filer_window);
GType view_collection_get_type(void);

#endif /* __VIEW_COLLECTION_H__ */
