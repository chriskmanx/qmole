/*
 * ROX-Filer, filer for the ROX desktop project
 * By Thomas Leonard, <tal197@users.sourceforge.net>.
 */

#ifndef __DROP_BOX_H__
#define __DROP_BOX_H__

#include <gtk/gtk.h>

typedef struct _DropBoxClass DropBoxClass;
typedef struct _DropBox DropBox;

#define DROP_BOX(obj) (GTK_CHECK_CAST((obj), drop_box_get_type(), DropBox))

GtkWidget *drop_box_new(const char *message);
GType drop_box_get_type(void);
void drop_box_set_path(DropBox *drop_box, const guchar *path);
const gchar *drop_box_get_path(DropBox *drop_box);

#endif /* __DROP_BOX_H__ */
