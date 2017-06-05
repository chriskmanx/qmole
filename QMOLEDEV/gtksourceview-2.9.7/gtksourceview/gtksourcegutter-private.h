#ifndef __GTK_SOURCE_GUTTER_PRIVATE_H__
#define __GTK_SOURCE_GUTTER_PRIVATE_H__

#include "gtksourcegutter.h"

G_BEGIN_DECLS

struct _GtkSourceView;

GtkSourceGutter *gtk_source_gutter_new (struct _GtkSourceView *view,
                                        GtkTextWindowType      type);

G_END_DECLS

#endif /* __GTK_SOURCE_GUTTER_PRIVATE_H__ */

/* vi:ts=8 */
