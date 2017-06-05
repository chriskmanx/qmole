/*
 * ROX-Filer, filer for the ROX desktop project
 * By Thomas Leonard, <tal197@users.sourceforge.net>.
 */

#ifndef __WRAPPED_H__
#define __WRAPPED_H__

#include <gtk/gtk.h>

typedef struct _WrappedLabelClass WrappedLabelClass;

struct _WrappedLabelClass {
	GtkWidgetClass parent;
};

struct _WrappedLabel {
	GtkWidget widget;
	PangoLayout *layout;
	gint width, text_width;
	int x_off, y_off;
};

#define TYPE_WRAPPED_LABEL wrapped_label_get_type()
#define WRAPPED_LABEL(obj) (GTK_CHECK_CAST((obj), TYPE_WRAPPED_LABEL, WrappedLabel))

GType wrapped_label_get_type(void);
GtkWidget *wrapped_label_new(const char *text, gint width);
void wrapped_label_set_text(WrappedLabel *wl, const char *text);

#endif /* __WRAPPED_H__ */
