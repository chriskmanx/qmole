#include <config.h>
#include <gtk/gtk.h>
#include <libgnomecanvas/libgnomecanvas.h>

GtkWidget *create_newwin(gboolean normal, gchar *appname, gchar *title);

GtkWidget *create_canvas_primitives (int aa);
GtkWidget *create_canvas_arrowhead (void);
GtkWidget *create_canvas_fifteen (void);
GtkWidget *create_canvas_features (void);
GtkWidget *create_canvas_rich_text (void);
GtkWidget *create_canvas_bezier_curve (void);
