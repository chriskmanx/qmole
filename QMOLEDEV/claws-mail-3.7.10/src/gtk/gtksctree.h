/* Mail Summary tree widget for Claws Mail */

#ifndef __GTK_SCTREE_H__
#define __GTK_SCTREE_H__

#include <gtk/gtk.h>
#include "gtk/gtkcmctree.h"

/* This code is based on "gtkflist.{h,c}" from mc-4.5.42 .*/

#define TYPE_GTK_SCTREE			(gtk_sctree_get_type ())
#define GTK_SCTREE(obj)			(G_TYPE_CHECK_INSTANCE_CAST ((obj), TYPE_GTK_SCTREE, GtkSCTree))
#define GTK_SCTREE_CLASS(klass)		(G_TYPE_CHECK_CLASS_CAST ((klass), TYPE_GTK_SCTREE, GtkSCTreeClass))
#define GTK_IS_SCTREE(obj)		(G_TYPE_CHECK_INSTANCE_TYPE ((obj), TYPE_GTK_SCTREE))
#define GTK_IS_SCTREE_CLASS(klass)	(G_TYPE_CHECK_CLASS_TYPE ((klass), TYPE_GTK_SCTREE))


typedef struct _GtkSCTree GtkSCTree;
typedef struct _GtkSCTreeClass GtkSCTreeClass;

struct _GtkSCTree {
	GtkCMCTree ctree;

	/* The anchor row for range selections */
	GtkCMCTreeNode *anchor_row;

	/* Mouse button and position saved on button press */
	gint dnd_press_button;
	gint dnd_press_x, dnd_press_y;

	/* Delayed selection information */
	gint dnd_select_pending;
	guint dnd_select_pending_state;
	gint dnd_select_pending_row;
	gint selecting_range;
	gboolean sorting;

	/* (dis)allow fancy color stripes */
	gboolean show_stripes;
#if !GTK_CHECK_VERSION(2,12,0)
	GtkTooltips *tooltips;
#endif
	gboolean always_expand_recursively;
	gboolean force_additive_sel;
	gboolean *use_markup;
};

struct _GtkSCTreeClass {
    	GtkCMCTreeClass parent_class;
    
    	/* Signal: invoke the popup menu for rows */
    	void (* row_popup_menu) (GtkSCTree *sctree, GdkEventButton *event);
    
    	/* Signal: invoke the popup menu for empty areas */
    	void (* empty_popup_menu) (GtkSCTree *sctree, GdkEventButton *event);

	/* Signal: open the file in the selected row */
	void (* open_row) (GtkSCTree *sctree);

    	/* Signal: initiate a drag and drop operation */
    	void (* start_drag) (GtkSCTree *sctree, gint button, GdkEvent *event);
};


GType gtk_sctree_get_type (void);

GtkWidget *gtk_sctree_new_with_titles	(gint		 columns, 
					 gint		 tree_column, 
					 gchar		*titles[]);
void gtk_sctree_select			(GtkSCTree	*sctree,
					 GtkCMCTreeNode	*node);
void gtk_sctree_select_with_state	(GtkSCTree	*sctree,
					 GtkCMCTreeNode	*node,
					 int		 state);
void gtk_sctree_unselect_all		(GtkSCTree	*sctree);

void gtk_sctree_set_anchor_row		(GtkSCTree	*sctree,
					 GtkCMCTreeNode	*node);

void gtk_sctree_remove_node		(GtkSCTree	*sctree,
					 GtkCMCTreeNode	*node);

void gtk_sctree_set_stripes(GtkSCTree  *sctree, gboolean show_stripes);
void gtk_sctree_set_recursive_expand(GtkSCTree  *sctree, gboolean rec_exp);

/***********************************************************
 *             Tree sorting functions                      *
 ***********************************************************/

void gtk_sctree_sort_node (GtkCMCTree *ctree, GtkCMCTreeNode *node);

void gtk_sctree_sort_recursive (GtkCMCTree *ctree, GtkCMCTreeNode *node);

GtkCMCTreeNode* gtk_sctree_insert_node        (GtkCMCTree *ctree,
                                             GtkCMCTreeNode *parent,
                                             GtkCMCTreeNode *sibling,
                                             gchar *text[],
                                             guint8 spacing,
                                             GdkPixbuf *pixbuf_closed,
                                             GdkPixbuf *pixbuf_opened,
                                             gboolean is_leaf,
                                             gboolean expanded);
void        gtk_sctree_set_node_info        (GtkCMCTree *ctree,
                                             GtkCMCTreeNode *node,
                                             const gchar *text,
                                             guint8 spacing,
                                             GdkPixbuf *pixbuf_closed,
                                             GdkPixbuf *pixbuf_opened,
                                             gboolean is_leaf,
                                             gboolean expanded);
GtkCMCTreeNode *
gtk_sctree_insert_gnode 		    (GtkCMCTree          *ctree,
					     GtkCMCTreeNode      *parent,
					     GtkCMCTreeNode      *sibling,
					     GNode             *gnode,
					     GtkCMCTreeGNodeFunc  func,
					     gpointer           data);

void gtk_sctree_set_column_tooltip	    (GtkSCTree		*sctree,
					     int		 column,
					     const gchar 	*tip);
void gtk_sctree_set_use_markup		    (GtkSCTree		*sctree,
					     int		 column,
					     gboolean		 markup);
gboolean
gtk_sctree_is_hot_spot (GtkSCTree *ctree, 
		       gint      x, 
		       gint      y);
#endif /* __GTK_SCTREE_H__ */
