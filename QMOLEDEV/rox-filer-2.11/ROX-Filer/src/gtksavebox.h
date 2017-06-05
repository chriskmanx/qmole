/*
 * SaveBox widget for the ROX desktop project
 * By Thomas Leonard, <tal197@users.sourceforge.net>.
 */

#ifndef __GTK_SAVEBOX_H__
#define __GTK_SAVEBOX_H__


#include <gdk/gdk.h>
#include <gtk/gtkdialog.h>
#include <gtk/gtkselection.h>


#ifdef __cplusplus
extern "C" {
#endif /* __cplusplus */

/* This is for the 'info' value of the GtkTargetList.
 * It's for the XdndDirectSave0 target - ignore requests for this target
 * because they're handled internally by the widget. Don't use this
 * value for anything else!
 */
#define GTK_TARGET_XDS 0x584453

#define GTK_TYPE_SAVEBOX		(gtk_savebox_get_type ())

#define GTK_SAVEBOX(obj)		\
	(GTK_CHECK_CAST ((obj), GTK_TYPE_SAVEBOX, GtkSavebox))

#define GTK_SAVEBOX_CLASS(klass)	\
	(GTK_CHECK_CLASS_CAST ((klass), GTK_TYPE_SAVEBOX, GtkSaveboxClass))

#define GTK_IS_SAVEBOX(obj) \
	(G_TYPE_CHECK_INSTANCE_TYPE ((obj), GTK_TYPE_SAVEBOX))

#define GTK_IS_SAVEBOX_CLASS(klass)	\
	(GTK_CHECK_CLASS_TYPE ((klass), GTK_TYPE_SAVEBOX))


typedef struct _GtkSavebox        GtkSavebox;
typedef struct _GtkSaveboxClass   GtkSaveboxClass;
typedef struct _GtkSaveboxButton  GtkSaveboxButton;

enum {
  GTK_XDS_SAVED,		/* Done the save - no problem */
  GTK_XDS_SAVE_ERROR,		/* Error during save - reported */
  GTK_XDS_NO_HANDLER,		/* Used internally (sanity checking) */
};

struct _GtkSavebox
{
  GtkDialog dialog;

  GtkWidget *discard_area;	/* Normally hidden */
  GtkWidget *drag_box;		/* Event box - contains pixmap, or NULL */
  GtkWidget *icon;		/* The pixmap widget */
  GtkWidget *entry;		/* Where the pathname goes */

  GtkTargetList *targets;	/* Formats that we can save in */
  gboolean  using_xds;		/* Have we sent XDS reply 'S' or 'F' yet? */
  gboolean  data_sent;		/* Did we send any data at all this drag? */

  GdkDragAction dnd_action;
};

struct _GtkSaveboxClass
{
  GtkDialogClass parent_class;

  gint (*save_to_file)	(GtkSavebox *savebox, guchar *pathname);
  void (*saved_to_uri)	(GtkSavebox *savebox, guchar *uri);
};


GType	   gtk_savebox_get_type 	(void);
GtkWidget* gtk_savebox_new		(const gchar *action);
void	   gtk_savebox_set_icon		(GtkSavebox *savebox,
					 GdkPixbuf *pixbuf);
void	   gtk_savebox_set_pathname	(GtkSavebox *savebox,
					 const gchar *pathname);
void	   gtk_savebox_set_has_discard	(GtkSavebox *savebox, gboolean setting);
void	   gtk_savebox_set_action	(GtkSavebox *savebox,
					 GdkDragAction action);


#ifdef __cplusplus
}
#endif /* __cplusplus */


#endif /* __GTK_SAVEBOX_H__ */
