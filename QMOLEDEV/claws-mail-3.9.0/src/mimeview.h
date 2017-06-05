/*
 * Sylpheed -- a GTK+ based, lightweight, and fast e-mail client
 * Copyright (C) 1999-2012 Hiroyuki Yamamoto and the Claws Mail team
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 * 
 */

#ifndef MIMEVIEW_H
#define MIMEVIEW_H

typedef struct _MimeViewerFactory 	MimeViewerFactory;
typedef struct _MimeViewer 		MimeViewer;

#include <glib.h>
#include <gdk/gdk.h>
#include <gtk/gtk.h>
#ifdef USE_PTHREAD
#include <pthread.h>
#endif

#include "textview.h"
#include "messageview.h"
#include "procmime.h"
#include "noticeview.h"

typedef enum
{
	MIMEVIEW_TEXT,
	MIMEVIEW_VIEWER
} MimeViewType;

#ifdef USE_PTHREAD
typedef struct _SigCheckData SigCheckData;
struct _SigCheckData
{
	pthread_t th;
	pthread_t cancel_th;
	gboolean th_init;
	gboolean cancel_th_init;

	MimeInfo *siginfo;
	gboolean free_after_use;
	gboolean destroy_mimeview;
	gboolean timeout;
};
#endif

struct _MimeView
{
	GtkWidget *hbox;
	GtkWidget *paned;
	GtkWidget *scrolledwin;
	GtkWidget *ctree;
	GtkWidget *mime_notebook;
	GtkWidget *ctree_mainbox;
	GtkWidget *icon_scroll;
	GtkWidget *icon_vbox;
	GtkWidget *icon_mainbox;
	GtkWidget *mime_toggle;
	GtkWidget *scrollbutton;
	MimeViewType type;
	gboolean ctree_mode;

	GtkWidget *popupmenu;

	GtkTreePath *opened;

	TextView *textview;
	MimeViewer *mimeviewer;

	MessageView *messageview;

	MimeInfo *mimeinfo;

	gchar *file;

	GSList *viewers;

	GtkTargetList *target_list; /* DnD */

	gint icon_count;
	MainWindow *mainwin;
#if !GTK_CHECK_VERSION(2,12,0)
	GtkTooltips *tooltips;
#endif

	NoticeView *siginfoview;
	MimeInfo *siginfo;
	MimeInfo *spec_part;
	GtkUIManager *ui_manager;
	GtkActionGroup *action_group;
	gboolean signed_part;

#ifdef USE_PTHREAD
	SigCheckData *check_data;
#endif
};

struct _MimeViewerFactory
{
	/**
         * Array of supported content types.
	 * Must be NULL terminated and lower case
	 */
	gchar **content_types;
	gint priority;
	
	MimeViewer *(*create_viewer) (void);
};

struct _MimeViewer
{
	MimeViewerFactory *factory;
	
	GtkWidget 	*(*get_widget)		(MimeViewer *);
	void 	 	(*show_mimepart)	(MimeViewer *, const gchar *infile, MimeInfo *);
	void		(*clear_viewer)		(MimeViewer *);
	void		(*destroy_viewer)	(MimeViewer *);
	gchar 		*(*get_selection)	(MimeViewer *);
	gboolean	(*scroll_page)		(MimeViewer *, gboolean up);
	void		(*scroll_one_line)	(MimeViewer *, gboolean up);
	gboolean	(*text_search)		(MimeViewer *, gboolean backward,
						 const gchar *str, 
						 gboolean case_sensitive);
	void		(*print)		(MimeViewer *);
	MimeView	*mimeview;
};

#ifdef __cplusplus
extern "C" {
#endif /* __cplusplus */


MimeView *mimeview_create	(MainWindow	*mainwin);
void mimeview_init		(MimeView	*mimeview);
void mimeview_show_message	(MimeView	*mimeview,
				 MimeInfo	*mimeinfo,
				 const gchar	*file);
gboolean mimeview_show_part	(MimeView 	*mimeview, 
				 MimeInfo 	*partinfo);
void mimeview_destroy		(MimeView	*mimeview);
void mimeview_update		(MimeView 	*mimeview);
void mimeview_clear		(MimeView	*mimeview);

MimeInfo *mimeview_get_selected_part	(MimeView	*mimeview);

gboolean mimeview_pass_key_press_event	(MimeView	*mimeview,
					 GdkEventKey	*event);

void mimeview_register_viewer_factory	(MimeViewerFactory *factory);
void mimeview_unregister_viewer_factory	(MimeViewerFactory *factory);
void mimeview_handle_cmd		(MimeView 	*mimeview, 
					 const gchar 	*cmd,
					 GdkEventButton *event,
					 gpointer	 data);
void mimeview_select_mimepart_icon	(MimeView 	*mimeview, 
					 MimeInfo 	*partinfo);
gboolean mimeview_scroll_page		(MimeView 	*mimeview, 
					 gboolean 	 up);
void mimeview_scroll_one_line		(MimeView 	*mimeview, 
					 gboolean 	 up);
gint mimeview_get_selected_part_num	(MimeView 	*mimeview);
void mimeview_select_part_num		(MimeView 	*mimeview, 
					 gint 		 i);
gboolean mimeview_has_viewer_for_content_type
					(MimeView	*mimeview,
					 const gchar	*content_type);
gboolean mimeview_tree_is_empty		(MimeView 	*mimeview);
void mimeview_save_as		(MimeView	*mimeview);
void mimeview_display_as_text	(MimeView	*mimeview);
void mimeview_launch		(MimeView	*mimeview,
				 MimeInfo	*partinfo);
#ifndef G_OS_WIN32
void mimeview_open_with		(MimeView	*mimeview);
#endif
void mimeview_check_signature(MimeView *mimeview);
void mimeview_select_next_part(MimeView *mimeview);
void mimeview_select_prev_part(MimeView *mimeview);


#ifdef __cplusplus
}
#endif /* __cplusplus */

#endif /* __MIMEVIEW_H__ */
