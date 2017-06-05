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

#ifndef __FOLDERVIEW_H__
#define __FOLDERVIEW_H__

typedef struct _FolderViewPopup	FolderViewPopup;
typedef struct _FolderColumnState	FolderColumnState;

#include <glib.h>
#include <gtk/gtk.h>
#include "gtk/gtksctree.h"

#include "mainwindow.h"
#include "viewtypes.h"
#include "folder.h"

typedef enum
{
	F_COL_FOLDER,
	F_COL_NEW,
	F_COL_UNREAD,
	F_COL_TOTAL
} FolderColumnType;

#define N_FOLDER_COLS	4

struct _FolderColumnState
{
	FolderColumnType type;
	gboolean visible;
};

struct _FolderView
{
	GtkWidget *scrolledwin;
	GtkWidget *ctree;

	GHashTable *popups;

	GtkCMCTreeNode *selected;
	GtkCMCTreeNode *opened;

	gboolean open_folder;

	GdkColor color_new;
	GdkColor color_op;

	MainWindow   *mainwin;
	SummaryView  *summaryview;

	gint folder_update_callback_id;
	gint folder_item_update_callback_id;
	
	/* DND states */
	GSList *nodes_to_recollapse;
	guint   drag_timer;		/* timer id */
	FolderItem *drag_item;		/* dragged item */
	GtkCMCTreeNode *drag_node;	/* drag node */
	
	GtkTargetList *target_list; /* DnD */
	FolderColumnState col_state[N_FOLDER_COLS];
	gint col_pos[N_FOLDER_COLS];
	gint deferred_refresh_id;
	Folder *scanning_folder;
	GtkUIManager *ui_manager;
	GtkActionGroup *popup_common_action_group;
	GtkActionGroup *popup_specific_action_group;
	gint scroll_timeout_id;
	gint scroll_value;
};

struct _FolderViewPopup
{
	gchar			 *klass;
	gchar			 *path;
	GtkActionEntry		 *entries;
	gint		 	  n_entries;
	GtkToggleActionEntry	 *toggle_entries;
	gint		 	  n_toggle_entries;
	GtkRadioActionEntry	 *radio_entries;
	gint		  	  n_radio_entries;
	gint			  radio_default;
	void		  	(*radio_callback)	(GtkAction *action, GtkRadioAction *current, gpointer data);
	void			(*add_menuitems)	(GtkUIManager *ui_manager, FolderItem *item);
	void			(*set_sensitivity)	(GtkUIManager *ui_manager, FolderItem *item);
};

void folderview_initialize		(void);
FolderView *folderview_create		(void);
void folderview_init			(FolderView	*folderview);

void folderview_set			(FolderView	*folderview);
void folderview_set_all			(void);

void folderview_select			(FolderView	*folderview,
					 FolderItem	*item);
void folderview_unselect		(FolderView	*folderview);
void folderview_select_next_unread	(FolderView	*folderview, 
					 gboolean 	 force_open);
void folderview_select_next_new		(FolderView	*folderview);
void folderview_select_next_marked	(FolderView	*folderview);

FolderItem *folderview_get_selected_item(FolderView	*folderview);

void folderview_rescan_tree		(Folder		*folder,
					 gboolean	 rebuild);
void folderview_fast_rescan_tree	(Folder 	*folder);
gint folderview_check_new		(Folder		*folder);
void folderview_check_new_all		(void);

void folderview_update_all_updated	(gboolean	 update_summary);

void folderview_move_folder		(FolderView 	*folderview,
					 FolderItem 	*from_folder,
					 FolderItem 	*to_folder,
					 gboolean	 copy);

void folderview_set_target_folder_color (gint		color_op);

void folderview_reinit_fonts		(FolderView *folderview);

void folderview_reflect_prefs		(void);
void folderview_register_popup		(FolderViewPopup	*fpopup);
void folderview_unregister_popup	(FolderViewPopup	*fpopup);
void folderview_update_search_icon	(FolderItem 		*item, 	
					 gboolean 		 matches);
void folderview_set_column_order	(FolderView		*folderview);
void folderview_finish_dnd		(const gchar 		*data, 
					 GdkDragContext 	*drag_context,
			   		 guint 			 time, 
					 FolderItem 		*item);
void folderview_close_opened		(FolderView 		*folderview);

#endif /* __FOLDERVIEW_H__ */
