/*
 * ROX-Filer, filer for the ROX desktop project
 * By Thomas Leonard, <tal197@users.sourceforge.net>.
 */

#ifndef _FILER_H
#define _FILER_H

#include <gtk/gtk.h>

enum {
	RESIZE_STYLE = 0,
	RESIZE_ALWAYS = 1,
	RESIZE_NEVER = 2,
};

typedef enum
{
	OPEN_SHIFT		= 0x01,	/* Do ShiftOpen */
	OPEN_SAME_WINDOW	= 0x02, /* Directories open in same window */
	OPEN_CLOSE_WINDOW	= 0x04, /* Opening files closes the window */
	OPEN_FROM_MINI		= 0x08,	/* Non-dir => close minibuffer */
} OpenFlags;

typedef enum
{
	FILER_NEEDS_RESCAN	= 0x01, /* Call may_rescan after scanning */
	FILER_UPDATING		= 0x02, /* (scanning) items may already exist */
	FILER_CREATE_THUMBS	= 0x04, /* Create thumbs when scan ends */
} FilerFlags;

/* Numbers used in options */
typedef enum
{
	VIEW_TYPE_COLLECTION = 0,	/* Icons view */
	VIEW_TYPE_DETAILS = 1		/* TreeView details list */
} ViewType;

/* Filter types */
typedef enum
{
	FILER_SHOW_ALL,           /* Show all files, modified by show_hidden */
	FILER_SHOW_GLOB,          /* Show files that match a glob pattern */
} FilterType;

/* What to do when all a mount point's windows are closed */
typedef enum {
	UNMOUNT_PROMPT_ASK = GPOINTER_TO_INT(NULL),
	UNMOUNT_PROMPT_NO_CHANGE,
	UNMOUNT_PROMPT_UNMOUNT,
	UNMOUNT_PROMPT_EJECT
} UnmountPrompt;

/* iter's next method has just returned the clicked item... */
typedef void (*TargetFunc)(FilerWindow *filer_window,
			   ViewIter *iter,
			   gpointer data);

struct _FilerWindow
{
	GtkWidget	*window;
	GtkBox		*toplevel_vbox, *view_hbox;
	gboolean	scanning;	/* State of the 'scanning' indicator */
	gchar		*sym_path;		/* Path the user sees */
	gchar		*real_path;		/* realpath(sym_path) */
	ViewIface	*view;
	ViewType	view_type;
	gboolean	temp_item_selected;
	gboolean	show_hidden;
	gboolean	filter_directories;
	FilerFlags	flags;
	SortType	sort_type;
	GtkSortType	sort_order;

	DetailsType	details_type;
	DisplayStyle	display_style;
	DisplayStyle	display_style_wanted;

	Directory	*directory;

	gboolean	had_cursor;	/* (before changing directory) */
	char		*auto_select;	/* If it we find while scanning */

	GtkWidget	*message;	/* The 'Running as ...' message */

	GtkWidget	*minibuffer_area;	/* The hbox to show/hide */
	GtkWidget	*minibuffer_label;	/* The operation name */
	GtkWidget	*minibuffer;		/* The text entry */
	int		mini_cursor_base;	/* XXX */
	MiniType	mini_type;

	FilterType      filter;
	gchar           *filter_string;  /* Glob or regexp pattern */
	gchar           *regexp;         /* Compiled regexp pattern */
	/* TRUE if hidden files are shown because the minibuffer leafname
	 * starts with a dot.
	 */
	gboolean 	temp_show_hidden;

	TargetFunc	target_cb;
	gpointer	target_data;

	GtkWidget	*toolbar;
	GtkWidget	*toolbar_text;
	GtkWidget	*scrollbar;

	gint		open_timeout;	/* Will resize and show window... */

	GtkStateType	selection_state;	/* for drawing selection */
	
	gboolean	show_thumbs;
	GList		*thumb_queue;		/* paths to thumbnail */
	GtkWidget	*thumb_bar, *thumb_progress;
	int		max_thumbs;		/* total for this batch */

	gint		auto_scroll;		/* Timer */

	char		*window_id;		/* For remote control */
};

extern FilerWindow 	*window_with_focus;
extern GList		*all_filer_windows;
extern GHashTable	*child_to_filer;
extern Option		o_filer_auto_resize, o_unique_filer_windows;
extern Option		o_filer_size_limit;

/* Prototypes */
void filer_init(void);
FilerWindow *filer_opendir(const char *path, FilerWindow *src_win, const gchar *wm_class);
gboolean filer_update_dir(FilerWindow *filer_window, gboolean warning);
void filer_update_all(void);
DirItem *filer_selected_item(FilerWindow *filer_window);
void change_to_parent(FilerWindow *filer_window);
void full_refresh(void);
void filer_openitem(FilerWindow *filer_window, ViewIter *iter, OpenFlags flags);
void filer_check_mounted(const char *real_path);
void filer_close_recursive(const char *path);
void filer_change_to(FilerWindow *filer_window,
			const char *path, const char *from);
gboolean filer_exists(FilerWindow *filer_window);
FilerWindow *filer_get_by_id(const char *id);
void filer_set_id(FilerWindow *, const char *id);
void filer_open_parent(FilerWindow *filer_window);
void filer_detach_rescan(FilerWindow *filer_window);
void filer_target_mode(FilerWindow	*filer_window,
			TargetFunc	fn,
			gpointer	data,
			const char	*reason);
GList *filer_selected_items(FilerWindow *filer_window);
void filer_create_thumb(FilerWindow *filer_window, const gchar *pathname);
void filer_cancel_thumbnails(FilerWindow *filer_window);
void filer_set_title(FilerWindow *filer_window);
void filer_create_thumbs(FilerWindow *filer_window);
void filer_add_tip_details(FilerWindow *filer_window,
			   GString *tip, DirItem *item);
void filer_selection_changed(FilerWindow *filer_window, gint time);
void filer_lost_selection(FilerWindow *filer_window, guint time);
void filer_window_set_size(FilerWindow *filer_window, int w, int h);
gboolean filer_window_delete(GtkWidget *window,
			     GdkEvent *unused,
			     FilerWindow *filer_window);
void filer_set_view_type(FilerWindow *filer_window, ViewType type);
void filer_window_toggle_cursor_item_selected(FilerWindow *filer_window);
void filer_perform_action(FilerWindow *filer_window, GdkEventButton *event);
gint filer_motion_notify(FilerWindow *filer_window, GdkEventMotion *event);
gint filer_key_press_event(GtkWidget *widget, GdkEventKey *event,
			   FilerWindow *filer_window);
void filer_set_autoscroll(FilerWindow *filer_window, gboolean auto_scroll);
void filer_refresh(FilerWindow *filer_window);

gboolean filer_match_filter(FilerWindow *filer_window, DirItem *item);
gboolean filer_set_filter(FilerWindow *filer_window,
			  FilterType type, const gchar *filter_string);
void filer_set_filter_directories(FilerWindow *fwin, gboolean filter_directories);
void filer_set_hidden(FilerWindow *fwin, gboolean hidden);
void filer_next_selected(FilerWindow *filer_window, int dir);
void filer_save_settings(FilerWindow *fwin);

UnmountPrompt filer_get_unmount_action(const char *path);
void filer_set_unmount_action(const char *path, UnmountPrompt action);

#endif /* _FILER_H */
