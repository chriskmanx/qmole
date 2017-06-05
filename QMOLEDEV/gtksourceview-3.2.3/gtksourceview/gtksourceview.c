/* -*- Mode: C; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8; coding: utf-8 -*- */
/* gtksourceview.c
 * This file is part of GtkSourceView
 *
 * Copyright (C) 2001 - Mikael Hermansson <tyan@linux.se> and
 *                      Chris Phelps <chicane@reninet.com>
 * Copyright (C) 2002 - Jeroen Zwartepoorte
 * Copyright (C) 2003 - Gustavo Giráldez and Paolo Maggi
 *
 * GtkSourceView is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 *
 * GtkSourceView is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
 */

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <string.h> /* For strlen */

#include <gtk/gtk.h>
#include <gdk/gdkkeysyms.h>
#include <pango/pango-tabs.h>

#include "gtksourceview-i18n.h"

#include "gtksourceview-marshal.h"
#include "gtksourceview-typebuiltins.h"
#include "gtksourcemark.h"
#include "gtksourceview.h"
#include "gtksourcecompletion-private.h"
#include "gtksourcecompletionutils.h"
#include "gtksourcegutter-private.h"
#include "gtksourcegutterrendererlines.h"
#include "gtksourcegutterrenderermarks.h"

/**
 * SECTION:view
 * @Short_description: The view object
 * @Title: GtkSourceView
 * @See_also: #GtkTextView,#GtkSourceBuffer
 *
 * GtkSourceView is the main object of the GtkSourceView library. It provides
 * a text view which syntax highlighting, undo/redo and text marks. Use a
 * #GtkSourceBuffer to display text with a GtkSourceView.
 */

/*
#define ENABLE_DEBUG
*/
#undef ENABLE_DEBUG

/*
#define ENABLE_PROFILE
*/
#undef ENABLE_PROFILE

#ifdef ENABLE_DEBUG
#define DEBUG(x) (x)
#else
#define DEBUG(x)
#endif

#ifdef ENABLE_PROFILE
#define PROFILE(x) (x)
#else
#define PROFILE(x)
#endif

#define GUTTER_PIXMAP 			16
#define DEFAULT_TAB_WIDTH 		8
#define MAX_TAB_WIDTH			32
#define MAX_INDENT_WIDTH		32

#define DEFAULT_RIGHT_MARGIN_POSITION	80
#define MAX_RIGHT_MARGIN_POSITION	1000

#define RIGHT_MARGIN_LINE_ALPHA		40
#define RIGHT_MARGIN_OVERLAY_ALPHA	15

/* Signals */
enum {
	UNDO,
	REDO,
	SHOW_COMPLETION,
	LINE_MARK_ACTIVATED,
	MOVE_LINES,
	MOVE_WORDS,
	SMART_HOME_END,
	LAST_SIGNAL
};

/* Properties */
enum {
	PROP_0,
	PROP_COMPLETION,
	PROP_SHOW_LINE_NUMBERS,
	PROP_SHOW_LINE_MARKS,
	PROP_TAB_WIDTH,
	PROP_INDENT_WIDTH,
	PROP_AUTO_INDENT,
	PROP_INSERT_SPACES,
	PROP_SHOW_RIGHT_MARGIN,
	PROP_RIGHT_MARGIN_POSITION,
	PROP_SMART_HOME_END,
	PROP_HIGHLIGHT_CURRENT_LINE,
	PROP_INDENT_ON_TAB,
	PROP_DRAW_SPACES
};

struct _GtkSourceViewPrivate
{
	GtkSourceStyleScheme *style_scheme;
	GdkRGBA         *right_margin_line_color;
	GdkRGBA         *right_margin_overlay_color;

	GtkSourceDrawSpacesFlags draw_spaces;
	GdkRGBA         *spaces_color;

	GHashTable 	*mark_categories;

	GtkSourceBuffer *source_buffer;

	GtkSourceGutter *left_gutter;
	GtkSourceGutter *right_gutter;

	GtkSourceGutterRenderer *line_renderer;
	GtkSourceGutterRenderer *marks_renderer;

	GdkRGBA          current_line_color;

	GtkSourceCompletion	*completion;

	gulong           notify_buffer_id;
	gint             num_line_digits;

	guint		 right_margin_pos;
	gint             cached_right_margin_pos;
	guint		 tab_width;
	gint		 indent_width;
	GtkSourceSmartHomeEndType smart_home_end;

	guint            tabs_set : 1;
	guint            show_line_numbers : 1;
	guint            show_line_marks : 1;
	guint            auto_indent : 1;
	guint            insert_spaces : 1;
	guint            highlight_current_line : 1;
	guint            indent_on_tab : 1;
	guint            show_right_margin  : 1;
	guint            style_scheme_applied : 1;
	guint            current_line_color_set : 1;
	guint            dispose_has_run : 1;
};

typedef struct _MarkCategory MarkCategory;

struct _MarkCategory
{
	GtkSourceMarkAttributes *attributes;
	gint                     priority;
};

G_DEFINE_TYPE (GtkSourceView, gtk_source_view, GTK_TYPE_TEXT_VIEW)


/* Implement DnD for application/x-color drops */
typedef enum {
	TARGET_COLOR = 200
} GtkSourceViewDropTypes;

static const GtkTargetEntry drop_types[] = {
	{"application/x-color", 0, TARGET_COLOR}
};

static guint signals[LAST_SIGNAL] = { 0 };

typedef enum
{
	ICON_TYPE_NONE,
	ICON_TYPE_PIXBUF,
	ICON_TYPE_STOCK,
	ICON_TYPE_NAME
} IconType;

/* Prototypes. */
static void 	gtk_source_view_dispose			(GObject            *object);
static void 	gtk_source_view_finalize			(GObject            *object);
static void	gtk_source_view_undo 			(GtkSourceView      *view);
static void	gtk_source_view_redo 			(GtkSourceView      *view);
static void	gtk_source_view_show_completion_real	(GtkSourceView      *view);
static void 	set_source_buffer 			(GtkSourceView      *view,
								 GtkTextBuffer      *buffer);
static void	gtk_source_view_populate_popup 		(GtkTextView        *view,
								 GtkMenu            *menu);
static void	gtk_source_view_move_cursor		(GtkTextView        *text_view,
								 GtkMovementStep     step,
								 gint                count,
								 gboolean            extend_selection);
static void 	menu_item_activate_cb 			(GtkWidget          *menu_item,
								 GtkTextView        *text_view);
static void 	gtk_source_view_get_lines			(GtkTextView       *text_view,
								 gint               first_y,
								 gint               last_y,
								 GArray            *buffer_coords,
								 GArray            *line_heights,
								 GArray            *numbers,
								 gint              *countp);
static gboolean gtk_source_view_draw 			(GtkWidget         *widget,
								 cairo_t           *cr);
static void	gtk_source_view_move_lines			(GtkSourceView     *view,
								 gboolean           copy,
								 gint               step);
static void	gtk_source_view_move_words			(GtkSourceView     *view,
								 gint               step);
static gboolean	gtk_source_view_key_press_event		(GtkWidget         *widget,
								 GdkEventKey       *event);
static void	view_dnd_drop 				(GtkTextView       *view,
								 GdkDragContext    *context,
								 gint               x,
								 gint               y,
								 GtkSelectionData  *selection_data,
								 guint              info,
								 guint              timestamp,
								 gpointer           data);
static gint	calculate_real_tab_width 			(GtkSourceView     *view,
								 guint              tab_size,
								 gchar              c);
static void	gtk_source_view_set_property		(GObject           *object,
								 guint              prop_id,
								 const GValue      *value,
								 GParamSpec        *pspec);
static void	gtk_source_view_get_property		(GObject           *object,
								 guint              prop_id,
								 GValue            *value,
								 GParamSpec        *pspec);
static void	gtk_source_view_style_updated			(GtkWidget         *widget);
static void	gtk_source_view_realize			(GtkWidget         *widget);
static void	gtk_source_view_update_style_scheme		(GtkSourceView     *view);

static MarkCategory *mark_category_new                  (GtkSourceMarkAttributes *attributes,
                                                         gint priority);
static void          mark_category_free                 (MarkCategory *category);

static void
gtk_source_view_constructed (GObject *object)
{
	GtkSourceView *view = GTK_SOURCE_VIEW (object);

	set_source_buffer (view, gtk_text_view_get_buffer (GTK_TEXT_VIEW (view)));

	G_OBJECT_CLASS (gtk_source_view_parent_class)->constructed (object);
}

/* Private functions. */
static void
gtk_source_view_class_init (GtkSourceViewClass *klass)
{
	GObjectClass	 *object_class;
	GtkTextViewClass *textview_class;
	GtkBindingSet    *binding_set;
	GtkWidgetClass   *widget_class;

	object_class 	 = G_OBJECT_CLASS (klass);
	textview_class 	 = GTK_TEXT_VIEW_CLASS (klass);
	widget_class 	 = GTK_WIDGET_CLASS (klass);

	object_class->constructed = gtk_source_view_constructed;
	object_class->dispose = gtk_source_view_dispose;
	object_class->finalize = gtk_source_view_finalize;
	object_class->get_property = gtk_source_view_get_property;
	object_class->set_property = gtk_source_view_set_property;

	widget_class->key_press_event = gtk_source_view_key_press_event;
	widget_class->draw = gtk_source_view_draw;
	widget_class->style_updated = gtk_source_view_style_updated;
	widget_class->realize = gtk_source_view_realize;

	textview_class->populate_popup = gtk_source_view_populate_popup;
	textview_class->move_cursor = gtk_source_view_move_cursor;

	klass->undo = gtk_source_view_undo;
	klass->redo = gtk_source_view_redo;
	klass->show_completion = gtk_source_view_show_completion_real;
	klass->move_lines = gtk_source_view_move_lines;
	klass->move_words = gtk_source_view_move_words;

	/**
	 * GtkSourceView:show-line-numbers:
	 *
	 * Whether to display line numbers
	 */
	g_object_class_install_property (object_class,
					 PROP_COMPLETION,
					 g_param_spec_object ("completion",
							      _("Completion"),
							      _("The completion object associated with the view"),
							      GTK_SOURCE_TYPE_COMPLETION,
							      G_PARAM_READABLE));

	/**
	 * GtkSourceView:show-line-numbers:
	 *
	 * Whether to display line numbers
	 */
	g_object_class_install_property (object_class,
					 PROP_SHOW_LINE_NUMBERS,
					 g_param_spec_boolean ("show-line-numbers",
							       _("Show Line Numbers"),
							       _("Whether to display line numbers"),
							       FALSE,
							       G_PARAM_READWRITE));
	/**
	 * GtkSourceView:show-line-marks:
	 *
	 * Whether to display line mark pixbufs
	 */
	g_object_class_install_property (object_class,
					 PROP_SHOW_LINE_MARKS,
					 g_param_spec_boolean ("show-line-marks",
							       _("Show Line Marks"),
							       _("Whether to display line mark pixbufs"),
							       FALSE,
							       G_PARAM_READWRITE));

	/**
	 * GtkSourceView:tab-width:
	 *
	 * Width of an tab character expressed in number of spaces.
	 */
	g_object_class_install_property (object_class,
					 PROP_TAB_WIDTH,
					 g_param_spec_uint ("tab-width",
							    _("Tab Width"),
							    _("Width of a tab character expressed in spaces"),
							    1,
							    MAX_TAB_WIDTH,
							    DEFAULT_TAB_WIDTH,
							    G_PARAM_READWRITE));

	/**
	 * GtkSourceView:indent-width:
	 *
	 * Width of an indentation step expressed in number of spaces.
	 */
	g_object_class_install_property (object_class,
					 PROP_INDENT_WIDTH,
					 g_param_spec_int ("indent-width",
							   _("Indent Width"),
							   _("Number of spaces to use for each step of indent"),
							   -1,
							   MAX_INDENT_WIDTH,
							   -1,
							   G_PARAM_READWRITE));

	g_object_class_install_property (object_class,
					 PROP_AUTO_INDENT,
					 g_param_spec_boolean ("auto_indent",
							       _("Auto Indentation"),
							       _("Whether to enable auto indentation"),
							       FALSE,
							       G_PARAM_READWRITE));

	g_object_class_install_property (object_class,
					 PROP_INSERT_SPACES,
					 g_param_spec_boolean ("insert_spaces_instead_of_tabs",
							       _("Insert Spaces Instead of Tabs"),
							       _("Whether to insert spaces instead of tabs"),
							       FALSE,
							       G_PARAM_READWRITE));

	/**
	 * GtkSourceView:show-right-margin:
	 *
	 * Whether to display the right margin.
	 */
	g_object_class_install_property (object_class,
					 PROP_SHOW_RIGHT_MARGIN,
					 g_param_spec_boolean ("show-right-margin",
							       _("Show Right Margin"),
							       _("Whether to display the right margin"),
							       FALSE,
							       G_PARAM_READWRITE));

	/**
	 * GtkSourceView:right-margin-position:
	 *
	 * Position of the right margin.
	 */
	g_object_class_install_property (object_class,
					 PROP_RIGHT_MARGIN_POSITION,
					 g_param_spec_uint ("right-margin-position",
							    _("Right Margin Position"),
							    _("Position of the right margin"),
							    1,
							    MAX_RIGHT_MARGIN_POSITION,
							    DEFAULT_RIGHT_MARGIN_POSITION,
							    G_PARAM_READWRITE));

	/**
	 * GtkSourceView:smart-home-end:
	 *
	 * Set the behavior of the HOME and END keys.
	 *
	 * Since: 2.0
	 */
	g_object_class_install_property (object_class,
					 PROP_SMART_HOME_END,
					 g_param_spec_enum ("smart_home_end",
							    _("Smart Home/End"),
							    _("HOME and END keys move to first/last "
							      "non whitespace characters on line before going "
							      "to the start/end of the line"),
							    GTK_SOURCE_TYPE_SMART_HOME_END_TYPE,
							    GTK_SOURCE_SMART_HOME_END_DISABLED,
							    G_PARAM_READWRITE));

	g_object_class_install_property (object_class,
					 PROP_HIGHLIGHT_CURRENT_LINE,
					 g_param_spec_boolean ("highlight_current_line",
							       _("Highlight current line"),
							       _("Whether to highlight the current line"),
							       FALSE,
							       G_PARAM_READWRITE));

	g_object_class_install_property (object_class,
					 PROP_INDENT_ON_TAB,
					 g_param_spec_boolean ("indent_on_tab",
							       _("Indent on tab"),
							       _("Whether to indent the selected text when the tab key is pressed"),
							       TRUE,
							       G_PARAM_READWRITE));

	/**
	 * GtkSourceView:draw-spaces:
	 *
	 * Set if and how the spaces should be visualized.
	 *
	 * Since: 2.4
	 */
	g_object_class_install_property (object_class,
					 PROP_DRAW_SPACES,
					 g_param_spec_flags ("draw-spaces",
							    _("Draw Spaces"),
							    _("Set if and how the spaces should be visualized"),
							    GTK_SOURCE_TYPE_DRAW_SPACES_FLAGS,
							    0,
							    G_PARAM_READWRITE));

	signals [UNDO] =
		g_signal_new ("undo",
			      G_TYPE_FROM_CLASS (klass),
			      G_SIGNAL_RUN_LAST | G_SIGNAL_ACTION,
			      G_STRUCT_OFFSET (GtkSourceViewClass, undo),
			      NULL,
			      NULL,
			      _gtksourceview_marshal_VOID__VOID,
			      G_TYPE_NONE,
			      0);

	signals [REDO] =
		g_signal_new ("redo",
			      G_TYPE_FROM_CLASS (klass),
			      G_SIGNAL_RUN_LAST | G_SIGNAL_ACTION,
			      G_STRUCT_OFFSET (GtkSourceViewClass, redo),
			      NULL,
			      NULL,
			      _gtksourceview_marshal_VOID__VOID,
			      G_TYPE_NONE,
			      0);

	/**
	 * GtkSourceView::show-completion:
	 * @view: The #GtkSourceView who emits the signal
	 *
	 * The ::show-completion signal is a keybinding signal which gets
	 * emitted when the user initiates a completion in default mode.
	 *
	 * Applications should not connect to it, but may emit it with
	 * #g_signal_emit_by_name if they need to control the default mode
	 * completion activation.
	 *
	 */
	signals [SHOW_COMPLETION] =
		g_signal_new ("show-completion",
			      G_TYPE_FROM_CLASS (klass),
			      G_SIGNAL_RUN_LAST | G_SIGNAL_ACTION,
			      G_STRUCT_OFFSET (GtkSourceViewClass, show_completion),
			      NULL,
			      NULL,
			      _gtksourceview_marshal_VOID__VOID,
			      G_TYPE_NONE,
			      0);

	/**
	 * GtkSourceView::line-mark-activated:
	 * @view: the #GtkSourceView
	 * @iter: a #GtkTextIter
	 * @event: the #GdkEvent that activated the event
	 *
	 * Emitted when a line mark has been activated (for instance when there
	 * was a button press in the line marks gutter). You can use @iter to
	 * determine on which line the activation took place.
	 */
	signals [LINE_MARK_ACTIVATED] =
		g_signal_new ("line-mark-activated",
			      G_TYPE_FROM_CLASS (klass),
			      G_SIGNAL_RUN_LAST,
			      G_STRUCT_OFFSET (GtkSourceViewClass, line_mark_activated),
			      NULL,
			      NULL,
			      _gtksourceview_marshal_VOID__BOXED_POINTER,
			      G_TYPE_NONE,
			      2,
			      GTK_TYPE_TEXT_ITER,
			      GDK_TYPE_EVENT | G_SIGNAL_TYPE_STATIC_SCOPE);

	/**
	 * GtkSourceView::move-lines:
	 * @view: the #GtkSourceView which received the signal
	 * @copy: %TRUE if the line should be copied,
	 *        %FALSE if it should be moved
	 * @count: the number of lines to move over.
	 *
	 * The ::move-lines signal is a keybinding which gets emitted
	 * when the user initiates moving a line. The default binding key
	 * is Alt+Up/Down arrow. And moves the currently selected lines,
	 * or the current line by @count. For the moment, only
	 * @count of -1 or 1 is valid.
	 *
	 * Since: 2.10
	 *
	 */
	signals [MOVE_LINES] =
		g_signal_new ("move-lines",
			      G_TYPE_FROM_CLASS (klass),
			      G_SIGNAL_RUN_LAST | G_SIGNAL_ACTION,
			      G_STRUCT_OFFSET (GtkSourceViewClass, move_lines),
			      NULL,
			      NULL,
			      _gtksourceview_marshal_VOID__BOOLEAN_INT,
			      G_TYPE_NONE, 2,
			      G_TYPE_BOOLEAN,
			      G_TYPE_INT);

	/**
	 * GtkSourceView::move-words:
	 * @view: the #GtkSourceView which received the signal
	 * @count: the number of words to move over
	 *
	 * The ::move-words signal is a keybinding which gets emitted
	 * when the user initiates moving a word. The default binding key
	 * is Alt+Left/Right Arrow and moves the current selection, or the current
	 * word by one word.
	 *
	 * Since: 3.0
	 */
	signals [MOVE_WORDS] =
		g_signal_new ("move-words",
			      G_TYPE_FROM_CLASS (klass),
			      G_SIGNAL_RUN_LAST | G_SIGNAL_ACTION,
			      G_STRUCT_OFFSET (GtkSourceViewClass, move_words),
			      NULL,
			      NULL,
			      _gtksourceview_marshal_VOID__INT,
			      G_TYPE_NONE, 1,
			      G_TYPE_INT);

	/**
	 * GtkSourceView::smart-home-end:
	 * @view: the #GtkSourceView
	 * @iter: a #GtkTextIter
	 * @count: the count
	 *
	 * Emitted when a the cursor was moved according to the smart home
	 * end setting. The signal is emitted after the cursor is moved, but
	 * during the GtkTextView::move-cursor action. This can be used to find
	 * out whether the cursor was moved by a normal home/end or by a smart
	 * home/end.
	 *
	 * Since: 3.0
	 */
	signals[SMART_HOME_END] =
		g_signal_new ("smart-home-end",
		              G_TYPE_FROM_CLASS (klass),
		              G_SIGNAL_RUN_LAST,
		              0,
		              NULL,
		              NULL,
		              _gtksourceview_marshal_VOID__BOXED_INT,
		              G_TYPE_NONE,
		              2,
		              GTK_TYPE_TEXT_ITER,
		              G_TYPE_INT);

	binding_set = gtk_binding_set_by_class (klass);

	gtk_binding_entry_add_signal (binding_set,
				      GDK_KEY_z,
				      GDK_CONTROL_MASK,
				      "undo", 0);
	gtk_binding_entry_add_signal (binding_set,
				      GDK_KEY_z,
				      GDK_CONTROL_MASK | GDK_SHIFT_MASK,
				      "redo", 0);
	gtk_binding_entry_add_signal (binding_set,
				      GDK_KEY_F14,
				      0,
				      "undo", 0);
	gtk_binding_entry_add_signal (binding_set,
				      GDK_KEY_space,
				      GDK_CONTROL_MASK,
				      "show-completion", 0);

	gtk_binding_entry_add_signal (binding_set,
				      GDK_KEY_Up,
				      GDK_MOD1_MASK,
				      "move_lines", 2,
				      G_TYPE_BOOLEAN, FALSE,
				      G_TYPE_INT, -1);
	gtk_binding_entry_add_signal (binding_set,
				      GDK_KEY_KP_Up,
				      GDK_MOD1_MASK,
				      "move_lines", 2,
				      G_TYPE_BOOLEAN, FALSE,
				      G_TYPE_INT, -1);
	gtk_binding_entry_add_signal (binding_set,
				      GDK_KEY_Down,
				      GDK_MOD1_MASK,
				      "move_lines", 2,
				      G_TYPE_BOOLEAN, FALSE,
				      G_TYPE_INT, 1);
	gtk_binding_entry_add_signal (binding_set,
				      GDK_KEY_KP_Down,
				      GDK_MOD1_MASK,
				      "move_lines", 2,
				      G_TYPE_BOOLEAN, FALSE,
				      G_TYPE_INT, 1);

	gtk_binding_entry_add_signal (binding_set,
				      GDK_KEY_Left,
				      GDK_MOD1_MASK,
				      "move_words", 1,
				      G_TYPE_INT, -1);
	gtk_binding_entry_add_signal (binding_set,
				      GDK_KEY_KP_Left,
				      GDK_MOD1_MASK,
				      "move_words", 1,
				      G_TYPE_INT, -1);
	gtk_binding_entry_add_signal (binding_set,
				      GDK_KEY_Right,
				      GDK_MOD1_MASK,
				      "move_words", 1,
				      G_TYPE_INT, 1);
	gtk_binding_entry_add_signal (binding_set,
				      GDK_KEY_KP_Right,
				      GDK_MOD1_MASK,
				      "move_words", 1,
				      G_TYPE_INT, 1);

	gtk_binding_entry_add_signal (binding_set,
				      GDK_KEY_Up,
				      GDK_MOD1_MASK | GDK_SHIFT_MASK,
				      "move_viewport", 2,
				      GTK_TYPE_SCROLL_STEP, GTK_SCROLL_STEPS,
				      G_TYPE_INT, -1);

	gtk_binding_entry_add_signal (binding_set,
				      GDK_KEY_KP_Up,
				      GDK_MOD1_MASK | GDK_SHIFT_MASK,
				      "move_viewport", 2,
				      GTK_TYPE_SCROLL_STEP, GTK_SCROLL_STEPS,
				      G_TYPE_INT, -1);

	gtk_binding_entry_add_signal (binding_set,
				      GDK_KEY_Down,
				      GDK_MOD1_MASK | GDK_SHIFT_MASK,
				      "move_viewport", 2,
				      GTK_TYPE_SCROLL_STEP, GTK_SCROLL_STEPS,
				      G_TYPE_INT, 1);

	gtk_binding_entry_add_signal (binding_set,
				      GDK_KEY_KP_Down,
				      GDK_MOD1_MASK | GDK_SHIFT_MASK,
				      "move_viewport", 2,
				      GTK_TYPE_SCROLL_STEP, GTK_SCROLL_STEPS,
				      G_TYPE_INT, 1);

	gtk_binding_entry_add_signal (binding_set,
				      GDK_KEY_Page_Up,
				      GDK_MOD1_MASK | GDK_SHIFT_MASK,
				      "move_viewport", 2,
				      GTK_TYPE_SCROLL_STEP, GTK_SCROLL_PAGES,
				      G_TYPE_INT, -1);

	gtk_binding_entry_add_signal (binding_set,
				      GDK_KEY_KP_Page_Up,
				      GDK_MOD1_MASK | GDK_SHIFT_MASK,
				      "move_viewport", 2,
				      GTK_TYPE_SCROLL_STEP, GTK_SCROLL_PAGES,
				      G_TYPE_INT, -1);

	gtk_binding_entry_add_signal (binding_set,
				      GDK_KEY_Page_Down,
				      GDK_MOD1_MASK | GDK_SHIFT_MASK,
				      "move_viewport", 2,
				      GTK_TYPE_SCROLL_STEP, GTK_SCROLL_PAGES,
				      G_TYPE_INT, 1);

	gtk_binding_entry_add_signal (binding_set,
				      GDK_KEY_KP_Page_Down,
				      GDK_MOD1_MASK | GDK_SHIFT_MASK,
				      "move_viewport", 2,
				      GTK_TYPE_SCROLL_STEP, GTK_SCROLL_PAGES,
				      G_TYPE_INT, 1);

	gtk_binding_entry_add_signal (binding_set,
				      GDK_KEY_Home,
				      GDK_MOD1_MASK | GDK_SHIFT_MASK,
				      "move_viewport", 2,
				      GTK_TYPE_SCROLL_STEP, GTK_SCROLL_ENDS,
				      G_TYPE_INT, -1);

	gtk_binding_entry_add_signal (binding_set,
				      GDK_KEY_KP_Home,
				      GDK_MOD1_MASK | GDK_SHIFT_MASK,
				      "move_viewport", 2,
				      GTK_TYPE_SCROLL_STEP, GTK_SCROLL_ENDS,
				      G_TYPE_INT, -1);

	gtk_binding_entry_add_signal (binding_set,
				      GDK_KEY_End,
				      GDK_MOD1_MASK | GDK_SHIFT_MASK,
				      "move_viewport", 2,
				      GTK_TYPE_SCROLL_STEP, GTK_SCROLL_ENDS,
				      G_TYPE_INT, 1);

	gtk_binding_entry_add_signal (binding_set,
				      GDK_KEY_KP_End,
				      GDK_MOD1_MASK | GDK_SHIFT_MASK,
				      "move_viewport", 2,
				      GTK_TYPE_SCROLL_STEP, GTK_SCROLL_ENDS,
				      G_TYPE_INT, 1);

	g_type_class_add_private (object_class, sizeof (GtkSourceViewPrivate));
}

static void
gtk_source_view_set_property (GObject      *object,
			      guint         prop_id,
			      const GValue *value,
			      GParamSpec   *pspec)
{
	GtkSourceView *view;

	g_return_if_fail (GTK_SOURCE_IS_VIEW (object));

	view = GTK_SOURCE_VIEW (object);

	switch (prop_id)
	{
		case PROP_SHOW_LINE_NUMBERS:
			gtk_source_view_set_show_line_numbers (view,
							       g_value_get_boolean (value));
			break;

		case PROP_SHOW_LINE_MARKS:
			gtk_source_view_set_show_line_marks (view,
							     g_value_get_boolean (value));
			break;

		case PROP_TAB_WIDTH:
			gtk_source_view_set_tab_width (view,
						       g_value_get_uint (value));
			break;

		case PROP_INDENT_WIDTH:
			gtk_source_view_set_indent_width (view,
							  g_value_get_int (value));
			break;

		case PROP_AUTO_INDENT:
			gtk_source_view_set_auto_indent (view,
							 g_value_get_boolean (value));
			break;

		case PROP_INSERT_SPACES:
			gtk_source_view_set_insert_spaces_instead_of_tabs (
							view,
							g_value_get_boolean (value));
			break;

		case PROP_SHOW_RIGHT_MARGIN:
			gtk_source_view_set_show_right_margin (view,
							       g_value_get_boolean (value));
			break;

		case PROP_RIGHT_MARGIN_POSITION:
			gtk_source_view_set_right_margin_position (view,
								   g_value_get_uint (value));
			break;

		case PROP_SMART_HOME_END:
			gtk_source_view_set_smart_home_end (view,
							    g_value_get_enum (value));
			break;

		case PROP_HIGHLIGHT_CURRENT_LINE:
			gtk_source_view_set_highlight_current_line (view,
								    g_value_get_boolean (value));
			break;

		case PROP_INDENT_ON_TAB:
			gtk_source_view_set_indent_on_tab (view,
							   g_value_get_boolean (value));
			break;

		case PROP_DRAW_SPACES:
			gtk_source_view_set_draw_spaces (view,
							 g_value_get_flags (value));
			break;

		default:
			G_OBJECT_WARN_INVALID_PROPERTY_ID (object, prop_id, pspec);
			break;
	}
}

static void
gtk_source_view_get_property (GObject    *object,
			      guint       prop_id,
			      GValue     *value,
			      GParamSpec *pspec)
{
	GtkSourceView *view;

	g_return_if_fail (GTK_SOURCE_IS_VIEW (object));

	view = GTK_SOURCE_VIEW (object);

	switch (prop_id)
	{
		case PROP_COMPLETION:
			g_value_set_object (value,
			                    gtk_source_view_get_completion (view));
			break;
		case PROP_SHOW_LINE_NUMBERS:
			g_value_set_boolean (value,
					     gtk_source_view_get_show_line_numbers (view));
			break;

		case PROP_SHOW_LINE_MARKS:
			g_value_set_boolean (value,
					     gtk_source_view_get_show_line_marks (view));
			break;

		case PROP_TAB_WIDTH:
			g_value_set_uint (value,
					  gtk_source_view_get_tab_width (view));
			break;

		case PROP_INDENT_WIDTH:
			g_value_set_int (value,
					 gtk_source_view_get_indent_width (view));
			break;

		case PROP_AUTO_INDENT:
			g_value_set_boolean (value,
					     gtk_source_view_get_auto_indent (view));
			break;

		case PROP_INSERT_SPACES:
			g_value_set_boolean (value,
					     gtk_source_view_get_insert_spaces_instead_of_tabs (view));
			break;

		case PROP_SHOW_RIGHT_MARGIN:
			g_value_set_boolean (value,
					     gtk_source_view_get_show_right_margin (view));
			break;

		case PROP_RIGHT_MARGIN_POSITION:
			g_value_set_uint (value,
					  gtk_source_view_get_right_margin_position (view));
			break;

		case PROP_SMART_HOME_END:
			g_value_set_enum (value,
					  gtk_source_view_get_smart_home_end (view));
			break;

		case PROP_HIGHLIGHT_CURRENT_LINE:
			g_value_set_boolean (value,
					     gtk_source_view_get_highlight_current_line (view));
			break;

		case PROP_INDENT_ON_TAB:
			g_value_set_boolean (value,
					     gtk_source_view_get_indent_on_tab (view));
			break;

		case PROP_DRAW_SPACES:
			g_value_set_flags (value,
					   gtk_source_view_get_draw_spaces (view));
			break;

		default:
			G_OBJECT_WARN_INVALID_PROPERTY_ID (object, prop_id, pspec);
			break;
	}
}

static void
notify_buffer (GtkSourceView *view)
{
	set_source_buffer (view,
		gtk_text_view_get_buffer (GTK_TEXT_VIEW (view)));
}

static void
gutter_renderer_marks_activate (GtkSourceGutterRenderer *renderer,
                                GtkTextIter             *iter,
                                const GdkRectangle      *area,
                                GdkEvent                *event,
                                GtkSourceView           *view)
{
	g_signal_emit (view,
	               signals[LINE_MARK_ACTIVATED],
	               0,
	               iter,
	               event);
}

static void
init_left_gutter (GtkSourceView *view)
{
	GtkSourceGutter *gutter;

	gutter = gtk_source_view_get_gutter (view, GTK_TEXT_WINDOW_LEFT);

	view->priv->line_renderer = gtk_source_gutter_renderer_lines_new ();
	g_object_set (view->priv->line_renderer,
	              "alignment-mode", GTK_SOURCE_GUTTER_RENDERER_ALIGNMENT_MODE_FIRST,
	              "yalign", 0.5,
	              "xalign", 1.0,
	              "xpad", 3,
	              "visible", FALSE,
	              NULL);

	gtk_source_gutter_insert (gutter,
	                          view->priv->line_renderer,
	                          GTK_SOURCE_VIEW_GUTTER_POSITION_LINES);

	view->priv->marks_renderer = gtk_source_gutter_renderer_marks_new ();
	g_object_set (view->priv->marks_renderer,
	              "visible", FALSE,
	              NULL);

	gtk_source_gutter_insert (gutter,
	                          view->priv->marks_renderer,
	                          GTK_SOURCE_VIEW_GUTTER_POSITION_MARKS);

	g_signal_connect (view->priv->marks_renderer,
	                  "activate",
	                  G_CALLBACK (gutter_renderer_marks_activate),
	                  view);
}

static void
gtk_source_view_init (GtkSourceView *view)
{
	GtkTargetList *tl;

	view->priv = G_TYPE_INSTANCE_GET_PRIVATE (view, GTK_SOURCE_TYPE_VIEW,
						  GtkSourceViewPrivate);

	view->priv->tab_width = DEFAULT_TAB_WIDTH;
	view->priv->tabs_set = FALSE;
	view->priv->indent_width = -1;
	view->priv->indent_on_tab = TRUE;
	view->priv->smart_home_end = GTK_SOURCE_SMART_HOME_END_DISABLED;
	view->priv->right_margin_pos = DEFAULT_RIGHT_MARGIN_POSITION;
	view->priv->cached_right_margin_pos = -1;

	gtk_text_view_set_left_margin (GTK_TEXT_VIEW (view), 2);
	gtk_text_view_set_right_margin (GTK_TEXT_VIEW (view), 2);

	view->priv->right_margin_line_color = NULL;
	view->priv->right_margin_overlay_color = NULL;
	view->priv->spaces_color = NULL;

	view->priv->mark_categories = g_hash_table_new_full (g_str_hash,
	                                                     g_str_equal,
	                                                     (GDestroyNotify) g_free,
	                                                     (GDestroyNotify) mark_category_free);

	init_left_gutter (view);

	tl = gtk_drag_dest_get_target_list (GTK_WIDGET (view));
	g_return_if_fail (tl != NULL);

	gtk_target_list_add_table (tl, drop_types, G_N_ELEMENTS (drop_types));

	gtk_widget_set_has_tooltip (GTK_WIDGET (view), TRUE);

	g_signal_connect (view,
			  "drag_data_received",
			  G_CALLBACK (view_dnd_drop),
			  NULL);

	view->priv->notify_buffer_id = g_signal_connect (view,
							 "notify::buffer",
							 G_CALLBACK (notify_buffer),
							 NULL);
}

static void
gtk_source_view_dispose (GObject *object)
{
	GtkSourceView *view = GTK_SOURCE_VIEW (object);

	/* notify_buffer() would recreate the buffer if it is set to null,
	 * and we don't want that to happen when destroying/finalizing */
	if (view->priv->notify_buffer_id)
	{
		g_signal_handler_disconnect (view, view->priv->notify_buffer_id);
		view->priv->notify_buffer_id = 0;
	}
	set_source_buffer (view, NULL);

	if (view->priv->completion != NULL)
	{
		g_object_unref (view->priv->completion);
		view->priv->completion = NULL;
	}

	if (view->priv->left_gutter)
	{
		g_object_unref (view->priv->left_gutter);
		view->priv->left_gutter = NULL;
	}

	if (view->priv->right_gutter)
	{
		g_object_unref (view->priv->right_gutter);
		view->priv->right_gutter = NULL;
	}

	view->priv->dispose_has_run = 1;

	G_OBJECT_CLASS (gtk_source_view_parent_class)->dispose (object);
}

static void
gtk_source_view_finalize (GObject *object)
{
	GtkSourceView *view;

	g_return_if_fail (object != NULL);
	g_return_if_fail (GTK_SOURCE_IS_VIEW (object));

	view = GTK_SOURCE_VIEW (object);

	if (view->priv->style_scheme)
	{
		g_object_unref (view->priv->style_scheme);
	}

	if (view->priv->right_margin_line_color != NULL)
	{
		gdk_rgba_free (view->priv->right_margin_line_color);
	}

	if (view->priv->right_margin_overlay_color != NULL)
	{
		gdk_rgba_free (view->priv->right_margin_overlay_color);
	}

	if (view->priv->spaces_color != NULL)
	{
		gdk_rgba_free (view->priv->spaces_color);
	}

	if (view->priv->mark_categories)
	{
		g_hash_table_destroy (view->priv->mark_categories);
	}

	G_OBJECT_CLASS (gtk_source_view_parent_class)->finalize (object);
}

static void
highlight_updated_cb (GtkSourceBuffer *buffer,
		      GtkTextIter     *start,
		      GtkTextIter     *end,
		      GtkTextView     *text_view)
{
	GdkRectangle visible_rect;
	GdkRectangle updated_rect;
	GdkRectangle redraw_rect;
	gint y;
	gint height;

	/* get visible area */
	gtk_text_view_get_visible_rect (text_view, &visible_rect);

	/* get updated rectangle */
	gtk_text_view_get_line_yrange (text_view, start, &y, &height);
	updated_rect.y = y;
	gtk_text_view_get_line_yrange (text_view, end, &y, &height);
	updated_rect.height = y + height - updated_rect.y;
	updated_rect.x = visible_rect.x;
	updated_rect.width = visible_rect.width;

	DEBUG ({
		g_print ("> highlight_updated start\n");
		g_print ("    lines udpated: %d - %d\n",
			 gtk_text_iter_get_line (start),
			 gtk_text_iter_get_line (end));
		g_print ("    visible area: %d - %d\n",
			 visible_rect.y, visible_rect.y + visible_rect.height);
		g_print ("    updated area: %d - %d\n",
			 updated_rect.y, updated_rect.y + updated_rect.height);
	});

	/* intersect both rectangles to see whether we need to queue a redraw */
	if (gdk_rectangle_intersect (&updated_rect, &visible_rect, &redraw_rect))
	{
		GdkRectangle widget_rect;

		gtk_text_view_buffer_to_window_coords (text_view,
						       GTK_TEXT_WINDOW_WIDGET,
						       redraw_rect.x,
						       redraw_rect.y,
						       &widget_rect.x,
						       &widget_rect.y);

		widget_rect.width = redraw_rect.width;
		widget_rect.height = redraw_rect.height;

		DEBUG ({
			g_print ("    invalidating: %d - %d\n",
				 widget_rect.y, widget_rect.y + widget_rect.height);
		});

		gtk_widget_queue_draw_area (GTK_WIDGET (text_view),
					    widget_rect.x,
					    widget_rect.y,
					    widget_rect.width,
					    widget_rect.height);
	}

	DEBUG ({
		g_print ("> highlight_updated end\n");
	});
}

static void
source_mark_updated_cb (GtkSourceBuffer *buffer,
			GtkSourceMark	*mark,
			GtkTextView     *text_view)
{
	/* TODO do something more intelligent here, namely
	 * invalidate only the area under the mark if possible */
	gtk_widget_queue_draw (GTK_WIDGET (text_view));
}

static void
buffer_style_scheme_changed_cb (GtkSourceBuffer *buffer,
				GParamSpec	*pspec,
				GtkSourceView   *view)
{
	gtk_source_view_update_style_scheme (view);
}

static void
set_source_buffer (GtkSourceView *view,
		   GtkTextBuffer *buffer)
{
	if (buffer == (GtkTextBuffer*) view->priv->source_buffer)
		return;

	if (view->priv->source_buffer)
	{
		g_signal_handlers_disconnect_by_func (view->priv->source_buffer,
						      highlight_updated_cb,
						      view);
		g_signal_handlers_disconnect_by_func (view->priv->source_buffer,
						      source_mark_updated_cb,
						      view);
		g_signal_handlers_disconnect_by_func (view->priv->source_buffer,
						      buffer_style_scheme_changed_cb,
						      view);

		g_object_unref (view->priv->source_buffer);
	}

	if (buffer && GTK_SOURCE_IS_BUFFER (buffer))
	{
		view->priv->source_buffer = g_object_ref (buffer);

		g_signal_connect (buffer,
				  "highlight_updated",
				  G_CALLBACK (highlight_updated_cb),
				  view);
		g_signal_connect (buffer,
				  "source_mark_updated",
				  G_CALLBACK (source_mark_updated_cb),
				  view);
		g_signal_connect (buffer,
				  "notify::style-scheme",
				  G_CALLBACK (buffer_style_scheme_changed_cb),
				  view);
	}
	else
	{
		view->priv->source_buffer = NULL;
	}

	/* if buffer isn't NULL then we aren't being destroyed, so call
	 * gtk_source_view_update_style_scheme(), that will check whether it's
	 * a GtkSourceBuffer or not */
	if (buffer)
	{
		gtk_source_view_update_style_scheme (view);
	}
}

static void
scroll_to_insert (GtkSourceView *view,
		  GtkTextBuffer *buffer)
{
	GtkTextMark *insert;
	GtkTextIter iter;
	GdkRectangle visible, location;

	insert = gtk_text_buffer_get_insert (buffer);
	gtk_text_buffer_get_iter_at_mark (buffer, &iter, insert);

	gtk_text_view_get_visible_rect (GTK_TEXT_VIEW (view), &visible);
	gtk_text_view_get_iter_location (GTK_TEXT_VIEW (view), &iter, &location);

	if (location.y < visible.y || location.y > visible.y + visible.height)
	{
		gtk_text_view_scroll_to_mark (GTK_TEXT_VIEW (view),
					      insert,
					      0.0,
					      TRUE,
					      0.5, 0.5);
	}
	else if (location.x < visible.x || location.x > visible.x + visible.width)
	{
		gdouble position;
		GtkAdjustment *adjustment;

		/* We revert the vertical position of the view because
		 * _scroll_to_iter will cause it to move and the
		 * insert mark is already visible vertically. */

		adjustment = gtk_scrollable_get_vadjustment (GTK_SCROLLABLE (view));
		position = gtk_adjustment_get_value (adjustment);

		/* Must use _to_iter as _to_mark scrolls in an
		 * idle handler and would prevent use from
		 * reverting the vertical position of the view. */
		gtk_text_view_scroll_to_iter (GTK_TEXT_VIEW (view),
					      &iter,
					      0.0,
					      TRUE,
					      0.5, 0.0);

		gtk_adjustment_set_value (adjustment, position);
	}
}

static void
gtk_source_view_undo (GtkSourceView *view)
{
	GtkTextBuffer *buffer;

	g_return_if_fail (GTK_SOURCE_IS_VIEW (view));

	buffer = gtk_text_view_get_buffer (GTK_TEXT_VIEW (view));

	if (gtk_text_view_get_editable (GTK_TEXT_VIEW (view)) &&
	    GTK_SOURCE_IS_BUFFER (buffer) &&
	    gtk_source_buffer_can_undo (GTK_SOURCE_BUFFER (buffer)))
	{
		gtk_source_buffer_undo (GTK_SOURCE_BUFFER (buffer));
		scroll_to_insert (view, buffer);
	}
}

static void
gtk_source_view_redo (GtkSourceView *view)
{
	GtkTextBuffer *buffer;

	g_return_if_fail (GTK_SOURCE_IS_VIEW (view));

	buffer = gtk_text_view_get_buffer (GTK_TEXT_VIEW (view));

	if (gtk_text_view_get_editable (GTK_TEXT_VIEW (view)) &&
	    GTK_SOURCE_IS_BUFFER (buffer) &&
	    gtk_source_buffer_can_redo (GTK_SOURCE_BUFFER (buffer)))
	{
		gtk_source_buffer_redo (GTK_SOURCE_BUFFER (buffer));
		scroll_to_insert (view, buffer);
	}
}

static GList *
get_user_requested_providers (GtkSourceCompletion *completion)
{
	GList *item;
	GList *ret = NULL;

	item = gtk_source_completion_get_providers (completion);

	while (item)
	{
		GtkSourceCompletionProvider *provider;

		provider = GTK_SOURCE_COMPLETION_PROVIDER (item->data);

		if (gtk_source_completion_provider_get_activation (provider) &
		    GTK_SOURCE_COMPLETION_ACTIVATION_USER_REQUESTED)
		{
			ret = g_list_prepend (ret, provider);
		}

		item = g_list_next (item);
	}

	return g_list_reverse (ret);
}

static void
gtk_source_view_show_completion_real (GtkSourceView *view)
{
	GtkSourceCompletion *completion;
	GtkSourceCompletionContext *context;
	GList *providers;

	completion = gtk_source_view_get_completion (view);
	context = gtk_source_completion_create_context (completion, NULL);

	g_object_set (context,
	              "activation",
	              GTK_SOURCE_COMPLETION_ACTIVATION_USER_REQUESTED,
	              NULL);

	providers = get_user_requested_providers (completion);

	gtk_source_completion_show (completion,
	                            providers,
	                            context);

	g_list_free (providers);
}

static void
gtk_source_view_populate_popup (GtkTextView *text_view,
				GtkMenu     *menu)
{
	GtkTextBuffer *buffer;
	GtkWidget *menu_item;

	buffer = gtk_text_view_get_buffer (text_view);
	if (!GTK_SOURCE_IS_BUFFER (buffer))
		return;

	/* separator */
	menu_item = gtk_separator_menu_item_new ();
	gtk_menu_shell_prepend (GTK_MENU_SHELL (menu), menu_item);
	gtk_widget_show (menu_item);

	/* create redo menu_item. */
	menu_item = gtk_image_menu_item_new_from_stock ("gtk-redo", NULL);
	g_object_set_data (G_OBJECT (menu_item), "gtk-signal", "redo");
	g_signal_connect (G_OBJECT (menu_item), "activate",
			  G_CALLBACK (menu_item_activate_cb), text_view);
	gtk_menu_shell_prepend (GTK_MENU_SHELL (menu), menu_item);
	gtk_widget_set_sensitive (menu_item,
				  (gtk_text_view_get_editable (text_view) &&
				   gtk_source_buffer_can_redo (GTK_SOURCE_BUFFER (buffer))));
	gtk_widget_show (menu_item);

	/* create undo menu_item. */
	menu_item = gtk_image_menu_item_new_from_stock ("gtk-undo", NULL);
	g_object_set_data (G_OBJECT (menu_item), "gtk-signal", "undo");
	g_signal_connect (G_OBJECT (menu_item), "activate",
			  G_CALLBACK (menu_item_activate_cb), text_view);
	gtk_menu_shell_prepend (GTK_MENU_SHELL (menu), menu_item);
	gtk_widget_set_sensitive (menu_item,
				  (gtk_text_view_get_editable (text_view) &&
				   gtk_source_buffer_can_undo (GTK_SOURCE_BUFFER (buffer))));
	gtk_widget_show (menu_item);
}

static void
move_cursor (GtkTextView       *text_view,
	     const GtkTextIter *new_location,
	     gboolean           extend_selection)
{
	GtkTextBuffer *buffer = gtk_text_view_get_buffer (text_view);

	if (extend_selection)
		gtk_text_buffer_move_mark_by_name (buffer, "insert",
						   new_location);
	else
		gtk_text_buffer_place_cursor (buffer, new_location);

	gtk_text_view_scroll_mark_onscreen (text_view,
					    gtk_text_buffer_get_insert (buffer));
}

static void
move_to_first_char (GtkTextView *text_view,
                    GtkTextIter *iter,
                    gboolean     display_line)
{
	GtkTextIter last;

	last = *iter;

	if (display_line)
	{
		gtk_text_view_backward_display_line_start (text_view, iter);
		gtk_text_view_forward_display_line_end (text_view, &last);
	}
	else
	{
		gtk_text_iter_set_line_offset (iter, 0);

		if (!gtk_text_iter_ends_line (&last))
		{
			gtk_text_iter_forward_to_line_end (&last);
		}
	}


	while (gtk_text_iter_compare (iter, &last) < 0)
	{
		gunichar c;

		c = gtk_text_iter_get_char (iter);

		if (g_unichar_isspace (c))
		{
			if (!gtk_text_iter_forward_visible_cursor_position (iter))
			{
				break;
			}
		}
		else
		{
			break;
		}
	}
}

static void
move_to_last_char (GtkTextView *text_view,
                   GtkTextIter *iter,
                   gboolean     display_line)
{
	GtkTextIter first;

	first = *iter;

	if (display_line)
	{
		gtk_text_view_forward_display_line_end (text_view, iter);
		gtk_text_view_backward_display_line_start (text_view, &first);
	}
	else
	{
		if (!gtk_text_iter_ends_line (iter))
		{
			gtk_text_iter_forward_to_line_end (iter);
		}

		gtk_text_iter_set_line_offset (&first, 0);
	}

	while (gtk_text_iter_compare (iter, &first) > 0)
	{
		gunichar c;

		if (!gtk_text_iter_backward_visible_cursor_position (iter))
		{
			break;
		}

		c = gtk_text_iter_get_char (iter);

		if (!g_unichar_isspace (c))
		{
			/* We've gone one cursor position too far. */
			if (!gtk_text_iter_forward_visible_cursor_position (iter))
			{
				/* There is some kind of bug where it won't move
				   to the last cursor position... */
				gtk_text_iter_forward_to_end (iter);
			}

			break;
		}
	}
}

static void
do_cursor_move_home_end (GtkTextView *text_view,
                         GtkTextIter *cur,
                         GtkTextIter *iter,
                         gboolean     extend_selection,
                         gint         count)
{
	/* if we are clearing selection, we need to move_cursor even
	 * if we are at proper iter because selection_bound may need
	 * to be moved */
	if (!gtk_text_iter_equal (cur, iter) || !extend_selection)
	{
		move_cursor (text_view, iter, extend_selection);
		g_signal_emit (text_view, signals[SMART_HOME_END], 0, iter, count);
	}
}

static void
gtk_source_view_move_cursor (GtkTextView    *text_view,
			     GtkMovementStep step,
			     gint            count,
			     gboolean        extend_selection)
{
	GtkSourceView *source_view = GTK_SOURCE_VIEW (text_view);
	GtkTextBuffer *buffer = gtk_text_view_get_buffer (text_view);
	GtkTextMark *mark;
	GtkTextIter cur, iter;
	gboolean move_display_line;
	gboolean move_paragraph;
	gboolean is_home_end;

	mark = gtk_text_buffer_get_insert (buffer);
	gtk_text_buffer_get_iter_at_mark (buffer, &cur, mark);
	iter = cur;

	move_display_line = step == GTK_MOVEMENT_DISPLAY_LINE_ENDS;
	move_paragraph = step == GTK_MOVEMENT_PARAGRAPH_ENDS;
	is_home_end = move_display_line || move_paragraph;

	if (is_home_end && count == -1)
	{
		gboolean athome;

		move_to_first_char (text_view, &iter, move_display_line);

		if (move_display_line)
		{
			athome = gtk_text_view_starts_display_line (text_view, &cur);
		}
		else
		{
			athome = gtk_text_iter_starts_line (&cur);
		}

		switch (source_view->priv->smart_home_end)
		{
			case GTK_SOURCE_SMART_HOME_END_BEFORE:
				if (!gtk_text_iter_equal (&cur, &iter) || athome)
				{
					do_cursor_move_home_end (text_view,
					                         &cur,
					                         &iter,
					                         extend_selection,
					                         count);
					return;
				}
				break;

			case GTK_SOURCE_SMART_HOME_END_AFTER:
				if (athome)
				{
					do_cursor_move_home_end (text_view,
					                         &cur,
					                         &iter,
					                         extend_selection,
					                         count);
					return;
				}
				break;

			case GTK_SOURCE_SMART_HOME_END_ALWAYS:
				do_cursor_move_home_end (text_view,
				                         &cur,
				                         &iter,
				                         extend_selection,
				                         count);
				return;

			default:
				break;
		}
	}
	else if (is_home_end && count == 1)
	{
		gboolean atend;

		move_to_last_char (text_view, &iter, move_display_line);

		if (move_display_line)
		{
			GtkTextIter display_end;
			display_end = cur;

			gtk_text_view_forward_display_line_end (text_view, &display_end);
			atend = gtk_text_iter_equal (&cur, &display_end);
		}
		else
		{
			atend = gtk_text_iter_ends_line (&cur);
		}

		switch (source_view->priv->smart_home_end)
		{
			case GTK_SOURCE_SMART_HOME_END_BEFORE:
				if (!gtk_text_iter_equal (&cur, &iter) || atend)
				{
					do_cursor_move_home_end (text_view,
					                         &cur,
					                         &iter,
					                         extend_selection,
					                         count);
					return;
				}
				break;

			case GTK_SOURCE_SMART_HOME_END_AFTER:
				if (atend)
				{
					do_cursor_move_home_end (text_view,
					                         &cur,
					                         &iter,
					                         extend_selection,
					                         count);
					return;
				}
				break;

			case GTK_SOURCE_SMART_HOME_END_ALWAYS:
				do_cursor_move_home_end (text_view,
				                         &cur,
				                         &iter,
				                         extend_selection,
				                         count);
				return;

			default:
				break;
		}
	}

	GTK_TEXT_VIEW_CLASS (gtk_source_view_parent_class)->move_cursor (text_view,
									 step,
									 count,
									 extend_selection);
}

static void
menu_item_activate_cb (GtkWidget   *menu_item,
		       GtkTextView *text_view)
{
	const gchar *gtksignal;

	gtksignal = g_object_get_data (G_OBJECT (menu_item), "gtk-signal");
	g_signal_emit_by_name (G_OBJECT (text_view), gtksignal);
}

/* This function is taken from gtk+/tests/testtext.c */
static void
gtk_source_view_get_lines (GtkTextView  *text_view,
			   gint          first_y,
			   gint          last_y,
			   GArray       *buffer_coords,
			   GArray       *line_heights,
			   GArray       *numbers,
			   gint         *countp)
{
	GtkTextIter iter;
	gint count;
	gint last_line_num = -1;

	g_array_set_size (buffer_coords, 0);
	g_array_set_size (numbers, 0);
	if (line_heights != NULL)
		g_array_set_size (line_heights, 0);

	/* Get iter at first y */
	gtk_text_view_get_line_at_y (text_view, &iter, first_y, NULL);

	/* For each iter, get its location and add it to the arrays.
	 * Stop when we pass last_y */
	count = 0;

	while (!gtk_text_iter_is_end (&iter))
	{
		gint y, height;

		gtk_text_view_get_line_yrange (text_view, &iter, &y, &height);

		g_array_append_val (buffer_coords, y);
		if (line_heights)
		{
			g_array_append_val (line_heights, height);
		}

		last_line_num = gtk_text_iter_get_line (&iter);
		g_array_append_val (numbers, last_line_num);

		++count;

		if ((y + height) >= last_y)
			break;

		gtk_text_iter_forward_line (&iter);
	}

	if (gtk_text_iter_is_end (&iter))
	{
		gint y, height;
		gint line_num;

		gtk_text_view_get_line_yrange (text_view, &iter, &y, &height);

		line_num = gtk_text_iter_get_line (&iter);

		if (line_num != last_line_num)
		{
			g_array_append_val (buffer_coords, y);
			if (line_heights)
				g_array_append_val (line_heights, height);
			g_array_append_val (numbers, line_num);
			++count;
		}
	}

	*countp = count;
}

static void
gtk_source_view_paint_line_background (GtkTextView    *text_view,
				       cairo_t        *cr,
				       int             y, /* in buffer coordinates */
				       int             height,
				       const GdkRGBA  *color)
{
	GdkRectangle visible_rect;
	GdkRectangle line_rect;
	gint win_y;
	gint margin;
	GtkAdjustment *hadjustment;

	gtk_text_view_get_visible_rect (text_view, &visible_rect);

	gtk_text_view_buffer_to_window_coords (text_view,
					       GTK_TEXT_WINDOW_TEXT,
					       visible_rect.x,
					       y,
					       &line_rect.x,
					       &win_y);

	line_rect.x = 0;
	line_rect.width = visible_rect.width;
	line_rect.y = win_y;
	line_rect.height = height;

	hadjustment = gtk_scrollable_get_hadjustment (GTK_SCROLLABLE (text_view));

	if (hadjustment)
	{
		margin = gtk_text_view_get_left_margin (text_view) -
			 (int) gtk_adjustment_get_value (hadjustment);
	}
	else
	{
		margin = gtk_text_view_get_left_margin (text_view);
	}

	line_rect.x += MAX (0, margin - 1);

	gdk_cairo_set_source_rgba (cr, (GdkRGBA *)color);
	cairo_set_line_width (cr, 1);
	cairo_rectangle (cr, line_rect.x + .5, line_rect.y + .5,
			 line_rect.width - 1, line_rect.height - 1);
	cairo_stroke_preserve (cr);
	cairo_fill (cr);
}

static void
gtk_source_view_paint_marks_background (GtkSourceView *view,
					cairo_t       *cr)
{
	GtkTextView *text_view;
	GdkRectangle clip;
	GArray *numbers;
	GArray *pixels;
	GArray *heights;
	gint y1, y2;
	gint count;
	gint i;

	if (view->priv->source_buffer == NULL ||
	    !gdk_cairo_get_clip_rectangle (cr, &clip))
	{
		return;
	}

	text_view = GTK_TEXT_VIEW (view);

	y1 = clip.y;
	y2 = y1 + clip.height;

	/* get the extents of the line printing */
	gtk_text_view_window_to_buffer_coords (text_view,
	                                       GTK_TEXT_WINDOW_TEXT,
	                                       0,
	                                       y1,
	                                       NULL,
	                                       &y1);

	gtk_text_view_window_to_buffer_coords (text_view,
	                                       GTK_TEXT_WINDOW_TEXT,
	                                       0,
	                                       y2,
	                                       NULL,
	                                       &y2);

	numbers = g_array_new (FALSE, FALSE, sizeof (gint));
	pixels = g_array_new (FALSE, FALSE, sizeof (gint));
	heights = g_array_new (FALSE, FALSE, sizeof (gint));

	/* get the line numbers and y coordinates. */
	gtk_source_view_get_lines (text_view,
	                           y1,
	                           y2,
	                           pixels,
	                           heights,
	                           numbers,
	                           &count);

	if (count == 0)
	{
		gint n = 0;
		gint y;
		gint height;
		GtkTextIter iter;

		gtk_text_buffer_get_start_iter (gtk_text_view_get_buffer (text_view), &iter);
		gtk_text_view_get_line_yrange (text_view, &iter, &y, &height);

		g_array_append_val (pixels, y);
		g_array_append_val (pixels, height);
		g_array_append_val (numbers, n);
		count = 1;
	}

	DEBUG ({
		g_message ("Painting marks background for line numbers %d - %d",
		           g_array_index (numbers, gint, 0),
		           g_array_index (numbers, gint, count - 1));
	});

	for (i = 0; i < count; ++i)
	{
		gint line_to_paint;
		GSList *marks;
		GdkRGBA background;
		int priority;

		line_to_paint = g_array_index (numbers, gint, i);

		marks = gtk_source_buffer_get_source_marks_at_line (view->priv->source_buffer,
		                                                    line_to_paint,
		                                                    NULL);

		priority = -1;

		while (marks != NULL)
		{
			GtkSourceMarkAttributes *attrs;
			gint prio;
			GdkRGBA bg;

			attrs = gtk_source_view_get_mark_attributes (view,
			                                             gtk_source_mark_get_category (marks->data),
			                                             &prio);

			if (attrs == NULL)
			{
				continue;
			}

			if (prio > priority &&
			    gtk_source_mark_attributes_get_background (attrs, &bg))
			{
				priority = prio;
				background = bg;
			}

			marks = g_slist_delete_link (marks, marks);
		}

		if (priority != -1)
		{
			gtk_source_view_paint_line_background (text_view,
			                                       cr,
			                                       g_array_index (pixels, gint, i),
			                                       g_array_index (heights, gint, i),
			                                       &background);
		}
	}

	g_array_free (heights, TRUE);
	g_array_free (pixels, TRUE);
	g_array_free (numbers, TRUE);
}

static void
draw_space_at_iter (cairo_t      *cr,
		    GtkTextView  *view,
		    GtkTextIter  *iter,
		    GdkRectangle  rect)
{
	gint x, y;
	double w;

	gtk_text_view_buffer_to_window_coords (view,
					       GTK_TEXT_WINDOW_TEXT,
					       rect.x,
					       rect.y + rect.height * 2 / 3,
					       &x,
					       &y);

	/* if the space is at a line-wrap position we get 0 width
	 * so we fallback to the height */
	w = rect.width ? rect.width : rect.height;

	cairo_save (cr);
	cairo_move_to (cr, x + w * 0.5, y);
	cairo_arc (cr, x + w * 0.5, y, 0.8, 0, 2 * G_PI);
	cairo_restore (cr);
}

static void
draw_tab_at_iter (cairo_t      *cr,
		  GtkTextView  *view,
		  GtkTextIter  *iter,
		  GdkRectangle  rect)
{
	gint x, y;
	double w, h;

	gtk_text_view_buffer_to_window_coords (view,
					       GTK_TEXT_WINDOW_TEXT,
					       rect.x,
					       rect.y + rect.height * 2 / 3,
					       &x,
					       &y);

	/* if the space is at a line-wrap position we get 0 width
	 * so we fallback to the height */
	w = rect.width ? rect.width : rect.height;
	h = rect.height;

	cairo_save (cr);
	cairo_move_to (cr, x + w * 1 / 8, y);
	cairo_rel_line_to (cr, w * 6 / 8, 0);
	cairo_rel_line_to (cr, -h * 1 / 4, -h * 1 / 4);
	cairo_rel_move_to (cr, +h * 1 / 4, +h * 1 / 4);
	cairo_rel_line_to (cr, -h * 1 / 4, +h * 1 / 4);
	cairo_restore (cr);
}

static void
draw_newline_at_iter (cairo_t      *cr,
		      GtkTextView  *view,
		      GtkTextIter  *iter,
		      GdkRectangle  rect)
{
	gint x, y;
	double w, h;

	gtk_text_view_buffer_to_window_coords (view,
					       GTK_TEXT_WINDOW_TEXT,
					       rect.x,
					       rect.y + rect.height * 1 / 3,
					       &x,
					       &y);

	/* width for new line is 0, we use 2 * h */
	w = 2 * rect.height;
	h = rect.height;

	cairo_save (cr);
	if (gtk_widget_get_default_direction () == GTK_TEXT_DIR_LTR)
	{
		cairo_move_to (cr, x + w * 7 / 8, y);
		cairo_rel_line_to (cr, 0, h * 1 / 3);
		cairo_rel_line_to (cr, -w * 6 / 8, 0);
		cairo_rel_line_to (cr, +h * 1 / 4, -h * 1 / 4);
		cairo_rel_move_to (cr, -h * 1 / 4, +h * 1 / 4);
		cairo_rel_line_to (cr, +h * 1 / 4, +h * 1 / 4);
	}
	else
	{
		cairo_move_to (cr, x + w * 1 / 8, y);
		cairo_rel_line_to (cr, 0, h * 1 / 3);
		cairo_rel_line_to (cr, w * 6 / 8, 0);
		cairo_rel_line_to (cr, -h * 1 / 4, -h * 1 / 4);
		cairo_rel_move_to (cr, +h * 1 / 4, +h * 1 / 4);
		cairo_rel_line_to (cr, -h * 1 / 4, -h * 1 / 4);
	}

	cairo_restore (cr);
}

static void
draw_nbsp_at_iter (cairo_t      *cr,
		   GtkTextView  *view,
		   GtkTextIter  *iter,
		   GdkRectangle  rect,
		   gboolean      narrowed)
{
	gint x, y;
	gdouble w, h;

	gtk_text_view_buffer_to_window_coords (view,
	                                       GTK_TEXT_WINDOW_TEXT,
	                                       rect.x,
	                                       rect.y + rect.height / 2,
	                                       &x,
	                                       &y);

	/* if the space is at a line-wrap position we get 0 width
	 * so we fallback to the height */
	w = rect.width ? rect.width : rect.height;
	h = rect.height;

	cairo_save (cr);
	cairo_move_to (cr, x + w * 1 / 6, y);
	cairo_rel_line_to (cr, w * 4 / 6, 0);
	cairo_rel_line_to (cr, -w * 2 / 6, +h * 1 / 4);
	cairo_rel_line_to (cr, -w * 2 / 6, -h * 1 / 4);

	if (narrowed)
	{
		cairo_fill (cr);
	}
	else
	{
		cairo_stroke (cr);
	}

	cairo_restore (cr);
}

static void
draw_spaces_at_iter (cairo_t       *cr,
		     GtkSourceView *view,
		     GtkTextIter   *iter,
		     GdkRectangle   rect)
{
	gunichar c;

	c = gtk_text_iter_get_char (iter);

	if (view->priv->draw_spaces & GTK_SOURCE_DRAW_SPACES_TAB &&
	    c == '\t')
	{
		draw_tab_at_iter (cr, GTK_TEXT_VIEW (view), iter, rect);
	}
	else if (view->priv->draw_spaces & GTK_SOURCE_DRAW_SPACES_NBSP &&
		 g_unichar_break_type (c) == G_UNICODE_BREAK_NON_BREAKING_GLUE)
	{
		/* We also need to check if we want to draw a narrowed space */
		draw_nbsp_at_iter (cr, GTK_TEXT_VIEW (view), iter, rect,
		                   c == 0x202F);
	}
	else if (view->priv->draw_spaces & GTK_SOURCE_DRAW_SPACES_SPACE &&
	         g_unichar_type (c) == G_UNICODE_SPACE_SEPARATOR)
	{
		draw_space_at_iter (cr, GTK_TEXT_VIEW (view), iter, rect);
	}
	else if (view->priv->draw_spaces & GTK_SOURCE_DRAW_SPACES_NEWLINE &&
		 gtk_text_iter_ends_line (iter) && !gtk_text_iter_is_end (iter))
	{
		draw_newline_at_iter (cr, GTK_TEXT_VIEW (view), iter, rect);
	}
}

static void
get_leading_trailing (GtkTextIter *iter,
                      GtkTextIter *leading,
                      GtkTextIter *trailing)
{
	GtkTextIter start;

	/* Find end of leading */
	start = *iter;
	gtk_text_iter_set_line_offset (&start, 0);

	while (TRUE)
	{
		gunichar ch = gtk_text_iter_get_char (&start);

		if (!g_unichar_isspace (ch) ||
		    gtk_text_iter_ends_line (&start) ||
		    !gtk_text_iter_forward_char (&start))
		{
			*leading = start;
			break;
		}
	}

	/* Find start of trailing */
	start = *iter;
	gtk_text_iter_forward_to_line_end (&start);

	while (TRUE)
	{
		gunichar ch = gtk_text_iter_get_char (&start);

		/* NOTE: ch can be 0 when iter is at the end
		   of the buffer */
		if (!(g_unichar_isspace (ch) || ch == 0) ||
		     gtk_text_iter_starts_line (&start) ||
		    !gtk_text_iter_backward_char (&start))
		{
			*trailing = start;
			break;
		}
	}
}

static gboolean
check_location (GtkSourceView *view,
                GtkTextIter   *iter,
                GtkTextIter   *leading,
                GtkTextIter   *trailing)
{
	gint flags = 0;
	gint location = view->priv->draw_spaces & (GTK_SOURCE_DRAW_SPACES_LEADING |
	                                           GTK_SOURCE_DRAW_SPACES_TEXT |
	                                           GTK_SOURCE_DRAW_SPACES_TRAILING);

	/* Draw all by default */
	if (!location)
	{
		return TRUE;
	}

	if (gtk_text_iter_compare (iter, trailing) >= 0)
	{
		flags |= GTK_SOURCE_DRAW_SPACES_TRAILING;
	}

	if (gtk_text_iter_compare (iter, leading) < 0)
	{
		flags |= GTK_SOURCE_DRAW_SPACES_LEADING;
	}

	if (flags == 0)
	{
		/* Neither leading nor trailing, must be in text */
		return location & GTK_SOURCE_DRAW_SPACES_TEXT;
	}
	else
	{
		return location & flags;
	}
}

static void
draw_tabs_and_spaces (GtkSourceView *view,
		      cairo_t       *cr)
{
	GtkTextView *text_view;
	GdkRectangle clip;
	gint x1, y1, x2, y2;
	GtkTextIter s, e;
	GtkTextIter leading, trailing;

	if (!gdk_cairo_get_clip_rectangle (cr, &clip))
		return;

	text_view = GTK_TEXT_VIEW (view);

	x1 = clip.x;
	y1 = clip.y;
	x2 = x1 + clip.width;
	y2 = y1 + clip.height;

	gtk_text_view_window_to_buffer_coords (text_view,
					       GTK_TEXT_WINDOW_TEXT,
					       x1,
					       y1,
					       &x1,
					       &y1);

	gtk_text_view_window_to_buffer_coords (text_view,
					       GTK_TEXT_WINDOW_TEXT,
					       x2,
					       y2,
					       &x2,
					       &y2);

	gtk_text_view_get_iter_at_location  (text_view,
	                                     &s,
	                                     x1, y1);
	gtk_text_view_get_iter_at_location  (text_view,
	                                     &e,
	                                     x2, y2);

	gdk_cairo_set_source_rgba (cr, view->priv->spaces_color);

	cairo_set_line_width (cr, 0.8);
	cairo_translate (cr, -0.5, -0.5);

	get_leading_trailing (&s, &leading, &trailing);

	do
	{
		GdkRectangle rect;
		gint ly;

		gtk_text_view_get_iter_location (text_view, &s, &rect);

		/* just iterate on the text that is in the exposed area */
		if (rect.x > x2)
		{
			if (!gtk_text_iter_forward_line	(&s))
			{
				break;
			}

			/* move to the first iter in the exposed area of
			 * the next line */
			gtk_text_view_get_line_yrange (text_view, &s, &ly, NULL);

			gtk_text_view_get_iter_at_location  (text_view,
							     &s,
							     x1, ly);

			/* move back one char otherwise tabs may not
			 * be redrawn */
			if (!gtk_text_iter_starts_line (&s))
			{
				gtk_text_iter_backward_char (&s);
			}

			get_leading_trailing (&s, &leading, &trailing);
			continue;
		}

		if (check_location (view, &s, &leading, &trailing))
		{
			draw_spaces_at_iter (cr, view, &s, rect);
		}

		if (!gtk_text_iter_forward_char (&s))
		{
			break;
		}

		if (gtk_text_iter_starts_line (&s))
		{
			get_leading_trailing (&s, &leading, &trailing);
		}

	} while (gtk_text_iter_compare (&s, &e) <= 0);

	cairo_stroke (cr);
}

static void
gtk_source_view_paint_right_margin (GtkSourceView *view,
                                    cairo_t       *cr)
{
	GdkRectangle visible_rect;
	GdkRectangle redraw_rect;
	double x;

	GtkTextView *text_view = GTK_TEXT_VIEW (view);

#ifdef ENABLE_PROFILE
	static GTimer *timer = NULL;
#endif

	g_return_if_fail (view->priv->right_margin_line_color != NULL);

	if (view->priv->cached_right_margin_pos < 0)
	{
		view->priv->cached_right_margin_pos =
			calculate_real_tab_width (view,
						  view->priv->right_margin_pos,
						  '_');
	}

#ifdef ENABLE_PROFILE
	if (timer == NULL)
		timer = g_timer_new ();

	g_timer_start (timer);
#endif

	gtk_text_view_get_visible_rect (text_view, &visible_rect);

	gtk_text_view_buffer_to_window_coords (text_view,
				       GTK_TEXT_WINDOW_TEXT,
				       visible_rect.x,
				       visible_rect.y,
				       &redraw_rect.x,
				       &redraw_rect.y);

	redraw_rect.width = visible_rect.width;
	redraw_rect.height = visible_rect.height;

	/* Offset with 0.5 is needed for a sharp line. */
	x = view->priv->cached_right_margin_pos -
		visible_rect.x + redraw_rect.x + 0.5 +
		gtk_text_view_get_left_margin (text_view);

	/* Default line width is 2.0 which is too wide. */
	cairo_set_line_width (cr, 1.0);

	cairo_move_to (cr, x, redraw_rect.y);
	cairo_line_to (cr, x, redraw_rect.y + redraw_rect.height);

	gdk_cairo_set_source_rgba (cr, view->priv->right_margin_line_color);

	cairo_stroke (cr);

	/* Only draw the overlay when the style scheme explicitly sets it. */
	if (view->priv->right_margin_overlay_color != NULL)
	{
		/* Draw the rectangle next to the line (x+.5). */
		cairo_rectangle (cr,
				 x + .5,
				 redraw_rect.y,
				 redraw_rect.width - x - .5,
				 redraw_rect.y + redraw_rect.height);

		gdk_cairo_set_source_rgba (cr, view->priv->right_margin_overlay_color);

		cairo_fill (cr);
	}

	PROFILE ({
		g_timer_stop (timer);
		g_message ("Time to draw the margin: %g (sec * 1000)",
		           g_timer_elapsed (timer, NULL) * 1000);
	});
}

static gboolean
gtk_source_view_draw (GtkWidget *widget,
	              cairo_t   *cr)
{
	GtkSourceView *view;
	GtkTextView *text_view;
	GdkWindow *window;
	gboolean event_handled;

	DEBUG ({
		g_print ("> gtk_source_view_draw start\n");
	});

	view = GTK_SOURCE_VIEW (widget);
	text_view = GTK_TEXT_VIEW (widget);

	window = gtk_text_view_get_window (text_view, GTK_TEXT_WINDOW_TEXT);

	cairo_save (cr);
	gtk_cairo_transform_to_window (cr, widget, window);

	event_handled = FALSE;

	/* check if the draw is for the text window first, and
	 * make sure the visible region is highlighted */
	if (gtk_cairo_should_draw_window (cr, window) &&
	    view->priv->source_buffer != NULL)
	{
		GdkRectangle visible_rect;
		GtkTextIter iter1, iter2;

		gtk_text_view_get_visible_rect (text_view, &visible_rect);
		gtk_text_view_get_line_at_y (text_view, &iter1,
					     visible_rect.y, NULL);
		gtk_text_iter_backward_line (&iter1);
		gtk_text_view_get_line_at_y (text_view, &iter2,
					     visible_rect.y
					     + visible_rect.height, NULL);
		gtk_text_iter_forward_line (&iter2);

		DEBUG ({
			g_print ("    draw area: %d - %d\n", visible_rect.y,
				 visible_rect.y + visible_rect.height);
			g_print ("    lines to update: %d - %d\n",
				 gtk_text_iter_get_line (&iter1),
				 gtk_text_iter_get_line (&iter2));
		});

		_gtk_source_buffer_update_highlight (view->priv->source_buffer,
						     &iter1, &iter2, FALSE);
	}

	if (gtk_widget_is_sensitive (widget) && view->priv->highlight_current_line &&
	    gtk_cairo_should_draw_window (cr, window))
	{
		GtkTextIter cur;
		gint y, height;
		GtkTextBuffer *buffer = gtk_text_view_get_buffer (text_view);

		gtk_text_buffer_get_iter_at_mark (buffer,
						  &cur,
						  gtk_text_buffer_get_insert (buffer));
		gtk_text_view_get_line_yrange (text_view, &cur, &y, &height);

		if (view->priv->current_line_color_set)
		{
			gtk_source_view_paint_line_background (text_view,
							       cr,
							       y, height,
							       &view->priv->current_line_color);
		}
		else
		{
			GtkStyleContext *context;
			GtkStateFlags state;
			GdkRGBA color;

			context = gtk_widget_get_style_context (widget);
			state = gtk_widget_get_state_flags (widget);
			gtk_style_context_get_background_color (context, state, &color);

			gtk_source_view_paint_line_background (text_view,
							       cr,
							       y, height,
							       &color);
		}
	}

	if (gtk_cairo_should_draw_window (cr, window))
	{
		gtk_source_view_paint_marks_background (view, cr);
	}

	/* Have GtkTextView draw the text first. */
	if (GTK_WIDGET_CLASS (gtk_source_view_parent_class)->draw)
	{
		/* Need to restore to original state here so the parent draw func
		* gets the correct context. */
		cairo_restore (cr);

		cairo_save (cr);

		event_handled =
			GTK_WIDGET_CLASS (gtk_source_view_parent_class)->draw (widget, cr);

		cairo_restore (cr);

		cairo_save (cr);
		gtk_cairo_transform_to_window (cr, widget, window);
	}

	/* Draw the right margin vertical line + overlay. */
	if (view->priv->show_right_margin &&
	    gtk_cairo_should_draw_window (cr, window))
	{
		gtk_source_view_paint_right_margin (view, cr);
	}

	if (view->priv->draw_spaces != 0 &&
	    gtk_cairo_should_draw_window (cr, window))
	{
		draw_tabs_and_spaces (view, cr);
	}

	DEBUG ({
		g_print ("> gtk_source_view_draw end\n");
	});

	cairo_restore (cr);

	return event_handled;
}

/*
 *This is a pretty important function...we call it when the tab_stop is changed,
 *And when the font is changed.
 *NOTE: You must change this with the default font for now...
 *It may be a good idea to set the tab_width for each GtkTextTag as well
 *based on the font that we set at creation time
 *something like style_cache_set_tabs_from_font or the like.
 *Now, this *may* not be necessary because most tabs wont be inside of another highlight,
 *except for things like multi-line comments (which will usually have an italic font which
 *may or may not be a different size than the standard one), or if some random language
 *definition decides that it would be spiffy to have a bg color for "start of line" whitespace
 *"^\(\t\| \)+" would probably do the trick for that.
 */
static gint
calculate_real_tab_width (GtkSourceView *view, guint tab_size, gchar c)
{
	PangoLayout *layout;
	gchar *tab_string;
	gint tab_width = 0;

	if (tab_size == 0)
		return -1;

	tab_string = g_strnfill (tab_size, c);
	layout = gtk_widget_create_pango_layout (GTK_WIDGET (view), tab_string);
	g_free (tab_string);

	if (layout != NULL) {
		pango_layout_get_pixel_size (layout, &tab_width, NULL);
		g_object_unref (G_OBJECT (layout));
	} else
		tab_width = -1;

	return tab_width;
}


/* ----------------------------------------------------------------------
 * Public interface
 * ---------------------------------------------------------------------- */

/**
 * gtk_source_view_new:
 *
 * Creates a new #GtkSourceView. An empty default buffer will be
 * created for you. If you want to specify your own buffer, consider
 * gtk_source_view_new_with_buffer().
 *
 * Return value: a new #GtkSourceView.
 **/
GtkWidget *
gtk_source_view_new (void)
{
	GtkWidget *widget;
	GtkSourceBuffer *buffer;

	buffer = gtk_source_buffer_new (NULL);
	widget = gtk_source_view_new_with_buffer (buffer);
	g_object_unref (buffer);
	return widget;
}

/**
 * gtk_source_view_new_with_buffer:
 * @buffer: a #GtkSourceBuffer.
 *
 * Creates a new #GtkSourceView widget displaying the buffer
 * @buffer. One buffer can be shared among many widgets.
 *
 * Return value: a new #GtkSourceView.
 **/
GtkWidget *
gtk_source_view_new_with_buffer (GtkSourceBuffer *buffer)
{
	GtkWidget *view;

	g_return_val_if_fail (buffer != NULL && GTK_SOURCE_IS_BUFFER (buffer), NULL);

	view = g_object_new (GTK_SOURCE_TYPE_VIEW, NULL);
	gtk_text_view_set_buffer (GTK_TEXT_VIEW (view), GTK_TEXT_BUFFER (buffer));

	return view;
}

/**
 * gtk_source_view_get_show_line_numbers:
 * @view: a #GtkSourceView.
 *
 * Returns whether line numbers are displayed beside the text.
 *
 * Return value: %TRUE if the line numbers are displayed.
 **/
gboolean
gtk_source_view_get_show_line_numbers (GtkSourceView *view)
{
	g_return_val_if_fail (view != NULL, FALSE);
	g_return_val_if_fail (GTK_SOURCE_IS_VIEW (view), FALSE);

	return (view->priv->show_line_numbers != FALSE);
}

/**
 * gtk_source_view_set_show_line_numbers:
 * @view: a #GtkSourceView.
 * @show: whether line numbers should be displayed.
 *
 * If %TRUE line numbers will be displayed beside the text.
 **/
void
gtk_source_view_set_show_line_numbers (GtkSourceView *view,
                                       gboolean       show)
{
	g_return_if_fail (view != NULL);
	g_return_if_fail (GTK_SOURCE_IS_VIEW (view));

	show = (show != FALSE);

	if (show == view->priv->show_line_numbers)
	{
		return;
	}

	gtk_source_gutter_renderer_set_visible (view->priv->line_renderer,
	                                        show);

	view->priv->show_line_numbers = show;

	g_object_notify (G_OBJECT (view), "show_line_numbers");
}

/**
 * gtk_source_view_get_show_line_marks:
 * @view: a #GtkSourceView.
 *
 * Returns whether line marks are displayed beside the text.
 *
 * Return value: %TRUE if the line marks are displayed.
 *
 * Since: 2.2
 **/
gboolean
gtk_source_view_get_show_line_marks (GtkSourceView *view)
{
	g_return_val_if_fail (GTK_SOURCE_IS_VIEW (view), FALSE);

	return (view->priv->show_line_marks != FALSE);
}

/**
 * gtk_source_view_set_show_line_marks:
 * @view: a #GtkSourceView.
 * @show: whether line marks should be displayed.
 *
 * If %TRUE line marks will be displayed beside the text.
 *
 * Since: 2.2
 **/
void
gtk_source_view_set_show_line_marks (GtkSourceView *view,
				     gboolean       show)
{
	g_return_if_fail (GTK_SOURCE_IS_VIEW (view));

	show = (show != FALSE);

	if (show == view->priv->show_line_marks)
	{
		return;
	}

	gtk_source_gutter_renderer_set_visible (view->priv->marks_renderer,
	                                        show);

	view->priv->show_line_marks = show;

	g_object_notify (G_OBJECT (view), "show_line_marks");
}

static gboolean
set_tab_stops_internal (GtkSourceView *view)
{
	PangoTabArray *tab_array;
	gint real_tab_width;

	real_tab_width = calculate_real_tab_width (view, view->priv->tab_width, ' ');

	if (real_tab_width < 0)
		return FALSE;

	tab_array = pango_tab_array_new (1, TRUE);
	pango_tab_array_set_tab (tab_array, 0, PANGO_TAB_LEFT, real_tab_width);

	gtk_text_view_set_tabs (GTK_TEXT_VIEW (view),
				tab_array);
	view->priv->tabs_set = TRUE;

	pango_tab_array_free (tab_array);

	return TRUE;
}

/**
 * gtk_source_view_set_tab_width:
 * @view: a #GtkSourceView.
 * @width: width of tab in characters.
 *
 * Sets the width of tabulation in characters.
 */
void
gtk_source_view_set_tab_width (GtkSourceView *view,
			       guint          width)
{
	guint save_width;

	g_return_if_fail (GTK_SOURCE_VIEW (view));
	g_return_if_fail (width > 0 && width <= MAX_TAB_WIDTH);

	if (view->priv->tab_width == width)
		return;

	save_width = view->priv->tab_width;
	view->priv->tab_width = width;
	if (set_tab_stops_internal (view))
	{
		g_object_notify (G_OBJECT (view), "tab-width");
	}
	else
	{
		g_warning ("Impossible to set tab width.");
		view->priv->tab_width = save_width;
	}
}

/**
 * gtk_source_view_get_tab_width:
 * @view: a #GtkSourceView.
 *
 * Returns the width of tabulation in characters.
 *
 * Return value: width of tab.
 */
guint
gtk_source_view_get_tab_width (GtkSourceView *view)
{
	g_return_val_if_fail (GTK_SOURCE_IS_VIEW (view), DEFAULT_TAB_WIDTH);

	return view->priv->tab_width;
}

/**
 * gtk_source_view_set_indent_width:
 * @view: a #GtkSourceView.
 * @width: indent width in characters.
 *
 * Sets the number of spaces to use for each step of indent.
 * If @width is -1, the value of the GtkSourceView::tab-width property
 * will be used.
 */
void
gtk_source_view_set_indent_width (GtkSourceView *view,
				  gint          width)
{
	g_return_if_fail (GTK_SOURCE_VIEW (view));
	g_return_if_fail ((width == -1) || (width > 0 && width <= MAX_INDENT_WIDTH));

	if (view->priv->indent_width != width)
	{
		view->priv->indent_width = width;
		g_object_notify (G_OBJECT (view), "indent-width");
	}
}

/**
 * gtk_source_view_get_indent_width:
 * @view: a #GtkSourceView.
 *
 * Returns the number of spaces to use for each step of indent.
 * See gtk_source_view_set_indent_width() for details.
 *
 * Return value: indent width.
 */
gint
gtk_source_view_get_indent_width (GtkSourceView *view)
{
	g_return_val_if_fail (view != NULL && GTK_SOURCE_IS_VIEW (view), 0);

	return view->priv->indent_width;
}

static gchar *
compute_indentation (GtkSourceView *view,
		     GtkTextIter   *cur)
{
	GtkTextIter start;
	GtkTextIter end;

	gunichar ch;
	gint line;

	line = gtk_text_iter_get_line (cur);

	gtk_text_buffer_get_iter_at_line (gtk_text_view_get_buffer (GTK_TEXT_VIEW (view)),
					  &start,
					  line);

	end = start;

	ch = gtk_text_iter_get_char (&end);

	while (g_unichar_isspace (ch) &&
	       (ch != '\n') &&
	       (ch != '\r') &&
	       (gtk_text_iter_compare (&end, cur) < 0))
	{
		if (!gtk_text_iter_forward_char (&end))
			break;

		ch = gtk_text_iter_get_char (&end);
	}

	if (gtk_text_iter_equal (&start, &end))
		return NULL;

	return gtk_text_iter_get_slice (&start, &end);
}

static guint
get_real_indent_width (GtkSourceView *view)
{
	return view->priv->indent_width < 0 ?
	       view->priv->tab_width :
	       (guint) view->priv->indent_width;
}

static gchar *
get_indent_string (guint tabs, guint spaces)
{
	gchar *str;

	str = g_malloc (tabs + spaces + 1);
	if (tabs > 0)
		memset (str, '\t', tabs);
	if (spaces > 0)
		memset (str + tabs, ' ', spaces);
	str[tabs + spaces] = '\0';

	return str;
}

static void
indent_lines (GtkSourceView *view, GtkTextIter *start, GtkTextIter *end)
{
	GtkTextBuffer *buf;
	gint start_line, end_line;
	gchar *tab_buffer = NULL;
	guint tabs = 0;
	guint spaces = 0;
	gint i;

	buf = gtk_text_view_get_buffer (GTK_TEXT_VIEW (view));

	start_line = gtk_text_iter_get_line (start);
	end_line = gtk_text_iter_get_line (end);

	if ((gtk_text_iter_get_visible_line_offset (end) == 0) &&
	    (end_line > start_line))
	{
		end_line--;
	}

	if (view->priv->insert_spaces)
	{
		spaces = get_real_indent_width (view);

		tab_buffer = g_strnfill (spaces, ' ');
	}
	else if (view->priv->indent_width > 0)
	{
		guint indent_width;

		indent_width = get_real_indent_width (view);
		spaces = indent_width % view->priv->tab_width;
		tabs = indent_width / view->priv->tab_width;

		tab_buffer = get_indent_string (tabs, spaces);
	}
	else
	{
		tabs = 1;
		tab_buffer = g_strdup ("\t");
	}

	gtk_text_buffer_begin_user_action (buf);

	for (i = start_line; i <= end_line; i++)
	{
		GtkTextIter iter;
		GtkTextIter iter2;
		guint replaced_spaces = 0;

		gtk_text_buffer_get_iter_at_line (buf, &iter, i);

		/* add spaces always after tabs, to avoid the case
		 * where "\t" becomes "  \t" with no visual difference */
		while (gtk_text_iter_get_char (&iter) == '\t')
		{
			gtk_text_iter_forward_char (&iter);
		}

		/* don't add indentation on empty lines */
		if (gtk_text_iter_ends_line (&iter))
			continue;

		/* if tabs are allowed try to merge the spaces
		 * with the tab we are going to insert (if any) */
		iter2 = iter;
		while (!view->priv->insert_spaces &&
		       (gtk_text_iter_get_char (&iter2) == ' ') &&
			replaced_spaces < view->priv->tab_width)
		{
			++replaced_spaces;

			gtk_text_iter_forward_char (&iter2);
		}

		if (replaced_spaces > 0)
		{
			gchar *indent_buf;
			guint t, s;

			t = tabs + (spaces + replaced_spaces) / view->priv->tab_width;
			s = (spaces + replaced_spaces) % view->priv->tab_width;
			indent_buf = get_indent_string (t, s);

			gtk_text_buffer_delete (buf, &iter, &iter2);
			gtk_text_buffer_insert (buf, &iter, indent_buf, -1);

			g_free (indent_buf);
		}
		else
		{
			gtk_text_buffer_insert (buf, &iter, tab_buffer, -1);
		}
	}

	gtk_text_buffer_end_user_action (buf);

	g_free (tab_buffer);

	gtk_text_view_scroll_mark_onscreen (GTK_TEXT_VIEW (view),
					    gtk_text_buffer_get_insert (buf));
}

static void
unindent_lines (GtkSourceView *view, GtkTextIter *start, GtkTextIter *end)
{
	GtkTextBuffer *buf;
	gint start_line, end_line;
	gint tab_width;
	gint indent_width;
	gint i;

	buf = gtk_text_view_get_buffer (GTK_TEXT_VIEW (view));

	start_line = gtk_text_iter_get_line (start);
	end_line = gtk_text_iter_get_line (end);

	if ((gtk_text_iter_get_visible_line_offset (end) == 0) &&
	    (end_line > start_line))
	{
		end_line--;
	}

	tab_width = view->priv->tab_width;
	indent_width = get_real_indent_width (view);

	gtk_text_buffer_begin_user_action (buf);

	for (i = start_line; i <= end_line; i++)
	{
		GtkTextIter iter, iter2;
		gint to_delete = 0;
		gint to_delete_equiv = 0;

		gtk_text_buffer_get_iter_at_line (buf, &iter, i);

		iter2 = iter;

		while (!gtk_text_iter_ends_line (&iter2) &&
		       to_delete_equiv < indent_width)
		{
			gunichar c;

			c = gtk_text_iter_get_char (&iter2);
			if (c == '\t')
			{
				to_delete_equiv += tab_width - to_delete_equiv % tab_width;
				++to_delete;
			}
			else if (c == ' ')
			{
				++to_delete_equiv;
				++to_delete;
			}
			else
			{
				break;
			}

			gtk_text_iter_forward_char (&iter2);
		}

		if (to_delete > 0)
		{
			gtk_text_iter_set_line_offset (&iter2, to_delete);
			gtk_text_buffer_delete (buf, &iter, &iter2);
		}
	}

	gtk_text_buffer_end_user_action (buf);

	gtk_text_view_scroll_mark_onscreen (GTK_TEXT_VIEW (view),
					    gtk_text_buffer_get_insert (buf));
}

static gint
get_line_offset_in_equivalent_spaces (GtkSourceView *view,
				      GtkTextIter *iter)
{
	GtkTextIter i;
	gint tab_width;
	gint n = 0;

	tab_width = view->priv->tab_width;

	i = *iter;
	gtk_text_iter_set_line_offset (&i, 0);

	while (!gtk_text_iter_equal (&i, iter))
	{
		gunichar c;

		c = gtk_text_iter_get_char (&i);
		if (c == '\t')
			n += tab_width - n % tab_width;
		else
			++n;

		gtk_text_iter_forward_char (&i);
	}

	return n;
}

static void
insert_tab_or_spaces (GtkSourceView *view,
		      GtkTextIter   *start,
		      GtkTextIter   *end)
{
	GtkTextBuffer *buf;
	gchar *tab_buf;
	gint cursor_offset = 0;

	if (view->priv->insert_spaces)
	{
		gint indent_width;
		gint pos;
		gint spaces;

		indent_width = get_real_indent_width (view);

		/* CHECK: is this a performance problem? */
		pos = get_line_offset_in_equivalent_spaces (view, start);
		spaces = indent_width - pos % indent_width;

		tab_buf = g_strnfill (spaces, ' ');
	}
	else if (view->priv->indent_width > 0)
	{
		GtkTextIter iter;
		gint i;
		gint tab_width;
		gint indent_width;
		gint from;
		gint to;
		gint preceding_spaces = 0;
		gint following_tabs = 0;
		gint equiv_spaces;
		gint tabs;
		gint spaces;

		tab_width = view->priv->tab_width;
		indent_width = get_real_indent_width (view);

		/* CHECK: is this a performance problem? */
		from = get_line_offset_in_equivalent_spaces (view, start);
		to = indent_width * (1 + from / indent_width);
		equiv_spaces = to - from;

		/* extend the selection to include
		 * preceding spaces so that if indentation
		 * width < tab width, two conseutive indentation
		 * width units get compressed into a tab */
		iter = *start;
		for (i = 0; i < tab_width; ++i)
		{
			gtk_text_iter_backward_char (&iter);

			if (gtk_text_iter_get_char (&iter) == ' ')
				++preceding_spaces;
			else
				break;
		}

		gtk_text_iter_backward_chars (start, preceding_spaces);

		/* now also extend the selection to the following tabs
		 * since we do not want to insert spaces before a tab
		 * since it may have no visual effect */
		while (gtk_text_iter_get_char (end) == '\t')
		{
			++following_tabs;
			gtk_text_iter_forward_char (end);
		}

		tabs = (preceding_spaces + equiv_spaces) / tab_width;
		spaces = (preceding_spaces + equiv_spaces) % tab_width;

		tab_buf = get_indent_string (tabs + following_tabs, spaces);

		cursor_offset = gtk_text_iter_get_offset (start) +
			        tabs +
		                (following_tabs > 0 ? 1 : spaces);
	}
	else
	{
		tab_buf = g_strdup ("\t");
	}

	buf = gtk_text_view_get_buffer (GTK_TEXT_VIEW (view));

	gtk_text_buffer_begin_user_action (buf);

	gtk_text_buffer_delete (buf, start, end);
	gtk_text_buffer_insert (buf, start, tab_buf, -1);

	/* adjust cursor position if needed */
	if (cursor_offset > 0)
	{
		GtkTextIter iter;

		gtk_text_buffer_get_iter_at_offset (buf, &iter, cursor_offset);
		gtk_text_buffer_place_cursor (buf, &iter);
	}

	gtk_text_buffer_end_user_action (buf);

	g_free (tab_buf);
}

static void
gtk_source_view_move_words (GtkSourceView *view, gint step)
{
	GtkTextBuffer *buf;
	GtkTextIter s, e, ns, ne;
	GtkTextMark *nsmark, *nemark;
	gchar *old_text, *new_text;

	buf = gtk_text_view_get_buffer (GTK_TEXT_VIEW (view));

	if (step == 0 || gtk_text_view_get_editable (GTK_TEXT_VIEW (view)) == FALSE)
	{
		return;
	}

	gtk_text_buffer_get_selection_bounds (buf, &s, &e);

	if (gtk_text_iter_compare (&s, &e) == 0)
	{
		if (!gtk_text_iter_starts_word (&s))
		{
			if (!gtk_text_iter_inside_word (&s) && !gtk_text_iter_ends_word (&s))
			{
				return;
			}
			else
			{
				gtk_text_iter_backward_word_start (&s);
			}
		}

		if (!gtk_text_iter_starts_word (&s))
		{
			return;
		}

		e = s;

		if (!gtk_text_iter_ends_word (&e))
		{
			if (!gtk_text_iter_forward_word_end (&e))
			{
				gtk_text_iter_forward_to_end (&e);
			}

			if (!gtk_text_iter_ends_word (&e))
			{
				return;
			}
		}
	}

	/* Swap the selection with the next or previous word, based on step */
	if (step > 0)
	{
		ne = e;

		if (!gtk_text_iter_forward_word_ends (&ne, step))
		{
			gtk_text_iter_forward_to_end (&ne);
		}

		if (!gtk_text_iter_ends_word (&ne) || gtk_text_iter_equal (&ne, &e))
		{
			return;
		}

		ns = ne;

		if (!gtk_text_iter_backward_word_start (&ns))
		{
			return;
		}
	}
	else
	{
		ns = s;

		if (!gtk_text_iter_backward_word_starts (&ns, -step))
		{
			return;
		}

		ne = ns;

		if (!gtk_text_iter_forward_word_end (&ne))
		{
			return;
		}
	}

	if (gtk_text_iter_in_range (&ns, &s, &e) ||
	    gtk_text_iter_in_range (&ne, &s, &e))
	{
		return;
	}

	old_text = gtk_text_buffer_get_text (buf, &s, &e, TRUE);
	new_text = gtk_text_buffer_get_text (buf, &ns, &ne, TRUE);

	gtk_text_buffer_begin_user_action (buf);

	nsmark = gtk_text_buffer_create_mark (buf, NULL, &ns, TRUE);
	nemark = gtk_text_buffer_create_mark (buf, NULL, &ne, FALSE);

	gtk_text_buffer_delete (buf, &s, &e);
	gtk_text_buffer_insert (buf, &s, new_text, -1);

	gtk_text_buffer_get_iter_at_mark (buf, &ns, nsmark);
	gtk_text_buffer_get_iter_at_mark (buf, &ne, nemark);

	gtk_text_buffer_delete (buf, &ns, &ne);
	gtk_text_buffer_insert (buf, &ns, old_text, -1);

	ne = ns;
	gtk_text_buffer_get_iter_at_mark (buf, &ns, nsmark);

	gtk_text_buffer_select_range (buf, &ns, &ne);

	gtk_text_buffer_delete_mark (buf, nsmark);
	gtk_text_buffer_delete_mark (buf, nemark);

	gtk_text_buffer_end_user_action (buf);

	gtk_text_view_scroll_mark_onscreen (GTK_TEXT_VIEW (view),
	                                    gtk_text_buffer_get_insert (buf));

	g_free (old_text);
	g_free (new_text);
}

static void
gtk_source_view_move_lines (GtkSourceView *view, gboolean copy, gint step)
{
	GtkTextBuffer *buf;
	GtkTextIter s, e;
	GtkTextMark *mark;
	gboolean down;
	gchar *text;

	buf = gtk_text_view_get_buffer (GTK_TEXT_VIEW (view));

	if (step == 0 || gtk_text_view_get_editable (GTK_TEXT_VIEW (view)) == FALSE)
		return;

	/* FIXME: for now we just handle a step of one line */

	down = step > 0;

	gtk_text_buffer_get_selection_bounds (buf, &s, &e);

	/* get the entire lines, including the paragraph terminator */
	gtk_text_iter_set_line_offset (&s, 0);
	if (!gtk_text_iter_starts_line (&e) ||
	     gtk_text_iter_get_line (&s) == gtk_text_iter_get_line (&e))
	{
		gtk_text_iter_forward_line (&e);
	}

	if ((!down && (0 == gtk_text_iter_get_line (&s))) ||
	    (down && (gtk_text_iter_is_end (&e))) ||
	    (down && (gtk_text_buffer_get_line_count (buf) == gtk_text_iter_get_line (&e))))
	{
		return;
	}

	text = gtk_text_buffer_get_slice (buf, &s, &e, TRUE);

	/* First special case) We are moving up the last line
	 * of the buffer, check if buffer ends with a paragraph
	 * delimiter otherwise append a \n ourselves */
	if (gtk_text_iter_is_end (&e))
	{
		GtkTextIter iter;
		iter = e;

		gtk_text_iter_set_line_offset (&iter, 0);
		if (!gtk_text_iter_ends_line (&iter) &&
		    !gtk_text_iter_forward_to_line_end (&iter))
		{
			gchar *tmp;

			tmp = g_strdup_printf ("%s\n", text);

			g_free (text);
			text = tmp;
		}
	}

	gtk_text_buffer_begin_user_action (buf);

	if (!copy)
		gtk_text_buffer_delete (buf, &s, &e);

	if (down)
	{
		gtk_text_iter_forward_line (&e);

		/* Second special case) We are moving down the last-but-one line
		 * of the buffer, check if buffer ends with a paragraph
		 * delimiter otherwise prepend a \n ourselves */
		if (gtk_text_iter_is_end (&e))
		{
			GtkTextIter iter;
			iter = e;

			gtk_text_iter_set_line_offset (&iter, 0);
			if (!gtk_text_iter_ends_line (&iter) &&
			    !gtk_text_iter_forward_to_line_end (&iter))
			{
				gtk_text_buffer_insert (buf, &e, "\n", -1);
			}
		}
	}
	else
	{
		gtk_text_iter_backward_line (&e);
	}

	/* use anon mark to be able to select after insertion */
	mark = gtk_text_buffer_create_mark (buf, NULL, &e, TRUE);

	gtk_text_buffer_insert (buf, &e, text, -1);

	gtk_text_buffer_end_user_action (buf);

	g_free (text);

	/* select the moved text */
	gtk_text_buffer_get_iter_at_mark (buf, &s, mark);
	gtk_text_buffer_select_range (buf, &s, &e);
	gtk_text_view_scroll_mark_onscreen (GTK_TEXT_VIEW (view),
					    gtk_text_buffer_get_insert (buf));

	gtk_text_buffer_delete_mark (buf, mark);
}

static gboolean
gtk_source_view_key_press_event (GtkWidget   *widget,
				 GdkEventKey *event)
{
	GtkSourceView *view;
	GtkTextBuffer *buf;
	GtkTextIter cur;
	GtkTextMark *mark;
	guint modifiers;
	gint key;
	gboolean editable;

	view = GTK_SOURCE_VIEW (widget);
	buf = gtk_text_view_get_buffer (GTK_TEXT_VIEW (widget));

	editable = gtk_text_view_get_editable (GTK_TEXT_VIEW (widget));

	/* Be careful when testing for modifier state equality:
	 * caps lock, num lock,etc need to be taken into account */
	modifiers = gtk_accelerator_get_default_mod_mask ();

	key = event->keyval;

	mark = gtk_text_buffer_get_insert (buf);
	gtk_text_buffer_get_iter_at_mark (buf, &cur, mark);

	if ((key == GDK_KEY_Return || key == GDK_KEY_KP_Enter) &&
	    !(event->state & GDK_SHIFT_MASK) &&
	    view->priv->auto_indent)
	{
		/* Auto-indent means that when you press ENTER at the end of a
		 * line, the new line is automatically indented at the same
		 * level as the previous line.
		 * SHIFT+ENTER allows to avoid autoindentation.
		 */
		gchar *indent = NULL;

		/* Calculate line indentation and create indent string. */
		indent = compute_indentation (view, &cur);

		if (indent != NULL)
		{
			/* Allow input methods to internally handle a key press event.
			 * If this function returns TRUE, then no further processing should be done
			 * for this keystroke. */
			if (gtk_text_view_im_context_filter_keypress (GTK_TEXT_VIEW (view), event))
				return TRUE;

			/* If an input method has inserted some text while handling the key press event,
			 * the cur iterm may be invalid, so get the iter again */
			gtk_text_buffer_get_iter_at_mark (buf, &cur, mark);

			/* Insert new line and auto-indent. */
			gtk_text_buffer_begin_user_action (buf);
			gtk_text_buffer_insert (buf, &cur, "\n", 1);
			gtk_text_buffer_insert (buf, &cur, indent, strlen (indent));
			g_free (indent);
			gtk_text_buffer_end_user_action (buf);
			gtk_text_view_scroll_mark_onscreen (GTK_TEXT_VIEW (widget),
							    mark);
			return TRUE;
		}
	}

	/* if tab or shift+tab:
	 * with shift+tab key is GDK_ISO_Left_Tab (yay! on win32 and mac too!)
	 */
	if ((key == GDK_KEY_Tab || key == GDK_KEY_KP_Tab || key == GDK_KEY_ISO_Left_Tab) &&
	    ((event->state & modifiers) == 0 ||
	     (event->state & modifiers) == GDK_SHIFT_MASK) &&
	    editable)
	{
		GtkTextIter s, e;
		gboolean has_selection;

		has_selection = gtk_text_buffer_get_selection_bounds (buf, &s, &e);

		if (view->priv->indent_on_tab)
		{
			/* shift+tab: always unindent */
			if (event->state & GDK_SHIFT_MASK)
			{
				unindent_lines (view, &s, &e);
				return TRUE;
			}

			/* tab: if we have a selection which spans one whole line
			 * or more, we mass indent, if the selection spans less then
			 * the full line just replace the text with \t
			 */
			if (has_selection &&
			    ((gtk_text_iter_starts_line (&s) && gtk_text_iter_ends_line (&e)) ||
			     (gtk_text_iter_get_line (&s) != gtk_text_iter_get_line (&e))))
			{
				indent_lines (view, &s, &e);
				return TRUE;
			}
		}

		insert_tab_or_spaces (view, &s, &e);
 		return TRUE;
	}

	return GTK_WIDGET_CLASS (gtk_source_view_parent_class)->key_press_event (widget, event);
}

/**
 * gtk_source_view_get_auto_indent:
 * @view: a #GtkSourceView.
 *
 * Returns whether auto indentation of text is enabled.
 *
 * Return value: %TRUE if auto indentation is enabled.
 **/
gboolean
gtk_source_view_get_auto_indent (GtkSourceView *view)
{
	g_return_val_if_fail (GTK_SOURCE_IS_VIEW (view), FALSE);

	return (view->priv->auto_indent != FALSE);
}

/**
 * gtk_source_view_set_auto_indent:
 * @view: a #GtkSourceView.
 * @enable: whether to enable auto indentation.
 *
 * If %TRUE auto indentation of text is enabled.
 **/
void
gtk_source_view_set_auto_indent (GtkSourceView *view, gboolean enable)
{
	g_return_if_fail (GTK_SOURCE_IS_VIEW (view));

	enable = (enable != FALSE);

	if (view->priv->auto_indent == enable)
		return;

	view->priv->auto_indent = enable;

	g_object_notify (G_OBJECT (view), "auto_indent");
}

/**
 * gtk_source_view_get_insert_spaces_instead_of_tabs:
 * @view: a #GtkSourceView.
 *
 * Returns whether when inserting a tabulator character it should
 * be replaced by a group of space characters.
 *
 * Return value: %TRUE if spaces are inserted instead of tabs.
 **/
gboolean
gtk_source_view_get_insert_spaces_instead_of_tabs (GtkSourceView *view)
{
	g_return_val_if_fail (GTK_SOURCE_IS_VIEW (view), FALSE);

	return (view->priv->insert_spaces != FALSE);
}

/**
 * gtk_source_view_set_insert_spaces_instead_of_tabs:
 * @view: a #GtkSourceView.
 * @enable: whether to insert spaces instead of tabs.
 *
 * If %TRUE any tabulator character inserted is replaced by a group
 * of space characters.
 **/
void
gtk_source_view_set_insert_spaces_instead_of_tabs (GtkSourceView *view, gboolean enable)
{
	g_return_if_fail (GTK_SOURCE_IS_VIEW (view));

	enable = (enable != FALSE);

	if (view->priv->insert_spaces == enable)
		return;

	view->priv->insert_spaces = enable;

	g_object_notify (G_OBJECT (view), "insert_spaces_instead_of_tabs");
}

/**
 * gtk_source_view_get_indent_on_tab:
 * @view: a #GtkSourceView.
 *
 * Returns whether when the tab key is pressed the current selection
 * should get indented instead of replaced with the \t character.
 *
 * Return value: %TRUE if the selection is indented when tab is pressed.
 *
 * Since: 1.8
 **/
gboolean
gtk_source_view_get_indent_on_tab (GtkSourceView *view)
{
	g_return_val_if_fail (GTK_SOURCE_IS_VIEW (view), FALSE);

	return (view->priv->indent_on_tab != FALSE);
}

/**
 * gtk_source_view_set_indent_on_tab:
 * @view: a #GtkSourceView.
 * @enable: whether to indent a block when tab is pressed.
 *
 * If %TRUE, when the tab key is pressed and there is a selection, the
 * selected text is indented of one level instead of being replaced with
 * the \t characters. Shift+Tab unindents the selection.
 *
 * Since: 1.8
 **/
void
gtk_source_view_set_indent_on_tab (GtkSourceView *view, gboolean enable)
{
	g_return_if_fail (GTK_SOURCE_IS_VIEW (view));

	enable = (enable != FALSE);

	if (view->priv->indent_on_tab == enable)
		return;

	view->priv->indent_on_tab = enable;

	g_object_notify (G_OBJECT (view), "indent_on_tab");
}

static void
view_dnd_drop (GtkTextView *view,
	       GdkDragContext *context,
	       gint x,
	       gint y,
	       GtkSelectionData *selection_data,
	       guint info,
	       guint timestamp,
	       gpointer data)
{

	GtkTextIter iter;

	if (info == TARGET_COLOR)
	{
		guint16 *vals;
		gchar string[] = "#000000";
		gint buffer_x;
		gint buffer_y;
		gint length = gtk_selection_data_get_length (selection_data);

		if (length < 0)
			return;

		if (gtk_selection_data_get_format (selection_data) != 16 || length != 8)
		{
			g_warning ("Received invalid color data\n");
			return;
		}

		vals = (guint16 *) gtk_selection_data_get_data (selection_data);

		vals[0] /= 256;
	        vals[1] /= 256;
		vals[2] /= 256;

		g_snprintf (string, sizeof (string), "#%02X%02X%02X", vals[0], vals[1], vals[2]);

		gtk_text_view_window_to_buffer_coords (view,
						       GTK_TEXT_WINDOW_TEXT,
						       x,
						       y,
						       &buffer_x,
						       &buffer_y);
		gtk_text_view_get_iter_at_location (view, &iter, buffer_x, buffer_y);

		if (gtk_text_view_get_editable (view))
		{
			gtk_text_buffer_insert (gtk_text_view_get_buffer (view),
						&iter,
						string,
						strlen (string));
			gtk_text_buffer_place_cursor (gtk_text_view_get_buffer (view),
						&iter);
		}

		/*
		 * FIXME: Check if the iter is inside a selection
		 * If it is, remove the selection and then insert at
		 * the cursor position - Paolo
		 */

		return;
	}
}

/**
 * gtk_source_view_get_highlight_current_line:
 * @view: a #GtkSourceView.
 *
 * Returns whether the current line is highlighted.
 *
 * Return value: %TRUE if the current line is highlighted.
 **/
gboolean
gtk_source_view_get_highlight_current_line (GtkSourceView *view)
{
	g_return_val_if_fail (GTK_SOURCE_IS_VIEW (view), FALSE);

	return (view->priv->highlight_current_line != FALSE);
}

/**
 * gtk_source_view_set_highlight_current_line:
 * @view: a #GtkSourceView.
 * @hl: whether to highlight the current line.
 *
 * If @hl is %TRUE the current line is highlighted.
 **/
void
gtk_source_view_set_highlight_current_line (GtkSourceView *view, gboolean hl)
{
	g_return_if_fail (GTK_SOURCE_IS_VIEW (view));

	hl = (hl != FALSE);

	if (view->priv->highlight_current_line == hl)
		return;

	view->priv->highlight_current_line = hl;

	gtk_widget_queue_draw (GTK_WIDGET (view));

	g_object_notify (G_OBJECT (view), "highlight_current_line");
}

/**
 * gtk_source_view_get_show_right_margin:
 * @view: a #GtkSourceView.
 *
 * Returns whether a right margin is displayed.
 *
 * Return value: %TRUE if the right margin is shown.
 **/
gboolean
gtk_source_view_get_show_right_margin (GtkSourceView *view)
{
	g_return_val_if_fail (GTK_SOURCE_IS_VIEW (view), FALSE);

	return (view->priv->show_right_margin != FALSE);
}

/**
 * gtk_source_view_set_show_right_margin:
 * @view: a #GtkSourceView.
 * @show: whether to show a right margin.
 *
 * If %TRUE a right margin is displayed.
 **/
void
gtk_source_view_set_show_right_margin (GtkSourceView *view, gboolean show)
{
	g_return_if_fail (GTK_SOURCE_IS_VIEW (view));

	show = (show != FALSE);

	if (view->priv->show_right_margin != show)
	{
		view->priv->show_right_margin = show;

		gtk_widget_queue_draw (GTK_WIDGET (view));

		g_object_notify (G_OBJECT (view), "show-right-margin");
	}
}

/**
 * gtk_source_view_get_right_margin_position:
 * @view: a #GtkSourceView.
 *
 * Gets the position of the right margin in the given @view.
 *
 * Return value: the position of the right margin.
 **/
guint
gtk_source_view_get_right_margin_position  (GtkSourceView *view)
{
	g_return_val_if_fail (GTK_SOURCE_IS_VIEW (view),
			      DEFAULT_RIGHT_MARGIN_POSITION);

	return view->priv->right_margin_pos;
}

/**
 * gtk_source_view_set_right_margin_position:
 * @view: a #GtkSourceView.
 * @pos: the width in characters where to position the right margin.
 *
 * Sets the position of the right margin in the given @view.
 **/
void
gtk_source_view_set_right_margin_position (GtkSourceView *view, guint pos)
{
	g_return_if_fail (GTK_SOURCE_IS_VIEW (view));
	g_return_if_fail (pos >= 1);
	g_return_if_fail (pos <= MAX_RIGHT_MARGIN_POSITION);

	if (view->priv->right_margin_pos != pos)
	{
		view->priv->right_margin_pos = pos;
		view->priv->cached_right_margin_pos = -1;

		gtk_widget_queue_draw (GTK_WIDGET (view));

		g_object_notify (G_OBJECT (view), "right-margin-position");
	}
}

/**
 * gtk_source_view_set_smart_home_end:
 * @view: a #GtkSourceView.
 * @smart_he: the desired behavior among #GtkSourceSmartHomeEndType.
 *
 * Set the desired movement of the cursor when HOME and END keys
 * are pressed.
 **/
void
gtk_source_view_set_smart_home_end (GtkSourceView             *view,
				    GtkSourceSmartHomeEndType  smart_he)
{
	g_return_if_fail (GTK_SOURCE_IS_VIEW (view));

	if (view->priv->smart_home_end == smart_he)
		return;

	view->priv->smart_home_end = smart_he;

	g_object_notify (G_OBJECT (view), "smart_home_end");
}

/**
 * gtk_source_view_get_smart_home_end:
 * @view: a #GtkSourceView.
 *
 * Returns a #GtkSourceSmartHomeEndType end value specifying
 * how the cursor will move when HOME and END keys are pressed.
 *
 * Return value: a #GtkSourceSmartHomeEndTypeend value.
 **/
GtkSourceSmartHomeEndType
gtk_source_view_get_smart_home_end (GtkSourceView *view)
{
	g_return_val_if_fail (GTK_SOURCE_IS_VIEW (view), FALSE);

	return view->priv->smart_home_end;
}

/**
 * gtk_source_view_set_draw_spaces:
 * @view: a #GtkSourceView.
 * @flags: #GtkSourceDrawSpacesFlags specifing how white spaces should
 * be displayed
 *
 * Set if and how the spaces should be visualized. Specifying @flags as 0 will
 * disable display of spaces.
 **/
void
gtk_source_view_set_draw_spaces (GtkSourceView            *view,
				 GtkSourceDrawSpacesFlags  flags)
{
	g_return_if_fail (GTK_SOURCE_IS_VIEW (view));

	if (view->priv->draw_spaces == flags)
		return;

	view->priv->draw_spaces = flags;

	gtk_widget_queue_draw (GTK_WIDGET (view));

	g_object_notify (G_OBJECT (view), "draw-spaces");
}

/**
 * gtk_source_view_get_draw_spaces:
 * @view: a #GtkSourceView
 *
 * Returns the #GtkSourceDrawSpacesFlags specifying if and how spaces
 * should be displayed for this @view.
 *
 * Returns: the #GtkSourceDrawSpacesFlags, 0 if no spaces should be drawn.
 **/
GtkSourceDrawSpacesFlags
gtk_source_view_get_draw_spaces (GtkSourceView *view)
{
	g_return_val_if_fail (GTK_SOURCE_IS_VIEW (view), 0);

	return view->priv->draw_spaces;
}

/**
 * gtk_source_view_get_visual_column:
 * @view: a #GtkSourceView.
 * @iter: a position in @view.
 *
 * Determines the visual column at @iter taking into
 * consideration the indent width of @view.
 *
 * Return value: the visual column at @iter.
 */
guint
gtk_source_view_get_visual_column (GtkSourceView     *view,
				   const GtkTextIter *iter)
{
	gunichar tab_char;
	GtkTextIter position;
	guint column, indent_width;

	g_return_val_if_fail (GTK_SOURCE_IS_VIEW (view), 0);
	g_return_val_if_fail (iter != NULL, 0);

	tab_char = g_utf8_get_char ("\t");

	column = 0;
	indent_width = get_real_indent_width (view);

	position = *iter;
	gtk_text_iter_set_line_offset (&position, 0);

	while (!gtk_text_iter_equal (&position, iter))
	{
		if (gtk_text_iter_get_char (&position) == tab_char)
		{
			column += (indent_width - (column % indent_width));
		}
		else
		{
			++column;
		}

		/* FIXME: this does not handle invisible text correctly, but
		 * gtk_text_iter_forward_visible_cursor_position is too slow */
		if (!gtk_text_iter_forward_char (&position))
			break;
	}

	return column;
}

static void
gtk_source_view_style_updated (GtkWidget *widget)
{
	GtkSourceView *view;

	/* call default handler first */
	GTK_WIDGET_CLASS (gtk_source_view_parent_class)->style_updated (widget);

	view = GTK_SOURCE_VIEW (widget);

	/* re-set tab stops, but only if we already modified them, i.e.
	 * do nothing with good old 8-space tabs */
	if (view->priv->tabs_set)
	{
		set_tab_stops_internal (view);
	}

	/* make sure the margin position is recalculated on next expose */
	view->priv->cached_right_margin_pos = -1;
}

static void
update_current_line_color (GtkSourceView *view)
{
	if (view->priv->style_scheme)
	{
		view->priv->current_line_color_set =
			_gtk_source_style_scheme_get_current_line_color (view->priv->style_scheme,
			                                                 &view->priv->current_line_color);
	}
}

static void
update_right_margin_colors (GtkSourceView *view)
{
	GtkWidget *widget = GTK_WIDGET (view);

	if (!gtk_widget_get_realized (widget))
		return;

	if (view->priv->right_margin_line_color != NULL)
	{
		gdk_rgba_free (view->priv->right_margin_line_color);
		view->priv->right_margin_line_color = NULL;
	}

	if (view->priv->right_margin_overlay_color != NULL)
	{
		gdk_rgba_free (view->priv->right_margin_overlay_color);
		view->priv->right_margin_overlay_color = NULL;
	}

	if (view->priv->style_scheme)
	{
		GtkSourceStyle	*style;

		style = _gtk_source_style_scheme_get_right_margin_style (view->priv->style_scheme);

		if (style != NULL)
		{
			gchar *color_str = NULL;
			gboolean color_set;
			GdkRGBA color;

			g_object_get (style,
				      "foreground-set", &color_set,
				      "foreground", &color_str,
				      NULL);

			if (color_set && (color_str != NULL) && gdk_rgba_parse (&color, color_str))
			{
				view->priv->right_margin_line_color = gdk_rgba_copy (&color);
				view->priv->right_margin_line_color->alpha =
					RIGHT_MARGIN_LINE_ALPHA / 255.;
			}

			g_free (color_str);
			color_str = NULL;

			g_object_get (style,
				      "background-set", &color_set,
				      "background", &color_str,
				      NULL);

			if (color_set && (color_str != NULL) && gdk_rgba_parse (&color, color_str))
			{
				view->priv->right_margin_overlay_color = gdk_rgba_copy (&color);
				view->priv->right_margin_overlay_color->alpha =
					RIGHT_MARGIN_OVERLAY_ALPHA / 255.;
			}

			g_free (color_str);
		}
	}

	if (view->priv->right_margin_line_color == NULL)
	{
		GtkStyleContext *context;
		GdkRGBA color;

		context = gtk_widget_get_style_context (widget);
		gtk_style_context_get_color (context, GTK_STATE_FLAG_NORMAL, &color);

		view->priv->right_margin_line_color = gdk_rgba_copy (&color);
		view->priv->right_margin_line_color->alpha =
			RIGHT_MARGIN_LINE_ALPHA / 255.;
	}
}

static void
update_spaces_color (GtkSourceView *view)
{
	GtkWidget *widget = GTK_WIDGET (view);

	if (!gtk_widget_get_realized (widget))
		return;

	if (view->priv->spaces_color != NULL)
	{
		gdk_rgba_free (view->priv->spaces_color);
		view->priv->spaces_color = NULL;
	}

	if (view->priv->style_scheme)
	{
		GtkSourceStyle	*style;

		style = _gtk_source_style_scheme_get_draw_spaces_style (view->priv->style_scheme);

		if (style != NULL)
		{
			gchar *color_str = NULL;
			GdkRGBA color;

			g_object_get (style,
				      "foreground", &color_str,
				      NULL);

			if (color_str != NULL && gdk_rgba_parse (&color, color_str))
			{
				view->priv->spaces_color = gdk_rgba_copy (&color);
			}

			g_free (color_str);
		}
	}

	if (view->priv->spaces_color == NULL)
	{
		GtkStyleContext *context;
		GdkRGBA color;

		context = gtk_widget_get_style_context (widget);
		gtk_style_context_get_color (context,
					     GTK_STATE_FLAG_INSENSITIVE,
					     &color);
		view->priv->spaces_color = gdk_rgba_copy (&color);
	}
}

static void
gtk_source_view_realize (GtkWidget *widget)
{
	GtkSourceView *view = GTK_SOURCE_VIEW (widget);

	GTK_WIDGET_CLASS (gtk_source_view_parent_class)->realize (widget);

	if (view->priv->style_scheme != NULL && !view->priv->style_scheme_applied)
	{
		_gtk_source_style_scheme_apply (view->priv->style_scheme, widget);
		view->priv->style_scheme_applied = TRUE;
	}

	update_current_line_color (view);
	update_right_margin_colors (view);
	update_spaces_color (view);
}

static void
gtk_source_view_update_style_scheme (GtkSourceView *view)
{
	GtkSourceStyleScheme *new_scheme;
	GtkTextBuffer *buffer;

	buffer = gtk_text_view_get_buffer (GTK_TEXT_VIEW (view));

	if (GTK_SOURCE_IS_BUFFER (buffer))
		new_scheme = gtk_source_buffer_get_style_scheme (GTK_SOURCE_BUFFER (buffer));
	else
		new_scheme = NULL;

	if (view->priv->style_scheme != new_scheme)
	{
		if (view->priv->style_scheme)
		{
			_gtk_source_style_scheme_unapply (view->priv->style_scheme, GTK_WIDGET (view));
			g_object_unref (view->priv->style_scheme);
		}

		view->priv->style_scheme = new_scheme;
		if (new_scheme)
			g_object_ref (new_scheme);

		if (gtk_widget_get_realized (GTK_WIDGET (view)))
		{
			_gtk_source_style_scheme_apply (new_scheme, GTK_WIDGET (view));
			update_current_line_color (view);
			update_right_margin_colors (view);
			update_spaces_color (view);
			view->priv->style_scheme_applied = TRUE;
		}
		else
		{
			view->priv->style_scheme_applied = FALSE;
		}
	}
}

static MarkCategory *
mark_category_new (GtkSourceMarkAttributes *attributes,
                   gint                     priority)
{
	MarkCategory* category = g_slice_new (MarkCategory);

	category->attributes = g_object_ref (attributes);
	category->priority = priority;

	return category;
}

static void
mark_category_free (MarkCategory *category)
{
	if (category)
	{
		g_object_unref (category->attributes);
		g_slice_free (MarkCategory, category);
	}
}

/**
 * gtk_source_view_get_completion:
 * @view: a #GtkSourceView.
 *
 * Gets the #GtkSourceCompletion associated with @view.
 *
 * Returns: (type GtkSource.Completion) (transfer none):
 * the #GtkSourceCompletion associated with @view.
 */
GtkSourceCompletion *
gtk_source_view_get_completion (GtkSourceView *view)
{
	g_return_val_if_fail (GTK_SOURCE_IS_VIEW (view), NULL);

	if (view->priv->completion == NULL && !view->priv->dispose_has_run)
	{
		view->priv->completion = gtk_source_completion_new (view);
	}

	return view->priv->completion;
}

/**
 * gtk_source_view_get_gutter:
 * @view: a #GtkSourceView.
 * @window_type: the gutter window type.
 *
 * Returns the #GtkSourceGutter object associated with @window_type for @view.
 * Only GTK_TEXT_WINDOW_LEFT and GTK_TEXT_WINDOW_RIGHT are supported,
 * respectively corresponding to the left and right gutter. The line numbers
 * and mark category icons are rendered in the gutter corresponding to
 * GTK_TEXT_WINDOW_LEFT.
 *
 * Since: 2.8
 *
 * Returns: (transfer none): the #GtkSourceGutter.
 **/
GtkSourceGutter *
gtk_source_view_get_gutter (GtkSourceView     *view,
                            GtkTextWindowType  window_type)
{
	g_return_val_if_fail (GTK_SOURCE_IS_VIEW (view), NULL);
	g_return_val_if_fail (window_type == GTK_TEXT_WINDOW_LEFT ||
	                      window_type == GTK_TEXT_WINDOW_RIGHT, NULL);

	if (window_type == GTK_TEXT_WINDOW_LEFT)
	{
		if (view->priv->left_gutter == NULL)
		{
			view->priv->left_gutter = gtk_source_gutter_new (view,
			                                                 window_type);
		}

		return view->priv->left_gutter;
	}
	else
	{
		if (view->priv->right_gutter == NULL)
		{
			view->priv->right_gutter = gtk_source_gutter_new (view,
			                                                 window_type);
		}

		return view->priv->right_gutter;
	}
}

/**
 * gtk_source_view_set_mark_attributes:
 * @view: a #GtkSourceView.
 * @category: the category.
 * @attributes: mark attributes.
 * @priority: priority of the category.
 *
 * Sets attributes and priority for the @category.
 */
void
gtk_source_view_set_mark_attributes (GtkSourceView           *view,
                                     const gchar             *category,
                                     GtkSourceMarkAttributes *attributes,
                                     gint                     priority)
{
	MarkCategory *mark_category;

	g_return_if_fail (GTK_SOURCE_IS_VIEW (view));
	g_return_if_fail (category != NULL);
	g_return_if_fail (GTK_SOURCE_IS_MARK_ATTRIBUTES (attributes));
	g_return_if_fail (priority >= 0);

	mark_category = mark_category_new (attributes, priority);
	g_hash_table_replace (view->priv->mark_categories,
	                      g_strdup (category),
	                      mark_category);
}

/**
 * gtk_source_view_get_mark_attributes:
 * @view: a #GtkSourceView.
 * @category: the category.
 * @priority: place where priority of the category will be stored.
 *
 * Gets attributes and priority for the @category.
 *
 * Returns: (transfer none): #GtkSourceMarkAttributes for the @category.
 * The object belongs to @view, so it must not be unreffed.
 */
GtkSourceMarkAttributes *
gtk_source_view_get_mark_attributes (GtkSourceView           *view,
                                     const gchar             *category,
                                     gint                    *priority)
{
	MarkCategory *mark_category;

	g_return_val_if_fail (GTK_SOURCE_IS_VIEW (view), NULL);
	g_return_val_if_fail (category != NULL, NULL);

	mark_category = g_hash_table_lookup (view->priv->mark_categories,
	                                     category);

	if (mark_category)
	{
		if (priority)
		{
			*priority = mark_category->priority;
		}
		return mark_category->attributes;
	}
	return NULL;
}
