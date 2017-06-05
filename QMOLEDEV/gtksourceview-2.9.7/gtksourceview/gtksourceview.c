/* -*- Mode: C; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8; coding: utf-8 -*- */
/*  gtksourceview.c
 *
 *  Copyright (C) 2001 - Mikael Hermansson <tyan@linux.se> and
 *  Chris Phelps <chicane@reninet.com>
 *
 *  Copyright (C) 2002 - Jeroen Zwartepoorte
 *
 *  Copyright (C) 2003 - Gustavo Giráldez and Paolo Maggi
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU Library General Public License as published by
 *  the Free Software Foundation; either version 2 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU Library General Public License for more details.
 *
 *  You should have received a copy of the GNU Library General Public License
 *  along with this program; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
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

/**
 * SECTION:view
 * @Short_description: The view object
 * @Title: GtkSourceView
 * @See_also: #GtkTextView,#GtkSourceBuffer
 *
 * GtkSourceView is the main object of the gtksourceview library. It provides
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

#define COMPOSITE_ALPHA                 225
#define GUTTER_PIXMAP 			16
#define DEFAULT_TAB_WIDTH 		8
#define MAX_TAB_WIDTH			32
#define MAX_INDENT_WIDTH		32

#define DEFAULT_RIGHT_MARGIN_POSITION	80
#define MAX_RIGHT_MARGIN_POSITION	200

#define RIGHT_MARING_LINE_ALPHA		40
#define RIGHT_MARING_OVERLAY_ALPHA	15

/* Signals */
enum {
	UNDO,
	REDO,
	SHOW_COMPLETION,
	LINE_MARK_ACTIVATED,
	MOVE_LINES,
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
	guint		 tab_width;
	gboolean	 tabs_set;
	gint		 indent_width;
	gboolean 	 show_line_numbers;
	gboolean	 show_line_marks;
	gboolean	 auto_indent;
	gboolean	 insert_spaces;
	gboolean	 highlight_current_line;
	gboolean	 indent_on_tab;
	GtkSourceSmartHomeEndType smart_home_end;

	gboolean	 show_right_margin;
	guint		 right_margin_pos;
	gint             cached_right_margin_pos;

	gboolean	 style_scheme_applied;
	GtkSourceStyleScheme *style_scheme;
	GdkColor        *right_margin_line_color;
	GdkColor        *right_margin_overlay_color;

	GtkSourceDrawSpacesFlags draw_spaces;
	GdkColor        *spaces_color;

	GHashTable 	*mark_categories;

	GtkSourceBuffer *source_buffer;

	GtkSourceGutter *left_gutter;
	GtkSourceGutter *right_gutter;

	GtkCellRenderer *line_renderer;
	GtkCellRenderer *marks_renderer;
	
	GdkColor         current_line_color;
	
	GtkSourceCompletion	*completion;
	
	guint            current_line_color_set : 1;
	guint            destroy_has_run : 1;
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

typedef struct
{
	gint priority;
	
	IconType icon_type;
	GdkPixbuf *icon_pixbuf;
	gchar *icon_stock;
	gchar *icon_name;
	
	GdkPixbuf *cached_icon;
	
	GtkSourceViewMarkTooltipFunc tooltip_func;
	gpointer tooltip_data;
	GDestroyNotify tooltip_data_notify;

	GdkColor background;
	guint background_set : 1;
	guint tooltip_markup : 1;
} MarkCategory;

/* Prototypes. */
static GObject *gtk_source_view_constructor		(GType               type,
							 guint               n_construct_properties,
							 GObjectConstructParam *construct_param);
static void 	gtk_source_view_finalize 		(GObject            *object);

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

static void 	gtk_source_view_get_lines 		(GtkTextView       *text_view,
				       			 gint               first_y,
				       			 gint               last_y,
				       			 GArray            *buffer_coords,
				       			 GArray            *line_heights,
				       			 GArray            *numbers,
				       			 gint              *countp);
static gint     gtk_source_view_expose 			(GtkWidget         *widget,
							 GdkEventExpose    *event);
static void	gtk_source_view_move_lines		(GtkSourceView     *view,
							 gboolean           copy,
							 gint               step);
static gboolean	gtk_source_view_key_press_event		(GtkWidget         *widget,
							 GdkEventKey       *event);
static void 	view_dnd_drop 				(GtkTextView       *view,
							 GdkDragContext    *context,
							 gint               x,
							 gint               y,
							 GtkSelectionData  *selection_data,
							 guint              info,
							 guint              timestamp,
							 gpointer           data);

static gint	calculate_real_tab_width 		(GtkSourceView     *view,
							 guint              tab_size,
							 gchar              c);

static void	gtk_source_view_set_property 		(GObject           *object,
							 guint              prop_id,
							 const GValue      *value,
							 GParamSpec        *pspec);
static void	gtk_source_view_get_property		(GObject           *object,
							 guint              prop_id,
							 GValue            *value,
							 GParamSpec        *pspec);

static void     gtk_source_view_style_set               (GtkWidget         *widget,
							 GtkStyle          *previous_style);
static void	gtk_source_view_realize			(GtkWidget         *widget);
static void	gtk_source_view_destroy			(GtkObject         *object);
static void	gtk_source_view_update_style_scheme	(GtkSourceView     *view);

static MarkCategory *
		gtk_source_view_get_mark_category	(GtkSourceView     *view,
							 GtkSourceMark     *mark);

static MarkCategory *
		gtk_source_view_ensure_category		(GtkSourceView     *view,
							 const gchar       *name);
static MarkCategory *
		mark_category_new			(gint               priority);
static void	mark_category_free			(MarkCategory      *cat);

/* Private functions. */
static void
gtk_source_view_class_init (GtkSourceViewClass *klass)
{
	GObjectClass	 *object_class;
	GtkTextViewClass *textview_class;
	GtkBindingSet    *binding_set;
	GtkWidgetClass   *widget_class;
	GtkObjectClass   *gtk_object_class;

	object_class 	 = G_OBJECT_CLASS (klass);
	textview_class 	 = GTK_TEXT_VIEW_CLASS (klass);
	widget_class 	 = GTK_WIDGET_CLASS (klass);
	gtk_object_class = GTK_OBJECT_CLASS (klass);

	object_class->constructor = gtk_source_view_constructor;
	object_class->finalize = gtk_source_view_finalize;
	object_class->get_property = gtk_source_view_get_property;
	object_class->set_property = gtk_source_view_set_property;

	widget_class->key_press_event = gtk_source_view_key_press_event;
	widget_class->expose_event = gtk_source_view_expose;
	widget_class->style_set = gtk_source_view_style_set;
	widget_class->realize = gtk_source_view_realize;
	gtk_object_class->destroy = gtk_source_view_destroy;

	textview_class->populate_popup = gtk_source_view_populate_popup;
	textview_class->move_cursor = gtk_source_view_move_cursor;

	klass->undo = gtk_source_view_undo;
	klass->redo = gtk_source_view_redo;
	klass->show_completion = gtk_source_view_show_completion_real;
	klass->move_lines = gtk_source_view_move_lines;

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
							      GTK_TYPE_SOURCE_COMPLETION,
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
							    GTK_TYPE_SOURCE_SMART_HOME_END_TYPE,
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
							    GTK_TYPE_SOURCE_DRAW_SPACES_FLAGS,
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

	binding_set = gtk_binding_set_by_class (klass);

	gtk_binding_entry_add_signal (binding_set,
				      GDK_z,
				      GDK_CONTROL_MASK,
				      "undo", 0);
	gtk_binding_entry_add_signal (binding_set,
				      GDK_z,
				      GDK_CONTROL_MASK | GDK_SHIFT_MASK,
				      "redo", 0);
	gtk_binding_entry_add_signal (binding_set,
				      GDK_F14,
				      0,
				      "undo", 0);
	gtk_binding_entry_add_signal (binding_set,
				      GDK_space,
				      GDK_CONTROL_MASK,
				      "show-completion", 0);

	gtk_binding_entry_add_signal (binding_set,
				      GDK_Up,
				      GDK_MOD1_MASK,
				      "move_lines", 2,
				      G_TYPE_BOOLEAN, FALSE,
				      G_TYPE_INT, -1);
	gtk_binding_entry_add_signal (binding_set,
				      GDK_KP_Up,
				      GDK_MOD1_MASK,
				      "move_lines", 2,
				      G_TYPE_BOOLEAN, FALSE,
				      G_TYPE_INT, -1);
	gtk_binding_entry_add_signal (binding_set,
				      GDK_Down,
				      GDK_MOD1_MASK,
				      "move_lines", 2,
				      G_TYPE_BOOLEAN, FALSE,
				      G_TYPE_INT, 1);
	gtk_binding_entry_add_signal (binding_set,
				      GDK_KP_Down,
				      GDK_MOD1_MASK,
				      "move_lines", 2,
				      G_TYPE_BOOLEAN, FALSE,
				      G_TYPE_INT, 1);

	gtk_binding_entry_add_signal (binding_set,
				      GDK_Up,
				      GDK_MOD1_MASK | GDK_SHIFT_MASK,
				      "move_viewport", 2,
				      GTK_TYPE_SCROLL_STEP, GTK_SCROLL_STEPS,
				      G_TYPE_INT, -1);

	gtk_binding_entry_add_signal (binding_set,
				      GDK_KP_Up,
				      GDK_MOD1_MASK | GDK_SHIFT_MASK,
				      "move_viewport", 2,
				      GTK_TYPE_SCROLL_STEP, GTK_SCROLL_STEPS,
				      G_TYPE_INT, -1);

	gtk_binding_entry_add_signal (binding_set,
				      GDK_Down,
				      GDK_MOD1_MASK | GDK_SHIFT_MASK,
				      "move_viewport", 2,
				      GTK_TYPE_SCROLL_STEP, GTK_SCROLL_STEPS,
				      G_TYPE_INT, 1);

	gtk_binding_entry_add_signal (binding_set,
				      GDK_KP_Down,
				      GDK_MOD1_MASK | GDK_SHIFT_MASK,
				      "move_viewport", 2,
				      GTK_TYPE_SCROLL_STEP, GTK_SCROLL_STEPS,
				      G_TYPE_INT, 1);

	gtk_binding_entry_add_signal (binding_set,
				      GDK_Page_Up,
				      GDK_MOD1_MASK | GDK_SHIFT_MASK,
				      "move_viewport", 2,
				      GTK_TYPE_SCROLL_STEP, GTK_SCROLL_PAGES,
				      G_TYPE_INT, -1);

	gtk_binding_entry_add_signal (binding_set,
				      GDK_KP_Page_Up,
				      GDK_MOD1_MASK | GDK_SHIFT_MASK,
				      "move_viewport", 2,
				      GTK_TYPE_SCROLL_STEP, GTK_SCROLL_PAGES,
				      G_TYPE_INT, -1);

	gtk_binding_entry_add_signal (binding_set,
				      GDK_Page_Down,
				      GDK_MOD1_MASK | GDK_SHIFT_MASK,
				      "move_viewport", 2,
				      GTK_TYPE_SCROLL_STEP, GTK_SCROLL_PAGES,
				      G_TYPE_INT, 1);

	gtk_binding_entry_add_signal (binding_set,
				      GDK_KP_Page_Down,
				      GDK_MOD1_MASK | GDK_SHIFT_MASK,
				      "move_viewport", 2,
				      GTK_TYPE_SCROLL_STEP, GTK_SCROLL_PAGES,
				      G_TYPE_INT, 1);

	gtk_binding_entry_add_signal (binding_set,
				      GDK_Home,
				      GDK_MOD1_MASK | GDK_SHIFT_MASK,
				      "move_viewport", 2,
				      GTK_TYPE_SCROLL_STEP, GTK_SCROLL_ENDS,
				      G_TYPE_INT, -1);

	gtk_binding_entry_add_signal (binding_set,
				      GDK_KP_Home,
				      GDK_MOD1_MASK | GDK_SHIFT_MASK,
				      "move_viewport", 2,
				      GTK_TYPE_SCROLL_STEP, GTK_SCROLL_ENDS,
				      G_TYPE_INT, -1);

	gtk_binding_entry_add_signal (binding_set,
				      GDK_End,
				      GDK_MOD1_MASK | GDK_SHIFT_MASK,
				      "move_viewport", 2,
				      GTK_TYPE_SCROLL_STEP, GTK_SCROLL_ENDS,
				      G_TYPE_INT, 1);

	gtk_binding_entry_add_signal (binding_set,
				      GDK_KP_End,
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

	g_return_if_fail (GTK_IS_SOURCE_VIEW (object));

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

	g_return_if_fail (GTK_IS_SOURCE_VIEW (object));

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
	/* we use view->buffer directly instead of get_buffer()
	 * since the latter causes the buffer to be recreated and that
	 * is not a good idea when finalizing */
	set_source_buffer (view, GTK_TEXT_VIEW (view)->buffer);
}

static gint
sort_marks_by_priority (gconstpointer m1,
			gconstpointer m2,
			gpointer data)
{
	GtkSourceMark *mark1 = GTK_SOURCE_MARK (m1);
	GtkSourceMark *mark2 = GTK_SOURCE_MARK (m2);
	GtkSourceView *view = GTK_SOURCE_VIEW (data);
	GtkTextIter iter1, iter2;
	gint line1;
	gint line2;

	gtk_text_buffer_get_iter_at_mark (gtk_text_mark_get_buffer (GTK_TEXT_MARK (mark1)),
					  &iter1,
					  GTK_TEXT_MARK (mark1));
	gtk_text_buffer_get_iter_at_mark (gtk_text_mark_get_buffer (GTK_TEXT_MARK (mark2)),
					  &iter2,
					  GTK_TEXT_MARK (mark2));

	line1 = gtk_text_iter_get_line (&iter1);
	line2 = gtk_text_iter_get_line (&iter2);

	if (line1 == line2)
	{
		guint priority1 = gtk_source_view_get_mark_category_priority (view,
						gtk_source_mark_get_category (mark1));
		guint priority2 = gtk_source_view_get_mark_category_priority (view,
						gtk_source_mark_get_category (mark2));

		return priority1 - priority2;
	}
	else
	{
		return line2 - line1;
	}
}

static GdkPixbuf *
get_icon_from_stock (GtkSourceView *view,
                     const gchar   *stock_id,
                     gint           size)
{
	gchar *sizename;
	GtkIconSize iconsize;
	
	/* Check special icon size */
	sizename = g_strdup_printf ("GtkSourceMarkCategoryIcon%d", size);
	iconsize = gtk_icon_size_from_name (sizename);
	
	if (iconsize == GTK_ICON_SIZE_INVALID)
	{
		iconsize = gtk_icon_size_register (sizename, size, size);
	}
	
	g_free (sizename);
	
	if (iconsize == GTK_ICON_SIZE_INVALID)
	{
		return NULL;
	}
	
	return gtk_widget_render_icon (GTK_WIDGET (view), 
	                               stock_id, 
	                               iconsize, 
	                               NULL);
}

static GdkPixbuf *
get_icon_from_name (GtkSourceView *view,
                    const gchar   *name,
                    gint           size)
{
	GtkIconTheme *theme;
	
	theme = gtk_icon_theme_get_for_screen (gtk_widget_get_screen (GTK_WIDGET (view)));
	
	return gtk_icon_theme_load_icon (theme,
	                                 name, 
	                                 size,
	                                 GTK_ICON_LOOKUP_USE_BUILTIN | 
	                                 GTK_ICON_LOOKUP_FORCE_SIZE,
	                                 NULL);
}

static void
remove_cached_category_icon (MarkCategory *category)
{
	if (category->cached_icon != NULL)
	{
		g_object_unref (category->cached_icon);
		category->cached_icon = NULL;
	}
}

static GdkPixbuf *
get_mark_category_pixbuf (GtkSourceView *view,
                          const gchar   *category,
                          gint           size)
{
	MarkCategory *cat;
	
	cat = g_hash_table_lookup (view->priv->mark_categories, category);
	
	if (cat == NULL)
	{
		return NULL;
	}
	
	if (cat->cached_icon && 
	    (gdk_pixbuf_get_height (cat->cached_icon) == size ||
	     gdk_pixbuf_get_width (cat->cached_icon) == size))
	{
		return cat->cached_icon;
	}
	
	/* Regenerate icon */
	remove_cached_category_icon (cat);
	
	switch (cat->icon_type)
	{
		case ICON_TYPE_NONE:
			break;
		case ICON_TYPE_PIXBUF:
			if (cat->icon_pixbuf == NULL)
			{
				return NULL;
			}
			
			if (gdk_pixbuf_get_width (cat->icon_pixbuf) <= size &&
			    gdk_pixbuf_get_height (cat->icon_pixbuf) <= size)
			{
				cat->cached_icon = g_object_ref (cat->icon_pixbuf);
			}
			else
			{
				cat->cached_icon = gdk_pixbuf_scale_simple (cat->icon_pixbuf,
					                                    size,
					                                    size,
					                                    GDK_INTERP_BILINEAR);
			}
			break;
		case ICON_TYPE_STOCK:
			cat->cached_icon = get_icon_from_stock (view,
			                                        cat->icon_stock,
			                                        size);
			break;
		case ICON_TYPE_NAME:
			cat->cached_icon = get_icon_from_name (view,
			                                       cat->icon_name,
			                                       size);
			break;
		default:
			g_return_val_if_reached (NULL);
	}

	return cat->cached_icon;
}

static GdkPixbuf *
composite_marks (GtkSourceView *view,
		 GSList        *marks,
		 gint           size)
{
	GdkPixbuf *composite;
	gint mark_width, mark_height;

	/* Draw the mark with higher priority */
	marks = g_slist_sort_with_data (marks, sort_marks_by_priority, view);

	composite = NULL;
	mark_width = mark_height = 0;

	/* composite all the pixbufs for the marks present at the line */
	do
	{
		GtkSourceMark *mark;
		GdkPixbuf *pixbuf;

		mark = marks->data;

		pixbuf = get_mark_category_pixbuf (view,
					           gtk_source_mark_get_category (mark),
					           size);

		if (pixbuf != NULL)
		{
			if (composite == NULL)
			{
				composite = gdk_pixbuf_copy (pixbuf);
				mark_width = gdk_pixbuf_get_width (composite);
				mark_height = gdk_pixbuf_get_height (composite);
			}
			else
			{
				gint pixbuf_w;
				gint pixbuf_h;

				pixbuf_w = gdk_pixbuf_get_width (pixbuf);
				pixbuf_h = gdk_pixbuf_get_height (pixbuf);

				gdk_pixbuf_composite (pixbuf,
						      composite,
						      0, 0,
						      mark_width, mark_height,
						      0, 0,
						      (double) pixbuf_w / mark_width,
						      (double) pixbuf_h / mark_height,
						      GDK_INTERP_BILINEAR,
						      COMPOSITE_ALPHA);
			}
		}

		marks = g_slist_next (marks);
	}
	while (marks);

	return composite;
}

static int
measure_line_height (GtkSourceView *view)
{
	PangoLayout *layout;
	gint height = 12;
	
	layout = gtk_widget_create_pango_layout (GTK_WIDGET (view), "QWERTY");
	
	if (layout)
	{
		pango_layout_get_pixel_size (layout, NULL, &height);
		g_object_unref (layout);
	}
	
	return height - 2;
}

static void
marks_renderer_data_func (GtkSourceGutter *gutter,
                          GtkCellRenderer *renderer,
                          gint             line_number,
                          gboolean         current_line,
                          GtkSourceView   *view)
{
	GSList *marks;
	GdkPixbuf *pixbuf = NULL;
	int size = 0;

	if (view->priv->source_buffer)
	{
		marks = gtk_source_buffer_get_source_marks_at_line (view->priv->source_buffer,
								    line_number,
								    NULL);

		if (marks != NULL)
		{
			GtkTextIter iter;

			gtk_text_buffer_get_iter_at_line (GTK_TEXT_BUFFER (view->priv->source_buffer),
							  &iter,
							  line_number);

			if (size == 0)
			{
				size = measure_line_height (view);
			}

			/* draw marks for the line */
			pixbuf = composite_marks (view, marks, size);
			g_slist_free (marks);
		}
	}

	g_object_set (G_OBJECT (renderer),
	              "pixbuf", pixbuf,
	              "xpad", 2,
	              "ypad", 1,
	              "yalign", 0.0,
	              "xalign", 0.5,
	              "mode", GTK_CELL_RENDERER_MODE_ACTIVATABLE,
	              NULL);
}

static void
line_renderer_data_func (GtkSourceGutter *gutter,
                         GtkCellRenderer *renderer,
                         gint             line_number,
                         gboolean         current_line,
                         GtkSourceView   *view)
{
	int weight;
	gchar *text;
	GtkStyle *style;

	if (current_line && gtk_text_view_get_cursor_visible (GTK_TEXT_VIEW (view)))
	{
		weight = PANGO_WEIGHT_BOLD;
	}
	else
	{
		weight = PANGO_WEIGHT_NORMAL;
	}

	text = g_strdup_printf ("%d", line_number + 1);
	g_object_set (G_OBJECT (renderer),
	              "text", text,
	              "xalign", 1.0,
	              "yalign", 0.0,
	              "xpad", 2,
	              "ypad", 0,
	              "weight", weight,
	              "mode", GTK_CELL_RENDERER_MODE_ACTIVATABLE,
	              NULL);

	style = gtk_widget_get_style (GTK_WIDGET (view));
	if (style != NULL)
	{
		g_object_set (G_OBJECT (renderer),
			      "foreground-gdk", &style->fg[GTK_STATE_NORMAL],
			      NULL);
	}

	g_object_set (G_OBJECT (renderer),
	              "background-set", FALSE,
	              NULL);

	g_free (text);
}

static void
line_renderer_size_func (GtkSourceGutter *gutter,
                         GtkCellRenderer *renderer,
                         GtkSourceView   *view)
{
	gchar *text;
	gint count;

	count = gtk_text_buffer_get_line_count (gtk_text_view_get_buffer (GTK_TEXT_VIEW (view)));
	text = g_strdup_printf ("%d", MAX(99, count));

	/* measure with bold, just in case font is rendered larger */
	g_object_set (G_OBJECT (renderer),
	              "text", text,
	              "xpad", 2,
	              "ypad", 0,
	              "weight", PANGO_WEIGHT_BOLD,
	               NULL);

	g_free (text);
}

static void
extend_selection_to_line (GtkTextBuffer *buf, GtkTextIter *line_start)
{
	GtkTextIter start;
	GtkTextIter end;
	GtkTextIter line_end;

	gtk_text_buffer_get_selection_bounds (buf, &start, &end);

	line_end = *line_start;
	gtk_text_iter_forward_to_line_end (&line_end);

	if (gtk_text_iter_compare (&start, line_start) < 0)
	{
		gtk_text_buffer_select_range (buf, &start, &line_end);
	}
	else if (gtk_text_iter_compare (&end, &line_end) < 0)
	{
		/* if the selection is in this line, extend
		 * the selection to the whole line */
		gtk_text_buffer_select_range (buf, &line_end, line_start);
	}
	else
	{
		gtk_text_buffer_select_range (buf, &end, line_start);
	}
}

static void
select_line (GtkTextBuffer *buf, GtkTextIter *line_start)
{
	GtkTextIter iter;

	iter = *line_start;

	if (!gtk_text_iter_ends_line (&iter))
		gtk_text_iter_forward_to_line_end (&iter);

	/* Select the line, put the cursor at the end of the line */
	gtk_text_buffer_select_range (buf, &iter, line_start);
}

static void
marks_renderer_size_func (GtkSourceGutter *gutter,
                          GtkCellRenderer *renderer,
                          GtkSourceView   *view)
{
	gint size;
	GdkPixbuf *pixbuf;
	
	size = measure_line_height (view);
	
	pixbuf = gdk_pixbuf_new (GDK_COLORSPACE_RGB,
	                         TRUE,
	                         8,
	                         size,
	                         size);

	g_object_set (G_OBJECT (renderer),
	              "pixbuf", pixbuf,
	              "xpad", 2,
	              "ypad", 1,
	              NULL);
	
	g_object_unref (pixbuf);
}

static void
renderer_activated (GtkSourceGutter *gutter,
                    GtkCellRenderer *renderer,
                    GtkTextIter     *iter,
                    GdkEvent        *event,
                    GtkSourceView   *view)
{
	if (renderer == view->priv->marks_renderer)
	{
		g_signal_emit (view,
		               signals[LINE_MARK_ACTIVATED],
		               0,
		               iter,
		               event);
	}
	else if (renderer == view->priv->line_renderer)
	{
		GtkTextBuffer *buf = gtk_text_view_get_buffer (GTK_TEXT_VIEW (view));

		if (event->type == GDK_BUTTON_PRESS && (event->button.button == 1))
		{
			if ((event->button.state & GDK_CONTROL_MASK) != 0)
			{
				/* Single click + Ctrl -> select the line */
				select_line (buf, iter);
			}
			else if ((event->button.state & GDK_SHIFT_MASK) != 0)
			{
				/* Single click + Shift -> extended current
				   selection to include the clicked line */
				extend_selection_to_line (buf, iter);
			}
			else
			{
				gtk_text_buffer_place_cursor (buf, iter);
			}
		}
		else if (event->type == GDK_2BUTTON_PRESS && (event->button.button == 1))
		{
			select_line (buf, iter);
		}
	}
}

static gboolean
set_tooltip_widget_from_marks (GtkSourceView *view,
			       GtkTooltip *tooltip,
			       GSList *marks)
{
	GtkWidget *vbox = NULL;

	while (marks != NULL)
	{
		const gchar *category;
		GtkSourceMark *mark;
		MarkCategory *cat;

		mark = marks->data;
		category = gtk_source_mark_get_category (mark);
		
		cat = gtk_source_view_get_mark_category (view, mark);

		if (cat != NULL && cat->tooltip_func != NULL)
		{
			gchar *text;

			text = cat->tooltip_func (mark, cat->tooltip_data);
			if (text != NULL)
			{
				GtkWidget *label;
				GtkWidget *hbox;

				if (vbox == NULL)
				{
					vbox = gtk_vbox_new (FALSE, 0);
					gtk_widget_show (vbox);
				}

				hbox = gtk_hbox_new (FALSE, 4);
				gtk_widget_show (hbox);
				gtk_box_pack_start (GTK_BOX (vbox), hbox,
				    FALSE, FALSE, 0);

				GdkPixbuf *pixbuf;
				gint size;

				label = gtk_label_new (NULL);

				if (cat->tooltip_markup)
					gtk_label_set_markup (GTK_LABEL (label), text);
				else
					gtk_label_set_text (GTK_LABEL (label), text);

				gtk_misc_set_alignment (GTK_MISC (label), 0, 0);
				gtk_widget_show (label);
			
				gtk_icon_size_lookup (GTK_ICON_SIZE_MENU, NULL, &size);
				pixbuf = get_mark_category_pixbuf (view, category, size);

				if (pixbuf != NULL)
				{
					GtkWidget *image;
					PangoLayoutLine *line;
					PangoRectangle rect;
					GtkWidget *align;

					align = gtk_alignment_new (0, 0, 0, 0);
					gtk_widget_show (align);

					image = gtk_image_new_from_pixbuf (pixbuf);
					gtk_misc_set_alignment (GTK_MISC (image), 0, 0);
					gtk_widget_show (image);

					/* Measure up to align exact */
					line = pango_layout_get_line (gtk_label_get_layout (GTK_LABEL (label)), 0);
					pango_layout_line_get_pixel_extents (line, NULL, &rect);

					gtk_alignment_set_padding (GTK_ALIGNMENT (align),
						                   (rect.height > size ? rect.height - size : size - rect.height) - 1,
						                   0, 0, 0);
					if (rect.height > size)
					{
						gtk_container_add (GTK_CONTAINER (align),
						                   image);

						image = align;
					}
					else if (size > rect.height)
					{
						gtk_container_add (GTK_CONTAINER (align),
						                   label);
						label = align;
					}
					else
					{
						gtk_widget_destroy (align);
					}

					gtk_box_pack_start (GTK_BOX (hbox), 
					                    image,
					                    FALSE,
					                    FALSE,
					                    0);
				}

				gtk_box_pack_end (GTK_BOX (hbox),
				                  label,
				                  TRUE,
				                  TRUE,
				                  0);
				
				if (g_slist_length (marks) != 1)
				{
					GtkWidget *separator;
					
					separator = gtk_hseparator_new ();
					gtk_widget_show (separator);
					gtk_box_pack_start (GTK_BOX (vbox), separator,
							    FALSE, FALSE, 0);
				}

				g_free (text);
			}
		}

		marks = g_slist_delete_link (marks, marks);
	}

	if (vbox == NULL)
		return FALSE;

	gtk_tooltip_set_custom (tooltip, vbox);

	return TRUE;
}

static gboolean
renderer_query_tooltip (GtkSourceGutter *gutter,
                        GtkCellRenderer *renderer,
                        GtkTextIter     *iter,
                        GtkTooltip      *tooltip,
                        GtkSourceView   *view)
{
	GSList *marks;
	GtkSourceBuffer *buffer;
	gint line;

	if (renderer != view->priv->marks_renderer)
	{
		return FALSE;
	}

	buffer = GTK_SOURCE_BUFFER (gtk_text_view_get_buffer (GTK_TEXT_VIEW (view)));
	line = gtk_text_iter_get_line (iter);

	marks = gtk_source_buffer_get_source_marks_at_line (buffer,
							    line,
							    NULL);

	if (marks != NULL)
	{
		marks = g_slist_sort_with_data (marks, sort_marks_by_priority, view);
		marks = g_slist_reverse (marks);

		return set_tooltip_widget_from_marks (view, tooltip, marks);
	}

	return FALSE;
}

static void
init_left_gutter (GtkSourceView *view)
{
	GtkSourceGutter *gutter;

	view->priv->line_renderer = gtk_cell_renderer_text_new ();
	view->priv->marks_renderer = gtk_cell_renderer_pixbuf_new ();

	gutter = gtk_source_view_get_gutter (view, GTK_TEXT_WINDOW_LEFT);

	gtk_source_gutter_insert (gutter, 
	                          view->priv->line_renderer, 
	                          GTK_SOURCE_VIEW_GUTTER_POSITION_LINES);

	gtk_source_gutter_insert (gutter, 
	                          view->priv->marks_renderer,
	                          GTK_SOURCE_VIEW_GUTTER_POSITION_MARKS);

	gtk_cell_renderer_set_fixed_size (view->priv->line_renderer, 0, 0);
	gtk_cell_renderer_set_fixed_size (view->priv->marks_renderer, 0, 0);

	gtk_source_gutter_set_cell_data_func (gutter,
	                                      view->priv->line_renderer,
	                                      (GtkSourceGutterDataFunc)line_renderer_data_func,
	                                      view,
	                                      NULL);

	gtk_source_gutter_set_cell_size_func (gutter,
	                                      view->priv->line_renderer,
	                                      (GtkSourceGutterSizeFunc)line_renderer_size_func,
	                                      view,
	                                      NULL);

	gtk_source_gutter_set_cell_data_func (gutter,
	                                      view->priv->marks_renderer,
	                                      (GtkSourceGutterDataFunc)marks_renderer_data_func,
	                                      view,
	                                      NULL);
	                                      
	gtk_source_gutter_set_cell_size_func (gutter,
	                                      view->priv->marks_renderer,
	                                      (GtkSourceGutterSizeFunc)marks_renderer_size_func,
	                                      view,
	                                      NULL);
	                                      
	g_signal_connect (gutter,
	                  "cell-activated",
	                  G_CALLBACK (renderer_activated),
	                  view);

	g_signal_connect (gutter,
	                  "query-tooltip",
	                  G_CALLBACK (renderer_query_tooltip),
	                  view);
}

static void
gtk_source_view_init (GtkSourceView *view)
{
	GtkTargetList *tl;

	view->priv = G_TYPE_INSTANCE_GET_PRIVATE (view, GTK_TYPE_SOURCE_VIEW,
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

	view->priv->mark_categories = g_hash_table_new_full (g_str_hash, g_str_equal,
							     (GDestroyNotify) g_free,
							     (GDestroyNotify) mark_category_free);

	init_left_gutter (view);

	tl = gtk_drag_dest_get_target_list (GTK_WIDGET (view));
	g_return_if_fail (tl != NULL);

	gtk_target_list_add_table (tl, drop_types, G_N_ELEMENTS (drop_types));

	g_signal_connect (view,
			  "drag_data_received",
			  G_CALLBACK (view_dnd_drop),
			  NULL);

	g_signal_connect (view,
			  "notify::buffer",
			  G_CALLBACK (notify_buffer),
			  NULL);
}

static GObject *
gtk_source_view_constructor (GType                  type,
			     guint                  n_construct_properties,
			     GObjectConstructParam *construct_param)
{
	GObject *object;
	GtkSourceView *view;

	object = G_OBJECT_CLASS(gtk_source_view_parent_class)->constructor (type,
									    n_construct_properties,
									    construct_param);
	view = GTK_SOURCE_VIEW (object);

	set_source_buffer (view, gtk_text_view_get_buffer (GTK_TEXT_VIEW (view)));

	return object;
}

static void
gtk_source_view_finalize (GObject *object)
{
	GtkSourceView *view;

	g_return_if_fail (object != NULL);
	g_return_if_fail (GTK_IS_SOURCE_VIEW (object));

	view = GTK_SOURCE_VIEW (object);

	if (view->priv->style_scheme)
		g_object_unref (view->priv->style_scheme);

	if (view->priv->right_margin_line_color != NULL)
		gdk_color_free (view->priv->right_margin_line_color);

	if (view->priv->right_margin_overlay_color != NULL)
		gdk_color_free (view->priv->right_margin_overlay_color);

	if (view->priv->spaces_color != NULL)
		gdk_color_free (view->priv->spaces_color);

	if (view->priv->mark_categories)
		g_hash_table_destroy (view->priv->mark_categories);

	if (view->priv->left_gutter)
		g_object_unref (view->priv->left_gutter);

	if (view->priv->right_gutter)
		g_object_unref (view->priv->right_gutter);

	set_source_buffer (view, NULL);

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

	if (buffer && GTK_IS_SOURCE_BUFFER (buffer))
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
		gtk_source_view_update_style_scheme (view);
}

static void
gtk_source_view_undo (GtkSourceView *view)
{
	GtkTextBuffer *buffer;

	g_return_if_fail (GTK_IS_SOURCE_VIEW (view));

	buffer = gtk_text_view_get_buffer (GTK_TEXT_VIEW (view));

	if (gtk_text_view_get_editable (GTK_TEXT_VIEW (view)) &&
	    GTK_IS_SOURCE_BUFFER (buffer) &&
	    gtk_source_buffer_can_undo (GTK_SOURCE_BUFFER (buffer)))
	{
		gtk_source_buffer_undo (GTK_SOURCE_BUFFER (buffer));
		gtk_text_view_scroll_mark_onscreen (GTK_TEXT_VIEW (view),
						    gtk_text_buffer_get_insert (buffer));
	}
}

static void
gtk_source_view_redo (GtkSourceView *view)
{
	GtkTextBuffer *buffer;

	g_return_if_fail (GTK_IS_SOURCE_VIEW (view));

	buffer = gtk_text_view_get_buffer (GTK_TEXT_VIEW (view));

	if (gtk_text_view_get_editable (GTK_TEXT_VIEW (view)) &&
	    GTK_IS_SOURCE_BUFFER (buffer) &&
	    gtk_source_buffer_can_redo (GTK_SOURCE_BUFFER (buffer)))
	{
		gtk_source_buffer_redo (GTK_SOURCE_BUFFER (buffer));
		gtk_text_view_scroll_mark_onscreen (GTK_TEXT_VIEW (view),
						    gtk_text_buffer_get_insert (buffer));
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
	if (!GTK_IS_SOURCE_BUFFER (buffer))
		return;

	/* separator */
	menu_item = gtk_menu_item_new ();
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
	GtkTextBuffer *buffer = text_view->buffer;

	if (extend_selection)
		gtk_text_buffer_move_mark_by_name (buffer, "insert",
						   new_location);
	else
		gtk_text_buffer_place_cursor (buffer, new_location);

	gtk_text_view_scroll_mark_onscreen (text_view,
					    gtk_text_buffer_get_insert (buffer));
}

static void
move_to_first_char (GtkTextIter *iter)
{
	gtk_text_iter_set_line_offset (iter, 0);

	while (!gtk_text_iter_ends_line (iter))
	{
		gunichar c;

		c = gtk_text_iter_get_char (iter);
		if (g_unichar_isspace (c))
			gtk_text_iter_forward_char (iter);
		else
			break;
	}
}

static void
move_to_last_char (GtkTextIter *iter)
{
	if (!gtk_text_iter_ends_line (iter))
		gtk_text_iter_forward_to_line_end (iter);

	while (!gtk_text_iter_starts_line (iter))
	{
		gunichar c;

		gtk_text_iter_backward_char (iter);
		c = gtk_text_iter_get_char (iter);
		if (!g_unichar_isspace (c))
		{
			/* We've gone one character too far. */
			gtk_text_iter_forward_char (iter);
			break;
		}
	}
}

static void
do_cursor_move (GtkTextView *text_view,
	        GtkTextIter *cur,
	        GtkTextIter *iter,
	        gboolean     extend_selection)
{
	/* if we are clearing selection, we need to move_cursor even
	 * if we are at proper iter because selection_bound may need
	 * to be moved */
	if (!gtk_text_iter_equal (cur, iter) || !extend_selection)
	{
		move_cursor (text_view, iter, extend_selection);
		return;
	}
}

static void
gtk_source_view_move_cursor (GtkTextView    *text_view,
			     GtkMovementStep step,
			     gint            count,
			     gboolean        extend_selection)
{
	GtkSourceView *source_view = GTK_SOURCE_VIEW (text_view);
	GtkTextBuffer *buffer = text_view->buffer;
	GtkTextMark *mark;
	GtkTextIter cur, iter;

	mark = gtk_text_buffer_get_insert (buffer);
	gtk_text_buffer_get_iter_at_mark (buffer, &cur, mark);
	iter = cur;

	if (step == GTK_MOVEMENT_DISPLAY_LINE_ENDS && count == -1)
	{
		move_to_first_char (&iter);

		switch (source_view->priv->smart_home_end) {

		case GTK_SOURCE_SMART_HOME_END_BEFORE:
			if (!gtk_text_iter_equal (&cur, &iter) ||
			    gtk_text_iter_starts_line (&cur))
			{
				do_cursor_move (text_view, &cur, &iter, extend_selection);
				return;
			}
			break;

		case GTK_SOURCE_SMART_HOME_END_AFTER:
			if (gtk_text_iter_starts_line (&cur))
			{
				do_cursor_move (text_view, &cur, &iter, extend_selection);
				return;
			}
			break;

		case GTK_SOURCE_SMART_HOME_END_ALWAYS:
			do_cursor_move (text_view, &cur, &iter, extend_selection);
			return;

		default:
			break;
		}
	}
	else if (step == GTK_MOVEMENT_DISPLAY_LINE_ENDS && count == 1)
	{
		move_to_last_char (&iter);

		switch (source_view->priv->smart_home_end) {

		case GTK_SOURCE_SMART_HOME_END_BEFORE:
			if (!gtk_text_iter_equal (&cur, &iter) ||
			    gtk_text_iter_ends_line (&cur))
			{
				do_cursor_move (text_view, &cur, &iter, extend_selection);
				return;
			}
			break;

		case GTK_SOURCE_SMART_HOME_END_AFTER:
			if (gtk_text_iter_ends_line (&cur))
			{
				do_cursor_move (text_view, &cur, &iter, extend_selection);
				return;
			}
			break;

		case GTK_SOURCE_SMART_HOME_END_ALWAYS:
			do_cursor_move (text_view, &cur, &iter, extend_selection);
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
	gint size;
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
  	size = 0;

  	while (!gtk_text_iter_is_end (&iter))
    	{
		gint y, height;

		gtk_text_view_get_line_yrange (text_view, &iter, &y, &height);

		g_array_append_val (buffer_coords, y);
		if (line_heights)
			g_array_append_val (line_heights, height);
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
				       GdkEventExpose *event,
				       int             y, /* in buffer coordinates */
				       int             height,
				       const GdkColor *color)
{
	GdkRectangle visible_rect;
	GdkRectangle line_rect;
	gint win_y;
	gint margin;
	cairo_t *cr;

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

	if (text_view->hadjustment)
		margin = gtk_text_view_get_left_margin (text_view) -
			 (int) text_view->hadjustment->value;
	else
		margin = gtk_text_view_get_left_margin (text_view);

	line_rect.x += MAX (0, margin - 1);

	cr = gdk_cairo_create (event->window);
	gdk_cairo_set_source_color (cr, (GdkColor *)color);
	cairo_set_line_width (cr, 1);
	cairo_rectangle (cr, line_rect.x + .5, line_rect.y + .5,
			 line_rect.width - 1, line_rect.height - 1);
	cairo_stroke_preserve (cr);
	cairo_fill (cr);
	cairo_destroy (cr);
}

static void
gtk_source_view_paint_marks_background (GtkSourceView  *view,
					GdkEventExpose *event)
{
	GtkTextView *text_view;
	GArray *numbers;
	GArray *pixels;
	GArray *heights;
	gint y1, y2;
	gint count;
	gint i;

	if (view->priv->source_buffer == NULL)
		return;

	text_view = GTK_TEXT_VIEW (view);

	y1 = event->area.y;
	y2 = y1 + event->area.height;

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
		GdkColor *background;
		int priority;

		line_to_paint = g_array_index (numbers, gint, i);

		marks = gtk_source_buffer_get_source_marks_at_line (view->priv->source_buffer,
								    line_to_paint,
								    NULL);

		background = NULL;
		priority = -1;

		while (marks != NULL)
		{
			MarkCategory *cat = NULL;

			cat = gtk_source_view_get_mark_category (view, marks->data);

			if (cat != NULL && cat->background_set && cat->priority > priority)
			{
				background = &cat->background;
				priority = cat->priority;
			}

			marks = g_slist_delete_link (marks, marks);
		}

		if (background != NULL)
			gtk_source_view_paint_line_background (text_view, event,
							       g_array_index (pixels, gint, i),
							       g_array_index (heights, gint, i),
							       background);
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

	gtk_text_view_buffer_to_window_coords (view,
					       GTK_TEXT_WINDOW_TEXT,
					       rect.x + rect.width / 2,
					       rect.y + rect.height * 2 / 3,
					       &x,
					       &y);

	cairo_save (cr);
	cairo_move_to (cr, x, y);
	cairo_arc (cr, x, y, 0.8, 0, 2 * G_PI);
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

	w = rect.width;
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
		   GdkRectangle  rect)
{
	gint x, y;
	gdouble w, h;

	gtk_text_view_buffer_to_window_coords (view,
					       GTK_TEXT_WINDOW_TEXT,
					       rect.x,
					       rect.y + rect.height / 2,
					       &x,
					       &y);

	w = rect.width;
	h = rect.height;

	cairo_save (cr);
	cairo_move_to (cr, x + w * 1 / 6, y);
	cairo_rel_line_to (cr, w * 4 / 6, 0);
	cairo_rel_line_to (cr, -w * 2 / 6, +h * 1 / 4);
	cairo_rel_line_to (cr, -w * 2 / 6, -h * 1 / 4);
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
		draw_nbsp_at_iter (cr, GTK_TEXT_VIEW (view), iter, rect);
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

		if (!g_unichar_isspace (ch) ||
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
	gint location = view->priv->draw_spaces & (GTK_SOURCE_DRAW_SPACES_LEADING |
	                                           GTK_SOURCE_DRAW_SPACES_TEXT |
	                                           GTK_SOURCE_DRAW_SPACES_TRAILING);

	/* Draw all by default */
	if (!location)
	{
		return TRUE;
	}

	/* If leading > trailing we are in an empty line so we paint also
	   for leading spaces */
	if (gtk_text_iter_compare (iter, trailing) >= 0)
	{
		return location & (GTK_SOURCE_DRAW_SPACES_TRAILING |
				   GTK_SOURCE_DRAW_SPACES_LEADING);
	}

	if (gtk_text_iter_compare (iter, leading) < 0)
	{
		return location & GTK_SOURCE_DRAW_SPACES_LEADING;
	}

	return location & GTK_SOURCE_DRAW_SPACES_TEXT;
}
static void
draw_tabs_and_spaces (GtkSourceView  *view,
		      GdkEventExpose *event)
{
	GtkTextView *text_view;
	gint x1, y1, x2, y2;
	GtkTextIter s, e;
	cairo_t *cr;
	GtkTextIter leading, trailing;

	text_view = GTK_TEXT_VIEW (view);

	x1 = event->area.x;
	y1 = event->area.y;
	x2 = x1 + event->area.width;
	y2 = y1 + event->area.height;

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

	cr = gdk_cairo_create (event->window);

	cairo_set_source_rgba (cr,
			       view->priv->spaces_color->red / 65535.,
			       view->priv->spaces_color->green / 65535.,
			       view->priv->spaces_color->blue / 65535.,
			       1);
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
	cairo_destroy (cr);
}

static void
gtk_source_view_paint_right_margin (GtkSourceView  *view,
                                    GdkEventExpose *event)
{
	GdkRectangle visible_rect;
	GdkRectangle redraw_rect;
	cairo_t *cr;
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

	cr = gdk_cairo_create (gtk_text_view_get_window (text_view, GTK_TEXT_WINDOW_TEXT));

	/* Set a clip region for the expose event. */
	cairo_rectangle (cr, event->area.x, event->area.y,
			 event->area.width, event->area.height);
	cairo_clip (cr);

	/* Offset with 0.5 is needed for a sharp line. */
	x = view->priv->cached_right_margin_pos -
		visible_rect.x + redraw_rect.x + 0.5 +
		gtk_text_view_get_left_margin (text_view);

	/* Default line width is 2.0 which is too wide. */
	cairo_set_line_width (cr, 1.0);

	cairo_move_to (cr, x, redraw_rect.y);
	cairo_line_to (cr, x, redraw_rect.y + redraw_rect.height);

	cairo_set_source_rgba (cr,
			       view->priv->right_margin_line_color->red / 65535.,
			       view->priv->right_margin_line_color->green / 65535.,
			       view->priv->right_margin_line_color->blue / 65535.,
			       RIGHT_MARING_LINE_ALPHA / 255.);

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

		cairo_set_source_rgba (cr,
				       view->priv->right_margin_overlay_color->red / 65535.,
				       view->priv->right_margin_overlay_color->green / 65535.,
				       view->priv->right_margin_overlay_color->blue / 65535.,
				       RIGHT_MARING_OVERLAY_ALPHA / 255.);

		cairo_fill (cr);
	}

	cairo_destroy (cr);

	PROFILE ({
		g_timer_stop (timer);
		g_message ("Time to draw the margin: %g (sec * 1000)",
		           g_timer_elapsed (timer, NULL) * 1000);
	});
}

static gint
gtk_source_view_expose (GtkWidget      *widget,
			GdkEventExpose *event)
{
	GtkSourceView *view;
	GtkTextView *text_view;
	gboolean event_handled;

	DEBUG ({
		g_print ("> gtk_source_view_expose start\n");
	});

	view = GTK_SOURCE_VIEW (widget);
	text_view = GTK_TEXT_VIEW (widget);

	event_handled = FALSE;

	/* check if the expose event is for the text window first, and
	 * make sure the visible region is highlighted */
	if (event->window == gtk_text_view_get_window (text_view, GTK_TEXT_WINDOW_TEXT) &&
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
			g_print ("    exposed area: %d - %d\n", visible_rect.y,
				 visible_rect.y + visible_rect.height);
			g_print ("    lines to update: %d - %d\n",
				 gtk_text_iter_get_line (&iter1),
				 gtk_text_iter_get_line (&iter2));
		});

		_gtk_source_buffer_update_highlight (view->priv->source_buffer,
						     &iter1, &iter2, FALSE);
	}

	if (GTK_WIDGET_IS_SENSITIVE(view) && view->priv->highlight_current_line &&
		    (event->window == gtk_text_view_get_window (text_view, GTK_TEXT_WINDOW_TEXT)))
	{
		GtkTextIter cur;
		gint y, height;
		GdkColor *color;

		gtk_text_buffer_get_iter_at_mark (text_view->buffer,
						  &cur,
						  gtk_text_buffer_get_insert (text_view->buffer));
		gtk_text_view_get_line_yrange (text_view, &cur, &y, &height);

		if (view->priv->current_line_color_set)
			color = &view->priv->current_line_color;
		else
			color = &widget->style->bg[GTK_WIDGET_STATE (widget)];

		gtk_source_view_paint_line_background (text_view, event, y, height, color);
	}

	if (event->window == gtk_text_view_get_window (text_view, GTK_TEXT_WINDOW_TEXT))
		gtk_source_view_paint_marks_background (view, event);

	/* Have GtkTextView draw the text first. */
	if (GTK_WIDGET_CLASS (gtk_source_view_parent_class)->expose_event)
		event_handled =
			GTK_WIDGET_CLASS (gtk_source_view_parent_class)->expose_event (widget, event);

	/* Draw the right margin vertical line + overlay. */
	if (view->priv->show_right_margin &&
	    (event->window == gtk_text_view_get_window (text_view, GTK_TEXT_WINDOW_TEXT)))
	{
		gtk_source_view_paint_right_margin (view, event);
	}

	if (view->priv->draw_spaces != 0 &&
	    (event->window == gtk_text_view_get_window (text_view, GTK_TEXT_WINDOW_TEXT)))
	{
		draw_tabs_and_spaces (view, event);
	}

	DEBUG ({
		g_print ("> gtk_source_view_expose end\n");
	});

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
 * Return value: a new #GtkSourceView
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
 * Return value: a new #GtkTextView.
 **/
GtkWidget *
gtk_source_view_new_with_buffer (GtkSourceBuffer *buffer)
{
	GtkWidget *view;

	g_return_val_if_fail (buffer != NULL && GTK_IS_SOURCE_BUFFER (buffer), NULL);

	view = g_object_new (GTK_TYPE_SOURCE_VIEW, NULL);
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
	g_return_val_if_fail (GTK_IS_SOURCE_VIEW (view), FALSE);

	return (view->priv->show_line_numbers != FALSE);
}

/**
 * gtk_source_view_set_show_line_numbers:
 * @view: a #GtkSourceView.
 * @show: whether line numbers should be displayed.
 *
 * If %TRUE line numbers will be displayed beside the text.
 *
 **/
void
gtk_source_view_set_show_line_numbers (GtkSourceView *view,
				       gboolean       show)
{
	GtkSourceGutter *gutter;

	g_return_if_fail (view != NULL);
	g_return_if_fail (GTK_IS_SOURCE_VIEW (view));

	show = (show != FALSE);

	if (show == view->priv->show_line_numbers)
	{
		return;
	}

	if (show)
	{
		gtk_cell_renderer_set_fixed_size (view->priv->line_renderer, -1, -1);
	}
	else
	{
		gtk_cell_renderer_set_fixed_size (view->priv->line_renderer, 0, 0);
	}

	view->priv->show_line_numbers = show;

	gutter = gtk_source_view_get_gutter (view, GTK_TEXT_WINDOW_LEFT);
	gtk_source_gutter_queue_draw (gutter);

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
	g_return_val_if_fail (GTK_IS_SOURCE_VIEW (view), FALSE);

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
	GtkSourceGutter *gutter;

	g_return_if_fail (GTK_IS_SOURCE_VIEW (view));

	show = (show != FALSE);

	if (show == view->priv->show_line_marks)
	{
		return;
	}


	if (show)
	{
		gtk_cell_renderer_set_fixed_size (view->priv->marks_renderer, -1, -1);
	}
	else
	{
		gtk_cell_renderer_set_fixed_size (view->priv->marks_renderer, 0, 0);
	}

	view->priv->show_line_marks = show;

	gutter = gtk_source_view_get_gutter (view, GTK_TEXT_WINDOW_LEFT);
	gtk_source_gutter_queue_draw (gutter);

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

	gtk_widget_ensure_style (GTK_WIDGET (view));

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
	g_return_val_if_fail (GTK_IS_SOURCE_VIEW (view), DEFAULT_TAB_WIDTH);

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
	g_return_val_if_fail (view != NULL && GTK_IS_SOURCE_VIEW (view), 0);

	return view->priv->indent_width;
}

static void
mark_category_set_background (MarkCategory *cat, const GdkColor *background)
{
	if (background != NULL)
	{
		cat->background_set = TRUE;
		cat->background = *background;
	}
	else
	{
		cat->background_set = FALSE;
	}
}

static MarkCategory *
mark_category_new (gint priority)
{
	MarkCategory *cat;

	cat = g_slice_new0 (MarkCategory);
	cat->priority = priority;

	return cat;
}

static void
mark_category_free (MarkCategory *cat)
{
	if (cat->tooltip_data_notify != NULL)
	{
		cat->tooltip_data_notify (cat->tooltip_data);
	}
	
	if (cat->icon_pixbuf)
	{
		g_object_unref (cat->icon_pixbuf);
	}
	
	if (cat->cached_icon)
	{
		g_object_unref (cat->cached_icon);
	}
	
	g_free (cat->icon_stock);
	g_free (cat->icon_name);
	
	g_slice_free (MarkCategory, cat);
}

static MarkCategory *
gtk_source_view_get_mark_category (GtkSourceView *view,
				   GtkSourceMark *mark)
{
	const gchar *category;
	category = gtk_source_mark_get_category (mark);
	if (category)
		return g_hash_table_lookup (view->priv->mark_categories,
					    category);
	else
		return NULL;
}

static MarkCategory *
gtk_source_view_ensure_category (GtkSourceView *view,
				 const gchar   *name)
{
	MarkCategory *cat;

	cat = g_hash_table_lookup (view->priv->mark_categories, name);

	if (cat == NULL)
	{
		cat = mark_category_new (0);
		g_hash_table_insert (view->priv->mark_categories,
				     g_strdup (name),
				     cat);
	}

	return cat;
}

/**
 * gtk_source_view_set_mark_category_icon_from_pixbuf:
 * @view: a #GtkSourceView.
 * @category: a mark category.
 * @pixbuf: a #GdkPixbuf or #NULL.
 *
 * Sets the icon to be used for @category to @pixbuf.
 * If @pixbuf is #NULL, the icon is unset.
 *
 * Since: 2.8
 */
void
gtk_source_view_set_mark_category_icon_from_pixbuf (GtkSourceView  *view,
                                                    const gchar    *category,
                                                    GdkPixbuf      *pixbuf)
{
	MarkCategory *cat;

	g_return_if_fail (GTK_IS_SOURCE_VIEW (view));
	g_return_if_fail (category != NULL);
	g_return_if_fail (pixbuf == NULL || GDK_IS_PIXBUF (pixbuf));

	cat = gtk_source_view_ensure_category (view, category);

	if (cat->icon_pixbuf != NULL)
	{
		g_object_unref (cat->icon_pixbuf);
		cat->icon_pixbuf = NULL;
	}
	
	remove_cached_category_icon (cat);

	if (pixbuf != NULL)
	{
		cat->icon_pixbuf = g_object_ref (pixbuf);
	}
	
	cat->icon_type = ICON_TYPE_PIXBUF;

	/* We may need to redraw the margin now */
	gtk_widget_queue_draw (GTK_WIDGET (view));
}

/**
 * gtk_source_view_set_mark_category_icon_from_icon_name:
 * @view: a #GtkSourceView.
 * @category: a mark category.
 * @name: the themed icon name or #NULL.
 *
 * Sets the icon to be used for @category to the named theme item @name.
 * If @name is #NULL, the icon is unset.
 *
 * Since: 2.8
 */
void
gtk_source_view_set_mark_category_icon_from_icon_name (GtkSourceView  *view,
                                                       const gchar    *category,
                                                       const gchar    *name)
{
	MarkCategory *cat;

	g_return_if_fail (GTK_IS_SOURCE_VIEW (view));
	g_return_if_fail (category != NULL);

	cat = gtk_source_view_ensure_category (view, category);

	if (cat->icon_name != NULL)
	{
		g_free (cat->icon_name);
		cat->icon_name = NULL;
	}
	
	remove_cached_category_icon (cat);

	if (name != NULL)
	{
		cat->icon_name = g_strdup (name);
	}
	
	cat->icon_type = ICON_TYPE_NAME;

	/* We may need to redraw the margin now */
	gtk_widget_queue_draw (GTK_WIDGET (view));
}

/**
 * gtk_source_view_set_mark_category_icon_from_stock:
 * @view: a #GtkSourceView.
 * @category: a mark category.
 * @stock_id: the stock id or #NULL.
 *
 * Sets the icon to be used for @category to the stock item @stock_id.
 * If @stock_id is #NULL, the icon is unset.
 *
 * Since: 2.8
 */
void
gtk_source_view_set_mark_category_icon_from_stock (GtkSourceView  *view,
                                                   const gchar    *category,
                                                   const gchar    *stock_id)
{
	MarkCategory *cat;

	g_return_if_fail (GTK_IS_SOURCE_VIEW (view));
	g_return_if_fail (category != NULL);

	cat = gtk_source_view_ensure_category (view, category);

	if (cat->icon_stock != NULL)
	{
		g_free (cat->icon_stock);
		cat->icon_stock = NULL;
	}
	
	remove_cached_category_icon (cat);

	if (stock_id != NULL)
	{
		cat->icon_stock = g_strdup (stock_id);
	}
	
	cat->icon_type = ICON_TYPE_STOCK;

	/* We may need to redraw the margin now */
	gtk_widget_queue_draw (GTK_WIDGET (view));
}

/**
 * gtk_source_view_set_mark_category_pixbuf:
 * @view: a #GtkSourceView.
 * @category: a mark category.
 * @pixbuf: a #GdkPixbuf or #NULL.
 *
 * Associates a given @pixbuf with a given mark @category.
 * If @pixbuf is #NULL, the pixbuf is unset.
 *
 * Deprecated: Use #gtk_source_view_set_mark_category_icon_from_pixbuf instead
 *
 * Since: 2.2
 */
#ifndef GTKSOURCEVIEW_DISABLE_DEPRECATED
void
gtk_source_view_set_mark_category_pixbuf (GtkSourceView *view,
					  const gchar   *category,
					  GdkPixbuf     *pixbuf)
{
	gtk_source_view_set_mark_category_icon_from_pixbuf (view, category, pixbuf);
}
#endif

/**
 * gtk_source_view_get_mark_category_pixbuf:
 * @view: a #GtkSourceView.
 * @category: a mark category.
 *
 * Gets the pixbuf which is associated with the given mark @category.
 *
 * Return value: the associated #GdkPixbuf, or %NULL if not found.
 *
 * Since: 2.2
 */
#ifndef GTKSOURCEVIEW_DISABLE_DEPRECATED
GdkPixbuf *
gtk_source_view_get_mark_category_pixbuf (GtkSourceView *view,
					  const gchar   *category)
{
	MarkCategory *cat;

	g_return_val_if_fail (GTK_IS_SOURCE_VIEW (view), NULL);
	g_return_val_if_fail (category != NULL, NULL);

	cat = g_hash_table_lookup (view->priv->mark_categories, category);

	if (cat != NULL && cat->icon_pixbuf != NULL)
		return g_object_ref (cat->icon_pixbuf);
	else
		return NULL;
}
#endif

static void
set_mark_category_tooltip_func (GtkSourceView   *view,
				const gchar     *category,
				GtkSourceViewMarkTooltipFunc func,
				gpointer	 user_data,
				GDestroyNotify   user_data_notify,
				gboolean markup)
{
	MarkCategory *cat;

	g_return_if_fail (GTK_IS_SOURCE_VIEW (view));
	g_return_if_fail (category != NULL);

	cat = gtk_source_view_ensure_category (view, category);

	if (cat->tooltip_data_notify)
		cat->tooltip_data_notify (cat->tooltip_data);

	cat->tooltip_func = func;
	cat->tooltip_markup = markup;
	cat->tooltip_data = user_data;
	cat->tooltip_data_notify = user_data_notify;

	if (func != NULL)
	{
		gtk_widget_set_has_tooltip (GTK_WIDGET (view), TRUE);
		if (GTK_WIDGET_REALIZED (view))
		{
			gtk_widget_trigger_tooltip_query (GTK_WIDGET (view));
		}
	}
}

/**
 * gtk_source_view_set_mark_category_tooltip_func:
 * @view: a #GtkSourceView.
 * @category: a mark category.
 * @func: a #GtkSourceViewMarkTooltipFunc or %NULL.
 * @user_data: user data which will be passed to @func.
 * @user_data_notify:a function to free the memory allocated for @user_data 
 * or %NULL if you do not want to supply such a function.
 *
 * Set a #GtkSourceViewMarkTooltipFunc used to set tooltip on marks from the
 * given mark @category.
 * If you also specified a function with
 * gtk_source_view_set_mark_category_tooltip_markup_func()  the markup 
 * variant takes precedence.
 *
 * <informalexample><programlisting><![CDATA[
 * static gchar *
 * tooltip_func (GtkSourceMark *mark,
 *               gpointer       user_data)
 * {
 *   gchar *text;
 *
 *   text = get_tooltip_for_mark (mark, user_data);
 *
 *   return text;
 * }
 *
 * ...
 *
 * GtkSourceView *view;
 *
 * gtk_source_view_set_mark_category_tooltip_func (view, "other-mark",
 *                                                 tooltip_func,
 *                                                 NULL, NULL);
 * ]]></programlisting></informalexample>
 *
 * Since: 2.8
 */
void
gtk_source_view_set_mark_category_tooltip_func (GtkSourceView   *view,
						const gchar     *category,
						GtkSourceViewMarkTooltipFunc func,
						gpointer	 user_data,
						GDestroyNotify   user_data_notify)
{
	set_mark_category_tooltip_func (view, category, func, user_data,
					user_data_notify, FALSE);
}

/**
 * gtk_source_view_set_mark_category_tooltip_markup_func:
 * @view: a #GtkSourceView.
 * @category: a mark category.
 * @markup_func: a #GtkSourceViewMarkTooltipFunc or %NULL.
 * @user_data: user data which will be passed to @func.
 * @user_data_notify:a function to free the memory allocated for @user_data 
 * or %NULL if you do not want to supply such a function.
 *
 * See gtk_source_view_set_mark_category_tooltip_func() for more information.
 *
 * Since: 2.8
 */
void
gtk_source_view_set_mark_category_tooltip_markup_func (GtkSourceView   *view,
						       const gchar     *category,
						       GtkSourceViewMarkTooltipFunc markup_func,
						       gpointer         user_data,
						       GDestroyNotify   user_data_notify)
{
	set_mark_category_tooltip_func (view, category, markup_func, user_data,
					user_data_notify, TRUE);
}

/**
 * gtk_source_view_set_mark_category_background:
 * @view: a #GtkSourceView.
 * @category: a mark category.
 * @color: background color or %NULL to unset it.
 *
 * Sets given background @color for mark @category.
 * If @color is #NULL, the background color is unset.
 *
 * Since: 2.4
 */
void
gtk_source_view_set_mark_category_background (GtkSourceView  *view,
					      const gchar    *category,
					      const GdkColor *color)
{
	MarkCategory *cat;

	g_return_if_fail (GTK_IS_SOURCE_VIEW (view));
	g_return_if_fail (category != NULL);

	cat = gtk_source_view_ensure_category (view, category);
	mark_category_set_background (cat, color);

	/* We may need to redraw the text background now */
	gtk_widget_queue_draw (GTK_WIDGET (view));
}

/**
 * gtk_source_view_get_mark_category_background:
 * @view: a #GtkSourceView.
 * @category: a mark category.
 * @dest: destination #GdkColor structure to fill in.
 *
 * Gets the background color associated with given @category.
 *
 * Return value: %TRUE if background color for @category was set
 * and @dest is set to a valid color, or %FALSE otherwise.
 *
 * Since: 2.4
 */
gboolean
gtk_source_view_get_mark_category_background (GtkSourceView *view,
					      const gchar   *category,
					      GdkColor      *dest)
{
	MarkCategory *cat;

	g_return_val_if_fail (GTK_IS_SOURCE_VIEW (view), FALSE);
	g_return_val_if_fail (category != NULL, FALSE);
	g_return_val_if_fail (dest != NULL, FALSE);

	cat = g_hash_table_lookup (view->priv->mark_categories, category);
	if (cat != NULL && cat->background_set)
	{
		*dest = cat->background;
		return TRUE;
	}
	else
	{
		return FALSE;
	}
}

/**
 * gtk_source_view_set_mark_category_priority:
 * @view: a #GtkSourceView.
 * @category: a mark category.
 * @priority: the priority for the category
 *
 * Set the @priority for the given mark @category. When there are
 * multiple marks on the same line, marks of categories with
 * higher priorities will be drawn on top.
 *
 * Since: 2.2
 */
void
gtk_source_view_set_mark_category_priority (GtkSourceView *view,
					    const gchar   *category,
					    gint          priority)
{
	MarkCategory *cat;

	g_return_if_fail (GTK_IS_SOURCE_VIEW (view));
	g_return_if_fail (category != NULL);

	cat = gtk_source_view_ensure_category (view, category);
	cat->priority = priority;

	/* We may need to redraw now, if the priorities affect appearance */
	gtk_widget_queue_draw (GTK_WIDGET (view));
}

/**
 * gtk_source_view_get_mark_category_priority:
 * @view: a #GtkSourceView.
 * @category: a mark category.
 *
 * Gets the priority which is associated with the given @category.
 *
 * Return value: the priority or if @category
 * exists but no priority was set, it defaults to 0.
 *
 * Since: 2.2
 */
gint
gtk_source_view_get_mark_category_priority (GtkSourceView *view,
					    const gchar   *category)
{
	MarkCategory *cat;

	g_return_val_if_fail (GTK_IS_SOURCE_VIEW (view), 0);
	g_return_val_if_fail (category != NULL, 0);

	cat = g_hash_table_lookup (view->priv->mark_categories, category);
	if (cat != NULL)
		return cat->priority;
	else
	{
		g_warning ("Marker Category %s does not exist!", category);
		return 0;
	}
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

static void
remove_previous_line_if_empty (GtkSourceView *view,
			       GtkTextIter   *iter)
{
	GtkTextBuffer *buf;
	GtkTextIter start;
	gunichar c;
	
	start = *iter;
	
	while (TRUE)
	{
		gtk_text_iter_backward_char (&start);
		
		if (gtk_text_iter_starts_line (&start))
			break;
	
		c = gtk_text_iter_get_char (&start);
		
		if (!g_unichar_isspace (c))
			return;
	}
	
	buf = gtk_text_view_get_buffer (GTK_TEXT_VIEW (view));
	
	gtk_text_buffer_delete (buf, &start, iter);
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

	if ((key == GDK_Return || key == GDK_KP_Enter) &&
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
			if (gtk_im_context_filter_keypress (GTK_TEXT_VIEW(view)->im_context, event))
				return TRUE;

			/* If an input method has inserted some text while handling the key press event,
			 * the cur iterm may be invalid, so get the iter again */
			gtk_text_buffer_get_iter_at_mark (buf, &cur, mark);

			/* Insert new line and auto-indent. */
			gtk_text_buffer_begin_user_action (buf);
			remove_previous_line_if_empty (view, &cur);
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
	if ((key == GDK_Tab || key == GDK_KP_Tab || key == GDK_ISO_Left_Tab) &&
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
	g_return_val_if_fail (GTK_IS_SOURCE_VIEW (view), FALSE);

	return (view->priv->auto_indent != FALSE);
}

/**
 * gtk_source_view_set_auto_indent:
 * @view: a #GtkSourceView.
 * @enable: whether to enable auto indentation.
 *
 * If %TRUE auto indentation of text is enabled.
 *
 **/
void
gtk_source_view_set_auto_indent (GtkSourceView *view, gboolean enable)
{
	g_return_if_fail (GTK_IS_SOURCE_VIEW (view));

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
	g_return_val_if_fail (GTK_IS_SOURCE_VIEW (view), FALSE);

	return (view->priv->insert_spaces != FALSE);
}

/**
 * gtk_source_view_set_insert_spaces_instead_of_tabs:
 * @view: a #GtkSourceView.
 * @enable: whether to insert spaces instead of tabs.
 *
 * If %TRUE any tabulator character inserted is replaced by a group
 * of space characters.
 *
 **/
void
gtk_source_view_set_insert_spaces_instead_of_tabs (GtkSourceView *view, gboolean enable)
{
	g_return_if_fail (GTK_IS_SOURCE_VIEW (view));

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
	g_return_val_if_fail (GTK_IS_SOURCE_VIEW (view), FALSE);

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
	g_return_if_fail (GTK_IS_SOURCE_VIEW (view));

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

		if (selection_data->length < 0)
			return;

		if ((selection_data->format != 16) || (selection_data->length != 8))
		{
			g_warning ("Received invalid color data\n");
			return;
		}

		vals = (guint16 *) selection_data->data;

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
 * @view: a #GtkSourceView
 *
 * Returns whether the current line is highlighted
 *
 * Return value: %TRUE if the current line is highlighted.
 **/
gboolean
gtk_source_view_get_highlight_current_line (GtkSourceView *view)
{
	g_return_val_if_fail (GTK_IS_SOURCE_VIEW (view), FALSE);

	return (view->priv->highlight_current_line != FALSE);
}

/**
 * gtk_source_view_set_highlight_current_line:
 * @view: a #GtkSourceView
 * @show: whether to highlight the current line
 *
 * If @show is %TRUE the current line is highlighted.
 **/
void
gtk_source_view_set_highlight_current_line (GtkSourceView *view, gboolean hl)
{
	g_return_if_fail (GTK_IS_SOURCE_VIEW (view));

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
	g_return_val_if_fail (GTK_IS_SOURCE_VIEW (view), FALSE);

	return (view->priv->show_right_margin != FALSE);
}

/**
 * gtk_source_view_set_show_right_margin:
 * @view: a #GtkSourceView.
 * @show: whether to show a right margin.
 *
 * If %TRUE a right margin is displayed
 **/
void
gtk_source_view_set_show_right_margin (GtkSourceView *view, gboolean show)
{
	g_return_if_fail (GTK_IS_SOURCE_VIEW (view));

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
	g_return_val_if_fail (GTK_IS_SOURCE_VIEW (view),
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
	g_return_if_fail (GTK_IS_SOURCE_VIEW (view));
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
	g_return_if_fail (GTK_IS_SOURCE_VIEW (view));

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
	g_return_val_if_fail (GTK_IS_SOURCE_VIEW (view), FALSE);

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
	g_return_if_fail (GTK_IS_SOURCE_VIEW (view));

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
	g_return_val_if_fail (GTK_IS_SOURCE_VIEW (view), 0);

	return view->priv->draw_spaces;
}

static void
gtk_source_view_style_set (GtkWidget *widget, GtkStyle *previous_style)
{
	GtkSourceView *view;

	g_return_if_fail (GTK_IS_SOURCE_VIEW (widget));

	/* call default handler first */
	if (GTK_WIDGET_CLASS (gtk_source_view_parent_class)->style_set)
		GTK_WIDGET_CLASS (gtk_source_view_parent_class)->style_set (widget, previous_style);

	view = GTK_SOURCE_VIEW (widget);
	if (previous_style)
	{
		/* If previous_style is NULL this is the initial
		 * emission and we can't set the tab array since the
		 * text view doesn't have a default style yet */

		/* re-set tab stops, but only if we already modified them, i.e.
		 * do nothing with good old 8-space tabs */
		if (view->priv->tabs_set)
			set_tab_stops_internal (view);

		/* make sure the margin position is recalculated on next expose */
		view->priv->cached_right_margin_pos = -1;
	}
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

	if (!GTK_WIDGET_REALIZED (view))
		return;

	if (view->priv->right_margin_line_color != NULL)
	{
		gdk_color_free (view->priv->right_margin_line_color);
		view->priv->right_margin_line_color = NULL;
	}

	if (view->priv->right_margin_overlay_color != NULL)
	{
		gdk_color_free (view->priv->right_margin_overlay_color);
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
			GdkColor color;

			g_object_get (style,
				      "foreground-set", &color_set,
				      "foreground", &color_str,
				      NULL);

			if (color_set && (color_str != NULL) && gdk_color_parse (color_str, &color))
			{
				view->priv->right_margin_line_color = gdk_color_copy (&color);
			}

			g_free (color_str);
			color_str = NULL;

			g_object_get (style,
				      "background-set", &color_set,
				      "background", &color_str,
				      NULL);

			if (color_set && (color_str != NULL) && gdk_color_parse (color_str, &color))
			{
				view->priv->right_margin_overlay_color = gdk_color_copy (&color);
			}

			g_free (color_str);
		}
	}

	if (view->priv->right_margin_line_color == NULL)
		view->priv->right_margin_line_color = gdk_color_copy (&widget->style->text[GTK_STATE_NORMAL]);
}

static void
update_spaces_color (GtkSourceView *view)
{
	GtkWidget *widget = GTK_WIDGET (view);

	if (!GTK_WIDGET_REALIZED (view))
		return;

	if (view->priv->spaces_color != NULL)
	{
		gdk_color_free (view->priv->spaces_color);
		view->priv->spaces_color = NULL;
	}

	if (view->priv->style_scheme)
	{
		GtkSourceStyle	*style;

		style = _gtk_source_style_scheme_get_draw_spaces_style (view->priv->style_scheme);

		if (style != NULL)
		{
			gchar *color_str = NULL;
			GdkColor color;

			g_object_get (style,
				      "foreground", &color_str,
				      NULL);

			if (color_str != NULL && gdk_color_parse (color_str, &color))
			{
				view->priv->spaces_color = gdk_color_copy (&color);
			}

			g_free (color_str);
		}
	}

	if (view->priv->spaces_color == NULL)
		view->priv->spaces_color = gdk_color_copy (&widget->style->text[GTK_STATE_INSENSITIVE]);
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
gtk_source_view_destroy (GtkObject *object)
{
	GtkSourceView *view = GTK_SOURCE_VIEW (object);

	if (!view->priv->destroy_has_run)
	{
		view->priv->destroy_has_run = TRUE;

		if (view->priv->completion != NULL)
		{
			g_object_unref (view->priv->completion);
			view->priv->completion = NULL;
		}
	}

	GTK_OBJECT_CLASS (gtk_source_view_parent_class)->destroy (object);
}

static void
gtk_source_view_update_style_scheme (GtkSourceView *view)
{
	GtkSourceStyleScheme *new_scheme;
	GtkTextBuffer *buffer;

	buffer = gtk_text_view_get_buffer (GTK_TEXT_VIEW (view));

	if (GTK_IS_SOURCE_BUFFER (buffer))
		new_scheme = gtk_source_buffer_get_style_scheme (GTK_SOURCE_BUFFER (buffer));
	else
		new_scheme = NULL;

	if (view->priv->style_scheme != new_scheme)
	{
		if (view->priv->style_scheme)
			g_object_unref (view->priv->style_scheme);
		view->priv->style_scheme = new_scheme;
		if (new_scheme)
			g_object_ref (new_scheme);

		if (GTK_WIDGET_REALIZED (view))
		{
			_gtk_source_style_scheme_apply (new_scheme, GTK_WIDGET (view));
			update_current_line_color (view);
			update_right_margin_colors (view);
			update_spaces_color (view);
			view->priv->style_scheme_applied = TRUE;
		}
		else
			view->priv->style_scheme_applied = FALSE;
	}
}
							 
/**
 * gtk_source_view_get_completion:
 * @view: a #GtkSourceView
 *
 * Gets the #GtkSourceCompletion associated with @view.
 *
 * Returns: the #GtkSourceCompletion associated with @view.
 */
GtkSourceCompletion *
gtk_source_view_get_completion (GtkSourceView *view)
{
	g_return_val_if_fail (GTK_IS_SOURCE_VIEW (view), NULL);
	
	if (view->priv->completion == NULL)
	{
		view->priv->completion = gtk_source_completion_new (view);
		g_object_ref_sink (view->priv->completion);
	}
	
	return view->priv->completion;
}

/**
 * gtk_source_view_get_gutter:
 * @view: a #GtkSourceView
 * @window_type: the gutter window type
 *
 * Returns the #GtkSourceGutter object associated with @window_type for @view.
 * Only GTK_TEXT_WINDOW_LEFT and GTK_TEXT_WINDOW_RIGHT are supported, 
 * respectively corresponding to the left and right gutter. The line numbers
 * and mark category icons are rendered in the gutter corresponding to
 * GTK_TEXT_WINDOW_LEFT.
 *
 * Since: 2.8
 *
 * Returns: the #GtkSourceGutter.
 **/
GtkSourceGutter *
gtk_source_view_get_gutter (GtkSourceView     *view,
                            GtkTextWindowType  window_type)
{
	g_return_val_if_fail (GTK_IS_SOURCE_VIEW (view), NULL);
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
