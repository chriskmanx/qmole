


#include <glib-object.h>
#include "gtksourceview-typebuiltins.h"


/* enumerations from "gtksourceiter.h" */
static const GFlagsValue _gtk_source_search_flags_values[] = {
  { GTK_SOURCE_SEARCH_VISIBLE_ONLY, "GTK_SOURCE_SEARCH_VISIBLE_ONLY", "visible-only" },
  { GTK_SOURCE_SEARCH_TEXT_ONLY, "GTK_SOURCE_SEARCH_TEXT_ONLY", "text-only" },
  { GTK_SOURCE_SEARCH_CASE_INSENSITIVE, "GTK_SOURCE_SEARCH_CASE_INSENSITIVE", "case-insensitive" },
  { 0, NULL, NULL }
};

GType
gtk_source_search_flags_get_type (void)
{
  static GType type = 0;

  if (!type)
    type = g_flags_register_static ("GtkSourceSearchFlags", _gtk_source_search_flags_values);

  return type;
}


/* enumerations from "gtksourceview.h" */
static const GEnumValue _gtk_source_view_gutter_position_values[] = {
  { GTK_SOURCE_VIEW_GUTTER_POSITION_LINES, "GTK_SOURCE_VIEW_GUTTER_POSITION_LINES", "lines" },
  { GTK_SOURCE_VIEW_GUTTER_POSITION_MARKS, "GTK_SOURCE_VIEW_GUTTER_POSITION_MARKS", "marks" },
  { 0, NULL, NULL }
};

GType
gtk_source_view_gutter_position_get_type (void)
{
  static GType type = 0;

  if (!type)
    type = g_enum_register_static ("GtkSourceViewGutterPosition", _gtk_source_view_gutter_position_values);

  return type;
}

static const GEnumValue _gtk_source_smart_home_end_type_values[] = {
  { GTK_SOURCE_SMART_HOME_END_DISABLED, "GTK_SOURCE_SMART_HOME_END_DISABLED", "disabled" },
  { GTK_SOURCE_SMART_HOME_END_BEFORE, "GTK_SOURCE_SMART_HOME_END_BEFORE", "before" },
  { GTK_SOURCE_SMART_HOME_END_AFTER, "GTK_SOURCE_SMART_HOME_END_AFTER", "after" },
  { GTK_SOURCE_SMART_HOME_END_ALWAYS, "GTK_SOURCE_SMART_HOME_END_ALWAYS", "always" },
  { 0, NULL, NULL }
};

GType
gtk_source_smart_home_end_type_get_type (void)
{
  static GType type = 0;

  if (!type)
    type = g_enum_register_static ("GtkSourceSmartHomeEndType", _gtk_source_smart_home_end_type_values);

  return type;
}

static const GFlagsValue _gtk_source_draw_spaces_flags_values[] = {
  { GTK_SOURCE_DRAW_SPACES_SPACE, "GTK_SOURCE_DRAW_SPACES_SPACE", "space" },
  { GTK_SOURCE_DRAW_SPACES_TAB, "GTK_SOURCE_DRAW_SPACES_TAB", "tab" },
  { GTK_SOURCE_DRAW_SPACES_NEWLINE, "GTK_SOURCE_DRAW_SPACES_NEWLINE", "newline" },
  { GTK_SOURCE_DRAW_SPACES_NBSP, "GTK_SOURCE_DRAW_SPACES_NBSP", "nbsp" },
  { GTK_SOURCE_DRAW_SPACES_LEADING, "GTK_SOURCE_DRAW_SPACES_LEADING", "leading" },
  { GTK_SOURCE_DRAW_SPACES_TEXT, "GTK_SOURCE_DRAW_SPACES_TEXT", "text" },
  { GTK_SOURCE_DRAW_SPACES_TRAILING, "GTK_SOURCE_DRAW_SPACES_TRAILING", "trailing" },
  { GTK_SOURCE_DRAW_SPACES_ALL, "GTK_SOURCE_DRAW_SPACES_ALL", "all" },
  { 0, NULL, NULL }
};

GType
gtk_source_draw_spaces_flags_get_type (void)
{
  static GType type = 0;

  if (!type)
    type = g_flags_register_static ("GtkSourceDrawSpacesFlags", _gtk_source_draw_spaces_flags_values);

  return type;
}


/* enumerations from "gtksourcecompletion.h" */
static const GEnumValue _gtk_source_completion_error_values[] = {
  { GTK_SOURCE_COMPLETION_ERROR_ALREADY_BOUND, "GTK_SOURCE_COMPLETION_ERROR_ALREADY_BOUND", "already-bound" },
  { GTK_SOURCE_COMPLETION_ERROR_NOT_BOUND, "GTK_SOURCE_COMPLETION_ERROR_NOT_BOUND", "not-bound" },
  { 0, NULL, NULL }
};

GType
gtk_source_completion_error_get_type (void)
{
  static GType type = 0;

  if (!type)
    type = g_enum_register_static ("GtkSourceCompletionError", _gtk_source_completion_error_values);

  return type;
}


/* enumerations from "gtksourcecompletioncontext.h" */
static const GFlagsValue _gtk_source_completion_activation_values[] = {
  { GTK_SOURCE_COMPLETION_ACTIVATION_NONE, "GTK_SOURCE_COMPLETION_ACTIVATION_NONE", "none" },
  { GTK_SOURCE_COMPLETION_ACTIVATION_INTERACTIVE, "GTK_SOURCE_COMPLETION_ACTIVATION_INTERACTIVE", "interactive" },
  { GTK_SOURCE_COMPLETION_ACTIVATION_USER_REQUESTED, "GTK_SOURCE_COMPLETION_ACTIVATION_USER_REQUESTED", "user-requested" },
  { 0, NULL, NULL }
};

GType
gtk_source_completion_activation_get_type (void)
{
  static GType type = 0;

  if (!type)
    type = g_flags_register_static ("GtkSourceCompletionActivation", _gtk_source_completion_activation_values);

  return type;
}




