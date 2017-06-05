
/* Generated data (by glib-mkenums) */

#include "gtk.h"
#include "gtkprivate.h"

/* enumerations from "gtkaboutdialog.h" */
GType
gtk_license_get_type (void)
{
    static GType etype = 0;
    if (G_UNLIKELY(etype == 0)) {
        static const GEnumValue values[] = {
            { GTK_LICENSE_UNKNOWN, "GTK_LICENSE_UNKNOWN", "unknown" },
            { GTK_LICENSE_CUSTOM, "GTK_LICENSE_CUSTOM", "custom" },
            { GTK_LICENSE_GPL_2_0, "GTK_LICENSE_GPL_2_0", "gpl-2-0" },
            { GTK_LICENSE_GPL_3_0, "GTK_LICENSE_GPL_3_0", "gpl-3-0" },
            { GTK_LICENSE_LGPL_2_1, "GTK_LICENSE_LGPL_2_1", "lgpl-2-1" },
            { GTK_LICENSE_LGPL_3_0, "GTK_LICENSE_LGPL_3_0", "lgpl-3-0" },
            { GTK_LICENSE_BSD, "GTK_LICENSE_BSD", "bsd" },
            { GTK_LICENSE_MIT_X11, "GTK_LICENSE_MIT_X11", "mit-x11" },
            { GTK_LICENSE_ARTISTIC, "GTK_LICENSE_ARTISTIC", "artistic" },
            { 0, NULL, NULL }
        };
        etype = g_enum_register_static (g_intern_static_string ("GtkLicense"), values);
    }
    return etype;
}

/* enumerations from "gtkaccelgroup.h" */
GType
gtk_accel_flags_get_type (void)
{
    static GType etype = 0;
    if (G_UNLIKELY(etype == 0)) {
        static const GFlagsValue values[] = {
            { GTK_ACCEL_VISIBLE, "GTK_ACCEL_VISIBLE", "visible" },
            { GTK_ACCEL_LOCKED, "GTK_ACCEL_LOCKED", "locked" },
            { GTK_ACCEL_MASK, "GTK_ACCEL_MASK", "mask" },
            { 0, NULL, NULL }
        };
        etype = g_flags_register_static (g_intern_static_string ("GtkAccelFlags"), values);
    }
    return etype;
}

/* enumerations from "gtkassistant.h" */
GType
gtk_assistant_page_type_get_type (void)
{
    static GType etype = 0;
    if (G_UNLIKELY(etype == 0)) {
        static const GEnumValue values[] = {
            { GTK_ASSISTANT_PAGE_CONTENT, "GTK_ASSISTANT_PAGE_CONTENT", "content" },
            { GTK_ASSISTANT_PAGE_INTRO, "GTK_ASSISTANT_PAGE_INTRO", "intro" },
            { GTK_ASSISTANT_PAGE_CONFIRM, "GTK_ASSISTANT_PAGE_CONFIRM", "confirm" },
            { GTK_ASSISTANT_PAGE_SUMMARY, "GTK_ASSISTANT_PAGE_SUMMARY", "summary" },
            { GTK_ASSISTANT_PAGE_PROGRESS, "GTK_ASSISTANT_PAGE_PROGRESS", "progress" },
            { GTK_ASSISTANT_PAGE_CUSTOM, "GTK_ASSISTANT_PAGE_CUSTOM", "custom" },
            { 0, NULL, NULL }
        };
        etype = g_enum_register_static (g_intern_static_string ("GtkAssistantPageType"), values);
    }
    return etype;
}

/* enumerations from "gtkbuilder.h" */
GType
gtk_builder_error_get_type (void)
{
    static GType etype = 0;
    if (G_UNLIKELY(etype == 0)) {
        static const GEnumValue values[] = {
            { GTK_BUILDER_ERROR_INVALID_TYPE_FUNCTION, "GTK_BUILDER_ERROR_INVALID_TYPE_FUNCTION", "invalid-type-function" },
            { GTK_BUILDER_ERROR_UNHANDLED_TAG, "GTK_BUILDER_ERROR_UNHANDLED_TAG", "unhandled-tag" },
            { GTK_BUILDER_ERROR_MISSING_ATTRIBUTE, "GTK_BUILDER_ERROR_MISSING_ATTRIBUTE", "missing-attribute" },
            { GTK_BUILDER_ERROR_INVALID_ATTRIBUTE, "GTK_BUILDER_ERROR_INVALID_ATTRIBUTE", "invalid-attribute" },
            { GTK_BUILDER_ERROR_INVALID_TAG, "GTK_BUILDER_ERROR_INVALID_TAG", "invalid-tag" },
            { GTK_BUILDER_ERROR_MISSING_PROPERTY_VALUE, "GTK_BUILDER_ERROR_MISSING_PROPERTY_VALUE", "missing-property-value" },
            { GTK_BUILDER_ERROR_INVALID_VALUE, "GTK_BUILDER_ERROR_INVALID_VALUE", "invalid-value" },
            { GTK_BUILDER_ERROR_VERSION_MISMATCH, "GTK_BUILDER_ERROR_VERSION_MISMATCH", "version-mismatch" },
            { GTK_BUILDER_ERROR_DUPLICATE_ID, "GTK_BUILDER_ERROR_DUPLICATE_ID", "duplicate-id" },
            { 0, NULL, NULL }
        };
        etype = g_enum_register_static (g_intern_static_string ("GtkBuilderError"), values);
    }
    return etype;
}

/* enumerations from "gtkcalendar.h" */
GType
gtk_calendar_display_options_get_type (void)
{
    static GType etype = 0;
    if (G_UNLIKELY(etype == 0)) {
        static const GFlagsValue values[] = {
            { GTK_CALENDAR_SHOW_HEADING, "GTK_CALENDAR_SHOW_HEADING", "show-heading" },
            { GTK_CALENDAR_SHOW_DAY_NAMES, "GTK_CALENDAR_SHOW_DAY_NAMES", "show-day-names" },
            { GTK_CALENDAR_NO_MONTH_CHANGE, "GTK_CALENDAR_NO_MONTH_CHANGE", "no-month-change" },
            { GTK_CALENDAR_SHOW_WEEK_NUMBERS, "GTK_CALENDAR_SHOW_WEEK_NUMBERS", "show-week-numbers" },
            { GTK_CALENDAR_SHOW_DETAILS, "GTK_CALENDAR_SHOW_DETAILS", "show-details" },
            { 0, NULL, NULL }
        };
        etype = g_flags_register_static (g_intern_static_string ("GtkCalendarDisplayOptions"), values);
    }
    return etype;
}

/* enumerations from "gtkcellrenderer.h" */
GType
gtk_cell_renderer_state_get_type (void)
{
    static GType etype = 0;
    if (G_UNLIKELY(etype == 0)) {
        static const GFlagsValue values[] = {
            { GTK_CELL_RENDERER_SELECTED, "GTK_CELL_RENDERER_SELECTED", "selected" },
            { GTK_CELL_RENDERER_PRELIT, "GTK_CELL_RENDERER_PRELIT", "prelit" },
            { GTK_CELL_RENDERER_INSENSITIVE, "GTK_CELL_RENDERER_INSENSITIVE", "insensitive" },
            { GTK_CELL_RENDERER_SORTED, "GTK_CELL_RENDERER_SORTED", "sorted" },
            { GTK_CELL_RENDERER_FOCUSED, "GTK_CELL_RENDERER_FOCUSED", "focused" },
            { 0, NULL, NULL }
        };
        etype = g_flags_register_static (g_intern_static_string ("GtkCellRendererState"), values);
    }
    return etype;
}

GType
gtk_cell_renderer_mode_get_type (void)
{
    static GType etype = 0;
    if (G_UNLIKELY(etype == 0)) {
        static const GEnumValue values[] = {
            { GTK_CELL_RENDERER_MODE_INERT, "GTK_CELL_RENDERER_MODE_INERT", "inert" },
            { GTK_CELL_RENDERER_MODE_ACTIVATABLE, "GTK_CELL_RENDERER_MODE_ACTIVATABLE", "activatable" },
            { GTK_CELL_RENDERER_MODE_EDITABLE, "GTK_CELL_RENDERER_MODE_EDITABLE", "editable" },
            { 0, NULL, NULL }
        };
        etype = g_enum_register_static (g_intern_static_string ("GtkCellRendererMode"), values);
    }
    return etype;
}

/* enumerations from "gtkcellrendereraccel.h" */
GType
gtk_cell_renderer_accel_mode_get_type (void)
{
    static GType etype = 0;
    if (G_UNLIKELY(etype == 0)) {
        static const GEnumValue values[] = {
            { GTK_CELL_RENDERER_ACCEL_MODE_GTK, "GTK_CELL_RENDERER_ACCEL_MODE_GTK", "gtk" },
            { GTK_CELL_RENDERER_ACCEL_MODE_OTHER, "GTK_CELL_RENDERER_ACCEL_MODE_OTHER", "other" },
            { 0, NULL, NULL }
        };
        etype = g_enum_register_static (g_intern_static_string ("GtkCellRendererAccelMode"), values);
    }
    return etype;
}

/* enumerations from "gtkcssprovider.h" */
GType
gtk_css_provider_error_get_type (void)
{
    static GType etype = 0;
    if (G_UNLIKELY(etype == 0)) {
        static const GEnumValue values[] = {
            { GTK_CSS_PROVIDER_ERROR_FAILED, "GTK_CSS_PROVIDER_ERROR_FAILED", "failed" },
            { GTK_CSS_PROVIDER_ERROR_SYNTAX, "GTK_CSS_PROVIDER_ERROR_SYNTAX", "syntax" },
            { GTK_CSS_PROVIDER_ERROR_IMPORT, "GTK_CSS_PROVIDER_ERROR_IMPORT", "import" },
            { GTK_CSS_PROVIDER_ERROR_NAME, "GTK_CSS_PROVIDER_ERROR_NAME", "name" },
            { GTK_CSS_PROVIDER_ERROR_DEPRECATED, "GTK_CSS_PROVIDER_ERROR_DEPRECATED", "deprecated" },
            { GTK_CSS_PROVIDER_ERROR_UNKNOWN_VALUE, "GTK_CSS_PROVIDER_ERROR_UNKNOWN_VALUE", "unknown-value" },
            { 0, NULL, NULL }
        };
        etype = g_enum_register_static (g_intern_static_string ("GtkCssProviderError"), values);
    }
    return etype;
}

/* enumerations from "gtkcsssection.h" */
GType
gtk_css_section_type_get_type (void)
{
    static GType etype = 0;
    if (G_UNLIKELY(etype == 0)) {
        static const GEnumValue values[] = {
            { GTK_CSS_SECTION_DOCUMENT, "GTK_CSS_SECTION_DOCUMENT", "document" },
            { GTK_CSS_SECTION_IMPORT, "GTK_CSS_SECTION_IMPORT", "import" },
            { GTK_CSS_SECTION_COLOR_DEFINITION, "GTK_CSS_SECTION_COLOR_DEFINITION", "color-definition" },
            { GTK_CSS_SECTION_BINDING_SET, "GTK_CSS_SECTION_BINDING_SET", "binding-set" },
            { GTK_CSS_SECTION_RULESET, "GTK_CSS_SECTION_RULESET", "ruleset" },
            { GTK_CSS_SECTION_SELECTOR, "GTK_CSS_SECTION_SELECTOR", "selector" },
            { GTK_CSS_SECTION_DECLARATION, "GTK_CSS_SECTION_DECLARATION", "declaration" },
            { GTK_CSS_SECTION_VALUE, "GTK_CSS_SECTION_VALUE", "value" },
            { 0, NULL, NULL }
        };
        etype = g_enum_register_static (g_intern_static_string ("GtkCssSectionType"), values);
    }
    return etype;
}

/* enumerations from "gtkdebug.h" */
GType
gtk_debug_flag_get_type (void)
{
    static GType etype = 0;
    if (G_UNLIKELY(etype == 0)) {
        static const GFlagsValue values[] = {
            { GTK_DEBUG_MISC, "GTK_DEBUG_MISC", "misc" },
            { GTK_DEBUG_PLUGSOCKET, "GTK_DEBUG_PLUGSOCKET", "plugsocket" },
            { GTK_DEBUG_TEXT, "GTK_DEBUG_TEXT", "text" },
            { GTK_DEBUG_TREE, "GTK_DEBUG_TREE", "tree" },
            { GTK_DEBUG_UPDATES, "GTK_DEBUG_UPDATES", "updates" },
            { GTK_DEBUG_KEYBINDINGS, "GTK_DEBUG_KEYBINDINGS", "keybindings" },
            { GTK_DEBUG_MULTIHEAD, "GTK_DEBUG_MULTIHEAD", "multihead" },
            { GTK_DEBUG_MODULES, "GTK_DEBUG_MODULES", "modules" },
            { GTK_DEBUG_GEOMETRY, "GTK_DEBUG_GEOMETRY", "geometry" },
            { GTK_DEBUG_ICONTHEME, "GTK_DEBUG_ICONTHEME", "icontheme" },
            { GTK_DEBUG_PRINTING, "GTK_DEBUG_PRINTING", "printing" },
            { GTK_DEBUG_BUILDER, "GTK_DEBUG_BUILDER", "builder" },
            { GTK_DEBUG_SIZE_REQUEST, "GTK_DEBUG_SIZE_REQUEST", "size-request" },
            { 0, NULL, NULL }
        };
        etype = g_flags_register_static (g_intern_static_string ("GtkDebugFlag"), values);
    }
    return etype;
}

/* enumerations from "gtkdialog.h" */
GType
gtk_dialog_flags_get_type (void)
{
    static GType etype = 0;
    if (G_UNLIKELY(etype == 0)) {
        static const GFlagsValue values[] = {
            { GTK_DIALOG_MODAL, "GTK_DIALOG_MODAL", "modal" },
            { GTK_DIALOG_DESTROY_WITH_PARENT, "GTK_DIALOG_DESTROY_WITH_PARENT", "destroy-with-parent" },
            { 0, NULL, NULL }
        };
        etype = g_flags_register_static (g_intern_static_string ("GtkDialogFlags"), values);
    }
    return etype;
}

GType
gtk_response_type_get_type (void)
{
    static GType etype = 0;
    if (G_UNLIKELY(etype == 0)) {
        static const GEnumValue values[] = {
            { GTK_RESPONSE_NONE, "GTK_RESPONSE_NONE", "none" },
            { GTK_RESPONSE_REJECT, "GTK_RESPONSE_REJECT", "reject" },
            { GTK_RESPONSE_ACCEPT, "GTK_RESPONSE_ACCEPT", "accept" },
            { GTK_RESPONSE_DELETE_EVENT, "GTK_RESPONSE_DELETE_EVENT", "delete-event" },
            { GTK_RESPONSE_OK, "GTK_RESPONSE_OK", "ok" },
            { GTK_RESPONSE_CANCEL, "GTK_RESPONSE_CANCEL", "cancel" },
            { GTK_RESPONSE_CLOSE, "GTK_RESPONSE_CLOSE", "close" },
            { GTK_RESPONSE_YES, "GTK_RESPONSE_YES", "yes" },
            { GTK_RESPONSE_NO, "GTK_RESPONSE_NO", "no" },
            { GTK_RESPONSE_APPLY, "GTK_RESPONSE_APPLY", "apply" },
            { GTK_RESPONSE_HELP, "GTK_RESPONSE_HELP", "help" },
            { 0, NULL, NULL }
        };
        etype = g_enum_register_static (g_intern_static_string ("GtkResponseType"), values);
    }
    return etype;
}

/* enumerations from "gtkdnd.h" */
GType
gtk_dest_defaults_get_type (void)
{
    static GType etype = 0;
    if (G_UNLIKELY(etype == 0)) {
        static const GFlagsValue values[] = {
            { GTK_DEST_DEFAULT_MOTION, "GTK_DEST_DEFAULT_MOTION", "motion" },
            { GTK_DEST_DEFAULT_HIGHLIGHT, "GTK_DEST_DEFAULT_HIGHLIGHT", "highlight" },
            { GTK_DEST_DEFAULT_DROP, "GTK_DEST_DEFAULT_DROP", "drop" },
            { GTK_DEST_DEFAULT_ALL, "GTK_DEST_DEFAULT_ALL", "all" },
            { 0, NULL, NULL }
        };
        etype = g_flags_register_static (g_intern_static_string ("GtkDestDefaults"), values);
    }
    return etype;
}

GType
gtk_target_flags_get_type (void)
{
    static GType etype = 0;
    if (G_UNLIKELY(etype == 0)) {
        static const GFlagsValue values[] = {
            { GTK_TARGET_SAME_APP, "GTK_TARGET_SAME_APP", "same-app" },
            { GTK_TARGET_SAME_WIDGET, "GTK_TARGET_SAME_WIDGET", "same-widget" },
            { GTK_TARGET_OTHER_APP, "GTK_TARGET_OTHER_APP", "other-app" },
            { GTK_TARGET_OTHER_WIDGET, "GTK_TARGET_OTHER_WIDGET", "other-widget" },
            { 0, NULL, NULL }
        };
        etype = g_flags_register_static (g_intern_static_string ("GtkTargetFlags"), values);
    }
    return etype;
}

/* enumerations from "gtkentry.h" */
GType
gtk_entry_icon_position_get_type (void)
{
    static GType etype = 0;
    if (G_UNLIKELY(etype == 0)) {
        static const GEnumValue values[] = {
            { GTK_ENTRY_ICON_PRIMARY, "GTK_ENTRY_ICON_PRIMARY", "primary" },
            { GTK_ENTRY_ICON_SECONDARY, "GTK_ENTRY_ICON_SECONDARY", "secondary" },
            { 0, NULL, NULL }
        };
        etype = g_enum_register_static (g_intern_static_string ("GtkEntryIconPosition"), values);
    }
    return etype;
}

/* enumerations from "gtkenums.h" */
GType
gtk_align_get_type (void)
{
    static GType etype = 0;
    if (G_UNLIKELY(etype == 0)) {
        static const GEnumValue values[] = {
            { GTK_ALIGN_FILL, "GTK_ALIGN_FILL", "fill" },
            { GTK_ALIGN_START, "GTK_ALIGN_START", "start" },
            { GTK_ALIGN_END, "GTK_ALIGN_END", "end" },
            { GTK_ALIGN_CENTER, "GTK_ALIGN_CENTER", "center" },
            { 0, NULL, NULL }
        };
        etype = g_enum_register_static (g_intern_static_string ("GtkAlign"), values);
    }
    return etype;
}

GType
gtk_arrow_placement_get_type (void)
{
    static GType etype = 0;
    if (G_UNLIKELY(etype == 0)) {
        static const GEnumValue values[] = {
            { GTK_ARROWS_BOTH, "GTK_ARROWS_BOTH", "both" },
            { GTK_ARROWS_START, "GTK_ARROWS_START", "start" },
            { GTK_ARROWS_END, "GTK_ARROWS_END", "end" },
            { 0, NULL, NULL }
        };
        etype = g_enum_register_static (g_intern_static_string ("GtkArrowPlacement"), values);
    }
    return etype;
}

GType
gtk_arrow_type_get_type (void)
{
    static GType etype = 0;
    if (G_UNLIKELY(etype == 0)) {
        static const GEnumValue values[] = {
            { GTK_ARROW_UP, "GTK_ARROW_UP", "up" },
            { GTK_ARROW_DOWN, "GTK_ARROW_DOWN", "down" },
            { GTK_ARROW_LEFT, "GTK_ARROW_LEFT", "left" },
            { GTK_ARROW_RIGHT, "GTK_ARROW_RIGHT", "right" },
            { GTK_ARROW_NONE, "GTK_ARROW_NONE", "none" },
            { 0, NULL, NULL }
        };
        etype = g_enum_register_static (g_intern_static_string ("GtkArrowType"), values);
    }
    return etype;
}

GType
gtk_attach_options_get_type (void)
{
    static GType etype = 0;
    if (G_UNLIKELY(etype == 0)) {
        static const GFlagsValue values[] = {
            { GTK_EXPAND, "GTK_EXPAND", "expand" },
            { GTK_SHRINK, "GTK_SHRINK", "shrink" },
            { GTK_FILL, "GTK_FILL", "fill" },
            { 0, NULL, NULL }
        };
        etype = g_flags_register_static (g_intern_static_string ("GtkAttachOptions"), values);
    }
    return etype;
}

GType
gtk_button_box_style_get_type (void)
{
    static GType etype = 0;
    if (G_UNLIKELY(etype == 0)) {
        static const GEnumValue values[] = {
            { GTK_BUTTONBOX_SPREAD, "GTK_BUTTONBOX_SPREAD", "spread" },
            { GTK_BUTTONBOX_EDGE, "GTK_BUTTONBOX_EDGE", "edge" },
            { GTK_BUTTONBOX_START, "GTK_BUTTONBOX_START", "start" },
            { GTK_BUTTONBOX_END, "GTK_BUTTONBOX_END", "end" },
            { GTK_BUTTONBOX_CENTER, "GTK_BUTTONBOX_CENTER", "center" },
            { 0, NULL, NULL }
        };
        etype = g_enum_register_static (g_intern_static_string ("GtkButtonBoxStyle"), values);
    }
    return etype;
}

GType
gtk_delete_type_get_type (void)
{
    static GType etype = 0;
    if (G_UNLIKELY(etype == 0)) {
        static const GEnumValue values[] = {
            { GTK_DELETE_CHARS, "GTK_DELETE_CHARS", "chars" },
            { GTK_DELETE_WORD_ENDS, "GTK_DELETE_WORD_ENDS", "word-ends" },
            { GTK_DELETE_WORDS, "GTK_DELETE_WORDS", "words" },
            { GTK_DELETE_DISPLAY_LINES, "GTK_DELETE_DISPLAY_LINES", "display-lines" },
            { GTK_DELETE_DISPLAY_LINE_ENDS, "GTK_DELETE_DISPLAY_LINE_ENDS", "display-line-ends" },
            { GTK_DELETE_PARAGRAPH_ENDS, "GTK_DELETE_PARAGRAPH_ENDS", "paragraph-ends" },
            { GTK_DELETE_PARAGRAPHS, "GTK_DELETE_PARAGRAPHS", "paragraphs" },
            { GTK_DELETE_WHITESPACE, "GTK_DELETE_WHITESPACE", "whitespace" },
            { 0, NULL, NULL }
        };
        etype = g_enum_register_static (g_intern_static_string ("GtkDeleteType"), values);
    }
    return etype;
}

GType
gtk_direction_type_get_type (void)
{
    static GType etype = 0;
    if (G_UNLIKELY(etype == 0)) {
        static const GEnumValue values[] = {
            { GTK_DIR_TAB_FORWARD, "GTK_DIR_TAB_FORWARD", "tab-forward" },
            { GTK_DIR_TAB_BACKWARD, "GTK_DIR_TAB_BACKWARD", "tab-backward" },
            { GTK_DIR_UP, "GTK_DIR_UP", "up" },
            { GTK_DIR_DOWN, "GTK_DIR_DOWN", "down" },
            { GTK_DIR_LEFT, "GTK_DIR_LEFT", "left" },
            { GTK_DIR_RIGHT, "GTK_DIR_RIGHT", "right" },
            { 0, NULL, NULL }
        };
        etype = g_enum_register_static (g_intern_static_string ("GtkDirectionType"), values);
    }
    return etype;
}

GType
gtk_expander_style_get_type (void)
{
    static GType etype = 0;
    if (G_UNLIKELY(etype == 0)) {
        static const GEnumValue values[] = {
            { GTK_EXPANDER_COLLAPSED, "GTK_EXPANDER_COLLAPSED", "collapsed" },
            { GTK_EXPANDER_SEMI_COLLAPSED, "GTK_EXPANDER_SEMI_COLLAPSED", "semi-collapsed" },
            { GTK_EXPANDER_SEMI_EXPANDED, "GTK_EXPANDER_SEMI_EXPANDED", "semi-expanded" },
            { GTK_EXPANDER_EXPANDED, "GTK_EXPANDER_EXPANDED", "expanded" },
            { 0, NULL, NULL }
        };
        etype = g_enum_register_static (g_intern_static_string ("GtkExpanderStyle"), values);
    }
    return etype;
}

GType
gtk_icon_size_get_type (void)
{
    static GType etype = 0;
    if (G_UNLIKELY(etype == 0)) {
        static const GEnumValue values[] = {
            { GTK_ICON_SIZE_INVALID, "GTK_ICON_SIZE_INVALID", "invalid" },
            { GTK_ICON_SIZE_MENU, "GTK_ICON_SIZE_MENU", "menu" },
            { GTK_ICON_SIZE_SMALL_TOOLBAR, "GTK_ICON_SIZE_SMALL_TOOLBAR", "small-toolbar" },
            { GTK_ICON_SIZE_LARGE_TOOLBAR, "GTK_ICON_SIZE_LARGE_TOOLBAR", "large-toolbar" },
            { GTK_ICON_SIZE_BUTTON, "GTK_ICON_SIZE_BUTTON", "button" },
            { GTK_ICON_SIZE_DND, "GTK_ICON_SIZE_DND", "dnd" },
            { GTK_ICON_SIZE_DIALOG, "GTK_ICON_SIZE_DIALOG", "dialog" },
            { 0, NULL, NULL }
        };
        etype = g_enum_register_static (g_intern_static_string ("GtkIconSize"), values);
    }
    return etype;
}

GType
gtk_sensitivity_type_get_type (void)
{
    static GType etype = 0;
    if (G_UNLIKELY(etype == 0)) {
        static const GEnumValue values[] = {
            { GTK_SENSITIVITY_AUTO, "GTK_SENSITIVITY_AUTO", "auto" },
            { GTK_SENSITIVITY_ON, "GTK_SENSITIVITY_ON", "on" },
            { GTK_SENSITIVITY_OFF, "GTK_SENSITIVITY_OFF", "off" },
            { 0, NULL, NULL }
        };
        etype = g_enum_register_static (g_intern_static_string ("GtkSensitivityType"), values);
    }
    return etype;
}

GType
gtk_text_direction_get_type (void)
{
    static GType etype = 0;
    if (G_UNLIKELY(etype == 0)) {
        static const GEnumValue values[] = {
            { GTK_TEXT_DIR_NONE, "GTK_TEXT_DIR_NONE", "none" },
            { GTK_TEXT_DIR_LTR, "GTK_TEXT_DIR_LTR", "ltr" },
            { GTK_TEXT_DIR_RTL, "GTK_TEXT_DIR_RTL", "rtl" },
            { 0, NULL, NULL }
        };
        etype = g_enum_register_static (g_intern_static_string ("GtkTextDirection"), values);
    }
    return etype;
}

GType
gtk_justification_get_type (void)
{
    static GType etype = 0;
    if (G_UNLIKELY(etype == 0)) {
        static const GEnumValue values[] = {
            { GTK_JUSTIFY_LEFT, "GTK_JUSTIFY_LEFT", "left" },
            { GTK_JUSTIFY_RIGHT, "GTK_JUSTIFY_RIGHT", "right" },
            { GTK_JUSTIFY_CENTER, "GTK_JUSTIFY_CENTER", "center" },
            { GTK_JUSTIFY_FILL, "GTK_JUSTIFY_FILL", "fill" },
            { 0, NULL, NULL }
        };
        etype = g_enum_register_static (g_intern_static_string ("GtkJustification"), values);
    }
    return etype;
}

GType
gtk_menu_direction_type_get_type (void)
{
    static GType etype = 0;
    if (G_UNLIKELY(etype == 0)) {
        static const GEnumValue values[] = {
            { GTK_MENU_DIR_PARENT, "GTK_MENU_DIR_PARENT", "parent" },
            { GTK_MENU_DIR_CHILD, "GTK_MENU_DIR_CHILD", "child" },
            { GTK_MENU_DIR_NEXT, "GTK_MENU_DIR_NEXT", "next" },
            { GTK_MENU_DIR_PREV, "GTK_MENU_DIR_PREV", "prev" },
            { 0, NULL, NULL }
        };
        etype = g_enum_register_static (g_intern_static_string ("GtkMenuDirectionType"), values);
    }
    return etype;
}

GType
gtk_message_type_get_type (void)
{
    static GType etype = 0;
    if (G_UNLIKELY(etype == 0)) {
        static const GEnumValue values[] = {
            { GTK_MESSAGE_INFO, "GTK_MESSAGE_INFO", "info" },
            { GTK_MESSAGE_WARNING, "GTK_MESSAGE_WARNING", "warning" },
            { GTK_MESSAGE_QUESTION, "GTK_MESSAGE_QUESTION", "question" },
            { GTK_MESSAGE_ERROR, "GTK_MESSAGE_ERROR", "error" },
            { GTK_MESSAGE_OTHER, "GTK_MESSAGE_OTHER", "other" },
            { 0, NULL, NULL }
        };
        etype = g_enum_register_static (g_intern_static_string ("GtkMessageType"), values);
    }
    return etype;
}

GType
gtk_movement_step_get_type (void)
{
    static GType etype = 0;
    if (G_UNLIKELY(etype == 0)) {
        static const GEnumValue values[] = {
            { GTK_MOVEMENT_LOGICAL_POSITIONS, "GTK_MOVEMENT_LOGICAL_POSITIONS", "logical-positions" },
            { GTK_MOVEMENT_VISUAL_POSITIONS, "GTK_MOVEMENT_VISUAL_POSITIONS", "visual-positions" },
            { GTK_MOVEMENT_WORDS, "GTK_MOVEMENT_WORDS", "words" },
            { GTK_MOVEMENT_DISPLAY_LINES, "GTK_MOVEMENT_DISPLAY_LINES", "display-lines" },
            { GTK_MOVEMENT_DISPLAY_LINE_ENDS, "GTK_MOVEMENT_DISPLAY_LINE_ENDS", "display-line-ends" },
            { GTK_MOVEMENT_PARAGRAPHS, "GTK_MOVEMENT_PARAGRAPHS", "paragraphs" },
            { GTK_MOVEMENT_PARAGRAPH_ENDS, "GTK_MOVEMENT_PARAGRAPH_ENDS", "paragraph-ends" },
            { GTK_MOVEMENT_PAGES, "GTK_MOVEMENT_PAGES", "pages" },
            { GTK_MOVEMENT_BUFFER_ENDS, "GTK_MOVEMENT_BUFFER_ENDS", "buffer-ends" },
            { GTK_MOVEMENT_HORIZONTAL_PAGES, "GTK_MOVEMENT_HORIZONTAL_PAGES", "horizontal-pages" },
            { 0, NULL, NULL }
        };
        etype = g_enum_register_static (g_intern_static_string ("GtkMovementStep"), values);
    }
    return etype;
}

GType
gtk_scroll_step_get_type (void)
{
    static GType etype = 0;
    if (G_UNLIKELY(etype == 0)) {
        static const GEnumValue values[] = {
            { GTK_SCROLL_STEPS, "GTK_SCROLL_STEPS", "steps" },
            { GTK_SCROLL_PAGES, "GTK_SCROLL_PAGES", "pages" },
            { GTK_SCROLL_ENDS, "GTK_SCROLL_ENDS", "ends" },
            { GTK_SCROLL_HORIZONTAL_STEPS, "GTK_SCROLL_HORIZONTAL_STEPS", "horizontal-steps" },
            { GTK_SCROLL_HORIZONTAL_PAGES, "GTK_SCROLL_HORIZONTAL_PAGES", "horizontal-pages" },
            { GTK_SCROLL_HORIZONTAL_ENDS, "GTK_SCROLL_HORIZONTAL_ENDS", "horizontal-ends" },
            { 0, NULL, NULL }
        };
        etype = g_enum_register_static (g_intern_static_string ("GtkScrollStep"), values);
    }
    return etype;
}

GType
gtk_orientation_get_type (void)
{
    static GType etype = 0;
    if (G_UNLIKELY(etype == 0)) {
        static const GEnumValue values[] = {
            { GTK_ORIENTATION_HORIZONTAL, "GTK_ORIENTATION_HORIZONTAL", "horizontal" },
            { GTK_ORIENTATION_VERTICAL, "GTK_ORIENTATION_VERTICAL", "vertical" },
            { 0, NULL, NULL }
        };
        etype = g_enum_register_static (g_intern_static_string ("GtkOrientation"), values);
    }
    return etype;
}

GType
gtk_corner_type_get_type (void)
{
    static GType etype = 0;
    if (G_UNLIKELY(etype == 0)) {
        static const GEnumValue values[] = {
            { GTK_CORNER_TOP_LEFT, "GTK_CORNER_TOP_LEFT", "top-left" },
            { GTK_CORNER_BOTTOM_LEFT, "GTK_CORNER_BOTTOM_LEFT", "bottom-left" },
            { GTK_CORNER_TOP_RIGHT, "GTK_CORNER_TOP_RIGHT", "top-right" },
            { GTK_CORNER_BOTTOM_RIGHT, "GTK_CORNER_BOTTOM_RIGHT", "bottom-right" },
            { 0, NULL, NULL }
        };
        etype = g_enum_register_static (g_intern_static_string ("GtkCornerType"), values);
    }
    return etype;
}

GType
gtk_pack_type_get_type (void)
{
    static GType etype = 0;
    if (G_UNLIKELY(etype == 0)) {
        static const GEnumValue values[] = {
            { GTK_PACK_START, "GTK_PACK_START", "start" },
            { GTK_PACK_END, "GTK_PACK_END", "end" },
            { 0, NULL, NULL }
        };
        etype = g_enum_register_static (g_intern_static_string ("GtkPackType"), values);
    }
    return etype;
}

GType
gtk_path_priority_type_get_type (void)
{
    static GType etype = 0;
    if (G_UNLIKELY(etype == 0)) {
        static const GEnumValue values[] = {
            { GTK_PATH_PRIO_LOWEST, "GTK_PATH_PRIO_LOWEST", "lowest" },
            { GTK_PATH_PRIO_GTK, "GTK_PATH_PRIO_GTK", "gtk" },
            { GTK_PATH_PRIO_APPLICATION, "GTK_PATH_PRIO_APPLICATION", "application" },
            { GTK_PATH_PRIO_THEME, "GTK_PATH_PRIO_THEME", "theme" },
            { GTK_PATH_PRIO_RC, "GTK_PATH_PRIO_RC", "rc" },
            { GTK_PATH_PRIO_HIGHEST, "GTK_PATH_PRIO_HIGHEST", "highest" },
            { 0, NULL, NULL }
        };
        etype = g_enum_register_static (g_intern_static_string ("GtkPathPriorityType"), values);
    }
    return etype;
}

GType
gtk_path_type_get_type (void)
{
    static GType etype = 0;
    if (G_UNLIKELY(etype == 0)) {
        static const GEnumValue values[] = {
            { GTK_PATH_WIDGET, "GTK_PATH_WIDGET", "widget" },
            { GTK_PATH_WIDGET_CLASS, "GTK_PATH_WIDGET_CLASS", "widget-class" },
            { GTK_PATH_CLASS, "GTK_PATH_CLASS", "class" },
            { 0, NULL, NULL }
        };
        etype = g_enum_register_static (g_intern_static_string ("GtkPathType"), values);
    }
    return etype;
}

GType
gtk_policy_type_get_type (void)
{
    static GType etype = 0;
    if (G_UNLIKELY(etype == 0)) {
        static const GEnumValue values[] = {
            { GTK_POLICY_ALWAYS, "GTK_POLICY_ALWAYS", "always" },
            { GTK_POLICY_AUTOMATIC, "GTK_POLICY_AUTOMATIC", "automatic" },
            { GTK_POLICY_NEVER, "GTK_POLICY_NEVER", "never" },
            { 0, NULL, NULL }
        };
        etype = g_enum_register_static (g_intern_static_string ("GtkPolicyType"), values);
    }
    return etype;
}

GType
gtk_position_type_get_type (void)
{
    static GType etype = 0;
    if (G_UNLIKELY(etype == 0)) {
        static const GEnumValue values[] = {
            { GTK_POS_LEFT, "GTK_POS_LEFT", "left" },
            { GTK_POS_RIGHT, "GTK_POS_RIGHT", "right" },
            { GTK_POS_TOP, "GTK_POS_TOP", "top" },
            { GTK_POS_BOTTOM, "GTK_POS_BOTTOM", "bottom" },
            { 0, NULL, NULL }
        };
        etype = g_enum_register_static (g_intern_static_string ("GtkPositionType"), values);
    }
    return etype;
}

GType
gtk_relief_style_get_type (void)
{
    static GType etype = 0;
    if (G_UNLIKELY(etype == 0)) {
        static const GEnumValue values[] = {
            { GTK_RELIEF_NORMAL, "GTK_RELIEF_NORMAL", "normal" },
            { GTK_RELIEF_HALF, "GTK_RELIEF_HALF", "half" },
            { GTK_RELIEF_NONE, "GTK_RELIEF_NONE", "none" },
            { 0, NULL, NULL }
        };
        etype = g_enum_register_static (g_intern_static_string ("GtkReliefStyle"), values);
    }
    return etype;
}

GType
gtk_resize_mode_get_type (void)
{
    static GType etype = 0;
    if (G_UNLIKELY(etype == 0)) {
        static const GEnumValue values[] = {
            { GTK_RESIZE_PARENT, "GTK_RESIZE_PARENT", "parent" },
            { GTK_RESIZE_QUEUE, "GTK_RESIZE_QUEUE", "queue" },
            { GTK_RESIZE_IMMEDIATE, "GTK_RESIZE_IMMEDIATE", "immediate" },
            { 0, NULL, NULL }
        };
        etype = g_enum_register_static (g_intern_static_string ("GtkResizeMode"), values);
    }
    return etype;
}

GType
gtk_scroll_type_get_type (void)
{
    static GType etype = 0;
    if (G_UNLIKELY(etype == 0)) {
        static const GEnumValue values[] = {
            { GTK_SCROLL_NONE, "GTK_SCROLL_NONE", "none" },
            { GTK_SCROLL_JUMP, "GTK_SCROLL_JUMP", "jump" },
            { GTK_SCROLL_STEP_BACKWARD, "GTK_SCROLL_STEP_BACKWARD", "step-backward" },
            { GTK_SCROLL_STEP_FORWARD, "GTK_SCROLL_STEP_FORWARD", "step-forward" },
            { GTK_SCROLL_PAGE_BACKWARD, "GTK_SCROLL_PAGE_BACKWARD", "page-backward" },
            { GTK_SCROLL_PAGE_FORWARD, "GTK_SCROLL_PAGE_FORWARD", "page-forward" },
            { GTK_SCROLL_STEP_UP, "GTK_SCROLL_STEP_UP", "step-up" },
            { GTK_SCROLL_STEP_DOWN, "GTK_SCROLL_STEP_DOWN", "step-down" },
            { GTK_SCROLL_PAGE_UP, "GTK_SCROLL_PAGE_UP", "page-up" },
            { GTK_SCROLL_PAGE_DOWN, "GTK_SCROLL_PAGE_DOWN", "page-down" },
            { GTK_SCROLL_STEP_LEFT, "GTK_SCROLL_STEP_LEFT", "step-left" },
            { GTK_SCROLL_STEP_RIGHT, "GTK_SCROLL_STEP_RIGHT", "step-right" },
            { GTK_SCROLL_PAGE_LEFT, "GTK_SCROLL_PAGE_LEFT", "page-left" },
            { GTK_SCROLL_PAGE_RIGHT, "GTK_SCROLL_PAGE_RIGHT", "page-right" },
            { GTK_SCROLL_START, "GTK_SCROLL_START", "start" },
            { GTK_SCROLL_END, "GTK_SCROLL_END", "end" },
            { 0, NULL, NULL }
        };
        etype = g_enum_register_static (g_intern_static_string ("GtkScrollType"), values);
    }
    return etype;
}

GType
gtk_selection_mode_get_type (void)
{
    static GType etype = 0;
    if (G_UNLIKELY(etype == 0)) {
        static const GEnumValue values[] = {
            { GTK_SELECTION_NONE, "GTK_SELECTION_NONE", "none" },
            { GTK_SELECTION_SINGLE, "GTK_SELECTION_SINGLE", "single" },
            { GTK_SELECTION_BROWSE, "GTK_SELECTION_BROWSE", "browse" },
            { GTK_SELECTION_MULTIPLE, "GTK_SELECTION_MULTIPLE", "multiple" },
            { 0, NULL, NULL }
        };
        etype = g_enum_register_static (g_intern_static_string ("GtkSelectionMode"), values);
    }
    return etype;
}

GType
gtk_shadow_type_get_type (void)
{
    static GType etype = 0;
    if (G_UNLIKELY(etype == 0)) {
        static const GEnumValue values[] = {
            { GTK_SHADOW_NONE, "GTK_SHADOW_NONE", "none" },
            { GTK_SHADOW_IN, "GTK_SHADOW_IN", "in" },
            { GTK_SHADOW_OUT, "GTK_SHADOW_OUT", "out" },
            { GTK_SHADOW_ETCHED_IN, "GTK_SHADOW_ETCHED_IN", "etched-in" },
            { GTK_SHADOW_ETCHED_OUT, "GTK_SHADOW_ETCHED_OUT", "etched-out" },
            { 0, NULL, NULL }
        };
        etype = g_enum_register_static (g_intern_static_string ("GtkShadowType"), values);
    }
    return etype;
}

GType
gtk_state_type_get_type (void)
{
    static GType etype = 0;
    if (G_UNLIKELY(etype == 0)) {
        static const GEnumValue values[] = {
            { GTK_STATE_NORMAL, "GTK_STATE_NORMAL", "normal" },
            { GTK_STATE_ACTIVE, "GTK_STATE_ACTIVE", "active" },
            { GTK_STATE_PRELIGHT, "GTK_STATE_PRELIGHT", "prelight" },
            { GTK_STATE_SELECTED, "GTK_STATE_SELECTED", "selected" },
            { GTK_STATE_INSENSITIVE, "GTK_STATE_INSENSITIVE", "insensitive" },
            { GTK_STATE_INCONSISTENT, "GTK_STATE_INCONSISTENT", "inconsistent" },
            { GTK_STATE_FOCUSED, "GTK_STATE_FOCUSED", "focused" },
            { 0, NULL, NULL }
        };
        etype = g_enum_register_static (g_intern_static_string ("GtkStateType"), values);
    }
    return etype;
}

GType
gtk_toolbar_style_get_type (void)
{
    static GType etype = 0;
    if (G_UNLIKELY(etype == 0)) {
        static const GEnumValue values[] = {
            { GTK_TOOLBAR_ICONS, "GTK_TOOLBAR_ICONS", "icons" },
            { GTK_TOOLBAR_TEXT, "GTK_TOOLBAR_TEXT", "text" },
            { GTK_TOOLBAR_BOTH, "GTK_TOOLBAR_BOTH", "both" },
            { GTK_TOOLBAR_BOTH_HORIZ, "GTK_TOOLBAR_BOTH_HORIZ", "both-horiz" },
            { 0, NULL, NULL }
        };
        etype = g_enum_register_static (g_intern_static_string ("GtkToolbarStyle"), values);
    }
    return etype;
}

GType
gtk_window_position_get_type (void)
{
    static GType etype = 0;
    if (G_UNLIKELY(etype == 0)) {
        static const GEnumValue values[] = {
            { GTK_WIN_POS_NONE, "GTK_WIN_POS_NONE", "none" },
            { GTK_WIN_POS_CENTER, "GTK_WIN_POS_CENTER", "center" },
            { GTK_WIN_POS_MOUSE, "GTK_WIN_POS_MOUSE", "mouse" },
            { GTK_WIN_POS_CENTER_ALWAYS, "GTK_WIN_POS_CENTER_ALWAYS", "center-always" },
            { GTK_WIN_POS_CENTER_ON_PARENT, "GTK_WIN_POS_CENTER_ON_PARENT", "center-on-parent" },
            { 0, NULL, NULL }
        };
        etype = g_enum_register_static (g_intern_static_string ("GtkWindowPosition"), values);
    }
    return etype;
}

GType
gtk_window_type_get_type (void)
{
    static GType etype = 0;
    if (G_UNLIKELY(etype == 0)) {
        static const GEnumValue values[] = {
            { GTK_WINDOW_TOPLEVEL, "GTK_WINDOW_TOPLEVEL", "toplevel" },
            { GTK_WINDOW_POPUP, "GTK_WINDOW_POPUP", "popup" },
            { 0, NULL, NULL }
        };
        etype = g_enum_register_static (g_intern_static_string ("GtkWindowType"), values);
    }
    return etype;
}

GType
gtk_wrap_mode_get_type (void)
{
    static GType etype = 0;
    if (G_UNLIKELY(etype == 0)) {
        static const GEnumValue values[] = {
            { GTK_WRAP_NONE, "GTK_WRAP_NONE", "none" },
            { GTK_WRAP_CHAR, "GTK_WRAP_CHAR", "char" },
            { GTK_WRAP_WORD, "GTK_WRAP_WORD", "word" },
            { GTK_WRAP_WORD_CHAR, "GTK_WRAP_WORD_CHAR", "word-char" },
            { 0, NULL, NULL }
        };
        etype = g_enum_register_static (g_intern_static_string ("GtkWrapMode"), values);
    }
    return etype;
}

GType
gtk_sort_type_get_type (void)
{
    static GType etype = 0;
    if (G_UNLIKELY(etype == 0)) {
        static const GEnumValue values[] = {
            { GTK_SORT_ASCENDING, "GTK_SORT_ASCENDING", "ascending" },
            { GTK_SORT_DESCENDING, "GTK_SORT_DESCENDING", "descending" },
            { 0, NULL, NULL }
        };
        etype = g_enum_register_static (g_intern_static_string ("GtkSortType"), values);
    }
    return etype;
}

GType
gtk_im_preedit_style_get_type (void)
{
    static GType etype = 0;
    if (G_UNLIKELY(etype == 0)) {
        static const GEnumValue values[] = {
            { GTK_IM_PREEDIT_NOTHING, "GTK_IM_PREEDIT_NOTHING", "nothing" },
            { GTK_IM_PREEDIT_CALLBACK, "GTK_IM_PREEDIT_CALLBACK", "callback" },
            { GTK_IM_PREEDIT_NONE, "GTK_IM_PREEDIT_NONE", "none" },
            { 0, NULL, NULL }
        };
        etype = g_enum_register_static (g_intern_static_string ("GtkIMPreeditStyle"), values);
    }
    return etype;
}

GType
gtk_im_status_style_get_type (void)
{
    static GType etype = 0;
    if (G_UNLIKELY(etype == 0)) {
        static const GEnumValue values[] = {
            { GTK_IM_STATUS_NOTHING, "GTK_IM_STATUS_NOTHING", "nothing" },
            { GTK_IM_STATUS_CALLBACK, "GTK_IM_STATUS_CALLBACK", "callback" },
            { GTK_IM_STATUS_NONE, "GTK_IM_STATUS_NONE", "none" },
            { 0, NULL, NULL }
        };
        etype = g_enum_register_static (g_intern_static_string ("GtkIMStatusStyle"), values);
    }
    return etype;
}

GType
gtk_pack_direction_get_type (void)
{
    static GType etype = 0;
    if (G_UNLIKELY(etype == 0)) {
        static const GEnumValue values[] = {
            { GTK_PACK_DIRECTION_LTR, "GTK_PACK_DIRECTION_LTR", "ltr" },
            { GTK_PACK_DIRECTION_RTL, "GTK_PACK_DIRECTION_RTL", "rtl" },
            { GTK_PACK_DIRECTION_TTB, "GTK_PACK_DIRECTION_TTB", "ttb" },
            { GTK_PACK_DIRECTION_BTT, "GTK_PACK_DIRECTION_BTT", "btt" },
            { 0, NULL, NULL }
        };
        etype = g_enum_register_static (g_intern_static_string ("GtkPackDirection"), values);
    }
    return etype;
}

GType
gtk_print_pages_get_type (void)
{
    static GType etype = 0;
    if (G_UNLIKELY(etype == 0)) {
        static const GEnumValue values[] = {
            { GTK_PRINT_PAGES_ALL, "GTK_PRINT_PAGES_ALL", "all" },
            { GTK_PRINT_PAGES_CURRENT, "GTK_PRINT_PAGES_CURRENT", "current" },
            { GTK_PRINT_PAGES_RANGES, "GTK_PRINT_PAGES_RANGES", "ranges" },
            { GTK_PRINT_PAGES_SELECTION, "GTK_PRINT_PAGES_SELECTION", "selection" },
            { 0, NULL, NULL }
        };
        etype = g_enum_register_static (g_intern_static_string ("GtkPrintPages"), values);
    }
    return etype;
}

GType
gtk_page_set_get_type (void)
{
    static GType etype = 0;
    if (G_UNLIKELY(etype == 0)) {
        static const GEnumValue values[] = {
            { GTK_PAGE_SET_ALL, "GTK_PAGE_SET_ALL", "all" },
            { GTK_PAGE_SET_EVEN, "GTK_PAGE_SET_EVEN", "even" },
            { GTK_PAGE_SET_ODD, "GTK_PAGE_SET_ODD", "odd" },
            { 0, NULL, NULL }
        };
        etype = g_enum_register_static (g_intern_static_string ("GtkPageSet"), values);
    }
    return etype;
}

GType
gtk_number_up_layout_get_type (void)
{
    static GType etype = 0;
    if (G_UNLIKELY(etype == 0)) {
        static const GEnumValue values[] = {
            { GTK_NUMBER_UP_LAYOUT_LEFT_TO_RIGHT_TOP_TO_BOTTOM, "GTK_NUMBER_UP_LAYOUT_LEFT_TO_RIGHT_TOP_TO_BOTTOM", "lrtb" },
            { GTK_NUMBER_UP_LAYOUT_LEFT_TO_RIGHT_BOTTOM_TO_TOP, "GTK_NUMBER_UP_LAYOUT_LEFT_TO_RIGHT_BOTTOM_TO_TOP", "lrbt" },
            { GTK_NUMBER_UP_LAYOUT_RIGHT_TO_LEFT_TOP_TO_BOTTOM, "GTK_NUMBER_UP_LAYOUT_RIGHT_TO_LEFT_TOP_TO_BOTTOM", "rltb" },
            { GTK_NUMBER_UP_LAYOUT_RIGHT_TO_LEFT_BOTTOM_TO_TOP, "GTK_NUMBER_UP_LAYOUT_RIGHT_TO_LEFT_BOTTOM_TO_TOP", "rlbt" },
            { GTK_NUMBER_UP_LAYOUT_TOP_TO_BOTTOM_LEFT_TO_RIGHT, "GTK_NUMBER_UP_LAYOUT_TOP_TO_BOTTOM_LEFT_TO_RIGHT", "tblr" },
            { GTK_NUMBER_UP_LAYOUT_TOP_TO_BOTTOM_RIGHT_TO_LEFT, "GTK_NUMBER_UP_LAYOUT_TOP_TO_BOTTOM_RIGHT_TO_LEFT", "tbrl" },
            { GTK_NUMBER_UP_LAYOUT_BOTTOM_TO_TOP_LEFT_TO_RIGHT, "GTK_NUMBER_UP_LAYOUT_BOTTOM_TO_TOP_LEFT_TO_RIGHT", "btlr" },
            { GTK_NUMBER_UP_LAYOUT_BOTTOM_TO_TOP_RIGHT_TO_LEFT, "GTK_NUMBER_UP_LAYOUT_BOTTOM_TO_TOP_RIGHT_TO_LEFT", "btrl" },
            { 0, NULL, NULL }
        };
        etype = g_enum_register_static (g_intern_static_string ("GtkNumberUpLayout"), values);
    }
    return etype;
}

GType
gtk_page_orientation_get_type (void)
{
    static GType etype = 0;
    if (G_UNLIKELY(etype == 0)) {
        static const GEnumValue values[] = {
            { GTK_PAGE_ORIENTATION_PORTRAIT, "GTK_PAGE_ORIENTATION_PORTRAIT", "portrait" },
            { GTK_PAGE_ORIENTATION_LANDSCAPE, "GTK_PAGE_ORIENTATION_LANDSCAPE", "landscape" },
            { GTK_PAGE_ORIENTATION_REVERSE_PORTRAIT, "GTK_PAGE_ORIENTATION_REVERSE_PORTRAIT", "reverse-portrait" },
            { GTK_PAGE_ORIENTATION_REVERSE_LANDSCAPE, "GTK_PAGE_ORIENTATION_REVERSE_LANDSCAPE", "reverse-landscape" },
            { 0, NULL, NULL }
        };
        etype = g_enum_register_static (g_intern_static_string ("GtkPageOrientation"), values);
    }
    return etype;
}

GType
gtk_print_quality_get_type (void)
{
    static GType etype = 0;
    if (G_UNLIKELY(etype == 0)) {
        static const GEnumValue values[] = {
            { GTK_PRINT_QUALITY_LOW, "GTK_PRINT_QUALITY_LOW", "low" },
            { GTK_PRINT_QUALITY_NORMAL, "GTK_PRINT_QUALITY_NORMAL", "normal" },
            { GTK_PRINT_QUALITY_HIGH, "GTK_PRINT_QUALITY_HIGH", "high" },
            { GTK_PRINT_QUALITY_DRAFT, "GTK_PRINT_QUALITY_DRAFT", "draft" },
            { 0, NULL, NULL }
        };
        etype = g_enum_register_static (g_intern_static_string ("GtkPrintQuality"), values);
    }
    return etype;
}

GType
gtk_print_duplex_get_type (void)
{
    static GType etype = 0;
    if (G_UNLIKELY(etype == 0)) {
        static const GEnumValue values[] = {
            { GTK_PRINT_DUPLEX_SIMPLEX, "GTK_PRINT_DUPLEX_SIMPLEX", "simplex" },
            { GTK_PRINT_DUPLEX_HORIZONTAL, "GTK_PRINT_DUPLEX_HORIZONTAL", "horizontal" },
            { GTK_PRINT_DUPLEX_VERTICAL, "GTK_PRINT_DUPLEX_VERTICAL", "vertical" },
            { 0, NULL, NULL }
        };
        etype = g_enum_register_static (g_intern_static_string ("GtkPrintDuplex"), values);
    }
    return etype;
}

GType
gtk_unit_get_type (void)
{
    static GType etype = 0;
    if (G_UNLIKELY(etype == 0)) {
        static const GEnumValue values[] = {
            { GTK_UNIT_PIXEL, "GTK_UNIT_PIXEL", "pixel" },
            { GTK_UNIT_POINTS, "GTK_UNIT_POINTS", "points" },
            { GTK_UNIT_INCH, "GTK_UNIT_INCH", "inch" },
            { GTK_UNIT_MM, "GTK_UNIT_MM", "mm" },
            { 0, NULL, NULL }
        };
        etype = g_enum_register_static (g_intern_static_string ("GtkUnit"), values);
    }
    return etype;
}

GType
gtk_tree_view_grid_lines_get_type (void)
{
    static GType etype = 0;
    if (G_UNLIKELY(etype == 0)) {
        static const GEnumValue values[] = {
            { GTK_TREE_VIEW_GRID_LINES_NONE, "GTK_TREE_VIEW_GRID_LINES_NONE", "none" },
            { GTK_TREE_VIEW_GRID_LINES_HORIZONTAL, "GTK_TREE_VIEW_GRID_LINES_HORIZONTAL", "horizontal" },
            { GTK_TREE_VIEW_GRID_LINES_VERTICAL, "GTK_TREE_VIEW_GRID_LINES_VERTICAL", "vertical" },
            { GTK_TREE_VIEW_GRID_LINES_BOTH, "GTK_TREE_VIEW_GRID_LINES_BOTH", "both" },
            { 0, NULL, NULL }
        };
        etype = g_enum_register_static (g_intern_static_string ("GtkTreeViewGridLines"), values);
    }
    return etype;
}

GType
gtk_drag_result_get_type (void)
{
    static GType etype = 0;
    if (G_UNLIKELY(etype == 0)) {
        static const GEnumValue values[] = {
            { GTK_DRAG_RESULT_SUCCESS, "GTK_DRAG_RESULT_SUCCESS", "success" },
            { GTK_DRAG_RESULT_NO_TARGET, "GTK_DRAG_RESULT_NO_TARGET", "no-target" },
            { GTK_DRAG_RESULT_USER_CANCELLED, "GTK_DRAG_RESULT_USER_CANCELLED", "user-cancelled" },
            { GTK_DRAG_RESULT_TIMEOUT_EXPIRED, "GTK_DRAG_RESULT_TIMEOUT_EXPIRED", "timeout-expired" },
            { GTK_DRAG_RESULT_GRAB_BROKEN, "GTK_DRAG_RESULT_GRAB_BROKEN", "grab-broken" },
            { GTK_DRAG_RESULT_ERROR, "GTK_DRAG_RESULT_ERROR", "error" },
            { 0, NULL, NULL }
        };
        etype = g_enum_register_static (g_intern_static_string ("GtkDragResult"), values);
    }
    return etype;
}

GType
gtk_size_request_mode_get_type (void)
{
    static GType etype = 0;
    if (G_UNLIKELY(etype == 0)) {
        static const GEnumValue values[] = {
            { GTK_SIZE_REQUEST_HEIGHT_FOR_WIDTH, "GTK_SIZE_REQUEST_HEIGHT_FOR_WIDTH", "height-for-width" },
            { GTK_SIZE_REQUEST_WIDTH_FOR_HEIGHT, "GTK_SIZE_REQUEST_WIDTH_FOR_HEIGHT", "width-for-height" },
            { GTK_SIZE_REQUEST_CONSTANT_SIZE, "GTK_SIZE_REQUEST_CONSTANT_SIZE", "constant-size" },
            { 0, NULL, NULL }
        };
        etype = g_enum_register_static (g_intern_static_string ("GtkSizeRequestMode"), values);
    }
    return etype;
}

GType
gtk_scrollable_policy_get_type (void)
{
    static GType etype = 0;
    if (G_UNLIKELY(etype == 0)) {
        static const GEnumValue values[] = {
            { GTK_SCROLL_MINIMUM, "GTK_SCROLL_MINIMUM", "minimum" },
            { GTK_SCROLL_NATURAL, "GTK_SCROLL_NATURAL", "natural" },
            { 0, NULL, NULL }
        };
        etype = g_enum_register_static (g_intern_static_string ("GtkScrollablePolicy"), values);
    }
    return etype;
}

GType
gtk_state_flags_get_type (void)
{
    static GType etype = 0;
    if (G_UNLIKELY(etype == 0)) {
        static const GFlagsValue values[] = {
            { GTK_STATE_FLAG_NORMAL, "GTK_STATE_FLAG_NORMAL", "normal" },
            { GTK_STATE_FLAG_ACTIVE, "GTK_STATE_FLAG_ACTIVE", "active" },
            { GTK_STATE_FLAG_PRELIGHT, "GTK_STATE_FLAG_PRELIGHT", "prelight" },
            { GTK_STATE_FLAG_SELECTED, "GTK_STATE_FLAG_SELECTED", "selected" },
            { GTK_STATE_FLAG_INSENSITIVE, "GTK_STATE_FLAG_INSENSITIVE", "insensitive" },
            { GTK_STATE_FLAG_INCONSISTENT, "GTK_STATE_FLAG_INCONSISTENT", "inconsistent" },
            { GTK_STATE_FLAG_FOCUSED, "GTK_STATE_FLAG_FOCUSED", "focused" },
            { 0, NULL, NULL }
        };
        etype = g_flags_register_static (g_intern_static_string ("GtkStateFlags"), values);
    }
    return etype;
}

GType
gtk_region_flags_get_type (void)
{
    static GType etype = 0;
    if (G_UNLIKELY(etype == 0)) {
        static const GFlagsValue values[] = {
            { GTK_REGION_EVEN, "GTK_REGION_EVEN", "even" },
            { GTK_REGION_ODD, "GTK_REGION_ODD", "odd" },
            { GTK_REGION_FIRST, "GTK_REGION_FIRST", "first" },
            { GTK_REGION_LAST, "GTK_REGION_LAST", "last" },
            { GTK_REGION_SORTED, "GTK_REGION_SORTED", "sorted" },
            { 0, NULL, NULL }
        };
        etype = g_flags_register_static (g_intern_static_string ("GtkRegionFlags"), values);
    }
    return etype;
}

GType
gtk_junction_sides_get_type (void)
{
    static GType etype = 0;
    if (G_UNLIKELY(etype == 0)) {
        static const GFlagsValue values[] = {
            { GTK_JUNCTION_NONE, "GTK_JUNCTION_NONE", "none" },
            { GTK_JUNCTION_CORNER_TOPLEFT, "GTK_JUNCTION_CORNER_TOPLEFT", "corner-topleft" },
            { GTK_JUNCTION_CORNER_TOPRIGHT, "GTK_JUNCTION_CORNER_TOPRIGHT", "corner-topright" },
            { GTK_JUNCTION_CORNER_BOTTOMLEFT, "GTK_JUNCTION_CORNER_BOTTOMLEFT", "corner-bottomleft" },
            { GTK_JUNCTION_CORNER_BOTTOMRIGHT, "GTK_JUNCTION_CORNER_BOTTOMRIGHT", "corner-bottomright" },
            { GTK_JUNCTION_TOP, "GTK_JUNCTION_TOP", "top" },
            { GTK_JUNCTION_BOTTOM, "GTK_JUNCTION_BOTTOM", "bottom" },
            { GTK_JUNCTION_LEFT, "GTK_JUNCTION_LEFT", "left" },
            { GTK_JUNCTION_RIGHT, "GTK_JUNCTION_RIGHT", "right" },
            { 0, NULL, NULL }
        };
        etype = g_flags_register_static (g_intern_static_string ("GtkJunctionSides"), values);
    }
    return etype;
}

GType
gtk_border_style_get_type (void)
{
    static GType etype = 0;
    if (G_UNLIKELY(etype == 0)) {
        static const GEnumValue values[] = {
            { GTK_BORDER_STYLE_NONE, "GTK_BORDER_STYLE_NONE", "none" },
            { GTK_BORDER_STYLE_SOLID, "GTK_BORDER_STYLE_SOLID", "solid" },
            { GTK_BORDER_STYLE_INSET, "GTK_BORDER_STYLE_INSET", "inset" },
            { GTK_BORDER_STYLE_OUTSET, "GTK_BORDER_STYLE_OUTSET", "outset" },
            { 0, NULL, NULL }
        };
        etype = g_enum_register_static (g_intern_static_string ("GtkBorderStyle"), values);
    }
    return etype;
}

/* enumerations from "gtkfilechooser.h" */
GType
gtk_file_chooser_action_get_type (void)
{
    static GType etype = 0;
    if (G_UNLIKELY(etype == 0)) {
        static const GEnumValue values[] = {
            { GTK_FILE_CHOOSER_ACTION_OPEN, "GTK_FILE_CHOOSER_ACTION_OPEN", "open" },
            { GTK_FILE_CHOOSER_ACTION_SAVE, "GTK_FILE_CHOOSER_ACTION_SAVE", "save" },
            { GTK_FILE_CHOOSER_ACTION_SELECT_FOLDER, "GTK_FILE_CHOOSER_ACTION_SELECT_FOLDER", "select-folder" },
            { GTK_FILE_CHOOSER_ACTION_CREATE_FOLDER, "GTK_FILE_CHOOSER_ACTION_CREATE_FOLDER", "create-folder" },
            { 0, NULL, NULL }
        };
        etype = g_enum_register_static (g_intern_static_string ("GtkFileChooserAction"), values);
    }
    return etype;
}

GType
gtk_file_chooser_confirmation_get_type (void)
{
    static GType etype = 0;
    if (G_UNLIKELY(etype == 0)) {
        static const GEnumValue values[] = {
            { GTK_FILE_CHOOSER_CONFIRMATION_CONFIRM, "GTK_FILE_CHOOSER_CONFIRMATION_CONFIRM", "confirm" },
            { GTK_FILE_CHOOSER_CONFIRMATION_ACCEPT_FILENAME, "GTK_FILE_CHOOSER_CONFIRMATION_ACCEPT_FILENAME", "accept-filename" },
            { GTK_FILE_CHOOSER_CONFIRMATION_SELECT_AGAIN, "GTK_FILE_CHOOSER_CONFIRMATION_SELECT_AGAIN", "select-again" },
            { 0, NULL, NULL }
        };
        etype = g_enum_register_static (g_intern_static_string ("GtkFileChooserConfirmation"), values);
    }
    return etype;
}

GType
gtk_file_chooser_error_get_type (void)
{
    static GType etype = 0;
    if (G_UNLIKELY(etype == 0)) {
        static const GEnumValue values[] = {
            { GTK_FILE_CHOOSER_ERROR_NONEXISTENT, "GTK_FILE_CHOOSER_ERROR_NONEXISTENT", "nonexistent" },
            { GTK_FILE_CHOOSER_ERROR_BAD_FILENAME, "GTK_FILE_CHOOSER_ERROR_BAD_FILENAME", "bad-filename" },
            { GTK_FILE_CHOOSER_ERROR_ALREADY_EXISTS, "GTK_FILE_CHOOSER_ERROR_ALREADY_EXISTS", "already-exists" },
            { GTK_FILE_CHOOSER_ERROR_INCOMPLETE_HOSTNAME, "GTK_FILE_CHOOSER_ERROR_INCOMPLETE_HOSTNAME", "incomplete-hostname" },
            { 0, NULL, NULL }
        };
        etype = g_enum_register_static (g_intern_static_string ("GtkFileChooserError"), values);
    }
    return etype;
}

/* enumerations from "gtkfilefilter.h" */
GType
gtk_file_filter_flags_get_type (void)
{
    static GType etype = 0;
    if (G_UNLIKELY(etype == 0)) {
        static const GFlagsValue values[] = {
            { GTK_FILE_FILTER_FILENAME, "GTK_FILE_FILTER_FILENAME", "filename" },
            { GTK_FILE_FILTER_URI, "GTK_FILE_FILTER_URI", "uri" },
            { GTK_FILE_FILTER_DISPLAY_NAME, "GTK_FILE_FILTER_DISPLAY_NAME", "display-name" },
            { GTK_FILE_FILTER_MIME_TYPE, "GTK_FILE_FILTER_MIME_TYPE", "mime-type" },
            { 0, NULL, NULL }
        };
        etype = g_flags_register_static (g_intern_static_string ("GtkFileFilterFlags"), values);
    }
    return etype;
}

/* enumerations from "gtkicontheme.h" */
GType
gtk_icon_lookup_flags_get_type (void)
{
    static GType etype = 0;
    if (G_UNLIKELY(etype == 0)) {
        static const GFlagsValue values[] = {
            { GTK_ICON_LOOKUP_NO_SVG, "GTK_ICON_LOOKUP_NO_SVG", "no-svg" },
            { GTK_ICON_LOOKUP_FORCE_SVG, "GTK_ICON_LOOKUP_FORCE_SVG", "force-svg" },
            { GTK_ICON_LOOKUP_USE_BUILTIN, "GTK_ICON_LOOKUP_USE_BUILTIN", "use-builtin" },
            { GTK_ICON_LOOKUP_GENERIC_FALLBACK, "GTK_ICON_LOOKUP_GENERIC_FALLBACK", "generic-fallback" },
            { GTK_ICON_LOOKUP_FORCE_SIZE, "GTK_ICON_LOOKUP_FORCE_SIZE", "force-size" },
            { 0, NULL, NULL }
        };
        etype = g_flags_register_static (g_intern_static_string ("GtkIconLookupFlags"), values);
    }
    return etype;
}

GType
gtk_icon_theme_error_get_type (void)
{
    static GType etype = 0;
    if (G_UNLIKELY(etype == 0)) {
        static const GEnumValue values[] = {
            { GTK_ICON_THEME_NOT_FOUND, "GTK_ICON_THEME_NOT_FOUND", "not-found" },
            { GTK_ICON_THEME_FAILED, "GTK_ICON_THEME_FAILED", "failed" },
            { 0, NULL, NULL }
        };
        etype = g_enum_register_static (g_intern_static_string ("GtkIconThemeError"), values);
    }
    return etype;
}

/* enumerations from "gtkiconview.h" */
GType
gtk_icon_view_drop_position_get_type (void)
{
    static GType etype = 0;
    if (G_UNLIKELY(etype == 0)) {
        static const GEnumValue values[] = {
            { GTK_ICON_VIEW_NO_DROP, "GTK_ICON_VIEW_NO_DROP", "no-drop" },
            { GTK_ICON_VIEW_DROP_INTO, "GTK_ICON_VIEW_DROP_INTO", "drop-into" },
            { GTK_ICON_VIEW_DROP_LEFT, "GTK_ICON_VIEW_DROP_LEFT", "drop-left" },
            { GTK_ICON_VIEW_DROP_RIGHT, "GTK_ICON_VIEW_DROP_RIGHT", "drop-right" },
            { GTK_ICON_VIEW_DROP_ABOVE, "GTK_ICON_VIEW_DROP_ABOVE", "drop-above" },
            { GTK_ICON_VIEW_DROP_BELOW, "GTK_ICON_VIEW_DROP_BELOW", "drop-below" },
            { 0, NULL, NULL }
        };
        etype = g_enum_register_static (g_intern_static_string ("GtkIconViewDropPosition"), values);
    }
    return etype;
}

/* enumerations from "gtkimage.h" */
GType
gtk_image_type_get_type (void)
{
    static GType etype = 0;
    if (G_UNLIKELY(etype == 0)) {
        static const GEnumValue values[] = {
            { GTK_IMAGE_EMPTY, "GTK_IMAGE_EMPTY", "empty" },
            { GTK_IMAGE_PIXBUF, "GTK_IMAGE_PIXBUF", "pixbuf" },
            { GTK_IMAGE_STOCK, "GTK_IMAGE_STOCK", "stock" },
            { GTK_IMAGE_ICON_SET, "GTK_IMAGE_ICON_SET", "icon-set" },
            { GTK_IMAGE_ANIMATION, "GTK_IMAGE_ANIMATION", "animation" },
            { GTK_IMAGE_ICON_NAME, "GTK_IMAGE_ICON_NAME", "icon-name" },
            { GTK_IMAGE_GICON, "GTK_IMAGE_GICON", "gicon" },
            { 0, NULL, NULL }
        };
        etype = g_enum_register_static (g_intern_static_string ("GtkImageType"), values);
    }
    return etype;
}

/* enumerations from "gtkmessagedialog.h" */
GType
gtk_buttons_type_get_type (void)
{
    static GType etype = 0;
    if (G_UNLIKELY(etype == 0)) {
        static const GEnumValue values[] = {
            { GTK_BUTTONS_NONE, "GTK_BUTTONS_NONE", "none" },
            { GTK_BUTTONS_OK, "GTK_BUTTONS_OK", "ok" },
            { GTK_BUTTONS_CLOSE, "GTK_BUTTONS_CLOSE", "close" },
            { GTK_BUTTONS_CANCEL, "GTK_BUTTONS_CANCEL", "cancel" },
            { GTK_BUTTONS_YES_NO, "GTK_BUTTONS_YES_NO", "yes-no" },
            { GTK_BUTTONS_OK_CANCEL, "GTK_BUTTONS_OK_CANCEL", "ok-cancel" },
            { 0, NULL, NULL }
        };
        etype = g_enum_register_static (g_intern_static_string ("GtkButtonsType"), values);
    }
    return etype;
}

/* enumerations from "gtknotebook.h" */
GType
gtk_notebook_tab_get_type (void)
{
    static GType etype = 0;
    if (G_UNLIKELY(etype == 0)) {
        static const GEnumValue values[] = {
            { GTK_NOTEBOOK_TAB_FIRST, "GTK_NOTEBOOK_TAB_FIRST", "first" },
            { GTK_NOTEBOOK_TAB_LAST, "GTK_NOTEBOOK_TAB_LAST", "last" },
            { 0, NULL, NULL }
        };
        etype = g_enum_register_static (g_intern_static_string ("GtkNotebookTab"), values);
    }
    return etype;
}

/* enumerations from "gtkprintoperation.h" */
GType
gtk_print_status_get_type (void)
{
    static GType etype = 0;
    if (G_UNLIKELY(etype == 0)) {
        static const GEnumValue values[] = {
            { GTK_PRINT_STATUS_INITIAL, "GTK_PRINT_STATUS_INITIAL", "initial" },
            { GTK_PRINT_STATUS_PREPARING, "GTK_PRINT_STATUS_PREPARING", "preparing" },
            { GTK_PRINT_STATUS_GENERATING_DATA, "GTK_PRINT_STATUS_GENERATING_DATA", "generating-data" },
            { GTK_PRINT_STATUS_SENDING_DATA, "GTK_PRINT_STATUS_SENDING_DATA", "sending-data" },
            { GTK_PRINT_STATUS_PENDING, "GTK_PRINT_STATUS_PENDING", "pending" },
            { GTK_PRINT_STATUS_PENDING_ISSUE, "GTK_PRINT_STATUS_PENDING_ISSUE", "pending-issue" },
            { GTK_PRINT_STATUS_PRINTING, "GTK_PRINT_STATUS_PRINTING", "printing" },
            { GTK_PRINT_STATUS_FINISHED, "GTK_PRINT_STATUS_FINISHED", "finished" },
            { GTK_PRINT_STATUS_FINISHED_ABORTED, "GTK_PRINT_STATUS_FINISHED_ABORTED", "finished-aborted" },
            { 0, NULL, NULL }
        };
        etype = g_enum_register_static (g_intern_static_string ("GtkPrintStatus"), values);
    }
    return etype;
}

GType
gtk_print_operation_result_get_type (void)
{
    static GType etype = 0;
    if (G_UNLIKELY(etype == 0)) {
        static const GEnumValue values[] = {
            { GTK_PRINT_OPERATION_RESULT_ERROR, "GTK_PRINT_OPERATION_RESULT_ERROR", "error" },
            { GTK_PRINT_OPERATION_RESULT_APPLY, "GTK_PRINT_OPERATION_RESULT_APPLY", "apply" },
            { GTK_PRINT_OPERATION_RESULT_CANCEL, "GTK_PRINT_OPERATION_RESULT_CANCEL", "cancel" },
            { GTK_PRINT_OPERATION_RESULT_IN_PROGRESS, "GTK_PRINT_OPERATION_RESULT_IN_PROGRESS", "in-progress" },
            { 0, NULL, NULL }
        };
        etype = g_enum_register_static (g_intern_static_string ("GtkPrintOperationResult"), values);
    }
    return etype;
}

GType
gtk_print_operation_action_get_type (void)
{
    static GType etype = 0;
    if (G_UNLIKELY(etype == 0)) {
        static const GEnumValue values[] = {
            { GTK_PRINT_OPERATION_ACTION_PRINT_DIALOG, "GTK_PRINT_OPERATION_ACTION_PRINT_DIALOG", "print-dialog" },
            { GTK_PRINT_OPERATION_ACTION_PRINT, "GTK_PRINT_OPERATION_ACTION_PRINT", "print" },
            { GTK_PRINT_OPERATION_ACTION_PREVIEW, "GTK_PRINT_OPERATION_ACTION_PREVIEW", "preview" },
            { GTK_PRINT_OPERATION_ACTION_EXPORT, "GTK_PRINT_OPERATION_ACTION_EXPORT", "export" },
            { 0, NULL, NULL }
        };
        etype = g_enum_register_static (g_intern_static_string ("GtkPrintOperationAction"), values);
    }
    return etype;
}

GType
gtk_print_error_get_type (void)
{
    static GType etype = 0;
    if (G_UNLIKELY(etype == 0)) {
        static const GEnumValue values[] = {
            { GTK_PRINT_ERROR_GENERAL, "GTK_PRINT_ERROR_GENERAL", "general" },
            { GTK_PRINT_ERROR_INTERNAL_ERROR, "GTK_PRINT_ERROR_INTERNAL_ERROR", "internal-error" },
            { GTK_PRINT_ERROR_NOMEM, "GTK_PRINT_ERROR_NOMEM", "nomem" },
            { GTK_PRINT_ERROR_INVALID_FILE, "GTK_PRINT_ERROR_INVALID_FILE", "invalid-file" },
            { 0, NULL, NULL }
        };
        etype = g_enum_register_static (g_intern_static_string ("GtkPrintError"), values);
    }
    return etype;
}

/* enumerations from "gtkrc.h" */
GType
gtk_rc_flags_get_type (void)
{
    static GType etype = 0;
    if (G_UNLIKELY(etype == 0)) {
        static const GFlagsValue values[] = {
            { GTK_RC_FG, "GTK_RC_FG", "fg" },
            { GTK_RC_BG, "GTK_RC_BG", "bg" },
            { GTK_RC_TEXT, "GTK_RC_TEXT", "text" },
            { GTK_RC_BASE, "GTK_RC_BASE", "base" },
            { 0, NULL, NULL }
        };
        etype = g_flags_register_static (g_intern_static_string ("GtkRcFlags"), values);
    }
    return etype;
}

GType
gtk_rc_token_type_get_type (void)
{
    static GType etype = 0;
    if (G_UNLIKELY(etype == 0)) {
        static const GEnumValue values[] = {
            { GTK_RC_TOKEN_INVALID, "GTK_RC_TOKEN_INVALID", "invalid" },
            { GTK_RC_TOKEN_INCLUDE, "GTK_RC_TOKEN_INCLUDE", "include" },
            { GTK_RC_TOKEN_NORMAL, "GTK_RC_TOKEN_NORMAL", "normal" },
            { GTK_RC_TOKEN_ACTIVE, "GTK_RC_TOKEN_ACTIVE", "active" },
            { GTK_RC_TOKEN_PRELIGHT, "GTK_RC_TOKEN_PRELIGHT", "prelight" },
            { GTK_RC_TOKEN_SELECTED, "GTK_RC_TOKEN_SELECTED", "selected" },
            { GTK_RC_TOKEN_INSENSITIVE, "GTK_RC_TOKEN_INSENSITIVE", "insensitive" },
            { GTK_RC_TOKEN_FG, "GTK_RC_TOKEN_FG", "fg" },
            { GTK_RC_TOKEN_BG, "GTK_RC_TOKEN_BG", "bg" },
            { GTK_RC_TOKEN_TEXT, "GTK_RC_TOKEN_TEXT", "text" },
            { GTK_RC_TOKEN_BASE, "GTK_RC_TOKEN_BASE", "base" },
            { GTK_RC_TOKEN_XTHICKNESS, "GTK_RC_TOKEN_XTHICKNESS", "xthickness" },
            { GTK_RC_TOKEN_YTHICKNESS, "GTK_RC_TOKEN_YTHICKNESS", "ythickness" },
            { GTK_RC_TOKEN_FONT, "GTK_RC_TOKEN_FONT", "font" },
            { GTK_RC_TOKEN_FONTSET, "GTK_RC_TOKEN_FONTSET", "fontset" },
            { GTK_RC_TOKEN_FONT_NAME, "GTK_RC_TOKEN_FONT_NAME", "font-name" },
            { GTK_RC_TOKEN_BG_PIXMAP, "GTK_RC_TOKEN_BG_PIXMAP", "bg-pixmap" },
            { GTK_RC_TOKEN_PIXMAP_PATH, "GTK_RC_TOKEN_PIXMAP_PATH", "pixmap-path" },
            { GTK_RC_TOKEN_STYLE, "GTK_RC_TOKEN_STYLE", "style" },
            { GTK_RC_TOKEN_BINDING, "GTK_RC_TOKEN_BINDING", "binding" },
            { GTK_RC_TOKEN_BIND, "GTK_RC_TOKEN_BIND", "bind" },
            { GTK_RC_TOKEN_WIDGET, "GTK_RC_TOKEN_WIDGET", "widget" },
            { GTK_RC_TOKEN_WIDGET_CLASS, "GTK_RC_TOKEN_WIDGET_CLASS", "widget-class" },
            { GTK_RC_TOKEN_CLASS, "GTK_RC_TOKEN_CLASS", "class" },
            { GTK_RC_TOKEN_LOWEST, "GTK_RC_TOKEN_LOWEST", "lowest" },
            { GTK_RC_TOKEN_GTK, "GTK_RC_TOKEN_GTK", "gtk" },
            { GTK_RC_TOKEN_APPLICATION, "GTK_RC_TOKEN_APPLICATION", "application" },
            { GTK_RC_TOKEN_THEME, "GTK_RC_TOKEN_THEME", "theme" },
            { GTK_RC_TOKEN_RC, "GTK_RC_TOKEN_RC", "rc" },
            { GTK_RC_TOKEN_HIGHEST, "GTK_RC_TOKEN_HIGHEST", "highest" },
            { GTK_RC_TOKEN_ENGINE, "GTK_RC_TOKEN_ENGINE", "engine" },
            { GTK_RC_TOKEN_MODULE_PATH, "GTK_RC_TOKEN_MODULE_PATH", "module-path" },
            { GTK_RC_TOKEN_IM_MODULE_PATH, "GTK_RC_TOKEN_IM_MODULE_PATH", "im-module-path" },
            { GTK_RC_TOKEN_IM_MODULE_FILE, "GTK_RC_TOKEN_IM_MODULE_FILE", "im-module-file" },
            { GTK_RC_TOKEN_STOCK, "GTK_RC_TOKEN_STOCK", "stock" },
            { GTK_RC_TOKEN_LTR, "GTK_RC_TOKEN_LTR", "ltr" },
            { GTK_RC_TOKEN_RTL, "GTK_RC_TOKEN_RTL", "rtl" },
            { GTK_RC_TOKEN_COLOR, "GTK_RC_TOKEN_COLOR", "color" },
            { GTK_RC_TOKEN_UNBIND, "GTK_RC_TOKEN_UNBIND", "unbind" },
            { GTK_RC_TOKEN_LAST, "GTK_RC_TOKEN_LAST", "last" },
            { 0, NULL, NULL }
        };
        etype = g_enum_register_static (g_intern_static_string ("GtkRcTokenType"), values);
    }
    return etype;
}

/* enumerations from "gtkrecentchooser.h" */
GType
gtk_recent_sort_type_get_type (void)
{
    static GType etype = 0;
    if (G_UNLIKELY(etype == 0)) {
        static const GEnumValue values[] = {
            { GTK_RECENT_SORT_NONE, "GTK_RECENT_SORT_NONE", "none" },
            { GTK_RECENT_SORT_MRU, "GTK_RECENT_SORT_MRU", "mru" },
            { GTK_RECENT_SORT_LRU, "GTK_RECENT_SORT_LRU", "lru" },
            { GTK_RECENT_SORT_CUSTOM, "GTK_RECENT_SORT_CUSTOM", "custom" },
            { 0, NULL, NULL }
        };
        etype = g_enum_register_static (g_intern_static_string ("GtkRecentSortType"), values);
    }
    return etype;
}

GType
gtk_recent_chooser_error_get_type (void)
{
    static GType etype = 0;
    if (G_UNLIKELY(etype == 0)) {
        static const GEnumValue values[] = {
            { GTK_RECENT_CHOOSER_ERROR_NOT_FOUND, "GTK_RECENT_CHOOSER_ERROR_NOT_FOUND", "not-found" },
            { GTK_RECENT_CHOOSER_ERROR_INVALID_URI, "GTK_RECENT_CHOOSER_ERROR_INVALID_URI", "invalid-uri" },
            { 0, NULL, NULL }
        };
        etype = g_enum_register_static (g_intern_static_string ("GtkRecentChooserError"), values);
    }
    return etype;
}

/* enumerations from "gtkrecentfilter.h" */
GType
gtk_recent_filter_flags_get_type (void)
{
    static GType etype = 0;
    if (G_UNLIKELY(etype == 0)) {
        static const GFlagsValue values[] = {
            { GTK_RECENT_FILTER_URI, "GTK_RECENT_FILTER_URI", "uri" },
            { GTK_RECENT_FILTER_DISPLAY_NAME, "GTK_RECENT_FILTER_DISPLAY_NAME", "display-name" },
            { GTK_RECENT_FILTER_MIME_TYPE, "GTK_RECENT_FILTER_MIME_TYPE", "mime-type" },
            { GTK_RECENT_FILTER_APPLICATION, "GTK_RECENT_FILTER_APPLICATION", "application" },
            { GTK_RECENT_FILTER_GROUP, "GTK_RECENT_FILTER_GROUP", "group" },
            { GTK_RECENT_FILTER_AGE, "GTK_RECENT_FILTER_AGE", "age" },
            { 0, NULL, NULL }
        };
        etype = g_flags_register_static (g_intern_static_string ("GtkRecentFilterFlags"), values);
    }
    return etype;
}

/* enumerations from "gtkrecentmanager.h" */
GType
gtk_recent_manager_error_get_type (void)
{
    static GType etype = 0;
    if (G_UNLIKELY(etype == 0)) {
        static const GEnumValue values[] = {
            { GTK_RECENT_MANAGER_ERROR_NOT_FOUND, "GTK_RECENT_MANAGER_ERROR_NOT_FOUND", "not-found" },
            { GTK_RECENT_MANAGER_ERROR_INVALID_URI, "GTK_RECENT_MANAGER_ERROR_INVALID_URI", "invalid-uri" },
            { GTK_RECENT_MANAGER_ERROR_INVALID_ENCODING, "GTK_RECENT_MANAGER_ERROR_INVALID_ENCODING", "invalid-encoding" },
            { GTK_RECENT_MANAGER_ERROR_NOT_REGISTERED, "GTK_RECENT_MANAGER_ERROR_NOT_REGISTERED", "not-registered" },
            { GTK_RECENT_MANAGER_ERROR_READ, "GTK_RECENT_MANAGER_ERROR_READ", "read" },
            { GTK_RECENT_MANAGER_ERROR_WRITE, "GTK_RECENT_MANAGER_ERROR_WRITE", "write" },
            { GTK_RECENT_MANAGER_ERROR_UNKNOWN, "GTK_RECENT_MANAGER_ERROR_UNKNOWN", "unknown" },
            { 0, NULL, NULL }
        };
        etype = g_enum_register_static (g_intern_static_string ("GtkRecentManagerError"), values);
    }
    return etype;
}

/* enumerations from "gtksizegroup.h" */
GType
gtk_size_group_mode_get_type (void)
{
    static GType etype = 0;
    if (G_UNLIKELY(etype == 0)) {
        static const GEnumValue values[] = {
            { GTK_SIZE_GROUP_NONE, "GTK_SIZE_GROUP_NONE", "none" },
            { GTK_SIZE_GROUP_HORIZONTAL, "GTK_SIZE_GROUP_HORIZONTAL", "horizontal" },
            { GTK_SIZE_GROUP_VERTICAL, "GTK_SIZE_GROUP_VERTICAL", "vertical" },
            { GTK_SIZE_GROUP_BOTH, "GTK_SIZE_GROUP_BOTH", "both" },
            { 0, NULL, NULL }
        };
        etype = g_enum_register_static (g_intern_static_string ("GtkSizeGroupMode"), values);
    }
    return etype;
}

/* enumerations from "gtkspinbutton.h" */
GType
gtk_spin_button_update_policy_get_type (void)
{
    static GType etype = 0;
    if (G_UNLIKELY(etype == 0)) {
        static const GEnumValue values[] = {
            { GTK_UPDATE_ALWAYS, "GTK_UPDATE_ALWAYS", "always" },
            { GTK_UPDATE_IF_VALID, "GTK_UPDATE_IF_VALID", "if-valid" },
            { 0, NULL, NULL }
        };
        etype = g_enum_register_static (g_intern_static_string ("GtkSpinButtonUpdatePolicy"), values);
    }
    return etype;
}

GType
gtk_spin_type_get_type (void)
{
    static GType etype = 0;
    if (G_UNLIKELY(etype == 0)) {
        static const GEnumValue values[] = {
            { GTK_SPIN_STEP_FORWARD, "GTK_SPIN_STEP_FORWARD", "step-forward" },
            { GTK_SPIN_STEP_BACKWARD, "GTK_SPIN_STEP_BACKWARD", "step-backward" },
            { GTK_SPIN_PAGE_FORWARD, "GTK_SPIN_PAGE_FORWARD", "page-forward" },
            { GTK_SPIN_PAGE_BACKWARD, "GTK_SPIN_PAGE_BACKWARD", "page-backward" },
            { GTK_SPIN_HOME, "GTK_SPIN_HOME", "home" },
            { GTK_SPIN_END, "GTK_SPIN_END", "end" },
            { GTK_SPIN_USER_DEFINED, "GTK_SPIN_USER_DEFINED", "user-defined" },
            { 0, NULL, NULL }
        };
        etype = g_enum_register_static (g_intern_static_string ("GtkSpinType"), values);
    }
    return etype;
}

/* enumerations from "gtktextbuffer.h" */
GType
gtk_text_buffer_target_info_get_type (void)
{
    static GType etype = 0;
    if (G_UNLIKELY(etype == 0)) {
        static const GEnumValue values[] = {
            { GTK_TEXT_BUFFER_TARGET_INFO_BUFFER_CONTENTS, "GTK_TEXT_BUFFER_TARGET_INFO_BUFFER_CONTENTS", "buffer-contents" },
            { GTK_TEXT_BUFFER_TARGET_INFO_RICH_TEXT, "GTK_TEXT_BUFFER_TARGET_INFO_RICH_TEXT", "rich-text" },
            { GTK_TEXT_BUFFER_TARGET_INFO_TEXT, "GTK_TEXT_BUFFER_TARGET_INFO_TEXT", "text" },
            { 0, NULL, NULL }
        };
        etype = g_enum_register_static (g_intern_static_string ("GtkTextBufferTargetInfo"), values);
    }
    return etype;
}

/* enumerations from "gtktextiter.h" */
GType
gtk_text_search_flags_get_type (void)
{
    static GType etype = 0;
    if (G_UNLIKELY(etype == 0)) {
        static const GFlagsValue values[] = {
            { GTK_TEXT_SEARCH_VISIBLE_ONLY, "GTK_TEXT_SEARCH_VISIBLE_ONLY", "visible-only" },
            { GTK_TEXT_SEARCH_TEXT_ONLY, "GTK_TEXT_SEARCH_TEXT_ONLY", "text-only" },
            { GTK_TEXT_SEARCH_CASE_INSENSITIVE, "GTK_TEXT_SEARCH_CASE_INSENSITIVE", "case-insensitive" },
            { 0, NULL, NULL }
        };
        etype = g_flags_register_static (g_intern_static_string ("GtkTextSearchFlags"), values);
    }
    return etype;
}

/* enumerations from "gtktextview.h" */
GType
gtk_text_window_type_get_type (void)
{
    static GType etype = 0;
    if (G_UNLIKELY(etype == 0)) {
        static const GEnumValue values[] = {
            { GTK_TEXT_WINDOW_PRIVATE, "GTK_TEXT_WINDOW_PRIVATE", "private" },
            { GTK_TEXT_WINDOW_WIDGET, "GTK_TEXT_WINDOW_WIDGET", "widget" },
            { GTK_TEXT_WINDOW_TEXT, "GTK_TEXT_WINDOW_TEXT", "text" },
            { GTK_TEXT_WINDOW_LEFT, "GTK_TEXT_WINDOW_LEFT", "left" },
            { GTK_TEXT_WINDOW_RIGHT, "GTK_TEXT_WINDOW_RIGHT", "right" },
            { GTK_TEXT_WINDOW_TOP, "GTK_TEXT_WINDOW_TOP", "top" },
            { GTK_TEXT_WINDOW_BOTTOM, "GTK_TEXT_WINDOW_BOTTOM", "bottom" },
            { 0, NULL, NULL }
        };
        etype = g_enum_register_static (g_intern_static_string ("GtkTextWindowType"), values);
    }
    return etype;
}

/* enumerations from "gtktoolbar.h" */
GType
gtk_toolbar_space_style_get_type (void)
{
    static GType etype = 0;
    if (G_UNLIKELY(etype == 0)) {
        static const GEnumValue values[] = {
            { GTK_TOOLBAR_SPACE_EMPTY, "GTK_TOOLBAR_SPACE_EMPTY", "empty" },
            { GTK_TOOLBAR_SPACE_LINE, "GTK_TOOLBAR_SPACE_LINE", "line" },
            { 0, NULL, NULL }
        };
        etype = g_enum_register_static (g_intern_static_string ("GtkToolbarSpaceStyle"), values);
    }
    return etype;
}

/* enumerations from "gtktoolpalette.h" */
GType
gtk_tool_palette_drag_targets_get_type (void)
{
    static GType etype = 0;
    if (G_UNLIKELY(etype == 0)) {
        static const GFlagsValue values[] = {
            { GTK_TOOL_PALETTE_DRAG_ITEMS, "GTK_TOOL_PALETTE_DRAG_ITEMS", "items" },
            { GTK_TOOL_PALETTE_DRAG_GROUPS, "GTK_TOOL_PALETTE_DRAG_GROUPS", "groups" },
            { 0, NULL, NULL }
        };
        etype = g_flags_register_static (g_intern_static_string ("GtkToolPaletteDragTargets"), values);
    }
    return etype;
}

/* enumerations from "gtktreemodel.h" */
GType
gtk_tree_model_flags_get_type (void)
{
    static GType etype = 0;
    if (G_UNLIKELY(etype == 0)) {
        static const GFlagsValue values[] = {
            { GTK_TREE_MODEL_ITERS_PERSIST, "GTK_TREE_MODEL_ITERS_PERSIST", "iters-persist" },
            { GTK_TREE_MODEL_LIST_ONLY, "GTK_TREE_MODEL_LIST_ONLY", "list-only" },
            { 0, NULL, NULL }
        };
        etype = g_flags_register_static (g_intern_static_string ("GtkTreeModelFlags"), values);
    }
    return etype;
}

/* enumerations from "gtktreeview.h" */
GType
gtk_tree_view_drop_position_get_type (void)
{
    static GType etype = 0;
    if (G_UNLIKELY(etype == 0)) {
        static const GEnumValue values[] = {
            { GTK_TREE_VIEW_DROP_BEFORE, "GTK_TREE_VIEW_DROP_BEFORE", "before" },
            { GTK_TREE_VIEW_DROP_AFTER, "GTK_TREE_VIEW_DROP_AFTER", "after" },
            { GTK_TREE_VIEW_DROP_INTO_OR_BEFORE, "GTK_TREE_VIEW_DROP_INTO_OR_BEFORE", "into-or-before" },
            { GTK_TREE_VIEW_DROP_INTO_OR_AFTER, "GTK_TREE_VIEW_DROP_INTO_OR_AFTER", "into-or-after" },
            { 0, NULL, NULL }
        };
        etype = g_enum_register_static (g_intern_static_string ("GtkTreeViewDropPosition"), values);
    }
    return etype;
}

/* enumerations from "gtktreeviewcolumn.h" */
GType
gtk_tree_view_column_sizing_get_type (void)
{
    static GType etype = 0;
    if (G_UNLIKELY(etype == 0)) {
        static const GEnumValue values[] = {
            { GTK_TREE_VIEW_COLUMN_GROW_ONLY, "GTK_TREE_VIEW_COLUMN_GROW_ONLY", "grow-only" },
            { GTK_TREE_VIEW_COLUMN_AUTOSIZE, "GTK_TREE_VIEW_COLUMN_AUTOSIZE", "autosize" },
            { GTK_TREE_VIEW_COLUMN_FIXED, "GTK_TREE_VIEW_COLUMN_FIXED", "fixed" },
            { 0, NULL, NULL }
        };
        etype = g_enum_register_static (g_intern_static_string ("GtkTreeViewColumnSizing"), values);
    }
    return etype;
}

/* enumerations from "gtkuimanager.h" */
GType
gtk_ui_manager_item_type_get_type (void)
{
    static GType etype = 0;
    if (G_UNLIKELY(etype == 0)) {
        static const GFlagsValue values[] = {
            { GTK_UI_MANAGER_AUTO, "GTK_UI_MANAGER_AUTO", "auto" },
            { GTK_UI_MANAGER_MENUBAR, "GTK_UI_MANAGER_MENUBAR", "menubar" },
            { GTK_UI_MANAGER_MENU, "GTK_UI_MANAGER_MENU", "menu" },
            { GTK_UI_MANAGER_TOOLBAR, "GTK_UI_MANAGER_TOOLBAR", "toolbar" },
            { GTK_UI_MANAGER_PLACEHOLDER, "GTK_UI_MANAGER_PLACEHOLDER", "placeholder" },
            { GTK_UI_MANAGER_POPUP, "GTK_UI_MANAGER_POPUP", "popup" },
            { GTK_UI_MANAGER_MENUITEM, "GTK_UI_MANAGER_MENUITEM", "menuitem" },
            { GTK_UI_MANAGER_TOOLITEM, "GTK_UI_MANAGER_TOOLITEM", "toolitem" },
            { GTK_UI_MANAGER_SEPARATOR, "GTK_UI_MANAGER_SEPARATOR", "separator" },
            { GTK_UI_MANAGER_ACCELERATOR, "GTK_UI_MANAGER_ACCELERATOR", "accelerator" },
            { GTK_UI_MANAGER_POPUP_WITH_ACCELS, "GTK_UI_MANAGER_POPUP_WITH_ACCELS", "popup-with-accels" },
            { 0, NULL, NULL }
        };
        etype = g_flags_register_static (g_intern_static_string ("GtkUIManagerItemType"), values);
    }
    return etype;
}

/* enumerations from "gtkwidget.h" */
GType
gtk_widget_help_type_get_type (void)
{
    static GType etype = 0;
    if (G_UNLIKELY(etype == 0)) {
        static const GEnumValue values[] = {
            { GTK_WIDGET_HELP_TOOLTIP, "GTK_WIDGET_HELP_TOOLTIP", "tooltip" },
            { GTK_WIDGET_HELP_WHATS_THIS, "GTK_WIDGET_HELP_WHATS_THIS", "whats-this" },
            { 0, NULL, NULL }
        };
        etype = g_enum_register_static (g_intern_static_string ("GtkWidgetHelpType"), values);
    }
    return etype;
}



/* Generated data ends here */

