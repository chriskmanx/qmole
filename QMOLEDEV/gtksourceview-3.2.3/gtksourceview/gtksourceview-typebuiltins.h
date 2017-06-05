


#ifndef __GTKSOURCEVIEW_TYPEBUILTINS_H__
#define __GTKSOURCEVIEW_TYPEBUILTINS_H__

#include <gtksourceview/gtksourcebuffer.h>
G_BEGIN_DECLS
#define GTK_SOURCE_TYPE_BRACKET_MATCH_TYPE gtk_source_bracket_match_type_get_type()
GType gtk_source_bracket_match_type_get_type (void);
G_END_DECLS

#include <gtksourceview/gtksourcecompletioncontext.h>
G_BEGIN_DECLS
#define GTK_SOURCE_TYPE_COMPLETION_ACTIVATION gtk_source_completion_activation_get_type()
GType gtk_source_completion_activation_get_type (void);
G_END_DECLS

#include <gtksourceview/gtksourcecompletion.h>
G_BEGIN_DECLS
#define GTK_SOURCE_TYPE_COMPLETION_ERROR gtk_source_completion_error_get_type()
GType gtk_source_completion_error_get_type (void);
G_END_DECLS

#include <gtksourceview/gtksourcegutterrenderer.h>
G_BEGIN_DECLS
#define GTK_SOURCE_TYPE_GUTTER_RENDERER_STATE gtk_source_gutter_renderer_state_get_type()
GType gtk_source_gutter_renderer_state_get_type (void);
G_END_DECLS

G_BEGIN_DECLS
#define GTK_SOURCE_TYPE_GUTTER_RENDERER_ALIGNMENT_MODE gtk_source_gutter_renderer_alignment_mode_get_type()
GType gtk_source_gutter_renderer_alignment_mode_get_type (void);
G_END_DECLS

#include <gtksourceview/gtksourceview.h>
G_BEGIN_DECLS
#define GTK_SOURCE_TYPE_VIEW_GUTTER_POSITION gtk_source_view_gutter_position_get_type()
GType gtk_source_view_gutter_position_get_type (void);
G_END_DECLS

G_BEGIN_DECLS
#define GTK_SOURCE_TYPE_SMART_HOME_END_TYPE gtk_source_smart_home_end_type_get_type()
GType gtk_source_smart_home_end_type_get_type (void);
G_END_DECLS

G_BEGIN_DECLS
#define GTK_SOURCE_TYPE_DRAW_SPACES_FLAGS gtk_source_draw_spaces_flags_get_type()
GType gtk_source_draw_spaces_flags_get_type (void);
G_END_DECLS

#endif /* __GTKSOURCEVIEW_TYPEBUILTINS_H__ */



