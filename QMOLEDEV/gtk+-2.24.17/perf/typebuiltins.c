
/* Generated data (by glib-mkenums) */

#include "gtkwidgetprofiler.h"

/* enumerations from "gtkwidgetprofiler.h" */
GType
gtk_widget_profiler_report_get_type (void)
{
    static GType etype = 0;
    if (G_UNLIKELY(etype == 0)) {
        static const GEnumValue values[] = {
            { GTK_WIDGET_PROFILER_REPORT_CREATE, "GTK_WIDGET_PROFILER_REPORT_CREATE", "create" },
            { GTK_WIDGET_PROFILER_REPORT_MAP, "GTK_WIDGET_PROFILER_REPORT_MAP", "map" },
            { GTK_WIDGET_PROFILER_REPORT_EXPOSE, "GTK_WIDGET_PROFILER_REPORT_EXPOSE", "expose" },
            { GTK_WIDGET_PROFILER_REPORT_DESTROY, "GTK_WIDGET_PROFILER_REPORT_DESTROY", "destroy" },
            { 0, NULL, NULL }
        };
        etype = g_enum_register_static (g_intern_static_string ("GtkWidgetProfilerReport"), values);
    }
    return etype;
}

#define __TYPE_BUILTINS_C__

/* Generated data ends here */

