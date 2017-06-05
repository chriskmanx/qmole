
/* Generated data (by glib-mkenums) */

#include <glib-object.h>
#include "libgnometypebuiltins.h"



/* enumerations from "gnome-triggers.h" */
static const GEnumValue _gnome_trigger_type_values[] = {
  { GTRIG_NONE, "GTRIG_NONE", "none" },
  { GTRIG_FUNCTION, "GTRIG_FUNCTION", "function" },
  { GTRIG_COMMAND, "GTRIG_COMMAND", "command" },
  { GTRIG_MEDIAPLAY, "GTRIG_MEDIAPLAY", "mediaplay" },
  { 0, NULL, NULL }
};

GType
gnome_trigger_type_get_type (void)
{
  static GType type = 0;

  if (!type)
    type = g_enum_register_static ("GnomeTriggerType", _gnome_trigger_type_values);

  return type;
}



/* enumerations from "gnome-program.h" */
static const GEnumValue _gnome_file_domain_values[] = {
  { GNOME_FILE_DOMAIN_UNKNOWN, "GNOME_FILE_DOMAIN_UNKNOWN", "unknown" },
  { GNOME_FILE_DOMAIN_LIBDIR, "GNOME_FILE_DOMAIN_LIBDIR", "libdir" },
  { GNOME_FILE_DOMAIN_DATADIR, "GNOME_FILE_DOMAIN_DATADIR", "datadir" },
  { GNOME_FILE_DOMAIN_SOUND, "GNOME_FILE_DOMAIN_SOUND", "sound" },
  { GNOME_FILE_DOMAIN_PIXMAP, "GNOME_FILE_DOMAIN_PIXMAP", "pixmap" },
  { GNOME_FILE_DOMAIN_CONFIG, "GNOME_FILE_DOMAIN_CONFIG", "config" },
  { GNOME_FILE_DOMAIN_HELP, "GNOME_FILE_DOMAIN_HELP", "help" },
  { GNOME_FILE_DOMAIN_APP_LIBDIR, "GNOME_FILE_DOMAIN_APP_LIBDIR", "app-libdir" },
  { GNOME_FILE_DOMAIN_APP_DATADIR, "GNOME_FILE_DOMAIN_APP_DATADIR", "app-datadir" },
  { GNOME_FILE_DOMAIN_APP_SOUND, "GNOME_FILE_DOMAIN_APP_SOUND", "app-sound" },
  { GNOME_FILE_DOMAIN_APP_PIXMAP, "GNOME_FILE_DOMAIN_APP_PIXMAP", "app-pixmap" },
  { GNOME_FILE_DOMAIN_APP_CONFIG, "GNOME_FILE_DOMAIN_APP_CONFIG", "app-config" },
  { GNOME_FILE_DOMAIN_APP_HELP, "GNOME_FILE_DOMAIN_APP_HELP", "app-help" },
  { 0, NULL, NULL }
};

GType
gnome_file_domain_get_type (void)
{
  static GType type = 0;

  if (!type)
    type = g_enum_register_static ("GnomeFileDomain", _gnome_file_domain_values);

  return type;
}



/* enumerations from "gnome-help.h" */
static const GEnumValue _gnome_help_error_values[] = {
  { GNOME_HELP_ERROR_INTERNAL, "GNOME_HELP_ERROR_INTERNAL", "internal" },
  { GNOME_HELP_ERROR_NOT_FOUND, "GNOME_HELP_ERROR_NOT_FOUND", "not-found" },
  { 0, NULL, NULL }
};

GType
gnome_help_error_get_type (void)
{
  static GType type = 0;

  if (!type)
    type = g_enum_register_static ("GnomeHelpError", _gnome_help_error_values);

  return type;
}



/* enumerations from "gnome-url.h" */
static const GEnumValue _gnome_url_error_values[] = {
  { GNOME_URL_ERROR_PARSE, "GNOME_URL_ERROR_PARSE", "parse" },
  { 0, NULL, NULL }
};

GType
gnome_url_error_get_type (void)
{
  static GType type = 0;

  if (!type)
    type = g_enum_register_static ("GnomeURLError", _gnome_url_error_values);

  return type;
}



/* Generated data ends here */

