
/* Generated data (by glib-mkenums) */

#include "gconf-client.h"

/* enumerations from "gconf-value.h" */
GType
gconf_value_type_get_type (void)
{
  static GType etype = 0;
  if (etype == 0) {
    static const GEnumValue values[] = {
      { GCONF_VALUE_INVALID, "GCONF_VALUE_INVALID", "invalid" },
      { GCONF_VALUE_STRING, "GCONF_VALUE_STRING", "string" },
      { GCONF_VALUE_INT, "GCONF_VALUE_INT", "int" },
      { GCONF_VALUE_FLOAT, "GCONF_VALUE_FLOAT", "float" },
      { GCONF_VALUE_BOOL, "GCONF_VALUE_BOOL", "bool" },
      { GCONF_VALUE_SCHEMA, "GCONF_VALUE_SCHEMA", "schema" },
      { GCONF_VALUE_LIST, "GCONF_VALUE_LIST", "list" },
      { GCONF_VALUE_PAIR, "GCONF_VALUE_PAIR", "pair" },
      { 0, NULL, NULL }
    };
    etype = g_enum_register_static ("GConfValueType", values);
  }
  return etype;
}

GType
gconf_unset_flags_get_type (void)
{
  static GType etype = 0;
  if (etype == 0) {
    static const GFlagsValue values[] = {
      { GCONF_UNSET_INCLUDING_SCHEMA_NAMES, "GCONF_UNSET_INCLUDING_SCHEMA_NAMES", "names" },
      { 0, NULL, NULL }
    };
    etype = g_flags_register_static ("GConfUnsetFlags", values);
  }
  return etype;
}


/* enumerations from "gconf-error.h" */
GType
gconf_error_get_type (void)
{
  static GType etype = 0;
  if (etype == 0) {
    static const GEnumValue values[] = {
      { GCONF_ERROR_SUCCESS, "GCONF_ERROR_SUCCESS", "success" },
      { GCONF_ERROR_FAILED, "GCONF_ERROR_FAILED", "failed" },
      { GCONF_ERROR_NO_SERVER, "GCONF_ERROR_NO_SERVER", "no-server" },
      { GCONF_ERROR_NO_PERMISSION, "GCONF_ERROR_NO_PERMISSION", "no-permission" },
      { GCONF_ERROR_BAD_ADDRESS, "GCONF_ERROR_BAD_ADDRESS", "bad-address" },
      { GCONF_ERROR_BAD_KEY, "GCONF_ERROR_BAD_KEY", "bad-key" },
      { GCONF_ERROR_PARSE_ERROR, "GCONF_ERROR_PARSE_ERROR", "parse-error" },
      { GCONF_ERROR_CORRUPT, "GCONF_ERROR_CORRUPT", "corrupt" },
      { GCONF_ERROR_TYPE_MISMATCH, "GCONF_ERROR_TYPE_MISMATCH", "type-mismatch" },
      { GCONF_ERROR_IS_DIR, "GCONF_ERROR_IS_DIR", "is-dir" },
      { GCONF_ERROR_IS_KEY, "GCONF_ERROR_IS_KEY", "is-key" },
      { GCONF_ERROR_OVERRIDDEN, "GCONF_ERROR_OVERRIDDEN", "overridden" },
      { GCONF_ERROR_OAF_ERROR, "GCONF_ERROR_OAF_ERROR", "oaf-error" },
      { GCONF_ERROR_LOCAL_ENGINE, "GCONF_ERROR_LOCAL_ENGINE", "local-engine" },
      { GCONF_ERROR_LOCK_FAILED, "GCONF_ERROR_LOCK_FAILED", "lock-failed" },
      { GCONF_ERROR_NO_WRITABLE_DATABASE, "GCONF_ERROR_NO_WRITABLE_DATABASE", "no-writable-database" },
      { GCONF_ERROR_IN_SHUTDOWN, "GCONF_ERROR_IN_SHUTDOWN", "in-shutdown" },
      { 0, NULL, NULL }
    };
    etype = g_enum_register_static ("GConfError", values);
  }
  return etype;
}


/* enumerations from "gconf-client.h" */
GType
gconf_client_preload_type_get_type (void)
{
  static GType etype = 0;
  if (etype == 0) {
    static const GEnumValue values[] = {
      { GCONF_CLIENT_PRELOAD_NONE, "GCONF_CLIENT_PRELOAD_NONE", "preload-none" },
      { GCONF_CLIENT_PRELOAD_ONELEVEL, "GCONF_CLIENT_PRELOAD_ONELEVEL", "preload-onelevel" },
      { GCONF_CLIENT_PRELOAD_RECURSIVE, "GCONF_CLIENT_PRELOAD_RECURSIVE", "preload-recursive" },
      { 0, NULL, NULL }
    };
    etype = g_enum_register_static ("GConfClientPreloadType", values);
  }
  return etype;
}

GType
gconf_client_error_handling_mode_get_type (void)
{
  static GType etype = 0;
  if (etype == 0) {
    static const GEnumValue values[] = {
      { GCONF_CLIENT_HANDLE_NONE, "GCONF_CLIENT_HANDLE_NONE", "handle-none" },
      { GCONF_CLIENT_HANDLE_UNRETURNED, "GCONF_CLIENT_HANDLE_UNRETURNED", "handle-unreturned" },
      { GCONF_CLIENT_HANDLE_ALL, "GCONF_CLIENT_HANDLE_ALL", "handle-all" },
      { 0, NULL, NULL }
    };
    etype = g_enum_register_static ("GConfClientErrorHandlingMode", values);
  }
  return etype;
}


/* Generated data ends here */

