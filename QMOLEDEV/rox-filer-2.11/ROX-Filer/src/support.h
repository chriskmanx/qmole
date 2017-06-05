/*
 * ROX-Filer, filer for the ROX desktop project
 * By Thomas Leonard, <tal197@users.sourceforge.net>.
 */

#ifndef _SUPPORT_H
#define _SUPPORT_H

#define PRETTY_SIZE_LIMIT 10000
#define TIME_FORMAT "%T %d %b %Y"

#include <glib-object.h>

XMLwrapper *xml_cache_load(const gchar *pathname);
int save_xml_file(xmlDocPtr doc, const gchar *filename);
xmlDocPtr soap_new(xmlNodePtr *ret_body);
char *pathdup(const char *path);
const guchar *make_path(const char *dir, const char *leaf);
const char *our_host_name(void);
const char *our_host_name_for_dnd(void);
void debug_free_string(void *data);
const char *user_name(uid_t uid);
const char *group_name(gid_t gid);
const char *format_size(off_t size);
const char *format_size_aligned(off_t size);
const gchar *format_double_size(double size);
char *fork_exec_wait(const char **argv);
const char *pretty_permissions(mode_t m);
gint applicable(uid_t uid, gid_t gid);
char *get_local_path(const EscapedPath *escaped_uri);
void close_on_exec(int fd, gboolean close);
void set_blocking(int fd, gboolean blocking);
char *pretty_time(const time_t *time);
guchar *copy_file(const guchar *from, const guchar *to);
guchar *shell_escape(const guchar *word);
gboolean is_sub_dir(const char *sub, const char *parent);
gboolean in_list(const guchar *item, const guchar *list);
GPtrArray *split_path(const guchar *path);
guchar *get_relative_path(const guchar *from, const guchar *to);
int text_to_boolean(const char *text, int defvalue);
char *readlink_dup(const char *path);
gchar *to_utf8(const gchar *src);
gchar *from_utf8(const gchar *src);
void ensure_utf8(gchar **string);
char *md5_hash(const char *message);
gchar *expand_path(const gchar *path);
void destroy_glist(GList **list);
void null_g_free(gpointer p);
CollateKey *collate_key_new(const guchar *name);
void collate_key_free(CollateKey *key);
int collate_key_cmp(const CollateKey *n1, const CollateKey *n2,
		    gboolean caps_first);
gboolean file_exists(const char *path);
GPtrArray *list_dir(const guchar *path);
gint strcmp2(gconstpointer a, gconstpointer b);
int stat_with_timeout(const char *path, struct stat *info);

EscapedPath *escape_uri_path(const char *path);
EscapedPath *encode_path_as_uri(const guchar *path);
gchar *unescape_uri(const EscapedPath *uri);
gchar *get_uri_scheme(const EscapedPath *uri);
gboolean available_in_path(const char *file);
char *get_value_from_desktop_file(const char *path,
				  const char *section,
				  const char *key,
				  GError **error);
gboolean get_values_from_desktop_file(const char *path,
				      GError **error,
				      const char *section,
				      const char *key,
				      gchar **value, ...);
gchar *build_command_with_path(const char *cmd, const char *path);
gchar *find_app(const char *appname);

#endif /* _SUPPORT_H */
