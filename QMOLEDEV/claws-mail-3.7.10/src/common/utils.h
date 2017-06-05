/*
 * Sylpheed -- a GTK+ based, lightweight, and fast e-mail client
 * Copyright (C) 1999-2011 Hiroyuki Yamamoto and the Claws Mail team
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 * 
 */

#ifndef __UTILS_H__
#define __UTILS_H__

#ifdef HAVE_CONFIG_H
#  include "config.h"
#endif

#ifdef HAVE_BACKTRACE
#include <execinfo.h>
#endif

#include <glib.h>
#include <glib-object.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <unistd.h>
#include <sys/types.h>
#include <dirent.h>
#include <time.h>
#if HAVE_ALLOCA_H
#  include <alloca.h>
#endif
#if HAVE_WCHAR_H
#  include <wchar.h>
#endif

#ifdef G_OS_WIN32

#define fsync _commit

#if GLIB_CHECK_VERSION (2, 13, 2)
#define pipe(phandles)  _pipe (phandles, 4096, _O_BINARY)
#endif
#endif
/* Wrappers for C library function that take pathname arguments. */
#if GLIB_CHECK_VERSION(2, 6, 0)
#  include <glib/gstdio.h>
#else

#define g_open		open
#define g_rename	rename
#define g_mkdir		mkdir
#define g_stat		stat
#define g_lstat		lstat
#define g_unlink	unlink
#define g_remove	remove
#define g_rmdir		rmdir
#define g_fopen		fopen
#define g_freopen	freopen

#endif /* GLIB_CHECK_VERSION */

#if !GLIB_CHECK_VERSION(2, 7, 0)

#ifdef G_OS_UNIX
#define g_chdir		chdir
#define g_chmod		chmod
#else
gint g_chdir	(const gchar	*path);
gint g_chmod	(const gchar	*path,
		 gint		 mode);
#endif /* G_OS_UNIX */

#endif /* !GLIB_CHECK_VERSION */

/* why is this sometimes undefined !? */
#ifndef G_MAXOFFSET
typedef gint64 goffset;
#define G_MINOFFSET	G_MININT64
#define G_MAXOFFSET	G_MAXINT64
#endif

#ifndef HAVE_U32_TYPEDEF
  #undef u32	    /* maybe there is a macro with this name */
  typedef guint32 u32;
  #define HAVE_U32_TYPEDEF
#endif

#ifndef BIG_ENDIAN_HOST
  #if (G_BYTE_ORDER == G_BIG_ENDIAN)
    #define BIG_ENDIAN_HOST 1
  #endif
#endif

#define CHDIR_RETURN_IF_FAIL(dir) \
{ \
	if (change_dir(dir) < 0) return; \
}

#define CHDIR_RETURN_VAL_IF_FAIL(dir, val) \
{ \
	if (change_dir(dir) < 0) return val; \
}

#define CHDIR_EXEC_CODE_RETURN_VAL_IF_FAIL(dir, val, code) \
{ \
	if (change_dir(dir) < 0) { \
		code \
		return val; \
	} \
}

#define Xalloca(ptr, size, iffail) \
{ \
	if ((ptr = alloca(size)) == NULL) { \
		g_warning("can't allocate memory\n"); \
		iffail; \
	} \
}

#define Xstrdup_a(ptr, str, iffail) \
{ \
	gchar *__tmp; \
 \
	if ((__tmp = alloca(strlen(str) + 1)) == NULL) { \
		g_warning("can't allocate memory\n"); \
		iffail; \
	} else \
		strcpy(__tmp, str); \
 \
	ptr = __tmp; \
}

#define Xstrndup_a(ptr, str, len, iffail) \
{ \
	gchar *__tmp; \
 \
	if ((__tmp = alloca(len + 1)) == NULL) { \
		g_warning("can't allocate memory\n"); \
		iffail; \
	} else { \
		strncpy(__tmp, str, len); \
		__tmp[len] = '\0'; \
	} \
 \
	ptr = __tmp; \
}

#define Xstrcat_a(ptr, str1, str2, iffail) \
{ \
	gchar *__tmp; \
	gint len1, len2; \
 \
	len1 = strlen(str1); \
	len2 = strlen(str2); \
	if ((__tmp = alloca(len1 + len2 + 1)) == NULL) { \
		g_warning("can't allocate memory\n"); \
		iffail; \
	} else { \
		memcpy(__tmp, str1, len1); \
		memcpy(__tmp + len1, str2, len2 + 1); \
	} \
 \
	ptr = __tmp; \
}

#define AUTORELEASE_STR(str, iffail) \
{ \
	gchar *__str; \
	Xstrdup_a(__str, str, iffail); \
	g_free(str); \
	str = __str; \
}

#define FILE_OP_ERROR(file, func) \
{ \
	g_printerr("%s: ", file); \
	fflush(stderr); \
	perror(func); \
}

#define IS_ASCII(c) (((guchar) c) <= 0177 ? 1 : 0)

/* from NetworkManager */
#if (defined(HAVE_BACKTRACE) && !defined(__FreeBSD__))
#define print_backtrace()						\
G_STMT_START								\
{									\
	void *_call_stack[512];						\
	int  _call_stack_size;						\
	char **_symbols;						\
	_call_stack_size = backtrace (_call_stack,			\
				      G_N_ELEMENTS (_call_stack));	\
	_symbols = backtrace_symbols (_call_stack, _call_stack_size);	\
	if (_symbols != NULL)						\
	{								\
		int _i;							\
		_i = 0;							\
		g_print ("traceback:\n");				\
		while (_i < _call_stack_size)				\
		{							\
			g_print ("%d:\t%s\n", _i, _symbols[_i]);	\
			_i++;						\
		}							\
		free (_symbols);					\
	}								\
}									\
G_STMT_END
#else
#define print_backtrace()						\
G_STMT_START								\
{									\
}									\
G_STMT_END
#endif


#define cm_return_val_if_fail(expr,val) G_STMT_START {			\
	if (!(expr)) {							\
		g_print("%s:%d Condition %s failed\n", __FILE__, __LINE__, #expr);\
		print_backtrace();					\
		g_print("\n");						\
		return val;						\
	} 								\
} G_STMT_END

#define cm_return_if_fail(expr) G_STMT_START {				\
	if (!(expr)) {							\
		g_print("%s:%d Condition %s failed\n", __FILE__, __LINE__, #expr);\
		print_backtrace();					\
		g_print("\n");						\
		return;							\
	} 								\
} G_STMT_END

#ifdef __cplusplus
extern "C" {
#endif

typedef gpointer (*GNodeMapFunc)	(gpointer nodedata, gpointer data);

/* debug functions */
void debug_set_mode		(gboolean mode);
gboolean debug_get_mode		(void);

#ifndef __CYGWIN__
#define debug_print \
	debug_print_real("%s:%d:", debug_srcname(__FILE__), __LINE__), \
	debug_print_real
#else
  /* FIXME: cygwin: why debug_srcname couldn't be resolved in library? */
#define debug_print \
	debug_print_real("%s:%d:", __FILE__, __LINE__), \
	debug_print_real
#endif

/* for macro expansion */
#define Str(x)	#x
#define Xstr(x)	Str(x)


/* System related stuff.  */

gboolean superuser_p (void);


/* String utilities.  */

void list_free_strings		(GList		*list);
void slist_free_strings		(GSList		*list);

void hash_free_strings		(GHashTable	*table);

gint str_case_equal		(gconstpointer	 v,
				 gconstpointer	 v2);
guint str_case_hash		(gconstpointer	 key);

void ptr_array_free_strings	(GPtrArray	*array);

/* number-string conversion */
gint to_number			(const gchar *nstr);
gchar *itos_buf			(gchar	     *nstr,
				 gint	      n);
gchar *itos			(gint	      n);
gchar *to_human_readable	(goffset      size);

/* alternative string functions */
gint strcmp2		(const gchar	*s1,
			 const gchar	*s2);
gchar *strstr2		(const gchar	*s1,
			 const gchar	*s2);
gint path_cmp		(const gchar	*s1,
			 const gchar	*s2);
gchar *strretchomp	(gchar		*str);
gchar *strtailchomp	(gchar		*str,
			 gchar		 tail_char);
gchar *strcrchomp	(gchar		*str);
gint file_strip_crs	(const gchar 	*file);
gchar *strcasestr	(const gchar	*haystack,
			 const gchar	*needle);
gpointer my_memmem	(gconstpointer	 haystack,
			 size_t		 haystacklen,
			 gconstpointer	 needle,
			 size_t		 needlelen);
gchar *strncpy2		(gchar		*dest,
			 const gchar	*src,
			 size_t		 n);

gboolean is_next_nonascii	(const gchar *s);
gint get_next_word_len		(const gchar *s);

/* functions for string parsing */
gint subject_compare			(const gchar	*s1,
					 const gchar	*s2);
gint subject_compare_for_sort		(const gchar	*s1,
					 const gchar	*s2);
void trim_subject			(gchar		*str);
void eliminate_parenthesis		(gchar		*str,
					 gchar		 op,
					 gchar		 cl);
void extract_parenthesis		(gchar		*str,
					 gchar		 op,
					 gchar		 cl);

void extract_quote			(gchar		*str,
					 gchar		 quote_chr);
void eliminate_address_comment		(gchar		*str);
gchar *strchr_with_skip_quote		(const gchar	*str,
					 gint		 quote_chr,
					 gint		 c);
void extract_address			(gchar		*str);
void extract_list_id_str		(gchar		*str);

GSList *address_list_append		(GSList		*addr_list,
					 const gchar	*str);
GSList *address_list_append_with_comments(GSList	*addr_list,
					 const gchar	*str);
GSList *references_list_prepend		(GSList		*msgid_list,
					 const gchar	*str);
GSList *references_list_append		(GSList		*msgid_list,
					 const gchar	*str);
GSList *newsgroup_list_append		(GSList		*group_list,
					 const gchar	*str);

GList *add_history			(GList		*list,
					 const gchar	*str);

void remove_return			(gchar		*str);
void remove_space			(gchar		*str);
void unfold_line			(gchar		*str);
void subst_char				(gchar		*str,
					 gchar		 orig,
					 gchar		 subst);
void subst_chars			(gchar 		*str, 	
					 gchar 		*orig, 
					 gchar 		subst);
void subst_for_filename			(gchar		*str);
void subst_for_shellsafe_filename	(gchar		*str);
gboolean is_ascii_str			(const gchar	*str);
gint get_quote_level			(const gchar	*str,
					 const gchar	*quote_chars);
gint check_line_length			(const gchar	*str,
					 gint		 max_chars,
					 gint		*line);

gchar **strsplit_with_quote		(const gchar	*str,
					 const gchar	*delim,
					 gint		 max_tokens);

gchar *get_abbrev_newsgroup_name	(const gchar	*group,
					 gint		 len);
gchar *trim_string			(const gchar	*str,
					 gint		 len);

GList *uri_list_extract_filenames	(const gchar	*uri_list);
gboolean is_uri_string			(const gchar	*str);
gchar *get_uri_path			(const gchar	*uri);
gint get_uri_len			(const gchar	*str);
void decode_uri				(gchar		*decoded_uri,
					 const gchar	*encoded_uri);
void decode_uri_with_plus		(gchar 		*decoded_uri, 
					 const gchar 	*encoded_uri, 
					 gboolean 	 with_plus);
gint scan_mailto_url			(const gchar	*mailto,
					 gchar	       **from,
					 gchar	       **to,
					 gchar	       **cc,
					 gchar	       **bcc,
					 gchar	       **subject,
					 gchar	       **body,
					 gchar	       ***attach,
					 gchar	       **inreplyto);

/* return static strings */
const gchar *get_home_dir		(void);
const gchar *get_rc_dir			(void);
void  set_rc_dir			(const gchar *dir);
gboolean rc_dir_is_alt			(void);
const gchar *get_mail_base_dir		(void);
const gchar *get_news_cache_dir		(void);
const gchar *get_imap_cache_dir		(void);
const gchar *get_mime_tmp_dir		(void);
const gchar *get_template_dir		(void);
const gchar *get_plugin_dir             (void);
const gchar *get_tmp_dir		(void);
const gchar *get_locale_dir		(void);
gchar *get_tmp_file			(void);
const gchar *get_domain_name		(void);
const gchar *get_desktop_file(void);
#ifdef G_OS_WIN32
const gchar *get_themes_dir             (void);
const gchar *get_cert_file		(void);
#endif
/* file / directory handling */
off_t get_file_size		(const gchar	*file);
off_t get_file_size_as_crlf	(const gchar	*file);

time_t get_file_mtime		(const gchar *file);

gboolean file_exist		(const gchar	*file,
				 gboolean	 allow_fifo);
gboolean is_relative_filename   (const gchar *file);
gboolean is_dir_exist		(const gchar	*dir);
gboolean is_file_entry_exist	(const gchar	*file);
gboolean dirent_is_regular_file	(struct dirent	*d);

#define is_file_exist(file)		file_exist(file, FALSE)
#define is_file_or_fifo_exist(file)	file_exist(file, TRUE)

gint change_dir			(const gchar	*dir);
gint make_dir			(const gchar	*dir);
gint make_dir_hier		(const gchar	*dir);
gint remove_all_files		(const gchar	*dir);
gint remove_numbered_files	(const gchar	*dir,
				 guint		 first,
				 guint		 last);
gint remove_numbered_files_not_in_list(const gchar *dir,
				       GSList *numberlist);
gint remove_all_numbered_files	(const gchar	*dir);
gint remove_dir_recursive	(const gchar	*dir);
gint append_file		(const gchar	*src,
				 const gchar	*dest,
				 gboolean	 keep_backup);
gint rename_force		(const gchar	*oldpath,
				 const gchar	*newpath);
gint copy_file			(const gchar	*src,
				 const gchar	*dest,
				 gboolean	 keep_backup);
gint move_file			(const gchar	*src,
				 const gchar	*dest,
				 gboolean	 overwrite);
gint copy_dir			(const gchar	*src,
				 const gchar	*dest);
gint copy_file_part_to_fp	(FILE		*fp,
				 off_t		 offset,
				 size_t		 length,
				 FILE		*dest_fp);
gint copy_file_part		(FILE		*fp,
				 off_t		 offset,
				 size_t		 length,
				 const gchar	*dest);

gchar *canonicalize_str		(const gchar	*str);
gint canonicalize_file		(const gchar	*src,
				 const gchar	*dest);
gint canonicalize_file_replace	(const gchar	*file);

gchar *normalize_newlines	(const gchar	*str);

gchar *get_outgoing_rfc2822_str	(FILE		*fp);

gint change_file_mode_rw	(FILE		*fp,
				 const gchar	*file);
FILE *my_tmpfile		(void);
FILE *get_tmpfile_in_dir	(const gchar 	*dir,
				 gchar	       **filename);
FILE *str_open_as_stream	(const gchar	*str);
gint str_write_to_file		(const gchar	*str,
				 const gchar	*file);
gchar *file_read_to_str		(const gchar	*file);
gchar *file_read_stream_to_str	(FILE		*fp);
gchar *file_read_to_str_no_recode(const gchar *file);
gchar *file_read_stream_to_str_no_recode(FILE *fp);

char *fgets_crlf(char *buf, int size, FILE *stream);

/* process execution */
gint execute_command_line	(const gchar	*cmdline,
				 gboolean	 async);
gchar *get_command_output	(const gchar	*cmdline);

/* open URI with external browser */
gint open_uri(const gchar *uri, const gchar *cmdline);
/* open file with text editor */
gint open_txt_editor(const gchar *filepath, const gchar *cmdline);

/* time functions */
time_t remote_tzoffset_sec	(const gchar	*zone);
time_t tzoffset_sec		(time_t		*now);
gchar *tzoffset			(time_t		*now);
void get_rfc822_date		(gchar		*buf,
				 gint		 len);

size_t fast_strftime		(gchar 			*buf, 
				 gint 			 buflen, 
				 const gchar 		*format, 
				 struct tm 		*lt);

/* debugging */
void debug_print_real	(const gchar *format, ...) G_GNUC_PRINTF(1, 2);
const char * debug_srcname (const char *file);

/* subject threading */
void * subject_table_lookup(GHashTable *subject_table, gchar * subject);
void subject_table_insert(GHashTable *subject_table, gchar * subject,
			  void * data);
void subject_table_remove(GHashTable *subject_table, gchar * subject);
void utils_free_regex(void);
gint subject_get_prefix_length (const gchar *subject);

/* quoting recognition */
const gchar * line_has_quote_char	(const gchar *str,
					 const gchar *quote_chars);

gint g_int_compare	(gconstpointer a, gconstpointer b);

gchar *generate_msgid		(gchar *buf, gint len, gchar *user_addr);
gchar *generate_mime_boundary	(const gchar *prefix);

gint quote_cmd_argument(gchar * result, guint size,
			const gchar * path);
GNode *g_node_map(GNode *node, GNodeMapFunc func, gpointer data);

gboolean get_hex_value(guchar *out, gchar c1, gchar c2);
void get_hex_str(gchar *out, guchar ch);

/* auto pointer for containers that support GType system */

#define G_TYPE_AUTO_POINTER	g_auto_pointer_register()
typedef struct AutoPointer	GAuto;
GType g_auto_pointer_register		(void);
GAuto *g_auto_pointer_new		(gpointer pointer);
GAuto *g_auto_pointer_new_with_free	(gpointer p, 
					 GFreeFunc free);
gpointer g_auto_pointer_get_ptr		(GAuto *auto_ptr);
GAuto *g_auto_pointer_copy		(GAuto *auto_ptr);
void g_auto_pointer_free		(GAuto *auto_ptr);
void replace_returns			(gchar *str);
gboolean get_uri_part	(const gchar *start,
		    	 const gchar *scanpos,
		     	 const gchar **bp,
		    	 const gchar **ep,
		   	 gboolean hdr);
gchar *make_uri_string	(const gchar *bp,
			 const gchar *ep);
gboolean get_email_part	(const gchar *start, 
			 const gchar *scanpos,
			 const gchar **bp, 
			 const gchar **ep,
			 gboolean hdr);
gchar *make_email_string(const gchar *bp,
			 const gchar *ep);
gchar *make_http_string (const gchar *bp,
			 const gchar *ep);

gchar *mailcap_get_command_for_type(const gchar *type, 
				    const gchar *file_to_open);
void mailcap_update_default	   (const gchar *type,
				    const gchar *command);

gboolean file_is_email(const gchar *filename);
gboolean sc_g_list_bigger(GList *list, gint max);
gboolean sc_g_slist_bigger(GSList *list, gint max);

int claws_unlink(const gchar *filename);
#ifdef __cplusplus
}
#endif

#endif /* __UTILS_H__ */
