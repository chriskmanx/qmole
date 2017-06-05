/*
 * ROX-Filer, filer for the ROX desktop project
 * Copyright (C) 2006, Thomas Leonard and others (see changelog for details).
 *
 * This program is free software; you can redistribute it and/or modify it
 * under the terms of the GNU General Public License as published by the Free
 * Software Foundation; either version 2 of the License, or (at your option)
 * any later version.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
 * FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
 * more details.
 *
 * You should have received a copy of the GNU General Public License along with
 * this program; if not, write to the Free Software Foundation, Inc., 59 Temple
 * Place, Suite 330, Boston, MA  02111-1307  USA
 */

/* support.c - (non-GUI) useful routines */

#include "config.h"

#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <netdb.h>
#include <errno.h>
#include <ctype.h>
#include <sys/param.h>
#include <pwd.h>
#include <grp.h>
#include <fcntl.h>
#include <sys/wait.h>
#include <string.h>
#include <time.h>
#include <unistd.h>
#include <libxml/parser.h>
#include <math.h>
#include <sys/mman.h>

#include "global.h"

#include "choices.h"
#include "main.h"
#include "options.h"
#include "support.h"
#include "fscache.h"
#include "main.h"
#include "xml.h"

static GHashTable *uid_hash = NULL;	/* UID -> User name */
static GHashTable *gid_hash = NULL;	/* GID -> Group name */

/* Static prototypes */
static void MD5Transform(guint32 buf[4], guint32 const in[16]);

/****************************************************************
 *			EXTERNAL INTERFACE			*
 ****************************************************************/

/* g_object_unref() the result! */
XMLwrapper *xml_cache_load(const gchar *pathname)
{
	static GFSCache *xml_cache = NULL;

	if (!xml_cache)
		xml_cache = g_fscache_new((GFSLoadFunc) xml_new, NULL, NULL);
	return g_fscache_lookup(xml_cache, pathname);
}

/* Save doc as XML as filename, 0 on success or 1 on failure */
int save_xml_file(xmlDocPtr doc, const gchar *filename)
{
#if LIBXML_VERSION > 20400
	if (xmlSaveFormatFileEnc(filename, doc, NULL, 1) < 0)
		return 1;
#else
	FILE *out;
	
	out = fopen(filename, "w");
	if (!out)
		return 1;

	xmlDocDump(out, doc);  /* Some versions return void */

	if (fclose(out))
		return 1;
#endif

	return 0;
}

/* Create a new SOAP message and return the document and the (empty)
 * body node.
 */
xmlDocPtr soap_new(xmlNodePtr *ret_body)
{
	xmlDocPtr  doc;
	xmlNodePtr root;
	xmlNs	   *env_ns;

	doc = xmlNewDoc("1.0");
	root = xmlNewDocNode(doc, NULL, "Envelope", NULL);
	xmlDocSetRootElement(doc, root);
	
	env_ns = xmlNewNs(root, SOAP_ENV_NS, "env");
	xmlSetNs(root, env_ns);

	*ret_body = xmlNewTextChild(root, env_ns, "Body", NULL);
	xmlNewNs(*ret_body, ROX_NS, "rox");

	return doc;
}

/* Like g_strdup, but does realpath() too (if possible) */
char *pathdup(const char *path)
{
	char real[MAXPATHLEN];

	g_return_val_if_fail(path != NULL, NULL);

	if (realpath(path, real))
		return g_strdup(real);

	return g_strdup(path);
}

/* Join the path to the leaf (adding a / between them) and
 * return a pointer to a static buffer with the result. Buffer is valid
 * until the next call to make_path.
 * The return value may be used as 'dir' for the next call.
 */
const guchar *make_path(const char *dir, const char *leaf)
{
	static GString *buffer = NULL;

	if (!buffer)
		buffer = g_string_new(NULL);

	g_return_val_if_fail(dir != NULL, buffer->str);
	g_return_val_if_fail(leaf != NULL, buffer->str);

	if (buffer->str != dir)
		g_string_assign(buffer, dir);

	if (dir[0] != '/' || dir[1] != '\0')
		g_string_append_c(buffer, '/');	/* For anything except "/" */

	g_string_append(buffer, leaf);

	return buffer->str;
}

/* Return our complete host name for DND */
const char *our_host_name_for_dnd(void)
{
	if (o_dnd_no_hostnames.int_value)
		return "";
	return our_host_name();
}

/* Return our complete host name, unconditionally */
const char *our_host_name(void)
{
	static char *name = NULL;

	if (!name)
	{
		char buffer[4096];

		if (gethostname(buffer, 4096) == 0)
		{
			/* gethostname doesn't always return the full name... */
			struct hostent *ent;

			buffer[4095] = '\0';
			ent = gethostbyname(buffer);
			name = g_strdup(ent ? ent->h_name : buffer);
		}
		else
		{
			g_warning("gethostname() failed - using localhost\n");
			name = g_strdup("localhost");
		}
	}

	return name;
}

void debug_free_string(void *data)
{
	g_print("Freeing string '%s'\n", (char *) data);
	g_free(data);
}

const char *user_name(uid_t uid)
{
	const char *retval;
	
	if (!uid_hash)
		uid_hash = g_hash_table_new(NULL, NULL);

	retval = g_hash_table_lookup(uid_hash, GINT_TO_POINTER(uid));

	if (!retval)
	{
		struct passwd *passwd;

		passwd = getpwuid(uid);
		retval = passwd ? g_strdup(passwd->pw_name)
			       : g_strdup_printf("[%d]", (int) uid);
		g_hash_table_insert(uid_hash, GINT_TO_POINTER(uid),
				(gchar *) retval);
	}

	return retval;
}

const char *group_name(gid_t gid)
{
	const char *retval;
	
	if (!gid_hash)
		gid_hash = g_hash_table_new(NULL, NULL);

	retval = g_hash_table_lookup(gid_hash, GINT_TO_POINTER(gid));

	if (!retval)
	{
		struct group *group;

		group = getgrgid(gid);
		retval = group ? g_strdup(group->gr_name)
			       : g_strdup_printf("[%d]", (int) gid);
		g_hash_table_insert(gid_hash, GINT_TO_POINTER(gid),
				(gchar *) retval);
	}

	return retval;
}

/* Return a string in the form '23 M' in a static buffer valid until
 * the next call.
 */
const char *format_size(off_t size)
{
	static	char *buffer = NULL;
	const char	*units;
	
	if (size >= PRETTY_SIZE_LIMIT)
	{
		size += 1023;
		size >>= 10;
		if (size >= PRETTY_SIZE_LIMIT)
		{
			size += 1023;
			size >>= 10;
			if (size >= PRETTY_SIZE_LIMIT)
			{
				size += 1023;
				size >>= 10;
				units = "G";
			}
			else
				units = "M";
		}
		else
			units = "K";
	}
	else
		units = _("B");

	g_free(buffer);
	buffer = g_strdup_printf("%" SIZE_FMT " %s", size, units);

	return buffer;
}

/* Return a string in the form '23M' in a static buffer valid until
 * the next call. Aligned to the right (5 chars).
 */
const char *format_size_aligned(off_t size)
{
	static	char *buffer = NULL;
	char	units;
	
	if (size >= PRETTY_SIZE_LIMIT)
	{
		size += 1023;
		size >>= 10;
		if (size >= PRETTY_SIZE_LIMIT)
		{
			size += 1023;
			size >>= 10;
			if (size >= PRETTY_SIZE_LIMIT)
			{
				size += 1023;
				size >>= 10;
				units = 'G';
			}
			else
				units = 'M';
		}
		else
			units = 'K';
	}
	else
		units = ' ';

	g_free(buffer);
	buffer = g_strdup_printf("%4" SIZE_FMT "%c", size, units);
	
	return buffer;
}

/*
 * Similar to format_size(), but this one uses a double argument since
 * unsigned long isn't wide enough on all platforms and we must be able to
 * sum sizes above 4 GB.
 */
const gchar *format_double_size(double size)
{
	static gchar	*buf = NULL;
	const char	*units;

	if (size >= PRETTY_SIZE_LIMIT)
	{
		size += 1023;
		size /= 1024;
		if (size >= PRETTY_SIZE_LIMIT)
		{
			size += 1023;
			size /= 1024;
			if (size >= PRETTY_SIZE_LIMIT)
			{
				size += 1023;
				size /= 1024;
				units = "G";
			}
			else
				units = "M";
		}
		else
			units = "K";

	}
	else if (size != 1)
		units = _("bytes");
	else
		units = _("byte");

	g_free(buf);
	buf = g_strdup_printf("%.0f %s", floor(size), units);

	return buf;
}

/* Fork and exec argv. Wait and return the child's exit status.
 * -1 if spawn fails.
 * Returns the error string from the command if any, or NULL on success.
 * If the process returns a non-zero exit status without producing a message,
 * a suitable message is created.
 * g_free() the result.
 */
char *fork_exec_wait(const char **argv)
{
	int	status;
	gchar	*errors = NULL;
	GError	*error = NULL;

	if (!g_spawn_sync(NULL, (char **) argv, NULL,
		     G_SPAWN_SEARCH_PATH | G_SPAWN_STDOUT_TO_DEV_NULL,
		     NULL, NULL,
		     NULL, &errors, &status, &error))
	{
		char *msg;

		msg = g_strdup(error->message);
		g_error_free(error);
		return msg;
	}

	if (errors && !*errors)
		null_g_free(&errors);

	if (!WIFEXITED(status))
	{
		if (!errors)
			errors = g_strdup("(Subprocess crashed?)");
	}
	else if (WEXITSTATUS(status))
	{
		if (!errors)
			errors = g_strdup(_("ERROR"));
	}

	if (errors)
		g_strstrip(errors);

	return errors;
}

/* If a file has this UID and GID, which permissions apply to us?
 * 0 = User, 1 = Group, 2 = World
 */
gint applicable(uid_t uid, gid_t gid)
{
	int	i;

	if (uid == euid)
		return 0;

	if (gid == egid)
		return 1;

	for (i = 0; i < ngroups; i++)
	{
		if (supplemental_groups[i] == gid)
			return 1;
	}

	return 2;
}

/* Converts a file's mode to a string. Result is a pointer
 * to a static buffer, valid until the next call.
 */
const char *pretty_permissions(mode_t m)
{
	static char buffer[] = "rwx,rwx,rwx/UG"
#ifdef S_ISVTX
	     "T"
#endif
	     ;

	buffer[0]  = m & S_IRUSR ? 'r' : '-';
	buffer[1]  = m & S_IWUSR ? 'w' : '-';
	buffer[2]  = m & S_IXUSR ? 'x' : '-';

	buffer[4]  = m & S_IRGRP ? 'r' : '-';
	buffer[5]  = m & S_IWGRP ? 'w' : '-';
	buffer[6]  = m & S_IXGRP ? 'x' : '-';

	buffer[8]  = m & S_IROTH ? 'r' : '-';
	buffer[9]  = m & S_IWOTH ? 'w' : '-';
	buffer[10] = m & S_IXOTH ? 'x' : '-';

	buffer[12] = m & S_ISUID ? 'U' : '-';
	buffer[13] = m & S_ISGID ? 'G' : '-';
#ifdef S_ISVTX
        buffer[14] = m & S_ISVTX ? 'T' : '-';
#endif

	return buffer;
}

/* Gets the canonical name for address and compares to our_host_name() */
static gboolean is_local_address(char *address)
{
	struct hostent *ent;

	ent = gethostbyname(address);

	return strcmp(our_host_name(), ent ? ent->h_name : address) == 0;
}

/* Convert a URI to a local pathname (or NULL if it isn't local).
 * The returned pointer needs to be passed to g_free() when done (if not NULL).
 * THIS IS A CHANGE. The return path has been processed by unescape_uri().
 * Possible formats:
 *	/path
 *	///path
 *	//host/path
 *	file://host/path
 */
char *get_local_path(const EscapedPath *escaped_uri)
{
	const char *uri = (char *) escaped_uri;

	if (*uri == '/')
	{
		char    *path, *uri_host;

		/* Just a local path - no host part */
		if (uri[1] != '/')
			return unescape_uri((EscapedPath *) uri);
		                    
		path = strchr(uri + 2, '/');
		if (!path)
			return NULL;	    /* //something */

		if (path - uri == 2)
		{
			/* ///path */
			return unescape_uri((EscapedPath *) path);
		}
		
		uri_host = g_strndup(uri + 2, path - uri - 2);
		if (is_local_address(uri_host))
		{
			g_free(uri_host);
			/* //myhost/path */
			return unescape_uri((EscapedPath *) path);
		}
		g_free(uri_host);

		return NULL;	    /* From a different host */
	}
	else
	{
		if (strncasecmp(uri, "file:", 5))
			return NULL;	    /* Don't know this format */

		uri += 5;

		if (*uri == '/')
			return get_local_path((EscapedPath *) uri);

		return NULL;
	}
}

/* Return the scheme part of a URI, or NULL if not a valid URI (see
 * RFC 2396).  g_free() the returned value.
 */
#define SCHEME_CHARS "+-."  /* In addition to alpha-numerics */
gchar *get_uri_scheme(const EscapedPath *uri)
{
	const gchar *start=(const gchar *) uri;
	const gchar *p=start;
	
	if(!g_ascii_isalpha(p[0]))
		return NULL;

	while(g_ascii_isalnum(p[0]) ||
	      strchr(SCHEME_CHARS, p[0]))
		p++;

	if(p[0]!=':')
		return NULL;

	return g_strndup(start, p-start);
}

/* Set the close-on-exec flag for this FD.
 * TRUE means that an exec()'d process will not get the FD.
 */
void close_on_exec(int fd, gboolean close)
{
	if (fcntl(fd, F_SETFD, close))
		g_warning("fcntl() failed: %s\n", g_strerror(errno));
}

void set_blocking(int fd, gboolean blocking)
{
	if (fcntl(fd, F_SETFL, blocking ? 0 : O_NONBLOCK))
		g_warning("fcntl() failed: %s\n", g_strerror(errno));
}

/* Format this time nicely.
 * g_free() the result.
 */
char *pretty_time(const time_t *time)
{
        char time_buf[32];
	struct tm *tms;

	if (time == NULL)
		return g_strdup("(null)");

	tms = localtime(time);
	if (tms == NULL)
		return g_strdup("(invalid time)");

        if (strftime(time_buf, sizeof(time_buf), TIME_FORMAT, tms) == 0)
		time_buf[0]= 0;

	return to_utf8(time_buf);
}

#ifndef O_NOFOLLOW
#  define O_NOFOLLOW 0x0
#endif

/* 'from' and 'to' are complete pathnames of files (not dirs or symlinks).
 * This spawns 'cp' to do the copy if lstat() succeeds, otherwise we
 * do the copy manually using vfs.
 *
 * Returns an error string, or NULL on success. g_free() the result.
 *
 * XXX: This was only used for libvfs...
 */
guchar *copy_file(const guchar *from, const guchar *to)
{
	const char *argv[] = {"cp", "-pRf", NULL, NULL, NULL};

	argv[2] = from;
	argv[3] = to;

	return fork_exec_wait(argv);
}

/* 'word' has all special characters escaped so that it may be inserted
 * into a shell command.
 * Eg: 'My Dir?' becomes 'My\ Dir\?'. g_free() the result.
 */
guchar *shell_escape(const guchar *word)
{
	GString	*tmp;
	guchar	*retval;

	tmp = g_string_new(NULL);

	while (*word)
	{
		if (strchr(" ?*['\"$~\\|();!`&", *word))
			g_string_append_c(tmp, '\\');
		g_string_append_c(tmp, *word);
		word++;
	}

	retval = tmp->str;
	g_string_free(tmp, FALSE);
	return retval;
}

/* TRUE iff `sub' is (or would be) an object inside the directory `parent',
 * (or the two are the same item/directory).
 * FALSE if parent doesn't exist.
 */
gboolean is_sub_dir(const char *sub_obj, const char *parent)
{
	struct stat parent_info;
	char *sub;

	if (mc_lstat(parent, &parent_info))
		return FALSE;		/* Parent doesn't exist */

	/* For checking Copy/Move operations do a realpath first on sub
	 * (the destination), since copying into a symlink is the same as
	 * copying into the thing it points to. Don't realpath 'parent' though;
	 * copying a symlink just makes a new symlink.
	 * 
	 * When checking if an icon depends on a file (parent), use realpath on
	 * sub (the icon) too.
	 */
	sub = pathdup(sub_obj);
	
	while (1)
	{
		char	    *slash;
		struct stat info;
		
		if (mc_lstat(sub, &info) == 0)
		{
			if (info.st_dev == parent_info.st_dev &&
				info.st_ino == parent_info.st_ino)
			{
				g_free(sub);
				return TRUE;
			}
		}
		
		slash = strrchr(sub, '/');
		if (!slash)
			break;
		if (slash == sub)
		{
			if (sub[1])
				sub[1] = '\0';
			else
				break;
		}
		else
			*slash = '\0';
	}

	g_free(sub);

	return FALSE;
}

/* True if the string 'list' contains 'item'.
 * Eg ("close", "close, help") -> TRUE
 */
gboolean in_list(const guchar *item, const guchar *list)
{
	int	len;

	len = strlen(item);
	
	while (*list)
	{
		if (strncmp(item, list, len) == 0 &&
		    !g_ascii_isalpha(list[len]))
			return TRUE;
		list = strchr(list, ',');
		if (!list)
			return FALSE;
		while (g_ascii_isspace(*++list))
			;
	}

	return FALSE;
}

/* Split a path into its components. Eg:
 *
 * /bob/fred -> ["bob", "fred"]
 * ///a//b// -> ["a", "b"]
 * /	     -> []
 *
 * The array and the strings in it must be freed after use.
 */
GPtrArray *split_path(const guchar *path)
{
	GPtrArray *array;
	guchar	*slash;

	g_return_val_if_fail(path != NULL, NULL);

	array = g_ptr_array_new();

	while (1)
	{
		while (path[0] == '/')
			path++;
		if (path[0] == '\0')
			break;
		
		slash = strchr(path, '/');
		if (slash)
		{
			g_ptr_array_add(array, g_strndup(path, slash - path));
			path = slash + 1;
			continue;
		}
		g_ptr_array_add(array, g_strdup(path));
		break;
	}

	return array;
}

/* Return the shortest path from 'from' to 'to'.
 * Eg: get_relative_path("/a/b/c", "a/d/e") -> "../d/e"
 */
guchar *get_relative_path(const guchar *from, const guchar *to)
{
	GString  *path;
	guchar	 *retval;
	GPtrArray *src, *dst;
	int	i, j;

	src = split_path(from);
	dst = split_path(to);

	/* The last component of src doesn't matter... */
	if (src->len)
	{
		g_free(src->pdata[src->len - 1]);
		g_ptr_array_remove_index(src, src->len - 1);
	}

	/* Strip off common path elements... */
	i = 0;
	while (i < src->len && i < dst->len)
	{
		guchar	*a = (guchar *) src->pdata[i];
		guchar	*b = (guchar *) dst->pdata[i];

		if (strcmp(a, b) != 0)
			break;
		i++;
	}

	/* Go up one dir for each element remaining in src */
	path = g_string_new(NULL);
	for (j = i; j < src->len; j++)
		g_string_append(path, "../");

	/* Go down one dir for each element remaining in dst */
	for (j = i; j < dst->len; j++)
	{
		g_string_append(path, (guchar *) dst->pdata[j]);
		g_string_append_c(path, '/');
	}

	if (path->str[path->len - 1] == '/')
		g_string_truncate(path, path->len - 1);
	if (path->len == 0)
		g_string_assign(path, ".");

	/* Free the arrays */
	for (i = 0; i < src->len; i++)
		g_free(src->pdata[i]);
	g_ptr_array_free(src, TRUE);
	for (i = 0; i < dst->len; i++)
		g_free(dst->pdata[i]);
	g_ptr_array_free(dst, TRUE);

	retval = path->str;
	g_string_free(path, FALSE);

	return retval;
}

/*
 * Interperet text as a boolean value.  Return defvalue if we don't
 * recognise it
 */
int text_to_boolean(const char *text, int defvalue)
{
	if (g_strcasecmp(text, "true")==0)
	        return TRUE;
	else if (g_strcasecmp(text, "false")==0)
	        return FALSE;
	else if (g_strcasecmp(text, "yes")==0)
	        return TRUE;
	else if (g_strcasecmp(text, "no")==0)
	        return FALSE;
	else if (g_ascii_isdigit(text[0]))
	        return !!atoi(text);

	return defvalue;
}

/* Return the pathname that this symlink points to.
 * NULL on error (not a symlink, path too long) and errno set.
 * g_free() the result.
 */
char *readlink_dup(const char *source)
{
	char	path[MAXPATHLEN + 1];
	int	got;

	got = readlink(source, path, MAXPATHLEN);
	if (got < 0 || got > MAXPATHLEN)
		return NULL;

	return g_strndup(path, got);
}

/*
 * This code implements the MD5 message-digest algorithm.
 * The algorithm is due to Ron Rivest. The original code was
 * written by Colin Plumb in 1993, and put in the public domain.
 * 
 * Modified to use glib datatypes. Put under GPL to simplify
 * licensing for ROX-Filer. Taken from Debian's dpkg package.
 */

#define md5byte unsigned char

typedef struct _MD5Context MD5Context;

struct _MD5Context {
	guint32 buf[4];
	guint32 bytes[2];
	guint32 in[16];
};

#if G_BYTE_ORDER == G_BIG_ENDIAN
static void byteSwap(guint32 *buf, unsigned words)
{
	md5byte *p = (md5byte *)buf;

	do {
		*buf++ = (guint32)((unsigned)p[3] << 8 | p[2]) << 16 |
			((unsigned)p[1] << 8 | p[0]);
		p += 4;
	} while (--words);
}
#else
#define byteSwap(buf,words)
#endif

/*
 * Start MD5 accumulation. Set bit count to 0 and buffer to mysterious
 * initialization constants.
 */
static void MD5Init(MD5Context *ctx)
{
	ctx->buf[0] = 0x67452301;
	ctx->buf[1] = 0xefcdab89;
	ctx->buf[2] = 0x98badcfe;
	ctx->buf[3] = 0x10325476;

	ctx->bytes[0] = 0;
	ctx->bytes[1] = 0;
}

/*
 * Update context to reflect the concatenation of another buffer full
 * of bytes.
 */
static void MD5Update(MD5Context *ctx, md5byte const *buf, unsigned len)
{
	guint32 t;

	/* Update byte count */

	t = ctx->bytes[0];
	if ((ctx->bytes[0] = t + len) < t)
		ctx->bytes[1]++;	/* Carry from low to high */

	t = 64 - (t & 0x3f);	/* Space available in ctx->in (at least 1) */
	if (t > len) {
		memcpy((md5byte *)ctx->in + 64 - t, buf, len);
		return;
	}
	/* First chunk is an odd size */
	memcpy((md5byte *)ctx->in + 64 - t, buf, t);
	byteSwap(ctx->in, 16);
	MD5Transform(ctx->buf, ctx->in);
	buf += t;
	len -= t;

	/* Process data in 64-byte chunks */
	while (len >= 64) {
		memcpy(ctx->in, buf, 64);
		byteSwap(ctx->in, 16);
		MD5Transform(ctx->buf, ctx->in);
		buf += 64;
		len -= 64;
	}

	/* Handle any remaining bytes of data. */
	memcpy(ctx->in, buf, len);
}

/*
 * Final wrapup - pad to 64-byte boundary with the bit pattern 
 * 1 0* (64-bit count of bits processed, MSB-first)
 * Returns the newly allocated string of the hash.
 */
static char *MD5Final(MD5Context *ctx)
{
	char *retval;
	int i;
	int count = ctx->bytes[0] & 0x3f;	/* Number of bytes in ctx->in */
	md5byte *p = (md5byte *)ctx->in + count;
	guint8	*bytes;

	/* Set the first char of padding to 0x80.  There is always room. */
	*p++ = 0x80;

	/* Bytes of padding needed to make 56 bytes (-8..55) */
	count = 56 - 1 - count;

	if (count < 0) {	/* Padding forces an extra block */
		memset(p, 0, count + 8);
		byteSwap(ctx->in, 16);
		MD5Transform(ctx->buf, ctx->in);
		p = (md5byte *)ctx->in;
		count = 56;
	}
	memset(p, 0, count);
	byteSwap(ctx->in, 14);

	/* Append length in bits and transform */
	ctx->in[14] = ctx->bytes[0] << 3;
	ctx->in[15] = ctx->bytes[1] << 3 | ctx->bytes[0] >> 29;
	MD5Transform(ctx->buf, ctx->in);

	byteSwap(ctx->buf, 4);

	retval = g_malloc(33);
	bytes = (guint8 *) ctx->buf;
	for (i = 0; i < 16; i++)
		sprintf(retval + (i * 2), "%02x", bytes[i]);
	retval[32] = '\0';
	
	return retval;
}

# ifndef ASM_MD5

/* The four core functions - F1 is optimized somewhat */

/* #define F1(x, y, z) (x & y | ~x & z) */
#define F1(x, y, z) (z ^ (x & (y ^ z)))
#define F2(x, y, z) F1(z, x, y)
#define F3(x, y, z) (x ^ y ^ z)
#define F4(x, y, z) (y ^ (x | ~z))

/* This is the central step in the MD5 algorithm. */
#define MD5STEP(f,w,x,y,z,in,s) \
	 (w += f(x,y,z) + in, w = (w<<s | w>>(32-s)) + x)

/*
 * The core of the MD5 algorithm, this alters an existing MD5 hash to
 * reflect the addition of 16 longwords of new data.  MD5Update blocks
 * the data and converts bytes into longwords for this routine.
 */
static void MD5Transform(guint32 buf[4], guint32 const in[16])
{
	register guint32 a, b, c, d;

	a = buf[0];
	b = buf[1];
	c = buf[2];
	d = buf[3];

	MD5STEP(F1, a, b, c, d, in[0] + 0xd76aa478, 7);
	MD5STEP(F1, d, a, b, c, in[1] + 0xe8c7b756, 12);
	MD5STEP(F1, c, d, a, b, in[2] + 0x242070db, 17);
	MD5STEP(F1, b, c, d, a, in[3] + 0xc1bdceee, 22);
	MD5STEP(F1, a, b, c, d, in[4] + 0xf57c0faf, 7);
	MD5STEP(F1, d, a, b, c, in[5] + 0x4787c62a, 12);
	MD5STEP(F1, c, d, a, b, in[6] + 0xa8304613, 17);
	MD5STEP(F1, b, c, d, a, in[7] + 0xfd469501, 22);
	MD5STEP(F1, a, b, c, d, in[8] + 0x698098d8, 7);
	MD5STEP(F1, d, a, b, c, in[9] + 0x8b44f7af, 12);
	MD5STEP(F1, c, d, a, b, in[10] + 0xffff5bb1, 17);
	MD5STEP(F1, b, c, d, a, in[11] + 0x895cd7be, 22);
	MD5STEP(F1, a, b, c, d, in[12] + 0x6b901122, 7);
	MD5STEP(F1, d, a, b, c, in[13] + 0xfd987193, 12);
	MD5STEP(F1, c, d, a, b, in[14] + 0xa679438e, 17);
	MD5STEP(F1, b, c, d, a, in[15] + 0x49b40821, 22);

	MD5STEP(F2, a, b, c, d, in[1] + 0xf61e2562, 5);
	MD5STEP(F2, d, a, b, c, in[6] + 0xc040b340, 9);
	MD5STEP(F2, c, d, a, b, in[11] + 0x265e5a51, 14);
	MD5STEP(F2, b, c, d, a, in[0] + 0xe9b6c7aa, 20);
	MD5STEP(F2, a, b, c, d, in[5] + 0xd62f105d, 5);
	MD5STEP(F2, d, a, b, c, in[10] + 0x02441453, 9);
	MD5STEP(F2, c, d, a, b, in[15] + 0xd8a1e681, 14);
	MD5STEP(F2, b, c, d, a, in[4] + 0xe7d3fbc8, 20);
	MD5STEP(F2, a, b, c, d, in[9] + 0x21e1cde6, 5);
	MD5STEP(F2, d, a, b, c, in[14] + 0xc33707d6, 9);
	MD5STEP(F2, c, d, a, b, in[3] + 0xf4d50d87, 14);
	MD5STEP(F2, b, c, d, a, in[8] + 0x455a14ed, 20);
	MD5STEP(F2, a, b, c, d, in[13] + 0xa9e3e905, 5);
	MD5STEP(F2, d, a, b, c, in[2] + 0xfcefa3f8, 9);
	MD5STEP(F2, c, d, a, b, in[7] + 0x676f02d9, 14);
	MD5STEP(F2, b, c, d, a, in[12] + 0x8d2a4c8a, 20);

	MD5STEP(F3, a, b, c, d, in[5] + 0xfffa3942, 4);
	MD5STEP(F3, d, a, b, c, in[8] + 0x8771f681, 11);
	MD5STEP(F3, c, d, a, b, in[11] + 0x6d9d6122, 16);
	MD5STEP(F3, b, c, d, a, in[14] + 0xfde5380c, 23);
	MD5STEP(F3, a, b, c, d, in[1] + 0xa4beea44, 4);
	MD5STEP(F3, d, a, b, c, in[4] + 0x4bdecfa9, 11);
	MD5STEP(F3, c, d, a, b, in[7] + 0xf6bb4b60, 16);
	MD5STEP(F3, b, c, d, a, in[10] + 0xbebfbc70, 23);
	MD5STEP(F3, a, b, c, d, in[13] + 0x289b7ec6, 4);
	MD5STEP(F3, d, a, b, c, in[0] + 0xeaa127fa, 11);
	MD5STEP(F3, c, d, a, b, in[3] + 0xd4ef3085, 16);
	MD5STEP(F3, b, c, d, a, in[6] + 0x04881d05, 23);
	MD5STEP(F3, a, b, c, d, in[9] + 0xd9d4d039, 4);
	MD5STEP(F3, d, a, b, c, in[12] + 0xe6db99e5, 11);
	MD5STEP(F3, c, d, a, b, in[15] + 0x1fa27cf8, 16);
	MD5STEP(F3, b, c, d, a, in[2] + 0xc4ac5665, 23);

	MD5STEP(F4, a, b, c, d, in[0] + 0xf4292244, 6);
	MD5STEP(F4, d, a, b, c, in[7] + 0x432aff97, 10);
	MD5STEP(F4, c, d, a, b, in[14] + 0xab9423a7, 15);
	MD5STEP(F4, b, c, d, a, in[5] + 0xfc93a039, 21);
	MD5STEP(F4, a, b, c, d, in[12] + 0x655b59c3, 6);
	MD5STEP(F4, d, a, b, c, in[3] + 0x8f0ccc92, 10);
	MD5STEP(F4, c, d, a, b, in[10] + 0xffeff47d, 15);
	MD5STEP(F4, b, c, d, a, in[1] + 0x85845dd1, 21);
	MD5STEP(F4, a, b, c, d, in[8] + 0x6fa87e4f, 6);
	MD5STEP(F4, d, a, b, c, in[15] + 0xfe2ce6e0, 10);
	MD5STEP(F4, c, d, a, b, in[6] + 0xa3014314, 15);
	MD5STEP(F4, b, c, d, a, in[13] + 0x4e0811a1, 21);
	MD5STEP(F4, a, b, c, d, in[4] + 0xf7537e82, 6);
	MD5STEP(F4, d, a, b, c, in[11] + 0xbd3af235, 10);
	MD5STEP(F4, c, d, a, b, in[2] + 0x2ad7d2bb, 15);
	MD5STEP(F4, b, c, d, a, in[9] + 0xeb86d391, 21);

	buf[0] += a;
	buf[1] += b;
	buf[2] += c;
	buf[3] += d;
}

# endif /* ASM_MD5 */

char *md5_hash(const char *message)
{
	MD5Context ctx;

	MD5Init(&ctx);
	MD5Update(&ctx, message, strlen(message));
	return MD5Final(&ctx);
}

/* Convert string 'src' from the current locale to UTF-8 */
gchar *to_utf8(const gchar *src)
{
	gchar *retval;

	if (!src)
		return NULL;

	retval = g_locale_to_utf8(src, -1, NULL, NULL, NULL);
	if (!retval)
		retval = g_convert_with_fallback(src, -1, "utf-8", "iso-8859-1",
						"?", NULL, NULL, NULL);

	return retval ? retval : g_strdup(src);
}

/* Ensure string at 'sp' is UTF-8. g_free() and replace by
 * UTF-8 version if not.
 */
void ensure_utf8(gchar **sp)
{
	gchar *s = *sp;

	if (!g_utf8_validate(s, -1, NULL))
	{
		*sp = to_utf8(s);
		g_free(s);
	}
}

/* Removes trailing / chars and converts a leading '~/' (if any) to
 * the user's home dir. g_free() the result.
 */
gchar *expand_path(const gchar *path)
{
	guchar		*retval;
	int		path_len;

	g_return_val_if_fail(path != NULL, NULL);

	path_len = strlen(path);
	while (path_len > 1 && path[path_len - 1] == '/')
		path_len--;
	
	retval = g_strndup(path, path_len);

	if (path[0] == '~' && (path[1] == '\0' || path[1] == '/'))
	{
		guchar *tmp = retval;

		retval = g_strconcat(home_dir, retval + 1, NULL);
		g_free(tmp);
	}

	return retval;
}

/* g_free() every element in the list, then free the list itself and
 * NULL the pointer to the list.
 */
void destroy_glist(GList **list)
{
	GList *l = *list;
	g_list_foreach(l, (GFunc) g_free, NULL);
	g_list_free(l);
	*list = NULL;
}

void null_g_free(gpointer p)
{
	g_free(*(gpointer *)p);
	*(gpointer *)p = NULL;
}

typedef struct _CollatePart CollatePart;

struct _CollateKey {
	CollatePart *parts;
	gboolean caps;
};

struct _CollatePart {
	guchar *text;	/* NULL => end of list */
	long number;
};

/* Break 'name' (a UTF-8 string) down into a list of (text, number) pairs.
 * The text parts processed for collating. This allows any two names to be
 * quickly compared later for intelligent sorting (comparing names is
 * speed-critical).
 */
CollateKey *collate_key_new(const guchar *name)
{
	const guchar *i;
	guchar *to_free = NULL;
	GArray *array;
	CollatePart new;
	CollateKey *retval;
	char *tmp;

	g_return_val_if_fail(name != NULL, NULL);

	array = g_array_new(FALSE, FALSE, sizeof(CollatePart));

	/* Ensure valid UTF-8 */
	if (!g_utf8_validate(name, -1, NULL))
	{
		to_free = to_utf8(name);
		name = to_free;
	}

	retval = g_new(CollateKey, 1);
	retval->caps = g_unichar_isupper(g_utf8_get_char(name));

	for (i = name; *i; i = g_utf8_next_char(i))
	{
		gunichar first_char;

		/* We're in a (possibly blank) text section starting at 'name'.
		 * Find the end of it (the next digit, or end of string).
		 * Note: g_ascii_isdigit takes char, not unichar, while
		 * g_unicode_isdigit returns true for non ASCII digits.
		 */
		first_char = g_utf8_get_char(i);
		if (first_char >= '0' && first_char <= '9')
		{
			char *endp;
			
			/* i -> first digit character */
			tmp = g_utf8_strdown(name, i - name);
			new.text = g_utf8_collate_key(tmp, -1);
			g_free(tmp);
			new.number = strtol(i, &endp, 10);

			g_array_append_val(array, new);

			g_return_val_if_fail(endp > (char *) i, NULL);

			name = endp;
			i = name - 1;
		}
	}

	tmp = g_utf8_strdown(name, i - name);
	new.text = g_utf8_collate_key(tmp, -1);
	g_free(tmp);
	new.number = -1;
	g_array_append_val(array, new);

	new.text = NULL;
	g_array_append_val(array, new);

	retval->parts = (CollatePart *) array->data;
	g_array_free(array, FALSE);

	if (to_free)
		g_free(to_free);	/* Only taken for invalid UTF-8 */

	return retval;
}

void collate_key_free(CollateKey *key)
{
	CollatePart *part;

	for (part = key->parts; part->text; part++)
		g_free(part->text);
	g_free(key->parts);
	g_free(key);
}

int collate_key_cmp(const CollateKey *key1, const CollateKey *key2,
		    gboolean caps_first)
{
	CollatePart *n1 = key1->parts;
	CollatePart *n2 = key2->parts;
	int r;

	if (caps_first)
	{
		if (key1->caps && !key2->caps)
			return -1;
		else if (key2->caps && !key1->caps)
			return 1;
	}

	while (1)
	{
		if (!n1->text)
			return n2->text ? -1 : 0;
		if (!n2->text)
			return 1;
		r = strcmp(n1->text, n2->text);
		if (r)
			return r;

		if (n1->number < n2->number)
			return -1;
		if (n1->number > n2->number)
			return 1;

		n1++;
		n2++;
	}
}

/* Returns TRUE if the object exists, FALSE if it doesn't.
 * For symlinks, the file pointed to must exist.
 */
gboolean file_exists(const char *path)
{
	struct stat info;

	return !mc_stat(path, &info);
}

/* Escape path for future use in URI */
EscapedPath *escape_uri_path(const char *path)
{
	const char *safe = ":-_./"; /* Plus isalnum() */
	const guchar *s;
	gchar *ans;
	GString *str;

	str = g_string_sized_new(strlen(path));

	for (s = path; *s; s++)
	{
		if (!g_ascii_isalnum(*s) && !strchr(safe, *s))
			g_string_append_printf(str, "%%%02x", *s);
		else
			str = g_string_append_c(str, *s);
	}

	ans = str->str;
	g_string_free(str, FALSE);

	return (EscapedPath *) ans;
}

EscapedPath *encode_path_as_uri(const guchar *path)
{
	gchar *tpath = (gchar *) escape_uri_path(path);
	gchar *uri;

	uri = g_strconcat("file://", our_host_name_for_dnd(), tpath, NULL);
	g_free(tpath);

	return (EscapedPath *) uri;
}

gchar *unescape_uri(const EscapedPath *uri)
{
	const char *uri_string = (char *) uri;
	const gchar *s;
	gchar *d;
	gchar *tmp;
	
	tmp = g_malloc(strlen(uri_string) + 1);
	for (s = uri_string, d = tmp; *s; s++, d++)
	{
		/*printf("%s\n", s);*/
		if (*s == '%' && g_ascii_isxdigit(s[1]) &&
				 g_ascii_isxdigit(s[2]))
		{
			int c;
			char buf[3];
			buf[0] = s[1];
			buf[1] = s[2];
			buf[2] = 0;
			c = (int) strtol(buf, NULL, 16);
			*d = c;
			s += 2;
		}
		else
			*d = *s;
	}
	*d = '\0';

	return tmp;
}

/* Used as the sort function for sorting GPtrArrays */
gint strcmp2(gconstpointer a, gconstpointer b)
{
	const char *aa = *(char **) a;
	const char *bb = *(char **) b;

	return g_strcasecmp(aa, bb);
}

/* Returns an array listing all the names in the directory 'path'.
 * The array is sorted.
 * '.' and '..' are skipped.
 * On error, the error is reported with g_warning and NULL is returned.
 */
GPtrArray *list_dir(const guchar *path)
{
	GDir *dir;
	GError *error = NULL;
	const char *leaf;
	GPtrArray *names;
	
	dir = g_dir_open(path, 0, &error);
	if (error)
	{
		g_warning("Can't list directory:\n%s", error->message);
		g_error_free(error);
		return NULL;
	}

	names = g_ptr_array_new();

	while ((leaf = g_dir_read_name(dir))) {
		if (leaf[0] != '.')
			g_ptr_array_add(names, g_strdup(leaf));
	}

	g_dir_close(dir);

	g_ptr_array_sort(names, strcmp2);

	return names;
}

int stat_with_timeout(const char *path, struct stat *info)
{
	int status;
	pid_t child;
	gboolean retval;

	child = fork();
	if (child < 0)
	{
		g_warning("stat_with_timeout: fork(): %s", g_strerror(errno));
		return -1;
	}

	if (child == 0)
	{
		/* Child */
		alarm(3);
		_exit(mc_stat(path, info) ? 1 : 0);
	}

	waitpid(child, &status, 0);

	if (status == 0)
		retval = mc_stat(path, info);
	else
		retval = -1;

	return retval;
}

/* From glib. */
static gchar *my_strchrnul(const gchar *str, gchar c)
{
	gchar *p = (gchar*) str;
	while (*p && (*p != c))
		++p;

	return p;
}

/* Based on code from glib. */
gboolean available_in_path(const char *file)
{
	const gchar *path, *p;
	gchar *name, *freeme;
	size_t len;
	size_t pathlen;
	gboolean found = FALSE;

	path = g_getenv("PATH");
	if (path == NULL)
		path = "/bin:/usr/bin:.";

	len = strlen(file) + 1;
	pathlen = strlen(path);
	freeme = name = g_malloc(pathlen + len + 1);

	/* Copy the file name at the top, including '\0'  */
	memcpy(name + pathlen + 1, file, len);
	name = name + pathlen;
	/* And add the slash before the filename  */
	*name = '/';

	p = path;
	do
	{
		char *startp;

		path = p;
		p = my_strchrnul(path, ':');

		if (p == path)
			/* Two adjacent colons, or a colon at the beginning or
			 * the end of `PATH' means to search the current
			 * directory.
			 */
			startp = name + 1;
		else
			startp = memcpy (name - (p - path), path, p - path);

		/* Try to execute this name.  If it works, execv will not
		 * return.
		 */
		if (access(startp, X_OK) == 0)
			found = TRUE;
	} while (!found && *p++ != '\0');

	g_free(freeme);

	return found;
}

static char *get_value_from_desktop_data(const char *data, size_t length,
					 const char *section, const char *key,
					 GError **error)
{
	int section_length, key_length;
	const char *last_possible_key_start;
	const char *last_possible_section_start;
	const char *next;

	section_length = strlen(section);
	last_possible_section_start = data + length - section_length - 3;

	for (next = data; next <= last_possible_section_start; next++)
	{
		if (next[0] != '[' || (next > data && next[-1] != '\n'))
			continue;
		next += 1;
		if (next[section_length] != ']' ||
		    next[section_length + 1] != '\n')
			continue;
		if (strncmp(next, section, section_length) != 0)
			continue;
		next += section_length + 1;	/* Newline */
		goto find_key;
	}
	return NULL;

find_key:
	key_length = strlen(key);
	last_possible_key_start = data + length - key_length - 3;
	for (;
	     next && next < last_possible_key_start;
	     next = memchr(next + 1, '\n', last_possible_key_start - next))
	{
		const char *nl;

		if (next[1] == '\n')
			continue; /* Cope with blank lines */

		next++;		/* Skip newline */

		if (next[0] == '[')
			break;	/* End of section */

		if (strncmp(next, key, key_length) != 0)
			continue;
		next += key_length;
		while (next[0] == ' ' || next[0] == '\t')
			next++;

		/* Note: if we had GLib 2.6, we could use
		 * g_get_language_names() to get translations here.
		 */
		if (next[0] != '=')
			continue;
		next++;
		while (next[0] == ' ' || next[0] == '\t')
			next++;
		nl = memchr(next, '\n', data + length - next);
		if (!nl)
			break;
		return g_strndup(next, nl - next);
	}

	return NULL;
}

/* Load .desktop file 'path' and return the value of the named key.
 * Sets error if the desktop file cannot be parsed.
 * Returns NULL (but does not set error) if the key is not present.
 * Returned string must be g_free()d.
 */
char *get_value_from_desktop_file(const char *path,
				  const char *section,
				  const char *key,
				  GError **error)
{
	char *value = NULL;
	struct stat info;
	int fd = -1;
	void *start = NULL;

	fd = open(path, O_RDONLY);
	if (fd == -1 || fstat(fd, &info) != 0)
	{
                g_set_error(error,
                             G_FILE_ERROR,
                             g_file_error_from_errno(errno),
                             _("Failed to open and stat file '%s': %s"),
                             path,
                             g_strerror(errno));
		goto err;
	}
	start = mmap(NULL, info.st_size, PROT_READ, MAP_SHARED, fd, 0);
	if (start == MAP_FAILED)
	{
                g_set_error(error,
                             G_FILE_ERROR,
                             g_file_error_from_errno(errno),
                             _("Failed to mmap file '%s': %s"),
                             path,
                             g_strerror(errno));
		goto err;
	}

	value = get_value_from_desktop_data((const char *) start,
					    info.st_size,
					    section, key,
					    error);
err:
	if (fd != -1)
		close(fd);

	if (start != NULL && start != MAP_FAILED)
		munmap(start, info.st_size);

	return value;
}

/* Load .desktop file 'path' and set the value of the named keys in the
 * NULL terminated list.
 * Sets error if the desktop file cannot be parsed and returns false.
 * Sets NULL (but does not set error) if the key is not present.
 * String set must be g_free()d.
 */
gboolean get_values_from_desktop_file(const char *path,
				      GError **error,
				      const char *section,
				      const char *key,
				      gchar **value, ...)
{
	struct stat info;
	int fd = -1;
	void *start = NULL;
	va_list list;

	fd = open(path, O_RDONLY);
	if (fd == -1 || fstat(fd, &info) != 0)
	{
                g_set_error(error,
                             G_FILE_ERROR,
                             g_file_error_from_errno(errno),
                             _("Failed to open and stat file '%s': %s"),
                             path,
                             g_strerror(errno));
		goto err;
	}
	start = mmap(NULL, info.st_size, PROT_READ, MAP_SHARED, fd, 0);
	if (start == MAP_FAILED)
	{
                g_set_error(error,
                             G_FILE_ERROR,
                             g_file_error_from_errno(errno),
                             _("Failed to mmap file '%s': %s"),
                             path,
                             g_strerror(errno));
		goto err;
	}

	va_start(list, value);
	while(section && key && value) {
		*value = get_value_from_desktop_data((const char *) start,
						     info.st_size,
						     section, key,
						     error);
		if(*error)
			break;

		section=va_arg(list, const char *);
		key=va_arg(list, const char *);
		value=va_arg(list, char **);
	}
		
err:
	if (fd != -1)
		close(fd);

	if (start != NULL && start != MAP_FAILED)
		munmap(start, info.st_size);

	return (*error==NULL);
}

/**
 * Build a command to execute with a supplied path.  If the command pattern
 * given contains @c $1 it is substituted with the path, otherwise the path
 * is appended to the pattern after a space character
 * @param[in] cmd command pattern, with optional @c $1
 * @param[in] path path to substitute or append
 * @return generated command line, pass to g_free() when done.
 */
gchar *build_command_with_path(const char *cmd, const char *path)
{
	const char *subs;
	gchar *result;

	for(subs=cmd; *subs; subs++)
	{
		if(subs[0]=='\\' && subs[1]=='$')
		{
			subs++;
			continue;
		}
		if(strncmp(subs, "$1", 2)==0)
			break;
	}
	if(*subs) {
		int size=strlen(cmd)+strlen(path)+1;
		result = g_new(char, size);
		strncpy(result, cmd, subs-cmd);
		result[subs-cmd] = 0;
		strcat(result, path);
		strcat(result, subs+2);
	} else {
		result = g_strconcat(cmd, " ", path, NULL);
	}

	return result;
}

/* Search for a application on $APPDIRPATH
 * (~/Apps:/usr/local/apps:/usr/apps) and return a copy of the path found.
 * Returns NULL if not found
 */
gchar *find_app(const char *appname)
{
  const gchar *path=g_getenv("APPDIRPATH");
  gchar **search;
  gchar *app=NULL;
  int i;

  if(path)
  {
	  search=g_strsplit(path, ":", 0);
  }
  else
  {
	  gchar *tmp;
	  
	  tmp=g_strdup_printf("%s/Apps:/usr/local/apps:/usr/apps",
			       g_get_home_dir());
	  search=g_strsplit(tmp, ":", 0);
	  g_free(tmp);
  }

  for(i=0; search[i]; i++)
  {
	  app=g_strconcat(search[i], "/", appname, NULL);
	  if(access(app, X_OK)==0)
		  goto out;
	  g_free(app);
  }
  app=NULL;

 out:
  g_strfreev(search);
  return app;
}
