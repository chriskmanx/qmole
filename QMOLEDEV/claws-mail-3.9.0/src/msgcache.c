/*
 * Sylpheed -- a GTK+ based, lightweight, and fast e-mail client
 * Copyright (C) 1999-2012 Hiroyuki Yamamoto & The Claws Mail Team
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

#ifdef HAVE_CONFIG_H
#  include "config.h"
#include "claws-features.h"
#endif

#include "defs.h"

#define _GNU_SOURCE
#include <stdio.h>

#include <glib.h>
#include <glib/gi18n.h>
#ifdef _WIN32
# include <w32lib.h>
# define MAP_FAILED	((char *) -1)
#else
# include <sys/mman.h>
#endif
#include <sys/types.h>
#include <sys/stat.h>

#include <time.h>

#include "msgcache.h"
#include "utils.h"
#include "procmsg.h"
#include "codeconv.h"
#include "timing.h"
#include "tags.h"
#include "prefs_common.h"

#ifdef HAVE_FWRITE_UNLOCKED
#define SC_FWRITE fwrite_unlocked
#else
#define SC_FWRITE fwrite
#endif

#if G_BYTE_ORDER == G_BIG_ENDIAN
#define bswap_32(x) \
     ((((x) & 0xff000000) >> 24) | (((x) & 0x00ff0000) >>  8) | \
      (((x) & 0x0000ff00) <<  8) | (((x) & 0x000000ff) << 24))
     
#define MMAP_TO_GUINT32(x)	\
	(((x[3]&0xff)) |	\
	 ((x[2]&0xff) << 8) |	\
	 ((x[1]&0xff) << 16) |	\
	 ((x[0]&0xff) << 24))

#define MMAP_TO_GUINT32_SWAPPED(x)	\
	(((x[0]&0xff)) |		\
	 ((x[1]&0xff) << 8) |		\
	 ((x[2]&0xff) << 16) |		\
	 ((x[3]&0xff) << 24))

static gboolean msgcache_use_mmap_read = TRUE;

#else
#define bswap_32(x) (x)

#define MMAP_TO_GUINT32(x)	\
	(((x[0]&0xff)) |	\
	 ((x[1]&0xff) << 8) |	\
	 ((x[2]&0xff) << 16) |	\
	 ((x[3]&0xff) << 24))

#define MMAP_TO_GUINT32_SWAPPED(x)	\
	(((x[0]&0xff)) |		\
	 ((x[1]&0xff) << 8) |		\
	 ((x[2]&0xff) << 16) |		\
	 ((x[3]&0xff) << 24))

static gboolean msgcache_use_mmap_read = TRUE;
#endif

static gboolean swapping = TRUE;

typedef enum
{
	DATA_READ,
	DATA_WRITE,
	DATA_APPEND
} DataOpenMode;

struct _MsgCache {
	GHashTable	*msgnum_table;
	GHashTable	*msgid_table;
	guint		 memusage;
	time_t		 last_access;
};

typedef struct _StringConverter StringConverter;
struct _StringConverter {
	gchar *(*convert) (StringConverter *converter, gchar *srcstr);
	void   (*free)    (StringConverter *converter);
};

typedef struct _StrdupConverter StrdupConverter;
struct _StrdupConverter {
	StringConverter converter;
};

typedef struct _CharsetConverter CharsetConverter;
struct _CharsetConverter {
	StringConverter converter;

	gchar *srccharset;
	gchar *dstcharset;
};

MsgCache *msgcache_new(void)
{
	MsgCache *cache;
	
	cache = g_new0(MsgCache, 1),
	cache->msgnum_table = g_hash_table_new(g_int_hash, g_int_equal);
	cache->msgid_table = g_hash_table_new(g_str_hash, g_str_equal);
	cache->last_access = time(NULL);

	return cache;
}

static gboolean msgcache_msginfo_free_func(gpointer num, gpointer msginfo, gpointer user_data)
{
	procmsg_msginfo_free((MsgInfo *)msginfo);
	return TRUE;
}											  

void msgcache_destroy(MsgCache *cache)
{
	cm_return_if_fail(cache != NULL);

	g_hash_table_foreach_remove(cache->msgnum_table, msgcache_msginfo_free_func, NULL);
	g_hash_table_destroy(cache->msgid_table);
	g_hash_table_destroy(cache->msgnum_table);
	g_free(cache);
}

void msgcache_add_msg(MsgCache *cache, MsgInfo *msginfo) 
{
	MsgInfo *newmsginfo;

	cm_return_if_fail(cache != NULL);
	cm_return_if_fail(msginfo != NULL);

	newmsginfo = procmsg_msginfo_new_ref(msginfo);
	g_hash_table_insert(cache->msgnum_table, &newmsginfo->msgnum, newmsginfo);
	if(newmsginfo->msgid != NULL)
		g_hash_table_insert(cache->msgid_table, newmsginfo->msgid, newmsginfo);
	cache->memusage += procmsg_msginfo_memusage(msginfo);
	cache->last_access = time(NULL);

	msginfo->folder->cache_dirty = TRUE;

	debug_print("Cache size: %d messages, %u bytes\n", g_hash_table_size(cache->msgnum_table), cache->memusage);
}

void msgcache_remove_msg(MsgCache *cache, guint msgnum)
{
	MsgInfo *msginfo;

	cm_return_if_fail(cache != NULL);

	msginfo = (MsgInfo *) g_hash_table_lookup(cache->msgnum_table, &msgnum);
	if(!msginfo)
		return;

	cache->memusage -= procmsg_msginfo_memusage(msginfo);
	if(msginfo->msgid)
		g_hash_table_remove(cache->msgid_table, msginfo->msgid);
	g_hash_table_remove(cache->msgnum_table, &msginfo->msgnum);
	procmsg_msginfo_free(msginfo);
	cache->last_access = time(NULL);

	msginfo->folder->cache_dirty = TRUE;

	debug_print("Cache size: %d messages, %u bytes\n", g_hash_table_size(cache->msgnum_table), cache->memusage);
}

void msgcache_update_msg(MsgCache *cache, MsgInfo *msginfo)
{
	MsgInfo *oldmsginfo, *newmsginfo;
	
	cm_return_if_fail(cache != NULL);
	cm_return_if_fail(msginfo != NULL);

	oldmsginfo = g_hash_table_lookup(cache->msgnum_table, &msginfo->msgnum);
	if(oldmsginfo && oldmsginfo->msgid) 
		g_hash_table_remove(cache->msgid_table, oldmsginfo->msgid);
	if (oldmsginfo) {
		g_hash_table_remove(cache->msgnum_table, &oldmsginfo->msgnum);
		cache->memusage -= procmsg_msginfo_memusage(oldmsginfo);
		procmsg_msginfo_free(oldmsginfo);
	}

	newmsginfo = procmsg_msginfo_new_ref(msginfo);
	g_hash_table_insert(cache->msgnum_table, &newmsginfo->msgnum, newmsginfo);
	if(newmsginfo->msgid)
		g_hash_table_insert(cache->msgid_table, newmsginfo->msgid, newmsginfo);
	cache->memusage += procmsg_msginfo_memusage(newmsginfo);
	cache->last_access = time(NULL);
	
	debug_print("Cache size: %d messages, %u bytes\n", g_hash_table_size(cache->msgnum_table), cache->memusage);

	msginfo->folder->cache_dirty = TRUE;

	return;
}

MsgInfo *msgcache_get_msg(MsgCache *cache, guint num)
{
	MsgInfo *msginfo;

	cm_return_val_if_fail(cache != NULL, NULL);

	msginfo = g_hash_table_lookup(cache->msgnum_table, &num);
	if(!msginfo)
		return NULL;
	cache->last_access = time(NULL);
	
	return procmsg_msginfo_new_ref(msginfo);
}

MsgInfo *msgcache_get_msg_by_id(MsgCache *cache, const gchar *msgid)
{
	MsgInfo *msginfo;
	
	cm_return_val_if_fail(cache != NULL, NULL);
	cm_return_val_if_fail(msgid != NULL, NULL);

	msginfo = g_hash_table_lookup(cache->msgid_table, msgid);
	if(!msginfo)
		return NULL;
	cache->last_access = time(NULL);
	
	return procmsg_msginfo_new_ref(msginfo);	
}

static void msgcache_get_msg_list_func(gpointer key, gpointer value, gpointer user_data)
{
	MsgInfoList **listptr = user_data;
	MsgInfo *msginfo = value;

	*listptr = g_slist_prepend(*listptr, procmsg_msginfo_new_ref(msginfo));
}

MsgInfoList *msgcache_get_msg_list(MsgCache *cache)
{
	MsgInfoList *msg_list = NULL;
	START_TIMING("");
	cm_return_val_if_fail(cache != NULL, NULL);

	g_hash_table_foreach((GHashTable *)cache->msgnum_table, msgcache_get_msg_list_func, (gpointer)&msg_list);	
	cache->last_access = time(NULL);
	
	msg_list = g_slist_reverse(msg_list);
	END_TIMING();
	return msg_list;
}

time_t msgcache_get_last_access_time(MsgCache *cache)
{
	cm_return_val_if_fail(cache != NULL, 0);
	
	return cache->last_access;
}

gint msgcache_get_memory_usage(MsgCache *cache)
{
	cm_return_val_if_fail(cache != NULL, 0);

	return cache->memusage;
}

/*
 *  Cache saving functions
 */

#define READ_CACHE_DATA(data, fp, total_len) \
{ \
	if ((tmp_len = msgcache_read_cache_data_str(fp, &data, conv)) < 0) { \
		procmsg_msginfo_free(msginfo); \
		error = TRUE; \
		goto bail_err; \
	} \
	total_len += tmp_len; \
}

#define READ_CACHE_DATA_INT(n, fp) \
{ \
	guint32 idata; \
	size_t ni; \
 \
	if ((ni = fread(&idata, sizeof(idata), 1, fp)) != 1) { \
		g_warning("read_int: Cache data corrupted, read %zd of %zd at " \
			  "offset %ld\n", ni, sizeof(idata), ftell(fp)); \
		procmsg_msginfo_free(msginfo); \
		error = TRUE; \
		goto bail_err; \
	} else \
		n = swapping ? bswap_32(idata) : (idata);\
}

#define GET_CACHE_DATA_INT(n)									\
{												\
	if (rem_len < 4) {									\
		g_print("error at rem_len:%d\n", rem_len);					\
		error = TRUE;									\
		goto bail_err;									\
	}											\
	n = (swapping ? (MMAP_TO_GUINT32_SWAPPED(walk_data)):(MMAP_TO_GUINT32(walk_data))); 	\
	walk_data += 4;	rem_len -= 4;								\
}

#define GET_CACHE_DATA(data, total_len) \
{ \
	GET_CACHE_DATA_INT(tmp_len);	\
	if (rem_len < tmp_len) {								\
		g_print("error at rem_len:%d (tmp_len %d)\n", rem_len, tmp_len);		\
		error = TRUE;									\
		goto bail_err;									\
	}											\
	if ((tmp_len = msgcache_get_cache_data_str(walk_data, &data, tmp_len, conv)) < 0) { \
		g_print("error at rem_len:%d\n", rem_len);\
		procmsg_msginfo_free(msginfo); \
		error = TRUE; \
		goto bail_err; \
	} \
	total_len += tmp_len; \
	walk_data += tmp_len; rem_len -= tmp_len; \
}


#define WRITE_CACHE_DATA_INT(n, fp)			\
{							\
	guint32 idata;					\
							\
	idata = (guint32)bswap_32(n);			\
	if (SC_FWRITE(&idata, sizeof(idata), 1, fp) != 1)	\
		w_err = 1;				\
	wrote += 4;					\
}

#define PUT_CACHE_DATA_INT(n)				\
{							\
	walk_data[0]=(((guint32)n)&0x000000ff);			\
	walk_data[1]=(((guint32)n)&0x0000ff00)>>8;		\
	walk_data[2]=(((guint32)n)&0x00ff0000)>>16;		\
	walk_data[3]=(((guint32)n)&0xff000000)>>24;		\
	walk_data += 4;					\
	wrote += 4;					\
}

#define WRITE_CACHE_DATA(data, fp) \
{ \
	size_t len;					\
	if (data == NULL)				\
		len = 0;				\
	else						\
		len = strlen(data);			\
	WRITE_CACHE_DATA_INT(len, fp);			\
	if (w_err == 0 && len > 0) {			\
		if (SC_FWRITE(data, 1, len, fp) != len)	\
			w_err = 1;			\
		wrote += len;				\
	} \
}

#define PUT_CACHE_DATA(data)				\
{							\
	size_t len;					\
	if (data == NULL)				\
		len = 0;				\
	else						\
		len = strlen(data);			\
	PUT_CACHE_DATA_INT(len);			\
	if (len > 0) {					\
		memcpy(walk_data, data, len);		\
		walk_data += len;			\
		wrote += len;				\
	}						\
}

static FILE *msgcache_open_data_file(const gchar *file, guint version,
				     DataOpenMode mode,
				     gchar *buf, size_t buf_size)
{
	FILE *fp;
	gint32 data_ver;

	cm_return_val_if_fail(file != NULL, NULL);

	if (mode == DATA_WRITE) {
		int w_err = 0, wrote = 0;
		if ((fp = g_fopen(file, "wb")) == NULL) {
			FILE_OP_ERROR(file, "fopen");
			return NULL;
		}
		if (change_file_mode_rw(fp, file) < 0)
			FILE_OP_ERROR(file, "chmod");

		WRITE_CACHE_DATA_INT(version, fp);
		if (w_err != 0) {
			g_warning("failed to write int\n");
			fclose(fp);
			return NULL;
		}
		return fp;
	}

	/* check version */
	if ((fp = g_fopen(file, "rb")) == NULL)
		debug_print("Mark/Cache file '%s' not found\n", file);
	else {
		if (buf && buf_size > 0)
			setvbuf(fp, buf, _IOFBF, buf_size);
		if (fread(&data_ver, sizeof(data_ver), 1, fp) != 1 ||
			 version != bswap_32(data_ver)) {
			g_message("%s: Mark/Cache version is different (%u != %u).\n",
				  file, bswap_32(data_ver), version);
			fclose(fp);
			fp = NULL;
		}
		data_ver = bswap_32(data_ver);
	}
	
	if (mode == DATA_READ)
		return fp;

	if (fp) {
		/* reopen with append mode */
		fclose(fp);
		if ((fp = g_fopen(file, "ab")) == NULL)
			FILE_OP_ERROR(file, "fopen");
	} else {
		/* open with overwrite mode if mark file doesn't exist or
		   version is different */
		fp = msgcache_open_data_file(file, version, DATA_WRITE, buf,
					    buf_size);
	}

	return fp;
}

static gint msgcache_read_cache_data_str(FILE *fp, gchar **str, 
					 StringConverter *conv)
{
	gchar *tmpstr = NULL;
	size_t ni;
	guint32 len;

	*str = NULL;
	if (!swapping) {
		if ((ni = fread(&len, sizeof(len), 1, fp) != 1) ||
		    len > G_MAXINT) {
			g_warning("read_data_str: Cache data (len) corrupted, read %zd "
				  "of %zd bytes at offset %ld\n", ni, sizeof(len), 
				  ftell(fp));
			return -1;
		}
	} else {
		if ((ni = fread(&len, sizeof(len), 1, fp) != 1) ||
		    bswap_32(len) > G_MAXINT) {
			g_warning("read_data_str: Cache data (len) corrupted, read %zd "
				  "of %zd bytes at offset %ld\n", ni, sizeof(len), 
				  ftell(fp));
			return -1;
		}
		len = bswap_32(len);
	}

	if (len == 0)
		return 0;

	tmpstr = g_try_malloc(len + 1);

	if(tmpstr == NULL) {
		return -1;
	}

	if ((ni = fread(tmpstr, 1, len, fp)) != len) {
		g_warning("read_data_str: Cache data corrupted, read %zd of %u "
			  "bytes at offset %ld\n", 
			  ni, len, ftell(fp));
		g_free(tmpstr);
		return -1;
	}
	tmpstr[len] = 0;

	if (conv != NULL) {
		*str = conv->convert(conv, tmpstr);
		g_free(tmpstr);
	} else 
		*str = tmpstr;

	return len;
}

static gint msgcache_get_cache_data_str(gchar *src, gchar **str, gint len,
					 StringConverter *conv)
{
	gchar *tmpstr = NULL;

	*str = NULL;

	if (len == 0)
		return 0;

	if(len > 2*1024*1024) {
		g_warning("read_data_str: refusing to allocate %d bytes.\n", len);
		return -1;
	}

	tmpstr = g_try_malloc(len + 1);

	if(tmpstr == NULL) {
		return -1;
	}

	memcpy(tmpstr, src, len);
	tmpstr[len] = 0;

	if (conv != NULL) {
		*str = conv->convert(conv, tmpstr);
		g_free(tmpstr);
	} else 
		*str = tmpstr;

	return len;
}

static gchar *strconv_charset_convert(StringConverter *conv, gchar *srcstr)
{
	CharsetConverter *charsetconv = (CharsetConverter *) conv;

	return conv_codeset_strdup(srcstr, charsetconv->srccharset, charsetconv->dstcharset);
}

static void strconv_charset_free(StringConverter *conv)
{
	CharsetConverter *charsetconv = (CharsetConverter *) conv;

	g_free(charsetconv->srccharset);
	g_free(charsetconv->dstcharset);
}

MsgCache *msgcache_read_cache(FolderItem *item, const gchar *cache_file)
{
	MsgCache *cache;
	FILE *fp;
	MsgInfo *msginfo;
	MsgTmpFlags tmp_flags = 0;
	gchar file_buf[BUFFSIZE];
	guint32 num;
        guint refnum;
	gboolean error = FALSE;
	StringConverter *conv = NULL;
	gchar *srccharset = NULL;
	const gchar *dstcharset = NULL;
	gchar *ref = NULL;
	guint memusage = 0;
	gint tmp_len = 0, map_len = -1;
	char *cache_data = NULL;
	struct stat st;

	cm_return_val_if_fail(cache_file != NULL, NULL);
	cm_return_val_if_fail(item != NULL, NULL);

	swapping = TRUE;

	/* In case we can't open the mark file with MARK_VERSION, check if we can open it with the
	 * swapped MARK_VERSION. As msgcache_open_data_file swaps it too, if this succeeds, 
	 * it means it's the old version (not little-endian) on a big-endian machine. The code has
	 * no effect on x86 as their file doesn't change. */

	if ((fp = msgcache_open_data_file
		(cache_file, CACHE_VERSION, DATA_READ, file_buf, sizeof(file_buf))) == NULL) {
		if ((fp = msgcache_open_data_file
		(cache_file, bswap_32(CACHE_VERSION), DATA_READ, file_buf, sizeof(file_buf))) == NULL)
			return NULL;
		else
			swapping = FALSE;
	}

	debug_print("\tReading %sswapped message cache from %s...\n", swapping?"":"un", cache_file);

	if (folder_has_parent_of_type(item, F_QUEUE)) {
		tmp_flags |= MSG_QUEUED;
	} else if (folder_has_parent_of_type(item, F_DRAFT)) {
		tmp_flags |= MSG_DRAFT;
	}

	if (msgcache_read_cache_data_str(fp, &srccharset, NULL) < 0) {
		fclose(fp);
		return NULL;
	}
	dstcharset = CS_UTF_8;
	if (srccharset == NULL || dstcharset == NULL) {
		conv = NULL;
	} else if (strcmp(srccharset, dstcharset) == 0) {
		debug_print("using Noop Converter\n");

		conv = NULL;
	} else {
		CharsetConverter *charsetconv;

		debug_print("using CharsetConverter\n");

		charsetconv = g_new0(CharsetConverter, 1);
		charsetconv->converter.convert = strconv_charset_convert;
		charsetconv->converter.free = strconv_charset_free;
		charsetconv->srccharset = g_strdup(srccharset);
		charsetconv->dstcharset = g_strdup(dstcharset);

		conv = (StringConverter *) charsetconv;
	}
	g_free(srccharset);

	cache = msgcache_new();

	if (msgcache_use_mmap_read == TRUE) {
		if (fstat(fileno(fp), &st) >= 0)
			map_len = st.st_size;
		else
			map_len = -1;
		if (map_len > 0) {
#ifdef G_OS_WIN32
			cache_data = NULL;
			HANDLE hFile, hMapping;
			hFile = (HANDLE) _get_osfhandle (fileno(fp));
			if (hFile == (HANDLE) -1)
				goto w32_fail;
			hMapping = CreateFileMapping(hFile, NULL, PAGE_WRITECOPY, 0, 0, NULL);
			if (!hMapping)
				goto w32_fail;
			cache_data = (unsigned char *)MapViewOfFile(hMapping, FILE_MAP_COPY, 0, 0, 0);
			CloseHandle (hMapping);
		w32_fail:
			;
#else
			cache_data = mmap(NULL, map_len, PROT_READ, MAP_PRIVATE, fileno(fp), 0);
#endif
		}
	} else {
		cache_data = NULL;
	}
	if (cache_data != NULL && cache_data != MAP_FAILED) {
		int rem_len = map_len-ftell(fp);
		char *walk_data = cache_data+ftell(fp);

		while(rem_len > 0) {
			GET_CACHE_DATA_INT(num);
			
			msginfo = procmsg_msginfo_new();
			msginfo->msgnum = num;
			memusage += sizeof(MsgInfo);

			GET_CACHE_DATA_INT(msginfo->size);
			GET_CACHE_DATA_INT(msginfo->mtime);
			GET_CACHE_DATA_INT(msginfo->date_t);
			GET_CACHE_DATA_INT(msginfo->flags.tmp_flags);

			GET_CACHE_DATA(msginfo->fromname, memusage);

			GET_CACHE_DATA(msginfo->date, memusage);
			GET_CACHE_DATA(msginfo->from, memusage);
			GET_CACHE_DATA(msginfo->to, memusage);
			GET_CACHE_DATA(msginfo->cc, memusage);
			GET_CACHE_DATA(msginfo->newsgroups, memusage);
			GET_CACHE_DATA(msginfo->subject, memusage);
			GET_CACHE_DATA(msginfo->msgid, memusage);
			GET_CACHE_DATA(msginfo->inreplyto, memusage);
			GET_CACHE_DATA(msginfo->xref, memusage);

			GET_CACHE_DATA_INT(msginfo->planned_download);
			GET_CACHE_DATA_INT(msginfo->total_size);
			GET_CACHE_DATA_INT(refnum);

			for (; refnum != 0; refnum--) {
				ref = NULL;

				GET_CACHE_DATA(ref, memusage);

				if (ref && *ref)
					msginfo->references =
						g_slist_prepend(msginfo->references, ref);
			}
			if (msginfo->references)
				msginfo->references =
					g_slist_reverse(msginfo->references);

			msginfo->folder = item;
			msginfo->flags.tmp_flags |= tmp_flags;

			g_hash_table_insert(cache->msgnum_table, &msginfo->msgnum, msginfo);
			if(msginfo->msgid)
				g_hash_table_insert(cache->msgid_table, msginfo->msgid, msginfo);
		}

#ifdef G_OS_WIN32
		UnmapViewOfFile((void*) cache_data);
#else
		munmap(cache_data, map_len);
#endif
	} else {
		while (fread(&num, sizeof(num), 1, fp) == 1) {
			if (swapping)
				num = bswap_32(num);

			msginfo = procmsg_msginfo_new();
			msginfo->msgnum = num;
			memusage += sizeof(MsgInfo);

			READ_CACHE_DATA_INT(msginfo->size, fp);
			READ_CACHE_DATA_INT(msginfo->mtime, fp);
			READ_CACHE_DATA_INT(msginfo->date_t, fp);
			READ_CACHE_DATA_INT(msginfo->flags.tmp_flags, fp);

			READ_CACHE_DATA(msginfo->fromname, fp, memusage);

			READ_CACHE_DATA(msginfo->date, fp, memusage);
			READ_CACHE_DATA(msginfo->from, fp, memusage);
			READ_CACHE_DATA(msginfo->to, fp, memusage);
			READ_CACHE_DATA(msginfo->cc, fp, memusage);
			READ_CACHE_DATA(msginfo->newsgroups, fp, memusage);
			READ_CACHE_DATA(msginfo->subject, fp, memusage);
			READ_CACHE_DATA(msginfo->msgid, fp, memusage);
			READ_CACHE_DATA(msginfo->inreplyto, fp, memusage);
			READ_CACHE_DATA(msginfo->xref, fp, memusage);

			READ_CACHE_DATA_INT(msginfo->planned_download, fp);
			READ_CACHE_DATA_INT(msginfo->total_size, fp);
			READ_CACHE_DATA_INT(refnum, fp);

			for (; refnum != 0; refnum--) {
				ref = NULL;

				READ_CACHE_DATA(ref, fp, memusage);

				if (ref && *ref)
					msginfo->references =
						g_slist_prepend(msginfo->references, ref);
			}
			if (msginfo->references)
				msginfo->references =
					g_slist_reverse(msginfo->references);

			msginfo->folder = item;
			msginfo->flags.tmp_flags |= tmp_flags;

			g_hash_table_insert(cache->msgnum_table, &msginfo->msgnum, msginfo);
			if(msginfo->msgid)
				g_hash_table_insert(cache->msgid_table, msginfo->msgid, msginfo);
		}
	}
bail_err:
	fclose(fp);

	if (conv != NULL) {
		if (conv->free != NULL)
			conv->free(conv);
		g_free(conv);
	}

	if(error) {
		msgcache_destroy(cache);
		return NULL;
	}

	cache->last_access = time(NULL);
	cache->memusage = memusage;

	debug_print("done. (%d items read)\n", g_hash_table_size(cache->msgnum_table));
	debug_print("Cache size: %d messages, %u bytes\n", g_hash_table_size(cache->msgnum_table), cache->memusage);

	return cache;
}

void msgcache_read_mark(MsgCache *cache, const gchar *mark_file)
{
	FILE *fp;
	MsgInfo *msginfo;
	MsgPermFlags perm_flags;
	guint32 num;
	gint map_len = -1;
	char *cache_data = NULL;
	struct stat st;
	gboolean error;

	swapping = TRUE;

	/* In case we can't open the mark file with MARK_VERSION, check if we can open it with the
	 * swapped MARK_VERSION. As msgcache_open_data_file swaps it too, if this succeeds, 
	 * it means it's the old version (not little-endian) on a big-endian machine. The code has
	 * no effect on x86 as their file doesn't change. */

	if ((fp = msgcache_open_data_file(mark_file, MARK_VERSION, DATA_READ, NULL, 0)) == NULL) {
		/* see if it isn't swapped ? */
		if ((fp = msgcache_open_data_file(mark_file, bswap_32(MARK_VERSION), DATA_READ, NULL, 0)) == NULL)
			return;
		else
			swapping = FALSE; /* yay */
	}
	debug_print("reading %sswapped mark file.\n", swapping?"":"un");
	
	if (msgcache_use_mmap_read) {
		if (fstat(fileno(fp), &st) >= 0)
			map_len = st.st_size;
		else
			map_len = -1;
		if (map_len > 0) {
#ifdef G_OS_WIN32
			cache_data = NULL;
			HANDLE hFile, hMapping;
			hFile = (HANDLE) _get_osfhandle (fileno(fp));
			if (hFile == (HANDLE) -1)
				goto w32_fail2;
			hMapping = CreateFileMapping(hFile, NULL, PAGE_WRITECOPY, 0, 0, NULL);
			if (!hMapping)
				goto w32_fail2;
			cache_data = (unsigned char *)MapViewOfFile(hMapping, FILE_MAP_COPY, 0, 0, 0);
			CloseHandle (hMapping);
		w32_fail2:
			;
#else
			cache_data = mmap(NULL, map_len, PROT_READ, MAP_PRIVATE, fileno(fp), 0);
#endif
		}
	} else {
		cache_data = NULL;
	}
	if (cache_data != NULL && cache_data != MAP_FAILED) {
		int rem_len = map_len-ftell(fp);
		char *walk_data = cache_data+ftell(fp);

		while(rem_len > 0) {
			GET_CACHE_DATA_INT(num);
			GET_CACHE_DATA_INT(perm_flags);
			msginfo = g_hash_table_lookup(cache->msgnum_table, &num);
			if(msginfo) {
				msginfo->flags.perm_flags = perm_flags;
			}
		}
#ifdef G_OS_WIN32
		UnmapViewOfFile((void*) cache_data);
#else
		munmap(cache_data, map_len);
#endif
	} else {
		while (fread(&num, sizeof(num), 1, fp) == 1) {
			if (swapping)
				num = bswap_32(num);
			if (fread(&perm_flags, sizeof(perm_flags), 1, fp) != 1) break;
			if (swapping)
				perm_flags = bswap_32(perm_flags);
			msginfo = g_hash_table_lookup(cache->msgnum_table, &num);
			if(msginfo) {
				msginfo->flags.perm_flags = perm_flags;
			}
		}	
	}
bail_err:
	fclose(fp);
	if (error) {
		debug_print("error reading cache mark from %s\n", mark_file);
	}
}

void msgcache_read_tags(MsgCache *cache, const gchar *tags_file)
{
	FILE *fp;
	MsgInfo *msginfo;
	guint32 num;
	gint map_len = -1;
	char *cache_data = NULL;
	struct stat st;
	gboolean error = FALSE;

	swapping = TRUE;

	/* In case we can't open the mark file with MARK_VERSION, check if we can open it with the
	 * swapped MARK_VERSION. As msgcache_open_data_file swaps it too, if this succeeds, 
	 * it means it's the old version (not little-endian) on a big-endian machine. The code has
	 * no effect on x86 as their file doesn't change. */

	if ((fp = msgcache_open_data_file(tags_file, TAGS_VERSION, DATA_READ, NULL, 0)) == NULL) {
		/* see if it isn't swapped ? */
		if ((fp = msgcache_open_data_file(tags_file, bswap_32(TAGS_VERSION), DATA_READ, NULL, 0)) == NULL)
			return;
		else
			swapping = FALSE; /* yay */
	}
	debug_print("reading %sswapped tags file.\n", swapping?"":"un");
	
	if (msgcache_use_mmap_read) {
		if (fstat(fileno(fp), &st) >= 0)
			map_len = st.st_size;
		else
			map_len = -1;
		if (map_len > 0) {
#ifdef G_OS_WIN32
			cache_data = NULL;
			HANDLE hFile, hMapping;
			hFile = (HANDLE) _get_osfhandle (fileno(fp));
			if (hFile == (HANDLE) -1)
				goto w32_fail6;
			hMapping = CreateFileMapping(hFile, NULL, PAGE_WRITECOPY, 0, 0, NULL);
			if (!hMapping)
				goto w32_fail6;
			cache_data = (unsigned char *)MapViewOfFile(hMapping, FILE_MAP_COPY, 0, 0, 0);
			CloseHandle (hMapping);
		w32_fail6:
			;
#else
			cache_data = mmap(NULL, map_len, PROT_READ, MAP_PRIVATE, fileno(fp), 0);
#endif
		}
	} else {
		cache_data = NULL;
	}
	if (cache_data != NULL && cache_data != MAP_FAILED) {
		int rem_len = map_len-ftell(fp);
		char *walk_data = cache_data+ftell(fp);

		while(rem_len > 0) {
			gint id = -1;
			GET_CACHE_DATA_INT(num);
			msginfo = g_hash_table_lookup(cache->msgnum_table, &num);
			if(msginfo) {
				g_slist_free(msginfo->tags);
				msginfo->tags = NULL;
				do {
					GET_CACHE_DATA_INT(id);
					if (id > 0) {
						msginfo->tags = g_slist_prepend(
							msginfo->tags, 
							GINT_TO_POINTER(id));
					}
				} while (id > 0);
				msginfo->tags = g_slist_reverse(msginfo->tags);
			}
		}
#ifdef G_OS_WIN32
		UnmapViewOfFile((void*) cache_data);
#else
		munmap(cache_data, map_len);
#endif
	} else {
		while (fread(&num, sizeof(num), 1, fp) == 1) {
			gint id = -1;
			if (swapping)
				num = bswap_32(num);
			msginfo = g_hash_table_lookup(cache->msgnum_table, &num);
			if(msginfo) {
				g_slist_free(msginfo->tags);
				msginfo->tags = NULL;
				do {
					if (fread(&id, sizeof(id), 1, fp) != 1) 
						id = -1;
					if (swapping)
						id = bswap_32(id);
					if (id > 0) {
						msginfo->tags = g_slist_prepend(
							msginfo->tags, 
							GINT_TO_POINTER(id));
					}
				} while (id > 0);
				msginfo->tags = g_slist_reverse(msginfo->tags);
			}
		}
	}
bail_err:
	fclose(fp);
	if (error) {
		debug_print("error reading cache tags from %s\n", tags_file);
	}
}

static int msgcache_write_cache(MsgInfo *msginfo, FILE *fp)
{
	MsgTmpFlags flags = msginfo->flags.tmp_flags & MSG_CACHED_FLAG_MASK;
	GSList *cur;
	int w_err = 0, wrote = 0;

	WRITE_CACHE_DATA_INT(msginfo->msgnum, fp);
	WRITE_CACHE_DATA_INT(msginfo->size, fp);
	WRITE_CACHE_DATA_INT(msginfo->mtime, fp);
	WRITE_CACHE_DATA_INT(msginfo->date_t, fp);
	WRITE_CACHE_DATA_INT(flags, fp);

	WRITE_CACHE_DATA(msginfo->fromname, fp);

	WRITE_CACHE_DATA(msginfo->date, fp);
	WRITE_CACHE_DATA(msginfo->from, fp);
	WRITE_CACHE_DATA(msginfo->to, fp);
	WRITE_CACHE_DATA(msginfo->cc, fp);
	WRITE_CACHE_DATA(msginfo->newsgroups, fp);
	WRITE_CACHE_DATA(msginfo->subject, fp);
	WRITE_CACHE_DATA(msginfo->msgid, fp);
	WRITE_CACHE_DATA(msginfo->inreplyto, fp);
	WRITE_CACHE_DATA(msginfo->xref, fp);
	WRITE_CACHE_DATA_INT(msginfo->planned_download, fp);
	WRITE_CACHE_DATA_INT(msginfo->total_size, fp);
        
	WRITE_CACHE_DATA_INT(g_slist_length(msginfo->references), fp);

	for (cur = msginfo->references; cur != NULL; cur = cur->next) {
		WRITE_CACHE_DATA((gchar *)cur->data, fp);
	}
	return w_err ? -1 : wrote;
}

static int msgcache_write_flags(MsgInfo *msginfo, FILE *fp)
{
	MsgPermFlags flags = msginfo->flags.perm_flags;
	int w_err = 0, wrote = 0;
	WRITE_CACHE_DATA_INT(msginfo->msgnum, fp);
	WRITE_CACHE_DATA_INT(flags, fp);
	return w_err ? -1 : wrote;
}

static int msgcache_write_tags(MsgInfo *msginfo, FILE *fp)
{
	GSList *cur = msginfo->tags;
	int w_err = 0, wrote = 0;

	WRITE_CACHE_DATA_INT(msginfo->msgnum, fp);
	for (; cur; cur = cur->next) {
		gint id = GPOINTER_TO_INT(cur->data);
		if (tags_get_tag(id) != NULL) {
			WRITE_CACHE_DATA_INT(id, fp);
		}
	}
	WRITE_CACHE_DATA_INT(-1, fp);

	return w_err ? -1 : wrote;
}

struct write_fps
{
	FILE *cache_fp;
	FILE *mark_fp;
	FILE *tags_fp;
	int error;
	guint cache_size;
	guint mark_size;
	guint tags_size;
};

static void msgcache_write_func(gpointer key, gpointer value, gpointer user_data)
{
	MsgInfo *msginfo;
	struct write_fps *write_fps;
	int tmp;

	msginfo = (MsgInfo *)value;
	write_fps = user_data;

	if (write_fps->cache_fp) {
		tmp = msgcache_write_cache(msginfo, write_fps->cache_fp);
		if (tmp < 0)
			write_fps->error = 1;
		else
			write_fps->cache_size += tmp;
	}
	if (write_fps->mark_fp) {
	tmp= msgcache_write_flags(msginfo, write_fps->mark_fp);
		if (tmp < 0)
			write_fps->error = 1;
		else
			write_fps->mark_size += tmp;
		}
	if (write_fps->tags_fp) {
		tmp = msgcache_write_tags(msginfo, write_fps->tags_fp);
		if (tmp < 0)
			write_fps->error = 1;
		else
			write_fps->tags_size += tmp;
	}
}

gint msgcache_write(const gchar *cache_file, const gchar *mark_file, const gchar *tags_file, MsgCache *cache)
{
	struct write_fps write_fps;
	gchar *new_cache, *new_mark, *new_tags;
	int w_err = 0, wrote = 0;

	START_TIMING("");
	cm_return_val_if_fail(cache != NULL, -1);

	new_cache = g_strconcat(cache_file, ".new", NULL);
	new_mark  = g_strconcat(mark_file, ".new", NULL);
	new_tags  = g_strconcat(tags_file, ".new", NULL);

	write_fps.error = 0;
	write_fps.cache_size = 0;
	write_fps.mark_size = 0;
	write_fps.tags_size = 0;

	/* open files and write headers */

	if (cache_file) {
		write_fps.cache_fp = msgcache_open_data_file(new_cache, CACHE_VERSION,
			DATA_WRITE, NULL, 0);
		if (write_fps.cache_fp == NULL) {
			g_free(new_cache);
			g_free(new_mark);
			g_free(new_tags);
			return -1;
		}
		WRITE_CACHE_DATA(CS_UTF_8, write_fps.cache_fp);
	} else {
		write_fps.cache_fp = NULL;
	}

	if (w_err != 0) {
		g_warning("failed to write charset\n");
		fclose(write_fps.cache_fp);
		claws_unlink(new_cache);
		g_free(new_cache);
		g_free(new_mark);
		g_free(new_tags);
		return -1;
	}

	if (mark_file) {
		write_fps.mark_fp = msgcache_open_data_file(new_mark, MARK_VERSION,
			DATA_WRITE, NULL, 0);
		if (write_fps.mark_fp == NULL) {
			fclose(write_fps.cache_fp);
			claws_unlink(new_cache);
			g_free(new_cache);
			g_free(new_mark);
			g_free(new_tags);
			return -1;
		}
	} else {
		write_fps.mark_fp = NULL;
	}

	if (tags_file) {
		write_fps.tags_fp = msgcache_open_data_file(new_tags, TAGS_VERSION,
			DATA_WRITE, NULL, 0);
		if (write_fps.tags_fp == NULL) {
			fclose(write_fps.cache_fp);
			fclose(write_fps.mark_fp);
			claws_unlink(new_cache);
			claws_unlink(new_mark);
			g_free(new_cache);
			g_free(new_mark);
			g_free(new_tags);
			return -1;
		}
	} else {
		write_fps.tags_fp = NULL;
	}

	debug_print("\tWriting message cache to %s and %s...\n", new_cache, new_mark);

	if (write_fps.cache_fp && change_file_mode_rw(write_fps.cache_fp, new_cache) < 0)
		FILE_OP_ERROR(new_cache, "chmod");

	/* headers written, note file size */
	if (write_fps.cache_fp)
		write_fps.cache_size = ftell(write_fps.cache_fp);
	if (write_fps.mark_fp)
		write_fps.mark_size = ftell(write_fps.mark_fp);
	if (write_fps.tags_fp)
		write_fps.tags_size = ftell(write_fps.tags_fp);

#ifdef HAVE_FWRITE_UNLOCKED
	/* lock files for write once (instead of once per fwrite) */
	if (write_fps.cache_fp)
		flockfile(write_fps.cache_fp);
	if (write_fps.mark_fp)
		flockfile(write_fps.mark_fp);
	if (write_fps.tags_fp)
		flockfile(write_fps.tags_fp);
#endif
	/* write data to the files */
	g_hash_table_foreach(cache->msgnum_table, msgcache_write_func, (gpointer)&write_fps);
#ifdef HAVE_FWRITE_UNLOCKED
	/* unlock files */
	if (write_fps.cache_fp)
		funlockfile(write_fps.cache_fp);
	if (write_fps.mark_fp)
		funlockfile(write_fps.mark_fp);
	if (write_fps.tags_fp)
		funlockfile(write_fps.tags_fp);
#endif
	/* flush buffers */
	if (write_fps.cache_fp)
		write_fps.error |= (fflush(write_fps.cache_fp) != 0);
	if (write_fps.mark_fp)
		write_fps.error |= (fflush(write_fps.mark_fp) != 0);
	if (write_fps.tags_fp)
		write_fps.error |= (fflush(write_fps.tags_fp) != 0);

	/* sync to filesystem */
	if (prefs_common.flush_metadata && write_fps.cache_fp)
		write_fps.error |= (fsync(fileno(write_fps.cache_fp)) != 0);
	if (prefs_common.flush_metadata && write_fps.mark_fp)
		write_fps.error |= (fsync(fileno(write_fps.mark_fp)) != 0);
	if (prefs_common.flush_metadata && write_fps.tags_fp)
		write_fps.error |= (fsync(fileno(write_fps.tags_fp)) != 0);

	/* close files */
	if (write_fps.cache_fp)
		write_fps.error |= (fclose(write_fps.cache_fp) != 0);
	if (write_fps.mark_fp)
		write_fps.error |= (fclose(write_fps.mark_fp) != 0);
	if (write_fps.tags_fp)
		write_fps.error |= (fclose(write_fps.tags_fp) != 0);


	if (write_fps.error != 0) {
		/* in case of error, forget all */
		claws_unlink(new_cache);
		claws_unlink(new_mark);
		claws_unlink(new_tags);
		g_free(new_cache);
		g_free(new_mark);
		g_free(new_tags);
		return -1;
	} else {
		/* switch files */
		if (cache_file)
			move_file(new_cache, cache_file, TRUE);
		if (mark_file)
			move_file(new_mark, mark_file, TRUE);
		if (tags_file)
			move_file(new_tags, tags_file, TRUE);
		cache->last_access = time(NULL);
	}

	g_free(new_cache);
	g_free(new_mark);
	g_free(new_tags);
	debug_print("done.\n");
	END_TIMING();
	return 0;
}

