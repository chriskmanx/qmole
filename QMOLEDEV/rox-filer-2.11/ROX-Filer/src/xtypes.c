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


/* 
 * xtypes.c - Extended filesystem attribute support for MIME types
 */

#include "config.h"
#include <stdio.h>
#include <string.h>
#include <errno.h>

#include <unistd.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>

#include <glib.h>

#include "global.h"
#include "type.h"
#include "xtypes.h"
#include "options.h"

Option o_xattr_ignore;

#define RETURN_IF_IGNORED(val) if(o_xattr_ignore.int_value) return (val)

#if defined(HAVE_GETXATTR)
/* Linux implementation */

#include <dlfcn.h>

static int (*dyn_setxattr)(const char *path, const char *name,
		     const void *value, size_t size, int flags) = NULL;
static ssize_t (*dyn_getxattr)(const char *path, const char *name,
			 void *value, size_t size) = NULL;
static ssize_t (*dyn_listxattr)(const char *path, char *list,
			 size_t size) = NULL;

void xattr_init(void)
{
	void *libc;
	
	libc = dlopen("libc.so.6", RTLD_LAZY | RTLD_NOLOAD);
	if (!libc)
	{
		/* Try a different name for uClib support */
		libc = dlopen("libc.so", RTLD_LAZY | RTLD_NOLOAD);
	}

	if (!libc)
		return;	/* Give up on xattr support */

	dyn_setxattr = (void *) dlsym(libc, "setxattr");
	dyn_getxattr = (void *) dlsym(libc, "getxattr");
	dyn_listxattr = (void *) dlsym(libc, "listxattr");
	
	option_add_int(&o_xattr_ignore, "xattr_ignore", FALSE);
}

int xattr_supported(const char *path)
{
	char buf[1];
	ssize_t nent;
	
	RETURN_IF_IGNORED(FALSE);
	
	if (!dyn_getxattr)
		return FALSE;

	if(path) {
		errno=0;
		nent=dyn_getxattr(path, XATTR_MIME_TYPE, buf, sizeof(buf));

		if(nent<0 && errno==ENOTSUP)
			return FALSE;
	}

	return TRUE;
}

int xattr_have(const char *path)
{
	char buf[1];
	ssize_t nent;
	
	RETURN_IF_IGNORED(FALSE);
	
	if (!dyn_listxattr)
		return FALSE;

	errno=0;
	nent=dyn_listxattr(path, buf, sizeof(buf));

	if(nent<0 && errno==ERANGE)
		return TRUE;
	
	return (nent>0);
}

gchar *xattr_get(const char *path, const char *attr, int *len)
{
	ssize_t size;
	gchar *buf;

	RETURN_IF_IGNORED(NULL);
	
	if (!dyn_getxattr)
		return NULL;

	size = dyn_getxattr(path, attr, "", 0);
	if (size > 0)
	{
		int new_size;

		buf = g_new(gchar, size + 1);
		new_size = dyn_getxattr(path, attr, buf, size);

		if(size == new_size)
		{
			buf[size] = '\0';
			
			if(len)
				*len=(int) size;

			return buf;
		}

		g_free(buf);
	}

	return NULL;

}

/* 0 on success */
int xattr_set(const char *path, const char *attr,
	      const char *value, int value_len)
{
	if(o_xattr_ignore.int_value)
	{
		errno = ENOSYS;
		return 1;
	}

	if (!dyn_setxattr)
	{
		errno = ENOSYS;
		return 1; /* Set attr failed */
	}

	if(value && value_len<0)
		value_len = strlen(value);

	return dyn_setxattr(path, attr, value, value_len, 0);
}


#elif defined(HAVE_ATTROPEN)

/* Solaris 9 implementation */

void xattr_init(void)
{	
	option_add_int(&o_xattr_ignore, "xattr_ignore", FALSE);
}

int xattr_supported(const char *path)
{
	RETURN_IF_IGNORED(FALSE);
#ifdef _PC_XATTR_ENABLED
	if(!path)
		return TRUE;
	
	return pathconf(path, _PC_XATTR_ENABLED);
#else
	return FALSE;
#endif
}

int xattr_have(const char *path)
{
	RETURN_IF_IGNORED(FALSE);
#ifdef _PC_XATTR_EXISTS
	return pathconf(path, _PC_XATTR_EXISTS)>0;
#else
	return FALSE;
#endif
}

#define MAX_ATTR_SIZE BUFSIZ
gchar *xattr_get(const char *path, const char *attr, int *len)
{
	int fd;
	char *buf=NULL;
	int nb;

	RETURN_IF_IGNORED(NULL);

#ifdef _PC_XATTR_EXISTS
	if(!pathconf(path, _PC_XATTR_EXISTS))
		return NULL;
#endif

	fd=attropen(path, attr, O_RDONLY);
  
	if(fd>=0) {
		buf = g_new(gchar, MAX_ATTR_SIZE);
		nb=read(fd, buf, MAX_ATTR_SIZE);
		if(nb>0) {
			buf[nb]=0;
		}
		close(fd);

		if(len)
			*len=nb;
	}

	return buf;
}

int xattr_set(const char *path, const char *attr,
	      const char *value, int value_len)
{
	int fd;
	int nb;

	if(o_xattr_ignore.int_value)
	{
		errno = ENOSYS;
		return 1;
	}

	if(value && value_len<0)
		value_len = strlen(value);

	fd=attropen(path, attr, O_WRONLY|O_CREAT, 0644);
	if(fd>0) {
		
		nb=write(fd, value, value_len);
		if(nb==value_len)
			ftruncate(fd, (off_t) nb);

		close(fd);

		if(nb>0)
			return 0;
	}
  
	return 1; /* Set type failed */
}

#else
/* No extended attributes available */

void xattr_init(void)
{
}

int xattr_supported(const char *path)
{
	return FALSE;
}

int xattr_have(const char *path)
{
	return FALSE;
}

gchar *xattr_get(const char *path, const char *attr, int *len)
{
	/* Fall back to non-extended */
	return NULL;
}

int xattr_set(const char *path, const char *attr,
	      const char *value, int value_len)
{
	errno = ENOSYS;
	return 1; /* Set type failed */
}

#endif

MIME_type *xtype_get(const char *path)
{
	MIME_type *type = NULL;
	gchar *buf;
	char *nl;

	buf = xattr_get(path, XATTR_MIME_TYPE, NULL);

	if(buf)
	{
		nl = strchr(buf, '\n');
		if(nl)
			*nl = 0;
		type = mime_type_lookup(buf);
		g_free(buf);
	}
	return type;
}

int xtype_set(const char *path, const MIME_type *type)
{
	int res;
	gchar *ttext;

	if(o_xattr_ignore.int_value)
	{
		errno = ENOSYS;
		return 1;
	}

	ttext = g_strdup_printf("%s/%s", type->media_type, type->subtype);
	res = xattr_set(path, XATTR_MIME_TYPE, ttext, -1);
	g_free(ttext);

	return res;
}

