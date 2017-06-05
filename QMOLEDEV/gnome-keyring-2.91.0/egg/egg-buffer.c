/* -*- Mode: C; indent-tabs-mode: t; c-basic-offset: 8; tab-width: 8 -*- */
/* egg-buffer.c - Generic data buffer, used by openssh, gnome-keyring

   Copyright (C) 2007 Stefan Walter

   The Gnome Keyring Library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Library General Public License as
   published by the Free Software Foundation; either version 2 of the
   License, or (at your option) any later version.

   The Gnome Keyring Library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Library General Public License for more details.

   You should have received a copy of the GNU Library General Public
   License along with the Gnome Library; see the file COPYING.LIB.  If not,
   write to the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
   Boston, MA 02111-1307, USA.

   Author: Stef Walter <stef@memberwebs.com>
*/
#include "config.h"

#include <string.h>
#include <stdarg.h>

#include "egg-buffer.h"

#define DEFAULT_ALLOCATOR  ((EggBufferAllocator)realloc)

int
egg_buffer_init (EggBuffer *buffer, size_t reserve)
{
	return egg_buffer_init_full (buffer, reserve, NULL);
}

int
egg_buffer_init_full (EggBuffer *buffer, size_t reserve, EggBufferAllocator allocator)
{
	memset (buffer, 0, sizeof (*buffer));
	
	if (!allocator) 
		allocator = DEFAULT_ALLOCATOR;
	if (reserve == 0)
		reserve = 64;

	buffer->buf = (allocator) (NULL, reserve);
	if (!buffer->buf) {
		buffer->failures++;
		return 0;
	}

	buffer->len = 0;
	buffer->allocated_len = reserve;
	buffer->failures = 0;
	buffer->allocator = allocator;

	return 1;
}

void
egg_buffer_init_static (EggBuffer* buffer, const unsigned char *buf, size_t len)
{
	memset (buffer, 0, sizeof (*buffer));

	buffer->buf = (unsigned char*)buf;
	buffer->len = len;
	buffer->allocated_len = len;
	buffer->failures = 0;

	/* A null allocator, and the buffer can't change in size */
	buffer->allocator = NULL;	
}

void
egg_buffer_init_allocated (EggBuffer *buffer, unsigned char *buf, size_t len,
                           EggBufferAllocator allocator)
{
	memset (buffer, 0, sizeof (*buffer));
	
	if (!allocator) 
		allocator = DEFAULT_ALLOCATOR;

	buffer->buf = buf;
	buffer->len = len;
	buffer->allocated_len = len;
	buffer->failures = 0;
	buffer->allocator = allocator;
}

void 
egg_buffer_reset (EggBuffer *buffer)
{
	memset (buffer->buf, 0, buffer->allocated_len);
	buffer->len = 0;
	buffer->failures = 0;
}

void
egg_buffer_uninit (EggBuffer *buffer)
{
	if (!buffer)
		return;

	/* 
	 * Free the memory block using allocator. If no allocator, 
	 * then this memory is ownerd elsewhere and not to be freed. 
	 */
	if (buffer->buf && buffer->allocator)
		(buffer->allocator) (buffer->buf, 0);
		
	memset (buffer, 0, sizeof (*buffer));
}

unsigned char*
egg_buffer_uninit_steal (EggBuffer *buffer, size_t *n_result)
{
	unsigned char *result;

	if (n_result)
		*n_result = buffer->len;
	result = buffer->buf;

	memset (buffer, 0, sizeof (*buffer));

	return result;
}

int
egg_buffer_set_allocator (EggBuffer *buffer, EggBufferAllocator allocator)
{
	unsigned char *buf = NULL;
	
	if (!allocator)
		allocator = DEFAULT_ALLOCATOR;
	if (buffer->allocator == allocator)
		return 1;
	
	if (buffer->allocated_len) {
		/* Reallocate memory block using new allocator */
		buf = (allocator) (NULL, buffer->allocated_len);
		if (buf == NULL)
			return 0;
		
		/* Copy stuff into new memory */
		memcpy (buf, buffer->buf, buffer->allocated_len);
	}
		
	/* If old wasn't static, then free it */
	if (buffer->allocator && buffer->buf)
		(buffer->allocator) (buffer->buf, 0);
		
	buffer->buf = buf;
	buffer->allocator = allocator;
	
	return 1;
}

int
egg_buffer_equal (EggBuffer *b1, EggBuffer *b2)
{
	if (b1->len != b2->len)
		return 0;
	return memcmp (b1->buf, b2->buf, b1->len) == 0;
}

int
egg_buffer_reserve (EggBuffer *buffer, size_t len)
{
	unsigned char *newbuf;
	size_t newlen;

	if (len < buffer->allocated_len)
		return 1;
		
	/* Calculate a new length, minimize number of buffer allocations */
	newlen = buffer->allocated_len * 2;
	if (len > newlen)
		newlen += len;
	
	/* Memory owned elsewhere can't be reallocated */	
	if (!buffer->allocator) {
		buffer->failures++;
		return 0;
	}

	/* Reallocate built in buffer using allocator */
	newbuf = (buffer->allocator) (buffer->buf, newlen);
	if (!newbuf) {
		buffer->failures++;
		return 0;
	}

	buffer->buf = newbuf;
	buffer->allocated_len = newlen;

	return 1;
}

int
egg_buffer_resize (EggBuffer *buffer, size_t len)
{
	if (!egg_buffer_reserve (buffer, len))
		return 0;
		
	buffer->len = len;
	return 1;
}

unsigned char*
egg_buffer_add_empty (EggBuffer *buffer, size_t len)
{
	size_t pos = buffer->len;
	if (!egg_buffer_reserve (buffer, buffer->len + len))
		return NULL;
	buffer->len += len;
	return buffer->buf + pos;
}

int 
egg_buffer_append (EggBuffer *buffer, const unsigned char *val,
                             size_t len)
{
	if (!egg_buffer_reserve (buffer, buffer->len + len))
		return 0; /* failures already incremented */
	memcpy (buffer->buf + buffer->len, val, len);
	buffer->len += len;
	return 1;
}

int
egg_buffer_add_byte (EggBuffer *buffer, unsigned char val)
{
	if (!egg_buffer_reserve (buffer, buffer->len + 1))
		return 0; /* failures already incremented */
	buffer->buf[buffer->len] = val;
	buffer->len++;
	return 1;
}

int
egg_buffer_get_byte (EggBuffer *buffer, size_t offset,
                               size_t *next_offset, unsigned char *val)
{
	unsigned char *ptr;
	if (buffer->len < 1 || offset > buffer->len - 1) {
		buffer->failures++;
		return 0;
	}
	ptr = (unsigned char*)buffer->buf + offset;
	if (val != NULL)
		*val = *ptr;
	if (next_offset != NULL)
		*next_offset = offset + 1;
	return 1;
}

void
egg_buffer_encode_uint16 (unsigned char* buf, uint16_t val)
{
	buf[0] = (val >> 8) & 0xff;
	buf[1] = (val >> 0) & 0xff;	
}

uint16_t
egg_buffer_decode_uint16 (unsigned char* buf)
{
	uint16_t val = buf[0] << 8 | buf[1];
	return val;
}

int
egg_buffer_add_uint16 (EggBuffer *buffer, uint16_t val)
{
	if (!egg_buffer_reserve (buffer, buffer->len + 2))
		return 0; /* failures already incremented */
	buffer->len += 2;
	egg_buffer_set_uint16 (buffer, buffer->len - 2, val);
	return 1;	
}

int
egg_buffer_set_uint16 (EggBuffer *buffer, size_t offset, uint16_t val)
{
	unsigned char *ptr;
	if (buffer->len < 2 || offset > buffer->len - 2) {
		buffer->failures++;
		return 0;
	}
	ptr = (unsigned char*)buffer->buf + offset;
	egg_buffer_encode_uint16 (ptr, val);
	return 1;
}

int
egg_buffer_get_uint16 (EggBuffer *buffer, size_t offset,
                       size_t *next_offset, uint16_t *val)
{
	unsigned char *ptr;
	if (buffer->len < 2 || offset > buffer->len - 2) {
		buffer->failures++;
		return 0;
	}
	ptr = (unsigned char*)buffer->buf + offset;
	if (val != NULL)
		*val = egg_buffer_decode_uint16 (ptr);
	if (next_offset != NULL)
		*next_offset = offset + 2;
	return 1;	
}

void 
egg_buffer_encode_uint32 (unsigned char* buf, uint32_t val)
{
	buf[0] = (val >> 24) & 0xff;
	buf[1] = (val >> 16) & 0xff;
	buf[2] = (val >> 8) & 0xff;
	buf[3] = (val >> 0) & 0xff;
}

uint32_t
egg_buffer_decode_uint32 (unsigned char* ptr)
{
	uint32_t val = ptr[0] << 24 | ptr[1] << 16 | ptr[2] << 8 | ptr[3];
	return val;
}

int 
egg_buffer_add_uint32 (EggBuffer *buffer, uint32_t val)
{
	if (!egg_buffer_reserve (buffer, buffer->len + 4))
		return 0; /* failures already incremented */
	buffer->len += 4;
	egg_buffer_set_uint32 (buffer, buffer->len - 4, val);
	return 1;
}

int
egg_buffer_set_uint32 (EggBuffer *buffer, size_t offset, uint32_t val)
{
	unsigned char *ptr;
	if (buffer->len < 4 || offset > buffer->len - 4) {
		buffer->failures++;
		return 0;
	}
	ptr = (unsigned char*)buffer->buf + offset;
	egg_buffer_encode_uint32 (ptr, val);
	return 1;
}

int
egg_buffer_get_uint32 (EggBuffer *buffer, size_t offset, size_t *next_offset,
                                 uint32_t *val)
{
	unsigned char *ptr;
	if (buffer->len < 4 || offset > buffer->len - 4) {
		buffer->failures++;
		return 0;
	}
	ptr = (unsigned char*)buffer->buf + offset;
	if (val != NULL)
		*val = egg_buffer_decode_uint32 (ptr);
	if (next_offset != NULL)
		*next_offset = offset + 4;
	return 1;
}

int
egg_buffer_add_uint64 (EggBuffer *buffer, uint64_t val)
{
	if (!egg_buffer_add_uint32 (buffer, ((val >> 32) & 0xffffffff)))
		return 0;
	return egg_buffer_add_uint32 (buffer, (val & 0xffffffff));
}

int
egg_buffer_get_uint64 (EggBuffer *buffer, size_t offset, 
					   size_t *next_offset, uint64_t *val)
{
	uint32_t a, b;
	if (!egg_buffer_get_uint32 (buffer, offset, &offset, &a))
		return 0;
	if (!egg_buffer_get_uint32 (buffer, offset, &offset, &b))
		return 0;
	if (val != NULL)
		*val = ((uint64_t)a) << 32 | b;
	if (next_offset != NULL)
		*next_offset = offset;
	return 1;
}

int
egg_buffer_add_byte_array (EggBuffer *buffer, const unsigned char *val,
                                     size_t len)
{
	if (val == NULL) 
		return egg_buffer_add_uint32 (buffer, 0xffffffff);
	if (len >= 0x7fffffff) {
		buffer->failures++;
		return 0; 
	}
	if (!egg_buffer_add_uint32 (buffer, len))
		return 0;
	return egg_buffer_append (buffer, val, len);
}

unsigned char*
egg_buffer_add_byte_array_empty (EggBuffer *buffer, size_t vlen)
{
	if (vlen >= 0x7fffffff) {
		buffer->failures++;
		return NULL; 
	}
	if (!egg_buffer_add_uint32 (buffer, vlen))
		return NULL;
	return egg_buffer_add_empty (buffer, vlen);
}

int
egg_buffer_get_byte_array (EggBuffer *buffer, size_t offset,
                           size_t *next_offset, const unsigned char **val,
                           size_t *vlen)
{
	uint32_t len;
	if (!egg_buffer_get_uint32 (buffer, offset, &offset, &len))
		return 0;
	if (len == 0xffffffff) {
		if (next_offset) 
			*next_offset = offset;
		if (val)
			*val = NULL;
		if (vlen)
			*vlen = 0;
		return 1;
	} else if (len >= 0x7fffffff) {
		buffer->failures++;
		return 0;
	}

	if (buffer->len < len || offset > buffer->len - len) {
		buffer->failures++;
		return 0;
	}
	
	if (val) 
		*val = buffer->buf + offset;
	if (vlen)
		*vlen = len;
	if (next_offset) 
		*next_offset = offset + len;

	return 1;
}

int
egg_buffer_add_string (EggBuffer *buffer, const char *str)
{
	if (str == NULL) {
		return egg_buffer_add_uint32 (buffer, 0xffffffff);
	} else {
		size_t len = strlen (str);
		if (len >= 0x7fffffff)
			return 0;
		if (!egg_buffer_add_uint32 (buffer, len))
			return 0;
		return egg_buffer_append (buffer, (unsigned char*)str, len);
	}
}

int
egg_buffer_get_string (EggBuffer *buffer, size_t offset, size_t *next_offset,
                       char **str_ret, EggBufferAllocator allocator)
{
	uint32_t len;
	
	if (!allocator)
		allocator = buffer->allocator;
	if (!allocator)
		allocator = DEFAULT_ALLOCATOR;
	
	if (!egg_buffer_get_uint32 (buffer, offset, &offset, &len)) {
		return 0;
	}
	if (len == 0xffffffff) {
		*next_offset = offset;
		*str_ret = NULL;
		return 1;
	} else if (len >= 0x7fffffff) {
		return 0;
	}
	
	if (buffer->len < len ||
	    offset > buffer->len - len) {
		return 0;
	}
	
	/* Make sure no null characters in string */
	if (memchr (buffer->buf + offset, 0, len) != NULL)
		return 0;
	
	/* The passed allocator may be for non-pageable memory */
	*str_ret = (allocator) (NULL, len + 1);
	if (!*str_ret)
		return 0;
	memcpy (*str_ret, buffer->buf + offset, len);

	/* Always zero terminate */
	(*str_ret)[len] = 0;
	*next_offset = offset + len;
	
	return 1;
}

int
egg_buffer_add_stringv (EggBuffer *buffer, const char** strv)
{
	const char **v;
	uint32_t n = 0;
	
	if (!strv)
		return 0;
	
	/* Add the number of strings coming */
	for (v = strv; *v; ++v)
		++n;
	if (!egg_buffer_add_uint32 (buffer, n))
		return 0;
	
	/* Add the individual strings */
	for (v = strv; *v; ++v) {
		if (!egg_buffer_add_string (buffer, *v))
			return 0;
	}
	
	return 1;
}

int
egg_buffer_get_stringv (EggBuffer *buffer, size_t offset, size_t *next_offset,
		                char ***strv_ret, EggBufferAllocator allocator)
{
	uint32_t n, i, j;
	size_t len;
	
	if (!allocator)
		allocator = buffer->allocator;
	if (!allocator)
		allocator = DEFAULT_ALLOCATOR;
	
	/* First the number of environment variable lines */
	if (!egg_buffer_get_uint32 (buffer, offset, &offset, &n))
		return 0;
	
	/* Then that number of strings */
	len = (n + 1) * sizeof (char*);
	*strv_ret = (char**)(allocator) (NULL, len);
	if (!*strv_ret)
		return 0;
	
	/* All null strings */
	memset (*strv_ret, 0, len);
	
	for (i = 0; i < n; ++i) {
		if (!egg_buffer_get_string (buffer, offset, &offset, 
		                            &((*strv_ret)[i]), allocator)) {
			
			/* Free all the strings on failure */
			for (j = 0; j < i; ++j) {
				if ((*strv_ret)[j])
					(allocator) ((*strv_ret)[j], 0);
			}
			
			return 0;
		}
	}
	
	if (next_offset != NULL)
		*next_offset = offset;
	
	return 1;
}
