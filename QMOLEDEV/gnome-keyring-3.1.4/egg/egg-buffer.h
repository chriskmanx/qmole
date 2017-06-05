/* -*- Mode: C; indent-tabs-mode: t; c-basic-offset: 8; tab-width: 8 -*- */
/* egg-buffer.h - Generic data buffer, used by openssh, gnome-keyring

   Copyright (C) 2007, Stefan Walter

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

#ifndef EGG_BUFFER_H
#define EGG_BUFFER_H

#include <stdlib.h>
#include <stdint.h>

/* -------------------------------------------------------------------
 * EggBuffer 
 * 
 * IMPORTANT: This is pure vanila standard C, no glib. We need this 
 * because certain consumers of this protocol need to be built 
 * without linking in any special libraries. ie: the PKCS#11 module.
 * 
 * Memory Allocation
 * 
 * Callers can set their own allocator. If NULL is used then standard 
 * C library heap memory is used and failures will not be fatal. Memory 
 * failures will instead result in a zero return value or 
 * egg_buffer_has_error() returning one.
 * 
 * If you use something like g_realloc as the allocator, then memory 
 * failures become fatal just like in a standard GTK program.
 * 
 * Don't change the allocator manually in the EggBuffer structure. The 
 * egg_buffer_set_allocator() func will reallocate and handle things 
 * properly.
 * 
 * Pointers into the Buffer
 * 
 * Any write operation has the posibility of reallocating memory
 * and invalidating any direct pointers into the buffer.
 */
 
/* The allocator for the EggBuffer. This follows the realloc() syntax and logic */
typedef void* (*EggBufferAllocator) (void* p, size_t len);

typedef struct _EggBuffer {
	unsigned char *buf;
	size_t len;
	size_t allocated_len;
	int failures; 
	EggBufferAllocator allocator;
} EggBuffer;

#define 	EGG_BUFFER_EMPTY		{ NULL, 0, 0, 0, NULL }

int             egg_buffer_init                 (EggBuffer *buffer, size_t reserve);

int             egg_buffer_init_full            (EggBuffer *buffer, 
                                                 size_t reserve, 
                                                 EggBufferAllocator allocator);

void            egg_buffer_init_static          (EggBuffer *buffer,
                                                 const unsigned char *buf,
                                                 size_t len);

void            egg_buffer_init_allocated       (EggBuffer *buffer,
                                                 unsigned char *buf,
                                                 size_t len,
                                                 EggBufferAllocator allocator);
                                                 
void            egg_buffer_uninit               (EggBuffer *buffer);

unsigned char*  egg_buffer_uninit_steal         (EggBuffer *buffer,
                                                 size_t *n_result);

int             egg_buffer_set_allocator        (EggBuffer *buffer,
                                                 EggBufferAllocator allocator);

void 		egg_buffer_reset		(EggBuffer *buffer);

int		egg_buffer_equal		(EggBuffer *b1,
						 EggBuffer *b2);

int             egg_buffer_reserve              (EggBuffer *buffer,
                                                 size_t len);
						 
int             egg_buffer_resize               (EggBuffer *buffer,
                                                 size_t len);

int		egg_buffer_append 		(EggBuffer *buffer,
						 const unsigned char *val,
						 size_t len);

unsigned char*  egg_buffer_add_empty            (EggBuffer *buffer,
                                                 size_t len);

int 		egg_buffer_add_byte		(EggBuffer *buffer,
						 unsigned char val);

int 		egg_buffer_get_byte		(EggBuffer *buffer,
						 size_t offset,
						 size_t *next_offset,
						 unsigned char *val);
									 
void 		egg_buffer_encode_uint32	(unsigned char* buf, 
						 uint32_t val);

uint32_t	egg_buffer_decode_uint32	(unsigned char* buf);

int 		egg_buffer_add_uint32		(EggBuffer *buffer,
						 uint32_t val);

int		egg_buffer_set_uint32		(EggBuffer *buffer,
						 size_t offset, 
						 uint32_t val);

int		egg_buffer_get_uint32		(EggBuffer *buffer,
						 size_t offset,
						 size_t *next_offset,
						 uint32_t *val);

void 		egg_buffer_encode_uint16	(unsigned char* buf, 
						 uint16_t val);

uint16_t	egg_buffer_decode_uint16	(unsigned char* buf);

int 		egg_buffer_add_uint16		(EggBuffer *buffer,
						 uint16_t val);

int		egg_buffer_set_uint16		(EggBuffer *buffer,
						 size_t offset, 
						 uint16_t val);

int		egg_buffer_get_uint16		(EggBuffer *buffer,
						 size_t offset,
						 size_t *next_offset,
						 uint16_t *val);

int		egg_buffer_add_byte_array	(EggBuffer *buffer,
						 const unsigned char *val,
						 size_t len);

int		egg_buffer_get_byte_array	(EggBuffer *buffer,
						 size_t offset,
						 size_t *next_offset,
						 const unsigned char **val,
						 size_t *vlen);

unsigned char*  egg_buffer_add_byte_array_empty (EggBuffer *buffer,
                                                 size_t vlen);						 

int             egg_buffer_add_string           (EggBuffer *buffer, 
                                                 const char *str);
                                                 
int             egg_buffer_get_string           (EggBuffer *buffer, 
                                                 size_t offset, 
                                                 size_t *next_offset, 
                                                 char **str_ret, 
                                                 EggBufferAllocator allocator);

int             egg_buffer_add_stringv          (EggBuffer *buffer, 
                                                 const char** strv);

int             egg_buffer_get_stringv          (EggBuffer *buffer,
                                                 size_t offset,
                                                 size_t *next_offset,
                                                 char ***strv_ret, 
                                                 EggBufferAllocator allocator);

int		egg_buffer_add_uint64		(EggBuffer *buffer,
						 uint64_t val);

int		egg_buffer_get_uint64		(EggBuffer *buffer,
						 size_t offset,
						 size_t *next_offset,
						 uint64_t *val);

#define		egg_buffer_length(b)		((b)->len)

#define 	egg_buffer_has_error(b)		((b)->failures > 0)

#endif /* EGG_BUFFER_H */

