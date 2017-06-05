/* -*- Mode: C; indent-tabs-mode: t; c-basic-offset: 8; tab-width: 8 -*- */
/* gnome-keyring-memory.c - library for allocating memory that is non-pageable

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

#include "gnome-keyring-memory.h"
#include "gnome-keyring-private.h"

#include "egg/egg-secure-memory.h"

#include <glib.h>

#include <string.h>

/**
 * SECTION:gnome-keyring-memory
 * @title: Non-pageable Memory
 * @short_description: Secure Non-pageable Memory
 *
 * Normal allocated memory can be paged to disk at the whim of the operating system.
 * This can be a serious problem for sensitive information like passwords, keys and secrets.
 *
 * GNOME Keyring holds passwords in non-pageable, or locked memory. This happens
 * both in the daemon and in the library. This is only possible if the OS contains
 * support for it.
 *
 * These functions allow applications to use to hold passwords and other
 * sensitive information.
 */

static GStaticMutex memory_mutex = G_STATIC_MUTEX_INIT;

#define WARNING  "couldn't allocate secure memory to keep passwords " \
                 "and or keys from being written to the disk"

#define ABORTMSG "The GNOME_KEYRING_PARANOID environment variable was set. " \
                 "Exiting..."


/*
 * These are called from gkr-secure-memory.c to provide appropriate
 * locking for memory between threads
 */

void
egg_memory_lock (void)
{
	g_static_mutex_lock (&memory_mutex);
}

void
egg_memory_unlock (void)
{
	g_static_mutex_unlock (&memory_mutex);
}

void*
egg_memory_fallback (void *p, size_t sz)
{
	const gchar *env;

	/* We were asked to free memory */
	if (!sz) {
		g_free (p);
		return NULL;
	}

	/* We were asked to allocate */
	if (!p) {
		env = g_getenv ("GNOME_KEYRING_PARANOID");
		if (env && *env) {
			g_message (WARNING);
			g_error (ABORTMSG);
		}

		return g_malloc0 (sz);
	}

	/*
	 * Reallocation is a bit of a gray area, as we can be asked
	 * by external libraries (like libgcrypt) to reallocate a
	 * non-secure block into secure memory. We cannot satisfy
	 * this request (as we don't know the size of the original
	 * block) so we just try our best here.
	 */

	return g_realloc (p, sz);
}

/* -----------------------------------------------------------------------------
 * PUBLIC FUNCTIONS
 */

/**
 * gnome_keyring_memory_alloc:
 * @sz: The new desired size of the memory block.
 *
 * Allocate a block of gnome-keyring non-pageable memory.
 *
 * If non-pageable memory cannot be allocated then normal memory will be
 * returned.
 *
 * Return value:  The new memory block which should be freed with
 * gnome_keyring_memory_free()
 **/
gpointer
gnome_keyring_memory_alloc (gulong sz)
{
	gpointer p;

	/* Try to allocate secure memory */
	p = egg_secure_alloc_full (sz, GKR_SECURE_USE_FALLBACK);

	/* Our fallback will always allocate */
	g_assert (p);

	return p;
}

/**
 * gnome_keyring_memory_try_alloc:
 * @sz: The new desired size of the memory block.
 *
 * Allocate a block of gnome-keyring non-pageable memory.
 *
 * If non-pageable memory cannot be allocated, then NULL is returned.
 *
 * Return value: The new block, or NULL if memory cannot be allocated.
 * The memory block should be freed with gnome_keyring_memory_free()
 */
gpointer
gnome_keyring_memory_try_alloc (gulong sz)
{
	return egg_secure_alloc_full (sz, 0);
}

/**
 * gnome_keyring_memory_realloc:
 * @p: The pointer to reallocate or NULL to allocate a new block.
 * @sz: The new desired size of the memory block, or 0 to free the memory.
 *
 * Reallocate a block of gnome-keyring non-pageable memory.
 *
 * Glib memory is also reallocated correctly. If called with a null pointer,
 * then a new block of memory is allocated. If called with a zero size,
 * then the block of memory is freed.
 *
 * If non-pageable memory cannot be allocated then normal memory will be
 * returned.
 *
 * Return value: The new block, or NULL if the block was freed.
 * The memory block should be freed with gnome_keyring_memory_free()
 */
gpointer
gnome_keyring_memory_realloc (gpointer p, gulong sz)
{
	gpointer n;

	if (!p) {
		return gnome_keyring_memory_alloc (sz);
	} else if (!sz) {
		 gnome_keyring_memory_free (p);
		 return NULL;
	} else if (!egg_secure_check (p)) {
		return g_realloc (p, sz);
	}

	/* First try and ask secure memory to reallocate */
	n = egg_secure_realloc_full (p, sz, GKR_SECURE_USE_FALLBACK);

	g_assert (n);

	return n;
}

/**
 * gnome_keyring_memory_try_realloc:
 * @p: The pointer to reallocate or NULL to allocate a new block.
 * @sz: The new desired size of the memory block.
 *
 * Reallocate a block of gnome-keyring non-pageable memory.
 *
 * Glib memory is also reallocated correctly when passed to this function.
 * If called with a null pointer, then a new block of memory is allocated.
 * If called with a zero size, then the block of memory is freed.
 *
 * If memory cannot be allocated, NULL is returned and the original block
 * of memory remains intact.
 *
 * Return value: The new block, or NULL if memory cannot be allocated.
 * The memory block should be freed with gnome_keyring_memory_free()
 */
gpointer
gnome_keyring_memory_try_realloc (gpointer p, gulong sz)
{
	gpointer n;

	if (!p) {
		return gnome_keyring_memory_try_alloc (sz);
	} else if (!sz) {
		 gnome_keyring_memory_free (p);
		 return NULL;
	} else if (!egg_secure_check (p)) {
		return g_try_realloc (p, sz);
	}

	/* First try and ask secure memory to reallocate */
	n = egg_secure_realloc_full (p, sz, 0);

	g_assert (n);

	return n;
}

/**
 * gnome_keyring_memory_free:
 * @p: The pointer to the beginning of the block of memory to free.
 *
 * Free a block of gnome-keyring non-pageable memory.
 *
 * Glib memory is also freed correctly when passed to this function. If called
 * with a null pointer then no action is taken.
 */
void
gnome_keyring_memory_free (gpointer p)
{
	if (!p)
		return;
	egg_secure_free_full (p, GKR_SECURE_USE_FALLBACK);
}

/**
 * gnome_keyring_memory_is_secure:
 * @p: The pointer to check
 *
 * Check if a pointer is in non-pageable memory allocated by gnome-keyring.
 *
 * Return value: Whether the memory is non-pageable or not
 */
gboolean
gnome_keyring_memory_is_secure (gpointer p)
{
	return egg_secure_check (p) ? TRUE : FALSE;
}

/**
 * gnome_keyring_memory_strdup:
 * @str: The null terminated string to copy
 *
 * Copy a string into non-pageable memory. If the input string is %NULL, then
 * %NULL will be returned.
 *
 * Return value: The copied string, should be freed with gnome_keyring_memory_free()
 */
gchar*
gnome_keyring_memory_strdup (const gchar* str)
{
	return egg_secure_strdup (str);
}
