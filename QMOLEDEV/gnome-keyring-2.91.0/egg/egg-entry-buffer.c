/* -*- Mode: C; indent-tabs-mode: t; c-basic-offset: 8; tab-width: 8 -*- */
/* egg-secure-buffer.c - secure memory gtkentry buffer

   Copyright (C) 2009 Stefan Walter

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

#include "egg-entry-buffer.h"
#include "egg-secure-memory.h"

#include <string.h>

/* Initial size of buffer, in bytes */
#define MIN_SIZE 16

struct _EggEntryBufferPrivate
{
	gchar *text;
	gsize text_size;
	gsize text_bytes;
	guint text_chars;
};

G_DEFINE_TYPE (EggEntryBuffer, egg_entry_buffer, GTK_TYPE_ENTRY_BUFFER);

/* --------------------------------------------------------------------------------
 * SECURE IMPLEMENTATIONS OF TEXT BUFFER
 */

static const gchar*
egg_entry_buffer_real_get_text (GtkEntryBuffer *buffer, gsize *n_bytes)
{
	EggEntryBuffer *self = EGG_ENTRY_BUFFER (buffer);
	if (n_bytes)
		*n_bytes = self->priv->text_bytes;
	if (!self->priv->text)
		return "";
	return self->priv->text;
}

static guint
egg_entry_buffer_real_get_length (GtkEntryBuffer *buffer)
{
	EggEntryBuffer *self = EGG_ENTRY_BUFFER (buffer);
	return self->priv->text_chars;
}

static guint
egg_entry_buffer_real_insert_text (GtkEntryBuffer *buffer, guint position,
                                    const gchar *chars, guint n_chars)
{
	EggEntryBuffer *self = EGG_ENTRY_BUFFER (buffer);
	EggEntryBufferPrivate *pv = self->priv;
	gsize n_bytes;
	gsize at;

	n_bytes = g_utf8_offset_to_pointer (chars, n_chars) - chars;

	/* Need more memory */
	if (n_bytes + pv->text_bytes + 1 > pv->text_size) {

		/* Calculate our new buffer size */
		while (n_bytes + pv->text_bytes + 1 > pv->text_size) {
			if (pv->text_size == 0) {
				pv->text_size = MIN_SIZE;
			} else {
				if (2 * pv->text_size < GTK_ENTRY_BUFFER_MAX_SIZE) {
					pv->text_size *= 2;
				} else {
					pv->text_size = GTK_ENTRY_BUFFER_MAX_SIZE;
					if (n_bytes > pv->text_size - pv->text_bytes - 1) {
						n_bytes = pv->text_size - pv->text_bytes - 1;
						n_bytes = g_utf8_find_prev_char (chars, chars + n_bytes + 1) - chars;
						n_chars = g_utf8_strlen (chars, n_bytes);
					}
					break;
				}
			}
		}

		pv->text = egg_secure_realloc (pv->text, pv->text_size);
	}

	/* Actual text insertion */
	at = g_utf8_offset_to_pointer (pv->text, position) - pv->text;
	g_memmove (pv->text + at + n_bytes, pv->text + at, pv->text_bytes - at);
	memcpy (pv->text + at, chars, n_bytes);

	/* Book keeping */
	pv->text_bytes += n_bytes;
	pv->text_chars += n_chars;
	pv->text[pv->text_bytes] = '\0';

	gtk_entry_buffer_emit_inserted_text (buffer, position, chars, n_chars);
	return n_chars;
}

static guint
egg_entry_buffer_real_delete_text (GtkEntryBuffer *buffer, guint position, guint n_chars)
{
	EggEntryBuffer *self = EGG_ENTRY_BUFFER (buffer);
	EggEntryBufferPrivate *pv = self->priv;
	gsize start, end;

	if (position > pv->text_chars)
		position = pv->text_chars;
	if (position + n_chars > pv->text_chars)
		n_chars = pv->text_chars - position;

	if (n_chars > 0) {
		start = g_utf8_offset_to_pointer (pv->text, position) - pv->text;
		end = g_utf8_offset_to_pointer (pv->text, position + n_chars) - pv->text;

		g_memmove (pv->text + start, pv->text + end, pv->text_bytes + 1 - end);
		pv->text_chars -= n_chars;
		pv->text_bytes -= (end - start);

		gtk_entry_buffer_emit_deleted_text (buffer, position, n_chars);
	}

	return n_chars;
}

/* --------------------------------------------------------------------------------
 *
 */

static void
egg_entry_buffer_init (EggEntryBuffer *self)
{
	EggEntryBufferPrivate *pv;
	pv = self->priv = G_TYPE_INSTANCE_GET_PRIVATE (self, EGG_TYPE_ENTRY_BUFFER, EggEntryBufferPrivate);

	pv->text = NULL;
	pv->text_chars = 0;
	pv->text_bytes = 0;
	pv->text_size = 0;
}

static void
egg_entry_buffer_finalize (GObject *obj)
{
	EggEntryBuffer *self = EGG_ENTRY_BUFFER (obj);
	EggEntryBufferPrivate *pv = self->priv;

	if (pv->text) {
		egg_secure_strfree (pv->text);
		pv->text = NULL;
		pv->text_bytes = pv->text_size = 0;
		pv->text_chars = 0;
	}

	G_OBJECT_CLASS (egg_entry_buffer_parent_class)->finalize (obj);
}

static void
egg_entry_buffer_class_init (EggEntryBufferClass *klass)
{
	GObjectClass *gobject_class = G_OBJECT_CLASS (klass);
	GtkEntryBufferClass *buffer_class = GTK_ENTRY_BUFFER_CLASS (klass);

	gobject_class->finalize = egg_entry_buffer_finalize;

	buffer_class->get_text = egg_entry_buffer_real_get_text;
	buffer_class->get_length = egg_entry_buffer_real_get_length;
	buffer_class->insert_text = egg_entry_buffer_real_insert_text;
	buffer_class->delete_text = egg_entry_buffer_real_delete_text;

	g_type_class_add_private (gobject_class, sizeof (EggEntryBufferPrivate));
}

/* --------------------------------------------------------------------------------
 *
 */

GtkEntryBuffer*
egg_entry_buffer_new (void)
{
	return g_object_new (EGG_TYPE_ENTRY_BUFFER, NULL);
}
