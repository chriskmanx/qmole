/* -*- Mode: C; indent-tabs-mode: t; c-basic-offset: 8; tab-width: 8 -*- */
/* egg-secure-buffer.h - secure memory gtkentry buffer

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

#ifndef __EGG_ENTRY_BUFFER_H__
#define __EGG_ENTRY_BUFFER_H__

#include <gtk/gtk.h>

G_BEGIN_DECLS

#define EGG_TYPE_ENTRY_BUFFER            (egg_entry_buffer_get_type ())
#define EGG_ENTRY_BUFFER(obj)            (G_TYPE_CHECK_INSTANCE_CAST ((obj), EGG_TYPE_ENTRY_BUFFER, EggEntryBuffer))
#define EGG_ENTRY_BUFFER_CLASS(klass)    (G_TYPE_CHECK_CLASS_CAST ((klass), EGG_TYPE_ENTRY_BUFFER, EggEntryBufferClass))
#define EGG_IS_ENTRY_BUFFER(obj)         (G_TYPE_CHECK_INSTANCE_TYPE ((obj), EGG_TYPE_ENTRY_BUFFER))
#define EGG_IS_ENTRY_BUFFER_CLASS(klass) (G_TYPE_CHECK_CLASS_TYPE ((klass), EGG_TYPE_ENTRY_BUFFER))
#define EGG_ENTRY_BUFFER_GET_CLASS(obj)  (G_TYPE_INSTANCE_GET_CLASS ((obj), EGG_TYPE_ENTRY_BUFFER, EggEntryBufferClass))

typedef struct _EggEntryBuffer            EggEntryBuffer;
typedef struct _EggEntryBufferClass       EggEntryBufferClass;
typedef struct _EggEntryBufferPrivate     EggEntryBufferPrivate;

struct _EggEntryBuffer
{
	GtkEntryBuffer parent;
	EggEntryBufferPrivate *priv;
};

struct _EggEntryBufferClass
{
	GtkEntryBufferClass parent_class;
};

GType                     egg_entry_buffer_get_type               (void) G_GNUC_CONST;

GtkEntryBuffer*           egg_entry_buffer_new                    (void);

G_END_DECLS

#endif /* __EGG_ENTRY_BUFFER_H__ */
