/*
 * gnome-keyring
 *
 * Copyright (C) 2008 Stefan Walter
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General  License as
 * published by the Free Software Foundation; either version 2.1 of
 * the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General  License for more details.
 *
 * You should have received a copy of the GNU Lesser General
 * License along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
 * 02111-1307, USA.
 */

#ifndef EGG_PADDING_H_
#define EGG_PADDING_H_

#include <glib.h>

#ifndef HAVE_EGG_ALLOCATOR
typedef void* (*EggAllocator) (void* p, gsize);
#define HAVE_EGG_ALLOCATOR
#endif

typedef gboolean         (*EggPadding)                                 (EggAllocator alloc,
                                                                        gsize n_block,
                                                                        gconstpointer input,
                                                                        gsize n_input,
                                                                        gpointer *output,
                                                                        gsize *n_output);

gboolean                 egg_padding_zero_pad                          (EggAllocator alloc,
                                                                        gsize n_block,
                                                                        gconstpointer raw,
                                                                        gsize n_raw,
                                                                        gpointer *padded,
                                                                        gsize *n_padded);

gboolean                 egg_padding_pkcs1_pad_01                      (EggAllocator alloc,
                                                                        gsize n_block,
                                                                        gconstpointer raw,
                                                                        gsize n_raw,
                                                                        gpointer *padded,
                                                                        gsize *n_padded);

gboolean                 egg_padding_pkcs1_pad_02                      (EggAllocator alloc,
                                                                        gsize n_block,
                                                                        gconstpointer raw,
                                                                        gsize n_raw,
                                                                        gpointer *padded,
                                                                        gsize *n_padded);

gboolean                 egg_padding_pkcs1_unpad_01                    (EggAllocator alloc,
                                                                        gsize n_block,
                                                                        gconstpointer padded,
                                                                        gsize n_padded,
                                                                        gpointer *raw,
                                                                        gsize *n_raw);

gboolean                 egg_padding_pkcs1_unpad_02                    (EggAllocator alloc,
                                                                        gsize n_block,
                                                                        gconstpointer padded,
                                                                        gsize n_padded,
                                                                        gpointer *raw,
                                                                        gsize *n_raw);

gboolean                 egg_padding_pkcs7_pad                         (EggAllocator alloc,
                                                                        gsize n_block,
                                                                        gconstpointer raw,
                                                                        gsize n_raw,
                                                                        gpointer *padded,
                                                                        gsize *n_padded);

gboolean                 egg_padding_pkcs7_unpad                       (EggAllocator alloc,
                                                                        gsize n_block,
                                                                        gconstpointer raw,
                                                                        gsize n_raw,
                                                                        gpointer *padded,
                                                                        gsize *n_padded);

#endif /* EGG_PADDING_H_ */
