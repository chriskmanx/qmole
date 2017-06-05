/*
 * gnome-keyring
 *
 * Copyright (C) 2008 Stefan Walter
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as
 * published by the Free Software Foundation; either version 2.1 of
 * the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
 * 02111-1307, USA.
 */

#ifndef __GKM_AES_KEY_H__
#define __GKM_AES_KEY_H__

#include <glib-object.h>

#include "gkm-secret-key.h"
#include "gkm-types.h"

#include <gcrypt.h>

#define GKM_FACTORY_AES_KEY            (gkm_aes_key_get_factory ())

#define GKM_TYPE_AES_KEY               (gkm_aes_key_get_type ())
#define GKM_AES_KEY(obj)               (G_TYPE_CHECK_INSTANCE_CAST ((obj), GKM_TYPE_AES_KEY, GkmAesKey))
#define GKM_AES_KEY_CLASS(klass)       (G_TYPE_CHECK_CLASS_CAST ((klass), GKM_TYPE_AES_KEY, GkmAesKeyClass))
#define GKM_IS_AES_KEY(obj)            (G_TYPE_CHECK_INSTANCE_TYPE ((obj), GKM_TYPE_AES_KEY))
#define GKM_IS_AES_KEY_CLASS(klass)    (G_TYPE_CHECK_CLASS_TYPE ((klass), GKM_TYPE_AES_KEY))
#define GKM_AES_KEY_GET_CLASS(obj)     (G_TYPE_INSTANCE_GET_CLASS ((obj), GKM_TYPE_AES_KEY, GkmAesKeyClass))

typedef struct _GkmAesKeyClass GkmAesKeyClass;
typedef struct _GkmAesKeyPrivate GkmAesKeyPrivate;

struct _GkmAesKeyClass {
	GkmSecretKeyClass parent_class;
};

GType                     gkm_aes_key_get_type           (void);

GkmFactory*               gkm_aes_key_get_factory        (void);

gsize                     gkm_aes_key_get_block_size     (GkmAesKey *self);

gcry_cipher_hd_t          gkm_aes_key_get_cipher         (GkmAesKey *self,
                                                          int mode);

#endif /* __GKM_AES_KEY_H__ */
