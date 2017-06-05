/*
 * gnome-keyring
 *
 * Copyright (C) 2009 Stefan Walter
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

#ifndef __GKM_SECRET_BINARY_H__
#define __GKM_SECRET_BINARY_H__

#include "gkm-secret-collection.h"

#include "gkm/gkm-data-types.h"
#include "gkm/gkm-types.h"

GkmDataResult          gkm_secret_binary_read        (GkmSecretCollection *collection,
                                                      GkmSecretData *sdata,
                                                      gconstpointer data,
                                                      gsize n_data);

GkmDataResult          gkm_secret_binary_write       (GkmSecretCollection *collection,
                                                      GkmSecretData *sdata,
                                                      gpointer *data,
                                                      gsize *n_data);

#endif /* __GKM_SECRET_BINARY_H__ */
