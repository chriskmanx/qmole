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

#ifndef __GKM_TRANSACTION_H__
#define __GKM_TRANSACTION_H__

#include <glib-object.h>

#include "gkm-types.h"

#include "pkcs11/pkcs11.h"

#define GKM_TYPE_TRANSACTION               (gkm_transaction_get_type ())
#define GKM_TRANSACTION(obj)               (G_TYPE_CHECK_INSTANCE_CAST ((obj), GKM_TYPE_TRANSACTION, GkmTransaction))
#define GKM_TRANSACTION_CLASS(klass)       (G_TYPE_CHECK_CLASS_CAST ((klass), GKM_TYPE_TRANSACTION, GkmTransactionClass))
#define GKM_IS_TRANSACTION(obj)            (G_TYPE_CHECK_INSTANCE_TYPE ((obj), GKM_TYPE_TRANSACTION))
#define GKM_IS_TRANSACTION_CLASS(klass)    (G_TYPE_CHECK_CLASS_TYPE ((klass), GKM_TYPE_TRANSACTION))
#define GKM_TRANSACTION_GET_CLASS(obj)     (G_TYPE_INSTANCE_GET_CLASS ((obj), GKM_TYPE_TRANSACTION, GkmTransactionClass))

typedef struct _GkmTransactionClass GkmTransactionClass;

struct _GkmTransactionClass {
	GObjectClass parent_class;

	/* signals --------------------------------------------------------- */

	gboolean (*complete) (GkmTransaction *transaction);
};

GType                       gkm_transaction_get_type               (void);

GkmTransaction*             gkm_transaction_new                    (void);

typedef gboolean            (*GkmTransactionFunc)                  (GkmTransaction *self,
                                                                    GObject *object,
                                                                    gpointer user_data);

void                        gkm_transaction_add                    (GkmTransaction *self,
                                                                    gpointer object,
                                                                    GkmTransactionFunc callback,
                                                                    gpointer user_data);

void                        gkm_transaction_fail                   (GkmTransaction *self,
                                                                    CK_RV result);

void                        gkm_transaction_complete               (GkmTransaction *self);

gboolean                    gkm_transaction_get_failed             (GkmTransaction *self);

CK_RV                       gkm_transaction_get_result             (GkmTransaction *self);

gboolean                    gkm_transaction_get_completed          (GkmTransaction *self);

gchar*                      gkm_transaction_unique_file            (GkmTransaction *self,
                                                                    const gchar *directory,
                                                                    const gchar *basename);

void                        gkm_transaction_write_file             (GkmTransaction *self,
                                                                    const gchar *filename,
                                                                    gconstpointer data,
                                                                    gsize n_data);

void                        gkm_transaction_remove_file            (GkmTransaction *self,
                                                                    const gchar *filename);

CK_RV                       gkm_transaction_complete_and_unref     (GkmTransaction *self);

#endif /* __GKM_TRANSACTION_H__ */
