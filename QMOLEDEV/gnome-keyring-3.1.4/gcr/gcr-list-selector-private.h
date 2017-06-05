/*
 * gnome-keyring
 *
 * Copyright (C) 2011 Collabora Ltd.
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
 *
 * Author: Stef Walter <stefw@collabora.co.uk>
 */

#ifndef __GCR_LIST_SELECTOR_PRIVATE_H__
#define __GCR_LIST_SELECTOR_PRIVATE_H__

#include "gcr-types.h"
#include "gcr-list-selector.h"
#include "gcr-live-search.h"

#include <gtk/gtk.h>

G_BEGIN_DECLS

void              _gcr_list_selector_set_live_search           (GcrListSelector *self,
                                                                GcrLiveSearch *search);

G_END_DECLS

#endif /* __GCR_LIST_SELECTOR_PRIVATE_H__ */
