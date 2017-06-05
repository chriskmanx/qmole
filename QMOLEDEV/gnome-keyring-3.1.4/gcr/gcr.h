/*
 * gnome-keyring
 *
 * Copyright (C) 2010 Collabora Ltd.
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

#ifndef GCR_API_SUBJECT_TO_CHANGE
#error "This API has not yet reached stability."
#endif

#ifndef __GCR_H__
#define __GCR_H__

#include <glib.h>

#define __GCR_H_INSIDE__

#include "gcr-types.h"

#include "gcr-certificate.h"
#include "gcr-certificate-chain.h"
#include "gcr-certificate-renderer.h"
#include "gcr-certificate-widget.h"
#include "gcr-collection-model.h"
#include "gcr-column.h"
#include "gcr-combo-selector.h"
#include "gcr-enum-types.h"
#include "gcr-key-renderer.h"
#include "gcr-key-widget.h"
#include "gcr-importer.h"
#include "gcr-library.h"
#include "gcr-list-selector.h"
#include "gcr-parser.h"
#include "gcr-renderer.h"
#include "gcr-pkcs11-certificate.h"
#include "gcr-simple-certificate.h"
#include "gcr-simple-collection.h"
#include "gcr-tree-selector.h"
#include "gcr-trust.h"
#include "gcr-unlock-options.h"
#include "gcr-unlock-options-widget.h"
#include "gcr-viewer.h"

#ifndef GCR_DISABLE_DEPRECATED

#include "gcr-certificate-basics-widget.h"
#include "gcr-certificate-details-widget.h"

#endif /* GCR_DISABLE_DEPRECATED */

#undef __GCR_H_INSIDE__

#endif /* __GCR_H__ */
