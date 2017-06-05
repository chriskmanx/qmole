/*
 * Copyright (C) 2007 Nokia Corporation
 * Copyright (C) 2007-2011 Collabora Ltd.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
 */

#ifndef GCR_DEBUG_H
#define GCR_DEBUG_H

#include "config.h"

#include <glib.h>

G_BEGIN_DECLS

/* Please keep this enum in sync with #keys in gcr-debug.c */
typedef enum {
	GCR_DEBUG_LIBRARY = 1 << 1,
	GCR_DEBUG_CERTIFICATE_CHAIN = 1 << 2,
	GCR_DEBUG_PARSE = 1 << 3,
	GCR_DEBUG_GNUPG = 1 << 4
} GcrDebugFlags;

gboolean           _gcr_debug_flag_is_set              (GcrDebugFlags flag);

void               _gcr_debug_set_flags                (const gchar *flags_string);

void               _gcr_debug_message                  (GcrDebugFlags flag,
                                                        const gchar *format,
                                                        ...) G_GNUC_PRINTF (2, 3);

G_END_DECLS

#endif /* GCR_DEBUG_H */

/* -----------------------------------------------------------------------------
 * Below this point is outside the GCR_DEBUG_H guard - so it can take effect
 * more than once. So you can do:
 *
 * #define DEBUG_FLAG GCR_DEBUG_ONE_THING
 * #include "gcr-debug.h"
 * ...
 * DEBUG ("if we're debugging one thing");
 * ...
 * #undef DEBUG_FLAG
 * #define DEBUG_FLAG GCR_DEBUG_OTHER_THING
 * #include "gcr-debug.h"
 * ...
 * DEBUG ("if we're debugging the other thing");
 * ...
 */

#ifdef DEBUG_FLAG
#ifdef WITH_DEBUG

#undef _gcr_debug
#define _gcr_debug(format, ...) \
	_gcr_debug_message (DEBUG_FLAG, "%s: " format, G_STRFUNC, ##__VA_ARGS__)

#undef _gcr_debugging
#define _gcr_debugging \
	_gcr_debug_flag_is_set (DEBUG_FLAG)

#else /* !defined (WITH_DEBUG) */

#undef _gcr_debug
#define _gcr_debug(format, ...) \
	do {} while (0)

#undef _gcr_debugging
#define _gcr_debugging 0

#endif /* !defined (WITH_DEBUG) */

#endif /* defined (DEBUG_FLAG) */
