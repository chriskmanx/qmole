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

#ifndef GKR_DEBUG_H
#define GKR_DEBUG_H

#include "config.h"

#include <glib.h>

G_BEGIN_DECLS

/* Please keep this enum in sync with #keys in gkr-debug.c */
typedef enum {
	GKR_DEBUG_OPERATION = 1 << 1,
} GkrDebugFlags;

gboolean           gkr_debug_flag_is_set               (GkrDebugFlags flag);

void               gkr_debug_set_flags                 (const gchar *flags_string);

void               gkr_debug_message                   (GkrDebugFlags flag,
                                                        const gchar *format,
                                                        ...) G_GNUC_PRINTF (2, 3);

G_END_DECLS

#endif /* GKR_DEBUG_H */

/* -----------------------------------------------------------------------------
 * Below this point is outside the GKR_DEBUG_H guard - so it can take effect
 * more than once. So you can do:
 *
 * #define DEBUG_FLAG GKR_DEBUG_ONE_THING
 * #include "gkr-debug.h"
 * ...
 * DEBUG ("if we're debugging one thing");
 * ...
 * #undef DEBUG_FLAG
 * #define DEBUG_FLAG GKR_DEBUG_OTHER_THING
 * #include "gkr-debug.h"
 * ...
 * DEBUG ("if we're debugging the other thing");
 * ...
 */

#ifdef DEBUG_FLAG
#ifdef WITH_DEBUG

#undef gkr_debug
#define gkr_debug(format, ...) \
	gkr_debug_message (DEBUG_FLAG, "%s: " format, G_STRFUNC, ##__VA_ARGS__)

#undef gkr_debugging
#define gkr_debugging \
	gkr_debug_flag_is_set (DEBUG_FLAG)

#else /* !defined (WITH_DEBUG) */

#undef gkr_debug
#define gkr_debug(format, ...) \
	do {} while (0)

#undef gkr_debugging
#define gkr_debugging 0

#endif /* !defined (WITH_DEBUG) */

#endif /* defined (DEBUG_FLAG) */
