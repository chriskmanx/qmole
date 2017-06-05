/* gnome-url.h
 * Copyright (C) 1998 James Henstridge <james@daa.com.au>
 * Copyright (C) 1999, 2000 Red Hat, Inc.
 * All rights reserved.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Library General Public License for more details.
 *
 * You should have received a copy of the GNU Library General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc.,  59 Temple Place - Suite 330, Cambridge, MA 02139, USA.
 */
/*
  @NOTATION@
 */

#ifndef GNOME_URL_H
#define GNOME_URL_H

#include <glib/gmacros.h>
#include <glib/gerror.h>

G_BEGIN_DECLS

/**
 * GnomeURLError:
 * @GNOME_URL_ERROR_PARSE: The parsing of the handler failed.
 *
 * The errors that can be returned due to bad parameters being pass to
 * gnome_url_show().
 */
typedef enum {
  GNOME_URL_ERROR_PARSE
} GnomeURLError;

#define GNOME_URL_ERROR (gnome_url_error_quark ())
GQuark gnome_url_error_quark (void) G_GNUC_CONST;

/* This function displays the given URL in the appropriate viewer.  The
 * Appropriate viewer is user definable, according to these rules:
 *  1) Extract the protocol from URL.  This is defined as everything before
 *     the first colon
 *  2) Check if the key /desktop/gnome/url-handlers/<protocol>-show exists in the
 *     gnome config database.  If it does, use use this as a command
 *     template.  If it doesn't, check for the key
 *     /desktop/gnome/url-handlers/default-show, and if that doesn't exist fallback
 *     on the compiled in default.
 *  3) substitute the %s in the template with the URL.
 *  4) call gnome_execute_shell, with this expanded command as the second
 *     argument.
 */

/* returns FALSE on error, TRUE if everything went fine */
/* Errors returned are either the GNOME_URL_ERROR_ ones or G_SPAWN_ERROR_ ones */
gboolean gnome_url_show(const char *url, GError **error);

G_END_DECLS
#endif
