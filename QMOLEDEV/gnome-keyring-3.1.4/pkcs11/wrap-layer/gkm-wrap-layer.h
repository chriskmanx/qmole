/*
 * gnome-keyring
 *
 * Copyright (C) 2010 Stefan Walter
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

#ifndef __GKM_WRAP_LAYER_H__
#define __GKM_WRAP_LAYER_H__

#include "pkcs11/pkcs11.h"

#include <glib.h>

CK_FUNCTION_LIST_PTR    gkm_wrap_layer_get_functions               (void);

CK_FUNCTION_LIST_PTR    gkm_wrap_layer_get_functions_no_prompts    (void);

void                    gkm_wrap_layer_reset_modules               (void);

void                    gkm_wrap_layer_add_module                  (CK_FUNCTION_LIST_PTR funcs);

void                    gkm_wrap_layer_mark_login_unlock_success   (void);

void                    gkm_wrap_layer_mark_login_unlock_failure   (const gchar *failed_password);

#endif /* __GKM_WRAP_LAYER_H__ */
