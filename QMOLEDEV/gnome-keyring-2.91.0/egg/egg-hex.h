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

#ifndef EGG_HEX_H_
#define EGG_HEX_H_

#include <glib.h>

gpointer              egg_hex_decode                         (const gchar *data,
                                                              gssize n_data, 
                                                              gsize *n_decoded);

gpointer              egg_hex_decode_full                    (const gchar *data,
                                                              gssize n_data,
                                                              gchar delim,
                                                              guint group,
                                                              gsize *n_decoded);

gchar*                egg_hex_encode                         (gconstpointer data,
                                                              gsize n_data);

gchar*                egg_hex_encode_full                    (gconstpointer data,
                                                              gsize n_data,
                                                              gboolean upper_case,
                                                              gchar delim,
                                                              guint group);

#endif /* EGG_HEX_H_ */
