/* select-keys.h - GTK+ based key selection
 *      Copyright (C) 2001-2012 Werner Koch (dd9jn) and the Claws Mail team
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 * 
 */

#ifndef GPGMEGTK_SELECT_KEYS_H
#define GPGMEGTK_SELECT_KEYS_H

#include <glib.h>
#include <gpgme.h>

typedef enum {
	KEY_SELECTION_OK,
	KEY_SELECTION_CANCEL,
	KEY_SELECTION_DONT
} SelectionResult;
	
gpgme_key_t *gpgmegtk_recipient_selection (GSList *recp_names, 
		SelectionResult *result,
		gpgme_protocol_t proto);


#endif /* GPGMEGTK_SELECT_KEYS_H */
