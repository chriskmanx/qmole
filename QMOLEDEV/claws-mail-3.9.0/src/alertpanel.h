/*
 * Sylpheed -- a GTK+ based, lightweight, and fast e-mail client
 * Copyright (C) 1999-2012 Hiroyuki Yamamoto and the Claws Mail team
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

#ifndef __ALERTPANEL_H__
#define __ALERTPANEL_H__

#include <glib.h>

typedef enum
{
	G_ALERTDEFAULT,
	G_ALERTALTERNATE,
	G_ALERTOTHER,
	G_ALERTCANCEL,
	G_ALERTWAIT,

	G_ALERTDISABLE	= 1 << 16
} AlertValue;

typedef enum
{
	ALERT_NOTICE,
	ALERT_QUESTION,
	ALERT_WARNING,
	ALERT_ERROR
} AlertType;
#define G_ALERT_VALUE_MASK	0x0000ffff

#define ALERTPANEL_OPENED_HOOKLIST "alertpanel_opened_hooklist"

AlertValue alertpanel_full(const gchar *title, const gchar *message,
			   const gchar *button1_label,
			   const gchar *button2_label,
			   const gchar *button3_label,
			   gboolean     can_disable,
			   GtkWidget   *widget,
			   AlertType    alert_type,
			   AlertValue   default_value);

AlertValue alertpanel	(const gchar	*title,
			 const gchar	*message,
			 const gchar	*button1_label,
			 const gchar	*button2_label,
			 const gchar	*button3_label);

AlertValue alertpanel_with_widget	(const gchar *title,
				  	 const gchar *message,
				  	 const gchar *button1_label,
				  	 const gchar *button2_label,
				  	 const gchar *button3_label,
					 gboolean     can_disable,
				 	 AlertValue   default_value,
					 GtkWidget   *widget);

void alertpanel_notice	(const gchar	*format,
			 ...) G_GNUC_PRINTF(1, 2);
void alertpanel_warning	(const gchar	*format,
			 ...) G_GNUC_PRINTF(1, 2);
void alertpanel_error	(const gchar	*format,
			 ...) G_GNUC_PRINTF(1, 2);
void alertpanel_error_log(const gchar	*format,
			  ...) G_GNUC_PRINTF(1, 2);

#endif /* __ALERTPANEL_H__ */
