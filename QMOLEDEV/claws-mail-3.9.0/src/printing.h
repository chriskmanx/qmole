/* Claws Mail -- a GTK+ based, lightweight, and fast e-mail client
 * Copyright (C) 2007-2012 Holger Berndt <hb@claws-mail.org> 
 * and the Claws Mail team
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

#ifndef __PRINTING_H__
#define __PRINTING_H__

#include <gtk/gtk.h>

#ifdef HAVE_CONFIG_H
#include "claws-features.h"
#endif

typedef struct _PrintData		PrintData;

typedef struct _PrintRenderer {
	PangoContext *(*get_pango_context)(gpointer renderer_data);
	gpointer (*get_data_to_print)(gpointer renderer_data, gint sel_start, gint sel_end);
	void (*cb_begin_print)(GtkPrintOperation *op, GtkPrintContext *context,
				gpointer user_data);
	void (*cb_draw_page)(GtkPrintOperation* op, GtkPrintContext*, gint page_nr,
			     gpointer user_data);
} PrintRenderer;

void printing_print(GtkTextView*, GtkWindow*, gint, gint);
void printing_print_full(GtkWindow *parent, PrintRenderer *renderer, gpointer renderer_data, 
			 gint sel_start, gint sel_end);
void printing_page_setup(GtkWindow*);
gpointer printing_get_renderer_data(PrintData *print_data);
gdouble  printing_get_zoom(PrintData *print_data);
void     printing_set_n_pages(PrintData *print_data, gint n_pages);
GtkPrintSettings *printing_get_settings(void);
GtkPageSetup *printing_get_page_setup(void);
void printing_store_settings(GtkPrintSettings *new_settings);

#endif /* __PRINTING_H__ */
