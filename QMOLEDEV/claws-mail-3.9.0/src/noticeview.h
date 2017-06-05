/* 
 * Sylpheed -- a GTK+ based, lightweight, and fast e-mail client
 * Copyright (C) 2002-2012 Hiroyuki Yamamoto & The Claws Mail Team
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

#ifndef NOTICEVIEW_H__
#define NOTICEVIEW_H__

#include "viewtypes.h"
#include "stock_pixmap.h"

struct _NoticeView 
{
	GtkWidget	*vbox;
	GtkWidget	*hsep;
	GtkWidget	*hbox;
	GtkWidget	*icon;
	GtkWidget	*text;
	GtkWidget	*button;
	GtkWidget	*button2;
	GtkWidget	*window;
	gboolean	 visible;
	gpointer	 user_data;
	gpointer	 user_data2;
	void		(*press) (NoticeView *, gpointer user_data);
	void		(*press2) (NoticeView *, gpointer user_data);
	gboolean	 icon_clickable;
	GtkWidget 	*evtbox;
#if !GTK_CHECK_VERSION(2,12,0)
	GtkTooltips *tooltips;
#endif
};

NoticeView	*noticeview_create	(MainWindow 	*mainwin);
void		 noticeview_destroy	(NoticeView	*noticeview);
void		 noticeview_init	(NoticeView	*noticeview);
void		 noticeview_set_icon	(NoticeView	*noticeview,
					 StockPixmap	 icon);
void		 noticeview_set_text	(NoticeView	*noticeview,
					 const gchar	*text);
void		 noticeview_set_button_text 
					(NoticeView	*noticeview,
					 const gchar    *text);
void		 noticeview_set_2ndbutton_text 
					(NoticeView	*noticeview,
					 const gchar    *text);
gboolean	 noticeview_is_visible  (NoticeView	*noticeview);
void		 noticeview_show	(NoticeView	*noticeview);
void		 noticeview_hide	(NoticeView	*noticeview);

void		 noticeview_set_button_press_callback
					(NoticeView	*noticeview,
					 void 		(*callback)(void),
					 gpointer	*user_data);
void		 noticeview_set_2ndbutton_press_callback
					(NoticeView	*noticeview,
					 void 		(*callback)(void),
					 gpointer	*user_data);
void		 noticeview_set_icon_clickable
					(NoticeView	*noticeview,
					 gboolean	 setting);			
void		 noticeview_set_tooltip
					(NoticeView	*noticeview,
					 const gchar	*text);			
#endif /* NOTICEVIEW_H__ */
