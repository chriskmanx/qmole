/*
 * Geeqie
 * Copyright (C) 2008 - 2012 The Geeqie Team
 *
 * Author: Vladimir Nadvornik
 * based on a patch by Uwe Ohse
 *
 * This software is released under the GNU General Public License (GNU GPL).
 * Please read the included file COPYING for more information.
 * This software comes with no warranty of any kind, use at your own risk!
 */

#ifndef HISTOGRAM_H
#define HISTOGRAM_H

/* Note: The order is important */
#define HCHAN_R 0
#define HCHAN_G 1
#define HCHAN_B 2
#define HCHAN_MAX 3
#define HCHAN_RGB 4
#define HCHAN_COUNT 5
#define HCHAN_DEFAULT HCHAN_RGB


Histogram *histogram_new(void);
void histogram_free(Histogram *histogram);
gint histogram_set_channel(Histogram *histogram, gint chan);
gint histogram_get_channel(Histogram *histogram);
gint histogram_set_mode(Histogram *histogram, gint mode);
gint histogram_get_mode(Histogram *histogram);
gint histogram_toggle_channel(Histogram *histogram);
gint histogram_toggle_mode(Histogram *histogram);
const gchar *histogram_label(Histogram *histogram);

void histmap_free(HistMap *histmap);

const HistMap *histmap_get(FileData *fd);
gboolean histmap_start_idle(FileData *fd);

gboolean histogram_draw(Histogram *histogram, const HistMap *histmap, GdkPixbuf *pixbuf, gint x, gint y, gint width, gint height);

void histogram_notify_cb(FileData *fd, NotifyType type, gpointer data);

#endif /* HISTOGRAM_H */
/* vim: set shiftwidth=8 softtabstop=0 cindent cinoptions={1s: */
