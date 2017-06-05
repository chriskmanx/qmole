/*
 * Geeqie
 * (C) 2004 John Ellis
 * Copyright (C) 2008 - 2012 The Geeqie Team
 *
 * Author: John Ellis
 *
 * This software is released under the GNU General Public License (GNU GPL).
 * Please read the included file COPYING for more information.
 * This software comes with no warranty of any kind, use at your own risk!
 */


#ifndef SIMILAR_H
#define SIMILAR_H


typedef struct _ImageSimilarityData ImageSimilarityData;
struct _ImageSimilarityData
{
	guint8 avg_r[1024];
	guint8 avg_g[1024];
	guint8 avg_b[1024];

	gboolean filled;
};


ImageSimilarityData *image_sim_new(void);
void image_sim_free(ImageSimilarityData *sd);

void image_sim_fill_data(ImageSimilarityData *sd, GdkPixbuf *pixbuf);
ImageSimilarityData *image_sim_new_from_pixbuf(GdkPixbuf *pixbuf);

gdouble image_sim_compare(ImageSimilarityData *a, ImageSimilarityData *b);
gdouble image_sim_compare_fast(ImageSimilarityData *a, ImageSimilarityData *b, gdouble min);


void image_sim_alternate_set(gboolean enable);
gboolean image_sim_alternate_enabled(void);
void image_sim_alternate_processing(ImageSimilarityData *sd);


#endif
/* vim: set shiftwidth=8 softtabstop=0 cindent cinoptions={1s: */
