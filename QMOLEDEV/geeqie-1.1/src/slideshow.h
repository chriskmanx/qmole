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


#ifndef SLIDESHOW_H
#define SLIDESHOW_H


#define SLIDESHOW_SUBSECOND_PRECISION 10
#define SLIDESHOW_MIN_SECONDS    0.1
#define SLIDESHOW_MAX_SECONDS 3600.0

/*
 * It works like this, it uses path_list, if that does not exist, it uses
 * CollectionData, then finally falls back to the layout listing.
 */

void slideshow_free(SlideShowData *ss);

gboolean slideshow_should_continue(SlideShowData *ss);

void slideshow_next(SlideShowData *ss);
void slideshow_prev(SlideShowData *ss);

SlideShowData *slideshow_start_from_filelist(LayoutWindow *target_lw, ImageWindow *imd, GList *list,
					      void (*stop_func)(SlideShowData *, gpointer), gpointer stop_data);
SlideShowData *slideshow_start_from_collection(LayoutWindow *target_lw, ImageWindow *imd, CollectionData *cd,
					       void (*stop_func)(SlideShowData *, gpointer), gpointer stop_data,
					       CollectInfo *start_info);
SlideShowData *slideshow_start(LayoutWindow *lw, gint start_point,
			       void (*stop_func)(SlideShowData *, gpointer), gpointer stop_data);

gboolean slideshow_paused(SlideShowData *ss);
void slideshow_pause_set(SlideShowData *ss, gboolean paused);
gboolean slideshow_pause_toggle(SlideShowData *ss);


#endif
/* vim: set shiftwidth=8 softtabstop=0 cindent cinoptions={1s: */
