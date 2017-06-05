/*
 * Geeqie
 * (C) 2006 John Ellis
 * Copyright (C) 2008 - 2012 The Geeqie Team
 *
 * Author: John Ellis
 *
 * This software is released under the GNU General Public License (GNU GPL).
 * Please read the included file COPYING for more information.
 * This software comes with no warranty of any kind, use at your own risk!
 */


#include "main.h"
#include "pan-types.h"

#include <math.h>


void pan_grid_compute(PanWindow *pw, FileData *dir_fd, gint *width, gint *height)
{
	GList *list;
	GList *work;
	gint x, y;
	gint grid_size;
	gint next_y;

	list = pan_list_tree(dir_fd, SORT_NAME, TRUE, pw->ignore_symlinks);

	grid_size = (gint)sqrt((gdouble)g_list_length(list));
	if (pw->size > PAN_IMAGE_SIZE_THUMB_LARGE)
		{
		grid_size = grid_size * (512 + PAN_THUMB_GAP) * pw->image_size / 100;
		}
	else
		{
		grid_size = grid_size * (PAN_THUMB_SIZE + PAN_THUMB_GAP);
		}

	next_y = 0;

	*width = PAN_BOX_BORDER * 2;
	*height = PAN_BOX_BORDER * 2;

	x = PAN_THUMB_GAP;
	y = PAN_THUMB_GAP;
	work = list;
	while (work)
		{
		FileData *fd;
		PanItem *pi;

		fd = work->data;
		work = work->next;

		if (pw->size > PAN_IMAGE_SIZE_THUMB_LARGE)
			{
			pi = pan_item_image_new(pw, fd, x, y, 10, 10);

			x += pi->width + PAN_THUMB_GAP;
			if (y + pi->height + PAN_THUMB_GAP > next_y) next_y = y + pi->height + PAN_THUMB_GAP;
			if (x > grid_size)
				{
				x = PAN_THUMB_GAP;
				y = next_y;
				}
			}
		else
			{
			pi = pan_item_thumb_new(pw, fd, x, y);

			x += PAN_THUMB_SIZE + PAN_THUMB_GAP;
			if (x > grid_size)
				{
				x = PAN_THUMB_GAP;
				y += PAN_THUMB_SIZE + PAN_THUMB_GAP;
				}
			}
		pan_item_size_coordinates(pi, PAN_THUMB_GAP, width, height);
		}

	g_list_free(list);
}
/* vim: set shiftwidth=8 softtabstop=0 cindent cinoptions={1s: */
