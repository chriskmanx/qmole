/* $Header: /cvsroot/lesstif/lesstif/test/Xm/drag_pixmaps/callbacks.c,v 1.2 2001/05/23 13:55:31 amai Exp $ */

#include <Xm/Xm.h>
#include "bitmap.h"
#include <stdio.h>

/* Callbacks for our palette of pixmaps (an XmDrawingArea) */

extern void drag_bitmap();
extern Bitmap_t bitmaps[];
GC              gc = NULL;

static void
create_gc(w)
	Widget          w;
{
	XGCValues       values;
	int             i = 0;
	Pixel           fg, bg;

	if (gc != 0)
		return;

	XtVaGetValues(w, XmNforeground, &fg, XmNbackground, &bg, NULL);
	values.foreground = fg;
	values.background = bg;
	values.fill_style = FillOpaqueStippled;
	gc = XCreateGC(XtDisplay(w), XtWindow(w), GCForeground | GCBackground | GCFillStyle, &values);
	while (bitmaps[i].name) {
		bitmaps[i].pixmap = XCreatePixmapFromBitmapData(XtDisplay(w), XtWindow(w), bitmaps[i].data, bitmaps[i].width, bitmaps[i].height, fg, bg, DefaultDepthOfScreen(XtScreen(w)));
		bitmaps[i].bitmap = XCreateBitmapFromData(XtDisplay(w), XtWindow(w), bitmaps[i].data, bitmaps[i].width, bitmaps[i].height);
		i++;
	}
}

void 
draw_bitmap(w, width, height, bitmap, x, y, line_height)
	Widget          w;
	Bitmap_p        bitmap;
	Position       *x, *y;
	Dimension      *line_height, width, height;
{
	XGCValues       values;

	if (*x + bitmap->width > width) {
		*x = 0;
		*y = *y + *line_height;
		*line_height = 0;
	}
	if (*y + bitmap->height > height)
		return;
	if (bitmap->height > *line_height)
		*line_height = bitmap->height;
	values.ts_x_origin = *x;
	values.ts_y_origin = *y;
	values.stipple = bitmap->bitmap;
	XChangeGC(XtDisplay(w), gc, GCStipple | GCTileStipXOrigin | GCTileStipYOrigin, &values);
	XFillRectangle(XtDisplay(w), XtWindow(w), gc, *x, *y, bitmap->width, bitmap->height);
	*x = *x + bitmap->width;
}

void
do_expose(w, client_data, call_data)
	Widget          w;
	XtPointer       client_data;
	XtPointer       call_data;
{
	int             i = 0;
	Dimension       line_height = 0;
	Position        x = 0;
	Position        y = 0;
	Dimension       width, height;

	create_gc(w);
	XtVaGetValues(w, XmNwidth, &width, XmNheight, &height, NULL);
	while (bitmaps[i].name)
		draw_bitmap(w, width, height, &(bitmaps[i++]), &x, &y, &line_height);
}

void
do_resize(w, client_data, call_data)
	Widget          w;
	XtPointer       client_data;
	XtPointer       call_data;
{
	if (XtIsRealized(w))
		XClearArea(XtDisplay(w), XtWindow(w), 0, 0, 0, 0, False);
}

void
do_input(w, client_data, cd)
	Widget          w;
	XtPointer       client_data;
	XtPointer       cd;
{
	int             i = 0;
	Dimension       line_height = 0;
	Position        x = 0;
	Position        y = 0;
	Dimension       width, height;
	Bitmap_p        bitmap = NULL;
	XmDrawingAreaCallbackStruct *call_data = (XmDrawingAreaCallbackStruct *) cd;

	if (call_data->event->type == ButtonPress && call_data->event->xbutton.button == 2) {
		XtVaGetValues(w, XmNwidth, &width, XmNheight, &height, NULL);
		while (bitmaps[i].name) {
			if (x + bitmaps[i].width > width) {
				x = 0;
				y = y + line_height;
				line_height = 0;
			}
			if (y + bitmaps[i].height > height)
				break;
			if (bitmaps[i].height > line_height)
				line_height = bitmaps[i].height;
			if (call_data->event->xbutton.x <= x + bitmaps[i].width
			    && call_data->event->xbutton.y <= y + bitmaps[i].height) {
				bitmap = &(bitmaps[i]);
				break;
			}
			x = x + bitmaps[i].width;
			i++;
		}
		if (bitmap) {
			printf("Got : %s\n", bitmap->name);
			drag_bitmap(w, bitmap, call_data->event);
		}
	}
}
