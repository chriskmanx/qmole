/* $Header: /cvsroot/lesstif/lesstif/test/Xm/drag_move/drag_bitmap.c,v 1.3 2001/05/23 13:49:24 amai Exp $ */

#include <Xm/Xm.h>
#include <Xm/DragDrop.h>
#include "bitmap.h"

extern Boolean export_bitmap();

void 
drag_bitmap(w, bitmap, event)
	Widget          w;
	Bitmap_p        bitmap;
	XEvent         *event;
{
	Widget          drag_context;
	Arg             al[10];
	int             ac = 0;
	Atom            export_targets[1];

	export_targets[0] = XA_STRING;

	XtSetArg(al[ac], XmNconvertProc, export_bitmap); ac++;
	XtSetArg(al[ac], XmNclientData, bitmap); ac++;
	XtSetArg(al[ac], XmNexportTargets, export_targets); ac++;
	XtSetArg(al[ac], XmNnumExportTargets, 1); ac++;
	XtSetArg(al[ac], XmNdragOperations, XmDROP_MOVE | XmDROP_COPY); ac++;
	drag_context = XmDragStart(w, event, al, ac);
}
