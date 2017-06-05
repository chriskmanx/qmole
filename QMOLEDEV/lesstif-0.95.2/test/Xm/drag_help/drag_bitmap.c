/* $Header: /cvsroot/lesstif/lesstif/test/Xm/drag_help/drag_bitmap.c,v 1.3 2001/05/23 13:42:30 amai Exp $ */

#include <Xm/Xm.h>
#include <Xm/DragDrop.h>
#include "bitmap.h"

extern Boolean  export_bitmap();

void
drop_site_enter(w, client_data, call_data)
	Widget          w;
	XtPointer       client_data;
	XtPointer       call_data;
{
	Bitmap_p        bitmap = (Bitmap_p) client_data;

	if (((XmDropSiteEnterCallbackStruct *) call_data)->dropSiteStatus ==
	    XmVALID_DROP_SITE)
		XtVaSetValues(w, XmNsourceCursorIcon, bitmap->state_icon, NULL);
}

void
drop_site_leave(w, client_data, call_data)
	Widget          w;
	XtPointer       client_data;
	XtPointer       call_data;
{
	XtVaSetValues(w, XmNsourceCursorIcon, NULL, NULL);
}

void
drag_bitmap(w, bitmap, event)
	Widget          w;
	Bitmap_p        bitmap;
	XEvent         *event;
{
	Widget          drag_context;
	Arg             al[10];
	int             ac = 0;
	Atom            export_targets[2];

	export_targets[0] = XA_STRING;
	export_targets[1] = XA_PIXMAP;

	if (bitmap->state_icon == NULL) {
		XtSetArg(al[ac], XmNpixmap, bitmap->bitmap); ac++;
		XtSetArg(al[ac], XmNwidth, bitmap->width); ac++;
		XtSetArg(al[ac], XmNheight, bitmap->height); ac++;
		bitmap->state_icon = XmCreateDragIcon(w, "drag_icon", al, ac);
		ac = 0;
	}
	XtSetArg(al[ac], XmNconvertProc, export_bitmap); ac++;
	XtSetArg(al[ac], XmNclientData, bitmap); ac++;
	XtSetArg(al[ac], XmNexportTargets, export_targets); ac++;
	XtSetArg(al[ac], XmNnumExportTargets, 2); ac++;
	XtSetArg(al[ac], XmNdragOperations, XmDROP_COPY); ac++;
	drag_context = XmDragStart(w, event, al, ac);

	/* Register callbacks for entering and leaving drop sites */
	if (drag_context) {
		XtAddCallback(drag_context, XmNdropSiteEnterCallback,
			      drop_site_enter, (XtPointer) bitmap);
		XtAddCallback(drag_context, XmNdropSiteLeaveCallback,
			      drop_site_leave, NULL);
		/* Extra target XA_PIXMAP */
		export_targets[0] = XA_STRING;
		export_targets[1] = XA_PIXMAP;
	}
}
