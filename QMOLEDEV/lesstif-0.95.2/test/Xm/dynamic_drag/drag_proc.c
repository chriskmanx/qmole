/* $Header: /cvsroot/lesstif/lesstif/test/Xm/dynamic_drag/drag_proc.c,v 1.3 2001/05/23 14:03:36 amai Exp $ */

#include <Xm/Xm.h>
#include <Xm/DragDrop.h>

extern void drop_site_validate();

void 
drag_proc(w, client_data, cd)
	Widget          w;
	XtPointer       client_data;
	XtPointer       cd;
{
	XmDragProcCallbackStruct *call_data
	= (XmDragProcCallbackStruct *) cd;
	static Pixmap   enter_pixmap;

	if (call_data->reason == XmCR_DROP_SITE_ENTER_MESSAGE &&
	    call_data->dropSiteStatus == XmVALID_DROP_SITE) {
		Atom            selection;

		XtVaGetValues(call_data->dragContext, XmNiccHandle, &selection,
			      NULL);
		XtVaGetValues(w, XmNlabelPixmap, &enter_pixmap, NULL);
		XtGetSelectionValue(w, selection, XA_PIXMAP, drop_site_validate,
			       (XtPointer) call_data, call_data->timeStamp);
	} else if (call_data->reason == XmCR_DROP_SITE_LEAVE_MESSAGE)
		XtVaSetValues(w, XmNlabelPixmap, enter_pixmap, NULL);
}
