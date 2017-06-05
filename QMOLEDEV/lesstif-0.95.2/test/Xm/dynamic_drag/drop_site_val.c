/* $Header: /cvsroot/lesstif/lesstif/test/Xm/dynamic_drag/drop_site_val.c,v 1.3 2001/05/23 14:03:36 amai Exp $ */

#include <Xm/Xm.h>
#include <Xm/DragDrop.h>

void 
drop_site_validate(w, closure, seltype, type, value, length, format)
	Widget          w;
	XtPointer       closure;
	Atom           *seltype;
	Atom           *type;
	XtPointer       value;
	unsigned long  *length;
	int            *format;
{

	XmDragProcCallbackStruct *call_data = (XmDragProcCallbackStruct *)
	closure;

	if (*type == XA_DRAWABLE) {
		Pixmap          pixmap = *(Pixmap *) value;
		int             x, y, status;
		Window          root_win;
		unsigned int    width, height, border, depth;

		status = XGetGeometry(XtDisplay(w), pixmap, &root_win, &x, &y,
				      &width, &height, &border, &depth);
		if (width != height)
			call_data->dropSiteStatus = XmINVALID_DROP_SITE;
		else
			XtVaSetValues(w, XmNlabelPixmap, pixmap, NULL);
		call_data->animate = False;
	}
}
