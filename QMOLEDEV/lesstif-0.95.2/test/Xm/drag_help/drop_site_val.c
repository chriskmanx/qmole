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
			XtVaSetValues(w, XmNlabelPixmap, pixmap, 0);
		call_data->animate = False;
	}
}
