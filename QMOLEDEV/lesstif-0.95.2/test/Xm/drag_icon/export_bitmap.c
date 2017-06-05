/* $Header: /cvsroot/lesstif/lesstif/test/Xm/drag_icon/export_bitmap.c,v 1.2 2001/05/23 13:45:57 amai Exp $ */

#include <Xm/Xm.h>
#include "bitmap.h"

Boolean
export_bitmap(w, selection, target, type_return, value, length,
	      format, max_length, client_data, request_id)
	Widget          w;
	Atom           *selection, *target, *type_return;
	XtPointer      *value, client_data;
	unsigned long  *length, *max_length;
	int            *format;
{
	Bitmap_p        bitmap;
	XTextProperty   tmp_prop;
	int             status = 0;

	if ((*target != XA_STRING) && (*target != XA_PIXMAP))
		return False;
	XtVaGetValues(w, XmNclientData, &bitmap, NULL);

	/* Handle both pixmap and string targets */

	if (*target == XA_PIXMAP) {
		Pixmap         *pixmap = (Pixmap *) XtMalloc(sizeof(Pixmap));
		*pixmap = bitmap->pixmap;
		*value = (XtPointer) pixmap;
		*type_return = XA_DRAWABLE;
		*length = sizeof(Pixmap);
		*format = 32;
		return True;
	}

	if (*target == XA_STRING) {
		status = XmbTextListToTextProperty(XtDisplay(w), &(bitmap->name), 1,
			       (XICCEncodingStyle) XStringStyle, &tmp_prop);
		if (status == Success || status > 0) {
			*type_return = XA_STRING;
			*format = 8;
			*value = (XtPointer) XtMalloc((unsigned) tmp_prop.nitems);
			memcpy((void *) *value,
			(void *) tmp_prop.value, (unsigned) tmp_prop.nitems);
			XFree((char *) tmp_prop.value);
			*length = tmp_prop.nitems;
			return True;
		}
	}
	return False;
}
