/* $Header: /cvsroot/lesstif/lesstif/test/Xm/drag_simple/transfer_bitmap.c,v 1.2 2001/05/23 13:55:31 amai Exp $ */

#include <Xm/Xm.h>

void 
transfer_bitmap(w, closure, seltype, type, value, length, format)
	Widget          w;
	XtPointer       closure;
	Atom           *seltype;
	Atom           *type;
	XtPointer       value;
	unsigned long  *length;
	int            *format;
{
	Widget          widget = (Widget) closure;

	if (*type == XA_STRING) {
		XTextProperty   tmp_prop;
		char          **tmp_value;
		int             num_vals;
		int             status;
		/* Extract bitmap name into tmp_value */
		tmp_prop.value = (unsigned char *) value;
		tmp_prop.encoding = *type;
		tmp_prop.format = *format;
		tmp_prop.nitems = *length;
		num_vals = 0;
		status = XmbTextPropertyToTextList(XtDisplay(widget), &tmp_prop,
						   &tmp_value, &num_vals);
		if (num_vals && (status == Success || status > 0)) {
			Pixel           fg, bg;
			/* Get pixmap and set labelPixmap resource */
			XtVaGetValues(widget, XmNforeground, &fg, XmNbackground,
				      &bg, NULL);
			XtVaSetValues(widget, XmNlabelPixmap,
			XmGetPixmap(XtScreen(widget), tmp_value[0], fg, bg),
				      NULL);
			XFreeStringList(tmp_value);
		}
	}
}
