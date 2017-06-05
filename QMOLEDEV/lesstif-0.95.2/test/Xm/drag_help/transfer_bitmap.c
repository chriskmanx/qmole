/* $Header: /cvsroot/lesstif/lesstif/test/Xm/drag_help/transfer_bitmap.c,v 1.2 2001/05/23 13:42:30 amai Exp $ */
#include <Xm/Xm.h>
#include <Xm/DragDrop.h>

extern GC gc;

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

	/* Handle pixmap as well as text */
	if (*type == XA_DRAWABLE) {
		Pixmap          pixmap = *(Pixmap *) value;
		if (pixmap == XmUNSPECIFIED_PIXMAP)
			XtVaSetValues(w, XmNtransferStatus, XmTRANSFER_FAILURE,
				      NULL);
		else {
			int             x, y, status;
			Window          root_win;
			unsigned int    width, height, border, depth;
			Pixmap          new_pixmap;

			status = XGetGeometry(XtDisplay(widget), pixmap, &root_win, &x,
				      &y, &width, &height, &border, &depth);
			new_pixmap = XCreatePixmap(XtDisplay(widget), XtWindow
					    (widget), width, height, depth);
			XCopyArea(XtDisplay(widget), pixmap, new_pixmap, gc, 0, 0,
				  width, height, 0, 0);
			XtVaSetValues(widget, XmNlabelPixmap, new_pixmap, NULL);
		}
	}
}
