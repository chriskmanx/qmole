/* $Header: /cvsroot/lesstif/lesstif/test/Xm/drag_move/transfer_bitmap.c,v 1.2 2001/05/23 13:49:24 amai Exp $ */

#include <Xm/Xm.h>
#include <Xm/DragDrop.h>
#include <Xm/AtomMgr.h>
#include "transfer.h"

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
	Transfer_p      transfer = (Transfer_p) closure;

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
		status = XmbTextPropertyToTextList(XtDisplay(transfer->widget), &tmp_prop,
						   &tmp_value, &num_vals);
		if (num_vals && (status == Success || status > 0)) {
			Pixel           fg, bg;
			XmDropTransferEntryRec transfer_entries[1];
			/* Get pixmap and set labelPixmap resource */
			XtVaGetValues(transfer->widget, XmNforeground, &fg, XmNbackground,
				      &bg, NULL);
			XtVaSetValues(transfer->widget, XmNlabelPixmap,
			XmGetPixmap(XtScreen(transfer->widget), tmp_value[0], fg, bg),
				      NULL);
			XFreeStringList(tmp_value);
			if (transfer->operation == XmDROP_MOVE) {
				/*
				 * We have successfully completed a move -
				 * tell the initiator to delete the
				 * information transferred by requesting
				 * transfer of the target `DELETE'
				 */
				transfer_entries[0].target = XmInternAtom(
									  XtDisplay(transfer->widget), "DELETE", False);
				transfer_entries[0].client_data = (XtPointer) NULL;
				XmDropTransferAdd(w, transfer_entries, 1);
			}
		}
	}
}
