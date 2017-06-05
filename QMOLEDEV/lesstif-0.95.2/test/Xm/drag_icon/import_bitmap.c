/* $Header: /cvsroot/lesstif/lesstif/test/Xm/drag_icon/import_bitmap.c,v 1.3 2001/05/23 13:45:57 amai Exp $ */

#include <Xm/Xm.h>
#include <Xm/DragDrop.h>

extern void     transfer_bitmap();

void
import_bitmap(w, client_data, call_data)
	Widget          w;
	XtPointer       client_data, call_data;
{
	XmDropProcCallback drop_data;
	XmDropTransferEntryRec transfer_entries[1];
	Arg             al[5];
	int             ac;

	drop_data = (XmDropProcCallback) call_data;
	ac = 0;
	if (drop_data->dropAction != XmDROP || drop_data->operation !=
	    XmDROP_COPY) {
		XtSetArg(al[ac], XmNtransferStatus, XmTRANSFER_FAILURE); ac++;
	} else {
		Atom           *export_list;
		int             export_count, i;
		Boolean         got_pixmap = False;
		Boolean         got_string = False;

		XtVaGetValues(drop_data->dragContext, XmNexportTargets,
			   &export_list, XmNnumExportTargets, &export_count,
			      NULL);
		for (i = 0; i < export_count; i++)
			if (export_list[i] == XA_STRING)
				got_string = True;
			else if (export_list[i] == XA_PIXMAP)
				got_pixmap = True;
		/* Favour pixmap over string */
		if (got_pixmap || got_string) {
			if (got_pixmap)
				transfer_entries[0].target = XA_PIXMAP;
			else
				transfer_entries[0].target = XA_STRING;
			transfer_entries[0].client_data = (XtPointer) w;
			XtSetArg(al[ac], XmNdropTransfers, transfer_entries); ac++;
			XtSetArg(al[ac], XmNnumDropTransfers, 1); ac++;
			XtSetArg(al[ac], XmNtransferProc, transfer_bitmap); ac++;
			drop_data->operation = XmDROP_COPY;
		} else {
			XtSetArg(al[ac], XmNtransferStatus, XmTRANSFER_FAILURE); ac++;
		}
	}
	XmDropTransferStart(drop_data->dragContext, al, ac);
}
