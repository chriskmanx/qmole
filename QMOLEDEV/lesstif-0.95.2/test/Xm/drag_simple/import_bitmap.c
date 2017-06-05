/* $Header: /cvsroot/lesstif/lesstif/test/Xm/drag_simple/import_bitmap.c,v 1.4 2001/05/23 13:55:31 amai Exp $ */

#include <stdio.h>
#include <Xm/Xm.h>
#include <Xm/DragDrop.h>

extern void transfer_bitmap();

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
		Boolean         ok = False;
		/* Check that there is a compatible target (we need STRING) */
		XtVaGetValues(drop_data->dragContext,
			      XmNexportTargets, &export_list,
			      XmNnumExportTargets, &export_count, NULL);
		for (i = 0; i < export_count; i++)
			if (export_list[i] == XA_STRING) {
				ok = True;
				break;
			}
		if (ok) {
			/* We want a single STRING transfer */
			transfer_entries[0].target = XA_STRING;
			transfer_entries[0].client_data = (XtPointer) w;
			XtSetArg(al[ac], XmNdropTransfers, transfer_entries); ac++;
			XtSetArg(al[ac], XmNnumDropTransfers, 1); ac++;
			XtSetArg(al[ac], XmNtransferProc, transfer_bitmap); ac++;
			drop_data->operation = XmDROP_COPY;

			fprintf(stderr, "Drop COPY has succeeded\n");
		} else {
			XtSetArg(al[ac], XmNtransferStatus, XmTRANSFER_FAILURE); ac++;

			fprintf(stderr, "Drop has failed\n");
		}
	}
	XmDropTransferStart(drop_data->dragContext, al, ac);
}
