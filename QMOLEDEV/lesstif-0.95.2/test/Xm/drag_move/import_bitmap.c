/* $Header: /cvsroot/lesstif/lesstif/test/Xm/drag_move/import_bitmap.c,v 1.3 2001/05/23 13:49:24 amai Exp $ */

#include <Xm/Xm.h>
#include <Xm/DragDrop.h>
#include "transfer.h"

extern void     transfer_bitmap();

void
drop_transfer_destroy(w, client_data, call_data)
	Widget          w;
	XtPointer       client_data;
	XtPointer       call_data;
{
	if (client_data)
		XtFree((char *) client_data);
}

void
import_bitmap(w, client_data, call_data)
	Widget          w;
	XtPointer       client_data, call_data;
{
	XmDropProcCallback drop_data;
	XmDropTransferEntryRec transfer_entries[1];
	Widget          drop_transfer;
	Transfer_p      transfer = NULL;
	Arg             al[5];
	int             ac;

	drop_data = (XmDropProcCallback) call_data;
	ac = 0;
	/* Support both move and copy */
	if (drop_data->dropAction != XmDROP
	    || (drop_data->operation != XmDROP_MOVE
		&& drop_data->operation != XmDROP_COPY)) {
		XtSetArg(al[ac], XmNtransferStatus, XmTRANSFER_FAILURE); ac++;
		XtSetArg(al[ac], XmNnumDropTransfers, 0); ac++;
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
			/*
			 * Initially request single transfer of STRING
			 * target. The transferProc will request transfer of
			 * DELETE target when it has got the STRING
			 */
			transfer = (Transfer_p) XtMalloc(sizeof(Transfer_t));
			transfer->widget = w;
			transfer->operation = drop_data->operation;
			transfer_entries[0].target = XA_STRING;
			transfer_entries[0].client_data = (XtPointer) transfer;
			XtSetArg(al[ac], XmNdropTransfers, transfer_entries); ac++;
			XtSetArg(al[ac], XmNnumDropTransfers, 1); ac++;
			XtSetArg(al[ac], XmNtransferProc, transfer_bitmap); ac++;
		} else {
			XtSetArg(al[ac], XmNtransferStatus, XmTRANSFER_FAILURE); ac++;
		}
	}
	drop_transfer = XmDropTransferStart(drop_data->dragContext, al, ac);
	if (drop_transfer)
		XtAddCallback(drop_transfer, XmNdestroyCallback,
			      drop_transfer_destroy, transfer);
}
