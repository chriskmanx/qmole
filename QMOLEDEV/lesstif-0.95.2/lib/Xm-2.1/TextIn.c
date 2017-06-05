/**
 *
 * $Header: /cvsroot/lesstif/lesstif/lib/Xm-2.1/TextIn.c,v 1.3 2006/04/19 18:42:22 dannybackx Exp $
 *
 * Copyright (C) 1995 Free Software Foundation, Inc.
 * Copyright © 1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004 LessTif Development Team
 *
 * This file is part of the GNU LessTif Library.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Library General Public License for more details.
 *
 * You should have received a copy of the GNU Library General Public
 * License along with this library; if not, write to the Free
 * Software Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 *
 **/

static const char rcsid[] = "$Header: /cvsroot/lesstif/lesstif/lib/Xm-2.1/TextIn.c,v 1.3 2006/04/19 18:42:22 dannybackx Exp $";

#include <LTconfig.h>

#include <stdio.h>
#include <string.h>
#include <stdlib.h>

#include <X11/Xatom.h>

#include <Xm/XmP.h>
#include <Xm/TextP.h>
#include <Xm/TextOutP.h>
#include <Xm/XmIm.h>

#include <XmI/XmI.h>

#include <Xm/AtomMgr.h>
#include <Xm/DragC.h>
#include <Xm/DropSMgr.h>
#include <Xm/DropTrans.h>
#include <XmI/AtomMgrI.h>
#include <XmI/DragDropI.h>

#include <XmI/DebugUtil.h>


#ifdef Out_FontTextWidth 
#undef Out_FontTextWidth
#endif
#define Out_FontTextWidth(o,s,l)  (int)_XmOut_FontTextWidth(o, s, l)


static void Activate(Widget w, XEvent *ev, String *params,
		     Cardinal *num_params);
static void MoveBackwardChar(Widget w, XEvent *ev, String *params,
			     Cardinal *num_params);
static void MoveBackwardParagraph(Widget w, XEvent *ev, String *params,
				  Cardinal *num_params);
static void MoveBackwardWord(Widget w, XEvent *ev, String *params,
			     Cardinal *num_params);
static void RingBell(Widget w, XEvent *ev, String *params,
		     Cardinal *num_params);
static void MoveBeginningOfFile(Widget w, XEvent *ev, String *params,
				Cardinal *num_params);
static void MoveToLineStart(Widget w, XEvent *ev, String *params,
			    Cardinal *num_params);
static void ClearSelection(Widget w, XEvent *ev, String *params,
			   Cardinal *num_params);
static void CopyClipboard(Widget w, XEvent *ev, String *params,
			  Cardinal *num_params);
static void CopyPrimary(Widget w, XEvent *ev, String *params,
			Cardinal *num_params);
static void ProcessCopy(Widget w, XEvent *ev, String *params,
			Cardinal *num_params);
static void CutClipboard(Widget w, XEvent *ev, String *params,
			 Cardinal *num_params);
static void CutPrimary(Widget w, XEvent *ev, String *params,
		       Cardinal *num_params);
static void DeleteForwardChar(Widget w, XEvent *ev, String *params,
			      Cardinal *num_params);
static void DeleteBackwardChar(Widget w, XEvent *ev, String *params,
			       Cardinal *num_params);
static void DeleteForwardWord(Widget w, XEvent *ev, String *params,
			      Cardinal *num_params);
static void DeleteBackwardWord(Widget w, XEvent *ev, String *params,
			       Cardinal *num_params);
static void DeleteCurrentSelection(Widget w, XEvent *ev, String *params,
				   Cardinal *num_params);
static void DeleteToEndOfLine(Widget w, XEvent *ev, String *params,
			      Cardinal *num_params);
static void DeleteToStartOfLine(Widget w, XEvent *ev, String *params,
				Cardinal *num_params);
static void DeselectAll(Widget w, XEvent *ev, String *params,
			Cardinal *num_params);
static void DoSelection(Widget w, XEvent *ev, String *params,
			Cardinal *num_params);
static void MoveEndOfFile(Widget w, XEvent *ev, String *params,
			  Cardinal *num_params);
static void MoveToLineEnd(Widget w, XEvent *ev, String *params,
			  Cardinal *num_params);
static void ExtendEnd(Widget w, XEvent *ev, String *params,
		      Cardinal *num_params);
static void StartExtendSelection(Widget w, XEvent *ev, String *params,
				 Cardinal *num_params);
static void FindWord(Widget w, XEvent *ev, String *params,
		     Cardinal *num_params);
static void MoveForwardChar(Widget w, XEvent *ev, String *params,
			    Cardinal *num_params);
static void MoveForwardParagraph(Widget w, XEvent *ev, String *params,
				 Cardinal *num_params);
static void MoveForwardWord(Widget w, XEvent *ev, String *params,
			    Cardinal *num_params);
static void DoGrabFocus(Widget w, XEvent *ev, String *params,
			Cardinal *num_params);
static void InsertString(Widget w, XEvent *ev, String *params,
			 Cardinal *num_params);
static void KeySelection(Widget w, XEvent *ev, String *params,
			 Cardinal *num_params);
static void KillForwardChar(Widget w, XEvent *ev, String *params,
			    Cardinal *num_params);
static void KillForwardWord(Widget w, XEvent *ev, String *params,
			    Cardinal *num_params);
static void KillBackwardChar(Widget w, XEvent *ev, String *params,
			     Cardinal *num_params);
static void KillBackwardWord(Widget w, XEvent *ev, String *params,
			     Cardinal *num_params);
static void KillCurrentSelection(Widget w, XEvent *ev, String *params,
				 Cardinal *num_params);
static void KillToEndOfLine(Widget w, XEvent *ev, String *params,
			    Cardinal *num_params);
static void KillToStartOfLine(Widget w, XEvent *ev, String *params,
			      Cardinal *num_params);
static void MoveDestination(Widget w, XEvent *ev, String *params,
			    Cardinal *num_params);
static void ProcessMove(Widget w, XEvent *ev, String *params,
			Cardinal *num_params);
static void InsertNewLine(Widget w, XEvent *ev, String *params,
			  Cardinal *num_params);
static void InsertNewLineAndBackup(Widget w, XEvent *ev, String *params,
				   Cardinal *num_params);
static void InsertNewLineAndIndent(Widget w, XEvent *ev, String *params,
				   Cardinal *num_params);
static void MoveNextLine(Widget aw, XEvent *ev, String *params,
			 Cardinal *num_params);
static void MoveNextPage(Widget aw, XEvent *ev, String *params,
			 Cardinal *num_params);
static void TraverseNextTabGroup(Widget w, XEvent *ev, String *params,
				 Cardinal *num_params);
static void MovePageLeft(Widget w, XEvent *ev, String *params,
			 Cardinal *num_params);
static void MovePageRight(Widget w, XEvent *ev, String *params,
			  Cardinal *num_params);
static void PasteClipboard(Widget w, XEvent *ev, String *params,
			   Cardinal *num_params);
static void TraversePrevTabGroup(Widget w, XEvent *ev, String *params,
				 Cardinal *num_params);
static void MovePreviousLine(Widget aw, XEvent *ev, String *params,
			     Cardinal *num_params);
static void MovePreviousPage(Widget aw, XEvent *ev, String *params,
			     Cardinal *num_params);
static void ProcessBDrag(Widget w, XEvent *ev, String *params,
			 Cardinal *num_params);
static void ProcessCancel(Widget w, XEvent *ev, String *params,
			  Cardinal *num_params);
static void ProcessDown(Widget w, XEvent *ev, String *params,
			Cardinal *num_params);
static void ProcessUp(Widget w, XEvent *ev, String *params,
		      Cardinal *num_params);
static void ProcessHome(Widget w, XEvent *ev, String *params,
			Cardinal *num_params);
static void ProcessReturn(Widget w, XEvent *ev, String *params,
			  Cardinal *num_params);
static void ProcessShiftDown(Widget w, XEvent *ev, String *params,
			     Cardinal *num_params);
static void ProcessShiftUp(Widget w, XEvent *ev, String *params,
			   Cardinal *num_params);
static void ProcessTab(Widget w, XEvent *ev, String *params,
		       Cardinal *num_params);
static void RedrawDisplay(Widget w, XEvent *ev, String *params,
			  Cardinal *num_params);
static void ScrollCursorVertically(Widget w, XEvent *ev, String *params,
			    Cardinal *num_params);
static void ScrollOneLineDown(Widget w, XEvent *ev, String *params,
			      Cardinal *num_params);
static void ScrollOneLineUp(Widget w, XEvent *ev, String *params,
			    Cardinal *num_params);
static void SecondaryAdjust(Widget w, XEvent *ev, String *params,
			    Cardinal *num_params);
static void SecondaryNotify(Widget w, XEvent *ev, String *params,
			    Cardinal *num_params);
static void SecondaryStart(Widget w, XEvent *ev, String *params,
			    Cardinal *num_params);
static void ExtendSelection(Widget w, XEvent *ev, String *params,
			    Cardinal *num_params);
static void SelectAll(Widget w, XEvent *ev, String *params,
		      Cardinal *num_params);
static void StartExtendSelection(Widget w, XEvent *ev, String *params,
				 Cardinal *num_params);
static void SelfInsert(Widget w, XEvent *ev, String *params,
		       Cardinal *num_params);
static void SetAnchor(Widget w, XEvent *ev, String *params,
		      Cardinal *num_params);
static void SetCursorPosition(Widget w, XEvent *ev, String *params,
			      Cardinal *num_params);
static void SetSelectionHint(Widget w, XEvent *ev, String *params,
			     Cardinal *num_params);
static void ToggleAddMode(Widget w, XEvent *ev, String *params,
			  Cardinal *num_params);
static void ToggleOverstrike(Widget w, XEvent *ev, String *params,
			  Cardinal *num_params);
static void TraverseHome(Widget w, XEvent *ev, String *params,
			 Cardinal *num_params);
static void TraverseDown(Widget w, XEvent *ev, String *params,
			 Cardinal *num_params);
static void TraverseUp(Widget w, XEvent *ev, String *params,
		       Cardinal *num_params);
static void TextFocusIn(Widget w, XEvent *ev, String *params,
			Cardinal *num_params);
static void TextFocusOut(Widget w, XEvent *ev, String *params,
			 Cardinal *num_params);
static void TextLeave(Widget w, XEvent *ev, String *params,
		      Cardinal *num_params);
static void VoidAction(Widget w, XEvent *ev, String *params,
		       Cardinal *num_params);
static void UnKill(Widget w, XEvent *ev, String *params,
		   Cardinal *num_params);

static Boolean drag_convert_proc(Widget w, Atom *selection,
				 Atom *target, Atom *type_return,
				 XtPointer *value_return,
				 unsigned long *length_return,
				 int *format_return);
static void drag_transfer_proc(Widget dw, XtPointer client, Atom *selection,
				 Atom *type,
				 XtPointer value,
				 unsigned long *length,
				 int *format);
static void drag_drop_finish(Widget w,
			     XtPointer client_data,
			     XtPointer call_data);

static void _KeySelection(Widget w, XEvent *ev, int flag,
                          String *params, Cardinal *np);

#define Offset(field) XtOffsetOf(XmTextInnerRec, inner.in.field)

/* Resources for the TextIn class */
static XtResource input_resources[] =
{
    {
	XmNpendingDelete, XmCPendingDelete, XmRBoolean,
	sizeof(Boolean), Offset(pendingdelete),
	XmRImmediate, (XtPointer)True
    },
    {
	XmNselectionArray, XmCSelectionArray, XmRPointer,
	sizeof(XtPointer), Offset(sarray),
	XmRImmediate, (XtPointer)NULL
	/* FIX ME: Motif has XmRInt, wacky value here */
    },
    {
	XmNselectionArrayCount, XmCSelectionArrayCount, XmRInt,
	sizeof(int), Offset(sarraycount),
	XmRImmediate, (XtPointer)3
	/* FIX ME: Motif has XmRInt, (XtPointer)whacko value here */
    },
    {
	XmNselectThreshold, XmCSelectThreshold, XmRInt,
	sizeof(int), Offset(threshold),
	XmRImmediate, (XtPointer)5
    },
};

/* action table table */

static XtActionsRec ZdefaultTextActionsTable[] =
{
    {"activate", Activate},
    {"backward-character", MoveBackwardChar},
    {"backward-paragraph", MoveBackwardParagraph},
    {"backward-word", MoveBackwardWord},
    {"beep", RingBell},
    {"beginning-of-file", MoveBeginningOfFile},
    {"beginning-of-line", MoveToLineStart},
    {"clear-selection", ClearSelection},
    {"copy-clipboard", CopyClipboard},
    {"copy-primary", CopyPrimary},
    {"copy-to", ProcessCopy},
    {"cut-clipboard", CutClipboard},
    {"cut-primary", CutPrimary},
    {"delete-next-character", DeleteForwardChar},
    {"delete-previous-character", DeleteBackwardChar},
    {"delete-next-word", DeleteForwardWord},
    {"delete-previous-word", DeleteBackwardWord},
    {"delete-selection", DeleteCurrentSelection},
    {"delete-to-end-of-line", DeleteToEndOfLine},
    {"delete-to-start-of-line", DeleteToStartOfLine},
    {"deselect-all", DeselectAll},
    {"do-quick-action", VoidAction},
    {"end-of-file", MoveEndOfFile},
    {"end-of-line", MoveToLineEnd},
    {"enter", _XmPrimitiveEnter},
    {"extend-adjust", ExtendSelection},
    {"extend-end", ExtendEnd},
    {"extend-start", StartExtendSelection},
    {"find-word", FindWord},
    {"forward-character", MoveForwardChar},
    {"forward-paragraph", MoveForwardParagraph},
    {"forward-word", MoveForwardWord},
    {"grab-focus", DoGrabFocus},
    {"Help", _XmPrimitiveHelp},
    {"insert-string", InsertString},
    {"key-select", KeySelection},
    {"kill-next-character", KillForwardChar},
    {"kill-next-word", KillForwardWord},
    {"kill-previous-character", KillBackwardChar},
    {"kill-previous-word", KillBackwardWord},
    {"kill-selection", KillCurrentSelection},
    {"kill-to-end-of-line", KillToEndOfLine},
    {"kill-to-start-of-line", KillToStartOfLine},
    {"leave", TextLeave},
    {"move-destination", MoveDestination},
    {"move-to", ProcessMove},
    {"newline", InsertNewLine},
    {"newline-and-backup", InsertNewLineAndBackup},
    {"newline-and-indent", InsertNewLineAndIndent},
    {"next-line", MoveNextLine},
    {"next-page", MoveNextPage},
    {"next-tab-group", TraverseNextTabGroup},
    {"page-left", MovePageLeft},
    {"page-right", MovePageRight},
    {"paste-clipboard", PasteClipboard},
    {"prev-tab-group", TraversePrevTabGroup},
    {"previous-line", MovePreviousLine},
    {"previous-page", MovePreviousPage},
    {"process-bdrag", ProcessBDrag},
    {"process-cancel", ProcessCancel},
    {"process-down", ProcessDown},
    {"process-up", ProcessUp},
    {"process-home", ProcessHome},
    {"process-return", ProcessReturn},
    {"process-shift-down", ProcessShiftDown},
    {"process-shift-up", ProcessShiftUp},
    {"process-tab", ProcessTab},
    {"quick-copy-set", VoidAction},
    {"quick-cut-set", VoidAction},
    {"redraw-display", RedrawDisplay},
    {"scroll-cursor-vertically", ScrollCursorVertically},
    {"scroll-one-line-down", ScrollOneLineDown},
    {"scroll-one-line-up", ScrollOneLineUp},
    {"secondary-adjust", SecondaryAdjust},
    {"secondary-notify", SecondaryNotify},
    {"secondary-start", SecondaryStart},
    {"select-adjust", DoSelection},
    {"select-all", SelectAll},
    {"select-end", DoSelection},
    {"select-start", StartExtendSelection},
    {"self-insert", SelfInsert},
    {"set-anchor", SetAnchor},
    {"set-insertion-point", SetCursorPosition},
    {"set-selection-hint", SetSelectionHint},
    {"toggle-add-mode", ToggleAddMode},
    {"toggle-overstrike", ToggleOverstrike},
    {"traverse-home", TraverseHome},
    {"traverse-next", TraverseDown},
    {"traverse-prev", TraverseUp},
    {"focusIn", TextFocusIn},
    {"focusOut", TextFocusOut},
    {"unkill", UnKill},
    {"unmap", _XmPrimitiveUnmap},
};


XtPointer _XmdefaultTextActionsTable = (XtPointer)ZdefaultTextActionsTable;

Cardinal _XmdefaultTextActionsTableSize = XtNumber(ZdefaultTextActionsTable);

static void
Invalidate(XmTextWidget w, XmTextPosition start, XmTextPosition end,
	   long delta)
{
    if ( delta == PASTENDPOS )
	In_HighlightEnd(Text_InputData(w)) = 0 ;
    else
	{
	/*CP:This works in some cases but not all !!
	In_HighlightEnd(Text_InputData(w)) += delta ;
	CP*/
	}
}


static void
InputGetValues(Widget w, ArgList args, Cardinal num_args)
{
    XtGetSubvalues(Text_InnerWidget(w),
		   input_resources, XtNumber(input_resources),
		   args, num_args);

    DEBUGOUT(_LtDebug(__FILE__, w, "InputGetValues :\n"));

    _LtDebugPrintArgList(__FILE__, w, args, num_args, True);
}


/*
 * Something analogous to OutputSetValues should probably happen here.
 * This means we'll have to create a temporary copy of some stuff, something
 * that Xt normally does for us. Unfortunately, somebody (me - hehe) got the
 * unfortunate idea to implement things this way.
 * Other ways are probably more trouble...
 */
static void
InputSetValues(Widget old, Widget request, Widget new_w,
	       ArgList args, Cardinal *num_args)
{
    DEBUGOUT(_LtDebug(__FILE__, new_w, "InputSetValues :\n"));

    DEBUGOUT(_LtDebugPrintArgList(__FILE__, new_w, args, *num_args, False));

/* FIX ME : Need to treat the following resources :
 *    XmNpendingDelete
 *      XmNselectionArray + XmNselectionArrayCount
 *      XmNselectThreshold
 */
}


static void
InputDestroy(Widget aw)
{
    XmTextWidget w = (XmTextWidget)aw;

    DEBUGOUT(_LtDebug(__FILE__, aw, "InputDestroy\n"));

    (*Text_Source(w)->RemoveWidget) (Text_Source(w), w);

    XtFree((char *)In_SelArray(Text_Input(w)->data) );
    XtFree((char *)Text_Input(w));
}

static void
GetSecResData(XmSecondaryResourceData *secres)
{
}

static InputRec inputRec =
{
    /* _InputDataRec             */ NULL,
    /* InputInvalidateProc       */ Invalidate,
    /* InputGetValuesProc        */ InputGetValues,
    /* InputSetValuesProc        */ InputSetValues,
    /* XtWidgetProc              */ InputDestroy,
    /* InputGetSecResProc        */ GetSecResData
};


extern void
_XmTextInputCreate(Widget aw, ArgList args, Cardinal num_args)
{
    XmTextWidget w = (XmTextWidget)aw;
    XmTextInnerWidget iw = (XmTextInnerWidget)w->text.inner_widget;
    InputData i;

    Text_Input(w) = (Input)XtMalloc(sizeof(InputRec));
    memcpy(Text_Input(w), &inputRec, sizeof(InputRec));

    i = Text_Input(w)->data = &iw->inner.in;

    XtGetSubresources(aw, iw,
		      aw->core.name,
		      aw->core.widget_class->core_class.class_name,
		      input_resources,
		      XtNumber(input_resources),
		      args, num_args);


    if (!Text_Source(w))
    {
	Text_Source(w) = _XmStringSourceCreate(Text_Value(w), False);
	Text_Source(w) -> data -> editable = Text_Editable(w);
    }

    (*Text_Source(w)->AddWidget) (Text_Source(w), w);

      /* CP :Thursday 13 May 1999
	FIXME : is this correct for version 2.x ? */
    In_SelArray(i) = (XmTextScanType*)XtMalloc ( sizeof(XmTextScanType) * 4 );
    In_SelArray(i)[0] = XmSELECT_POSITION;
    In_SelArray(i)[1] = XmSELECT_WORD;
    In_SelArray(i)[2] = XmSELECT_LINE;
    In_SelArray(i)[3] = XmSELECT_ALL;
    In_SelArrayCount(i) = 4;
    In_SelArrayIndex(i) = 0;

    In_LastTime(i) = 0;
    In_HighlightStart(i) = 0;
    i->extendDir = XmsdRight;
    i->cancel = True;
    i->overstrike = False;

    DEBUGOUT(_LtDebug(__FILE__, aw,
		      "_XmTextInputCreate (selectionArrayCount %d)\n",
		      In_SelArrayCount(i)));

    DEBUGOUT(_LtDebugPrintArgList(__FILE__, aw, args, num_args, False));
}

/* Scan type utilities ---------------------------------------------------
 */
static XmTextScanType
ScanType(XmTextWidget w)
{
    InputData i = Text_InputData(w);
    XmTextScanType type;

    if (In_SelArray(i))
    {
	if (In_SelArrayIndex(i) >= In_SelArrayCount(i))
	{
	    In_SelArrayIndex(i) = 0;
	}

	type = In_SelArray(i)[In_SelArrayIndex(i)];
    }
    else
    {
	switch (In_SelArrayIndex(i))
	{
	case 1:
	    type = XmSELECT_WORD;
	    break;

	case 2:
	    type = XmSELECT_LINE;
	    break;

	case 3:
	    type = XmSELECT_ALL;
	    break;

	default:
	    type = XmSELECT_POSITION;
	    In_SelArrayIndex(i) = 0;
	    break;
	}
    }

    DEBUGOUT(_LtDebug(__FILE__, (Widget)w,
		      "ScanType(%d) -> %s\n", In_SelArray(i),
		      (type == XmSELECT_WORD) ? "XmSELECT_WORD" :
		      (type == XmSELECT_LINE) ? "XmSELECT_LINE" :
		      (type == XmSELECT_ALL)  ? "XmSELECT_ALL" :
		      (type == XmSELECT_POSITION) ? "XmSELECT_POSITION" : "???"));

    return type;
}

/* High level text insert and delete routines ------------------------------ */
static void
VerifyBell(XmTextWidget w)
{
    if (Text_VerifyBell(w))
    {
	XBell(XtDisplay((Widget)w), 50);
    }
}

/* Need this in XmTextRemove, hence no longer static */
extern void
_XmTextDelete(XmTextWidget w, XEvent *ev, XmTextPosition start, XmTextPosition end)
{
    Widget aw = (Widget)w;
    XmTextVerifyCallbackStruct cbs;
    XmTextBlockRec blockrec;

/*    printf("_XmTextDelete - %li %li\n", start, end); */
    if (end <= 0)
    {
	end = 0;
    }

    if ( start > end )
    {
	XmTextPosition tmp = start;
	start = end;
	end = tmp;
    }
    blockrec.ptr = NULL;
    blockrec.length = 0;
    blockrec.format = XmFMT_8_BIT;

    cbs.reason = XmCR_MODIFYING_TEXT_VALUE;
    cbs.event = ev;
/*CP: when doing a delete-previous-char these are not the same. Is this true for other actions ? */
/*    cbs.startPos = cbs.newInsert = start; */
    cbs.startPos = start;
    if (Text_CursorPos(w) <= start) {
	cbs.newInsert = Text_CursorPos(w);
    } else if ( Text_CursorPos(w) + ( end - start ) > Text_LastPos(w) )
    {
	/*CP: WE do not want to new insertion position to be past the end. */
	cbs.newInsert = Text_CursorPos(w) - ( end - start ) ;

	if (cbs.newInsert < 0)
		cbs.newInsert = 0;	/* Bug # 721010 */
    }
    else
    {
	/* rws 4 Mar 2000
	   This is wrong in the case of delete-previous-character. Think about
	   it, if you are at position 1 and do a delete-previous, you should
	   end up at position 0. Is the other half of this if not always
	   correct??
	*/
	/*
	cbs.newInsert = Text_CursorPos(w);
	*/
	cbs.newInsert = Text_CursorPos(w) - ( end - start ) ;

	/* Danny 17 mar 2000 don't go below position 0 */
	if (cbs.newInsert < 0)
		cbs.newInsert = 0;
    }
    cbs.endPos = end;
    cbs.currInsert = Text_CursorPos(w);
    cbs.text = &blockrec;
    cbs.doit = True;

    if (Text_ModifyVerifyCallback(w))
    {
	XtCallCallbacks((Widget)w, XmNmodifyVerifyCallback, &cbs);

	if (!cbs.doit)
	{
	    VerifyBell(w);
	}
    }

    if (cbs.doit)
    {
	XmTextStatus status;

	start = cbs.startPos;
	end = cbs.endPos;
	status = (*Text_Source(w)->Replace) (w, ev, &start, &end,
					     &blockrec, False);

	if (status == EditDone)
	{
	    /*
	     * FIX ME - This code probably belongs in TextStrSo.c.
	     * Think about what should happen if multiple widgets use one
	     * source.
	     * Danny 8/5/1997
	     */
#if 1
	    /* CP:17 May 1999: this does not seem to be needed */
	    _XmTextUpdateLineTable((Widget)w, cbs.startPos,
				   cbs.endPos, &blockrec, True);
#endif
	    _XmTextSetCursorPosition(aw, cbs.newInsert);
	}
    }

    /* FIX ME:  Are we required to free this if it exists?  It can only exist
     * if the user made it, but does Motif think that the user malloced it?
     */
/*   if (blockrec.ptr) XtFree(blockrec.ptr); */
}


static void
DoInsert(XmTextWidget w, XEvent *ev, char *buf, int len)
{
    Widget aw = (Widget)w;
    XmTextVerifyCallbackStruct cbs;
    XmTextBlockRec blockrec;
    Boolean sel;
    XmTextPosition left, right, startInsert;

    if (len <= 0)
    {
	return;
    }

    if (!Text_Editable(w))
    {
	VerifyBell(w);
	return;
    }

    (*Text_Output(w)->DrawInsertionPoint) (w, Text_CursorPos(w), off);

    blockrec.ptr = XtMalloc(len + 1);
    blockrec.length = len;
    blockrec.format = XmFMT_8_BIT;
    strncpy(blockrec.ptr, buf, len);
    blockrec.ptr[len] = '\0';

    /* Danny 1/7/97 */
    if (Text_CursorPos(w) == PASTENDPOS)
    {
	Text_CursorPos(w) = Text_LastPos(w);	/* End of text */
    }

    cbs.reason = XmCR_MODIFYING_TEXT_VALUE;
    cbs.event = ev;

    /* SG 23/08/1998, replace primary selection if exists, pending delete
       is true and the insertion cursor is inside the selection.
     */
    sel = (*Text_Source(w)->GetSelection) (Text_Source(w), &left, &right);

    if (sel && In_PendingDelete(Text_Input(w)->data) &&
        Text_CursorPos(w)>=left && Text_CursorPos(w)<=right)
    {
        cbs.currInsert = Text_CursorPos(w);
        cbs.startPos = left;
        cbs.endPos = right;
        startInsert = left ;
        cbs.newInsert = cbs.currInsert;
    } else if (Text_InputData(w)->overstrike) {
	DEBUGOUT(_LtDebug(__FILE__, (Widget)w, "DoInsert overstrike\n"));
        cbs.currInsert = cbs.startPos = cbs.endPos = Text_CursorPos(w);
        startInsert = cbs.currInsert ;
        cbs.newInsert = cbs.currInsert;
	/*
	 * Implement the overstrike rules :
	 *	- insert if at the end of a line,
	 *	- overwrite otherwise
	 */
	if (1) {	/* FIX ME */
		cbs.endPos ++;
	}
    } else {
        cbs.currInsert = cbs.startPos = cbs.endPos = Text_CursorPos(w);
        startInsert = cbs.currInsert ;
        cbs.newInsert = cbs.currInsert;
    }

    DEBUGOUT(_LtDebug(__FILE__, (Widget)w,
		      "DoInsert before CB: CursorPos %08x len %08x\n",
		      Text_CursorPos(w), len));

    cbs.text = &blockrec;
    cbs.doit = True;

    if (Text_ModifyVerifyCallback(w))
    {
	XtCallCallbacks((Widget)w, XmNmodifyVerifyCallback, &cbs);

	if (!cbs.doit)
	{
	    VerifyBell(w);
	}
    }

    if (cbs.doit)
    {
	XmTextStatus status;
	XmTextPosition start, end;

	start = cbs.startPos;
	end = cbs.endPos;
	DEBUGOUT(_LtDebug(__FILE__, (Widget)w,
			  "DoInsert: start %08x:%08x end %08x:%08x\n",
			  start, cbs.startPos, end, cbs.endPos));
	status = (*Text_Source(w)->Replace) (w, ev, &start,
					     &end, &blockrec, False);

	if (status == EditDone && blockrec.length > 0 )
	{
	    _XmTextSetCursorPosition(aw, startInsert + blockrec.length );
	}
    }
    Text_PendingOff(w) = False; 
    (*Text_Output(w)->DrawInsertionPoint) (w, Text_CursorPos(w), on );

    XtFree(blockrec.ptr);
}


/*
 * Figure out what is supposed to get selected after this many clicks,
 *      also select it.
 * Number of clicks was recorded elsewhere, and stored into
 *      In_SelArrayIndex(Text_InputData(w)). The function ScanType()
 *      translates that into a type of selection (e.g. XmSELECT_WORD),
 *      based on which we really select text here.
 */
static void
DoScanType(XmTextWidget w, XEvent *ev, XmTextPosition pos)
{
    InputData i = Text_InputData(w);
    XmTextScanType st = ScanType(w);
    XmTextPosition left, right;

    switch (st)
    {
    case XmSELECT_POSITION:
	DEBUGOUT(_LtDebug(__FILE__, (Widget)w,
			  "DoScanType: XmSELECT_POSITION\n"));

	_XmTextSetCursorPosition((Widget)w, pos);

	/* Undo selection */
	XmTextSetSelection((Widget)w, pos, pos, ev->xbutton.time);

	In_HighlightPivot(i) = Text_CursorPos(w);
	break;

    case XmSELECT_WORD:
	right = (*Text_Source(w)->Scan) (Text_Source(w), pos, st,
					 XmsdRight, 1, False);
	left = (*Text_Source(w)->Scan) (Text_Source(w), right, st,
					XmsdLeft, 1, False);
        In_HighlightPivot(i) = left;

	DEBUGOUT(_LtDebug(__FILE__, (Widget)w,
		       "DoScanType: XmSELECT_WORD (left %d pos %d right %d)\n",
			  left, pos, right));

	XmTextSetSelection((Widget)w, left, right, ev->xbutton.time);

	break;

    case XmSELECT_LINE:
	right = (*Text_Source(w)->Scan) (Text_Source(w), pos, st,
					 XmsdRight, 1, False);
	left = (*Text_Source(w)->Scan) (Text_Source(w), right, st,
					XmsdLeft, 1, False);
        In_HighlightPivot(i) = left;

	DEBUGOUT(_LtDebug(__FILE__, (Widget)w,
		       "DoScanType: XmSELECT_LINE (left %d pos %d right %d)\n",
			  left, pos, right));

	XmTextSetSelection((Widget)w, left, right + 1, ev->xbutton.time);
	break;

    case XmSELECT_OUT_LINE:	/* Motif 2.* feature - unimplemented here */
	DEBUGOUT(_LtDebug(__FILE__, (Widget)w,
			  "DoScanType: XmSELECT_OUT_LINE\n"));
	break;

    case XmSELECT_ALL:
	In_HighlightPivot(i) = left = 0;
	right = XmTextGetLastPosition((Widget)w);

	DEBUGOUT(_LtDebug(__FILE__, (Widget)w,
			"DoScanType: XmSELECT_ALL (left %d pos %d right %d)\n",
			  left, pos, right));
 
	XmTextSetSelection((Widget)w, left, right, ev->xbutton.time);

	break;

    default:
	DEBUGOUT(_LtDebug(__FILE__, (Widget)w, "DoScanType: ???\n"));

	break;
    }
}

static void DoUnselectionBecauseOfKeyNavigation(Widget w, XEvent *ev)
{
    if ( _XmStringSourceHasSelection(GetSrc(w)) )
    {
	XmTextSetSelection(w,-1,-1,ev->xkey.time);
    }
}

/* Action Routines -------------------------------------------------------- */

static void
Activate(Widget w, XEvent *ev, String *params, Cardinal *num_params)
{
    XmTextWidget tw = (XmTextWidget)w;
    XmAnyCallbackStruct cb;

    if (Text_EditMode(tw) != XmSINGLE_LINE_EDIT)
    {
	return;
    }

    DEBUGOUT(_LtDebug(__FILE__, w, "Activate\n"));

    cb.reason = XmCR_ACTIVATE;
    cb.event = ev;

    XtCallCallbackList(w, tw->text.activate_callback, (XtPointer)&cb);
    if (XmIsManager(XtParent(w)))
    {
        XmParentProcessDataRec data;

	data.input_action.process_type = XmINPUT_ACTION;
	data.input_action.event = ev;
	data.input_action.action = XmPARENT_ACTIVATE;
	data.input_action.params = params;
	data.input_action.num_params = num_params;

	XtCallActionProc(XtParent(w), "ManagerParentActivate",
			 ev, params, *num_params);
    }
}

static void
MoveBackwardChar(Widget w, XEvent *ev, String *params, Cardinal *num_params)
{
    DEBUGOUT(_LtDebug(__FILE__, w, "MoveBackwardChar\n"));
    if (! Text_AddMode(w))
	DoUnselectionBecauseOfKeyNavigation(w,ev);
    if (Text_CursorPos(w) > 0)
    {
        OutputData o = Text_OutputData((XmTextWidget)w);
        XmTextPosition pos;
        char *ptr;
        int     next, i;

        ptr = Text_Source(w)->data->ptr;

        next = Out_FontTextWidth(o, ptr, Text_CursorPos(w)-1);
        pos = Text_CursorPos(w)-1;
        for(i=Text_CursorPos(w)-2; i>=0; i--){
            if (next > Out_FontTextWidth(o, ptr, i)){
                pos = i + 1;
                break;
            }
            if (next == Out_FontTextWidth(o, ptr, i)){
                pos = i;
            }
        }

	_XmTextSetCursorPosition((Widget)w, pos);
    }
}

static void
MoveBackwardParagraph(Widget w, XEvent *ev,
		      String *params, Cardinal *num_params)
{
    DEBUGOUT(_LtDebug(__FILE__, w, "MoveBackwardParagraph"));

    if (! Text_AddMode(w))
	DoUnselectionBecauseOfKeyNavigation(w,ev);
    if (Text_CursorPos(w) > 0)
    {
	XmTextPosition pos;

	pos = (*Text_Source(w)->Scan) (Text_Source(w), Text_CursorPos(w),
				     XmSELECT_WHITESPACE, XmsdLeft, 1, False);

	pos = (*Text_Source(w)->Scan) (Text_Source(w), pos,
				       XmSELECT_PARAGRAPH, XmsdLeft, 1, False);

	_XmTextSetCursorPosition((Widget)w, pos);
    }
}

static void
MoveBackwardWord(Widget w, XEvent *ev, String *params, Cardinal *num_params)
{
    DEBUGOUT(_LtDebug(__FILE__, w, "MoweBackwardWord"));

    if (! Text_AddMode(w))
	DoUnselectionBecauseOfKeyNavigation(w,ev);
    if (Text_CursorPos(w) > 0)
    {
	XmTextPosition pos;

	pos = (*Text_Source(w)->Scan) (Text_Source(w), Text_CursorPos(w),
				     XmSELECT_WHITESPACE, XmsdLeft, 1, False);

	pos = (*Text_Source(w)->Scan) (Text_Source(w), pos,
				       XmSELECT_WORD, XmsdLeft, 1, False);

	_XmTextSetCursorPosition((Widget)w, pos);
    }
}

static void
RingBell(Widget w, XEvent *ev, String *params, Cardinal *num_params)
{
    XBell(XtDisplay((Widget)w), 50);
}

static void
MoveBeginningOfFile(Widget w, XEvent *ev, String *params,
		    Cardinal *num_params)
{
    DEBUGOUT(_LtDebug(__FILE__, w, "MoveBeginningOfFile"));

    if (! Text_AddMode(w))
	DoUnselectionBecauseOfKeyNavigation(w,ev);
    if (Text_CursorPos(w) > 0)
    {
	_XmTextSetCursorPosition(w, 0);
    }
}

static void
MoveToLineStart(Widget w, XEvent *ev, String *params,
		Cardinal *num_params)
{
    XmTextPosition pos;

    
    if (*num_params==1 && !strcmp("extend", *params)) {
       DEBUGOUT(_LtDebug(__FILE__, w, "MoveToLineStart(extend)"));
       _KeySelection(w, ev, 3, params, num_params);   
    } else {
       DEBUGOUT(_LtDebug(__FILE__, w, "MoveToLineStart"));
       if (! Text_AddMode(w))
	DoUnselectionBecauseOfKeyNavigation(w,ev);
       pos = (*Text_Source(w)->Scan) (Text_Source(w), Text_CursorPos(w),
				   XmSELECT_LINE, XmsdLeft, 1, False);
       _XmTextSetCursorPosition(w, pos);
    }
}

static void
ClearSelection(Widget w, XEvent *ev, String *params, Cardinal *num_params)
{
    DEBUGOUT(_LtDebug(__FILE__, w, "ClearSelection\n"));
}

static void
CopyClipboard(Widget w, XEvent *ev, String *params, Cardinal *num_params)
{
    DEBUGOUT(_LtDebug(__FILE__, w, "CopyClipboard\n"));
    XmTextCopy(w, ev->xkey.time);
}

static void
CopyPrimary(Widget w, XEvent *ev, String *params, Cardinal *num_params)
{
    DEBUGOUT(_LtDebug(__FILE__, w, "CopyPrimary\n"));
}

/* For Selection */
typedef struct {
    XEvent	*ev;
    int		use_ctext;
} SelectionClientData;

/*
 * _XmTextGetSelection is the function that's called when we receive the
 * string from a copy/paste operation.
 * Now we need to insert this text into our widget.
 *
 * It seems that if the widget is read only then we never get here
 * so there is no need to check this (SG 18/08/1998)
 */
static void
_XmTextGetSelection(Widget w, XtPointer client, Atom *selection, Atom *type,
		    XtPointer value, unsigned long *len, int *format)
{
    SelectionClientData *client_data = (SelectionClientData*)client;
    XEvent *ev = (XEvent*)client_data->ev;

    DEBUGOUT(_LtDebug(__FILE__, w, "_XmTextGetSelection\n"));

    if ((!value) || (*len == 0))
    {
    	if (*selection == XA_SECONDARY)
    	{
            if (client_data->use_ctext == 1){
                client_data->use_ctext = 0;
                XtGetSelectionValue(w, XA_SECONDARY, XA_STRING,
                                _XmTextGetSelection,
                                (XtPointer)client_data, ev->xbutton.time);
            }
            else{
                client_data->use_ctext = 1;
                XtGetSelectionValue(w, XA_PRIMARY,
                                XmInternAtom(XtDisplay(w), "COMPOUND_TEXT", False),
                                _XmTextGetSelection,
                                (XtPointer)client_data, ev->xbutton.time);
            }
    	}
    	else
    	{
            if (client_data->use_ctext == 1){
                client_data->use_ctext = 0;
                XtGetSelectionValue(w, XA_PRIMARY, XA_STRING,
                                _XmTextGetSelection,
                                (XtPointer)client_data, ev->xbutton.time);
            }
            else{
                XtFree((char *)client);
            }
    	}
    }
    else
    {
        if (*type == XmInternAtom(XtDisplay(w), "COMPOUND_TEXT", False)||
                        *type == XA_STRING)
	{
	    char *s = (char *)value;

	    if (s)
	    {
		if (*selection == XA_SECONDARY)
		{
                    XTextProperty prop;
                    int count, i;
                    char **list;

                    if (*type == XA_STRING){
                        char *buf;

                        buf = XtMalloc(*len + 1);
                        strncpy(buf, (char *)value, *len);
                        buf[*len] = '\0';

                        XmbTextListToTextProperty(XtDisplay(w), &buf, 1,
                                XCompoundTextStyle, &prop);

                        XtFree(buf);
                    }else{
                        prop.value = (unsigned char *)value;
                        prop.encoding = *type;
                        prop.format = *format;
                        prop.nitems = *len;
                    }

                    XmbTextPropertyToTextList(XtDisplay(w), &prop, &list, &count);

                    for(i=0; i<count; i++){
                        DoInsert((XmTextWidget)w, ev, list[i], strlen(list[i]));
                    }
		}
		else
		{
		XmTextPosition pos;

		    pos = (*Text_Output(w)->XYToPos)((XmTextWidget)w,
		    			ev->xbutton.x, ev->xbutton.y);
		    XmTextSetCursorPosition(w, pos);
                    {
                        XTextProperty prop;
                        int count, i;
                        char **list;

                        if (*type == XA_STRING){
                            char *buf;

                            buf = XtMalloc(*len + 1);
                            strncpy((char *)buf, value, *len);
                            buf[*len] = '\0';

                            XmbTextListToTextProperty(XtDisplay(w), &buf, 1,
                                        XCompoundTextStyle, &prop);

                            XtFree(buf);
                        }else{
                            prop.value = (unsigned char *)value;
                            prop.encoding = *type;
                            prop.format = *format;
                            prop.nitems = *len;
                        }

                        XmbTextPropertyToTextList(XtDisplay(w), &prop, &list, &count);

                        for(i=0; i<count; i++){
                            DoInsert((XmTextWidget)w, ev, list[i], strlen(list[i]));
                        }
                    }
		}
	    }
	}
	XtFree((char *)value);
	XtFree((char *)client);
    }
}

static void
ProcessCopy(Widget w, XEvent *ev, String *params, Cardinal *num_params)
{
    DEBUGOUT(_LtDebug(__FILE__, w, "ProcessCopy\n"));

    if (Text_Editable(w))
    {
        SelectionClientData *client_data;

        client_data = XtNew(SelectionClientData);
        client_data->ev = XtNew(XEvent);

        *client_data->ev = *ev;
        client_data->use_ctext = 1;

    	Text_PendingOff(w)= True;
        XtGetSelectionValue(w, XA_SECONDARY,
                        XmInternAtom(XtDisplay(w), "COMPOUND_TEXT", False),
                        _XmTextGetSelection,
                        (XtPointer)client_data , ev->xbutton.time);
    }
}

static void
ProcessCancel(Widget w, XEvent *ev, String *params, Cardinal *num_params)
{
    DEBUGOUT(_LtDebug(__FILE__, w, "ProcessCancel\n"));
    /* rws 15 Jan 2000
       There is more to it than this. It needs to cancel some operations that
       may be in progress, otherwise do this. Look at the man page.
     */
    if (XmIsManager(XtParent(w)))
    {
    	XtCallActionProc(XtParent(w), "ManagerParentCancel", ev, params, *num_params);
    }
}

static void
CutClipboard(Widget w, XEvent *ev, String *params, Cardinal *num_params)
{
    DEBUGOUT(_LtDebug(__FILE__, w, "CutClipboard\n"));
    XmTextCut(w, ev->xbutton.time);
}

static void
CutPrimary(Widget w, XEvent *ev, String *params, Cardinal *num_params)
{
    DEBUGOUT(_LtDebug(__FILE__, w, "CutPrimary\n"));
}

/*
 * The specs say :
 *    In normal (not add-mode) mode :
 *      - if there is a selection, delete it
 *      - else delete the next character.
 *    In add mode :
 *      - if there's a selection, and the cursor's in it,
 *        and PendingDelete is True, then delete the selection
 *      - otherwise delete the next character.
 */
static void
DeleteForwardChar(Widget w, XEvent *ev, String *params,
		  Cardinal *num_params)
{
    Boolean sel;
    XmTextPosition left, right;

    DEBUGOUT(_LtDebug(__FILE__, w, "DeleteForwardChar"));

    if (!Text_Editable(w))
    {
	VerifyBell((XmTextWidget)w);
	return;
    }

    sel = (*Text_Source(w)->GetSelection) (Text_Source(w), &left, &right);

    /* SG 22/08/1998 - O'Reilly, and experience says that Motif 1.2 deletes
       the selection as per the "add mode" sction above. However add mode
       is not mentioned at all as far as I can tell in Motif 1.2, is this
       a Motif 2.0 thing ? If so it doesn't belong here, except possibly
       ifdef'ed .
     */
    if (sel && In_PendingDelete(Text_Input(w)->data) &&
        Text_CursorPos(w)>=left && Text_CursorPos(w)<=right)
    {
        _XmTextDelete((XmTextWidget)w, ev, left, right);
    }
    else if (Text_CursorPos(w) < Text_LastPos(w))
    {
        OutputData o = Text_OutputData((XmTextWidget)w);
        XmTextPosition pos;
        char    *ptr;
        int     curr, i;

        ptr = Text_Source(w)->data->ptr;
        curr = Out_FontTextWidth(o, ptr, Text_CursorPos(w));
        pos = Text_CursorPos(w);
        for(i=Text_CursorPos(w)+1; i<=Text_LastPos(w); i++){
            if (curr < Out_FontTextWidth(o, ptr, i)){
                pos = i;
                break;
            }
        }

        _XmTextDelete((XmTextWidget)w, ev, Text_CursorPos(w),
    	     pos);
    }
}

static void
DeleteBackwardChar(Widget w, XEvent *ev, String *params,
		   Cardinal *num_params)
{
    Boolean sel;
    XmTextPosition left, right;
    DEBUGOUT(_LtDebug(__FILE__, w, "DeleteBackardChar"));

    if (!Text_Editable(w))
    {
	VerifyBell((XmTextWidget)w);
	return;
    }

    sel = (*Text_Source(w)->GetSelection) (Text_Source(w), &left, &right);

    if(sel && In_PendingDelete(Text_Input(w)->data) &&
       Text_CursorPos(w)>=left && Text_CursorPos(w)<=right)
    {
	/*CP: we have to reset the selection */
      (*Text_Source(w)->SetSelection) (Text_Source(w), right, right, ev->xkey.time);
      _XmTextDelete((XmTextWidget)w,ev,left,right);
    }
    else if (Text_CursorPos(w) > 0)
    {
        OutputData o = Text_OutputData((XmTextWidget)w);
        XmTextPosition pos;
        char    *ptr;
        int     next, i;

        ptr = Text_Source(w)->data->ptr;
        next = Out_FontTextWidth(o, ptr, Text_CursorPos(w)-1);
        pos = Text_CursorPos(w)-1;
        for(i=Text_CursorPos(w)-2; i>=0; i--){
            if (next > Out_FontTextWidth(o, ptr, i)){
                pos = i + 1;
                break;
            }
            if (next == Out_FontTextWidth(o, ptr, i)){
                pos = i;
            }
        }

	_XmTextDelete((XmTextWidget)w, ev, pos, Text_CursorPos(w));
    }
}

static void
DeleteForwardWord(Widget w, XEvent *ev, String *params, Cardinal *num_params)
{
    Boolean sel;
    XmTextPosition left, right;
    DEBUGOUT(_LtDebug(__FILE__, w, "DeleteForwardWord\n"));

    if (!Text_Editable(w))
    {
	VerifyBell((XmTextWidget)w);
	return;
    }

    sel = (*Text_Source(w)->GetSelection) (Text_Source(w), &left, &right);

    if(sel && In_PendingDelete(Text_Input(w)->data) &&
       Text_CursorPos(w)>=left && Text_CursorPos(w)<=right)
    {
      _XmTextDelete((XmTextWidget)w,ev,left,right);
    }
    else
    {
    XmTextPosition pos;

        pos = (*Text_Source(w)->Scan) (Text_Source(w), Text_CursorPos(w),
                                       XmSELECT_WORD, XmsdRight, 1, False);

        pos = (*Text_Source(w)->Scan) (Text_Source(w), pos,
                                       XmSELECT_WHITESPACE, XmsdRight, 1, False);
        _XmTextDelete((XmTextWidget)w, ev,Text_CursorPos(w),pos);
    }
}

static void
DeleteBackwardWord(Widget w, XEvent *ev, String *params, Cardinal *num_params)
{
    Boolean sel;
    XmTextPosition left, right;
    DEBUGOUT(_LtDebug(__FILE__, w, "DeleteBackwardWord\n"));

    if (!Text_Editable(w))
    {
	VerifyBell((XmTextWidget)w);
	return;
    }

    sel = (*Text_Source(w)->GetSelection) (Text_Source(w), &left, &right);

    if(sel && In_PendingDelete(Text_Input(w)->data) &&
       Text_CursorPos(w)>=left && Text_CursorPos(w)<=right)
    {
      _XmTextDelete((XmTextWidget)w,ev,left,right);
    }
    else if (Text_CursorPos(w) > 0)
    {
        XmTextPosition pos;

        pos = (*Text_Source(w)->Scan) (Text_Source(w), Text_CursorPos(w),
                                     XmSELECT_WHITESPACE, XmsdLeft, 1, False);

        pos = (*Text_Source(w)->Scan) (Text_Source(w), pos,
                                       XmSELECT_WORD, XmsdLeft, 1, False);

        _XmTextDelete((XmTextWidget)w,ev,pos,Text_CursorPos(w));
    }
}

static void
DeleteCurrentSelection(Widget w, XEvent *ev,
		       String *params, Cardinal *num_params)
{
    Boolean sel;
    XmTextPosition left, right;

    DEBUGOUT(_LtDebug(__FILE__, w, "DeleteCurrentSelection\n"));

    if (!Text_Editable(w))
    {
        VerifyBell((XmTextWidget)w);
        return;
    }

    sel = (*Text_Source(w)->GetSelection) (Text_Source(w), &left, &right);

    if(sel)
    {
      _XmTextDelete((XmTextWidget)w,ev,left,right);
    }
}

static void
DeleteToEndOfLine(Widget w, XEvent *ev, String *params, Cardinal *num_params)
{
    XmTextPosition pos;
    DEBUGOUT(_LtDebug(__FILE__, w, "DeleteToEndOfLine\n"));

    if (!Text_Editable(w))
    {
	VerifyBell((XmTextWidget)w);
	return;
    }

    pos = (*Text_Source(w)->Scan) (Text_Source(w), Text_CursorPos(w),
				   XmSELECT_LINE, XmsdRight, 1, False);
    _XmTextDelete((XmTextWidget)w,ev,Text_CursorPos(w),pos);
}

static void
DeleteToStartOfLine(Widget w, XEvent *ev, String *params, Cardinal *num_params)
{
    XmTextPosition pos;
    DEBUGOUT(_LtDebug(__FILE__, w, "DeleteToStartOfLine\n"));

    if (!Text_Editable(w))
    {
	VerifyBell((XmTextWidget)w);
	return;
    }

    pos = (*Text_Source(w)->Scan) (Text_Source(w), Text_CursorPos(w),
				   XmSELECT_LINE, XmsdLeft, 1, False);
    _XmTextDelete((XmTextWidget)w,ev,pos,Text_CursorPos(w));
}

static void
DeselectAll(Widget w, XEvent *ev, String *params, Cardinal *num_params)
{
    DEBUGOUT(_LtDebug(__FILE__, w, "DeselectAll\n"));
    (*Text_Source(w)->SetSelection)(Text_Source(w),1,0,ev->xkey.time);
}


static void
MoveToLineEnd(Widget w, XEvent *ev, String *params, Cardinal *num_params)
{
    XmTextPosition pos;


    if (*num_params==1 && !strcmp("extend", *params)) {
        DEBUGOUT(_LtDebug(__FILE__, w, "MoveToLineEnd(extend)"));
        _KeySelection(w, ev, 4, params, num_params);   
    } else
    {
        DEBUGOUT(_LtDebug(__FILE__, w, "MoveToLineEnd"));
        if (! Text_AddMode(w))
	DoUnselectionBecauseOfKeyNavigation(w,ev);
        pos = (*Text_Source(w)->Scan) (Text_Source(w), Text_CursorPos(w),
				   XmSELECT_LINE, XmsdRight, 1, False);
        _XmTextSetCursorPosition(w, pos);
     }
}

static void
MoveEndOfFile(Widget w, XEvent *ev, String *params, Cardinal *num_params)
{
    XmTextPosition pos;

    DEBUGOUT(_LtDebug(__FILE__, w, "MoveToEndOfFile"));

    if (! Text_AddMode(w))
	DoUnselectionBecauseOfKeyNavigation(w,ev);
    pos = Text_LastPos(w);

    _XmTextSetCursorPosition(w, pos);
}


static void
DoSelection(Widget w, XEvent *ev, String *params, Cardinal *num_params)
{
    if (ev->type != MotionNotify)
    {
	return;
    }

    DEBUGOUT(_LtDebug(__FILE__, w, "DoSelection"));
}

/*
 * ExtendEnd gets called at the end of a selection, e.g. after you've moved
 * the pointer while holding down MB1 (i.e. the left mouse button).
 * If, after that, MB1 is released, then this method is called.
 */
static void
ExtendEnd(Widget w, XEvent *ev, String *params, Cardinal *num_params)
{
    InputData i = Text_InputData(w);

    if (!i->extending)
    {
	return;
    }

    DEBUGOUT(_LtDebug(__FILE__, w, "ExtendEnd"));

    /*
     * Set a flag for ExtendSelection() to know that when it happens,
     * it must be a new selection.
     */
    i->extending = False;
}

/*
 * To continue the example above (comments above ExtendEnd), ExtendSelection
 * is called while moving the pointer while holding down MB1.
 */
static void
ExtendSelection(Widget w, XEvent *ev, String *params, Cardinal *num_params)
{
    XmTextPosition pos, left, right, cursor_pos;
    XmTextScanType st;
    InputData i = Text_InputData(w);
    Boolean sel;

    Text_InputData(w) -> select_pos_y = ev->xbutton.y ;
    Text_InputData(w) -> select_pos_x = ev->xbutton.x ;

    pos = (*Text_Output(w)->XYToPos) ((XmTextWidget)w,
				      ev->xbutton.x, ev->xbutton.y);

    sel = (*Text_Source(w)->GetSelection) (Text_Source(w), &left, &right);

    if (sel) {
	DEBUGOUT(_LtDebug(__FILE__, w,
		"ExtendSelection (pos %d, origleft %d) existing sel. %d-%d\n",
		pos, In_HighlightPivot(i), left, right));
    } else {
	DEBUGOUT(_LtDebug(__FILE__, w,
		"ExtendSelection (pos %d, origleft %d) (x %d y %d)\n",
		pos, In_HighlightPivot(i), ev->xbutton.x, ev->xbutton.y));
    }

    if (In_HighlightPivot(i) < 0) {
	In_HighlightPivot(i) = pos;
    }

    st = ScanType((XmTextWidget)w);

    if (pos >= In_HighlightPivot(i)) {
	if (st == XmSELECT_POSITION) {
		left = In_HighlightPivot(i);
		right = pos;
	} else {
		right = (*Text_Source(w)->Scan)(Text_Source(w), pos, st,
			XmsdRight, 1, False);
		left = In_HighlightPivot(i);
	}
	cursor_pos = right;
    } else {
	if (st == XmSELECT_POSITION) {
		left = pos;
		right = In_HighlightPivot(i);
	} else {
		left = (*Text_Source(w)->Scan)(Text_Source(w), pos, st,
			XmsdLeft, 1, False);
		right = (*Text_Source(w)->Scan)(Text_Source(w),
			In_HighlightPivot(i), st, XmsdRight, 1, False);
	}
	cursor_pos = left;
    }

    XmTextSetSelection(w,left,right,ev->xbutton.time);
    XmTextSetCursorPosition(w, cursor_pos);

    i->extending = True;
}

static void
FindWord(Widget w, XEvent *ev, String *params, Cardinal *num_params)
{
}

static void
MoveForwardChar(Widget w, XEvent *ev, String *params, Cardinal *num_params)
{
    DEBUGOUT(_LtDebug(__FILE__, w, "MoveForwardChar\n"));

    if (! Text_AddMode(w))
	DoUnselectionBecauseOfKeyNavigation(w,ev);
    if (Text_CursorPos(w) < Text_LastPos(w))
    {
        OutputData o = Text_OutputData((XmTextWidget)w);
        XmTextPosition pos;
        char *ptr;
        int     curr, i;

        ptr = Text_Source(w)->data->ptr;
        curr = Out_FontTextWidth(o, ptr, Text_CursorPos(w));
        pos = Text_CursorPos(w);
        for(i=Text_CursorPos(w)+1; i<=Text_LastPos(w); i++){
            if (curr < Out_FontTextWidth(o, ptr, i)){
                pos = i;
                break;
            }
        }

        _XmTextSetCursorPosition(w, pos);
    }
}

static void
MoveForwardParagraph(Widget w, XEvent *ev, String *params,
		     Cardinal *num_params)
{
    XmTextPosition pos;

    DEBUGOUT(_LtDebug(__FILE__, w, "MoveForwardParagraph"));

    if (! Text_AddMode(w))
	DoUnselectionBecauseOfKeyNavigation(w,ev);
    pos = (*Text_Source(w)->Scan) (Text_Source(w), Text_CursorPos(w),
				   XmSELECT_PARAGRAPH, XmsdRight, 1, False);

    pos = (*Text_Source(w)->Scan) (Text_Source(w), pos,
				   XmSELECT_WHITESPACE, XmsdRight, 1, False);

    _XmTextSetCursorPosition(w, pos);
}


static void
MoveForwardWord(Widget w, XEvent *ev, String *params,
		Cardinal *num_params)
{
    XmTextPosition pos;

    DEBUGOUT(_LtDebug(__FILE__, w, "MoveForwardWord"));

    if (! Text_AddMode(w))
	DoUnselectionBecauseOfKeyNavigation(w,ev);
    pos = (*Text_Source(w)->Scan) (Text_Source(w), Text_CursorPos(w),
				   XmSELECT_WORD, XmsdRight, 1, False);

    pos = (*Text_Source(w)->Scan) (Text_Source(w), pos,
				   XmSELECT_WHITESPACE, XmsdRight, 1, False);

    _XmTextSetCursorPosition(w, pos);
}

static void
DoGrabFocus(Widget w, XEvent *ev, String *params, Cardinal *num_params)
{
    InputData i = Text_InputData(w);
    XmTextPosition pos;

    DEBUGOUT(_LtDebug(__FILE__, w, "DoGrabFocus"));

    XmProcessTraversal((Widget)w, XmTRAVERSE_CURRENT);

    pos = (*Text_Output(w)->XYToPos) ((XmTextWidget)w,
				      ev->xbutton.x, ev->xbutton.y);

    if ((In_LastTime(i) + XtGetMultiClickTime(XtDisplay((Widget)w))) >
	ev->xbutton.time)
    {
	In_SelArrayIndex(i)++;
    }
    else
    {
	In_SelArrayIndex(i) = 0;
    }

    DoScanType((XmTextWidget)w, ev, pos);

    In_LastTime(i) = ev->xbutton.time;
}


static void
InsertString(Widget w, XEvent *ev, String *params, Cardinal *num_params)
{
    DEBUGOUT(_LtDebug(__FILE__, w, "InsertString\n"));
    if ( *num_params == 1 )
           XmTextInsert ( w, XmTextGetCursorPosition(w), params[0] );
}

static void
_KeySelection(Widget w, XEvent *ev, int flag, String *params, Cardinal *np)
{
    Boolean sel;
    XmTextPosition left, right, old;

    sel = (*Text_Source(w)->GetSelection) (Text_Source(w), &left, &right);

    if ( ! sel )
    {
	In_HighlightPivot( Text_InputData(w) ) = Text_CursorPos(w) ;
    }

    old = Text_CursorPos(w);

    switch (flag)
    {
    case 1:			/* left */
	MoveBackwardChar(w, ev, params, np);

	DEBUGOUT(_LtDebug(__FILE__, w,
			  "KeySelection old %d new %d left %d right %d\n",
			  old, Text_CursorPos(w), left, right));

	if (old == Text_CursorPos(w))
	{
	    break;
	}

	if (sel)
	{			/* Already have a selection, extend it */
	    if (old == left)
	    {
		left = Text_CursorPos(w);
	    }

	    if (old == right)
	    {
		right = Text_CursorPos(w);
	    }
	}
	else
	{			/* New selection */
	    left = Text_CursorPos(w);
	    right = old;
	}

	(*Text_Source(w)->SetSelection) (Text_Source(w),
				         left, right, ev->xkey.time);
	break;

    case 2:			/* right */
	MoveForwardChar(w, ev, params, np);

	DEBUGOUT(_LtDebug(__FILE__, w,
			  "KeySelection old %d new %d left %d right %d\n",
			  old, Text_CursorPos(w), left, right));

	if (old == Text_CursorPos(w))
	{
	    break;
	}

	if (sel)
	{			/* Already have a selection, extend it */
	    if (old == left)
	    {
		left = Text_CursorPos(w);
	    }

	    if (old == right)
	    {
		right = Text_CursorPos(w);
	    }
	}
	else
	{			/* New selection */
	    left = old;
	    right = Text_CursorPos(w);
	}
	(*Text_Source(w)->SetSelection) (Text_Source(w),
					 left, right, ev->xkey.time);
	break;

    case 3:			/* up */
	MovePreviousLine(w, ev, params, np);

	DEBUGOUT(_LtDebug(__FILE__, w,
			  "KeySelection old %d new %d left %d right %d\n",
			  old, Text_CursorPos(w), left, right));

	if (old == Text_CursorPos(w))
	{
	    break;
	}

	if (sel)
	{			/* Already have a selection, extend it */
	    if (old == left)
	    {
		left = Text_CursorPos(w);
	    }

	    if (old == right)
	    {
		right = Text_CursorPos(w);
	    }
	}
	else
	{			/* New selection */
	    left = Text_CursorPos(w);
	    right = old;
	}
	(*Text_Source(w)->SetSelection) (Text_Source(w),
					 left, right, ev->xkey.time);
	break;

    case 4:			/* down */
	MoveNextLine(w, ev, params, np);

	DEBUGOUT(_LtDebug(__FILE__, w,
			  "KeySelection old %d new %d left %d right %d\n",
			  old, Text_CursorPos(w), left, right));

	if (old == Text_CursorPos(w))
	{
	    break;
	}

	if (sel)
	{			/* Already have a selection, extend it */
	    if (old == left)
	    {
		left = Text_CursorPos(w);
	    }
	    if (old == right)
	    {
		right = Text_CursorPos(w);
	    }
	}
	else
	{			/* New selection */
	    left = old;
	    right = Text_CursorPos(w);
	}
	(*Text_Source(w)->SetSelection) (Text_Source(w),
					 left, right, ev->xkey.time);
	break;
    }
}

/*
 * KeySelection is the action routine for key-select.
 * Can either get called with no parameters,
 * or with "left", "right", "up", or "down". Any more ?
 */
static void
KeySelection(Widget w, XEvent *ev, String *params, Cardinal *num_params)
{
    int flag = 0;

    if (*num_params == 0)
    {
	DEBUGOUT(_LtDebug(__FILE__, w, "KeySelection()\n"));
    }
    else
    {
	DEBUGOUT(_LtDebug(__FILE__, w, "KeySelection(%s)\n", params[0]));
    }

    if (*num_params == 1)
    {
	if (strcmp(params[0], "left") == 0)
	{
	    flag = 1;
	}
	else if (strcmp(params[0], "right") == 0)
	{
	    flag = 2;
	}
#if 0
	else if (strcmp(params[0], "up") == 0)
	{
	    flag = 3;
	}
	else if (strcmp(params[0], "down") == 0)
	{
	    flag = 4;
	}
#endif
    }

    _KeySelection(w, ev, flag, params, num_params);
}

/*
 * ProcessShiftDown : extend the selection by another line
 */
static void
ProcessShiftDown(Widget w, XEvent *ev, String *params, Cardinal *num_params)
{
    DEBUGOUT(_LtDebug(__FILE__, w, "ProcessShiftDown\n"));

    _KeySelection(w, ev, 4, params, num_params);
}

static void
ProcessShiftUp(Widget w, XEvent *ev, String *params, Cardinal *num_params)
{
    DEBUGOUT(_LtDebug(__FILE__, w, "ProcessShiftUp\n"));

    _KeySelection(w, ev, 3, params, num_params);
}

static void
KillForwardChar(Widget w, XEvent *ev, String *params,
		Cardinal *num_params)
{
    DEBUGOUT(_LtDebug(__FILE__, w, "KillForwardChar\n"));

    DeleteForwardChar(w, ev, params, num_params);
}

static void
KillForwardWord(Widget w, XEvent *ev, String *params,
		Cardinal *num_params)
{
    DEBUGOUT(_LtDebug(__FILE__, w, "KillForwardWord\n"));

    DeleteForwardWord(w, ev, params, num_params);
}

static void
KillBackwardChar(Widget w, XEvent *ev, String *params,
		 Cardinal *num_params)
{
    DEBUGOUT(_LtDebug(__FILE__, w, "KillBackwardChar\n"));

    DeleteBackwardChar(w, ev, params, num_params);
}

static void
KillBackwardWord(Widget w, XEvent *ev, String *params,
		 Cardinal *num_params)
{
    DEBUGOUT(_LtDebug(__FILE__, w, "KillBackwardWord\n"));

    DeleteBackwardWord(w, ev, params, num_params);
}

static void
KillCurrentSelection(Widget w, XEvent *ev, String *params,
		     Cardinal *num_params)
{
    DEBUGOUT(_LtDebug(__FILE__, w, "KillCurrentSelection\n"));
}

static void
KillToEndOfLine(Widget w, XEvent *ev, String *params,
		Cardinal *num_params)
{
    DEBUGOUT(_LtDebug(__FILE__, w, "KillToEndOfLine\n"));

    DeleteToEndOfLine(w, ev, params, num_params);
}

static void
KillToStartOfLine(Widget w, XEvent *ev, String *params,
		  Cardinal *num_params)
{
    DEBUGOUT(_LtDebug(__FILE__, w, "KillToStartOfLine\n"));
    DeleteToStartOfLine(w, ev, params, num_params);
}

static void
MoveDestination(Widget w, XEvent *ev, String *params,
		Cardinal *num_params)
{
    DEBUGOUT(_LtDebug(__FILE__, w, "MoveDestination\n"));
}

static void
ProcessMove(Widget w, XEvent *ev, String *params,
	    Cardinal *num_params)
{
    DEBUGOUT(_LtDebug(__FILE__, w, "ProcessMove\n"));
}

static void
TraverseNextTabGroup(Widget w, XEvent *ev, String *params,
		     Cardinal *num_params)
{
    DEBUGOUT(_LtDebug(__FILE__, w, "TraverseNextTabGroup\n"));

    XtCallActionProc(w, "PrimitiveNextTabGroup", ev, params, *num_params);
}

static void
MoveNextLine(Widget aw, XEvent *ev, String *params, Cardinal *num_params)
{
    XmTextWidget w = (XmTextWidget)aw;
/* SG (15/08/1998) having these as unsigned gives erroneous results
   in the test index <= maxindex-2 when maxindex is 1
    unsigned int index, maxindex;
*/
    int index, maxindex;
    XmTextPosition pos = Text_CursorPos(w);

    DEBUGOUT(_LtDebug(__FILE__, aw, "MoveNextLine\n"));
    if (! Text_AddMode(w))
	DoUnselectionBecauseOfKeyNavigation(aw,ev);

    index = _XmTextGetTableIndex(w, pos);
    if (index == (maxindex = Text_TotalLines(w) - 1))
    {
	pos = Text_LastPos(w);
    }
    else
    {
	pos += Text_LineTable(w)[index + 1].start_pos -
	    Text_LineTable(w)[index].start_pos;

	if (index <= maxindex - 2
	    && pos >= Text_LineTable(w)[index + 2].start_pos)
	{
	    pos = Text_LineTable(w)[index + 2].start_pos - 1;
	}
	else if (pos > Text_LastPos(w))
	{
	    pos = Text_LastPos(w);
	}
    }

    if (pos != Text_CursorPos(w))
    {
	_XmTextSetCursorPosition((Widget)w, pos);
    }
}


static void
MoveNextPage(Widget aw, XEvent *ev, String *params, Cardinal *num_params)
{
    XmTextWidget w = (XmTextWidget)aw;
    int index, maxindex;
    XmTextPosition pos = Text_CursorPos(w);
#ifndef BUG45
    int delta = Out_Rows(Text_OutputData(w));
#else
    int delta = Text_LineCount(w) - 1;
#endif

    DEBUGOUT(_LtDebug(__FILE__, aw, "MoveNextPage"));
    if (! Text_AddMode(w))
	DoUnselectionBecauseOfKeyNavigation(aw,ev);

    index = _XmTextGetTableIndex(w, pos);
    maxindex = Text_TotalLines(w) - 1;

    if (index > maxindex - delta)
    {
	pos = Text_LastPos(w);
    }
    else
    {
	pos += Text_LineTable(w)[index + delta].start_pos -
	    Text_LineTable(w)[index].start_pos;

	if (index <= maxindex - delta - 1
	    && pos >= Text_LineTable(w)[index + delta + 1].start_pos)
	{
	    pos = Text_LineTable(w)[index + delta + 1].start_pos - 1;
	}
	else if (pos > Text_LastPos(w))
	{
	    pos = Text_LastPos(w);
	}
    }

    if (pos != Text_CursorPos(w))
    {
	_XmTextSetCursorPosition((Widget)w, pos);
    }
}

static void
InsertNewLine(Widget w, XEvent *ev, String *params, Cardinal *num_params)
{
    DEBUGOUT(_LtDebug(__FILE__, w, "InsertNewLine\n"));

    if (!Text_Editable(w))
    {
	return;			/* if we can't edit */
    }
    else if (Text_EditMode(w) == XmSINGLE_LINE_EDIT)
    {
	Activate(w, ev, params, num_params);
	return;
    }

    DoInsert((XmTextWidget)w, ev, "\n", 1);
}

static void
InsertNewLineAndBackup(Widget w, XEvent *ev, String *params, Cardinal *num_params)
{
    DEBUGOUT(_LtDebug(__FILE__, w, "InsertNewLineAndBackup\n"));

    if ((!Text_Editable(w)) || (Text_EditMode(w) == XmSINGLE_LINE_EDIT))
    {
	/* if we can't edit, or we are in single line only mode. */
	return;
    }

    DoInsert((XmTextWidget)w, ev, "\n", 1);
}

static void
InsertNewLineAndIndent(Widget w, XEvent *ev, String *params,
		       Cardinal *num_params)
{
    DEBUGOUT(_LtDebug(__FILE__, w, "InsertNewLineAndIndent\n"));

    if ((!Text_Editable(w)) || (Text_EditMode(w) == XmSINGLE_LINE_EDIT))
    {
	/* if we can't edit, or we are in single line only mode. */
	return;
    }
    else
    {
       XmTextBlockRec  block;
       XmTextPosition beginingOfLine, right;
       beginingOfLine = (*Text_Source(w)->Scan) (Text_Source(w),
                               Text_CursorPos(w), XmSELECT_LINE,  XmsdLeft, 1, False);
       right = (*Text_Source(w)->Scan) (Text_Source(w), beginingOfLine,
                               XmSELECT_WHITESPACE, XmsdRight,
                               Text_CursorPos(w) - beginingOfLine, /* so we do not select more
                                                                      whitespace than just this line */
                               False);

               /* passing "beginingOfLine - 1" should mean we also get the newline :-) */
       (*Text_Source(w)->ReadSource) (Text_Source(w), beginingOfLine - 1, right, &block);
       DoInsert((XmTextWidget)w, ev, block.ptr , block.length );
       XtFree(block.ptr);
    }
}

static void
MovePageLeft(Widget w, XEvent *ev, String *params, Cardinal *num_params)
{
    DEBUGOUT(_LtDebug(__FILE__, w, "MovePageLeft\n"));
    if (! Text_AddMode(w))
	DoUnselectionBecauseOfKeyNavigation(w,ev);
}

static void
MovePageRight(Widget w, XEvent *ev, String *params, Cardinal *num_params)
{
    DEBUGOUT(_LtDebug(__FILE__, w, "MovePageRight\n"));
    if (! Text_AddMode(w))
	DoUnselectionBecauseOfKeyNavigation(w,ev);
}

static void
PasteClipboard(Widget w, XEvent *ev, String *params, Cardinal *num_params)
{
    DEBUGOUT(_LtDebug(__FILE__, w, "PasteClipboard\n"));
    XmTextPaste(w);
}

static void
TraversePrevTabGroup(Widget w, XEvent *ev, String *params, Cardinal *num_params)
{
    DEBUGOUT(_LtDebug(__FILE__, w, "TraversePrevTabGroup\n"));

    XtCallActionProc(w, "PrimitivePrevTabGroup", ev, params, *num_params);
}


static void
MovePreviousLine(Widget aw, XEvent *ev, String *params, Cardinal *num_params)
{
    XmTextWidget w = (XmTextWidget)aw;
    unsigned int index;
    XmTextPosition pos = Text_CursorPos(w);

    DEBUGOUT(_LtDebug(__FILE__, aw, "MovePreviousLine\n"));

    if (! Text_AddMode(w))
	DoUnselectionBecauseOfKeyNavigation(aw,ev);
    index = _XmTextGetTableIndex(w, pos);
    if (index == 0)
    {
	pos = Text_FirstPos(w);
    }
    else
    {
	pos += Text_LineTable(w)[index - 1].start_pos -
	    Text_LineTable(w)[index].start_pos;

	if (pos >= Text_LineTable(w)[index].start_pos)
	{
	    pos = Text_LineTable(w)[index].start_pos - 1;
	}
    }
    if (pos != Text_CursorPos(w))
    {
	_XmTextSetCursorPosition((Widget)w, pos);
    }
}


static void
MovePreviousPage(Widget aw, XEvent *ev, String *params, Cardinal *num_params)
{
    XmTextWidget w = (XmTextWidget)aw;
    unsigned int index;
    XmTextPosition pos = Text_CursorPos(w);
#ifndef BUG45
    Cardinal delta = Out_Rows(Text_OutputData(w));
#else
    Cardinal delta = Text_LineCount(w) - 1;
#endif

    DEBUGOUT(_LtDebug(__FILE__, aw, "MovePreviousPage"));

    if (! Text_AddMode(w))
	DoUnselectionBecauseOfKeyNavigation(aw,ev);
    index = _XmTextGetTableIndex(w, pos);
    if (index < delta)
    {
	pos = Text_FirstPos(w);
    }
    else
    {
	pos += Text_LineTable(w)[index - delta].start_pos -
	    Text_LineTable(w)[index].start_pos;

	if (pos >= Text_LineTable(w)[index - delta + 1].start_pos)
	{
	    pos = Text_LineTable(w)[index - delta + 1].start_pos - 1;
	}
    }
    if (pos != Text_CursorPos(w))
    {
	_XmTextSetCursorPosition((Widget)w, pos);
    }
}

static void
ProcessBDrag(Widget w, XEvent *ev, String *params, Cardinal *num_params)
{
    Atom export_target[3];
    Arg args[10];
    int n = 0;
    Widget dc;
    XmTextPosition position;
    XmSourceData d = Text_Source(w)->data;

    _XmObjectLock(w);
    DEBUGOUT(_LtDebug(__FILE__, w, "ProcessBDrag\n"));

    /* If widget has no selection or cursor is not in selection,
       then return. */
    position = (*Text_Output(w)->XYToPos)((XmTextWidget)w, 
    			ev->xbutton.x, ev->xbutton.y);

    if (!d->hasselection ||
    	position < d->left ||
	position >= d->right)
    {

    	_XmObjectUnlock(w);
	return;
    }

    /* set up some stuff */
    export_target[0] = XmInternAtom(XtDisplay(w), _XA_COMPOUND_TEXT, False);
    export_target[1] = XmInternAtom(XtDisplay(w), _XA_TEXT, False);
    export_target[2] = XA_STRING;

    XtSetArg(args[n], XmNexportTargets, export_target); n++;
    XtSetArg(args[n], XmNnumExportTargets, 3); n++;
    XtSetArg(args[n], XmNdragOperations, XmDROP_COPY|XmDROP_MOVE); n++;
    XtSetArg(args[n], XmNclientData, w); n++;
    XtSetArg(args[n], XmNconvertProc, drag_convert_proc); n++;
    XtSetArg(args[n], XmNsourceCursorIcon, _XmGetTextualDragIcon(w)); n++;

    dc = XmDragStart(w, ev, args, n);

    if (dc)
    {
	XtAddCallback(dc, XmNdragDropFinishCallback, drag_drop_finish, NULL);
    }
    _XmObjectUnlock(w);
}

static void
drag_drop_finish(Widget w, XtPointer client_data, XtPointer call_data)
{
    DEBUGOUT(_LtDebug(__FILE__, w, "%s:drag_drop_finish(%d)\n",
    	__FILE__, __LINE__));
}

static Boolean
drag_convert_proc(Widget w, Atom *selection,
		  Atom *target, Atom *type_return,
		  XtPointer *value_return,
		  unsigned long *length_return,
		  int *format_return)
{
    XmSourceData d;
    XmTextPosition left, right;
    Boolean sel;
    Widget src;
    Atom atom_comp_text;
    Atom atom_text;
    Atom atom_motif_drop;
    Atom atom_delete;

    atom_comp_text =  XmInternAtom(XtDisplay(w), _XA_COMPOUND_TEXT, False);
    atom_text = XmInternAtom(XtDisplay(w), _XA_TEXT, False);
    atom_motif_drop =  XmInternAtom(XtDisplay(w), _XA_MOTIF_DROP, False);
    atom_delete =  XmInternAtom(XtDisplay(w), _XA_DELETE, False);

    if (*selection != atom_motif_drop)
    {
    	DEBUGOUT(_LtDebug(__FILE__, w, "selection is not MOTIF_DROP\n"));
	return False;
    }

    DEBUGOUT(_LtDebug(__FILE__, w, "We're dealing with MOTIF_DROP\n"));

    XtVaGetValues(w, XmNclientData, &src, NULL);
    d = Text_Source(src)->data;
    _XmObjectLock(src);
 
    if (*target == XA_STRING){
        *length_return = (long)(d->right - d->left);
        *value_return = XtMalloc(d->right - d->left + 1);
	strncpy((char *)*value_return, &d->ptr[d->left], d->right - d->left);
        *type_return = XA_STRING;
    }
    else if (*target == atom_comp_text || *target == atom_text){

	    XTextProperty prop;
	    char *buf;
	    int ret;

	    buf = XtMalloc(d->right - d->left + 1);
            strncpy(buf, &d->ptr[d->left], d->right - d->left);
	    buf[d->right - d->left] = '\0';

	    ret = XmbTextListToTextProperty(XtDisplay(src), &buf, 1,
	    			XCompoundTextStyle, &prop);
		
	    XtFree(buf);
	    if (ret != 0){
	    	*length_return = 0;
	    	*value_return  = NULL;
	    }
	    else{
	    	buf = XtMalloc(prop.nitems + 1);
		strncpy(buf, (char*)prop.value, prop.nitems);
		buf[prop.nitems] = '\0';
	    	*length_return = prop.nitems;
	    	*value_return  = buf;
	    }
	   *type_return = atom_comp_text;

    }
    else if (*target == atom_delete)
    {
    	sel =(*Text_Source((Widget)src)->GetSelection)(Text_Source((Widget)src),
		&left, &right);
	if (sel)
	{
            (*Text_Source((Widget)src)->SetSelection) (Text_Source((Widget)src),
		right, right, CurrentTime);
            _XmTextDelete((XmTextWidget)src, NULL, left, right);
	}

        *value_return = NULL;
        *type_return = XmInternAtom(XtDisplay(w), "NULL", False);
        *length_return = 0;
    }
    else
    {
        _XmObjectUnlock(src);
	return False;
    }

    *format_return = 8;

    _XmObjectUnlock(src);
    return True;
}

static void
drag_transfer_proc(Widget dw, XtPointer client, Atom *selection, Atom *type,
		 XtPointer value, unsigned long *length, int *format)
{
    XmTextWidget w = (XmTextWidget)client;
    Atom atom_comp_text;
    Atom atom_text;
    Atom atom_null;
    Arg args[2];

    atom_comp_text =  XmInternAtom(XtDisplay(w), _XA_COMPOUND_TEXT, False);
    atom_text = XmInternAtom(XtDisplay(w), _XA_TEXT, False);
    atom_null =  XmInternAtom(XtDisplay(w), "NULL", False);

    if (*type != atom_null && (!value || 
    	(*type != atom_comp_text && *type != atom_text && *type != XA_STRING)))
    {

    DEBUGOUT(_LtDebug(__FILE__, (Widget)w, "%s\n", 
	 "drag_transfer_proc: DnD Transfer Failed(Text)"));

        XtSetArg(args[0], XmNtransferStatus, XmTRANSFER_FAILURE);
        XtSetArg(args[1], XmNnumDropTransfers, 0);
        XtSetValues(dw, args, 2);

    }
    else if (*type != atom_null)
    {
	XTextProperty prop;
	int count, i;
	char **list;

	if (*type == XA_STRING){
	    char *buf;

	    buf = XtMalloc(*length + 1);
	    strncpy(buf, (char *)value, *length);
	    buf[*length] = '\0';

	    /* Convert from XA_STRING to COMPOUND_TEXT */
	    XmbTextListToTextProperty(XtDisplay(w), &buf, 1,
			XCompoundTextStyle, &prop);

	    XtFree(buf);
        }
        else{
            prop.value = (unsigned char *)value;
            prop.encoding = *type;
            prop.format = *format;
            prop.nitems = *length;
        }

        XmbTextPropertyToTextList(XtDisplay(w), &prop, &list, &count);

        for(i=0; i<count; i++){
    	    DoInsert(w, NULL, list[i], strlen(list[i]));
        }

    }

    if (value)
        XtFree((char *)value);
}


extern void
_Lttext_process_drop(Widget w, XtPointer client_data, XtPointer call_data)
{
    XmDropProcCallbackStruct *DropInfo =
        (XmDropProcCallbackStruct *) call_data;
    XmDropTransferEntryRec  entry[2];
    Atom atom_comp_text;
    Atom atom_text;
    Atom atom_null;
    Atom *export_targets;
    Atom target;
    Cardinal num_export_targets;
    Arg     args[10];
    Cardinal      n = 0;
    XmTextPosition pos;
    Boolean comp_text, text, string; 

    atom_comp_text =  XmInternAtom(XtDisplay(w), _XA_COMPOUND_TEXT, False);
    atom_text = XmInternAtom(XtDisplay(w), _XA_TEXT, False);
    atom_null = XmInternAtom(XtDisplay(w), "NULL", False);

    if ((DropInfo->dropAction == XmDROP) && 
        ((DropInfo->operation == XmDROP_COPY) ||
	 (DropInfo->operation == XmDROP_MOVE)))
    {
        /* Get valid target */
        XtSetArg(args[n], XmNexportTargets, &export_targets); n++;
        XtSetArg(args[n], XmNnumExportTargets, &num_export_targets); n++;
        XtGetValues(DropInfo->dragContext, args, n);
	comp_text = text = string = False;
        for (n = 0; n < num_export_targets; n++) {
            if (export_targets[n] == atom_comp_text)
                comp_text = True;
	    else if (export_targets[n] == atom_text)
                text = True;
	    else if (export_targets[n] == XA_STRING)
                string = True;
        }
	if (comp_text)
	    target = atom_comp_text;
	else if (text)
	    target = atom_text;
	else if (string)
	    target = XA_STRING;
	else
	    target = atom_null;

        entry[0].target = target;
        entry[0].client_data = (XtPointer) w;

	n = 0;
	if (target == atom_null)
	{
            XtSetArg(args [n], XmNtransferStatus, XmTRANSFER_FAILURE); n++;
            XtSetArg(args [n], XmNnumDropTransfers, 0); n++;
            DropInfo->operation = XmDROP_NOOP;
            DropInfo->dropSiteStatus = XmINVALID_DROP_SITE;
	}
	else if (DropInfo->operation == XmDROP_MOVE)
	{
            entry[1].target = XmInternAtom(XtDisplay(w), _XA_DELETE, False);
            entry[1].client_data = (XtPointer) w;
            XtSetArg(args [n], XmNnumDropTransfers, 2); n++;
            DropInfo->dropSiteStatus = XmINVALID_DROP_SITE;
	}
	else
	{
            XtSetArg(args [n], XmNnumDropTransfers, 1); n++;
            DropInfo->dropSiteStatus = XmINVALID_DROP_SITE;
	}
        XtSetArg(args [n], XmNdropTransfers, entry); n++;
        XtSetArg(args [n], XmNtransferProc, drag_transfer_proc); n++;

	/* We should move cursor at this point?. */
        pos = (*Text_Output(w)->XYToPos)((XmTextWidget)w,
		    			DropInfo->x, DropInfo->y);
	XmTextSetCursorPosition(w, pos);
    }
    else /* veto */
    {
        XtSetArg(args [n], XmNtransferStatus, XmTRANSFER_FAILURE); n++;
        XtSetArg(args [n], XmNnumDropTransfers, 0); n++;
        DropInfo->operation = XmDROP_NOOP;
        DropInfo->dropSiteStatus = XmINVALID_DROP_SITE;
    }

    XmDropTransferStart (DropInfo->dragContext, args, n);    

}


static void
ProcessDown(Widget w, XEvent *ev, String *params, Cardinal *num_params)
{
    DEBUGOUT(_LtDebug(__FILE__, w, "ProcessDown\n"));

    MoveNextLine(w, ev, params, num_params);
}


static void
ProcessHome(Widget w, XEvent *ev, String *params,
	    Cardinal *num_params)
{
    DEBUGOUT(_LtDebug(__FILE__, w, "ProcessHome\n"));

    MoveToLineStart(w, ev, params, num_params);
}


static void
ProcessReturn(Widget w, XEvent *ev, String *params, Cardinal *num_params)
{
    DEBUGOUT(_LtDebug(__FILE__, w, "ProcessReturn\n"));

    InsertNewLine(w, ev, params, num_params);
}


static void
ProcessTab(Widget w, XEvent *ev, String *params, Cardinal *num_params)
{
    DEBUGOUT(_LtDebug(__FILE__, w, "ProcessTab\n"));
    if (Text_EditMode(w) == XmSINGLE_LINE_EDIT)
    {
    	_XmMgrTraversal(w, XmTRAVERSE_NEXT_TAB_GROUP);
    }
    else
    {
    	DoInsert((XmTextWidget)w, ev, "\t", 1);
    }
}


static void
ProcessUp(Widget w, XEvent *ev, String *params, Cardinal *num_params)
{
    DEBUGOUT(_LtDebug(__FILE__, w, "ProcessUp\n"));

    MovePreviousLine(w, ev, params, num_params);
}


static void
RedrawDisplay(Widget w, XEvent *ev, String *params, Cardinal *num_params)
{
    xmTextClassRec.core_class.expose(w, NULL, (Region)NULL);
}


static void
ScrollOneLineUp(Widget w, XEvent *ev, String *params, Cardinal *num_params)
{
    DEBUGOUT(_LtDebug(__FILE__, w, "ScrollOneLineUp\n"));
    XmTextScroll(w, -1);
}


static void
SecondaryAdjust(Widget w, XEvent *ev, String *params, Cardinal *num_params)
{
    DEBUGOUT(_LtDebug(__FILE__, w, "SecondaryAdjust\n"));
}


static void
SecondaryNotify(Widget w, XEvent *ev, String *params, Cardinal *num_params)
{
    DEBUGOUT(_LtDebug(__FILE__, w, "SecondaryNotify\n"));
}


static void
SecondaryStart(Widget w, XEvent *ev, String *params, Cardinal *num_params)
{
    DEBUGOUT(_LtDebug(__FILE__, w, "SecondaryStart\n"));
}


static void
SelectAll(Widget w, XEvent *ev, String *params, Cardinal *num_params)
{
    DEBUGOUT(_LtDebug(__FILE__, w, "SelectAll\n"));
}


static void
StartExtendSelection(Widget w, XEvent *ev, String *params, Cardinal *num_params)
{
    DEBUGOUT(_LtDebug(__FILE__, w, "StartExtendSelection\n"));

    DoGrabFocus(w, ev, params, num_params);
}



static void
SelfInsert(Widget w, XEvent *ev, String *params, Cardinal *num_params)
{
#define INSERTCHARBUFSIZ 128
    char buf[INSERTCHARBUFSIZ], *buf2;
    KeySym keysym;
    int len, status;

    DEBUGOUT(_LtDebug(__FILE__, w, "SelfInsert"));

    len = XmImMbLookupString((Widget)w, (XKeyPressedEvent *)ev, buf, INSERTCHARBUFSIZ, &keysym, &status);

    /* Handle Oveflow case. (not evaluated)*/
    buf2 = XtMalloc(len+1);
    if (status == XBufferOverflow)
	XmImMbLookupString((Widget)w, (XKeyPressedEvent *)ev, buf2, len, &keysym, &status);
    else
	strncpy(buf2, buf, len);

    if (_LtDebugInDebug(__FILE__, w))
    {
	DEBUGOUT(_LtDebug(__FILE__, w, "XmImMbLookupString => %d\n", len));
	DEBUGOUT(_LtDebug0(__FILE__, w, "\tStatus %s\n",
			   (status == XLookupNone) ? "none" :
			   (status == XLookupChars) ? "chars" :
			   (status == XLookupBoth) ? "both" :
			   (status == XLookupKeySym) ? "keysym" :
			   (status == XBufferOverflow) ? "overflow" : "????"));

	if (status == XLookupBoth || status == XLookupKeySym)
	{
	    DEBUGOUT(_LtDebug0(__FILE__, w, "\tKeySym 0x%X\n", keysym));
	}

	if (len > 0)
	{
	    int i;

	    DEBUGOUT(_LtDebug0(__FILE__, w, "\tBuffer "));
	    for (i = 0; i < len; i++)
	    {
		DEBUGOUT(_LtDebug(__FILE__, w, " %X", 0xFF & buf[i]));
	    }
	    DEBUGOUT(_LtDebug0(__FILE__, w, "\n"));
	}
    }

    if (len > 0)
    {				/* FIX ME */
	if (status == XLookupBoth || status == XLookupChars)
	{
	    DoInsert((XmTextWidget)w, ev, buf2, len);
	}
    }
}

/* This is the insertion point for secondary selections */
static void
SetAnchor(Widget w, XEvent *ev, String *params, Cardinal *num_params)
{
    DEBUGOUT(_LtDebug(__FILE__, w, "SetAnchor\n"));

    Text_DestPosition(w) = (*Text_Output(w)->XYToPos)
	((XmTextWidget)w, ev->xbutton.x, ev->xbutton.y);
}

static void
SetCursorPosition(Widget w, XEvent *ev, String *params, Cardinal *num_params)
{
    DEBUGOUT(_LtDebug(__FILE__, w, "SetCursorPosition\n"));
}

static void
SetSelectionHint(Widget w, XEvent *ev, String *params, Cardinal *num_params)
{
    DEBUGOUT(_LtDebug(__FILE__, w, "SetSelectionHint\n"));
}

static void
ScrollCursorVertically(Widget w, XEvent *ev, String *params, Cardinal *num_params)
{
    DEBUGOUT(_LtDebug(__FILE__, w, "ScrollCursorVertically\n"));
}


static void
ScrollOneLineDown(Widget w, XEvent *ev, String *params, Cardinal *num_params)
{
    DEBUGOUT(_LtDebug(__FILE__, w, "ScrollOneLineDown\n"));
    XmTextScroll(w, 1);
}

static void
ToggleAddMode(Widget w, XEvent *ev, String *params, Cardinal *num_params)
{
    DEBUGOUT(_LtDebug(__FILE__, w, "ToggleAddMode\n"));
    Text_AddMode(w) = ! Text_AddMode(w);
}


static void
ToggleOverstrike(Widget w, XEvent *ev, String *params, Cardinal *num_params)
{
    DEBUGOUT(_LtDebug(__FILE__, w, "ToggleOverstrike\n"));
    Text_InputData(w) -> overstrike = ! Text_InputData(w) -> overstrike ;
    _XmCursorOverstrike(w);
}

static void
TextLeave(Widget w, XEvent *ev, String *params, Cardinal *num_params)
{
    DEBUGOUT(_LtDebug(__FILE__, w, "TextLeave\n"));
}

static void
TraverseHome(Widget w, XEvent *ev, String *params, Cardinal *num_params)
{
    DEBUGOUT(_LtDebug(__FILE__, w, "TraverseHome\n"));

    XtCallActionProc(w, "PrimitiveTraverseHome", ev, params, *num_params);
}

static void
TraverseDown(Widget w, XEvent *ev, String *params, Cardinal *num_params)
{
    DEBUGOUT(_LtDebug(__FILE__, w, "TraverseDown\n"));

    XtCallActionProc(w, "PrimitiveTraverseDown", ev, params, *num_params);
}

static void
TraverseUp(Widget w, XEvent *ev, String *params, Cardinal *num_params)
{
    DEBUGOUT(_LtDebug(__FILE__, w, "TraverseUp\n"));

    XtCallActionProc(w, "PrimitiveTraverseUp", ev, params, *num_params);
}

static void
TextFocusIn(Widget w, XEvent *ev, String *params, Cardinal *num_params)
{
#if EXTRA_FOCUS
    XmTextVerifyCallbackStruct cbs;
    OutputData o = Text_OutputData(w);
#endif

    DEBUGOUT(_LtDebug(__FILE__, w, "TextFocusIn\n"));

    if (Text_Editable(w))
    {
	XmImSetFocusValues(w, NULL, 0);
        _XmImSendSpot(w);
    }
#if EXTRA_FOCUS
    if (!Out_HasFocus(o))
    {
	if (Text_FocusCallback(w))
	{
	    cbs.reason = XmCR_FOCUS;
	    cbs.event = ev;
	    cbs.currInsert = cbs.newInsert = Text_CursorPos(w);
	    cbs.startPos = cbs.endPos = 0;
	    cbs.text = NULL;

	    XtCallCallbackList((Widget)w, Text_FocusCallback(w), &cbs);
	}

	Out_HasFocus(o) = True;
    }
#endif
    /*
     * 980827 - pgw@hungry.com: Fix for highlight defect.
     */
    XtCallActionProc((Widget)w, "PrimitiveFocusIn", ev,
			 params, *num_params);

}

static void
TextFocusOut(Widget w, XEvent *ev, String *params, Cardinal *num_params)
{
#if EXTRA_FOCUS
    XmTextVerifyCallbackStruct cbs;
    OutputData o = Text_OutputData(w);
#endif

    DEBUGOUT(_LtDebug(__FILE__, w, "TextFocusOut\n"));

    if (Text_Editable(w))
    {
	XmImUnsetFocus(w);
    }
#if EXTRA_FOCUS
    if (Out_HasFocus(o))
    {
	if (Text_LosingFocusCallback(w))
	{
	    cbs.reason = XmCR_LOSING_FOCUS;
	    cbs.event = ev;
	    cbs.currInsert = cbs.newInsert = Text_CursorPos(w);
	    cbs.startPos = cbs.endPos = 0;
	    cbs.text = NULL;

	    XtCallCallbackList((Widget)w, Text_LosingFocusCallback(w), &cbs);
	}

	Out_HasFocus(o) = False;
    }
#endif
    /*
     * 980827 - pgw@hungry.com: Fix for highlight defect.
     */
    XtCallActionProc((Widget)w, "PrimitiveFocusOut", ev,
			 params, *num_params);

}

static void
UnKill(Widget w, XEvent *ev, String *params, Cardinal *num_params)
{
    DEBUGOUT(_LtDebug(__FILE__, w, "UnKill\n"));
}

static void
VoidAction(Widget w, XEvent *ev, String *params, Cardinal *num_params)
{
    DEBUGOUT(_LtDebug(__FILE__, w, "VoidAction\n"));
}


/*
 * Quasi-Public functions -----------------------------------------------------
 */


Widget
_XmTextGetDropReciever(Widget w)
{
    /* FIX ME */
    return w;
}

Boolean
_XmTextHasDestination(Widget w)
{
    /* FIX ME */
    return False;
}

void
_XmTextInputGetSecResData(XmSecondaryResourceData *secResDataRtn)
{
}

XmTextPosition
_XmTextGetAnchor(XmTextWidget tw)
{
    InputData i = Text_InputData(tw);
    return In_HighlightPivot(i);
}

Boolean
_XmTextSetDestinationSelection(Widget w,
			       XmTextPosition position,
			       Boolean disown,
			       Time set_time)
{
    /* FIX ME */
    return False;
}

Boolean
_XmTextSetSel2(XmTextWidget tw,
	       XmTextPosition left,
	       XmTextPosition right,
	       Time set_time)
{
    /*CP: first attempt */
    InputData inp = Text_InputData(tw);

    if ( left < 0 ) 
		/*CP: it seems Motif does not do this check ! 
		|| right > Text_LastPos(tw) ) */
	return False;

    inp -> sel2Left = left;
    inp -> sel2Right = right;
    inp -> hasSel2 = True;
    inp -> sec_time = set_time;
    return True;
}


extern Boolean
_XmTextGetSel2(XmTextWidget tw,
	       XmTextPosition *left,
	       XmTextPosition *right)
{
    InputData inp = Text_InputData(tw);
    if ( ! inp -> hasSel2 )
	return False;

    *left = inp -> sel2Left ;
    *right = inp -> sel2Right ;
    return True;
}
