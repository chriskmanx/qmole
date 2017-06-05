/**
 *
 * $Header: /cvsroot/lesstif/lesstif/lib/Xm-2.1/TextStrSo.c,v 1.4 2006/04/19 18:42:22 dannybackx Exp $
 *
 * Copyright (C) 1995 Free Software Foundation, Inc.
 * Copyright © 1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2004, 2005 LessTif Development Team
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

static const char rcsid[] = "$Header: /cvsroot/lesstif/lesstif/lib/Xm-2.1/TextStrSo.c,v 1.4 2006/04/19 18:42:22 dannybackx Exp $";

/*
 * Empirical evidence (fooling around with the structures found in Motif
 * apps at run time) shows that the following variables reflect the
 * selection (i.e. the PRIMARY selection) :
 *      - hasselection (Boolean) is set when we have the selection
 *      - left, right (XmTextPosition) indicate the edges of the selection
 *      - prim_time (Time) is the time of the primary selection.
 *
 * The reader has by now understood that the selection is not an XmText but
 * an XmTextSource issue.
 */

#define	DO_SANITY

/* Defining this is a bit too verbose for my liking ... */
#undef VERBOSE__LtDebug

#include <LTconfig.h>

#include <string.h>
#include <stdlib.h>

#include <Xm/XmP.h>
#include <Xm/TextP.h>
#include <Xm/VendorSEP.h>
#include <Xm/AtomMgr.h>

#include <XmI/DebugUtil.h>

#define SOURCE_ALLOC_SIZE 256


static char *
StrPtr(XmSourceData d, XmTextPosition pos)
{
    char *ptr;

    ptr = d->ptr + pos;
    return ptr;
}


#if 0
static char *
StrNext(XmSourceData d, XmTextPosition pos)
{
    return d->ptr + pos + 1;
}


static char *
StrPrev(XmSourceData d, XmTextPosition pos)
{
    if (pos > 0)
    {
	return d->ptr + pos + 1;
    }
    else
    {
	return NULL;
    }
}
#endif

/* AddWidget ----------------------------------------------------------------
 *
 * Method that adds a widget to the source structure so that this new widget
 * will be updated as the text in the source changes.
 */
static void
AddWidget(XmTextSource source, XmTextWidget w)
{
    XmSourceData d;

    d = source->data;
    d->numwidgets++;
    d->widgets = (XmTextWidget *)XtRealloc((char *)d->widgets,
				       sizeof(XmTextWidget *) * d->numwidgets);

    d->widgets[d->numwidgets - 1] = w;

    if (d->maxallowed == 0)
    {				/* 1st time adding a widget! */
	d->maxallowed = Text_MaxLength(w);

	if (d->value == (char *)XmUNSPECIFIED || d->value == NULL)
	{
	    d->value = XtNewString("");
	    d->old_length = 0;
	}
	else
	{
	    d->value = XtNewString(d->value);
/*CP:wrong - the "old_length" is the lengh in chars of the longest line
	    d->old_length = (d->value) ? strlen(d->value) : 0;
*/
	}
	d->length = (d->value) ? strlen(d->value) : 0;
	d->maxlength = d->length + SOURCE_ALLOC_SIZE;
	d->ptr = XtMalloc(d->maxlength + 1);
	strcpy(d->ptr, d->value);

	d->gap_start = d->ptr;
	d->gap_end = d->ptr;
    }

    Text_LastPos(w) = d->length;
}

/* RemoveWidget -------------------------------------------------------------

 * Removes a widget from a source, but DOES NOT delete the source if removing
 * the last widget.
 *
 * Danny 4/5/97 - why not ??
 *      Added it for now.
 */
static void
RemoveWidget(XmTextSource source, XmTextWidget w)
{
    XmSourceData d;
    XmTextWidget *get, *store;
    int i, found = 0;

    d = source->data;
    get = store = d->widgets;
    for (i = 0; i < d->numwidgets; i++)
    {
	if (*get != w)
	{
	    *store = *get;
	    store++;
	}
	else
	{
	    found++;
	}

	get++;
    }
    d->numwidgets -= found;

    if (d->numwidgets == 0)
    {
	_XmStringSourceDestroy(source);
    }
}

/* CountLines ---------------------------------------------------------------

 * Counts the number of lines...  Slightly more tricky than at first glance,
 * but only slightly.  Stuff having to do with " \n" having 2 lines.  Think
 * about it.
 */
static int
CountLines(XmTextSource source, XmTextPosition start, unsigned long len)
{
    XmSourceData d;
    char *ptr;
    int count;

    d = source->data;
    ptr = StrPtr(d, start);

    count = 0;
    while (len-- > 0)
    {
	if (ptr[len] == '\n')
	{
	    count++;
	}
    }

    return count;
}

/* ReadSource ---------------------------------------------------------------

 * Copies the text in the source to a text block.
 */
static XmTextPosition
ReadSource(XmTextSource source,
	   XmTextPosition pos,
	   XmTextPosition last,
	   XmTextBlock block)
{
    XmSourceData d;
    int len;

    d = source->data;

    if (((unsigned)last) > ((unsigned)d->length))
    {
	last = d->length;
    }

    if (pos > last)
    {
	int swap;

	swap = pos;
	pos = last;
	last = swap;
    }

    len = last - pos;
    block->length = len;
    block->ptr = XtMalloc(len + 1);
    block->format = XmFMT_8_BIT;

    if (len > 0)
    {
	strncpy(block->ptr, &d->ptr[pos], len);
    }

    block->ptr[len] = '\0';

    if ( d->old_length < ( last - pos ) )
    	{
    	if ( ( pos == 0 || d->ptr[pos-1] == '\n' ) &&
    		( last == d->length || d->ptr[last] == '\n' ) && len > d->old_length )
	    d->old_length = len;
	}
    return last;
}

/* Replace ------------------------------------------------------------------

 * (And sundry functions)
 *
 * Replace text in the source with something else.
 */
static void
CheckSize(XmSourceData d, int len)
{
#if 0
	/* Increase size more drastically */
	if (d->length + len > d->maxlength) {
		if (d->maxlength < len || d->maxlength < 4096)
			d->maxlength = 8192;
		else
			d->maxlength *= 2;
		d->ptr = XtRealloc(d->ptr, d->maxlength);
	}
#else
	/* Increase size in smaller increments */
	if (d->length + len > d->maxlength) {
		int i;

		i = SOURCE_ALLOC_SIZE;
		if (i < len) {
			i = len;
		}

		d->maxlength += i + 1;
		d->ptr = XtRealloc(d->ptr, d->maxlength);

		DEBUGOUT(_LtDebug(__FILE__, NULL,
			"*source->CheckSize: Alloced new space\n"));
	}
#endif
}

static void
Insert(XmSourceData d, XmTextPosition start, char *ptr, int len)
{
    int i;

#ifdef	DO_SANITY
/* Sanity check */
    if (start < 0)
    {
	start = 0;
    }
#endif

    for (i = d->length - 1; i >= start; i--)
    {
	d->ptr[i + len] = d->ptr[i];
    }

    strncpy(&d->ptr[start], ptr, len);
    d->length += len;
}

/*
 * This function should also take care of highlighting and selections.
 *
 * NOTE: test/Xm/text/test3 shows that if a widget is modified, callbacks
 *      are called only for the widget involved, NOT for all widgets displaying
 *      that particular source.
 *      Therefore, we only work on one widget in part of the implementation.
 *
 * NOTE : Do NOT check for d->editable (in order not to do the editing).
 *      At this low level this function gets called for ALL editing, not just
 *      the user-inflicted ones. Therefore edits must happen.
 */
static XmTextStatus
Replace(XmTextWidget w,
	XEvent *ev,
	XmTextPosition *startret,
	XmTextPosition *endret,
	XmTextBlock block,
	Boolean call_callback)
{
    XmSourceData d;
    int i;
    XmTextPosition start, end, lastPos = 0;
    XmTextVerifyCallbackStruct cbs;

	DEBUGOUT(_LtDebug(__FILE__, (Widget)w,
		"XmTextStrSrc-Replace(start %d, end %d, call %d)\n",
		*startret, *endret, call_callback));

    if (!call_callback || _XmTextModifyVerify(w, ev, startret, endret, &Text_CursorPos(w), block, NULL, False))
    {
	d = Text_Source(w)->data;

	start = *startret;
	end = *endret;

	if (start > end)
	{
	    int swap;

	    swap = start;
	    start = end;
	    end = swap;
	}

	if (end > d->length)
	{
	    end = d->length;
	}

	if (start > d->length)
	{
	    return EditError;
	}

	if ( ( start >= end ) &&
		   d->length + (end - start) + block->length > d->maxallowed)
	{
	    return EditError;
	}

	for (i = 0; i < d->numwidgets; i++)
	{
	    _XmTextDisableRedisplay(d->widgets[i], True);
	}

	if (end > start)
	{				/* we need to delete some stuff */
	    char *dest, *source, *last;

	    dest = d->ptr + start;
	    source = d->ptr + end;
	    last = d->ptr + d->length;
	    while (source < last)
	    {
		*dest++ = *source++;
	    }

	    d->length -= end - start;
	}

	if (block && block->length > 0)
	{
	    CheckSize(d, block->length);
	    Insert(d, start, block->ptr, block->length);
	}

	for (i = 0; i < d->numwidgets; i++)
	{
	    XmTextPosition startPos = (*Text_Source(w)->Scan)(Text_Source(w), 0, XmSELECT_ALL, XmsdLeft, 1, False ) ;
	    lastPos = (*Text_Source(w)->Scan)(Text_Source(w), startPos, XmSELECT_ALL, XmsdRight, 1, False ) ;
	}

       if ( d -> hasselection )
	    {
	    /* rws 15 Jan 2000
	       What if only part of the selection is being replaced???
	    if ( start == d->left && end == d-> right )
	    */
	    if ( start <= d->right && end >= d->left )
		{
		    /* CP:15 May 1999: If the text we are replacing is the
		    ** selected text then unselect it. */
		d->hasselection = False;
		for (i = 0; i < d->numwidgets; i++)
		  {
#if 0
		    _XmHighlightRecNode* p;
		    p = Text_Highlight(d->widgets[i]).list;
		    while (p) {
		      _XmHighlightRecNode* k=p;
		      p = p->next;
		      XtFree((XtPointer)k);
		    }
		    Text_Highlight(d->widgets[i]).list = NULL;
#else
		    Text_Highlight(d->widgets[i]).number = 0;
#endif
		  }
		}
	    /*CP:15 May 1999:
		    at this point we do not want to overwrite the selection
		    but just to move its coordinates if we are inserting
		    text before the selection. */

	    else if ( start <  d-> left )
		    {
		    d-> left += block -> length;
		    d-> right += block -> length;
		    for (i = 0; i < d->numwidgets; i++)
			    {
			    XmTextSetHighlight((Widget)d->widgets[i],
				    d-> left, d->right, XmHIGHLIGHT_SELECTED);
			    }
		    }
	    }
	/* End selection */

	if (Text_CursorPos(w) == Text_LastPos(w))
	{
	    if (call_callback)
	    {
		Text_LastPos(w) = lastPos;
	    }
	    if (lastPos != Text_CursorPos(w))
	    {
		_XmTextSetCursorPosition((Widget)w, lastPos);
	    }
	}
	Text_LastPos(w) = lastPos;
	if (Text_CursorPos(w) > Text_LastPos(w))
	{
	    Text_CursorPos(w) = Text_LastPos(w);
	}
	if (Text_ValueChangedCallback(w))
	{
	    cbs.reason = XmCR_VALUE_CHANGED;
	    cbs.currInsert = cbs.newInsert = start;
	    cbs.startPos = cbs.endPos = start;
	    cbs.text = block;

	    XtCallCallbacks((Widget)w, XmNvalueChangedCallback, &cbs);
	}

	for (i = 0; i < d->numwidgets; i++) {
		_XmTextUpdateLineTable((Widget)d->widgets[i], start, end, block, True);
	}

	for (i = 0; i < d->numwidgets; i++) {
		_XmTextInvalidate(d->widgets[i], start, end, block->length - end + start);
	}

#if 1
	/* Update the insertion position */
	for (i = 0; i < d->numwidgets; i++)
	{
	    XmTextPosition	p = Text_CursorPos(d->widgets[i]);

	    if (p < *startret) {
		    /* no action */
	    } else if (p < *endret) {
		    _XmTextSetCursorPosition((Widget)d->widgets[i], *startret);
	    } else {
		    _XmTextSetCursorPosition((Widget)d->widgets[i], p + block->length);
	    }
	}
#endif

	for (i = 0; i < d->numwidgets; i++)
	{
	    _XmTextEnableRedisplay(d->widgets[i]);
	}
    }

    return EditDone;
}

/* Scan ---------------------------------------------------------------------

 * Search the source for something: (either forwards or backwards)
 *
 * XmSELECT_POSITION
 * XmSELECT_WHITESPACE
 * XmSELECT_WORD
 * XmSELECT_LINE
 * XmSELECT_ALL
 * XmSELECT_PARAGRAPH
 */
#define InWhiteSpace(a) (a==' ' || a=='\t' || a=='\n')
#define InWord(a) (!InWhiteSpace(a))

static XmTextPosition
Scan(XmTextSource source, XmTextPosition pos, XmTextScanType type,
     XmTextScanDirection dir, int count, Boolean inc)
{
    XmSourceData d = source->data;
    Boolean found = False;
    char *ptr;
#ifdef VERBOSE__LtDebug
    XmTextPosition ipos = pos;
#endif

    if (pos > d->length)
    {
#if 0
	/* This is reported to be a bad idea. */
	pos = d->length;
#endif
	return(pos);
    }

    if (count < 0)
    {
	count = d->length;
    }

    /*
     * This may not be completely right.
     *      If you start in whitespace, move towards non-whitespace before
     *      doing anything else.
     *      Currently, do this for XmSELECT_WORD.
     *      It may be appropriate to do this for other selections as well.
     * Danny 4/6/97
     */
    if (dir == XmsdLeft && type == XmSELECT_WORD)
    {
	ptr = StrPtr(d, pos - 1);
	while (count > 0 && pos > 0 && InWhiteSpace(*ptr))
	{
	    pos--;
	    ptr = StrPtr(d, pos - 1);
	}
    }
    else if (dir == XmsdRight && type == XmSELECT_WORD)
    {
	ptr = StrPtr(d, pos);
	while (count > 0 && pos < d->length && InWhiteSpace(*ptr))
	{
	    pos++;
	    ptr = StrPtr(d, pos);
	}
    }

    /* End initial whitespace treatment */

    if (dir == XmsdLeft)
    {
	while (count > 0 && pos > 0)
	{
	    ptr = StrPtr(d, pos - 1);
	    switch (type)
	    {
	    case XmSELECT_WHITESPACE:
		if (!InWhiteSpace(*ptr))
		{
		    found = True;
		}
		break;

	    case XmSELECT_WORD:
		if (!InWord(*ptr))
		{
		    found = True;
		}
		break;

	    case XmSELECT_LINE:
		if (*ptr == '\n')
		{
		    found = True;
		}
		break;

	    case XmSELECT_PARAGRAPH:
		if (*ptr == '\n')
		{
		    found = True;
		}
		break;

	    case XmSELECT_ALL:
		/*CP: probably a bit too simple */
		found = True;
		pos = 0;
		break;

	    default:
		found = True;
		break;
	    }

	    if (found)
	    {
#ifdef VERBOSE__LtDebug
		DEBUGOUT(_LtDebug(__FILE__, (Widget)d->widgets[0],
				  "Scan(%d)=>%d\n", ipos, pos));
#endif
		count--;
		if ( count == 0 )
		    return ( inc ? pos - 1 : pos );
		found = False;    
	    }

	    pos--;
	}
    }
    else
    {				/* dir == XmsdRight */
	while (count > 0 && pos < d->length)
	{
	    ptr = StrPtr(d, pos);
	    switch (type)
	    {
	    case XmSELECT_WHITESPACE:
		if (!InWhiteSpace(*ptr))
		{
		    found = True;
		}
		break;

	    case XmSELECT_WORD:
		if (!InWord(*ptr))
		{
		    found = True;
		}
		break;

	    case XmSELECT_LINE:
		if (*ptr == '\n')
		{
		    found = True;
		}
		break;

	    case XmSELECT_PARAGRAPH:
		if (*ptr == '\n')
		{
		    found = True;
		}
		break;

	    case XmSELECT_ALL:
		if (! *ptr )
		{
		    found = True;
		}
		break;

	    default:
		found = True;
		break;
	    }

	    if (found)
	    {
#ifdef VERBOSE__LtDebug
		DEBUGOUT(_LtDebug(__FILE__, (Widget)d->widgets[0],
				  "Scan(%d)=>%d\n", ipos, pos));
#endif
		count--;
		if ( count == 0 )
		    return ( inc ? pos+1 : pos );
		found = False;    
		if ( type == XmSELECT_WORD )
		    while ( InWhiteSpace(*ptr ) )
			{
			pos++;
			ptr = StrPtr(d, pos);
			}
	    }

	    pos++;
	}
    }

#ifdef VERBOSE__LtDebug
    DEBUGOUT(_LtDebug(__FILE__, (Widget)d->widgets[0],
		      "Scan(%d)=>%d\n", ipos, pos));
#endif
    if ( dir == XmsdRight && pos != d->length )
	return ( inc ? pos+1 : pos );
    if ( dir == XmsdLeft && pos != 0 )
	return ( inc ? pos - 1 : pos );
    return pos;
}


static Boolean
GetSelection(XmTextSource source, XmTextPosition *left,
	     XmTextPosition *right)
{
    *left = source->data->left;
    *right = source->data->right;
    if (source->data->hasselection)
    {
	/* rws 2 May 1998
	   In ml the Insert Text button in the Reply window calls
	   XmTextGetSelectionPosition and ignores the return value.  Instead
	   if compares the value of left and right to determine if anything
	   is selected.  Therefore always setting left and right makes it 
	   happy.  If we do not have the selection left == right anyway, so
	   everyone should be happy.
	*left = source->data->left;
	*right = source->data->right;
	*/
	return True;
    }

    return False;
}

/*
 * This gets called from Xt to get a copy of whatever's selected in our text
 * widget. We're delivering it all in one go; according to Asente & Swick
 * Xt will break it up for us if the data delivered is too big.
 *
 * FIX ME is it wise to do this for e.g. a 1MB selection ?
 */
static Boolean
_XmTextConvertSelection(Widget w, Atom *selection, Atom *target, Atom *type,
			XtPointer *value, unsigned long *length, int *format)
{
    XmSourceData d = Text_Source(w)->data;

    DEBUGOUT(_LtDebug(__FILE__, w, "_XmTextConvertSelection\n"));

    if (d->hasselection)
    {
#if 0
	/* rws 12 Dec 1997
	   I think we need to make a copy of the value here. In ml if you
	   select part of a message and try to paste it into another window
	   you get a segfault in free(). It would appear that Xt is free'ing
	   the contents of value.
	 */
	*value = &d->ptr[d->left];
#endif
	Atom atom_comp_text;
	Atom atom_text;

	atom_comp_text =  XmInternAtom(XtDisplay(w), "COMPOUND_TEXT", False);
	atom_text =  XmInternAtom(XtDisplay(w), "TEXT", False);
	if (*target == XA_STRING){
	    *length = (long)(d->right - d->left);
	    *value = XtMalloc(d->right - d->left + 1);
	    strncpy((char *)*value, &d->ptr[d->left], d->right - d->left);
	    *type = XA_STRING;
	}
	else if (*target == atom_comp_text || *target == atom_text){

	    XTextProperty prop;
	    char *buf;
	    int ret;

	    buf = XtMalloc(d->right - d->left + 1);
	    strncpy(buf, &d->ptr[d->left], d->right - d->left);
	    buf[d->right - d->left] = '\0';

	    ret = XmbTextListToTextProperty(XtDisplay(w), &buf, 1,
	    			XCompoundTextStyle, &prop);

	    XtFree(buf);
	    if (ret != 0){
	    	*length = 0;
	    	*value  = NULL;
	    }
	    else{
	    	buf = XtMalloc(prop.nitems + 1);
		strncpy(buf, (char*)prop.value, prop.nitems);
		buf[prop.nitems] = '\0';
	    	*length = prop.nitems;
	    	*value  = buf;
	    }
	    *type = atom_comp_text;

	}
	else
	    return False;

	*format = 8;
	return True;
    }

    return False;
}

/*
 * Another widget has taken the selection that we used to own.
 */
static void
_XmTextLoseSelection(Widget w, Atom *selection)
{
    XmAnyCallbackStruct	cbs;
    XmTextWidget	tw = (XmTextWidget)w;

    if (*selection == XA_PRIMARY)
    {
	DEBUGOUT(_LtDebug(__FILE__, w, "_XmTextLoseSelection(PRIMARY)\n"));

	XmTextSetHighlight(w, 0, XmTextGetLastPosition(w),
			   XmHIGHLIGHT_NORMAL);

	/*
	 * FIX ME
	 * Need to call losePrimaryCallback here ?
	 */
	cbs.reason = XmCR_LOSE_PRIMARY;
	cbs.event = NULL;	/* ??? */
	XtCallCallbackList(w,
                           tw->text.lose_primary_callback,
                           (XtPointer)&cbs);
    }
    else
    {
	DEBUGOUT(_LtDebug(__FILE__, w, "_XmTextLoseSelection(?)\n"));
	/* ??? */
    }
}

/*
 * SetSelection : indicate that we have the selection.
 *
 * Actions :
 *      - indicate it in the source
 *      - deal with Xt Selections (XtOwnSelection etc.)
 *      - call callback(s) (LoseSelection,GainSelection)
 *      - tell the widgets that display us about this (highlight)
 */
static void
SetSelection(XmTextSource source, XmTextPosition left,
	     XmTextPosition right, Time time)
{
    XmSourceData d = source->data;
    int i;
    Boolean gain = False;

    DEBUGOUT(_LtDebug(__FILE__, NULL, "XmTextStrSource SetSelection %d %d\n",
		      left, right));

    if (left >= right)
    {				/* No decent selection */
        /* SG 23/08/1998, just bring into line with changes in TextF.c */
        if(!source->data->hasselection)
        {
            return; /* Already no selection, don't do unnecessary work */
        }
	source->data->hasselection = False;
	source->data->left = -1;
	source->data->right = -1;
	source->data->prim_time = time;

	XtDisownSelection((Widget)d->widgets[0], XA_PRIMARY, time);

	for (i = 0; i < d->numwidgets; i++)
	{
	    XmTextSetHighlight((Widget)d->widgets[i],
			       0, XmTextGetLastPosition((Widget)d->widgets[0]),
			       XmHIGHLIGHT_NORMAL);
	}

	return;
    }

    /* Remember whether we used to have the selection */
    gain = (source->data->hasselection == False);

    /* We have a decent selection; indicate in memory */
    source->data->hasselection = True;
    source->data->left = left;
    source->data->right = right;
    source->data->prim_time = time;

    /* Xt, callbacks */
    DEBUGOUT(_LtDebug(__FILE__, (Widget)d->widgets[0],
		      "XtOwnSelection(_, XA_PRIMARY, ...)\n"));

    if (!XtOwnSelection((Widget)d->widgets[0], XA_PRIMARY, time,
			_XmTextConvertSelection, _XmTextLoseSelection, NULL))
    {
	gain = False;
    }

    if (gain)
    {
	XmAnyCallbackStruct cbs;

	cbs.reason = XmCR_GAIN_PRIMARY;
	cbs.event = NULL;	/* Have no information nested this deep */

	/* FIX ME : Only for one widget ? */
	XtCallCallbackList((Widget)source->data->widgets[0],
			   source->data->widgets[0]->text.gain_primary_callback,
			   (XtPointer)&cbs);

    }

    /* Widgets */
    for (i = 0; i < d->numwidgets; i++) {
	/* Unset the highlight over the complete widget */
	XmTextSetHighlight((Widget)d->widgets[i],
		0, XmTextGetLastPosition((Widget)d->widgets[0]),
		XmHIGHLIGHT_NORMAL);
	/* Highlight the selected area */
	XmTextSetHighlight((Widget)d->widgets[i], left, right,
		XmHIGHLIGHT_SELECTED);
    }
}

static XmTextSourceRec sourceRec =
{
    /* _XmSourceDataRec      */ NULL,
    /* AddWidgetProc         */ AddWidget,
    /* CountLinesProc        */ CountLines,
    /* RemoveWidgetProc      */ RemoveWidget,
    /* ReadProc              */ ReadSource,
    /* ReplaceProc           */ Replace,
    /* ScanProc              */ Scan,
    /* GetSelectionProc      */ GetSelection,
    /* SetSelectionProc      */ SetSelection
};


extern XmTextSource
_XmStringSourceCreate(char *value, Boolean is_wchar)
{
    XmTextSource source;
    XmSourceData d;


    source = (XmTextSource)XtMalloc(sizeof(XmTextSourceRec));
    memcpy(source, &sourceRec, sizeof(XmTextSourceRec));

    d = (XmSourceData)XtMalloc(sizeof(XmSourceDataRec));
    d->source = source;
    d->widgets = NULL;
    d->numwidgets = 0;
    d->value = value;
    d->ptr = d->gap_start = d->gap_end = NULL;
    d->length = d->maxlength = d->maxallowed = 0;
    d->old_length = 1;
    d->right = d->left = 0;
    d->hasselection = False;
    d->prim_time = 0;
	/*CP: How do I set the character used for new lines ?? : FIXME */
    d->PSWC_NWLN= XtNewString("\n");
    source->data = d;

    return source;
}



/*
 * Quasi-Public functions -----------------------------------------------------
 */


extern void
_XmStringSourceDestroy(XmTextSource source)
{
    XmSourceData d = source->data;

    /* rws 1 Apr 1999
       Even if these are == 0, we have still allocates space with XtNewString(0),
       and therefore it must be freed.
     */
    /*
    if (d->old_length > 0)
    */
    {
	XtFree(d->value);
    }
    /*
    if (d->length > 0)
    */
    {
	XtFree(d->ptr);
    }
	/* tills 12 apr 1999 free the widget list also (memleak) */
	XtFree((char*)d->widgets);
    XtFree(d->PSWC_NWLN);
    XtFree((char *)d);
    XtFree((char *)source);
}


extern char *
_XmStringSourceGetValue(XmTextSource source, Boolean want_wchar)
{
    XmSourceData d = source->data;
    char *txt;

    if (want_wchar)
    {
	DEBUGOUT(_LtDebug(__FILE__, NULL,
	   "_XmStringSourceGetValue: wide characters not implemented yet.\n"));

	/* Returning NULL seems to do strange things */
	txt = XtMalloc(1);
	txt[0] = '\0';
	return txt;
    }

    txt = XtMalloc(d->length + 1);

    strncpy(txt, d->ptr, d->length);
    txt[d->length] = '\0';

    DEBUGOUT(_LtDebug(__FILE__, NULL,
		      "_XmStringSourceGetValue => %p '%s'\n", txt, txt));

    return txt;
}


extern void
_XmStringSourceSetValue(XmTextWidget w, char *value)
{
    XmTextVerifyCallbackStruct cbs;
    XmTextBlockRec tb;
    int len;

    (Text_Source(w)->SetSelection)(Text_Source(w),1,0,
		XtLastTimestampProcessed ( XtDisplay ( w ) ));

    len = value ? strlen(value) : 0;

    cbs.reason = XmCR_MODIFYING_TEXT_VALUE;
    cbs.doit = True;
    cbs.event = NULL;
    cbs.startPos = 0;
    cbs.endPos = Text_LastPos(w);
    cbs.currInsert = Text_CursorPos(w);
    cbs.newInsert = Text_CursorPos(w);

    if (value)
    {
        tb.ptr = XtMalloc(len + 1);	/* NULL-terminate */
        tb.length = len;
        tb.format = XmFMT_8_BIT;
        strncpy(tb.ptr, value, len);
        tb.ptr[len] = '\0';             /* NULL-terminate */
    }
    else
    {
	/* Motif sets ptr to NULL for delete, not to NULL string */
        tb.ptr = NULL;
        tb.length = 0;
        tb.format = XmFMT_8_BIT;
    }
    cbs.text = &tb;
    if (Text_ModifyVerifyCallback(w))
    {
        XtCallCallbacks((Widget)w, XmNmodifyVerifyCallback, &cbs);
    }
#if 0
    /* FIXME ? */
    /* SG 23/08/1998 this is wrong as the callback structure is NOT
       the same for wide characters, given that the rest of wide character
       support is missing - I think it preferable to skip this for now.
     */
    if (TextF_WcsModifyVerifyCallback(w))
    {
	XtCallCallbacks((Widget)w, XmNmodifyVerifyCallbackWcs, &cbs);
    }
#endif
    if (cbs.doit)
    {
	XmTextStatus status ;
	Text_BottomPos(w) = 0; /*CP: probably not the right value or place for initialization !*/
	status = (Text_Source(w)->Replace)(w, NULL, &cbs.startPos, &cbs.endPos, &tb, False);
        /* rws 2 Apr 2000
           Done in Replace
        if ( (status == EditDone) && ( Text_ValueChangedCallback(w)) )
        {
            XmAnyCallbackStruct any;
            any.reason = XmCR_VALUE_CHANGED;
            any.event = NULL;

            XtCallCallbacks((Widget)w, XmNvalueChangedCallback, &any );
        }
        */
    }
#if 0
    else {
	VerifyBell(w);
    }
#endif
    if (tb.ptr)
    {
	XtFree(tb.ptr);
    }
}


extern Boolean
_XmStringSourceHasSelection(XmTextSource source)
{
    return source->data->hasselection;
}


extern Boolean
_XmStringSourceGetEditable(XmTextSource source)
{
    return source->data->editable;
}


extern void
_XmStringSourceSetEditable(XmTextSource source, Boolean editable)
{
    XmSourceData d;

    d = source->data;
    d->editable = editable;
}


extern int
_XmStringSourceGetMaxLength(XmTextSource source)
{
    return source->data->maxlength;
}


extern void
_XmStringSourceSetMaxLength(XmTextSource source, int max)
{
    XmSourceData d;

    d = source->data;
    if (max > d->length)
    {
	XmTextBlockRec block;

	block.ptr = "";
	block.length = 0;
	block.format = XmFMT_8_BIT;

	/* FIX ME Why is the following line commented out ? */
/*     (*source->Replace)(source, NULL, NULL, max, d->length, &block, True); */
    }

    d->maxallowed = max;
}


extern char *
_XmStringSourceGetString(XmTextWidget w,
			 XmTextPosition from,
			 XmTextPosition to,
			 Boolean want_wchar)
{
#if 1
    _XmWarning((Widget)w, "_XmStringSourceGetString is not implemented yet.");

    return NULL;
#else
    if (want_wchar)
    {
	DEBUGOUT(_LtDebug(__FILE__, NULL,
	  "_XmStringSourceGetString: wide characters not implemented yet.\n"));

	return NULL;
    }
    return NULL;		/* FIX ME */
#endif
}


extern Boolean
_XmTextFindStringBackwards(Widget w,
			   XmTextPosition start,
			   char *search_string,
			   XmTextPosition *position)
{
    _XmWarning(w, "_XmTextFindStringBackwards is not implemented yet.");

    return False;		/* FIX ME */
}


extern Boolean
_XmTextFindStringForwards(Widget w,
			  XmTextPosition start,
			  char *search_string,
			  XmTextPosition *position)
{
    return False;		/* FIX ME */
}


extern Boolean
_XmStringSourceFindString(Widget w,
			  XmTextPosition start,
			  char *string,
			  XmTextPosition *position)
{
    _XmWarning(w, "_XmStringSourceFindString is not implemented yet.");

    return False;		/* FIX ME */
}


extern void
_XmStringSourceSetGappedBuffer(XmSourceData data, XmTextPosition position)
{
    _XmWarning(NULL, "_XmStringSourceSetGappedBuffer is not implemented yet.");

    /* FIX ME */
}

Boolean
_XmTextModifyVerify(XmTextWidget initiator,
		    XEvent *event,
		    XmTextPosition *start,
		    XmTextPosition *end,
		    XmTextPosition *cursorPos,
		    XmTextBlock block,
		    XmTextBlock newblock,
		    Boolean *freeBlock)
{
XmTextVerifyCallbackStruct cbs;

    cbs.reason = XmCR_MODIFYING_TEXT_VALUE;
    cbs.event = event;
    cbs.doit = True;
    cbs.currInsert = *cursorPos;
    cbs.newInsert = *cursorPos;
    cbs.startPos = *start;
    cbs.endPos = *end;
    cbs.text = block;
    if (Text_ModifyVerifyCallback(initiator))
    {
        XtCallCallbacks((Widget)initiator, XmNmodifyVerifyCallback, &cbs);
    }
    return(cbs.doit);
}
