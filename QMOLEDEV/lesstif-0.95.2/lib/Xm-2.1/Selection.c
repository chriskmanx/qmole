/**
 *
 * $Id: Selection.c,v 1.1 2004/08/28 19:22:45 dannybackx Exp $
 *
 * Copyright (C) 1995 Free Software Foundation, Inc.
 * Copyright (C) 1995-2001 LessTif Development Team
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

static const char rcsid[] = "$Id: Selection.c,v 1.1 2004/08/28 19:22:45 dannybackx Exp $";

#include <LTconfig.h>

#include <string.h>

#include <XmI/XmI.h>
#include <Xm/XmP.h>
#include <Xm/AtomMgr.h>
#include <Xm/TextP.h>
#include <Xm/TextFP.h>
#include <Xm/TextSelP.h>
#include <Xm/TextFSelP.h>

#include <XmI/DebugUtil.h>


Boolean
_XmTextFieldConvert(Widget w,
		    Atom *selection,
		    Atom *target,
		    Atom *type,
		    XtPointer *value,
		    unsigned long *length,
		    int *format)
{
    return False;
}

void
_XmTextFieldLoseSelection(Widget w, Atom *selection)
{
}

Boolean
_XmTextConvert(Widget w,
	       Atom *selection,
	       Atom *target,
	       Atom *type,
	       XtPointer *value,
	       unsigned long *length,
	       int *format)
{
    char    *value_ptr;
    Boolean get_sel_flag;
    Boolean motif_sel_flag;
    Boolean secondary_sel_flag;
    Boolean primary_sel_flag;
    Boolean destination_sel_flag;
    Atom    encoding;
    Atom    motif_drop_atom;
    Atom    time_stamp_atom;
    Atom    compound_text_atom;
    Atom    text_atom;
    Atom    multiple_atom;
    Atom    targets_atom;
    Atom    delete_atom;
    Atom    insert_selection_atom;
    Atom    motif_destination_atom;
    XmTextSource    source_ts2;
    char    	*source_string;
    XmTextPosition     right_tp;
    XmTextPosition     left_tp;
    char    *list;
    Arg     args;
    Widget  wid;
    XTextProperty   text_prop_ret;
    int     count;
    Widget  wid2;
    int     status;

    motif_destination_atom = XmInternAtom( XtDisplay(w), "MOTIF_DESTINATION",
    	    	    	    	    	   False );
    insert_selection_atom = XmInternAtom( XtDisplay(w), "INSERT_SELECTION",
    	    	    	    	    	  False );
    delete_atom = XmInternAtom( XtDisplay(w), "DELETE", False );
    targets_atom = XmInternAtom( XtDisplay(w), "TARGETS", False );
    multiple_atom = XmInternAtom( XtDisplay(w), "MULTIPLE", False );
    text_atom = XmInternAtom( XtDisplay(w), "TEXT" , False );
    compound_text_atom = XmInternAtom( XtDisplay(w), "COMPOUND_TEXT", False );
    time_stamp_atom = XmInternAtom( XtDisplay(w), "TIMESTAMP", False );
    motif_drop_atom = XmInternAtom( XtDisplay(w), "_MOTIF_DROP", False );
    left_tp = 0;
    right_tp = 0;
    count = 0;
    list = "ABC";
    if ( *selection == motif_drop_atom )
    {
	XtSetArg( args, XmNclientData, &wid );
    	XtGetValues( w, &args, 1 );
    	wid2 = wid;
    }
    else
    {
    	wid2 = w;
    }

    if ( wid2 == NULL )
    {
    	return False;
    }

    source_ts2 = Text_Source(wid2);
    status = XmbTextListToTextProperty( XtDisplay(wid2), &list,
    	    	    	    	     1, XTextStyle, &text_prop_ret );
    encoding = 0x270f;
    if ( status  == Success )
    {
    	encoding = text_prop_ret.encoding;
    }
    XFree( (char *)text_prop_ret.value );
    if ( *selection == XA_PRIMARY )
    {
    	get_sel_flag = (Text_Source(wid2)->GetSelection)( source_ts2, &left_tp,
	    	    	    	    	    	 &right_tp);
    	primary_sel_flag = True;
    	motif_sel_flag = False;
    	destination_sel_flag = False;
    	secondary_sel_flag = False;
    }
    else if ( *selection == motif_destination_atom )
    {
    	get_sel_flag = Text_Input(wid)->data->sel_start;
    	destination_sel_flag = True;
    	motif_sel_flag = False;
    	primary_sel_flag = False;
    	secondary_sel_flag = False;
    }
    else if ( *selection == XA_SECONDARY )
    {
    	get_sel_flag = _XmTextGetSel2( (XmTextWidget)wid2, &left_tp,
	    	    	    	       &right_tp );
    	secondary_sel_flag = True;
    	motif_sel_flag = False;

    	primary_sel_flag = False;
    	destination_sel_flag = False;
    }
    else if ( *selection == motif_drop_atom )
    {
    	get_sel_flag = (Text_Source(wid2)->GetSelection)( source_ts2, &left_tp,
	    	    	    	    	    	    	  &right_tp );
    	motif_sel_flag = True;
    	secondary_sel_flag = False;

    	primary_sel_flag = False;
    	destination_sel_flag = False;
    }
    else
    {
	return False;
    }

    if ( *target == targets_atom )
    {
    	value_ptr = XtMalloc( 10 * sizeof( void *) );
    	*value = value_ptr;
    	*value_ptr++ = encoding;
    	*value_ptr++ = targets_atom;
    	*value_ptr++ = multiple_atom;
    	*value_ptr++ = time_stamp_atom;
    	count += 4;
    	if ( primary_sel_flag == False || destination_sel_flag == False )
	{
	    *value_ptr++ = insert_selection_atom;
    	    count += 1;
	}
    	if ( primary_sel_flag == True
    	|| secondary_sel_flag == True
	|| motif_sel_flag == True )
	{
    	    *value_ptr++ = compound_text_atom;
    	    *value_ptr++ = text_atom;
    	    *value_ptr++ = 31;
    	    count += 3;
	}
    	if ( primary_sel_flag == True || motif_sel_flag == True )
	{
    	    *value_ptr++ = delete_atom;
	    count += 1;
	}
    	*type = 4;
    	*length = count;

    	*format = 0x20;
    	return True;
    }
    else if ( *target == time_stamp_atom )
    {
    	value_ptr = XtMalloc( sizeof(void *) );
    	if ( primary_sel_flag == True )
	{
    	    *value_ptr = source_ts2->data->prim_time;
	}
	else if ( destination_sel_flag == True )
	{
    	    *value_ptr = Text_Input(wid2)->data->sec_time;
	}
	else if ( secondary_sel_flag == True || motif_sel_flag == True )
	{
    	    *value_ptr = Text_Input(wid2)->data->lasttime;
    	}
    	*value = value_ptr;
    	*type = 0x14;
    	*length = 4;
    	*format = 0x20;
    	return True;
    }
    else if ( *target == XA_STRING )
    {
    	*type = 0x1f;
    	*format = 8;
    	if ( destination_sel_flag == True || get_sel_flag == False )
	{
    	    return False;
	}
    	source_string = _XmStringSourceGetString( (XmTextWidget)wid2,
	    	    	    	left_tp, right_tp, False );
    	status = XmbTextListToTextProperty( XtDisplay(wid2), &source_string,
	    	    	    	    	  1, 0, (XTextProperty*)&right_tp );
    	XtFree( source_string );
    	if ( status == Success || status > 0 )
	{
    	    value_ptr = XtMalloc( text_prop_ret.nitems );
    	    *value = value_ptr;
    	    memcpy( value_ptr, text_prop_ret.value,
	    	    text_prop_ret.nitems );
    	    XFree( text_prop_ret.value );
    	    *length = text_prop_ret.nitems;
    	    return True;
    	}
	else
	{
	    *value = NULL;
    	    *length = 0x1c;
    	    return False;
	}
    }
    else if ( *target == text_atom )
    {
    	if ( destination_sel_flag == True || get_sel_flag == False )
	{
    	    return False;
	}
    	source_string = _XmStringSourceGetString( (XmTextWidget)wid2,
	    	    	    	    	    	  left_tp, right_tp,
						  False );
    	status = XmbTextListToTextProperty( XtDisplay(wid2), &source_string,
	    	    	    	    1, XStdICCTextStyle, &text_prop_ret );
    	*type = text_prop_ret.encoding;
    	*format = text_prop_ret.format;
    	XtFree( source_string );
    	if ( status == Success || status > 0 )
	{
    	    value_ptr = XtMalloc( text_prop_ret.nitems );
    	    *value = value_ptr;
    	    memcpy( value_ptr, text_prop_ret.value,
	    	    text_prop_ret.nitems );
    	    XFree( text_prop_ret.value );
    	    *length = text_prop_ret.nitems;
    	    return True;
	}
        *value = NULL;
        *length = 0;
    	return False;
    }
    else if ( *target == encoding )
    {
    	*type = encoding;
    	*format = 0x8;
        if ( destination_sel_flag == True || get_sel_flag == False )
	{
    	    return False;
	}
    	value_ptr = _XmStringSourceGetString( (XmTextWidget)wid2, left_tp,
	    	    	    	    	      right_tp, False );
    	*value = value_ptr;
    	*length = strlen( value_ptr );
    	return True;
    }
    else if ( *target == compound_text_atom )
    {
    	*type = compound_text_atom;
    	*format = 0x8;
    	if ( destination_sel_flag == True || get_sel_flag == False )
	{
    	    return False;
	}
    	source_string = _XmStringSourceGetString( (XmTextWidget)wid2, left_tp,
	    	    	    	    	    	  right_tp, False );
    	status = XmbTextListToTextProperty( XtDisplay(wid2), &source_string,
	    	    	    	    	    1, XCompoundTextStyle,
				    	    &text_prop_ret );
    	XtFree( source_string );
    	if ( status == Success || status > 0 )
	{
    	    value_ptr = XtMalloc( sizeof(void *) );
    	    *value = value_ptr;
    	    memcpy( value_ptr, text_prop_ret.value, text_prop_ret.nitems );
    	    XFree( text_prop_ret.value );
    	    *length = text_prop_ret.nitems;
            return True;
	}
	else
	{
            *value = NULL;
            *length = 0;
    	    return False;
    	}
    }
    else if ( *target == insert_selection_atom )
    {
    	if ( secondary_sel_flag == True )
	{
    	    return False;
	}
#if 0
    	return ConvertInsertSelection( w, selection, type, value,
	    	    	    	       length, format );
#else
	return False;
#endif
    }
    else if ( *target == delete_atom )
    {
    	if ( primary_sel_flag == False && motif_sel_flag == False )
	{
	    return False;
    	}
	/* the other flags are valid FIXME */
    	return False;
    }
    else
    {
    	return False;
    }
}

void
_XmTextLoseSelection(Widget w, Atom *selection)
{
}
