/* $Id: LTV6Editres.c,v 1.1 2004/08/28 19:22:44 dannybackx Exp $ */
/* $XConsortium: EditresCom.c,v 1.33 94/04/17 20:16:05 rws Exp $ */

/*

   Copyright (c) 1989  X Consortium

   Permission is hereby granted, free of charge, to any person obtaining a copy
   of this software and associated documentation files (the "Software"), to deal
   in the Software without restriction, including without limitation the rights
   to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
   copies of the Software, and to permit persons to whom the Software is
   furnished to do so, subject to the following conditions:

   The above copyright notice and this permission notice shall be included in
   all copies or substantial portions of the Software.

   THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
   IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
   FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.  IN NO EVENT SHALL THE
   X CONSORTIUM BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN
   AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
   CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

   Except as contained in this notice, the name of the X Consortium shall not be
   used in advertising or otherwise to promote the sale, use or other dealings
   in this Software without prior written authorization from the X Consortium.

 */

/*
 * Author:  Chris D. Peterson, Dave Sternlicht, MIT X Consortium
 */

#include <LTconfig.h>

#include <stdio.h>
#include <string.h>

#include <X11/IntrinsicP.h>	/* To get into the composite and core widget
				   structures. */
#include <X11/ObjectP.h>	/* For XtIs<Classname> macros. */
#include <X11/StringDefs.h>	/* for XtRString. */
#include <X11/ShellP.h>		/* for Application Shell Widget class. */

#include <X11/Xatom.h>
#include <X11/Xmd.h>
#include <X11/keysym.h>
#include <Xm/XmP.h>
#include <XmI/XmI.h>
#include <XmI/LTV6EditresP.h>

#include <XmI/DebugUtil.h>


/* local prototypes */
static void _LesstifEditResPutString8(ProtocolStream *stream, char *str);
static void _LesstifEditResPut8(ProtocolStream *stream, unsigned int value);
static void _LesstifEditResPut16(ProtocolStream *stream, unsigned int value);
static void _LesstifEditResPut32(ProtocolStream *stream, unsigned long value);
static void _LesstifEditResPutWidgetInfo(ProtocolStream *stream, WidgetInfo *info);
static void _LesstifEditResResetStream(ProtocolStream *stream);

static Boolean _LesstifEditResGet8(ProtocolStream *stream, unsigned char *val);
static Boolean _LesstifEditResGet16(ProtocolStream *stream, unsigned short *val);
static Boolean _LesstifEditResGetSigned16(ProtocolStream *stream, short *val);
static Boolean _LesstifEditResGet32(ProtocolStream *stream, unsigned long *val);
static Boolean _LesstifEditResGetString8(ProtocolStream *stream, char **str);
static Boolean _LesstifEditResGetWidgetInfo(ProtocolStream *stream, WidgetInfo *info);


#define _LesstifEditResPutBool _LesstifEditResPut8
#define _LesstifEditResPutResourceType _LesstifEditResPut8

static void _LesstifEditResGetStringValues(Widget, Arg *, int);

/************************************************************
 *
 * Local structure definitions.
 *
 ************************************************************/

typedef enum
{
    BlockNone, BlockSetValues, BlockAll
}
EditresBlock;

typedef struct _SetValuesEvent
{
    EditresCommand type;	/* first field must be type. */
    WidgetInfo *widgets;
    unsigned short num_entries;	/* number of set values requests. */
    char *name;
    char *res_type;
    XtPointer value;
    unsigned short value_len;
}
SetValuesEvent;

typedef struct _SVErrorInfo
{
    SetValuesEvent *event;
    ProtocolStream *stream;
    unsigned short *count;
    WidgetInfo *entry;
}
SVErrorInfo;

typedef struct _GetValuesEvent
{
    EditresCommand type;	/* first field must be type */
    WidgetInfo *widgets;
    unsigned short num_entries;	/* number of get values requests */
    char *name;
}
GetValuesEvent;

typedef struct _FindChildEvent
{
    EditresCommand type;	/* first field must be type. */
    WidgetInfo *widgets;
    short x, y;
}
FindChildEvent;

typedef struct _GenericGetEvent
{
    EditresCommand type;	/* first field must be type. */
    WidgetInfo *widgets;
    unsigned short num_entries;	/* number of set values requests. */
}
GenericGetEvent, GetResEvent, GetGeomEvent;

/*
 * Things that are common to all events.
 */

typedef struct _AnyEvent
{
    EditresCommand type;	/* first field must be type. */
    WidgetInfo *widgets;
}
AnyEvent;

/*
 * The event union.
 */

typedef union _EditresEvent
{
    AnyEvent any_event;
    SetValuesEvent set_values_event;
    GetResEvent get_resources_event;
    GetGeomEvent get_geometry_event;
    FindChildEvent find_child_event;
}
EditresEvent;

typedef struct _Globals
{
    EditresBlock block;
    SVErrorInfo error_info;
    ProtocolStream stream;
    ProtocolStream *command_stream;	/* command stream. */
#if defined(LONG64) || defined(WORD64)
    unsigned long base_address;
#endif
}
Globals;

#define CURRENT_PROTOCOL_VERSION 5L

#define streq(a,b) (strcmp( (a), (b) ) == 0)

static Atom res_editor_command, res_editor_protocol, client_value;

static Globals globals;

static void SendFailure(Widget w, Atom sel, ResIdent ident, char *str);
static void SendCommand(Widget w, Atom sel, ResIdent ident,
			EditresCommand command, ProtocolStream *stream);
static void InsertWidget(ProtocolStream * stream, Widget w);
static void ExecuteCommand(Widget w, Atom sel, ResIdent ident,
			   EditresEvent * event);
static void FreeEvent(EditresEvent *event);
static void ExecuteSetValues(Widget w, SetValuesEvent *sv_event,
			     WidgetInfo *entry, ProtocolStream *stream,
			     unsigned short *count);
static void ExecuteGetGeometry(Widget w, ProtocolStream *stream);
static void ExecuteGetResources(Widget w, ProtocolStream *stream);

static void GetCommand(Widget w, XtPointer data, Atom *selection, Atom *type,
		       XtPointer value, unsigned long *length, int *format);
static void LoadResources(Widget w);
static void DumpChildren(Widget w, ProtocolStream *stream,
			 unsigned short *count);
static char *DumpWidgets(Widget w, EditresEvent *event,
			 ProtocolStream *stream);
static char *DoSetValues(Widget w, EditresEvent *event,
			 ProtocolStream *stream);
static char *DoFindChild(Widget w, EditresEvent *event,
			 ProtocolStream *stream);
static char *DoGetGeometry(Widget w, EditresEvent *event,
			   ProtocolStream *stream);
static char *DoGetResources(Widget w, EditresEvent *event,
			    ProtocolStream *stream);
static char *DumpValues(Widget w, EditresEvent *event, ProtocolStream *stream);

#if XtSpecificationRelease < 6
static Status
XInternAtoms(Display *dpy, char **names, int count, Bool ifExists, Atom *aret)
{
    int i;

    for (i = 0; i < count; i++)
    {
	aret[i] = XInternAtom(dpy, names[i], ifExists);
    }

    return Success;
}
#endif

/*
 * avoid linking with Xmu
 */

static void
XmuCopyISOLatin1Lowered(char *dst, XmConst char *src)
{
    register unsigned char *dest, *source;

    for (dest = (unsigned char *)dst, source = (unsigned char *)src;
	 *source;
	 source++, dest++)
    {
	if ((*source >= XK_A) && (*source <= XK_Z))
	    *dest = *source + (XK_a - XK_A);
	else if ((*source >= XK_Agrave) && (*source <= XK_Odiaeresis))
	    *dest = *source + (XK_agrave - XK_Agrave);
	else if ((*source >= XK_Ooblique) && (*source <= XK_Thorn))
	    *dest = *source + (XK_oslash - XK_Ooblique);
	else
	    *dest = *source;
    }
    *dest = '\0';
}


/************************************************************
 *
 * Resource Editor Communication Code
 *
 ************************************************************/


/*    Function Name: _XmNSEEditResCheckMessages (_XmNSEEditResCheckMessages)
 *    Description: This callback routine is set on all shell widgets,
 *                   and checks to see if a client message event
 *                   has come from the resource editor.
 *    Arguments: w - the shell widget.
 *                 data - *** UNUSED ***
 *                 event - The X Event that triggered this handler.
 *                 cont - *** UNUSED ***.
 *    Returns: none.
 */

extern void
_XmNSEEditResCheckMessages(Widget w,
                           XtPointer data, 
                           XEvent *event,
                           Boolean *cont)
{
    Time time;
    ResIdent ident;
    static Boolean first_time = False;
    static Atom res_editor, res_comm;
    Display *dpy;

    if (event->type == ClientMessage)
    {
	XClientMessageEvent *c_event = (XClientMessageEvent *)event;
	dpy = XtDisplay(w);

	if (!first_time)
	{
	    Atom atoms[4];
	    static char *names[] =
	    {
		EDITRES_NAME, EDITRES_COMMAND_ATOM,
		EDITRES_PROTOCOL_ATOM, EDITRES_CLIENT_VALUE};

	    first_time = True;
	    XInternAtoms(dpy, names, 4, False, atoms);
	    res_editor = atoms[0];
	    res_editor_command = atoms[1];
	    res_editor_protocol = atoms[2];
	    /* Used in later procedures. */
	    client_value = atoms[3];
	    LoadResources(w);
	}

	if ((c_event->message_type != res_editor) ||
	    (c_event->format != EDITRES_SEND_EVENT_FORMAT))
	    return;

	time = c_event->data.l[0];
	res_comm = c_event->data.l[1];
	ident = (ResIdent)c_event->data.l[2];
	if (c_event->data.l[3] != CURRENT_PROTOCOL_VERSION)
	{
	    _LesstifEditResResetStream(&globals.stream);
	    _LesstifEditResPut8(&globals.stream, CURRENT_PROTOCOL_VERSION);
	    SendCommand(w, res_comm, ident, ProtocolMismatch, &globals.stream);
	    return;
	}

	XtGetSelectionValue(w, res_comm, res_editor_command,
			    GetCommand, (XtPointer)(unsigned long)ident, time);
    }
}


/*    Function Name: BuildEvent
 *    Description: Takes the info out the protocol stream an constructs
 *                   the proper event structure.
 *    Arguments: w - widget to own selection, in case of error.
 *                 sel - selection to send error message beck in.
 *                 data - the data for the request.
 *                 ident - the id number we are looking for.
 *                 length - length of request.
 *    Returns: the event, or NULL.
 */

#define ERROR_MESSAGE ("Client: Improperly formatted protocol request")


static EditresEvent *
BuildEvent(Widget w, Atom sel, XtPointer data, ResIdent ident,
	   unsigned long length)
{
    EditresEvent *event;
    ProtocolStream alloc_stream, *stream;
    unsigned char temp;
    register unsigned int i;

    stream = &alloc_stream;	/* easier to think of it this way... */

    stream->current = stream->top = (unsigned char *)data;
    stream->size = HEADER_SIZE;	/* size of header. */

    /*
     * Retrieve the Header.
     */

    if (length < HEADER_SIZE)
    {
	SendFailure(w, sel, ident, ERROR_MESSAGE);
	return (NULL);
    }

    (void)_LesstifEditResGet8(stream, &temp);
    if (temp != ident)		/* Id's don't match, ignore request. */
	return (NULL);

    event = (EditresEvent *)XtCalloc(sizeof(EditresEvent), 1);

    (void)_LesstifEditResGet8(stream, &temp);
    event->any_event.type = (EditresCommand)temp;
    (void)_LesstifEditResGet32(stream, &(stream->size));
    stream->top = stream->current;	/* reset stream to top of value. */

    /*
     * Now retrieve the data segment.
     */

    switch (event->any_event.type)
    {
    case SendWidgetTree:
	break;			/* no additional info */
    case SetValues:
	{
	    SetValuesEvent *sv_event = (SetValuesEvent *)event;

	    if (!(_LesstifEditResGetString8(stream, &(sv_event->name)) &&
		  _LesstifEditResGetString8(stream, &(sv_event->res_type))))
	    {
		goto done;
	    }

	    /*
	     * Since we need the value length, we have to pull the
	     * value out by hand.
	     */

	    if (!_LesstifEditResGet16(stream, &(sv_event->value_len)))
		goto done;

	    sv_event->value = XtMalloc(sizeof(char) *
				         (sv_event->value_len + 1));

	    for (i = 0; i < sv_event->value_len; i++)
	    {
		if (!_LesstifEditResGet8(stream,
					 (unsigned char *)sv_event->value + i))
		{
		    goto done;
		}
	    }

	    /* NULL terminate that sucker. */
	    ((char *)sv_event->value)[i] = '\0';

	    if (!_LesstifEditResGet16(stream, &(sv_event->num_entries)))
		goto done;

	    sv_event->widgets = (WidgetInfo *)
		XtCalloc(sizeof(WidgetInfo), sv_event->num_entries);

	    for (i = 0; i < sv_event->num_entries; i++)
	    {
		if (!_LesstifEditResGetWidgetInfo(stream,
						  sv_event->widgets + i))
		    goto done;
	    }
	}
	break;
    case FindChild:
	{
	    FindChildEvent *find_event = (FindChildEvent *)event;

	    find_event->widgets = (WidgetInfo *)
		XtCalloc(sizeof(WidgetInfo), 1);

	    if (!(_LesstifEditResGetWidgetInfo(stream, find_event->widgets) &&
		  _LesstifEditResGetSigned16(stream, &(find_event->x)) &&
		  _LesstifEditResGetSigned16(stream, &(find_event->y))))
	    {
		goto done;
	    }

	}
	break;
    case GetGeometry:
    case GetResources:
	{
	    GenericGetEvent *get_event = (GenericGetEvent *)event;

	    if (!_LesstifEditResGet16(stream, &(get_event->num_entries)))
		goto done;

	    get_event->widgets = (WidgetInfo *)
		XtCalloc(sizeof(WidgetInfo), get_event->num_entries);
	    for (i = 0; i < get_event->num_entries; i++)
	    {
		if (!_LesstifEditResGetWidgetInfo(stream,
						  get_event->widgets + i))
		    goto done;
	    }
	}
	break;

    case GetValues:
	{
	    GetValuesEvent *gv_event = (GetValuesEvent *)event;
	    _LesstifEditResGetString8(stream, &(gv_event->name));
	    _LesstifEditResGet16(stream, &(gv_event->num_entries));
	    gv_event->widgets = (WidgetInfo *)
		XtCalloc(sizeof(WidgetInfo), gv_event->num_entries);
	    _LesstifEditResGetWidgetInfo(stream, gv_event->widgets);
	}
	break;

    default:
	{
	    char buf[BUFSIZ];

	    sprintf(buf, "Unknown Protocol request %d.", event->any_event.type);
	    SendFailure(w, sel, ident, buf);
	    return (NULL);
	}
    }
    return (event);

  done:

    SendFailure(w, sel, ident, ERROR_MESSAGE);
    FreeEvent(event);
    return (NULL);
}


/*      Function Name: FreeEvent
 *    Description: Frees the event structure and any other pieces
 *                   in it that need freeing.
 *      Arguments: event - the event to free.
 *      Returns: none.
 */

static void
FreeEvent(EditresEvent *event)
{
    if (event->any_event.widgets != NULL)
    {
	XtFree((char *)event->any_event.widgets->ids);
	XtFree((char *)event->any_event.widgets);
    }

    if (event->any_event.type == SetValues)
    {
	XtFree(event->set_values_event.name);	/* XtFree does not free if */
	XtFree(event->set_values_event.res_type);	/* value is NULL. */
    }

    XtFree((char *)event);
}


/*      Function Name: GetCommand
 *    Description: Gets the Command out of the selection asserted by the
 *                   resource manager.
 *      Arguments: (See Xt XtConvertSelectionProc)
 *                 data - contains the ident number for the command.
 *      Returns: none.
 */

static void
GetCommand(Widget w, XtPointer data, Atom *selection, Atom *type,
	   XtPointer value, unsigned long *length, int *format)
{
    ResIdent ident = (ResIdent)(unsigned long)data;
    EditresEvent *event;

    if ((*type != res_editor_protocol) || (*format != EDITRES_FORMAT))
	return;

    if ((event = BuildEvent(w, *selection, value, ident, *length)) != NULL)
    {
	ExecuteCommand(w, *selection, ident, event);
	FreeEvent(event);
    }
}


/*    Function Name: ExecuteCommand
 *    Description: Executes a command string received from the 
 *                   resource editor.
 *    Arguments: w       - a widget.
 *                 command - the command to execute.
 *                 value - the associated with the command.
 *    Returns: none.
 *
 * NOTES:  munges str
 */

static void
ExecuteCommand(Widget w, Atom sel, ResIdent ident, EditresEvent * event)
{
    char *(*func)(Widget, EditresEvent *, ProtocolStream *);
    char *str;

    if (globals.block == BlockAll)
    {
	SendFailure(w, sel, ident,
		    "This client has blocked all Editres commands.");
	return;
    }
    else if ((globals.block == BlockSetValues) &&
	     (event->any_event.type == SetValues))
    {
	SendFailure(w, sel, ident,
		    "This client has blocked all SetValues requests.");
	return;
    }

    switch (event->any_event.type)
    {
    case SendWidgetTree:
#if defined(LONG64) || defined(WORD64)
	globals.base_address = (unsigned long)w & 0xFFFFFFFF00000000;
#endif
	func = DumpWidgets;
	break;
    case SetValues:
	func = DoSetValues;
	break;
    case FindChild:
	func = DoFindChild;
	break;
    case GetGeometry:
	func = DoGetGeometry;
	break;
    case GetResources:
	func = DoGetResources;
	break;
    case GetValues:
	func = DumpValues;
	break;
    default:
	{
	    char buf[BUFSIZ];
	    sprintf(buf, "Unknown Protocol request %d.", event->any_event.type);
	    SendFailure(w, sel, ident, buf);
	    return;
	}
    }

    _LesstifEditResResetStream(&globals.stream);
    if ((str = (*func) (w, event, &globals.stream)) == NULL)
	SendCommand(w, sel, ident, PartialSuccess, &globals.stream);
    else
    {
	SendFailure(w, sel, ident, str);
	XtFree(str);
    }
}


/*    Function Name: ConvertReturnCommand
 *    Description: Converts a selection.
 *    Arguments: w - the widget that owns the selection.
 *               selection - selection to convert.
 *               target - target type for this selection.
 *               type_ret - type of the selection.
 *               value_ret - selection value;
 *               length_ret - lenght of this selection.
 *               format_ret - the format the selection is in.
 *    Returns: True if conversion was sucessful.
 */

static Boolean
ConvertReturnCommand(Widget w, Atom *selection, Atom *target, Atom *type_ret,
		     XtPointer *value_ret, unsigned long *length_ret,
		     int *format_ret)
{
    /*
     * I assume the intrinsics give me the correct selection back.
     */

    if ((*target != client_value))
	return (False);

    *type_ret = res_editor_protocol;
    *value_ret = (XtPointer)globals.command_stream->real_top;
    *length_ret = globals.command_stream->size + HEADER_SIZE;
    *format_ret = EDITRES_FORMAT;

    return (True);
}


/*      Function Name: CommandDone
 *    Description: done with the selection.
 *      Arguments: *** UNUSED ***
 *      Returns: none.
 */

static void
CommandDone(Widget widget, Atom *selection, Atom *target)
{
    /* Keep the toolkit from automaticaly freeing the selection value */
}


/*    Function Name: SendFailure
 *    Description: Sends a failure message.
 *    Arguments: w - the widget to own the selection.
 *               sel - the selection to assert.
 *               ident - the identifier.
 *               str - the error message.
 *    Returns: none.
 */

static void
SendFailure(Widget w, Atom sel, ResIdent ident, char *str)
{
    _LesstifEditResResetStream(&globals.stream);
    _LesstifEditResPutString8(&globals.stream, str);
    SendCommand(w, sel, ident, Failure, &globals.stream);
}

/*    Function Name: BuildReturnPacket
 *    Description: Builds a return packet, given the data to send.
 *    Arguments: ident - the identifier.
 *               command - the command code.
 *               stream - the protocol stream.
 *    Returns: packet - the packet to send.
 */

static XtPointer
BuildReturnPacket(ResIdent ident, EditresCommand command,
		  ProtocolStream *stream)
{
    long old_alloc, old_size;
    unsigned char *old_current;

    /*
     * We have cleverly keep enough space at the top of the header
     * for the return protocol stream, so all we have to do is
     * fill in the space.
     */

    /* 
     * Fool the insert routines into putting the header in the right
     * place while being damn sure not to realloc (that would be very bad.
     */

    old_current = stream->current;
    old_alloc = stream->alloc;
    old_size = stream->size;

    stream->current = stream->real_top;
    stream->alloc = stream->size + (2 * HEADER_SIZE);

    _LesstifEditResPut8(stream, ident);
    _LesstifEditResPut8(stream, (unsigned char)command);
    _LesstifEditResPut32(stream, old_size);

    stream->alloc = old_alloc;
    stream->current = old_current;
    stream->size = old_size;

    return ((XtPointer)stream->real_top);
}


/*      Function Name: SendCommand
 *    Description: Builds a return command line.
 *      Arguments: w - the widget to own the selection.
 *                 sel - the selection to assert.
 *                 ident - the identifier.
 *                 command - the command code.
 *                 stream - the protocol stream.
 *      Returns: none.
 */

static void
SendCommand(Widget w, Atom sel, ResIdent ident, EditresCommand command,
	    ProtocolStream *stream)
{
    BuildReturnPacket(ident, command, stream);
    globals.command_stream = stream;

/*
 * I REALLY want to own the selection.  Since this was not triggered
 * by a user action, and I am the only one using this atom it is safe to
 * use CurrentTime.
 */

    XtOwnSelection(w, sel, CurrentTime,
		   ConvertReturnCommand, NULL, CommandDone);
}


/************************************************************
 *
 * Generic Utility Functions.
 *
 ************************************************************/

/*      Function Name: FindChildren
 *    Description: Retuns all children (popup, normal and otherwise)
 *                   of this widget
 *      Arguments: parent - the parent widget.
 *                 children - the list of children.
 *                 normal - return normal children.
 *                 popup - return popup children.
 *      Returns: the number of children.
 */

static int
FindChildren(Widget parent, Widget **children, Boolean normal, Boolean popup)
{
    CompositeWidget cw = (CompositeWidget)parent;
    Cardinal i, num_children, current = 0;

    num_children = 0;

    if (XtIsWidget(parent) && popup)
	num_children += parent->core.num_popups;

    if (XtIsComposite(parent) && normal)
	num_children += cw->composite.num_children;

    if (num_children == 0)
    {
	*children = NULL;
	return (0);
    }

    *children = (Widget *)XtMalloc((Cardinal)sizeof(Widget) * num_children);

    if (XtIsComposite(parent) && normal)
	for (i = 0; i < cw->composite.num_children; i++, current++)
	    (*children)[current] = cw->composite.children[i];

    if (XtIsWidget(parent) && popup)
	for (i = 0; i < parent->core.num_popups; i++, current++)
	    (*children)[current] = parent->core.popup_list[i];

    return (num_children);
}


/*      Function Name: IsChild
 *    Description: check to see of child is a child of parent.
 *      Arguments: top - the top of the tree.
 *                 parent - the parent widget.
 *                 child - the child.
 *      Returns: none.
 */

static Boolean
IsChild(Widget top, Widget parent, Widget child)
{
    int i, num_children;
    Widget *children;

    if (parent == NULL)
	return (top == child);

    num_children = FindChildren(parent, &children, True, True);

    for (i = 0; i < num_children; i++)
    {
	if (children[i] == child)
	{
	    XtFree((char *)children);
	    return (True);
	}
    }

    XtFree((char *)children);
    return (False);
}


/*      Function Name: VerifyWidget
 *    Description: Makes sure all the widgets still exist.
 *      Arguments: w - any widget in the tree.
 *                 info - the info about the widget to verify.
 *      Returns: an error message or NULL.
 */

static char *
VerifyWidget(Widget w, WidgetInfo *info)
{
    Widget top;

    register int count;
    register Widget parent;
    register unsigned long *child;

    for (top = w; XtParent(top) != NULL; top = XtParent(top))
    {
    }

    parent = NULL;
    child = info->ids;
    count = 0;

    while (True)
    {
	if (!IsChild(top, parent, (Widget)*child))
	    return (XtNewString("This widget no longer exists in the client."));

	if (++count == info->num_widgets)
	    break;

	parent = (Widget)*child++;
    }

    info->real_widget = (Widget)*child;
    return (NULL);
}


/************************************************************
 *
 * Code to Perform SetValues operations.
 *
 ************************************************************/



/*      Function Name:  DoSetValues
 *    Description: performs the setvalues requested.
 *      Arguments: w - a widget in the tree.
 *                 event - the event that caused this action.
 *                 stream - the protocol stream to add.
 *      Returns: NULL.
 */

static char *
DoSetValues(Widget w, EditresEvent *event, ProtocolStream *stream)
{
    char *str;
    register unsigned i;
    unsigned short count = 0;
    SetValuesEvent *sv_event = (SetValuesEvent *)event;

    /* insert 0, will be overwritten later. */
    _LesstifEditResPut16(stream, count);

    for (i = 0; i < sv_event->num_entries; i++)
    {
	if ((str = VerifyWidget(w, &(sv_event->widgets[i]))) != NULL)
	{
	    _LesstifEditResPutWidgetInfo(stream, &(sv_event->widgets[i]));
	    _LesstifEditResPutString8(stream, str);
	    XtFree(str);
	    count++;
	}
	else
	    ExecuteSetValues(sv_event->widgets[i].real_widget,
			     sv_event, sv_event->widgets + i, stream, &count);
    }

    /*
     * Overwrite the first 2 bytes with the real count.
     */

    *(stream->top) = count >> XER_NBBY;
    *(stream->top + 1) = count;
    return (NULL);
}


/*    Function Name: HandleToolkitErrors
 *    Description: Handles X Toolkit Errors.
 *    Arguments: name - name of the error.
 *               type - type of the error.
 *               class - class of the error.
 *               msg - the default message.
 *               params, num_params - the extra parameters for this message.
 *    Returns: none.
 */

static void
HandleToolkitErrors(String name, String type, String class, String msg,
		    String *params, Cardinal *num_params)
{
    SVErrorInfo *info = &globals.error_info;
    char buf[BUFSIZ];

    if (streq(name, "unknownType"))
	sprintf(buf, "The `%s' resource is not used by this widget.",
		info->event->name);
    else if (streq(name, "noColormap"))
	sprintf(buf, msg, params[0]);
    else if (streq(name, "conversionFailed") || streq(name, "conversionError"))
    {
	if (streq((char *)info->event->value, XtRString))
	    sprintf(buf,
		    "Could not convert the string '%s' for the `%s' resource.",
		    (char *)info->event->value, info->event->name);
	else
	    sprintf(buf, "Could not convert the `%s' resource.",
		    info->event->name);
    }
    else
	sprintf(buf, "Name: %s, Type: %s, Class: %s, Msg: %s",
		name, type, class, msg);

    /*
     * Insert this info into the protocol stream, and update the count.
     */

    (*(info->count))++;
    _LesstifEditResPutWidgetInfo(info->stream, info->entry);
    _LesstifEditResPutString8(info->stream, buf);
}


/*      Function Name: ExecuteSetValues
 *    Description: Performs a setvalues for a given command.
 *      Arguments: w - the widget to perform the set_values on.
 *                 sv_event - the set values event.
 *                 sv_info - the set_value info.
 *      Returns: none.
 */

static void
ExecuteSetValues(Widget w, SetValuesEvent *sv_event, WidgetInfo *entry,
		 ProtocolStream *stream, unsigned short *count)
{
    XtErrorMsgHandler old;

    SVErrorInfo *info = &globals.error_info;
    info->event = sv_event;	/* No data can be passed to */
    info->stream = stream;	/* an error handler, so we */
    info->count = count;	/* have to use a global, YUCK... */
    info->entry = entry;

    old = XtAppSetWarningMsgHandler(XtWidgetToApplicationContext(w),
				    HandleToolkitErrors);

    XtVaSetValues(w, XtVaTypedArg,
		  sv_event->name, sv_event->res_type,
		  sv_event->value, sv_event->value_len,
		  NULL);

    (void)XtAppSetWarningMsgHandler(XtWidgetToApplicationContext(w), old);
}


/************************************************************
 *
 * Code for Creating and dumping widget tree.
 *
 ************************************************************/

/*    Function Name:  DumpWidgets
 *    Description: Given a widget it builds a protocol packet
 *                 containing the entire widget heirarchy.
 *    Arguments: w - a widget in the tree.
 *               event - the event that caused this action.
 *               stream - the protocol stream to add.
 *    Returns: NULL
 */

#define TOOLKIT_TYPE ("Xt")


static char *
DumpWidgets(Widget w, EditresEvent *event, ProtocolStream *stream)
{
    unsigned short count = 0;

    /* Find Tree's root. */
    for (; XtParent(w) != NULL; w = XtParent(w))
    {
    }

    /*
     * hold space for count, overwritten later. 
     */

    _LesstifEditResPut16(stream, (unsigned int)0);

    DumpChildren(w, stream, &count);

    /*
     * write out toolkit type (Xt, of course...). 
     */

    _LesstifEditResPutString8(stream, TOOLKIT_TYPE);

    /*
     * Overwrite the first 2 bytes with the real count.
     */

    *(stream->top) = count >> XER_NBBY;
    *(stream->top + 1) = count;
    return (NULL);
}


/*    Function Name: DumpChildren
 *    Description: Adds a child's name to the list.
 *    Arguments: w - the widget to dump.
 *               stream - the stream to dump to.
 *               count - number of dumps we have performed.
 *    Returns: none.
 */

/* This is a trick/kludge.  To make shared libraries happier (linking
 * against Xmu but not linking against Xt, and apparently even work
 * as we desire on SVR4, we need to avoid an explicit data reference
 * to applicationShellWidgetClass.  XtIsTopLevelShell is known
 * (implementation dependent assumption!) to use a bit flag.  So we
 * go that far.  Then, we test whether it is an applicationShellWidget
 * class by looking for an explicit class name.  Seems pretty safe.
 */
static Bool
isApplicationShell(Widget w)
{
    register WidgetClass c;

    if (!XtIsTopLevelShell(w))
	return False;
    for (c = XtClass(w); c; c = c->core_class.superclass)
    {
	if (!strcmp(c->core_class.class_name, "ApplicationShell"))
	    return True;
    }
    return False;
}


static void
DumpChildren(Widget w, ProtocolStream *stream, unsigned short *count)
{
    int i, num_children;
    Widget *children;
    unsigned long window;
    char *class;

    (*count)++;

    InsertWidget(stream, w);	/* Insert the widget into the stream. */

    _LesstifEditResPutString8(stream, XtName(w));	/* Insert name */

    if (isApplicationShell(w))
	class = ((ApplicationShellWidget)w)->application.class;
    else
	class = XtClass(w)->core_class.class_name;

    _LesstifEditResPutString8(stream, class);	/* Insert class */

    if (XtIsWidget(w))
      {
	if (XtIsRealized(w))
	    window = XtWindow(w);
	else
	    window = EDITRES_IS_UNREALIZED;
      }
    else
	window = EDITRES_IS_OBJECT;

    _LesstifEditResPut32(stream, window);	/* Insert window id. */

    /*
     * Find children and recurse.
     */

    num_children = FindChildren(w, &children, True, True);
    for (i = 0; i < num_children; i++)
	DumpChildren(children[i], stream, count);

    XtFree((char *)children);
}


/************************************************************
 *
 * Code for getting the geometry of widgets.
 *
 ************************************************************/

/*    Function Name:  DoGetGeometry
 *    Description: retrieves the Geometry of each specified widget.
 *    Arguments: w - a widget in the tree.
 *               event - the event that caused this action.
 *               stream - the protocol stream to add.
 *    Returns: NULL
 */

static char *
DoGetGeometry(Widget w, EditresEvent *event, ProtocolStream *stream)
{
    unsigned i;
    char *str;
    GetGeomEvent *geom_event = (GetGeomEvent *)event;

    _LesstifEditResPut16(stream, geom_event->num_entries);

    for (i = 0; i < geom_event->num_entries; i++)
    {

	/* 
	 * Send out the widget id. 
	 */

	_LesstifEditResPutWidgetInfo(stream, &(geom_event->widgets[i]));
	if ((str = VerifyWidget(w, &(geom_event->widgets[i]))) != NULL)
	{
	    _LesstifEditResPutBool(stream, True);	/* an error occured. */
	    _LesstifEditResPutString8(stream, str);	/* set message. */
	    XtFree(str);
	}
	else
	    ExecuteGetGeometry(geom_event->widgets[i].real_widget, stream);
    }
    return (NULL);
}


/*    Function Name: ExecuteGetGeometry
 *    Description: Gets the geometry for each widget specified.
 *    Arguments: w - the widget to get geom on.
 *               stream - stream to append to.
 *    Returns: True if no error occured.
 */

static void
ExecuteGetGeometry(Widget w, ProtocolStream *stream)
{
    int i;
    Boolean mapped_when_man;
    Dimension width, height, border_width;
    Arg args[8];
    Cardinal num_args = 0;
    Position x, y;

    if (!XtIsRectObj(w) || (XtIsWidget(w) && !XtIsRealized(w)))
    {
	_LesstifEditResPutBool(stream, False);	/* no error. */
	_LesstifEditResPutBool(stream, False);	/* not visable. */
	for (i = 0; i < 5; i++)	/* fill in extra space with 0's. */
	    _LesstifEditResPut16(stream, 0);
	return;
    }

    XtSetArg(args[num_args], XtNwidth, &width); num_args++;
    XtSetArg(args[num_args], XtNheight, &height); num_args++;
    XtSetArg(args[num_args], XtNborderWidth, &border_width); num_args++;
    XtSetArg(args[num_args], XtNmappedWhenManaged, &mapped_when_man);
    num_args++;
    XtGetValues(w, args, num_args);

    if (!(XtIsManaged(w) && mapped_when_man) && XtIsWidget(w))
    {
	XWindowAttributes attrs;

	/* 
	 * The toolkit does not maintain mapping state, we have
	 * to go to the server.
	 */

	if (XGetWindowAttributes(XtDisplay(w), XtWindow(w), &attrs) != 0)
	{
	    if (attrs.map_state != IsViewable)
	    {
		_LesstifEditResPutBool(stream, False);	/* no error. */
		_LesstifEditResPutBool(stream, False);	/* not visable. */

		for (i = 0; i < 5; i++)
		    _LesstifEditResPut16(stream, 0);
		return;
	    }
	}
	else
	{
	    _LesstifEditResPut8(stream, True);	/* Error occured. */
	    _LesstifEditResPutString8(stream, "XGetWindowAttributes failed.");
	    return;
	}
    }

    XtTranslateCoords(w, -((int)border_width), -((int)border_width), &x, &y);

    _LesstifEditResPutBool(stream, False);	/* no error. */
    _LesstifEditResPutBool(stream, True);	/* Visable. */
    _LesstifEditResPut16(stream, x);
    _LesstifEditResPut16(stream, y);
    _LesstifEditResPut16(stream, width);
    _LesstifEditResPut16(stream, height);
    _LesstifEditResPut16(stream, border_width);
}


/************************************************************
 *
 * Code for executing FindChild.
 *
 ************************************************************/

/*    Function Name: PositionInChild
 *    Description: returns true if this location is in the child.
 *    Arguments: child - the child widget to check.
 *               x, y - location of point to check in the parent's
 *                      coord space.
 *    Returns: True if the position is in this child.
 */

static Boolean
PositionInChild(Widget child, int x, int y)
{
    Arg args[6];
    Cardinal num;
    Dimension width, height, border_width;
    Position child_x, child_y;
    Boolean mapped_when_managed;

    if (!XtIsRectObj(child))	/* we must at least be a rect obj. */
	return (False);

    num = 0;
    XtSetArg(args[num], XtNmappedWhenManaged, &mapped_when_managed); num++;
    XtSetArg(args[num], XtNwidth, &width); num++;
    XtSetArg(args[num], XtNheight, &height); num++;
    XtSetArg(args[num], XtNx, &child_x); num++;
    XtSetArg(args[num], XtNy, &child_y); num++;
    XtSetArg(args[num], XtNborderWidth, &border_width); num++;
    XtGetValues(child, args, num);
    if (XmIsVendorShell(XtParent(child)))
    {
    	child_x = -border_width;
    	child_y = -border_width;
    }

    /*
     * The only way we will know of the widget is mapped is to see if
     * mapped when managed is True and this is a managed child.  Otherwise
     * we will have to ask the server if this window is mapped.
     */

    if (XtIsWidget(child) && !(mapped_when_managed && XtIsManaged(child)))
    {
	XWindowAttributes attrs;

	if (XGetWindowAttributes(XtDisplay(child),
				 XtWindow(child), &attrs) != 0)
	{
	    /* oops */
	}
	else if (attrs.map_state != IsViewable)
	    return (False);
    }

    return (x >= child_x) &&
	(x <= (child_x + (Position)width + 2 * (Position)border_width)) &&
	(y >= child_y) &&
	(y <= (child_y + (Position)height + 2 * (Position)border_width));
}


/*      Function Name: _FindChild
 *    Description: Finds the child that actually contatians the point shown.
 *      Arguments: parent - a widget that is known to contain the point
 *                          specified.
 *                 x, y - The point in coordinates relative to the 
 *                        widget specified.
 *      Returns: none.
 */

static Widget
_FindChild(Widget parent, int x, int y)
{
    Widget *children;
    int i = FindChildren(parent, &children, True, False);

    while (i > 0)
    {
	i--;

	if (PositionInChild(children[i], x, y))
	{
	    Widget child = children[i];

	    XtFree((char *)children);
	    return (_FindChild(child, x - child->core.x, y - child->core.y));
	}
    }

    XtFree((char *)children);
    return (parent);
}


/*    Function Name: DoFindChild
 *    Description: finds the child that contains the location specified.
 *    Arguments: w - a widget in the tree.
 *               event - the event that caused this action.
 *               stream - the protocol stream to add.
 *    Returns: an allocated error message if something went horribly
 *             wrong and no set values were performed, else NULL.
 */

static char *
DoFindChild(Widget w, EditresEvent *event, ProtocolStream *stream)
{
    char *str;
    Widget parent, child;
    Position parent_x, parent_y;
    FindChildEvent *find_event = (FindChildEvent *)event;

    if ((str = VerifyWidget(w, find_event->widgets)) != NULL)
	return (str);

    parent = find_event->widgets->real_widget;

    XtTranslateCoords(parent, (Position)0, (Position)0,
		      &parent_x, &parent_y);

    child = _FindChild(parent, find_event->x - (int)parent_x,
		       find_event->y - (int)parent_y);

    InsertWidget(stream, child);
    return (NULL);
}


/************************************************************
 *
 * Procedures for performing GetResources.
 *
 ************************************************************/

/*    Function Name: DoGetResources
 *    Description: Gets the Resources associated with the widgets passed.
 *    Arguments: w - a widget in the tree.
 *               event - the event that caused this action.
 *               stream - the protocol stream to add.
 *    Returns: NULL
 */

static char *
DoGetResources(Widget w, EditresEvent *event, ProtocolStream *stream)
{
    unsigned int i;
    char *str;
    GetResEvent *res_event = (GetResEvent *)event;

    /* number of replys */
    _LesstifEditResPut16(stream, res_event->num_entries);

    for (i = 0; i < res_event->num_entries; i++)
    {
	/* 
	 * Send out the widget id. 
	 */
	_LesstifEditResPutWidgetInfo(stream, &(res_event->widgets[i]));
	if ((str = VerifyWidget(w, &(res_event->widgets[i]))) != NULL)
	{
	    _LesstifEditResPutBool(stream, True);	/* an error occured. */
	    _LesstifEditResPutString8(stream, str);	/* set message. */
	    XtFree(str);
	}
	else
	{
	    _LesstifEditResPutBool(stream, False);	/* no error occured. */
	    ExecuteGetResources(res_event->widgets[i].real_widget,
				stream);
	}
    }
    return (NULL);
}


/*    Function Name: ExecuteGetResources.
 *    Description: Gets the resources for any individual widget.
 *    Arguments: w - the widget to get resources on.
 *               stream - the protocol stream.
 *    Returns: none.
 */

static void
ExecuteGetResources(Widget w, ProtocolStream *stream)
{
    XtResourceList norm_list, cons_list;
    Cardinal num_norm, num_cons;
    register int i;

    /* 
     * Get Normal Resources. 
     */

    XtGetResourceList(XtClass(w), &norm_list, &num_norm);

    if (XtParent(w) != NULL)
	XtGetConstraintResourceList(XtClass(XtParent(w)),
				    &cons_list, &num_cons);
    else
	num_cons = 0;

    _LesstifEditResPut16(stream, num_norm + num_cons);	/* how many resources. */

    /*
     * Insert all the normal resources.
     */

    for (i = 0; i < (int)num_norm; i++)
    {
	_LesstifEditResPutResourceType(stream, NormalResource);
	_LesstifEditResPutString8(stream, norm_list[i].resource_name);
	_LesstifEditResPutString8(stream, norm_list[i].resource_class);
	_LesstifEditResPutString8(stream, norm_list[i].resource_type);
    }
    XtFree((char *)norm_list);

    /*
     * Insert all the constraint resources.
     */

    if (num_cons > 0)
    {
	for (i = 0; i < (int)num_cons; i++)
	{
	    _LesstifEditResPutResourceType(stream, ConstraintResource);
	    _LesstifEditResPutString8(stream, cons_list[i].resource_name);
	    _LesstifEditResPutString8(stream, cons_list[i].resource_class);
	    _LesstifEditResPutString8(stream, cons_list[i].resource_type);
	}
	XtFree((char *)cons_list);
    }
}


/*
 *    Function Name: DumpValues
 *    Description: Returns resource values to the resource editor.
 *    Arguments: event - the event that caused this action.
 *               stream - the protocol stream to add.
 *    Returns: NULL
 */

static char *
DumpValues(Widget w, EditresEvent *event, ProtocolStream *stream)
{
    Arg warg[1];
    String res_value = NULL;
    GetValuesEvent *gv_event = (GetValuesEvent *)event;
    char buffer[64], *str;

    res_value = buffer;

    /* put the count in the stream. */

    _LesstifEditResPut16(stream, (unsigned int)1);

    /* get the resource of the widget asked for by the */
    /* resource editor and insert it into the stream */
    XtSetArg(warg[0], gv_event->name, &res_value);
/*_XtGetStringValues(gv_event->widgets[0].ids[0], warg, 1);*/
/* New Editres stuff here */
    if ((str = VerifyWidget(w, &(gv_event->widgets[0]))) != NULL)
    {
	_LesstifEditResPutString8(stream, str);
	XtFree(str);
    }
    else
    {
	_LesstifEditResGetStringValues(gv_event->widgets[0].real_widget, warg, 1);
	if (!res_value)
	    res_value = "NoValue";
	_LesstifEditResPutString8(stream, res_value);
    }
/* End New Editres stuff */
    return (NULL);
}


/************************************************************
 *
 * Code for inserting values into the protocol stream.
 *
 ************************************************************/

/*    Function Name: InsertWidget
 *    Description: Inserts the full parent heirarchy of this
 *                 widget into the protocol stream as a widget list.
 *    Arguments: stream - the protocol stream.
 *               w - the widget to insert.
 *    Returns: none
 */

static void
InsertWidget(ProtocolStream * stream, Widget w)
{
    Widget temp;
    unsigned long *widget_list;
    register int i, num_widgets;

    for (temp = w, i = 0; temp != 0; temp = XtParent(temp), i++)
    {
    }

    num_widgets = i;
    widget_list = (unsigned long *)
	XtMalloc(sizeof(unsigned long) * num_widgets);

    /*
     * Put the widgets into the list.
     * make sure that they are inserted in the list from parent -> child.
     */

    for (i--, temp = w; temp != NULL; temp = XtParent(temp), i--)
	widget_list[i] = (unsigned long)temp;

    _LesstifEditResPut16(stream, num_widgets);	/* insert number of widgets. */
    for (i = 0; i < num_widgets; i++)	/* insert Widgets themselves. */
	_LesstifEditResPut32(stream, widget_list[i]);

    XtFree((char *)widget_list);
}


/************************************************************
 *
 * All of the following routines are public.
 *
 ************************************************************/

/*    Function Name: _LesstifEditResPutString8
 *    Description: Inserts a string into the protocol stream.
 *    Arguments: stream - stream to insert string into.
 *               str - string to insert.
 *    Returns: none.
 */

static void
_LesstifEditResPutString8(ProtocolStream *stream, char *str)
{
    int i, len = strlen(str);

    _LesstifEditResPut16(stream, len);
    for (i = 0; i < len; i++, str++)
	_LesstifEditResPut8(stream, *str);
}


/*    Function Name: _LesstifEditResPut8
 *    Description: Inserts an 8 bit integer into the protocol stream.
 *    Arguments: stream - stream to insert string into.
 *               value - value to insert.
 *    Returns: none
 */

static void
_LesstifEditResPut8(ProtocolStream *stream, unsigned int value)
{
    unsigned char temp;

    if (stream->size >= stream->alloc)
    {
	stream->alloc += 100;
	stream->real_top = (unsigned char *)XtRealloc(
						      (char *)stream->real_top,
						  stream->alloc + HEADER_SIZE);
	stream->top = stream->real_top + HEADER_SIZE;
	stream->current = stream->top + stream->size;
    }

    temp = (unsigned char)(value & BYTE_MASK);
    *((stream->current)++) = temp;
    (stream->size)++;
}

/*    Function Name: _LesstifEditResPut16
 *    Description: Inserts a 16 bit integer into the protocol stream.
 *    Arguments: stream - stream to insert string into.
 *               value - value to insert.
 *    Returns: void
 */

static void
_LesstifEditResPut16(ProtocolStream *stream, unsigned int value)
{
    _LesstifEditResPut8(stream, (value >> XER_NBBY) & BYTE_MASK);
    _LesstifEditResPut8(stream, value & BYTE_MASK);
}


/*    Function Name: _LesstifEditResPut32
 *    Description: Inserts a 32 bit integer into the protocol stream.
 *    Arguments: stream - stream to insert string into.
 *               value - value to insert.
 *    Returns: void
 */

static void
_LesstifEditResPut32(ProtocolStream *stream, unsigned long value)
{
    int i;

    for (i = 3; i >= 0; i--)
	_LesstifEditResPut8(stream, (value >> (XER_NBBY * i)) & BYTE_MASK);
}


/*    Function Name: _LesstifEditResPutWidgetInfo
 *    Description: Inserts the widget info into the protocol stream.
 *    Arguments: stream - stream to insert widget info into.
 *               info - info to insert.
 *    Returns: none
 */

static void
_LesstifEditResPutWidgetInfo(ProtocolStream *stream, WidgetInfo *info)
{
    unsigned int i;

    _LesstifEditResPut16(stream, info->num_widgets);
    for (i = 0; i < info->num_widgets; i++)
	_LesstifEditResPut32(stream, info->ids[i]);
}


/************************************************************
 *
 * Code for retrieving values from the protocol stream.
 *
 ************************************************************/


/*    Function Name: _LesstifEditResResetStream
 *    Description: resets the protocol stream
 *    Arguments: stream - the stream to reset.
 *    Returns: none.
 */

static void
_LesstifEditResResetStream(ProtocolStream *stream)
{
    stream->current = stream->top;
    stream->size = 0;
    if (stream->real_top == NULL)
    {
	stream->real_top = (unsigned char *)XtRealloc(
						      (char *)stream->real_top,
						  stream->alloc + HEADER_SIZE);
	stream->top = stream->real_top + HEADER_SIZE;
	stream->current = stream->top + stream->size;
    }
}


/*
 * NOTE NOTE NOTE NOTE NOTE NOTE NOTE NOTE NOTE NOTE NOTE 
 *
 * The only modified field if the "current" field.
 *
 * The only fields that must be set correctly are the "current", "top"
 * and "size" fields.
 */

/*    Function Name: _LesstifEditResGetg8
 *    Description: Retrieves an unsigned 8 bit value
 *                 from the protocol stream.
 *    Arguments: stream.
 *               val - a pointer to value to return.
 *    Returns: True if sucessful.
 */

static Boolean
_LesstifEditResGet8(ProtocolStream *stream, unsigned char *val)
{
    if (stream->size < (unsigned long)(stream->current - stream->top))
	return (False);

    *val = *((stream->current)++);
    return (True);
}


/*    Function Name: _LesstifEditResGet16
 *    Description: Retrieves an unsigned 16 bit value
 *                 from the protocol stream.
 *    Arguments: stream.
 *               val - a pointer to value to return.
 *    Returns: True if sucessful.
 */

static Boolean
_LesstifEditResGet16(ProtocolStream *stream, unsigned short *val)
{
    unsigned char temp1, temp2;

    if (!(_LesstifEditResGet8(stream, &temp1) &&
	  _LesstifEditResGet8(stream, &temp2)))
	return (False);

    *val = (((unsigned short)temp1 << XER_NBBY) + ((unsigned short)temp2));
    return (True);
}


/*      Function Name: _LesstifEditResGetSigned16
 *    Description: Retrieves an signed 16 bit value from the protocol stream.
 *      Arguments: stream.
 *                 val - a pointer to value to return.
 *      Returns: True if sucessful.
 */

static Boolean
_LesstifEditResGetSigned16(ProtocolStream *stream, short *val)
{
    unsigned char temp1, temp2;

    if (!(_LesstifEditResGet8(stream, &temp1) &&
	  _LesstifEditResGet8(stream, &temp2)))
	return (False);

    if (temp1 & (1 << (XER_NBBY - 1)))
    {				/* If the sign bit is active. */
	*val = -1;		/* store all 1's */
	*val &= (temp1 << XER_NBBY);	/* Now and in the MSB */
	*val &= temp2;		/* and LSB */
    }
    else
	*val = (((unsigned short)temp1 << XER_NBBY) + ((unsigned short)temp2));

    return (True);
}


/*    Function Name: _LesstifEditResGet32
 *    Description: Retrieves an unsigned 32 bit value
 *                 from the protocol stream.
 *    Arguments: stream.
 *               val - a pointer to value to return.
 *    Returns: True if sucessful.
 */

static Boolean
_LesstifEditResGet32(ProtocolStream *stream, unsigned long *val)
{
    unsigned short temp1, temp2;

    if (!(_LesstifEditResGet16(stream, &temp1) &&
	  _LesstifEditResGet16(stream, &temp2)))
	return (False);

    *val = (((unsigned short)temp1 << (XER_NBBY * 2)) +
	    ((unsigned short)temp2));
    return (True);
}


/*    Function Name: _LesstifEditResGetString8
 *    Description: Retrieves an 8 bit string value from the protocol stream.
 *    Arguments: stream - the protocol stream
 *               str - the string to retrieve.
 *    Returns: True if retrieval was successful.
 */

static Boolean
_LesstifEditResGetString8(ProtocolStream *stream, char **str)
{
    unsigned short len;
    register unsigned i;

    if (!_LesstifEditResGet16(stream, &len))
    {
	return (False);
    }

    *str = XtMalloc(sizeof(char) * (len + 1));

    for (i = 0; i < len; i++)
    {
	if (!_LesstifEditResGet8(stream, (unsigned char *)*str + i))
	{
	    XtFree(*str);
	    *str = NULL;
	    return (False);
	}
    }
    (*str)[i] = '\0';		/* NULL terminate that sucker. */
    return (True);
}

/*    Function Name: _LesstifEditResGetWidgetInfo
 *    Description: Retrieves the list of widgets that follow and stores
 *                 them in the widget info structure provided.
 *    Arguments: stream - the protocol stream
 *               info - the widget info struct to store into.
 *    Returns: True if retrieval was successful.
 */

static Boolean
_LesstifEditResGetWidgetInfo(ProtocolStream *stream, WidgetInfo *info)
{
    unsigned int i;

    if (!_LesstifEditResGet16(stream, &(info->num_widgets)))
	return (False);

    info->ids = (unsigned long *)XtMalloc(sizeof(long) * (info->num_widgets));

    for (i = 0; i < info->num_widgets; i++)
    {
	if (!_LesstifEditResGet32(stream, info->ids + i))
	{
	    XtFree((char *)info->ids);
	    info->ids = NULL;
	    return (False);
	}
#if defined(LONG64) || defined(WORD64)
	info->ids[i] |= globals.base_address;
#endif
    }
    return (True);
}

/************************************************************
 *
 * Code for Loading the EditresBlock resource.
 *
 ************************************************************/

/*    Function Name: CvStringToBlock
 *    Description: Converts a string to an editres block value.
 *    Arguments: dpy - the display.
 *               args, num_args - **UNUSED **
 *               from_val, to_val - value to convert, and where to put result
 *               converter_data - ** UNUSED **
 *    Returns: True if conversion was sucessful.
 */


static Boolean
CvtStringToBlock(Display *dpy, XrmValue *args, Cardinal *num_args,
		 XrmValue *from_val, XrmValue *to_val,
		 XtPointer *converter_data)
{
    char ptr[BUFSIZ];
    static EditresBlock block;

    XmuCopyISOLatin1Lowered(ptr, from_val->addr);

    if (streq(ptr, "none"))
	block = BlockNone;
    else if (streq(ptr, "setvalues"))
	block = BlockSetValues;
    else if (streq(ptr, "all"))
	block = BlockAll;
    else
    {
	Cardinal num_params = 1;
	String params[1];

	params[0] = from_val->addr;
	XtAppWarningMsg(XtDisplayToApplicationContext(dpy),
			"CvtStringToBlock", "unknownValue", "EditresError",
			"Could not convert string \"%s\" to EditresBlock.",
			params, &num_params);
	return (False);
    }

    if (to_val->addr != NULL)
    {
	if (to_val->size < sizeof(EditresBlock))
	{
	    to_val->size = sizeof(EditresBlock);
	    return (False);
	}
	*(EditresBlock *)(to_val->addr) = block;
    }
    else
	to_val->addr = (XtPointer)block;

    to_val->size = sizeof(EditresBlock);
    return (True);
}

#define XtREditresBlock ("EditresBlock")

/*    Function Name: LoadResources
 *    Description: Loads a global resource the determines of this
 *                 application should allow Editres requests.
 *    Arguments: w - any widget in the tree.
 *    Returns: none.
 */

static void
LoadResources(Widget w)
{
    static XtResource resources[] =
    {
	{"editresBlock", "EditresBlock", XtREditresBlock, sizeof(EditresBlock),
	 XtOffsetOf(Globals, block), XtRImmediate, (XtPointer)BlockNone}
    };

    for (; XtParent(w) != NULL; w = XtParent(w))
    {
    }

    XtAppSetTypeConverter(XtWidgetToApplicationContext(w),
			  XtRString, XtREditresBlock, CvtStringToBlock,
			  NULL, (Cardinal)0, XtCacheAll, NULL);

    XtGetApplicationResources(w, (XtPointer)&globals, resources,
			      XtNumber(resources), NULL, (Cardinal)0);
}


static void
_LesstifEditResCvtWarningHandler(String name,
				 String type,
				 String class,
				 String def,
				 String *params,
				 Cardinal *num_params)
{
    /* just ignore the warning */
    return;
}


static void
_LesstifEditResGetStringValues(Widget w, Arg *warg, int numargs)
{
    XtResourceList res_list;
    Cardinal num_res;
    XtResource *res = NULL;
    long value;
    int size, i;
    char *string = "";
    char *buffer;
    Arg args[1];
    XrmValue to, from, to_color;


    /*
     * Look for the resource.
     */
    XtGetResourceList(XtClass(w), &res_list, &num_res);
    for (i = 0; i < (int)num_res && res == NULL; i++)
    {
	if (0 == strcmp(res_list[i].resource_name, warg->name))
	    res = &res_list[i];
    }
    if (res == NULL && XtParent(w) != NULL)
    {
	XtFree((char *)res_list);
	XtGetConstraintResourceList(XtClass(XtParent(w)), &res_list, &num_res);
    }
    for (i = 0; i < (int)num_res && res == NULL; i++)
    {
	if (0 == strcmp(res_list[i].resource_name, warg->name))
	    res = &res_list[i];
    }

    if (res == NULL)
    {
	/* Couldn't find resource */

	XtFree((char *)res_list);
	*(XtPointer *)(warg->value) = NULL;
	return;
    }

    size = res->resource_size;
    buffer = *(char **)(warg->value);


    /* try to get the value in the proper size */
    switch (res->resource_size)
    {
#if (LONG_BIT == 64)
	long v8;
#endif
	int v4;
	short v2;
	char v1;

    case 1:
	XtSetArg(args[0], res->resource_name, &v1);
	XtGetValues(w, args, 1);
	value = (int)v1;
	break;
    case 2:
	XtSetArg(args[0], res->resource_name, &v2);
	XtGetValues(w, args, 1);
	value = (int)v2;
	break;
    case 4:
	XtSetArg(args[0], res->resource_name, &v4);
	XtGetValues(w, args, 1);
	value = (int)v4;
	break;
#if (LONG_BIT == 64)
    case 8:
	XtSetArg(args[0], res->resource_name, &v8);
	XtGetValues(w, args, 1);
	value = (long)v8;
	break;
#endif
    default:
	fprintf(stderr, "_LesstifEditresGetStringValues: bad size %d\n",
		res->resource_size);
	string = "bad size";
	goto done;
    }


    DEBUGOUT(_LtDebug(__FILE__, w,
		      "_LesstifEditResGetStringValues: converting from %s\n",
		      res->resource_type));

    /*
     * If the resource is already String, no conversion needed.
     */
    if (strcmp(XtRString, res->resource_type) == 0)
    {
	if (value == 0)
	    string = "(null)";
	else
	    string = (char *)value;
    }
    else
    {
	XtErrorMsgHandler old_handler;

	/*
	 * Ignore conversion warnings.
	 */
	old_handler = XtAppSetWarningMsgHandler(XtWidgetToApplicationContext(w),
					     _LesstifEditResCvtWarningHandler);
	from.size = res->resource_size;
	from.addr = (caddr_t)&value;
	to.addr = NULL;
	to.size = 0;
	to_color.addr = NULL;
	to_color.size = 0;
	/*
	 * Special case for type Pixel.
	 */
	if (0 == strcmp(res->resource_type, XtRPixel)
	    && XtConvertAndStore(w, XtRPixel, &from, XtRColor, &to)
	    && XtConvertAndStore(w, XtRColor, &to, XtRString, &to_color))
	{
	    string = to_color.addr;
	}
	else if (XtConvertAndStore(w, res->resource_type,
				   &from, XtRString, &to))
	{
	    string = to.addr;
	}
	else
	{
	    /* 
	     * Conversion failed, fall back to representing it as integer.
	     */
	    switch (res->resource_size)
	    {
		case sizeof(char):
		  sprintf(buffer, "%d", (unsigned char)value);
		string = buffer;
		break;
		case sizeof(short):
		  sprintf(buffer, "%d", (short)value);
		string = buffer;
		break;
		case sizeof(int):
		  sprintf(buffer, "%d", (int)value);
		string = buffer;
		break;
#if (LONG_BIT == 64)
		case sizeof(long):
		  sprintf(buffer, "%ld", value);
		string = buffer;
		break;
#endif
	    default:
		break;
	    }
	    strcat(buffer, " (integer fallback conversion)");
	}
	/*
	 * Restore original warning handler.
	 */
	XtAppSetWarningMsgHandler(XtWidgetToApplicationContext(w), old_handler);
    }


    if (string == NULL)
    {
	/* can't happen */
#ifdef DEBUG
	fprintf(stderr, "_LesstifEditResPutValueString8: couldn't convert to string\n");
	fprintf(stderr, "Class = %s Type = %s Name = %s\n", res->resource_type,
		res->resource_name, res->resource_class);
#endif
	string = "";
    }
  done:
    *(char **)(warg->value) = string;
#ifdef DEBUG
    /* stream isn't defined - this won't compile */
    /*fprintf(stderr, "put %s at %#x\n", string, stream->current); */
#endif

    XtFree((char *)res_list);
}
