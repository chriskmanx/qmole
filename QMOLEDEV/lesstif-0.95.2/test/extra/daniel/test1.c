/* $Header: /cvsroot/lesstif/lesstif/test/extra/daniel/test1.c,v 1.2 2002/05/15 10:55:06 amai Exp $ */
/***********************************************************/
/* Copyright 1996 Daniel Dardailler.  
Permission to use, copy, modify, distribute, and sell this software
for any purpose is hereby granted without fee, provided that the above
copyright notice appear in all copies and that both that copyright
notice and this permission notice appear in supporting documentation,
and that the name of Daniel Dardailler not be used in advertising or
publicity pertaining to distribution of the software without specific,
written prior permission.  Daniel Dardailler makes no representations
about the suitability of this software for any purpose.  It is
provided "as is" without express or implied warranty.
************************************************************/

#include <stdlib.h>
#include <stdio.h>

#include <X11/Xlib.h>
#include <X11/Intrinsic.h>
#include <X11/StringDefs.h>
#include <X11/Xos.h>
#include <X11/Xatom.h>
#include <X11/Shell.h>
#include <X11/Xaw/Label.h>
#include <X11/Xaw/Box.h>
#include <X11/cursorfont.h>
#include "Dnd.h"

/************************************************************
  This demo program creates two Aw labels, one is draggrable, 
  the other is a drop site (change the label when you drop)
  It does a bunch of tracing.
************************************************************/


static void StartDrag(Widget w, XEvent *event,
		      String *params, Cardinal *num_params);
static Atom Dnd_wm_state, Dnd_selection, Dnd_transfer_success ;
static Cursor valid_cursor, invalid_cursor;
static String drag_trans = "<Btn2Down>: StartDrag()" ;
static Atom my_targets[] = { XA_STRING } ;
static Atom cv_targets[3] ;
static XtPointer closures[3] ;
static Boolean in_drag = False ;
static Widget drag_label, drop_label, top_level, box ;
static XtActionsRec drag_actions [] = {{"StartDrag", StartDrag}} ;


static void
SelectTopLevels(
        Display *dpy,
        Window win)
{
    Window 		root, parent;
    Window 		*children;
    unsigned int 	nchildren;
    int		 	i;
    Atom 		type = None;
    int 		format;
    unsigned long 	nitems, after;
    unsigned char 	*data;

    if (win != DefaultRootWindow(dpy)) {
	/* only ask property kid of the root */
	XGetWindowProperty(dpy, win, Dnd_wm_state , 0, 0, False, 
			   AnyPropertyType,
			   &type, &format, &nitems, &after, &data);
	XFree(data);
    } else {
	printf ("WM_STATE on: ");
    }
 
    /* if the window has the WM_STATE property, select D&D events for it */
    if (type) {
	XSelectInput(dpy, win, EnterWindowMask | LeaveWindowMask
		     | Button2MotionMask);
	printf("%d ",win);
    } else {
	if (!XQueryTree(dpy, win, &root, &parent, &children, &nchildren) ||
	    (nchildren == 0))
	  return ;
	for (i = nchildren - 1; i >= 0; i--) {
	    SelectTopLevels(dpy, children[i]) ;
	}
	XFree(children);
    }
    
    if (win == DefaultRootWindow(dpy)) printf("\n----------\n");
}

static void 
ChangeCursorInvalid(Display * dpy, Time time)
{
    printf("ChangeCursorInvalid\n");
    XChangeActivePointerGrab(dpy, 
			     ButtonMotionMask|ButtonReleaseMask|
			     EnterWindowMask|LeaveWindowMask,
			     invalid_cursor, time);
}

static void 
ChangeCursorValid(Display * dpy, Time time)
{
    printf("ChangeCursorValid\n");
    XChangeActivePointerGrab(dpy, 
			     ButtonMotionMask|ButtonReleaseMask|
			     EnterWindowMask|LeaveWindowMask,
			     valid_cursor, time);
}


static void 
StartDrag(
	  Widget	w,
	  XEvent	*event,
	  String *params, Cardinal *num_params)
{
    printf("StartDrag\n");

    /* grab the pointer so that we get all crossing events */
    XGrabPointer(XtDisplay(w), DefaultRootWindow(XtDisplay(w)), True,
		 ButtonMotionMask|ButtonReleaseMask|
		 EnterWindowMask|LeaveWindowMask,
		 GrabModeSync, GrabModeAsync,
		 None, invalid_cursor,
		 event->xbutton.time);

    in_drag = True ;

    XAllowEvents(XtDisplay(w), SyncPointer, event->xbutton.time);
}

static Boolean 
SourceConvert(
        Widget w,
        Atom *selection,
        Atom *target,
        Atom *type,
        XtPointer *value,
        unsigned long *length,
        int *format )
{
    printf("SourceConvert %d %s\n", *target, 
	   XGetAtomName(XtDisplay(w),*target));

    if (*target == XA_STRING) {
	String label ;
	*type   = XA_STRING;
	XtVaGetValues(drag_label, XtNlabel, &label, NULL);
	*value = XtNewString(label);
	*length = strlen(label);
	printf("value %s length %d\n", *value, *length);
	*format = 8;
	return True;
    } else 
    if (*target == Dnd_transfer_success) {
	printf("transfer success\n");
	*type   = Dnd_transfer_success;
	*length = 0 ;
	*value = NULL ;
	*format = 32;
	return True;
    } else 
    if (*target == XInternAtom(XtDisplay(w), "DELETE", False)) {
	*type = XInternAtom(XtDisplay(w), "NULL", False);
	*value = NULL;
	*length = 0;
	*format = 8;
	return True;
    }

  return False;
}

static void 
ReceiverConvert(
        Widget w,
        XtPointer client_data,	
        Atom *selection,
        Atom *type,		
        XtPointer val,
        unsigned long *length,	
        int *format )		
{
    printf("ReceiverConvert selection %d %s\n",
	   *selection, XGetAtomName(XtDisplay(w), *selection));
    printf("type %d %s\n",*type, 
	   (*type)?XGetAtomName(XtDisplay(w), *type):"00");
    printf("format %d\n",*format);
    printf("length %d\n",*length);

    if (*type != 0 && *type != Dnd_transfer_success) {
	XtVaSetValues(drop_label, XtNlabel, val, NULL);

	/* copy data received in our drop site */
	/* indicate success by asking the source to convert 
	   XmTRANSFER_SUCCESS */
	XtGetSelectionValue(w, *selection,
			    Dnd_transfer_success, ReceiverConvert, NULL, 
			    CurrentTime);
	/* we should get called again with this target */
    } 
    /* else, got transfer success back, free whatever we want */

 /* Note: I get some weird 
    Warning: 
      Name: dd
      Class: ApplicationShell
      We lost the drop selection
    on a (real) Motif initiator side when I get to this point... 
    No time to investigate right now */    
}

int
main(argc, argv)
	int argc;
	char **argv;
{
    XtAppContext app_context;
    unsigned char src_protocol_style = DND_DRAG_DYNAMIC ;
    char my_dnd_selection_name[30] ;
    Boolean do_messaging = False, do_drop = False, in_drop_site = False ;
    Window cur_window = 0 ;
    Atom * src_targets ;
    unsigned short num_src_targets ;
    Position dropx, dropy ;
    Dimension dropw, droph ;

    top_level = XtVaAppInitialize(&app_context, "DndTest", 
				  NULL, 0, &argc, argv, NULL, 
				  XtNallowShellResize, True, NULL); 
    XtAppAddActions(app_context, (XtActionList)drag_actions, 
		    XtNumber(drag_actions));

    /* Init atoms */
    Dnd_wm_state =   XInternAtom(XtDisplay(top_level), "WM_STATE", False);
    Dnd_transfer_success =   XInternAtom(XtDisplay(top_level), 
					 "XmTRANSFER_SUCCESS", False);

    /* This one needs to be unique for each drag, not just for each client,
       as there can be race condition with drag transfer not finish while
       another drag is started */
    sprintf(my_dnd_selection_name, "_MY_DND_SELECTION_%d", getpid());
    Dnd_selection =  XInternAtom(XtDisplay(top_level), 
				 my_dnd_selection_name, False);


    invalid_cursor = XCreateFontCursor(XtDisplay(top_level),XC_pirate);
    valid_cursor = XCreateFontCursor(XtDisplay(top_level), XC_target);

    box = XtCreateManagedWidget("box",
				boxWidgetClass, top_level, NULL, 0);

    drag_label = XtVaCreateManagedWidget("drag",
				  labelWidgetClass, box, NULL);
    XtOverrideTranslations(drag_label, XtParseTranslationTable(drag_trans));

    drop_label = XtVaCreateManagedWidget("drop",
				    labelWidgetClass, box, NULL);

    XtRealizeWidget(top_level); /* need the windows right below */


     /* register as a drag source. In real life, one would have to 
	update the property per drag source, for real target indication */
    DndWriteSourceProperty(XtDisplay(top_level), XtWindow(top_level),
			   Dnd_selection, my_targets, 1);

    /* gotta be ready to convert our selection - same thing */
    XtOwnSelection(top_level, Dnd_selection, CurrentTime, 
		   SourceConvert, NULL, NULL);
		 
    if (argc > 1) {
	if (strcmp(argv[1], "none") == 0) 
	    src_protocol_style = DND_DRAG_NONE ;
	else
	if (strcmp(argv[1], "drop_only") == 0) 
	    src_protocol_style = DND_DRAG_DROP_ONLY;
	else
	if (strcmp(argv[1], "dynamic") == 0) 
	    src_protocol_style = DND_DRAG_DYNAMIC ;
    }

    /* register the top_level as a drop site too */
    DndWriteReceiverProperty(XtDisplay(top_level), XtWindow(top_level),
			     src_protocol_style);

    /* really just need to be done on start drag (and tracked too as window
       are added, destroyed. Motif uses private properties to find these */
    SelectTopLevels(XtDisplay(top_level), 
		   DefaultRootWindow(XtDisplay(top_level))); 

    /* for now use my own loop for tracking events directly, 
       rather messy... */
    while(1) {
	XEvent event ;
	unsigned char receiv_protocol_style;
	XClientMessageEvent cm ;
	DndData dnd_data ;
	char receiver ;

	XtAppNextEvent(app_context, &event);

	switch(event.type) {

	case EnterNotify:
	    if (!in_drag) break ;

	    printf("enter win %d sub %d\n", event.xcrossing.window,
		   event.xcrossing.subwindow);

	    cur_window = event.xcrossing.window ;

	    /* just enter a new top_level, check its dnd data */
	    DndReadReceiverProperty(event.xcrossing.display,
				    event.xcrossing.window,
				    &receiv_protocol_style);

	    printf("receiv_protocol_style %d\n",receiv_protocol_style);

	    if (receiv_protocol_style == DND_DRAG_NONE) {
		ChangeCursorInvalid(event.xcrossing.display, 
				    event.xcrossing.time) ;
		do_messaging = False ;
		do_drop = False ;
	    } else 
	    if (receiv_protocol_style == DND_DRAG_DROP_ONLY) {
		ChangeCursorValid(event.xcrossing.display, 
				  event.xcrossing.time) ;
		do_messaging = False ;
		do_drop = True ;
	    } else 
            if (receiv_protocol_style == DND_DRAG_DYNAMIC) {

		/* we'll get valid visual on drop_site_entered */
		ChangeCursorInvalid(event.xcrossing.display, 
				    event.xcrossing.time) ; 

		dnd_data.reason = DND_TOP_LEVEL_ENTER ;
		dnd_data.time = event.xcrossing.time ;
		dnd_data.src_window = XtWindow(top_level);
		dnd_data.property = Dnd_selection ;

		DndFillClientMessage (event.xcrossing.display,
				      cur_window,
				      &cm, &dnd_data, 0);

		XSendEvent(event.xcrossing.display,
			   cur_window, False, 0, (XEvent *)&cm) ;

		printf("XSendEvent DND_TOP_LEVEL_ENTER %d\n", cur_window);

		do_messaging = True ;
		do_drop = False ;
	    }
	    break ;
	case LeaveNotify:
	    if (!in_drag || !do_messaging) break ;

	    printf("leave win %d sub %d\n", event.xcrossing.window, 
		   event.xcrossing.subwindow);

	    ChangeCursorInvalid(event.xcrossing.display, 
				event.xcrossing.time) ; 

	    dnd_data.reason = DND_TOP_LEVEL_LEAVE ;
	    dnd_data.time = event.xcrossing.time ;
	    dnd_data.src_window = XtWindow(top_level);

	    DndFillClientMessage (event.xcrossing.display,
				  cur_window,
				  &cm, &dnd_data, 0);

	    XSendEvent(event.xcrossing.display,
		       cur_window, False, 0, (XEvent *)&cm) ;

	    printf("XSendEvent DND_TOP_LEVEL_LEAVE %d\n", 
		   cur_window);

	    do_messaging = False ;
	    cur_window = 0 ;

	    break ;
	case MotionNotify:
	    if (!in_drag || !do_messaging) break ;

	    printf("motion win %d sub %d\n", event.xmotion.window, 
		   event.xmotion.subwindow);
		
	    dnd_data.reason = DND_DRAG_MOTION ;
	    dnd_data.time = event.xmotion.time ;
	    dnd_data.operation = DND_MOVE|DND_COPY;
	    dnd_data.operations = DND_MOVE|DND_COPY;
	    dnd_data.x = event.xmotion.x_root ;
	    dnd_data.y = event.xmotion.y_root ;

	    DndFillClientMessage (event.xmotion.display,
				  cur_window,
				  &cm, &dnd_data, 0);

	    XSendEvent(event.xmotion.display,
		       cur_window, False, 0, (XEvent *)&cm) ;
	    
	    printf("XSendEvent DND_DRAG_MOTION %d\n", cur_window);

	    break ;
	case ButtonRelease:
	    if (!in_drag) break ;

	    printf("release in %d\n",cur_window);
	    XUngrabPointer(XtDisplay(top_level), event.xbutton.time);
	    in_drag = False ;

	    if (do_drop) {

		dnd_data.reason = DND_DROP_START ;
		dnd_data.time = event.xbutton.time ;
		dnd_data.operation = DND_MOVE|DND_COPY;
		dnd_data.operations = DND_MOVE|DND_COPY;
		dnd_data.src_window = XtWindow(top_level);
		dnd_data.property = Dnd_selection ;

		DndFillClientMessage (event.xbutton.display,
				      cur_window,
				      &cm, &dnd_data, 0);

		XSendEvent(event.xbutton.display,
			  cur_window, False, 0, (XEvent *)&cm) ;

		printf("XSendEvent DND_DROP_START %d\n", cur_window);

		do_messaging = False ;
		do_drop = False ;
	    }
	    break ;
	case ClientMessage:

	    if (!(DndParseClientMessage ((XClientMessageEvent*)&event, 
					 &dnd_data, &receiver))) {
		printf("not a valid Dnd client message\n");
		break ;
	    }

	    if (dnd_data.reason == DND_DRAG_MOTION) {
		if (receiver) {
		    printf("receiver echoing drag motion\n");
		    /* don't need to do anything, really, since
		       we're not supporting better_x,y position */
		} else {
		    printf("source sending a drag motion\n");
		    /* check if in drop site, and depending on the state,
		       send a drop site enter or drop site leave or echo */
		    printf("x %d y %d\n",dnd_data.x, dnd_data.y);
		    if (dnd_data.x > dropx && dnd_data.y > dropy && 
			dnd_data.x < dropx + (Position)dropw &&
			dnd_data.y < dropy + (Position)droph) {
			if (!in_drop_site) {
			    in_drop_site = True ;

			    dnd_data.reason = DND_DROP_SITE_ENTER ;
			    dnd_data.time = CurrentTime ;
			    dnd_data.operation = DND_MOVE|DND_COPY;
			    dnd_data.operations = DND_MOVE|DND_COPY;

			    DndFillClientMessage (event.xclient.display,
						  cur_window,
						  &cm, &dnd_data, 0);

			    XSendEvent(event.xbutton.display,
				       cur_window, False, 0, 
				       (XEvent *)&cm) ;


			    printf("XSendEvent DND_DROP_SITE_ENTER %d\n", 
				   cur_window);

			} else {
			    dnd_data.reason = DND_DRAG_MOTION ;
			    dnd_data.time = CurrentTime ;
			    dnd_data.operation = DND_MOVE|DND_COPY;
			    dnd_data.operations = DND_MOVE|DND_COPY;

			    DndFillClientMessage (event.xclient.display,
						  cur_window,
						   &cm, &dnd_data, 0);

			    XSendEvent(event.xbutton.display,
				       cur_window, False, 0, 
				       (XEvent *)&cm) ;


			    printf("XSendEvent DND_DRAG_MOTION %d\n", 
				   cur_window);

			}
		    } else {
			if (in_drop_site) {
			    in_drop_site = False ;

			    dnd_data.reason = DND_DROP_SITE_LEAVE ;
			    dnd_data.time = CurrentTime ;

			    DndFillClientMessage (event.xclient.display,
						  cur_window,
						  &cm, &dnd_data, 0);

			    XSendEvent(event.xbutton.display,
				       cur_window, False, 0, 
				       (XEvent *)&cm) ;


			    printf("XSendEvent DND_DROP_SITE_LEAVE %d\n", 
				   cur_window);
			}
		    }
		}
	    } else
	    if (dnd_data.reason == DND_TOP_LEVEL_ENTER) {
		/* get the size of our drop site for later use */
		XtVaGetValues(drop_label, 
			      XtNwidth, &dropw, XtNheight, &droph, NULL);
		XtTranslateCoords(drop_label, 0, 0, &dropx, &dropy);
		printf("x %d y %d w %d h %d\n", dropx, dropy, dropw, droph);

		printf("source sending a top level enter %d\n",
		       dnd_data.src_window);

		cur_window = dnd_data.src_window ;
		
		/* no answer needed, just read source property */
		DndReadSourceProperty (event.xclient.display,
				       cur_window,
				       dnd_data.property,
				       &src_targets, &num_src_targets);
		printf("src_targets %d num_src_targets %d\n",
		       src_targets[0], num_src_targets);
		/* we only support string for now */
		if (num_src_targets && src_targets[0] == XA_STRING) 
		    printf("src target ok\n");		    
	    } else
	    if (dnd_data.reason == DND_TOP_LEVEL_LEAVE) {
		printf("source sending a top level leave\n");
		cur_window = 0 ;
		/* no need to do anything */
	    } else
	    if (dnd_data.reason == DND_DROP_SITE_ENTER) {
		printf("receiver sending drop site enter\n");
		ChangeCursorValid(event.xclient.display, CurrentTime);
		do_drop = True ;
	    } else
	    if (dnd_data.reason == DND_DROP_SITE_LEAVE) {
		printf("receiver sending drop site leave\n");
		ChangeCursorInvalid(event.xclient.display, CurrentTime);
		do_drop = False ;
	    } else 
	    if (dnd_data.reason == DND_OPERATION_CHANGED) {
		if (receiver) {
		    printf("receiver echoing operation changed\n");
		} else {
		    printf("source sending an operation changed\n");
		    /* need to echo */
		}
	    } else
	    if (dnd_data.reason == DND_DROP_START) {
		if (receiver) {
		    printf("receiver echoing drop start\n");
		} else {
		    printf("source sending a drop start\n");

		    if (!in_drop_site) break ;

		    /* need to echo and then request a convert */
		    dnd_data.reason = DND_DROP_START ;
		    dnd_data.time = CurrentTime ;

		    DndFillClientMessage (event.xclient.display,
					  cur_window,
					  &cm, &dnd_data, 0);

		    XSendEvent(event.xbutton.display,
			       cur_window, False, 0, 
			       (XEvent *)&cm) ;


		    printf("XSendEvent DND_DROP_START %d\n", 
			   cur_window);

		    /* ask for a convertion - the selection name is
		     the same as the property on which the source
		     stored its target, weird, but that's the way it 
		     was done in Motif... */
		    cv_targets[0] = XA_STRING ;
		    cv_targets[1] = XInternAtom(XtDisplay(top_level), 
						"TEXT", False);
		    cv_targets[2] = XInternAtom(XtDisplay(top_level), 
						"COMPOUND_TEXT", False);
		    XtGetSelectionValues(top_level, dnd_data.property,
					 cv_targets, XtNumber(cv_targets), 
					 ReceiverConvert, closures, 
					 CurrentTime);
					
		}
		printf("drop action %d\n", dnd_data.completion);
	    }
	    break ;
	default:
	    break ;
	}

	XtDispatchEvent(&event);
    }
    
    exit(0);
}
