/**
 *
 * $Header: /cvsroot/lesstif/lesstif/test/common/Test.c,v 1.37 2002/05/01 15:19:15 amai Exp $
 *
 * Copyright (C) 1996 Free Software Foundation, Inc.
 * Copyright (C) 1996-2002 LessTif Development Team
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

#include "LTTconfig.h"
 
#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include <X11/IntrinsicP.h>
#include <X11/StringDefs.h>
#include <X11/keysym.h>
#include <X11/Core.h>

#define LIB_LTTEST
#include "Test.h"

/* ************** */

int GlobalErrors = 0;

/* local prototypes */
static void Initialize(Widget w);
static void Delay(Widget w);
static void Exit(Widget w);
static void Popup(Widget w, Boolean *mapped, XEvent *event);
static void TimeOut(Boolean *mapped);
static void LessTifTestPrintEvent(Widget w, XEvent *event);
static void LessTifTestProcessEvent(Widget w);

typedef struct
{
    Boolean autoExit;
    int exitDelay;
    int geometrySlop;
    Boolean printEvents;
    String dumpOnFailureFile;
} AppResources_t, *AppResourcesPtr;
static AppResources_t AppResources;

static XtResource resources[] =
{
    {"autoExit", "AutoExit", XtRBoolean, sizeof(Boolean), XtOffset(AppResourcesPtr, autoExit), XtRImmediate, (void *)False},
    {"exitDelay", "ExitDelay", XtRInt, sizeof(int), XtOffset(AppResourcesPtr, exitDelay), XtRImmediate, (void *)0},
    {"geometrySlop", "GeometrySlop", XtRInt, sizeof(int), XtOffset(AppResourcesPtr, geometrySlop), XtRImmediate, (void *)0},
    {"printEvents", "PrintEvents", XtRBoolean, sizeof(Boolean), XtOffset(AppResourcesPtr, printEvents), XtRImmediate, (void *)False},
    {"dumpFile", "DumpFile", XtRString, sizeof(String), XtOffset(AppResourcesPtr, dumpOnFailureFile), XtRImmediate, (void *)NULL},
};

/* ************** */

extern const char *
XdbGeometryResult2String(XtGeometryResult r)
{
    switch (r)
    {
    case XtGeometryYes:
	return "Yes";

    case XtGeometryNo:
	return "No";

    case XtGeometryAlmost:
	return "Almost";

    case XtGeometryDone:
	return "Done";

    default:
	return "(invalid geometry result)";
    }
}

/* ************** */

extern const char *
XdbWidgetGeometry2String(XtWidgetGeometry *g)
{
    static char o1[128], o2[128], b[20], *out = NULL;
    int i;

    if (g == NULL)
    {
	return "NULL_GEOMETRY";
    }

    if (g->request_mode == 0)
    {
	return "GEOMETRY_NO_FIELDS";
    }

/* Some magic to ensure you can call this sucker twice in one C function call */
    if (out == &o1[0])
    {
	out = &o2[0];
    }
    else
    {
	out = &o1[0];
    }

    out[0] = '\0';
    if (g->request_mode & CWX)
    {
	sprintf(b, "x %d ", g->x);
	strcat(out, b);
    }
    if (g->request_mode & CWY)
    {
	sprintf(b, "y %d ", g->y);
	strcat(out, b);
    }
    if (g->request_mode & CWWidth)
    {
	sprintf(b, "w %d ", g->width);
	strcat(out, b);
    }
    if (g->request_mode & CWHeight)
    {
	sprintf(b, "h %d ", g->height);
	strcat(out, b);
    }
    if (g->request_mode & CWBorderWidth)
    {
	sprintf(b, "bw %d ", g->border_width);
	strcat(out, b);
    }

    for (i = 0; out[i]; i++)
    {
    }

    if (i > 0 && out[i - 1] == ' ')
    {
	out[i - 1] = '\0';
    }

    return out;
}


extern const char *
XdbBoolean2String(int b)
{
    if (b)
    {
	return "True";
    } else
    {
	return "False";
    }
}


/* ************** */

static void
Initialize(Widget w)
{
    static Boolean inited = False;

    if (!inited)
    {
      Display *display = XtIsSubclass(w, coreWidgetClass) ? XtDisplay(w) : XtDisplay(XtParent(w));

	XtGetApplicationResources(w,
				  &AppResources,
				  resources, XtNumber(resources),
				  NULL, 0);
	XSynchronize(display, True);
	inited = True;
    }
}

/* ************** */

extern int
LessTifTestGetSlop(Widget w)
{
    Initialize(w);
    return (AppResources.geometrySlop);
}

/* ************** */

extern void
LessTifTestSetSlop(Widget w, int slop)
{
    Initialize(w);
    AppResources.geometrySlop = slop;
}

/* ************** */

static void
Exit(Widget w)
{
/*    Display *display = 
      XtIsSubclass(w,coreWidgetClass) ? XtDisplay(w) : XtDisplay(XtParent(w));
    XtAppContext app_context=XtWidgetToApplicationContext(w); */

    /* destroy application and reclaim memory */ 

    /* works, but leaks memory */
    XtDestroyWidget(w);
    /* Adding this call causes a core dump in DeleteShellFromHookObj */
    /* XtDestroyApplicationContext(app_context);  */

    /* XCloseDisplay(display); */

    printf("exit status >%i<\n", GlobalErrors);
    exit(GlobalErrors);
}

/* ************** */

static void
LessTifTestPrintEvent(Widget w, XEvent *event)
{
    Display *display = XtIsSubclass(w, coreWidgetClass) ? XtDisplay(w) : XtDisplay(XtParent(w));

	switch (event->xany.type)
	{
	case KeyPress:
	    fprintf(stdout, "KeyPress\n");
	    break;
	case KeyRelease:
	    fprintf(stdout, "KeyRelease\n");
	    break;
	case ButtonPress:
	    fprintf(stdout, "ButtonPress %s %ld\n",
	    	XtName(XtWindowToWidget(display, event->xany.window)),
	    	event->xany.serial
	    	);
	    break;
	case ButtonRelease:
	    fprintf(stdout, "ButtonRelease %s %ld\n",
	    	XtName(XtWindowToWidget(display, event->xany.window)),
	    	event->xany.serial
	    	);
	    break;
	case MotionNotify:
	    fprintf(stdout, "MotionNotify\n");
	    break;
	case EnterNotify:
	    fprintf(stdout, "EnterNotify %s %ld\n",
	    	XtName(XtWindowToWidget(display, event->xany.window)),
	    	event->xany.serial
	    	);
	    break;
	case LeaveNotify:
	    fprintf(stdout, "LeaveNotify %s %ld\n",
	    	XtName(XtWindowToWidget(display, event->xany.window)),
	    	event->xany.serial
	    	);
	    break;
	case FocusIn:
	    fprintf(stdout, "FocusIn %s %ld\n",
	    	XtName(XtWindowToWidget(display, event->xany.window)),
	    	event->xany.serial
	    	);
	    break;
	case FocusOut:
	    fprintf(stdout, "FocusOut %s %ld\n",
	    	XtName(XtWindowToWidget(display, event->xany.window)),
	    	event->xany.serial
	    	);
	    break;
	case KeymapNotify:
	    fprintf(stdout, "KeymapNotify\n");
	    break;
	case Expose:
	    fprintf(stdout, "Expose %s %ld\n",
	    	XtName(XtWindowToWidget(display, event->xany.window)),
	    	event->xany.serial
	    	);
	    break;
	case GraphicsExpose:
	    fprintf(stdout, "GraphicsExpose\n");
	    break;
	case NoExpose:
	    fprintf(stdout, "NoExpose\n");
	    break;
	case VisibilityNotify:
	    fprintf(stdout, "VisibilityNotify\n");
	    break;
	case CreateNotify:
	    fprintf(stdout, "CreateNotify\n");
	    break;
	case DestroyNotify:
	    fprintf(stdout, "DestroyNotify\n");
	    break;
	case UnmapNotify:
	    fprintf(stdout, "UnmapNotify %s %ld\n",
	    	XtName(XtWindowToWidget(display, event->xany.window)),
	    	event->xany.serial
	    	);
	    break;
	case MapNotify:
	    fprintf(stdout, "MapNotify %s %ld\n",
	    	XtName(XtWindowToWidget(display, event->xany.window)),
	    	event->xany.serial
	    	);
	    break;
	case MapRequest:
	    fprintf(stdout, "MapRequest\n");
	    break;
	case ReparentNotify:
	    fprintf(stdout, "ReparentNotify\n");
	    break;
	case ConfigureNotify:
	    fprintf(stdout, "ConfigureNotify %s %ld\n",
	    	XtName(XtWindowToWidget(display, event->xany.window)),
	    	event->xany.serial
	    	);
	    break;
	case ConfigureRequest:
	    fprintf(stdout, "ConfigureRequest\n");
	    break;
	case GravityNotify:
	    fprintf(stdout, "GravityNotify\n");
	    break;
	case ResizeRequest:
	    fprintf(stdout, "ResizeRequest\n");
	    break;
	case CirculateNotify:
	    fprintf(stdout, "CirculateNotify\n");
	    break;
	case CirculateRequest:
	    fprintf(stdout, "CirculateRequest\n");
	    break;
	case PropertyNotify:
	    fprintf(stdout, "PropertyNotify\n");
	    break;
	case SelectionClear:
	    fprintf(stdout, "SelectionClear\n");
	    break;
	case SelectionRequest:
	    fprintf(stdout, "SelectionRequest\n");
	    break;
	case SelectionNotify:
	    fprintf(stdout, "SelectionNotify\n");
	    break;
	case ColormapNotify:
	    fprintf(stdout, "ColormapNotify\n");
	    break;
	case ClientMessage:
	    fprintf(stdout, "ClientMessage\n");
	    break;
	case MappingNotify:
	    fprintf(stdout, "MappingNotify\n");
	    break;
	case LASTEvent:
	    fprintf(stdout, "LASTEvent\n");
	    break;

	default:
	    fprintf(stdout, "Unknown event\n");
	    break;
	}
}

/* ************** */

static void
LessTifTestProcessEvent(Widget w)
{
	XEvent event;

	if (XtIMXEvent == XtAppPending(XtWidgetToApplicationContext(w)))
	{
	    XtAppNextEvent(XtWidgetToApplicationContext(w), &event);
	    if (AppResources.printEvents)
	    {
		LessTifTestPrintEvent(w, &event);
	    }
	    if (XtDispatchEvent(&event))
	    {
	    }
	    else
	    {
		if (AppResources.printEvents)
		{
		    printf("\tNot Dispatched\n");
		}
	    }
	}
	else
	{
	    XtAppProcessEvent(XtWidgetToApplicationContext(w), XtIMTimer | XtIMAlternateInput);
	}
}

/* ************** */

static void
Popup(Widget w, Boolean *mapped, XEvent *event)
{
    if (event->type == MapNotify)
    {
	*mapped = True;
    }
}

/* ************** */

static void
TimeOut(Boolean *mapped)
{
    *mapped = True;
}

/* ************** */

extern void
LessTifTestDelay(Widget w, unsigned long interval)
{
    Boolean mapped = False;
    XtAppContext app_context=XtWidgetToApplicationContext(w);

    XtAppAddTimeOut(app_context, interval,
                    (XtTimerCallbackProc)TimeOut, (XtPointer)&mapped); 
    while (!mapped)
    {
	XtAppProcessEvent(app_context, XtIMTimer);
    }
}

/* ************** */

static void
Delay(Widget w)
{
    LessTifTestDelay(w, AppResources.exitDelay);
}

/* ************** */

extern void
LessTifTestFlushEvents(Widget w)
{
    Display *display = XtIsSubclass(w, coreWidgetClass) ? XtDisplay(w) : XtDisplay(XtParent(w));

    XSync(display, False);
    while (XtAppPending(XtWidgetToApplicationContext(w)))
    {
	LessTifTestProcessEvent(w);
	XFlush(display);
    }
    Delay(w);
}

/* ************** */

extern void
LessTifTestPrintEvents(Widget w, Boolean flag)
{
	LessTifTestFlushEvents(w);
	AppResources.printEvents = flag;
}

/* ************** */

extern int
LessTifTestResizeWidget(Widget w, Dimension wt, Dimension ht)
{
    Dimension bw;

    Initialize(w);
    XtVaGetValues(w,
		  XtNborderWidth, &bw,
		  NULL);
    XtResizeWidget(w, wt, ht, bw);
    LessTifTestFlushEvents(w);
    return (0);
}

/* ************** */

extern int
LessTifTestWaitForIt(Widget w)
{
    XWindowAttributes window_attributes;
    Boolean mapped = False;
    Display *display = XtIsSubclass(w, coreWidgetClass) ? XtDisplay(w) : XtDisplay(XtParent(w));
    Window window = XtIsSubclass(w, coreWidgetClass) ? XtWindow(w) : XtWindow(XtParent(w));

    Initialize(w);
    XGetWindowAttributes(display, window, &window_attributes);
    if (window_attributes.map_state == IsUnmapped)
    {

	XtAddEventHandler(w, StructureNotifyMask, False,
	                  (XtEventHandler)Popup, (XtPointer)&mapped);
	XSync(display, False);
	while (!mapped || XtAppPending(XtWidgetToApplicationContext(w)))
	{
	    XtAppProcessEvent(XtWidgetToApplicationContext(w), XtIMAll);
	    XFlush(display);
	}
	XtRemoveEventHandler(w, StructureNotifyMask, False,
	                     (XtEventHandler)Popup, (XtPointer)&mapped);
    }
    Delay(w);
    return (0);
}

/* ************** */

extern int
LessTifTestMainLoop(Widget w)
{
    Initialize(w);
    if (!AppResources.autoExit)
    {
	XtAppMainLoop(XtWidgetToApplicationContext(w));
    }
    else
    {
	LessTifTestWaitForIt(w);
	Delay(w);
	if (AppResources.dumpOnFailureFile && GlobalErrors)
	{
	String cmd;
	String format = "xwd -id 0x%x | xwdtopnm | ppmtogif -interlace >%s";

	    cmd = XtMalloc(strlen(AppResources.dumpOnFailureFile) + strlen(format) + 20);
	    sprintf(cmd, format, XtWindow(w), AppResources.dumpOnFailureFile);
	    system(cmd);
	    XtFree(cmd);
	}
	Exit(w);
    }
    return (0);
}

/* ************** */

extern void
LessTifTestWarpPointerAbove(Widget w)
{
    Dimension width, height;
    Position x, y;
    Window window = XtIsSubclass(w, coreWidgetClass) ? XtWindow(w) : XtWindow(XtParent(w));
    Display *display = XtIsSubclass(w, coreWidgetClass) ? XtDisplay(w) : XtDisplay(XtParent(w));

    XtVaGetValues(w,
		  XtNwidth, &width,
		  XtNheight, &height,
		  XtNx, &x,
		  XtNy, &y,
		  NULL);
    if (XtIsSubclass(w, coreWidgetClass))
    {
	x = y = 0;
    }
    XWarpPointer(display, None, window, 0, 0, 0, 0, x + (width / 2), y - 1);
    LessTifTestFlushEvents(w);
    Delay(w);
}

/* ************** */

extern void
LessTifTestWarpPointer(Widget w)
{
    Dimension width, height;
    Position x, y;
    Window window = XtIsSubclass(w, coreWidgetClass) ? XtWindow(w) : XtWindow(XtParent(w));
    Display *display = XtIsSubclass(w, coreWidgetClass) ? XtDisplay(w) : XtDisplay(XtParent(w));

    XtVaGetValues(w,
		  XtNwidth, &width,
		  XtNheight, &height,
		  XtNx, &x,
		  XtNy, &y,
		  NULL);
    if (XtIsSubclass(w, coreWidgetClass))
    {
	x = y = 0;
    }
    XWarpPointer(display, None, window, 0, 0, 0, 0, x + (width / 2), y + (height / 2));
    LessTifTestFlushEvents(w);
    Delay(w);
}

/* ************** */

extern void
LessTifTestKeyRelease(Widget w, KeySym keysym, unsigned int state)
{
    Display *display = XtIsSubclass(w, coreWidgetClass) ? XtDisplay(w) : XtDisplay(XtParent(w));
    Window window = XtIsSubclass(w, coreWidgetClass) ? XtWindow(w) : XtWindow(XtParent(w));
    XKeyEvent key_event =
    {
	KeyRelease,
	0,
	False,
	NULL, /* display, */
	None, /* window, */
	None, /* root, */
	None, /* subwindow, */
	CurrentTime,
	0, 0,
	0, 0,
	0,
	0x09,
	True
    };
    XEvent *event;
    unsigned int keys_buttons;

    key_event.keycode = XKeysymToKeycode(display, keysym);
    key_event.display=display;
    key_event.state=state;
    key_event.window=key_event.root=key_event.subwindow=window;
    LessTifTestWarpPointer(w);
    XQueryPointer(display, window,
		  &key_event.root,
		  &key_event.subwindow,
		  &key_event.x_root, &key_event.y_root,
		  &key_event.x, &key_event.y,
		  &keys_buttons);
    event = (XEvent*)&key_event;
    event->xany.serial = NextRequest(display);
    event->xbutton.time = XtLastTimestampProcessed(display);
    LessTifTestDelay(w, 200);
    XSendEvent(display, key_event.window, True, KeyReleaseMask, event);
    LessTifTestFlushEvents(w);
    Delay(w);
}

/* ************** */

extern void
LessTifTestKeyPress(Widget w, KeySym keysym, unsigned int state)
{
    Display *display = XtIsSubclass(w, coreWidgetClass) ? XtDisplay(w) : XtDisplay(XtParent(w));
    Window window = XtIsSubclass(w, coreWidgetClass) ? XtWindow(w) : XtWindow(XtParent(w));
    XKeyEvent key_event =
    {
	KeyPress,
	0,
	False,
	NULL, /* display, */
	None, /* window, */
	None, /* root, */
	None, /* subwindow, */
	CurrentTime,
	0, 0,
	0, 0,
	0,
	0x09,
	True
    };
    XEvent *event;
    unsigned int keys_buttons;

    key_event.keycode = XKeysymToKeycode(display, keysym);
    key_event.display=display;
    key_event.state=state;
    key_event.window=key_event.root=key_event.subwindow=window;
    LessTifTestWarpPointer(w);
    XQueryPointer(display, window,
		  &key_event.root,
		  &key_event.subwindow,
		  &key_event.x_root, &key_event.y_root,
		  &key_event.x, &key_event.y,
		  &keys_buttons);
    event = (XEvent*)&key_event;
    event->xany.serial = NextRequest(display);
    event->xbutton.time = XtLastTimestampProcessed(display);
    LessTifTestDelay(w, 200);
    XSendEvent(display, InputFocus, True, KeyPressMask, event);
    LessTifTestFlushEvents(w);
    Delay(w);
}

/* ************** */

extern void
LessTifTestKeysym(Widget w, KeySym keysym)
{
    Display *display = XtIsSubclass(w, coreWidgetClass) ? XtDisplay(w) : XtDisplay(XtParent(w));
    Window window = XtIsSubclass(w, coreWidgetClass) ? XtWindow(w) : XtWindow(XtParent(w));
    XKeyEvent key_event =
    {
	KeyPress,
	0,
	False,
	NULL, /* display, */
	None, /* window, */
	None, /* root, */
	None, /* subwindow, */
	CurrentTime,
	0, 0,
	0, 0,
	0,
	0x09,
	True
    };
    XEvent *event;
    unsigned int keys_buttons;

    key_event.keycode = XKeysymToKeycode(display, keysym);
    key_event.display=display;
    key_event.window=key_event.root=key_event.subwindow=window;
    LessTifTestWarpPointer(w);
    XQueryPointer(display, window,
		  &key_event.root,
		  &key_event.subwindow,
		  &key_event.x_root, &key_event.y_root,
		  &key_event.x, &key_event.y,
		  &keys_buttons);
    event = (XEvent*)&key_event;
    event->xany.serial = NextRequest(display);
    event->xbutton.time = XtLastTimestampProcessed(display);
    LessTifTestDelay(w, 200);
    XSendEvent(display, InputFocus, True, KeyPressMask, event);
    key_event.type = KeyRelease;
    key_event.window = XtWindow(XtParent(w));
    XSendEvent(display, key_event.window, True, KeyReleaseMask, event);
    LessTifTestFlushEvents(w);
    Delay(w);
}

/* ************** */

extern void
LessTifTestEsc(Widget w)
{
    LessTifTestKeysym(w, XK_Escape);
}

/* ************** */

extern void
LessTifTestBtnDown(Widget w, int button)
{
    Display *display = XtIsSubclass(w, coreWidgetClass) ? XtDisplay(w) : XtDisplay(XtParent(w));
    Window window = XtIsSubclass(w, coreWidgetClass) ? XtWindow(w) : XtWindow(XtParent(w));
    XButtonEvent button_event =
    {
	ButtonPress,
	0,
	False,
	NULL, /* display, */
	None, /* window, */
	None, /* window, */
	None, /* window, */
	CurrentTime,
	0, 0,
	0, 0,
	0,
	Button1,
	True
    };
    XEvent *event;
    unsigned int keys_buttons;

    button_event.button = button;
    button_event.display=display;
    button_event.window=button_event.root=button_event.subwindow=window;
    LessTifTestWarpPointer(w);
    XQueryPointer(display, window,
		  &button_event.root,
		  &button_event.subwindow,
		  &button_event.x_root, &button_event.y_root,
		  &button_event.x, &button_event.y,
		  &keys_buttons);
    event = (XEvent*)&button_event;
    event->xany.serial = NextRequest(display);
    event->xbutton.time = XtLastTimestampProcessed(display);
    LessTifTestDelay(w, 200);
    XSendEvent(display, PointerWindow, True, ButtonPressMask, event);
    LessTifTestFlushEvents(w);
    Delay(w);
}

/* ************** */

extern void
LessTifTestBtnUp(Widget w, int button)
{
    Display *display = XtIsSubclass(w, coreWidgetClass) ? XtDisplay(w) : XtDisplay(XtParent(w));
    Window window = XtIsSubclass(w, coreWidgetClass) ? XtWindow(w) : XtWindow(XtParent(w));
    XButtonEvent button_event =
    {
	ButtonRelease,
	0,
	False,
	NULL, /* display, */
	None, /* window, */
	None, /* window, */
	None, /* window, */
	CurrentTime,
	0, 0,
	0, 0,
	Button1Mask,
	Button1,
	True
    };
    XEvent *event;
    unsigned int keys_buttons;

    switch (button)
    {
    case 1:
	button_event.state = Button1Mask;
	break;
    case 2:
	button_event.state = Button2Mask;
	break;
    case 3:
	button_event.state = Button3Mask;
	break;
    default:
	button_event.state = 0;
    	break;
    }
    button_event.button = button;
    button_event.display=display;
    button_event.window=button_event.root=button_event.subwindow=window;
    LessTifTestWarpPointer(w);
    XQueryPointer(display, window,
		  &button_event.root,
		  &button_event.subwindow,
		  &button_event.x_root, &button_event.y_root,
		  &button_event.x, &button_event.y,
		  &keys_buttons);
    event = (XEvent*)&button_event;
    event->xany.serial = NextRequest(display);
    event->xbutton.time = XtLastTimestampProcessed(display);
    LessTifTestDelay(w, 200);
    XSendEvent(display, PointerWindow, True, ButtonReleaseMask, event);
    LessTifTestFlushEvents(w);
    Delay(w);
}

/* ************** */

extern void
LessTifTestBtn1Down(Widget w)
{
    LessTifTestBtnDown(w, 1);
}

/* ************** */

extern void
LessTifTestBtn1Up(Widget w)
{
    LessTifTestBtnUp(w, 1);
}

/* ************** */

extern void
LessTifTestBtn2Down(Widget w)
{
    LessTifTestBtnDown(w, 2);
}

/* ************** */

extern void
LessTifTestBtn2Up(Widget w)
{
    LessTifTestBtnUp(w, 2);
}

/* ************** */

extern void
LessTifTestBtn3Down(Widget w)
{
    LessTifTestBtnDown(w, 3);
}

/* ************** */

extern void
LessTifTestBtn3Up(Widget w)
{
    LessTifTestBtnUp(w, 3);
}

/* ************** */

extern int
LessTifTestPushButton(Widget w)
{
    Initialize(w);
    if (XtIsSubclass(w, coreWidgetClass))
    {
	XtCallActionProc(w, "ArmAndActivate", NULL, NULL, 0);
    }
    else
    {
    	LessTifTestBtn1Down(XtParent(w));
    	LessTifTestBtn1Up(XtParent(w));
    }
    LessTifTestFlushEvents(w);
    return (0);
}

/* ************** */
