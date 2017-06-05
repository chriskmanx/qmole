#include <Xm/Xm.h>
#include <Xm/MwmUtil.h>
#include <Xm/AtomMgr.h>
#include <Xm/PushB.h>
#include <Xm/Protocols.h>
#include <stdio.h>

/* f.send_msg test
 *
 *"An application can specify a message for MWM to send the application when
 * the user invokes the f.send_msg function.  The application places a
 * _MOTIF_WM_MESSAGES atom on the WM_PROTOCOLS property for the window.  The
 * application also places an atom on the _MOTIF_WM_MESSAGES property.  When the
 * f.send_msg function is invoked with this atom as the argument, MWM send the
 * application a ClientMessage.  The application can use XmAddWMProtocols() to
 * place a _MOTIF_WM_MESSAGES atom on the WM_PROTOCOLS property, and it can use
 * XmAddProtocolCallback to place an atom on the _MOTIF_WM_MESSAGES property and
 * associate it with a routine to be called when MWM sends the ClientMessage."
 */

#define MENU \
    "Punt     Shift Alt<Key>F7  f.send_msg %d\n"\
    "Lower2   Ctrl Alt<Key>F1  f.lower\n"\
    "Close2   Ctrl Alt<Key>F4  f.kill\n"


Widget toplevel, one;

static char *FallBack[] = {
		"*.geometrySlop: 2",
		NULL
};

void
punt(Widget w, XtPointer data, XtPointer cbs)
{
   printf("Punting...\n");
}

void
cb(Widget w, XtPointer data, XtPointer cbs)
{
    Atom XA_MWM_MENU;
    int actual_format;
    Atom actual_type;
    unsigned long nitems, bytesafter;
    unsigned char *buf;

    XA_MWM_MENU = XmInternAtom(XtDisplay(toplevel), _XA_MWM_MENU, False);

    if (XGetWindowProperty(XtDisplay(toplevel), XtWindow(toplevel),
			   XA_MWM_MENU, 0L, 64L, False, AnyPropertyType,
			   &actual_type, &actual_format, &nitems,
			   &bytesafter, &buf) == Success)
    {
	printf("XA_MWM_MENU: %ld\n", XA_MWM_MENU);
	printf("Actual_type: %ld Actual_format: %d nitems: %ld\n",
		actual_type, actual_format, nitems);
	printf("BytesAfter: %ld buf:\n'\n%s'\n", bytesafter, buf);
    }
    else
	printf("Property not found\n");
}

int
main(int argc, char **argv)
{
  XtAppContext app;
  XmFontList fontlist;
  XmString xmstr1 = XmStringCreateLtoR("Here\nIs\nA\nLabel", "MY_FONT");
  Atom XA_MWM_MESSAGES, PUNT;
  char buf[256];

  XtSetLanguageProc(NULL, NULL, NULL);

  toplevel = XtVaAppInitialize(&app, "Label", NULL, 0, &argc, argv, FallBack, NULL);

  fontlist = XmFontListAppendEntry(NULL,
			   XmFontListEntryCreate("MY_FONT",
						 XmFONT_IS_FONT,
						 XLoadQueryFont(XtDisplay(toplevel), 
 	                                         "-adobe-helvetica-bold-o-normal--17-0-75-75-p-*-iso8859-1")));

  one = XtVaCreateManagedWidget("One", 
                                xmPushButtonWidgetClass, 
                                toplevel, XmNfontList, fontlist, 
				XmNlabelString, xmstr1,
				XtNborderWidth, 20,
				NULL);
  XtAddCallback(one, XmNactivateCallback, cb, NULL);


  XA_MWM_MESSAGES = XmInternAtom(XtDisplay(toplevel), _XA_MWM_MESSAGES, False);
  XmAddWMProtocols(toplevel, &XA_MWM_MESSAGES, 1);
  PUNT = XmInternAtom(XtDisplay(toplevel), "PUNT", False);
  XmAddProtocolCallback(toplevel, XA_MWM_MESSAGES, PUNT, punt, NULL);

  sprintf(buf, MENU, PUNT);
  XtVaSetValues(toplevel, XmNmwmMenu, buf, NULL);

  XtRealizeWidget(toplevel);

  
{
    static XtWidgetGeometry Expected[] = {
   CWWidth | CWHeight            ,   50,   50,   57,   84, 0,0,0, /* One */ 
    };
    PrintDetails(  toplevel ,Expected);
};
   LessTifTestMainLoop(  toplevel );

  exit(0);
}
