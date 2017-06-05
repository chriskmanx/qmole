#include <Xm/PushB.h>
#include <stdio.h>

static char *FallBack[] = {
		"*.geometrySlop: 2",
		NULL
};

void
_LesstifDragEventHandler(Widget w,
			 XtPointer data,
			 XEvent *event,
			 Boolean *cont)
{
    XClientMessageEvent * c_event = (XClientMessageEvent *) event;
    int i;

    if (event->type != ClientMessage)
	return;

    printf ("Message type = %s\n", XGetAtomName(XtDisplay(w), c_event->message_type));
    printf ("Received by %d\n", (int)data);
    printf ("Window ID = %lx\n", (long)c_event->window);
    printf ("Format = %d\n", c_event->format);

    for (i=0; i<20; i++)
	printf ("b[%d] = %x\n", i, c_event->data.b[i]);
}

int
main(int argc, char **argv)
{
  Widget toplevel, one;
  XtAppContext app;
  XmFontList fontlist;
  XmString xmstr1 = XmStringCreateLtoR("Here\nIs\nA\nLabel", "MY_FONT");

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

  XtAddEventHandler(toplevel, (EventMask)0, True,
		    (XtEventHandler)_LesstifDragEventHandler, 0);

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
