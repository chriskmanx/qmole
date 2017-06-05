#include <Xm/Xm.h>
#include <Xm/MwmUtil.h>
#include <Xm/AtomMgr.h>
#include <Xm/PushB.h>
#include <stdio.h>

#define MENU \
    "Lower2    Ctrl Alt<Key>F1  f.lower\n"\
    "Close2    Ctrl Alt<Key>F4  f.kill\n"


Widget toplevel, one;

static char *FallBack[] = {
		"*.geometrySlop: 2",
		NULL
};

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

  XtVaSetValues(toplevel, XmNmwmMenu, MENU, NULL);

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
