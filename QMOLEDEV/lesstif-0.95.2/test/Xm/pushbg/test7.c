/* test for the behavior of some obscure internal gadget functions.  Check
    out the comments in GadgetUtil.c for details. */

#include <Xm/XmP.h>
#include <Xm/PushBG.h>
#include <Xm/BulletinB.h>
#include <stdio.h>

Widget toplevel, one, two;

static char *FallBack[] = {
		"*.geometrySlop: 1",
		NULL
};

void
event_proc(Widget w, XtPointer cd, XEvent *ev, Boolean *cont) {
    XmGadget three, four;
    XButtonEvent *event = (XButtonEvent *)ev;

    three = _XmInputInGadget(w, event->x, event->y);
    four = _XmInputForGadget(w, event->x, event->y);

    /*
    printf("%p %d %d\n", two, three->rectangle.sensitive, three->rectangle.ancestor_sensitive);
    printf("InGadget returned: %p:  ForGadget returned: %p\n",
	   three, four);
    */
    if (four)
	*cont = False;
    else
	*cont = True;
}

int
main(int argc, char **argv)
{
  XtAppContext app;
  XmFontList fontlist;
  XmString xmstr1;
  Dimension w,i,s,l,t,h;

  XtSetLanguageProc(NULL, NULL, NULL);

  toplevel = XtVaAppInitialize(&app, "Label", NULL, 0, &argc, argv, FallBack, NULL);

  fontlist = XmFontListAppendEntry(NULL,
			   XmFontListEntryCreate("MY_FONT",
						 XmFONT_IS_FONT,
						 XLoadQueryFont(XtDisplay(toplevel), 
 	                                         "-adobe-helvetica-bold-o-normal--17-0-75-75-p-*-iso8859-1")));

  xmstr1 = XmStringCreateLtoR("Here\nIs\nA\nDefault\nButton", "MY_FONT");

  two = XtVaCreateManagedWidget("Button1", xmBulletinBoardWidgetClass,
				 toplevel, NULL);

  one = XtVaCreateManagedWidget("One", 
                                xmPushButtonGadgetClass, 
                                two, 
				XmNfontList, fontlist, 
				XmNlabelString, xmstr1, 
				XmNshowAsDefault, 1,
				XmNdefaultButtonShadowThickness, 3,
				XmNsensitive, False,
				NULL);

  XtAddEventHandler(two, ButtonPressMask|ButtonReleaseMask, True,
		    event_proc, NULL);

  XtRealizeWidget(toplevel);
  XtVaGetValues(one,
		XmNhighlightThickness, &i,
		XmNshadowThickness, &s,
		XmNmarginWidth, &w,
		XmNmarginHeight, &h,
		XmNmarginLeft, &l,
		XmNmarginTop, &t,
		NULL);

printf("highlight: %d shad: %d marWid: %d marHei: %d marLeft: %d marTop: %d\n",
	 i, s, w, h, l, t);

/* Note: the following values are the result of
 * querying the current geometry.
 */
{
static XtWidgetGeometry Expected[] = {
   CWWidth | CWHeight            ,  425,  317,  113,  143, 0,0,0, /* Button1 */
   CWWidth | CWHeight | CWX | CWY,   10,   10,   92,  122, 0,0,0, /* One */
};
/* toplevel should be replaced with to correct applicationShell */
PrintDetails(toplevel, Expected);
}
LessTifTestMainLoop(toplevel);
  exit(0);
}
