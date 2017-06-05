/* test for bulletinboard */
#undef NEED_EDITRES

#include <Xm/Xm.h>
#include <Xm/BulletinB.h>
#include <Xm/PushB.h>
#include <Xm/PushBG.h>
#ifdef NEED_EDITRES
#include <X11/Xmu/Editres.h>
#endif
#include <stdio.h>

void
doit(Widget w, XtPointer data, XtPointer cbs) {
    XtVaSetValues(w, XmNx, 20, XmNy, 40, XmNwidth, 50, XmNheight, 60, NULL);
}

int
main(int argc, char **argv)
{
  Widget toplevel, one;
  XtAppContext app;
  Dimension thick = 0;
  Widget c;

  XtSetLanguageProc(NULL, NULL, NULL);

  toplevel = XtVaAppInitialize(&app, "Label", NULL, 0, &argc, argv, NULL, NULL);

  one = XtVaCreateManagedWidget("One", 
                                xmBulletinBoardWidgetClass, 
                                toplevel,
				XmNallowOverlap, False,
				XmNwidth, 100, XmNheight, 150,
				NULL);

  c = XtVaCreateManagedWidget("test1",
			      xmPushButtonGadgetClass,
			      one,
			      NULL);
  XtAddCallback(c, XmNactivateCallback, doit, NULL);

  c = XtVaCreateManagedWidget("test2",
			      xmPushButtonGadgetClass,
			      one,
			      XmNx, 20, XmNy, 20,
			      NULL);
  XtAddCallback(c, XmNactivateCallback, doit, NULL);

#ifdef NEED_EDITRES
  XtAddEventHandler(toplevel, (EventMask)0, True,
                    (XtEventHandler)_XEditResCheckMessages, NULL);
#endif

  XtRealizeWidget(toplevel);

  XtVaGetValues(one, XmNshadowThickness, &thick, NULL);
  printf("shadow thickness: %d\n", thick);


/* Note: the following values are the result of
 * querying the current geometry.
 */
{
static XtWidgetGeometry Expected[] = {
   CWWidth | CWHeight            ,    0,    0,  100,  150, 0,0,0, /* One */
   CWWidth | CWHeight | CWX | CWY,   10,   10,   42,   25, 0,0,0, /* test1 */
   CWWidth | CWHeight | CWX | CWY,   20,   20,   42,   25, 0,0,0, /* test2 */
};
/* toplevel should be replaced with to correct applicationShell */
PrintDetails(toplevel, Expected);
}
LessTifTestMainLoop(toplevel);
  exit(0);
}
