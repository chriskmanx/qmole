/* test for bulletinboard */
#undef NEED_EDITRES

#include <Xm/XmP.h>
#include <Xm/BulletinB.h>
#include <Xm/PushB.h>
#ifdef NEED_EDITRES
#include <X11/Xmu/Editres.h>
#endif
#include <stdio.h>


void
doit(Widget w, XtPointer data, XtPointer cbs) {
    XtVaSetValues(w, XmNx, 0, XmNy, 0, NULL);
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
				NULL);

  c = XtVaCreateManagedWidget("test1",
			      xmPushButtonWidgetClass,
			      one,
			      NULL);
  XtAddCallback(c, XmNactivateCallback, doit, NULL);

#ifdef NEED_EDITRES
  XtAddEventHandler(toplevel, (EventMask)0, True,
                    (XtEventHandler)_XEditResCheckMessages, NULL);
#endif

  XtRealizeWidget(toplevel);

  XtVaGetValues(one, XmNshadowThickness, &thick, NULL);
  printf("shadow thickness: %d\n", thick);

  
{
    static XtWidgetGeometry Expected[] = {
   CWWidth | CWHeight            ,   50,   50,   63,   46, 0,0,0, /* One */
   CWWidth | CWHeight | CWX | CWY,   10,   10,   42,   25, 0,0,0, /* test1 */ 
    };
    PrintDetails(  toplevel ,Expected);
};
   LessTifTestMainLoop(  toplevel );

  exit(0);
}
