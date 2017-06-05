/* test for bulletinboard */
#undef NEED_EDITRES

#include <Xm/Xm.h>
#include <Xm/BulletinB.h>
#include <Xm/Label.h>
#ifdef NEED_EDITRES
#include <X11/Xmu/Editres.h>
#endif
#include <stdio.h>

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
			      xmLabelWidgetClass,
			      one,
			      NULL);

  c = XtVaCreateManagedWidget("test2",
			      xmLabelWidgetClass,
			      one,
			      NULL);

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
   CWWidth | CWHeight            ,  415,  430,   55,   38, 0,0,0, /* One */
   CWWidth | CWHeight | CWX | CWY,   10,   10,   34,   17, 0,0,0, /* test1 */
   CWWidth | CWHeight | CWX | CWY,   10,   10,   34,   17, 0,0,0, /* test2 */
};
/* toplevel should be replaced with to correct applicationShell */
PrintDetails(toplevel, Expected);
}
LessTifTestMainLoop(toplevel);
  exit(0);
}
