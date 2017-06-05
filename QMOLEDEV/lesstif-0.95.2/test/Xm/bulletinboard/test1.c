/* test for bulletinboard */
/* this is useful when checking motif */
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
  Dimension thick = 0, wd, ht;
  Position x,y;
  Widget c;

  XtSetLanguageProc(NULL, NULL, NULL);

  toplevel = XtVaAppInitialize(&app, "Label", NULL, 0, &argc, argv, NULL, NULL);

  one = XtVaCreateManagedWidget("One", 
                                xmBulletinBoardWidgetClass, 
                                toplevel,
				XmNshadowThickness, 12,
				XmNshadowType, XmSHADOW_ETCHED_OUT_DASH,
				NULL);

  c = XtVaCreateManagedWidget("test1",
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
  XtVaGetValues(c, XmNx, &x, XmNy, &y, XmNwidth, &wd, XmNheight, &ht, NULL);
  printf("label x,y: %d %d  wd,ht: %d %d\n", x, y, wd, ht);


/* Note: the following values are the result of
 * querying the current geometry.
 */
{
static XtWidgetGeometry Expected[] = {
   CWWidth | CWHeight            ,  414,  377,   66,   49, 0,0,0, /* One */
   CWWidth | CWHeight | CWX | CWY,   10,   10,   34,   17, 0,0,0, /* test1 */
};
/* toplevel should be replaced with to correct applicationShell */
PrintDetails(toplevel, Expected);
}
LessTifTestMainLoop(toplevel);
  exit(0);
}
