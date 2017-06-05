
#include <Xm/PushBG.h>
#include <Xm/BulletinB.h>
#include <stdio.h>

void
cb(Widget w, XtPointer userData, XtPointer cbs) {
  XmPushButtonCallbackStruct *pbcs = (XmPushButtonCallbackStruct *)cbs;

  printf("PushButton activated: click_count: %d\n", pbcs->click_count);
}

int
main(int argc, char **argv)
{
  Widget toplevel, one,two;
  XtAppContext app;
  Dimension w,i,s,l,t,h;

  XtSetLanguageProc(NULL, NULL, NULL);

  toplevel = XtVaAppInitialize(&app, "PBG1", NULL, 0, &argc, argv, NULL, NULL);

  two = XtVaCreateManagedWidget("Two", xmBulletinBoardWidgetClass, toplevel, NULL);

  one = XtVaCreateManagedWidget("OneBlueButton", xmPushButtonGadgetClass, two,
				NULL);

  XtAddCallback(one, XmNactivateCallback, cb, NULL);

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

  {
  static XtWidgetGeometry Expected[] = {
  	CWWidth | CWHeight,		0,	0,	111,	46,	0,0,0,	/* Form */
  	CWWidth | CWHeight | CWX | CWY,	10,	10,	90,	25,	0,0,0,	/* two */
};

  PrintDetails(toplevel, Expected);
  }
  LessTifTestMainLoop(toplevel);
  /*
  XtAppMainLoop(app);
  */

  exit(0);
}
