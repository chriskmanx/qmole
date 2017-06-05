/* $Header: /cvsroot/lesstif/lesstif/test/Xm/pushbg/test8.c,v 1.6 2001/06/18 08:58:54 amai Exp $
From:        Eric Howe <mu@clio.trends.ca>
To:          Rick Scott <rwscott@omnisig.com>
Subject:     Re: cvs commit: hungry/lesstif/test/Xm/pushbg test8.c Makefile.am
Date:        Thu, 23 Jul 1998 00:23:22 -0400 (EDT)
cc:          lesstif@hungry.com

*/

#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include <Xm/Xm.h>
#include <Xm/BulletinB.h>
#include <Xm/PushBG.h>

#include "../../common/Test.h"


static void
DumpResources(Widget w)
{
Dimension showAsDefault;
Dimension defaultButtonShadowThickness;
Dimension marginBottom;
Dimension marginHeight;
Dimension marginLeft;
Dimension marginRight;
Dimension marginTop;
Dimension marginWidth;
Dimension highlightThickness;
Dimension shadowThickness;
Dimension borderWidth;
Dimension height;
Dimension width;
Dimension pheight;
Dimension pwidth;

	XtVaGetValues(XtParent(w),
		XmNheight, &pheight,
		XmNwidth, &pwidth,
		NULL);
	XtVaGetValues(w,
		XmNshowAsDefault, &showAsDefault,
		XmNdefaultButtonShadowThickness, &defaultButtonShadowThickness,
		XmNmarginBottom, &marginBottom,
		XmNmarginHeight, &marginHeight,
		XmNmarginLeft, &marginLeft,
		XmNmarginLeft, &marginRight,
		XmNmarginTop, &marginTop,
		XmNmarginWidth, &marginWidth,
		XmNhighlightThickness, &highlightThickness,
		XmNshadowThickness, &shadowThickness,
		XmNheight, &height,
		XmNwidth, &width,
		NULL);
	printf("%s: XmNshowAsDefault - %d\n", XtName(w), showAsDefault);
	printf("%s: XmNdefaultButtonShadowThickness - %d\n", XtName(w), defaultButtonShadowThickness);
	printf("%s: XmNmarginBottom - %d\n", XtName(w), marginBottom);
	printf("%s: XmNmarginHeight - %d\n", XtName(w), marginHeight);
	printf("%s: XmNmarginLeft - %d\n", XtName(w), marginLeft);
	printf("%s: XmNmarginRight - %d\n", XtName(w), marginRight);
	printf("%s: XmNmarginTop - %d\n", XtName(w), marginTop);
	printf("%s: XmNmarginWidth - %d\n", XtName(w), marginWidth);
	printf("%s: XmNhightlightThickness - %d\n", XtName(w), highlightThickness);
	printf("%s: XmNshadowThickness - %d\n", XtName(w), shadowThickness);
	printf("%s: XmNborderWidth - %d\n", XtName(w), borderWidth);
	printf("%s: XmNheight - %d\n", XtName(w), height);
	printf("%s: XmNwidth - %d\n", XtName(w), width);
	printf("%s: XmNheight - %d\n", XtName(XtParent(w)), pheight);
	printf("%s: XmNwidth - %d\n", XtName(XtParent(w)), pwidth);
	printf("\n");
}

static void
dothings(Widget w, XtPointer closure, XtPointer call)
{
static	String    s = NULL;
	XmString  x;
	Dimension shadow;

	XtVaGetValues(w, XmNshowAsDefault, &shadow, NULL);
	shadow = (shadow > 0) ? 0 : 1;

	if(s == NULL) {
		s = XtNewString("haha");
	}
	else {
		s = XtRealloc(s, strlen(s) + 4 + 1);
		strcat(s, "haha");
	}

	x = XmStringCreateLocalized(s);
	XtVaSetValues(w,
		XmNshowAsDefault, shadow,
		XmNlabelString,   x,
		NULL);
	XmStringFree(x);
	DumpResources(w);
}


int
main(int argc, char **argv)
{
	Widget top, one,two;
	XtAppContext app;

	XtSetLanguageProc(NULL, NULL, NULL);

	top = XtVaAppInitialize(&app, "HaHa", NULL, 0, &argc, argv, NULL,
			XmNallowShellResize, True,
			NULL);

	two = XtVaCreateManagedWidget("Two", xmBulletinBoardWidgetClass,
			top, NULL);
	one = XtVaCreateManagedWidget("OnePushButton", xmPushButtonGadgetClass,
			two, NULL);

	XtAddCallback(one, XmNactivateCallback, dothings, NULL);

	XtRealizeWidget(top);
	DumpResources(one);
  {
  static XtWidgetGeometry Expected[] = {
  	CWWidth | CWHeight,		0,	0,	111,	46,	0,0,0,	/* Form */
  	CWWidth | CWHeight | CWX | CWY,	10,	10,	90,	25,	0,0,0,	/* two */
};

  PrintDetails(top, Expected);
  }
  LessTifTestMainLoop(top);
  /*
	XtAppMainLoop(app);
	*/

	exit(0);
}
