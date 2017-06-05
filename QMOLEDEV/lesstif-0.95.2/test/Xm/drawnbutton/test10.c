/* $Id: test10.c,v 1.4 2002/05/01 15:39:21 amai Exp $ */
/*
Rick Scott wrote:
  
> I think we're in pretty good shape Jon.
   
Not so good, I have an old bug reported on Fri, 12 Mar 1999 18:55:01 +0100
and corrected, that relive with Rick changes on DrawnB.c 1.38.  I have
already report that on Wed, 20 Oct 1999 17:00:15 +0200.

Now, I write a new test case, add it as test/Xm/drawnbutton/test10.c

. test9  fail with DrawnB.c 1.38 but work with 1.39
. test10 work with DrawnB.c 1.38 but fail with 1.39

Any comments are welcome.
--
Edouard G. Parmelan
http://egp.free.fr
*/

#include <stdlib.h>
#include <stdio.h>

#include <Xm/Xm.h>
#include <Xm/Form.h>
#include <Xm/RowColumn.h>
#include <Xm/DrawnB.h>

#include "../../common/Test.h"

int
main(int argc, char **argv)
{
  XtAppContext theApp;
  Widget toplevel;
  Widget form;
  Widget butt;
  Pixmap pixmap;
  Pixel fg, bg;

  toplevel = XtVaAppInitialize(&theApp, "xlogo64", NULL, 0,
			       &argc, argv, NULL, NULL);

  form = XmCreateForm (toplevel, "Form", NULL, 0);
  XtManageChild (form);
  XtVaSetValues (form,
		 XmNwidth, 100,
		 XmNheight, 100,
		 NULL);

  butt= XtVaCreateManagedWidget("Button1", xmDrawnButtonWidgetClass, form, 
				NULL);
  XtManageChild (butt);

  XtRealizeWidget(toplevel);

  fg = XBlackPixelOfScreen(DefaultScreenOfDisplay(XtDisplay(toplevel)));
  bg = XWhitePixelOfScreen(DefaultScreenOfDisplay(XtDisplay(toplevel)));

  pixmap = XmGetPixmap(XtScreen(butt), "xlogo64", fg, bg);

  XtVaSetValues(butt,
		XmNlabelPixmap, pixmap,
		XmNlabelType, XmPIXMAP,
		NULL);

/* Note: the following values are the result of
 * querying the current geometry.
 */
{
#if XmVERSION > 1
static XtWidgetGeometry Expected[] = {
   {CWWidth | CWHeight            ,    0,    0,  100,  100, 0,0,0, /* Form */},
   {CWWidth | CWHeight | CWX | CWY,    0,    0,   76,   76, 0,0,0, /* Button1 */},
};
#else
static XtWidgetGeometry Expected[] = {
   {CWWidth | CWHeight            ,    0,    0,  100,  100, 0,0,0, /* Form */},
   {CWWidth | CWHeight | CWX | CWY,    0,    0,   12,   12, 0,0,0, /* Button1 */},
};
#endif
/* toplevel should be replaced with to correct applicationShell */
PrintDetails(toplevel, Expected);
}
LessTifTestMainLoop(toplevel);
  exit(0);
}
