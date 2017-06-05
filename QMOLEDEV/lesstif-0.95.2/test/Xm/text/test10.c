/* $Header: /cvsroot/lesstif/lesstif/test/Xm/text/test10.c,v 1.5 2001/05/16 13:10:19 amai Exp $ */

#include <stdlib.h>
#include <stdio.h>

#include <Xm/Text.h> 
#include <Xm/TextF.h> 
#include <Xm/PushB.h>
#include <Xm/BulletinB.h>

Widget toplevel, text, bb, tf, b;
XtAppContext app;

void Doit(Widget w, XtPointer client, XtPointer call)
{
	int	i;
	char	*s;

	s = XmTextFieldGetString(tf);
	i = atoi(s);

	fprintf(stderr, "===> XmTextShowPosition(%d)\n", i);
	XmTextShowPosition(text, i);
}

int
main(int argc, char **argv)
{
  XtSetLanguageProc(NULL, NULL, NULL);

  toplevel = XtVaAppInitialize(&app, "Text", NULL, 0, &argc, argv, NULL, NULL);

  bb = XtVaCreateManagedWidget("rc", xmBulletinBoardWidgetClass, toplevel,
	NULL);

  tf = XtVaCreateManagedWidget("tf", xmTextFieldWidgetClass, bb,
		XmNx,		10,
		XmNy,		10,
	NULL);

  b = XtVaCreateManagedWidget("b", xmPushButtonWidgetClass, bb,
		XmNx,		150,
		XmNy,		10,
	NULL);

  text = XmCreateScrolledText(bb, "text", NULL, 0);
#if 1
  XtManageChild(text);
#endif
  XtVaSetValues(XtParent(text),
		XmNx,		10,
		XmNy,		50,
		XmNwidth,	100,
		XmNheight,	80,
	NULL);
  XtVaSetValues(text,
		XmNeditMode,	XmMULTI_LINE_EDIT,
	NULL); 

  XtAddCallback(b, XmNactivateCallback, Doit, NULL);

  XtRealizeWidget(toplevel);

  

/* Note: the following values are the result of
 * querying the current geometry.
 */
{
static XtWidgetGeometry Expected[] = {
   CWWidth | CWHeight            ,  317,  334,  179,  141, 0,0,0, /* rc */
   CWWidth | CWHeight | CWX | CWY,   10,   10,  138,   31, 0,0,0, /* tf */
   CWWidth | CWHeight | CWX | CWY,  150,   10,   18,   25, 0,0,0, /* b */
   CWWidth | CWHeight | CWX | CWY,   10,   50,  100,   80, 0,0,0, /* textSW */
   CWWidth | CWHeight | CWX | CWY,    0,   65,  100,   15, 0,0,0, /* HorScrollBar */
   CWWidth | CWHeight | CWX | CWY,    0,    0,  100,   61, 0,0,0, /* text */
};
/* toplevel should be replaced with to correct applicationShell */
PrintDetails(toplevel, Expected);
}
LessTifTestMainLoop(toplevel);
  exit(0);
}
