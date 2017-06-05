/*
 **
More fault isolation.  I get two zeros for width and height using this
on the Sun3, but it works fine on the PC.

Also, editres "flash active widgets" works fine when used with Athena
widgets on the sun.
 **
 */

#include <Xm/Xm.h>
#include <Xm/PushB.h>
#include <stdio.h>

void
cb(Widget w, XtPointer data, XtPointer cbs)
{
  Dimension width, height;
  Arg args[2];
  Cardinal num_args = 0;
  
  XtSetArg(args[num_args], XtNwidth, &width); num_args++;
  XtSetArg(args[num_args], XtNheight, &height); num_args++;
  XtGetValues(w, args, num_args);

  printf("width = %d; height = %d\n", width, height);
}

int
main(int argc, char **argv)
{
  Widget toplevel, one;
  XtAppContext app;

  XtSetLanguageProc(NULL, NULL, NULL);

  toplevel = XtVaAppInitialize(&app, "Label", NULL, 0, &argc, argv, NULL, NULL);

  one = XtVaCreateManagedWidget("button button", xmPushButtonWidgetClass, toplevel,
				NULL);

  XtAddCallback(one, XmNactivateCallback, cb, NULL);

  XtRealizeWidget(toplevel);
  {
  static XtWidgetGeometry Expected[] = {
  	CWWidth | CWHeight,		0,	0,	90,	25,	0,0,0,	/* Form */
  	CWWidth | CWHeight | CWX | CWY,	45,	30,	30,	25,	0,0,0,	/* two */
};

  PrintDetails(toplevel, Expected);
  }
  LessTifTestMainLoop(toplevel);
  /*
  XtAppMainLoop(app);
  */
  /* NOTREACHED */
  exit(0);
}


