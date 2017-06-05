/*
 * $Header: /cvsroot/lesstif/lesstif/test/Xm/menushell/test9.c,v 1.2 2003/07/16 21:16:04 dannybackx Exp $
 */
#include <Xm/Xm.h>
#include <Xm/RowColumn.h>
#include <Xm/Text.h>
#include <Xm/PushB.h>

#include <stdio.h>

Widget tw, toplevel, popup, button1, button2;
XtAppContext theApp;

char *fallback[] = {
        "*button1.labelString:                  Button 1",
        "*button1.mnemonic:                     B",
        "*button1.acceleratorText:              Ctrl-B",
        "*button1.accelerator:                  Ctrl<Key>b",
        "*button2.labelString:                  Button 2",
        "*button2.mnemonic:                     u",
        "*button2.acceleratorText:              Ctrl-U",
        "*button2.accelerator:                  Ctrl<Key>U",
        NULL   
};
 
int
main(int argc,
     char **argv)
{
	Arg args[2];
	int nargs;
	String menuPost = NULL;
	unsigned int foo;

	toplevel = XtAppInitialize(&theApp, "test12", NULL, 0,
		&argc, argv, fallback, NULL, 0);

	tw = XtVaCreateManagedWidget("text", xmTextWidgetClass,
		toplevel,
			XtNwidth,	400,
			XtNheight,	300,
		NULL);

  nargs = 0;
#if XmVERSION > 1
  XtSetArg(args[nargs],XmNpopupEnabled,XmPOPUP_AUTOMATIC_RECURSIVE);
  ++nargs;
#endif
  popup = XmCreatePopupMenu(tw,
                            "popup",
                           args,nargs);
/*  XtManageChild(popup); */

  XtVaSetValues(popup, XmNwhichButton, 3, NULL);

  button1 = XtVaCreateManagedWidget("button1",
				    xmPushButtonWidgetClass,
				    popup,
				    NULL);

  button2 = XtVaCreateManagedWidget("button2",
				    xmPushButtonWidgetClass,
				    popup,
				    NULL);

  XtRealizeWidget(toplevel);

  {
    static XtWidgetGeometry Expected[] = {
      CWWidth | CWHeight            ,   57,   73,   34,   17, 0,0,0, /* label */
    };
    /* toplevel should be replaced with to correct applicationShell */
    PrintDetails(toplevel, Expected);
  }
  LessTifTestMainLoop(toplevel);

  exit(0);
}
