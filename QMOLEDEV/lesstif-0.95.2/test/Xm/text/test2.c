/* $Header: /cvsroot/lesstif/lesstif/test/Xm/text/test2.c,v 1.5 2002/03/30 17:18:02 amai Exp $ */

#include <stdlib.h>

#include <Xm/Text.h> 

static char *fallbacks[] =
{
    "*one.rows:10",
    NULL
};

int
main(int argc, char **argv)
{
  Widget toplevel, one;
  XtAppContext app;

  XtSetLanguageProc(NULL, NULL, NULL);

  toplevel = XtVaAppInitialize(&app, "Label", NULL, 0, &argc, argv,
			       fallbacks, NULL);

  one = XmCreateScrolledText(toplevel, "one", NULL, 0);
  XtManageChild(one);

  XtRealizeWidget(toplevel);

  
{
    static XtWidgetGeometry Expected[] = {
   CWWidth | CWHeight            ,   56,   72,  138,   50, 0,0,0, /* oneSW */
   CWWidth | CWHeight | CWX | CWY,    0,   35,  138,   15, 0,0,0, /* HorScrollBar */
   CWWidth | CWHeight | CWX | CWY,    0,    0,  138,   31, 0,0,0, /* one */ 
    };
    PrintDetails(toplevel,Expected);
};
  LessTifTestMainLoop(toplevel);

  exit(0);
}

