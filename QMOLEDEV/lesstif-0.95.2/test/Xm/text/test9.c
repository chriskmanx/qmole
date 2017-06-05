/* $Id: test9.c,v 1.4 2000/12/08 14:31:26 amai Exp $ */

#include <stdlib.h>
#include <Xm/Text.h> 

#define	LS	"Axelle Red / A Tatons this is a very long title for a CD"

int
main(int argc, char **argv)
{
  Widget toplevel, one;
  XtAppContext app;
  Arg argl[2];
  int ac;

  XtSetLanguageProc(NULL, NULL, NULL);

  toplevel = XtVaAppInitialize(&app,"Text",NULL,0,&argc,argv,NULL,
		XmNwidth,	360,
	NULL);

  ac = 0;
#if 0
  XtSetArg(argl[ac], XmNwidth, 360); ac++;
#endif
  one = XmCreateText(toplevel, "one", argl, ac);
  XtManageChild(one);

  XmTextSetString(one, LS);
  XmTextSetCursorPosition(one, 56);

  XtRealizeWidget(toplevel);

  
{
    static XtWidgetGeometry Expected[] = {
   CWWidth | CWHeight            ,   57,   73,  360,   31, 0,0,0, /* one */ 
    };
    PrintDetails(toplevel,Expected);
};
  LessTifTestMainLoop(toplevel);

  exit(0);
}

