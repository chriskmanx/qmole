/* $Header: /cvsroot/lesstif/lesstif/test/Xm/textf/test1.c,v 1.4 2002/04/01 13:36:36 amai Exp $ */

#include <stdlib.h>
#include <Xm/Xm.h>
#include <Xm/TextF.h> 
    
int
main(int argc, char **argv)
{
  Widget toplevel, one;
  XtAppContext app;

  XtSetLanguageProc(NULL, NULL, NULL);

  toplevel = XtVaAppInitialize(&app,"Label",NULL,0,&argc,argv,NULL,NULL);

  one = XtVaCreateManagedWidget("one",xmTextFieldWidgetClass,toplevel, 
                                XmNcolumns, 10, NULL); 

  XtRealizeWidget(toplevel);

  
{
    static XtWidgetGeometry Expected[] = {
   CWWidth | CWHeight            ,   57,   73,   78,   31, 0,0,0, /* one */ 
    };
    PrintDetails(toplevel,Expected);
};
  LessTifTestMainLoop(toplevel);

  exit(0);
}

