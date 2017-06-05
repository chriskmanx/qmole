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
                                XmNcolumns, 10, 
                                XmNvalue, "hello",
                                NULL); 

  XtRealizeWidget(toplevel);

  
{
    static XtWidgetGeometry Expected[] = {
   CWWidth | CWHeight            ,   50,   50,   78,   31, 0,0,0, /* one */ 
    };
    PrintDetails(toplevel,Expected);
};
  LessTifTestMainLoop(toplevel);

  exit(0);
}

