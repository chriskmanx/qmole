#include <Xm/Xm.h>
#include <Xm/TextF.h> 
    
int
main(int argc, char **argv)
{
  Widget toplevel, one;
  XtAppContext app;
  Pixel fore, back;

  XtSetLanguageProc(NULL, NULL, NULL);

  toplevel = XtVaAppInitialize(&app,"Label",NULL,0,&argc,argv,NULL,NULL);

  one = XtVaCreateManagedWidget("one",xmTextFieldWidgetClass,toplevel, 
                                XmNcolumns, 10, NULL); 

  XtRealizeWidget(toplevel);

  XtVaGetValues(one,
  	XmNforeground, &fore,
  	XmNbackground, &back,
  	NULL);
  XtVaSetValues(one,
  	XmNforeground, back,
  	NULL);
  XtVaSetValues(one,
  	XmNforeground, fore,
  	NULL);


  
{
    static XtWidgetGeometry Expected[] = {
   CWWidth | CWHeight            ,   57,   73,   78,   31, 0,0,0, /* one */ 
    };
    PrintDetails(toplevel,Expected);
};
  LessTifTestMainLoop(toplevel);

  exit(0);
}

