/* $Header: /cvsroot/lesstif/lesstif/test/Xm/textf/test14.c,v 1.2 2001/08/01 13:55:31 amai Exp $

   [ lesstif-Bugs-446600 ] XmTextFieldSetHighlight does not work...
   Example for XmHIGHLIGHT_SECONDARY_SELECTED.
  
*/


#include <stdlib.h>

#include <Xm/Xm.h>
#include <Xm/TextF.h> 


int
main(int argc, char *argv[])
{
  Widget toplevel, one;
  XtAppContext app;
  char *text="This is a stupid text.";


  toplevel = XtVaAppInitialize(&app, "Label",
                               NULL, 0,
                               &argc, argv,
                               NULL, NULL);

  one = XtVaCreateManagedWidget("one", xmTextFieldWidgetClass, toplevel,
                                XmNcolumns, 30,
                                NULL); 

  XmTextFieldSetString(one, text);
  
  XmTextFieldSetHighlight(one, 10, 16, XmHIGHLIGHT_SECONDARY_SELECTED);
  
  XtRealizeWidget(toplevel);

  LessTifTestMainLoop(toplevel);

  exit(0);
}
