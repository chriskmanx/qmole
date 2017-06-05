/* $Header: /cvsroot/lesstif/lesstif/test/Xm/textf/test16.c,v 1.2 2002/06/04 16:22:07 dannybackx Exp $

   [ lesstif-Bugs-446600 ] XmTextFieldSetHighlight does not work...
   Example more than one selection, as reported by Al Lykken.
  
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
  
  XmTextFieldSetHighlight(one, 10, 16, XmHIGHLIGHT_SELECTED);
  XmTextFieldSetHighlight(one, 5, 7, XmHIGHLIGHT_SELECTED);
  
  XtRealizeWidget(toplevel);

  LessTifTestMainLoop(toplevel);

  exit(0);
}
