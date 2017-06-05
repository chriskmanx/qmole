/* $Header: /cvsroot/lesstif/lesstif/test/Xm/textf/test17.c,v 1.2 2002/10/23 19:18:44 dannybackx Exp $ */

#include <stdlib.h>
#include <Xm/Xm.h>
#include <Xm/TextF.h> 

String fallback[] = {
	"*text.renderTable:		one, two",
/* This currently works THIS IS A HACK, THE STUFF BELOW SHOULD WORK */
	"*one*fontName:			verdana",
	"*one*renditionForeground:	red",
	"*one*fontStyle:		bold",
	"*one.fontSize:			24",
	"*tabList:			1.5in, +1.5in, +1.5in, +1.5in",
	"*text*blinkRate:		500",
	"*text.value:			abc",
	"*text.columns:			10",
	"*.geometrySlop:		2",
	NULL
};

int
main(int argc, char **argv)
{
  Widget toplevel, one;
  XtAppContext app;

  XtSetLanguageProc(NULL, NULL, NULL);

  toplevel = XtVaAppInitialize(&app,"Label",NULL,0,&argc,argv,fallback,NULL);

  one = XtVaCreateManagedWidget("text", xmTextFieldWidgetClass, toplevel, NULL); 

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

