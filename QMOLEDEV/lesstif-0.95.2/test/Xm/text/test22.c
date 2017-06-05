/* $Header: /cvsroot/lesstif/lesstif/test/Xm/text/test22.c,v 1.2 2002/05/10 08:18:51 dannybackx Exp $ */

#include <stdlib.h>
#include <stdio.h>
#include <Xm/RowColumn.h>
#include <Xm/Text.h> 
#include <Xm/ToggleB.h>


void Doit(Widget w, XtPointer client, XtPointer call)
{
	char	*s = (char *)client;
	String	v = NULL;
	short	rows;

	fprintf(stderr, "Callback %s, widget value '%s'\n", s, XmTextGetString(w));

	XtVaGetValues(w,
		XmNvalue,	&v,
		XmNrows,	&rows,
	    NULL);

	fprintf(stderr, "GetValues => value '%s', rows %d\n", v, rows);
	XtFree(v);
}

void change(Widget w, XtPointer client, XtPointer call)
{
	Widget t = (Widget)client;
	XmToggleButtonCallbackStruct *cbp = (XmToggleButtonCallbackStruct*)call;

	fprintf(stderr, "Setting text sensitive to %d\n", cbp->set);
	XtSetSensitive(t, cbp->set);
}

char *fallback[] = {
	"*text*blinkRate:	500",
	"*text.value:		abc",
	"*.geometrySlop: 2",
	NULL
};

int
main(int argc, char **argv)
{
  Widget toplevel, text, rc, toggle;
  XtAppContext app;

  XtSetLanguageProc(NULL, NULL, NULL);

  toplevel = XtVaAppInitialize(&app,"Label",NULL,0,&argc,argv, fallback, NULL);

  rc = XtVaCreateManagedWidget("rc", xmRowColumnWidgetClass, toplevel,
	NULL);

  toggle = XtVaCreateManagedWidget("sensitive", xmToggleButtonWidgetClass,
	rc,
		XmNset,	True,
	NULL);

  text = XtVaCreateManagedWidget("text", xmTextWidgetClass, rc, 
		XmNrows, 10,
	NULL); 

  XtAddCallback(toggle, XmNvalueChangedCallback, change, (XtPointer)text);
/*  XtAddCallback(toggle, XmNvalueChangedCallback, Doit, XmNvalueChangedCallback); */

  XtAddCallback(text, XmNmodifyVerifyCallback, Doit, XmNmodifyVerifyCallback);
  XtAddCallback(text, XmNactivateCallback, Doit, XmNactivateCallback);
  XtAddCallback(text, XmNlosingFocusCallback, Doit, XmNlosingFocusCallback);
  XtAddCallback(text, XmNfocusCallback, Doit, XmNfocusCallback);

  XtRealizeWidget(toplevel);

  
{
    static XtWidgetGeometry Expected[] = {
   CWWidth | CWHeight            ,   56,   72,  144,   63, 0,0,0, /* rc */
   CWWidth | CWHeight | CWX | CWY,    3,    3,  138,   23, 0,0,0, /* sensitive */
   CWWidth | CWHeight | CWX | CWY,    3,   29,  138,   31, 0,0,0, /* text */ 
    };
    PrintDetails(toplevel,Expected);
};
  LessTifTestMainLoop(toplevel);

  exit(0);
}
