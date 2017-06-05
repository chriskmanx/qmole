/* $Header: /cvsroot/lesstif/lesstif/test/Xm/text/test23.c,v 1.2 2002/10/20 09:34:59 dannybackx Exp $ */

#include <stdlib.h>
#include <stdio.h>
#include <Xm/RowColumn.h>
#include <Xm/Text.h> 
#include <Xm/ToggleB.h>

String fallback[] = {
	"*text.renderTable:		one, two",
/* This currently works THIS IS A HACK, THE STUFF BELOW SHOULD WORK */
	"*one*fontName:		verdana",
	"*one*renditionForeground:	red",
	"*one*fontStyle:		bold",
//	"*four.fontSize:		10",
	"*tabList:			1.5in, +1.5in, +1.5in, +1.5in",
	"*text*blinkRate:		500",
	"*text.value:			abc",
//	"*.geometrySlop:		2",
	NULL
};

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

	fprintf(stderr, "Setting text cursorPositionVisible to %d\n", cbp->set);
	XtVaSetValues(t, XmNcursorPositionVisible, cbp->set, NULL);
}

int
main(int argc, char **argv)
{
  Widget toplevel, text, rc, toggle;
  XtAppContext app;

  XtSetLanguageProc(NULL, NULL, NULL);

  toplevel = XtVaAppInitialize(&app, "Label",
  	/* options */	NULL, 0,
	/* args */	&argc, argv,
	/* fallback */	fallback,
	/* args */	NULL);

  rc = XtVaCreateManagedWidget("rc", xmRowColumnWidgetClass, toplevel,
	NULL);

  toggle = XtVaCreateManagedWidget("cursor visible", xmToggleButtonWidgetClass,
	rc,
		XmNset,	True,
	NULL);

  text = XtVaCreateManagedWidget("text", xmTextWidgetClass, rc, 
		XmNrows, 10,
	NULL); 

  XtAddCallback(toggle, XmNvalueChangedCallback, change, (XtPointer)text);
  XtAddCallback(toggle, XmNvalueChangedCallback, Doit, XmNvalueChangedCallback);

  XtAddCallback(text, XmNmodifyVerifyCallback, Doit, XmNmodifyVerifyCallback);
  XtAddCallback(text, XmNactivateCallback, Doit, XmNactivateCallback);
  XtAddCallback(text, XmNlosingFocusCallback, Doit, XmNlosingFocusCallback);
  XtAddCallback(text, XmNfocusCallback, Doit, XmNfocusCallback);

  XtRealizeWidget(toplevel);

  
{
    static XtWidgetGeometry Expected[] = {
   CWWidth | CWHeight            ,   56,   72,  144,   63, 0,0,0, /* rc */
   CWWidth | CWHeight | CWX | CWY,    3,    3,  138,   23, 0,0,0, /* cursor visible */
   CWWidth | CWHeight | CWX | CWY,    3,   29,  138,   31, 0,0,0, /* text */ 
    };
    PrintDetails(toplevel,Expected);
};
  LessTifTestMainLoop(toplevel);

  exit(0);
}
