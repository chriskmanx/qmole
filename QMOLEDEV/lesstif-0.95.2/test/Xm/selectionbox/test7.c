#include <stdio.h>
#include <stdlib.h>
#include <Xm/PushB.h>
#include <Xm/SelectioB.h>
#include <Xm/Text.h>

#define	XMS(s)		XmStringCreateSimple ( s );
#define	XMG(s,c)	XmStringGetLtoR( s, XmSTRING_DEFAULT_CHARSET, &c );

static	void	selectionBox ();
static	void	okCB();
static	void	applyCB();
static	void	erCB();

int	main ( int argc, char **argv )
{
	XtAppContext	appShell;
	Widget		TheApp;
	Widget		push;

	TheApp = XtAppInitialize ( &appShell, "TEST",
		NULL, 0, &argc, argv, NULL, NULL, 0 );

	push = XmCreatePushButton ( TheApp, "PushMe", NULL, 0 );
	XtManageChild (push);

	XtAddCallback ( push, XmNactivateCallback, selectionBox, NULL );

	XtRealizeWidget (TheApp);


  
{
    static XtWidgetGeometry Expected[] = {
   CWWidth | CWHeight            ,   50,   50,   48,   25, 0,0,0, /* PushMe */ 
    };
    PrintDetails(TheApp,Expected);
};
  LessTifTestMainLoop(TheApp);

	exit(0);
}

static	void	selectionBox ( Widget w, XtPointer client, XtPointer call_data )
{
	int	n;
	Arg	args[10];
	Widget	db;
	XmString	str[3];

	str[0] = XMS("I'm a choice");
	str[1] = XMS("I'm better");
	str[2] = XMS("I'm the best");

	db = XmCreateSelectionDialog ( w, "TheDB", NULL, 0 );

	n=0;
	XtSetArg ( args[n], XmNdeleteResponse, XmDESTROY ); n++;
	XtSetArg ( args[n], XmNdialogStyle, XmDIALOG_FULL_APPLICATION_MODAL ); n++;
	XtSetArg ( args[n], XmNmustMatch, TRUE ); n++;
	XtSetArg ( args[n], XmNlistItems, str ); n++;
	XtSetArg ( args[n], XmNlistItemCount, 3 ); n++;
	XtSetValues ( db, args, n );

	XmStringFree ( str[0] );
	XmStringFree ( str[1] );
	XmStringFree ( str[2] );

	XtAddCallback ( db, XmNokCallback, okCB, NULL );
	XtAddCallback ( db, XmNapplyCallback, applyCB, NULL );
	XtAddCallback ( db, XmNnoMatchCallback, erCB, NULL );
	XtSetSensitive( XmSelectionBoxGetChild(db, XmDIALOG_HELP_BUTTON),FALSE);

	XtManageChild (db);
}

static	void okCB ( Widget w, XtPointer c, XmSelectionBoxCallbackStruct *cbs )
{
	char	*search=NULL;

	if ( cbs->value )
		XMG ( cbs->value, search );

	(void) fprintf ( stderr, "okCB: value:\t\"%s\"\n", cbs->value );
	(void) fprintf ( stderr, "okCB: search:\t\"%s\"\n", search );
}

static	void applyCB ( Widget w, XtPointer c, XmSelectionBoxCallbackStruct *cbs )
{
	char	*search=NULL;

	if ( cbs->value )
		XMG ( cbs->value, search );

	(void) fprintf ( stderr, "applyCB: value:\t\"%s\"\n", cbs->value );
	(void) fprintf ( stderr, "applyCB: search:\t\"%s\"\n", search );
}

#include <Xm/MessageB.h>

static	void	erCB ( w, client, call_data )
Widget				w;
XtPointer			client;
XmSelectionBoxCallbackStruct	*call_data;
{
	Widget	db;
	Arg	args[10];
	int	n;
	XmString msg;
	char	*value;
	char	temp[256];

	XMG ( call_data->value, value );

	sprintf ( temp, "\"%s\"", value );
	XtFree(value);

	msg = XMS (temp);

	n=0;
	XtSetArg ( args[n], XmNdialogStyle, XmDIALOG_FULL_APPLICATION_MODAL); n++;
	XtSetArg ( args[n], XmNdeleteResponse, XmDESTROY ); n++;
	XtSetArg ( args[n], XmNmessageString, msg ); n++;

	db = XmCreateErrorDialog ( XtParent(XtParent(w)), "BadSelection", args, n );
	XtSetSensitive( XmMessageBoxGetChild (db,XmDIALOG_CANCEL_BUTTON),FALSE);
	XtSetSensitive( XmMessageBoxGetChild (db,XmDIALOG_HELP_BUTTON), FALSE);

	XmStringFree(msg);
	XBell(XtDisplay(db), 0 );
	XtManageChild ( db );

}

