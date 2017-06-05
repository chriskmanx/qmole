/*

$Header: /cvsroot/lesstif/lesstif/test/Xm/textf/test12.c,v 1.2 2002/04/01 13:36:36 amai Exp $

From:        "Edward A. Falk" <falconer@best.com>
To:          lesstif@lesstif.org
Subject:     Bug report: XmTextFieldSetSelection()  -- lesstif 0.89-9
Date:        Fri, 10 Mar 2000 15:32:56 -0800 (PST)

Hello; I think the header comments explain the probelm in
enough detail

	-ed falk
*/

/* Demonstrate text selection/focus bug.  I wish to have the user enter
 * some text to replace the text already in the textfield.  I do this
 * by setting the keyboard focus and by selecting all the text in
 * the textfield.
 *
 * Since I don't know how long the current value in the textfield is,
 * I just select the range 0,99 (I know there will never be more than 99
 * characters in the textfield.)
 *
 * Having done so, the user is unable to type anything into the textfield,
 * and eventually there is a core dump if you play with it enough.
 *
 * Conjecture:  XmTextFieldSetSelection() is not bounds-checking its
 * arguments, and is setting the insert position beyond the end of the
 * space allocated for input.
 *
 * Work-around:  Before setting the selection, read the current value of
 * the textfield, get the length of it, and set the selection to that
 * value.
 *
 */

#include <stdlib.h>
#include <stdio.h>

#include <Xm/Xm.h>
#include <Xm/PushB.h>
#include <Xm/TextF.h>
#include <Xm/RowColumn.h>

static	Widget	textf ;

static	void	buttonCB(Widget, XtPointer client, XtPointer data) ;

static	String	fallback[] = {
	"*button.labelString:	enter text",
	"*textf.value:	23.5",
	NULL} ;

main(int argc, char **argv)
{
	XtAppContext	app ;
	Widget		toplevel, panel ;
	Widget		button ;

	XtSetLanguageProc(NULL,NULL,NULL) ;

	toplevel = XtVaAppInitialize(&app, "Textfocus", NULL,0, &argc,argv,
		fallback,NULL) ;

	panel = XtVaCreateManagedWidget("panel",
			xmRowColumnWidgetClass, toplevel,
			XmNorientation, XmVERTICAL,
			0) ;

	button = XtCreateManagedWidget("button",xmPushButtonWidgetClass,
		panel,NULL,0);
	textf = XmCreateTextField(panel, "textf", NULL,0) ;
	XtManageChild(textf) ;
	XtAddCallback(button, XmNactivateCallback, buttonCB, NULL) ;

	XtRealizeWidget(toplevel) ;

	LessTifTestMainLoop(toplevel);

#if	0
	XtAppMainLoop(app) ;
#endif

	exit(0) ;
}


static	void
buttonCB(Widget w, XtPointer client, XtPointer data)
{
	XmString	str ;

	XmProcessTraversal(textf, XmTRAVERSE_CURRENT) ;
	XmTextFieldSetSelection(textf, 0,99, CurrentTime) ;
}
