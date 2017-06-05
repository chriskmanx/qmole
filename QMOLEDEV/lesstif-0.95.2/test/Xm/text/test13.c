/* $Header: /cvsroot/lesstif/lesstif/test/Xm/text/test13.c,v 1.6 2001/06/18 14:30:00 amai Exp $ */

#include <stdlib.h>

#include <Xm/Xm.h>
#include <Xm/Form.h>
#include <Xm/Frame.h>
#include <Xm/Label.h>
#include <Xm/MwmUtil.h>
#include <Xm/PushB.h>
#include <Xm/Text.h>

#include "../../common/Test.h"

static char *FallBack[] = {
		"*.geometrySlop: 1",
		NULL
};

Widget	parentForm, amdContentPulldown,
	amdSubjectWidget, amdToWidget,
	amdFromWidget, amdCcWidget,
	amdBccWidget, ancelText, appMailDialogCancelPushed,
	amdTopWidget, appCloseMailDialog, amdDialog,
	eaTopWidget, edTopWidget;

char	*amdContentText = "This is a message",
	*amdSubjectText = "Subject :",
	*amdToText = "To :",
	*amdFromText = "From :",
	*amdCcText = "Cc :",
	*amdBccText = "Bcc :",
	*amdCancelText = "Cancel",
	*amdSendText = "Send",
	*eaApplicationName = "TedDemo",
	*eaMailDialogName = "TedDialog";
/*
 * This is extracted from Ted, an easy Rich Text processor
 */

/************************************************************************/
/*  Make the frame for selecting a content.				*/
/************************************************************************/

static Widget appMailDialogMakeContentFrame( Widget		above,
					Widget			parentForm,
					Display *		display,
					XtPointer *	amd)
    {
    Widget	contentFrame;
    Widget	titleWidget;
    Widget	insideWidget;

    XmString	labelString;

    Arg		al[20];
    int		ac= 0;

    XtSetArg( al[ac],	XmNleftAttachment,	XmATTACH_FORM ); ac++;
    XtSetArg( al[ac],	XmNrightAttachment,	XmATTACH_FORM ); ac++;
    XtSetArg( al[ac],	XmNtopAttachment,	XmATTACH_WIDGET ); ac++;
    XtSetArg( al[ac],	XmNtopWidget,		above ); ac++;

    XtSetArg( al[ac],	XmNleftOffset,		5 ); ac++;
    XtSetArg( al[ac],	XmNrightOffset,		5 ); ac++;
    XtSetArg( al[ac],	XmNtopOffset,		5 ); ac++;

    XtSetArg( al[ac],	XmNmarginHeight,	5 ); ac++;

    contentFrame= XmCreateFrame( parentForm, "", al, ac );

    /**********************/
    ac= 0;
    labelString= XmStringCreateSimple( amdContentText );

    XtSetArg( al[ac],	XmNchildType,	XmFRAME_TITLE_CHILD ); ac++;
    XtSetArg( al[ac],	XmNlabelString,	labelString ); ac++;
    XtSetArg( al[ac],	XmNchildHorizontalAlignment,
					XmALIGNMENT_CENTER ); ac++;

    titleWidget= XmCreateLabel( contentFrame, "", al, ac );

    XmStringFree( labelString );

    /**********************/
    ac= 0;

    insideWidget= XmCreateForm( contentFrame, "", al, ac );

    XtManageChild( insideWidget );
    XtManageChild( titleWidget );
    XtManageChild( contentFrame );

    return contentFrame;
    }

/************************************************************************/
/*  Make the form with the text widgets.				*/
/************************************************************************/
static Widget appMailMakeLabelAndText(	const char *	label,
					int		pos,
					Widget		parent,
					char *name)
    {
    XmString	labelString;

    Widget	labelWidget;
    Widget	textWidget;

    Arg		al[20];
    int		ac= 0;

    labelString= XmStringCreateSimple( (char *)label );

    XtSetArg( al[ac],	XmNleftAttachment,	XmATTACH_POSITION ); ac++;
    XtSetArg( al[ac],	XmNleftPosition,	0 ); ac++;
    XtSetArg( al[ac],	XmNtopAttachment,	XmATTACH_POSITION ); ac++;
    XtSetArg( al[ac],	XmNtopPosition,		pos ); ac++;
    XtSetArg( al[ac],	XmNrightAttachment,	XmATTACH_POSITION ); ac++;
    XtSetArg( al[ac],	XmNrightPosition,	1 ); ac++;
    XtSetArg( al[ac],	XmNbottomAttachment,	XmATTACH_POSITION ); ac++;
    XtSetArg( al[ac],	XmNbottomPosition,	pos+ 2 ); ac++;

    XtSetArg( al[ac],	XmNleftOffset,		5 ); ac++;
    XtSetArg( al[ac],	XmNtopOffset,		5 ); ac++;
    XtSetArg( al[ac],	XmNrightOffset,		5 ); ac++;
    XtSetArg( al[ac],	XmNbottomOffset,	5 ); ac++;

    XtSetArg( al[ac],	XmNlabelString,		labelString ); ac++;

    labelWidget= XmCreateLabel( parent, name, al, ac );

    XmStringFree( labelString );

    ac= 0;
    XtSetArg( al[ac],	XmNleftAttachment,	XmATTACH_POSITION ); ac++;
    XtSetArg( al[ac],	XmNleftPosition,	1 ); ac++;
    XtSetArg( al[ac],	XmNtopAttachment,	XmATTACH_POSITION ); ac++;
    XtSetArg( al[ac],	XmNtopPosition,		pos ); ac++;
    XtSetArg( al[ac],	XmNrightAttachment,	XmATTACH_POSITION ); ac++;
    XtSetArg( al[ac],	XmNrightPosition,	10 ); ac++;
    XtSetArg( al[ac],	XmNbottomAttachment,	XmATTACH_POSITION ); ac++;
    XtSetArg( al[ac],	XmNbottomPosition,	pos+ 2 ); ac++;

    XtSetArg( al[ac],	XmNleftOffset,		5 ); ac++;
    XtSetArg( al[ac],	XmNtopOffset,		5 ); ac++;
    XtSetArg( al[ac],	XmNrightOffset,		5 ); ac++;
    XtSetArg( al[ac],	XmNbottomOffset,	5 ); ac++;

    textWidget= XmCreateText( parent, name, al, ac );

    XtManageChild( labelWidget );
    XtManageChild( textWidget );

    return textWidget;
    }

static Widget appMailDialogMakeTextForm(	Widget		parentForm,
						XtPointer *	amd )
    {
    Widget	textForm;

    Arg		al[20];
    int		ac= 0;

    XtSetArg( al[ac],	XmNleftAttachment,	XmATTACH_FORM ); ac++;
    XtSetArg( al[ac],	XmNrightAttachment,	XmATTACH_FORM ); ac++;
    XtSetArg( al[ac],	XmNtopAttachment,	XmATTACH_FORM ); ac++;

    XtSetArg( al[ac],	XmNleftOffset,		5 ); ac++;
    XtSetArg( al[ac],	XmNrightOffset,		5 ); ac++;
    XtSetArg( al[ac],	XmNtopOffset,		5 ); ac++;

    XtSetArg( al[ac],	XmNfractionBase,	10 ); ac++;

    textForm= XmCreateForm( parentForm, "", al, ac );

    amdSubjectWidget= appMailMakeLabelAndText( amdSubjectText,
			    0, textForm, "Subject");
    amdToWidget= appMailMakeLabelAndText( amdToText,
			    2, textForm, "To");
    amdFromWidget= appMailMakeLabelAndText( amdFromText,
			    4, textForm, "From");
    amdCcWidget= appMailMakeLabelAndText( amdCcText,
			    6, textForm, "Cc");
    amdBccWidget= appMailMakeLabelAndText( amdBccText,
			    8, textForm, "Bcc");

    XtManageChild( textForm );

    return textForm;
    }

/************************************************************************/
/*  Make the form with the two buttons.					*/
/************************************************************************/
static Widget appMailDialogMakeButtonForm( Widget		parentForm,
					Widget			above,
					XtPointer *	amd )
    {
    Widget	buttonForm;
    Widget	revertButton;
    Widget	setButton;

    XmString	labelString;

    Arg		al[20];
    int		ac= 0;

    XtSetArg( al[ac],	XmNbottomAttachment,	XmATTACH_FORM ); ac++;
    XtSetArg( al[ac],	XmNleftAttachment,	XmATTACH_FORM ); ac++;
    XtSetArg( al[ac],	XmNrightAttachment,	XmATTACH_FORM ); ac++;
    XtSetArg( al[ac],	XmNtopAttachment,	XmATTACH_WIDGET ); ac++;

    XtSetArg( al[ac],	XmNtopWidget,		above ); ac++;

    XtSetArg( al[ac],	XmNleftOffset,		0 ); ac++;
    XtSetArg( al[ac],	XmNrightOffset,		0 ); ac++;
    XtSetArg( al[ac],	XmNtopOffset,		0 ); ac++;
    XtSetArg( al[ac],	XmNbottomOffset,	0 ); ac++;

    XtSetArg( al[ac],	XmNfractionBase,	2 ); ac++;

    buttonForm= XmCreateForm( parentForm, "", al, ac );

    /**********************/
    labelString= XmStringCreateSimple( amdCancelText );

    ac= 0;
    XtSetArg( al[ac],	XmNtopAttachment,	XmATTACH_POSITION ); ac++;
    XtSetArg( al[ac],	XmNtopPosition,		0 ); ac++;
    XtSetArg( al[ac],	XmNtopOffset,		8 ); ac++;

    XtSetArg( al[ac],	XmNleftAttachment,	XmATTACH_POSITION ); ac++;
    XtSetArg( al[ac],	XmNleftPosition,	0 ); ac++;
    XtSetArg( al[ac],	XmNleftOffset,		8 ); ac++;

    XtSetArg( al[ac],	XmNrightAttachment,	XmATTACH_POSITION ); ac++;
    XtSetArg( al[ac],	XmNrightPosition,	1 ); ac++;
    XtSetArg( al[ac],	XmNrightOffset,		5 ); ac++;

    XtSetArg( al[ac],	XmNbottomAttachment,	XmATTACH_POSITION ); ac++;
    XtSetArg( al[ac],	XmNbottomPosition,	2 ); ac++;
    XtSetArg( al[ac],	XmNbottomOffset,	8 ); ac++;

    XtSetArg( al[ac],	XmNlabelString,		labelString ); ac++;

    revertButton= XmCreatePushButton( buttonForm, "", al, ac );

    /**********************/
    labelString= XmStringCreateSimple( amdSendText );

    ac= 0;
    XtSetArg( al[ac],	XmNtopAttachment,	XmATTACH_POSITION ); ac++;
    XtSetArg( al[ac],	XmNtopPosition,		0 ); ac++;
    XtSetArg( al[ac],	XmNtopOffset,		8 ); ac++;

    XtSetArg( al[ac],	XmNleftAttachment,	XmATTACH_POSITION ); ac++;
    XtSetArg( al[ac],	XmNleftPosition,	1 ); ac++;
    XtSetArg( al[ac],	XmNleftOffset,		8 ); ac++;

    XtSetArg( al[ac],	XmNrightAttachment,	XmATTACH_POSITION ); ac++;
    XtSetArg( al[ac],	XmNrightPosition,	2 ); ac++;
    XtSetArg( al[ac],	XmNrightOffset,		5 ); ac++;

    XtSetArg( al[ac],	XmNbottomAttachment,	XmATTACH_POSITION ); ac++;
    XtSetArg( al[ac],	XmNbottomPosition,	2 ); ac++;
    XtSetArg( al[ac],	XmNbottomOffset,	8 ); ac++;

    XtSetArg( al[ac],	XmNlabelString,		labelString ); ac++;

    setButton= XmCreatePushButton( buttonForm, "", al, ac );

    /**********************/

    XtManageChild( setButton );
    XtManageChild( revertButton );

    XtManageChild( buttonForm );

    return buttonForm;
    }

/************************************************************************/
/*  make a page tool.							*/
/************************************************************************/
static void * appMakeMailDialog(	XtPointer *	ea,
					XtPointer *		ed,
					Widget			printOption,
					const char *		pixmapName )
    {
    Display *		display= XtDisplay( printOption );
    
    Arg			al[20];
    int			ac= 0;

    Widget		textForm;
    Widget		contentFrame;
    Widget		buttonForm;

    Dimension		width;

    MwmHints		hints;

    Pixmap		iconPixmap= (Pixmap)0;

    hints.flags= MWM_HINTS_FUNCTIONS|MWM_HINTS_DECORATIONS;
    hints.functions=	MWM_FUNC_MOVE		|
			MWM_FUNC_MINIMIZE	|
			MWM_FUNC_CLOSE		;
    hints.decorations=	MWM_DECOR_BORDER	|
			MWM_DECOR_TITLE		|
			MWM_DECOR_MENU		|
			MWM_DECOR_MINIMIZE	;

    XtSetArg( al[ac], XmNdeleteResponse,	XmDO_NOTHING ); ac++;
    XtSetArg( al[ac], XmNallowShellResize,	True );
    XtSetArg( al[ac], XmNmwmDecorations,	hints.decorations ); ac++;
    XtSetArg( al[ac], XmNmwmFunctions,		hints.functions ); ac++;

    if  ( iconPixmap )
	{ XtSetArg( al[ac], XmNiconPixmap,	iconPixmap ); ac++; }


    amdTopWidget= XtCreatePopupShell( eaMailDialogName,
						    transientShellWidgetClass,
						    eaTopWidget, al, ac );

    ac= 0;
    XtSetArg( al[ac], XmNallowResize,	True ); ac++;
    amdDialog= XmCreateForm( amdTopWidget, "", al, ac );

    textForm= appMailDialogMakeTextForm( amdDialog, NULL );

    contentFrame= appMailDialogMakeContentFrame( textForm,
					    amdDialog, display, NULL );
    buttonForm= appMailDialogMakeButtonForm( amdDialog, contentFrame,
								NULL );

    XtManageChild( amdDialog );

    XtPopup( amdTopWidget, XtGrabExclusive );

    return (void *)NULL;
    }

void quit(Widget w, XtPointer client, XtPointer call)
{
	exit(0);
}

int
main(int argc, char **argv)
{
  Widget toplevel, w;
  XtAppContext app;

  XtSetLanguageProc(NULL, NULL, NULL);

  toplevel = XtVaAppInitialize(&app,"Label",NULL,0,&argc,argv, FallBack, NULL);
  eaTopWidget = toplevel;

  w = XmCreatePushButton( toplevel, "Quit", NULL, 0 );
  XtManageChild(w);
  XtAddCallback(w, XmNactivateCallback, quit, 0);

  (void)appMakeMailDialog(NULL, NULL, toplevel, NULL);
  XtRealizeWidget(toplevel);
  
  LessTifTestWaitForIt(amdDialog);

/* Note: the following values are the result of
 * querying the current geometry.
 */
{
XtWidgetGeometry Expected[] = {
   CWWidth | CWHeight            ,    6,   22,  690,  285, 0,0,0, /*  */
   CWWidth | CWHeight | CWX | CWY,    5,    5,  680,  205, 0,0,0, /*  */
   CWWidth | CWHeight | CWX | CWY,    5,    5,   58,   31, 0,0,0, /* Subject */
   CWWidth | CWHeight | CWX | CWY,   73,    5,  602,   31, 0,0,0, /* Subject */
   CWWidth | CWHeight | CWX | CWY,    5,   46,   58,   31, 0,0,0, /* To */
   CWWidth | CWHeight | CWX | CWY,   73,   46,  602,   31, 0,0,0, /* To */
   CWWidth | CWHeight | CWX | CWY,    5,   87,   58,   31, 0,0,0, /* From */
   CWWidth | CWHeight | CWX | CWY,   73,   87,  602,   31, 0,0,0, /* From */
   CWWidth | CWHeight | CWX | CWY,    5,  128,   58,   31, 0,0,0, /* Cc */
   CWWidth | CWHeight | CWX | CWY,   73,  128,  602,   31, 0,0,0, /* Cc */
   CWWidth | CWHeight | CWX | CWY,    5,  169,   58,   31, 0,0,0, /* Bcc */
   CWWidth | CWHeight | CWX | CWY,   73,  169,  602,   31, 0,0,0, /* Bcc */
   CWWidth | CWHeight | CWX | CWY,    5,  215,  680,   29, 0,0,0, /*  */
   CWWidth | CWHeight | CWX | CWY,  287,    0,  106,   17, 0,0,0, /*  */
   CWWidth | CWHeight | CWX | CWY,    2,   22,  676,    1, 0,0,0, /*  */
   CWWidth | CWHeight | CWX | CWY,    0,  244,  690,   41, 0,0,0, /*  */
   CWWidth | CWHeight | CWX | CWY,    8,    8,  332,   25, 0,0,0, /*  */
   CWWidth | CWHeight | CWX | CWY,  353,    8,  332,   25, 0,0,0, /*  */
};
/* toplevel should be replaced with to correct applicationShell */
PrintDetails(amdDialog, Expected);
}
LessTifTestMainLoop(toplevel);
  /*
  XtAppMainLoop(app);
  */

  exit(0);
}
