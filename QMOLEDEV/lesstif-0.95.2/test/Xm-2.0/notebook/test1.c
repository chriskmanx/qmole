/* $Header: /cvsroot/lesstif/lesstif/test/Xm-2.0/notebook/test1.c,v 1.10 2002/05/01 15:47:32 amai Exp $ */

#include <stdio.h>
#include <stdlib.h>

#include <Xm/Xm.h>
#include <Xm/Form.h>
#include <Xm/Label.h>
#include <Xm/List.h>
#include <Xm/MenuShell.h>
#include <Xm/Notebook.h>
#include <Xm/PushB.h>
#include <Xm/RowColumn.h>
#include <Xm/ScrolledW.h>
#include <Xm/Separator.h>
#include <Xm/TextF.h>

#include "../../common/Test.h"


void
check_geometry(Widget w)
{
   static int result_index = 0;

static XtWidgetGeometry Expected[] = {
/* result test 0 */
{  CWWidth | CWHeight            ,    4,   23,  639,  565, 0,0,0 }, /* test1.motif */
{  CWWidth | CWHeight            ,    4,   23,  639,  565, 0,0,0 }, /* notebook */
{  CWWidth | CWHeight | CWX | CWY,  446,  507,   94,   29, 0,0,0 }, /* PageScroller */
{  CWWidth | CWHeight | CWX | CWY,   20,    2,   54,   25, 0,0,0 }, /* NBTextField */
{  CWWidth | CWHeight | CWX | CWY,  -20,  -20,   20,   20, 0,0,0 }, /* MajorTabScrollerNext */
{  CWWidth | CWHeight | CWX | CWY,  -20,  -20,   20,   20, 0,0,0 }, /* MajorTabScrollerPrevious */
{  CWWidth | CWHeight | CWX | CWY,  -20,  -20,   20,   20, 0,0,0 }, /* MinorTabScrollerNext */
{  CWWidth | CWHeight | CWX | CWY,  -20,  -20,   20,   20, 0,0,0 }, /* MinorTabScrollerPrevious */
{  CWWidth | CWHeight | CWX | CWY, -500, -504,  500,  504, 0,0,0 }, /* configureWindow */
{  CWWidth | CWHeight | CWX | CWY,  383,  160,   99,   30, 0,0,0 }, /* moveDownButton */
{  CWWidth | CWHeight | CWX | CWY,  259,  160,   99,   30, 0,0,0 }, /* moveUpButton */
{  CWWidth | CWHeight | CWX | CWY,   10,  160,   99,   30, 0,0,0 }, /* addDBButton */
{  CWWidth | CWHeight | CWX | CWY,   10,  200,  482,   20, 0,0,0 }, /* separator2 */
{  CWWidth | CWHeight | CWX | CWY,  134,  160,  100,   30, 0,0,0 }, /* deleteButton */
{  CWWidth | CWHeight | CWX | CWY,   10,  426,  480,   20, 0,0,0 }, /* separator1 */
{  CWWidth | CWHeight | CWX | CWY,  222,  380,  268,   36, 0,0,0 }, /* dbDescFileTf */
{  CWWidth | CWHeight | CWX | CWY,  222,  332,  268,   36, 0,0,0 }, /* dbFileTf */
{  CWWidth | CWHeight | CWX | CWY,   10,  383,  200,   43, 0,0,0 }, /* label5 */
{  CWWidth | CWHeight | CWX | CWY,   10,  331,  202,   42, 0,0,0 }, /* label4 */
{  CWWidth | CWHeight | CWX | CWY,   10,  280,  202,   41, 0,0,0 }, /* label3 */
{  CWWidth | CWHeight | CWX | CWY,  222,  280,  268,   35, 0,0,0 }, /* optionMenu1 */
{  CWWidth | CWHeight | CWX | CWY,    3,    3,   10,   29, 0,0,0 }, /* OptionLabel */
{  CWWidth | CWHeight | CWX | CWY,   16,    3,  103,   29, 0,0,0 }, /* OptionButton */
{  CWWidth | CWHeight | CWX | CWY,  222,  230,  268,   36, 0,0,0 }, /* dbNameTf */
{  CWWidth | CWHeight | CWX | CWY,   10,  230,  202,   40, 0,0,0 }, /* label2 */
{  CWWidth | CWHeight | CWX | CWY,   10,   50,  481,  100, 0,0,0 }, /* scrolledWindow1 */
{  CWWidth | CWHeight | CWX | CWY,    0,    0,   15,  104, 0,0,0 }, /* VertScrollBar */
{  CWWidth | CWHeight | CWX | CWY,    0,    0,  481,  100, 0,0,0 }, /* dbList */
{  CWWidth | CWHeight | CWX | CWY,   10,   10,  101,   30, 0,0,0 }, /* label1 */
{  CWWidth | CWHeight | CWX | CWY,  230,  456,  115,   31, 0,0,0 }, /* cancelButton */
{  CWWidth | CWHeight | CWX | CWY,   20,  456,  115,   31, 0,0,0 }, /* applyButton */
{  CWWidth | CWHeight | CWX | CWY,    0,    0,    1,    1, 0,0,0 }, /* menuShell */
{  CWWidth | CWHeight | CWX | CWY,    0,    0,   78,   25, 0,0,0 }, /* pulldownMenu5 */
{  CWWidth | CWHeight | CWX | CWY,    2,    2,   74,   21, 0,0,0 }, /* pushButton5 */
{  CWWidth | CWHeight | CWX | CWY,  -90,  -25,   90,   25, 0,0,0 }, /* ConfigurePage */
{  CWWidth | CWHeight | CWX | CWY,   40,    3,  500,  504, 0,0,0 }, /* tf */
{  CWWidth | CWHeight | CWX | CWY,  540,   10,   90,   25, 0,0,0 }, /* Editor */
{  CWWidth | CWHeight | CWX | CWY,  -36,  -25,   36,   25, 0,0,0 }, /* quit */
{  CWWidth | CWHeight | CWX | CWY,  545,   38,   90,   25, 0,0,0 }, /* Quit */
};

#if 0
   PrintDetails2(w, NULL);
#else
  if (result_index <= 0)
  {
     PrintDetails2(w, Expected);
     fflush(stdout);
     result_index ++;
  }
#endif
}

void Quit(Widget w, XtPointer client, XtPointer call)
{
	exit(0);
}

Widget CreateConfigureWindow(Widget parent);
Widget	form, toplevel, deleteButton, dbDescFileTf, dbFileTf, DBOptionMenu,
	dbNameTf, dbList;

int
main(int argc, char **argv)
{
	Widget		toplevel, nb, b, tf, cw;
	XtAppContext	app;
	Arg		al[10];
	int		ac;

	XtSetLanguageProc(NULL, NULL, NULL);

	toplevel = XtVaAppInitialize(&app, "Notebook", NULL, 0,
		&argc, argv, NULL, NULL);

	ac = 0;
	XtSetArg(al[ac], XmNbindingType, XmSPIRAL); ac++;
	nb = XmCreateNotebook(toplevel, "notebook", al, ac);

	ac = 0;
	XtSetArg(al[ac], XmNnotebookChildType, XmPAGE); ac++;
	XtSetArg(al[ac], XmNpageNumber, 1); ac++;
	tf = XmCreateTextField(nb, "tf", al, ac);
	XtManageChild(tf);

	ac = 0;
	XtSetArg(al[ac], XmNnotebookChildType, XmPAGE); ac++;
	XtSetArg(al[ac], XmNpageNumber, 2); ac++;
	b = XmCreatePushButton(nb, "quit", al, ac);
	XtManageChild(b);
	XtAddCallback(b, XmNactivateCallback, Quit, NULL);

	cw = CreateConfigureWindow(nb);
	XtManageChild(cw);
	ac = 0;
	XtSetArg(al[ac], XmNpageNumber, 0); ac++;
	XtSetValues(cw, al, ac);

	ac = 0;
	XtSetArg(al[ac], XmNpageNumber, 0); ac++;
	b = XmCreatePushButton(nb, "ConfigurePage", al, ac);
	XtManageChild(b);

	ac = 0;
	XtSetArg(al[ac], XmNpageNumber, 1); ac++;
	b = XmCreatePushButton(nb, "Editor", al, ac);
	XtManageChild(b);

	ac = 0;
	XtSetArg(al[ac], XmNpageNumber, 2); ac++;
	b = XmCreatePushButton(nb, "Quit", al, ac);
	XtManageChild(b);

	XtManageChild(nb);

	XtRealizeWidget(toplevel);

	check_geometry(toplevel);

	LessTifTestMainLoop(toplevel);

	exit(0);
}

#define	LARGE_WIDTH	400
#define	LARGE_HEIGHT	500
#define	SMALL_WIDTH	200
#define	SMALL_HEIGHT	250

/*
 * Function:
 *      CONVERT(w, from_string, to_type, to_size, success);
 * Description:
 *      A converter wrapper for convenience from BuilderXcessory.
 * Input:
 *      w - Widget : the widget to use for conversion
 *	from_string - char * : the string to convert from
 *	to_type - char * : the type to convert to
 *	to_size - int : the size of the conversion result
 *	success - Boolean* : Set to the result value of the conversion
 * Output:
 *      None
 */
XtPointer CONVERT(Widget w, char *from_string, char * to_type, int to_size, Boolean * success)
{
    XrmValue		fromVal, toVal;	/* resource holders		*/
    Boolean		convResult;	/* return value			*/
    unsigned char	oByte;		/* one byte result		*/
    unsigned short	tByte;		/* two byte result		*/
    XtPointer		fByte;		/* four byte result		*/
    XtPointer		aByte;		/* allocated result		*/
    
    /*
     * Zero it.
     */
    fByte = aByte = NULL;
    *success = False;

    /*
     * Sometimes we do not know this at code output.
     */
    if (to_size == 0)
    {
	if (!strcmp(to_type, XmRXmString) || !strcmp(to_type, XmRXmStringTable))
	{
	    to_size = sizeof(XtPointer);
	}
	else
	{
	    to_size = strlen(from_string);
	}
    }
        
    /*
     * Set up the list.
     */
    fromVal.size = strlen(from_string) + 1;
    fromVal.addr = from_string;

    switch( to_size )
    {
    case 1:
	toVal.size = sizeof(unsigned char);
	toVal.addr = (XtPointer)&oByte;
	break;
    case 2:
	toVal.size = sizeof(unsigned short);
	toVal.addr = (XtPointer)&tByte;
	break;
    default:
	toVal.size = sizeof(XtPointer);
	toVal.addr = (XtPointer)&fByte;
	break;
    }
    
    convResult = XtConvertAndStore(w, 
				   XmRString, 
				   &fromVal,
				   to_type,
				   &toVal);
    
    if( convResult )
    {
	switch( to_size )
	{
	case 1:
	    fByte = (XtPointer)((int)oByte);
	    break;
	case 2:
	    fByte = (XtPointer)((int)tByte);
	    break;
	default:
	    break;
	}
    }
    

    /*
     * Conversion will fail if we need more than 4 bytes.
     * For strings it will fail always the first time.
     */
    if( !convResult && toVal.size != to_size )
    {
	/*
	 * Need to allocate more space for this one.
	 */
	toVal.addr = (XtPointer)XtMalloc(toVal.size);
	fByte = aByte = toVal.addr;
	convResult = XtConvertAndStore(w, 
				       XmRString, 
				       &fromVal,
				       to_type,
				       &toVal);
    }
    
    /*
     * Free any thing useless we may have allocated.
     */
    if( !convResult )
    {
	XtFree((char*)aByte);
	aByte = NULL;
    }
    
    /*
     * Return the result.
     */
    *success = convResult;
    /*SUPPRESS 80*/
    return(fByte);
}

/*
 * Function: CreateConfigureWindow()
 *        Create configureWindow hierarchy of widgets.
 */
Widget
CreateConfigureWindow(Widget parent)
{
    Arg    	args[512];
    Cardinal   	argcnt;
    Boolean   	argok;
    Widget 	retval;
    Widget	configureWindow;
    Widget	applyButton;
    Widget	cancelButton;
    Widget	label1;
    Widget	scrolledWindow1;
    Widget	label2;
    Widget	menuShell;
    Widget	pulldownMenu5;
    Widget	pushButton5;
    Widget	label3;
    Widget	label4;
    Widget	label5;
    Widget	separator1;
    Widget	separator2;
    Widget	addDBButton;
    Widget	moveUpButton;
    Widget	moveDownButton;

    argok = False;

    argcnt = 0;
    XtSetArg(args[argcnt], XmNautoUnmanage, False); argcnt++;
    XtSetArg(args[argcnt], XmNresizePolicy, XmRESIZE_GROW); argcnt++;
    XtSetArg(args[argcnt], XmNx, 0); argcnt++;
    XtSetArg(args[argcnt], XmNy, 0); argcnt++;
    XtSetArg(args[argcnt], XmNwidth, 500); argcnt++;
    XtSetArg(args[argcnt], XmNheight, 504); argcnt++;
    configureWindow = XtCreateWidget("configureWindow",
		xmFormWidgetClass,
		parent,
		args,
		argcnt);
    retval = configureWindow;

    argcnt = 0;
    XtSetArg(args[argcnt], XmNlabelString, 
             CONVERT(parent,"Move Down", "XmString", 0, &argok)); if (argok) argcnt++;
    XtSetArg(args[argcnt], XmNrecomputeSize, False); argcnt++;
    XtSetArg(args[argcnt], XmNtopAttachment, XmATTACH_FORM); argcnt++;
    XtSetArg(args[argcnt], XmNbottomAttachment, XmATTACH_NONE); argcnt++;
    XtSetArg(args[argcnt], XmNleftAttachment, XmATTACH_FORM); argcnt++;
    XtSetArg(args[argcnt], XmNrightAttachment, XmATTACH_NONE); argcnt++;
    XtSetArg(args[argcnt], XmNtopOffset, 0); argcnt++;
    XtSetArg(args[argcnt], XmNbottomOffset, 0); argcnt++;
    XtSetArg(args[argcnt], XmNleftOffset, 25); argcnt++;
    XtSetArg(args[argcnt], XmNrightOffset, 0); argcnt++;
    XtSetArg(args[argcnt], XmNwidth, 99); argcnt++;
    XtSetArg(args[argcnt], XmNheight, 30); argcnt++;
    moveDownButton = XtCreateWidget("moveDownButton",
		xmPushButtonWidgetClass,
		configureWindow,
		args,
		argcnt);

    XtManageChild(moveDownButton);

    argcnt = 0;
    XtSetArg(args[argcnt], XmNlabelString, 
             CONVERT(parent,"Move Up", "XmString", 0, &argok)); if (argok) argcnt++;
    XtSetArg(args[argcnt], XmNrecomputeSize, False); argcnt++;
    XtSetArg(args[argcnt], XmNtopAttachment, XmATTACH_FORM); argcnt++;
    XtSetArg(args[argcnt], XmNbottomAttachment, XmATTACH_NONE); argcnt++;
    XtSetArg(args[argcnt], XmNleftAttachment, XmATTACH_FORM); argcnt++;
    XtSetArg(args[argcnt], XmNrightAttachment, XmATTACH_NONE); argcnt++;
    XtSetArg(args[argcnt], XmNtopOffset, 0); argcnt++;
    XtSetArg(args[argcnt], XmNbottomOffset, 0); argcnt++;
    XtSetArg(args[argcnt], XmNleftOffset, 25); argcnt++;
    XtSetArg(args[argcnt], XmNrightOffset, 0); argcnt++;
    XtSetArg(args[argcnt], XmNwidth, 99); argcnt++;
    XtSetArg(args[argcnt], XmNheight, 30); argcnt++;
    moveUpButton = XtCreateWidget("moveUpButton",
		xmPushButtonWidgetClass,
		configureWindow,
		args,
		argcnt);

    XtManageChild(moveUpButton);

    argcnt = 0;
    XtSetArg(args[argcnt], XmNlabelString, 
             CONVERT(parent,"New", "XmString", 0, &argok)); if (argok) argcnt++;
    XtSetArg(args[argcnt], XmNrecomputeSize, False); argcnt++;
    XtSetArg(args[argcnt], XmNtopAttachment, XmATTACH_FORM); argcnt++;
    XtSetArg(args[argcnt], XmNbottomAttachment, XmATTACH_NONE); argcnt++;
    XtSetArg(args[argcnt], XmNtopOffset, 10); argcnt++;
    XtSetArg(args[argcnt], XmNbottomOffset, 0); argcnt++;
    XtSetArg(args[argcnt], XmNx, 10); argcnt++;
    XtSetArg(args[argcnt], XmNwidth, 99); argcnt++;
    XtSetArg(args[argcnt], XmNheight, 30); argcnt++;
    addDBButton = XtCreateWidget("addDBButton",
		xmPushButtonWidgetClass,
		configureWindow,
		args,
		argcnt);

    XtManageChild(addDBButton);

    argcnt = 0;
    XtSetArg(args[argcnt], XmNtopAttachment, XmATTACH_FORM); argcnt++;
    XtSetArg(args[argcnt], XmNbottomAttachment, XmATTACH_NONE); argcnt++;
    XtSetArg(args[argcnt], XmNtopOffset, 50); argcnt++;
    XtSetArg(args[argcnt], XmNbottomOffset, 0); argcnt++;
    XtSetArg(args[argcnt], XmNx, 10); argcnt++;
    XtSetArg(args[argcnt], XmNwidth, 482); argcnt++;
    XtSetArg(args[argcnt], XmNheight, 20); argcnt++;
    separator2 = XtCreateWidget("separator2",
		xmSeparatorWidgetClass,
		configureWindow,
		args,
		argcnt);
    XtManageChild(separator2);

    argcnt = 0;
    XtSetArg(args[argcnt], XmNsensitive, True); argcnt++;
    XtSetArg(args[argcnt], XmNlabelString, 
             CONVERT(parent,"Delete", "XmString", 0, &argok)); if (argok) argcnt++;
    XtSetArg(args[argcnt], XmNrecomputeSize, False); argcnt++;
    XtSetArg(args[argcnt], XmNtopAttachment, XmATTACH_FORM); argcnt++;
    XtSetArg(args[argcnt], XmNbottomAttachment, XmATTACH_NONE); argcnt++;
    XtSetArg(args[argcnt], XmNleftAttachment, XmATTACH_FORM); argcnt++;
    XtSetArg(args[argcnt], XmNrightAttachment, XmATTACH_NONE); argcnt++;
    XtSetArg(args[argcnt], XmNtopOffset, 0); argcnt++;
    XtSetArg(args[argcnt], XmNbottomOffset, 0); argcnt++;
    XtSetArg(args[argcnt], XmNleftOffset, 25); argcnt++;
    XtSetArg(args[argcnt], XmNwidth, 100); argcnt++;
    XtSetArg(args[argcnt], XmNheight, 30); argcnt++;
    XtSetArg(args[argcnt], XmNtopAttachment, XmATTACH_OPPOSITE_WIDGET); argcnt++;
    XtSetArg(args[argcnt], XmNtopWidget, addDBButton); argcnt++;
    XtSetArg(args[argcnt], XmNleftAttachment, XmATTACH_WIDGET); argcnt++;
    XtSetArg(args[argcnt], XmNleftWidget, addDBButton); argcnt++;
    deleteButton = XtCreateWidget("deleteButton",
		xmPushButtonWidgetClass,
		configureWindow,
		args,
		argcnt);

    XtManageChild(deleteButton);

    argcnt = 0;
    XtSetArg(args[argcnt], XmNtopAttachment, XmATTACH_FORM); argcnt++;
    XtSetArg(args[argcnt], XmNbottomAttachment, XmATTACH_NONE); argcnt++;
    XtSetArg(args[argcnt], XmNleftAttachment, XmATTACH_FORM); argcnt++;
    XtSetArg(args[argcnt], XmNrightAttachment, XmATTACH_FORM); argcnt++;
    XtSetArg(args[argcnt], XmNtopOffset, 10); argcnt++;
    XtSetArg(args[argcnt], XmNbottomOffset, 0); argcnt++;
    XtSetArg(args[argcnt], XmNleftOffset, 10); argcnt++;
    XtSetArg(args[argcnt], XmNrightOffset, 10); argcnt++;
    XtSetArg(args[argcnt], XmNx, 10); argcnt++;
    XtSetArg(args[argcnt], XmNwidth, 480); argcnt++;
    XtSetArg(args[argcnt], XmNheight, 20); argcnt++;
    separator1 = XtCreateWidget("separator1",
		xmSeparatorWidgetClass,
		configureWindow,
		args,
		argcnt);
    XtManageChild(separator1);

    argcnt = 0;
    XtSetArg(args[argcnt], XmNtopAttachment, XmATTACH_FORM); argcnt++;
    XtSetArg(args[argcnt], XmNbottomAttachment, XmATTACH_NONE); argcnt++;
    XtSetArg(args[argcnt], XmNleftAttachment, XmATTACH_FORM); argcnt++;
    XtSetArg(args[argcnt], XmNrightAttachment, XmATTACH_FORM); argcnt++;
    XtSetArg(args[argcnt], XmNtopOffset, -3); argcnt++;
    XtSetArg(args[argcnt], XmNbottomOffset, 0); argcnt++;
    XtSetArg(args[argcnt], XmNleftOffset, 0); argcnt++;
    XtSetArg(args[argcnt], XmNrightOffset, 10); argcnt++;
    XtSetArg(args[argcnt], XmNwidth, 268); argcnt++;
    XtSetArg(args[argcnt], XmNheight, 36); argcnt++;
    dbDescFileTf = XtCreateWidget("dbDescFileTf",
		xmTextFieldWidgetClass,
		configureWindow,
		args,
		argcnt);

    XtManageChild(dbDescFileTf);

    argcnt = 0;
    XtSetArg(args[argcnt], XmNtopAttachment, XmATTACH_FORM); argcnt++;
    XtSetArg(args[argcnt], XmNbottomAttachment, XmATTACH_NONE); argcnt++;
    XtSetArg(args[argcnt], XmNleftAttachment, XmATTACH_FORM); argcnt++;
    XtSetArg(args[argcnt], XmNrightAttachment, XmATTACH_FORM); argcnt++;
    XtSetArg(args[argcnt], XmNtopOffset, 1); argcnt++;
    XtSetArg(args[argcnt], XmNbottomOffset, 0); argcnt++;
    XtSetArg(args[argcnt], XmNleftOffset, 0); argcnt++;
    XtSetArg(args[argcnt], XmNrightOffset, 10); argcnt++;
    XtSetArg(args[argcnt], XmNwidth, 268); argcnt++;
    XtSetArg(args[argcnt], XmNheight, 36); argcnt++;
    dbFileTf = XtCreateWidget("dbFileTf",
		xmTextFieldWidgetClass,
		configureWindow,
		args,
		argcnt);

    XtManageChild(dbFileTf);

    argcnt = 0;
    XtSetArg(args[argcnt], XmNlabelString, 
             CONVERT(parent,"Description File Name :", "XmString", 0, &argok)); if (argok) argcnt++;
    XtSetArg(args[argcnt], XmNrecomputeSize, False); argcnt++;
    XtSetArg(args[argcnt], XmNtopAttachment, XmATTACH_FORM); argcnt++;
    XtSetArg(args[argcnt], XmNbottomAttachment, XmATTACH_NONE); argcnt++;
    XtSetArg(args[argcnt], XmNtopOffset, 10); argcnt++;
    XtSetArg(args[argcnt], XmNbottomOffset, 0); argcnt++;
    XtSetArg(args[argcnt], XmNx, 10); argcnt++;
    XtSetArg(args[argcnt], XmNwidth, 200); argcnt++;
    XtSetArg(args[argcnt], XmNheight, 43); argcnt++;
    label5 = XtCreateWidget("label5",
		xmLabelWidgetClass,
		configureWindow,
		args,
		argcnt);
    XtManageChild(label5);

    argcnt = 0;
    XtSetArg(args[argcnt], XmNlabelString, 
             CONVERT(parent,"File Name :", "XmString", 0, &argok)); if (argok) argcnt++;
    XtSetArg(args[argcnt], XmNrecomputeSize, False); argcnt++;
    XtSetArg(args[argcnt], XmNtopAttachment, XmATTACH_FORM); argcnt++;
    XtSetArg(args[argcnt], XmNbottomAttachment, XmATTACH_NONE); argcnt++;
    XtSetArg(args[argcnt], XmNleftAttachment, XmATTACH_FORM); argcnt++;
    XtSetArg(args[argcnt], XmNrightAttachment, XmATTACH_FORM); argcnt++;
    XtSetArg(args[argcnt], XmNtopOffset, 10); argcnt++;
    XtSetArg(args[argcnt], XmNbottomOffset, 0); argcnt++;
    XtSetArg(args[argcnt], XmNleftOffset, 10); argcnt++;
    XtSetArg(args[argcnt], XmNrightOffset, 0); argcnt++;
    XtSetArg(args[argcnt], XmNx, 10); argcnt++;
    XtSetArg(args[argcnt], XmNwidth, 202); argcnt++;
    XtSetArg(args[argcnt], XmNheight, 42); argcnt++;
    label4 = XtCreateWidget("label4",
		xmLabelWidgetClass,
		configureWindow,
		args,
		argcnt);
    XtManageChild(label4);

    argcnt = 0;
    XtSetArg(args[argcnt], XmNlabelString, 
             CONVERT(parent,"Database Type :", "XmString", 0, &argok)); if (argok) argcnt++;
    XtSetArg(args[argcnt], XmNrecomputeSize, False); argcnt++;
    XtSetArg(args[argcnt], XmNtopAttachment, XmATTACH_FORM); argcnt++;
    XtSetArg(args[argcnt], XmNbottomAttachment, XmATTACH_NONE); argcnt++;
    XtSetArg(args[argcnt], XmNleftAttachment, XmATTACH_FORM); argcnt++;
    XtSetArg(args[argcnt], XmNrightAttachment, XmATTACH_FORM); argcnt++;
    XtSetArg(args[argcnt], XmNtopOffset, 10); argcnt++;
    XtSetArg(args[argcnt], XmNbottomOffset, 0); argcnt++;
    XtSetArg(args[argcnt], XmNleftOffset, 10); argcnt++;
    XtSetArg(args[argcnt], XmNrightOffset, 0); argcnt++;
    XtSetArg(args[argcnt], XmNx, 10); argcnt++;
    XtSetArg(args[argcnt], XmNwidth, 202); argcnt++;
    XtSetArg(args[argcnt], XmNheight, 41); argcnt++;
    label3 = XtCreateWidget("label3",
		xmLabelWidgetClass,
		configureWindow,
		args,
		argcnt);
    XtManageChild(label3);

    argcnt = 0;
    XtSetArg(args[argcnt], XmNlabelString, 
             CONVERT(parent," ", "XmString", 0, &argok)); if (argok) argcnt++;
    XtSetArg(args[argcnt], XmNtopAttachment, XmATTACH_FORM); argcnt++;
    XtSetArg(args[argcnt], XmNbottomAttachment, XmATTACH_NONE); argcnt++;
    XtSetArg(args[argcnt], XmNleftAttachment, XmATTACH_FORM); argcnt++;
    XtSetArg(args[argcnt], XmNrightAttachment, XmATTACH_FORM); argcnt++;
    XtSetArg(args[argcnt], XmNtopOffset, 10); argcnt++;
    XtSetArg(args[argcnt], XmNbottomOffset, 0); argcnt++;
    XtSetArg(args[argcnt], XmNleftOffset, 0); argcnt++;
    XtSetArg(args[argcnt], XmNrightOffset, 10); argcnt++;
    XtSetArg(args[argcnt], XmNrowColumnType, XmMENU_OPTION); argcnt++;
    XtSetArg(args[argcnt], XmNwidth, 268); argcnt++;
    XtSetArg(args[argcnt], XmNheight, 31); argcnt++;
    DBOptionMenu = XtCreateWidget("optionMenu1",
		xmRowColumnWidgetClass,
		configureWindow,
		args,
		argcnt);

    XtManageChild(DBOptionMenu);

    argcnt = 0;
    XtSetArg(args[argcnt], XmNtopAttachment, XmATTACH_FORM); argcnt++;
    XtSetArg(args[argcnt], XmNbottomAttachment, XmATTACH_NONE); argcnt++;
    XtSetArg(args[argcnt], XmNleftAttachment, XmATTACH_FORM); argcnt++;
    XtSetArg(args[argcnt], XmNrightAttachment, XmATTACH_FORM); argcnt++;
    XtSetArg(args[argcnt], XmNtopOffset, 0); argcnt++;
    XtSetArg(args[argcnt], XmNbottomOffset, 0); argcnt++;
    XtSetArg(args[argcnt], XmNleftOffset, 10); argcnt++;
    XtSetArg(args[argcnt], XmNrightOffset, 10); argcnt++;
    XtSetArg(args[argcnt], XmNwidth, 268); argcnt++;
    XtSetArg(args[argcnt], XmNheight, 36); argcnt++;
    dbNameTf = XtCreateWidget("dbNameTf",
		xmTextFieldWidgetClass,
		configureWindow,
		args,
		argcnt);

    XtManageChild(dbNameTf);

    argcnt = 0;
    XtSetArg(args[argcnt], XmNlabelString, 
             CONVERT(parent,"Database Name", "XmString", 0, &argok)); if (argok) argcnt++;
    XtSetArg(args[argcnt], XmNrecomputeSize, False); argcnt++;
    XtSetArg(args[argcnt], XmNtopAttachment, XmATTACH_FORM); argcnt++;
    XtSetArg(args[argcnt], XmNbottomAttachment, XmATTACH_NONE); argcnt++;
    XtSetArg(args[argcnt], XmNtopOffset, 10); argcnt++;
    XtSetArg(args[argcnt], XmNbottomOffset, 0); argcnt++;
    XtSetArg(args[argcnt], XmNx, 10); argcnt++;
    XtSetArg(args[argcnt], XmNwidth, 202); argcnt++;
    XtSetArg(args[argcnt], XmNheight, 40); argcnt++;
    XtSetArg(args[argcnt], XmNtopAttachment, XmATTACH_WIDGET); argcnt++;
    XtSetArg(args[argcnt], XmNtopWidget, separator2); argcnt++;
    label2 = XtCreateWidget("label2",
		xmLabelWidgetClass,
		configureWindow,
		args,
		argcnt);
    XtManageChild(label2);

    argcnt = 0;
    XtSetArg(args[argcnt], XmNscrollingPolicy, XmAPPLICATION_DEFINED); argcnt++;
    XtSetArg(args[argcnt], XmNtopAttachment, XmATTACH_FORM); argcnt++;
    XtSetArg(args[argcnt], XmNbottomAttachment, XmATTACH_NONE); argcnt++;
    XtSetArg(args[argcnt], XmNleftAttachment, XmATTACH_FORM); argcnt++;
    XtSetArg(args[argcnt], XmNrightAttachment, XmATTACH_FORM); argcnt++;
    XtSetArg(args[argcnt], XmNtopOffset, 10); argcnt++;
    XtSetArg(args[argcnt], XmNbottomOffset, 0); argcnt++;
    XtSetArg(args[argcnt], XmNleftOffset, 10); argcnt++;
    XtSetArg(args[argcnt], XmNrightOffset, 9); argcnt++;
    XtSetArg(args[argcnt], XmNx, 10); argcnt++;
    XtSetArg(args[argcnt], XmNwidth, 481); argcnt++;
    XtSetArg(args[argcnt], XmNheight, 100); argcnt++;
    scrolledWindow1 = XtCreateWidget("scrolledWindow1",
		xmScrolledWindowWidgetClass,
		configureWindow,
		args,
		argcnt);
    XtManageChild(scrolledWindow1);

    argcnt = 0;
    XtSetArg(args[argcnt], XmNlabelString, 
             CONVERT(parent,"Database Names :", "XmString", 0, &argok)); if (argok) argcnt++;
    XtSetArg(args[argcnt], XmNrecomputeSize, False); argcnt++;
    XtSetArg(args[argcnt], XmNleftAttachment, XmATTACH_FORM); argcnt++;
    XtSetArg(args[argcnt], XmNrightAttachment, XmATTACH_NONE); argcnt++;
    XtSetArg(args[argcnt], XmNleftOffset, 10); argcnt++;
    XtSetArg(args[argcnt], XmNx, 10); argcnt++;
    XtSetArg(args[argcnt], XmNy, 10); argcnt++;
    XtSetArg(args[argcnt], XmNwidth, 101); argcnt++;
    XtSetArg(args[argcnt], XmNheight, 30); argcnt++;
    label1 = XtCreateWidget("label1",
		xmLabelWidgetClass,
		configureWindow,
		args,
		argcnt);
    XtManageChild(label1);

    argcnt = 0;
    XtSetArg(args[argcnt], XmNlabelString, 
             CONVERT(parent,"Cancel", "XmString", 0, &argok)); if (argok) argcnt++;
    XtSetArg(args[argcnt], XmNrecomputeSize, False); argcnt++;
    XtSetArg(args[argcnt], XmNtopAttachment, XmATTACH_FORM); argcnt++;
    XtSetArg(args[argcnt], XmNbottomAttachment, XmATTACH_NONE); argcnt++;
    XtSetArg(args[argcnt], XmNleftAttachment, XmATTACH_FORM); argcnt++;
    XtSetArg(args[argcnt], XmNrightAttachment, XmATTACH_NONE); argcnt++;
    XtSetArg(args[argcnt], XmNtopOffset, 10); argcnt++;
    XtSetArg(args[argcnt], XmNleftOffset, 95); argcnt++;
    XtSetArg(args[argcnt], XmNrightOffset, 0); argcnt++;
    XtSetArg(args[argcnt], XmNwidth, 115); argcnt++;
    XtSetArg(args[argcnt], XmNheight, 31); argcnt++;
    XtSetArg(args[argcnt], XmNtopAttachment, XmATTACH_WIDGET); argcnt++;
    XtSetArg(args[argcnt], XmNtopWidget, separator1); argcnt++;
    cancelButton = XtCreateWidget("cancelButton",
		xmPushButtonWidgetClass,
		configureWindow,
		args,
		argcnt);

    XtManageChild(cancelButton);

    argcnt = 0;
    XtSetArg(args[argcnt], XmNlabelString, 
             CONVERT(parent,"Apply", "XmString", 0, &argok)); if (argok) argcnt++;
    XtSetArg(args[argcnt], XmNrecomputeSize, False); argcnt++;
    XtSetArg(args[argcnt], XmNtopAttachment, XmATTACH_FORM); argcnt++;
    XtSetArg(args[argcnt], XmNbottomAttachment, XmATTACH_NONE); argcnt++;
    XtSetArg(args[argcnt], XmNtopOffset, 10); argcnt++;
    XtSetArg(args[argcnt], XmNx, 20); argcnt++;
    XtSetArg(args[argcnt], XmNwidth, 115); argcnt++;
    XtSetArg(args[argcnt], XmNheight, 31); argcnt++;
    XtSetArg(args[argcnt], XmNtopAttachment, XmATTACH_WIDGET); argcnt++;
    XtSetArg(args[argcnt], XmNtopWidget, separator1); argcnt++;
    applyButton = XtCreateWidget("applyButton",
		xmPushButtonWidgetClass,
		configureWindow,
		args,
		argcnt);

    XtManageChild(applyButton);
    XtAddCallback(applyButton, XmNactivateCallback, Quit, 0);

    argcnt = 0;
    XtSetArg(args[argcnt], XmNleftOffset, 95); argcnt++;
    XtSetArg(args[argcnt], XmNleftAttachment, XmATTACH_WIDGET); argcnt++;
    XtSetArg(args[argcnt], XmNleftWidget, applyButton); argcnt++;
    XtSetValues(cancelButton, args, argcnt);

    argcnt = 0;
    XtSetArg(args[argcnt], XmNselectionPolicy, XmSINGLE_SELECT); argcnt++;
    XtSetArg(args[argcnt], XmNwidth, 481); argcnt++;
    XtSetArg(args[argcnt], XmNheight, 100); argcnt++;
    dbList = XtCreateWidget("dbList",
		xmListWidgetClass,
		scrolledWindow1,
		args,
		argcnt);

    XtManageChild(dbList);

    argcnt = 0;
    XtSetArg(args[argcnt], XmNtopOffset, 10); argcnt++;
    XtSetArg(args[argcnt], XmNtopAttachment, XmATTACH_WIDGET); argcnt++;
    XtSetArg(args[argcnt], XmNtopWidget, label1); argcnt++;
    XtSetValues(scrolledWindow1, args, argcnt);

    argcnt = 0;
    XtSetArg(args[argcnt], XmNtopOffset, 0); argcnt++;
    XtSetArg(args[argcnt], XmNtopAttachment, XmATTACH_OPPOSITE_WIDGET); argcnt++;
    XtSetArg(args[argcnt], XmNtopWidget, label2); argcnt++;
    XtSetArg(args[argcnt], XmNleftOffset, 10); argcnt++;
    XtSetArg(args[argcnt], XmNleftAttachment, XmATTACH_WIDGET); argcnt++;
    XtSetArg(args[argcnt], XmNleftWidget, label2); argcnt++;
    XtSetValues(dbNameTf, args, argcnt);

    argcnt = 0;
    XtSetArg(args[argcnt], XmNwidth, 1); argcnt++;
    XtSetArg(args[argcnt], XmNheight, 1); argcnt++;
    menuShell = XtCreatePopupShell("menuShell",
		xmMenuShellWidgetClass,
		XtParent(DBOptionMenu),
		args,
		argcnt);

    argcnt = 0;
    XtSetArg(args[argcnt], XmNrowColumnType, XmMENU_PULLDOWN); argcnt++;
    XtSetArg(args[argcnt], XmNx, 0); argcnt++;
    XtSetArg(args[argcnt], XmNy, 0); argcnt++;
    XtSetArg(args[argcnt], XmNwidth, 78); argcnt++;
    XtSetArg(args[argcnt], XmNheight, 25); argcnt++;
    pulldownMenu5 = XtCreateWidget("pulldownMenu5",
		xmRowColumnWidgetClass,
		menuShell,
		args,
		argcnt);


    argcnt = 0;
    pushButton5 = XtCreateWidget("pushButton5",
		xmPushButtonWidgetClass,
		pulldownMenu5,
		args,
		argcnt);

    XtManageChild(pushButton5);

    argcnt = 0;
    XtSetArg(args[argcnt], XmNtopOffset, 10); argcnt++;
    XtSetArg(args[argcnt], XmNtopAttachment, XmATTACH_WIDGET); argcnt++;
    XtSetArg(args[argcnt], XmNtopWidget, label2); argcnt++;
    XtSetArg(args[argcnt], XmNleftOffset, 0); argcnt++;
    XtSetArg(args[argcnt], XmNleftAttachment, XmATTACH_OPPOSITE_WIDGET); argcnt++;
    XtSetArg(args[argcnt], XmNleftWidget, dbNameTf); argcnt++;
    XtSetArg(args[argcnt], XmNsubMenuId, pulldownMenu5); argcnt++;
    XtSetValues(DBOptionMenu, args, argcnt);

    argcnt = 0;
    XtSetArg(args[argcnt], XmNtopOffset, 10); argcnt++;
    XtSetArg(args[argcnt], XmNtopAttachment, XmATTACH_WIDGET); argcnt++;
    XtSetArg(args[argcnt], XmNtopWidget, label2); argcnt++;
    XtSetArg(args[argcnt], XmNrightOffset, 0); argcnt++;
    XtSetArg(args[argcnt], XmNrightAttachment, XmATTACH_OPPOSITE_WIDGET); argcnt++;
    XtSetArg(args[argcnt], XmNrightWidget, label2); argcnt++;
    XtSetValues(label3, args, argcnt);

    argcnt = 0;
    XtSetArg(args[argcnt], XmNtopOffset, 10); argcnt++;
    XtSetArg(args[argcnt], XmNtopAttachment, XmATTACH_WIDGET); argcnt++;
    XtSetArg(args[argcnt], XmNtopWidget, label3); argcnt++;
    XtSetArg(args[argcnt], XmNrightOffset, 0); argcnt++;
    XtSetArg(args[argcnt], XmNrightAttachment, XmATTACH_OPPOSITE_WIDGET); argcnt++;
    XtSetArg(args[argcnt], XmNrightWidget, label3); argcnt++;
    XtSetValues(label4, args, argcnt);

    argcnt = 0;
    XtSetArg(args[argcnt], XmNtopOffset, 10); argcnt++;
    XtSetArg(args[argcnt], XmNtopAttachment, XmATTACH_WIDGET); argcnt++;
    XtSetArg(args[argcnt], XmNtopWidget, label4); argcnt++;
    XtSetValues(label5, args, argcnt);

    argcnt = 0;
    XtSetArg(args[argcnt], XmNtopOffset, 1); argcnt++;
    XtSetArg(args[argcnt], XmNtopAttachment, XmATTACH_OPPOSITE_WIDGET); argcnt++;
    XtSetArg(args[argcnt], XmNtopWidget, label4); argcnt++;
    XtSetArg(args[argcnt], XmNleftOffset, 0); argcnt++;
    XtSetArg(args[argcnt], XmNleftAttachment, XmATTACH_OPPOSITE_WIDGET); argcnt++;
    XtSetArg(args[argcnt], XmNleftWidget, dbNameTf); argcnt++;
    XtSetValues(dbFileTf, args, argcnt);

    argcnt = 0;
    XtSetArg(args[argcnt], XmNtopOffset, -3); argcnt++;
    XtSetArg(args[argcnt], XmNtopAttachment, XmATTACH_OPPOSITE_WIDGET); argcnt++;
    XtSetArg(args[argcnt], XmNtopWidget, label5); argcnt++;
    XtSetArg(args[argcnt], XmNleftOffset, 0); argcnt++;
    XtSetArg(args[argcnt], XmNleftAttachment, XmATTACH_OPPOSITE_WIDGET); argcnt++;
    XtSetArg(args[argcnt], XmNleftWidget, dbNameTf); argcnt++;
    XtSetValues(dbDescFileTf, args, argcnt);

    argcnt = 0;
    XtSetArg(args[argcnt], XmNtopOffset, 10); argcnt++;
    XtSetArg(args[argcnt], XmNtopAttachment, XmATTACH_WIDGET); argcnt++;
    XtSetArg(args[argcnt], XmNtopWidget, dbDescFileTf); argcnt++;
    XtSetValues(separator1, args, argcnt);

    argcnt = 0;
    XtSetArg(args[argcnt], XmNtopOffset, 50); argcnt++;
    XtSetArg(args[argcnt], XmNtopAttachment, XmATTACH_WIDGET); argcnt++;
    XtSetArg(args[argcnt], XmNtopWidget, scrolledWindow1); argcnt++;
    XtSetValues(separator2, args, argcnt);

    argcnt = 0;
    XtSetArg(args[argcnt], XmNtopOffset, 10); argcnt++;
    XtSetArg(args[argcnt], XmNtopAttachment, XmATTACH_WIDGET); argcnt++;
    XtSetArg(args[argcnt], XmNtopWidget, scrolledWindow1); argcnt++;
    XtSetValues(addDBButton, args, argcnt);

    argcnt = 0;
    XtSetArg(args[argcnt], XmNtopOffset, 0); argcnt++;
    XtSetArg(args[argcnt], XmNtopAttachment, XmATTACH_OPPOSITE_WIDGET); argcnt++;
    XtSetArg(args[argcnt], XmNtopWidget, addDBButton); argcnt++;
    XtSetArg(args[argcnt], XmNleftOffset, 25); argcnt++;
    XtSetArg(args[argcnt], XmNleftAttachment, XmATTACH_WIDGET); argcnt++;
    XtSetArg(args[argcnt], XmNleftWidget, deleteButton); argcnt++;
    XtSetValues(moveUpButton, args, argcnt);

    argcnt = 0;
    XtSetArg(args[argcnt], XmNtopOffset, 0); argcnt++;
    XtSetArg(args[argcnt], XmNtopAttachment, XmATTACH_OPPOSITE_WIDGET); argcnt++;
    XtSetArg(args[argcnt], XmNtopWidget, moveUpButton); argcnt++;
    XtSetArg(args[argcnt], XmNleftOffset, 25); argcnt++;
    XtSetArg(args[argcnt], XmNleftAttachment, XmATTACH_WIDGET); argcnt++;
    XtSetArg(args[argcnt], XmNleftWidget, moveUpButton); argcnt++;
    XtSetValues(moveDownButton, args, argcnt);

    return( retval );
}
