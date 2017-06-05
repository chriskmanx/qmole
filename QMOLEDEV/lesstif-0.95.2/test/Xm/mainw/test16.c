/*
   From:        CP Hennessy <cp.hennessy@iname.com>
   To:          danny@hungry.com, rwscott@hungry.com
   Subject:     Re: [Bug 53] Changed - incorrect size of form used when doing 
   query_geometry
   Date:        Mon, 01 Feb 1999 01:24:31 +0100

   Hi,

   bughungry-daemon@lust.hungry.com wrote:
   > 
   > http://bugs.hungry.com/show_bug.cgi?id=53
   > + ------- Additional Comments From rwscott@hungry.com  01/27/99 15:49 -------
   > + Could you come up with an example that shows the problem that this fixes?
   > + I am hesitant to commit this without more detail.

   I am adding a piece of source code ( less than 200 lines )
   which should show you the problem. When you run it just
   click on the "File" button, and resize the window, click on the
   "File" button again.

   This may have something to do with the way the help button is used
   also.

   BTW when the fix I suggested is not used then the form seems to
   try to get the maximum size it can ( i.e. the screen size ?? ).

   I hope that I have give you enough.

   Should I fill in some info in the bug report ?

   CPFrom:        CP Hennessy <cp.hennessy@iname.com>
   To:          danny@hungry.com, rwscott@hungry.com
   Subject:     Re: [Bug 53] Changed - incorrect size of form used when doing 
   query_geometry
   Date:        Mon, 01 Feb 1999 01:24:31 +0100
 */

#include <stdio.h>
#include <stdlib.h>
#include <Xm/Form.h>
#include <Xm/Label.h>
#include <Xm/MainW.h>
#include <Xm/PanedW.h>
#include <Xm/Text.h>
#include <Xm/RowColumn.h>
#include <Xm/CascadeB.h>

static XtWidgetGeometry Expected[] = {
   CWWidth | CWHeight            ,  135,  413,  517,  380, 0,0,0, /* main */
   CWWidth | CWHeight | CWX | CWY,    0,    0,  517,   31, 0,0,0, /* main-menu */
   CWWidth | CWHeight | CWX | CWY,  436,    5,   76,   21, 0,0,0, /* helpButton */
   CWWidth | CWHeight | CWX | CWY,    5,    5,   40,   21, 0,0,0, /* File */
   CWWidth | CWHeight | CWX | CWY,    0,   31,  517,  349, 0,0,0, /* form */
   CWWidth | CWHeight | CWX | CWY,    0,    0,  517,  349, 0,0,0, /* textSW */
   CWWidth | CWHeight | CWX | CWY,    0,  334,  498,   15, 0,0,0, /* HorScrollBar */
   CWWidth | CWHeight | CWX | CWY,  502,    0,   15,  330, 0,0,0, /* VertScrollBar */
   CWWidth | CWHeight | CWX | CWY,    0,    0,  498,  330, 0,0,0, /* text */

   CWWidth | CWHeight            ,  135,  413,  517,  380, 0,0,0, /* main */
   CWWidth | CWHeight | CWX | CWY,    0,    0,  517,   31, 0,0,0, /* main-menu */
   CWWidth | CWHeight | CWX | CWY,  490,    5,   22,   21, 0,0,0, /* helpButton */
   CWWidth | CWHeight | CWX | CWY,    5,    5,   40,   21, 0,0,0, /* File */
   CWWidth | CWHeight | CWX | CWY,    0,   31,  517,  349, 0,0,0, /* form */
   CWWidth | CWHeight | CWX | CWY,    0,    0,  517,  349, 0,0,0, /* textSW */
   CWWidth | CWHeight | CWX | CWY,    0,  334,  498,   15, 0,0,0, /* HorScrollBar */
   CWWidth | CWHeight | CWX | CWY,  502,    0,   15,  330, 0,0,0, /* VertScrollBar */
   CWWidth | CWHeight | CWX | CWY,    0,    0,  498,  330, 0,0,0, /* text */

   CWWidth | CWHeight            ,  135,  413,  517,  380, 0,0,0, /* main */
   CWWidth | CWHeight | CWX | CWY,    0,    0,  517,   31, 0,0,0, /* main-menu */
   CWWidth | CWHeight | CWX | CWY,  424,    5,   88,   21, 0,0,0, /* helpButton */
   CWWidth | CWHeight | CWX | CWY,    5,    5,   40,   21, 0,0,0, /* File */
   CWWidth | CWHeight | CWX | CWY,    0,   31,  517,  349, 0,0,0, /* form */
   CWWidth | CWHeight | CWX | CWY,    0,    0,  517,  349, 0,0,0, /* textSW */
   CWWidth | CWHeight | CWX | CWY,    0,  334,  498,   15, 0,0,0, /* HorScrollBar */
   CWWidth | CWHeight | CWX | CWY,  502,    0,   15,  330, 0,0,0, /* VertScrollBar */
   CWWidth | CWHeight | CWX | CWY,    0,    0,  498,  330, 0,0,0, /* text */

   CWWidth | CWHeight            ,  135,  413,  517,  380, 0,0,0, /* main */
   CWWidth | CWHeight | CWX | CWY,    0,    0,  517,   31, 0,0,0, /* main-menu */
   CWWidth | CWHeight | CWX | CWY,  490,    5,   22,   21, 0,0,0, /* helpButton */
   CWWidth | CWHeight | CWX | CWY,    5,    5,   40,   21, 0,0,0, /* File */
   CWWidth | CWHeight | CWX | CWY,    0,   31,  517,  349, 0,0,0, /* form */
   CWWidth | CWHeight | CWX | CWY,    0,    0,  517,  349, 0,0,0, /* textSW */
   CWWidth | CWHeight | CWX | CWY,    0,  334,  498,   15, 0,0,0, /* HorScrollBar */
   CWWidth | CWHeight | CWX | CWY,  502,    0,   15,  330, 0,0,0, /* VertScrollBar */
   CWWidth | CWHeight | CWX | CWY,    0,    0,  498,  330, 0,0,0, /* text */

   CWWidth | CWHeight            ,  135,  413,  775,  380, 0,0,0, /* main */
   CWWidth | CWHeight | CWX | CWY,    0,    0,  775,   31, 0,0,0, /* main-menu */
   CWWidth | CWHeight | CWX | CWY,  748,    5,   22,   21, 0,0,0, /* helpButton */
   CWWidth | CWHeight | CWX | CWY,    5,    5,   40,   21, 0,0,0, /* File */
   CWWidth | CWHeight | CWX | CWY,    0,   31,  775,  349, 0,0,0, /* form */
   CWWidth | CWHeight | CWX | CWY,    0,    0,  775,  349, 0,0,0, /* textSW */
   CWWidth | CWHeight | CWX | CWY,    0,  334,  756,   15, 0,0,0, /* HorScrollBar */
   CWWidth | CWHeight | CWX | CWY,  760,    0,   15,  330, 0,0,0, /* VertScrollBar */
   CWWidth | CWHeight | CWX | CWY,    0,    0,  756,  330, 0,0,0, /* text */

   CWWidth | CWHeight            ,  135,  413,  775,  380, 0,0,0, /* main */
   CWWidth | CWHeight | CWX | CWY,    0,    0,  775,   31, 0,0,0, /* main-menu */
   CWWidth | CWHeight | CWX | CWY,  682,    5,   88,   21, 0,0,0, /* helpButton */
   CWWidth | CWHeight | CWX | CWY,    5,    5,   40,   21, 0,0,0, /* File */
   CWWidth | CWHeight | CWX | CWY,    0,   31,  775,  349, 0,0,0, /* form */
   CWWidth | CWHeight | CWX | CWY,    0,    0,  775,  349, 0,0,0, /* textSW */
   CWWidth | CWHeight | CWX | CWY,    0,  334,  756,   15, 0,0,0, /* HorScrollBar */
   CWWidth | CWHeight | CWX | CWY,  760,    0,   15,  330, 0,0,0, /* VertScrollBar */
   CWWidth | CWHeight | CWX | CWY,    0,    0,  756,  330, 0,0,0, /* text */

   CWWidth | CWHeight            ,  135,  413,  775,  380, 0,0,0, /* main */
   CWWidth | CWHeight | CWX | CWY,    0,    0,  775,   31, 0,0,0, /* main-menu */
   CWWidth | CWHeight | CWX | CWY,  748,    5,   22,   21, 0,0,0, /* helpButton */
   CWWidth | CWHeight | CWX | CWY,    5,    5,   40,   21, 0,0,0, /* File */
   CWWidth | CWHeight | CWX | CWY,    0,   31,  775,  349, 0,0,0, /* form */
   CWWidth | CWHeight | CWX | CWY,    0,    0,  775,  349, 0,0,0, /* textSW */
   CWWidth | CWHeight | CWX | CWY,    0,  334,  756,   15, 0,0,0, /* HorScrollBar */
   CWWidth | CWHeight | CWX | CWY,  760,    0,   15,  330, 0,0,0, /* VertScrollBar */
   CWWidth | CWHeight | CWX | CWY,    0,    0,  756,  330, 0,0,0, /* text */

   CWWidth | CWHeight            ,  135,  413,  775,  380, 0,0,0, /* main */
   CWWidth | CWHeight | CWX | CWY,    0,    0,  775,   31, 0,0,0, /* main-menu */
   CWWidth | CWHeight | CWX | CWY,  748,    5,   22,   21, 0,0,0, /* helpButton */
   CWWidth | CWHeight | CWX | CWY,    5,    5,   40,   21, 0,0,0, /* File */
   CWWidth | CWHeight | CWX | CWY,    0,   31,  775,  349, 0,0,0, /* form */
   CWWidth | CWHeight | CWX | CWY,    0,    0,  775,  349, 0,0,0, /* textSW */
   CWWidth | CWHeight | CWX | CWY,    0,  334,  756,   15, 0,0,0, /* HorScrollBar */
   CWWidth | CWHeight | CWX | CWY,  760,    0,   15,  330, 0,0,0, /* VertScrollBar */
   CWWidth | CWHeight | CWX | CWY,    0,    0,  756,  330, 0,0,0, /* text */

   CWWidth | CWHeight            ,  135,  413,  775,  380, 0,0,0, /* main */
   CWWidth | CWHeight | CWX | CWY,    0,    0,  775,   31, 0,0,0, /* main-menu */
   CWWidth | CWHeight | CWX | CWY,  682,    5,   88,   21, 0,0,0, /* helpButton */
   CWWidth | CWHeight | CWX | CWY,    5,    5,   40,   21, 0,0,0, /* File */
   CWWidth | CWHeight | CWX | CWY,    0,   31,  775,  349, 0,0,0, /* form */
   CWWidth | CWHeight | CWX | CWY,    0,    0,  775,  349, 0,0,0, /* textSW */
   CWWidth | CWHeight | CWX | CWY,    0,  334,  756,   15, 0,0,0, /* HorScrollBar */
   CWWidth | CWHeight | CWX | CWY,  760,    0,   15,  330, 0,0,0, /* VertScrollBar */
   CWWidth | CWHeight | CWX | CWY,    0,    0,  756,  330, 0,0,0, /* text */
};

Widget button, menuBar, mainW, text, helpButton, form;

void ChangeHelpButton
  (
      Widget fileB,
      Widget helpButton,
      XtPointer call_data)
{
    static int index = 1;
    XmString xmstr;
    switch (index)
    {
    case 1:
	XmTextSetString(text, "1:a small test");
	break;

    case 2:
	XmTextSetString(text, "2:a bigger test");
	break;

    case 3:
	XmTextSetString(text, "3:another small test");

    default:
	index = 0;
    }

    switch (index)
    {
    case 1:
	xmstr = XmStringCreateLocalized("1");
	break;

    case 2:
	xmstr = XmStringCreateLocalized("a bit bigger");
	break;

    case 3:
	xmstr = XmStringCreateLocalized("small");
	break;

    case 4:
	xmstr = XmStringCreateLocalized("really enormously big");
	break;

    default:
	xmstr = XmStringCreateLocalized("0");
	index = 0;
    }
    index++;

    XtVaSetValues(helpButton,
		  XmNlabelString, xmstr,
		  (String *)NULL);

    XmStringFree(xmstr);

}

/*-------------------------------------------------------------
**      CreateMainEditorWindow
**              Create MainWindow and subwidgets.
*/
Widget CreateMainEditorWindow
  (
      Widget app_shell)
{
    Arg al[10];			/*  arg list    */
    Cardinal ac;		/*  arg count   */

    ac = 0;
    XtSetArg(al[ac], XmNshadowThickness, 0);
    ac++;
    XtSetArg(al[ac], XmNgeometry, "80x24");
    ac++;
    mainW = XmCreateMainWindow(app_shell, "main", al, ac);

    menuBar = XmCreateMenuBar(mainW, "main-menu", NULL, 0);
    XtManageChild(menuBar);

    helpButton = XmCreateCascadeButton(menuBar, "helpButton", NULL, 0);
    XtManageChild(helpButton);

    button = XmCreateCascadeButton(menuBar, "File", NULL, 0);
    XtManageChild(button);
    XtAddCallback(button, XmNactivateCallback,
		  (XtCallbackProc)ChangeHelpButton, (XtPointer)helpButton);

    ac = 0;
    form = XmCreateForm(mainW, "form", al, ac);
    XtManageChild(form);

    ac = 0;
    XtSetArg(al[ac], XmNeditMode, XmMULTI_LINE_EDIT);
    ac++;
    XtSetArg(al[ac], XmNrows, 24);
    ac++;
    XtSetArg(al[ac], XmNcolumns, 80);
    ac++;
    XtSetArg(al[ac], XmNtopAttachment, XmATTACH_FORM);
    ac++;
    XtSetArg(al[ac], XmNbottomAttachment, XmATTACH_FORM);
    ac++;
    XtSetArg(al[ac], XmNleftAttachment, XmATTACH_FORM);
    ac++;
    XtSetArg(al[ac], XmNrightAttachment, XmATTACH_FORM);
    ac++;
    text = XmCreateScrolledText(form, "text", al, ac);
    XtManageChild(text);

    XtVaSetValues(menuBar,
		  XmNmenuHelpWidget, (XtArgVal)helpButton,
		  (String *)NULL);
    XtManageChild(mainW);
    XtManageChild(button);

    return (mainW);
}

Widget CreateTopLevel
  (
      String applicationName,
      Display *display)
{
    Widget toplevel;
    toplevel = XtVaAppCreateShell(NULL, applicationName,
				  applicationShellWidgetClass, display,
				  XmNallowShellResize, (XtArgVal)True,
				  NULL);

    return (toplevel);
}

Widget initX
  (
      XtAppContext *context,
      String application,
      XrmOptionDescList options,
      Cardinal num_options,
      unsigned int *argc,
      String *argv)
{
    Display *disp;
    XtToolkitInitialize();
    *context = XtCreateApplicationContext();

    disp = XtOpenDisplay(*context, NULL, application, application,
			 options, num_options, (int *)argc, argv);

    if (disp == NULL)
    {
	(void)fprintf(stderr, "ERROR:Cannot open display :%s\n", (String)getenv("DISPLAY"));
	exit(1);
    }

    return (CreateTopLevel(application, disp));
}

int main
  (
      int argc,
      String *argv)

{
    XtAppContext context;

    Widget topLevel = initX(&context, "textHelpButton", NULL,
			    0, &argc, argv);

    Widget mainWindow = CreateMainEditorWindow(topLevel);

    XtManageChild(mainWindow);

    XtRealizeWidget(topLevel);

    LessTifTestWaitForIt(topLevel);
    PrintDetails(topLevel, Expected);

    LessTifTestBtn1Down(button);
    LessTifTestBtn1Up(button);
    PrintDetails(topLevel, Expected);

    LessTifTestBtn1Down(button);
    LessTifTestBtn1Up(button);
    PrintDetails(topLevel, Expected);

    LessTifTestBtn1Down(button);
    LessTifTestBtn1Up(button);
    PrintDetails(topLevel, Expected);

    LessTifTestBtn1Down(button);
    LessTifTestBtn1Up(button);
    PrintDetails(topLevel, Expected);

    {
    Dimension w;

    	XtVaGetValues(topLevel,
    		XmNwidth, &w,
    		NULL);
    	XtVaSetValues(topLevel,
    		XmNwidth, (3 * w) / 2,
    		NULL);
    }

    LessTifTestBtn1Down(button);
    LessTifTestBtn1Up(button);
    PrintDetails(topLevel, Expected);

    LessTifTestBtn1Down(button);
    LessTifTestBtn1Up(button);
    PrintDetails(topLevel, Expected);

    LessTifTestBtn1Down(button);
    LessTifTestBtn1Up(button);
    PrintDetails(topLevel, Expected);

    LessTifTestBtn1Down(button);
    LessTifTestBtn1Up(button);
    PrintDetails(topLevel, Expected);

    LessTifTestMainLoop(topLevel);
    /*
       XtAppMainLoop  ( context );
     */

    return (-1);
}
