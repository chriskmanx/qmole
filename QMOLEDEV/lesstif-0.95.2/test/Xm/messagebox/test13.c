/*
From:        sg@ipsys.co.uk (Stephen Gallimore)
To:          lesstif@hungry.com
Subject:     New test for message boxes (but also shows a scrolled text problem)
Date:        Wed, 12 Aug 1998 10:51:12 +0100


Find below a new test for message boxes with a scrolled text widget as a
work area child. The test shows three problems with 0.85 as of a couple of
days ago and so presumably 0.86 .

1. When the message box is in a dialog shell and a scroll text widget (STW from
   now on) is added after the message box has been manged, the STW will be
   missing its scrollbars. Adding the STW before the message box is managed
   will work correctly. Note a test against 0.84 didn't show this problem.

2. The STW inside a message box does not get its vertical scrolling correct
   (this could be a general problem with the STW and nothing to do with
    message boxes - I haven't had the time to check) The vertical scrollbar
   is totally out of sync with the contents of the text widget. I was a
   little scared that I had introduced a problem with my XmStringExtent
   patches of a few days ago, but I don't think that is the case as the
   Scrolled List widget works correctly and a quick test against a 0.84
   installation at work this morning shows that it exhibits the same problem
   - so I think this may be a long standing problem.

3. In order to size a message box in a dialog shell it is necessary to place
   the message box inside a form widget and size the form instead. Placing
   a message box directly inside the dialog shell with width and height
   resources correctly sizes the shell window but the message box is 
   laid out as if no width and height resources were present, i.e. it
   squashes up in the top left hand corner.

None of the above problems happen on Solaris,AIX,HP or OSF-1 and were
discovered while porting existing live code to Lesstif.

regards,
Stephen Gallimore

*******************************************************************************
Lincoln Software Limited          Stephen Gallimore
Marlborough Court 
Pickford Street                   Tel:    +44 (0)1625 616722 
Macclesfield                      Fax:    +44 (0)1625 616780 
Cheshire. SK11 6JD                E-mail: stephen.gallimore@lincolnsoftware.com 
United Kingdom                    Web:    www.lincolnsoftware.com 
            "Never trust a crown green bowler under 30"
*******************************************************************************

---------------------------------- Cut Here ------------------------------
*/
/* Test13 , Showing a number of problems with scrolled text widgets as
   work areas in message box widgets in Lesstif 0.85 (and 0.86?). 
   Provided by Stephen Gallimore, sg@lincolnsoftware.com (12/08/1998)
*/
#include <Xm/Xm.h>
#include <Xm/MessageB.h>
#include <Xm/Text.h>
#include <Xm/Form.h>
#include <Xm/DialogS.h>

Widget toplevel;

int
main (int argc, char **argv)
{
  XtAppContext theApp;
  Widget dialog, w_edit,w_shell,w_form;
  XmString xmstr;
  int n;
  Arg args[10];
  char *txt = "This is some\ntext to edit.";
  toplevel = XtVaAppInitialize (&theApp, "test13", NULL, 0,
				&argc, argv, NULL, NULL);
  XtVaSetValues(toplevel,
   	XmNallowShellResize, True,
   	NULL);

/* BUG 1
   This shows a problem with the vertical scrolling of the scrolled
   text window. When the window first comes up a single line of
   text should be visible. However it is not possible to get to the
   second line of text using the scroll bar. Now stretch the window
   a bit and type some newlines until you get past the bottom so that
   the vertical scroll bar can be moved. Type in some text at this position.
   Now scroll up and down with the thumb and arrows, compare with scrolling
   in the text widget using the arrow keys.
   It should be fairly obvious that the vertical
   scrollbar is nowhere near in sync with the contents of the text widget.
 */
  dialog = XmCreateMessageBox(toplevel, "MyDialog", NULL, 0);

  XtManageChild (dialog);

  n=0;
  XtSetArg(args[n],XmNeditMode,XmMULTI_LINE_EDIT);n++;
  XtSetArg(args[n],XmNeditable,True);n++;
  XtSetArg(args[n],XmNvalue,(char *)txt);n++;
  w_edit = XmCreateScrolledText(dialog,"UItext",args,n);
  XtManageChild(w_edit);


/* BUG 2
   Problem of missing scrollbars on Scrolled Text inside a message box
   in a Dialog shell. This occurs when the scrolled window is added to
   the message box AFTER the message box widget is managed
 */    
    n = 0;
    XtSetArg(args[n], XmNdeleteResponse,XmDO_NOTHING); n++;
    w_shell = XmCreateDialogShell(toplevel,"UIinput",args,n);

    /* Create input dialog*/
/* BUG 3
   If we create the message box directly inside the dialog shell
   setting a width and height correctly sets the dimensions of the
   dialog shell, but the message box lays out as if no width and
   height resources had been set. This is not the behaviour on
   Solaris,AIX,HP and OSF-1 . Luckilly putting message boxes inside
   a form and sizing the form gets around this particular problem.
*/
#define SHOW_BUG3

#ifndef SHOW_BUG3
    n=0;
    XtSetArg(args[n], XmNwidth,300);n++;
    XtSetArg(args[n], XmNheight,300);n++;
    XtSetArg(args[n], XmNautoUnmanage,False);n++;
    XtSetArg(args[n], XmNdialogStyle, XmDIALOG_FULL_APPLICATION_MODAL);n++;
    w_form = XmCreateForm(w_shell,"",args,n);
    XtManageChild(w_form);

    n = 0;
    XtSetArg(args[n], XmNtopAttachment,XmATTACH_FORM);n++;
    XtSetArg(args[n], XmNleftAttachment,XmATTACH_FORM);n++;
    XtSetArg(args[n], XmNrightAttachment,XmATTACH_FORM);n++;
    XtSetArg(args[n], XmNbottomAttachment,XmATTACH_FORM);n++;
    dialog = XmCreateMessageBox(w_form, "MyDialog", args, n);
#else
    n = 0;
    XtSetArg(args[n], XmNwidth,300);n++;
    XtSetArg(args[n], XmNheight,300);n++;
    dialog = XmCreateMessageBox(w_shell, "MyDialog", args, n);
#endif

/* BUG 2 part 2
   Manage the message box dialog here and the scrolled text widget
   will not have scrollbars. This works on Solaris,AIX,HP and OSF-1

*/
   XtManageChild (dialog);

  n=0;
  XtSetArg(args[n],XmNeditMode,XmMULTI_LINE_EDIT);n++;
  XtSetArg(args[n],XmNeditable,True);n++;
  XtSetArg(args[n],XmNvalue,(char *)txt);n++;
  w_edit = XmCreateScrolledText(dialog,"UItext",args,n);
  XtManageChild(w_edit);
/* BUG 2 part 3
   Manage dialog after scrolled text has been added and the 
   scrolled text will have scrollbars. */
/*  XtManageChild (dialog); */

  XtPopup(w_shell,XtGrabNone);

  XtRealizeWidget (toplevel);

{
static XtWidgetGeometry Expected[] = {
   CWWidth | CWHeight            ,   56,   72,  214,  149, 0,0,0, /* MyDialog */
   CWWidth | CWHeight | CWX | CWY,    0,    0,    4,    4, 0,0,0, /* Symbol */
   CWWidth | CWHeight | CWX | CWY,   11,   11,  192,    4, 0,0,0, /* Message */
   CWWidth | CWHeight | CWX | CWY,    0,   85,  214,    2, 0,0,0, /* Separator */
   CWWidth | CWHeight | CWX | CWY,   11,   97,   64,   41, 0,0,0, /* OK */
   CWWidth | CWHeight | CWX | CWY,   75,   97,   64,   41, 0,0,0, /* Cancel */
   CWWidth | CWHeight | CWX | CWY,  139,   97,   64,   41, 0,0,0, /* Help */
   CWWidth | CWHeight | CWX | CWY,   11,   25,  192,   50, 0,0,0, /* UItextSW */
   CWWidth | CWHeight | CWX | CWY,    0,   35,  173,   15, 0,0,0, /* HorScrollBar */
   CWWidth | CWHeight | CWX | CWY,  177,    0,   15,   31, 0,0,0, /* VertScrollBar */
   CWWidth | CWHeight | CWX | CWY,    0,    0,  173,   31, 0,0,0, /* UItext */
};
/* toplevel should be replaced with to correct applicationShell */
PrintDetails(toplevel, Expected);
}
  LessTifTestMainLoop(toplevel);

  exit (0);
}

