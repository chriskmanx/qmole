/* $Id: test17.c,v 1.2 1994/07/20 23:07:55 rwscott Exp $ */
/*
modal text does not expand modal correctly.


From: Todd Denniston
Subject: modal text does not expand modal correctly.
Date: Wed, 12 Jul 2000 20:31:29 -0700


We have an application which behaves differently when built with Lesstif vs when
built with Motif.
There are three differences:
1.  When the modal is displayed with Motif the buttons on the base application can
not be pressed again, this is expected behavior, but with Lesstif they can be
pressed over and over again.

2.  When the modal is displayed with Motif the modal is resized to account for the
size, number of lines, of the text shown.  Lesstif does not resize at all.  I
believe this has been discussed here before but I do not know the outcome, were
there any easy workarounds?

3.  When the modal is displayed with Motif the text spans the modal to within ~5
pixels of each side of the modal, with Lesstif the text only spans about half of
the modal left justified.


We have found a workaround for #1 in our program, but we are still working on 2
and 3, as time is pressing for us pointers to where to look in the Lesstif code
might yield Lesstif fixes, otherwise we'll probably hack something up in the code
we know.



Thanks for everything.

setup:
all programs display on the linux host with an FVWM2 window manager
- linux
Linux mars 2.2.14 #1-mars first 2.2.14 Fri Mar 31 13:13:01 EST 2000 i686 unknown
        Slackware 7.0 system - stock + lesstif
        lesstif-0.91.4 - built from source, installed to ./lib91.4/
        attached program

compile:
gcc modal.c -I. -I./lib91.4/include/ -I/usr/X11R6/include/X11 \
-I/usr/include/pthread/mit/sys -ggdb -L./lib91.4/lib/ -L/usr/X11R6/lib \
-lXm -lXt -lX11 -lXext -o modal


- solaris
SunOS pegasus 5.6 Generic_105181-12 sun4m sparc SUNW,SPARCstation-5
        Motif(tm?) - whatever version came with the solaris 2.6 install
        gcc --version = 2.8.1
        attached program

compile:
#to get gcc and some needed libs
export PATH=/usr/local/bin:$PATH;export LD_LIBRARY_PATH=/usr/openwin/lib/
gcc modal.c -I. -I/usr/remote/lib/lesstif/include/ \
-I/usr/X11R6/include/X11 -I/usr/include/pthread/mit/sys -ggdb \
-I/usr/openwin/share/include/X11 -I/usr/openwin/share/include/X11/ \
-L/usr/lib -L/usr/openwin/lib/ -L/usr/X11R6/lib -lXm -lXt -lX11 -lXext \
-lsocket -lnsl -lposix4

-- 
______________________________________________________________________________
Todd Denniston, NSWC Crane
E-Mail: <A  HREF="mailto:Todd.Denniston@SSA.Crane.Navy.Mil">mailto:Todd.Denniston@SSA.Crane.Navy.Mil</A></PRE>

*/

/* 
   minor changes from amai
     fix some compiler warnings. 
     Especially the long string displayed had to be fixed.
*/

#include <Xm/DrawingA.h>
#include <Xm/DrawnB.h>
#include <Xm/Form.h>
#include <Xm/Label.h>
#include <Xm/Text.h>
#include <Xm/RowColumn.h>
#include <Xm/MwmUtil.h>
#include <Xm/MessageB.h>
#include <Xm/PushB.h>
#include <Xm/Xm.h>
#include <stdlib.h>
#include <stdio.h>

#define FractionBase 100

static void pushed(Widget dialog, XtPointer client_data, 
                     XtPointer cbs);
void dlg_callback(Widget dialog, XtPointer client_dat,
                  XmAnyCallbackStruct *cbs);
static void myexit(Widget dialog, XtPointer  client_data, 
                    XtPointer cbs);

typedef struct {
    Widget  DialogWidget;
    Widget  ModalAlertButton;
} DialogType;
DialogType ModalAlertDialog;

char *font4name = "*helvetica-medium-r-*-24*";
XFontStruct *font4 = NULL;
XtAppContext app;
Widget toplevel;
Display *display;


int main(argc, argv)
    char *argv[];
{
Widget button, rowcolumn;

    toplevel = XtVaAppInitialize(&app, "Test",
                                 NULL, 0, &argc, argv, NULL, NULL);

    rowcolumn = XtCreateManagedWidget("rowcolumn",
                                      xmRowColumnWidgetClass, 
                                      toplevel, NULL, 0);

    button = XtCreateManagedWidget("Create Modal",
                                   xmPushButtonWidgetClass, 
                                   rowcolumn, NULL, 0);
    XtAddCallback(button, XmNactivateCallback,
                  pushed, (XtPointer)NULL);

    button = XtCreateManagedWidget("EXIT",
                                   xmPushButtonWidgetClass, 
                                   rowcolumn, NULL, 0);
    XtAddCallback(button, XmNactivateCallback, myexit,
                  NULL);
    display = XtDisplay(toplevel);
    if((font4 = XLoadQueryFont(display,font4name)) == NULL)
    {
        printf("\ncouldn't get font4");
    }

    XtRealizeWidget(toplevel);
    LessTifTestMainLoop(toplevel);
    /*
    XtAppMainLoop(app);
    */

    exit(0);

}

static void pushed(Widget dialog, XtPointer client_data, 
                     XtPointer cbs)

{
  Widget MessageArea;
  XFontStruct   *FontStruct = font4;
  XmFontList fontlist = XmFontListCreate(FontStruct,"charset4");
  int decorations = MWM_DECOR_TITLE | MWM_DECOR_BORDER; 
  Screen *scrn = XtScreen(toplevel);
  Arg args[11];
  int i=0;
  int center_offset = 15;
  const Dimension width = 640;
  int message_length;
  int message_width;
  char message[]="we really should see this whole message on the screen at one time, \
RaaLly we should.";

  /* set dialog's resources */
  XtSetArg(args[i], XmNdialogStyle, XmDIALOG_FULL_APPLICATION_MODAL);i++;  
  XtSetArg(args[i], XmNdeleteResponse,XmDESTROY);i++;
  XtSetArg(args[i], XmNtitle,"TEST");i++;
  XtSetArg(args[i], XmNfractionBase, FractionBase);i++;
  XtSetArg(args[i], XmNwidth, width);i++;
  XtSetArg(args[i], XmNhorizontalSpacing, 15);i++;
  XtSetArg(args[i], XmNverticalSpacing, 15);i++;
  XtSetArg(args[i], XmNmwmDecorations, decorations);i++; 
  XtSetArg(args[i], XmNforeground, WhitePixelOfScreen(scrn));i++; 
  XtSetArg(args[i], XmNbackground, BlackPixelOfScreen(scrn));i++;
   
  /*create form dialog */
  ModalAlertDialog.DialogWidget = XmCreateFormDialog( toplevel,
                                         "ModalAlertDialog",args,i);
  
  



  /*
   * create alert message to display 
   */

  /* find offsets from sides to center the text */
  message_length = strlen(message);
  message_width  =  XTextWidth(FontStruct,message,message_length);
  

  if (message_width < width )
    {
      /* an extra 5 pixels (per side) are needed for x to be happy in 
       * it's wrapping.
       */
      center_offset = ((width - message_width ) / 2) - 5;
    }
  else
    {
      /* don't allow text to touch the side of the window.*/
      center_offset = 1;
    }

  MessageArea = 
      XtVaCreateManagedWidget("message_area",
                              xmTextWidgetClass,
                              ModalAlertDialog.DialogWidget,
                              XmNrightAttachment, XmATTACH_FORM,
                              XmNleftAttachment,  XmATTACH_FORM,
                              XmNtopAttachment,   XmATTACH_FORM,
                              XmNbottomAttachment,   XmATTACH_FORM,
                              XmNleftOffset,      center_offset,
                              XmNrightOffset,     center_offset,
                              XmNeditable,False,
                              XmNeditMode,XmMULTI_LINE_EDIT,
                              XmNforeground, WhitePixelOfScreen(scrn),
                              XmNbackground, BlackPixelOfScreen(scrn),
                              XmNwordWrap,True,
                              XmNresizeHeight,True,
                              XmNcursorPositionVisible,False,
                              XmNshadowThickness,0,
                              XmNborderWidth,0,
                              XmNhighlightThickness,0,
                              XmNvalue, message,
                              XmNfontList, fontlist,
                              NULL);

   
 
  
  /*END DIALOG BODY*/

  XtManageChild(ModalAlertDialog.DialogWidget);
  XtPopup(XtParent(ModalAlertDialog.DialogWidget),XtGrabNone);
   
   
  /* free X resource */
  XmFontListFree(fontlist);

}


void dlg_callback(dialog, client_data, cbs)
Widget dialog;
XtPointer client_data;
XmAnyCallbackStruct *cbs;
{
    XtPopdown(XtParent(dialog));
}

static void myexit(Widget dialog,XtPointer client_data, 
                     XtPointer cbs )
{
    exit(0);
}
