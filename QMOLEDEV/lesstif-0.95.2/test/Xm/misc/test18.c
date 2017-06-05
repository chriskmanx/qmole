/* $Header: /cvsroot/lesstif/lesstif/test/Xm/misc/test18.c,v 1.4 2001/05/15 14:20:17 amai Exp $

From:        Ken Murchison <ken@oceana.com>
To:          lesstif@lesstif.org
Subject:     Visual differences with Motif
Date:        Sat, 14 Nov 1998 05:12:57 +0000

I have noticed a few visual differences between Lesstif and Motif that
have been around since at least 0.85, so I figured it was time to speak
up.  The three things that I have noticed are listed below and are
demonstrated in the attached code.

1. Option menu behavior (see option menu)

Motif: A click on the cascade button pops up the pulldown menu (note
that the cascade button does NOT stay depressed).  A second click on the
cascade button pops DOWN the pulldown menu.

Lesstif: A click on the cascade button pops up the pulldown menu (note
that the cascade button stays depressed).  A second click on the cascade
button does nothing.  You must either select an item in the menu or
click outside the widget to pop down the pulldown menu.

2. Multi-line toggle button in a menu (see menubar)

Motif: All toggle buttons in a menu are the same size regardless of the
number of lines of text contained in the labels.

Lesstif: The size of the toggle buttons is proportional to the number of
lines of text contained in the labels.

3. Multi-line toggle button (see checkboxes)

Motif: The toggle button is top-justified with the first line of text in
the label.

Lesstif: The toggle button is vertically centered w.r.t. the label text.

I'll take a crack at fixing these things when I get time, but I'm sure
they can be fixed fairly quickly by one of the core guys.

Regards,
Ken
--
Kenneth Murchison          Oceana Matrix Ltd.
Software Engineer          21 Princeton Place
mailto:ken@oceana.com      Orchard Park, NY 14127
http://www.oceana.com      716-662-8973 x26
*/


#include <stdlib.h>

#include <Xm/RowColumn.h>
#include <Xm/MainW.h>
#include <Xm/ScrolledW.h>
#include <Xm/PushB.h>
#include <Xm/ToggleB.h>
#include <Xm/CascadeBG.h>

#include "../../common/Test.h"

int
main(int argc, char *argv[])
{
    Widget toplevel, main_w, rc, option_menu;
    Widget menubar, pulldown, cascade;
    XmString label, text0, text1, text2;
    XtAppContext app;

    XtSetLanguageProc (NULL, NULL, NULL);

    toplevel = XtVaAppInitialize (&app, "Lesstif Test", NULL, 0,
				  &argc, argv, NULL, NULL);

    /* Create a MainWindow widget that contains a RowColumn
     * widget as its work window.
     */
    main_w = XtVaCreateManagedWidget ("main_w",
        xmMainWindowWidgetClass, toplevel, NULL);

    rc = XtVaCreateWidget ("rowcol", xmRowColumnWidgetClass, main_w, NULL);

    /* Create a MenuBar and PulldownMenu with three ToggleButtons:
     * one with a single line label, one with a 2 line label,
     * and one with a 3 line label.
     */
    label = XmStringCreateLocalized ("Pulldown");
    menubar = XmVaCreateSimpleMenuBar (main_w, "menubar",
				     XmVaCASCADEBUTTON, label, 'P',
				     NULL);
    XmStringFree (label);

    text0 = XmStringCreateLocalized("1-line Toggle");
    text1 = XmStringCreateLtoR("2-line\nToggle", XmFONTLIST_DEFAULT_TAG);
    text2 = XmStringCreateLtoR("3-\nline\nToggle", XmFONTLIST_DEFAULT_TAG);
    pulldown =
      XmVaCreateSimplePulldownMenu (menubar, "_pulldown", 0, 0,
				    XmVaTOGGLEBUTTON, text0, '1', NULL, NULL,
				    XmVaTOGGLEBUTTON, text1, '2', NULL, NULL,
				    XmVaTOGGLEBUTTON, text2, '3', NULL, NULL,
				    NULL);

    XtManageChild (menubar);

    /* Create three ToggleButtons: one with a single line label,
     * one with a 2 line label, and one with a three line label.
     */
    XtVaCreateManagedWidget("tb0",
			    xmToggleButtonWidgetClass, rc,
			    XmNlabelString, text0,
			    NULL);
    XmStringFree(text0);

    XtVaCreateManagedWidget("tb1",
			    xmToggleButtonWidgetClass, rc,
			    XmNlabelString, text1,
			    NULL);
    XmStringFree(text1);

    XtVaCreateManagedWidget("tb2",
			    xmToggleButtonWidgetClass, rc,
			    XmNlabelString, text2,
			    NULL);
    XmStringFree(text2);

    /* Create an OptionMenu with three PushButtons.
     */
    label = XmStringCreateLocalized ("Option:");
    text0 = XmStringCreateLocalized ("Opt 1");
    text1 = XmStringCreateLocalized ("Opt 2");
    text2 = XmStringCreateLocalized ("Opt 3");
    option_menu = XmVaCreateSimpleOptionMenu (rc, "option_menu",
        label, 'O', 0 /*initial menu selection*/, 0,
        XmVaPUSHBUTTON, text0, '1', NULL, NULL,
        XmVaPUSHBUTTON, text1, '2', NULL, NULL,
        XmVaPUSHBUTTON, text2, '3', NULL, NULL,
        NULL);
    XmStringFree (text0);
    XmStringFree (text1);
    XmStringFree (text2);
    XmStringFree (label);

    XtManageChild (option_menu);

    XtManageChild (rc);

    XtRealizeWidget (toplevel);

    
{
    static XtWidgetGeometry Expected[] = {
   CWWidth | CWHeight            ,  438,  364,  128,  183, 0,0,0, /* main_w */
   CWWidth | CWHeight | CWX | CWY,    0,   31,  128,  152, 0,0,0, /* rowcol */
   CWWidth | CWHeight | CWX | CWY,    3,    3,  122,   25, 0,0,0, /* tb0 */
   CWWidth | CWHeight | CWX | CWY,    3,   31,  122,   34, 0,0,0, /* tb1 */
   CWWidth | CWHeight | CWX | CWY,    3,   68,  122,   47, 0,0,0, /* tb2 */
   CWWidth | CWHeight | CWX | CWY,    3,  118,  122,   31, 0,0,0, /* option_menu */
   CWWidth | CWHeight | CWX | CWY,    3,    3,   46,   25, 0,0,0, /* OptionLabel */
   CWWidth | CWHeight | CWX | CWY,   52,    3,   67,   25, 0,0,0, /* OptionButton */
   CWWidth | CWHeight | CWX | CWY,    0,    0,  128,   31, 0,0,0, /* menubar */
   CWWidth | CWHeight | CWX | CWY,    5,    5,   64,   21, 0,0,0, /* button_0 */
};
/* toplevel should be replaced with to correct applicationShell */
PrintDetails(toplevel, Expected);
}
LessTifTestMainLoop(toplevel);

exit(0);
}
