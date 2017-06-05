/* $Header: /cvsroot/lesstif/lesstif/test/Xm/vasimple/test4.c,v 1.3 2002/03/22 00:00:07 amai Exp $

The cascading menus do not appear when I build a popup menu when the popup
menu items are built using separate calls to XtVaCreateManagedWidget. The
cascading menus do appear if I build the popup menu all in one line.

My original application with the cascading menus does work with OSF Motif
and Red Hat Motif. The Lesstif problem appears on both PC's and Sun's, the
only places it has been tried.

The test code below demonstrates the problem. The menu items in the
cascading menu do not appear. To see the case that does work
run the code with any extra command line argument and the cascading menu items
do appear.

The Linux users of my application (a big C++ framework for solving partial
differential equations, available free from http://www.llnl.gov/casc/Overture)
have been complaining about this bug for some time so any help that you can
give would be appreciated.
Thanks,
    Bill Henshaw.

*/


#include <stdlib.h>
#include <stdio.h>

/*   Test routine for pop-up menus with cascading menus */
/* to run the case that works add any extra argument to the command line */
#include <Xm/RowColumn.h>
#include <Xm/MainW.h>
#include <Xm/DrawingA.h>
#include <Xm/PushBG.h>
#include <Xm/CascadeB.h>
#include <Xm/CascadeBG.h>

/* input() -- called in responses to events in the DrawingArea;
 * button-3 pops up menu.
 */
void
input(Widget widget, XtPointer client_data, XtPointer call_data)
{
    Widget popup = (Widget) client_data;
    XmDrawingAreaCallbackStruct *cbs =
        (XmDrawingAreaCallbackStruct *) call_data;

    if (cbs->event->xany.type != ButtonPress ||
            cbs->event->xbutton.button != 3)
        return;

    /* Position the menu where the event occurred */
    XmMenuPosition (popup, (XButtonPressedEvent *) (cbs->event));
    XtManageChild (popup);
}

/* popup_cb() -- invoked when the user selects an item in the popup menu */
void
popup_cb(Widget menu_item, XtPointer client_data, XtPointer call_data)
{
    int item_no = (int) client_data;

    if (item_no == 4) /* Exit was selected -- exit */
        exit (0);
    puts (XtName (menu_item)); /* Otherwise, just print the selection */
}

/* set_width() -- called when items in the Line Width pullright menu
 * are selected.
 */
void
set_width(Widget menu_item, XtPointer client_data, XtPointer call_data)
{
    int item_no = (int) client_data;

    printf ("Line weight = %d\n", 1 << item_no);
}

int
main(int argc, char *argv[])
{

    XmString line, square, circle, weight, exit_s, exit_acc;
    XmString w_one, w_two, w_four, w_eight;
    Widget toplevel, main_w, drawing_a, popup_menu, pullright, gidget;
    XtAppContext app;

    XtSetLanguageProc (NULL, NULL, NULL);

    toplevel = XtVaAppInitialize (&app, "Demos", NULL, 0,
        &argc, argv, NULL, NULL);

    /* Create a MainWindow widget that contains a DrawingArea in
     * its work window.
     */
    main_w = XtVaCreateManagedWidget ("main_w",
        xmMainWindowWidgetClass, toplevel,
        XmNscrollingPolicy,      XmAUTOMATIC,
        NULL);
    /* Create a DrawingArea -- no actual drawing will be done. */
    drawing_a = XtVaCreateManagedWidget ("drawing_a",
        xmDrawingAreaWidgetClass, main_w,
        XmNwidth,  500,
        XmNheight, 500,
        NULL);

    line    = XmStringCreateLocalized ("Line");
    square  = XmStringCreateLocalized ("Square");
    circle  = XmStringCreateLocalized ("Circle");
    weight  = XmStringCreateLocalized ("Line Width");
    exit_s  = XmStringCreateLocalized ("Exit");
    exit_acc = XmStringCreateLocalized ("Ctrl+C");
    if( argc>1 )
    { /*  this works */
      popup_menu =
         XmVaCreateSimplePopupMenu (drawing_a, "popup", popup_cb,
                                    XmVaPUSHBUTTON, line, 'L', NULL, NULL,
                                    XmVaPUSHBUTTON, square, 'S', NULL, NULL,
                                    XmVaPUSHBUTTON, circle, 'C', NULL, NULL,
                                    XmVaCASCADEBUTTON, weight, 'W',
                                    XmVaSEPARATOR,
                                    XmVaPUSHBUTTON, exit_s, 'x', "Ctrl<Key>c", exit_acc,
                                    NULL);
    }
    else
    {
      popup_menu =
          XmVaCreateSimplePopupMenu (drawing_a, "popup", popup_cb, NULL);
      gidget =
         XtVaCreateManagedWidget("Line",xmPushButtonGadgetClass, popup_menu,
	                         NULL);
      XtAddCallback(gidget,XmNactivateCallback,popup_cb,(void*)'L');
      gidget =
         XtVaCreateManagedWidget("Square",xmPushButtonGadgetClass, popup_menu,
	                         NULL);
      XtAddCallback(gidget,XmNactivateCallback,popup_cb,(void*)'S');
      gidget =
         XtVaCreateManagedWidget("Circle",xmPushButtonGadgetClass, popup_menu,
	                         NULL);
      XtAddCallback(gidget,XmNactivateCallback,popup_cb,(void*)'C');

      /*  *********** this next line doesn't work ************** */
      gidget =
         XtVaCreateManagedWidget("Line Width", xmCascadeButtonGadgetClass,
	                          popup_menu, NULL);
      XtAddCallback(gidget,XmNactivateCallback,popup_cb,(void*)'W');

      gidget =
         XtVaCreateManagedWidget("Exit", xmPushButtonGadgetClass, popup_menu,
	                         NULL);
      XtAddCallback(gidget,XmNactivateCallback,popup_cb,(void*)'x');
    }

    XmStringFree (line);
    XmStringFree (square);
    XmStringFree (circle);
    XmStringFree (weight);
    XmStringFree (exit_s);

    /* create pullright for "Line Width" button -- this is the 4th item! */
    w_one     = XmStringCreateLocalized (" 1 ");
    w_two     = XmStringCreateLocalized (" 2 ");
    w_four    = XmStringCreateLocalized (" 4 ");
    w_eight   = XmStringCreateLocalized (" 8 ");
    pullright = 
       XmVaCreateSimplePulldownMenu (popup_menu,
                                     "pullright", 3 /* menu item offset */, set_width,
                                     XmVaPUSHBUTTON, w_one,   '1', NULL, NULL,
                                     XmVaPUSHBUTTON, w_two,   '2', NULL, NULL,
                                     XmVaPUSHBUTTON, w_four,  '4', NULL, NULL,
                                     XmVaPUSHBUTTON, w_eight, '8', NULL, NULL,
                                     NULL);
    XmStringFree (w_one);
    XmStringFree (w_two);
    XmStringFree (w_four);
    XmStringFree (w_eight);

    /* after popup menu is created, add callback for all input events */
    XtAddCallback (drawing_a, XmNinputCallback, input, popup_menu);

    XtRealizeWidget (toplevel);
    LessTifTestMainLoop(  toplevel );
    
    exit(0);
}
