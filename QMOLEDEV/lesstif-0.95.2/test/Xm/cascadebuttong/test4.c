/* $Header: /cvsroot/lesstif/lesstif/test/Xm/cascadebuttong/test4.c,v 1.6 2001/05/23 15:00:02 amai Exp $ */

#include <stdlib.h>
#include <stdio.h>

#include <Xm/RowColumnP.h>
#include <Xm/MainWP.h>
#include <Xm/DrawingAP.h>
#include <Xm/CascadeBGP.h>
#include <Xm/CascadeBP.h>
#include <Xm/PushBGP.h>
#include <Xm/ToggleBGP.h>


static void
PrintDimensions(Widget w, XtPointer clientData, XtPointer callData)
{
    XmCascadeButtonGadget cw = (XmCascadeButtonGadget) w;

    fprintf(stderr, "Widget %s - geo %d %d %d %d\n",
	    XrmQuarkToString(cw->object.xrm_name),
	     cw->rectangle.x, cw->rectangle.y,
	    cw->rectangle.width, cw->rectangle.height);
    fprintf(stderr, "BorderWidth: %3d HighlightThickness %3d ShadowThickness: %3d\n",
	    cw->rectangle.border_width,
	    cw->gadget.highlight_thickness,
	    cw->gadget.shadow_thickness);
    fprintf(stderr, "MarginLeft: %3d MarginRight %3d MarginTop: %3d MarginBottom: %3d\n",
	    cw->label.cache->margin_left,
	    cw->label.cache->margin_right,
	    cw->label.cache->margin_top,
	    cw->label.cache->margin_bottom);
    fprintf(stderr, "MarginHeight: %3d MarginWidth %3d\n",
	    cw->label.cache->margin_height,
	    cw->label.cache->margin_width);
    fprintf(stderr, "TextRect x: %3d y: %3d w: %3d h: %3d\n",
	    cw->label.TextRect.x,
	    cw->label.TextRect.y,
	    cw->label.TextRect.width,
	    cw->label.TextRect.height);
    fprintf(stderr, "AccTextRect x: %3d y: %3d w: %3d h: %3d\n",
	    cw->label.acc_TextRect.x,
	    cw->label.acc_TextRect.y,
	    cw->label.acc_TextRect.width,
	    cw->label.acc_TextRect.height);
    fprintf(stderr, "CascadeRect x: %3d y: %3d w: %3d h: %3d\n\n",
	    cw->cascade_button.cascade_rect.x,
	    cw->cascade_button.cascade_rect.y,
	    cw->cascade_button.cascade_rect.width,
	    cw->cascade_button.cascade_rect.height);

}

static char *fallbacks[] = {
	"*cb1.labelString:			Menu",
	"*cascade1.mnemonic:			M",
	"*cb2.labelString:			Print Dimensions",
	"*cb3.labelString:			Short string",
	"*pb1.labelString:			Dummy",
	NULL	/* The end */
};

int
main(int argc, char *argv[])
{
    XtAppContext app;
    Widget toplevel, main_w, menubar, cb1, cb2, cb3, pb1, pane1, pane2,
	drawing_a;

    XtSetLanguageProc (NULL, NULL, NULL);

    toplevel = XtVaAppInitialize (&app, "Test4", NULL, 0,
				  &argc, argv, fallbacks,
				  NULL);

    /* Create a MainWindow widget that contains a DrawingArea in
     * its work window. 
     */

    main_w = XtVaCreateManagedWidget ("main_w",
        xmMainWindowWidgetClass, toplevel,
        XmNscrollingPolicy,  XmAUTOMATIC,
        NULL);
    menubar = XmCreateMenuBar (main_w, "menubar", NULL, 0);

    pane1 = XmCreatePulldownMenu (menubar, "pane1", NULL, 0);
    pane2 = XmCreatePulldownMenu (pane1, "pane2", NULL, 0);

    cb1 = XtVaCreateManagedWidget ("cb1",
        xmCascadeButtonGadgetClass, menubar,
        XmNsubMenuId,   pane1,
        NULL);

    cb2 = XtVaCreateManagedWidget ("cb2",
        xmCascadeButtonGadgetClass, pane1,
        XmNsubMenuId,   pane2,
        NULL);

    cb3 = XtVaCreateManagedWidget ("cb3",
        xmCascadeButtonWidgetClass, pane1,
        XmNsubMenuId,   pane2,
        NULL);

    XtAddCallback(cb2, XmNactivateCallback, PrintDimensions, NULL);
    XtAddCallback(cb2, XmNcascadingCallback, PrintDimensions, NULL);


    pb1 = XtVaCreateManagedWidget ("pb1",
        xmPushButtonGadgetClass, pane2,
        NULL);

    XtManageChild (menubar);

    /* Create a DrawingArea -- no actual drawing will be done. */
    drawing_a = XtVaCreateManagedWidget ("drawing_a",
        xmDrawingAreaWidgetClass, main_w,
        XmNwidth, 500,
        XmNheight,500,
        NULL);

    XtRealizeWidget (toplevel);
    

/* Note: the following values are the result of
 * querying the current geometry.
 */
{
static XtWidgetGeometry Expected[] = {
   CWWidth | CWHeight            ,  218,  358,  131,  162, 0,0,0, /* main_w */
   CWWidth | CWHeight | CWX | CWY,    4,   35,  100,  100, 0,0,0, /* ClipWindow */
   CWWidth | CWHeight | CWX | CWY,    0,    0,  500,  500, 0,0,0, /* drawing_a */
   CWWidth | CWHeight | CWX | CWY,  112,   31,   19,  108, 0,0,0, /* VertScrollBar */
   CWWidth | CWHeight | CWX | CWY,    0,  143,  108,   19, 0,0,0, /* HorScrollBar */
   CWWidth | CWHeight | CWX | CWY,    0,    0,  131,   31, 0,0,0, /* menubar */
   CWWidth | CWHeight | CWX | CWY,    5,    5,   40,   21, 0,0,0, /* cb1 */
};
/* toplevel should be replaced with to correct applicationShell */
PrintDetails(toplevel, Expected);
}
LessTifTestMainLoop(toplevel);
LessTifTestMainLoop(toplevel);

exit(0);
}


