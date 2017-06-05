/* Prints CascadeButtonGadget dimensions when it is in a Option Menu.
 * If one tryes it with different settings e.g shadowThickness,
 * fontList, marginBottom and marginTop, it reveals a lot of the
 * inner geometry of CascadeButton
 */

#include <Xm/RowColumnP.h>
#include <Xm/CascadeBGP.h>
#include <Xm/PushBGP.h>
#include <stdio.h>

static void
PrintDimensions(Widget w, XtPointer clientData, XtPointer callData)
{
    XmCascadeButtonGadget cw = (XmCascadeButtonGadget) w;
    XtWidgetGeometry preferred;


    fprintf(stderr, "Widget %s - x: %3d y: %3d width: %3d height: %3d\n",
	    XrmQuarkToString(cw->object.xrm_name),
	     cw->rectangle.x, cw->rectangle.y,
	    cw->rectangle.width, cw->rectangle.height);
    XtQueryGeometry(w, NULL, &preferred);
    fprintf(stderr, "Preferred geometry, Width %3d, Height %3d\n",
	    preferred.width, preferred.height);

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
    /*    XmCascadeButtonGadgetHighlight(w, False); */

}

char *fallbacks[] =
{
    "*Test14.allowShellResize: True",
    /*    "*om.labelString: Options", */
    "*pb1.labelString: Short Label",
    "*pb2.labelString: Longer Label",
    "*pb3.labelString: Mutch mutch longer Label",
   NULL
};

int
main(int argc, char **argv)
{
    XtAppContext app;
    Widget toplevel, pane1, om, pb1, pb2, pb3, cb1;
    Arg args[1];
    
    toplevel = XtVaAppInitialize(&app, "Test5", NULL, 0,
				 &argc, argv, fallbacks, NULL);

    pane1 = XmCreatePulldownMenu(toplevel, "pane1", NULL, 0);
    pb1 = XtVaCreateManagedWidget ("pb1",
        xmPushButtonGadgetClass, pane1,
        NULL);

    pb2 = XtVaCreateManagedWidget ("pb2",
        xmPushButtonGadgetClass, pane1,
        NULL);

    pb3 = XtVaCreateManagedWidget ("pb3",
        xmPushButtonGadgetClass, pane1,
        NULL);

    XtSetArg(args[0], XmNsubMenuId, pane1);
    om = XmCreateOptionMenu(toplevel, "om", args, 1);

    cb1 = XmOptionButtonGadget(om);
    XtAddCallback(cb1, XmNactivateCallback, PrintDimensions, NULL);
    XtAddCallback(cb1, XmNcascadingCallback, PrintDimensions, NULL);

    XtManageChild(om);
    /* XtManageChild(pane1); */

    XtRealizeWidget(toplevel);
    

/* Note: the following values are the result of
 * querying the current geometry.
 */
{
static XtWidgetGeometry Expected[] = {
   CWWidth | CWHeight            ,  363,  381,  194,   35, 0,0,0, /* om */
   CWWidth | CWHeight | CWX | CWY,    3,    3,    4,   29, 0,0,0, /* OptionLabel */
   CWWidth | CWHeight | CWX | CWY,   10,    3,  181,   29, 0,0,0, /* OptionButton */
};
/* toplevel should be replaced with to correct applicationShell */
PrintDetails(toplevel, Expected);
}
LessTifTestMainLoop(toplevel);
    exit(0);
}
