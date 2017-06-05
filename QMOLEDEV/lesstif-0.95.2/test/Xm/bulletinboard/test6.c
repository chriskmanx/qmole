/*
 * corners.c, from O'Reilly
 */
#include <Xm/BulletinB.h>
#include <Xm/PushBG.h>

char *corners[] = {
    "Top-Left", "Top-Right",
    "Bottom-Left", "Bottom-Right"
};

static void
resize(Widget w, XEvent *event, String *args, Cardinal *num_args)
{
    WidgetList children;
    XConfigureEvent *ev = (XConfigureEvent *)event;
    int width = ev->width;
    int height = ev->height;
    Dimension w_width, w_height;
    short margin_w, margin_h;

    XtVaGetValues(w,
		  XmNchildren, &children,
		  XmNmarginWidth, &margin_w,
		  XmNmarginHeight, &margin_h,
		  NULL);

    XtVaSetValues(children[0],
		  XmNx, margin_w,
		  XmNy, margin_h,
		  NULL);

    XtVaGetValues(children[1],
		  XmNwidth, &w_width,
		  NULL);
    XtVaSetValues(children[1],
		  XmNx, width - margin_w - w_width,
		  XmNy, margin_h,
		  NULL);

    XtVaGetValues(children[2],
		  XmNheight, &w_height,
		  NULL);
    XtVaSetValues(children[2],
		  XmNx, margin_w,
	 	  XmNy, height - margin_h - w_height,
		  NULL);

    XtVaGetValues(children[3],
		  XmNheight, &w_height,
		  XmNwidth, &w_width,
		  NULL);
    XtVaSetValues(children[3],
		  XmNx, width - margin_w - w_width,
		  XmNy, height - margin_h - w_height,
		  NULL);
}

int
main(int argc, char **argv)
{
    Widget toplevel, bboard;
    XtAppContext app;
    XtActionsRec rec;
    int i;

    toplevel = XtVaAppInitialize(&app, "Demos", NULL, 0,
				 &argc, argv, NULL, NULL);

    bboard = XtVaCreateManagedWidget("bboard", xmBulletinBoardWidgetClass,
				     toplevel, NULL);

    rec.string = "resize";
    rec.proc = resize;
    XtAppAddActions(app, &rec, 1);
    XtOverrideTranslations(bboard,
			   XtParseTranslationTable("<Configure>: resize()"));


    for (i = 0; i < XtNumber(corners); i++)
	XtVaCreateManagedWidget(corners[i], xmPushButtonGadgetClass,
				bboard, NULL);

    XtRealizeWidget(toplevel);


/* Note: the following values are the result of
 * querying the current geometry.
 */
{
static XtWidgetGeometry Expected[] = {
   CWWidth | CWHeight            ,    0,    0,  105,   46, 0,0,0, /* bboard */
   CWWidth | CWHeight | CWX | CWY,   10,   10,   60,   25, 0,0,0, /* Top-Left */
   CWWidth | CWHeight | CWX | CWY,   10,   10,   66,   25, 0,0,0, /* Top-Right */
   CWWidth | CWHeight | CWX | CWY,   10,   10,   78,   25, 0,0,0, /* Bottom-Left */
   CWWidth | CWHeight | CWX | CWY,   10,   10,   84,   25, 0,0,0, /* Bottom-Right */
};
/* toplevel should be replaced with to correct applicationShell */
PrintDetails(toplevel, Expected);
}
LessTifTestMainLoop(toplevel);
  exit(0);
}
