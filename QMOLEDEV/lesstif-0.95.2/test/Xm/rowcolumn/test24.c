/** $Header: /cvsroot/lesstif/lesstif/test/Xm/rowcolumn/test24.c,v 1.7 2001/05/15 14:46:10 amai Exp $
    -- vertical tight layout of widgets with XmNadjustLast = False.

    resulting layout should be something like this:

    button1
    button2
    button3
    button4
    button5

    resizing smaller will yield something like this:

    button1 button4
    button2 button5
    button3

**/

#include <stdlib.h>
#include <stdio.h>

#include <Xm/XmP.h>
#ifdef LESSTIF_REVISION
#include <XmI/XmI.h>
#endif
#include <Xm/RowColumnP.h>
#include <Xm/PushB.h>


#ifdef LESSTIF_REVISION
XmRCKidGeometry
_XmRCGetKidGeo(Widget w, Widget instig,
               XtWidgetGeometry *instig_request,
               int uniform_border, Dimension border,
               int uniform_width_margins, int uniform_height_margins,
               Widget help, Widget toc, int geoType)
{
    int i;
    XmRCKidGeometry kid_geometry;
    Dimension mmt = 0, mmb = 0;
    XmRCKidGeometry boxes = NULL;
 
printf("%s GET KID GEO: Type: %d %p %p ", XtName(w), geoType, instig, instig_request);
if (instig)
	printf("%s\n", XtName(instig));
else
	printf("\n");

    boxes = (XmRCKidGeometry)XtCalloc(MGR_NumChildren(w) + 1,
                                      sizeof(XmRCKidGeometryRec));
    for (i = 0; i < MGR_NumChildren(w); i++)
    {
        kid_geometry = &(boxes[i]);
        kid_geometry->kid = MGR_Children(w)[i];
 
 
        if (!XtIsManaged(kid_geometry->kid))
        {
            if (RCC_WasManaged(kid_geometry->kid))
            {
                RCC_WasManaged(kid_geometry->kid) = False;
            }
            continue;
        }
 
        _XmGeoLoadValues(kid_geometry->kid, geoType,
                             instig, instig_request, &kid_geometry->box);
 
 
        RCC_WasManaged(kid_geometry->kid) = True;
 
        kid_geometry->box.width += (2 * XtBorderWidth(kid_geometry->kid));
        kid_geometry->box.height += (2 * XtBorderWidth(kid_geometry->kid));
 
        if (mmt < RCC_MarginTop(kid_geometry->kid))
            mmt = RCC_MarginTop(kid_geometry->kid);
        if (mmb < RCC_MarginBottom(kid_geometry->kid))
            mmb = RCC_MarginBottom(kid_geometry->kid);
    }
 
    for (i = 0; i < MGR_NumChildren(w); i++)
    {
        kid_geometry = &(boxes[i]);
 
        if (!XtIsManaged(kid_geometry->kid) || !RC_DoMarginAdjust(w))
            continue;
 
        if (mmt > kid_geometry->margin_top)
            kid_geometry->margin_top = mmt;
        if (mmb > kid_geometry->margin_bottom)
            kid_geometry->margin_bottom = mmb;
    }
 
    return boxes;
}
#endif
void
_XmRCSetKidGeo(XmRCKidGeometry kg, Widget instigator)
{
    /* Now we actually lay out our children.  */
    while (kg->kid != NULL)
    {
        if (kg->kid == instigator)
        {
            XtX(instigator) = kg->box.x;
            XtY(instigator) = kg->box.y;
            XtWidth(instigator) = kg->box.width;
            XtHeight(instigator) = kg->box.height;
            instigator->core.border_width = kg->box.border_width;
        }
        else if (kg->box.x != XtX(kg->kid) ||
                 kg->box.y != XtY(kg->kid) ||
                 kg->box.width != XtWidth(kg->kid) ||
                 kg->box.height != XtHeight(kg->kid) ||
                 kg->box.border_width != XtBorderWidth(kg->kid))
            _XmConfigureObject(kg->kid,
                               kg->box.x, kg->box.y,
                               kg->box.width, kg->box.height,
                               kg->box.border_width);
        kg++;
    }
}
 

void
resize(Widget w, XtPointer a, XtPointer b)
{
    XtUnmanageChild(w);
    XtVaSetValues(w, XmNwidth, 100, XmNheight, 100, NULL);
    XtManageChild(w);
}

void
changelab(Widget w, XtPointer a, XtPointer b)
{
    XmString str = XmStringCreateSimple("This is a changed string");

    XtVaSetValues(w, XmNlabelString, str, NULL);
}

void
unman(Widget w, XtPointer a, XtPointer b)
{
    XtUnmanageChild(w);
}

void
setpos(Widget w, XtPointer a, XtPointer b)
{
    XtVaSetValues(w, XmNpositionIndex, 1, NULL);
}

int
main(int argc, char **argv)
{
    XtAppContext theApp;
    Widget toplevel, rc, rc0;
    Widget button0, button1, button2, button3, button4, button5;

    toplevel = XtVaAppInitialize(&theApp, "rc-test1", NULL, 0,
				 &argc, argv, NULL, NULL);

    rc0 = XtVaCreateManagedWidget("RCParent", xmRowColumnWidgetClass,
				  toplevel, NULL);

    button0 = XtVaCreateManagedWidget("button0",
				      xmPushButtonWidgetClass,
				      rc0,
				      NULL);

    rc = XtVaCreateManagedWidget("rowcolumn",
				 xmRowColumnWidgetClass,
				 rc0,
				 XmNorientation, XmVERTICAL,
				 XmNpacking, XmPACK_TIGHT,
				 XmNadjustLast, False,
				 XmNwidth, 500,
				 XmNheight, 500,
				 NULL);

    button1 = XtVaCreateManagedWidget("button1",
				      xmPushButtonWidgetClass,
				      rc,
				      NULL);

    button2 = XtVaCreateWidget("button2",
				      xmPushButtonWidgetClass,
				      rc,
				      XtNwidth, 200, XtNheight, 200,
				      NULL);
    XtManageChild(button2);

    XtAddCallback(button2, XmNactivateCallback, resize, NULL);

    button3 = XtVaCreateManagedWidget("button3",
				      xmPushButtonWidgetClass,
				      rc,
				      NULL);
    XtAddCallback(button3, XmNactivateCallback, changelab, NULL);

    button4 = XtVaCreateManagedWidget("button4",
				      xmPushButtonWidgetClass,
				      rc,
				      NULL);
    XtAddCallback(button4, XmNactivateCallback, unman, NULL);

    button5 = XtVaCreateManagedWidget("button5",
				      xmPushButtonWidgetClass,
				      rc,
				      NULL);
    XtAddCallback(button5, XmNactivateCallback, setpos, NULL);

    XtRealizeWidget(toplevel);

  {
  static XtWidgetGeometry Expected[] = {
  	CWWidth | CWHeight,		0,	0,	300,	85,	0,0,0,
  	CWWidth | CWHeight | CWX | CWY,	3,	3,	96,	38,	0,0,0,
  	CWWidth | CWHeight | CWX | CWY,	102,	3,	96,	38,	0,0,0,
  	CWWidth | CWHeight | CWX | CWY,	201,	3,	96,	38,	0,0,0,
  	CWWidth | CWHeight | CWX | CWY,	3,	44,	96,	38,	0,0,0,
  	CWWidth | CWHeight | CWX | CWY,	102,	44,	96,	38,	0,0,0,
};

#if 0
  PrintDetails(toplevel, Expected);
#endif
  }
    LessTifTestMainLoop(toplevel);
    /*
    XtAppMainLoop(theApp);    
    */
    exit(0);
}


