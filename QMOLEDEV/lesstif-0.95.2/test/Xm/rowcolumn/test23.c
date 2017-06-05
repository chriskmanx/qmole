/*
 * duplicate xmcd's checkbox.
 * OH, WOE IS ME, THE ORDER OF OPERATIONS MAKES A HUGE DIFFERENCE IN MOTIF
 * BUT NOT IN LESSTIF.  Sigh -- we're more predictable??!?
 */
#include <stdio.h>
#include <Xm/XmP.h>
#ifdef LESSTIF_REVISION
#include <XmI/XmI.h>
#endif
#include <Xm/Form.h>
#include <Xm/Frame.h>
#include <Xm/RowColumnP.h>
#include <Xm/ToggleB.h>

#ifdef LESSTIF_REVISION
#define lt_dump_boxes(a,b,c) dump_boxes(a,b,c)
#else
#define lt_dump_boxes(a,b,c) NULL
#endif
void
dump_boxes(WidgetList wl, XmRCKidGeometry boxes, int cnt)
{
    int i;
 
    for (i = 0; i < cnt; i++)
    {
        printf("Child: %-20s:%p ", XtName(wl[i]), wl[i]);
#if 0
	if (boxes)
	    printf(" box: %-20s:%p\n", XtName(boxes[i].kid), boxes[i].kid);
	else
	    putchar('\n');
#endif
        printf("       x/y: %3d %3d  w/h: %3d %3d\n",
                XtX(wl[i]), XtY(wl[i]), XtWidth(wl[i]), XtHeight(wl[i]));
#if 0
	if (boxes)
            printf("   box x/y: %3d %3d  w/h: %3d %3d mt/mb: %3d %3d\n",
                    boxes[i].box.x, boxes[i].box.y,
                    boxes[i].box.width, boxes[i].box.height,
                    boxes[i].margin_top, boxes[i].margin_bottom);
	else
	    printf("   no box data\n");
#endif
    }
    putchar('\n');
}

#define btnlbl_width 16
#define btnlbl_height 16
static unsigned char btnlbl_bits[] = {
   0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0xf8, 0x1f, 0x04, 0x20, 0x84, 0x21,
   0x44, 0x22, 0x44, 0x22, 0xc4, 0x23, 0x44, 0x22, 0x44, 0x22, 0x04, 0x20,
   0xf8, 0x1f, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00
};


#define lock_width 16
#define lock_height 16
static unsigned char lock_bits[] = {
   0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0xc0, 0x03, 0x20, 0x04, 0x20, 0x04,
   0x20, 0x04, 0xf0, 0x0f, 0xf0, 0x0f, 0xf0, 0x0f, 0xf0, 0x0f, 0xf0, 0x0f,
   0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00
};


#define repeat_width 16
#define repeat_height 16
static unsigned char repeat_bits[] = {
   0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x01, 0xf8, 0x13,
   0x0c, 0x31, 0x04, 0x20, 0x04, 0x20, 0x8c, 0x30, 0xc8, 0x1f, 0x80, 0x00,
   0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00
};


#define shuffle_width 16
#define shuffle_height 16
static unsigned char shuffle_bits[] = {
   0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x78, 0x00, 0xcc, 0x00,
   0x84, 0x0c, 0xc0, 0x3c, 0x60, 0x3c, 0x30, 0x0c, 0x00, 0x00, 0x30, 0x00,
   0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00
};

String fallback[] = {
	"*.highlightThickness: 0",
	"*.labelType: PIXMAP",
	"*XmFrame*highlightThickness: 1",
	"*XmFrame*indicatorSize: 13",
	"*XmFrame*borderWidth: 1",
	"*mainForm.width: 360",
	"*mainForm.height: 135",
	"*mainForm*borderWidth: 0",
	NULL
};

Pixmap
bm_to_px(Widget w, void *bits, int width, int height, int depth)
{
    Display *display = XtDisplay(w);
    Window win = XtWindow(w);
    int screen = DefaultScreen(display);
    Pixmap ret;
    Pixel fg, bg;

    XtVaGetValues(w, XmNforeground, &fg, XmNbackground, &bg, NULL);

    ret = XCreatePixmapFromBitmapData(display, win, (char *)bits,
				      width, height, fg, bg, depth);

    if (ret == None)
	return XmUNSPECIFIED_PIXMAP;
    return ret;
}

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
 
int
main(int argc, char **argv)
{
	XtAppContext app;
	Arg arg[10];
	Widget toplevel, form, frame, rc, t1, t2, t3, t4;
	int i;
	Pixmap px;
	Boolean rw, rh;

	toplevel = XtVaAppInitialize(&app, "rc-test23", NULL, 0,
				     &argc, argv, fallback, NULL);

	form = XmCreateForm(toplevel, "mainForm", NULL, 0);

	frame = XmCreateFrame(form, "frame", NULL, 0);

	i = 0;
	XtSetArg(arg[i], XmNrowColumnType, XmWORK_AREA); i++;
	XtSetArg(arg[i], XmNnumColumns, 1); i++;
	XtSetArg(arg[i], XmNspacing, 2); i++;
	XtSetArg(arg[i], XmNmarginHeight, 4); i++;
	XtSetArg(arg[i], XmNorientation, XmVERTICAL); i++;
	XtSetArg(arg[i], XmNisHomogeneous, True); i++;
	XtSetArg(arg[i], XmNentryClass, xmToggleButtonWidgetClass); i++;
	rc = XmCreateRowColumn(frame, "checkBox", arg, i);

	t1 = XmCreateToggleButton(rc, "button_0", NULL, 0);
	t2 = XmCreateToggleButton(rc, "button_1", NULL, 0);
	t3 = XmCreateToggleButton(rc, "button_2", NULL, 0);
	t4 = XmCreateToggleButton(rc, "button_3", NULL, 0);

	printf("Before managing...\n");
	lt_dump_boxes(MGR_Children(rc), RC_Boxes(rc), MGR_NumChildren(rc));

	XtManageChild(form);
	XtManageChild(frame);
	XtManageChild(rc);
	XtManageChild(t1);
	XtManageChild(t2);
	XtManageChild(t3);
	XtManageChild(t4);

	XtVaGetValues(rc, XmNresizeWidth, &rw, XmNresizeHeight, &rh, NULL);
	printf("RW: %d RH: %d\n", rw, rh);

	printf("After managing...\n");
	lt_dump_boxes(MGR_Children(rc), RC_Boxes(rc), MGR_NumChildren(rc));

	XtVaSetValues(frame,
			XmNleftAttachment, XmATTACH_POSITION,
			XmNleftPosition, 0,
			XmNrightAttachment, XmATTACH_POSITION,
			XmNrightPosition, 14,
			XmNtopAttachment, XmATTACH_POSITION,
			XmNtopPosition, 0,
			XmNbottomAttachment, XmATTACH_POSITION,
			XmNbottomPosition, 60,
			XmNshadowType, XmSHADOW_OUT,
			NULL);

	XtVaSetValues(t1,
			XmNrecomputeSize, False,
			XmNheight, 16,
			NULL);

	XtVaSetValues(t2,
			XmNrecomputeSize, False,
			XmNheight, 16,
			NULL);
	XtVaSetValues(t3,
			XmNrecomputeSize, False,
			XmNheight, 16,
			NULL);
	XtVaSetValues(t4,
			XmNrecomputeSize, False,
			XmNheight, 16,
			NULL);

	XtVaGetValues(rc, XmNresizeWidth, &rw, XmNresizeHeight, &rh, NULL);
	printf("RW: %d RH: %d\n", rw, rh);
	printf("After setting height...\n");
	lt_dump_boxes(MGR_Children(rc), RC_Boxes(rc), MGR_NumChildren(rc));

	XtRealizeWidget(toplevel);

	printf("After realize...\n");
	lt_dump_boxes(MGR_Children(rc), RC_Boxes(rc), MGR_NumChildren(rc));

	px = bm_to_px(rc, btnlbl_bits, btnlbl_width, btnlbl_height,
		      rc->core.depth);
	XtVaSetValues(t1, XmNlabelPixmap, px, NULL);

	px = bm_to_px(rc, lock_bits, lock_width, lock_height,
		      rc->core.depth);
	XtVaSetValues(t2, XmNlabelPixmap, px, NULL);

	px = bm_to_px(rc, repeat_bits, repeat_width, repeat_height,
		      rc->core.depth);
	XtVaSetValues(t3, XmNlabelPixmap, px, NULL);

	px = bm_to_px(rc, shuffle_bits, shuffle_width, shuffle_height,
		      rc->core.depth);
	XtVaSetValues(t4, XmNlabelPixmap, px, NULL);

	printf("After setting pixmaps...\n");
	lt_dump_boxes(MGR_Children(rc), RC_Boxes(rc), MGR_NumChildren(rc));

/* Note: the following values are the result of
 * querying the current geometry.
 */
{
static XtWidgetGeometry Expected[] = {
   CWWidth | CWHeight            ,    0,    0,  360,  135, 0,0,0, /* mainForm */
   CWWidth | CWHeight | CWX | CWY,    0,    0,   50,   81, 0,0,0, /* frame */
   CWWidth | CWHeight | CWX | CWY,    2,    2,   46,   77, 0,0,0, /* checkBox */
   CWWidth | CWHeight | CWX | CWY,    3,    4,   40,   16, 0,0,0, /* button_0 */
   CWWidth | CWHeight | CWX | CWY,    3,   22,   40,   16, 0,0,0, /* button_1 */
   CWWidth | CWHeight | CWX | CWY,    3,   40,   40,   16, 0,0,0, /* button_2 */
   CWWidth | CWHeight | CWX | CWY,    3,   58,   40,   16, 0,0,0, /* button_3 */
};
/* toplevel should be replaced with to correct applicationShell */
PrintDetails(toplevel, Expected);
}
LessTifTestMainLoop(toplevel);

	exit(0);
}
