/*
 *
 * Blob.c - XmpBlob widget
 *
 */

#include <stdio.h>
#include <BlobP.h>
#include <Xm/DrawP.h>
#include <Xm/RepType.h>

static void ClassInitialize();
static void ClassPartInitialize();
static void Initialize();
static void Destroy();
static void Resize();
static void Redisplay();
static Boolean SetValues();
static XtGeometryResult QueryGeometry();
static void DrawVisual();
static void DrawShadow();
static void CalcWidgetSize();
static void CalcVisualSize();
static void CreateGC();
static void DestroyGC();
static GC SelectGC();
static void BlobActivate();

/* Blobs really don't have a preferred size, so provide an artificial one */

#define BLOB_SIZE 30

#define defaultTranslations "\
<EnterWindow>:		PrimitiveEnter()\n\
<LeaveWindow>:		PrimitiveLeave()\n\
~s ~m ~a <Key>Return:	PrimitiveParentActivate()\n\
<Key>osfActivate:	PrimitiveParentActivate()\n\
<Key>osfCancel:		PrimitiveParentCancel()\n\
<Key>osfHelp:		PrimitiveHelp()\n\
<Key>osfSelect:		BlobActivate()\n\
<Key>space:		BlobActivate()\n\
<Btn1Down>:		BlobActivate()"


static XtActionsRec ActionsList[] = {
	{"BlobActivate",	BlobActivate }
};


static XtResource resources[] = 
{
    {
	XmNblobShape,
	XmCBlobShape,
	XmRBlobShape,
	sizeof (unsigned char),
	XtOffsetOf( struct _XmpBlobRec, blob.blob_shape),
	XmRImmediate,
	(XtPointer)XmBLOB_OVAL
    },
    {
 	XmNmarginWidth, 
	XmCMarginWidth, 
	XmRHorizontalDimension, 
	sizeof (Dimension),
	XtOffsetOf( struct _XmpBlobRec, blob.margin_width), 
	XmRImmediate,
	(XtPointer) 4
    },
    {
	XmNmarginHeight, 
	XmCMarginHeight, 
	XmRVerticalDimension, 
	sizeof (Dimension),
	XtOffsetOf( struct _XmpBlobRec, blob.margin_height),
	XmRImmediate,
	(XtPointer) 4
    },
};


static XmSyntheticResource syn_resources[] =
{
    { 
	XmNmarginWidth, 
	sizeof (Dimension),
	XtOffsetOf( struct _XmpBlobRec, blob.margin_width), 
	_XmFromHorizontalPixels,
	_XmToHorizontalPixels 
    },
    { 
	XmNmarginHeight, 
	sizeof (Dimension),
	XtOffsetOf( struct _XmpBlobRec, blob.margin_height),
	_XmFromVerticalPixels, 
	_XmToVerticalPixels 
    },
};


externaldef (xmpblobclassrec) XmpBlobClassRec xmpBlobClassRec = {
  {
    (WidgetClass)&xmPrimitiveClassRec,        /* superclass            */
    "XmpBlob",                                /* class_name            */
    sizeof(XmpBlobRec),                       /* widget_size           */
    ClassInitialize,                          /* class_initialize      */
    ClassPartInitialize,                      /* class_part_initialize */
    FALSE,                                    /* class_inited          */
    Initialize,                               /* initialize            */
    NULL,                                     /* initialize_hook       */
    XtInheritRealize,                         /* realize               */
    ActionsList,                              /* actions               */
    XtNumber(ActionsList),                    /* num_actions           */
    resources,                                /* resources             */
    XtNumber(resources),                      /* num_resources         */
    NULLQUARK,                                /* xrm_class             */
    TRUE,                                     /* compress_motion       */
    XtExposeCompressMaximal,                  /* compress_exposure     */
    TRUE,                                     /* compress_enterleave   */
    FALSE,                                    /* visible_interest      */
    Destroy,                                  /* destroy               */
    Resize,                                   /* resize                */
    Redisplay,                                /* expose                */
    SetValues,                                /* set_values            */
    NULL,                                     /* set_values_hook       */
    XtInheritSetValuesAlmost,                 /* set_values_almost     */
    NULL,                                     /* get_values_hook       */
    NULL,                                     /* accept_focus          */
    XtVersion,                                /* version               */
    NULL,                                     /* callback_private      */
    defaultTranslations,                      /* tm_table              */
    QueryGeometry,                            /* query_geometry        */
    NULL,                                     /* display_accelerator   */
    NULL,                                     /* extension             */
  },
  {                                           /* XmPrimitive           */
    XmInheritBorderHighlight,                 /* border_highlight      */
    XmInheritBorderUnhighlight,               /* border_unhighlight    */
    XtInheritTranslations,                    /* translations          */
    NULL,                                     /* arm_and_activate      */
    syn_resources,                            /* syn_resources         */
    XtNumber(syn_resources),                  /* num_syn_resources     */
    NULL,                                     /* extension             */
  },
  {                                           /* XmpBlob               */
    DrawVisual,                               /* draw_visual           */
    DrawShadow,                               /* draw_shadow           */
    CreateGC,                                 /* create_gc             */
    DestroyGC,                                /* destroy_gc            */
    SelectGC,                                 /* select_gc             */
    CalcWidgetSize,                           /* calc_widget_size      */
    CalcVisualSize,                           /* calc_visual_size      */
    NULL,                                     /* extension             */
  }
};

externaldef( xmpblobwidgetclass) WidgetClass xmpBlobWidgetClass =
                                (WidgetClass) &xmpBlobClassRec;



static String BlobShapeNames[] = {
	"blob_oval",
	"blob_rectangle"
};

static XmRepTypeId blobShapeId;


/*********************************************************************
 *
 *  ClassInitialize
 *
 ************************************************************************/
static void ClassInitialize()
{
    /* Register representation type for XmNblobShape */

    blobShapeId = XmRepTypeRegister (XmRBlobShape, BlobShapeNames,
					NULL, XtNumber(BlobShapeNames));
}



/*********************************************************************
 *
 *  ClassPartInitialize
 *
 ************************************************************************/
static void ClassPartInitialize (widgetClass)
    WidgetClass widgetClass;
{
    XmpBlobWidgetClass wc = (XmpBlobWidgetClass)widgetClass;
    XmpBlobWidgetClass sc = (XmpBlobWidgetClass) wc->core_class.superclass;

   /* Process method inheritance for subclasses of XmpBlob */

    if (wc->blob_class.draw_visual == XmInheritDrawVisual)
	wc->blob_class.draw_visual = sc->blob_class.draw_visual;
    if (wc->blob_class.draw_shadow == XmInheritDrawShadow)
	wc->blob_class.draw_shadow = sc->blob_class.draw_shadow;
    if (wc->blob_class.create_gc == XmInheritCreateGC)
	wc->blob_class.create_gc = sc->blob_class.create_gc;
    if (wc->blob_class.destroy_gc == XmInheritDestroyGC)
	wc->blob_class.destroy_gc = sc->blob_class.destroy_gc;
    if (wc->blob_class.select_gc == XmInheritSelectGC)
	wc->blob_class.select_gc = sc->blob_class.select_gc;
    if (wc->blob_class.calc_widget_size == XmInheritCalcWidgetSize)
	wc->blob_class.calc_widget_size = sc->blob_class.calc_widget_size;
    if (wc->blob_class.calc_visual_size == XmInheritCalcVisualSize)
	wc->blob_class.calc_visual_size = sc->blob_class.calc_visual_size;
}



/************************************************************
 *
 *  Initialize
 *
 ************************************************************/
static void Initialize (request_w, new_w, args, num_args)
    Widget request_w;
    Widget new_w;
    ArgList args;
    Cardinal *num_args;
{
    XmpBlobWidgetClass wc = (XmpBlobWidgetClass)XtClass(new_w);
    XmpBlobWidget rw = (XmpBlobWidget)request_w;
    XmpBlobWidget nw = (XmpBlobWidget)new_w;

    /* Validate XmNblobShape */

    if (!XmRepTypeValidValue (blobShapeId, nw->blob.blob_shape, (Widget)nw))
	nw->blob.blob_shape = XmBLOB_OVAL;

    /* Create GC's */

    if (wc->blob_class.create_gc) (*(wc->blob_class.create_gc))((Widget)nw);

    /* Remember application geometry settings */

    nw->blob.compute_width = True;
    nw->blob.compute_height = True;

    if (rw->core.width != 0) {
        nw->blob.compute_width = False;
        nw->blob.pref_width = rw->core.width;
        nw->core.width = rw->core.width;
    }
    if (rw->core.height != 0) {
        nw->blob.compute_height = False;
        nw->blob.pref_height = rw->core.height;
        nw->core.height = rw->core.height;
    }

    /* If we are a Blob calculate ideal size. If we are a subclass let	*/
    /* the subclass Initialize do it - the subclass CalcWidgetSize	*/
    /* method may need information derived in its Initialize method.	*/

    if ((WidgetClass)wc == xmpBlobWidgetClass) {
	if (wc->blob_class.calc_widget_size)
	    (*(wc->blob_class.calc_widget_size))((Widget)nw);

	nw->blob.pref_width = nw->core.width;
	nw->blob.pref_height = nw->core.height;

	if (wc->core_class.resize) (*(wc->core_class.resize))((Widget)nw);
    }
}



/************************************************************************
 *
 *  Destroy
 *
 ************************************************************************/
static void Destroy (w)
    Widget w;
{
    XmpBlobWidgetClass wc = (XmpBlobWidgetClass) XtClass(w);

    /* Destroy GC's */

    if (wc->blob_class.destroy_gc) (*(wc->blob_class.destroy_gc))(w);
}



/************************************************************************
 *
 *  Resize
 *
 ************************************************************************/
static void Resize (w)
    Widget w;
{
    XmpBlobWidget bw = (XmpBlobWidget)w;
    Dimension mt, mw, mh;

    /* Configure the blob using its current size */

    mt = bw->primitive.highlight_thickness + bw->primitive.shadow_thickness;

    mw = mt + bw->blob.margin_width;
    mh = mt + bw->blob.margin_height;

    /* Make effective use of white space when geometry is constrained: */

    if (bw->core.width >= 2 * mw + BLOB_SIZE) {
	/* All the room we need, fill it with the blob */
	bw->blob.visual_x = mw;
	bw->blob.visual_width = bw->core.width - 2 * mw;
    }
    else if (bw->core.width > 2 * mt + BLOB_SIZE) {
	/* Space is tight, encroach on the margins to maintain BLOB_SIZE */
	bw->blob.visual_x = (bw->core.width - BLOB_SIZE)/2;
	bw->blob.visual_width = BLOB_SIZE;
    }
    else if (bw->core.width > 2 * mt) {
	/* Can't fit BLOB_SIZE. Take what we can get. */
	bw->blob.visual_x = mt;
	bw->blob.visual_width = bw->core.width - 2 * mt;
    }
    else {
	/* No room for the blob */
	bw->blob.visual_width = 0;
    }

    if (bw->core.height >= 2 * mh + BLOB_SIZE) {
	/* All the room we need, fill it with the blob */
	bw->blob.visual_y = mh;
	bw->blob.visual_height = bw->core.height - 2 * mh;
    }
    else if (bw->core.height > 2 * mt + BLOB_SIZE) {
	/* Space is tight, encroach on the margins to maintain BLOB_SIZE */
	bw->blob.visual_y = (bw->core.height - BLOB_SIZE)/2;
	bw->blob.visual_height = BLOB_SIZE;
    }
    else if (bw->core.height > 2 * mt) {
	/* Can't fit BLOB_SIZE. Take what we can get. */
	bw->blob.visual_y = mt;
	bw->blob.visual_height = bw->core.height - 2 * mt;
    }
    else {
	/* No room for the blob */
	bw->blob.visual_height = 0;
    }
}



/************************************************************************
 *
 *  Redisplay
 *
 ***********************************************************************/
static void Redisplay (w, event, region)
    Widget w;
    XEvent *event;
    Region region;
{
    XmpBlobWidgetClass wc = (XmpBlobWidgetClass)XtClass(w);
    XmpBlobWidget bw = (XmpBlobWidget)w;

    /* Draw the class's visual */

    if (wc->blob_class.draw_visual) (*wc->blob_class.draw_visual) (w);

    /* Draw the class's shadow */

    if (wc->blob_class.draw_shadow) (*wc->blob_class.draw_shadow) (w);

    /* Update the class's traversal highlight */

    if (bw->primitive.highlighted) {
	if (wc->primitive_class.border_highlight)
	    (*wc->primitive_class.border_highlight) (w);
    }
    else {
        if (wc->primitive_class.border_unhighlight)
            (*wc->primitive_class.border_unhighlight) (w);
    }
}



/************************************************************************
 *
 *  SetValues
 *
 ************************************************************************/
static Boolean SetValues (old_w, request_w, new_w, args, num_args)
    Widget old_w;
    Widget request_w;
    Widget new_w;
    ArgList args;
    Cardinal *num_args;
{
    XmpBlobWidgetClass wc = (XmpBlobWidgetClass)XtClass(new_w);
    XmpBlobWidget cw = (XmpBlobWidget)old_w;
    XmpBlobWidget rw = (XmpBlobWidget)request_w;
    XmpBlobWidget nw = (XmpBlobWidget)new_w;
    Boolean redisplayFlag = False;

    /* Validate XmNblobShape */

    if (nw->blob.blob_shape != cw->blob.blob_shape) {
	if (!XmRepTypeValidValue (blobShapeId, nw->blob.blob_shape, (Widget)nw))
            nw->blob.blob_shape = cw->blob.blob_shape;
	else
	    redisplayFlag = True;
    }

    /* Redisplay on change in sensitivity */

    if (XtIsSensitive((Widget)nw) != XtIsSensitive((Widget)cw)) {
	redisplayFlag = True;
    }

    /* Update GC's on color change */

    if (nw->primitive.foreground != cw->primitive.foreground) {
	if (wc->blob_class.destroy_gc)
	    (*(wc->blob_class.destroy_gc))((Widget)cw);
	if (wc->blob_class.create_gc)
	    (*(wc->blob_class.create_gc))((Widget)nw);
        redisplayFlag = True;
    }

    /* Check for application geometry settings. '0' means 'ideal size' */

    if (rw->core.width == 0) {
	nw->core.width = 0;
	nw->blob.compute_width = True;
    }
    else if (rw->core.width != cw->core.width) {
	nw->core.width = rw->core.width;
	nw->blob.pref_width = rw->core.width;
	nw->blob.compute_width = False;
    }
    if (rw->core.height == 0) {
	nw->core.height = 0;
	nw->blob.compute_height = True;
    }
    else if (rw->core.height != cw->core.height) {
	nw->core.height = rw->core.height;
	nw->blob.pref_height = rw->core.height;
	nw->blob.compute_height = False;
    }

    nw->blob.reconfigure = False;
    if (nw->core.width != cw->core.width ||
        nw->core.height != cw->core.height ||
        nw->blob.margin_width != cw->blob.margin_width ||
        nw->blob.margin_height != cw->blob.margin_height ||
        nw->primitive.shadow_thickness != cw->primitive.shadow_thickness ||
        nw->primitive.highlight_thickness !=
                cw->primitive.highlight_thickness) {

	/* If we are a Blob calculate ideal size. If we are a subclass let */
	/* the subclass SetValues do it - the subclass CalcWidgetSize      */
	/* method may need information derived in its SetValues method.    */

	if ((WidgetClass)wc == xmpBlobWidgetClass) {
	    if (wc->blob_class.calc_widget_size)
		(*(wc->blob_class.calc_widget_size))((Widget)nw);

	    nw->blob.pref_width = nw->core.width;
	    nw->blob.pref_height = nw->core.height;

	    if (nw->core.width == cw->core.width &&
		nw->core.height == cw->core.height) {
		if (wc->core_class.resize)
		    (*(wc->core_class.resize))((Widget)nw);
	    }
	}
	else {
	    nw->blob.reconfigure = True;
	}
 	redisplayFlag = True;
    }

    return (redisplayFlag);
}



/************************************************************************
 *
 *  QueryGeometry
 *
 ************************************************************************/
static XtGeometryResult QueryGeometry (w, request, reply)
    Widget w;
    XtWidgetGeometry *request;
    XtWidgetGeometry *reply;
{
    XmpBlobWidget bw = (XmpBlobWidget)w;

    /* Return our preferred size */

    if ((request->request_mode & CWWidth) &&
	request->width == bw->blob.pref_width &&
	(request->request_mode & CWHeight) &&
	request->height == bw->blob.pref_height) {

	return (XtGeometryYes);
    }

    if (bw->core.width == bw->blob.pref_width &&
	bw->core.height == bw->blob.pref_height) {

	return (XtGeometryNo);
    }

    reply->request_mode = (CWWidth | CWHeight);
    reply->width = bw->blob.pref_width;
    reply->height = bw->blob.pref_height;

    return (XtGeometryAlmost);
}



/************************************************************************
 *
 *  DrawVisual
 *
 ************************************************************************/
static void DrawVisual (w)
    Widget w;
{
    XmpBlobWidgetClass wc = (XmpBlobWidgetClass)XtClass(w);
    XmpBlobWidget bw = (XmpBlobWidget)w;

    /* Draw XmNblobShape */

    if (bw->blob.visual_width > 0 && bw->blob.visual_height > 0) {
	switch (bw->blob.blob_shape) {
	    case(XmBLOB_OVAL):
	    default:
		XFillArc (XtDisplay(w), XtWindow(w),
			wc->blob_class.select_gc(w),
			bw->blob.visual_x, bw->blob.visual_y,
			bw->blob.visual_width, bw->blob.visual_height,
			0, 360 * 64);
		break;
	    case(XmBLOB_RECTANGLE):
		XFillRectangle (XtDisplay(w), XtWindow(w),
                        wc->blob_class.select_gc(w),
                        bw->blob.visual_x, bw->blob.visual_y,
                        bw->blob.visual_width, bw->blob.visual_height);
		break;
	}
    }
}



/************************************************************************
 *
 *  DrawShadow
 *
 ************************************************************************/
static void DrawShadow (w)
    Widget w;
{
    XmpBlobWidget bw = (XmpBlobWidget)w;

    /* Draw rectangular 3D shadow */

    if (bw->core.width > 2 * bw->primitive.highlight_thickness &&
	bw->core.height > 2 * bw->primitive.highlight_thickness &&
	bw->primitive.shadow_thickness > 0) {
	_XmDrawShadows (XtDisplay (bw), XtWindow (bw),
		bw->primitive.top_shadow_GC,
		bw->primitive.bottom_shadow_GC,
		bw->primitive.highlight_thickness,
		bw->primitive.highlight_thickness,
		bw->core.width - 2 * bw->primitive.highlight_thickness,
		bw->core.height - 2 * bw->primitive.highlight_thickness,
		bw->primitive.shadow_thickness, XmSHADOW_ETCHED_OUT);
    }
}



/************************************************************************
 *
 *  CreateGC
 *
 ************************************************************************/
static void CreateGC (w )
    Widget w;
{
    XmpBlobWidget bw = (XmpBlobWidget)w;
    XGCValues values;
    XtGCMask valueMask;

    /* Create normal GC */

    valueMask = GCForeground | GCBackground | GCGraphicsExposures;

    values.foreground = bw->primitive.foreground;
    values.background = bw->core.background_pixel;
    values.graphics_exposures = False;

    bw->blob.normal_gc = XtGetGC ((Widget)bw, valueMask, &values);

    /* Create grayed-out GC */

    valueMask |= GCFillStyle | GCStipple;
    values.fill_style = FillStippled;
    values.stipple = XmGetPixmapByDepth (XtScreen((Widget)(bw)),
				"50_foreground", 1, 0, 1);

    bw->blob.insensitive_gc = XtGetGC((Widget)bw, valueMask, &values);
}



/************************************************************************
 *
 *  DestroyGC
 *
 ************************************************************************/
static void DestroyGC (w)
    Widget w;
{
    XmpBlobWidget bw = (XmpBlobWidget)w;

    /* Destroy GC's */

    XtReleaseGC ((Widget)bw, bw->blob.normal_gc);
    XtReleaseGC ((Widget)bw, bw->blob.insensitive_gc);
}



/************************************************************************
 *
 *  SelectGC
 *
 ************************************************************************/
static GC SelectGC (w)
    Widget w;
{
    XmpBlobWidget bw = (XmpBlobWidget)w;
    GC drawGC;

    /* Select drawing GC */

    drawGC = XtIsSensitive((Widget)bw) ? bw->blob.normal_gc : bw->blob.insensitive_gc;

    return (drawGC);
}



/************************************************************************
 *
 *  CalcWidgetSize 
 *
 ************************************************************************/
static void CalcWidgetSize (w)
    Widget w;
{
    XmpBlobWidgetClass wc = (XmpBlobWidgetClass)XtClass(w);
    XmpBlobWidget bw = (XmpBlobWidget)w;

    /* Calculate ideal size. If application set geometry, use that. */

    if (wc->blob_class.calc_visual_size)
                (*(wc->blob_class.calc_visual_size))((Widget)bw);

    if (bw->blob.compute_width)
	bw->core.width = bw->blob.visual_width + 2 * (
		bw->blob.margin_width +
		bw->primitive.shadow_thickness +
		bw->primitive.highlight_thickness);
    else
	bw->core.width = bw->blob.pref_width;

    if (bw->blob.compute_height)
	bw->core.height = bw->blob.visual_height + 2 * (
		bw->blob.margin_height +
		bw->primitive.shadow_thickness +
		bw->primitive.highlight_thickness);
    else
	bw->core.height = bw->blob.pref_height;

}



/************************************************************************
 *
 *  CalcVisualSize 
 *
 ************************************************************************/
static void CalcVisualSize (w)
    Widget w;
{
    XmpBlobWidget bw = (XmpBlobWidget)w;

    /* Calculate ideal size of class's visual */
    bw->blob.visual_width = BLOB_SIZE;
    bw->blob.visual_height = BLOB_SIZE;
}



/************************************************************************
 *
 *  BlobActivate
 *
 ************************************************************************/
static void BlobActivate (w, event, params, num_params)
    Widget w;
    XEvent *event;
    String *params;
    Cardinal *num_params;
{

    /* Take focus */

    XmProcessTraversal (w, XmTRAVERSE_CURRENT);

    /* Activate noisily! */

    XBell (XtDisplay(w), 0);
}



/************************************************************************
 *
 *  XmpCreateBlob
 *
 ************************************************************************/
Widget XmpCreateBlob (parent, name, arglist, argCount)
    Widget parent;
    char *name;
    Arg *arglist;
    Cardinal argCount;
{
    return (XtCreateWidget (name, xmpBlobWidgetClass,
				parent, arglist, argCount));
}

