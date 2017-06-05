/*
 *
 * Grid.c - XmpGrid widget
 *
 */

#include <GridP.h>
#include <Xm/DrawP.h>
#include <Xm/GadgetP.h>

#define GetGridConstraint(w) \
        (&((XmpGridConstraintPtr) (w)->core.constraints)->grid)

#define Max(x, y) (((x) > (y)) ? (x) : (y))

static void Initialize();
static void Resize();
static void Redisplay();
static Boolean SetValues();
static XtGeometryResult QueryGeometry();
static XtGeometryResult GeometryManager();
static void ChangeManaged();
static Boolean ConstraintSetValues();
static void Layout();
static void CalcSize();

static XtResource resources[] =
{
    {
	XmNrows, 
	XmCRows, 
	XmRShort, 
	sizeof (short),
	XtOffsetOf( struct _XmpGridRec, grid.rows),
	XmRImmediate, (XtPointer) 4
    },
    {
	XmNcolumns, 
	XmCColumns, 
	XmRShort, 
	sizeof (short),
	XtOffsetOf( struct _XmpGridRec, grid.columns),
	XmRImmediate, (XtPointer) 4
    },
    {
	XmNmarginWidth, 
	XmCMarginWidth, 
	XmRHorizontalDimension, 
	sizeof (Dimension),
	XtOffsetOf( struct _XmpGridRec, grid.margin_width),
	XmRImmediate, (XtPointer) 10
    },
    {
	XmNmarginHeight, 
	XmCMarginHeight, 
	XmRVerticalDimension,
	sizeof (Dimension),
	XtOffsetOf( struct _XmpGridRec, grid.margin_height),
	XmRImmediate, (XtPointer) 10
    },
};

static XmSyntheticResource syn_resources[] = 
{
    {
	XmNmarginWidth, 
	sizeof (Dimension),
	XtOffsetOf( struct _XmpGridRec, grid.margin_width),
	_XmFromHorizontalPixels,
	_XmToHorizontalPixels
    },
    {
	XmNmarginHeight, 
	sizeof (Dimension),
	XtOffsetOf( struct _XmpGridRec, grid.margin_height),
	_XmFromVerticalPixels,
	_XmToVerticalPixels
    }
};

static XtResource constraints[] =
{
    {
	XmNgridMarginWidth,
	XmCGridMarginWidth,
	XmRHorizontalDimension, sizeof (Dimension),
	XtOffsetOf( struct _XmpGridConstraintRec, grid.grid_margin_width),
	XmRImmediate, (XtPointer) 0
    },
    {
	XmNgridMarginHeight,
	XmCGridMarginHeight,
	XmRVerticalDimension,sizeof (Dimension),
	XtOffsetOf( struct _XmpGridConstraintRec, grid.grid_margin_height),
	XmRImmediate, (XtPointer) 0
    },
};

static XmSyntheticResource syn_constraints[] =
{
    {
	XmNgridMarginWidth,
	sizeof (Dimension),
	XtOffsetOf( struct _XmpGridConstraintRec, grid.grid_margin_width),
	_XmFromHorizontalPixels,
	_XmToHorizontalPixels
    },
    {
	XmNgridMarginHeight,
	sizeof (Dimension),
	XtOffsetOf( struct _XmpGridConstraintRec, grid.grid_margin_height),
	_XmFromVerticalPixels,
	_XmToVerticalPixels
    },
};




/****************************************************************
 *
 * XmpGrid class record
 *
 ****************************************************************/

externaldef(xmgridclassrec) XmpGridClassRec xmpGridClassRec = 
{
  {                                           /* core_class            */
    (WidgetClass) &xmManagerClassRec,         /* superclass            */
    "XmpGrid",                                /* class_name            */
    sizeof(XmpGridRec),                       /* widget_size           */
    NULL,                                     /* class_part_initialize */
    NULL,                                     /* class_initialize      */
    FALSE,                                    /* class_inited          */
    Initialize,                               /* initialize            */
    NULL,                                     /* initialize_hook       */
    XtInheritRealize,                         /* realize               */
    NULL,                                     /* actions               */
    0,                                        /* num_actions           */
    resources,                                /* resources             */
    XtNumber(resources),                      /* num_resources         */
    NULLQUARK,                                /* xrm_class             */
    TRUE,                                     /* compress_motion       */
    XtExposeCompressMaximal,                  /* compress_exposure     */
    TRUE,                                     /* compress_enterleave   */
    FALSE,                                    /* visible_interest      */
    NULL,                                     /* destroy               */
    Resize,                                   /* resize                */
    Redisplay,                                /* expose                */
    SetValues,                                /* set_values            */
    NULL,                                     /* set_values_hook       */
    XtInheritSetValuesAlmost,                 /* set_values_almost     */
    NULL,                                     /* get_values_hook       */
    NULL,                                     /* accept_focus          */
    XtVersion,                                /* version               */
    NULL,                                     /* callback_private      */
    XtInheritTranslations,                    /* tm_table              */
    QueryGeometry,                            /* query_geometry        */
    NULL,                                     /* display_accelerator   */
    NULL,                                     /* extension             */
  },

  {                                           /* composite_class       */
    GeometryManager,                          /* geometry_manager      */
    ChangeManaged,                            /* change_managed        */
    XtInheritInsertChild,                     /* insert_child          */
    XtInheritDeleteChild,                     /* delete_child          */
    NULL,                                     /* extension             */
  },

  {                                           /* constraint_class      */
    constraints,                              /* resources             */   
    XtNumber(constraints),                    /* num_resources         */   
    sizeof (XmpGridConstraintRec),            /* constraint_size       */   
    NULL,                                     /* initialize            */   
    NULL,                                     /* destroy               */   
    ConstraintSetValues,                      /* set_values            */   
    NULL,                                     /* extension             */
  },

  {                                           /* manager class         */
    XtInheritTranslations,                    /* translations          */
    syn_resources,                            /* syn_resources         */
    XtNumber(syn_resources),                  /* num_syn_resources     */
    syn_constraints,                          /* syn_constraint_resources */
    XtNumber(syn_constraints),                /* num_syn_constraint_resources */
    XmInheritParentProcess,                   /* parent_process        */
    NULL,                                     /* extension             */    
  },

  {                                           /* grid class            */
    NULL,                                     /* extension             */    
  }        
};

externaldef(xmpgridwidgetclass) WidgetClass xmpGridWidgetClass =
			                        (WidgetClass) &xmpGridClassRec;




/************************************************************************
 *
 *  Initialize
 *
 ************************************************************************/
static void Initialize (request_w, new_w, args, num_args)
    Widget request_w;
    Widget new_w;
    ArgList args;
    Cardinal *num_args;
{
    XmpGridWidget rw = (XmpGridWidget)request_w;
    XmpGridWidget nw = (XmpGridWidget)new_w;

    nw->grid.processing_constraints = False;

    /* Remember application geometry settings */

    nw->grid.compute_width = True;
    nw->grid.compute_height = True;

    if (rw->core.width != 0) {
	nw->grid.compute_width = False;
	nw->grid.pref_width = rw->core.width;
	nw->core.width = rw->core.width;
    }
    if (rw->core.height != 0) {
	nw->grid.compute_height = False;
	nw->grid.pref_height = rw->core.height;
	nw->core.height = rw->core.height;
    }

    /* Calculate ideal size */

    CalcSize (nw, NULL, &(nw->core.width), &(nw->core.height));

    nw->grid.pref_width = nw->core.width;
    nw->grid.pref_height = nw->core.height;

    Resize((Widget)nw);
}




/************************************************************************
 *
 *  Resize 
 *
 ************************************************************************/
static void Resize (w)
    Widget w;
{
    /* Configure using current size */

    Layout ((XmpGridWidget)w, NULL);
}




/************************************************************************
 *
 *  Redisplay
 *
 ************************************************************************/
static void Redisplay (w, event, region)
    Widget w;
    XEvent *event;
    Region region;
{
    /* Pass exposure down to gadget children */

    _XmRedisplayGadgets (w, event, region);
}




/************************************************************************
 *
 *  Set Values
 *
 ************************************************************************/
static Boolean SetValues (old_w, request_w, new_w, args, num_args)
    Widget old_w;
    Widget request_w;
    Widget new_w;
    ArgList args;
    Cardinal *num_args;
{
    XmpGridWidget cw = (XmpGridWidget)old_w;
    XmpGridWidget rw = (XmpGridWidget)request_w;
    XmpGridWidget nw = (XmpGridWidget)new_w;
    Boolean redisplay = False;

    /* Check for application geometry settings. '0' means 'ideal size' */

    if (rw->core.width == 0) {
	rw->core.width = 0;
        nw->grid.compute_width = True;
    }
    else if (rw->core.width != cw->core.width) {
        nw->grid.compute_width = False;
        nw->grid.pref_width = rw->core.width;
        nw->core.width = rw->core.width;
    }
    if (rw->core.height == 0) {
	rw->core.height = 0;
        nw->grid.compute_height = True;
    }
    else if (rw->core.height != cw->core.height) {
        nw->grid.compute_height = False;
        nw->grid.pref_height = rw->core.height;
        nw->core.height = rw->core.height;
    }

    if (nw->core.width != cw->core.width ||
	nw->core.height != cw->core.height ||
	nw->grid.margin_width != cw->grid.margin_width ||
	nw->grid.margin_height != cw->grid.margin_height ||
	nw->grid.rows != cw->grid.rows ||
	nw->grid.columns != cw->grid.columns) {

	/* Calculate new size */

	CalcSize (nw, NULL, &nw->core.width, &nw->core.height);

	nw->grid.pref_width = nw->core.width;
	nw->grid.pref_height = nw->core.height;

	/* If there will be no geometry request better reconfigure now */

	if (nw->core.width == cw->core.width &&
	    nw->core.height == cw->core.height) {
	    Resize(nw);
	}
    }

   return (redisplay);
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
    XmpGridWidget gw = (XmpGridWidget)w;

    /* Return our preferred size */

    if ((request->request_mode & CWWidth) &&
	request->width == gw->grid.pref_width &&
	(request->request_mode & CWHeight) &&
	request->height == gw->grid.pref_height) {

	return (XtGeometryYes);
    }

    if (gw->core.width == gw->grid.pref_width &&
	gw->core.height == gw->grid.pref_height) {

	return (XtGeometryNo);
    }

    reply->request_mode = (CWWidth | CWHeight);
    reply->width = gw->grid.pref_width;
    reply->height = gw->grid.pref_height;

    return (XtGeometryAlmost);
}




/************************************************************************
 *
 *  Geometry Manager
 *
 ************************************************************************/
static XtGeometryResult GeometryManager (w, request, reply)
    Widget w;
    XtWidgetGeometry *request;
    XtWidgetGeometry *reply;
{
    XmpGridWidget gw = (XmpGridWidget) XtParent(w);
    XtWidgetGeometry parentRequest;
    XtGeometryResult result;
    Position curX, curY;
    Dimension curWidth, curHeight, curBW;

    /* If the request was caused by ConstraintSetValues reset the flag */

    if (gw->grid.processing_constraints) {
	gw->grid.processing_constraints = False;
	request -> border_width -= 1;
    }

    curX = w->core.x;
    curY = w->core.y;
    curWidth = w->core.width;
    curHeight = w->core.height;
    curBW = w->core.border_width;

    if (request->request_mode & CWX) w->core.x = request->x;
    if (request->request_mode & CWY) w->core.y = request->y;
    if (request->request_mode & CWWidth) w->core.width = request->width;
    if (request->request_mode & CWHeight) w->core.height = request->height;
    if (request->request_mode & CWBorderWidth) w->core.border_width =
						request->border_width;

    /* Calculate new ideal size */

    CalcSize (gw, w, &parentRequest.width, &parentRequest.height);

    gw->grid.pref_width = parentRequest.width;
    gw->grid.pref_height = parentRequest.height;

    /* Ask parent if new size is acceptable */

    parentRequest.request_mode = CWWidth | CWHeight;
    if (request->request_mode & XtCWQueryOnly)
	 parentRequest.request_mode |= XtCWQueryOnly;

    result = XtMakeGeometryRequest ((Widget)gw, &parentRequest, NULL);

    if (result == XtGeometryAlmost) result = XtGeometryNo;

    if (result == XtGeometryNo || request->request_mode & XtCWQueryOnly) {

	/* Not accepted. Restore original geometry. */
	w->core.x = curX;
	w->core.y = curY;
	w->core.width = curWidth;
	w->core.height = curHeight;
	w->core.border_width = curBW;
    }
    else {
	/* Accepted. Reconfigure children. */
	Layout (gw, w);
	/* If instigator hasn't changed size, force it to reconfigure */
	if (w->core.width == curWidth && w->core.height == curHeight) {
	    WidgetClass wc = XtClass(w);
	    if (wc->core_class.resize) (*(wc->core_class.resize))(w);
	}
    }

    return (result);
}



/************************************************************************
 *
 *  ChangeManaged
 *
 ************************************************************************/
static void ChangeManaged (w)
    Widget w;
{
    XmpGridWidget gw = (XmpGridWidget) w;
    Dimension gridWidth, gridHeight;

    /* Calculate ideal size */

    CalcSize (gw, NULL, &gridWidth, &gridHeight);

    gw->grid.pref_width = gridWidth;
    gw->grid.pref_height = gridHeight;

    /* Ask parent if new size is acceptable */

    while (XtMakeResizeRequest ((Widget)gw, gridWidth, gridHeight,
			       &gridWidth, &gridHeight) == XtGeometryAlmost);

    /* Reconfigure children with agreed size */

    Layout (gw, NULL);

    /* Update keyboard traversal */

    _XmNavigChangeManaged ((Widget)gw);
}




/************************************************************************
 *
 *  Constraint SetValues
 *
 ************************************************************************/
static Boolean ConstraintSetValues (cw, rw, nw, args, num_args)
    Widget cw;
    Widget rw;
    Widget nw;
    ArgList args ;
    Cardinal *num_args ;
{
    XmpGridConstraint nc;
    XmpGridConstraint cc;
    XmpGridWidget gw;
    Boolean reconfigure = False;

    if (!XtIsRectObj (nw)) return (False);

    gw = (XmpGridWidget)XtParent(nw);
    nc = GetGridConstraint(nw);
    cc = GetGridConstraint(cw);

    /* Check for change in XmNgridMarginWidth, XmNgridMarginHeight */

    if (nc->grid_margin_width != cc->grid_margin_width ||
	nc->grid_margin_height != cc->grid_margin_height) {
	reconfigure = True;
    }

    if (reconfigure && XtIsManaged (nw)) {

	/* Signal to Xt that a reconfigure is needed */

	gw->grid.processing_constraints = True;
	nw->core.border_width += 1;
    }

    return (False);
}




/************************************************************************
 *
 *  Layout
 *
 ************************************************************************/
static void Layout (gw, instigator)
    XmpGridWidget gw;
    Widget instigator;
{
    Dimension mw = gw->grid.margin_width;
    Dimension mh = gw->grid.margin_height;
    Dimension gridWidth;
    Dimension gridHeight;
    Dimension availWidth = 1;
    Dimension availHeight = 1;
    Dimension childWidth = 1;
    Dimension childHeight = 1;
    Dimension incWidth;
    Dimension incHeight;
    int row, column;
    int i;

    /* Configure children using current size */

    gridWidth = gw->core.width;
    gridHeight = gw->core.height;

    if (gridWidth > 2 * mw) availWidth = gridWidth - 2 * mw;
    incWidth = availWidth / gw->grid.columns;
    if (incWidth > 0) childWidth = incWidth;

    if (gridHeight > 2 * mh) availHeight = gridHeight - 2 * mh;
    incHeight = availHeight / gw->grid.rows;
    if (incHeight > 0) childHeight = incHeight;

    row = 0;
    column = 0;

    for (i = 0; i < gw->composite.num_children; i++) {
	Widget ic = gw->composite.children[i];
	XmpGridConstraint glc = GetGridConstraint (ic);
	Dimension gmw = glc->grid_margin_width;
	Dimension gmh = glc->grid_margin_height;
	Position cx, cy;
	Dimension cw, ch, cb;

	if (!XtIsManaged(ic)) continue;

	cb = ic->core.border_width;

	if (incWidth > 2 * (gmw + cb)) {
	    cx = mw + column * incWidth + gmw;
	    cw = incWidth - 2 * (gmw + cb);
	}
	else {
	    cx = mw + column * incWidth + incWidth/2 - cb;
	    cw = 1;
	}
	if (incHeight > 2 * (gmh + cb)) {
	    cy = mh + row * incHeight + gmh;
	    ch = incHeight - 2 * (gmh + cb);
	}
	else {
	    cy = mh + row* incHeight + incHeight/2 - cb;
	    ch = 1;
	}

	/* If layout is instigated by the GeometryManager don't configure the */
	/* requesting child, just set its geometry and let Xt configure it.   */

	if (ic != instigator) {
	    XtConfigureWidget (ic, cx, cy, cw, ch, cb);
	}
	else {
	    ic->core.x = cx;
	    ic->core.y = cy;
	    ic->core.width = cw;
	    ic->core.height = ch;
	    ic->core.border_width = cb;
	}

	column += 1;
	if (column == gw->grid.columns) {
	    column = 0;
	    row += 1;
	}
    }
}




/************************************************************************
 *
 *  CalcSize
 *
 ************************************************************************/
static void CalcSize (gw, instigator, gridWidth, gridHeight)
    XmpGridWidget gw;
    Widget instigator;
    Dimension *gridWidth;
    Dimension *gridHeight;
{
    Dimension maxWidth = 1;
    Dimension maxHeight = 1;
    int i;

    /* Calculate the ideal size. If application set geometry, use that. */

    for (i = 0; i < gw->composite.num_children; i++) {
	Widget ic = gw->composite.children[i];

	if (XtIsManaged(ic)) {
	    XmpGridConstraint glc = GetGridConstraint (ic);
	    Dimension width, height;
	    Dimension cw, ch, cb;

	    /* Get child's preferred geometry. If child is involved in a */
	    /* geometry negotiation, use its requested size instead.     */

	    if (ic != instigator) {
		XtWidgetGeometry reply;

		XtQueryGeometry (ic, NULL, &reply);
		cw = (reply.request_mode & CWWidth) ? reply.width :
							ic->core.width;
		ch = (reply.request_mode & CWHeight) ? reply.height :
							ic->core.height;
	    }
	    else {
		cw = ic->core.width;
		ch = ic->core.height;
	    }

	    cb = ic->core.border_width;

	    width = cw + 2 * (cb + glc->grid_margin_width);
	    height = ch + 2 * (cb + glc->grid_margin_height);

	    maxWidth = Max (width, maxWidth);
	    maxHeight = Max (height, maxHeight);
	}
    }

    if (gridWidth) {
	if (gw->grid.compute_width)
	    *gridWidth = maxWidth * gw->grid.columns +
				2 * (gw->grid.margin_width);
	else
	    *gridWidth = gw->grid.pref_width;
    }

    if (gridHeight) {
	if (gw->grid.compute_height)
	    *gridHeight = maxHeight * gw->grid.rows +
				2 * (gw->grid.margin_height);
	else
	    *gridHeight = gw->grid.pref_height;
    }
}




/************************************************************************
 *
 *  XmpCreateGrid
 *
 ************************************************************************/
Widget XmpCreateGrid (parent, name, arglist, argcount)
    Widget parent ;
    char *name ;
    ArgList arglist ;
    Cardinal argcount ;
{
    return (XtCreateWidget (name, xmpGridWidgetClass, 
				parent, arglist, argcount));
}

