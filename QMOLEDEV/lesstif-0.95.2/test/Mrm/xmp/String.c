#include <StringP.h>
#include <Xm/RepType.h>

static void DefaultFont();
static XmImportOperator ToInternalXmString();
static void ToExternalXmString();
static void ClassInitialize();
static void Initialize();
static void Destroy();
static void Resize();
static Boolean SetValues();
static void DrawVisual();
static void CalcVisualSize();
static void CreateGC();


#define defaultTranslations "\
<EnterWindow>:		PrimitiveEnter()\n\
<LeaveWindow>:		PrimitiveLeave()\n\
~s ~m ~a <Key>Return:	PrimitiveParentActivate()\n\
<Key>osfActivate:	PrimitiveParentActivate()\n\
<Key>osfCancel:		PrimitiveParentCancel()\n\
<Key>osfHelp:		PrimitiveHelp()"


static XtResource resources[] = 
{
   {
	XmNtraversalOn,
	XmCTraversalOn,
	XmRBoolean,
	sizeof (Boolean),
	XtOffsetOf( struct _XmPrimitiveRec, primitive.traversal_on),
	XmRImmediate, 
	(XtPointer) False
    },
    {
	XmNshadowThickness,
	XmCShadowThickness,
	XmRHorizontalDimension,
	sizeof (Dimension),
	XtOffsetOf( struct _XmpStringRec, primitive.shadow_thickness),
	XmRImmediate,
	(XtPointer) 0
    },
    {    
	XmNtext,
	XmCText,
	XmRXmString,
	sizeof(_XmString),
	XtOffsetOf( struct _XmpStringRec,string.text),
	XmRImmediate,
	(XtPointer) NULL
    },
    {
	XmNfontList,
	XmCFontList,
	XmRFontList,
	sizeof(XmFontList),
	XtOffsetOf( struct _XmpStringRec,string.font_list),
	XtRCallProc,
	(XtPointer) DefaultFont
    },
    {
	XmNalignment,
	XmCAlignment,
	XmRAlignment,
	sizeof(unsigned char),
	XtOffsetOf( struct _XmpStringRec,string.alignment),
	XmRImmediate, 
	(XtPointer) XmALIGNMENT_CENTER
    },
    { 
	XmNstringDirection,
	XmCStringDirection,
	XmRStringDirection,
	sizeof(unsigned char),
	XtOffsetOf( struct _XmpStringRec,string.string_direction),
	XmRImmediate,
	(XtPointer) XmSTRING_DIRECTION_DEFAULT 
   },
};

static XmSyntheticResource syn_resources[] =
{
    {
	XmNtext, 
	sizeof (_XmString),
	XtOffsetOf( struct _XmpStringRec, string.text),
	ToExternalXmString,
	ToInternalXmString
    },
};


externaldef (xmpstringclassrec) XmpStringClassRec xmpStringClassRec = {
  {
    (WidgetClass)&xmpBlobClassRec,            /* superclass            */
    "XmpString",                              /* class_name            */
    sizeof(XmpStringRec),                     /* widget_size           */
    ClassInitialize,                          /* class_initialize      */
    NULL,                                     /* class_part_initialize */
    FALSE,                                    /* class_inited          */
    Initialize,                               /* initialize            */
    NULL,                                     /* initialize_hook       */
    XtInheritRealize,                         /* realize               */
    NULL,                                     /* actions               */
    NULL,                                     /* num_actions           */
    resources,                                /* resources             */
    XtNumber(resources),                      /* num_resources         */
    NULLQUARK,                                /* xrm_class             */
    TRUE,                                     /* compress_motion       */
    XtExposeCompressMaximal,                  /* compress_exposure     */
    TRUE,                                     /* compress_enterleave   */
    FALSE,                                    /* visible_interest      */
    Destroy,                                  /* destroy               */
    Resize,                                   /* resize                */
    XtInheritExpose,                          /* expose                */
    SetValues,                                /* set_values            */
    NULL,                                     /* set_values_hook       */
    XtInheritSetValuesAlmost,                 /* set_values_almost     */
    NULL,                                     /* get_values_hook       */
    NULL,                                     /* accept_focus          */
    XtVersion,                                /* version               */
    NULL,                                     /* callback_private      */
    defaultTranslations,                      /* tm_table              */
    XtInheritQueryGeometry,                   /* query_geometry        */
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
    XmInheritDrawShadow,                      /* draw_shadow           */
    CreateGC,                                 /* create_text_gc        */
    XmInheritDestroyGC,                       /* destroy_text_gc       */
    XmInheritSelectGC,                        /* select_text_gc        */
    XmInheritCalcWidgetSize,                  /* calc_widget_size      */
    CalcVisualSize,                           /* calc_visual_size      */
    NULL,                                     /* extension             */
  },
  {                                           /* XmpString             */
    XmLABEL_FONTLIST,                         /* default_fontlist_type */
    NULL,                                     /* extension             */
  }
};

externaldef( xmpstringwidgetclass) WidgetClass xmpStringWidgetClass =
                                (WidgetClass) &xmpStringClassRec;


static XmRepTypeId alignmentId;
static XmRepTypeId stringDirectionId;


/************************************************************************
 *
 *  DefaultFont
 *
 ***********************************************************************/
static void DefaultFont (w, offset, value)
    Widget w;
    int offset;
    XrmValue *value;
{
    XmpStringWidgetClass wc = (XmpStringWidgetClass)XtClass(w);
    static XmFontList f;

    /* Dynamic default for XmNfontList. Get class's default font type. */

    f = _XmGetDefaultFontList (w, wc->string_class.default_fontlist_type);

    value->addr = (char *)&f;
    value->size = sizeof(f);
}



/************************************************************************
 *
 *  ToInternalXmString
 *
 ***********************************************************************/
static XmImportOperator ToInternalXmString (w, offset, value)
    Widget w;
    int offset;
    XtArgVal *value;
{
    XmpStringWidget sw = (XmpStringWidget)w;
    _XmString is = NULL;

    /* Convert synthetic resource to internal format. */

    if (sw->string.text != NULL &&
	_XmStringIsXmString ((XmString)sw->string.text))
	is = _XmStringCreate ((XmString)sw->string.text);

    *value = (XtArgVal)is;

    return XmSYNTHETIC_LOAD;
}



/************************************************************************
 *
 *  ToExternalXmString
 *
 ***********************************************************************/
static void ToExternalXmString (w, resource, value)
    Widget w;
    XrmQuark resource;
    XtArgVal *value;
{
    XmpStringWidget sw = (XmpStringWidget)w;
    XmString es = NULL;

    /* Convert synthetic resource value to external format */
 
    if (sw->string.text != NULL)
	es = _XmStringCreateExternal (sw->string.font_list, sw->string.text);

    *value = (XtArgVal)es;
}



/*********************************************************************
 *
 *  ClassInitialize
 *
 ********************************************************************/         
static void ClassInitialize()
{

    /* Use Motif representation types for XmNalignment, XmNstringDirection */

    alignmentId = XmRepTypeGetId (XmRAlignment);
    stringDirectionId = XmRepTypeGetId (XmRStringDirection);
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
    XmpStringWidgetClass wc = (XmpStringWidgetClass)XtClass(new_w);
    XmpStringWidget nw = (XmpStringWidget)new_w;
    unsigned char stringDirection;
    Arg dirArgs[1];

    /* Validate  XmNalignment */

    if (!XmRepTypeValidValue (alignmentId, nw->string.alignment, (Widget)nw))
	nw->string.alignment = XmALIGNMENT_CENTER;
    
   /* Get Default XmNstringDirection from parent */

   if (nw->string.string_direction == XmSTRING_DIRECTION_DEFAULT) {
	if (XmIsManager (XtParent(nw))) {
	    XtSetArg (dirArgs[0], XmNstringDirection, &stringDirection);
	    XtGetValues (XtParent(nw), dirArgs, 1);
	    nw->string.string_direction = stringDirection;
	}
	else {
	    nw->string.string_direction = XmSTRING_DIRECTION_L_TO_R;
	}
    }

    /* Validate XmNstringDirection */

    if (!XmRepTypeValidValue (stringDirectionId, nw->string.string_direction,
			 (Widget)nw))
	nw->string.string_direction = XmSTRING_DIRECTION_L_TO_R;

    /* Make a local copy of XmNfontList */

    nw->string.font_list = XmFontListCopy (nw->string.font_list);

    /* If we are a String calculate ideal size. If we are a subclass let  */
    /* the subclass Initialize do it - the subclass CalcWidgetSize      */
    /* method may need information derived in its Initialize method.    */

    if ((WidgetClass)wc == xmpStringWidgetClass) {
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
    XmpStringWidget sw = (XmpStringWidget)w;

    /* Free XmNtext and XmNfontList */

    if (sw->string.text != NULL)
	_XmStringFree (sw->string.text);

    if (sw->string.font_list != NULL)
	XmFontListFree (sw->string.font_list);
}



/************************************************************************
 *
 * Resize
 *
 ************************************************************************/
static void Resize (w)
    Widget w;
{
    XmpStringWidget sw = (XmpStringWidget)w;
    Dimension mw, mh;

    /* Configure internal geometry using current size */

    if (sw->blob.visual_width == 0 || sw->blob.visual_height == 0) return;

    mw = sw->blob.margin_width +
	sw->primitive.highlight_thickness +
	sw->primitive.shadow_thickness;
    mh = sw->blob.margin_height +
	sw->primitive.highlight_thickness +
	sw->primitive.shadow_thickness;

    if (sw->string.alignment == XmALIGNMENT_CENTER) {
	/* Center */
	sw->blob.visual_x = (sw->core.width - sw->blob.visual_width)/2;
    }
    else if ((sw->string.string_direction == XmSTRING_DIRECTION_L_TO_R &&
	      sw->string.alignment == XmALIGNMENT_BEGINNING) ||
	     (sw->string.string_direction == XmSTRING_DIRECTION_R_TO_L &&
              sw->string.alignment == XmALIGNMENT_END)) {
	/* Left */
	sw->blob.visual_x = mw;
    }
    else if ((sw->string.string_direction == XmSTRING_DIRECTION_L_TO_R &&
              sw->string.alignment == XmALIGNMENT_END) ||
             (sw->string.string_direction == XmSTRING_DIRECTION_R_TO_L &&
              sw->string.alignment == XmALIGNMENT_BEGINNING)) {
	/* Right */
	sw->blob.visual_x = sw->core.width - mw - sw->blob.visual_width;
    }   

    sw->blob.visual_y = (sw->core.height - sw->blob.visual_height)/2;
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
    XmpStringWidgetClass wc = (XmpStringWidgetClass)XtClass(new_w);
    XmpStringWidget cw = (XmpStringWidget)old_w;
    XmpStringWidget nw = (XmpStringWidget)new_w;
    Boolean redisplayFlag = False;

    /* Free old XmNtext */

    if (nw->string.text != cw->string.text) {   
	_XmStringFree(cw->string.text);
    }

    /* Update XmNtext on XmNfontList change */

    if (nw->string.font_list != cw->string.font_list) {
	nw->string.font_list = XmFontListCopy (nw->string.font_list);
	_XmStringUpdate (nw->string.font_list, nw->string.text);
	XmFontListFree (cw->string.font_list);
    }

    /* Validate XmNalignment */

    if (nw->string.alignment != cw->string.alignment) {
	if (!XmRepTypeValidValue(alignmentId, nw->string.alignment, (Widget)nw))
	    nw->string.alignment = nw->string.alignment;
    }

    /* Validate XmNstringDirection */

    if (nw->string.string_direction != cw->string.string_direction) {
	if (!XmRepTypeValidValue (stringDirectionId,nw->string.string_direction,
				(Widget)nw))
	    nw->string.string_direction = cw->string.string_direction;
	else
	    redisplayFlag = True;
    }

    if (nw->blob.reconfigure == True ||
	nw->string.text != cw->string.text ||
	nw->string.font_list != cw->string.font_list ||
	nw->string.alignment != cw->string.alignment ||
	nw->string.string_direction != cw->string.string_direction) {

	/* If we are a String calculate ideal size. If we are a subclass let */
	/* the subclass SetValues do it - the subclass CalcWidgetSize        */
	/* method may need information derived in its SetValues method.      */

	if ((WidgetClass)wc == xmpStringWidgetClass) {
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
 *  DrawVisual
 *
 ************************************************************************/
static void DrawVisual (w)
    Widget w;
{
    XmpStringWidgetClass wc = (XmpStringWidgetClass)XtClass(w);
    XmpStringWidget sw = (XmpStringWidget)w;

   /* Override Blob method - draw XmNtext */

    if (sw->string.text &&
	sw->blob.visual_width != 0 &&
	sw->blob.visual_height != 0) {
	_XmStringDraw (XtDisplay(sw), XtWindow(sw),
		sw->string.font_list, sw->string.text,
		wc->blob_class.select_gc(w),
		sw->blob.visual_x, sw->blob.visual_y,
		sw->blob.visual_width, sw->string.alignment,
		sw->string.string_direction, NULL);
    }
}



/************************************************************************
 *
 *  CreateGC
 *
 ************************************************************************/
static void CreateGC (w)
    Widget w;
{
    XmpStringWidget sw = (XmpStringWidget)w;
    XGCValues values;
    XtGCMask valueMask;
    XFontStruct *fs = (XFontStruct *) NULL;

    /* Override Blob method - create GC's with a font */

    valueMask = GCForeground | GCBackground | GCFont | GCGraphicsExposures;

    values.foreground = sw->primitive.foreground;
    values.background = sw->core.background_pixel;
    values.graphics_exposures = False;

    _XmFontListGetDefaultFont(sw->string.font_list, &fs);
    if (fs != NULL)
	values.font = fs->fid;
    else
	valueMask &= ~GCFont;

    sw->blob.normal_gc = XtGetGC ((Widget)sw, valueMask, &values);

    valueMask |= GCFillStyle | GCStipple;
    values.fill_style = FillStippled;
    values.stipple = XmGetPixmapByDepth (XtScreen((Widget)(sw)),
				"50_foreground", 1, 0, 1);

    sw->blob.insensitive_gc = XtGetGC((Widget) sw, valueMask, &values);
}



/************************************************************************
 *
 * CalcVisualSize
 *
 ************************************************************************/
static void CalcVisualSize (w)
    Widget w;
{
    XmpStringWidget sw = (XmpStringWidget)w;

    /* Override Blob method - make room for XmNtext */

    if (sw->string.text && !_XmStringEmpty(sw->string.text)) {
        _XmStringExtent (sw->string.font_list, sw->string.text,
			&(sw->blob.visual_width), &(sw->blob.visual_height));
    }
    else {
	sw->blob.visual_width = 0;
	sw->blob.visual_height = 0;
    }
}



/************************************************************************
 *
 *  XmpCreateString
 *      Externally accessable function for creating a String widget
 *
 ************************************************************************/
Widget XmpCreateString (parent, name, arglist, argCount)
    Widget parent;
    char *name;
    Arg *arglist;
    Cardinal argCount;
{
    return (XtCreateWidget(name,xmpStringWidgetClass,parent,arglist,argCount));
}

