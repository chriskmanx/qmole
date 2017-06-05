/*
 * SpinButton.c, Interleaf, 16aug93 2:37pm Version 1.1.
 */

/***********************************************************
Copyright 1993 Interleaf, Inc.

Permission to use, copy, modify, and distribute this software
and its documentation for any purpose without fee is granted,
provided that the above copyright notice appear in all copies
and that both copyright notice and this permission notice appear
in supporting documentation, and that the name of Interleaf not
be used in advertising or publicly pertaining to distribution of
the software without specific written prior permission.

Interleaf makes no representation about the suitability of this
software for any purpose. It is provided "AS IS" without any
express or implied warranty. 
******************************************************************/

/*
 * (C) Copyright 1991,1992, 1993
 * Interleaf, Inc.
 * 9 Hillside Avenue, 
 * Waltham, MA  02154
 *
 * SpinButton.c (DtSpinButtonWidget):
 *
 * I wanted a margin around the widget (outside the shadow, like buttons), 
 * so that the spin-button could be made the smae size as a 
 * push-button, etc.  The bulletin-board widget always puts the shadow at 
 * the outside edge of the widget, so spin-button is a sublcass of
 * manager, and we do everything ourselves.
 * 
 * One must be carefull when using Dimension (for core width and height).
 * Dimension is an unsigned short.  This causes problems when subtracting
 * and ending up with what should be a negative number (but it doesn't).
 * All child widget positioning is done by the spin_button.  We don't
 * use any heavy-weight forms, etc. to help us out.
 * 
 * There is no padding when editable.  If using a label, give it a
 * small margin, so it doesn't run up against the side of our
 * shadow or the arrow.
 *
 * Make some of the SpinButton functions common, so they can be shared
 * with ComboBox.
 *
 * Known bugs:
 *	    Changing margin_width or margin_height resources when the
 *	    spin_button has focus will probably result in display glitches.
 */
#include <stdio.h>
#include "SpinButtonP.h"

static void	ClassInitialize AA(());
static void	Initialize AA((DtSpinButtonWidget request, 
			       DtSpinButtonWidget new, ArgList given_args, 
			       Cardinal *num_args));
static XmNavigability WidgetNavigable AA((DtSpinButtonWidget spin));
static void	_SpinButtonFocusIn AA((DtSpinButtonWidget spin, XEvent *event, 
				       char **params, Cardinal *num_params));
static void	_SpinButtonFocusOut AA((DtSpinButtonWidget spin, XEvent *event,
					char **params, Cardinal *num_params));
static void	DrawHighlight AA((DtSpinButtonWidget spin, Boolean clear));
static void	_SpinButtonUp AA((DtSpinButtonWidget spin, 
				  XEvent *event, char **params, 
				  Cardinal *num_params));
static void	_SpinButtonDown AA((DtSpinButtonWidget spin, 
				    XEvent *event, char **params, 
				    Cardinal *num_params));
static void	_SpinButtonLeft AA((DtSpinButtonWidget spin, 
				    XEvent *event, char **params, 
				    Cardinal *num_params));
static void	_SpinButtonRight AA((DtSpinButtonWidget spin, 
				     XEvent *event, char **params, 
				     Cardinal *num_params));
static void	_SpinButtonBeginLine AA((DtSpinButtonWidget spin, 
					 XEvent *event, char **params, 
					 Cardinal *num_params));
static void	_SpinButtonEndLine AA((DtSpinButtonWidget spin, 
				       XEvent *event, char **params, 
				       Cardinal *num_params));
static void	CheckResources AA((DtSpinButtonWidget spin));
static void	Destroy AA((DtSpinButtonWidget spin));
static void	Resize AA((DtSpinButtonWidget spin));
static void	Redisplay AA((DtSpinButtonWidget w, XEvent *event, 
			      Region region));
static XtGeometryResult GeometryManager AA((Widget w, 
					    XtWidgetGeometry *request, 
					    XtWidgetGeometry *reply));
static void	SetSpinButtonSize AA((DtSpinButtonWidget spin));
static void	ForceChildSizes AA((DtSpinButtonWidget spin));
static void	LayoutChildren AA((DtSpinButtonWidget spin));
static Boolean	SetValues AA((DtSpinButtonWidget current, 
			      DtSpinButtonWidget request, 
			      DtSpinButtonWidget new));
static void	ClearShadow AA((DtSpinButtonWidget w, Boolean all));
static void	DrawShadow AA((DtSpinButtonWidget w));
static void	StoreResourceInfo AA((DtSpinButtonPart *spin_p,
				      DtSpinButtonPart *old_p,
				      Boolean do_items));
static char*	GetTextString AA((XmString xm_string));
static void	SetTextFieldData AA((DtSpinButtonWidget spin));
static void	SetMaximumLabelSize AA((DtSpinButtonPart *spin_p));
static void	SetLabelData AA((DtSpinButtonWidget spin));
static void	timer_dispatch AA((XtPointer client_data, XtIntervalId *id));
static void	TextFieldActivate AA((DtSpinButtonPart *spin_p));
static Boolean	SendCallback AA((DtSpinButtonWidget spin, XEvent *event,
				 Boolean value_changed, int position,
				 float current, Boolean crossed));
static void	FinishUpDown AA((DtSpinButtonWidget spin, 
				 XtPointer arrow_call_data, int new_position,
				 float new_current, Boolean crossed));
static void	up_cb AA((Widget w, XtPointer client_data, 
			  XtPointer call_data));
static void	down_cb AA((Widget w, XtPointer client_data, 
			    XtPointer call_data));
static void	disarm_cb AA((Widget w, XtPointer client_data, 
			      XtPointer call_data));
static void	grab_leave_cb AA((Widget w, XtPointer client_data, 
				  Event *event, Boolean *dispatch));
static void	text_losing_focus_cb AA((Widget w, XtPointer client_data,
					 XtPointer call_data));
static void	text_activate_cb AA((Widget w, XtPointer client_data,
				     XtPointer call_data));
static void	text_focus_cb AA((Widget w, XtPointer client_data,
				  XtPointer call_data));
static XmImportOperator _XmSetSyntheticResForChild AA((Widget widget,
						       int offset, 
						       XtArgVal * value));

static XmString InitLabel = NULL;
static XtTranslations child_trans;
XtTranslations spin_trans;

#define SPIN_SHADOW(w)	    w->manager.shadow_thickness
#define SPIN_MARGIN_W(w)    w->spin_button.margin_width
#define SPIN_MARGIN_H(w)    w->spin_button.margin_height
#define MAXINT 2147483647  /* Taken from TextF.c */

/* Only need one timer for all widget instances */
static XtIntervalId timer;

static char SpinButtonTranslationTable[] = "\
	<FocusIn>:		SpinButtonFocusIn() \n\
	<FocusOut>:		SpinButtonFocusOut() \n\
        <Key>osfUp:		SpinButtonUp() \n\
        <Key>osfDown:		SpinButtonDown() \n\
        <Key>osfRight:		SpinButtonRight() \n\
        <Key>osfLeft:		SpinButtonLeft() \n\
        <Key>osfBeginLine:	SpinButtonBeginLine() \n\
        <Key>osfEndLine:	SpinButtonEndLine() \n\
";

static char SpinButtonChildTranslationTable[] = "\
        <Key>osfUp:		SpinButtonUp(child) \n\
        <Key>osfDown:		SpinButtonDown(child) \n\
        <Key>osfRight:		SpinButtonRight(child) \n\
        <Key>osfLeft:		SpinButtonLeft(child) \n\
        <Key>osfBeginLine:	SpinButtonBeginLine(child) \n\
        <Key>osfEndLine:	SpinButtonEndLine(child) \n\
";

static XtActionsRec SpinButtonActionTable[] = {
       {"SpinButtonFocusIn",	(XtActionProc)_SpinButtonFocusIn},
       {"SpinButtonFocusOut",	(XtActionProc)_SpinButtonFocusOut},
       {"SpinButtonUp",		(XtActionProc)_SpinButtonUp},
       {"SpinButtonDown",	(XtActionProc)_SpinButtonDown},
       {"SpinButtonRight",	(XtActionProc)_SpinButtonRight},
       {"SpinButtonLeft",	(XtActionProc)_SpinButtonLeft},
       {"SpinButtonBeginLine",	(XtActionProc)_SpinButtonBeginLine},
       {"SpinButtonEndLine",	(XtActionProc)_SpinButtonEndLine},
};

/* DtSpinButtonWidget resources */
#define offset(field) XtOffset(DtSpinButtonWidget, field)
static XtResource resources[] = {
    {XmNshadowThickness, XmCShadowThickness, XmRHorizontalDimension, 
	 sizeof(Dimension), offset(manager.shadow_thickness),
	 XmRImmediate, (XtPointer)TEXT_FIELD_SHADOW},

    /* Common resources */
    {XmNactivateCallback, XmCCallback, XmRCallback, sizeof(XtCallbackList),
	 offset(spin_button.activate_callback), XmRCallback, 
	 (XtPointer)NULL},
    {XmNalignment, XmCAlignment, XmRAlignment, sizeof(unsigned char),
	 offset(spin_button.alignment), XmRImmediate, 
	 (XtPointer)XmALIGNMENT_BEGINNING},
    {XmNarrowLayout, XmCArrowLayout, XmRArrowLayout, sizeof(unsigned char),
	 offset(spin_button.arrow_layout), XmRImmediate, 
	 (XtPointer)XmARROWS_BEGINNING},
    {XmNarrowSensitivity, XmCArrowSensitivity, XmRArrowSensitivity,
	 sizeof(unsigned char), offset(spin_button.arrow_sensitivity),
	 XmRImmediate, (XtPointer)XmARROWS_SENSITIVE},
    {XmNchildType, XmCChildType, XmRChildType, sizeof(unsigned char),
	 offset(spin_button.child_type), XmRImmediate, (XtPointer)XmSTRING},
    {XmNcolumns, XmCColumns, XmRShort, sizeof(short),
	 offset(spin_button.text_columns), XmRImmediate, (XtPointer)20},
    {XmNdecimalPoints, XmCDecimalPoints, XmRInt, sizeof(unsigned int),
	 offset(spin_button.decimal_points), XmRImmediate, (XtPointer)0},
    {XmNeditable, XmCEditable, XmRBoolean, sizeof(Boolean),
	 offset(spin_button.editable), XmRImmediate, (XtPointer)TRUE},
    {XmNfocusCallback, XmCCallback, XmRCallback, sizeof(XtCallbackList),
	 offset(spin_button.focus_callback), XmRCallback, 
	 (XtPointer)NULL},
    {XmNincrement, XmCIncrement, XmRInt, sizeof(int),
	 offset(spin_button.numeric_increment), XmRImmediate, (XtPointer)1},
    {XmNinitialDelay, XmCInitialDelay, XmRInt, sizeof(unsigned int),
	 offset(spin_button.initial_delay), XmRImmediate, (XtPointer)250},
    {XmNitemCount, XmCItemCount, XmRInt, sizeof(unsigned int),
	 offset(spin_button.item_count), XmRImmediate, (XtPointer)0},
    {XmNitems, XmCItems, XmRXmStringTable, sizeof(XmStringTable),
	 offset(spin_button.items), XmRImmediate, (XtPointer)NULL},
    {XmNlosingFocusCallback, XmCCallback, XmRCallback, sizeof(XtCallbackList),
	 offset(spin_button.losing_focus_callback), XmRCallback, 
	 (XtPointer)NULL},
    {XmNmarginHeight, XmCMarginHeight, XmRVerticalDimension, sizeof(Dimension),
	 offset(spin_button.margin_height), XmRImmediate, (XtPointer)MARGIN},
    {XmNmarginWidth, XmCMarginWidth, XmRHorizontalDimension, sizeof(Dimension),
	 offset(spin_button.margin_width), XmRImmediate, (XtPointer)MARGIN},
    {XmNmaximum, XmCMaximum, XmRInt, sizeof(int), 
	 offset(spin_button.maximum), XmRImmediate, (XtPointer)1},
    {XmNmaxLength, XmCMaxLength, XmRInt, sizeof(int),
	 offset(spin_button.text_max_length), XmRImmediate, (XtPointer)MAXINT},
    {XmNminimum, XmCMinimum, XmRInt, sizeof(int), 
	 offset(spin_button.minimum), XmRImmediate, (XtPointer)0},
    {XmNmodifyVerifyCallback, XmCCallback, XmRCallback, 
	 sizeof(XtCallbackList), offset(spin_button.modify_verify_callback),
	 XmRCallback, (XtPointer)NULL},
    {XmNposition, XmCPosition, XmRInt, sizeof(unsigned int),
	 offset(spin_button.position), XmRImmediate, (XtPointer)0},
    {XmNrecomputeSize, XmCRecomputeSize, XmRBoolean, sizeof(Boolean),
	 offset(spin_button.recompute_size), XmRImmediate, (XtPointer)TRUE},
    {XmNrepeatDelay, XmCRepeatDelay, XmRInt, sizeof(unsigned int),
	 offset(spin_button.repeat_delay), XmRImmediate, (XtPointer)200},
    {XmNtextField, XmCTextField, XmRWidget, sizeof(Widget),
	 offset(spin_button.text), XmRImmediate, (XtPointer)NULL},
    {XmNvalueChangedCallback, XmCCallback, XmRCallback, 
	 sizeof(XtCallbackList), offset(spin_button.value_changed_callback),
	 XmRCallback, (XtPointer)NULL},
    {XmNwrap, XmCWrap, XmRBoolean, sizeof(Boolean),
	 offset(spin_button.wrap), XmRImmediate, (XtPointer)TRUE},
};

/* Synthetic resources.  Only used for Motif API arrowSize right now */
static XmSyntheticResource syn_resources[] = {
    {XmNarrowSize, sizeof(Dimension), offset(spin_button.arrow_size), 
	 _DtSpinButtonGetArrowSize, _XmSetSyntheticResForChild},
};
#undef offset

/* Need Class Extension for widget navigation */
static XmBaseClassExtRec baseClassExtRec = {
    NULL,
    NULLQUARK,
    XmBaseClassExtVersion,
    sizeof(XmBaseClassExtRec),
    (XtInitProc)NULL,			/* InitializePrehook	*/
    (XtSetValuesFunc)NULL,		/* SetValuesPrehook	*/
    (XtInitProc)NULL,			/* InitializePosthook	*/
    (XtSetValuesFunc)NULL,		/* SetValuesPosthook	*/
    NULL,				/* secondaryObjectClass	*/
    (XtInitProc)NULL,			/* secondaryCreate	*/
    (XmGetSecResDataFunc)NULL, 		/* getSecRes data	*/
    { 0 },      			/* fastSubclass flags	*/
    (XtArgsProc)NULL,			/* getValuesPrehook	*/
    (XtArgsProc)NULL,			/* getValuesPosthook	*/
    (XtWidgetClassProc)NULL,            /* classPartInitPrehook */
    (XtWidgetClassProc)NULL,            /* classPartInitPosthook*/
    NULL,                               /* ext_resources        */
    NULL,                               /* compiled_ext_resources*/
    0,                                  /* num_ext_resources    */
    FALSE,                              /* use_sub_resources    */
    WidgetNavigable,                    /* widgetNavigable      */
    (XmFocusChangeProc)NULL,            /* focusChange          */
    (XmWrapperData)NULL			/* wrapperData 		*/
};

/*
 * Define Class Record.
 */
DtSpinButtonClassRec dtSpinButtonClassRec =
{
    {		/* core_class fields      */
    (WidgetClass)&(xmManagerClassRec),		/* superclass         */    
    (String)"DtSpinButton",			/* class_name         */    
    (Cardinal)sizeof(DtSpinButtonRec),		/* widget_size        */    
    (XtProc)ClassInitialize,			/* class_initialize   */    
    (XtWidgetClassProc)NULL,			/* class_part_init    */    
    (XtEnum)FALSE,				/* class_inited       */    
    (XtInitProc)Initialize,			/* initialize         */    
    (XtArgsProc)NULL,				/* initialize_hook    */    
    (XtRealizeProc)XtInheritRealize,		/* realize            */    
    (XtActionList)SpinButtonActionTable,	/* actions	       */    
    (Cardinal)XtNumber(SpinButtonActionTable),	/* num_actions        */    
    (XtResourceList)resources,			/* resources          */    
    (Cardinal)XtNumber(resources),		/* num_resources      */    
    (XrmClass)NULLQUARK,			/* xrm_class          */    
    (Boolean)TRUE,				/* compress_motion    */    
    (XtEnum)XtExposeCompressMaximal,		/* compress_exposure  */    
    (Boolean)TRUE,				/* compress_enterleave*/    
    (Boolean)FALSE,				/* visible_interest   */    
    (XtWidgetProc)Destroy,			/* destroy            */    
    (XtWidgetProc)Resize,			/* resize             */    
    (XtExposeProc)Redisplay,			/* expose             */    
    (XtSetValuesFunc)SetValues,			/* set_values         */    
    (XtArgsFunc)NULL,				/* set values hook    */    
    (XtAlmostProc)XtInheritSetValuesAlmost,	/* set values almost  */    
    (XtArgsProc)NULL,				/* get values hook    */    
    (XtAcceptFocusProc)NULL,			/* accept_focus       */    
    (XtVersionType)XtVersion,			/* Version            */    
    (XtPointer)NULL,				/* PRIVATE cb list    */
    (String)XtInheritTranslations,		/* tm_table           */
    (XtGeometryHandler)XtInheritQueryGeometry,	/* query_geom         */
    (XtStringProc)XtInheritDisplayAccelerator,	/* display_accelerator*/
    (XtPointer)&baseClassExtRec			/* extension	      */
    },
    {		/* composite_class fields */
    (XtGeometryHandler)GeometryManager,		/* geometry_manager   */     
    (XtWidgetProc)XtInheritChangeManaged,	/* change_managed     */     
    (XtWidgetProc)XtInheritInsertChild,		/* insert_child	      */     
    (XtWidgetProc)XtInheritDeleteChild,		/* delete_child	      */     
    (XtPointer)NULL				/* extension	      */     
    },
    {		/* constraint_class fields */
    (XtResourceList)NULL,			/* resources	      */     
    (Cardinal)0,				/* num_resources      */     
    (Cardinal)0,				/* constraint_size    */     
    (XtInitProc)NULL,				/* initialize	      */     
    (XtWidgetProc)NULL,				/* destroy	      */     
    (XtSetValuesFunc)NULL,			/* set_values	      */     
    (XtPointer)NULL				/* extension          */     
    },
    {		/* manager class     */
    (String)XtInheritTranslations,		/* translations       */     
    (XmSyntheticResource*)syn_resources,	/* syn resources      */     
    (int)XtNumber(syn_resources),		/* num syn_resources  */     
    (XmSyntheticResource*)NULL,			/* get_cont_resources */     
    (int)0,					/* num_get_cont_resources */ 
    (XmParentProcessProc)XmInheritParentProcess,/* parent_process     */     
    (XtPointer)NULL				/* extension          */     
    },
    {		/* spin_button_class fields */     
    (Boolean)0,
    }
};

WidgetClass dtSpinButtonWidgetClass = (WidgetClass)&dtSpinButtonClassRec;


/* 
 * Must set up the record type for the class extensions to work.
 */
static void
ClassInitialize()
{
    baseClassExtRec.record_type = XmQmotif;
    child_trans = XtParseTranslationTable(SpinButtonChildTranslationTable);
    spin_trans = XtParseTranslationTable(SpinButtonTranslationTable);
}

/*
 * SpinButton initialization function.  This builds the widgets inside
 * our widget, to get the correct layout.  If the editable resource
 * is TRUE, we create a textField; if FALSE, we create a label.  If the
 * user changes this resource later, we will create the other widget
 * (textField or Label).  We don't want to carry backage from both
 * widgets if the user never changes the editable resource.
 */
static void
Initialize(request, new, given_args, num_args)
DtSpinButtonWidget request;
DtSpinButtonWidget new;
ArgList given_args;
Cardinal *num_args;
{
    DtSpinButtonPart *spin_p = (DtSpinButtonPart*)&(new->spin_button);
    char *widget_name;
    Arg args[20];
    int n;

    /* Overwrite the manager's focusIn and focusOut translations */
    XtOverrideTranslations((Widget)new, spin_trans);

    if (InitLabel == NULL)
	InitLabel = XmStringCreateSimple("SpinButton");
    widget_name = XtMalloc(strlen(XtName(new)) + 10);

    spin_p->text = (Widget)NULL;
    spin_p->label = (Widget)NULL;
    spin_p->old_width = 0;
    spin_p->old_height = 0;
    spin_p->init_cb = TRUE;
    spin_p->grabbed = FALSE;

    CheckResources(new);

    /*
     * Create the text or label depending on editable resource.
     */
    if (spin_p->editable) {
	sprintf(widget_name, "%s_TF", XtName(new));
	n = 0;
	XtSetArg(args[n], XmNcolumns, spin_p->text_columns); n++;
	XtSetArg(args[n], XmNmaxLength, spin_p->text_max_length); n++;
	XtSetArg(args[n], XmNmarginWidth, 2); n++;
	XtSetArg(args[n], XmNmarginHeight, 2); n++;
	spin_p->text = XtCreateManagedWidget(widget_name,
					     xmTextFieldWidgetClass,
					     (Widget)new, args, n);
	XtAddCallback(spin_p->text, XmNlosingFocusCallback, 
		      text_losing_focus_cb, (XtPointer)new);
	XtAddCallback(spin_p->text, XmNactivateCallback, 
		      text_activate_cb, (XtPointer)new);
	XtAddCallback(spin_p->text, XmNfocusCallback, 
		      text_focus_cb, (XtPointer)new);
    }
    else {
	sprintf(widget_name, "%s_Label", XtName(new));
	SPIN_SHADOW(new) = LABEL_SHADOW;
	n = 0;
	XtSetArg(args[n], XmNalignment, spin_p->alignment); n++;
	XtSetArg(args[n], XmNrecomputeSize, FALSE); n++;
	XtSetArg(args[n], XmNlabelString, InitLabel); n++;
	XtSetArg(args[n], XmNmarginLeft, LABEL_PADDING); n++;
	XtSetArg(args[n], XmNmarginRight, LABEL_PADDING); n++;
	XtSetArg(args[n], XmNmarginWidth, 0); n++;
	XtSetArg(args[n], XmNmarginHeight, 0); n++;
	spin_p->label = XtCreateManagedWidget(widget_name, xmLabelWidgetClass,
					      (Widget)new, args, n);
	XtOverrideTranslations((Widget)spin_p->label, child_trans);
    }

    /*
     * Create the 2 ArrowWidgets.
     */
    sprintf(widget_name, "%s_Up", XtName(new));
    n = 0;
    if (spin_p->arrow_layout == XmARROWS_SPLIT) {
	XtSetArg(args[n], XmNarrowDirection, XmARROW_RIGHT); n++;
    }
    XtSetArg(args[n], XmNhighlightThickness, 0); n++;
    XtSetArg(args[n], XmNshadowThickness, 0); n++;
    XtSetArg(args[n], XmNtraversalOn, FALSE); n++;
    XtSetArg(args[n], XmNforeground, new->core.background_pixel); n++;
    spin_p->up_arrow = XtCreateManagedWidget(widget_name, 
					     xmArrowButtonWidgetClass,
					     (Widget)new, args, n);
    XtOverrideTranslations((Widget)spin_p->up_arrow, child_trans);

    sprintf(widget_name, "%s_Down", XtName(new));
    if (spin_p->arrow_layout == XmARROWS_SPLIT) {
	XtSetArg(args[n], XmNarrowDirection, XmARROW_LEFT); n++;
    }
    else {
	XtSetArg(args[n], XmNarrowDirection, XmARROW_DOWN); n++;
    }
    spin_p->down_arrow = XtCreateManagedWidget(widget_name, 
					       xmArrowButtonWidgetClass,
					       (Widget)new, args, n);
    XtOverrideTranslations((Widget)spin_p->down_arrow, child_trans);

    /* Set sensitivity of arrows (up arrow is right arrow) */
    if ((spin_p->arrow_sensitivity == XmARROWS_INSENSITIVE) ||
	(spin_p->arrow_sensitivity == XmARROWS_LEFT_SENSITIVE)) 
	XtSetSensitive(spin_p->up_arrow, FALSE);
    if ((spin_p->arrow_sensitivity == XmARROWS_INSENSITIVE) ||
	(spin_p->arrow_sensitivity == XmARROWS_RIGHT_SENSITIVE)) 
	XtSetSensitive(spin_p->down_arrow, FALSE);

    /* 
     * Arm causes the value to change and the timer to start.
     * Disarm (leaveNotify from grab) causes the timer to stop.
     */
    XtAddCallback(spin_p->up_arrow, XmNarmCallback, up_cb, (XtPointer)new);
    XtAddCallback(spin_p->up_arrow, XmNdisarmCallback, disarm_cb, 
		  (XtPointer)new);
    XtAddEventHandler(spin_p->up_arrow, LeaveWindowMask, FALSE, grab_leave_cb, 
		      (XtPointer)new);
    XtAddCallback(spin_p->down_arrow, XmNarmCallback, down_cb, (XtPointer)new);
    XtAddCallback(spin_p->down_arrow, XmNdisarmCallback, disarm_cb, 
		  (XtPointer)new);
    XtAddEventHandler(spin_p->down_arrow, LeaveWindowMask, FALSE, 
		      grab_leave_cb, (XtPointer)new);

    /* Initialize everything based on what the resource values are */
    StoreResourceInfo(spin_p, NULL, TRUE);

    /*
     * Set initial value in text or label if items was specified
     */
    if (spin_p->editable == FALSE) {
	SetLabelData(new);
	SetMaximumLabelSize(spin_p);
    }
    else
	SetTextFieldData(new);
    SetSpinButtonSize(new);
    LayoutChildren(new);
    XtFree(widget_name);
}


/*
 * Allow the manager to gain focus if not editable.  If editable (using
 * text-field), then let the toolkit give focus to the text-field.
 */
static XmNavigability
WidgetNavigable(spin)
DtSpinButtonWidget spin;
{   
    XmNavigationType nav_type = ((XmManagerWidget)spin)->manager.navigation_type;

    if (spin->core.sensitive &&  spin->core.ancestor_sensitive &&
	((XmManagerWidget)spin)->manager.traversal_on) {
	if ((nav_type == XmSTICKY_TAB_GROUP) ||
	    (nav_type == XmEXCLUSIVE_TAB_GROUP) ||
	    ((nav_type == XmTAB_GROUP) &&
	     !_XmShellIsExclusive((Widget)spin))) {
	    if (spin->spin_button.editable)
		return(XmDESCENDANTS_TAB_NAVIGABLE);
	    else
		return(XmTAB_NAVIGABLE);
	}
	return(XmDESCENDANTS_NAVIGABLE);
    }
    return(XmNOT_NAVIGABLE);
}

/* 
 * The spin-button gets focus.
 */
static void 
_SpinButtonFocusIn(spin, event, params, num_params)
DtSpinButtonWidget spin;
XEvent *event;
char **params;
Cardinal *num_params;
{
    DrawHighlight(spin, FALSE);
}

/* 
 * The spin-button loses focus.
 */
static void 
_SpinButtonFocusOut(spin, event, params, num_params)
DtSpinButtonWidget spin;
XEvent *event;
char **params;
Cardinal *num_params;
{
    DrawHighlight(spin, TRUE);
}

/*
 * This function gets called whenever we draw or clear the shadow (to
 * redraw highlight during resize, etc), as well as during focus_in
 * and focus_out events.
 */
static void
DrawHighlight(spin, clear)
DtSpinButtonWidget spin;
Boolean clear;
{
    XRectangle rect[4] ;

    if (XtIsRealized(spin)) {
	if (clear) {
	    rect[0].x = rect[1].x = rect[2].x = 0;
	    rect[3].x = spin->spin_button.old_width - SPIN_MARGIN_W(spin);
	    rect[0].y = rect[2].y = rect[3].y = 0 ;
	    rect[1].y = spin->spin_button.old_height - SPIN_MARGIN_H(spin);
	    rect[0].width = rect[1].width = spin->spin_button.old_width;
	    rect[2].width = rect[3].width = SPIN_MARGIN_W(spin);
	    rect[0].height = rect[1].height = SPIN_MARGIN_H(spin);
	    rect[2].height = rect[3].height = spin->spin_button.old_height;
	    XFillRectangles(XtDisplayOfObject((Widget)spin),
			    XtWindowOfObject((Widget)spin), 
			    spin->manager.background_GC, rect, 4);
	}
	else if (XmGetFocusWidget((Widget)spin) == (Widget)spin) {
	    rect[0].x = rect[1].x = rect[2].x = 0;
	    rect[3].x = XtWidth(spin) - SPIN_MARGIN_W(spin);
	    rect[0].y = rect[2].y = rect[3].y = 0 ;
	    rect[1].y = XtHeight(spin) - SPIN_MARGIN_H(spin);
	    rect[0].width = rect[1].width = XtWidth(spin);
	    rect[2].width = rect[3].width = SPIN_MARGIN_W(spin);
	    rect[0].height = rect[1].height = SPIN_MARGIN_H(spin);
	    rect[2].height = rect[3].height = XtHeight(spin);
	    XFillRectangles(XtDisplayOfObject((Widget)spin),
			    XtWindowOfObject((Widget)spin), 
			    spin->manager.highlight_GC, rect, 4);
	}
    }
}


/*
 * osfUp virtual key hit.  Simulate hitting the up arrow.
 */
static void 
_SpinButtonUp(spin, event, params, num_params)
DtSpinButtonWidget spin;
XEvent *event;
char **params;
Cardinal *num_params;
{
    if (*num_params != 0) /* params means label or arrows */
	spin = (DtSpinButtonWidget)XtParent(spin);

    if (spin->spin_button.arrow_layout != XmARROWS_SPLIT) {
	up_cb((Widget)spin->spin_button.up_arrow, (XtPointer)spin, NULL);
	disarm_cb((Widget)spin->spin_button.up_arrow, (XtPointer)spin, NULL);
    }
}

/*
 * osfDown virtual key hit.  Simulate hitting the down arrow.
 */
static void 
_SpinButtonDown(spin, event, params, num_params)
DtSpinButtonWidget spin;
XEvent *event;
char **params;
Cardinal *num_params;
{
    if (*num_params != 0) /* params means label or arrows */
	spin = (DtSpinButtonWidget)XtParent(spin);

    if (spin->spin_button.arrow_layout != XmARROWS_SPLIT) {
	down_cb((Widget)spin->spin_button.down_arrow, (XtPointer)spin, NULL);
	disarm_cb((Widget)spin->spin_button.down_arrow, (XtPointer)spin, NULL);
    }
}

/*
 * osfRight virtual key hit.  Simulate hitting the up arrow.
 */
static void 
_SpinButtonRight(spin, event, params, num_params)
DtSpinButtonWidget spin;
XEvent *event;
char **params;
Cardinal *num_params;
{
    if (*num_params != 0) /* params means label or arrows */
	spin = (DtSpinButtonWidget)XtParent(spin);

    if (spin->spin_button.arrow_layout == XmARROWS_SPLIT) {
	up_cb((Widget)spin->spin_button.up_arrow, (XtPointer)spin, NULL);
	disarm_cb((Widget)spin->spin_button.up_arrow, (XtPointer)spin, NULL);
    }
}

/*
 * osfLeft virtual key hit.  Simulate hitting the down arrow.
 */
static void 
_SpinButtonLeft(spin, event, params, num_params)
DtSpinButtonWidget spin;
XEvent *event;
char **params;
Cardinal *num_params;
{
    if (*num_params != 0) /* params means label or arrows */
	spin = (DtSpinButtonWidget)XtParent(spin);

    if (spin->spin_button.arrow_layout == XmARROWS_SPLIT) {
	down_cb((Widget)spin->spin_button.down_arrow, (XtPointer)spin, NULL);
	disarm_cb((Widget)spin->spin_button.down_arrow, (XtPointer)spin, NULL);
    }
}

/*
 * osfBeginLine virtual key hit.  Go to first item.
 */
static void 
_SpinButtonBeginLine(spin, event, params, num_params)
DtSpinButtonWidget spin;
XEvent *event;
char **params;
Cardinal *num_params;
{
    DtSpinButtonPart *spin_p;
    int new_position;
    float new_current = 0;
    
    if (*num_params != 0) /* params means label or arrows */
	spin = (DtSpinButtonWidget)XtParent(spin);
    spin_p = (DtSpinButtonPart*)&(spin->spin_button);

    if (spin_p->child_type == XmNUMERIC) {
	new_position = spin_p->minimum;
	new_current = spin_p->min;
    }
    else {
	new_position = 0;
    }
    if (SendCallback(spin, event, FALSE, new_position,
		     new_current, FALSE) == TRUE) {
	/* User said yes, so set widget values */
	spin_p->position = new_position;
	spin_p->current = new_current;
	if (spin_p->editable)
	    SetTextFieldData(spin);
	else
	    SetLabelData(spin);
	
	/* send value_changed callback */
	(void)SendCallback(spin, event, TRUE, spin_p->position,
			   spin_p->current, FALSE);
    }
}

/*
 * osfEndLine virtual key hit.  Go to last item.
 */
static void 
_SpinButtonEndLine(spin, event, params, num_params)
DtSpinButtonWidget spin;
XEvent *event;
char **params;
Cardinal *num_params;
{
    DtSpinButtonPart *spin_p;
    int new_position;
    float new_current = 0;
    
    if (*num_params != 0) /* params means label or arrows */
	spin = (DtSpinButtonWidget)XtParent(spin);
    spin_p = (DtSpinButtonPart*)&(spin->spin_button);

    if (spin_p->child_type == XmNUMERIC) {
	new_position = spin_p->maximum;
	new_current = spin_p->max;
    }
    else {
	new_position = spin_p->item_count - 1;
    }
    if (SendCallback(spin, event, FALSE, new_position,
		     new_current, FALSE) == TRUE) {
	/* User said yes, so set widget values */
	spin_p->position = new_position;
	spin_p->current = new_current;
	if (spin_p->editable)
	    SetTextFieldData(spin);
	else
	    SetLabelData(spin);
	
	/* send value_changed callback */
	(void)SendCallback(spin, event, TRUE, spin_p->position,
			   spin_p->current, FALSE);
    }
}

/*
 * This function goes through most of the resources and makes sure 
 * they have legal values.
 */
static void
CheckResources(spin)
DtSpinButtonWidget spin;
{
    DtSpinButtonPart *spin_p = (DtSpinButtonPart*)&(spin->spin_button);

    if ((spin_p->arrow_sensitivity != XmARROWS_SENSITIVE) &&
	(spin_p->arrow_sensitivity != XmARROWS_LEFT_SENSITIVE) &&
	(spin_p->arrow_sensitivity != XmARROWS_RIGHT_SENSITIVE) &&
	(spin_p->arrow_sensitivity != XmARROWS_INSENSITIVE)) {
	XtWarning(SPIN_ARROW_SENSITIVE);
	spin_p->arrow_sensitivity = XmARROWS_SENSITIVE;
    }
    if ((spin_p->alignment != XmALIGNMENT_CENTER) && 
	(spin_p->alignment != XmALIGNMENT_BEGINNING) &&
	(spin_p->alignment != XmALIGNMENT_END)) {
	XtWarning(SPIN_ALIGNMENT);
	spin_p->alignment = XmALIGNMENT_CENTER;
    }
    if (spin_p->initial_delay <= 0) {
	XtWarning(SPIN_INIT_DELAY);
	spin_p->initial_delay = 250;
    }
    if ((spin_p->arrow_layout != XmARROWS_FLAT_BEGINNING) && 
	(spin_p->arrow_layout != XmARROWS_FLAT_END) &&
	(spin_p->arrow_layout != XmARROWS_SPLIT) &&
	(spin_p->arrow_layout != XmARROWS_BEGINNING) &&
	(spin_p->arrow_layout != XmARROWS_END)) {
	XtWarning(SPIN_ARROW_LAYOUT);
	spin_p->arrow_layout = XmARROWS_BEGINNING;
    }
    if (spin_p->repeat_delay <= 0) {
	XtWarning(SPIN_REPEAT_DELAY);
	spin_p->repeat_delay = 200;
    }
    if (spin_p->item_count < 0) {
	XtWarning(SPIN_ITEM_COUNT);
	spin_p->item_count = 0;
    }
    if ((spin_p->child_type == XmSTRING) &&
	((spin_p->position < 0) ||
	 ((spin_p->position >= spin_p->item_count) &&
	  (spin_p->item_count > 0)))) {
	XtWarning(SPIN_POSITION_STRING);
	spin_p->position = 0;
    }
    if ((spin_p->decimal_points < 0) ||
	(spin_p->decimal_points > MAX_FLOAT_DECIMALS)) {
	XtWarning(SPIN_DECIMAL_POINTS);
	spin_p->decimal_points = 0;
    }
    if (spin_p->minimum > spin_p->maximum) {
	XtWarning(SPIN_MIN_MAX);
	spin_p->minimum = spin_p->maximum;
    }
    if ((spin_p->child_type == XmNUMERIC) &&
	((spin_p->position < spin_p->minimum) ||
	 (spin_p->position > spin_p->maximum) ||
	 ((spin_p->position % spin_p->numeric_increment) != 0))) {
	XtWarning(SPIN_POSITION_NUMERIC);
	spin_p->position = spin_p->minimum;
    }
}


/*
 * Destroy procedure called by the toolkit.  Remove all callbacks and
 * event-handlers. 
 */
static void 
Destroy(spin)
DtSpinButtonWidget spin;
{
    DtSpinButtonPart *spin_p = (DtSpinButtonPart*)&(spin->spin_button);
    int i;

    if ((int)timer)
	XtRemoveTimeOut(timer);

    if (spin_p->items) {
	for (i = 0; i < spin_p->item_count; i++) {
	    XmStringFree(spin_p->items[i]);
	    }
	XtFree((char*)spin_p->items);
    }

    if (spin_p->text) {
	XtRemoveCallback(spin_p->text, XmNlosingFocusCallback, 
			 text_losing_focus_cb, (XtPointer)spin);
	XtRemoveCallback(spin_p->text, XmNactivateCallback, 
			 text_activate_cb, (XtPointer)spin);
	XtRemoveCallback(spin_p->text, XmNfocusCallback, 
			 text_focus_cb, (XtPointer)spin);
    }
	
    XtRemoveCallback(spin_p->up_arrow, XmNarmCallback, up_cb, (XtPointer)spin);
    XtRemoveCallback(spin_p->up_arrow, XmNdisarmCallback, disarm_cb, 
		     (XtPointer)spin);
    XtRemoveEventHandler(spin_p->up_arrow, LeaveWindowMask, FALSE, 
			 grab_leave_cb, (XtPointer)spin);
    
    XtRemoveCallback(spin_p->down_arrow, XmNarmCallback, down_cb, 
		     (XtPointer)spin);
    XtRemoveCallback(spin_p->down_arrow, XmNdisarmCallback, disarm_cb, 
		     (XtPointer)spin);
    XtRemoveEventHandler(spin_p->down_arrow, LeaveWindowMask, FALSE, 
			 grab_leave_cb, (XtPointer)spin);
}


/*
 * Resize function called by toolkit.  The size of our spin-box
 * has already been changed.  That is why we must store 
 * old_width and old_height.
 */
static void
Resize(spin)
DtSpinButtonWidget spin;
{
    DtSpinButtonPart *spin_p = (DtSpinButtonPart*)&(spin->spin_button);

    ClearShadow(spin, TRUE);
    LayoutChildren(spin);
    DrawShadow(spin);
    spin_p->old_width = spin->core.width;
    spin_p->old_height = spin->core.height;
}


/*
 * Redisplay function called by toolkit. The widget didn't change size, 
 * so just redisplay the shadow.
 */
static void
Redisplay(w, event, region)
DtSpinButtonWidget w;
XEvent *event;
Region region;
{
    DrawShadow(w, SPIN_MARGIN_W(w), SPIN_MARGIN_H(w), SPIN_SHADOW(w));
}


/*
 * GeometryManager function called by toolkit when a child resizes/moves.
 * We are not allowing any changes but width/height of the text-field.
 * this is because the user can retrieve the text-field and make changes
 * that we want to honor.  If they mess around with the label or arrow,
 * then we won't honor the request.
 * If the text-field requests a change, then make the change, and allow
 * our SetSpinButtonSize() and LayoutChildren() figure out what size will
 * be allowed.
 * Returning GeometryDone was suppose to tell the toolkit
 * that we resized the child ourselves, but the text-field had trouble
 * with this (its' geometry_manager wasn't called or working right?), so
 * we return GeometryYes.
 */
static XtGeometryResult
GeometryManager(w, request, reply)
Widget w; /* child */
XtWidgetGeometry *request;
XtWidgetGeometry *reply;
{
    DtSpinButtonWidget spin = (DtSpinButtonWidget)(w->core.parent);

    /* Ignore everything but text-field */
    if (w != spin->spin_button.text)
	return(XtGeometryNo);

    /* Only allow width/height changes */
    if (!(request->request_mode & (CWWidth | CWHeight)))
	return(XtGeometryNo);
    
    /* Set the text-field to the requested size */
    if (request->request_mode & CWWidth)
	w->core.width = request->width;
    if (request->request_mode & CWHeight)
	w->core.height = request->height;
    XtResizeWidget(w, w->core.width, w->core.height, w->core.border_width);
    
    ClearShadow(spin, TRUE);
    if (spin->spin_button.recompute_size)
	SetSpinButtonSize(spin);
    LayoutChildren(spin);
    DrawShadow(spin);
    return(XtGeometryYes);
}

/* 
 * This function sets the size of the spin_button widget based on the
 * current size of the children.  Don't worry if it doesn't work, the
 * children will be squeezed in later.
 */
static void
SetSpinButtonSize(spin)
DtSpinButtonWidget spin;
{
    DtSpinButtonPart *spin_p = (DtSpinButtonPart*)&(spin->spin_button);
    Widget text_holder = spin_p->editable ? spin_p->text : spin_p->label;
    Dimension shadow = SPIN_SHADOW(spin) * 2;
    short arrow_width, arrow_height;
    short height = text_holder->core.height;

    /* 
     * Find out how big the arrow can be (needed to get 
     * available_width for text_holder).
     */
    arrow_width = (Dimension)(text_holder->core.height * ARROW_MULT);
    arrow_width = (arrow_width < ARROW_MIN) ? ARROW_MIN : arrow_width;

    /* Get height based on arrow width */
    arrow_height = arrow_width;
    if ((spin_p->arrow_layout == XmARROWS_BEGINNING) ||
	(spin_p->arrow_layout == XmARROWS_END))
	arrow_height += arrow_height;

    /* Make height bigger of 2 - arrows vs text_holder */
    if (arrow_height > (short)text_holder->core.height)
	height = arrow_height;

    /* If not stacked add extra width for arrows */
    if ((spin_p->arrow_layout != XmARROWS_BEGINNING) &&
	(spin_p->arrow_layout != XmARROWS_END)) {
	arrow_width += arrow_width;
    }

    (void)XtMakeResizeRequest((Widget)spin, arrow_width +
			      text_holder->core.width + shadow +
			      (SPIN_MARGIN_W(spin) * 2), 
			      height + shadow + (SPIN_MARGIN_H(spin) * 2), 
			      NULL, NULL);
    spin_p->old_width = spin->core.width;
    spin_p->old_height = spin->core.height;
}

/*
 * This function makes the text_holder (label or text-field) smaller
 * if the spin_button couldn't grow to the needed full size.  It will
 * also make the text_holder grow if there is space.  The textfield will
 * grow with the spin_button, but the label will only grow to its' 
 * maximum size.  The label will also shrink down to nothing, but the
 * text-field will always keep its' core height.
 */
static void
ForceChildSizes(spin)
DtSpinButtonWidget spin;
{
    DtSpinButtonPart *spin_p = (DtSpinButtonPart*)&(spin->spin_button);
    short available_height, available_width;
    short arrow_width;

    /* Calculate available height for children */
    if ((available_height = spin->core.height - (SPIN_SHADOW(spin) * 2) - 
	 (SPIN_MARGIN_H(spin) * 2)) <= 0)
	available_height = 1;

    /* Get initial available width for children */
    available_width = (spin->core.width - (SPIN_SHADOW(spin) * 2) - 
		       (SPIN_MARGIN_W(spin) * 2));

    /* label only grows to maximum width needed */
    if ((spin_p->editable == FALSE) && 
	(available_height > (short)spin_p->label_max_height))
	available_height = spin_p->label_max_height;
    else if (spin_p->editable) 
	available_height = spin_p->text->core.height;
    
    /* 
     * Find out how big the arrow can be (needed to get 
     * available_width for text_holder).
     */
    arrow_width = (Dimension)(available_height * ARROW_MULT);
    arrow_width = (arrow_width < ARROW_MIN) ? ARROW_MIN : arrow_width;
	
    /* Make sure width isn't too small or too big */
    if ((available_width -= arrow_width) <= 0)
	available_width = 1;

    /* If not stacked subtract extra arrow */
    if ((spin_p->arrow_layout != XmARROWS_BEGINNING) &&
	(spin_p->arrow_layout != XmARROWS_END)) {
	available_width -= arrow_width;
    }
    
    if (spin_p->editable == FALSE) {  /** label **/
	if (available_width > (short)spin_p->label_max_length)
	    available_width = spin_p->label_max_length;

	if ((available_width != spin_p->label->core.width) ||
	    (available_height != spin_p->label->core.height))
	    XtResizeWidget(spin_p->label, available_width, available_height,
			   spin_p->label->core.border_width);
    }
    else if (spin_p->text->core.width != available_width)  /** TextField **/
	XtResizeWidget(spin_p->text, available_width,
		       spin_p->text->core.height,
		       spin_p->text->core.border_width);
    if ((arrow_width != spin_p->up_arrow->core.width) ||
	(spin_p->up_arrow->core.height != arrow_width)) {
	available_height = (available_height < ARROW_MIN) ? ARROW_MIN : 
                            available_height;
	XtResizeWidget(spin_p->up_arrow, arrow_width, arrow_width,
		       spin_p->up_arrow->core.border_width);
    }
    if ((arrow_width != spin_p->down_arrow->core.width) ||
	(spin_p->down_arrow->core.height != arrow_width)) {
	available_height = (available_height < ARROW_MIN) ? ARROW_MIN : 
                            available_height;
	XtResizeWidget(spin_p->down_arrow, arrow_width, arrow_width,
		       spin_p->down_arrow->core.border_width);
    }
}

/*
 * This function positions the children within the spin_button widget.
 * It calls ForceChildSizes() to make sure the children fit within the
 * spin_button widget, but it will not try to resize the spin_button widget.
 */
static void
LayoutChildren(spin)
DtSpinButtonWidget spin;
{
    DtSpinButtonPart *spin_p = (DtSpinButtonPart*)&(spin->spin_button);
    Widget text_holder = spin_p->editable ? spin_p->text : spin_p->label;
    Position start_x = SPIN_SHADOW(spin) + SPIN_MARGIN_W(spin);
    Position start_y = SPIN_SHADOW(spin) + SPIN_MARGIN_H(spin);
    short available_height = spin->core.height - (start_y * 2);
    Position y, arrow_y;
    Dimension arrow_height;
    
    ForceChildSizes(spin);

    /* Center text_holder within spin_button */
    y = available_height - text_holder->core.height;
    y = ((y < 0) ? 0 : y)/2 + start_y;

    arrow_height = spin_p->up_arrow->core.height;
    if ((spin_p->arrow_layout == XmARROWS_BEGINNING) ||
	(spin_p->arrow_layout == XmARROWS_END))
	arrow_height += arrow_height;
    arrow_y = available_height - arrow_height;
    arrow_y = ((arrow_y < 0) ? 0 : arrow_y)/2 + start_y;

    switch (spin_p->arrow_layout) {
    case XmARROWS_FLAT_BEGINNING:
	XtMoveWidget(spin_p->up_arrow, start_x, arrow_y);
	start_x += spin_p->up_arrow->core.width;
	XtMoveWidget(spin_p->down_arrow, start_x, arrow_y);
	start_x += spin_p->down_arrow->core.width;
	XtMoveWidget(text_holder, start_x, y);
	break;
    case XmARROWS_FLAT_END:
	XtMoveWidget(text_holder, start_x, y);
	/*  
	 * This start_x places arrow right after text_holder.  With
	 * labels we want the arrow at the end of the spin_button, so
	 * the user can use recompute_size more effectively.
	 * start_x += text_holder->core.width;
	 */
	start_x = (spin->core.width - start_x - 
		   spin_p->up_arrow->core.width -
		   spin_p->down_arrow->core.width);
	XtMoveWidget(spin_p->up_arrow, start_x, arrow_y);
	start_x += spin_p->up_arrow->core.width;
	XtMoveWidget(spin_p->down_arrow, start_x, arrow_y);
	break;
    case XmARROWS_SPLIT:
	XtMoveWidget(spin_p->down_arrow, start_x, arrow_y);
	start_x += spin_p->down_arrow->core.width;
	XtMoveWidget(text_holder, start_x, y);
	start_x += text_holder->core.width;
	XtMoveWidget(spin_p->up_arrow, start_x, arrow_y);
	break;
    case XmARROWS_BEGINNING:
	XtMoveWidget(spin_p->up_arrow, start_x, arrow_y);
	arrow_y += spin_p->up_arrow->core.height;
	XtMoveWidget(spin_p->down_arrow, start_x, arrow_y);
	start_x += spin_p->down_arrow->core.width;
	XtMoveWidget(text_holder, start_x, y);
	break;
    case XmARROWS_END:
	XtMoveWidget(text_holder, start_x, y);
	/*  
	 * This start_x places arrow right after text_holder.  With
	 * labels we want the arrow at the end of the spin_button, so
	 * the user can use recompute_size more effectively.
	 * start_x += text_holder->core.width;
	 */
	start_x = spin->core.width - start_x - spin_p->up_arrow->core.width;
	XtMoveWidget(spin_p->up_arrow, start_x, arrow_y);
	arrow_y += spin_p->up_arrow->core.width;
	XtMoveWidget(spin_p->down_arrow, start_x, arrow_y);
	break;
    }
}


/*
 * SetValues() routine for SpinButton widget. Most of the real work
 * is done in SetItems() and SetNumeric().  If items is set we store
 * our own XmStrings. 
 * This function was built with static spin-buttons in mind, meaning that
 * is-numeric or editable resources won't be changed constantly.  These
 * resources can be changed, but this function doesn't handle them in
 * a super-efficient manor.  for example, changing child-type will cause
 * the label to be resize even if it isn't managed.
 */
static Boolean
SetValues(current, request, new)
DtSpinButtonWidget current;
DtSpinButtonWidget request;
DtSpinButtonWidget new;
{
    DtSpinButtonPart *new_p = (DtSpinButtonPart*)&(new->spin_button);
    DtSpinButtonPart *cur_p = (DtSpinButtonPart*)&(current->spin_button);
    Boolean store_info;
    char *widget_name;
    Boolean label_size_changed = FALSE;
    Arg args[20];
    int n;

    CheckResources(new);

    if (new_p->text != cur_p->text) {
	XtWarning(SPIN_TEXT);
	new_p->text = cur_p->text;
    }

    /*
     * Editable resource changed.  If the widget (textField or Label)
     * doesn't exist, then create it.
     */
    if (new_p->editable != cur_p->editable) {
	if (new_p->editable) {
	    XtUnmanageChild(new_p->label);
	    if (new_p->text == NULL) {
		widget_name = XtMalloc(strlen(XtName(new)) + 10);
		sprintf(widget_name, "%s_TF", XtName(new));
		n = 0;
		XtSetArg(args[n], XmNcolumns, new_p->text_columns); n++;
		XtSetArg(args[n], XmNmaxLength, new_p->text_max_length); n++;
		XtSetArg(args[n], XmNmarginWidth, 2); n++;
		XtSetArg(args[n], XmNmarginHeight, 2); n++;
		new_p->text = XtCreateManagedWidget(widget_name,
						    xmTextFieldWidgetClass,
						    (Widget)new, args, n);
		XtAddCallback(new_p->text, XmNlosingFocusCallback, 
			      text_losing_focus_cb, (XtPointer)new);
		XtAddCallback(new_p->text, XmNactivateCallback, 
			      text_activate_cb, (XtPointer)new);
		XtAddCallback(new_p->text, XmNfocusCallback, 
			      text_focus_cb, (XtPointer)new);
		XtFree(widget_name);
	    }
	    else
		XtManageChild(new_p->text);
	}
	else {
	    XtUnmanageChild(new_p->text);
	    if (new_p->label == NULL) {
		widget_name = XtMalloc(strlen(XtName(new)) + 10);
		sprintf(widget_name, "%s_Label", XtName(new));
		n = 0;
		XtSetArg(args[n], XmNalignment, new_p->alignment); n++;
		XtSetArg(args[n], XmNrecomputeSize, FALSE); n++;
		XtSetArg(args[n], XmNlabelString, InitLabel); n++;
		XtSetArg(args[n], XmNmarginLeft, LABEL_PADDING); n++;
		XtSetArg(args[n], XmNmarginRight, LABEL_PADDING); n++;
		XtSetArg(args[n], XmNmarginWidth, 0); n++;
		XtSetArg(args[n], XmNmarginHeight, 0); n++;
		new_p->label = XtCreateManagedWidget(widget_name,
						     xmLabelWidgetClass,
						     (Widget)new, args, n);
		XtOverrideTranslations((Widget)new_p->label, child_trans);
		XtFree(widget_name);
	    }
	    else
		XtManageChild(new_p->label);
	}
	/* 
	 * Text-fields and labels have different shadows.  Only
	 * change if user didn't change the shadow resource.
	 */
	if (SPIN_SHADOW(new) == SPIN_SHADOW(current))
	    SPIN_SHADOW(new) = (new_p->editable) ? TEXT_FIELD_SHADOW :
		                                   LABEL_SHADOW;
    }

    /* Check arrow sensitivity (up arrow is right arrow)*/
    if (new_p->arrow_sensitivity != cur_p->arrow_sensitivity) {
	XtSetSensitive(new_p->up_arrow, 
		       ((new_p->arrow_sensitivity == XmARROWS_SENSITIVE) ||
			(new_p->arrow_sensitivity == XmARROWS_RIGHT_SENSITIVE)));
	XtSetSensitive(new_p->down_arrow, 
		       ((new_p->arrow_sensitivity == XmARROWS_SENSITIVE) ||
			(new_p->arrow_sensitivity == XmARROWS_LEFT_SENSITIVE)));
    }

    /*
     * Check arrow layout.  Only need to change arrows if going to or
     * from XmARROWS_SPLIT.  The LayoutChildren() routine actually
     * positions the arrows in the correct place.
     */
    if (new_p->arrow_layout != cur_p->arrow_layout) {
	if (new_p->arrow_layout == XmARROWS_SPLIT) {
	    XtSetArg(args[0], XmNarrowDirection, XmARROW_RIGHT);
	    XtSetValues(new_p->up_arrow, args, 1);
	    XtSetArg(args[0], XmNarrowDirection, XmARROW_LEFT);
	    XtSetValues(new_p->down_arrow, args, 1);
	}
	else if (cur_p->arrow_layout == XmARROWS_SPLIT) {
	    XtSetArg(args[0], XmNarrowDirection, XmARROW_UP);
	    XtSetValues(new_p->up_arrow, args, 1);
	    XtSetArg(args[0], XmNarrowDirection, XmARROW_DOWN);
	    XtSetValues(new_p->down_arrow, args, 1);
	}
    }

    if (new_p->text && (new_p->text == cur_p->text)) {
	n = 0;
	if (new_p->text_columns != cur_p->text_columns) {
	    XtSetArg(args[n], XmNcolumns, new_p->text_columns); n++;
	}
	if (new_p->text_max_length != cur_p->text_max_length) {
	    XtSetArg(args[n], XmNmaxLength, new_p->text_max_length); n++;
	}
	if (n > 0)
	    XtSetValues(new_p->text, args, n);
    }
    
    /*
     * LabelWidget alignment has changed.
     */
    if (new_p->label && (new_p->alignment != cur_p->alignment)) {
	XtSetArg(args[0], XmNalignment, new_p->alignment);
	XtSetValues(new_p->label, args, 1);
    }

    store_info = ((new_p->items != cur_p->items) ||
		  (new_p->item_count != cur_p->item_count) ||
		  (new_p->decimal_points != cur_p->decimal_points) ||
		  (new_p->maximum != cur_p->maximum) ||
		  (new_p->minimum != cur_p->minimum) ||
		  (new_p->numeric_increment != cur_p->numeric_increment) ||
		  ((new_p->child_type == XmNUMERIC) &&
		   (new_p->position != cur_p->position)));
    if (store_info)
	StoreResourceInfo(new_p, cur_p, (new_p->items != cur_p->items));
    
    if (new_p->label && (store_info || 
			 (new_p->label != cur_p->label) ||
			 (new_p->child_type != cur_p->child_type))) {
	SetMaximumLabelSize(new_p);
	label_size_changed = TRUE;
    }
    
    if (store_info ||
	(new_p->alignment != cur_p->alignment) ||
	(new_p->editable != cur_p->editable) ||
	(new_p->position != cur_p->position) ||
	(new_p->label != cur_p->label) ||
	(new_p->child_type != cur_p->child_type)) {
	if (new_p->editable)
	    SetTextFieldData(new);
	else
	    SetLabelData(new);
    }

    /*
     * Must recalculate the spin_Button and re-layout the children.
     * If this is not editable, then set the label to its' maximum
     * size; it will get chopped if it is too big.  This is needed 
     * because we shrink the label down, and SetSpinButtonSize() uses
     * the label's core sizes to figure what size to become.
     */
    if ((new_p->editable != cur_p->editable) ||
	(SPIN_MARGIN_W(new) != SPIN_MARGIN_W(current)) ||
	(SPIN_MARGIN_H(new) != SPIN_MARGIN_H(current)) ||
	(SPIN_SHADOW(new) != SPIN_SHADOW(current)) ||
	(new_p->arrow_layout != cur_p->arrow_layout) ||
	(!new_p->editable && label_size_changed)) {
	ClearShadow(current, TRUE);
	if (new_p->recompute_size)
	    SetSpinButtonSize(new);
	LayoutChildren(new);
	DrawShadow(new);
    }
    
    return(FALSE);
}


/*
 * This function clears the shadow around our widget.  If all is TRUE,
 * then clear all 4 sides; otherwise, only clear the right and bottom
 * sides (during resize). 
 */ 
static void
ClearShadow(w, all)
DtSpinButtonWidget w;
Boolean all;
{
    Dimension shadow = SPIN_SHADOW(w);
    Dimension margin_w = SPIN_MARGIN_W(w);
    Dimension margin_h = SPIN_MARGIN_H(w);

    if ((shadow > 0) && XtIsRealized(w)) {
	if (all) {
	    XClearArea(XtDisplayOfObject((Widget)w),
		       XtWindowOfObject((Widget)w), 
		       margin_w, margin_h,
		       w->spin_button.old_width - (margin_w * 2),
		       shadow, FALSE);
	    XClearArea(XtDisplayOfObject((Widget)w),
		       XtWindowOfObject((Widget)w), 
		       margin_w, margin_h, shadow, 
		       w->spin_button.old_height - (margin_h * 2), FALSE);
	}
	XClearArea(XtDisplayOfObject((Widget)w), XtWindowOfObject((Widget)w),
		   margin_w, w->spin_button.old_height - margin_h - shadow,
		   w->spin_button.old_width - (margin_w * 2), shadow, FALSE);
	XClearArea(XtDisplayOfObject((Widget)w), XtWindowOfObject((Widget)w),
		   w->spin_button.old_width - margin_w - shadow,
		   margin_h, shadow, 
		   w->spin_button.old_height - (margin_h * 2), FALSE);
    }
    DrawHighlight(w, TRUE);
}

/* 
 * This functions draws the shadow around our spin-button.
 */
static void
DrawShadow(w)
DtSpinButtonWidget w;
{
    Dimension shadow = SPIN_SHADOW(w);
    Dimension margin_w = SPIN_MARGIN_W(w);
    Dimension margin_h = SPIN_MARGIN_H(w);
    
    if ((shadow > 0) && XtIsRealized(w)) {
	_XmDrawShadows(XtDisplayOfObject((Widget)w),
		       XtWindowOfObject((Widget)w),
		       w->manager.top_shadow_GC,
		       w->manager.bottom_shadow_GC, 
		       margin_w, margin_h,
		       w->core.width - (margin_w * 2),
		       w->core.height - (margin_h * 2),
		       shadow, XmSHADOW_OUT);
    }
    DrawHighlight(w, FALSE);
}

/*
 * This function sets up the items information for the SpinButton, as
 * well as variables needed for child_type.
 */
static void
StoreResourceInfo(spin_p, old_p, do_items)
DtSpinButtonPart *spin_p;
DtSpinButtonPart *old_p;
Boolean do_items;
{
    XmStringTable table;
    int i, base = 1;

    if (do_items && spin_p->items) {
	/* Free up current items if needed */
	if (old_p && old_p->items) {
	    for (i = 0; i < old_p->item_count; i++) {
		XmStringFree(old_p->items[i]);
	    }
	    XtFree((char*)old_p->items);
	}
	    
	/*
	 * Loop through all the items, copy them into our space.
	 */
	table = (XmStringTable)XtMalloc(sizeof(XmString) * spin_p->item_count);
	for (i = 0; i < spin_p->item_count; i++) {
	    table[i] = XmStringCopy(spin_p->items[i]);
	}
	spin_p->items = table;
	for (i = 0; i < spin_p->item_count; i++)
	    spin_p->items[i] = table[i];
    }

    /*
     * Store the numeric information
     */

    /* get base number for convert ints to floats */
    for (i = 0; i < spin_p->decimal_points; i++)
	base *= 10;

    /* Set new initial values */
    spin_p->min = (float)spin_p->minimum/base;
    spin_p->max = (float)spin_p->maximum/base;
    spin_p->increment = (float)spin_p->numeric_increment/base;

    spin_p->current = spin_p->position/base;

    /* Create format string used to build correct XmString value */
    spin_p->float_format[0] = '%';
    sprintf((char*)(spin_p->float_format+1), ".%df", spin_p->decimal_points);
}


/* Caller must free string */
static char*
GetTextString(xm_string)
XmString xm_string;
{
    XmStringContext context;
    XmStringComponentType type;
    XmStringCharSet charset;
    XmStringDirection direction;
    XmStringComponentType unknown_tag;
    unsigned short ul;
    unsigned char *uv;
    char *text = NULL;
    Boolean done = FALSE;

    XmStringInitContext(&context, xm_string);
    
    /* Loop until 1st char* found */
    while (!done) {
	type = XmStringGetNextComponent(context, &text, &charset,
					&direction, &unknown_tag, 
					&ul, &uv);
	switch (type) {
	case XmSTRING_COMPONENT_END:
	    done = TRUE;
	    break;
	case XmSTRING_COMPONENT_TEXT:
	case XmSTRING_COMPONENT_LOCALE_TEXT:
	    done = TRUE;
	    break;
	default:
	    break;
	}
    }
    XmStringFreeContext(context);
    return(text);
}

/*
 * Take the string out of the list and put it into the text-field.
 * text-fields don't handle xm-strings, so we must get the char*
 * out of it (only getting the first segment).  This is slower than
 * storing the text-strings (char*) ourselves, but that would take
 * up a lot of memory.  Since this setting happens during a user
 * action, speed isn't a problem.
 */
static void
SetTextFieldData(spin)
DtSpinButtonWidget spin;
{
    DtSpinButtonPart *spin_p = (DtSpinButtonPart*)&(spin->spin_button);
    char string[NUMERIC_LENGTH];
    XmString xm_string;
    char *text;
    Arg arg;
    
    
    if (spin_p->child_type == XmNUMERIC) {
	sprintf(string, spin_p->float_format, (float)spin_p->current);
	XtSetArg(arg, XmNvalue, string);
	XtSetValues(spin_p->text, &arg, 1);
    }
    else {
	if (spin_p->items == NULL){
	    XtSetArg(arg, XmNvalue, "");
	    XtSetValues(spin_p->text, &arg, 1);
	    return;
	}
	else {
	    xm_string = spin_p->items[spin_p->position];
	    if ((text = GetTextString(xm_string))) {
		XtSetArg(arg, XmNvalue, text);
		XtSetValues(spin_p->text, &arg, 1);
		XtFree(text);
	    }
	}
    }
}

/*
 * Set the maximum size of the label, depending on the
 * characteristics of the list of items.  Not very efficient
 * if switching from numeric to non-numeric.
 */
static void
SetMaximumLabelSize(spin_p)
DtSpinButtonPart *spin_p;
{
    XmString xm_string;
    XmFontList font_list;
    char string[NUMERIC_LENGTH];
    Dimension width, height;
    Dimension longest = 0;
    Dimension highest = 0;
    Arg args[5];
    int i;

    /* Get font info from the widget */
    XtSetArg(args[0], XmNfontList, &font_list);
    XtGetValues(spin_p->label, args, 1);

    if (spin_p->child_type == XmNUMERIC) {
	/* Find out maximum size of the widget from min/max */
	sprintf(string, spin_p->float_format, spin_p->min);
	xm_string = XmStringCreateSimple(string);
	XmStringExtent(font_list, xm_string, &longest, &highest);
	XmStringFree(xm_string);
	sprintf(string, spin_p->float_format, spin_p->max);
	xm_string = XmStringCreateSimple(string);
	XmStringExtent(font_list, xm_string, &width, &height);
	XmStringFree(xm_string);
	
	longest = (width > longest) ? width : longest;
	highest = (height > highest) ? height : highest;
    }
    else if (spin_p->items) {
	/*
	 * Loop through all the items to find the biggest dimensions
	 */
	for (i = 0; i < spin_p->item_count; i++) {
	    XmStringExtent(font_list, spin_p->items[i], &width, &height);
	    longest = (width > longest) ? width : longest;
	    highest = (height > highest) ? height : highest;
	}
    }
    else
	XmStringExtent(font_list, InitLabel, &longest, &highest);

    spin_p->label_max_length = longest + (LABEL_PADDING * 2);
    spin_p->label_max_height = highest;
    XtResizeWidget(spin_p->label, spin_p->label_max_length, highest,
		   spin_p->label->core.border_width);
}


/*
 * Put the current list item into the label.
 */
static void
SetLabelData(spin)
DtSpinButtonWidget spin;
{
    DtSpinButtonPart *spin_p = (DtSpinButtonPart*)&(spin->spin_button);
    XmString xm_string;
    char string[NUMERIC_LENGTH];
    int index = spin_p->position;
    Arg arg;

    if (spin_p->child_type == XmNUMERIC) {
	sprintf(string, spin_p->float_format, (float)spin_p->current);
	xm_string = XmStringCreateSimple(string);
	XtSetArg(arg, XmNlabelString, xm_string);
	XtSetValues(spin_p->label, &arg, 1);
    }
    else {
	/*
	 * If the item is not empty, get the current item from the list, else
	 * use InitLabel.
	 */
	xm_string = (spin_p->items) ? spin_p->items[index] : InitLabel;
	XtSetArg(arg, XmNlabelString, xm_string);
	XtSetValues(spin_p->label, &arg, 1);
    }
}

/*
 * Timout dispatch routine.  This calls the appropriate callback function
 * to simulate up or down arrow activation.
 */
static void
timer_dispatch(client_data, id)
XtPointer client_data;
XtIntervalId *id;
{
    DtSpinButtonWidget spin_w = (DtSpinButtonWidget)client_data;
    DtSpinButtonPart *spin_p = (DtSpinButtonPart*)&(spin_w->spin_button);

    timer = 0;
    spin_p->init_cb = FALSE;
    if (spin_p->which_arrow == XmARROW_UP) {
	if (spin_p->grabbed) {
	    XtRemoveGrab(spin_p->up_arrow);
	    spin_p->grabbed = FALSE;
	}
	up_cb(NULL, client_data, NULL);
    }
    else {
	if (spin_p->grabbed) {
	    XtRemoveGrab(spin_p->down_arrow);
	    spin_p->grabbed = FALSE;
	}
	down_cb(NULL, client_data, NULL);
    }
}

static void
TextFieldActivate(spin_p)
DtSpinButtonPart *spin_p;
{
    XmTextFieldWidget w = (XmTextFieldWidget)(spin_p->text);
    XmAnyCallbackStruct cb;
    char string[NUMERIC_LENGTH];
    char *data = NULL;
    char *text = NULL;
    Arg arg;
    Boolean free_me = TRUE;
    
    XtSetArg(arg, XmNvalue, &data);
    XtGetValues((Widget)w, &arg, 1);

    if (spin_p->child_type == XmNUMERIC) {
	sprintf(string, spin_p->float_format, (float)spin_p->current);
	text = string;
	free_me = FALSE;
    }
    else if (spin_p->items)
	text = GetTextString(spin_p->items[spin_p->position]);

    if (text && data && (strcmp(text, data) == 0)) {
	if (free_me)
	    XtFree(text);
	return;
    }
    /* Only send callback if both are not NULL */
    else if (!((text == NULL) && (data == NULL))) {
	cb.reason = XmCR_ACTIVATE;
	cb.event  = NULL;
	XtCallCallbackList((Widget)w, w->text.activate_callback,
			   (XtPointer)&cb);
	if (text && free_me)
	    XtFree(text);
    }
}

/*
 * This function calls the appropriate callback for the spin-button.
 * It gathers the correct arguments and fills in the callback structure.
 */
static Boolean
SendCallback(spin, event, value_changed, position, current, crossed)
DtSpinButtonWidget spin;
XEvent *event;
Boolean value_changed;
int position;
float current; /* Used for numeric */
Boolean crossed;
{
    DtSpinButtonPart *spin_p = (DtSpinButtonPart*)&(spin->spin_button);
    DtSpinButtonCallbackStruct cb;
    XmString xm_string = NULL;
    char string[NUMERIC_LENGTH];

    if (spin_p->child_type == XmNUMERIC) {
	sprintf(string, spin_p->float_format, (float)current);
	xm_string = XmStringCreateSimple(string);
    }
    else {
	xm_string = (spin_p->items) ? spin_p->items[position] : InitLabel;
	xm_string = XmStringCopy(xm_string);
    }

    if (event)
	cb.reason = XmCR_OK;
    else if (spin_p->which_arrow == XmARROW_UP)
	cb.reason = XmCR_SPIN_UP;
    else
	cb.reason = XmCR_SPIN_DOWN;
    cb.doit = TRUE;
    cb.event = event;
    cb.widget = (Widget)spin;
    cb.position = position;
    cb.value = xm_string;
    cb.crossed_boundary = crossed;
    if (value_changed) {
	XtCallCallbackList((Widget)spin, spin_p->value_changed_callback, 
			   (XtPointer)&cb);
	cb.doit = TRUE;
    }
    else {
	XtCallCallbackList((Widget)spin, spin_p->modify_verify_callback, 
			   (XtPointer)&cb);
    }
    XmStringFree(xm_string);

    return(cb.doit);
}


/*
 * This function gets called by the up/down arm callback functions.
 * We set up the timer and send the modify-verify and value-changed
 * callbacks. 
 * There are potential problems if the user does some weird stuff 
 * in the callbacks.  I have added protection against the case where
 * a user does a grab (with XtAddGrab/XtPopup/etc.) in the callbacks.
 * Grabbing in the callback would cause us to lose the button-release
 * (disarm), which would make the timer go on forever.  A grab is
 * done after the callbacks just to make sure we will receive the
 * button-release.  The button-release will call disarm_cb() which will
 * un-grab and disable the timer.  I have also added a leave callback
 * which helps to protect against these kinds of problems.  
 * If the callback goes into another event-loop (which I hope would
 * never happen), we would spin continuously (since our XtAddGrab never
 * get called), until the user left the window (which would call 
 * grab_leave_cb).  The grabbed flag gets set if we do the grab, so that
 * we know if we can remove the grab.  Our XtAddGrab() might not get called
 * if the callback enters another event-loop.
 *
 * The event sent in the callback will be NULL during continuous spinning.
 */
static void
FinishUpDown(spin, arrow_call_data, new_position, new_current, crossed)
DtSpinButtonWidget spin;
XtPointer arrow_call_data;
int new_position;
float new_current;
Boolean crossed;
{
    DtSpinButtonPart *spin_p = (DtSpinButtonPart*)&(spin->spin_button);
    XmArrowButtonCallbackStruct *arrow_data;
    XEvent *last_event = NULL;
    int repeat_delay = spin_p->repeat_delay;

    if (spin_p->init_cb)    
	repeat_delay = spin_p->initial_delay;
    timer = XtAppAddTimeOut(XtWidgetToApplicationContext((Widget)spin),
			    repeat_delay, timer_dispatch, (XtPointer)spin);

    /* Try to get Xevent */
    if ((arrow_data = (XmArrowButtonCallbackStruct*)arrow_call_data))
	last_event = arrow_data->event;

    /* 
     * Send modify_verify callback.  If user says no, then
     * clear the timer and reset the state before returning.
     */
    if (SendCallback(spin, last_event, FALSE, new_position,
		     new_current, crossed) == FALSE) {
	XtRemoveTimeOut(timer);
	timer = (XtIntervalId)NULL;
	spin->spin_button.init_cb = TRUE;
	return;
    }

    /* User said yes, so set widget values */
    spin_p->position = new_position;
    spin_p->current = new_current;
    if (spin_p->editable)
	SetTextFieldData(spin);
    else
	SetLabelData(spin);

    /* send value_changed callback */
    (void)SendCallback(spin, last_event, TRUE, spin_p->position,
		       spin_p->current, crossed);

    /* See notes at top of function on XtAddGrab usage */
    spin_p->grabbed = TRUE;
    if (spin_p->which_arrow == XmARROW_UP)
	XtAddGrab(spin_p->up_arrow, FALSE, FALSE);
    else
	XtAddGrab(spin_p->down_arrow, FALSE, FALSE);
}

/*
 * Show the next value in the SpinButton.  If numeric, just add the
 * increment value.  If using string-table, get the next one in the
 * table.  This function takes care of wrap around.  Set the arrow
 * type.  This is needed for the timer_dispatch function.  up_cb
 * gets called the first time the button is pressed, and each time the
 * timer goes off.
 * All widget internals are expected to be correct here; they
 * get verified when set by the user.
 */
static void
up_cb(w, client_data, call_data)
Widget w;
XtPointer client_data;
XtPointer call_data;
{
    DtSpinButtonWidget spin = (DtSpinButtonWidget)client_data;
    DtSpinButtonPart *spin_p = (DtSpinButtonPart*)&(spin->spin_button);
    Boolean crossed_boundary = FALSE;
    int new_position = spin_p->position;
    float new_current = spin_p->current;

    if (spin_p->editable)
	TextFieldActivate(spin_p);

    /*
     * If non-numeric and no items then ignore the user activate event.
     */
    if ((spin_p->child_type == XmSTRING) && (spin_p->items == NULL))
	return;
    
    if (spin_p->child_type == XmNUMERIC) {
	if ((new_current + spin_p->increment) > spin_p->max) {
	    if (spin_p->wrap) {
		new_position = spin_p->minimum;
		new_current = spin_p->min;
		crossed_boundary = TRUE;
	    }
	    else
		XBell(XtDisplayOfObject((Widget)spin), 0);
	}
	else {
	    new_position += spin_p->numeric_increment;
	    new_current += spin_p->increment;
	}
    }
    else if (spin_p->items) {
	if (new_position == (spin_p->item_count - 1)) {
	    if (spin_p->wrap) {
		new_position = 0;
		crossed_boundary = TRUE;
	    }
	    else
		XBell(XtDisplayOfObject((Widget)spin), 0);
	}
	else
	    new_position++;
    }

    spin_p->which_arrow = XmARROW_UP;
    FinishUpDown(spin, call_data, new_position, new_current, crossed_boundary);
}


/*
 * Show the previous value in the SpinButton.  If numeric, just decrement
 * the increment value.  If using string-table, get the previous one in the
 * table.  This function takes care of wrap around. Set the arrow
 * type.  This is needed for the timer_dispatch function.  down_cb
 * gets called the first time the button is pressed, and each time the
 * timer goes off.
 * All widget internals are expected to be correct here; they
 * get verified when set by the user.
 */
static void
down_cb(w, client_data, call_data)
Widget w;
XtPointer client_data;
XtPointer call_data;
{
    DtSpinButtonWidget spin = (DtSpinButtonWidget)client_data;
    DtSpinButtonPart *spin_p = (DtSpinButtonPart*)&(spin->spin_button);
    Boolean crossed_boundary = FALSE;
    int new_position = spin_p->position;
    float new_current = spin_p->current;
    
    if (spin_p->editable)
	TextFieldActivate(spin_p);

    /*
     * If non-numeric and no items then ignore the user activate event.
     */
    if ((spin_p->child_type == XmSTRING) && (spin_p->items == NULL))
	return;

    if (spin_p->child_type == XmNUMERIC) {
	if ((new_current - spin_p->increment) < spin_p->min) {
	    if (spin_p->wrap) {
		new_current = spin_p->max;
		new_position = spin_p->maximum;
		crossed_boundary = TRUE;
	    }
	    else
		XBell(XtDisplayOfObject((Widget)spin), 0);
	}
	else {
	    new_current -= spin_p->increment;
	    new_position -= spin_p->numeric_increment;
	}
    }
    else if (spin_p->items) {
	if (new_position == 0) {
	    if (spin_p->wrap) {
		new_position = spin_p->item_count - 1;
		crossed_boundary = TRUE;
	    }
	    else
		XBell(XtDisplayOfObject((Widget)spin), 0);
	}
	else
	    new_position--;
    }

    spin_p->which_arrow = XmARROW_DOWN;
    FinishUpDown(spin, call_data, new_position, new_current, crossed_boundary);
}

static void
disarm_cb(w, client_data, call_data)
Widget w;
XtPointer client_data;
XtPointer call_data;
{
    DtSpinButtonWidget spin = (DtSpinButtonWidget)client_data;

    if ((int)timer) {
	spin->spin_button.init_cb = TRUE;
	XtRemoveTimeOut(timer);
	timer = (XtIntervalId)NULL;
	if (spin->spin_button.grabbed) {
	    XtRemoveGrab(w);
	    spin->spin_button.grabbed = FALSE;
	}
    }
}

static void
grab_leave_cb(w, client_data, event, dispatch)
Widget w;
XtPointer client_data;
XEvent *event;
Boolean *dispatch;
{
    DtSpinButtonWidget spin = (DtSpinButtonWidget)client_data;

    if ((int)timer && 
	((event->xcrossing.mode == NotifyGrab) ||
	 (event->xcrossing.mode == NotifyUngrab) ||
	 (!(event->xcrossing.state & Button1Mask)))) {
	spin->spin_button.init_cb = TRUE;
	XtRemoveTimeOut(timer);
	timer = (XtIntervalId)NULL;
	if (spin->spin_button.grabbed) {
	    XtRemoveGrab(w);
	    spin->spin_button.grabbed = FALSE;
	}
    }
}

/*
 * We get the text-field losing-focus callback, so pass it on to
 * the user if they requested it.  Our losing-focus callback 
 * is just a convenience callback, so that the user doesn't
 * have to get the text-field first.  This make our integration
 * with XDesigner a little easier.
 */
static void
text_losing_focus_cb(w, client_data, call_data)
Widget w;
XtPointer client_data;
XtPointer call_data;
{
    DtSpinButtonWidget spin = (DtSpinButtonWidget)client_data;

    if (spin->spin_button.losing_focus_callback)
	XtCallCallbackList((Widget)spin, 
			   spin->spin_button.losing_focus_callback, 
			   (XtPointer)call_data);
}

/*
 * We get the text-field activate callback, so pass it on to
 * the user if they requested it.  Our activate callback 
 * is just a convenience callback, so that the user doesn't
 * have to get the text-field first.  This make our integration
 * with XDesigner a little easier.
 */
static void
text_activate_cb(w, client_data, call_data)
Widget w;
XtPointer client_data;
XtPointer call_data;
{
    DtSpinButtonWidget spin = (DtSpinButtonWidget)client_data;
    
    if (spin->spin_button.activate_callback)
	XtCallCallbackList((Widget)spin, 
			   spin->spin_button.activate_callback, 
			   (XtPointer)call_data);
}

/*
 * We get the text-field focus callback, so pass it on to
 * the user if they requested it.  Our focus callback 
 * is just a convenience callback, so that the user doesn't
 * have to get the text-field first.  This make our integration
 * with XDesigner a little easier.
 */
static void
text_focus_cb(w, client_data, call_data)
Widget w;
XtPointer client_data;
XtPointer call_data;
{
    DtSpinButtonWidget spin = (DtSpinButtonWidget)client_data;

    if (spin->spin_button.focus_callback)
	XtCallCallbackList((Widget)spin, 
			   spin->spin_button.focus_callback, 
			   (XtPointer)call_data);
}


/*
 * Synthetic resource get functions.
 */

static XmImportOperator
_XmSetSyntheticResForChild(widget, offset, value)
Widget widget;
int offset;
XtArgVal * value;
{ 
    return(XmSYNTHETIC_LOAD);
}

void
_DtSpinButtonGetArrowSize(w, resource_offset, value)
Widget w;
int resource_offset;
XtArgVal *value;
{
    DtSpinButtonWidget spin = (DtSpinButtonWidget)w;

    *value = (XtArgVal)spin->spin_button.up_arrow->core.height;
}

/*
 * Routines which manipulate the SpinButton list.  These are external
 * for use by users of our widget.
 */

Widget 
DtCreateSpinButton(parent, name, arglist, num_args)
Widget parent;
char *name;
Arg *arglist;
int num_args;
{
    return(XtCreateWidget(name, dtSpinButtonWidgetClass, parent,
			  arglist, num_args));
}

void
DtSpinButtonAddItem(spin, item, pos)
DtSpinButtonWidget spin;
XmString item;
int pos;
{
    DtSpinButtonPart *spin_p = (DtSpinButtonPart*)&(spin->spin_button);
    XmString old, new_str, tmp;
    int total_items;

    total_items = spin_p->item_count + 1;
    spin_p->items = (XmString *)XtRealloc((char*)spin_p->items, 
					  (sizeof(XmString) * total_items));
    new_str = XmStringCopy(item);

    pos--;  /* User gives pos starting at 1 (0 means end of list) */

    if ((pos < 0) || (pos > spin_p->item_count))
	pos = spin_p->item_count;

    if (pos >= spin_p->item_count)
        spin_p->items[pos] = new_str;
    else {
        old = spin_p->items[pos];
        spin_p->items[pos] = new_str;
	for (pos++; pos < total_items; pos++) {
	    tmp = spin_p->items[pos];
	    spin_p->items[pos] = old;
	    old = tmp;
        }
    }
    spin_p->item_count = total_items;

    if (spin_p->label) {
	SetMaximumLabelSize(spin_p);
	if (spin_p->editable == FALSE) {
	    ClearShadow(spin, TRUE);
	    if (spin_p->recompute_size)
		SetSpinButtonSize(spin);
	    LayoutChildren(spin);
	    DrawShadow(spin);
	}
    }

    /* Update the text-field or label */
    if (spin_p->editable)
	SetTextFieldData(spin);
    else
	SetLabelData(spin);
}



void
DtSpinButtonDeletePos(spin, pos)
DtSpinButtonWidget spin;
int pos;
{
    DtSpinButtonPart *spin_p = (DtSpinButtonPart*)&(spin->spin_button);
    int total_items;

    if (spin_p->item_count < 1)
      return;

    pos--;
    if ((pos < 0) || (pos > spin_p->item_count))
	pos = spin_p->item_count - 1;

    total_items = spin_p->item_count - 1;
    XmStringFree(spin_p->items[pos]);

    if (pos < spin_p->item_count) {
	for (; pos < total_items; pos++)
	    spin_p->items[pos] = spin_p->items[pos+1];
    }
    spin_p->items = (XmString *)XtRealloc((char*)spin_p->items,
					  (sizeof(XmString) * total_items));
    spin_p->item_count = total_items;

    if (spin_p->label) {
	SetMaximumLabelSize(spin_p);
	if (spin_p->editable == FALSE) {
	    ClearShadow(spin, TRUE);
	    if (spin_p->recompute_size)
		SetSpinButtonSize(spin);
	    LayoutChildren(spin);
	    DrawShadow(spin);
	}
    }

    /* Update the text-field or label */
    if (spin_p->editable)
	SetTextFieldData(spin);
    else
	SetLabelData(spin);
}

/*
 * Make the given item the currently visible item in the
 * text-field or label.
 */
void
DtSpinButtonSetItem(spin, item)
DtSpinButtonWidget spin;
XmString item;
{
    DtSpinButtonPart *spin_p = (DtSpinButtonPart*)&(spin->spin_button);
    int i;

    if (item && spin_p->items) {
	for (i = 0; i < spin_p->item_count; i++)
	    if (XmStringCompare(item, spin_p->items[i]))
		break;
	if (i < spin_p->item_count) {
	    spin_p->position = i;
	    if (spin_p->editable)
		SetTextFieldData(spin);
	    else
		SetLabelData(spin);
	}
	else
	    XtWarning(SPIN_SET_ITEM);
    }
    else
	XtWarning(SPIN_SET_ITEM);
}
