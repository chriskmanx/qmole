
#include <Xm/XmP.h>
#include <Xm/BulletinBP.h>
#include <Xm/PushBP.h>
#include <Xm/PushBGP.h>
/* #include <XmI/MacrosI.h> */
#include "TrivialP.h"

/*
 * Forward Declarations
 */
static void class_initialize();
static void class_part_initialize(WidgetClass class);
static void initialize(Widget request, Widget new,
		       ArgList args, Cardinal *num_args);
static void destroy(Widget w);
static Boolean set_values(Widget current, Widget request, Widget new,
			  ArgList args, Cardinal *num_args);
 
/*@@@@@@@@@ SPECIAL @@@@@@@@@*/
XmGeoMatrix trivial_matrix_create(Widget _w, Widget _from,
				  XtWidgetGeometry *_pref);
Boolean trivial_NoGeoRequest(XmGeoMatrix _geoSpec);
/*@@@@@@@@@ SPECIAL @@@@@@@@@*/

static XmBaseClassExtRec _XmTrivialCoreClassExtRec = {
    /* next_extension            */ NULL,
    /* record_type               */ NULLQUARK,                             
    /* version                   */ XmBaseClassExtVersion,
    /* size                      */ sizeof(XmBaseClassExtRec),
    /* initialize_prehook        */ NULL,
    /* set_values_prehook        */ NULL,
    /* initialize_posthook       */ NULL,
    /* set_values_posthook       */ NULL,
    /* secondary_object_class    */ NULL,
    /* secondary_object_create   */ NULL,
    /* get_secondary_resources   */ NULL,
    /* fast_subclass             */ { 0 },
    /* get_values_prehook        */ NULL,
    /* get_values_posthook       */ NULL,
    /* class_part_init_prehook   */ NULL,
    /* class_part_init_posthook  */ NULL,
    /* ext_resources             */ NULL,
    /* compiled_ext_resources    */ NULL,
    /* num_ext_resources         */ 0,
    /* use_sub_resources         */ FALSE,
    /* widget_navigable          */ NULL,
    /* focus_change              */ NULL,
    /* wrapper_data              */ NULL
};

static XmManagerClassExtRec _XmTrivialMClassExtRec = {
    /* next_extension            */ NULL,
    /* record_type               */ NULLQUARK,
    /* version                   */ XmManagerClassExtVersion,
    /* record_size               */ sizeof(XmManagerClassExtRec),
    /* traversal_children        */ NULL /* FIXME */
};

XmTrivialClassRec xmTrivialClassRec = {
    /* Core class part */
    {
	/* superclass            */ (WidgetClass)&xmBulletinBoardClassRec,
        /* class_name            */ "XmTrivial",
	/* widget_size           */ sizeof(XmBulletinBoardRec),
	/* class_initialize      */ class_initialize,
	/* class_part_initialize */ class_part_initialize,
	/* class_inited          */ FALSE,
	/* initialize            */ initialize,
	/* initialize_hook       */ NULL,
	/*@@@@@@@@@ SPECIAL @@@@@@@@@*/
	/* realize               */ XtInheritRealize,
	/*@@@@@@@@@ SPECIAL @@@@@@@@@*/
	/* actions               */ NULL,
	/* num_actions           */ 0,
	/* resources             */ NULL,
	/* num_resources         */ 0,
	/* xrm_class             */ NULLQUARK,
	/* compress_motion       */ TRUE,
	/* compress_exposure     */ XtExposeCompressMaximal,
	/* compress_enterleave   */ TRUE,
	/* visible_interest      */ FALSE,
	/* destroy               */ destroy,
	/*@@@@@@@@@ SPECIAL @@@@@@@@@*/
	/* resize                */ XtInheritResize,
	/*@@@@@@@@@ SPECIAL @@@@@@@@@*/
	/* expose                */ XtInheritExpose,
	/* set_values            */ set_values,
	/* set_values_hook       */ NULL,
	/* set_values_almost     */ XtInheritSetValuesAlmost,
	/* get_values_hook       */ NULL,
	/* accept_focus          */ NULL,
	/* version               */ XtVersion,
	/* callback offsets      */ NULL,
	/* tm_table              */ NULL,
	/*@@@@@@@@@ SPECIAL @@@@@@@@@*/
	/* query_geometry        */ XtInheritQueryGeometry,
	/*@@@@@@@@@ SPECIAL @@@@@@@@@*/
	/* display_accelerator   */ NULL,
	/* extension             */ NULL /*(XtPointer)&_XmTrivialCoreClassExtRec*/
    },
    /* Composite class part */
    {
	/*@@@@@@@@@ SPECIAL @@@@@@@@@*/
	/* geometry manager */ XtInheritGeometryManager, 
        /* change_managed   */ XtInheritChangeManaged, 
	/*@@@@@@@@@ SPECIAL @@@@@@@@@*/
        /* insert_child     */ XtInheritInsertChild,
        /* delete_child     */ XtInheritDeleteChild,
        /* extension        */ NULL
    },
    /* Constraint class part */
    {
	/* subresources      */ NULL,
        /* subresource_count */ 0,
        /* constraint_size   */ 0,
        /* initialize        */ NULL,
        /* destroy           */ NULL,
        /* set_values        */ NULL,
        /* extension         */ NULL
    },
    /* XmManager class part */
    {
        /* translations                 */ XtInheritTranslations,
        /* syn_resources                */ NULL,
        /* num_syn_resources            */ 0,
        /* syn_constraint_resources     */ NULL,
        /* num_syn_constraint_resources */ 0,
        /* parent_process               */ XmInheritParentProcess,
        /* extension                    */ (XtPointer)&_XmTrivialMClassExtRec
    },
    /* XmBulletinBoard Area part */
    {
	/* always_install_accelerators  */ False,
	/*@@@@@@@@@ SPECIAL @@@@@@@@@*/
	/* geo_matrix_create            */ trivial_matrix_create,
	/*@@@@@@@@@ SPECIAL @@@@@@@@@*/
	/* focus_moved_proc             */ XmInheritFocusMovedProc,
	/* extension                    */ NULL

    },
    /* XmTrivial Class Part */
    {
	/* extension */	0
    }
};

WidgetClass xmTrivialWidgetClass = (WidgetClass)&xmTrivialClassRec;

static void 
class_initialize()
{
    _XmTrivialCoreClassExtRec.record_type = XmQmotif;
}

static void
class_part_initialize(WidgetClass widget_class)
{
}

static void
initialize(Widget request,
	   Widget new,
	   ArgList args,
	   Cardinal *num_args)
{
}

static void
destroy(Widget w)
{
}

static Boolean
set_values(Widget old,
	   Widget request,
	   Widget new,
	   ArgList args,
	   Cardinal *num_args)
{
    Boolean refresh_needed = False;

    /*@@@@@@@@@ SPECIAL @@@@@@@@@*/
    BB_InSetValues(new) = True;
    /*@@@@@@@@@ SPECIAL @@@@@@@@@*/

    /* do any class specific stuff */

    /*@@@@@@@@@ SPECIAL @@@@@@@@@*/
    BB_InSetValues(new) = False;
    /*@@@@@@@@@ SPECIAL @@@@@@@@@*/

    /*@@@@@@@@@ SPECIAL @@@@@@@@@*/
    if (refresh_needed && (XtClass(new) == xmTrivialWidgetClass))
    {
	_XmBulletinBoardSizeUpdate(new);
	return False;
    }
    /*@@@@@@@@@ SPECIAL @@@@@@@@@*/

    return refresh_needed;
}

/*@@@@@@@@@ SPECIAL @@@@@@@@@*/
XmGeoMatrix
trivial_matrix_create(Widget _w, Widget _from, XtWidgetGeometry *_pref)
{
    XmGeoMatrix geoSpec;
    register XmGeoRowLayout layoutPtr;
    register XmKidGeometry boxPtr;
    Cardinal numKids;
    int i, nrows;
    Widget child;

    /*
    numKids = MGR_NumChildren(_w);
    */
    numKids = (((XmManagerWidget)(_w))->composite.num_children);

    /*@@@@@@@@@ SPECIAL @@@@@@@@@*/
    /* compute the number of rows you want here.  Trivial only has one */
    nrows = 1;
    /*@@@@@@@@@ SPECIAL @@@@@@@@@*/

    /*@@@@@@@@@ SPECIAL @@@@@@@@@*/
    geoSpec = _XmGeoMatrixAlloc(nrows, numKids, 0);
    geoSpec->composite = (Widget)_w;
    geoSpec->instigator = (Widget)_from;
    if (_pref)
        geoSpec->instig_request = *_pref;
    geoSpec->margin_w = BB_MarginWidth(_w) + MGR_ShadowThickness(_w);
    geoSpec->margin_h = BB_MarginHeight(_w) + MGR_ShadowThickness(_w);
    geoSpec->no_geo_request = trivial_NoGeoRequest;
    /*@@@@@@@@@ SPECIAL @@@@@@@@@*/

    layoutPtr = &(geoSpec->layouts->row);
    boxPtr = geoSpec->boxes;

    /*@@@@@@@@@ SPECIAL @@@@@@@@@*/

    /* row 1 */
    layoutPtr->fill_mode = XmGEO_CENTER;
    layoutPtr->fit_mode = XmGEO_WRAP;
    layoutPtr->even_width = 1;
    layoutPtr->even_height = 1;
    layoutPtr->space_above = BB_MarginHeight(_w);
    for (i = 0; i < numKids; i++) {

	/*
	child = MGR_Children(_w)[i];
	*/
	child = (((XmManagerWidget)(_w))->composite.children)[i];

	if ((XmIsPushButton(child) || XmIsPushButtonGadget(child)) &&
	    XtIsManaged(child) && _XmGeoSetupKid(boxPtr, child))
	{
	    boxPtr++;
	}
    }
    layoutPtr++;

    /* end marker */
    layoutPtr->space_above = 0;
    layoutPtr->end = TRUE;
    /*@@@@@@@@@ SPECIAL @@@@@@@@@*/

    return(geoSpec);
}

Boolean
trivial_NoGeoRequest(XmGeoMatrix geo)
{
    if (BB_InSetValues(geo->composite) &&
	XtClass(geo->composite) == xmTrivialWidgetClass)
	return TRUE;

    return FALSE;
}
/*@@@@@@@@@ SPECIAL @@@@@@@@@*/

Widget
XmCreateTrivial(Widget parent,
		char *name,
		Arg *argList,
		Cardinal argcount)
{
    return XtCreateWidget(name,
			  xmTrivialWidgetClass,
			  parent,
			  argList,
			  argcount);
}
