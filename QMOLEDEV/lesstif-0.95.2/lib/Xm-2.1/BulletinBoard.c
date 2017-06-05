/**
 *
 * $Header: /cvsroot/lesstif/lesstif/lib/Xm-2.1/BulletinBoard.c,v 1.4 2005/03/19 10:02:24 dannybackx Exp $
 *
 * Copyright (C) 1995 Free Software Foundation, Inc.
 * Copyright © 1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2004, 2005 LessTif Development Team
 *
 * This file is part of the GNU LessTif Library.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Library General Public License for more details.
 *
 * You should have received a copy of the GNU Library General Public
 * License along with this library; if not, write to the Free
 * Software Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 *
 **/

static const char rcsid[] = "$Header: /cvsroot/lesstif/lesstif/lib/Xm-2.1/BulletinBoard.c,v 1.4 2005/03/19 10:02:24 dannybackx Exp $";

#include <LTconfig.h>

#include <XmI/XmI.h>

#include <Xm/XmP.h>
#include <Xm/BaseClassP.h>
#include <Xm/BulletinBP.h>
#include <Xm/TransltnsP.h>
#include <Xm/DialogS.h>
#include <Xm/ArrowB.h>
#include <Xm/ArrowBG.h>
#include <Xm/LabelP.h>
#include <Xm/LabelGP.h>
#include <Xm/PushB.h>
#include <Xm/PushBG.h>
#include <Xm/MwmUtil.h>
#include <Xm/VendorSEP.h>

#include <Xm/SpecRenderT.h>

#include <XmI/DebugUtil.h>

/* #defines being used in this file:
      #ifdef USE_WIDGETS
*/


/* We use a 'private', i.e. non-declared but actually
   exported call from OpenGroup's Intrinsic sources
   (lib/Xt/Callback.c) in this file.
   Note that we don't give the precise type here ...
 */
extern void 
_XtRemoveAllCallbacks(
  /* InternalCallbackList *callbacks; */
 void *callbacks);

/* similar for this ... */
extern void
_XtAddCallback(XtCallbackList *,
               XtCallbackProc,
               XtPointer);

/* Forward Declarations */
static void class_initialize(void);
static void class_part_initialize(WidgetClass w_class);
static void initialize(Widget request, Widget new_w, ArgList args, Cardinal *num_args);
static void destroy(Widget w);
static void resize(Widget w);
/* static void realize(Widget w, XtValueMask *value_mask, XSetWindowAttributes *attributes); */
static XtGeometryResult query_geometry(Widget w,
				       XtWidgetGeometry *proposed,
				       XtWidgetGeometry *answer);
static Boolean set_values(Widget current, Widget request, Widget new_w,
			  ArgList args, Cardinal *num_args);
static void expose(Widget w, XEvent *event, Region region);
static XtGeometryResult geometry_manager(Widget w,
					 XtWidgetGeometry *request,
					 XtWidgetGeometry *reply);
static void change_managed(Widget w);
static void insert_child(Widget w);
static void delete_child(Widget w);

#if 0
static void constraint_initialize(Widget request, Widget new_w, Arg *args, Cardinal *nargs);
static Boolean constraint_set_values(Widget old, Widget request, Widget new_w,
				     Arg *args, Cardinal *nargs);
#endif

static Boolean _XmBBParentProcess(Widget widget, XmParentProcessData data);
static void _XmBulletinBoardDialogStyleDefault(Widget w, int offset, XrmValue *val);
static XmRenderTable GetRenderTable(Widget w, XtEnum renderTableType);

/*
 * Resources for the Bulletin board class
 */
#define Offset(field) XtOffsetOf(XmBulletinBoardRec, bulletin_board.field)
#define MGR_Offset(field) XtOffsetOf(XmBulletinBoardRec, manager.field)
static XtResource resources[] =
{
    {
	XmNshadowType, XmCShadowType, XmRShadowType,
	sizeof(unsigned char), Offset(shadow_type),
	XmRImmediate, (XtPointer)XmSHADOW_OUT
    },
    {
	XmNshadowThickness, XmCShadowThickness, XmRHorizontalDimension,
	sizeof(Dimension), MGR_Offset(shadow_thickness),
	XmRImmediate, (XtPointer)0
    },
    {
	XmNmarginWidth, XmCMarginWidth, XmRHorizontalDimension,
	sizeof(Dimension), Offset(margin_width),
	XmRImmediate, (XtPointer)10
    },
    {
	XmNmarginHeight, XmCMarginHeight, XmRVerticalDimension,
	sizeof(Dimension), Offset(margin_height),
	XmRImmediate, (XtPointer)10
    },
    {
	XmNdefaultButton, XmCWidget, XmRWidget,
	sizeof(Widget), Offset(default_button),
	XmRImmediate, (XtPointer)NULL
    },
    {
	XmNcancelButton, XmCWidget, XmRWidget,
	sizeof(Widget), Offset(cancel_button),
	XmRImmediate, (XtPointer)NULL
    },
    {
	XmNfocusCallback, XmCCallback, XmRCallback,
	sizeof(XtCallbackList), Offset(focus_callback),
	XmRImmediate, (XtPointer)NULL
    },
    {
	XmNmapCallback, XmCCallback, XmRCallback,
	sizeof(XtCallbackList), Offset(map_callback),
	XmRImmediate, (XtPointer)NULL
    },
    {
	XmNunmapCallback, XmCCallback, XmRCallback,
	sizeof(XtCallbackList), Offset(unmap_callback),
	XmRImmediate, (XtPointer)NULL
    },
    {
	XmNbuttonFontList, XmCButtonFontList, XmRFontList,
	sizeof(XmFontList), Offset(button_font_list),
	XmRFontList, (XtPointer)NULL
    },
    {
	XmNlabelFontList, XmCLabelFontList, XmRFontList,
	sizeof(XmFontList), Offset(label_font_list),
	XmRFontList, (XtPointer)NULL
    },
    {
	XmNtextFontList, XmCTextFontList, XmRFontList,
	sizeof(XmFontList), Offset(text_font_list),
	XmRFontList, (XtPointer)NULL
    },
    {
	XmNtextTranslations, XmCTranslations, XmRTranslationTable,
	sizeof(XtTranslations), Offset(text_translations),
	XmRImmediate, (XtPointer)NULL
    },
    {
	XmNallowOverlap, XmCAllowOverlap, XmRBoolean,
	sizeof(Boolean), Offset(allow_overlap),
	XmRImmediate, (XtPointer)True
    },
    {
	XmNautoUnmanage, XmCAutoUnmanage, XmRBoolean,
	sizeof(Boolean), Offset(auto_unmanage),
	XmRImmediate, (XtPointer)True
    },
    {
	XmNdefaultPosition, XmCDefaultPosition, XmRBoolean,
	sizeof(Boolean), Offset(default_position),
	XmRImmediate, (XtPointer)True
    },
    {
	XmNresizePolicy, XmCResizePolicy, XmRResizePolicy,
	sizeof(unsigned char), Offset(resize_policy),
	XmRImmediate, (XtPointer)XmRESIZE_ANY
    },
    {
	XmNnoResize, XmCNoResize, XmRBoolean,
	sizeof(Boolean), Offset(no_resize),
	XmRImmediate, (XtPointer)False
    },
    {
	XmNdialogStyle, XmCDialogStyle, XmRDialogStyle,
	sizeof(unsigned char), Offset(dialog_style),
	XmRCallProc, (XtPointer)_XmBulletinBoardDialogStyleDefault
    },
    {
	XmNdialogTitle, XmCDialogTitle, XmRXmString,
	sizeof(XmString), Offset(dialog_title),
	XmRString, (XtPointer)NULL
    },
};

static XmSyntheticResource syn_resources[] =
{
    {
	XmNdialogTitle,
	sizeof(XmString), Offset(dialog_title),
	_XmExportXmString, NULL
    },
    {
	XmNmarginWidth,
	sizeof(Dimension), Offset(margin_width),
	_XmFromHorizontalPixels, _XmToHorizontalPixels
    },
    {
	XmNmarginHeight,
	sizeof(Dimension), Offset(margin_height),
	_XmFromVerticalPixels, _XmToVerticalPixels
    }
};

static char _XmBulletinB_mapTranslations[] =
	"";

static XtActionsRec actions[] =
{
    /* {"Return", _XmReturn}, */
    {"BulletinBoardReturn", (XtActionProc)_XmBulletinBoardReturn},
    {"BulletinBoardCancel", (XtActionProc)_XmBulletinBoardCancel},
};

static XtTranslations mapTrans;
static XtTranslations defTrans;

#if 0
static XmBaseClassExtRec _XmBulletinBCoreClassExtRec = {
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
    /* use_sub_resources         */ False,
    /* widget_navigable          */ XmInheritWidgetNavigable,
    /* focus_change              */ XmInheritFocusChange,
    /* wrapper_data              */ NULL
};

static CompositeClassExtensionRec bbCompositeExt = 
{
    /* next_extension */  NULL,
    /* record_type    */  NULLQUARK,
    /* version        */  XtCompositeExtensionVersion,
    /* record_size    */  sizeof(CompositeClassExtensionRec),
    /* accepts_objects */ True,
#if XtSpecificationRelease >= 6
    /* allows_change_managed_set */ True
#endif
};

static XmManagerClassExtRec _XmBulletinBMClassExtRec = {
    /* next_extension            */ NULL,
    /* record_type               */ NULLQUARK,
    /* version                   */ XmManagerClassExtVersion,
    /* record_size               */ sizeof(XmManagerClassExtRec),
    /* traversal_children        */ NULL
};
#endif

XmBulletinBoardClassRec xmBulletinBoardClassRec = {
    /* Core class part */
    {
	/* superclass            */ (WidgetClass) &xmManagerClassRec,
        /* class_name            */ "XmBulletinBoard",
	/* widget_size           */ sizeof(XmBulletinBoardRec),
	/* class_initialize      */ class_initialize,
	/* class_part_initialize */ class_part_initialize,
	/* class_inited          */ False,
	/* initialize            */ initialize,
	/* initialize_hook       */ NULL,
	/* realize               */ XtInheritRealize /*realize*/,
	/* actions               */ actions,
	/* num_actions           */ XtNumber(actions),
	/* resources             */ resources,
	/* num_resources         */ XtNumber(resources),
	/* xrm_class             */ NULLQUARK,
	/* compress_motion       */ True,
	/* compress_exposure     */ XtExposeCompressMaximal,
	/* compress_enterleave   */ True,
	/* visible_interest      */ False,
	/* destroy               */ destroy,
	/* resize                */ resize,
	/* expose                */ expose,
	/* set_values            */ set_values,
	/* set_values_hook       */ NULL,
	/* set_values_almost     */ XtInheritSetValuesAlmost,
	/* get_values_hook       */ NULL,
	/* accept_focus          */ NULL,
	/* version               */ XtVersion,
	/* callback offsets      */ NULL,
	/* tm_table              */ _XmBulletinB_defaultTranslations,
	/* query_geometry        */ query_geometry,
	/* display_accelerator   */ NULL,
	/* extension             */ (XtPointer)NULL /*&_XmBulletinBCoreClassExtRec*/
    },
    /* Composite class part */
    {
	/* geometry manager */ geometry_manager, 
        /* change_managed   */ change_managed, 
        /* insert_child     */ insert_child,
        /* delete_child     */ delete_child,
        /* extension        */ (XtPointer)NULL /*&bbCompositeExt,*/
    },
    /* Constraint class part */
    {
	/* subresources      */ NULL,
        /* subresource_count */ 0,
        /* constraint_size   */ 0,
        /* initialize        */ NULL /*constraint_initialize*/,
        /* destroy           */ NULL,
        /* set_values        */ NULL /*constraint_set_values*/,
        /* extension         */ NULL,
    },
    /* XmManager class part */
    {
	/* translations                 */ XtInheritTranslations,
        /* syn_resources                */ syn_resources,
        /* num_syn_resources            */ XtNumber(syn_resources),
        /* syn_constraint_resources     */ NULL,
        /* num_syn_constraint_resources */ 0,
        /* parent_process               */ _XmBBParentProcess,
	/* extension                    */ (XtPointer)NULL /*&_XmBulletinBMClassExtRec*/
    },
    /* XmBulletinBoard class part */
    {
	/* always_install_accelerators	*/ False,
	/* geo_matrix_create		*/ NULL,
	/* focus_moved_proc		*/ NULL,
	/* extension			*/ NULL,
    },
};

static XmSpecRenderTraitRec _XmBulletinBoardTraitRec = {
	/* version      */      0,
	/* cb           */      GetRenderTable
};

WidgetClass xmBulletinBoardWidgetClass = (WidgetClass)&xmBulletinBoardClassRec;

static void
class_initialize(void)
{
	/* Motif does not have this method */
#if 0
	_XmBulletinBCoreClassExtRec.record_type = XmQmotif;
#endif

	mapTrans = XtParseTranslationTable(_XmBulletinB_mapTranslations);
	defTrans = XtParseTranslationTable(_XmBulletinB_defaultTranslations);

	if (! XmeTraitSet((XtPointer)xmBulletinBoardWidgetClass, XmQTspecifyRenderTable,
			(XtPointer)&_XmBulletinBoardTraitRec)) {
		_XmWarning(NULL, "XmBulletinBoard ClassInitialize: XmeTraitSet failed\n");
	}
}

static void
class_part_initialize(WidgetClass widget_class)
{
    XmBulletinBoardWidgetClass bbclass =
    (XmBulletinBoardWidgetClass)widget_class;
    XmBulletinBoardWidgetClass sclass =
    (XmBulletinBoardWidgetClass)(widget_class->core_class.superclass);
    CompositeClassExtension ext, *extptr;

    extptr = (CompositeClassExtension *)_XmGetClassExtensionPtr(
		    (XmGenericClassExt *)&(bbclass->composite_class.extension),
								   NULLQUARK);

    if (extptr == NULL || *extptr == NULL)
    {
	ext = (CompositeClassExtension)XtNew(CompositeClassExtensionRec);

	if (ext != NULL)
	{
	    ext->next_extension = bbclass->composite_class.extension;
	    ext->record_type = NULLQUARK;
	    ext->version = XtCompositeExtensionVersion;
	    ext->record_size = sizeof(CompositeClassExtensionRec);
	    ext->accepts_objects = True;
#if XtSpecificationRelease >= 6
	    ext->allows_change_managed_set = True;
#endif
	    bbclass->composite_class.extension = (XtPointer)ext;
	}
    }
    else if (!(*extptr)->accepts_objects)
    {
	(*extptr)->accepts_objects = True;
    }

    if (bbclass->bulletin_board_class.geo_matrix_create ==
	XmInheritGeoMatrixCreate && widget_class != xmBulletinBoardWidgetClass)
    {
	bbclass->bulletin_board_class.geo_matrix_create =
	    sclass->bulletin_board_class.geo_matrix_create;
    }

    if (bbclass->bulletin_board_class.focus_moved_proc ==
	XmInheritFocusMovedProc)
    {
	if (sclass->bulletin_board_class.focus_moved_proc)
	{
	    bbclass->bulletin_board_class.focus_moved_proc =
		sclass->bulletin_board_class.focus_moved_proc;
	}
	else
	{
	    bbclass->bulletin_board_class.focus_moved_proc =
		_XmBulletinBoardFocusMoved;
	}
    }

    _XmFastSubclassInit(widget_class, XmBULLETIN_BOARD_BIT);
}

static void
initialize(Widget request, Widget new_w, ArgList args, Cardinal *num_args)
{
    XmBulletinBoardWidget bb = (XmBulletinBoardWidget)new_w;
    XmBulletinBoardWidgetClass bbclass;

    DEBUGOUT(_LtDebug(__FILE__, new_w, "BB Initialize LabelFontList %p ButtonFontList %p\n",
			    BB_LabelFontList(bb), BB_ButtonFontList(bb)));

        if (!BB_LabelFontList(bb)) {
		BB_LabelFontList(bb) = _XmGetDefaultFontList(new_w, XmLABEL_FONTLIST);
        } else {
		BB_LabelFontList(bb) = XmFontListCopy(BB_LabelFontList(new_w));
	}

	if (!BB_ButtonFontList(bb)) {
		BB_ButtonFontList(bb) = _XmGetDefaultFontList(new_w, XmBUTTON_FONTLIST);
	} else {
		BB_ButtonFontList(bb) = XmFontListCopy(BB_ButtonFontList(new_w));
	}

	if (!BB_TextFontList(bb)) {
		BB_TextFontList(bb) = _XmGetDefaultFontList(new_w, XmTEXT_FONTLIST);
	} else {
		BB_TextFontList(bb) = XmFontListCopy(BB_TextFontList(new_w));
	}

    if (((XmBulletinBoardWidgetClass)XtClass(new_w))->bulletin_board_class.always_install_accelerators)
    {
	XtAugmentTranslations(new_w, mapTrans);
	XtAugmentTranslations(new_w, defTrans);
    }
    /* rws 30 Aug 1998
       We seem to loose a few hundred bytes every time we parse a translation
       table, so like mapTrans lets parse it in class_initialize and use the
       parsed version here.
    XtAugmentTranslations(new_w,
		  XtParseTranslationTable(_XmBulletinB_defaultTranslations));
		  */

    if (XmIsDialogShell(XtParent(new_w)))
    {
	DEBUGOUT(_LtDebug(__FILE__, new_w, "Init(%s)\n",
		_LtDebugDialogStyle2String(BB_DialogStyle(new_w))));
	switch (BB_DialogStyle(new_w))
	{
	case XmDIALOG_MODELESS:
	    XtVaSetValues(XtParent(new_w), XmNmwmInputMode,
			  MWM_INPUT_MODELESS, NULL);
	    break;

	case XmDIALOG_PRIMARY_APPLICATION_MODAL:
	    XtVaSetValues(XtParent(new_w), XmNmwmInputMode,
			  MWM_INPUT_PRIMARY_APPLICATION_MODAL, NULL);
	    break;

	case XmDIALOG_FULL_APPLICATION_MODAL:
	    XtVaSetValues(XtParent(new_w), XmNmwmInputMode,
			  MWM_INPUT_FULL_APPLICATION_MODAL, NULL);
	    break;

	case XmDIALOG_SYSTEM_MODAL:
	    XtVaSetValues(XtParent(new_w), XmNmwmInputMode,
			  MWM_INPUT_SYSTEM_MODAL, NULL);
	    break;
	}
	/*
	 * --aldi: Better do the realize call of our parent -- which is a
	 * shell -- first here. Otherwise we have *real trouble* with
	 * the grab mechanism. This replaces the call to XtRealizeWidget()
	 * within the initialize_posthook() method of the BulletinBoard
	 * class. And it does get us rid off that %&!$ double installed
	 * grabs when using a dialog shell.
	 */
	XtRealizeWidget(XtParent(new_w));

	/*
	 * Shouldn't this be in synthetic resource handlers?
	 *
	 * MLM: No.  The bottom line is that the resource converter can
	 * bypass the arglist passed to initialize; this means that it
	 * makes just as much sense to catch this here as it does in a
	 * Synth resource handler, and duplicating the work here.  This
	 * little jewel of knowledge came from Danny and I working out the
	 * rules for internal strings, as applicable for Labels.
	 */
	if (BB_DialogTitle(new_w))
	{
	    char *p;

	    if ((p = _XmStringGetTextConcat(BB_DialogTitle(new_w))))
	    {
		XtVaSetValues(XtParent(new_w), XtNtitle, p, NULL);
		XtFree(p);
	    }

	    BB_DialogTitle(new_w) = XmStringCopy(BB_DialogTitle(new_w));
	}
    }

    if (XtIsSubclass(XtParent(new_w), xmDialogShellWidgetClass) ||
	XtIsSubclass(XtParent(new_w), vendorShellWidgetClass))
    {
	if (MGR_ShadowThickness(new_w) == 0)
	{
	    MGR_ShadowThickness(new_w) = 1;
	}
    }

    BB_DynamicDefaultButton(new_w) = NULL;
    BB_DynamicCancelButton(new_w) = NULL;

    bbclass = (XmBulletinBoardWidgetClass)XtClass(new_w);
    if (bbclass->bulletin_board_class.focus_moved_proc)
    {
	Widget ve;

	if ((ve = _LtFindVendorExt(new_w)) != NULL)
	{
	    _XtAddCallback(&VSEP_FocusMovedCallback(ve),
			   bbclass->bulletin_board_class.focus_moved_proc,
                           (XtPointer)new_w);
	}
    }

    /* initialize these to values that aren't possible */
    BB_OldWidth(new_w) = -1;
    BB_OldHeight(new_w) = -1;
    BB_OldShadowThickness(new_w) = 0; /* better than uninitialized */

    BB_GeoCache(new_w) = NULL;
    MGR_InitialFocus(new_w) = BB_DefaultButton(new_w);
    BB_InSetValues(new_w) = False;
}

static void
destroy(Widget w)
{
  Widget ve;

    DEBUGOUT(_LtDebug("RWS", w,"%s:destroy(%d)\n",
    	__FILE__, __LINE__
    	));

    if (BB_DialogTitle(w))
        XmStringFree(BB_DialogTitle(w));
    XmFontListFree(BB_LabelFontList(w));
    XmFontListFree(BB_ButtonFontList(w));
    XmFontListFree(BB_TextFontList(w));
    if ((ve = _LtFindVendorExt(w)) != NULL)
      {
	_XtRemoveAllCallbacks(&VSEP_FocusMovedCallback(ve));
	/* mitch
	XtRemoveAllCallbacks(ve,XmNfocusMovedCallback);
	*/
      }
    

}

static Boolean
set_values(Widget old,
	   Widget request,
	   Widget new_w,
	   ArgList args,
	   Cardinal *num_args)
{
    XmBulletinBoardWidget ow = (XmBulletinBoardWidget)old;
    XmBulletinBoardWidget nw = (XmBulletinBoardWidget)new_w;
    Boolean need_refresh = False;

    DEBUGOUT(_LtDebug(__FILE__, new_w,
		      "%s(%d):set_values - %i args\n"
		      "\t    old X %5i Y %5i W %5i H %5i\n"
		      "\trequest X %5i Y %5i W %5i H %5i\n"
		      "\t  new_w X %5i Y %5i W %5i H %5i\n",
		      __FILE__, __LINE__,
		      *num_args,
		      XtX(old), XtY(old),
		      XtWidth(old), XtHeight(old),
		      XtX(request), XtY(request),
		      XtWidth(request), XtHeight(request),
		      XtX(new_w), XtY(new_w),
		      XtWidth(new_w), XtHeight(new_w)));
    DEBUGOUT(_LtDebugPrintArgList(__FILE__, new_w, args, *num_args, False));

    BB_InSetValues(new_w) = True;

    if (BB_DefaultButton(ow) != BB_DefaultButton(nw))
    {
	Cardinal i;

	/* the default button has changed, so set the old one's showAsDefault
	   to 0, and make the new one display as the default. */

	for (i = 0; i < MGR_NumChildren(new_w); i++)
	{
	    _XmBulletinBoardSetDefaultShadow(MGR_Children(new_w)[i]);
	}

	_XmBBUpdateDynDefaultButton(new_w);

	MGR_InitialFocus(new_w) = BB_DefaultButton(nw);
	need_refresh = True;	/* do we need to ? */
    }

    if (XmIsDialogShell(XtParent(new_w)))
    {
	/*
	 * Shouldn't this be in synthetic resource handlers ?
	 * MLM 960524 not according to a dump of the synthetics.  Only an
	 * export proc is given, to copy the existing title.
	 */
	if (!XmStringCompare(BB_DialogTitle(old), BB_DialogTitle(new_w)))
	{
	    char *p;

	    if ((p = _XmStringGetTextConcat(BB_DialogTitle(new_w))))
	    {
		XtVaSetValues(XtParent(new_w), XtNtitle, p, NULL);
		XtFree(p);
		BB_DialogTitle(new_w) = XmStringCopy(BB_DialogTitle(new_w));
	    }

            XmStringFree(BB_DialogTitle(old));
        }
        else if (BB_DialogTitle(old) != BB_DialogTitle(new_w))
        {
            BB_DialogTitle(new_w) = XmStringCopy(BB_DialogTitle(new_w));
	    XmStringFree(BB_DialogTitle(old));
	}

#if 0
	/*
	 * If we're a dialog, refuse to be placed away from 0,0
	 */
	/* rws 23 Jan 1999
	   Na, let DialogShell handle the geometry request that Xt will make.
	 */
	if (XtX(new_w) != 0)
	{
	    XtX(new_w) = 0;
	    need_refresh = True;
	}

	if (XtY(new_w) != 0)
	{
	    XtY(new_w) = 0;
	    need_refresh = True;
	}
#endif
    }

    if (XmIsDialogShell(XtParent(new_w)))
    {
	if (BB_DialogStyle(new_w) != BB_DialogStyle(old))
	{
	    DEBUGOUT(_LtDebug(__FILE__, new_w, "SetValues(%s)\n",
	    	_LtDebugDialogStyle2String(BB_DialogStyle(new_w))));

	    switch (BB_DialogStyle(new_w))
	    {
	    case XmDIALOG_MODELESS:
		XtVaSetValues(XtParent(new_w), XmNmwmInputMode,
			      MWM_INPUT_MODELESS, NULL);
		break;

	    case XmDIALOG_PRIMARY_APPLICATION_MODAL:
		XtVaSetValues(XtParent(new_w), XmNmwmInputMode,
			      MWM_INPUT_PRIMARY_APPLICATION_MODAL, NULL);
		break;

	    case XmDIALOG_FULL_APPLICATION_MODAL:
		XtVaSetValues(XtParent(new_w), XmNmwmInputMode,
			      MWM_INPUT_FULL_APPLICATION_MODAL, NULL);
		break;

	    case XmDIALOG_SYSTEM_MODAL:
		XtVaSetValues(XtParent(new_w), XmNmwmInputMode,
			      MWM_INPUT_SYSTEM_MODAL, NULL);
		break;

	    }
	}
    }

    if (BB_LabelFontList(new_w) != BB_LabelFontList(old))
    {
	XmFontListFree(BB_LabelFontList(old));
	BB_LabelFontList(new_w) = XmFontListCopy(BB_LabelFontList(new_w));
    }

    if (BB_ButtonFontList(new_w) != BB_ButtonFontList(old))
    {
	XmFontListFree(BB_ButtonFontList(old));
	BB_ButtonFontList(new_w) = XmFontListCopy(BB_ButtonFontList(new_w));
    }

    if (BB_TextFontList(new_w) != BB_TextFontList(old))
    {
	XmFontListFree(BB_TextFontList(old));
	BB_TextFontList(new_w) = XmFontListCopy(BB_TextFontList(new_w));
    }

    BB_InSetValues(new_w) = False;

    if (XtWidth(new_w) != XtWidth(old) || XtHeight(new_w) != XtHeight(old))
    {
	need_refresh = True;
    }

    if (need_refresh == True && XtClass(new_w) == xmBulletinBoardWidgetClass)
    {
	_XmBulletinBoardSizeUpdate(new_w);
	return False;
    }

    return need_refresh;
}

static XtGeometryResult
query_geometry(Widget w, XtWidgetGeometry *proposed, XtWidgetGeometry *answer)
{
    XmBulletinBoardWidgetClass bbc = (XmBulletinBoardWidgetClass)XtClass(w);
    XtGeometryResult res;

    DEBUGOUT(_LtDebug(__FILE__, w, "%s:query_geometry(%d) - proposed %s\n",
    		__FILE__, __LINE__,
    		_LtDebugWidgetGeometry2String(proposed)));

    if (bbc->bulletin_board_class.geo_matrix_create)
    {
	res = _XmHandleQueryGeometry(w, proposed, answer, BB_ResizePolicy(w),
				  bbc->bulletin_board_class.geo_matrix_create);

	return res;
    }

    res = _XmGMHandleQueryGeometry(w, proposed, answer,
				   BB_MarginWidth(w), BB_MarginHeight(w),
				   BB_ResizePolicy(w));

    DEBUGOUT(_LtDebug(__FILE__, w,
		      "BB wants %d %d\n", answer->width, answer->height));

    return res;
}

static void
handle_resize(Widget w, XmGeoCreateProc mat_make)
{
    Dimension wd, ht;
    XmGeoMatrix geo;

    wd = XtWidth(w);
    ht = XtHeight(w);

    geo = mat_make(w, NULL, NULL);

    _XmGeoMatrixGet(geo, XmGET_PREFERRED_SIZE);

    _XmGeoArrangeBoxes(geo, 0, 0, &wd, &ht);

    _XmGeoMatrixSet(geo);

    if (XtIsRealized(w))
    {
	_XmClearShadowType(w, BB_OldWidth(w), BB_OldHeight(w),
			   BB_OldShadowThickness(w), 0);

	_XmDrawShadows(XtDisplay(w), XtWindow(w),
		       MGR_TopShadowGC(w), MGR_BottomShadowGC(w),
		       0, 0, XtWidth(w), XtHeight(w),
		       MGR_ShadowThickness(w), BB_ShadowType(w));
    }

    _XmGeoMatrixFree(geo);

    /*
    BB_OldWidth(w) = XtWidth(w);
    BB_OldHeight(w) = XtHeight(w);
    BB_OldShadowThickness(w) = MGR_ShadowThickness(w);
    */
}

static void
resize(Widget w)
{
    XmBulletinBoardClassRec *bb = (XmBulletinBoardClassRec *)XtClass(w);

    DEBUGOUT(_LtDebug(__FILE__, w, "resize\n"));

    if (bb->bulletin_board_class.geo_matrix_create)
    {
	handle_resize(w, bb->bulletin_board_class.geo_matrix_create);

	return;
    }

    _XmGMEnforceMargin(w,
		       BB_MarginWidth(w), BB_MarginHeight(w),
		       False);

#if 0
    _XmClearShadowType(w, BB_OldWidth(w), BB_OldHeight(w),
		       BB_OldShadowThickness(w), 0);

    BB_OldShadowThickness(w) = 0;
#endif

#if 0
    /* rws 21 Jul 1998
       _XmGMDoLayout will request a new size from the parent if it wants to.
       This is bad in the resize method, we have to live with the size we
       are given!  Since BulletinBoard, without a GeoMatrix proc, doesn't
       layout much of anything, let's just skip this and see what happens.
       This should not affect any of the standard dialogs since they all
       use the GeoMatirx. ( test/Xm/pushbg/test8 )
     */
    if (XtIsRealized(w) || XtWidth(w) == 0 || XtHeight(w) == 0)
    {
	_XmGMDoLayout(w, BB_MarginWidth(w), BB_MarginHeight(w),
		      BB_ResizePolicy(w), True);
    }
#endif

#if 0
    if ((XtWidth(w) < BB_OldWidth(w) || XtHeight(w) < BB_OldHeight(w)) &&
	XtIsRealized(w))
    {
	_XmDrawShadows(XtDisplay(w), XtWindow(w),
		       MGR_TopShadowGC(w), MGR_BottomShadowGC(w),
		       0, 0, XtWidth(w), XtHeight(w),
		       MGR_ShadowThickness(w), BB_ShadowType(w));
    }

    BB_OldWidth(w) = XtWidth(w);
    BB_OldHeight(w) = XtHeight(w);
    BB_OldShadowThickness(w) = MGR_ShadowThickness(w);
#endif
}

/*
 * handle subclasses that use the GeoCache
 */
static XtGeometryResult
handle_geometry_manager(Widget w,
			XtWidgetGeometry *desired, XtWidgetGeometry *allowed,
			XmGeoCreateProc mat_make)
{
    Widget bb = XtParent(w);
    XtGeometryResult res;

    DEBUGOUT(_LtDebug2(__FILE__, bb, w,
		       "handle_geometry_manager\n\tdesired %s\n",
		       	_LtDebugWidgetGeometry2String(desired)));

    if (!(desired->request_mode & (CWWidth | CWHeight)))
    {
	return XtGeometryYes;
    }

    if (BB_OldShadowThickness(bb) != 0 ||
	BB_ResizePolicy(bb) != XmRESIZE_NONE)
    {
	_XmClearShadowType(bb, BB_OldWidth(bb), BB_OldHeight(bb),
			   BB_OldShadowThickness(bb), 0);
	BB_OldShadowThickness(bb) = 0;
    }

    res = _XmHandleGeometryManager(bb, w, desired, allowed,
				   BB_ResizePolicy(bb), &BB_GeoCache(bb),
				   mat_make);

    if (!BB_InSetValues(bb) ||
	XtWidth(bb) > BB_OldWidth(bb) || XtHeight(bb) > BB_OldHeight(bb))
    {
	if (XtIsRealized(bb))
	{
	    _XmClearBorder(XtDisplay(bb), XtWindow(bb),
		       0,0,
			BB_OldWidth(bb), BB_OldHeight(bb),
			BB_OldShadowThickness(bb));
	    _XmDrawShadows(XtDisplay(bb), XtWindow(bb),
			   MGR_TopShadowGC(bb), MGR_BottomShadowGC(bb),
			   0, 0, XtWidth(bb), XtHeight(bb),
			   MGR_ShadowThickness(bb), BB_ShadowType(bb));
	}
    }

    BB_OldWidth(bb) = XtWidth(bb);
    BB_OldHeight(bb) = XtHeight(bb);
    BB_OldShadowThickness(bb) = MGR_ShadowThickness(bb);

    return res;
}

/*
 * Geometry Manager is always called by a child of BB.
 *      It asks to get a different geometry. We may allow this.
 *      Also we may have to resize ourself because of this.
 */
static XtGeometryResult
geometry_manager(Widget w, XtWidgetGeometry *desired, XtWidgetGeometry *allowed)
{
    Widget bb = XtParent(w);
    XmBulletinBoardWidgetClass bbc = (XmBulletinBoardWidgetClass)XtClass(bb);

    DEBUGOUT(_LtDebug2(__FILE__, bb, w, "GeometryManager\n"));

    if (bbc->bulletin_board_class.geo_matrix_create)
    {
	return handle_geometry_manager(w, desired, allowed,
				  bbc->bulletin_board_class.geo_matrix_create);
    }

    return _XmGMHandleGeometryManager(bb, w, desired, allowed,
				      BB_MarginWidth(bb), BB_MarginHeight(bb),
				      BB_ResizePolicy(bb), BB_AllowOverlap(bb));
}

static void
handle_change_managed(Widget w, XmGeoCreateProc mat_make)
{
    Dimension wd, ht;
    XmGeoMatrix geo;
    XtWidgetGeometry request;

    if (!XtIsRealized(w))
    {
	/* rws 5 Apr 1998
	   If we are not realized use the width/height. This will be
	   the user specified size, or 0x0 if not specified.
	 */
#if 0
	wd = ht = 0;
#else
	wd = XtWidth(w);
	ht = XtHeight(w);
#endif
    }
    else if (BB_ResizePolicy(w) != XmNONE)
    {
	wd = ht = 0;
    }
    else
    {
	wd = XtWidth(w);
	ht = XtHeight(w);
    }

    geo = mat_make(w, NULL, NULL);

    _XmGeoMatrixGet(geo, XmGET_PREFERRED_SIZE);

    _XmGeoArrangeBoxes(geo, 0, 0, &wd, &ht);

    if (BB_ResizePolicy(w) == XmRESIZE_GROW)
    {
	/* check the return against the original.  If the procedure would
	 * like the BB to shrink, call again */
	if (wd < XtWidth(w) || ht < XtHeight(w))
	{
	    wd = XtWidth(w);
	    ht = XtHeight(w);
	    _XmGeoArrangeBoxes(geo, 0, 0, &wd, &ht);
	}
    }

    if (wd == XtWidth(w) && ht == XtHeight(w))
    {
	/* rws 16 Aug 1998
	   Ok, so the BB hasn't changed size so we do not need to make the
	   geo request but, the kids may not have been positioned yet, see
	   messagebox/test14.  So, let's always do the set.
	 */
	_XmGeoMatrixSet(geo);
	_XmGeoMatrixFree(geo);
	_XmNavigChangeManaged(w);

	return;
    }

    request.request_mode = (CWWidth | CWHeight);
    request.width = wd;
    request.height = ht;
    request.request_mode |= CWBorderWidth;
    request.border_width = XtBorderWidth(w);

    {
    XtGeometryResult res;

	res = _XmMakeGeometryRequest(w, &request);
	DEBUGOUT(_LtDebug(__FILE__, w, "handle_change_managed request %s %s\n",
		_LtDebugWidgetGeometry2String(&request),
		_LtDebugGeometryResult2String(res)));
    	if (res == XtGeometryNo)
    	{
	    /* rws 16 Aug 1998
	       If the parent says NO we must re-layout the kids, since they
	       are laid out to their preferred places at this point.
	       (messagebox/test13)
	     */
	    request.width = XtWidth(w);
	    request.height = XtHeight(w);
    	}
    }

    if (request.width != wd || request.height != ht)
    {
	_XmGeoArrangeBoxes(geo, 0, 0, &request.width, &request.height);
    }

    _XmGeoMatrixSet(geo);

    if (XtIsRealized(w))
    {
	_XmClearShadowType(w, BB_OldWidth(w), BB_OldHeight(w),
			   BB_OldShadowThickness(w), 0);

	_XmDrawShadows(XtDisplay(w), XtWindow(w),
		       MGR_TopShadowGC(w), MGR_BottomShadowGC(w),
		       0, 0, XtWidth(w), XtHeight(w),
		       MGR_ShadowThickness(w), BB_ShadowType(w));
    }

    _XmGeoMatrixFree(geo);

    BB_OldWidth(w) = XtWidth(w);
    BB_OldHeight(w) = XtHeight(w);
    BB_OldShadowThickness(w) = MGR_ShadowThickness(w);

    _XmNavigChangeManaged(w);
}

static void
change_managed(Widget w)
{
	XmBulletinBoardClassRec *bb = (XmBulletinBoardClassRec *)XtClass(w);

	/*
	 * Try to get the MODAL behaviour rolling.
	 */
	DEBUGOUT(_LtDebug(__FILE__, w, "BB-ChangeManaged(%s)\n",
		_LtDebugDialogStyle2String(BB_DialogStyle(w))));

	switch (BB_DialogStyle(w))
	{
	case XmDIALOG_MODELESS:
	    XtVaSetValues(XtParent(w), XmNmwmInputMode,
			  MWM_INPUT_MODELESS, NULL);
	    break;

	case XmDIALOG_PRIMARY_APPLICATION_MODAL:
	    XtVaSetValues(XtParent(w), XmNmwmInputMode,
			  MWM_INPUT_PRIMARY_APPLICATION_MODAL, NULL);
	    break;

	case XmDIALOG_FULL_APPLICATION_MODAL:
	    XtVaSetValues(XtParent(w), XmNmwmInputMode,
			  MWM_INPUT_FULL_APPLICATION_MODAL, NULL);
	    break;

	case XmDIALOG_SYSTEM_MODAL:
	    XtVaSetValues(XtParent(w), XmNmwmInputMode,
			  MWM_INPUT_SYSTEM_MODAL, NULL);
	    break;
	}

	if (bb->bulletin_board_class.geo_matrix_create) {
		handle_change_managed(w, bb->bulletin_board_class.geo_matrix_create);
		return;
	}

	_XmGMEnforceMargin(w, BB_MarginWidth(w), BB_MarginHeight(w), False);

	_XmClearShadowType(w, BB_OldWidth(w), BB_OldHeight(w), BB_OldShadowThickness(w), 0);

	BB_OldShadowThickness(w) = 0;

	/* mainw/test13 */
	if (/* XtIsRealized(w) || */ XtWidth(w) == 0 || XtHeight(w) == 0) {
		_XmGMDoLayout(w, BB_MarginWidth(w), BB_MarginHeight(w),
			BB_ResizePolicy(w), False);
	}

	if ((XtWidth(w) < BB_OldWidth(w) || XtHeight(w) < BB_OldHeight(w)) &&
			XtIsRealized(w)) {
		_XmDrawShadows(XtDisplay(w), XtWindow(w),
			MGR_TopShadowGC(w), MGR_BottomShadowGC(w),
			0, 0, XtWidth(w), XtHeight(w),
			MGR_ShadowThickness(w), BB_ShadowType(w));
	}

	BB_OldWidth(w) = XtWidth(w);
	BB_OldHeight(w) = XtHeight(w);
	BB_OldShadowThickness(w) = MGR_ShadowThickness(w);

	_XmNavigChangeManaged(w);
}

Widget
XmCreateBulletinBoard(Widget parent, char *name,
		      Arg *arglist, Cardinal argcount)
{
    return XtCreateWidget(name,
			  xmBulletinBoardWidgetClass,
			  parent,
			  arglist,
			  argcount);
}

Widget
XmCreateBulletinBoardDialog(Widget parent, char *name,
			    Arg *arglist, Cardinal argcount)
{
    Widget d;
    char *s;

    s = _XmMakeDialogName(name);

    d = XtCreateWidget(s, xmDialogShellWidgetClass, parent, arglist, argcount);
    XtFree(s);

    return XtCreateWidget(name,
			  xmBulletinBoardWidgetClass,
			  d,
			  arglist, argcount);
}

#if 0
static void
constraint_initialize(Widget request, Widget new_w, Arg *args, Cardinal *nargs)
{
    DEBUGOUT(_LtDebug(__FILE__, new_w, "constraint_initialize\n"));
    DEBUGOUT(_LtDebug2(__FILE__, XtParent(new_w), new_w,
		       "_XmBulletinBoardconstraint_initialize: %i args\n\t"
		       "request X %5i Y %5i W %5i H %5i\n\t  new_w X %5i Y %5i"
		       " W %5i H %5i\n\t   BB W %5i H %5i\n",
		       *nargs,
		       XtX(request), XtY(request),
		       XtWidth(request), XtHeight(request),
		       XtX(new_w), XtY(new_w),
		       XtWidth(new_w), XtHeight(new_w),
		       XtWidth(XtParent(new_w)), XtHeight(XtParent(new_w))));

    DEBUGOUT(_LtDebugPrintArgList(__FILE__, new_w, args, *nargs, False));

    if (XtWidth(request) != 0)
    {
	XtWidth(new_w) = XtWidth(request);
    }
    if (XtHeight(request) != 0)
    {
	XtHeight(new_w) = XtHeight(request);
    }
}

static Boolean
constraint_set_values(Widget old, Widget request, Widget new_w,
		      Arg *args, Cardinal *nargs)
{
    DEBUGOUT(_LtDebug2(__FILE__, XtParent(new_w), new_w,
		       "constraint_set_values\n"));
    DEBUGOUT(_LtDebug2(__FILE__, XtParent(new_w), new_w,
		       "_XmBulletinBoardconstraint_set_values: %i args\n\t"
		       "current X %5i Y %5i W %5i H %5i\n\trequest X %5i Y %5i"
		       " W %5i H %5i\n\t  new_w X %5i Y %5i W %5i H %5i\n\t"
		       "  BB W %5i H %5i\n",
		       *nargs,
		       XtX(old), XtY(old),
		       XtWidth(old), XtHeight(old),
		       XtX(request), XtY(request),
		       XtWidth(request), XtHeight(request),
		       XtX(new_w), XtY(new_w),
		       XtWidth(new_w), XtHeight(new_w),
		       XtWidth(XtParent(new_w)), XtHeight(XtParent(new_w))));
    DEBUGOUT(_LtDebugPrintArgList(__FILE__, new_w, args, *nargs, False));

    return False;
}
#endif

#if 0
static void
handle_realize(Widget w, XmGeoCreateProc mat_make)
{
    Dimension wd, ht;
    XmGeoMatrix geo;
    XtWidgetGeometry request;

    wd = XtWidth(w);
    ht = XtHeight(w);

    geo = mat_make(w, NULL, NULL);

    _XmGeoMatrixGet(geo, XmGET_PREFERRED_SIZE);

    _XmGeoArrangeBoxes(geo, 0, 0, &wd, &ht);

    if (BB_ResizePolicy(w) == XmRESIZE_GROW)
    {
	/* check the return against the original.  If the procedure would
	 * like the BB to shrink, call again */
	if (wd < XtWidth(w) || ht < XtHeight(w))
	{
	    wd = XtWidth(w);
	    ht = XtHeight(w);
	    _XmGeoArrangeBoxes(geo, 0, 0, &wd, &ht);
	}
    }

    if (wd == XtWidth(w) && ht == XtHeight(w))
    {
	_XmGeoMatrixFree(geo);

	return;
    }

    request.request_mode = (CWWidth | CWHeight);
    request.width = wd;
    request.height = ht;
    request.request_mode |= CWBorderWidth;
    request.border_width = XtBorderWidth(w);

    _XmMakeGeometryRequest(w, &request);

    if (request.width != wd || request.height != ht)
    {
	_XmGeoArrangeBoxes(geo, 0, 0, &request.width, &request.height);
    }

    _XmGeoMatrixSet(geo);

    if (XtIsRealized(w))
    {

	_XmClearShadowType(w, BB_OldWidth(w), BB_OldHeight(w),
			   BB_OldShadowThickness(w), 0);

	_XmDrawShadows(XtDisplay(w), XtWindow(w),
		       MGR_TopShadowGC(w), MGR_BottomShadowGC(w),
		       0, 0, XtWidth(w), XtHeight(w),
		       MGR_ShadowThickness(w), BB_ShadowType(w));
    }

    _XmGeoMatrixFree(geo);

    BB_OldWidth(w) = XtWidth(w);
    BB_OldHeight(w) = XtHeight(w);
    BB_OldShadowThickness(w) = MGR_ShadowThickness(w);
}
#endif

#if 0
static void
realize(Widget w,
	XtValueMask *value_mask,
	XSetWindowAttributes *attributes)
{
    /* Motif inherits this method */
    XmBulletinBoardClassRec *bb = (XmBulletinBoardClassRec *)XtClass(w);

#if 1
    /* bulletinboard/test11 */
    if (XtWidth(w) == 0) XtWidth(w) = 1;
    if (XtHeight(w) == 0) XtHeight(w) = 1;
#endif

    DEBUGOUT(_LtDebug(__FILE__, w, "%s:realize(%d) - %dx%d\n",
    	__FILE__, __LINE__,
    	XtWidth(w), XtHeight(w)));

#define superclass (&xmManagerClassRec)
    (*superclass->core_class.realize) (w, value_mask, attributes);
#undef superclass

    if (bb->bulletin_board_class.geo_matrix_create)
    {
	handle_realize(w, bb->bulletin_board_class.geo_matrix_create);

	return;
    }

#if 0
    /* rws 21 Oct 1998
       This should not be necessary. All this should get done in change_managed.
       (bulletinboard/test14)
     */
    _XmGMEnforceMargin(w,
		       BB_MarginWidth(w), BB_MarginHeight(w),
		       False);

    _XmClearShadowType(w, BB_OldWidth(w), BB_OldHeight(w),
		       BB_OldShadowThickness(w), 0);

    BB_OldShadowThickness(w) = 0;

    _XmGMDoLayout(w, BB_MarginWidth(w), BB_MarginHeight(w),
		  BB_ResizePolicy(w), False);

    if (XtWidth(w) < BB_OldWidth(w) || XtHeight(w) < BB_OldHeight(w))
    {
	_XmDrawShadows(XtDisplay(w), XtWindow(w),
		       MGR_TopShadowGC(w), MGR_BottomShadowGC(w),
		       0, 0, XtWidth(w), XtHeight(w),
		       MGR_ShadowThickness(w), BB_ShadowType(w));
    }

    BB_OldWidth(w) = XtWidth(w);
    BB_OldHeight(w) = XtHeight(w);
    BB_OldShadowThickness(w) = MGR_ShadowThickness(w);
#endif
}
#endif

static void
expose(Widget w, XEvent *event, Region region)
{
    DEBUGOUT(_LtDebug(__FILE__, w, "expose\n"));

    if (BB_OldWidth(w) != XtWidth(w) ||
        BB_OldHeight(w) != XtHeight(w) ||
        BB_OldShadowThickness(w) != MGR_ShadowThickness(w))
    {
	_XmClearBorder(XtDisplay(w), XtWindow(w),
		       0,0,
			BB_OldWidth(w), BB_OldHeight(w),
			BB_OldShadowThickness(w));
	BB_OldWidth(w) = XtWidth(w);
	BB_OldHeight(w) = XtHeight(w);
	BB_OldShadowThickness(w) = MGR_ShadowThickness(w);
    }

    _XmRedisplayGadgets(w, event, region);

    if (MGR_ShadowThickness(w))
    {
	_XmDrawShadows(XtDisplay(w), XtWindow(w),
		       MGR_TopShadowGC(w), MGR_BottomShadowGC(w),
		       0, 0, XtWidth(w), XtHeight(w),
		       MGR_ShadowThickness(w), BB_ShadowType(w));
    }
}

static void
_XmBbButton(Widget w, XtPointer client, XtPointer call)
{
    XmBulletinBoardWidget bb = (XmBulletinBoardWidget)XtParent(w);

    DEBUGOUT(_LtDebug2(__FILE__, (Widget)bb, w, "XmBbButton\n"));

    /*
     * How do we make sure the APPLY or HELP buttons don't
     * trigger unmanaging ??  
     *
     * Somebody already caught this.  See comments in SelectionBox.c (look
     * for auto_unmanage).
     */
    if (BB_AutoUnmanage(bb))
    {
	Widget s = XtParent((Widget)bb);

	if (XtIsSubclass(s, xmDialogShellWidgetClass))
	{
	    XtUnmanageChild((Widget)bb);
	    DEBUGOUT(_LtDebug2(__FILE__, (Widget)bb, w, "AutoUnmanage\n"));

#if 0
	    /* rws 19 Sep 1998
	       This is a job for Shell, not BB
	     */
	    /* XtNpopdownCallback */
	    XtCallCallbackList(s, Shell_PopdownCallback(s), call);
#endif
	}
    }
}

/*
 * Keep track of button children
 */
static void
insert_child(Widget w)
{
#define	superclass	(&xmManagerClassRec)
    (*superclass->composite_class.insert_child) (w);
#undef	superclass

    if (_XmIsFastSubclass(XtClass(w), XmPUSH_BUTTON_GADGET_BIT) ||
	_XmIsFastSubclass(XtClass(w), XmPUSH_BUTTON_BIT))
    {
	XtAddCallback(w, XmNactivateCallback, _XmBbButton, NULL);

	if (BB_DefaultButton(XtParent(w)))
	{
	    _XmBulletinBoardSetDefaultShadow(w);
	}
    }
}

static void
delete_child(Widget w)
{
    Widget bb = XtParent(w);

    DEBUGOUT(_LtDebug2(__FILE__, bb, w, "delete_child\n"));

#define	superclass	(&xmManagerClassRec)
    (*superclass->composite_class.delete_child) (w);
#undef superclass

    if (w == BB_CancelButton(bb))
    {
	BB_CancelButton(bb) = NULL;
    }
    if (w == BB_DynamicCancelButton(bb))
    {
	BB_DynamicCancelButton(bb) = NULL;
    }
    if (w == BB_DynamicDefaultButton(bb))
    {
	BB_DynamicDefaultButton(bb) = NULL;
    }
    if (w == BB_DefaultButton(bb))
    {
	BB_DefaultButton(bb) = NULL;
	MGR_InitialFocus(bb) = NULL;
    }
}

static Boolean
_XmBBParentProcess(Widget widget, XmParentProcessData data)
{
    DEBUGOUT(_LtDebug(__FILE__, widget, "_XmBBParentProcess\n"));

    if (data->input_action.process_type == XmINPUT_ACTION)
    {
	if (data->input_action.action == XmPARENT_ACTIVATE)
	{
	    if (BB_DefaultButton(widget) || BB_DynamicDefaultButton(widget))
	    {
		_XmBulletinBoardReturn(widget,
				   data->input_action.event,
				   data->input_action.params,
				   data->input_action.num_params);

		return True;
	    }
	    else
	    {
	    Widget w = XtParent(widget);
	    XmManagerWidgetClass mwc = (XmManagerWidgetClass)w->core.widget_class;

		if (XmIsManager(w) && mwc->manager_class.parent_process)
		{
		    return((*mwc->manager_class.parent_process) (w, data));
		}
	    }
	}
	else if (data->input_action.action == XmPARENT_CANCEL)
	{
	    DEBUGOUT(_LtDebug(__FILE__, widget, "_XmBBParentProcess CANCEL\n"));
	    if (BB_CancelButton(widget))
	    {
		DEBUGOUT(_LtDebug(__FILE__, widget, "_XmBBParentProcess CANCEL %s\n",XtName(BB_CancelButton(widget))));
		_XmBulletinBoardCancel(widget,
				       data->input_action.event,
				       data->input_action.params,
				       data->input_action.num_params);

		return True;
	    }
	    else
	    {
	    Widget w = XtParent(widget);
	    XmManagerWidgetClass mwc = (XmManagerWidgetClass)w->core.widget_class;

		DEBUGOUT(_LtDebug(__FILE__, widget, "_XmBBParentProcess CANCEL no button\n"));
		if (XmIsManager(w) && mwc->manager_class.parent_process)
		{
		    return((*mwc->manager_class.parent_process) (w, data));
		}
	    }
	}
    }

    return False;
}

Widget
_XmBB_CreateButtonG(Widget bb, XmString l_string, char *name)
{
    Widget button;
    Arg args[2];
    Cardinal n = 0;

    if (l_string && l_string != (XmString)XmUNSPECIFIED)
    {
	XtSetArg(args[n], XmNlabelString, l_string); n++;
    }
#ifdef USE_WIDGETS
    button = XmCreatePushButton(bb, name, args, n);
#else
    button = XmCreatePushButtonGadget(bb, name, args, n);
#endif
    _XmBulletinBoardSetDefaultShadow(button);

    return button;
}

Widget
_XmBB_CreateLabelG(Widget bb, XmString l_string, char *name)
{
    Widget label;
    Arg args[2];
    Cardinal n = 0;

    if (l_string && l_string != (XmString)XmUNSPECIFIED)
    {
	XtSetArg(args[n], XmNlabelString, l_string); n++;
    }

#ifdef USE_WIDGETS
    label = XmCreateLabel(bb, name, args, n);
#else
    label = XmCreateLabelGadget(bb, name, args, n);
#endif

    return label;
}

void
_XmBulletinBoardSizeUpdate(Widget w)
{
    XmBulletinBoardWidgetClass bbc = (XmBulletinBoardWidgetClass)XtClass(w);

    if (!XtIsRealized(w))
    {
	return;
    }

    if (bbc->bulletin_board_class.geo_matrix_create == NULL)
    {
	BB_OldWidth(w) = XtWidth(w);
	BB_OldHeight(w) = XtHeight(w);

	return;
    }

    if (!BB_OldShadowThickness(w) && BB_ResizePolicy(w) != XmRESIZE_NONE)
    {
	_XmClearShadowType(w, BB_OldWidth(w), BB_OldHeight(w),
			   BB_OldShadowThickness(w), 0);

	BB_OldShadowThickness(w) = 0;
    }

    _XmHandleSizeUpdate(w, BB_ResizePolicy(w),
			bbc->bulletin_board_class.geo_matrix_create);

    if ((XtWidth(w) < BB_OldWidth(w) || XtHeight(w) < BB_OldHeight(w)) &&
	XtIsRealized(w))
    {
	_XmDrawShadows(XtDisplay(w), XtWindow(w),
		       MGR_TopShadowGC(w), MGR_BottomShadowGC(w),
		       0, 0, XtWidth(w), XtHeight(w),
		       MGR_ShadowThickness(w), BB_ShadowType(w));
    }

    BB_OldWidth(w) = XtWidth(w);
    BB_OldHeight(w) = XtHeight(w);
    BB_OldShadowThickness(w) = MGR_ShadowThickness(w);
}

void
_XmBulletinBoardFocusMoved(Widget wid, XtPointer client_data, XtPointer data)
{
    XmFocusMovedCallback cbs = (XmFocusMovedCallback)data;
    Widget bb = (Widget)client_data;
    XmAnyCallbackStruct cb;
    Boolean to_bb = False, from_bb = False;
    Widget default_button = NULL;
    Widget par;

    DEBUGOUT(_LtDebug(__FILE__, wid, "%s:%d - %s %s %s\n",
    	__FILE__, __LINE__,
    	cbs->cont ? "True" : "False",
    	cbs->old_focus ? XtName(cbs->old_focus) : "NULL",
    	cbs->new_focus ? XtName(cbs->new_focus) : "NULL"));

    if (!cbs->cont)
    {
	return;
    }
    for (par = cbs->new_focus; par != NULL && !XtIsShell(par);
	 par = XtParent(par))
    {

	if (bb == par)
	{
	    to_bb = True;
	    break;
	}
	if (XmIsBulletinBoard(par))
	{
	    if (default_button == NULL)
	    {
		default_button = BB_DefaultButton(par);
	    }
	}
    }
    for (par = cbs->old_focus; par != NULL && !XtIsShell(par);
	 par = XtParent(par))
    {
	if (bb == par)
	{
	    from_bb = True;
	    break;
	}
    }
    if (to_bb)
    {
	if (default_button == NULL)
	{
	    default_button = BB_DefaultButton(bb);
	}
	if (default_button == NULL)
	{
	    BB_DynamicDefaultButton(bb) = NULL;
	}
	else
	{
	    par = cbs->new_focus;
	    if (XmIsPushButton(cbs->new_focus) ||
		XmIsPushButtonGadget(cbs->new_focus))
	    {
		_XmBulletinBoardSetDynDefaultButton(bb, cbs->new_focus);
	    }
	    else if (cbs->focus_policy == XmEXPLICIT ||
		     !XmIsManager(cbs->new_focus) ||
		     cbs->old_focus == NULL ||
		     (!XmIsPushButtonGadget(cbs->old_focus) &&
		      !XmIsPushButton(cbs->old_focus)))
	    {
		if (XtIsManaged(default_button))
		{
		    _XmBulletinBoardSetDynDefaultButton(bb, default_button);
		}
	    }
	}
	if (!from_bb)
	{
	    cb.reason = XmCR_FOCUS;
	    cb.event = cbs->event;
	    XtCallCallbackList(bb, BB_FocusCallback(bb), data);
	}
    }
    else
    {
	if (from_bb && cbs->new_focus != NULL)
	{
	    _XmBulletinBoardSetDynDefaultButton(bb, NULL);
	}
    }
}

void
_XmBulletinBoardUnmap(Widget w)
{
    XmAnyCallbackStruct cb;

    cb.event = NULL;
    cb.reason = XmCR_UNMAP;

    XtCallCallbackList(w, BB_UnmapCallback(w), (XtPointer)&cb);
}

void
_XmBulletinBoardMap(Widget w)
{
    XmAnyCallbackStruct cb;

    cb.event = NULL;
    cb.reason = XmCR_MAP;
    XtCallCallbackList(w, BB_MapCallback(w), (XtPointer)&cb);
}

static void
ArmAndActivate(Widget wid, XEvent *event,
		       String *params, Cardinal *numParams)
{
    if (XmIsGadget(wid))
    {
	if ((((XmGadgetClass)(XtClass(wid)))->gadget_class.arm_and_activate))
	{
	    (((XmGadgetClass)(XtClass(wid)))->gadget_class.arm_and_activate)
		(wid,
		    event,
		    params,
		    numParams);
	}
    }
    else if (XmIsPrimitive(wid))
    {
	if ((((XmPrimitiveWidgetClass)(XtClass(wid)))->primitive_class.arm_and_activate))
	{
	    (((XmPrimitiveWidgetClass)(XtClass(wid)))->primitive_class.arm_and_activate)
		(wid,
		    event,
		    params,
		    numParams);
	}
    }
}

void
_XmBulletinBoardReturn(Widget wid, XEvent *event,
		       String *params, Cardinal *numParams)
{
    /* FIX ME -- this needs filling in */
    /*
       rws 12 Jul 1997
       This is now filled in but, is it correct??
       see man XmBulletinBoard under KActivate
     */
    XmPushButtonCallbackStruct cbs;

    cbs.reason = XmCR_OK;
    cbs.event = event;

    if (BB_DynamicDefaultButton(wid) &&
	XtIsSensitive(BB_DynamicDefaultButton(wid)) &&
	XtIsManaged(BB_DynamicDefaultButton(wid)) &&
	XtIsRealized(BB_DynamicDefaultButton(wid)))
    {
	ArmAndActivate(BB_DynamicDefaultButton(wid), event, params, numParams);
    }
    else if (BB_DefaultButton(wid) &&
	XtIsSensitive(BB_DefaultButton(wid)) &&
	XtIsManaged(BB_DefaultButton(wid)) &&
	XtIsRealized(BB_DefaultButton(wid)))
    {
	ArmAndActivate(BB_DefaultButton(wid), event, params, numParams);
    }
}

void
_XmBulletinBoardCancel(Widget wid, XEvent *event,
		       String *params, Cardinal *numParams)
{
    XmPushButtonCallbackStruct cbs;

    /* see man XmBulletinBoard under KCancel */
    cbs.reason = XmCR_ACTIVATE;
    cbs.event = event;

    if (BB_CancelButton(wid) && XtIsSensitive(BB_CancelButton(wid)) &&
	XtIsManaged(BB_CancelButton(wid)) && XtIsRealized(BB_CancelButton(wid)))
    {
	ArmAndActivate(BB_CancelButton(wid), event, params, numParams);
    }
    else if (BB_DynamicCancelButton(wid) &&
	     XtIsSensitive(BB_DynamicCancelButton(wid)) &&
	     XtIsManaged(BB_DynamicCancelButton(wid)) &&
	     XtIsRealized(BB_DynamicCancelButton(wid)))
    {
	ArmAndActivate(BB_DynamicCancelButton(wid), event, params, numParams);
    }
}

void
_XmBulletinBoardSetDefaultShadow(Widget button)
{
    Dimension st, dbst;
    Arg args[2];

    if (_XmIsFastSubclass(XtClass(button), XmPUSH_BUTTON_GADGET_BIT) ||
	_XmIsFastSubclass(XtClass(button), XmPUSH_BUTTON_BIT))
    {

	XtSetArg(args[0], XmNdefaultButtonShadowThickness, &dbst);
	XtSetArg(args[1], XmNshadowThickness, &st);
	XtGetValues(button, args, 2);

	if (st > 1)
	{
	    st >>= 1;
	}

	XtSetArg(args[0], XmNdefaultButtonShadowThickness, st);
	XtSetValues(button, args, 1);
    }
}

void
_XmBulletinBoardSetDynDefaultButton(Widget wid, Widget newDefaultButton)
{
    Arg argl[2];

    DEBUGOUT(_LtDebug(__FILE__, wid, "%s:%d - %s\n",
    	__FILE__, __LINE__,
    	newDefaultButton ? XtName(newDefaultButton) : "NULL"));

    if (newDefaultButton && wid != XtParent(newDefaultButton))
    {
    	return;
    }
    if (BB_DynamicDefaultButton(wid) != NULL)
    {
	if (!BB_DynamicDefaultButton(wid)->core.being_destroyed)
	{
	    XtSetArg(argl[0], XmNshowAsDefault, 0);
	    XtSetValues(BB_DynamicDefaultButton(wid), argl, 1);
	}
    }

    if (newDefaultButton == NULL)
    {
	BB_DynamicDefaultButton(wid) = NULL;

	return;
    }

    if (XmIsPushButtonGadget(newDefaultButton) ||
	XmIsPushButton(newDefaultButton))
    {
	BB_DynamicDefaultButton(wid) = newDefaultButton;

	XtSetArg(argl[0], XmNshowAsDefault, 1);

	XtSetValues(BB_DynamicDefaultButton(wid), argl, 1);
    }
}

void
_XmBBUpdateDynDefaultButton(Widget bb)
{
    DEBUGOUT(_LtDebug(__FILE__, bb, "%s:_XmBBUpdateDynDefault(%d) - %s(%smanaged) %s\n",
    	__FILE__, __LINE__,
    	BB_DynamicDefaultButton(bb) ? XtName(BB_DynamicDefaultButton(bb)) : "NULL",
    	BB_DynamicDefaultButton(bb) ? XtIsManaged(BB_DynamicDefaultButton(bb)) ? "" : "not " : "not ",
    	BB_DefaultButton(bb) ? XtName(BB_DefaultButton(bb)) : "NULL"));

    if (BB_DynamicDefaultButton(bb) &&
        XtIsManaged(BB_DynamicDefaultButton(bb)))
    {
	_XmBulletinBoardSetDynDefaultButton(bb, BB_DynamicDefaultButton(bb));
    }
    else
    {
	_XmBulletinBoardSetDynDefaultButton(bb, BB_DefaultButton(bb));
    }
}

static void 
_XmBulletinBoardDialogStyleDefault(Widget w,
				   int offset,
				   XrmValue *val)
{
    static unsigned char style;

    if (XmIsDialogShell(XtParent(w)))
    {
	style = XmDIALOG_MODELESS;
    }
    else
    {
	style = XmDIALOG_WORK_AREA;
    }

    val->addr = (XPointer)&style;
}

static XmRenderTable GetRenderTable(Widget w, XtEnum renderTableType)
{
	XmBulletinBoardWidget bb = (XmBulletinBoardWidget)w;

	switch(renderTableType) {
	case XmLABEL_RENDER_TABLE:
			return bb->bulletin_board.label_font_list;
	case XmBUTTON_RENDER_TABLE:
			return bb->bulletin_board.button_font_list;
	case XmTEXT_RENDER_TABLE:
			return bb->bulletin_board.text_font_list;
													    }
	return NULL;
}
