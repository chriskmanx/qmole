/**
 *
 * $Header: /cvsroot/lesstif/lesstif/lib/Xm-2.1/RowColumn.c,v 1.10 2007/09/12 20:35:55 jwrdegoede Exp $
 *
 * Copyright (C) 1996 Free Software Foundation, Inc.
 * Copyright © 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2004, 2005 LessTif Development Team
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

static const char rcsid[] = "$Header: /cvsroot/lesstif/lesstif/lib/Xm-2.1/RowColumn.c,v 1.10 2007/09/12 20:35:55 jwrdegoede Exp $";

#include <LTconfig.h>

#include <stdio.h>
#include <string.h>
#include <stdarg.h>

#include <XmI/XmI.h>
#include <Xm/XmP.h>
#include <Xm/BaseClassP.h>
#include <Xm/CascadeBP.h>
#include <Xm/CascadeBGP.h>
#include <Xm/DrawnBP.h>
#include <Xm/Label.h>
#include <Xm/LabelG.h>
#include <Xm/ManagerP.h>
#include <Xm/MenuShellP.h>
#include <Xm/MenuUtilP.h>
#include <Xm/PushBP.h>
#include <Xm/PushBGP.h>
#include <Xm/RepType.h>
#include <Xm/RowColumnP.h>
#include <Xm/RCUtilsP.h>
#include <Xm/ScreenP.h>
#include <Xm/Separator.h>
#include <Xm/SeparatoG.h>
#include <Xm/TearOffBP.h>
#include <Xm/ToggleBP.h>
#include <Xm/ToggleBGP.h>
#include <Xm/TransltnsP.h>
#include <Xm/TearOffP.h>
#include <Xm/BulletinBP.h>
#include <XmI/MessagesI.h>

#include <XmI/DebugUtil.h>

/*
 * builtin widgets for option menus
 */
#define RC_OPTION_LABEL		"OptionLabel"
#define RC_OPTION_CBG		"OptionButton"

static Boolean _XmRCdefaultIsAligned = True;
static Boolean _XmRCdefaultAdjustMargin = True;
static Boolean _XmRCdefaultRadioBehavior = False; /* This should be true according to misc/test16
                                                     But it messes up toggle buttons in menus!
                                                   */
static Boolean _XmRCdefaultRadioAlwaysOne = True;
static Boolean _XmRCdefaultIsHomogeneous = True;
static Boolean _XmRCdefaultPopupEnabled = True;

/* Forward Declarations */

static void class_initialize(void);
static void class_part_initialize(WidgetClass w_class);
static void initialize(Widget request, Widget new_w, ArgList args,
		       Cardinal *num_args);
static void initialize_prehook(Widget request, Widget new_w, ArgList args,
			       Cardinal *num_args);
static void initialize_posthook(Widget request, Widget new_w, ArgList args,
			        Cardinal *num_args);
static void destroy(Widget w);
static void resize(Widget w);
static void realize(Widget w, XtValueMask *value_mask,
		    XSetWindowAttributes *attributes);
static void expose(Widget w, XEvent *event, Region region);
static XtGeometryResult query_geometry(Widget w, XtWidgetGeometry *proposed,
				       XtWidgetGeometry *answer);
static Boolean set_values(Widget current, Widget request, Widget new_w,
			  ArgList args, Cardinal *num_args);

/* T. Straumann: added get_values_hook method */
static void get_values_hook(Widget w, ArgList arg, Cardinal *nargs);
static XtGeometryResult geometry_manager(Widget w, XtWidgetGeometry *request,
					 XtWidgetGeometry *reply);
static void change_managed(Widget w);
static void insert_child(Widget w);
static void delete_child(Widget w);
static void constraint_initialize(Widget request, Widget neww,
				  ArgList args, Cardinal *num_args);
static Boolean constraint_set_values(Widget current, Widget request, Widget neww,
				     ArgList args, Cardinal *num_args);
static XmNavigability widget_navigable(Widget w);
static void AddPopupHandlers(Widget new_w);
static void MenuProcEntry(int proc, Widget rc,...);
static void ArmAndActivate(Widget w, XEvent *e, String *args, Cardinal *nargs);
static void _XmOptionCallback(Widget w, XtPointer cd, XtPointer cbs);
static void RadioHandler(Widget w);
static void _XmFromMenuPost(Widget widget, int offset, XtArgVal *value);
static void ParsePostString(Widget widget, String menuPost);
static XmImportOperator _XmToMenuPost(Widget widget, int offset,
				      XtArgVal *value);
static Cardinal _XmRowColumnOrderProc(Widget widget);
static void _XmRowColumnEntryClassDefault(Widget w,
					  int offset,
					  XrmValue *val);

/*
static void _XmRowColumnIsHomogeneousDefault(Widget w,
					     int offset,
					     XrmValue *val);
*/

/*
static void _XmRowColumnMenuAcceleratorDefault(Widget w,
					       int offset,
					       XrmValue *val);
*/


static void _XmPopupButtonPressHandler(Widget w, XtPointer client_data,
				       XEvent *event, Boolean *cont);
static void _XmFixOptionMenu(Widget new_w, Boolean use_set_values);

/*
 * offset macros
 */
#define Offset(field) XtOffsetOf(XmRowColumnRec, row_column.field)
#define MGR_Offset(field) XtOffsetOf(XmRowColumnRec, manager.field)
#define COMP_Offset(field) XtOffsetOf(XmRowColumnRec, composite.field)

/* Resources for the RowColumn class */
static XtResource resources[] =
{
    {
	XmNresizeWidth, XmCResizeWidth, XmRBoolean,
	sizeof(Boolean), Offset(resize_width),
	XmRImmediate, (XtPointer)True
    },
    {
	XmNresizeHeight, XmCResizeHeight, XmRBoolean,
	sizeof(Boolean), Offset(resize_height),
	XmRImmediate, (XtPointer)True
    },
    {
	XmNwhichButton, XmCWhichButton, XmRWhichButton,
	sizeof(unsigned int), Offset(postButton),
	XmRImmediate, (XtPointer)XmUNSPECIFIED
	/* add support for this!!! */
    },
    {
	XmNmenuPost, XmCMenuPost, XmRString,
	sizeof(String), Offset(menuPost),
	XmRString, (XtPointer)NULL
    },
    {
	XmNadjustLast, XmCAdjustLast, XmRBoolean,
	sizeof(Boolean), Offset(adjust_last),
	XmRImmediate, (XtPointer)True
    },
    {
	XmNmarginWidth, XmCMarginWidth, XmRHorizontalDimension,
	sizeof(Dimension), Offset(margin_width),
	XmRImmediate, (XtPointer)XmINVALID_DIMENSION
    },
    {
	XmNmarginHeight, XmCMarginHeight, XmRVerticalDimension,
	sizeof(Dimension), Offset(margin_height),
	XmRImmediate, (XtPointer)XmINVALID_DIMENSION
    },
    {
	XmNentryCallback, XmCCallback, XmRCallback,
	sizeof(XtCallbackList), Offset(entry_callback),
	XmRCallback, (XtPointer)NULL
    },
    {
	XmNmapCallback, XmCCallback, XmRCallback,
	sizeof(XtCallbackList), Offset(map_callback),
	XmRCallback, (XtPointer)NULL
    },
    {
	XmNunmapCallback, XmCCallback, XmRCallback,
	sizeof(XtCallbackList), Offset(unmap_callback),
	XmRCallback, (XtPointer)NULL
    },
    {
	XmNorientation, XmCOrientation, XmROrientation,
	sizeof(unsigned char), Offset(orientation),
	XmRImmediate, (XtPointer)XmUNSPECIFIED
	/*FIX ME: Should be XmROrientation, (XtPointer)some whacko thing here */
    },
    {
	XmNspacing, XmCSpacing, XmRHorizontalDimension,
	sizeof(Dimension), Offset(spacing),
	XmRImmediate, (XtPointer)XmINVALID_DIMENSION
    },
    {
	XmNentryBorder, XmCEntryBorder, XmRHorizontalDimension,
	sizeof(Dimension), Offset(entry_border),
	XmRImmediate, (XtPointer)0
    },
    {
	XmNisAligned, XmCIsAligned, XmRBoolean,
	sizeof(Boolean), Offset(do_alignment),
	XmRBoolean, (XtPointer)&_XmRCdefaultIsAligned
    },
    {
	XmNentryAlignment, XmCAlignment, XmRAlignment,
	sizeof(unsigned char), Offset(entry_alignment),
	XtRImmediate, (XtPointer)XmALIGNMENT_BEGINNING
	/* FIX ME: Motif has XmRAlignment, (XtPointer)whacko value */
    },
    {
	XmNadjustMargin, XmCAdjustMargin, XmRBoolean,
	sizeof(Boolean), Offset(adjust_margin),
	XtRBoolean, (XtPointer)&_XmRCdefaultAdjustMargin
    },
    {
	XmNpacking, XmCPacking, XmRPacking,
	sizeof(unsigned char), Offset(packing),
	XmRImmediate, (XtPointer)XmUNSPECIFIED	/* To be overruled in initialize */
    },
    {
	XmNnumColumns, XmCNumColumns, XmRShort,
	sizeof(short), Offset(num_columns),
	XtRImmediate, (XtPointer)1
	/* FIX ME: Motif has XmRShort, (XtPointer)whacko value */
    },
    {
	XmNradioBehavior, XmCRadioBehavior, XmRBoolean,
	sizeof(Boolean), Offset(radio),
	XmRBoolean, (XtPointer)&_XmRCdefaultRadioBehavior
    },
    {
	XmNradioAlwaysOne, XmCRadioAlwaysOne, XmRBoolean,
	sizeof(Boolean), Offset(radio_one),
	XmRBoolean, (XtPointer)&_XmRCdefaultRadioAlwaysOne
    },
    {
	XmNisHomogeneous, XmCIsHomogeneous, XmRBoolean,
	sizeof(Boolean), Offset(homogeneous),
	XmRBoolean, (XtPointer)&_XmRCdefaultIsHomogeneous
    },
    {
	XmNentryClass, XmCEntryClass, XmRWidgetClass,
	sizeof(WidgetClass), Offset(entry_class),
	XmRCallProc, (XtPointer)_XmRowColumnEntryClassDefault
	/* Motif has XmRWidgetClass, (XtPointer)NULL */
    },
    {
	XmNrowColumnType, XmCRowColumnType, XmRRowColumnType,
	sizeof(unsigned char), Offset(type),
	XtRImmediate, (XtPointer)XmWORK_AREA
	/* FIX ME: Motif has XmRRowColumnType, (XtPointer)whacko value */
    },
    {
	XmNmenuHelpWidget, XmCMenuWidget, XmRMenuWidget,
	sizeof(Widget), Offset(help_pushbutton),
	XmRImmediate, (XtPointer)NULL
	/* FIX ME: Motif has XmRMenuWidget, (XtPointer)whacko value */
    },
    {
	XmNlabelString, XmCXmString, XmRXmString,
	sizeof(XmString), Offset(option_label),
	XmRXmString, (XtPointer)NULL
    },
    {
	XmNsubMenuId, XmCMenuWidget, XmRMenuWidget,
	sizeof(Widget), Offset(option_submenu),
	XmRImmediate, (XtPointer)NULL
	/* FIX ME: Motif has XmRMenuWidget, (XtPointer)whacko value */
    },
    {
	XmNmenuHistory, XmCMenuWidget, XmRMenuWidget,
	sizeof(Widget), Offset(memory_subwidget),
	XmRImmediate, (XtPointer)NULL
	/* FIX ME: Motif has XmRMenuWidget, (XtPointer)whacko value */
    },
    {
	XmNpopupEnabled, XmCPopupEnabled, XmRBoolean,
	sizeof(Boolean), Offset(popup_enabled),
	XmRBoolean, (XtPointer)&_XmRCdefaultPopupEnabled
    },
    {
	XmNmenuAccelerator, XmCAccelerators, XmRString,
	sizeof(String), Offset(menu_accelerator),
	XmRString, (XtPointer)""
    },
    {
	XmNmnemonic, XmCMnemonic, XmRKeySym,
	sizeof(KeySym), Offset(mnemonic),
	XmRImmediate, (XtPointer)NULL
    },
    {
	XmNmnemonicCharSet, XmCMnemonicCharSet, XmRString,
	sizeof(String), Offset(mnemonicCharSet),
	XtRImmediate, (XtPointer)XmFONTLIST_DEFAULT_TAG
    },
    {
	XmNshadowThickness, XmCShadowThickness, XmRHorizontalDimension,
	sizeof(Dimension), MGR_Offset(shadow_thickness),
	XmRImmediate, (XtPointer)XmINVALID_DIMENSION
    },
    {
	XmNpostFromList, XmCPostFromList, XmRWidgetList,
	sizeof(WidgetList), Offset(postFromList),
	XmRWidgetList, (XtPointer)NULL
    },
    {
	XmNpostFromCount, XmCPostFromCount, XmRInt,
	sizeof(int), Offset(postFromCount),
	XmRImmediate, (XtPointer)XmUNSPECIFIED
    },
    {
	XmNnavigationType, XmCNavigationType, XmRNavigationType,
	sizeof(XmNavigationType), MGR_Offset(navigation_type),
	XmRImmediate, (XtPointer)((XmNavigationType)XmUNSPECIFIED)
    },
    {
	XmNentryVerticalAlignment, XmCVerticalAlignment, XmRVerticalAlignment,
	sizeof(unsigned char), Offset(entry_vertical_alignment),
	XtRImmediate, (XtPointer)XmALIGNMENT_BEGINNING
	/* FIX ME: Motif has XmRVerticalAlignment, (XtPointer)whacko values */
    },
    {
	XmNtearOffModel, XmCTearOffModel, XmRTearOffModel,
	sizeof(unsigned char), Offset(TearOffModel),
	XtRImmediate, (XtPointer)XmTEAR_OFF_DISABLED
	/* FIX ME: Motif has XmRTearOffModel, (XtPointer)whacko values */
    },
    {
	XmNtearOffMenuActivateCallback, XmCCallback, XmRCallback,
	sizeof(XtCallbackList), Offset(tear_off_activated_callback),
	XmRCallback, (XtPointer)NULL
    },
    {
	XmNtearOffMenuDeactivateCallback, XmCCallback, XmRCallback,
	sizeof(XtCallbackList), Offset(tear_off_deactivated_callback),
	XmRCallback, (XtPointer)NULL
    },
    {
	XmNinsertPosition, XmCInsertPosition, XmRFunction,
	sizeof(XtOrderProc), COMP_Offset(insert_position),
	XmRImmediate, (XtPointer)_XmRowColumnOrderProc,
    }
};

static XmSyntheticResource syn_resources[] =
{
    {
	XmNmnemonicCharSet,
	sizeof(String), Offset(mnemonicCharSet),
	NULL /* FIX ME */ , NULL
    },
    {
	XmNmenuAccelerator,
	sizeof(String), Offset(menu_accelerator),
	NULL /* FIX ME */ , NULL
    },
    {
	XmNmenuPost,
	sizeof(String), Offset(menuPost),
	_XmFromMenuPost, _XmToMenuPost,
    },
    {
	XmNlabelString,
	sizeof(XmString), Offset(option_label),
	NULL /* FIX ME */ , NULL
    },
    {
	XmNspacing,
	sizeof(Dimension), Offset(spacing),
	_XmFromHorizontalPixels, _XmToHorizontalPixels
    },
    {
	XmNmarginHeight,
	sizeof(Dimension), Offset(margin_height),
	_XmFromVerticalPixels, _XmToVerticalPixels
    },
    {
	XmNmarginWidth,
	sizeof(Dimension), Offset(margin_width),
	_XmFromHorizontalPixels, _XmToHorizontalPixels
    },
    {
	XmNentryBorder,
	sizeof(Dimension), Offset(entry_border),
	_XmFromHorizontalPixels, _XmToHorizontalPixels
    }
};

#undef Offset

#define Offset(field) XtOffsetOf(XmRowColumnConstraintRec, row_column.field)
static XtResource rowColumnConstraintResources[] =
{
    {
	XmNpositionIndex, XmCPositionIndex, XmRShort,
	sizeof(short), Offset(position_index),
	XmRImmediate, (XtPointer)XmLAST_POSITION
    }
};

/* not all actually written yet... */
#define STATIC_ACTION(fn) static void (fn)(Widget, XEvent*, String*, Cardinal*)
#define ACTION(fn) void (fn)(Widget, XEvent*, String*, Cardinal*)

STATIC_ACTION(MenuBarGadgetSelect);
ACTION(_XmMenuHelp);
STATIC_ACTION(MenuEnter);
STATIC_ACTION(MenuUnmap);
STATIC_ACTION(MenuFocusIn);
STATIC_ACTION(MenuFocusOut);
STATIC_ACTION(DoBtnEventCleanupReplay);

#undef STATIC_ACTION
#undef ACTION

static void _XmFocusOut(Widget, XEvent*, String*, Cardinal*);
static void _XmFocusIn(Widget, XEvent*, String*, Cardinal*);
static void _XmUnmap(Widget, XEvent*, String*, Cardinal*);
static void _XmNoop(Widget, XEvent*, String*, Cardinal*);

static XtActionsRec actions[] =
{

    {"MenuHelp", _XmMenuHelp},
    {"MenuBtnDown", _XmMenuBtnDown},
    {"MenuBtnUp", _XmMenuBtnUp},

    {"MenuBarGadgetSelect", MenuBarGadgetSelect},
    {"FocusOut", _XmFocusOut},
    {"FocusIn", _XmFocusIn},
    {"Unmap", _XmUnmap},
    {"Noop", _XmNoop},
    {"MenuTraverseLeft", _XmMenuTraverseLeft},
    {"MenuTraverseRight", _XmMenuTraverseRight},
    {"MenuTraverseUp", _XmMenuTraverseUp},
    {"MenuTraverseDown", _XmMenuTraverseDown},
    {"MenuEscape", _XmMenuEscape},
    {"MenuFocusIn", MenuFocusIn},
    {"MenuFocusOut", MenuFocusOut},
    {"MenuUnmap", MenuUnmap},
    {"MenuEnter", MenuEnter},
    /*{"MenuGadgetReturn", _XmMenuReturn},*/
    {"MenuGadgetEscape", _XmMenuEscape},
    {"MenuGadgetTraverseLeft", _XmRC_GadgetTraverseLeft},
    {"MenuGadgetTraverseRight", _XmRC_GadgetTraverseRight},
    {"MenuGadgetTraverseUp", _XmRC_GadgetTraverseUp},
    {"MenuGadgetTraverseDown", _XmRC_GadgetTraverseDown},
};


static XmBaseClassExtRec _XmRowColumnCoreClassExtRec =
{
    /* next_extension            */ NULL,
    /* record_type               */ NULLQUARK,
    /* version                   */ XmBaseClassExtVersion,
    /* size                      */ sizeof(XmBaseClassExtRec),
    /* initialize_prehook        */ initialize_prehook,
    /* set_values_prehook        */ XmInheritSetValuesPrehook /*NULL*/,
    /* initialize_posthook       */ initialize_posthook,
    /* set_values_posthook       */ XmInheritSetValuesPosthook /*NULL*/,
    /* secondary_object_class    */ XmInheritClass /*NULL*/,
    /* secondary_object_create   */ XmInheritSecObjectCreate /*NULL*/,
    /* get_secondary_resources   */ XmInheritGetSecResData /*NULL*/,
    /* fast_subclass             */ {0},
    /* get_values_prehook        */ XmInheritGetValuesPrehook /*NULL*/,
    /* get_values_posthook       */ XmInheritGetValuesPosthook /*NULL*/,
    /* class_part_init_prehook   */ NULL,
    /* class_part_init_posthook  */ NULL,
    /* ext_resources             */ NULL,
    /* compiled_ext_resources    */ NULL,
    /* num_ext_resources         */ 0,
    /* use_sub_resources         */ False,
    /* widget_navigable          */ widget_navigable,
    /* focus_change              */ XmInheritFocusChange,
    /* wrapper_data              */ NULL
};

#if 0
static CompositeClassExtensionRec rcCompositeExt =
{
    /* next_extension */ NULL,
    /* record_type    */ NULLQUARK,
    /* version        */ XtCompositeExtensionVersion,
    /* record_size    */ sizeof(CompositeClassExtensionRec),
    /* accepts_objects */ True,
#if XtSpecificationRelease >= 6
    /* allows_change_managed_set */ True
#endif
};
#endif

static XmManagerClassExtRec _XmRowColumnMClassExtRec =
{
    /* next_extension            */ NULL,
    /* record_type               */ NULLQUARK,
    /* version                   */ XmManagerClassExtVersion,
    /* record_size               */ sizeof(XmManagerClassExtRec),
    /* traversal_children        */ NULL /* FIX ME */
};

XmRowColumnClassRec xmRowColumnClassRec =
{
    /* Core class part */
    {
	/* superclass            */ (WidgetClass) & xmManagerClassRec,
	/* class_name            */ "XmRowColumn",
	/* widget_size           */ sizeof(XmRowColumnRec),
	/* class_initialize      */ class_initialize,
	/* class_part_initialize */ class_part_initialize,
	/* class_inited          */ False,
	/* initialize            */ initialize,
	/* initialize_hook       */ NULL,
	/* realize               */ realize,
	/* actions               */ actions,
	/* num_actions           */ XtNumber(actions),
	/* resources             */ resources,
	/* num_resources         */ XtNumber(resources),
	/* xrm_class             */ NULLQUARK,
	/* compress_motion       */ True,
	/* compress_exposure     */ XtExposeCompressMaximal /*XtExposeCompressMultiple*/,
	/* compress_enterleave   */ False /*True*/,
	/* visible_interest      */ False /*True*/,
	/* destroy               */ destroy,
	/* resize                */ resize,
	/* expose                */ expose,
	/* set_values            */ set_values,
	/* set_values_hook       */ NULL,
	/* set_values_almost     */ XtInheritSetValuesAlmost,
	/* get_values_hook       */ get_values_hook,
	/* accept_focus          */ NULL,
	/* version               */ XtVersion,
	/* callback offsets      */ NULL,
	/* tm_table              */ XtInheritTranslations, /* should be NULL but
	                                                      we loose togglebg's in
	                                                      a work area if it is 
	                                                      togglegb/test1 */
	/* query_geometry        */ query_geometry,
	/* display_accelerator   */ NULL /*XtInheritDisplayAccelerator*/,
	/* extension             */ (XtPointer) &_XmRowColumnCoreClassExtRec
    },
    /* Composite class part */
    {
	/* geometry manager */	geometry_manager,
	/* change_managed   */	change_managed,
	/* insert_child     */	insert_child,
	/* delete_child     */	delete_child,
	/* extension        */	(XtPointer)NULL /*&rcCompositeExt*/,
    },
    /* Constraint class part */
    {
	/* subresources      */ rowColumnConstraintResources,
	/* subresource_count */ XtNumber(rowColumnConstraintResources),
	/* constraint_size   */ sizeof(XmRowColumnConstraintRec),
	/* initialize        */ constraint_initialize,
	/* destroy           */ NULL,
	/* set_values        */ constraint_set_values,
	/* extension         */ NULL,
    },
    /* XmManager class part */
    {
	/* translations           */ XtInheritTranslations,
	/* syn_resources          */ syn_resources,
	/* num_syn_resources      */ XtNumber(syn_resources),
	/* syn_constraint_res     */ NULL,
	/* num_syn_constraint_res */ 0,
	/* parent_process         */ XmInheritParentProcess,
	/* extension              */ (XtPointer)&_XmRowColumnMClassExtRec
    },
    /* XmRowColumn Area part */
    {
	/* menuProcedures   */ MenuProcEntry,
	/* armAndActivate   */ ArmAndActivate,
	/* traversalHandler */ _XmMenuTraversalHandler,
	/* extension        */ NULL,
    },
};


WidgetClass xmRowColumnWidgetClass = (WidgetClass)&xmRowColumnClassRec;


static XtTranslations menubar_trans;
static XtTranslations option_trans;
static XtTranslations menu_trans;
static XtTranslations menutrav_trans;

static void
class_initialize(void)
{
    _XmRowColumnCoreClassExtRec.record_type = XmQmotif;

    menubar_trans = XtParseTranslationTable(_XmRowColumn_bar_table);
    option_trans = XtParseTranslationTable(_XmRowColumn_option_table);
    menu_trans = XtParseTranslationTable(_XmRowColumn_menu_table);
    menutrav_trans = XtParseTranslationTable(_XmRowColumn_menu_traversal_table);
}

static void
class_part_initialize(WidgetClass widget_class)
{
    CompositeClassExtension ext, *extptr;
    XmRowColumnWidgetClass rc_class = (XmRowColumnWidgetClass)widget_class;

    extptr = (CompositeClassExtension *)
	_XmGetClassExtensionPtr((XmGenericClassExt *)
				&(rc_class->composite_class.extension),
				NULLQUARK);

    if (extptr == NULL || *extptr == NULL)
    {
	ext = (CompositeClassExtension)XtNew(CompositeClassExtensionRec);
	if (ext != NULL)
	{
	    ext->next_extension = rc_class->composite_class.extension;
	    ext->record_type = NULLQUARK;
	    ext->version = XtCompositeExtensionVersion;
	    ext->record_size = sizeof(CompositeClassExtensionRec);
	    ext->accepts_objects = True;
#if XtSpecificationRelease >= 6
	    ext->allows_change_managed_set = True;
#endif
	    rc_class->composite_class.extension = (XtPointer)ext;
	}
    }

    _XmFastSubclassInit(widget_class, XmROW_COLUMN_BIT);
}

static void
_XmRcCreateTearOffControl(Widget rc)
{
    RC_TearOffControl(rc) = XtVaCreateManagedWidget("TearOffControl",
						xmTearOffButtonWidgetClass, rc,
						    XmNpositionIndex, 0,
						    NULL);
}

static void
_XmRcDestroyTearOffControl(Widget rc)
{
    if (RC_TearOffControl(rc))
    {
	XtDestroyWidget(RC_TearOffControl(rc));
    }
    RC_TearOffControl(rc) = NULL;
}

static void
initialize_prehook(Widget request, Widget new_w,
		   ArgList args, Cardinal *num_args)
{
    if (RC_Type(new_w) == XmWORK_AREA)
    {
	MGR_NavigationType(new_w) = XmTAB_GROUP;
    }
    else
    {
	MGR_NavigationType(new_w) = XmNONE;
    }

    MGR_TraversalOn(new_w) = True;

    /* this is needed for Xmt.  We probably ought to follow suit with
     * what we do in Label et. al. and override the translations here
     * rather than in initialize. */
    _XmSaveCoreClassTranslations(new_w);

    switch (RC_Type(new_w))
    {
    case XmMENU_PULLDOWN:
    case XmMENU_POPUP:
	CoreClassTranslations(new_w) = (String)menu_trans;
	break;

    case XmMENU_BAR:
	CoreClassTranslations(new_w) = (String)menubar_trans;
	break;

    case XmMENU_OPTION:
	CoreClassTranslations(new_w) = (String)option_trans;
	break;

    case XmWORK_AREA:
    /* rws 8 Mar 1998
       I think that we should be installing the Manager translations here.
       misc/test16 shows that Motif sets the tm_table entry to NULL. If we 
       do this toggle button gadgets in work areas will not activate.
       togglebg/test1
     */
    default:
	break;
    }
}

static void
initialize_posthook(Widget request, Widget new_w,
		    ArgList args, Cardinal *num_args)
{
    _XmRestoreCoreClassTranslations(new_w);

    if (RC_Type(new_w) == XmMENU_OPTION)
    {
	Widget cb;
	if (RC_OptionLabel(new_w) != NULL)
	{
	    XtVaCreateManagedWidget(RC_OPTION_LABEL,
				    xmLabelGadgetClass,
				    new_w,
				    XmNlabelString, RC_OptionLabel(new_w),
				    XmNmnemonic, RC_Mnemonic(new_w),
				    XmNmnemonicCharSet,
				    RC_MnemonicCharSet(new_w),
				    NULL);
	}
	else
	{
	    XmString s = XmStringCreateLtoR("", XmFONTLIST_DEFAULT_TAG);

	    XtVaCreateManagedWidget(RC_OPTION_LABEL,
				    xmLabelGadgetClass,
				    new_w,
				    XmNlabelString, s,
				    XmNmnemonic, RC_Mnemonic(new_w),
				    XmNmnemonicCharSet,
				    RC_MnemonicCharSet(new_w),
				    NULL);
	    XmStringFree(s);
	}
	cb = XtVaCreateManagedWidget(RC_OPTION_CBG,
				     xmCascadeButtonGadgetClass,
				     new_w,
				     XmNsubMenuId, RC_OptionSubMenu(new_w),
				     XmNrecomputeSize, False,
				     XmNalignment, XmALIGNMENT_CENTER,
				     NULL);

	RC_DoMarginAdjust(new_w) = False; /* This is to get test28 correct */
	_XmFixOptionMenu(new_w, True);

	/* If we have XmNmenuHistory, copy it */
	if (RC_MemWidget(new_w))
	{
	    XmString xms = NULL;
	    Arg a;
	    XtSetArg(a, XmNlabelString, &xms);
	    XtGetValues(RC_MemWidget(new_w), &a, 1);
	    XtSetArg(a, XmNlabelString, xms);
	    XtSetValues(cb, &a, 1);
	    XmStringFree(xms);
	}
    }
}

static void
initialize(Widget request, Widget new_w,
	   ArgList args, Cardinal *num_args)
{
    RC_Boxes(new_w) = NULL;	/* no initial children */

    DEBUGOUT(_LtDebug(__FILE__, new_w,
		      "%s:initialize: %i args\n"
		      "\trequest X %5i Y %5i W %5i H %5i\n"
		      "\t  new_w X %5i Y %5i W %5i H %5i\n",
		      __FILE__, *num_args,
		      XtX(request), XtY(request),
		      XtWidth(request), XtHeight(request),
		      XtX(new_w), XtY(new_w),
		      XtWidth(new_w), XtHeight(new_w)));
    DEBUGOUT(_LtDebugPrintArgList(__FILE__, new_w, args, *num_args, False));

    RC_TearOffLastSelectToplevel(new_w) = NULL;
    RC_TearOffFocusItem(new_w) = NULL;
#if 1
    /* test38 */
    /* ddd Edit->GDB settings (option menus) */
    if (RC_Type(new_w) == XmWORK_AREA)
    {
	if (XtWidth(new_w) == 0)
	{
	    XtWidth(new_w) = 16;
	}
	if (XtHeight(new_w) == 0)
	{
	    XtHeight(new_w) = 16;
	}
    }
#endif
    /* FIX ME: Check if parent is MenuShell if pulldown or option menu */
    if (RC_Type(new_w) == XmMENU_OPTION)
    {
#if 0
	/* rws 19 Oct 1999
	   openDX shows that Motif seems to force this no matter what is
	   specified.
	   dx -prompter then select Image file. The option menu should not
	   have the shadow around it.
	 */
	if (MGR_ShadowThickness(new_w) == XmINVALID_DIMENSION)
#endif
	{
	    MGR_ShadowThickness(new_w) = 0;
	}
    }
    else
    {
	if (MGR_ShadowThickness(new_w) == XmINVALID_DIMENSION)
	{
	    if (RC_Type(new_w) == XmMENU_PULLDOWN ||
		RC_Type(new_w) == XmMENU_POPUP ||
		RC_Type(new_w) == XmMENU_BAR)
	    {
		MGR_ShadowThickness(new_w) = 2;
	    }
	    else
	    {
		MGR_ShadowThickness(new_w) = 0;
	    }
	}
    }

    /* now, we install our translations, 
     * depending on the row column type */

    if (RC_Type(new_w) == XmMENU_PULLDOWN ||
	RC_Type(new_w) == XmMENU_POPUP ||
	RC_Type(new_w) == XmMENU_BAR)
    {
	XtOverrideTranslations(new_w, menutrav_trans);
    }

    /* menuPost stuff. */
    if (RC_MenuPost(new_w) == NULL)
    {
	if (RC_Type(new_w) == XmMENU_OPTION ||
	    RC_Type(new_w) == XmWORK_AREA ||
	    RC_Type(new_w) == XmMENU_BAR)
	{
	    RC_MenuPost(new_w) = "<Btn1Down>";
	}
    }
    else
        ParsePostString(new_w, RC_MenuPost(new_w));

    if (RC_Type(new_w) == XmMENU_POPUP)
    {
	if (RC_PostButton(new_w) == XmUNSPECIFIED)
	{
	    RC_PostEventType(new_w) = ButtonPress;
	    RC_PostButton(new_w) = Button3; /* Fixme: this should be BMenu */
	    RC_PostModifiers(new_w) = 0;
	}
	AddPopupHandlers(new_w);

        if (XtParent(new_w) && XmIsMenuShell(XtParent(new_w)))
        {
            MS_PrivateShell(XtParent(new_w)) = False;
        }
    }

    ((XmRowColumnWidget)new_w)->row_column.armed = 0;	/* FIX ME: macro */

    if (RC_Spacing(new_w) == XmINVALID_DIMENSION)
    {
	if (RC_Type(new_w) == XmMENU_OPTION || RC_Type(new_w) == XmWORK_AREA)
	{
	    RC_Spacing(new_w) = 3;
	}
	else
	{
	    RC_Spacing(new_w) = 0;
	}
    }

    if (RC_MarginW(new_w) == XmINVALID_DIMENSION)
    {
	if (RC_Type(new_w) == XmMENU_POPUP || RC_Type(new_w) == XmMENU_PULLDOWN)
	{
	    RC_MarginW(new_w) = 0;
	}
	else
	{
	    RC_MarginW(new_w) = 3;
	}
    }
    if (RC_MarginH(new_w) == XmINVALID_DIMENSION)
    {
	if (RC_Type(new_w) == XmMENU_POPUP || RC_Type(new_w) == XmMENU_PULLDOWN)
	{
	    RC_MarginH(new_w) = 0;
	}
	else
	{
	    RC_MarginH(new_w) = 3;
	}
    }

    RC_LastSelectToplevel(new_w) = NULL;  /* widget id of the menu at the
    					     top of the heir
    					   */

    if (RC_Type(new_w) == XmMENU_BAR || RC_Type(new_w) == XmMENU_OPTION)
    {
	RC_LastSelectToplevel(new_w) = new_w;
	if (RC_OptionSubMenu(new_w))
	{
	    /* Make sure the sub-menu has a back pointer to the option menu */
	    RC_LastSelectToplevel(RC_OptionSubMenu(new_w)) = new_w;
	}
    }

    /* Internal widgets */

    RC_CascadeBtn(new_w) = NULL;	/* widget id of the cascade button
    					   that posted this menu
    					 */
    RC_PopupPosted(new_w) = NULL;	/* widget id of the menu posted from
    					   any button in this menu
    					 */

    /* Tear off stuff */
    RC_SetTornOff(new_w, False);
    RC_SetFromInit(new_w, False);
    RC_SetTearOffDirty(new_w, False);
    RC_SetTearOffActive(new_w, False);
    RC_ParentShell(new_w) = NULL;
    RC_TearOffControl(new_w) = NULL;
    if ((RC_Type(new_w) == XmMENU_POPUP || RC_Type(new_w) == XmMENU_PULLDOWN) &&
	RC_TearOffModel(new_w) == XmTEAR_OFF_ENABLED)
    {
	_XmRcCreateTearOffControl(new_w);
    }

    /* just to initialize it. */
    RC_SetFromResize(new_w, False);
    RC_SetArmed(new_w, False);

    if (RC_Packing(new_w) == (unsigned char)XmUNSPECIFIED)
    {
	/*
	 * This is what the XmRowColumn manual page says
	 *      (end of the text describing XmNpacking).
	 */
	if (RC_RadioBehavior(new_w) && RC_Type(new_w) == XmWORK_AREA)
	{
	    RC_Packing(new_w) = XmPACK_COLUMN;
	}
	else if (RC_Type(new_w) == XmMENU_OPTION)
	{
	    RC_Packing(new_w) = XmPACK_TIGHT;
	}
	else
	{
	    RC_Packing(new_w) = XmPACK_TIGHT;
	}
    }

    if (RC_Orientation(new_w) == (unsigned char)XmUNSPECIFIED)
    {
	if (RC_Type(new_w) == XmMENU_PULLDOWN ||
	    RC_Type(new_w) == XmMENU_POPUP)
	{
	    RC_Orientation(new_w) = XmVERTICAL;
	}
	else if (RC_Type(new_w) == XmMENU_OPTION ||
		 RC_Type(new_w) == XmMENU_BAR)
	{
	    RC_Orientation(new_w) = XmHORIZONTAL;
	}
	else
	{
	    RC_Orientation(new_w) = XmVERTICAL;
	}
    }

    /* Be verbose about it ... */
	DEBUGOUT(_LtDebug(__FILE__,
			  new_w,
		    "Initialize: RadioBehavior %s, RC_Type %s => Packing %s\n",
			  _LtDebugBoolean2String(RC_RadioBehavior(new_w)),
			  _LtDebugRcType2String(RC_Type(new_w)),
			  _LtDebugPacking2String(RC_Packing(new_w))));

#if 0
    /* FIX ME: This should be enabled as soon as MGR_HighlightedWidget(w) =
     * NULL is moved from MenuUnmap to Manager's event handler.
     */
    if (RC_Type(new_w) == XmMENU_PULLDOWN)
    {
	new_w->core.mapped_when_managed = False;
    }
#endif
    RC_OldWidth(new_w) = XtWidth(new_w);
    RC_OldHeight(new_w) = XtHeight(new_w);
    RC_OldShadowThickness(new_w) = MGR_ShadowThickness(new_w);

}

static void xtWarnCB(String message)
{
}

static void
destroy(Widget w)
{
     /* initialize() called AddPopupHandlers() upon a popup menu */
     if (RC_Type(w) == XmMENU_POPUP)
     {
         Widget realpar = NULL;

         if (XtIsShell(XtParent(w)))
         {
             realpar = XtParent(XtParent(w));
         }
         else
         {
             realpar = XtParent(w);
         }

         if (realpar != NULL)
         {
             XtErrorHandler old_handler;
             XtRemoveEventHandler(realpar,
                                  ButtonPressMask|ButtonReleaseMask,
                                  False, _XmPopupButtonPressHandler,
                                  (XtPointer)w );
             /* Yes this may not be necessary, shut up Xt see:
                http://sourceforge.net/tracker/index.php?func=detail&aid=1217326&group_id=8596&atid=108596 */
             old_handler = XtAppSetWarningHandler(
                                        XtWidgetToApplicationContext(realpar),
                                        xtWarnCB);
	     XtUngrabButton(realpar, RC_PostButton(w), RC_PostModifiers(w));
	     XtAppSetWarningHandler(XtWidgetToApplicationContext(realpar),
	                            old_handler);
         }
     }

    if (RC_Boxes(w))
    {
	XtFree((char *)RC_Boxes(w));
    }
}

Widget
XmOptionButtonGadget(Widget option_menu)
{
    return XtNameToWidget(option_menu, RC_OPTION_CBG);
}

Widget
XmOptionLabelGadget(Widget option_menu)
{
    return XtNameToWidget(option_menu, RC_OPTION_LABEL);
}

static Boolean
set_values(Widget old, Widget request, Widget new_w,
	   ArgList args, Cardinal *num_args)
{
    Boolean need_refresh = False, need_relayout = False;

    DEBUGOUT(_LtDebug(__FILE__, new_w,
		      "set_values: %i args\n"
		      "\t    old X %5i Y %5i W %5i H %5i\n"
		      "\trequest X %5i Y %5i W %5i H %5i\n"
		      "\t  new_w X %5i Y %5i W %5i H %5i\n",
		      *num_args,
		      XtX(old), XtY(old),
		      XtWidth(old), XtHeight(old),
		      XtX(request), XtY(request),
		      XtWidth(request), XtHeight(request),
		      XtX(new_w), XtY(new_w),
		      XtWidth(new_w), XtHeight(new_w)));
    DEBUGOUT(_LtDebugPrintArgList(__FILE__, new_w, args, *num_args, False));
    DEBUGOUT(_LtDebug("EMACS", new_w,
		      "set_values: %i args\n"
		      "\t    old X %5i Y %5i W %5i H %5i\n"
		      "\trequest X %5i Y %5i W %5i H %5i\n"
		      "\t  new_w X %5i Y %5i W %5i H %5i\n",
		      *num_args,
		      XtX(old), XtY(old),
		      XtWidth(old), XtHeight(old),
		      XtX(request), XtY(request),
		      XtWidth(request), XtHeight(request),
		      XtX(new_w), XtY(new_w),
		      XtWidth(new_w), XtHeight(new_w)));
    DEBUGOUT(_LtDebugPrintArgList("EMACS", new_w, args, *num_args, False));

    if (MGR_ShadowThickness(old) != MGR_ShadowThickness(new_w))
    {
	if (!(RC_Type(new_w) == XmMENU_PULLDOWN || RC_Type(new_w) == XmMENU_POPUP))
	{
	    _XmWarning(new_w, _XmMsgRowColumn_0020);
	    MGR_ShadowThickness(new_w) = MGR_ShadowThickness(old);
	}
    }
    if (RC_EntryAlignment(old) != RC_EntryAlignment(new_w))
    {
	Cardinal i;
	Arg a;

	XtSetArg(a, XmNalignment, RC_EntryAlignment(new_w));
	DEBUGOUT(_LtDebug(__FILE__, new_w,
			  "Setting Alignment for children to %s\n",
			  _LtDebugAlignment2String(RC_EntryAlignment(new_w))));

	for (i = 0; i < MGR_NumChildren(new_w); i++)
	{
	    if (XmIsLabel(MGR_Children(new_w)[i])
		|| XmIsLabelGadget(MGR_Children(new_w)[i]))
	    {
		DEBUGOUT(_LtDebug2(__FILE__, new_w, MGR_Children(new_w)[i],
				   "Set Alignment to %s\n",
			       _LtDebugAlignment2String(RC_EntryAlignment(new_w))));
		XtSetValues(MGR_Children(new_w)[i], &a, 1);
	    }
	}
    }

    if ((RC_Orientation(old) != RC_Orientation(new_w)) ||
	(RC_Packing(old) != RC_Packing(new_w)))
    {
	need_relayout = True;
	need_refresh = True;
    }

    if ((RC_TearOffModel(new_w) != RC_TearOffModel(old)) &&
	(RC_Type(new_w) == XmMENU_POPUP || RC_Type(new_w) == XmMENU_PULLDOWN))
    {
	if (RC_TearOffModel(new_w) == XmTEAR_OFF_ENABLED)
	{
	    _XmRcCreateTearOffControl(new_w);
	}
	else
	{
	    _XmRcDestroyTearOffControl(new_w);
	}

	need_relayout = True;
    }

    if (RC_OptionLabel(new_w) != RC_OptionLabel(old))
    {
        int i;
 
        /* Oh yes this is very efficient :-( */
        for (i = 0; i < MGR_NumChildren(new_w); i++)
        {
            if (XmIsLabelGadget(MGR_Children(new_w)[i]) &&
		!XmIsCascadeButtonGadget(MGR_Children(new_w)[i]))
            {
		if (RC_OptionLabel(new_w) == NULL)
		{
		    XtUnmanageChild(MGR_Children(new_w)[i]);
		}
		else
		{
		    XtVaSetValues(MGR_Children(new_w)[i],
				  XmNlabelString, RC_OptionLabel(new_w), NULL);

		    if (!XtIsManaged(MGR_Children(new_w)[i]))
		    {
			XtManageChild(MGR_Children(new_w)[i]);

			need_relayout = True;
			need_refresh = True;
		    }
		}
 
                DEBUGOUT(_LtDebug2(__FILE__, new_w, MGR_Children(new_w)[i],
                                   "Assign OptionLabelString\n"));

                break;
            }
	}
    }

    if (RC_Type(new_w) == XmMENU_OPTION &&
	RC_OptionSubMenu(new_w) != RC_OptionSubMenu(old))
    {
	int i;

	/* Make sure the sub-menu has a back pointer to the option menu */
	RC_LastSelectToplevel(RC_OptionSubMenu(new_w)) = new_w;
	/* Oh yes this is very efficient :-( */
	for (i = 0; i < MGR_NumChildren(new_w); i++)
	{
	    if (XtIsSubclass(MGR_Children(new_w)[i],
			     xmCascadeButtonGadgetClass))
	    {
		XtVaSetValues(MGR_Children(new_w)[i],
			      XmNsubMenuId,
			      RC_OptionSubMenu(new_w),
			      NULL);

		DEBUGOUT(_LtDebug2(__FILE__,
				   new_w,
				   MGR_Children(new_w)[i],
				   "Assign SubMenuId (%s)\n",
				   XtName(RC_OptionSubMenu(new_w))));
		break;
	    }
	}

	_XmFixOptionMenu(new_w, True);
    }

    /* rws take care of setValues on menuHistory */
    /* rws 21 Nov 1998
       rowcolumn/test46 shows that setting menuHistory on the submenu
       sets the item also!!!!
     */
    if (RC_MemWidget(new_w) != RC_MemWidget(old))
    {
	if (RC_Type(new_w) == XmMENU_OPTION)
	{
	    _XmOptionCallback(RC_MemWidget(new_w), (XtPointer)new_w, NULL);
	}
	else if (RC_LastSelectToplevel(new_w) && RC_Type(RC_LastSelectToplevel(new_w)) == XmMENU_OPTION)
	{
	    RC_MemWidget(RC_LastSelectToplevel(new_w)) = RC_MemWidget(new_w);
	    /* dwilliss 28-may-04
	     * Not enough to just set mem_widget on RC_LastSelectToplevel
	     * we need to tell it that its changed or the label on the
	     * option menu won't change
	     */
	    _XmOptionCallback(RC_MemWidget(RC_LastSelectToplevel(new_w)),
			    (XtPointer)RC_LastSelectToplevel(new_w), NULL);
	}
    }
    if (RC_NCol(new_w) != RC_NCol(old))
    {
	need_relayout = True;
    }

    if (need_relayout)
    {
	DEBUGOUT(_LtDebug(__FILE__, new_w,
			  "_XmRCAdjustSize from set_values\n"));
	DEBUGOUT(_LtDebug("EMACS", new_w,
			  "_XmRCAdjustSize from set_values\n"));
	_XmRCAdjustSize(new_w, NULL, NULL);
    }

	/* We may get stuff for our parent passed. If that's so, pass it along. */
	if (XmIsMenuShell(XtParent(new_w)) && (RC_Type(new_w) == XmMENU_POPUP
			|| RC_Type(new_w) == XmMENU_OPTION
			|| RC_Type(new_w) == XmMENU_PULLDOWN)) {
		Arg	al[5];
		int	i, ac = 0;
		for (i=0; i<*num_args; i++) {
			if (strcmp(args[i].name, XmNx) == 0) {
				XtSetArg(al[ac], XmNx, args[i].value);
				ac++;
			}
			if (strcmp(args[i].name, XmNy) == 0) {
				XtSetArg(al[ac], XmNy, args[i].value);
				ac++;
			}
		}
		if (ac)
			XtSetValues(XtParent(new_w), al, ac);
	}

    return need_refresh;
}
/* rws 23 Feb 1999
   Motif does not have this method
 */

/* T. Straumann: If asked for XmNchildren / XmNnumChildren by XtGetValues,
 *				 TearOffControl doesn't show up in the list (M*TIF behavior).
 *				 LESSTIF however passes TearOffControl as children[0] back
 *				 to the requestor.
 *				 I added a get_values_hook method to achieve M*TIF semantics.
 */
static void
get_values_hook(Widget w, ArgList args, Cardinal *nargs)
{
	Cardinal i;
	static XrmQuark Qchildren = NULLQUARK;
	static XrmQuark QnumChildren = NULLQUARK;
	XrmQuark *ql;

	/* return if there's no tear off control button */
	if (   ! RC_TearOffControl(w)
		|| ! MGR_Children(w) 
		|| MGR_Children(w)[0] != RC_TearOffControl(w) /* paranoia */)
		return;

	/* one time initialization */
	if ( NULLQUARK == Qchildren )
		Qchildren = XrmStringToQuark(XmNchildren);

	if ( NULLQUARK == QnumChildren )
		QnumChildren = XrmStringToQuark(XmNnumChildren);

	/* is it really worth the effort (quark vs. strcmp) ? */
	ql = (XrmQuark *)XtMalloc(sizeof(XrmQuark)* *nargs);

	for (i=0; i< *nargs; i++)
		ql[i] = XrmStringToQuark(args[i].name);

	/* search through the list whether they ask for children / numChildren */
	for (i=0; i< *nargs; i++)
	{
		/* reduce the number of children by one, if the
		 * TearOffControl is there.
		 */
		if ( ql[i] == QnumChildren )
			(*(Cardinal*)args[i].value)--;

		/* adjust the 'children' pointer; skip TearOffControl */
		if ( ql[i] == Qchildren )
			(*(WidgetList*)args[i].value)++;
	}
	XtFree((char*)ql);
}

static void
realize(Widget w, XtValueMask *value_mask, XSetWindowAttributes *attributes)
{

    DEBUGOUT(_LtDebug(__FILE__, w, "%s:realize(%d) - %dx%d\n",
    	__FILE__, __LINE__,
    	XtWidth(w), XtHeight(w)));

#define superclass (&xmManagerClassRec)
    (*superclass->core_class.realize) (w, value_mask, attributes);
#undef superclass
    if (XmIsMenuShell(XtParent(w)) && MS_PrivateShell(XtParent(w)))
    {
	/* if this is in a private menu shell it was managed during
	   insert_child.  However if someone (Mozilla's go menu)
	   un-realizes the menu and then realizes it again we must
	   make sure it is managed.
	 */
	XtManageChild(w);
    }
}

static void
resize(Widget w)
{
    if (!RC_FromResize(w))
    {
    /* rws 6 Jun 1998
       recursion avoidance.  Shows up when resizing the width of Mozilla
     */
	DEBUGOUT(_LtDebug(__FILE__, w,
	    "RC resize -> wid %d ht %d, call _XmRCAdjustSize\n",
	    XtWidth(w), XtHeight(w)));
	DEBUGOUT(_LtDebug("EMACS", w,
	    "RC resize -> wid %d ht %d, call _XmRCAdjustSize\n",
	    XtWidth(w), XtHeight(w)));
#if 0
	if (XtWindow(w))
	    XClearArea(XtDisplay(w), XtWindow(w),
		    0, 0, XtWidth(w), XtHeight(w),
		    False);
#endif

	RC_SetFromResize(w, 1);

	_XmRCAdjustSize(w, NULL, NULL);

	RC_SetFromResize(w, 0);

	if (RC_Type(w) != XmWORK_AREA && XtIsRealized(w))
	{
	    _XmClearShadowType(w, RC_OldWidth(w), RC_OldHeight(w),
			       RC_OldShadowThickness(w), 0);

	    _XmDrawShadows(XtDisplay(w), XtWindow(w),
			   MGR_TopShadowGC(w), MGR_BottomShadowGC(w),
			   0, 0, XtWidth(w), XtHeight(w),
			   MGR_ShadowThickness(w), XmSHADOW_OUT);
	}
	RC_OldWidth(w) = XtWidth(w);
	RC_OldHeight(w) = XtHeight(w);
	RC_OldShadowThickness(w) = MGR_ShadowThickness(w);
    }
}

static void
expose(Widget w, XEvent *event, Region region)
{
    DEBUGOUT(_LtDebug(__FILE__, w, "Expose (wid %d ht %d)\n",
		XtWidth(w), XtHeight(w)));

    if (RC_Type(w) != XmWORK_AREA)
    {
	_XmDrawShadows(XtDisplay(w),
		       XtWindow(w),
		       MGR_TopShadowGC(w),
		       MGR_BottomShadowGC(w),
		       0, 0,
		       XtWidth(w),
		       XtHeight(w),
		       MGR_ShadowThickness(w),
		       XmSHADOW_OUT);
    }

    /* display the gadgets, if there are any */
    _XmRedisplayGadgets(w, event, region);
}

static XtGeometryResult
query_geometry(Widget w,
	       XtWidgetGeometry *request,
	       XtWidgetGeometry *reply)
{
    XtWidgetGeometry rcg;

    Dimension width, height;

    DEBUGOUT(_LtDebug(__FILE__, w,
		      "RC QueryGeometry, request %s\n",
		      _LtDebugWidgetGeometry2String(request)));

    DEBUGOUT(_LtDebug(__FILE__, w,
		      "_XmRCPreferredSize from query_geometry\n"));

    rcg = *request;
    _XmRCPreferredSize(w, &rcg);

#if 1
    /* test36 test38 test39 */
    rcg.width = rcg.width < 16 ? 16 : rcg.width;
    rcg.height = rcg.height < 16 ? 16 : rcg.height;

    width = (request->request_mode & CWWidth) ? (request->width < 16 ? 16 : request->width) : rcg.width;
    height = (request->request_mode & CWHeight) ? (request->height < 16 ? 16 : request->height) : rcg.height;
#endif

    DEBUGOUT(_LtDebug(__FILE__, w,
		      "preferred size %s\n", _LtDebugWidgetGeometry2String(&rcg)));

    reply->width = width;
    reply->height = height;

    return _XmGMReplyToQueryGeometry(w, request, reply);
}

/*
 * Every time we return XtGeometryYes, we have a situation where a
 *  child widget is granted permission to change its geometry. We'll have to
 *  assume that it does this, and change our "boxes".
 * An alternative would be to indicate in boxes that its contents is invalid,
 *  and have the next user of boxes look up the new geometry info.
 * Partially applied 25/8/1996 (Danny).
 */
static XtGeometryResult
geometry_manager(Widget w,
		 XtWidgetGeometry *request,
		 XtWidgetGeometry *reply)
{
    Widget rc = XtParent(w);
    XtWidgetGeometry wants;
    int ask, got;

    wants = *request;
    *reply = wants;

    DEBUGOUT(_LtDebug2(__FILE__, rc, w,
		       "RC geometry_manager: _XmRCAdjustSize\n"));
    DEBUGOUT(_LtDebug(__FILE__, rc,
		      "Is currently %d %d.\n", XtWidth(rc), XtHeight(rc)));
    DEBUGOUT(_LtDebug2("EMACS", rc, w,
		       "RC geometry_manager: _XmRCAdjustSize\n"));
    DEBUGOUT(_LtDebug("EMACS", rc,
		      "Is currently %d %d.\n", XtWidth(rc), XtHeight(rc)));

    if (_XmRCAdjustSize(rc, w, reply) == XtGeometryNo)
    {
	DEBUGOUT(_LtDebug(__FILE__, rc, "Parent said no. So will we.\n"));

	return XtGeometryNo;
    }

    DEBUGOUT(_LtDebug(__FILE__, rc,
		      "RC geometry_manager after _XmRCAdjustSize [%s] now at %d %d\n",
			   _LtDebugWidgetGeometry2String(reply),
		      XtWidth(rc), XtHeight(rc)));
    DEBUGOUT(_LtDebug("EMACS", rc,
		      "RC geometry_manager after _XmRCAdjustSize\n"));
    DEBUGOUT(_LtDebug("EMACS", rc,
		      "Now at %d %d.\n", XtWidth(rc), XtHeight(rc)));

    reply->request_mode &= wants.request_mode;

    ask = got = 0;

    if ((wants.request_mode & CWWidth) == CWWidth)
    {
	ask++;

	if (reply->width == wants.width)
	{
	    got++;
	}
	else 
	{
	    reply->request_mode &= ~CWWidth;
	}
    }
    if ((wants.request_mode & CWHeight) == CWHeight)
    {
	ask++;

	if (reply->height == wants.height)
	{
	    got++;
    	}
	else 
	{
	    reply->request_mode &= ~CWHeight;
	}
    }
    if ((wants.request_mode & CWBorderWidth) == CWBorderWidth)
    {
	ask++;

	if (reply->border_width == wants.border_width)
	{
	    got++;
	}
	else 
	{
	    reply->request_mode &= ~CWBorderWidth;
	}
    }

    /*
     * the next two are a little tricky.  If the child asked for x/y
     * and the packing type isn't NONE, they don't get it.
     */
    if ((wants.request_mode & CWX) == CWX)
    {
	ask++;

	if (RC_Packing(rc) == XmPACK_NONE && reply->x == wants.x)
	{
	    got++;
	}
	else 
	{
	    reply->request_mode &= ~CWX;
	}
    }
    if ((wants.request_mode & CWY) == CWY)
    {
	ask++;

	if (RC_Packing(rc) == XmPACK_NONE && reply->y == wants.y)
	{
	    got++;
	}
	else 
	{
	    reply->request_mode &= ~CWY;
	}
    }

    /* are we ok with everything they want. */
    if (ask == got && ask != 0)
    {
	DEBUGOUT(_LtDebug2(__FILE__, rc, w,
			   "geometry_manager request [%s] reply [%s] => YES\n",
			   _LtDebugWidgetGeometry2String(&wants),
			   _LtDebugWidgetGeometry2String(reply)));
	DEBUGOUT(_LtDebug2("EMACS", rc, w,
			   "geometry_manager request [%s] reply [%s] => YES\n",
			   _LtDebugWidgetGeometry2String(&wants),
			   _LtDebugWidgetGeometry2String(reply)));

	_XmRCSetMargins(rc);
	_XmRCSetKidGeo(RC_Boxes(rc), w);

	return XtGeometryYes;
    }
    /* no, but we can always compromise. LayoutNone is mostly handled if
     * we got No when we made the geometry request. Otherwise the layout
     * routine computed a compromise, which is typically the size they
     * are before they made this request. */
    else
    {
	if (RC_Packing(rc) != XmPACK_NONE)
	{
	    reply->request_mode &= ~(CWX | CWY);
	}

	DEBUGOUT(_LtDebug2(__FILE__, rc, w,
			"geometry_manager request [%s] reply [%s] => ALMOST\n",
			   _LtDebugWidgetGeometry2String(&wants),
			   _LtDebugWidgetGeometry2String(reply)));
	DEBUGOUT(_LtDebug2("EMACS", rc, w,
			"geometry_manager request [%s] reply [%s] => ALMOST\n",
			   _LtDebugWidgetGeometry2String(&wants),
			   _LtDebugWidgetGeometry2String(reply)));

	return XtGeometryAlmost;
    }
}

static void
change_managed(Widget w)
{
Widget tlm;

    DEBUGOUT(_LtDebug(__FILE__, w, "change_managed()\n"));

    DEBUGOUT(_LtDebug(__FILE__, w, "_XmRCAdjustSize from change_managed %dx%d\n",
    	XtWidth(w), XtHeight(w)));
    DEBUGOUT(_LtDebug("EMACS", w, "_XmRCAdjustSize from change_managed %dx%d\n",
    	XtWidth(w), XtHeight(w)));

    _XmRCAdjustSize(w, NULL, NULL);

    /* rws 28 Apr 1998
       If widget w is a pulldown menu that is associated to an option menu
       we have a problem here.  The option menu XmNmenuHistory could be
       pointing to a widget that has just been un-managed.  This becomes
       a problem if the un-manage is the first step of a destroy.  If we 
       do not find the associated option menu and fix up its XmNmenuHistory
       it will be left pointing to a widget that has been destroyed.  What
       we need is some sort of pointer from the pulldown menu to the 
       option menu.  RC_CascadeBtn() could be used but, currently it is
       only valid when the pulldown is visible.  The same can be said
       for RC_LastSelectToplevel().

       RC_CascadeBtn() is set to NULL in MenuShell:MenuShellPopdownOne()
       RC_LastSelectToplevel() will be set correctly whenever the menu
	   gets popped up. If we ensure that it is set correctly whenever
	   the subMenuId of an option menu changes, that should do it!
     */
    {
    	tlm = RC_LastSelectToplevel(w);
    	if (tlm && RC_Type(tlm) == XmMENU_OPTION)
    	{
    	Widget mem;

    		mem = RC_MemWidget(tlm);
    		/* rws 25 May 1998
    		   I do not really like this XtIsSensitive stuff.  We should
    		   be able to pick a new mem widget even if the menu is not
    		   sensitive.  This is needed for xmcd 2.3 otherwise it
    		   screws up on startup because it creates an in-sensitve
    		   option menu.
    		 */
    		if (XtIsSensitive(tlm) && mem && !XtIsManaged(mem))
    		{
    		Widget new_mem;

    			/* need to pick a new memWidget */
    			new_mem = _XmMenuNextItem(w, MGR_Children(w)[MGR_NumChildren(w) - 1]);
    			if (new_mem != mem)
    			{
			    _XmOptionCallback(new_mem, (XtPointer)tlm, NULL);
    			}
    			else
    			{
			    _XmOptionCallback(NULL, (XtPointer)tlm, NULL);
    			}
    		}
    	}
    }
    if (tlm && RC_Type(tlm) == XmMENU_OPTION)
    {
	/* rws 30 Apr 1998
	   we need to force the option menu (tlm) to re-calculate its
	   layout based on the new size of its submenu (w)
	 */
        _XmRCAdjustSize(tlm, NULL, NULL);  /* dwilliss 17 Jun 2004 - OK, now it does :-) */
    }

    if (RC_Type(w) != XmWORK_AREA && XtIsRealized(w))
    {
	_XmClearShadowType(w, RC_OldWidth(w), RC_OldHeight(w),
			   RC_OldShadowThickness(w), 0);

	_XmDrawShadows(XtDisplay(w), XtWindow(w),
		       MGR_TopShadowGC(w), MGR_BottomShadowGC(w),
		       0, 0, XtWidth(w), XtHeight(w),
		       MGR_ShadowThickness(w), XmSHADOW_OUT);
    }
    RC_OldWidth(w) = XtWidth(w);
    RC_OldHeight(w) = XtHeight(w);
    RC_OldShadowThickness(w) = MGR_ShadowThickness(w);

    _XmNavigChangeManaged(w);
}

static void
AddPopupHandlers(Widget new_w)
{
    Widget realpar = NULL;

    /* since we're not managed -- we leave that up to the application
     * to get us to pop up, we realize ourselves here. */
    /* XtRealizeWidget(new_w); */

    if (XtIsShell(XtParent(new_w)))
    {
	realpar = XtParent(XtParent(new_w));
    }
    else
    {
	realpar = XtParent(new_w);
    }
    if (realpar == NULL)
    {
	_XmError(new_w, "Can't determine parent of popup menu!\n");
    }

    XtInsertEventHandler(realpar, ButtonPressMask|ButtonReleaseMask,
			 False, _XmPopupButtonPressHandler, (XtPointer)new_w,
			 XtListHead);

    DEBUGOUT(_LtDebug(__FILE__, NULL, "GRAB BUTTON: %p %s %d %x\n",
		      realpar, XtName(realpar),
		      RC_PostButton(new_w), RC_PostModifiers(new_w)));

    XtGrabButton(realpar, RC_PostButton(new_w), RC_PostModifiers(new_w),
		 True, ButtonReleaseMask, GrabModeSync, GrabModeSync,
		 XtWindow(realpar), _XmGetMenuCursorByScreen(XtScreen(new_w)));
}

static void
MenuProcEntry(int proc, Widget w,...)
{
    va_list arg_list;
    Display *dpy = XtDisplay(w);
    /* Widget top_menu; */
    Widget *shell;
    Boolean *was_torn;
    XEvent *event;
 
    va_start(arg_list, w);

    DEBUGOUT(_LtDebug("MENU", w, "%s:MenuProcEntry(%d) - %s\n",
	__FILE__, __LINE__,
	_LtDebugMenuEnum2String(proc)
	));
    DEBUGOUT(_LtDebug("ENTRY", w, "%s:MenuProcEntry(%d) - %s\n",
	__FILE__, __LINE__,
	_LtDebugMenuEnum2String(proc)
	));

    switch (proc)
    {
    case XmMENU_BUTTON_POPDOWN:
	/* widget is requesting that the menu it is in be popped down
	 */
	{
	Widget button = w;
	Widget menu = XtParent(button);
	Widget shell;
	Boolean *poppedUp;
	Cardinal num_params = 0;

	    event = (XEvent *)va_arg(arg_list, XtPointer);
	    poppedUp = va_arg(arg_list, XtPointer);

	    if (XmIsRowColumn(menu))
	    {
		shell = XtParent(menu);
		if (XmIsMenuShell(shell))
		{
		    if (Shell_PoppedUp(shell))
		    {
			_XmMenuFocus(w, XmMENU_FOCUS_RESTORE, CurrentTime);
			MSClass_PopdownDone(shell)(shell, event, NULL, &num_params);
#if 1
			if (RC_CascadeBtn(menu))
			{
			    Lab_MenuDisarm(RC_CascadeBtn(menu));
			    if (RC_PopupPosted(XtParent(RC_CascadeBtn(menu))) && RC_PopupPosted(XtParent(RC_CascadeBtn(menu))) == menu)
			    {
				RC_PopupPosted(XtParent(RC_CascadeBtn(menu))) = NULL;
				_XmCallRowColumnUnmapCallback(menu, event);
			    }
			}
#endif
			*poppedUp = True;
		    }
		    else
		    {
			*poppedUp = False;
		    }
		}
		else
		{
		    if (_XmIsActiveTearOff(menu))
		    {
		    }
		    else
		    {
			_XmWarning(w, "%s(%d) - RCClass_MenuProcs XmMENU_BUTTON_POPDOWN called with button not in a menu shell", __FILE__, __LINE__);
		    }
		    *poppedUp = False;
		}
		/* set the history of menu to button */
		if (!XmIsTearOffButton(button))
		{
		    RC_MemWidget(menu) = button;
		}
		/* if an option menu, change the CascadeButtonGadget label */
		{
		Widget tlm;

		    tlm = RC_LastSelectToplevel(menu);
		    if (tlm && XmIsRowColumn(tlm) && RC_Type(tlm) == XmMENU_OPTION)
		    {
		    	_XmOptionCallback(RC_MemWidget(menu), (XtPointer)tlm, NULL);
		    }
		}
	    }
	    else
	    {
		_XmWarning(w, "%s(%d) - RCClass_MenuProcs XmMENU_BUTTON_POPDOWN called with button not in a menu", __FILE__, __LINE__);
		*poppedUp = False;
	    }
	}
	break;

    case XmMENU_PROCESS_TREE:
	break;

    case XmMENU_TRAVERSAL:
	/* Arm the first menu item that is not a TearOff
	 */
	{
	XmTraversalDirection dir;
	Widget menu = w;

	    dir = va_arg(arg_list, XmTraversalDirection);
	    switch(dir)
	    {
	    case XmTRAVERSE_HOME:
	        if (RC_CascadeBtn(menu) != 0 && RC_MemWidget(menu) != 0) 
	        {
		        _XmMenuArmItem(RC_MemWidget(menu));
		}
		else if (MGR_NumChildren(menu) > 0 && XmGetTearOffControl(menu) != MGR_Children(menu)[0])
		{
		Widget w1 = MGR_Children(menu)[0];

			_XmMenuArmItem(w1);
		}
		else if (MGR_NumChildren(menu) > 1)
		{
		Widget w1 = MGR_Children(menu)[1];

			_XmMenuArmItem(w1);
		}
	    	break;
	    default:
		_XmWarning(w, "%s(%d) - RCClass_MenuProcs XmMENU_TRAVERSAL called with unknown direction", __FILE__, __LINE__);
	    	break;
	    }
	}
	break;

    case XmMENU_SHELL_POPDOWN:
	{
	Widget button = w;
	Widget menu = XtParent(button);
	Widget shell;
	Boolean *poppedUp;
	Cardinal num_params = 0;

	    event = (XEvent *)va_arg(arg_list, XtPointer);
	    poppedUp = va_arg(arg_list, XtPointer);
	    if (XmIsRowColumn(menu))
	    {
		shell = _XmGetRC_PopupPosted(menu);
		if (shell)
		{
		    if (XmIsMenuShell(shell))
		    {
			if (Shell_PoppedUp(shell))
			{
			    MSClass_PopdownEveryone(shell)(shell, event, NULL, &num_params);
			    *poppedUp = True;
			}
			else
			{
			    *poppedUp = False;
			}
		    }
		    else
		    {
			if (XtIsTransientShell(shell) || _XmIsActiveTearOff(menu))
			{
			}
			else
			{
			    _XmWarning(w, "%s(%d) - RCClass_MenuProcs XmMENU_SHELL_POPDOWN called with button not in a menu shell %s %s", __FILE__, __LINE__, XtName(menu), XtName(shell));
			}
			*poppedUp = False;
		    }
		}
		else
		{
		    /* nothing popped up */
		    *poppedUp = False;
		}
	    }
	    else
	    {
		_XmWarning(w, "%s(%d) - RCClass_MenuProcs XmMENU_SHELL_POPDOWN called with button not in a menu", __FILE__, __LINE__);
		*poppedUp = False;
	    }
	}
	break;

    case XmMENU_CALLBACK:
	/* Called by all label sub-classes that are children of RC just
	   before invoking any activate or valuechanged callbacks.
	 */
	{
	XmAnyCallbackStruct *cbs;

	    DEBUGOUT(_LtDebug("ENTRY", w, "%s:MenuProcEntry(%d) - %s %sRowColumn %s entryCallbacks\n",
		__FILE__, __LINE__,
		_LtDebugMenuEnum2String(proc),
		XmIsRowColumn(XtParent(w)) ? "" : "not ",
		XtHasCallbacks(XtParent(w), XmNentryCallback) == XtCallbackHasSome ? "has" : "no"
		));

	    if (RC_RadioBehavior(XtParent(w)))
		RadioHandler(w);

	    cbs = va_arg(arg_list, XmAnyCallbackStruct *);
	    if (XtHasCallbacks(XtParent(w), XmNentryCallback) == XtCallbackHasSome)
	    {
	    XmRowColumnCallbackStruct rcb;

		rcb.reason = XmCR_ACTIVATE;
		rcb.event = cbs->event;
		rcb.widget = w;
		rcb.callbackstruct = (char *)cbs;
		if (XtHasCallbacks(w, XmNactivateCallback) == XtCallbackHasSome || 
		    XtHasCallbacks(w, XmNvalueChangedCallback) == XtCallbackHasSome)
		{
		XtCallbackList list = NULL;
		int i;

		    /* rws 8 Jul 1999
		       We have to do a GetValues here because Xt seems to
		       store callback lists in some sort of compiled form.
		     */
		    XtVaGetValues(w,
			XmNactivateCallback, &list,
			XmNvalueChangedCallback, &list,
			NULL);
		    for (i = 0; list[i].callback != NULL; i++)
		    {
			rcb.data = (char *)list[i].closure;
			XtCallCallbackList(XtParent(w), RC_Entry_cb(XtParent(w)), (XtPointer)&rcb);
		    }
		}
		else
		{
		    rcb.data = "";
		    XtCallCallbackList(XtParent(w), RC_Entry_cb(XtParent(w)), (XtPointer)&rcb);
		}
	    }
	    else
	    {
	    }
	}
	break;

    case XmMENU_BUTTON:
	/* widget got a button event, verify that it is a valid button for
	   this type of menu
	 */
	{
	Widget button = w;
	Widget menu = XtParent(button);
	XButtonEvent *xbe;
	Boolean *validButton;
	Widget tlm;

	    if (XmIsRowColumn(button))
	    {
	    	menu = button;
	    }
	    event = (XEvent *)va_arg(arg_list, XtPointer);
	    validButton = va_arg(arg_list, XtPointer);
	    DEBUGOUT(_LtDebug0("MENU", w, "\t%s\n",
	    	_LtDebugEventType2String(event->type)
	    	));
	    if (event && (event->type == ButtonPress || event->type == ButtonRelease))
	    {
		xbe = &(event->xbutton);
		if (XmIsRowColumn(menu))
		{
		    _XmGetActiveTopLevelMenu(menu, &tlm);
		    if (tlm && XmIsRowColumn(tlm))
		    {
			DEBUGOUT(_LtDebug0("MENU", w, "\t%s %s\n",
			    _LtDebugRcType2String(RC_Type(tlm)),
			    XtName(tlm)
			    ));
			switch(RC_Type(tlm))
			{
			case XmMENU_BAR:
			case XmMENU_PULLDOWN:
			    *validButton = (xbe->button == 1);
			    break;
			case XmMENU_OPTION:
			    *validButton = (xbe->button == 1) && 
					  ((Widget)_XmInputForGadget(w, xbe->x, xbe->y) == XmOptionButtonGadget(w));
			    break;
			case XmMENU_POPUP:
			    *validButton = ((xbe->button == 1) ||
					    (xbe->button == 3));
			    break;
			default:
			    _XmWarning(w, "%s(%d) - RCClass_MenuProcs XmMENU_BUTTON do not know the type of TopLevelMenu %s", __FILE__, __LINE__,XtName(tlm));
			    *validButton = False;
			    break;
			}
		    }
		    else
		    {
			_XmWarning(w, "%s(%d) - RCClass_MenuProcs XmMENU_BUTTON could not find TopLevelMenu", __FILE__, __LINE__);
			*validButton = False;
		    }
		}
		else
		{
		    _XmWarning(w, "%s(%d) - RCClass_MenuProcs XmMENU_BUTTON called by widget without RC parent", __FILE__, __LINE__);
		    *validButton = False;
		}
	    }
	    else
	    {
		_XmWarning(w, "%s(%d) - RCClass_MenuProcs XmMENU_BUTTON called with non button event", __FILE__, __LINE__);
		*validButton = False;
	    }
	}
	break;

    case XmMENU_CASCADING:
	{
	Widget cb = w;
	Widget sub_pane, mem_widget;
	Widget shell;
	Position my_x, my_y;
	unsigned char string_direction;

	    /* FIX ME: lots of this should move to MenuShell:manage_child */
	    event = (XEvent *)va_arg(arg_list, XtPointer);

	    if (XmIsGadget(cb))
	    {
		sub_pane = CBG_Submenu(cb);
		string_direction = LabG_StringDirection(cb);
	    }
	    else
	    {
		sub_pane = CB_Submenu(cb);
		string_direction = Lab_StringDirection(cb);
	    }

	    DEBUGOUT(_LtDebug0(__FILE__, cb, "\t%s sub-menu %s\n",
	    	_LtDebugRcType2String(RC_Type(XtParent(cb))),
	    	sub_pane ? XtName(sub_pane) : "None"
	    	));
	    DEBUGOUT(_LtDebug0("MENU", cb, "\t%s sub-menu %s\n",
	    	_LtDebugRcType2String(RC_Type(XtParent(cb))),
	    	sub_pane ? XtName(sub_pane) : "None"
	    	));
	    if (!sub_pane)
	    {
		DEBUGOUT(_LtDebug(__FILE__, cb, "Cascading Popup: no sub pane\n"));

		return;
	    }
	    {
	    Widget shell;
	    Boolean was_torn;

		RCClass_MenuProcs(XtClass(sub_pane))(XmMENU_RESTORE_TEAROFF_TO_MENUSHELL,
					     sub_pane, &shell, &was_torn, event);
	    }

	    /* FIX ME: if shared menu shell, configure object, ordinary popup */

	    shell = XtParent(sub_pane);

	    /* Register inside the RC that this cascadebutton is the one that 
	     * triggered the menu.
	     */
	    RC_CascadeBtn(sub_pane) = cb;

	    /* position the row column inside the menushell */
	    _XmMoveObject(sub_pane, 0, 0);

	    /* now move the menushell */
	    /* rws 21 Dec 1997
	       If the is a tear off it is possible that it has been re-parented
	       a few times.  This seems to mess up where Xt thinks the cascade
	       button is. Therefore we must suffer the round trip server request
	       to make sure that we get the correct position.
	     */
#undef TEAROFF_FIX
            /* amai: xmgrace 5.x works better w/o this ... */
	       
#ifdef TEAROFF_FIX
	    if (!_XmIsActiveTearOff(XtParent(cb)))
	    {
#endif
		XtTranslateCoords(cb, 0, 0, &my_x, &my_y);
#ifdef TEAROFF_FIX
	    }
	    else
	    {
	    Window child;
	    int x,y;

		XTranslateCoordinates(XtDisplay(cb),
			XtWindow(cb),
			RootWindowOfScreen(XtScreen(cb)),
			0, 0,
			&x, &y,
			&child);
		my_x = x; my_y = y;
	    }
#endif

	    switch (RC_Type(XtParent(cb)))
	    {
	    case XmMENU_BAR:

		if (string_direction == XmSTRING_DIRECTION_R_TO_L)
		    my_x += XtWidth(cb) - XtWidth(sub_pane);

		my_y += XtHeight(cb);

		/* If the menu goes off the bottom of the screen,
		 * try placing it above the bar.
		 */
		if (my_y > (Position)(HeightOfScreen(XtScreen(cb))
				      - XtHeight(sub_pane)) &&
		    my_y - (Position)(XtHeight(cb) + XtHeight(sub_pane)) >= 0)
		    my_y -= XtHeight(cb) + XtHeight(sub_pane);
		break;

	    case XmMENU_POPUP:
		_XmCallRowColumnMapCallback(sub_pane, event);

	    case XmMENU_PULLDOWN:

		my_x += string_direction == XmSTRING_DIRECTION_L_TO_R
		    ? (XmIsGadget(cb)
		       ? CBG_Cascade_x(cb) + CBG_Cascade_width(cb)
		       : CB_Cascade_x(cb) + CB_Cascade_width(cb))
		    : (XmIsGadget(cb)
		       ? CBG_Cascade_x(cb)
		       : CB_Cascade_x(cb))
		      - XtWidth(sub_pane);

		my_y += XmIsGadget(cb)
		    ? CBG_Cascade_y(cb)
		    : CB_Cascade_y(cb);
		break;

	    case XmMENU_OPTION:

		mem_widget = RC_MemWidget(XtParent(cb));

		my_x += string_direction == XmSTRING_DIRECTION_L_TO_R
		    ? (LabG_Highlight(cb)
		       + (mem_widget
			  ? MGR_ShadowThickness(sub_pane) - XtX(mem_widget)
			  : 0))
		    : (XtWidth(cb) - XtWidth(sub_pane) - LabG_Highlight(cb)
		       - (mem_widget
			  ? MGR_ShadowThickness(sub_pane) - XtX(mem_widget)
			  : 0));

		/* If the menu goes off one side of the screen,
		 * try placing it one the opposide side of the cascade.
		 */
		if (my_x > (Position)(WidthOfScreen(XtScreen(cb))
				      - XtWidth(sub_pane)) &&
		    my_x - (Position)XtWidth(sub_pane) >= 0)
		{
		    my_x -= string_direction == XmSTRING_DIRECTION_L_TO_R
			? XtWidth(sub_pane)
			: XtWidth(cb);
		}
		else if (my_x < 0 && my_x + (Position)(XtWidth(cb)) <=
			 (Position)(WidthOfScreen(XtScreen(cb))
				    - XtWidth(sub_pane)))
		{
		    my_x += string_direction == XmSTRING_DIRECTION_L_TO_R
			? XtWidth(cb)
			: XtWidth(sub_pane);
		}

		my_y += mem_widget
		    ? ((XtHeight(cb) - XtHeight(mem_widget)) / 2)
		      - XtY(mem_widget)
		    : LabG_Highlight(cb);
		break;
	    }

	    /* Generic check to make sure the menu doesn't go off the edge
	     * of the screen, especially not the top or left [right].
	     * But see specific tweaks for menu_bar Y and menu_option X.
	     */
	    if (string_direction == XmSTRING_DIRECTION_R_TO_L && my_x < 0)
		my_x = 0;
	    if (my_x >
		(Position)(WidthOfScreen(XtScreen(cb)) - XtWidth(sub_pane)))
		my_x = WidthOfScreen(XtScreen(cb)) - XtWidth(sub_pane);
	    if (string_direction == XmSTRING_DIRECTION_L_TO_R && my_x < 0)
		my_x = 0;

	    if (my_y >
		(Position)(HeightOfScreen(XtScreen(cb)) - XtHeight(sub_pane)))
		my_y = HeightOfScreen(XtScreen(cb)) - XtHeight(sub_pane);
	    if (my_y < 0)
		my_y = 0;

	    _XmMoveObject(shell, my_x, my_y);

	    RC_PopupPosted(XtParent(cb)) = sub_pane;
	    DEBUGOUT(_LtDebug(__FILE__, cb, "%s posted by %s top %s\n",
			XtName(sub_pane),
			XtName(XtParent(cb)),
			RC_LastSelectToplevel(XtParent(cb)) ? XtName(RC_LastSelectToplevel(XtParent(cb))) : "NULL"));

	    RC_LastSelectToplevel(sub_pane) = RC_LastSelectToplevel(XtParent(cb));
	    {
	    int i;

	    	for (i = 0; i < MGR_NumChildren(sub_pane); i++)
	    	{
	    	Widget w = MGR_Children(sub_pane)[i];

		    _XmMenuDisarmItem(w);
	    	}
	    }

	    _XmCallRowColumnMapCallback(sub_pane, event); /* rowcolumn/test45 */
	    /* This private shell stuff should be hidden in MenuShell somewhere
	       Nothing but MenuShell needs to know anything about private shells!
	     */
	    if (!MS_PrivateShell(shell))
	    {
		XtManageChild(sub_pane);
		/* rws 13 Mar 1997
		   Not too sure about this yet so keep it out for now.
		   rws 5 Nov 1998
		   rowcolumn/test43 needs this.
		*/
		XAllowEvents(XtDisplay(sub_pane), SyncPointer, CurrentTime);
 
		_XmAddGrab(shell, False, False);
	    }
	    else
	    {
		MSClass_PopupSharedMenuPane(shell)(shell, sub_pane, event);
	    }
	}
	break;

    case XmMENU_SUBMENU:
	{
	Widget button = w;
	Widget submenu = XmIsGadget(w) ? CBG_Submenu(w) : CB_Submenu(w);

	    if (RC_Type(XtParent(w)) == XmMENU_OPTION &&
		submenu != NULL && XmIsRowColumn(submenu))
	    {
		RC_CascadeBtn(submenu) = button;
		RC_OptionSubMenu(XtParent(w)) = submenu;
		/* rws 5 May 1998
		   should be setting RC_LastSelectTopLevel(submenu) = XtParent(button);
		   I think.
		 */

		if (XmIsGadget(w))
		{
		    _XmFixOptionMenu(XtParent(w), False);
		}
		else
		{
		    _XmFixOptionMenu(XtParent(w), True);
		}
	    }
	}
	break;

    case XmMENU_ARM:
	/* grab the keyboard and freeze everything so that we can be sure that
	   the next event goes to the right place. */

	_XmGrabKeyboard(w, True,
			GrabModeSync, GrabModeSync,
			CurrentTime);


	XAllowEvents(dpy, AsyncKeyboard, CurrentTime);

	_XmAddGrab(w, True, True);

	/* Initiate a new pointer grab for the menu bar */
	/* Pointer mode will change to Sync anyway with the
	   XAllowEvents. It's also set here, according to xscope - Chris 6/23 */
	_XmGrabPointer(w, True,
		       (ButtonPressMask | ButtonReleaseMask | EnterWindowMask |
			LeaveWindowMask),
		       GrabModeSync,
		       GrabModeAsync,
		       None,
		       _XmGetMenuCursorByScreen(XtScreen(w)),
		       CurrentTime);


	/* Process events until the next button event */
	XAllowEvents(dpy, SyncPointer, CurrentTime);

	RC_SetArmed(w, True);
	break;

    case XmMENU_DISARM:
	_XmUngrabPointer(w, CurrentTime);

	_XmUngrabKeyboard(w, CurrentTime);

	_XmRemoveGrab(w);


	RC_SetArmed(w, False);

	break;

    case XmMENU_BAR_CLEANUP:
	{
	Cardinal i = 0;

	    DoBtnEventCleanupReplay(w, NULL, NULL, &i);
	}
	break;

    case XmMENU_STATUS:
	break;

    case XmMENU_MEMWIDGET_UPDATE:
	/* rws 5 May 1998
	   this could call _XmFixOptionMenu
	 */
	break;

    case XmMENU_POPDOWN:
	break;

    case XmMENU_RESTORE_EXCLUDED_TEAROFF_TO_TOPLEVEL_SHELL:
	event = va_arg(arg_list, XEvent *);
	_XmRestoreExcludedTearOffToToplevelShell(w, event);
	break;

    case XmMENU_RESTORE_TEAROFF_TO_TOPLEVEL_SHELL:
	/* fix up RC_LastSelectTopLevel and RC_CascadeBtn */
	event = va_arg(arg_list, XEvent *);
	_XmRestoreTearOffToToplevelShell(w, event);
	break;

    case XmMENU_RESTORE_TEAROFF_TO_MENUSHELL:
	shell = va_arg(arg_list, Widget *);
	was_torn = va_arg(arg_list, Boolean *);
	event = va_arg(arg_list, XEvent *);

	/* fix up RC_LastSelectTopLevel and RC_CascadeBtn */
	if (RC_TearOffModel(w) == XmTEAR_OFF_ENABLED)
	{
	    DEBUGOUT(_LtDebug(__FILE__, w,
			      "restoring tear off menu to menu shell\n"));
 
	    if (RC_TornOff(w))
	    {
		*was_torn = True;
	    }
 
	    _XmRestoreTearOffToMenuShell(w, event);
 
	    *shell = XtParent(w);
	}
	break;

    case XmMENU_GET_LAST_SELECT_TOPLEVEL:
	break;

    case XmMENU_TEAR_OFF_ARM:
	/*
	RCClass_MenuProcs(xmRowColumnWidgetClass)(XmMENU_ARM, w, NULL);
	*/
	break;

    default:
	break;
    }

    va_end(arg_list);
}

static void
ArmAndActivate(Widget w, XEvent *e, String *args, Cardinal *nargs)
{
}

/* rws add callbacks to sub menu */
static void
_XmFixOptionMenu(Widget new_w, Boolean use_set_values)
{
    /* rws
     * Add an activateCallback to all the children of the menu
     * display the label of the menuHistory or first item
     * set menuHistory if necessary
     */

    XmString Label;
    Pixmap labelPixmap = None, labelInsensitivePixmap = None;
    unsigned char labelType;

    DEBUGOUT(_LtDebug(__FILE__, new_w, "_XmFixOptionMenu\n"));

    if (RC_OptionSubMenu(new_w))
    {
	RC_OptionSubMenu(RC_OptionSubMenu(new_w)) = new_w;

	/* Try to set a history. This may fail when the menu is empty */
	/* Crash prevention for test/Xm/scrolledwindow/test13 */
	if (RC_MemWidget(new_w) == NULL)
	{
	    if (MGR_NumChildren(RC_OptionSubMenu(new_w)))
	    {

		if (XmIsTearOffButton(MGR_Children(RC_OptionSubMenu(new_w))[0]))
		{
		    if (MGR_NumChildren(RC_OptionSubMenu(new_w)) > 2)
		    {
			RC_MemWidget(new_w) =
			    MGR_Children(RC_OptionSubMenu(new_w))[1];
		    }
		}
		else
		{
		    RC_MemWidget(new_w) =
			MGR_Children(RC_OptionSubMenu(new_w))[0];
		}

		DEBUGOUT(_LtDebug2(__FILE__, new_w, RC_MemWidget(new_w),
				   "_XmFixOptionMenu: Set RC_MemWidget\n"));
	    /* 1-sep-04 -- dwilliss - don't ever let MemWidget get set to, for example, a seperator
	       because we always assume it's a label of some kind */
		if (RC_MemWidget(new_w) != NULL && !XmIsLabel(RC_MemWidget(new_w)) && !XmIsLabelGadget(RC_MemWidget(new_w))) 
		{
		    RC_MemWidget(new_w) = NULL;
		}
	    }
	}

	if (RC_MemWidget(new_w) != NULL)
	{
	    Widget button;

	    XtVaGetValues(RC_MemWidget(new_w), 
	    		XmNlabelString, &Label,
			XmNlabelPixmap, &labelPixmap, 
			XmNlabelInsensitivePixmap, &labelInsensitivePixmap,
			XmNlabelType, &labelType,
			NULL);

	    /*
	     * OK, here's the fix for the bug demonstrated in
	     * test/Xm/rowcolumn/test26.c.  The error is in the
	     * usage of _XmFixOptionMenu in CascadeBG's initialize() and
	     * set_values() methods.
	     */
	     /* rws 15 Nov 1998
	        This may not be relevent anymore.  There was a problem with
	        memory allocation in MessageB up to and including 1.28 such
	        that we were allocating 1 less row than we were using to
	        do the layout, therefore we were messing up some memory that
	        we should not have been.  Since I can't remember how test26
	        demonstrated this problem I'm going to leave this here.
	      */
	    if ((button = XmOptionButtonGadget(new_w)) != NULL)
	    {
		if (use_set_values)
		{
		    XtVaSetValues(button,
				  XmNlabelString, Label,
				  XmNlabelPixmap, labelPixmap, 
				  XmNlabelInsensitivePixmap, labelInsensitivePixmap,
				  XmNlabelType, labelType,
				  NULL);

		    /* rws 27 Apr 1997 label should be freed, no??? */
		    XmStringFree(Label);
		}
		else
		{
		    if (_XmStringIsXmString((XmString)LabG_Label(button)))
		    {
			XmStringFree((XmString)LabG_Label(button));
		    }
		    else
		    {
			_XmStringFree(LabG_Label(button));
		    }

		    if (_XmStringIsXmString(Label))
		    {
			LabG_Label(button) = _XmStringCreate(Label);
			XmStringFree(Label);
		    }
		    else
		    {
			LabG_Label(button) = (_XmString)Label;
			LabG_LabelType(button) = labelType;
			LabG_Pixmap(button) = labelPixmap;
			LabG_PixmapInsensitive(button) = labelInsensitivePixmap;
		    }
		}
	    }
	}
    }
}

/* rws Option menu callback */
static void
_XmOptionCallback(Widget w,
		  XtPointer cd,
		  XtPointer cs)
{
    XmString ButtonLabel;
    Pixmap labelPixmap, labelInsensitivePixmap;
    unsigned char labelType;
    Widget rc = (Widget)cd;

    if (w)
    {
	XtVaGetValues(w, XmNlabelString, &ButtonLabel,
			XmNlabelPixmap, &labelPixmap, 
			XmNlabelInsensitivePixmap, &labelInsensitivePixmap,
			XmNlabelType, &labelType,
			NULL);
	XtVaSetValues(XmOptionButtonGadget(rc), 
			XmNlabelString, ButtonLabel,
			XmNlabelPixmap, labelPixmap, 
			XmNlabelInsensitivePixmap, labelInsensitivePixmap,
			XmNlabelType, labelType,
			NULL);
    }
    else
    {
	ButtonLabel = XmStringCreateSimple("");
	XtVaSetValues(XmOptionButtonGadget(rc), 
			XmNlabelString, ButtonLabel,
			NULL);
    }

    RC_MemWidget(rc) = w;

    XmStringFree(ButtonLabel);
}


static void
RadioHandler(Widget w)
{
    Widget rc = XtParent(w);
    Widget tgl, tgl_unset = NULL;
    Cardinal i;

    Boolean tgl_state = XmIsGadget(w) ? TBG_Set(w) : TB_Set(w);

    DEBUGOUT(_LtDebug2(__FILE__, rc, w, "RadioHandler\n"));

    /*
     * Set the menu history
     */
    if ((w != NULL) && (XtParent(w) != NULL)) {
	XtVaSetValues (XtParent(w), XmNmenuHistory, w, NULL);
    }

    /*
     * If the radio box is supposed to be "always one" then resetting
     * a toggle may not be allowed.
     * Detect this situation first.
     */
    if (RC_RadioAlwaysOne(rc) && tgl_state == False)
    {
	int cnt = 0;

	for (i = 0; i < MGR_NumChildren(rc); i++)
	{
	    tgl = MGR_Children(rc)[i];
	    if (XmIsToggleButton(tgl) && XmToggleButtonGetState(tgl))
	    {
		cnt++;
	    }
	    else if (XmIsToggleButtonGadget(tgl) &&
		     XmToggleButtonGadgetGetState(tgl))
	    {
		cnt++;
	    }
	}
	if (cnt == 0)
	{
	    /* Whow. You're resetting the radio box without written permission.
	     * Stop right there ! */
	    if (XmIsToggleButton(w))
	    {
		DEBUGOUT(_LtDebug2(__FILE__, rc, w,
			"RadioHandler: toggle cnt 0 widget -> True\n"));
		XmToggleButtonSetState(w, True, True);
	    }
	    else if (XmIsToggleButtonGadget(w))
	    {
		DEBUGOUT(_LtDebug2(__FILE__, rc, w,
			"RadioHandler: toggle cnt 0 gadget -> True\n"));
		XmToggleButtonGadgetSetState(w, True, True);
	    }

	    return;
	}
    }

    /*
     * We're ok. Now for the real work.
     * Find which toggles need to change because of this action.
     */
    /*
     * Try to be more correct. Danny 12/6/1998
     *
     * This change also implies changed definitions in ToggleB[G].c
     *
     * We're exhibiting an ambivalent state with the above code.
     * In callback functions called by the XmToggleButtonSetState(_,_,True),
     * while only part of the buttons have been modified, some of the buttons
     * reflect a state that they shouldn't be in.
     */

    /* Turning off a toggle has no further repercussions,
     * unless we are in must-have-one mode, which is dealt with above.
     */
    if( tgl_state == False ) return;

    /* Turning on a toggle, Might need to unset someone else */
    for (i=0; i<MGR_NumChildren(rc); i++) {
	tgl = MGR_Children(rc)[i];
	if (tgl == w)
		continue;
	if (XmIsToggleButton(tgl) && XmToggleButtonGetState(tgl)) {
	    tgl_unset = tgl;
	} else if (XmIsToggleButtonGadget(tgl)
			&& XmToggleButtonGadgetGetState(tgl)) {
	    tgl_unset = tgl;
	}
    }

    DEBUGOUT(_LtDebug(__FILE__, rc, "Widget to unset %s\n",
	tgl_unset ? XtName(tgl_unset) : "(null)"));

    /* Set the data structures */

    if (tgl_unset) {
	if (XmIsToggleButton(tgl_unset)) {
	    DEBUGOUT(_LtDebug2(__FILE__, rc, tgl_unset,
		"Call XmToggleButtonSetState(tgl, False, True)"));
	    XmToggleButtonSetState(tgl_unset, False, True);
	} else if (XmIsToggleButtonGadget(tgl_unset)) {
	    DEBUGOUT(_LtDebug2(__FILE__, rc, tgl_unset,
		"Call XmToggleButtonGadgetSetState(tgl, False, True)"));
	    XmToggleButtonGadgetSetState(tgl_unset, False, True);
	}
    }
}

static Cardinal
_XmRowColumnOrderProc(Widget widget)
{
    if (RCC_PositionIndex(widget) == XmLAST_POSITION ||
	RCC_PositionIndex(widget) >=
	MGR_NumChildren(XtParent(widget)))
    {
	return MGR_NumChildren(XtParent(widget));
    }
    else
    {
	return RCC_PositionIndex(widget);
    }
}

static void
insert_child(Widget w)
{
    Widget rc = (Widget)XtParent(w);
    Cardinal i;

#if 0
    if ((RC_RadioBehavior(rc) || RC_RadioAlwaysOne(rc)) &&
	((XtClass(w) == xmToggleButtonWidgetClass) ||
	 (XtClass(w) == xmToggleButtonGadgetClass)))
    {
	XtAddCallback(w, XmNvalueChangedCallback, _XmRadioCallback, rc);
    }
#endif

#define superclass (&xmManagerClassRec)
    (*superclass->composite_class.insert_child) (w);
#undef superclass

    for (i = 0; i < MGR_NumChildren(rc); i++)
    {
	RCC_PositionIndex(MGR_Children(rc)[i]) = i;
    }

    DEBUGOUT(_LtDebug2(__FILE__, rc, w, "InsertChild\n"));

    /*
     * if it's a label subclass, set its alignment to our
     * RC_EntryAlignment resource
     */
    if (RC_IsAligned(rc) &&
	(XmIsLabel(w) || XmIsLabelGadget(w)))
    {
	Arg arg;

	XtSetArg(arg, XmNalignment, RC_EntryAlignment(rc));
	XtSetValues(w, &arg, 1);
	DEBUGOUT(_LtDebug2(__FILE__, rc, w,
			   "insert_child: Set Alignment to %s\n",
			   _LtDebugAlignment2String(RC_EntryAlignment(rc))));
    }

    if (XtHasCallbacks(rc, XmNentryCallback) == XtCallbackHasSome)
    {
	if (XmIsLabel(w))
	{
	    (LabClass_SetOverrideCallback(XtClass(w)))(w);
	}
	else if (XmIsLabelGadget(w))
	{
	    (LabGClass_SetOverrideCallback(XtClass(w)))(w);
	}
    }

    if (XmIsTearOffButton(w))
    {
	RC_TearOffControl(rc) = w;
	DEBUGOUT(_LtDebug2(__FILE__, rc, w,
			   "InsertChild: this is the TearOff control\n"));
    }

    /* Option Menu : handle inserting children */
    if (RC_CascadeBtn(rc))
    {
	Widget p = XtParent(RC_CascadeBtn(rc));
	if (p && XmIsRowColumn(p) && RC_Type(p) == XmMENU_OPTION)
	{
	    _XmFixOptionMenu(p, True);
	}
    }
}

static void
delete_child(Widget w)
{
    Widget rc = XtParent(w);
    Cardinal i;

    DEBUGOUT(_LtDebug2(__FILE__, rc, w, "DeleteChild\n"));

#define	superclass	(&xmManagerClassRec)
    (*superclass->composite_class.delete_child) (w);
#undef superclass

    for (i = 0; i < MGR_NumChildren(rc); i++)
    {
	RCC_PositionIndex(MGR_Children(rc)[i]) = i;
    }
    if (MGR_ActiveChild(rc) == w)
    {
    	MGR_ActiveChild(rc) = NULL;
    }
}

static void
constraint_initialize(Widget request, Widget new_w,
		      ArgList args, Cardinal *num_args)
{
    Dimension *baselines;
    int num_baselines;

    DEBUGOUT(_LtDebug2(__FILE__, XtParent(new_w), new_w,
		       "%s:constraint_initialize(%d) - %i args\n"
		       "\trequest X %5i Y %5i W %5i H %5i\n"
		       "\t  new_w X %5i Y %5i W %5i H %5i\n"
		       "\t     RC W %5i H %5i\n",
		       __FILE__, __LINE__,
		       *num_args,
		       XtX(request), XtY(request),
		       XtWidth(request), XtHeight(request),
		       XtX(new_w), XtY(new_w),
		       XtWidth(new_w), XtHeight(new_w),
		       XtWidth(XtParent(new_w)), XtHeight(XtParent(new_w))));
    DEBUGOUT(_LtDebugPrintArgList(__FILE__, new_w, args, *num_args, False));

    RCC_MarginTop(new_w) = RCC_MarginBottom(new_w) = 0;
    if (XmIsLabel(new_w))
    {
	if (!XmIsCascadeButton(new_w) || CB_Submenu(new_w))
	{
	    if (RC_Orientation(XtParent(new_w)) == XmHORIZONTAL)
	    {
		RCC_MarginTop(new_w) = Lab_MarginTop(new_w);
		RCC_MarginBottom(new_w) = Lab_MarginBottom(new_w);
	    }
	    else
	    {
		RCC_MarginTop(new_w) = Lab_MarginLeft(new_w);
		RCC_MarginBottom(new_w) = Lab_MarginRight(new_w);
	    }
	}
    }
    else if (XmIsLabelGadget(new_w))
    {
	if (!XmIsCascadeButtonGadget(new_w) || CBG_Submenu(new_w))
	{
	    if (RC_Orientation(XtParent(new_w)) == XmHORIZONTAL)
	    {
		RCC_MarginTop(new_w) = LabG_MarginTop(new_w);
		RCC_MarginBottom(new_w) = LabG_MarginBottom(new_w);
	    }
	    else
	    {
		RCC_MarginTop(new_w) = LabG_MarginLeft(new_w);
		RCC_MarginBottom(new_w) = LabG_MarginRight(new_w);
	    }
	}
    }

    /* we set the baseline of the child to the lowest baseline of the widget */
    if (XmWidgetGetBaselines(new_w, &baselines, &num_baselines))
    {
	RCC_Baseline(new_w) = baselines[num_baselines - 1];
	XtFree((char *)baselines);
    }
    else
    {
	/* is this right ? */
	RCC_Baseline(new_w) = 0;
    }
}


static Boolean
constraint_set_values(Widget current, Widget request, Widget new_w,
		      ArgList args, Cardinal *num_args)
{
    Dimension *baselines;
    int num_baselines;
    Boolean refresh_needed = False;
    Cardinal i;
    Widget rc = XtParent(new_w);

    DEBUGOUT(_LtDebug2(__FILE__, (Widget)rc, new_w,
		       "%s:constraint_set_values(%d) - %i args\n"
		       "\tcurrent X %5i Y %5i W %5i H %5i\n"
		       "\trequest X %5i Y %5i W %5i H %5i\n"
		       "\t  new_w X %5i Y %5i W %5i H %5i\n"
		       "\t     RC W %5i H %5i\n",
		       __FILE__, __LINE__,
		       *num_args,
		       XtX(current), XtY(current),
		       XtWidth(current), XtHeight(current),
		       XtX(request), XtY(request),
		       XtWidth(request), XtHeight(request),
		       XtX(new_w), XtY(new_w),
		       XtWidth(new_w), XtHeight(new_w),
		       XtWidth(XtParent(new_w)), XtHeight(XtParent(new_w))));
    DEBUGOUT(_LtDebugPrintArgList(__FILE__, new_w, args, *num_args, False));
    DEBUGOUT(_LtDebug2("EMACS", (Widget)rc, new_w,
		       "%s:constraint_set_values(%d) - %i args\n"
		       "\tcurrent X %5i Y %5i W %5i H %5i\n"
		       "\trequest X %5i Y %5i W %5i H %5i\n"
		       "\t  new_w X %5i Y %5i W %5i H %5i\n"
		       "\t     RC W %5i H %5i\n",
		       __FILE__, __LINE__,
		       *num_args,
		       XtX(current), XtY(current),
		       XtWidth(current), XtHeight(current),
		       XtX(request), XtY(request),
		       XtWidth(request), XtHeight(request),
		       XtX(new_w), XtY(new_w),
		       XtWidth(new_w), XtHeight(new_w),
		       XtWidth(XtParent(new_w)), XtHeight(XtParent(new_w))));
    DEBUGOUT(_LtDebugPrintArgList("EMACS", new_w, args, *num_args, False));

    if (XmIsLabel(new_w))
    {
	if (!XmIsCascadeButton(new_w) ||
	    (CB_Submenu(new_w) && Lab_MenuType(new_w) != XmMENU_OPTION))
	{
	    if (RC_Orientation(XtParent(new_w)) == XmHORIZONTAL)
	    {
		RCC_MarginTop(new_w) = Lab_MarginTop(new_w);
		RCC_MarginBottom(new_w) = Lab_MarginBottom(new_w);
	    }
	    else
	    {
		RCC_MarginTop(new_w) = Lab_MarginLeft(new_w);
		RCC_MarginBottom(new_w) = Lab_MarginRight(new_w);
	    }
	}
    }
    else if (XmIsLabelGadget(new_w))
    {
	if (!XmIsCascadeButtonGadget(new_w) ||
	    (CBG_Submenu(new_w) && LabG_MenuType(new_w) != XmMENU_OPTION))
	{
	    if (RC_Orientation(XtParent(new_w)) == XmHORIZONTAL)
	    {
		RCC_MarginTop(new_w) = LabG_MarginTop(new_w);
		RCC_MarginBottom(new_w) = LabG_MarginBottom(new_w);
	    }
	    else
	    {
		RCC_MarginTop(new_w) = LabG_MarginLeft(new_w);
		RCC_MarginBottom(new_w) = LabG_MarginRight(new_w);
	    }
	}
    }
    if (RC_OptionSubMenu(rc))
    {
	if (RC_Type(RC_OptionSubMenu(rc)) == XmMENU_OPTION)
	{
	    if(RC_MemWidget(RC_OptionSubMenu(rc)) == new_w)
	    {
		_XmOptionCallback(new_w, (XtPointer)RC_OptionSubMenu(rc), NULL);
	    }
	    _XmRCAdjustSize(RC_OptionSubMenu(rc), NULL, NULL); 
	}
    }
    if (RCC_PositionIndex(new_w) != RCC_PositionIndex(current))
    {
	Widget savew;
	int pos, lastpos;

#if 1
	/* rws 23 Mar 1999
	   If there is a TearOffControl it doesn't seem to count!!  In other
	   words changing an item to position 0 will really change it to 
	   position 1 _iff_ there is a TearOffControl.
	   (rowcolumn/test51)
	 */
	if (RC_TearOffControl(rc))
	{
	    pos = RCC_PositionIndex(new_w) + 1;
	}
	else
	{
	    pos = RCC_PositionIndex(new_w);
	}
#endif
	lastpos = RCC_PositionIndex(current);

	if (RCC_PositionIndex(new_w) == XmLAST_POSITION)
	{
	    RCC_PositionIndex(new_w) = MGR_NumChildren(rc) - 1;
	    pos = RCC_PositionIndex(new_w);
	}

	savew = MGR_Children(rc)[lastpos];

	if (pos < lastpos)
	{
	    for (i = lastpos; i > pos; i--)
	    {
		MGR_Children(rc)[i] = MGR_Children(rc)[i - 1];
	    }

	    MGR_Children(rc)[pos] = savew;
	}
	else if (pos > lastpos)
	{
	    for (i = lastpos; i < pos; i++)
	    {
		MGR_Children(rc)[i] = MGR_Children(rc)[i + 1];
	    }

	    MGR_Children(rc)[pos] = savew;
	}

	for (i = 0; i < MGR_NumChildren(rc); i++)
	{
	    RCC_PositionIndex(MGR_Children(rc)[i]) = i;
	}

	DEBUGOUT(_LtDebug(__FILE__, new_w,
			  "_XmRCAdjustSize from constraint_set_values\n"));
	DEBUGOUT(_LtDebug("EMACS", new_w,
			  "_XmRCAdjustSize from constraint_set_values\n"));
	_XmRCAdjustSize(rc, NULL, NULL);

	refresh_needed = True;
    }

    /* we set the baseline of the child to the lowest baseline of the widget */
    if (XmWidgetGetBaselines(new_w, &baselines, &num_baselines))
    {
	RCC_Baseline(new_w) = baselines[num_baselines - 1];
	XtFree((char *)baselines);
    }
    else
    {
	/* is this right ? */
	RCC_Baseline(new_w) = 0;
    }

    DEBUGOUT(_LtDebug(__FILE__, new_w, "constraint_set_values (end): %d %d\n",
		      RCC_MarginTop(new_w),
		      RCC_MarginBottom(new_w)));

    return refresh_needed;
}

static XmNavigability
widget_navigable(Widget w)
{
    DEBUGOUT(_LtDebug(__FILE__, w, "%s(%d):widget_navigable\n", __FILE__, __LINE__));
    DEBUGOUT(_LtDebug(__FILE__, w, "    Sensitive      %s\n", _LtDebugBoolean2String(XtSensitive(w))));
    DEBUGOUT(_LtDebug(__FILE__, w, "    TraversalOn    %s\n", _LtDebugBoolean2String(MGR_TraversalOn(w))));
    DEBUGOUT(_LtDebug(__FILE__, w, "    RC_Type        %s\n", _LtDebugRcType2String(RC_Type(w))));
    DEBUGOUT(_LtDebug(__FILE__, w, "    DragMode       %s\n", _LtDebugBoolean2String(_XmGetInDragMode(w))));
    /*
    DEBUGOUT(_LtDebug(__FILE__, w, "    RC_PopupPosted %s\n", 
    	RC_PopupPosted(w) ? XtName(RC_PopupPosted(w)) : "NULL" ));
    	*/
    DEBUGOUT(_LtDebug(__FILE__, w, "    NavType        %s\n", _LtDebugNavigationType2String(MGR_NavigationType(w))));
    if (XtSensitive(w) && MGR_TraversalOn(w) &&
	(((RC_Type(w) == XmWORK_AREA || RC_Type(w) == XmMENU_OPTION)) ||
        ((RC_Type(w) == XmMENU_PULLDOWN || RC_Type(w) == XmMENU_POPUP) && !_XmGetInDragMode(w)) ||
        (RC_Type(w) == XmMENU_BAR && !_XmGetInDragMode(w) && RC_PopupPosted(w))))
    {
	if (MGR_NavigationType(w) == XmSTICKY_TAB_GROUP ||
	    MGR_NavigationType(w) == XmEXCLUSIVE_TAB_GROUP ||
	    (MGR_NavigationType(w) == XmTAB_GROUP && !_XmShellIsExclusive(w)))
	{
	    return XmDESCENDANTS_TAB_NAVIGABLE;
	}

	return XmDESCENDANTS_NAVIGABLE;
    }

    return XmNOT_NAVIGABLE;
}

/* action routines */

static void
DoBtnEventCleanupReplay(Widget w, XEvent *event,
			String *params, Cardinal *num_params)
{
    Widget menu_shell;
    Widget tlrc;

    DEBUGOUT(_LtDebug(__FILE__, w, "DoBtnEventCleanupReplay\n"));

    _XmGetActiveTopLevelMenu(w, &tlrc);  /* this is RC_LastSelectToplevel() no?? */

    if (!tlrc)
    {
    /* rws 24 Jun 1998
       filesb/test5 gets us into this mess when you click on the Button.
       It is a cascade button in a menu bar with no menu! So if we get a
       button up and there is no menu we still have to clean up the mess
       that is there.
     */
    Widget tmp = RC_LastSelectToplevel(w);

	if (RC_IsArmed(tmp))
	{
	    MenuProcEntry(XmMENU_DISARM, tmp, NULL);
	    _XmMenuSetInPMMode(w, False);
	    _XmSetInDragMode(w, False);
	    XAllowEvents(XtDisplay(w), ReplayPointer, CurrentTime);
	}
	return;
    }

    menu_shell = XtParent(tlrc);

    if (XmIsMenuShell(menu_shell))
    {
    DEBUGOUT(_LtDebug(__FILE__, w,
		   "DoBtnEventCleanupReplay: calling MenuShellPopdownDone\n"));
    XtCallActionProc(menu_shell, "MenuShellPopdownDone",
		     event, params, *num_params);
    _XmCallRowColumnUnmapCallback(tlrc, event); /* rowcolumn/test45 */
    }
    else if (_XmIsActiveTearOff(tlrc))
    {
    Boolean poppedUp;

	/* should use RC_CascadeBtn(RC_PopupPosted(tlrc)) I think.
	   MGR_ActiveChild is really only for the active Gadget.
	 */
	/*
	 *	T. Straumann: use RC_popupPosted(tlrc) and make sure
	 *				  it exists (fixes segfault if releasing
	 *			      button outside of a torn-off menu)
	 */
	if ( RC_PopupPosted(tlrc) && RC_CascadeBtn(RC_PopupPosted(tlrc)) )
	{
	    RC_MenuShellPopdown(RC_CascadeBtn(RC_PopupPosted(tlrc)), event, &poppedUp);
	}
    }

    _XmSetInDragMode(w, False);

    /* Replay the event */
    DEBUGOUT(_LtDebug(__FILE__, w,
		      "DoBtnEventCleanupReplay: XAllowEvents (replay)\n"));

    /* FIX ME: should depend on resource */
    /* FIX ME: doesn't work */
    XAllowEvents(XtDisplay(w), ReplayPointer, CurrentTime);
}

void
_XmMenuBtnDown(Widget w, XEvent *event, String *params, Cardinal *num_params)
{
Boolean validButton;

    DEBUGOUT(_LtDebug(__FILE__, w, "_XmMenuBtnDown\n"));

    {
    Widget gadget;

	gadget = (Widget)_XmInputForGadget(w,
					   event->xkey.x,
					   event->xkey.y);

	if (gadget && event->xbutton.window == XtWindow(w))
	{
	    /* Event was in a gadget */
	    DEBUGOUT(_LtDebug2(__FILE__, w, gadget, "MenuBtnDown() in gadget\n"));

	    MGR_HighlightedWidget(w) = gadget;
	    _XmDispatchGadgetInput((Widget)gadget, event, XmARM_EVENT);
           _XmMenuFocus(w, XmMENU_FOCUS_SAVE, CurrentTime);
	   /* Don't return from here, see bug #721016.
           return;
	   */
	}
    }
    DEBUGOUT(_LtDebug("MENU", w, "_XmMenuBtnDown - %i %s\n",
    	event->xbutton.button,
    	_XmIsEventUnique(event) ? "unique" : "not-unique"
    	));
    DEBUGOUT(_LtDebug("MENU", w, "_XmMenuBtnDown - %s %s posted\n",
    	_XmGetInDragMode(w) ? "dragging" : "not-dragging",
    	RC_PopupPosted(w) ? XtName(RC_PopupPosted(w)) : "nothing"
    	));
    /*
    RC_MenuButton(w, event, &validButton);
    */
(((XmRowColumnClassRec *)(XtClass(w)))->row_column_class.menuProcedures)(XmMENU_BUTTON, w, event, &validButton);
#if 1
    /* rws 16 Mar 1999
       This sequence is a result of xscope!!!!
     */
    if (validButton)
    {
	_XmGrabKeyboard(w, True,
			GrabModeSync, GrabModeSync,
			CurrentTime);
	if (!_XmGetInDragMode(w) && RC_PopupPosted(w))
	{
	Boolean poppedUp;
	Cardinal i;

	    _XmMenuFocus(w, XmMENU_FOCUS_RESTORE, CurrentTime);
	    for (i = 0; i < MGR_NumChildren(RC_PopupPosted(w)); i++)
	    {
	    Widget w1 = MGR_Children(RC_PopupPosted(w))[i];

		_XmMenuDisarmItem(w1);
	    }
	    if (RC_CascadeBtn(RC_PopupPosted(w)))
	    {
		RC_MenuShellPopdown(RC_CascadeBtn(RC_PopupPosted(w)), event, &poppedUp);
	    }
	    RC_SetArmed(w, False);
	}
	else
	{
	    _XmMenuFocus(w, XmMENU_FOCUS_SAVE, CurrentTime);
	    _XmMenuFocus(w, XmMENU_FOCUS_SET, CurrentTime);
	    XAllowEvents(XtDisplay(w), AsyncKeyboard, CurrentTime);

	    /* Initiate a new pointer grab for the menu bar */
	    /* Pointer mode will change to Sync anyway with the
	       XAllowEvents. It's also set here, according to xscope - Chris 6/23 */
	    _XmGrabPointer(w, True,
			   (ButtonPressMask | ButtonReleaseMask | EnterWindowMask |
			    LeaveWindowMask),
			   GrabModeSync,
			   GrabModeAsync,
			   None,
			   _XmGetMenuCursorByScreen(XtScreen(w)),
			   CurrentTime);

	    /* rws 7 Apr 1999
	       What's up with this????
	     */
	    if (RC_Type(w) != XmMENU_OPTION)
	    {
		_XmAddGrab(w, True, True);
	    }

	    /* Process events until the next button event */
	    RC_SetArmed(w, True);
	    _XmSetInDragMode(w, True);
	}
    }
    else
    {
	DEBUGOUT(_LtDebug0("MENU", w, "_XmMenuBtnDown - %s\n",
	    "\tbutton not valid\n"
	    ));
    }
    XAllowEvents(XtDisplay(w), SyncPointer, CurrentTime);
#endif
}

void
_XmMenuBtnUp(Widget w, XEvent *event, String *params, Cardinal *num_params)
{
Widget gadget;

    gadget = (Widget)_XmInputForGadget(w,
				       event->xkey.x,
				       event->xkey.y);

    if (gadget && event->xbutton.window == XtWindow(w))
    {

	DEBUGOUT(_LtDebug2(__FILE__, w, gadget, "MenuBtnUp(in gadget)\n"));

	_XmDispatchGadgetInput((Widget)gadget, event, XmACTIVATE_EVENT);
	/*
	 * Don't return here - see bug #721016.
	 * return;
	 */
    }
    DEBUGOUT(_LtDebug("MENU", w, "_XmMenuBtnUp - %s %s posted\n",
    	_XmGetInDragMode(w) ? "dragging" : "not-dragging",
    	RC_PopupPosted(w) ? XtName(RC_PopupPosted(w)) : "nothing"
    	));

    XAllowEvents(XtDisplay(w), SyncPointer, CurrentTime);
    if (_XmGetInDragMode(w))
    {
    Boolean poppedUp;

#if 1
	/* rws 16 Mar 1999
	   This sequence is from xscope!!!!
	 */
	_XmUngrabKeyboard(w, CurrentTime);
	_XmUngrabPointer(w, CurrentTime);
	_XmRemoveGrab(w);
#endif
	if (RC_PopupPosted(w))
	{
	Cardinal i;

	    _XmMenuFocus(w, XmMENU_FOCUS_RESTORE, CurrentTime);
	    for (i = 0; i < MGR_NumChildren(RC_PopupPosted(w)); i++)
	    {
	    Widget w1 = MGR_Children(RC_PopupPosted(w))[i];

		    _XmMenuDisarmItem(w1);
	    }
	    if (RC_CascadeBtn(RC_PopupPosted(w)))
	    {
		RC_MenuShellPopdown(RC_CascadeBtn(RC_PopupPosted(w)), event, &poppedUp);
	    }
	}
	RC_SetArmed(w, False);
	_XmSetInDragMode(w, False);
    }
    else
    {
	if (RC_PopupPosted(w))
	{
	}
	else
	{
	    _XmUngrabPointer(w, CurrentTime);
	    _XmUngrabKeyboard(w, CurrentTime);
	    _XmRemoveGrab(w);
	    RC_SetArmed(w, False);
	    /* 25 Mar 1999
	       If a menu button pops up a dialog, this steals focus away from
	       the dialog.  This must be done somewhere else.
	    _XmMenuFocus(w, XmMENU_FOCUS_RESTORE, CurrentTime);
	    */
	}
    }
    if (_XmIsActiveTearOff(w))
    {
	if (RC_TearOffFocusItem(w))
	{
		Lab_MenuArm(RC_TearOffFocusItem(w));
	}
    }
}

/* FIX ME: this should happen from XmMenuShell:_XmEnterRowColumn */
static void
MenuEnter(Widget w, XEvent *event, String *params, Cardinal *num_params)
{
}

static void
MenuUnmap(Widget w, XEvent *event, String *params, Cardinal *num_params)
{
    DEBUGOUT(_LtDebug(__FILE__, w, "MenuUnmap()\n"));

    MGR_HighlightedWidget(w) = NULL;	/* FIX ME: move elsewhere */
}

static void
MenuFocusIn(Widget w, XEvent *event, String *params, Cardinal *num_params)
{
    DEBUGOUT(_LtDebug(__FILE__, w, "MenuFocusIn\n"));
}

static void
MenuFocusOut(Widget w, XEvent *event, String *params, Cardinal *num_params)
{
    DEBUGOUT(_LtDebug(__FILE__, w, "MenuFocusOut\n"));
    /* restore the focus to the widget that had it before moving to the menu bar */
}

static void
MenuBarGadgetSelect(Widget w, XEvent *event,
		    String *params, Cardinal *num_params)
{
    DEBUGOUT(_LtDebug(__FILE__, w, "MenuBarGadgetSelect()\n"));
}

static void
_XmFocusOut(Widget w, XEvent *event,
		    String *params, Cardinal *num_params)
{
    DEBUGOUT(_LtDebug(__FILE__, w, "_XmFocusOut()\n"));
}

static void
_XmFocusIn(Widget w, XEvent *event,
		    String *params, Cardinal *num_params)
{
    DEBUGOUT(_LtDebug(__FILE__, w, "FocusIn()\n"));
}

static void
_XmUnmap(Widget w, XEvent *event,
		    String *params, Cardinal *num_params)
{
    DEBUGOUT(_LtDebug(__FILE__, w, "_XmUnmap()\n"));
}

static void
_XmNoop(Widget w, XEvent *event,
		    String *params, Cardinal *num_params)
{
    DEBUGOUT(_LtDebug(__FILE__, w, "_XmNoop()\n"));
}

/*
 * most of these are already written under different names
 */
void
_XmPostPopupMenu(Widget wid, XEvent *event)
{
    XmMenuState state = _XmGetMenuState(wid);
    int type = event->type;
    Time time;

    DEBUGOUT(_LtDebug(__FILE__, wid, "PostPopupMenu()\n"));
    DEBUGOUT(_LtDebug("MENU", wid, "%s:%s(%d)\n", __FILE__, "_XmPostPopupMenu" , __LINE__));

    time = XtLastTimestampProcessed(XtDisplay(wid));
    if (type == 0)
    {
	type = KeyPress;
	/* rws 2 Apr 1998
	   This makes the popup's in ddd and Mozilla work but, the real
	   problem is the GrabButton is not getting called for some reason.
	   The grab is placed on the RC in AddPopupHandlers but it never
	   gets triggered?????
	 */
	type = ButtonPress;
    }

    state->RC_ButtonEventStatus.waiting_to_be_managed = False;
    state->RC_ButtonEventStatus.time = time;

    if (!XmIsRowColumn(wid) || RC_Type(wid) != XmMENU_POPUP)
    {
	_XmWarning(wid, "_XmPostPopupMenu sent non rowcolumn widget\n");

	return;
    }

    if (type == KeyPress || type == KeyRelease)
    {
	_XmSetInDragMode(wid, False);
    }
    else
    {
	_XmSetInDragMode(wid, True);
    }
    _XmMenuSetInPMMode(wid, True);

    if (event->type == ButtonRelease)
    {
	RCClass_MenuProcs(XtClass(wid))(XmMENU_ARM, wid, NULL);
    }

    _XmMenuFocus(wid, XmMENU_FOCUS_SAVE, CurrentTime);
    /* rws 7 May 1998
       Mozilla's Bookmark, Forward, Back button menus do not post without
       this XtManageChild().
     */
    if (!XtIsManaged(wid))
        XtManageChild(wid);
    else
        XtCallActionProc(XtParent(wid), "XtMenuPopup", event, NULL, 0);

    RC_SetArmed(wid, True);

    RC_CascadeBtn(wid) = NULL;
}

void
_XmSetPopupMenuClick(Widget wid,
		     Boolean popupMenuClick)
{
    RC_PopupMenuClick(wid) = True;
}

Boolean
_XmGetPopupMenuClick(Widget wid)
{
    return RC_PopupMenuClick(wid);
}

void
_XmAllowAcceleratedInsensitiveUnmanagedMenuItems(Widget wid,
						 Boolean allowed)
{
    XmMenuState state = _XmGetMenuState(wid);

    state->RC_allowAcceleratedInsensitiveUnmanagedMenuItems = allowed;
}

void
_XmSetSwallowEventHandler(Widget widget, Boolean add_handler)
{
}

static Boolean
_XmIsWidgetViewable(Widget w)
{
    XWindowAttributes window_attributes;

    if (w == NULL || CoreBeingDestroyed(w))
	return False;

    XGetWindowAttributes(XtDisplay(w), XtWindow(w), &window_attributes);
    return(window_attributes.map_state == IsViewable ? True : False);
}

void
_XmMenuFocus(Widget w, int operation, Time _time)
{
    XmMenuState state = _XmGetMenuState(w);
    Window window_return;
    int revert_to_return;
    static Boolean SavedState = False;

    DEBUGOUT(_LtDebug(__FILE__, w, "%s(%d):_XmMenuFocus() - %s\n",
    	__FILE__, __LINE__,
    	_LtDebugMenuFocusOp2String(operation)));
    DEBUGOUT(_LtDebug("FOCUS", w, "%s(%d):_XmMenuFocus() - %s\n",
    	__FILE__, __LINE__,
    	_LtDebugMenuFocusOp2String(operation)));
    /* nothing but a guess here.  There are three operations that seem
       to take place with respect to focus and menus.

       1 - saving the keyboard focus (done at the start of the menu stuff.)
       2 - setting the keyboard focus (done at several points.)
       3 - restoring the keyboard focus (done at the end of the menu stuff.)

       So, those are the three "operations" that are valid to this function.
     */
    switch (operation)
    {
    case XmMENU_FOCUS_SAVE:
	if (! SavedState)
	{
	    XGetInputFocus(XtDisplay(w),
			   &state->RC_menuFocus.oldFocus,
			   &state->RC_menuFocus.oldRevert);

	    state->RC_menuFocus.oldWidget = XtWindowToWidget(XtDisplay(w),
						     state->RC_menuFocus.oldFocus);
	    SavedState = True;
	    DEBUGOUT(_LtDebug("FOCUS", w, "\tsaving %s\n",
		state->RC_menuFocus.oldWidget ? XtName(state->RC_menuFocus.oldWidget) : "NULL"));
	}
	else
	{
#if 0
	    _XmWarning(w, "%s(%d):_XmMenuFocus() - %s\n    trying to save twice",
		__FILE__, __LINE__, _LtDebugMenuFocusOp2String(operation));
#endif
	}
	break;

    case XmMENU_FOCUS_RESTORE:

	if (SavedState)
	{
	    if (! state->RC_menuFocus.oldWidget)
	    {
#if 0
		_XmWarning(w,"%s(%d) - Restoring focus to NULL widget!\n    Must have missed a focus save somewhere.",
			    __FILE__, __LINE__);
#else
		/* rws 7 Mar 2000
		   Let's try this and see what happens.
		 */
		XSetInputFocus(XtDisplay(w), PointerRoot, RevertToNone, _time);
#endif
	    }
	    else
	    {
		if ((state->RC_menuFocus.oldFocus != PointerRoot && state->RC_menuFocus.oldFocus != None) && ! _XmIsWidgetViewable(state->RC_menuFocus.oldWidget))
		{
		    _XmWarning(XtWindowToWidget(XtDisplay(w), state->RC_menuFocus.oldFocus), "%s(%d):_XmMenuFocus() - %s\n    window is not viewable",
			__FILE__, __LINE__, _LtDebugMenuFocusOp2String(operation));
		}
		else
		{
		    DEBUGOUT(_LtDebug(__FILE__, w, "  Restore focus to %s\n",
			XtName(state->RC_menuFocus.oldWidget)));
		    DEBUGOUT(_LtDebug("FOCUS", w, "  Restore focus to %s\n",
			XtName(state->RC_menuFocus.oldWidget)));
		    if (CoreBeingDestroyed(state->RC_menuFocus.oldWidget))
		    {
		       _XmWarning(w,"%s(%d) - Restoring focus to %s which is being destroyed!\n    Using None instead.",
				__FILE__, __LINE__,
				XtName(state->RC_menuFocus.oldWidget));
		       state->RC_menuFocus.oldFocus = None ;
		    }
		    XSetInputFocus(XtDisplay(w),
				   state->RC_menuFocus.oldFocus,
				   state->RC_menuFocus.oldRevert,
				   _time);

		    XmProcessTraversal(state->RC_menuFocus.oldWidget, XmTRAVERSE_CURRENT);

		    XGetInputFocus(XtDisplay(w),
				   &window_return,
				   &revert_to_return);

		    if (state->RC_menuFocus.oldFocus != window_return
			|| state->RC_menuFocus.oldRevert != revert_to_return)
		    {
			DEBUGOUT(_LtDebug(__FILE__, w, "  SetInputFocus call failed.\n"));
			state->RC_menuFocus.oldFocus = window_return;
			state->RC_menuFocus.oldRevert = revert_to_return;
			state->RC_menuFocus.oldWidget = XtWindowToWidget(XtDisplay(w),
						     state->RC_menuFocus.oldFocus);
		    }
		    else
		    {
			state->RC_menuFocus.oldWidget = NULL;
			state->RC_menuFocus.oldFocus = None;
			state->RC_menuFocus.oldRevert = RevertToPointerRoot;
		    }
		}
	    }
	    SavedState = False;
	}
	else
	{
#ifdef	LESSTIF_VERBOSE
	    _XmWarning(w, "%s(%d):_XmMenuFocus() - %s\n    trying to restore without a save",
		__FILE__, __LINE__, _LtDebugMenuFocusOp2String(operation));
#endif
	}
	break;

    case XmMENU_FOCUS_SET:
	if (! SavedState)
	{
#ifdef	LESSTIF_VERBOSE
	    _XmWarning(w, "%s(%d):_XmMenuFocus() - %s\n    trying to set without a save, I'll save for you",
		__FILE__, __LINE__, _LtDebugMenuFocusOp2String(operation));
#endif
	    _XmMenuFocus(w, XmMENU_FOCUS_SAVE, CurrentTime);
	}
	if (! _XmIsWidgetViewable(w))
	{
	    /*
	    _XmWarning(w, "%s(%d):_XmMenuFocus() - %s\n    window is not viewable",
		__FILE__, __LINE__, _LtDebugMenuFocusOp2String(operation));
		*/
	}
	else
	{
	    XSetInputFocus(XtDisplay(w),
			   XtWindow(w),
			   RevertToPointerRoot,
			   _time);

	    XtSetKeyboardFocus(w, w);

	    XGetInputFocus(XtDisplay(w),
			   &window_return,
			   &revert_to_return);

	    if (window_return != XtWindow(w) ||
		revert_to_return != RevertToPointerRoot)
	    {
		DEBUGOUT(_LtDebug(__FILE__, w, "  setting input focus failed\n"));
		_XmUngrabKeyboard(w, _time);
		return;
	    }
	}
	break;
    }
}

void
_XmGetActiveTopLevelMenu(Widget wid, Widget *rwid)
{
    Widget toplevelrc;

    DEBUGOUT(_LtDebug(__FILE__, wid, "_XmGetActiveTopLevelMenu()\n"));
    /* find the topmost menu pane */
    if ((toplevelrc = RC_LastSelectToplevel(wid)))
    {
	/* Pulldown and option menus keep track over their toplevel
	 * RowColumn...
	 */
	Widget top_pane;

	DEBUGOUT(_LtDebug(__FILE__, wid, "_XmGetActiveTopLevelMenu() - RC_LastSelectTopLevel %s\n",
		XtName(toplevelrc)));
	if ((top_pane = RC_PopupPosted(toplevelrc)))
	{
	    toplevelrc = top_pane;
	    DEBUGOUT(_LtDebug(__FILE__, wid, "_XmGetActiveTopLevelMenu() - RC_PopupPosted %s\n",
		XtName(toplevelrc)));
	}
	else
	{
	    if (_XmIsActiveTearOff(wid))
	    {
	    	toplevelrc = wid;
	    }
	    else
	    {
	    }
	}
    }
    else
    {
	/* ... popup menus don't . */
	Widget cb;

	toplevelrc = wid;
	cb = RC_CascadeBtn(toplevelrc);

	while (cb)
	{
	    toplevelrc = XtParent(cb);
	    cb = RC_CascadeBtn(toplevelrc);
	}
    }

    DEBUGOUT(_LtDebug(__FILE__, wid, "_XmGetActiveTopLevelMenu() - returning %s\n",
		toplevelrc ? XtName(toplevelrc) : "NULL" ));
    *rwid = toplevelrc;
}

Boolean
_XmMatchBSelectEvent(Widget wid, XEvent *event)
{
    return False;
}

Boolean
_XmMatchBDragEvent(Widget wid, XEvent *event)
{
    return False;
}

void
_XmHandleMenuButtonPress(Widget wid, XEvent *event)
{
}

void
_XmCallRowColumnMapCallback(Widget wid, XEvent *event)
{
    XmRowColumnCallbackStruct cbs;

    cbs.reason = XmCR_MAP;
    cbs.event = event;
    cbs.widget = NULL;
    cbs.data = NULL;
    cbs.callbackstruct = NULL;

    XtCallCallbackList(wid, RC_Map_cb(wid), (XtPointer)&cbs);
}

void
_XmCallRowColumnUnmapCallback(Widget wid, XEvent *event)
{
    XmRowColumnCallbackStruct cbs;

    cbs.reason = XmCR_UNMAP;
    cbs.event = event;
    cbs.widget = NULL;
    cbs.data = NULL;
    cbs.callbackstruct = NULL;

    XtCallCallbackList(wid, RC_Unmap_cb(wid), (XtPointer)&cbs);
}

void
_XmMenuPopDown(Widget w, XEvent *event, Boolean *popped_up)
{
    _XmWarning(w, "%s(%d) - _XmMenuPopDown() not implemented ", __FILE__, __LINE__);
}

Boolean
_XmIsActiveTearOff(Widget w)
{
    return RC_TearOffActive(w);
}

void
_XmMenuHelp(Widget w, XEvent *event, String *params, Cardinal *num_params)
{
    XtCallCallbacks(w, XmNhelpCallback, NULL);
}

static void
ParsePostString(Widget widget, String menuPost)
{
    unsigned int button = 0;
    int type = 0;
    unsigned int modifiers = 0;
    Widget realpar = NULL;

    DEBUGOUT(_LtDebug(__FILE__, widget,
		      "############### In _XmToMenuPost(%s)\n", menuPost));

    if (XtIsShell(XtParent(widget)))
    {
        realpar = XtParent(XtParent(widget));
    }
    else
    {
        realpar = XtParent(widget);
    }

    if (realpar != NULL && RC_PostButton(widget) != XmUNSPECIFIED)
    {
        XtUngrabButton(realpar,
                       RC_PostButton(widget), RC_PostModifiers(widget));
#if 0
	XtUngrabKeyboard(realpar, CurrentTime); /* XXX 761607 */
#endif
    }

    if (menuPost == NULL)
    {
	DEBUGOUT(_LtDebug(__FILE__, widget,
			  "  Default case -- menupost == null\n"));
        return;
    }

    if (_XmMapBtnEvent(menuPost, &type, &button, &modifiers))
    {
	/* does this one ever change? */
	RC_PostEventType(widget) = type;
	RC_PostButton(widget) = button;
	RC_PostModifiers(widget) = modifiers;

	if (RC_Type(widget) == XmMENU_POPUP && type == ButtonRelease)
	{
	    _XmSetPopupMenuClick(widget, True);
	}
	else
	{
	    _XmSetPopupMenuClick(widget, False);
	}

        if (realpar)
        {
            XtGrabButton(realpar,
                         RC_PostButton(widget), RC_PostModifiers(widget),
		         True, ButtonReleaseMask, GrabModeSync, GrabModeSync,
		         XtWindow(realpar),
                         _XmGetMenuCursorByScreen(XtScreen(widget)));
        }
    }
}

/* synthetic resource converters */
static XmImportOperator
_XmToMenuPost(Widget widget, int offset, XtArgVal *value)
{
    String menuPost = (String)*value;	/* the string we're checking/parsing. */

    if (menuPost)
    {
        /* Fixme: this could be a leak */
        RC_MenuPost(widget) = XtNewString(menuPost);
    }

    ParsePostString(widget, RC_MenuPost(widget));

    return XmSYNTHETIC_NONE;
}

static void
_XmPopupButtonPressHandler(Widget w, XtPointer client_data,
			   XEvent *event, Boolean *cont)
{
    Widget rc = (Widget)client_data;
    XmMenuState state = _XmGetMenuState(w);

    DEBUGOUT(_LtDebug(__FILE__, w,
		      "BUTTON PRESS %p %s %p %s\n",
		      w, XtName(w), rc, XtName(rc)));
#if 0
    printf("BUTTON PRESS %p %s %p %s\n",
		      w, XtName(w), rc, XtName(rc));
#endif
    /* first check if the event is valid wrt our menupost resource */
    if (event->xany.type != RC_PostEventType(rc) ||
	event->xbutton.button != RC_PostButton(rc) ||
        event->xbutton.state != RC_PostModifiers(rc))
    {
	DEBUGOUT(_LtDebug(__FILE__, w,
			  "Not post button. %d %d Button %d PF %d\n",
			  event->xany.type, ButtonPress,
			  event->xbutton.button, RC_PostButton(rc)));
	XtUngrabPointer(w,CurrentTime);
	XtUngrabKeyboard(w,CurrentTime);
	return;
    }

    /* if we're already waiting to be managed, do nothing. */
    if (state->RC_ButtonEventStatus.waiting_to_be_managed)
    {
	DEBUGOUT(_LtDebug(__FILE__, w, "Waiting to be managed\n"));
	XtUngrabPointer(w,CurrentTime);
	XtUngrabKeyboard(w,CurrentTime);
	return;
    }

    DEBUGOUT(_LtDebug(__FILE__, rc, "  Filling in ButtonEventStatusRec\n"));
    DEBUGOUT(_LtDebug(__FILE__, rc,
		      "  Event: win %08x subwin: %08x time: %d ser: %d\n",
		      event->xbutton.window, event->xbutton.subwindow,
		      event->xbutton.time, event->xbutton.serial));
    DEBUGOUT(_LtDebug(__FILE__, rc,
		      "         x_root %d y_root: %d x: %d y: %d "
		      "state: %d button: %d\n",
		      event->xbutton.x_root, event->xbutton.y_root,
		      event->xbutton.x, event->xbutton.y,
		      event->xbutton.state, event->xbutton.button));

    state->RC_ButtonEventStatus.waiting_to_be_managed = True;
    state->RC_ButtonEventStatus.event = event->xbutton;
    state->RC_ButtonEventStatus.time = event->xbutton.time;
    state->RC_ButtonEventStatus.verified = True; /* Is it really? FIX ME */
	XtUngrabPointer(w,CurrentTime);
	XtUngrabKeyboard(w,CurrentTime);

#if XmVERSION > 1
    /* 10/16/98 pgw@hungry.com
       FIX ME: This is a quick hack to get things working.
     */
    if (RC_PopupEnabled(rc) == XmPOPUP_AUTOMATIC
        || RC_PopupEnabled(rc) == XmPOPUP_AUTOMATIC_RECURSIVE)
    {
        XButtonEvent *be = (XButtonEvent*)event;

        /* does it match what XmNwhichButton is set to */
        if(be->button == RC_PostButton(rc))
        {
            XmMenuPosition(rc, be);
            XtManageChild(rc);
        }
    }
#endif
}

static void
_XmFromMenuPost(Widget widget,
		int offset,
		XtArgVal *value)
{
    DEBUGOUT(_LtDebug(__FILE__, widget, "In _XmFromMenuPost()\n"));

    if(RC_MenuPost(widget))
        *value = (XtArgVal) XtNewString(RC_MenuPost(widget));
    else
        *value = (XtArgVal) NULL;
}

static void
_XmRowColumnEntryClassDefault(Widget w,
			      int offset,
			      XrmValue *val)
{
    static WidgetClass wclass;
/*    XmRowColumnWidget rc = (XmRowColumnWidget)w;

    if (RC_RadioBehavior(rc) && RC_Type(rc) == XmWORK_AREA)
	wclass = xmToggleButtonGadgetClass;
    else if (RC_Type(rc) == XmMENU_BAR)
	wclass = xmCascadeButtonClass;
    else
 *//* hmmm.... */ ;

    val->addr = (XPointer)&wclass;
}

#if 0
static void
_XmRowColumnIsHomogeneousDefault(Widget w,
				 int offset,
				 XrmValue *val)
{
    static Boolean homo;

    if ((RC_RadioBehavior(w) && RC_Type(w) == XmWORK_AREA) ||
	RC_Type(w) == XmMENU_BAR)
    {
	homo = True;
    }
    else
    {
	homo = False;
    }

    val->addr = (XtPointer)&homo;
}
#endif

#if 0
static void
_XmRowColumnMenuAcceleratorDefault(Widget w,
				   int offset,
				   XrmValue *val)
{
    static String foo;

    if (!foo)
    {
	foo = XtNewString("foo");
    }

    val->addr = (XtPointer)&foo;
}
#endif
