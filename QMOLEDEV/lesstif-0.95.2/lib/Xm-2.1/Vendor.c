/**
 *
 * $Header: /cvsroot/lesstif/lesstif/lib/Xm-2.1/Vendor.c,v 1.5 2005/03/19 15:15:27 dannybackx Exp $
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

static const char rcsid[] = "$Id: Vendor.c,v 1.5 2005/03/19 15:15:27 dannybackx Exp $";

#include <LTconfig.h>

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/* amai, 5/2001: (This is not very nice ...)
   Later on we use some internal stuff from the Intrinsics which we 
   should _not_ use. In rare conditions we may lack some stuff from the
   headers if this setting is not done (-> e.g. _XtBoolean).
   I hope that this does not hurt anywhere. */ 
#ifndef FUNCPROTO
#define FUNCPROTO 11
#endif

#include <X11/IntrinsicP.h>
#include <X11/StringDefs.h>
#include <X11/Shell.h>
#include <X11/ShellP.h>
#include <X11/Vendor.h>
#include <X11/VendorP.h>

#include <XmI/XmI.h>
#include <XmI/AtomMgrI.h>

#include <Xm/XmP.h>
#include <Xm/AtomMgr.h>
#include <Xm/BulletinBP.h>
#include <Xm/ShellEP.h>
#include <Xm/MenuShellP.h>
#include <Xm/VendorSEP.h>
#include <Xm/VendorSP.h>
#include <Xm/DisplayP.h>
#include <Xm/ScreenP.h>
#include <Xm/BaseClassP.h>
#include <Xm/Protocols.h>
#include <Xm/ProtocolsP.h>
#include <Xm/DialogSEP.h>
#include <Xm/ExtObjectP.h>
#include <Xm/VendorS.h>

#include <Xm/SpecRenderT.h>

#include <Xm/TraitP.h>

#include <XmI/DebugUtil.h>

#if defined(__EMX__) || defined(__CYGWIN__)
extern void _LtXmFixupVendorShell(void);
#ifdef __EMX__
unsigned long _DLL_InitTerm(unsigned long mod_handle, unsigned long flag);
#endif
#ifdef __CYGWIN__
int __stdcall DllMain(unsigned long mod_handle, unsigned long flag, void *routine);
#endif
#endif

/* We use a 'private', i.e. non-declared, but actually exported call
    from the original Intrinsic sources (lib/Xt/Callback.c) here.
    Note that we don't give the precise prototype:
 */
extern void 
_XtRemoveCallback (
    /* InternalCallbackList   *callbacks, */
    void               *callbacks,
    XtCallbackProc	callback,
    XtPointer		closure);

/***************************************************************************
 *
 * Vendor shell class record
 *
 ***************************************************************************/

static void class_initialize(void);
static void class_part_initialize(WidgetClass w_class);
static void initialize(Widget req, Widget new_w, ArgList args, Cardinal *num_args);
static Boolean set_values(Widget old, Widget req, Widget new_w, ArgList args, Cardinal *num_args);
static void destroy(Widget w);
static void realize(Widget w, XtValueMask *value_mask, XSetWindowAttributes *attributes);
static void change_managed(Widget w);
static void resize(Widget w);
static void get_values_prehook(Widget widget, ArgList args, Cardinal *num_args);
static void get_values_posthook(Widget widget, ArgList args, Cardinal *num_args);
static void initialize_prehook(Widget req, Widget new_w, ArgList args, Cardinal *num_args);
static void initialize_posthook(Widget req, Widget new_w, ArgList args, Cardinal *num_args);
static Boolean set_values_prehook(Widget old, Widget req, Widget new_w,
				  ArgList args, Cardinal *num_args);
static Boolean set_values_posthook(Widget old, Widget req, Widget new_w,
				   ArgList args, Cardinal *num_args);
static void insert_child(Widget w);
static void delete_child(Widget w);
static XtGeometryResult geometry_manager(Widget wid,
					 XtWidgetGeometry *request,
					 XtWidgetGeometry *reply);
static Cardinal get_sec_res_data(WidgetClass wc, XmSecondaryResourceData **data);
static void secondary_object_create(Widget req, Widget new_w, ArgList args, Cardinal *num_args);
static void WmProtocolHandler(Widget w, XtPointer client, XtPointer call);

/*
 * "forwards" needed within the vendor class implementation
 */
static void LTAddGrab(Widget wid, Boolean exclusive, Boolean spring_loaded,
		      XmVendorShellExtObject ve,
		      XmVendorShellExtObject grabber);
static void LTRemoveGrab(Widget wid, XmVendorShellExtObject ve, Boolean remove_grab_physically);
static void LTRemoveGrabCallback(Widget wid, XtPointer client_data, XtPointer callback_data);
static void LTShellPopupCallback(Widget w, XtPointer ClientData, XtPointer CallbackData);
static void LTShellPopdownCallback(Widget w, XtPointer ClientData, XtPointer CallbackData);
static void LTGrabRelatives(XmVendorShellExtObject grabber);
static void VendorFromHorizontalPixels(Widget w, int offset, XtArgVal *value);
static void VendorFromVerticalPixels(Widget w, int offset, XtArgVal *value);
static XmImportOperator VendorToHorizontalPixels(Widget w, int offset, XtArgVal *value);
static XmImportOperator VendorToVerticalPixels(Widget w, int offset, XtArgVal *value);
static Bool ConfigEventForMe(Display *dpy, XEvent *event, char *arg);
static XmRenderTable GetRenderTable(Widget w, XtEnum renderTableType);

/*
 * the following is for the extension record for LessTif Vendor Shells
 * this is needed before the shell record as the shell's base class extension
 * needs some of the data defined here.
 */
static void _XmVendorExtInitialize(Widget req, Widget new_w,
				   ArgList args, Cardinal *num_args);
static Boolean _XmVendorExtSetValues(Widget cw, Widget rw, Widget nw,
				     ArgList args, Cardinal *nargs);
static void _XmVendorExtDestroy(Widget w);

#define Offset(field) XtOffsetOf(XmVendorShellExtRec, vendor.field)
static XtResource ext_resources[] =
{
    {
	XmNextensionType, XmCExtensionType, XmRExtensionType,
     sizeof(unsigned char), XtOffsetOf(XmVendorShellExtRec, ext.extensionType),
	XtRImmediate, (XtPointer)XmSHELL_EXTENSION
    },
    {
	XmNdefaultFontList, XmCDefaultFontList, XmRFontList,
	sizeof(XmFontList), Offset(default_font_list),
	XmRImmediate, (XtPointer)NULL
    },
    {
	XmNbuttonFontList, XmCButtonFontList, XmRFontList,
	sizeof(XmFontList), Offset(button_font_list),
	XmRImmediate, (XtPointer)NULL
    },
    {
	XmNlabelFontList, XmCLabelFontList, XmRFontList,
	sizeof(XmFontList), Offset(label_font_list),
	XmRImmediate, (XtPointer)NULL
    },
    {
	XmNtextFontList, XmCTextFontList, XmRFontList,
	sizeof(XmFontList), Offset(text_font_list),
	XmRImmediate, NULL
    },
    {
	XmNaudibleWarning, XmCAudibleWarning, XmRAudibleWarning,
	sizeof(unsigned char), Offset(audible_warning),
	XmRImmediate, (XtPointer)XmBELL
    },
    {
	XmNshellUnitType, XmCShellUnitType, XmRUnitType,
	sizeof(unsigned char), Offset(unit_type),
	XmRImmediate, (XtPointer)XmPIXELS	/* CHECK THIS */
    },
    {
	XmNdeleteResponse, XmCDeleteResponse, XmRDeleteResponse,
	sizeof(unsigned char), Offset(delete_response),
	XmRImmediate, (XtPointer)XmDESTROY
    },
    {
	XmNkeyboardFocusPolicy, XmCKeyboardFocusPolicy, XmRKeyboardFocusPolicy,
	sizeof(unsigned char), Offset(focus_policy),
	XmRImmediate, (XtPointer)XmEXPLICIT
    },
    {
	XmNmwmDecorations, XmCMwmDecorations, XmRInt,
	sizeof(int), Offset(mwm_hints.decorations),
	XmRImmediate, (XtPointer)-1
    },
    {
	XmNmwmFunctions, XmCMwmFunctions, XmRInt,
	sizeof(int), Offset(mwm_hints.functions),
	XmRImmediate, (XtPointer)-1
    },
    {
	XmNmwmInputMode, XmCMwmInputMode, XmRInt,
	sizeof(int), Offset(mwm_hints.input_mode),
	XmRImmediate, (XtPointer)-1
    },
    {
	XmNmwmMenu, XmCMwmMenu, XmRString,
	sizeof(String), Offset(mwm_menu),
	XmRImmediate, NULL
    },
    {
	XmNfocusMovedCallback, XmCCallback, XmRCallback,
	sizeof(XtCallbackList), Offset(focus_moved_callback),
	XmRImmediate, NULL
    },
    {
	XmNrealizeCallback, XmCCallback, XmRCallback,
	sizeof(XtCallbackList), Offset(realize_callback),
	XmRImmediate, NULL
    },
    {
	XmNinputMethod, XmCInputMethod, XmRString,
	sizeof(String), Offset(input_method_string),
	XtRImmediate, (XtPointer)NULL	/* FIX ME */
    },
    {
	XmNpreeditType, XmCPreeditType, XmRString,
	sizeof(String), Offset(preedit_type_string),
	XtRImmediate, (XtPointer)"OffTheSpot,OverTheSpot,Root"
    },
    {
	XmNlightThreshold, XmCLightThreshold, XmRInt,
	sizeof(int), Offset(light_threshold),
	XtRImmediate, (XtPointer)0
    },
    {
	XmNdarkThreshold, XmCDarkThreshold, XmRInt,
	sizeof(int), Offset(dark_threshold),
	XtRImmediate, (XtPointer)0
    },
    {
	XmNforegroundThreshold, XmCForegroundThreshold, XmRInt,
	sizeof(int), Offset(foreground_threshold),
	XtRImmediate, (XtPointer)0
    },
    /* FIX ME which version does this originate at ? */
    {
	XmNlayoutDirection, XmCLayoutDirection, XmRDirection,
	sizeof(XmDirection), Offset(layout_direction),
	XtRImmediate, (XtPointer)XmLEFT_TO_RIGHT
    },
    {
	XmNinputPolicy, XmCInputPolicy, XmRInputPolicy,
	sizeof(XmInputPolicy), Offset(input_policy),
	XtRImmediate, (XtPointer)XmPER_SHELL
    },
};


static XmSyntheticResource ext_syn_resources[] =
{
    {
	XmNx,
	sizeof(Position), XtOffsetOf(VendorShellRec, core.x),
	VendorFromHorizontalPixels, VendorToHorizontalPixels
    },
    {
	XmNy,
	sizeof(Position), XtOffsetOf(VendorShellRec, core.y),
	VendorFromVerticalPixels, VendorToVerticalPixels
    },
    {
	XmNwidth,
	sizeof(Dimension), XtOffsetOf(VendorShellRec, core.width),
	VendorFromHorizontalPixels, VendorToHorizontalPixels
    },
    {
	XmNheight,
	sizeof(Dimension), XtOffsetOf(VendorShellRec, core.height),
	VendorFromVerticalPixels, VendorToVerticalPixels
    },
    {
	XmNborderWidth,
	sizeof(Dimension), XtOffsetOf(VendorShellRec, core.border_width),
	VendorFromHorizontalPixels, VendorToHorizontalPixels
    },
    {
	XmNminWidth,
	sizeof(int), XtOffsetOf(VendorShellRec, wm.size_hints.min_width),
	VendorFromHorizontalPixels , VendorToHorizontalPixels
    },
    {
	XmNminHeight,
	sizeof(int), XtOffsetOf(VendorShellRec, wm.size_hints.min_height),
	VendorFromVerticalPixels , VendorToVerticalPixels
    },
    {
	XmNmaxWidth,
	sizeof(int), XtOffsetOf(VendorShellRec, wm.size_hints.max_width),
	VendorFromHorizontalPixels , VendorToHorizontalPixels
    },
    {
	XmNmaxHeight,
	sizeof(int), XtOffsetOf(VendorShellRec, wm.size_hints.max_height),
	VendorFromVerticalPixels , VendorToVerticalPixels
    },
    {
	XmNiconX,
	sizeof(Position), XtOffsetOf(VendorShellRec, wm.wm_hints.icon_x),
	VendorFromHorizontalPixels , VendorToHorizontalPixels
    },
    {
	XmNiconY,
	sizeof(Position), XtOffsetOf(VendorShellRec, wm.wm_hints.icon_y),
	VendorFromVerticalPixels , VendorToVerticalPixels
    },
#if 0
    {
	XmNmwmFunctions,
	sizeof(), XtOffsetOf(),
	NULL /* FIX ME */ , NULL
    },
#endif
};


XmVendorShellExtClassRec xmVendorShellExtClassRec = {
    /* Object Class Part */
    {
 	/* superclass         */    (WidgetClass)&xmShellExtClassRec,
	/* class_name         */    "VendorShell",
	/* size               */    sizeof(XmVendorShellExtRec),
	/* class_initialize   */    NULL,
	/* class_part_initialize*/  NULL,
	/* Class init'ed ?    */    False,
	/* initialize         */    NULL,
	/* initialize_hook    */    NULL,
	/* pad                */    NULL,
	/* pad                */    NULL,
	/* pad                */    0,
	/* resources          */    ext_resources,
	/* resource_count     */    XtNumber(ext_resources),
	/* xrm_class          */    NULLQUARK,
	/* pad                */    False,
	/* pad                */    False,
	/* pad                */    False,
	/* pad                */    False,
	/* destroy            */    _XmVendorExtDestroy,
	/* pad                */    NULL,
	/* pad                */    NULL,
	/* set_values         */    NULL,
	/* set_values_hook    */    NULL,
	/* pad                */    NULL,
	/* get_values_hook    */    NULL,
	/* pad                */    NULL,
	/* version            */    XtVersion,
	/* callback_offsets   */    NULL,
	/* pad                */    NULL,
	/* pad                */    NULL,
	/* pad                */    NULL,
	/* extension          */    NULL
    },
    /* XmExtObject part */
    {
        /* syn_resources      */ ext_syn_resources,
        /* num_syn_resources  */ XtNumber(ext_syn_resources),
        /* extension          */ NULL
    },
    /* Desktop Class part */
    {
        /* child_class           */ NULL,
        /* insert_child          */ XmInheritWidgetProc,
        /* delete_child          */ XmInheritWidgetProc,
        /* extension             */ NULL
    },
    /* ShellExt Class part */
    {
	/* structure_notify_handler */ XmInheritEventHandler,
	/* extension                */ NULL
    },
    /* VendorClass Part */
    {
	/* delete_window_handler */ WmProtocolHandler,
	/* offset_handler        */ NULL, /* FIX ME */
	/* extension             */ NULL
    }
};

static XmSpecRenderTraitRec _XmVendorShellTraitRec = {
	/* version      */      0,
	/* cb           */      GetRenderTable
};

WidgetClass xmVendorShellExtObjectClass = (WidgetClass) &xmVendorShellExtClassRec;

static int default_shell_int = XtUnspecifiedShellInt;

static XtResource vendor_resources[] = {
    {
	XmNx, XmCPosition, XmRShellHorizPos,
	sizeof(Position), XtOffsetOf(VendorShellRec, core.x),
	XmRImmediate, (XtPointer)0
    },
    {
	XmNy, XmCPosition, XmRShellVertPos,
	sizeof(Position), XtOffsetOf(VendorShellRec, core.y),
	XmRImmediate, (XtPointer)0
    },
    {
	XmNwidth, XmCDimension, XmRShellHorizDim,
	sizeof(Dimension), XtOffsetOf(VendorShellRec, core.width),
	XmRImmediate, (XtPointer)0
    },
    {
	XmNheight, XmCDimension, XmRShellVertDim,
	sizeof(Dimension), XtOffsetOf(VendorShellRec, core.height),
	XmRImmediate, (XtPointer)0
    },
    {
	XmNborderWidth, XmCBorderWidth, XmRShellHorizDim,
	sizeof(Dimension), XtOffsetOf(VendorShellRec, core.border_width),
	XmRImmediate, (XtPointer)0
    },
    {
	XmNbaseWidth, XmCBaseWidth, XmRHorizontalInt,
	sizeof(int), XtOffsetOf(VendorShellRec, wm.base_width),
        XmRHorizontalInt, (XtPointer)&default_shell_int
    },
    {
	XmNbaseHeight, XmCBaseHeight, XmRVerticalInt,
	sizeof(int), XtOffsetOf(VendorShellRec, wm.base_height),
        XmRVerticalInt, (XtPointer)&default_shell_int
    },
    {
	XmNminWidth, XmCMinWidth, XmRHorizontalInt,
	sizeof(int), XtOffsetOf(VendorShellRec, wm.size_hints.min_width),
        XmRHorizontalInt, (XtPointer)&default_shell_int
    },
    {
	XmNminHeight, XmCMinHeight, XmRVerticalInt,
	sizeof(int), XtOffsetOf(VendorShellRec, wm.size_hints.min_height),
        XmRVerticalInt, (XtPointer)&default_shell_int
    },
    {
	XmNmaxWidth, XmCMaxWidth, XmRHorizontalInt,
	sizeof(int), XtOffsetOf(VendorShellRec, wm.size_hints.max_width),
        XmRHorizontalInt, (XtPointer)&default_shell_int
    },
    {
	XmNmaxHeight, XmCMaxHeight, XmRVerticalInt,
	sizeof(int), XtOffsetOf(VendorShellRec, wm.size_hints.max_height),
        XmRVerticalInt, (XtPointer)&default_shell_int
    },
    {
	XmNwidthInc, XmCWidthInc, XmRHorizontalInt,
	sizeof(int), XtOffsetOf(VendorShellRec, wm.size_hints.width_inc),
        XmRHorizontalInt, (XtPointer)&default_shell_int
    },
    {
	XmNheightInc, XmCHeightInc, XmRVerticalInt,
	sizeof(int), XtOffsetOf(VendorShellRec, wm.size_hints.height_inc),
        XmRVerticalInt, (XtPointer)&default_shell_int
    },
    {
	XmNminAspectX, XmCMinAspectX, XmRHorizontalInt,
	sizeof(int), XtOffsetOf(VendorShellRec, wm.size_hints.min_aspect.x),
        XmRHorizontalInt, (XtPointer)&default_shell_int
    },
    {
	XmNminAspectY, XmCMinAspectY, XmRVerticalInt,
	sizeof(int), XtOffsetOf(VendorShellRec, wm.size_hints.min_aspect.y),
        XmRVerticalInt, (XtPointer)&default_shell_int
    },
    {
	XmNmaxAspectX, XmCMaxAspectX, XmRHorizontalInt,
	sizeof(int), XtOffsetOf(VendorShellRec, wm.size_hints.max_aspect.x),
        XmRHorizontalInt, (XtPointer)&default_shell_int
    },
    {
	XmNmaxAspectY, XmCMaxAspectY, XmRVerticalInt,
	sizeof(int), XtOffsetOf(VendorShellRec, wm.size_hints.max_aspect.y),
        XmRVerticalInt, (XtPointer)&default_shell_int
    },
    {
	XmNiconPixmap, XmCIconPixmap, XmRPixmap,
	sizeof(Pixmap), XtOffsetOf(VendorShellRec, wm.wm_hints.icon_pixmap),
        XmRPixmap, (XtPointer)NULL
    },
    {
	XmNiconX, XmCIconX, XmRHorizontalInt,
	sizeof(int), XtOffsetOf(VendorShellRec, wm.wm_hints.icon_x),
        XmRHorizontalInt, (XtPointer)&default_shell_int
    },
    {
	XmNiconY, XmCIconY, XmRVerticalInt,
	sizeof(int), XtOffsetOf(VendorShellRec, wm.wm_hints.icon_y),
        XmRVerticalInt, (XtPointer)&default_shell_int
    },
    {
	XtNinput, XtCInput, XmRBool,
	sizeof(Bool), XtOffsetOf(WMShellRec, wm.wm_hints.input),
	XtRImmediate, (XtPointer)True
    },
    {
	XmNwindowGroup, XmCWindowGroup, XmRWindow,
	sizeof(Window), XtOffsetOf(WMShellRec, wm.wm_hints.window_group),
	XmRImmediate, (XtPointer)XtUnspecifiedWindowGroup
    }
};

static XmBaseClassExtRec _XmVendorSCoreClassExtRec = {
    /* next_extension            */ NULL,
    /* record_type               */ NULLQUARK,                             
    /* version                   */ XmBaseClassExtVersion,
    /* size                      */ sizeof(XmBaseClassExtRec),
    /* initialize_prehook        */ initialize_prehook,
    /* set_values_prehook        */ set_values_prehook,
    /* initialize_posthook       */ initialize_posthook,
    /* set_values_posthook       */ set_values_posthook,
    /* secondary_object_class    */ (WidgetClass)&xmVendorShellExtClassRec,
    /* secondary_object_create   */ secondary_object_create,
    /* get_secondary_resources   */ get_sec_res_data,
    /* fast_subclass             */ { 0 },
    /* get_values_prehook        */ get_values_prehook,
    /* get_values_posthook       */ get_values_posthook,
    /* class_part_init_prehook   */ NULL,
    /* class_part_init_posthook  */ NULL,
    /* ext_resources             */ NULL,
    /* compiled_ext_resources    */ NULL,
    /* num_ext_resources         */ 0,
    /* use_sub_resources         */ False,
    /* widget_navigable          */ NULL,
    /* focus_change              */ NULL,
    /* wrapper_data              */ NULL
};

static CompositeClassExtensionRec vendorCompositeExt =
{
    /* next_extension */            NULL,
    /* record_type    */            NULLQUARK,
    /* version        */            XtCompositeExtensionVersion,
    /* record_size    */            sizeof(CompositeClassExtensionRec),
    /* accepts_objects */           True,
#if XtSpecificationRelease >= 6
    /* allows_change_managed_set */ False
#endif
};

static ShellClassExtensionRec shellClassExtRec = {
    NULL,
    NULLQUARK,
    XtShellExtensionVersion,
    sizeof(ShellClassExtensionRec),
    _XmRootGeometryManager
};

VendorShellClassRec vendorShellClassRec = {
    /* Core Class Part */
    {
	/* superclass            */ (WidgetClass)&wmShellClassRec,
	/* class_name	         */ "VendorShell",
	/* size                  */ sizeof(VendorShellRec),
	/* class_initialize      */ class_initialize,
	/* class_part_initialize */ class_part_initialize,
	/* Class init'ed ?       */ False,
	/* initialize            */ initialize,
	/* initialize_hook       */ NULL,		
	/* realize               */ realize,
	/* actions               */ NULL,
	/* num_actions           */ 0,
	/* resources             */ vendor_resources,
	/* resource_count        */ XtNumber(vendor_resources),
	/* xrm_class             */ NULLQUARK,
	/* compress_motion       */ False,
	/* compress_exposure     */ True,
	/* compress_enterleave   */ False,
	/* visible_interest      */ False,
	/* destroy               */ destroy,
	/* resize                */ resize,
	/* expose                */ NULL,
	/* set_values            */ set_values,
	/* set_values_hook       */ NULL,			
	/* set_values_almost     */ XtInheritSetValuesAlmost,  
	/* get_values_hook       */ NULL,
	/* accept_focus          */ NULL,
	/* intrinsics version    */ XtVersion,
	/* callback offsets      */ NULL,
	/* tm_table              */ NULL,
	/* query_geometry        */ NULL,
	/* display_accelerator   */ NULL,
	/* extension             */ (XtPointer)&_XmVendorSCoreClassExtRec,
    },
    /* Composite Class Part */
    {
	/* geometry_manager */ geometry_manager,
        /* change_managed   */ change_managed,
        /* insert_child     */ insert_child,
        /* delete_child     */ delete_child,
        /* extension        */ (XtPointer) &vendorCompositeExt,
    },
    /* Shell Class Part */
    {
        /* extension */ (XtPointer)&shellClassExtRec
    },
    /* WMShell Class Part */
    {
        /* extension */ NULL
    },
    /* Vendor Shell Class Part */
    {
        /* extension */ NULL
    }
};


WidgetClass vendorShellWidgetClass = (WidgetClass)(&vendorShellClassRec);

static Display *default_display = NULL;


static void
class_initialize(void)
{
	int		ncom;
	XtResourceList	combined, shells;
	Cardinal	nshells;

	/* we only do this here so that people can override the default reptypes,
	 * based on information from Harald Albrecht */
	XmSetColorCalculation(NULL);
	_XmRegisterConverters();
	_XmRegisterPixmapConverters();

	ncom = XtNumber(ext_resources) + xmShellExtClassRec.object_class.num_resources;

	_XmTransformSubResources(xmShellExtClassRec.object_class.resources,
			xmShellExtClassRec.object_class.num_resources,
			&shells, &nshells);

	combined = (XtResourceList)XtMalloc(sizeof(XtResource) * ncom);

	memcpy(combined, shells, nshells * sizeof(XtResource));
	memcpy(&combined[nshells], ext_resources,
			XtNumber(ext_resources) * sizeof(XtResource));

	xmVendorShellExtClassRec.object_class.resources = combined;
	xmVendorShellExtClassRec.object_class.num_resources = ncom;

	_XmInitializeExtensions();
	_XmVendorSCoreClassExtRec.record_type = XmQmotif;

	_XmBuildExtResources((WidgetClass)&xmVendorShellExtClassRec);

	if (((XmShellExtObjectClass)&xmVendorShellExtClassRec)->desktop_class.insert_child ==
			XtInheritInsertChild) {
		((XmShellExtObjectClass)&xmVendorShellExtClassRec)->desktop_class.insert_child =
			((XmShellExtObjectClass)xmDesktopClass)->desktop_class.insert_child;
	}
	if (((XmShellExtObjectClass)&xmVendorShellExtClassRec)->desktop_class.delete_child ==
			XtInheritDeleteChild) {
		((XmShellExtObjectClass)&xmVendorShellExtClassRec)->desktop_class.delete_child =
			((XmShellExtObjectClass)xmDesktopClass)->desktop_class.delete_child;
	}

	if (((XmShellExtObjectClass)&xmVendorShellExtClassRec)->shell_class.structureNotifyHandler
			== XmInheritEventHandler) {
		((XmShellExtObjectClass)&xmVendorShellExtClassRec)->
			shell_class.structureNotifyHandler =
			((XmShellExtObjectClass)xmShellExtClass)->
			shell_class.structureNotifyHandler;
	}

	/* Initialize traits */
	_XmInitTraits();

	if (! XmeTraitSet((XtPointer)vendorShellWidgetClass, XmQTspecifyRenderTable,
				(XtPointer)&_XmVendorShellTraitRec)) {
		_XmWarning(NULL, "(Xm)VendorShell ClassInitialize: XmeTraitSet failed\n");
	}

}

static void
VendorFromHorizontalPixels(Widget w, int offset, XtArgVal *value)
{
    *value = (XtArgVal)(*(short *)((char *)XtParent(w) + offset));

    _XmFromHorizontalPixels(XtParent(w), offset, value);
}


static void
VendorFromVerticalPixels(Widget w, int offset, XtArgVal *value)
{
    *value = (XtArgVal)(*(short *)((char *)XtParent(w) + offset));

    _XmFromVerticalPixels(XtParent(w), offset, value);
}


static XmImportOperator
VendorToHorizontalPixels(Widget w, int offset, XtArgVal *value)
{
    _XmToHorizontalPixels(XtParent(w), offset, value);

    return XmSYNTHETIC_NONE;
}


static XmImportOperator
VendorToVerticalPixels(Widget w, int offset, XtArgVal *value)
{
    _XmToVerticalPixels(XtParent(w), offset, value);

    return XmSYNTHETIC_NONE;
}


static void
class_part_initialize(WidgetClass widget_class)
{
    CompositeClassExtension ext, *extptr;
    VendorShellWidgetClass vsclass = (VendorShellWidgetClass)widget_class;

    DEBUGOUT(_LtDebug(__FILE__, NULL,
		      "Vendor Shell's class_part_initialize()\n"));

    extptr = (CompositeClassExtension *)_XmGetClassExtensionPtr((XmGenericClassExt *)&(vsclass->composite_class.extension),
								NULLQUARK);

    if (extptr == NULL || *extptr == NULL)
    {
	ext = (CompositeClassExtension)XtNew(CompositeClassExtensionRec);
	if (ext != NULL)
	{
	    ext->next_extension = vsclass->composite_class.extension;
	    ext->record_type = NULLQUARK;
	    ext->version = XtCompositeExtensionVersion;
	    ext->record_size = sizeof(CompositeClassExtensionRec);
	    ext->accepts_objects = True;
#if XtSpecificationRelease >= 6
	    ext->allows_change_managed_set = False;
#endif
	    vsclass->composite_class.extension = (XtPointer)ext;
	}
    }

    _XmBaseClassPartInitialize(widget_class);
    _XmFastSubclassInit(widget_class, XmVENDOR_SHELL_BIT);

    /* compile the resources */
    if (widget_class == vendorShellWidgetClass)
    {
        _XmSortResourceList((XrmResource **)vsclass->core_class.resources,
                            vsclass->core_class.num_resources);
    }
}


/*
 * Helper function: checks whether a shell is a (real) popup shell or some
 * other kind of shell. We need this one lateron when we have do deal with
 * top level shells.
 * The check is done by searching the list of popups which is maintained by
 * our parent widget. As the widget we're looking for has probably just been
 * added a few machine cycles ago, we're browsing the list backwards.
 */
static Boolean
LTIsARealPopupShell(Widget wid)
{
    Widget dad = XtParent(wid);
    WidgetList popups;
    int i;

    /* Oops, we might got an application shell with no parent */
    if (dad)
    {
	popups = dad->core.popup_list;
	for (i = dad->core.num_popups; --i >= 0;)
	{
	    if (popups[i] == wid)
	    {
		return True;
	    }
	}
    }

    return False;
}


/*
 * Helper function: returns the next parental shell for any given shell
 * widget. If there isn't any parent shell (as we're might already be at the
 * top of the hill) then the function returns NULL.
 */
static Widget
LTGetParentShell(Widget w)
{
    while (((w = XtParent(w)) != NULL) && !XtIsVendorShell(w))
    {
    }

    return w;
}


/*
 * When given the current shell widget this function returns the desktop
 * extension object belonging to the parent shell of the shell given.
 * Well -- if we're already at the top level of the shell hierarchy then
 * the function returns the widget id of the screen widget.
 */
static Widget
LTGetDesktopLogicalParentForShell(Widget w)
{
    Widget logParent = NULL;
    XmWidgetExtData extData;

    if (!XtIsWMShell(w))
    {
	_XmError(w, "LTGetDesktopLogicalParentForShell: "
		 "need a WM shell or a subclass of.");
    }

    if (((WMShellRec *)w)->wm.transient)
    {
	if (XtIsTransientShell(w))
	{
	    /*
	     * If the current shell (in "w") is transient and is indeed of
	     * the transientShell widget class (some kind of dialog), then
	     * we may already may have a shell we're transient for. If we
	     * don't have such a shell, we search for our next parental shell
	     * and use that as the shell we're transient for.
	     */
	    logParent = ((TransientShellRec *)w)->transient.transient_for;

	    if (logParent == NULL)
	    {
		logParent = LTGetParentShell(w);
		((TransientShellRec *)w)->transient.transient_for = logParent;
	    }
	}
	else
	{
	    /*
	     * We're transient, but we are something other than a real
	     * transient shell. So look out for the nearest parental shell
	     * and use that.
	     */
	    logParent = LTGetParentShell(w);
	}
    }

    /*
     * Now if we have found a suitable shell then we're returning the
     * extension object associated with that shell.
     */
    if (logParent && XtIsVendorShell(logParent))
    {
	extData = _XmGetWidgetExtData(logParent, XmSHELL_EXTENSION);
	if (extData == NULL)
	{
	    _XmError(logParent, "vendor shell has no shell extension data");
	}

	return extData->widget;
    }

    /*
     * In case there is no parental shell left -- or -- there is something
     * wrong with the parental shell (that is it is not a vendor shell or
     * a subclass of) then we return the screen widget. This way the
     * vendor extension object is added to the screen's widget child list.
     */
    return XmGetXmScreen(XtScreenOfObject(w));

}


static void
secondary_object_create(Widget req, Widget new_w,
			ArgList args, Cardinal *num_args)
{
    XmBaseClassExt *bce;
    Widget desktopParent;
    XmDisplay d = (XmDisplay)XmGetXmDisplay(XtDisplay(new_w));
    XmWidgetExtData ed;
    Cardinal size;
    XtPointer nsec, rsec;
    extern void _XtAddCallback(XtCallbackList *, XtCallbackProc, XtPointer);

    Display_ShellCount(d)++;

    bce = _XmGetBaseClassExtPtr(XtClass(new_w), XmQmotif);

    DEBUGOUT(_LtDebug(__FILE__, new_w, "secondary_object_create\n"));

    /*
     * The shell extension objects have their XmNdesktopParent set pointing
     * to the next higher level within the shells' hierarchy. The other
     * "parent" resource -- XmNlogicalParent -- indicates the widget the
     * extension object is associated with.
     * --aldi 97/01/03: someone "optimized" here so that setting the
     * logicalParent and the extensionType is done after the XtCreateWidget().
     * But the initialize() method of the Desktop object relies on the
     * desktopParent resource being set already when it is called.
     */
    desktopParent = LTGetDesktopLogicalParentForShell(new_w);

    size = (*bce)->secondaryObjectClass->core_class.widget_size;

    nsec = XtCalloc(1, size);
    rsec = _XmExtObjAlloc(size);

    ((XmExtRec *)nsec)->object.self = (Widget)nsec;
    ((XmExtRec *)nsec)->object.widget_class = (*bce)->secondaryObjectClass;
    ((XmExtRec *)nsec)->object.parent = new_w;
    ((XmExtRec *)nsec)->object.xrm_name = new_w->core.xrm_name;
    ((XmExtRec *)nsec)->object.being_destroyed = False;
    ((XmExtRec *)nsec)->object.destroy_callbacks = NULL;
    ((XmExtRec *)nsec)->object.constraints = NULL;

    ExtObj_LogicalParent(nsec) = new_w;
    ExtObj_ExtensionType(nsec) = XmSHELL_EXTENSION;

    Desktop_Parent(nsec) = desktopParent;

    XtGetSubresources(new_w, nsec, NULL, NULL,
                      (*bce)->secondaryObjectClass->core_class.resources,
                      (*bce)->secondaryObjectClass->core_class.num_resources,
                      args, *num_args);

    ed = (XmWidgetExtData)XtMalloc(sizeof(XmWidgetExtDataRec));
    ed->widget = (Widget)nsec;
    ed->reqWidget = (Widget)rsec;

    memcpy(rsec, nsec, size);
    ((XmExtRec *)rsec)->object.self = (Widget)rsec;

    _XmPushWidgetExtData(new_w, ed, XmSHELL_EXTENSION);

    _XmExtImportArgs((Widget)nsec, args, num_args);

    _XtAddCallback(&VSEP_RealizeCallback(ed->widget),
		   _XmVendorExtRealize, NULL);

    /*
     * Now install some callbacks for use with the grab mechanism...
     */
    DEBUGOUT(_LtDebug2(__FILE__, new_w, ed->widget, "Install pop up/down callbacks\n"));

    XtAddCallback(new_w, XmNpopupCallback,
		  LTShellPopupCallback, (XtPointer)ed->widget);
    XtAddCallback(new_w, XmNpopdownCallback,
		  LTShellPopdownCallback, (XtPointer)ed->widget);
}


static void
initialize_prehook(Widget req, Widget new_w,
		   ArgList args, Cardinal *num_args)
{
    XmBaseClassExt bce;

    /*
     * I had to move this here to avoid recursion problems with XmDisplay
     * 051696 -- seems those problems may have been related to the bugs
     * in BaseClass.  -- MLM
     */
    if (!XmIsDisplay(new_w))
    {

	bce = *(XmBaseClassExt *)_XmGetBaseClassExtPtr(XtClass(new_w),
						       XmQmotif);
	if (bce && bce->secondaryObjectClass)
	{
	    if (bce->secondaryObjectCreate)
		(bce->secondaryObjectCreate) (req, new_w, args, num_args);
	}
    }
}


static void
initialize_posthook(Widget req, Widget new_w,
		    ArgList args, Cardinal *num_args)
{
    XmWidgetExtData ext;

    if ((ext = _XmGetWidgetExtData(new_w, XmSHELL_EXTENSION)) != NULL)
    {
	_XmExtObjFree((XtPointer)ext->reqWidget);
        ext->reqWidget = NULL;
    }
}


#define VSEPC_DeleteWindowHandler(w) \
    (((XmVendorShellExtClassRec *)XtClass(w))-> \
					vendor_class.delete_window_handler)


static void
initialize(Widget req, Widget new_w,
	   ArgList args, Cardinal *num_args)
{
    XmWidgetExtData data;
    XtEventHandler str_not;
    XmShellExtClassRec *shellc;

    DEBUGOUT(_LtDebug(__FILE__, new_w, "####VendorShell initialize\n"));

    ((WMShellWidget)new_w)->wm.wait_for_wm = True;

    data = _XmGetWidgetExtData(new_w, XmSHELL_EXTENSION);
    if (data)
    {
	_XmVendorExtInitialize(data->reqWidget, data->widget, args, num_args);
    }

    if (!default_display)
    {
	default_display = XtDisplay(new_w);
    }

    if (!XmIsDisplay(new_w))
    {
#ifdef LESSTIF_EDITRES
	/* add the handler for editres messages */
	XtAddEventHandler(new_w, (EventMask)0, True,
			  (XtEventHandler)_XmNSEEditResCheckMessages, NULL);
#endif

	DEBUGOUT(_LtDebug(__FILE__, new_w,
			  "Setting up virtual key bindings\n"));

	XtSetKeyTranslator(XtDisplay(new_w), (XtKeyProc)XmTranslateKey);

	if (data)
	{
	    shellc = (XmShellExtClassRec *)XtClass(data->widget);
	    str_not = shellc->shell_class.structureNotifyHandler;

	    XtAddEventHandler(new_w,
			      (EventMask)FocusChangeMask |
			      EnterWindowMask | LeaveWindowMask,
			      True,
			      (XtEventHandler)_XmTrackShellFocus,
			      (XtPointer)data->widget);

	    XtAddEventHandler(new_w,
			      (EventMask)StructureNotifyMask,
			      True,
			      (XtEventHandler)str_not,
			      (XtPointer)data->widget);
	}
    }
}


/*
 * Don't forget to clean up: the shell extension data as well as the
 * extension object for example...
 */
static void
destroy(Widget w)
{
    XmWidgetExtData data;

    DEBUGOUT(_LtDebug(__FILE__, w, "VendorShell Destroy()\n"));
/*  DEBUGOUT(_LtDebug("NEDIT", w, "VendorShell Destroy()\n")); */

    _XmPopWidgetExtData(w, &data, XmSHELL_EXTENSION);
    if (data != NULL) {
	DEBUGOUT(_LtDebug2(__FILE__, w, data->widget,
		"VendorShell Destroy: destroy child\n"));

	/* mitch
	XtDestroyWidget(data->widget);
	*/
	_XmVendorExtDestroy(data->widget);
	XtFree((char *)data);
    }
}


static void
_XmVendorExtDestroy(Widget w)
{
	XmVendorShellExtObject ve = (XmVendorShellExtObject)w;
	Atom wm_delete_window;
	Widget shell = ExtObj_LogicalParent(w);

	DEBUGOUT(_LtDebug(__FILE__, w, "VendorShellExt Destroy()\n"));

	if (VSEP_DefaultFontList(ve)) {
		XmFontListFree(VSEP_DefaultFontList(ve));
	}
	if (VSEP_ButtonFontList(ve)) {
		XmFontListFree(VSEP_ButtonFontList(ve));
	}
	if (VSEP_TextFontList(ve)) {
		XmFontListFree(VSEP_TextFontList(ve));
	}
	if (VSEP_LabelFontList(ve)) {
		XmFontListFree(VSEP_LabelFontList(ve));
	}

	if (VSEP_MwmMenu(ve) != NULL) {
		XtFree((char*)VSEP_MwmMenu(ve));
	}

	_XmDestroyFocusData(VSEP_FocusData(ve));

	wm_delete_window = XmInternAtom(XtDisplay(shell), _XA_WM_DELETE_WINDOW, False);

	/* This removes all protocols and destroys the associated extension object */
	_XmDestroyProtocols(shell);
	_XtRemoveCallback(&VSEP_RealizeCallback(w), _XmVendorExtRealize, NULL);
	XtFree((char *)w);
}


static void
realize(Widget w, XtValueMask *value_mask, XSetWindowAttributes *attributes)
{
    DEBUGOUT(_LtDebug(__FILE__, w, "VendorRealize\n"));

    if (XtWidth(w) == 0)
    	XtWidth(w) = 1;
    if (XtHeight(w) == 0)
    	XtHeight(w) = 1;
#define superclass (&wmShellClassRec)
    (*superclass->core_class.realize) (w, value_mask, attributes);
#undef superclass
#if 0
    {
    int i;

    	printf("%s %s %d %d %s\n",
    		XtName(w),
    		XtClass(w)->core_class.class_name,
    		CoreNumPopups(w),
    		MGR_NumChildren(w),
    		XtParent(w) ? XtName(XtParent(w)) : "NULL"
    		);
    	for (i = 0; i < MGR_NumChildren(w); i++)
    	{
	    printf("\t%s %s\n",
	    	XtName(MGR_Children(w)[i]),
	    	XtClass(MGR_Children(w)[i])->core_class.class_name
	    	);
	    if (XtIsTransientShell(MGR_Children(w)[i]))
	    {
		XSetTransientForHint(XtDisplay(w),
				     XtWindow(MGR_Children(w)[i]),
				     XtWindow(w));
	    }
    	}
    }
#endif
}


static void
WmProtocolHandler(Widget w, XtPointer client, XtPointer call)
{
    XmVendorShellExtObject ve = (XmVendorShellExtObject)client;
    Cardinal i;

    DEBUGOUT(_LtDebug(__FILE__, w, "WmProtocolHandler\n"));

    switch (VSEP_DeleteResponse(ve))
    {
    case XmDESTROY:
	XtDestroyWidget(w);
	if (XtIsApplicationShell(w))
	{
	    DEBUGOUT(_LtDebug(__FILE__, w,
			      "WmProtocolHandler(DeleteResponse XmDESTROY) "
			      "- Exiting (WM_DELETE_WINDOW)\n"));
	    XtDestroyApplicationContext(XtWidgetToApplicationContext(w));
	    exit(0);
	}
	DEBUGOUT(_LtDebug(__FILE__, w,
			  "WmProtocolHandler(DeleteResponse XmDESTROY)\n"));
	break;

    case XmUNMAP:
	XtPopdown(w);
	break;

    case XmDO_NOTHING:
	DEBUGOUT(_LtDebug(__FILE__, w,
			  "WmProtocolHandler(DeleteResponse XmNO_NOTHING)\n"));
	return;
    }
}


static void
resize(Widget w)
{
    int i;

    /* Chain up for now -- needed due to the resize mechanism */
    wmShellClassRec.core_class.resize(w);

    for (i = MGR_NumChildren(w) - 1; i >= 0; i--)
    {

	if (XtIsWidget(MGR_Children(w)[i]) &&
	    XtIsManaged(MGR_Children(w)[i]))
	{
	    XtSetKeyboardFocus(MGR_Children(w)[i], MGR_Children(w)[i]);
	}
    }
}


/*
 * set_values_prehook
 */
static Boolean
set_values_prehook(Widget old, Widget req, Widget new_w,
		   ArgList args, Cardinal *num_args)
{
    XmBaseClassExt *bce;
    XmWidgetExtData data;
    Cardinal size;
    XtPointer nsec, rsec;
    Widget ve;

    bce = _XmGetBaseClassExtPtr(XtClass(new_w), XmQmotif);
    size = (*bce)->secondaryObjectClass->core_class.widget_size;

    nsec = _XmExtObjAlloc(size);
    rsec = _XmExtObjAlloc(size);

    ve = _LtFindVendorExt(new_w);

    data = (XmWidgetExtData)XtMalloc(sizeof(XmWidgetExtDataRec));

    memcpy(rsec, ve, size);
    memcpy(nsec, ve, size);

    data->widget = (Widget)ve;
    data->oldWidget = (Widget)nsec;
    data->reqWidget = (Widget)rsec;

    _XmPushWidgetExtData(new_w, data, XmSHELL_EXTENSION);

    XtSetSubvalues((XtPointer)data->widget,
                   (*bce)->secondaryObjectClass->core_class.resources,
                   (*bce)->secondaryObjectClass->core_class.num_resources,
                   args, *num_args);

    _XmExtImportArgs(data->widget, args, num_args);

    return False;
}


static Boolean
set_values_posthook(Widget old, Widget req, Widget new_w,
		    ArgList args, Cardinal *num_args)
{
    XmWidgetExtData data;

    _XmPopWidgetExtData(new_w, &data, XmSHELL_EXTENSION);

    _XmExtObjFree((XtPointer)data->oldWidget);
    _XmExtObjFree((XtPointer)data->reqWidget);

    XtFree((char *)data);

    return False;
}

static Boolean
set_values(Widget old, Widget req, Widget new_w, ArgList args, Cardinal *num_args)
{
	XmWidgetExtData		data;
	Boolean			refresh = False;
	XmVendorShellExtObject	ve;

	DEBUGOUT(_LtDebug(__FILE__, new_w,
		"VendorSetValues: %i args\n"
		"\t    old X %5i Y %5i W %5i H %5i\n"
		"\trequest X %5i Y %5i W %5i H %5i\n"
		"\t  new_w X %5i Y %5i W %5i H %5i\n",
		*num_args,
		XtX(old), XtY(old),
		XtWidth(old), XtHeight(old),
		XtX(req), XtY(req),
		XtWidth(req), XtHeight(req),
		XtX(new_w), XtY(new_w),
		XtWidth(new_w), XtHeight(new_w)));
	DEBUGOUT(_LtDebugPrintArgList(__FILE__, new_w, args, *num_args, False));

	data = _XmGetWidgetExtData(new_w, XmSHELL_EXTENSION);
	if (data) {
		refresh = _XmVendorExtSetValues(data->oldWidget, data->reqWidget,
					data->widget, args, num_args);
	}

	return refresh;
}

static void
get_values_prehook(Widget widget, ArgList args, Cardinal *num_args)
{
    XmBaseClassExt *bce;
    XmWidgetExtData data;
    Cardinal size;
    XtPointer nsec;
    Widget ve;

    bce = _XmGetBaseClassExtPtr(XtClass(widget), XmQmotif);
    size = (*bce)->secondaryObjectClass->core_class.widget_size;

    nsec = _XmExtObjAlloc(size);

    ve = _LtFindVendorExt(widget);

    memcpy(nsec, ve, size);

    data = (XmWidgetExtData)XtMalloc(sizeof(XmWidgetExtDataRec));
    data->widget = (Widget)nsec;

    _XmPushWidgetExtData(widget, data, XmSHELL_EXTENSION);

    XtGetSubvalues((XtPointer)data->widget,
                   (*bce)->secondaryObjectClass->core_class.resources,
                   (*bce)->secondaryObjectClass->core_class.num_resources,
		   args, *num_args);

    _XmExtGetValuesHook(data->widget, args, num_args);
}


static void
get_values_posthook(Widget widget, ArgList args, Cardinal *num_args)
{
	XmWidgetExtData ext;

	_XmPopWidgetExtData(widget, &ext, XmSHELL_EXTENSION);
	_XmExtObjFree((XtPointer)ext->widget);
	XtFree((char *)ext);
}

static void
change_managed(Widget wid)
{
    int i;

    DEBUGOUT(_LtDebug(__FILE__, wid, "VendorChangeManaged()\n"));
#define superclass (&wmShellClassRec)
    (*superclass->composite_class.change_managed) (wid);
#undef superclass

    for (i = MGR_NumChildren(wid) - 1; i >= 0; i--)
    {
	if (XtIsWidget(MGR_Children(wid)[i]) &&
	    XtIsManaged(MGR_Children(wid)[i]))
	{
	    XtSetKeyboardFocus(wid, MGR_Children(wid)[i]);
	}
    }
}

static void
insert_child(Widget w)
{
	DEBUGOUT(_LtDebug(__FILE__, w, "insert_child()\n"));
	/* Keep those pesky objects OUT of the child list */
	if (!XtIsRectObj(w)) {
		return;
	}

#define superclass (&wmShellClassRec)
	(*superclass->composite_class.insert_child) (w);
#undef superclass
}


static void
delete_child(Widget w)
{
    DEBUGOUT(_LtDebug(__FILE__, w, "delete_child()\n"));
    DEBUGOUT(_LtDebug2("RWS", XtParent(w), w,"%s:delete_child(%d)\n",
    	__FILE__, __LINE__
    	));
    /* Keep those pesky objects OUT of the child list */
    if (!XtIsRectObj(w))
    {
	return;
    }

#define superclass (&wmShellClassRec)
    (*superclass->composite_class.delete_child) (w);
#undef superclass
}


extern Cardinal
_XmFilterResources(XtResource *resources,
		   Cardinal numResources,
		   WidgetClass filterClass,
		   XtResource **filteredResourcesRtn)
{
    *filteredResourcesRtn = NULL;
    return 0;
}


/*
 * This dumps LessTif's idea about Xt's grab list.
 * Changed so it gets triggered both by selecting this file and by GRAB,
 * but not twice :-)
 */
static void
dump_grab_list(Widget w)
{
    Cardinal i;

    if (_LtDebugInDebug(__FILE__, w))
    {
	DEBUGOUT(_LtDebug(__FILE__, w, "Current Grab List\n"));
	for (i = 0; i < Display_NumModals(w); i++)
	{
	    DEBUGOUT(_LtDebug0(__FILE__, w, "\tWid: %p (%s) ve: %p grabber: %p exclusive: %d sprung: %d\n",
		   Display_Modals(w)[i].wid,
		   XtName(Display_Modals(w)[i].wid),
		   Display_Modals(w)[i].ve,
		   Display_Modals(w)[i].grabber, Display_Modals(w)[i].exclusive,
		   Display_Modals(w)[i].springLoaded));
	}
    } else if (_LtDebugInDebug("GRAB", w)) {
	DEBUGOUT(_LtDebug("GRAB", w, "Current Grab List\n"));
	for (i = 0; i < Display_NumModals(w); i++)
	{
	    DEBUGOUT(_LtDebug0("GRAB", w, "\tWid: %p (%s) ve: %p grabber: %p exclusive: %d sprung: %d\n",
		   Display_Modals(w)[i].wid,
		   XtName(Display_Modals(w)[i].wid),
		   Display_Modals(w)[i].ve,
		   Display_Modals(w)[i].grabber, Display_Modals(w)[i].exclusive,
		   Display_Modals(w)[i].springLoaded));
	}
    }
}


/*
 * Put a widget on LessTif's grab list and issue an Intrinsics' grab.
 * The correct use of the function's parameters is a little bit tricky:
 *   - In the simplest case, leave "ve" and "grabber" set to NULL and use
 *     "wid", "exclusive", and "spring_loaded" exactly the same way as you
 *     would using XtAddGrab(). _XmAddGrab() does it just that way.
 *   - You can set "wid" to NULL, but then you *must* specify a valid
 *     "ve" (= vendor shell extension object). In this case the grab is
 *     set to the logical parent to which the "ve" object is bound to.
 */
static void
LTAddGrab(Widget wid, Boolean exclusive, Boolean spring_loaded,
	  XmVendorShellExtObject ve, XmVendorShellExtObject grabber)
{
	Widget d;
	XmModalData	modal = NULL, ps;
	int i;

	/*
	 * If there's no widget wid specified, we'll take the logical parent
	 * of the vendor shell extension object as the grab destination instead.
	 */
	if (wid == NULL) {
		wid = ExtObj_LogicalParent(ve);
		DEBUGOUT(_LtDebug(__FILE__, wid, "LTAddGrab(NULL)\n"));
	} else {
		DEBUGOUT(_LtDebug(__FILE__, wid, "LTAddGrab\n"));
	}

	d = XmGetXmDisplay(XtDisplayOfObject(wid));
	ps = Display_Modals(d);

#if 0
	/* Xt doesn't check what's in there already so why should we ? */
	for (i = Display_NumModals(d); i > 0; i--, ps++) {
#if 1
		/*
		 * This possibly fixes the nedit menu problem.
		 * Now we're comparing not only the widget but also its
		 * status. This probably matches Xt's algorithm better.
		 */
		if (ps->wid == wid && ps->exclusive == exclusive) {
			DEBUGOUT(_LtDebug(__FILE__, wid,
				"LTAddGrab(double) [%d %d] old [%d %d]\n",
				ps->exclusive, ps->springLoaded,
				exclusive, spring_loaded));
			modal = ps;
		}
#else
		if (ps->wid == wid) {
			DEBUGOUT(_LtDebug(__FILE__, wid,
				"LTAddGrab(double) [%d %d] old [%d %d]\n",
				ps->exclusive, ps->springLoaded,
				exclusive, spring_loaded));
			/*
			 * FIX ME this tests for a known problem, it may be
			 * necessary to handle more cases.
			 */
			if (exclusive == ps->exclusive)
				return;
			modal = ps;
		}
#endif
	}
#endif
	dump_grab_list(d);

	if (modal == 0) {
		/* grow the modal list, if necessary */
		if (Display_NumModals(d) >= Display_MaxModals(d)) {
			Display_MaxModals(d) += 8;
			Display_Modals(d) = (XmModalData)XtRealloc(
				(char *)Display_Modals(d),
				sizeof(XmModalDataRec) * Display_MaxModals(d));
		}
		modal = Display_Modals(d) + Display_NumModals(d);
		Display_NumModals(d)++;
	}

	/*
	 * Now occupy a free entry and pollute it with information about the
	 * grab. Then introduce the grab to the Intrinsics and make sure the
	 * grab gets removed if the grab widget should ever be destroyed
	 * before releasing the grab. This is necessary so LessTif's grab list
	 * stays in sync with the Intrinsics's list.
	 */
	modal->wid = wid;
	modal->ve = ve;
	modal->grabber = grabber;
	modal->exclusive = exclusive;
	modal->springLoaded = spring_loaded;

	DEBUGOUT(_LtDebug("GRAB", wid, "%s:XtAddGrab(%d) - %s %s\n", 
		__FILE__, __LINE__,
		exclusive ? "True" : "False",
		spring_loaded ? "True" : "False"));
	XtAddGrab(wid, exclusive, spring_loaded);

	/*
	 * Never, NEVER, add this callback before you'd set up the grab. See
	 * below ( RemoveLessTifGrab() ) for some explanation.
	 */
	XtAddCallback(wid, XmNdestroyCallback,
		(XtCallbackProc)LTRemoveGrabCallback,
		(XtPointer)ve);
	DEBUGOUT(_LtDebug(__FILE__, wid, "LTAddGrab - After it is added\n"));
	DEBUGOUT(_LtDebug("GRAB", wid, "LTAddGrab - After it is added\n"));
	dump_grab_list(d);
}


/*
 * The "public" grab interface. Put a widget on LessTif's grab list. This
 * will also set an Intrinsic's grab on that widget. The tricky thing comes
 * in whenever you remove the grab from such a widget... but see the comments
 * below for more information. Please note that you should always use
 * _XmAddGrab() instead of XtAddGrab() within LessTif.
 */
extern void
_XmAddGrab(Widget wid, Boolean exclusive, Boolean spring_loaded)
{
    DEBUGOUT(_LtDebug(__FILE__, wid, "_XmAddGrab()\n"));
    LTAddGrab(wid, exclusive, spring_loaded,
	      (XmVendorShellExtObject)NULL, (XmVendorShellExtObject)NULL);
}


/*
 * The third parameter "remove_grab" is just a small optimization, so we
 * don't need to remove callbacks and grabs if the widget to be removed
 * from the grab list is already in the phase of being destroyed.
 * There's an exception though:
 *  It seems that Xt is cleaning up the grabs okay
 *  for anything except topLevelShells!
 *
 * One last reflection about what we're doing here:
 *   Especially during destruction of a widget we must restore right at
 *   this place all those grabs that have been issued after the grab on
 *   that widget in destruction. We therefore reissue the appropriate
 *   grabs. We *do rely* on the fact that during destruction of a widget
 *   the grab on that widget has *already been removed* by a callback.
 *   That grab in turn had been set up by the Intrinsics. So we *rely* on
 *   the calling order of callbacks. Sigh. This is the reason why you
 *   *must never* do the XtAddCallback() before the XtAddGrab() in
 *   LTAddGrab() -- or you're dead: in this case the cascade will break.
 *   See: LessTif and it's alter ego M*tif is an excellent example of
 *   clean design. Bang, bang, bang!
 */
static void
LTRemoveGrab(Widget wid, XmVendorShellExtObject ve,
	     Boolean remove_grab_physically)
{
    Widget d;
    XmModalData pd, ps;
    int i, skipped;

    if (wid == NULL) {
	wid = ExtObj_LogicalParent(ve);
	DEBUGOUT(_LtDebug2(__FILE__, wid, (Widget)ve, "LTRemoveGrab\n"));
	DEBUGOUT(_LtDebug2("GRAB", wid, (Widget)ve, "LTRemoveGrab\n"));
    } else {
	DEBUGOUT(_LtDebug(__FILE__, wid, "LTRemoveGrab\n"));
	DEBUGOUT(_LtDebug("GRAB", wid, "LTRemoveGrab\n"));
    }

    /*
     * If we got called during the destruction phase of a widget/object, we
     * don't mind about removing the callback at all -- the callback will
     * blast of into ethernity as soon as the widget fades away.
     */
    if (remove_grab_physically)
    {
	XtRemoveCallback(wid, XmNdestroyCallback,
			 (XtCallbackProc)LTRemoveGrabCallback,
			 (XtPointer)ve);
    }

    /*
     * Now walk through the grab list and reissue all grabs that have
     * been issued after the grab(s) we just about to kill. Due to the
     * Intrinsics' cascade concept, we first must remove the grabs and
     * only then we can regrab the remaining widgets. Otherwise we can
     * put our list out of sync with the grabs set by the Intrinsics'
     */
    d = XmGetXmDisplay(XtDisplayOfObject(wid));
    dump_grab_list(d);
    pd = ps = Display_Modals(d);
    skipped = 0;

    for (i = Display_NumModals(d); i > 0; i--, ps++)
    {
/*	DEBUGOUT(_LtDebug2("GRAB", XtParent(wid), wid, "NEDIT\n")); */

/* amai: The famous "NEdit freeze" bug (an out of sync problem between
         the Xt and LT grab list) was cured by changing
           if ((ps->wid == wid) && !CoreBeingDestroyed(wid) && remove_grab_physically)
	to
           if ((ps->wid == wid) && remove_grab_physically)
	
	This 'fix' in turn triggered XtRemoveGrabs with bogus arguments, leading
	to Xt warning messages. See comments at beginning of LTRemoveGrab()!
*/
#if 1
	if ((ps->wid == wid) && remove_grab_physically) {
		DEBUGOUT(_LtDebug("GRAB", wid, "XtRemoveGrab-1\n"));
		XtRemoveGrab(wid);
	} else
#endif
#if 1
	/* 1.83 version */
	if ((XtParent(wid) == NULL) ||
			((ps->wid == wid) && !CoreBeingDestroyed(wid) &&
			!XtIsTopLevelShell(wid) && remove_grab_physically)) {
		DEBUGOUT(_LtDebug("GRAB", wid, "XtRemoveGrab-2\n"));
		XtRemoveGrab(wid);
	}
#endif
    }

    DEBUGOUT(_LtDebug("GRAB", wid, "Loop to restore grabs : %d iterations\n",
	Display_NumModals(d)));

    ps = pd;
    for (i = Display_NumModals(d); i > 0; i--, ps++, pd++)
    {
	do
	{
	    if (ps->wid == wid)
	    {
		DEBUGOUT(_LtDebug0("GRAB", wid, "\t%d : %s (skip, equal)\n",
			i, ps->wid ? XtName(ps->wid) : "(null)"));
		ps++;
		i--;
		skipped++;	/* skip this entry */
	    }
	    else if ((ps->grabber == ve) && ve)
	    {

		DEBUGOUT(_LtDebug0("GRAB", wid, "\t%d : %s (skip, primary appl modal)\n",
			i, ps->wid ? XtName(ps->wid) : "(null)"));
		/* Get rid off all primary application modal grabs too. */
		ps++;
		i--;
		skipped++;
	    }
	    /* nothing more to skip at the moment */
	    else
	    {
		DEBUGOUT(_LtDebug0("GRAB", wid, "\t%d : %s\n",
			i, ps->wid ? XtName(ps->wid) : "(null)"));
		break;
	    }

	}
	while (i > 0);

	/*
	 * See if we've already reached the end of the list and leave
	 * the loop then. Otherwise check if we've skipped one or more
	 * entries. We know then that we must reissue all grabs coming
	 * after the first entry skipped.
	 */
	if (i <= 0)
	{
	    DEBUGOUT(_LtDebug0("GRAB", wid, "\tLeave loop\n"));
	    break;
	}

	if (pd != ps)
	{
	    *pd = *ps;
	    DEBUGOUT(_LtDebug("GRAB", pd->wid, "%s:XtAddGrab(%d) - %s %s\n", 
		__FILE__, __LINE__,
		pd->exclusive ? "True" : "False",
		pd->springLoaded ? "True" : "False"));
	    XtAddGrab(pd->wid, pd->exclusive, pd->springLoaded);
	}
    }

    Display_NumModals(d) -= skipped;
    DEBUGOUT(_LtDebug("GRAB", wid, "End of loop to restore grabs : %d modals left\n",
	Display_NumModals(d)));
}


/*
 * This is called whenever a widget issued a grab and is now being
 * destroyed. We then must remove the grab from LessTif's grab list
 * to this list in sync with the list from the Intrinsics. Note that the
 * grab has already been removed when we come to this function as calling
 * LTAddGrab() installs another callback located inside the Xt lib and that
 * callback removed the grab.
 */
static void
LTRemoveGrabCallback(Widget wid, XtPointer client_data,
		     XtPointer callback_data)
{
    DEBUGOUT(_LtDebug(__FILE__, wid, "LTRemoveGrabCallback()\n"));
    DEBUGOUT(_LtDebug("GRAB", wid, "LTRemoveGrabCallback()\n"));

    /* rws 14 Feb 1998
       This is the cause of the segfault in ddd when pressing OK from
       the edit->preferences dialog.  When the "init_shell" is created,
       or popped up, a grab gets added.  This callback is being invoked
       because the "init_shell" is being destroyed. For some reason the
       "init_shell" is not being considered a subclass of vendor and
       the grab was not being removed but the widget was getting destroyed.
       This causes problem later when we are playing with the grab list since
       this widget no longer exists _but_ is still on the grab list.

       The only way we get to this point is from a callback added in
       LTAddGrab.  That being the case it should be safe to remove the
       grab!!!
     */
#if 0
    if (!XtIsSubclass(wid, vendorShellWidgetClass))
#endif
    {
#if 1
	/* rws 18 Sep 1999
	   This seems to be the problem of the nedit File->Open problem
	   Also ddd --separate-windows
	 */
	LTRemoveGrab(wid, (XmVendorShellExtObject)client_data, False);
#else
	LTRemoveGrab(wid, (XmVendorShellExtObject)client_data, True);
#endif
    }
}


/*
 * Simply remove a grab from a widget (identified by "wid"). This results
 * in: a) removal of the Intrinsics' grab,
 *     b) removal of the widget from LessTif's grab list.
 * All widgets that did add a grab after our widget wid had added a grab
 * will be put back on their grab. So LessTif's grab list isn't a cascade
 * like the Intrinsics' grab list but rather a modality list.
 */
extern void
_XmRemoveGrab(Widget wid)
{
    DEBUGOUT(_LtDebug(__FILE__, wid, "_XmRemoveGrab()\n"));
    LTRemoveGrab(wid, NULL, True);
}


/*
 * Whenever a primary application modal dialog shows up we must add grabs
 * to all those shells which are not a parental shell of that dialog. This
 * function as well as the next one -- LTGrabRelatives() -- are responsible
 * for this task. LTGrabRelatives() ascends the shell shadow hierarchy
 * starting with the primary application modal dialog and calls LTGrabKids()
 * for every side-branch. LTGrabKids() then recursively put all shells
 * located within the branch on the grab list again.
 */
static void
LTGrabKids(XmVendorShellExtObject ve,
	   XmVendorShellExtObject skip_branch,
	   XmVendorShellExtObject grabber)
{
    int num_kids;
    WidgetList kids;
    Widget logParent;

    DEBUGOUT(_LtDebug(__FILE__, NULL, "LTGrabKids()\n"));
    /*
     * Depending on the class of object within the shadow shell hierarchy
     * we've to choose one out of three different ways to get the object's
     * child list.
     */
    if (XmIsDisplay(ve))
    {
	/*
	 * We do use the short path and dive into the next deeper level
	 * of the shadow hierarchy. There we recursively repeat our task.
	 */
	kids = MGR_Children((Widget)ve);
	num_kids = MGR_NumChildren((Widget)ve);
	for (; --num_kids >= 0; kids++)
	{
	    if (((XmVendorShellExtObject)*kids != skip_branch) &&
		XmIsScreen(*kids))
	    {
		LTGrabKids((XmVendorShellExtObject)*kids,
			   skip_branch, grabber);
	    }
	}

	return;
    }
    else if (_XmIsFastSubclass(XtClass(ve), XmSCREEN_BIT))
    {
	kids = ((XmScreen)ve)->desktop.children;
	num_kids = ((XmScreen)ve)->desktop.num_children;
    }
    else
    {
	kids = Desktop_Children(ve);
	num_kids = Desktop_NumChildren(ve);
    }

    /*
     * We're either working on the children of a screen widget or a
     * vendor shell extension object. So we're sure in every case that
     * these kids are vendor shell extension objects.
     */
    for (; --num_kids >= 0; kids++)
    {
	if ((XmVendorShellExtObject)*kids != skip_branch)
	{
	    logParent = ExtObj_LogicalParent(*kids);
	    /*
	     * If the shell (those vendor shell extension object we're just
	     * observing) has been popped up we must set a non-exclusive
	     * grab on it or otherwise it will get no user input events.
	     * In case this shell is not a popup shell but rather a top
	     * level shell and it has been realized then we must add
	     * a non-exclusive grab, too. Otherwise we would cut it off
	     * from user input.
	     */
	    if (Shell_PoppedUp(logParent))
	    {
		LTAddGrab(NULL, False, False,
			  (XmVendorShellExtObject)*kids, grabber);
	    }
	    else if (XtIsRealized(logParent) &&
		     !LTIsARealPopupShell(logParent))
	    {
		LTAddGrab(NULL, False, False,
			  (XmVendorShellExtObject)*kids, grabber);
	    }
	    /*
	     * Dive into next level of the shadow shell hierarchy and
	     * repeat your task there...
	     */
	    LTGrabKids((XmVendorShellExtObject)*kids,
		       skip_branch, grabber);
	}
    }
}


/*
 * Within this function we start at those vendor shell extension object
 * which has belongs to the dialog just about to pop up. Then we ascend
 * the shadow shell hierarchy, and at each level we descend into those
 * branches which we haven't visited so far. This way we only add a grab
 * to such dialogs which are not parents of the current pop up dialog.
 */
static void
LTGrabRelatives(XmVendorShellExtObject grabber)
{
    XmVendorShellExtObject eo, skip_branch;

    DEBUGOUT(_LtDebug(__FILE__, NULL, "LTGrabRelatives()\n"));
    eo = (XmVendorShellExtObject)Desktop_Parent(grabber);
    skip_branch = grabber;
    for (;;)
    {
	/*
	 * Descend into side-branches not visited so far...
	 */
	LTGrabKids(eo, skip_branch, grabber);
	skip_branch = eo;
	if (_XmIsFastSubclass(XtClass(eo), XmDISPLAY_BIT))
	{
	    /*
	     * We've reached the top of the shadow shell hierarchy. So
	     * let us make a break. We've now visited all important
	     * relatives.
	     */
	    break;
	}
	else if (_XmIsFastSubclass(XtClass(eo), XmSCREEN_BIT))
	{
	    eo = (XmVendorShellExtObject)XtParent(eo);
	}
	else
	{
	    eo = (XmVendorShellExtObject)Desktop_Parent(eo);
	}
    }
}				/* LTGrabRelatives */


/*
 * Whenever a shell widget pops up on the display of a surprised user this
 * callback handler makes sure the appropiate grabs are installed according
 * to the modality mode of the shell. Ouch, what a sentence.
 */
static void
LTShellPopupCallback(Widget w, XtPointer ClientData, XtPointer CallbackData)
{
    XmVendorShellExtObject ve = (XmVendorShellExtObject)ClientData;
    Widget ws;
    XtGrabKind GrabKind = XtGrabNone;
    Boolean GrabRelatives = False;

    DEBUGOUT(_LtDebug(__FILE__, w, "LTShellPopupCallback(%s)\n",
	_LtDebugMwmInput2String(VSEP_MwmHints(ve).input_mode)));

    ws = XmGetXmScreen(XtScreenOfObject(w));

    VSEP_XAtMap(ve) = XtX(w);
    VSEP_YAtMap(ve) = XtY(w);
    if (!XtIsRealized(w) /* == None */ )
	XtRealizeWidget(w);

    /* FIX ME! Is the next one right?? */
    VSEP_LastMapRequest(ve) = LastKnownRequestProcessed(XtDisplayOfObject(w));

    switch (VSEP_MwmHints(ve).input_mode)
    {
    case MWM_INPUT_PRIMARY_APPLICATION_MODAL:
	/*
	 * Input to the ancestors of this window is prohibited. That is, no
	 * parental shell will receive input, whereas our relatives (cousines)
	 * will still receive input as will all dialogs of other top level
	 * shells within our application.
	 */
	GrabKind = XtGrabExclusive;
	if (Screen_MwmPresent(ws))
	{
	    /*
	     * This is at least what M*tif does: if can't find mwm it just
	     * disables ALL other dialogs after popping up a primary
	     * application modal dialog. SOOOORRRRYYY. But I don't know
	     * what support from mwm here would be necessary.
	     */
	    GrabRelatives = True;
	}
	break;

    case MWM_INPUT_SYSTEM_MODAL:
	/*
	 * Input only goes to this window, no other window from any other
	 * application or ourself can receive input. This needs help from
	 * the window manager.
	 *
	 * Fall through.
	 */

    case MWM_INPUT_FULL_APPLICATION_MODAL:
	/*
	 * Input only goes to this window within this application. Other
	 * applications receive input as normal.
	 */
	GrabKind = XtGrabExclusive;	/* Only dispatch all incoming events
					 * to us.
					 */
	break;

    case MWM_INPUT_MODELESS:
    default:
	/*
	 * The input goes to any window as usual.
	 */
	GrabKind = XtGrabNonexclusive;
	break;
    }

    if (GrabKind != XtGrabNone)
    {
	LTAddGrab(NULL,
		  GrabKind == XtGrabExclusive ? True : False, False,
		  ve, ve);
    }
    VSEP_GrabKind(ve) = GrabKind;
    if (GrabRelatives)
    {
	LTGrabRelatives(ve);
    }
}


static void
LTShellPopdownCallback(Widget w, XtPointer ClientData, XtPointer CallbackData)
{
    XmVendorShellExtObject ve = (XmVendorShellExtObject)ClientData;

    DEBUGOUT(_LtDebug(__FILE__, w, "ShellPopdown callback\n"));

    if (VSEP_GrabKind(ve) != XtGrabNone)
    {
	DEBUGOUT(_LtDebug(__FILE__, w, "Remove grab\n"));

	LTRemoveGrab(NULL, ve, True);
    }
}

#ifndef DEFAULT_WM_TIMEOUT
#define DEFAULT_WM_TIMEOUT 500
#endif


static void
ComputeWMSizeHints(WMShellWidget w, XSizeHints * hints)
{
    long flags;

    hints->flags = flags = w->wm.size_hints.flags;

#define copy(field) hints->field = w->wm.size_hints.field

    if (flags & (USPosition | PPosition))
    {
	copy(x);
	copy(y);
    }
    if (flags & (USSize | PSize))
    {
	copy(width);
	copy(height);
    }
    if (flags & PMinSize)
    {
	copy(min_width);
	copy(min_height);
    }
    if (flags & PMaxSize)
    {
	copy(max_width);
	copy(max_height);
    }
    if (flags & PResizeInc)
    {
	copy(width_inc);
	copy(height_inc);
    }
    if (flags & PAspect)
    {
	copy(min_aspect.x);
	copy(min_aspect.y);
	copy(max_aspect.x);
	copy(max_aspect.y);
    }
#undef copy
#define copy(field) hints->field = w->wm.field
    if (flags & PBaseSize)
    {
	copy(base_width);
	copy(base_height);
    }
    if (flags & PWinGravity)
    {
	copy(win_gravity);
    }
#undef copy
}


static void
SetWMSizeHints(WMShellWidget w)
{
    XSizeHints *size_hints = XAllocSizeHints();

    if (size_hints == NULL)
    {
	_XmError((Widget)w, "XAllocSizeHints failed.");
    }

    ComputeWMSizeHints(w, size_hints);

    XSetWMNormalHints(XtDisplay((Widget)w), XtWindow((Widget)w), size_hints);

    XFree((char *)size_hints);
}

#if XtSpecificationRelease < 6

#define WM_CONFIGURE_DENIED(w) (((WMShellWidget) (w))->wm.wm_configure_denied)
#define WM_MOVED(w) (((WMShellWidget) (w))->wm.wm_moved)

typedef struct
{
    Widget w;
    unsigned long request_num;
    Boolean done;
}
QueryStruct;

/* A prototype for a private libXt function ... */
/* rws 17 Dec 2000
   The lower case w may look wrong here, BUT IT IS NOT
 */
extern int _XtwaitForSomething(_XtBoolean ignoreTimers, _XtBoolean ignoreInputs,
			       _XtBoolean ignoreEvents, _XtBoolean block,
			       unsigned long *howlong, XtAppContext app);

static Bool
ConfigEventForMe(Display *dpy, XEvent *event, char *arg)
{
    QueryStruct *q = (QueryStruct *) arg;
    Widget w = q->w;

    if ((dpy != XtDisplay(w)) || (event->xany.window != XtWindow(w)))
    {
	return FALSE;
    }
    if (event->xany.serial >= q->request_num)
    {
	if (event->type == ConfigureNotify)
	{
	    q->done = TRUE;

	    return TRUE;
	}
	else
	{
	    /* This is draft-ICCCM stuff; here for compatibility */
	    if (event->type == ClientMessage &&
		(event->xclient.message_type == WM_CONFIGURE_DENIED(w) ||
		 event->xclient.message_type == WM_MOVED(w)))
	    {
		q->done = TRUE;

		return TRUE;
	    }
	}
    }
    else if (event->type == ConfigureNotify ||
	     (event->type == ClientMessage &&
	      (event->xclient.message_type == WM_CONFIGURE_DENIED(w) ||
	       event->xclient.message_type == WM_MOVED(w))))
    {
	/* flush old events */
	return TRUE;
    }

    if (event->type == ReparentNotify &&
	event->xreparent.window == XtWindow(w))
    {
	/* we might get ahead of this event, so just in case someone
	 * asks for coordinates before this event is dispatched...
	 */
	ShellWidget s = (ShellWidget)w;
	if (event->xreparent.parent != RootWindowOfScreen(XtScreen(w)))
	{
	    s->shell.client_specified &= ~_XtShellNotReparented;
	}
	else
	{
	    s->shell.client_specified |= _XtShellNotReparented;
	}
    }

    return FALSE;
}


static int
WaitForWM(ShellWidget w, XEvent *event, unsigned long request_num)
{
    XtAppContext app = XtWidgetToApplicationContext((Widget)w);
    QueryStruct q;
    unsigned long timeout;

    if (XtIsWMShell((Widget)w))
    {
	timeout = ((WMShellWidget)w)->wm.wm_timeout;
    }
    else
    {
	timeout = DEFAULT_WM_TIMEOUT;
    }

    XFlush(XtDisplay(w));
    q.w = (Widget)w;
    q.request_num = request_num;
    q.done = FALSE;

    for (;;)
    {
	/*
	 * look for match event and discard all prior configures
	 */
	if (XCheckIfEvent(XtDisplay(w), event, ConfigEventForMe, (char *)&q))
	{
	    if (q.done)
	    {
		return TRUE;
	    }
	    else
	    {
		continue;	/* flush old events */
	    }
	}

/* rws 17 Dec 2000
   The lower case w may look wrong here, BUT IT IS NOT
 */
	if (_XtwaitForSomething(TRUE, TRUE, FALSE, TRUE, &timeout, app) != -1)
	{
	    continue;
	}
	if (timeout == 0)
	{
	    return FALSE;
	}
    }
}


extern XtGeometryResult
_XmRootGeometryManager(Widget wid,
		       XtWidgetGeometry *request,
		       XtWidgetGeometry *reply)
{
    ShellWidget w = (ShellWidget)wid;
    XWindowChanges values;
    unsigned int mask = request->request_mode;
    XEvent event;
    Boolean wm;
    struct _OldXSizeHints *hintp = NULL;
    int oldx, oldy, oldwidth, oldheight, oldborder_width;
    unsigned long request_num;
    extern String XtCXtToolkitError;

    if (XtIsWMShell(wid))
    {
	wm = True;
	hintp = &((WMShellWidget)w)->wm.size_hints;
	/* for draft-ICCCM wm's, need to make sure hints reflect
	 * (current) reality so client can move and size separately. */
	hintp->x = XtX(w);
	hintp->y = XtY(w);
	hintp->width = XtWidth(w);
	hintp->height = XtHeight(w);
    }
    else
    {
	wm = False;
    }

    oldx = XtX(w);
    oldy = XtY(w);
    oldwidth = XtWidth(w);
    oldheight = XtHeight(w);
    oldborder_width = XtBorderWidth(w);

#define PutBackGeometry() \
        { XtX(w) = oldx; \
          XtY(w) = oldy; \
          XtWidth(w) = oldwidth; \
          XtHeight(w) = oldheight; \
          XtBorderWidth(w) = oldborder_width; }

    if (mask & CWX)
    {
	if (XtX(w) == request->x)
	{
	    mask &= ~CWX;
	}
	else
	{
	    XtX(w) = values.x = request->x;
	    if (wm)
	    {
		hintp->flags &= ~USPosition;
		hintp->flags |= PPosition;
		hintp->x = values.x;
	    }
	}
    }

    if (mask & CWY)
    {
	if (XtY(w) == request->y)
	{
	    mask &= ~CWY;
	}
	else
	{
	    XtY(w) = values.y = request->y;
	    if (wm)
	    {
		hintp->flags &= ~USPosition;
		hintp->flags |= PPosition;
		hintp->y = values.y;
	    }
	}
    }

    if (mask & CWBorderWidth)
    {
	if (XtBorderWidth(w) == request->border_width)
	{
	    mask &= ~CWBorderWidth;
	}
	else
	{
	    XtBorderWidth(w) = values.border_width = request->border_width;
	}
    }

    if (mask & CWWidth)
    {
	if (XtWidth(w) == request->width)
	{
	    mask &= ~CWWidth;
	}
	else
	{
	    XtWidth(w) = values.width = request->width;
	    if (wm)
	    {
		hintp->flags &= ~USSize;
		hintp->flags |= PSize;
		hintp->width = values.width;
	    }
	}
    }

    if (mask & CWHeight)
    {
	if (XtHeight(w) == request->height)
	{
	    mask &= ~CWHeight;
	}
	else
	{
	    XtHeight(w) = values.height = request->height;
	    if (wm)
	    {
		hintp->flags &= ~USSize;
		hintp->flags |= PSize;
		hintp->height = values.height;
	    }
	}
    }

    if (mask & CWStackMode)
    {
	values.stack_mode = request->stack_mode;
	if (mask & CWSibling)
	{
	    values.sibling = XtWindow(request->sibling);
	}
    }

    if (!XtIsRealized((Widget)w))
    {
	return XtGeometryYes;
    }

    request_num = NextRequest(XtDisplay(w));

    XConfigureWindow(XtDisplay((Widget)w), XtWindow((Widget)w), mask, &values);

    if (wm && !w->shell.override_redirect &&
	mask & (CWX | CWY | CWWidth | CWHeight | CWBorderWidth))
    {
	SetWMSizeHints((WMShellWidget)w);
    }

    if (w->shell.override_redirect)
    {
	return XtGeometryYes;
    }

    /* If no non-stacking bits are set, there's no way to tell whether
     * or not this worked, so assume it did */
    if (!(mask & ~(CWStackMode | CWSibling)))
    {
	return XtGeometryYes;
    }

    if (wm && ((WMShellWidget)w)->wm.wait_for_wm == FALSE)
    {
	/* From Xt:
	 * the window manager is sick so I will do the work and
	 * say no so if a new WM starts up, or the current one recovers
	 * my size requests will be visible
	 *
	 * From MLM:
	 * This is madness, I think.  That means you'll refuse any resize
	 * requests, unless the WM follows the ICCCM, right?
	 */
	return XtGeometryYes;
    }

    if (WaitForWM(w, &event, request_num))
    {
	/* got an event */
	if (event.type == ConfigureNotify)
	{


#define NEQ(x, msk) ((mask & msk) && (values.x != event.xconfigure.x))
	    if (NEQ(x, CWX) ||
		NEQ(y, CWY) ||
		NEQ(width, CWWidth) ||
		NEQ(height, CWHeight) ||
		NEQ(border_width, CWBorderWidth))
	    {
#undef NEQ
		XPutBackEvent(XtDisplay(w), &event);
		PutBackGeometry();

		/*
		 * We just potentially re-ordered the event queue
		 * w.r.t. ConfigureNotifies with some trepidation.
		 * But this is probably a Good Thing because we
		 * will know the new true state of the world sooner
		 * this way.
		 */

		DEBUGOUT(_LtDebug(__FILE__, (Widget)w,
				  "Returning XtGeometryNo because config "
				  "differed\n"));

		return XtGeometryNo;
	    }
	    else
	    {
		XtWidth(w) = event.xconfigure.width;
		XtHeight(w) = event.xconfigure.height;
		XtBorderWidth(w) = event.xconfigure.border_width;

		if (event.xany.send_event ||	/* ICCCM compliant synth */
		    w->shell.client_specified & _XtShellNotReparented)
		{

		    XtX(w) = event.xconfigure.x;
		    XtY(w) = event.xconfigure.y;
		    w->shell.client_specified |= _XtShellPositionValid;
		}
		else
		{
		    w->shell.client_specified &= ~_XtShellPositionValid;
		}

		return XtGeometryYes;
	    }
	}
	else if (!wm ||
		 (event.type == ClientMessage &&
		  event.xclient.message_type == WM_CONFIGURE_DENIED(w)))
	{
	    PutBackGeometry();

	    DEBUGOUT(_LtDebug(__FILE__, (Widget)w,
			      "Returning XtGeometryNo because config "
			      "denied\n"));

	    return XtGeometryNo;
	}
	else if (event.type == ClientMessage &&
		 event.xclient.message_type == WM_MOVED(w))
	{
	    XtX(w) = event.xclient.data.s[0];
	    XtY(w) = event.xclient.data.s[1];
	    w->shell.client_specified |= _XtShellPositionValid;
	    return XtGeometryYes;
	}
	else
	{
	    XtAppWarningMsg(XtWidgetToApplicationContext((Widget)w),
			    "internalError", "shell", XtCXtToolkitError,
			    "Shell's window manager interaction is broken",
			    (String *)NULL, (Cardinal *)NULL);
	}
    }
    else if (wm)
    {
	/* no event */
	((WMShellWidget)w)->wm.wait_for_wm = FALSE;
	/* timed out; must be broken */
    }

#undef PutBackGeometry

    return XtGeometryYes;
}

#else

/*
 * For R6:
 */

typedef struct
{
    Widget w;
    unsigned long request_num;
    Boolean done;
}
QueryStruct;

/* A prototype for a private libXt function ... */
extern int _XtWaitForSomething(XtAppContext app,
			       _XtBoolean ignoreEvents,
			       _XtBoolean ignoreTimers,
			       _XtBoolean ignoreInputs,
			       _XtBoolean ignoreSignals,
			       _XtBoolean block,
#ifdef XTHREADS
			       _XtBoolean drop_lock,
#endif
			       unsigned long *howlong);

/* amai: citing the docs, e.g. from
 *     ftp://ftp.x.org/pub/R6.6/xc/lib/Xt/NextEvent.c
 * for that
 * Routine to block in the toolkit.  This should be the only call to select.
 *
 * This routine returns when there is something to be done.
 *
 * Before calling this with ignoreInputs==False, app->outstandingQueue should
 * be checked; this routine will not verify that an alternate input source
 * has not already been enqueued.
 *
 *
 * _XtWaitForSomething( appContext, 
 *                      ignoreEvent, ignoreTimers, ignoreInputs, ignoreSignals,
 *			block, drop_lock, howlong)
 * XtAppContext app;	     (Displays to check wait on)
 *
 * Boolean ignoreEvents;     (Don't return if XEvents are available
 *                              Also implies forget XEvents exist)
 *
 * Boolean ignoreTimers;     (Ditto for timers)
 *
 * Boolean ignoreInputs;     (Ditto for input callbacks )
 *
 * Boolean ignoreSignals;    (Ditto for signals)
 *
 * Boolean block;	     (Okay to block)
 *
 * Boolean drop_lock         (drop lock before going into select/poll)
 *
 * TimeVal howlong;	     (howlong to wait for if blocking and not
 *				doing Timers... Null means forever.
 *				Maybe should mean shortest of both)
 * Returns display for which input is available, if any
 * and if ignoreEvents==False, else returns -1
 *
 * if ignoring everything && block=True && howlong=NULL, you'll have
 * lots of time for coffee; better not try it!  In fact, it probably
 * makes little sense to do this regardless of the value of howlong
 * (bottom line is, we don't bother checking here).
 *
 * If drop_lock is FALSE, the app->lock->mutex is not unlocked before
 * entering select/poll. It is illegal for drop_lock to be FALSE if
 * ignoreTimers, ignoreInputs, or ignoreSignals is FALSE.
 */

static Bool
ConfigEventForMe(Display *dpy, XEvent *event, char *arg)
{
    QueryStruct *q = (QueryStruct *) arg;
    Widget w = q->w;

    if ((dpy != XtDisplay(w)) || (event->xany.window != XtWindow(w)))
    {
	return FALSE;
    }
    if (event->xany.serial >= q->request_num)
    {
	if (event->type == ConfigureNotify)
	{
	    q->done = TRUE;
	    return TRUE;
	}
    }
    else if (event->type == ConfigureNotify)
    {
	/* flush old events */
	return TRUE;
    }

    if (event->type == ReparentNotify
	&& event->xreparent.window == XtWindow(w))
    {
	/* we might get ahead of this event, so just in case someone
	 * asks for coordinates before this event is dispatched...
	 */
	ShellWidget s = (ShellWidget)w;
	if (event->xreparent.parent != RootWindowOfScreen(XtScreen(w)))
	{
	    s->shell.client_specified &= ~_XtShellNotReparented;
	}
	else
	{
	    s->shell.client_specified |= _XtShellNotReparented;
	}
    }

    return FALSE;
}


static int
WaitForWM(ShellWidget w, XEvent *event, unsigned long request_num)
{
    XtAppContext app = XtWidgetToApplicationContext((Widget)w);
    QueryStruct q;
    unsigned long timeout;

    if (XtIsWMShell((Widget)w))
    {
	timeout = ((WMShellWidget)w)->wm.wm_timeout;
    }
    else
    {
	timeout = DEFAULT_WM_TIMEOUT;
    }

    XFlush(XtDisplay(w));
    q.w = (Widget)w;
    q.request_num = request_num;
    q.done = FALSE;

    /*
     * look for match event and discard all prior configures
     */
    while (XCheckIfEvent(XtDisplay(w), event, ConfigEventForMe, (char *)&q))
    {
	if (q.done)
	{
	    return TRUE;
	}
    }

/*
  amai: we have to get rid of this _XtWaitForSomething ASAP:
    this call changes it's signature based on compile-time options
    for the X libraries :-(
    So beyond the pure annoying fact of using a non-documented, private call
    from libXt we also break cross-compiling.
    And no, a run-time if-clause is not a "real" solution IMHO.
    A proposal is to install an even handler and a timer 
       XtAppAddTimeOut()
       XtAddEventHandler()
    and then do the remaining job in the triggered callbacks.
    WaitForWM() is only called once in the code, one has to check
    what is done _there_.
*/
    while (timeout > 0)
    {
	if (_XtWaitForSomething(app,
				FALSE,
				TRUE,
				TRUE,
				TRUE,
				TRUE,
#ifdef XTHREADS
				FALSE,
#endif
				&timeout) != -1)
	{
	    while (XCheckIfEvent(XtDisplay(w), event,
				 ConfigEventForMe, (char *)&q))
	    {
		if (q.done)
		{
		    return TRUE;
		}
	    }
	}
    }
    return FALSE;
}


XtGeometryResult
_XmRootGeometryManager(Widget gw,
		       XtWidgetGeometry *request,
		       XtWidgetGeometry *reply)
{
    ShellWidget w = (ShellWidget)gw;
    XWindowChanges values;
    unsigned int mask = request->request_mode;
    XEvent event;
    Boolean wm;
    struct _OldXSizeHints *hintp = NULL;
    int oldx, oldy, oldwidth, oldheight, oldborder_width;
    unsigned long request_num;
    extern String XtCXtToolkitError;

    if (XtIsWMShell(gw))
    {
	wm = True;
	hintp = &((WMShellWidget)w)->wm.size_hints;
	/* for draft-ICCCM wm's, need to make sure hints reflect
	   (current) reality so client can move and size separately. */
	hintp->x = XtX(w);
	hintp->y = XtY(w);
	hintp->width = XtWidth(w);
	hintp->height = XtHeight(w);
    }
    else
    {
	wm = False;
    }

    oldx = XtX(w);
    oldy = XtY(w);
    oldwidth = XtWidth(w);
    oldheight = XtHeight(w);
    oldborder_width = XtBorderWidth(w);

#define PutBackGeometry() \
        { XtX(w) = oldx; \
          XtY(w) = oldy; \
          XtWidth(w) = oldwidth; \
          XtHeight(w) = oldheight; \
          XtBorderWidth(w) = oldborder_width; }

    if (mask & CWX)
    {
	if (XtX(w) == request->x)
	{
	    mask &= ~CWX;
	}
	else
	{
	    XtX(w) = values.x = request->x;
	    if (wm)
	    {
		hintp->flags &= ~USPosition;
		hintp->flags |= PPosition;
		hintp->x = values.x;
	    }
	}
    }
    if (mask & CWY)
    {
	if (XtY(w) == request->y)
	{
	    mask &= ~CWY;
	}
	else
	{
	    XtY(w) = values.y = request->y;
	    if (wm)
	    {
		hintp->flags &= ~USPosition;
		hintp->flags |= PPosition;
		hintp->y = values.y;
	    }
	}
    }
    if (mask & CWBorderWidth)
    {
	if (XtBorderWidth(w) == request->border_width)
	{
	    mask &= ~CWBorderWidth;
	}
	else
	{
	    XtBorderWidth(w) = values.border_width = request->border_width;
	}
    }
    if (mask & CWWidth)
    {
	if (XtWidth(w) == request->width)
	{
	    mask &= ~CWWidth;
	}
	else
	{
	    XtWidth(w) = values.width = request->width;
	    if (wm)
	    {
		hintp->flags &= ~USSize;
		hintp->flags |= PSize;
		hintp->width = values.width;
	    }
	}
    }
    if (mask & CWHeight)
    {
	if (XtHeight(w) == request->height)
	{
	    mask &= ~CWHeight;
	}
	else
	{
	    XtHeight(w) = values.height = request->height;
	    if (wm)
	    {
		hintp->flags &= ~USSize;
		hintp->flags |= PSize;
		hintp->height = values.height;
	    }
	}
    }
    if (mask & CWStackMode)
    {
	values.stack_mode = request->stack_mode;
	if (mask & CWSibling)
	{
	    values.sibling = XtWindow(request->sibling);
	}
    }

    if (!XtIsRealized((Widget)w))
    {
	return XtGeometryYes;
    }

    request_num = NextRequest(XtDisplay(w));
    XConfigureWindow(XtDisplay((Widget)w), XtWindow((Widget)w), mask, &values);

    if (wm && !w->shell.override_redirect
	&& mask & (CWX | CWY | CWWidth | CWHeight | CWBorderWidth))
    {
	SetWMSizeHints((WMShellWidget)w);
    }

    if (w->shell.override_redirect)
    {
	return XtGeometryYes;
    }

    /* If no non-stacking bits are set, there's no way to tell whether
       or not this worked, so assume it did */

    if (!(mask & ~(CWStackMode | CWSibling)))
    {
	return XtGeometryYes;
    }

    if (wm && ((WMShellWidget)w)->wm.wait_for_wm == FALSE)
    {
	/*
	 * From R6:
	 * the window manager is sick so I will do the work and 
	 * say no so if a new WM starts up, or the current one recovers
	 * my size requests will be visible
	 *
	 * From MLM:
	 * This is madness, I think.  That means you'll refuse any
	 * resize requests, unless the WM follows the ICCCM, right?
	 */
	return XtGeometryYes;
    }

    if (WaitForWM(w, &event, request_num))
    {
	/* got an event */
	if (event.type == ConfigureNotify)
	{

#define NEQ(x, msk) ((mask & msk) && (values.x != event.xconfigure.x))
	    if (NEQ(x, CWX) ||
		NEQ(y, CWY) ||
		NEQ(width, CWWidth) ||
		NEQ(height, CWHeight) ||
		NEQ(border_width, CWBorderWidth))
	    {
#undef NEQ
		XPutBackEvent(XtDisplay(w), &event);
		PutBackGeometry();
		/*
		 * We just potentially re-ordered the event queue
		 * w.r.t. ConfigureNotifies with some trepidation.
		 * But this is probably a Good Thing because we
		 * will know the new true state of the world sooner
		 * this way.
		 */

		DEBUGOUT(_LtDebug(__FILE__, (Widget)w,
				  "Returning XtGeometryNo because config "
				  "differed\n"));

		return XtGeometryNo;
	    }
	    else
	    {
		XtWidth(w) = event.xconfigure.width;
		XtHeight(w) = event.xconfigure.height;
		XtBorderWidth(w) = event.xconfigure.border_width;
		if (event.xany.send_event ||	/* ICCCM compliant synth */
		    w->shell.client_specified & _XtShellNotReparented)
		{

		    XtX(w) = event.xconfigure.x;
		    XtY(w) = event.xconfigure.y;
		    w->shell.client_specified |= _XtShellPositionValid;
		}
		else
		{
		    w->shell.client_specified &= ~_XtShellPositionValid;
		}

		return XtGeometryYes;
	    }
	}
	else if (!wm)
	{
	    PutBackGeometry();

	    DEBUGOUT(_LtDebug(__FILE__, (Widget)w,
			      "Returning XtGeometryNo because not "
			      "WMShell\n"));

	    return XtGeometryNo;
	}
	else
	{
	    XtAppWarningMsg(XtWidgetToApplicationContext((Widget)w),
			    "internalError", "shell", XtCXtToolkitError,
			    "Shell's window manager interaction is broken",
			    (String *)NULL, (Cardinal *)NULL);
	}
    }
    else if (wm)
    {
	/* no event */
	((WMShellWidget)w)->wm.wait_for_wm = FALSE;
	/* timed out; must be broken */
    }
#undef PutBackGeometry

    return XtGeometryYes;
}

#endif /* XtSpecificationRelease < 6 */


/*
 * if this doesn't exist, the BaseClass geometry wrapper won't work.
 */
static XtGeometryResult
geometry_manager(Widget wid,
		 XtWidgetGeometry *request,
		 XtWidgetGeometry *reply)
{
    ShellWidget shell = (ShellWidget)XtParent(wid);
    XtWidgetGeometry req;

    DEBUGOUT(_LtDebug2(__FILE__, XtParent(wid), wid, "%s:geometry_manager(%d) - request (%s)\n",
    	__FILE__, __LINE__,
    	_LtDebugWidgetGeometry2String(request)));

    if ((request->width == 0 && (request->request_mode & CWWidth)) ||
	(request->height == 0 && (request->request_mode & CWHeight)))
    {
	DEBUGOUT(_LtDebug(__FILE__, wid,
		          "Returning XtGeometryNo because asked "
		          "for 0 w/h\n"));

	return XtGeometryNo;
    }

    if (shell->shell.allow_shell_resize == False && XtIsRealized(wid) && (request->request_mode & (CWWidth | CWHeight)))
    {
	DEBUGOUT(_LtDebug(__FILE__, wid,
		          "Returning XtGeometryNo because "
		          "no shell resize\n"));

	return XtGeometryNo;
    }

    /* rws 12 Dec 1997
       mgs bubble help
    if (request->request_mode & (CWX | CWY))
    {
	DEBUGOUT(_LtDebug(__FILE__, wid,
		          "Returning XtGeometryNo because "
		          "asked for x/y\n"));

	return XtGeometryNo;
    }
    */

    /*
     * This is a slight change from the Xt implementation.  For some reason,
     * even though we were passing in XtCWQueryOnly in the request call below,
     * the shell was still getting resized.  For now, if we pass the above
     * tests, and are only querying, return Yes.
     */
    if (request->request_mode & XtCWQueryOnly)
    {
	return XtGeometryYes;
    }

    req.request_mode = (request->request_mode & XtCWQueryOnly);
    if (request->request_mode & CWWidth)
    {
	req.width = request->width;
	req.request_mode |= CWWidth;
    }
    if (request->request_mode & CWHeight)
    {
	req.height = request->height;
	req.request_mode |= CWHeight;
    }
    if (request->request_mode & CWBorderWidth)
    {
	req.border_width = request->border_width;
	req.request_mode |= CWBorderWidth;
    }
    if ((request->request_mode & CWX) && request->x != 0)
    {
	req.x = request->x;
	req.request_mode |= CWX;
    }
    if ((request->request_mode & CWY) && request->y != 0)
    {
	req.y = request->y;
	req.request_mode |= CWY;
    }

    if (XtMakeGeometryRequest((Widget)shell, &req, NULL) == XtGeometryYes)
    {
	if (!(request->request_mode & XtCWQueryOnly))
	{
	    wid->core.width = /*req.width; */ shell->core.width;
	    wid->core.height = /*req.height; */	shell->core.height;
	    if (request->request_mode & CWBorderWidth)
	    {
		wid->core.x = wid->core.y = -request->border_width;
	    }
	}

	XSync(XtDisplay(wid), False);

	return XtGeometryYes;
    }

   DEBUGOUT(_LtDebug(__FILE__, wid,
		     "Returning XtGeometryNo because "
		     "XtMakeGeometryRequest failed\n"));

    return XtGeometryNo;
}


static void
_XmVendorExtInitialize(Widget req,
		       Widget new_w,
		       ArgList args,
		       Cardinal *num_args)
{
    XmVendorShellExtObject ve = (XmVendorShellExtObject)new_w;
    Atom wm_delete_window;

    DEBUGOUT(_LtDebug(__FILE__, new_w, "VendorShellExtInitialize\n"));

    if (VSEP_DefaultFontList(ve) != NULL)
    {
	VSEP_DefaultFontList(ve) = XmFontListCopy(VSEP_DefaultFontList(ve));
    }

    /* initialize the button font list */
    if (VSEP_ButtonFontList(ve) == NULL)
    {
	if (VSEP_DefaultFontList(ve)) {
	    VSEP_ButtonFontList(ve) = XmFontListCopy(VSEP_DefaultFontList(ve));
	} else {
	    VSEP_ButtonFontList(ve) =
		_XmGetDefaultFontList(ExtObj_LogicalParent(new_w), XmBUTTON_FONTLIST);
	}
    } else {
	VSEP_ButtonFontList(ve) = XmFontListCopy(VSEP_ButtonFontList(ve));
    }

    /* initialize the label font list */
    if (VSEP_LabelFontList(ve) == NULL) {
	if (VSEP_DefaultFontList(ve)) {
	    VSEP_LabelFontList(ve) = XmFontListCopy(VSEP_DefaultFontList(ve));
	} else {
	    VSEP_LabelFontList(ve) =
		_XmGetDefaultFontList(ExtObj_LogicalParent(new_w), XmLABEL_FONTLIST);
	}
    } else {
	VSEP_LabelFontList(ve) = XmFontListCopy(VSEP_LabelFontList(ve));
    }

    /* initialize the text font list */
    if (VSEP_TextFontList(ve) == NULL) {
	if (VSEP_DefaultFontList(ve)) {
	    VSEP_TextFontList(ve) = XmFontListCopy(VSEP_DefaultFontList(ve));
	} else {
	    VSEP_TextFontList(ve) =
		_XmGetDefaultFontList(ExtObj_LogicalParent(new_w), XmTEXT_FONTLIST);
	}
    } else {
	VSEP_TextFontList(ve) = XmFontListCopy(VSEP_TextFontList(ve));
    }

    VSEP_MwmHints(ve).flags = 0;

    if (VSEP_MwmMenu(ve) != NULL)
    {
	VSEP_MwmMenu(ve) = XtNewString(VSEP_MwmMenu(ve));
    }

    if (VSEP_MwmHints(ve).functions != -1)
    {
	VSEP_MwmHints(ve).flags |= MWM_HINTS_FUNCTIONS;
    }
    if (VSEP_MwmHints(ve).decorations != -1)
    {
	VSEP_MwmHints(ve).flags |= MWM_HINTS_DECORATIONS;
    }
    if (VSEP_MwmHints(ve).input_mode != -1)
    {
	VSEP_MwmHints(ve).flags |= MWM_HINTS_INPUT_MODE;
    }

    VSEP_ImInfo(ve) = NULL;

    VSEP_FocusData(ve) = _XmCreateFocusData();

    wm_delete_window = XmInternAtom(XtDisplay(ExtObj_LogicalParent(new_w)),
				    _XA_WM_DELETE_WINDOW, False);

    _XmInitProtocols(ExtObj_LogicalParent(new_w));

    XmAddWMProtocols(ExtObj_LogicalParent(new_w), &wm_delete_window, 1);
    XmSetWMProtocolHooks(ExtObj_LogicalParent(new_w), wm_delete_window,
			 NULL, NULL,
			 VSEPC_DeleteWindowHandler(new_w), (XtPointer)new_w);
}

static Boolean
_XmVendorExtSetValues(Widget cw, Widget rw, Widget nw, ArgList args, Cardinal *nargs)
{
    Atom at;
    long prop32[5];
    XmVendorShellExtObject o= (XmVendorShellExtObject)nw;

    DEBUGOUT(_LtDebug(__FILE__, nw,
		      "VendorShellExtSetValues, args:%d\n",
		      *nargs));

    if (VSEP_ButtonFontList(nw) != VSEP_ButtonFontList(cw))
    {
	XmFontListFree(VSEP_ButtonFontList(cw));
	VSEP_ButtonFontList(nw) = XmFontListCopy(VSEP_ButtonFontList(nw));
    }
    if (VSEP_LabelFontList(nw) != VSEP_LabelFontList(cw))
    {
	XmFontListFree(VSEP_LabelFontList(cw));
	VSEP_LabelFontList(nw) = XmFontListCopy(VSEP_LabelFontList(nw));
    }
    if (VSEP_TextFontList(nw) != VSEP_TextFontList(cw))
    {
	XmFontListFree(VSEP_TextFontList(cw));
	VSEP_TextFontList(nw) = XmFontListCopy(VSEP_TextFontList(nw));
    }

    VSEP_MwmHints(nw).flags = 0;

    if (VSEP_MwmHints(nw).functions != -1)
    {
	VSEP_MwmHints(nw).flags |= MWM_HINTS_FUNCTIONS;
    }
    if (VSEP_MwmHints(nw).decorations != -1)
    {
	VSEP_MwmHints(nw).flags |= MWM_HINTS_DECORATIONS;
    }
    if (VSEP_MwmHints(nw).input_mode != -1)
    {
	VSEP_MwmHints(nw).flags |= MWM_HINTS_INPUT_MODE;
    }

    if (XtIsRealized(ExtObj_LogicalParent(nw)) &&
	memcmp(&VSEP_MwmHints(nw), &VSEP_MwmHints(cw), sizeof(MwmHints)) != 0)
    {
	at = XmInternAtom(XtDisplay(nw), _XA_MWM_HINTS, False);
	/* note the 32 format = long on client side */
	prop32[0]=VSEP_MwmHints(nw).flags;
	prop32[1]=VSEP_MwmHints(nw).functions;
	prop32[2]=VSEP_MwmHints(nw).decorations;
	prop32[3]=VSEP_MwmHints(nw).input_mode;
	prop32[4]=VSEP_MwmHints(nw).status;
	XChangeProperty(XtDisplay(nw), XtWindow(nw), at, at, 32,
			PropModeReplace,
			(unsigned char *)prop32,
			PROP_MOTIF_WM_HINTS_ELEMENTS);
    }

/* rws 13 Sep 2000
   We have to do this whether we are realized or not.
   test/Xm/mwm/test5 -xrm "*autoExit: True" show this.
   It does a SetValues on mwmMenu with a static string, if we do not
   copy this string we end up segfaulting when things get destroyed!!!
 */
    if (/*XtIsRealized(ExtObj_LogicalParent(nw)) &&*/
	((!VSEP_MwmMenu(nw) && VSEP_MwmMenu(cw)) ||
	 (VSEP_MwmMenu(nw) && !VSEP_MwmMenu(cw)) ||
	 (VSEP_MwmMenu(nw) && VSEP_MwmMenu(cw) &&
	  strcmp(VSEP_MwmMenu(nw), VSEP_MwmMenu(cw)) != 0)))
    {
	if (VSEP_MwmMenu(cw))
	{
	    XtFree(VSEP_MwmMenu(cw));
	}

	if (VSEP_MwmMenu(nw))
	{
	    VSEP_MwmMenu(nw) = XtNewString(VSEP_MwmMenu(nw));
	}

	if (XtIsRealized(ExtObj_LogicalParent(nw)) && VSEP_MwmMenu(nw) != NULL)
	{
	    /* Only do this if we have a reason for it.
	     * The Motif 2.1 mwm seems to crash without this. */
	    at = XmInternAtom(XtDisplay(nw), _XA_MWM_MENU, False);
	    /* note the 8 format */
	    XChangeProperty(XtDisplay(nw), XtWindow(nw), at, at, 8,
		PropModeReplace,
		(unsigned char *)VSEP_MwmMenu(nw), strlen(VSEP_MwmMenu(nw)));
	}
    }

	return False;
}


extern void
_XmVendorExtRealize(Widget w, XtPointer closure, XtPointer call_data)
{
    Atom at;
    Widget par;
    Cardinal i;
    long prop32[5];

    DEBUGOUT(_LtDebug(__FILE__, w, "XmVendorExtRealize\n"));

	_XmPickupUnspecifiedPixmaps(XtDisplay(w));

    par = ExtObj_LogicalParent(w);

    if (!XmIsDisplay(par))
    {
	_XmInstallProtocols(par);
    }

    DEBUGOUT(_LtDebug(__FILE__, w,
		 "_XmVendorExtRealize[flags %d, functions %d, decorations %d, "
		      "input_mode %d, status %d\n",
		      VSEP_MwmHints(w).flags,
		      VSEP_MwmHints(w).functions,
		      VSEP_MwmHints(w).decorations,
		      VSEP_MwmHints(w).input_mode,
		      VSEP_MwmHints(w).status));

    /*
     * Note all the fields must be in the right order here !
     * flags, functions, decorations, input_mode, status
     * The XChangeProperty will set them all in one go.
     * note the 32 format for HINTS, the 8 format for MENU
     */
    /* format 32 = long on client side */
    prop32[0]=VSEP_MwmHints(w).flags;
    prop32[1]=VSEP_MwmHints(w).functions;
    prop32[2]=VSEP_MwmHints(w).decorations;
    prop32[3]=VSEP_MwmHints(w).input_mode;
    prop32[4]=VSEP_MwmHints(w).status;
    at = XmInternAtom(XtDisplay(par), _XA_MWM_HINTS, False);
    XChangeProperty(XtDisplay(par), XtWindow(par), at, at, 32,
		    PropModeReplace,
		    (unsigned char *)prop32,
		    PROP_MOTIF_WM_HINTS_ELEMENTS);

    if (VSEP_MwmMenu(w)) {
	/* Only do this if we have a reason for it.
	 * The Motif 2.1 mwm seems to crash without this. */
	at = XmInternAtom(XtDisplay(par), _XA_MWM_MENU, False);
	XChangeProperty(XtDisplay(par), XtWindow(par), at, at, 8,
		    PropModeReplace,
		    (unsigned char *)VSEP_MwmMenu(w), strlen(VSEP_MwmMenu(w)));
    }

    for (i = 0; i < par->core.num_popups; i++)
    {
	if (XtIsTransientShell(par->core.popup_list[i]))
	{
	    Arg args[2];
	    int argc = 0;

	    XtSetArg(args[argc], XmNtransientFor, par);
	    argc++;
	    XtSetArg(args[argc], XmNwindowGroup, XtWindow(par));
	    argc++;
	    XtSetValues(par->core.popup_list[i], args, argc);
	    if (XtIsRealized(par->core.popup_list[i]))
	    {
		XSetTransientForHint(XtDisplay(par->core.popup_list[i]),
				     XtWindow(par->core.popup_list[i]),
				     XtWindow(par));
	    }
	}
    }

    /*
     * In case this is something like a top level shell and not a real
     * pop up shell we have to do a explicit grab here or we do lose the
     * ability to get user input whenever a non-modal dialog shows up.
     */
    if (!LTIsARealPopupShell(par))
    {
	LTAddGrab(NULL, False, False,
		  (XmVendorShellExtObject)w, (XmVendorShellExtObject)w);
    }
}


static Cardinal
get_sec_res_data(WidgetClass wc, XmSecondaryResourceData **data)
{
    /* FIX ME */

    return _XmSecondaryResourceData(&_XmVendorSCoreClassExtRec,
				    data, NULL, NULL, NULL, NULL);
}


extern Display *
_XmGetDefaultDisplay(void)
{
    return default_display;
}


extern unsigned char
_XmGetAudibleWarning(Widget w)
{
    XmWidgetExtData data;
    DEBUGOUT(_LtDebug(__FILE__, w, "_XmGetAudibleWarning\n"));
    if ( XmIsVendorShell (w) && ( data = _XmGetWidgetExtData(w, XmSHELL_EXTENSION ) ) )
	{
	XmVendorShellExtObject vSE = (XmVendorShellExtObject)data -> widget ;
	unsigned char audibleWarning = vSE -> vendor.audible_warning ;
	return audibleWarning ;
	}
    
    _XmWarning(w, "_XmGetAudibleWarning: widget has invalid class");
    return False;
}


extern char *
_XmGetIconPixmapName(void)
{
    return NULL;
}


extern void
_XmClearIconPixmapName(void)
{
}


/* To be used elsewhere ... */
extern Widget
_LtFindVendorExt(Widget w)
{
    Widget p;
    XmWidgetExtData data;

    if (w == (Widget)0)
    {
	return NULL;
    }

    for (p = w;
	 !XtIsSubclass(p, vendorShellWidgetClass) && XtParent(p);
	 p = XtParent(p))
    {
    }

    data = _XmGetWidgetExtData(p, XmSHELL_EXTENSION);
    if (data)
    {
	return data->widget;
    }
    else
    {
	return NULL;
    }
}


/*
  From: Martin Simmons:
    Perhaps LessTif can guard against this common linking problem?  
    E.g. the following function will check the VendorShell for consistency:

     returns     True if successful, i.e. everything's fine
     returns     False if no widget was supplied as an argument
     _XmError()s if linkage is/was wrong
 */
extern Boolean
_LtCheckClassOfVendorShell(Widget w)
{
    WidgetClass widget_class;
    
    if (!w)
       return False;
    for (widget_class = XtClass(w); widget_class != NULL; widget_class = widget_class->core_class.superclass)
    {
        if (0 == strcmp(widget_class->core_class.class_name, "VendorShell")) {
            if (widget_class->core_class.extension == (XtPointer)&_XmVendorSCoreClassExtRec)
                return True;
            else
                _XmError(w, "Application not linked correctly: try putting -lXm before -lXt.");
        }
    }

    return False;
}


/*
   This corrects for the (shared) library loading mechanism in OS/2 and Windows
   which differs from those on many standard Unix systems.
   The routine should be called before any other function,
   actually done by the _DLL_InitTerm-function in OS/2 and DllMain in Windows.
   On un*x/ELF systems the problem addressed here seems to be avoided
   by specifying the libraries in correct, canonical order on the linker
   command line.
   amai (20010112): I once decided to make it static, but this was an error:
   to call it from a static libXm one may need this symbol.
 */
#if defined(__EMX__) || defined(__CYGWIN__)
extern void
_LtXmFixupVendorShell(void)
{

    transientShellWidgetClass->core_class.superclass =
       (WidgetClass)&vendorShellClassRec;
    topLevelShellWidgetClass->core_class.superclass =
       (WidgetClass)&vendorShellClassRec;
}


#ifdef __EMX__
unsigned long
_DLL_InitTerm(unsigned long mod_handle, unsigned long flag)
{
    if (flag == 0UL)  /* initialize DLL */
    {
	    _LtXmFixupVendorShell();
	    return 1; /* success */
    }
    else     /* terminate DLL */
    {
        return 1; /* success */
    }
}
#endif

#ifdef __CYGWIN__
int __stdcall
DllMain(unsigned long mod_handle, unsigned long flag, void *routine)
{
    switch (flag)
    {
        case 1: /* DLL_PROCESS_ATTACH - process attach */
            _LtXmFixupVendorShell();
            break;
        case 0: /* DLL_PROCESS_DETACH - process detach */
            break;
    }
    return 1;
}
#endif

#endif /* __EMX__ || __CYGWIN__ */

static XmRenderTable GetRenderTable(Widget w, XtEnum renderTableType)
{
	XmWidgetExtData data;

	if (XmIsVendorShell(w) && (data = _XmGetWidgetExtData(w, XmSHELL_EXTENSION))) {
		XmVendorShellExtObject ve = (XmVendorShellExtObject)data->widget ;

		switch(renderTableType) {
		case XmLABEL_RENDER_TABLE:
			return ve->vendor.label_font_list;
		case XmBUTTON_RENDER_TABLE:
			return ve->vendor.button_font_list;
		case XmTEXT_RENDER_TABLE:
			return ve->vendor.text_font_list;
		}
	}
	return NULL;
}
