/**
 *
 * $Header: /cvsroot/lesstif/lesstif/lib/Xm-2.1/Container.c,v 1.1 2004/08/28 19:22:43 dannybackx Exp $
 *
 * Copyright (C) 1997-1999 Free Software Foundation, Inc.
 * Copyright © 1999, 2000, 2001, 2002 LessTif Development Team
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
#include <LTconfig.h>

#include <XmI/XmI.h>
#include <Xm/XmP.h>
#include <Xm/BaseClassP.h>
#include <Xm/Container.h>
#include <Xm/ContainerP.h>
#include <Xm/ContainerT.h>
#include <Xm/ContItemT.h>
#include <Xm/TransltnsP.h>
#include <Xm/PushBG.h>

#include <XmI/DebugUtil.h>

static const char rcsid[] = "$Header: /cvsroot/lesstif/lesstif/lib/Xm-2.1/Container.c,v 1.1 2004/08/28 19:22:43 dannybackx Exp $";

/*
 * Forward Declarations
 */

static void class_initialize(void);
static void class_part_initialize(WidgetClass widget_class);
static void Initialize(Widget, Widget, ArgList, Cardinal *);
static void resize(Widget w);
static Boolean SetValues(Widget, Widget, Widget, ArgList, Cardinal *);
static void expose(Widget w, XEvent *event, Region region);
static XtGeometryResult QueryGeometry(Widget, XtWidgetGeometry *, XtWidgetGeometry *);
static void _XmContainerLayout(Widget w, Dimension *pwid, Dimension *pht);
static void _XmContainerLayoutOL(Widget w, CwidNode n, Position x0, Position y0,
			Dimension *pwid, Dimension *pht, int lvl);
static void Destroy(Widget);

/* composite */
static XtGeometryResult GeometryManager(Widget, XtWidgetGeometry *, XtWidgetGeometry *);
static void ChangeManaged(Widget w);

/* constraint */
static void ConstraintInitialize(Widget request, Widget new_w, ArgList args, Cardinal *num_args);
static void ConstraintDestroy(Widget w);
static Boolean ConstraintSetValues(Widget, Widget, Widget, ArgList, Cardinal *);

/*
 * Actions
 */
static void ContainerActivate(Widget, XEvent *, String *, Cardinal *);
static void ContainerBeginExtend(Widget, XEvent *, String *, Cardinal *);
static void ContainerBeginSelect(Widget, XEvent *, String *, Cardinal *);
static void ContainerBeginToggle(Widget, XEvent *, String *, Cardinal *);
static void ContainerButtonMotion(Widget, XEvent *, String *, Cardinal *);
static void ContainerCancel(Widget, XEvent *, String *, Cardinal *);
static void ContainerDeselectAll(Widget, XEvent *, String *, Cardinal *);
static void ContainerEndExtend(Widget, XEvent *, String *, Cardinal *);
static void ContainerEndSelect(Widget, XEvent *, String *, Cardinal *);
static void ContainerEndToggle(Widget, XEvent *, String *, Cardinal *);
static void ContainerEndTransfer(Widget, XEvent *, String *, Cardinal *);
static void ContainerExpandOrCollapse(Widget, XEvent *, String *, Cardinal *);
static void ContainerExtend(Widget, XEvent *, String *, Cardinal *);
static void ContainerExtendCursor(Widget, XEvent *, String *, Cardinal *);
static void ContainerHandleBtn1Down(Widget, XEvent *, String *, Cardinal *);
static void ContainerHandleBtn1Motion(Widget, XEvent *, String *, Cardinal *);
static void ContainerHandleBtn1Up(Widget, XEvent *, String *, Cardinal *);
static void ContainerHandleBtn2Down(Widget, XEvent *, String *, Cardinal *);
static void ContainerHandleBtn2Motion(Widget, XEvent *, String *, Cardinal *);
static void ContainerHandleBtn2Up(Widget, XEvent *, String *, Cardinal *);
static void ContainerMoveCursor(Widget, XEvent *, String *, Cardinal *);
static void ContainerPrimaryCopy(Widget, XEvent *, String *, Cardinal *);
static void ContainerPrimaryLink(Widget, XEvent *, String *, Cardinal *);
static void ContainerPrimaryMove(Widget, XEvent *, String *, Cardinal *);
static void ContainerSelect(Widget, XEvent *, String *, Cardinal *);
static void ContainerSelectAll(Widget, XEvent *, String *, Cardinal *);
static void ContainerStartTransfer(Widget, XEvent *, String *, Cardinal *);
static void ContainerToggleMode(Widget, XEvent *, String *, Cardinal *);

/*
 * Trait
 */
static void _ContainerTrait_GetValues(Widget container, XmContainerData d);

static XmContainerTraitRec _ContainerTraitRec = {
	/* version */		0,
	/* getvalues */		_ContainerTrait_GetValues
};

/*
 * Bitmaps, "stolen" from XltListTree
 */
#define folder_width 16
#define folder_height 12
static unsigned char folder_bits[] =
{
  0x00, 0x1f, 0x80, 0x20, 0x7c, 0x5f, 0x02, 0x40, 0x02, 0x40, 0x02, 0x40,
  0x02, 0x40, 0x02, 0x40, 0x02, 0x40, 0x02, 0x40, 0x02, 0x40, 0xfc, 0x3f,
};

#define folderopen_width 16
#define folderopen_height 12
static unsigned char folderopen_bits[] =
{
  0x00, 0x3e, 0x00, 0x41, 0xf8, 0xd5, 0xac, 0xaa, 0x54, 0xd5, 0xfe, 0xaf,
  0x01, 0xd0, 0x02, 0xa0, 0x02, 0xe0, 0x04, 0xc0, 0x04, 0xc0, 0xf8, 0x7f,
};

enum {
	_XmUnknown,
	_XmIconButton,
	_XmChild
};

/*
 * resources for the Container class
 */
#define Offset(field) XtOffsetOf(XmContainerRec, container.field)
static XtResource resources[] =
{
    {
	XmNmarginHeight, XmCMarginHeight, XmRVerticalDimension,
	sizeof(Dimension), Offset(margin_h),
	XmRImmediate, (XtPointer)0
    },
    {
	XmNmarginWidth, XmCMarginWidth, XmRHorizontalDimension,
	sizeof(Dimension), Offset(margin_w),
	XmRImmediate, (XtPointer)0
    },
    {
	XmNautomaticSelection, XmCAutomaticSelection, XmRChar,
	sizeof(unsigned char), Offset(automatic),
	XmRImmediate, (XtPointer)XmAUTO_SELECT
    },
    {
	XmNcollapsedStatePixmap, XmCCollapsedStatePixmap, XmRPixmap,
	sizeof(Pixmap), Offset(collapsed_state_pixmap),
	XmRImmediate, (XtPointer)XmUNSPECIFIED_PIXMAP	/* FIXME default should be dynamic */
    },
    {
	XmNconvertCallback, XmCCallback, XmRCallback,
	sizeof(XtCallbackList), Offset(convert_cb),
	XmRImmediate, (XtPointer)0
    },
    {
	XmNdefaultActionCallback, XmCCallback, XmRCallback,
	sizeof(XtCallbackList), Offset(default_action_cb),
	XmRImmediate, (XtPointer)0
    },
    {
	XmNdestinationCallback, XmCCallback, XmRCallback,
	sizeof(XtCallbackList), Offset(destination_cb),
	XmRImmediate, (XtPointer)0
    },
    {
	XmNdetailColumnHeading, XmCDetailColumnHeading, XmRXmStringTable,
	sizeof(XmStringTable), Offset(detail_heading),
	XmRImmediate, (XtPointer)0
    },
    {
	XmNdetailColumnHeadingCount, XmCDetailColumnHeadingCount, XmRCardinal,
	sizeof(Cardinal), Offset(detail_heading_count),
	XmRImmediate, (XtPointer)0
    },
    {
	XmNdetailOrder, XmCDetailOrder, XmRCardinalList,
	sizeof(Cardinal *), Offset(detail_order),
	XmRImmediate, (XtPointer)0
    },
    {
	XmNdetailOrderCount, XmCDetailOrderCount, XmRCardinal,
	sizeof(Cardinal), Offset(detail_order_count),
	XmRImmediate, (XtPointer)0	/* FIXME should be dynamic */
    },
    {
	XmNdetailTabList, XmCDetailTabList, XmRTabList,
	sizeof(XmTabList), Offset(detail_tablist),
	XmRImmediate, (XtPointer)0
    },
    {
	XmNentryViewType, XmCEntryViewType, XmRChar,
	sizeof(unsigned char), Offset(entry_viewtype),
	XmRImmediate, (XtPointer)XmANY_ICON
    },
    {
	XmNexpandedStatePixmap, XmCExpandedStatePixmap, XmRPixmap,
	sizeof(Pixmap), Offset(expanded_state_pixmap),
	XmRImmediate, (XtPointer)XmUNSPECIFIED_PIXMAP	/* FIXME should be dynamic */
    },
#if 0
    {
	XmNfontList, XmCFontList, XmRFontList,
	sizeof(XmFontList), Offset(fontlist),
	XmRImmediate, (XtPointer)0
    },
#endif
    {
	XmNlargeCellHeight, XmCCellHeight, XmRDimension,
	sizeof(Dimension), Offset(large_cell_height),
	XmRImmediate, (XtPointer)0	/* FIXME should be dynamic */
    },
    {
	XmNlargeCellWidth, XmCCellWidth, XmRDimension,
	sizeof(Dimension), Offset(large_cell_width),
	XmRImmediate, (XtPointer)0	/* FIXME should be dynamic */
    },
    {
	XmNlayoutType, XmCLayoutType, XmRChar,
	sizeof(unsigned char), Offset(layout_type),
	XmRImmediate, (XtPointer)XmSPATIAL
    },
    {
	XmNoutlineButtonPolicy, XmCOutlineButtonPolicy, XmRChar,
	sizeof(unsigned char), Offset(ob_policy),
	XmRImmediate, (XtPointer)XmOUTLINE_BUTTON_PRESENT
    },
    {
	XmNoutlineChangedCallback, XmCCallback, XmRCallback,
	sizeof(XtCallbackList), Offset(outline_cb),
	XmRImmediate, (XtPointer)0
    },
    {
	XmNoutlineColumnWidth, XmCOutlineColumnWidth, XmRDimension,
	sizeof(Dimension), Offset(ob_width),
	XmRImmediate, (XtPointer)0	/* FIXME should be dynamic */
    },
    {
	XmNoutlineIndentation, XmCOutlineIndentation, XmRDimension,
	sizeof(Dimension), Offset(outline_indent),
	XmRImmediate, (XtPointer)40
    },
    {
	XmNoutlineLineStyle, XmCLineStyle, XmRChar,
	sizeof(unsigned char), Offset(outline_sep_style),
	XmRImmediate, (XtPointer)XmSINGLE
    },
    {
	XmNprimaryOwnership, XmCPrimaryOwnership, XmRChar,
	sizeof(unsigned char), Offset(primary_ownership),
	XmRImmediate, (XtPointer)XmOWN_POSSIBLE_MULTIPLE
    },
    {
	XmNrenderTable, XmCRenderTable, XmRRenderTable,
	sizeof(XmRenderTable), Offset(render_table),
	XmRImmediate, (XtPointer)0	/* FIXME should be dynamic */
    },
    {
	XmNselectColor, XmCSelectColor, XmRPixel,
	sizeof(Pixel), Offset(select_color),
	XmRImmediate, (XtPointer)0	/* FIXME should be dynamic */
    },
    {
	XmNselectedObjects, XmCSelectedObjects, XmRWidgetList,
	sizeof(WidgetList), Offset(selected_items),
	XmRImmediate, (XtPointer)0
    },
    {
	XmNselectedObjectCount, XmCSelectedObjectCount, XmRInt,
	sizeof(unsigned int), Offset(selected_item_count),
	XmRImmediate, (XtPointer)0
    },
    {
	XmNselectionCallback, XmCCallback, XmRCallback,
	sizeof(XtCallbackList), Offset(selection_cb),
	XmRImmediate, (XtPointer)0
    },
    {
	XmNselectionPolicy, XmCSelectionPolicy, XmRChar,
	sizeof(unsigned char), Offset(selection_policy),
	XmRImmediate, (XtPointer)XmEXTENDED_SELECT
    },
    {
	XmNselectionTechnique, XmCSelectionTechnique, XmRChar,
	sizeof(unsigned char), Offset(selection_technique),
	XmRImmediate, (XtPointer)XmTOUCH_OVER
    },
    {
	XmNsmallCellHeight, XmCCellHeight, XmRDimension,
	sizeof(Dimension), Offset(small_cell_height),
	XmRImmediate, (XtPointer)0	/* FIXME should be dynamic */
    },
    {
	XmNsmallCellWidth, XmCCellWidth, XmRDimension,
	sizeof(Dimension), Offset(small_cell_width),
	XmRImmediate, (XtPointer)0	/* FIXME should be dynamic */
    },
    {
	XmNspatialIncludeModel, XmCSpatialIncludeModel, XmRChar,
	sizeof(unsigned char), Offset(include_model),
	XmRImmediate, (XtPointer)XmAPPEND
    },
    {
	XmNspatialResizeModel, XmCSpatialResizeModel, XmRChar,
	sizeof(unsigned char), Offset(resize_model),
	XmRImmediate, (XtPointer)XmGROW_MINOR
    },
    {
	XmNspatialSnapModel, XmCSpatialSnapModel, XmRChar,
	sizeof(unsigned char), Offset(snap_model),
	XmRImmediate, (XtPointer)XmNONE
    },
    {
	XmNspatialStyle, XmCSpatialStyle, XmRChar,
	sizeof(unsigned char), Offset(spatial_style),
	XmRImmediate, (XtPointer)XmGRID
    },
#undef	Offset
};

#define	Offset(x) (XtOffsetOf(XmContainerConstraintRec, container.x))
static XtResource containerConstraintResources [] = {
	{
		XmNentryParent, XmCWidget, XmRWidget,
		sizeof(Widget), Offset(entry_parent),
		XmRImmediate, (XtPointer)0
	},
	{
		XmNoutlineState, XmCOutlineState, XmRUnsignedChar,
		sizeof(unsigned char), Offset(outline_state),
		XmRImmediate, XmCOLLAPSED
	},
	{
		XmNpositionIndex, XmCPositionIndex, XmRInt,
		sizeof(int), Offset(position_index),
		XmRImmediate, (XtPointer)0	/* FIX ME init in ConstraintInitialize ? */
	},
};
#undef Offset

static XtActionsRec actions[] = {
	{ "ContainerActivate",		ContainerActivate },
	{ "ContainerBeginExtend",	ContainerBeginExtend },
	{ "ContainerBeginSelect",	ContainerBeginSelect },
	{ "ContainerBeginToggle",	ContainerBeginToggle },
	{ "ContainerButtonMotion",	ContainerButtonMotion },
	{ "ContainerCancel",		ContainerCancel },
	{ "ContainerDeselectAll",	ContainerDeselectAll },
	{ "ContainerEndExtend",		ContainerEndExtend },
	{ "ContainerEndSelect",		ContainerEndSelect },
	{ "ContainerEndToggle",		ContainerEndToggle },
	{ "ContainerEndTransfer",	ContainerEndTransfer },
	{ "ContainerExpandOrCollapse",	ContainerExpandOrCollapse },
	{ "ContainerExtend",		ContainerExtend },
	{ "ContainerExtendCursor",	ContainerExtendCursor },
	{ "ContainerHandleBtn1Down",	ContainerHandleBtn1Down },
	{ "ContainerHandleBtn1Motion",	ContainerHandleBtn1Motion },
	{ "ContainerHandleBtn1Up",	ContainerHandleBtn1Up },
	{ "ContainerHandleBtn2Down",	ContainerHandleBtn2Down },
	{ "ContainerHandleBtn2Motion",	ContainerHandleBtn2Motion },
	{ "ContainerHandleBtn2Up",	ContainerHandleBtn2Up },
	{ "ContainerMoveCursor",	ContainerMoveCursor },
	{ "ContainerPrimaryCopy",	ContainerPrimaryCopy },
	{ "ContainerPrimaryLink",	ContainerPrimaryLink },
	{ "ContainerPrimaryMove",	ContainerPrimaryMove },
	{ "ContainerSelect",		ContainerSelect },
	{ "ContainerSelectAll",		ContainerSelectAll },
	{ "ContainerStartTransfer",	ContainerStartTransfer },
	{ "ContainerToggleMode",	ContainerToggleMode },
};

XmContainerClassRec xmContainerClassRec = {
    /* Core class part */
    {
	/* superclass            */ (WidgetClass) &xmManagerClassRec,
        /* class_name            */ "XmContainer",
	/* widget_size           */ sizeof(XmContainerRec),
	/* class_initialize      */ class_initialize,
	/* class_part_initialize */ class_part_initialize,
	/* class_inited          */ False,
	/* initialize            */ Initialize,
	/* initialize_hook       */ NULL,
	/* realize               */ XtInheritRealize,
	/* actions               */ actions,
	/* num_actions           */ XtNumber(actions),
	/* resources             */ resources,
	/* num_resources         */ XtNumber(resources),
	/* xrm_class             */ NULLQUARK,
	/* compress_motion       */ True,
	/* compress_exposure     */ XtExposeCompressMaximal,
	/* compress_enterleave   */ True,
	/* visible_interest      */ False,
	/* destroy               */ Destroy,
	/* resize                */ resize,
	/* expose                */ expose,
	/* set_values            */ SetValues,
	/* set_values_hook       */ NULL,
	/* set_values_almost     */ XtInheritSetValuesAlmost,
	/* get_values_hook       */ NULL,
	/* accept_focus          */ NULL,
	/* version               */ XtVersion,
	/* callback offsets      */ NULL,
	/* tm_table              */ _XmContainer_defaultTranslations,
	/* query_geometry        */ QueryGeometry,
	/* display_accelerator   */ NULL,
	/* extension             */ (XtPointer)NULL /*&_XmContainerCoreClassExtRec*/
    },
    /* Composite class part */
    {
	/* geometry manager */ GeometryManager, 
        /* change_managed   */ ChangeManaged, 
        /* insert_child     */ XtInheritInsertChild,
        /* delete_child     */ XtInheritDeleteChild,
        /* extension        */ (XtPointer)NULL
    },
    /* Constraint class part */
    {
	/* subresources      */ containerConstraintResources,
        /* subresource_count */ XtNumber(containerConstraintResources), 
        /* constraint_size   */ sizeof(XmContainerConstraintRec),
        /* initialize        */ ConstraintInitialize,
        /* destroy           */ ConstraintDestroy,
        /* set_values        */ ConstraintSetValues,
        /* extension         */ NULL,  
    },
    /* XmManager class part */
    {
	/* translations                 */	XtInheritTranslations,
        /* syn_resources                */	NULL,
        /* num_syn_resources            */	0,
        /* syn_constraint_resources     */	NULL,
        /* num_syn_constraint_resources */	0,
        /* parent_process               */	XmInheritParentProcess,
	/* extension                    */	(XtPointer)NULL
    },
    /* XmContainer Area part */
    {
	/* extension */				NULL,
    },
};

WidgetClass xmContainerWidgetClass = (WidgetClass)&xmContainerClassRec;

Widget XmCreateContainer(Widget parent, String name, ArgList arglist, Cardinal argcount)
{
	return XtCreateWidget(name, xmContainerWidgetClass, parent, arglist, argcount);
}

static void class_part_initialize(WidgetClass widget_class)
{
	/* FIX ME: Needs to be implemented */
	DEBUGOUT(_LtDebug(__FILE__, NULL, "XmContainer class_part_initialize\n"));
}

static XtTranslations container_traversal_trans;

static void class_initialize(void)
{
	container_traversal_trans = XtParseTranslationTable(_XmContainer_traversalTranslations);

	if (! XmeTraitSet((XtPointer)xmContainerWidgetClass, XmQTcontainer,
			(XtPointer) &_ContainerTraitRec)) {
		_XmWarning(NULL, "XmContainer ClassInitialize: XmeTraitSet failed\n");
	}
}

static void Initialize(Widget request, Widget new_w, ArgList args, Cardinal *num_args)
{
	Dimension	wid, ht;
	XGCValues	values;
	XtGCMask	mask;

	/* FIX ME: Needs to be implemented */

	DEBUGOUT(_LtDebug(__FILE__, new_w, "XmContainer initialize\n"));

	XtAugmentTranslations(new_w, container_traversal_trans);

	ContainerFirstNode(new_w) = 0;
	ContainerLastNode(new_w) = 0;
	ContainerIconHeader(new_w) = 0;
	ContainerMarqueeMode(new_w) = False;
	ContainerSelectedItemCount(new_w) = 0;
	ContainerSelectedItems(new_w) = NULL;
	ContainerAnchorCWid(new_w) = NULL;
	ContainerDruggee(new_w) = NULL;
	ContainerSizeOb(new_w) = NULL;
	ContainerDragContext(new_w) = NULL;
	ContainerDetailOrder(new_w) = NULL;
	ContainerDetailOrderCount(new_w) = 0;
	ContainerDetailHeading(new_w) = NULL;
	ContainerOutlineSegs(new_w) = NULL;
	ContainerDragOffsetX(new_w) = 0;
	ContainerDragOffsetY(new_w) = 0;
	ContainerSavedDetailHeadingCount(new_w) = 0;

	ContainerSmallCellDimFixed(new_w) = False;
	ContainerLargeCellDimFixed(new_w) = False;

	_XmContainerLayout(new_w, &wid, &ht);

	XtWidth(new_w) = (wid < 100) ? 100 : wid;
	XtHeight(new_w) = (ht < 100) ? 100 : ht;

	if (ContainerCollapsedStatePixmap(new_w) == XmUNSPECIFIED_PIXMAP) {
		ContainerCollapsedStatePixmap(new_w) = XCreateBitmapFromData(
			XtDisplay(new_w), RootWindowOfScreen(XtScreen(new_w)),
			(char *)folder_bits, folder_width, folder_height);
	}
	if (ContainerExpandedStatePixmap(new_w) == XmUNSPECIFIED_PIXMAP) {
		ContainerExpandedStatePixmap(new_w) = XCreateBitmapFromData(
			XtDisplay(new_w), RootWindowOfScreen(XtScreen(new_w)),
			(char *)folderopen_bits, folderopen_width, folderopen_height);
	}

	ContainerMarqueeGC(new_w) = NULL;	/* FIX ME */
	mask = GCForeground;
	values.foreground = MGR_Foreground(new_w);
	ContainerNormalGC(new_w) = XtGetGC(new_w, mask, &values);

	/*
	 * XmNselectColor
	 *	Specifies a Pixel that can be accessed by children of the Container
	 *	and used to indicate that the child is in a selected state.
	 *	In addition to a Pixel value, the following symbolic values can be
	 *	specified:
	 *		XmDEFAULT_SELECT_COLOR
	 *			Specifies a color between the background and the bottom
	 *			shadow color. 
	 *		XmREVERSED_GROUND_COLORS
	 *			Forces the select color to the foreground color and
	 *			causes the default color of any text rendered over the
	 *			select color to be the background color.
	 *		HIGHLIGHT_COLOR
	 *			Forces the fill color to use the highlight color.
	 */
	switch (ContainerSelectColor(new_w)) {
	case XmDEFAULT_SELECT_COLOR:
		break;
	case XmREVERSED_GROUND_COLORS:
		break;
	case XmHIGHLIGHT_COLOR:
		break;
	default:
		/* Don't change it */
		break;
	}
}

static void resize(Widget w)
{
	DEBUGOUT(_LtDebug(__FILE__, w, "XmContainer resize (%d x %d)\n",
		XtWidth(w), XtHeight(w)));
	_XmContainerLayout(w, NULL, NULL);
}

static Boolean
SetValues(Widget current, Widget request, Widget new_w, ArgList args, Cardinal *num_args)
{
	/* FIX ME: Needs to be implemented */
#define	NE(r)	(r(request) != r(new_w))

	DEBUGOUT(_LtDebug(__FILE__, new_w, "XmContainer SetValues\n"));
	DEBUGOUT(_LtDebugPrintArgList(__FILE__, new_w, args, *num_args, False));

	if (NE(ContainerEntryViewType)) {
		/* Must pass this to children using the ContainerItem trait */

		XmContainerItemTrait	tr;
		int			i;
		Widget			child;
		XmContainerItemDataRec	data;

		data.valueMask = ContItemViewType;
		data.view_type = ContainerEntryViewType(new_w);

		for (i=0; i<MGR_NumChildren(new_w); i++) {
			child = MGR_Children(new_w)[i];
			if ((tr = (XmContainerItemTrait)XmeTraitGet((XtPointer)XtClass(child),
								XmQTcontainerItem))) {
				tr->setValues(child, &data);
			} else {
				continue;	/* Shouldn't happen */
			}
		}
	}

	return True;
#undef	NE
}

static void expose(Widget w, XEvent *event, Region region)
{
	if (! XtIsRealized(w))
		return;

	DEBUGOUT(_LtDebug(__FILE__, w, "XmContainer expose\n"));
	/* FIX ME: Needs to be implemented */

	_XmRedisplayGadgets(w, event, region);

	if (ContainerLayoutType(w) == XmOUTLINE) { /* Draw the lines */
		int	i;

		for (i=0; i<MGR_NumChildren(w); i++) {
			Widget 		c, last, child = MGR_Children(w)[i];
			CwidNode	cn, n;
			Position	x0, y0, my;

			if (child->core.being_destroyed)
				continue;
			if (CC_OutlineState(child) == XmCOLLAPSED)
				continue;
			if ((cn = CC_Child(child)) == NULL)
				continue;

			/* Find the non-icon child with the highest Y */
			my=-1; last = NULL;
			for (n=cn; n; n=n->next_ptr) {
				c = n->widget_ptr;

				if (CC_CWidType(c) == _XmIconButton)
					continue;
				else if (XtY(c) > my) {
					my = XtY(c);
					last = c;
				}
			}
			if (! last)
				last = cn->widget_ptr;

			/* From the related widget - this is the button */
			c = CC_Related(child);
			x0 = XtX(c) + XtWidth(c) / 2;
			y0 = XtY(c) + XtHeight(c);

			my = XtY(last) + XtHeight(last) / 2;

			/* Vertical line */
			XDrawLine(XtDisplay(w), XtWindow(w),
				ContainerNormalGC(w),
				x0, y0, x0, my);
			/*
			 * Horizontal lines
			 * Draw these only to the folder button if a child has children of its own.
			 */
			for (n=cn; n; n=n->next_ptr) {
				Position	x, y;

				c = n->widget_ptr;

				if (CC_CWidType(c) == _XmIconButton)
					continue;

				y = XtY(c) + XtHeight(c) / 2;

				if (n->child_ptr && CC_Related(c)) {
					Widget	r = CC_Related(c);
					x = XtX(r);
				} else {
					x = XtX(c);
				}

				XDrawLine(XtDisplay(w), XtWindow(w), ContainerNormalGC(w),
					x0, y, x, y);
			}
			
		}
	}
}

/*
 * Utilities to handle the selection list.
 */
static void ClearSelectedItems(Widget w)
{
	XmContainerItemTrait	t;
	int			i;
	XmContainerItemDataRec	r;

	for (i=0; i<ContainerSelectedItemCount(w); i++) {
		Widget child = ContainerSelectedItems(w)[i];

		/* Call the trait to change the appearance */
		t = (XmContainerItemTrait) XmeTraitGet((XtPointer)XtClass(child),
			XmQTcontainerItem);
		if (t) {
			r.valueMask = ContItemVisualEmphasis;
			r.visual_emphasis = XmNOT_SELECTED;
			t->setValues(child, &r);
		}
	}

	if (ContainerSelectedItems(w))
		XtFree((char *)ContainerSelectedItems(w));
	ContainerSelectedItems(w) = NULL;
	ContainerSelectedItemCount(w) = 0;
}

static void AddSelectedItem(Widget w, Widget si)
{
	int			i;
	XmContainerItemTrait	t;

	if (! si)
		return;

	for (i=0; i<ContainerSelectedItemCount(w); i++)
		if (ContainerSelectedItems(w)[i] == si)
			return;

	if (ContainerSelectedItems(w)) {
		ContainerSelectedItems(w) = (WidgetList)XtRealloc(
			(char *)ContainerSelectedItems(w),
			(ContainerSelectedItemCount(w)+2) * sizeof(Widget));
	} else {
		ContainerSelectedItems(w) = (WidgetList)XtMalloc(2 * sizeof(Widget));
	}
	ContainerSelectedItems(w)[ContainerSelectedItemCount(w)] = si;
	ContainerSelectedItemCount(w)++;
	ContainerSelectedItems(w)[ContainerSelectedItemCount(w)] = NULL;

	/* Call the trait to change the appearance */
	t = (XmContainerItemTrait) XmeTraitGet((XtPointer)XtClass(si),
		XmQTcontainerItem);
	if (t) {
		XmContainerItemDataRec	r;

		r.valueMask = ContItemVisualEmphasis;
		r.visual_emphasis = XmSELECTED;
		t->setValues(si, &r);
	}
}

static Boolean ItemIsSelected(Widget w, Widget child)
{
	int	i;

	for (i=0; i<ContainerSelectedItemCount(w); i++)
		if (ContainerSelectedItems(w)[i] == child)
			return True;
	return False;
}

static void RemoveSelectedItem(Widget w, Widget child)
{
	int	i, j;
	XmContainerItemTrait	t;

	for (j=-1, i=0; i<ContainerSelectedItemCount(w); i++)
		if (ContainerSelectedItems(w)[i] == child)
			j = i;
	if (j < 0)
		return;	/* FIX ME error message ? */
	for (i=j; i<ContainerSelectedItemCount(w)-1; i++)
		ContainerSelectedItems(w)[i] = ContainerSelectedItems(w)[i+1];
	if (ContainerSelectedItemCount(w) > 0)
		ContainerSelectedItemCount(w)--;
	
	/* Don't bother making the structure smaller */
	/* Call the trait to change the appearance */
	t = (XmContainerItemTrait) XmeTraitGet((XtPointer)XtClass(child),
		XmQTcontainerItem);
	if (t) {
		XmContainerItemDataRec	r;

		r.valueMask = ContItemVisualEmphasis;
		r.visual_emphasis = XmNOT_SELECTED;
		t->setValues(child, &r);
	}
	
}

static void MarqueeStart(Widget w, XEvent *evp)
{
	/* FIX ME need to check evp->type first */
	ContainerMarqueeStart(w).x = evp->xmotion.x;
	ContainerMarqueeStart(w).y = evp->xmotion.y;
}

static void SelectItemRange(Widget w, Widget a, Widget b)
{
	/* FIX ME how do you determine the range ?
	 * Assuming it is in child order,  but this is almost certainly wrong.
	 */
	int	i, ia=0, ib=0;
	for (i=0; i<MGR_NumChildren(w); i++) {
		if (MGR_Children(w)[i] == a)
			ia = i;
		if (MGR_Children(w)[i] == b)
			ib = i;
	}

	if (ia > ib) {
		i = ia;
		ia = ib;
		ib = i;
	}
	for (i=ia; i<=ib; i++) {
			AddSelectedItem(w, MGR_Children(w)[i]);
	}
}

static void UnselectItemRange(Widget w, Widget a, Widget b)
{
	/* FIX ME how do you determine the range ?
	 * Assuming it is in child order,  but this is almost certainly wrong.
	 */
	int	i, ia=0, ib=0;
	for (i=0; i<MGR_NumChildren(w); i++) {
		if (MGR_Children(w)[i] == a)
			ia = i;
		if (MGR_Children(w)[i] == b)
			ib = i;
	}
	if (ia > ib) {
		i = ia;
		ia = ib;
		ib = i;
	}
	for (i=ia; i<=ib; i++) {
		RemoveSelectedItem(w, MGR_Children(w)[i]);
	}
}

/*
 * For debugging purposes
 */
static void ContainerDebugPrintTree(int lvl, Widget cont, CwidNode node)
{
	int	i;

	if (lvl == 0) {
		for (node = ContainerFirstNode(cont); node; node = node->next_ptr) {
			ContainerDebugPrintTree(lvl+1, cont, node);
		}
		return;
	}
	if (node == 0)
		return;

	for (i=0; i<lvl; i++)
		DEBUGOUT(_LtDebug0(__FILE__, cont, "    "));
	DEBUGOUT(_LtDebug0(__FILE__, cont, "%s\n", XtName(node->widget_ptr)));
	for (node = node->child_ptr; node; node = node->next_ptr)
		ContainerDebugPrintTree(lvl+1, cont, node);
}

/*
 * Recursive function to walk down the tree
 */
static void
_XmContainerLayoutOL(Widget w, CwidNode n, Position x0, Position y0,
			Dimension *pwid, Dimension *pht, int lvl)
{
	Position	oldx = x0,
			mx, my, x, y;
	CwidNode	node;

/* FIX ME */
#define	HMARGIN	8
#define	WMARGIN	4

	if (n == NULL)
		return;

	ContainerDebugPrintTree(0, w, NULL);

	/*
	 * If we have a lvl parameter then we might as well use it to
	 * prevent us from having infinite recursion.
	 */
	if (lvl > MGR_NumChildren(w) + 5) {
		_XmWarning(w, "Recursion problem !!\n");
		return;
	}

	mx = x0;
	my = y0;
	node = n;
	while (node) {
		Widget	nw = node->widget_ptr;

		x0 = oldx;

		DEBUGOUT(_LtDebug2(__FILE__, w, nw,
			"_XmContainerLayoutOL[%d](X0 %d Y0 %d)(next %s, child %s)\n",
			lvl, x0, y0,
			node->next_ptr ? XtName(node->next_ptr->widget_ptr)
					: "none",
			node->child_ptr ? XtName(node->child_ptr->widget_ptr)
					: "none"));

		if (CC_CWidType(nw) == _XmIconButton) {
			node = node->next_ptr;
			continue;
		}

		/* Position the widget and, if relevant, its icon */
		if (node->child_ptr && CC_Related(nw)) {
			Widget other = CC_Related(nw);
			Position	ox, oy;

			ox = x0;
			oy = y0 + HMARGIN + (XtHeight(nw) - XtHeight(other)) / 2;
			x = x0 + WMARGIN + ContainerMarginWidth(w) + XtWidth(other);
			y = y0 + HMARGIN;

			if (ox != XtX(other) || oy != XtY(other)) {
				_XmMoveObject(other, ox, oy);
				DEBUGOUT(_LtDebug2(__FILE__, w, other,
					"Move related child to %d %d\n", ox, oy));
			}
			if (x != XtX(nw) || y != XtY(nw)) {
				_XmMoveObject(nw, x, y);
			}
			mx = x + XtWidth(nw) + XtWidth(other)
				+ 2 * ContainerMarginWidth(w) + 2 * WMARGIN;
			my = y + ContainerMarginHeight(w) + XtHeight(nw) + HMARGIN;
		} else {
			x = x0;
			y = y0 + HMARGIN;
			if (x != XtX(nw) || y != XtY(nw)) {
				_XmMoveObject(nw, x, y);
			}
			
			mx = x + ContainerMarginWidth(w) + WMARGIN + XtWidth(nw);
			my = y + ContainerMarginHeight(w) + HMARGIN + XtHeight(nw);
		}
		DEBUGOUT(_LtDebug2(__FILE__, w, nw,
			"Move child to %d %d\n", x, y));

		if (node->child_ptr) {
			if (CC_OutlineState(nw) == XmEXPANDED) {
				_XmContainerLayoutOL(w, node->child_ptr,
					x + ContainerOutlineIndent(w), my,
					(Dimension *)&x0, (Dimension *)&y0, lvl+1);
				if (x0 > mx)
					mx = x0;
				if (my < y0)
					my = y0;
			} else { /* XmCOLLAPSED */
				/* FIX ME  x0 = mx; */ y0 = my;
			}
		}
		node = node->next_ptr;

		/* X0 is updated at the beginning of the loop. */
		y0 = my;
	}
	x0 = mx;
	if (*pwid < mx)
		*pwid = mx;
	y0 = my;
	if (*pht < my)
		*pht = my;
	DEBUGOUT(_LtDebug(__FILE__, w, "_XmContainerLayoutOL[%d] -> %d %d\n",
		lvl, x0, y0));
}

/*
 * "Outline" Layout
 *
 * Show items in a tree, in PositionIndex order within EntryParent order.
 * Lines are drawn to show parent-child relationship.
 */
static void _XmContainerLayoutOutline(Widget w, Dimension *pwid, Dimension *pht)
{
	Dimension wid, ht;
	int		i;

	wid = ContainerMarginWidth(w);
	ht = 0;

	DEBUGOUT(_LtDebug(__FILE__, w, "_XmContainerLayoutOutline(%d children)\n",
		MGR_NumChildren(w)));

	_XmContainerLayoutOL(w, ContainerFirstNode(w),
		ContainerMarginWidth(w), ContainerMarginHeight(w),
		&wid, &ht, 0);

	/* Determine widget size */
	for (i=0; i<MGR_NumChildren(w); i++) {
		Widget child = MGR_Children(w)[i];
		if (XtIsRealized(child) && XtIsManaged(child)) {
			Dimension	m;
			m = XtX(child) + XtWidth(child) + ContainerMarginWidth(w);
			if (m > wid)
				wid = m;
			m = XtY(child) + XtHeight(child) + ContainerMarginHeight(w);
			if (ht < m)
				ht = m;
		}
	}

	if (pwid)
		*pwid = wid;
	if (pht)
		*pht = ht;
	return;
}

/*
 * FIX ME this is simplistic
 *
 * ContainerLayoutType == XmSPATIAL
 *
 * Layout influenced by
 *	ContainerSpatialStyle (XmCELLS, XmGRID, XmNONE)
 *	ContainerSpatialIncludeModel (XmAPPEND, XmCLOSEST, XmFIRST_FIT)
 *	ContainerSpatialSnapModel (XmCENTER, XmSNAP_TO_GRID, XmNONE)
 *
 * Items with XmNentryParent values are not displayed.
 *
 * XmNspatialStyle
 * XmCELLS	Lays out items within a grid of same-size cells. Each item
 *		occupies as many cells as required to contain the item
 *		dimensions.
 * XmGRID	Lays out items within a grid of same-size cells. Each item
 *		occupies only one cell. Items that are larger than the cell
 *		size may overlap other items.
 * XmNONE	Lays out items according to XmNx and XmNy.
 *
 * XmNspatialSnapModel
 * XmCENTER		Center the items as follows, depending on the value of
 *			XmNentryViewType:
 *	XmLARGE_ICON		The child is centered in the cell horizontally
 *				and baseline-aligned vertically.
 *	XmSMALL_ICON		The child is centered in the cell vertically on its
 *				baseline and aligned with the left or right of the
 *				cell horizontally, depending on the value of
 *				XmNlayoutDirection.
 * XmSNAP_TO_GRID	Position the item at the upper-left or upper-right corner
 *			of the cell(s), depending on the value of
 *			XmNlayoutDirection.
 * XmNONE		Position the item according to the position specified
 *			by XmNx and XmNy. If the position is not within the
 *			coordinates of the cell(s), then position the item at
 *			the upper-left or upper-right corner of the cell(s),
 *			depending on the value of XmNlayoutDirection.
 *
 * XmNspatialIncludeModel
 *	Specifies the layout of an item when the item is managed in the Container
 *	when XmNlayoutType is XmSPATIAL and XmNspatialStyle is XmGRID or XmCELLS.
 * XmAPPEND 	Places the item after the last occupied cell according to
 *		XmNlayoutDirection.
 * XmCLOSEST	Places the item in the free cell closest to the position
 *		specified by XmNx and XmNy.
 * XmFIRST_FIT	Places the item in the first free cell according to
 *		XmNlayoutDirection.
 *
 * XmNspatialResizeModel
 *	Specifies how Container will attempt to grow its dimensions when
 *	XmNlayoutType is XmSPATIAL and XmNspatialStyle is XmGRID or XmCELLS
 *	and there are not enough cells to contain a new Container item.
 * XmGROW_BALANCED	Container will request both width and height growth
 *			from its parent.
 * XmGROW_MAJOR		Container will request growth in its major dimension
 *			from its parent. Container's major dimension is width
 *			when the precedence of XmNlayoutDirection is horizontal,
 *			and height when vertical.
 * XmGROW_MINOR		Container will request growth in its minor dimension from
 *			its parent. Container's minor dimension is height when
 *			the precedence of XmNlayoutDirection is horizontal, and
 *			width when vertical. 
 */
static void _XmContainerLayoutSpatial(Widget w, Dimension *pwid, Dimension *pht)
{
	Dimension	cw=0, ch=0, contwid, contht, incx, incy, rowmaxht=0;
	Cardinal	i;
	Position	x, y;
	int		nrows, ncells, ncols, rowindex;

	DEBUGOUT(_LtDebug(__FILE__, w,
		"XmContainerLayoutSpatial: children %d Init dim %dx%d "
		"SpatialStyle %s IncludeModel %s "
		" SnapModel %s ResizeModel %s\n",
		MGR_NumChildren(w),
		XtWidth(w), XtHeight(w),
		(ContainerSpatialStyle(w) == XmCELLS) ? "XmCELLS" :
		(ContainerSpatialStyle(w) == XmGRID) ? "XmGRID" :
		(ContainerSpatialStyle(w) == XmNONE) ? "XmNONE" :
		"???",
		(ContainerIncludeModel(w) == XmAPPEND) ? "XmAPPEND" :
		(ContainerIncludeModel(w) == XmCLOSEST) ? "XmCLOSEST" :
		(ContainerIncludeModel(w) == XmFIRST_FIT) ? "XmFIRST_FIT" :
		"???",
		(ContainerSnapModel(w) == XmCENTER) ? "XmCENTER" :
		(ContainerSnapModel(w) == XmSNAP_TO_GRID) ? "XmSNAP_TO_GRID" :
		(ContainerSnapModel(w) == XmNONE) ? "XmNONE" :
		"???",
		(ContainerSpatialResizeModel(w) == XmGROW_BALANCED) ? "XmGROW_BALANCED" :
		(ContainerSpatialResizeModel(w) == XmGROW_MAJOR) ? "XmGROW_MAJOR" :
		(ContainerSpatialResizeModel(w) == XmGROW_MINOR) ? "XmGROW_MINOR" :
		"???"));

	/* Figure out cell size */
	if (ContainerSpatialStyle(w) == XmCELLS || ContainerSpatialStyle(w) == XmGRID) {
		if (ContainerSmallCellDimFixed(w) == False
				&& ContainerEntryViewType(w) == XmSMALL_ICON) {
			/* Initialize */
			if (MGR_NumChildren(w)) {
				ContainerSmallCellWidth(w) = XtWidth(MGR_Children(w)[0]);
				ContainerSmallCellHeight(w) = XtHeight(MGR_Children(w)[0]);
			} else {
				ContainerSmallCellWidth(w) =
				ContainerSmallCellHeight(w) = 1;
			}
			for (i=0; i<MGR_NumChildren(w); i++) {
				Widget	child = MGR_Children(w)[i];
				if (! XtIsManaged(child))
					continue;
				if (CC_EntryParent(child))
					continue;
				if (ContainerSpatialStyle(w) == XmCELLS) {
					if (ContainerSmallCellHeight(w) > XtHeight(child))
						ContainerSmallCellHeight(w) = XtHeight(child);
					if (ContainerSmallCellWidth(w) > XtWidth(child))
						ContainerSmallCellWidth(w) = XtWidth(child);
				} else {
					if (ContainerSmallCellHeight(w) < XtHeight(child))
						ContainerSmallCellHeight(w) = XtHeight(child);
					if (ContainerSmallCellWidth(w) < XtWidth(child))
						ContainerSmallCellWidth(w) = XtWidth(child);
				}
			}
			cw = ContainerSmallCellWidth(w);
			ch = ContainerSmallCellHeight(w);
		} else if (ContainerLargeCellDimFixed(w) == False
				&& ContainerEntryViewType(w) == XmLARGE_ICON) {
			/* Initialize */
			if (MGR_NumChildren(w)) {
				ContainerLargeCellWidth(w) = XtWidth(MGR_Children(w)[0]);
				ContainerLargeCellHeight(w) = XtHeight(MGR_Children(w)[0]);
			} else {
				ContainerLargeCellWidth(w) =
				ContainerLargeCellHeight(w) = 1;
			}
			for (i=0; i<MGR_NumChildren(w); i++) {
				Widget	child = MGR_Children(w)[i];

				if (child->core.being_destroyed)
					continue;
				if (! XtIsManaged(child))
					continue;
				if (CC_EntryParent(child))
					continue;
				if (ContainerSpatialStyle(w) == XmCELLS) {
					if (ContainerLargeCellHeight(w) > XtHeight(child))
						ContainerLargeCellHeight(w) = XtHeight(child);
					if (ContainerLargeCellWidth(w) > XtWidth(child))
						ContainerLargeCellWidth(w) = XtWidth(child);
				} else {
					if (ContainerLargeCellHeight(w) < XtHeight(child))
						ContainerLargeCellHeight(w) = XtHeight(child);
					if (ContainerLargeCellWidth(w) < XtWidth(child))
						ContainerLargeCellWidth(w) = XtWidth(child);
				}
			}
			cw = ContainerLargeCellWidth(w);
			ch = ContainerLargeCellHeight(w);
		} else {
			if (ContainerEntryViewType(w) == XmLARGE_ICON) {
				cw = ContainerLargeCellWidth(w);
				ch = ContainerLargeCellHeight(w);
			} else if (ContainerEntryViewType(w) == XmSMALL_ICON) {
				cw = ContainerSmallCellWidth(w);
				ch = ContainerSmallCellHeight(w);
			}
		}
	}

	DEBUGOUT(_LtDebug(__FILE__, w, "XmContainerLayoutSpatial: cell size %d %d\n", cw, ch));
	/*
	 * Figure out how many cells we need
	 */
	for (ncells=0, i=0; i<MGR_NumChildren(w); i++) {
		Widget	child = MGR_Children(w)[i];

		if (child->core.being_destroyed)
			continue;
		if (! XtIsManaged(child))
			continue;
		if (CC_EntryParent(child))
			continue;
		switch (ContainerSpatialStyle(w)) {
		case XmGRID:
			ncells++;
			break;
		case XmCELLS:
			ncells++;
			ncells += XtWidth(child) / cw;
			ncells += XtHeight(child) / ch;
			break;
		default:
			break;
		} /* switch() */
	}

	/*
	 * Do we need to grow, and if yes, in which direction ?
	 */
	switch (ContainerSpatialResizeModel(w)) {
	case XmGROW_BALANCED:
		/* FIX ME, fall through for now */
	default:
	case XmGROW_MAJOR:
		if (MGR_LayoutDirection(w) == XmHORIZONTAL) {
			/* Increment width */
			contht = XtHeight(w) - 2 * ContainerMarginHeight(w);
			nrows = contht / ch;
			if (nrows) {
				ncols = ncells / nrows;
				if ((ncells % nrows) != 0)
					ncols++;
			} else
				ncols = ncells;
			contwid = cw * ncols;
		} else {
			/* Increment height */
			contwid = XtWidth(w) - 2 * ContainerMarginWidth(w);
			ncols = contwid / cw;
			if (ncols) {
				nrows = ncells / ncols;
				if ((ncells % ncols) != 0)
					nrows++;
			} else
				nrows = ncells;
			contht = ch * nrows;
		}
		break;
	case XmGROW_MINOR:
		if (MGR_LayoutDirection(w) == XmVERTICAL) {
			/* Increment width */
			contht = XtHeight(w) - 2 * ContainerMarginHeight(w);
			if (ch)
				nrows = contht / ch;
			else
				nrows = 1;
			if (nrows) {
				ncols = ncells / nrows;
				if ((ncells % nrows) != 0)
					ncols++;
			} else
				ncols = ncells;
			contwid = cw * ncols;
		} else {
			/* Increment height */
			contwid = XtWidth(w) - 2 * ContainerMarginWidth(w);
			if (cw)
				ncols = contwid / cw;
			else
				ncols = 1;
			if (ncols) {
				nrows = ncells / ncols;
				if ((ncells % ncols) != 0)
					nrows++;
			} else
				nrows = ncells;
			contht = ch * nrows;
		}
	}

	/*
	 * Layout children over cells and figure out how big Container must be.
	 */
	x = ContainerMarginWidth(w);
	y = ContainerMarginHeight(w);
	rowindex = 1;
	for (i=0; i<MGR_NumChildren(w); i++) {
		Widget	child = MGR_Children(w)[i];

		if (! XtIsManaged(child))
			continue;
		if (child->core.being_destroyed)
			continue;

		if (CC_EntryParent(child)) {
			/*
			 * Move out of sight. Can't unmanage because we wouldn't know
			 * when/whether to manage again.
			 */
			_XmMoveObject(child,
				-100-XtWidth(child),
				-100-XtHeight(child));
			continue;
		}

		if (ContainerSpatialStyle(w) == XmNONE) {
			x = CC_UserX(child);
			y = CC_UserY(child);
		}

		_XmMoveObject(child, x, y);

		switch (ContainerSpatialStyle(w)) {
		case XmCELLS:
			_XmMoveObject(child, x, y);

			incx = XtWidth(child) / cw;
			incx *= cw;
			incy = XtHeight(child) / ch;
			incy *= ch;
			if (rowmaxht < incy)
				rowmaxht = incy;
			if (x + incx >= contwid) {
				x = ContainerMarginWidth(w);
				y += rowmaxht;
				rowmaxht = 0;
			}
			break;			
		case XmGRID:
			rowindex++;
			if (rowindex > ncols) {
				rowindex = 1;
				x = ContainerMarginWidth(w);
				y += ch;
			} else {
				x += cw;
			}
			break;
		case XmNONE:
		default:
			break;
		}
	}

	contwid += 2 * ContainerMarginWidth(w);
	contht += 2 * ContainerMarginHeight(w);

	if (pwid)
		*pwid = contwid;
	if (pht)
		*pht = contht;
}

/*
 * FIX ME this is simplistic
 */
static void _XmContainerLayoutSimple(Widget w, Dimension *pwid, Dimension *pht)
{
	Dimension wid, ht;
	Cardinal i;
	Position x, y;

	wid = ContainerMarginWidth(w);
	ht = 0;

	for (i=0; i<MGR_NumChildren(w); i++) {
		Widget	child = MGR_Children(w)[i];
		if (XtIsManaged(child)) {
			wid += XtWidth(child) + ContainerMarginWidth(w);
			if (XtHeight(child) > ht)
				ht = XtHeight(child);
		}
	}

	ht += 2 * ContainerMarginHeight(w);

	if (pwid)
		*pwid = wid;
	if (pht)
		*pht = ht;

	x = ContainerMarginWidth(w);
	y = ContainerMarginHeight(w);

	for (i=0; i<MGR_NumChildren(w); i++) {
		Widget	child = MGR_Children(w)[i];
		if (XtIsManaged(child)) {
			_XmMoveObject(child, x, y);
			x += XtWidth(child) + ContainerMarginWidth(w);

			if (x > XtWidth(w) - ContainerMarginWidth(w)) {
				x = ContainerMarginWidth(w);
				if (XtHeight(child) < 20)
					y += 30;
				y += XtHeight(child);	/* FIX ME simplistic */
			}
		}
	}
}

/*
 * FIX ME this is simplistic
 */
static void _XmContainerLayoutDetail(Widget w, Dimension *pwid, Dimension *pht)
{
	Dimension wid, ht;
	Cardinal i;
	Position x, y;

	wid = ContainerMarginWidth(w);
	ht = 0;

	for (i=0; i<MGR_NumChildren(w); i++) {
		Widget	child = MGR_Children(w)[i];
		if (XtIsManaged(child)) {
			wid += XtWidth(child) + ContainerMarginWidth(w);
			if (XtHeight(child) > ht)
				ht = XtHeight(child);
		}
	}

	ht += 2 * ContainerMarginHeight(w);

	if (pwid)
		*pwid = wid;
	if (pht)
		*pht = ht;

	x = ContainerMarginWidth(w);
	y = ContainerMarginHeight(w);

	for (i=0; i<MGR_NumChildren(w); i++) {
		Widget	child = MGR_Children(w)[i];
		if (XtIsManaged(child)) {
			_XmMoveObject(child, x, y);
			x += XtWidth(child) + ContainerMarginWidth(w);

			if (x > XtWidth(w) - ContainerMarginWidth(w)) {
				x = ContainerMarginWidth(w);
				if (XtHeight(child) < 20)
					y += 30;
				y += XtHeight(child);	/* FIX ME simplistic */
			}
		}
	}
}

static void _XmContainerLayout(Widget w, Dimension *pwid, Dimension *pht)
{
	DEBUGOUT(_LtDebug(__FILE__, w, "_XmContainerLayout %s\n",
		(ContainerLayoutType(w) == XmDETAIL) ? "XmDETAIL" :
		(ContainerLayoutType(w) == XmSPATIAL) ? "XmSPATIAL" :
		(ContainerLayoutType(w) == XmOUTLINE) ? "XmOUTLINE" :
		"??"));

	switch (ContainerLayoutType(w)) {
	case XmDETAIL:
		_XmContainerLayoutDetail(w, pwid, pht);
		break;
	case XmOUTLINE:
		_XmContainerLayoutOutline(w, pwid, pht);
		break;
	case XmSPATIAL:
		_XmContainerLayoutSpatial(w, pwid, pht);
		break;
	default:
		_XmContainerLayoutSimple(w, pwid, pht);
		break;
	}
}

static XtGeometryResult
QueryGeometry(Widget w, XtWidgetGeometry *request, XtWidgetGeometry *reply)
{
	Dimension		wid, ht;
	XtWidgetGeometry	g;

	/* FIX ME: Needs to be implemented */

	DEBUGOUT(_LtDebug(__FILE__, w, "XmContainer QueryGeometry\n"));
	g = *request;

	_XmContainerLayout(w, &wid, &ht);

	wid = (request->request_mode & CWWidth) ?
		(request->width < wid) ? wid : request->width : wid;
	ht = (request->request_mode & CWHeight) ?
		(request->height < ht) ? ht : request->height : ht;

	reply->request_mode = CWWidth | CWHeight;
	reply->width = (wid == 0) ? 1 : wid;
	reply->height = (ht == 0) ? 1 : ht;

	return _XmGMReplyToQueryGeometry(w, &g, reply);
}

static void Destroy(Widget w)
{
	int	i;

	ClearSelectedItems(w);
	for (i=0; i<MGR_NumChildren(w); i++) {
		/* Check first if they still have a constraint record */
		if (CoreConstraints(MGR_Children(w)[i]) && ! MGR_Children(w)[i]->core.being_destroyed) {
			XtFree((char *)CC_Node(MGR_Children(w)[i]));
			CC_Node(MGR_Children(w)[i]) = NULL;
		}
	}
}

/* composite */
static XtGeometryResult
GeometryManager(Widget w, XtWidgetGeometry *request, XtWidgetGeometry *reply)
{
	DEBUGOUT(_LtDebug(__FILE__, w, "XmContainer GeometryManager\n"));

	/* FIX ME: Needs to be implemented */
	*reply = *request;
	return XtGeometryYes;
}

static void ChangeManaged(Widget w)
{
	DEBUGOUT(_LtDebug(__FILE__, w, "XmContainer ChangeManaged\n"));

	/* FIX ME: Needs to be implemented */
	_XmNavigChangeManaged(w);
	_XmContainerLayout(w, 0, 0);
}

static void _XmContainerExpandNode(Widget c)
{
	CwidNode		n;
	Widget			r;

	CC_OutlineState(c) = XmEXPANDED;

	r = CC_Related(c);
	if (r) {
		XtManageChild(r);
		XtVaSetValues(r,
			XmNoutlineState, XmEXPANDED,
			XmNlabelPixmap, ContainerExpandedStatePixmap(XtParent(r)),
			NULL);
	}

	for (n=CC_Node(c)->child_ptr; n; n=n->next_ptr) {
		if (CC_Related(n->widget_ptr)) {
			XtManageChild(CC_Related(n->widget_ptr));
		}
		XtManageChild(n->widget_ptr);
	}

	/* Geometry is handled in our caller !! */
}

static void _XmContainerUnmapSubtree(CwidNode n)
{
	CwidNode	c;
	Widget		w, r;

	DEBUGOUT(_LtDebug(__FILE__, n->widget_ptr, "_XmContainerUnmapSubtree\n"));
	if (!n)
		return;

	w = n->widget_ptr;
	if (CC_CWidType(w) == _XmIconButton)
		return;

	CC_OutlineState(w) = XmCOLLAPSED;
	r = CC_Related(w);
	if (r)
		XtVaSetValues(r,
			XmNoutlineState, XmCOLLAPSED,
			XmNlabelPixmap, ContainerCollapsedStatePixmap(XtParent(r)),
			NULL);

	for (c=n->child_ptr; c; c=c->next_ptr) {
		if (CC_CWidType(c->widget_ptr) != _XmIconButton) {
			Widget	r = CC_Related(c->widget_ptr);

			XtUnmanageChild(c->widget_ptr);
			if (r)
				XtUnmanageChild(r);
		}
		if (CC_CWidType(c->widget_ptr) != _XmIconButton)
			_XmContainerUnmapSubtree(c);
	}
}

/*
 * The pushbutton gadget gets toggled; need to open/close this part
 * of the tree. "Client" is the associated IconGadget.
 */
static void _XmContainerToggle(Widget w, XtPointer client, XtPointer call)
{
	Widget			b = (Widget) client,
				o = CC_Related(b);
	Dimension		wid, ht;
	XtGeometryResult	res;

	if (CC_OutlineState(o) == XmCOLLAPSED) {
		_XmContainerExpandNode(o);
	} else {
		/* Unmap children and their children */
		_XmContainerUnmapSubtree(CC_Node(o));
	}

	/* Query our ideal geometry */
	_XmContainerLayout(w, &wid, &ht);

	DEBUGOUT(_LtDebug2(__FILE__, w, o, "_XmContainerToggle: request %d %d\n", wid, ht));

	/* We'll need to request more real estate */
	res = XtMakeResizeRequest(w, wid, ht, &wid, &ht);
	if (res == XtGeometryYes) {
		XtWidth(w) = wid;
		XtHeight(w) = ht;
	}

	XClearWindow(XtDisplay(w), XtWindow(w));
}

/*
 * Attach widget CH to the right place in the tree structure.
 *	EP is the XmNentryParent.
 */
static void EntryParentHookup(Widget cw, Widget ch, Widget ep)
{
	Widget	prev = 0, nxt = 0;

	/* Need to initialize PositionIndex. FIX ME */
	if (CC_PositionIndex(ch) == 0) {
		int	i, m;

		/* Find children with the same EntryParent */
		/* Get the maximum PositionIndex among them */
		m = -1;
		for (i=0; i<MGR_NumChildren(cw); i++) {
			Widget c = MGR_Children(cw)[i];

			if (CC_CWidType(c) == _XmIconButton)
				continue;

			if (CC_EntryParent(c) == ep && CC_PositionIndex(c) > m) {
				m = CC_PositionIndex(c);
				prev = c;
			}
		}

		/* Assign one more than the maximum found */
		if (m >= 0) {
			CC_PositionIndex(ch) = m + 1;
			DEBUGOUT(_LtDebug2(__FILE__, ch, prev,
				"positionIndex %d\n", m+1));
		}
	} else {
		int	i, m;

		/* Find the child with the same EntryParent that has the largest
		 * PositionIndex smaller than ours.
		 */
		m = -1;
		for (i=0; i<MGR_NumChildren(cw); i++) {
			Widget	c = MGR_Children(cw)[i];
			int	p = CC_PositionIndex(c);

			if (CC_CWidType(c) == _XmIconButton)
				continue;
			if (CC_EntryParent(c) == ep && p < CC_PositionIndex(ch) && p > m) {
				prev = c;
				m = p;
			}
		}
	}

	if (CC_OutlineState(ch) == 0xFF) {
		/* This is the value we pass when creating a "folder" icon */
		CC_CWidType(ch) = _XmIconButton;
	} else {
		CC_CWidType(ch) = _XmChild;
	}

	/* Build the tree structure if OUTLINE */
	CC_Node(ch) = NULL;
	CC_Related(ch) = NULL;

	if (ContainerLayoutType(cw) == XmOUTLINE) {
		CC_Node(ch) = (CwidNode)XtMalloc(sizeof(XmCwidNodeRec));
		CC_Next(ch) = CC_Prev(ch) = CC_Child(ch) = NULL;

		CC_Widget(ch) = ch;
		
		if (CC_EntryParent(ch)) {
			CC_Parent(ch) = CC_Node(CC_EntryParent(ch));
			DEBUGOUT(_LtDebug2(__FILE__, cw, ch,
				"Constraint EntryParentHookup: parent is %s %p\n",
				XtName(CC_EntryParent(ch)),
				CC_EntryParent(ch)));
		} else {
			CC_Parent(ch) = 0;
		}

		if (prev) {
			if (CC_Next(prev)) {	/* Need to do some fixing */
				/* Now : prev -> nxt , to be prev -> this -> nxt */
				nxt = CC_Next(prev)->widget_ptr;

				CC_Next(ch) = CC_Node(nxt);
				CC_Prev(nxt) = CC_Node(ch);
			}
			CC_Prev(ch) = CC_Node(prev);
			CC_Next(prev) = CC_Node(ch);
		}
	}

	/* First/last */
	if (ContainerFirstNode(cw) == 0) {
		ContainerFirstNode(cw) = CC_Node(ch);
		ContainerLastNode(cw) = CC_Node(ch);
	}

	DEBUGOUT(_LtDebug2(__FILE__, cw, ch,
		"XmContainer EntryParentHookup (parent %p PI %d X %d Y %d)\n",
		CC_EntryParent(ch),
		CC_PositionIndex(ch),
		CC_UserX(ch),
		CC_UserY(ch)));

	/* Create a button for my parent if it doesn't have any yet */
	if (ContainerLayoutType(cw) == XmOUTLINE
			&& CC_CWidType(ch) == _XmChild
			&& CC_Node(ch)
			&& CC_Parent(ch)) {
		Widget	parent = CC_Parent(ch)->widget_ptr;

		if (!CC_Related(parent)) {
	 		Arg	al[5];
			int	ac = 0;
			Widget	b;

			if (CC_OutlineState(parent) == XmCOLLAPSED) {
				XtSetArg(al[ac], XmNlabelPixmap, ContainerCollapsedStatePixmap(cw)); ac++;
			} else {
				XtSetArg(al[ac], XmNlabelPixmap, ContainerExpandedStatePixmap(cw)); ac++;
			}
			XtSetArg(al[ac], XmNlabelType, XmPIXMAP); ac++;
			XtSetArg(al[ac], XmNoutlineState, (XtPointer)0xFF); ac++;
			CC_Related(parent) =
			b = XmCreatePushButtonGadget(cw, "icon", al, ac);
			CC_Related(b) = parent;
			XtManageChild(b);
			CC_CWidType(b) = _XmIconButton;

			/*
			 * Allocate a node for the new button,
			 * and insert it in the tree.
			 * This is mucho easier here which is why we
			 * pull the 0xFF outlineState trick above.
			 *
			 * Note that the _XmIconButton is only attached to the node
			 * that it belongs to, it doesn't have previous/next pointers.
			 */
			CC_Node(b) = (CwidNode)XtMalloc(sizeof(XmCwidNodeRec));

			CC_Next(b) = CC_Prev(b) = NULL;
			CC_Child(b) = NULL;
			CC_Parent(b) = CC_Node(parent);

			CC_Widget(b) = b;
#if 0
			/* Doesn't currently seem to work FIX ME */
			XtAddCallback(b, XmNactivateCallback,
				_XmContainerToggle, (XtPointer)parent);
#endif
		}

		/* Obviously this means the parent didn't have a child yet */
		if (CC_Child(parent) == NULL)
			CC_Child(parent) = CC_Node(ch);
	}
}

#include <stdio.h>

/*
 * Take a child widget out of the tree.
 *	This is often called with w->core.being_destroyed == True.
 *	FIX ME currently there is obviously still something wrong here :
 *	exactly what needs to be done for those widgets ?
 */
static void EntryParentClear(Widget cw, Widget ch)
{
	DEBUGOUT(_LtDebug2(__FILE__, cw, ch, "EntryParentClear\n"));

	/* Maybe this is the first node */
	if (ContainerFirstNode(cw) && ContainerFirstNode(cw)->widget_ptr == ch) {
		ContainerFirstNode(cw) = CC_Next(ch);
	}

	if (ch->core.being_destroyed) {
		DEBUGOUT(_LtDebug2(__FILE__, cw, ch, "EntryParentClear ZOMBIE %p\n", ch));
		return;
	}
	if (CC_CWidType(ch) == _XmIconButton)
		return;
	if (CC_Child(ch)) {
		_XmWarning(ch, "EntryParentClear: node has children, this should not happen\n");
	}

	/*
	 * Move out of sight. Can't unmanage because we wouldn't know
	 * when/whether to manage again.
	 */
	_XmMoveObject(ch, -100-XtWidth(ch), -100-XtHeight(ch));

	/* Remove link from the parent if this is its first child. */
	if (CC_Parent(ch)) {
		Widget	p = CC_Parent(ch)->widget_ptr;
		if (CC_Child(p) && CC_Child(p)->widget_ptr == ch) {
			CC_Child(p) = CC_Next(ch);
		}
	}

	/* Remove previous/next links */
	if (CC_Next(ch) && CC_Prev(ch)) {	/* Have both CC_Next and CC_Prev */
		Widget	n = CC_Next(ch)->widget_ptr,
			p = CC_Prev(ch)->widget_ptr;
		CC_Next(p) = CC_Node(n);
		CC_Prev(n) = CC_Node(p);

	} else if (CC_Next(ch)) {		/* Only a CC_Next */
		Widget	n = CC_Next(ch)->widget_ptr;
		CC_Prev(n) = NULL;
	} else if (CC_Prev(ch)) {		/* Only a CC_Prev */
		Widget	p = CC_Prev(ch)->widget_ptr;
		CC_Next(p) = NULL;
	} else {				/* Neither : nothing to do */
	}

	/* Remove IconButton node if there's one related to this widget. */
	if (CC_Related(ch)) {
		Widget	r = CC_Related(ch);

		DEBUGOUT(_LtDebug2(__FILE__, ch, r, "Related icon-button %p being destroyed\n", r));

		CC_Related(r) = NULL;
		CC_Next(r) = CC_Prev(r) = CC_Parent(r) = CC_Child(r) = NULL;
		CC_Node(r) = NULL;
		CC_Related(ch) = NULL;

		XtDestroyWidget(r);
	}

	CC_Next(ch) = CC_Prev(ch) = NULL;
	XtFree((char *)CC_Node(ch));
	CC_Node(ch) = NULL;
}

/* constraint */
/* FIX ME must maintain ContainerLastNode */
static void
ConstraintInitialize(Widget request, Widget new_w, ArgList args, Cardinal *num_args)
{
	Widget				cw = XtParent(new_w), prev = NULL;
/*	XmContainerConstraintPtr	con = (XmContainerConstraintPtr)CoreConstraints(new_w); */
	Dimension			wid, ht;
	XtGeometryResult		res;

	DEBUGOUT(_LtDebug2(__FILE__, cw, new_w, "ConstraintInitialize\n"));
	DEBUGOUT(
		if (CC_EntryParent(new_w)) {
		_LtDebug2(__FILE__, new_w, CC_EntryParent(new_w),
			"EntryParent\n");
	});

	CC_Node(new_w) = NULL;

	/* Need to initialize PositionIndex. FIX ME */
	if (CC_PositionIndex(new_w) == 0) {
		int	i, m;

		/* Find children with the same EntryParent */
		/* Get the maximum PositionIndex among them */
		m = -1;
		for (i=0; i<MGR_NumChildren(cw); i++) {
			Widget c = MGR_Children(cw)[i];

			if (CC_CWidType(c) == _XmIconButton)
				continue;

			if (CC_EntryParent(c) == CC_EntryParent(new_w)
					&& CC_PositionIndex(c) > m) {
				m = CC_PositionIndex(c);
				prev = c;
			}
		}

		/* Assign one more than the maximum found */
		if (m >= 0) {
			CC_PositionIndex(new_w) = m + 1;
			DEBUGOUT(_LtDebug2(__FILE__, new_w, prev,
				"positionIndex %d\n", m+1));
		}
	}

	if (CC_OutlineState(new_w) == 0xFF) {
		/* This is the value we pass when creating a "folder" icon */
		DEBUGOUT(_LtDebug2(__FILE__, cw, new_w, "Yes !\n"));
		CC_CWidType(new_w) = _XmIconButton;
	} else {
		CC_CWidType(new_w) = _XmChild;
	}

	/* Build the tree structure if OUTLINE */
	CC_Node(new_w) = NULL;
	CC_Related(new_w) = NULL;

	EntryParentHookup(cw, new_w, CC_EntryParent(new_w));

	_XmContainerLayout(cw, &wid, &ht);

	DEBUGOUT(_LtDebug2(__FILE__, cw, new_w,
		"ContainerConstraintInitialise: geo %d.%d -> %d.%d\n",
		XtWidth(cw), XtHeight(cw), wid, ht));

	if (XtWidth(cw) < wid || XtHeight(cw) < ht) {
		if (wid < XtWidth(cw))
			wid = XtWidth(cw);
		if (ht < XtHeight(cw))
			ht = XtHeight(cw);
		res = XtMakeResizeRequest(cw, wid, ht, &wid, &ht);
		if (res == XtGeometryYes) {
			XtWidth(cw) = wid;
			XtHeight(cw) = ht;
		}
	}
}

static void ConstraintDestroy(Widget w)
{
	Widget			cw = XtParent(w);

	DEBUGOUT(_LtDebug2(__FILE__, cw, w, "XmContainer ConstraintDestroy\n"));

	if (ItemIsSelected(cw, w))
		RemoveSelectedItem(cw, w);
	EntryParentClear(cw, w);

	/* Remove graphics leftovers by redrawing completely. */
	if (XtWindow(cw))
		XClearWindow(XtDisplay(cw), XtWindow(cw));
}

static Boolean
ConstraintSetValues(Widget current, Widget request, Widget new_w, ArgList args, Cardinal *num_args)
{
	Widget	c = XtParent(new_w);
	Boolean	relayout = False;


	DEBUGOUT(_LtDebug2(__FILE__, c, new_w, "XmContainer ConstraintSetValues\n"));
	/* FIX ME: Needs to be implemented */
#if 0
	if (CC_OutlineState(new_w) != CC_OutlineState(current)) {
		Pixmap	p;

		if (CC_OutlineState(new_w) == XmCOLLAPSED)
			p = ContainerCollapsedStatePixmap(c);
		else
			p = ContainerExpandedStatePixmap(c);

		XtVaSetValues(new_w, XmNlabelPixmap, p, NULL);
	}
#endif

	/* Add the XmNentryParent to an already existing widget */
	if (CC_EntryParent(new_w) != CC_EntryParent(current)) {
		/* Remove the widget from the old place in the tree. */
		EntryParentClear(c, new_w);

		/* Hook up the widget in the new place in the tree */
		EntryParentHookup(c, new_w, CC_EntryParent(new_w));

		relayout = True;
	}
#if 0
	/* This doesn't belong here */
	if ((ContainerLargeCellWidth(new_w) != ContainerLargeCellWidth(current))
		|| (ContainerLargeCellHeight(new_w) != ContainerLargeCellHeight(current))
		|| (ContainerSmallCellWidth(new_w) != ContainerSmallCellWidth(current))
		|| (ContainerSmallCellHeight(new_w) != ContainerSmallCellHeight(current))) {
		relayout = True;
	}
#endif

	if (relayout)
		_XmContainerLayout(c, NULL, NULL);
	return relayout;
}

/*
 * XmContainerGetItemChildren allocates a WidgetList and stores within it
 * the widget IDs of all widgets that have item specified as the value of
 * their XmNentryParent resource.
 * The application programmer is responsible for freeing the allocated
 * WidgetList using XtFree. The number of widget IDs returned in item_children
 * is returned by the function. If no widgets specify item as the value of
 * their XmNentryParent resource, the function returns zero and
 * item_children is left unchanged.
 */
int XmContainerGetItemChildren(Widget w, Widget item, WidgetList *item_children)
{
	Widget	*l;
	int	i, j, num;

	for (i=0, num=0; i<MGR_NumChildren(w); i++)
		if (CC_EntryParent(MGR_Children(w)[i]) == item)
			num++;
	if (num == 0)
		return 0;

	l = (Widget *)XtCalloc(num + 1, sizeof(Widget));
	for (i=0, j=0; i < MGR_NumChildren(w) && j < num; i++)
		if (CC_EntryParent(MGR_Children(w)[i]) == item)
			l[j++] = MGR_Children(w)[i];
	*item_children = l;

	return num;
}

void XmContainerRelayout(Widget w)
{
	_XmWarning(w, "XmContainerRelayout(): not yet implemented!");
}

void XmContainerReorder(Widget w, WidgetList cwid_list, int cwid_count)
{
	_XmWarning(w, "XmContainerReorder(): not yet implemented!");
}

Boolean XmContainerCut(Widget w, Time timestamp)
{
	_XmWarning(NULL, "XmContainerCut(): not yet implemented!");
	return False;
}

/*
 * XmContainerCopy copies the primary selected container items to the clipboard.
 * This routine calls the XmNconvertCallback procedures, possibly multiple times,
 * with the selection member of the XmConvertCallbackStruct set to CLIPBOARD and
 * with the parm member set to XmCOPY.
 *
 * container	Specifies the Container widget ID.
 * timestamp	Specifies the server time at which to modify the selection value.
 *
 * The function returns False in the following cases:
 *	if the primary selection is NULL,
 *	if the widget does not own the primary selection, or
 *	if the function is unable to gain ownership of the clipboard selection.
 * Otherwise, it returns True.
 */
Boolean XmContainerCopy(Widget w, Time timestamp)
{
	_XmWarning(w, "XmContainerCopy(): not yet implemented!");
	return False;
}

/*
 * XmContainerPaste requests data transfer from the clipboard selection
 * to the Container. This routine calls the widget's XmNdestinationCallback
 * procedures with the selection member of the XmDestinationCallbackStruct
 * set to CLIPBOARD and with the operation member set to XmCOPY. The Container
 * widget itself performs no transfers; the XmNdestinationCallback procedures
 * are responsible for inserting the clipboard selection and for taking any
 * related actions.
 *
 * container	Specifies the Container widget ID.
 *
 * The function returns False if no data transfer takes place.
 * Otherwise, it returns True.
 */
Boolean XmContainerPaste(Widget w)
{
	_XmWarning(w, "XmContainerPaste(): not yet implemented!");
	return False;
}

Boolean XmContainerCopyLink(Widget w, Time timestamp)
{
	_XmWarning(w, "XmContainerCopyLink(): not yet implemented!");
	return False;
}

/*
 * XmContainerPasteLink requests data transfer from the clipboard selection
 * to the Container. This routine calls the widget's XmNdestinationCallback
 * procedures with the selection member of the XmDestinationCallbackStruct
 * set to CLIPBOARD and with the operation member set to XmLINK. The Container
 * widget itself performs no transfers; the XmNdestinationCallback procedures
 * are responsible for inserting the link to the clipboard selection and for
 * taking any related actions.
 */
Boolean XmContainerPasteLink(Widget w)
{
	_XmWarning(w, "XmContainerPasteLink(): not yet implemented!");
	return False;
}

static void
_ContainerTrait_GetValues(Widget container, XmContainerData d)
{
	DEBUGOUT(_LtDebug(__FILE__, container, "_ContainerTrait_GetValues\n"));

	if (d->valueMask & ContDetailOrder) {
		d->detail_order = ContainerDetailOrder(container);
		d->detail_order_count = ContainerDetailOrderCount(container);
	}
	if (d->valueMask & ContDetailTabList) {
		d->detail_tablist = ContainerDetailTablist(container);
	}
	if (d->valueMask & ContFirstColumnWidth) {
		d->first_column_width = ContainerFirstColWidth(container);
	}
	if (d->valueMask & ContSelectionMode) {
#if 0
		/* FIX ME what is this ? */
		d->selection_mode = ContainerSelectionMode(container);
#endif
	}
	if (d->valueMask & ContSelectColor) {
		d->select_color = ContainerSelectColor(container);
	}
}

/*
 * Figure out which object the user 'hit'
 */
static Widget HitObject(Widget cont, Position x, Position y)
{
	/* FIX ME this is currently very simplistic */
	int	i;

	for (i=0; i<MGR_NumChildren(cont); i++) {
		Widget child = MGR_Children(cont)[i];

		if (XtX(child) <= x && x <= XtX(child) + XtWidth(child)
			&& XtY(child) <= y && y <= XtY(child) + XtHeight(child)) {

			return child;
		}
	}
	return NULL;
}

/*
 * Actions
 *
 * ContainerActivate() calls XmNdefaultActionCallback with reason XmCR_DEFAULT_ACTION.
 */
static void ContainerActivate(Widget w, XEvent *event, String *params, Cardinal *num_params)
{
	XmContainerSelectCallbackStruct	cbs;
	Widget				fw;
#if 1
	DEBUGOUT(_LtDebugAction(__FILE__, w, "XmContainer ContainerActivate", params, num_params));
#endif
	/* This should probably be the technique to get this ...
	fw = XmGetFocusWidget(w);
	** */

	fw = ContainerIconHeader(w);	/* Hack */
	if (!fw)
		return;				/* FIX ME is this the right action ? */

	if (CC_CWidType(fw) == _XmIconButton) {
		_XmContainerToggle(w, (XtPointer)fw, NULL);
	} else {
		/* */
		cbs.reason = XmCR_DEFAULT_ACTION;
		cbs.event = event;
		cbs.selected_items = NULL;	/* FIX ME */
		cbs.selected_item_count = 0;	/* FIX ME */
		cbs.auto_selection_type = 0;	/* FIX ME */

		XtCallCallbackList(w, ContainerDefaultActionCallback(w), (XtPointer)&cbs);
	}
}

/*
 * This function should be used in the Action event handlers to
 * figure out whether to fold/expand the tree or to do action
 * processing.
 */
static int _XmContainerToggleEvent(Widget w, XEvent *event)
{
	Widget	fw = ContainerIconHeader(w);

	if (fw && CC_CWidType(fw) == _XmIconButton) {
		_XmContainerToggle(w, (XtPointer)fw, NULL);
		return False;
	} else {
		return True;
	}

	return True;	/* Means still have to treat this */	
}

/*
 * ContainerBeginExtend()
 *	Returns if XmNselectionPolicy is XmSINGLE_SELECT or XmBROWSE_SELECT.
 *	Returns if XmNlayoutType is XmSPATIAL.
 *	Otherwise, this action sets the selection state of all items between the
 *	anchor item and the item under the pointer to the selection state of the
 *	anchor item. The location cursor is moved to the item under the pointer.
 *	If XmNautomaticSelection is XmAUTO_SELECT, the XmNselectionCallback(s) is
 *	called with either XmCR_MULTIPLE_SELECT or XmCR_EXTENDED_SELECT as the
 *	reason depending on XmNselectionPolicy, and with auto_selection_type
 *	XmAUTO_CHANGE.
 */
static void
ContainerBeginExtend(Widget w, XEvent *event, String *params, Cardinal *num_params)
{
	Widget	hit;

	DEBUGOUT(_LtDebugAction(__FILE__, w, "ContainerBeginExtend", params, num_params));

	if (ContainerSelectionPolicy(w) == XmSINGLE_SELECT
			|| ContainerSelectionPolicy(w) == XmBROWSE_SELECT
			|| ContainerLayoutType(w) == XmSPATIAL)
		return;

	hit = HitObject(w, event->xbutton.x, event->xbutton.y);

	if (ItemIsSelected(w, ContainerAnchorCWid(w))) {
		SelectItemRange(w, ContainerAnchorCWid(w), hit);
	} else {
		UnselectItemRange(w, ContainerAnchorCWid(w), hit);
	}

	/* FIX ME set the location cursor */

	if (ContainerAutomaticSelection(w) == XmAUTO_SELECT) {
		XmContainerSelectCallbackStruct	cbs;
	
		if (ContainerSelectionPolicy(w) == XmEXTENDED_SELECT)
			cbs.reason = XmCR_EXTENDED_SELECT;
		else
			cbs.reason = XmCR_MULTIPLE_SELECT;

		cbs.auto_selection_type = XmAUTO_CHANGE;
		cbs.event = event;
		cbs.selected_items = ContainerSelectedItems(w);
		cbs.selected_item_count = ContainerSelectedItemCount(w);
		XtCallCallbackList(w, ContainerSelectionCallback(w), (XtPointer)&cbs);
	}
}

/*
 * ContainerBeginSelect() :
 *	If this is a second ContainerBeginSelect() action that has occurred
 *	within the time specified by the display's multiclick time, this
 *	action calls XmNdefaultActionCallback with reason XmCR_DEFAULT_ACTION
 *	and returns.
 *	Otherwise, processing depends on the value of XmNselectionPolicy as follows: 
 *	XmSINGLE_SELECT
 *		This action deselects all items and toggles the item (if any) under the pointer. 
 *	XmBROWSE_SELECT
 *		This action deselects all items and toggles the item (if any) under the pointer.
 *		This item is now the anchor item for further selection. If XmNautomaticSelection
 *		is XmAUTO_SELECT and a change in any item's selection state is made, the
 *		XmNselectionCallback(s) is called with reason XmCR_BROWSE_SELECT and
 *		auto_selection_type XmAUTO_BEGIN.
 *	XmMULTIPLE_SELECT
 *		If the pointer is over an item and XmNselectionTechnique is not XmMARQUEE,
 *		this action toggles the selection state of that item. The item becomes the anchor
 *		item for further selection. If XmNselectionTechnique is XmMARQUEE,
 *		XmMARQUEE_EXTEND_START, or XmMARQUEE_EXTEND_BOTH, this action sets
 *		the start point for the Marquee rectangle. If XmNselectionTechnique is
 *		XmMARQUEE_EXTEND_START or XmMARQUEE_EXTEND_BOTH and the pointer is over an item,
 *		this action draws the Marquee rectangle around the item. If XmNautomaticSelection
 *		is XmAUTO_SELECT, the XmNselectionCallback(s) is called with reason
 *		XmCR_MULTIPLE_SELECT and auto_selection_type XmAUTO_BEGIN.
 *	XmEXTENDED_SELECT
 *		All items are first deselected. Processing is then identical to the case where
 *		XmNselectionPolicy is XmMULTIPLE_SELECT, except that XmCR_EXTENDED_SELECT is the
 *		callback reason given if XmNselectionCallback is called. 
 */
static void
ContainerBeginSelect(Widget w, XEvent *event, String *params, Cardinal *num_params)
{
	Time	mctime = XtGetMultiClickTime(XtDisplay(w));
	Widget	hit;

	DEBUGOUT(_LtDebugAction(__FILE__, w, "ContainerBeginSelect", params, num_params));
	DEBUGOUT(_LtDebug(__FILE__, w, "\tEvent at %d %d\n", event->xbutton.x, event->xbutton.y));

	/* If this is the second ContainerBeginSelect within the multiclick time,
	 * then call the default action callback */
	if (ContainerLastClickTime(w) &&
			event->xbutton.time - ContainerLastClickTime(w) < mctime) {
		XmContainerSelectCallbackStruct	cbs;

		cbs.reason = XmCR_DEFAULT_ACTION;
		cbs.event = event;
		cbs.selected_items = ContainerSelectedItems(w);
		cbs.selected_item_count = ContainerSelectedItemCount(w);
		cbs.auto_selection_type = 0;	/* FIX ME */

		XtCallCallbackList(w, ContainerDefaultActionCallback(w), (XtPointer)&cbs);
		return;
	}

	ContainerLastClickTime(w) = event->xbutton.time;
	hit = HitObject(w, event->xbutton.x, event->xbutton.y);
	ContainerIconHeader(w) = hit;	/* FIX ME this resource probably serves another puspose */

	DEBUGOUT(_LtDebug(__FILE__, w, "Hit '%s'\n",
		hit ? XtName(hit) : "(nothing)"));

	/* FIX ME another hack */
	if (hit && CC_CWidType(hit) == _XmIconButton) {
#if 1
		/* It's probably not as simple as this */
		XtCallActionProc(w, "ManagerGadgetActivate", event, NULL, 0);
#endif
	}

	switch (ContainerSelectionPolicy(w)) {
	case XmSINGLE_SELECT:
		ClearSelectedItems(w);
		AddSelectedItem(w, hit);
		break;
	case XmBROWSE_SELECT:
		ClearSelectedItems(w);
		if (hit) {
			AddSelectedItem(w, hit);
			ContainerAnchorCWid(w) = hit;
			if (ContainerAutomaticSelection(w) == XmAUTO_SELECT) {
				XmContainerSelectCallbackStruct cbs;

				cbs.reason = XmBROWSE_SELECT;
				cbs.auto_selection_type = XmAUTO_BEGIN;
				cbs.event = event;
				cbs.selected_items = ContainerSelectedItems(w);
				cbs.selected_item_count = ContainerSelectedItemCount(w);
				XtCallCallbackList(w, ContainerSelectionCallback(w), (XtPointer)&cbs);
			}			
		}
		break;
	default:
	case XmEXTENDED_SELECT:
		ClearSelectedItems(w);
		/* Fall through !! */
	case XmMULTIPLE_SELECT:
		if (hit && ContainerSelectionTechnique(w) != XmMARQUEE) {
			/* Toggle selection state of this item */
			if (ItemIsSelected(w, hit))
				RemoveSelectedItem(w, hit);
			else
				AddSelectedItem(w, hit);
			ContainerAnchorCWid(w) = hit;
		}
		if (ContainerSelectionTechnique(w) == XmMARQUEE
			|| ContainerSelectionTechnique(w) == XmMARQUEE_EXTEND_START
			|| ContainerSelectionTechnique(w) == XmMARQUEE_EXTEND_BOTH) {
			MarqueeStart(w, event);
		}
		if (hit && (ContainerSelectionTechnique(w) == XmMARQUEE_EXTEND_START
			|| ContainerSelectionTechnique(w) == XmMARQUEE_EXTEND_BOTH)) {
			/* FIX ME */
		}
		if (ContainerAutomaticSelection(w) == XmAUTO_SELECT) {
			XmContainerSelectCallbackStruct cbs;

			if (ContainerSelectionPolicy(w) == XmEXTENDED_SELECT)
				cbs.reason = XmCR_EXTENDED_SELECT;
			else
				cbs.reason = XmCR_MULTIPLE_SELECT;
			cbs.auto_selection_type = XmAUTO_BEGIN;
			cbs.event = event;
			cbs.selected_items = ContainerSelectedItems(w);
			cbs.selected_item_count = ContainerSelectedItemCount(w);
			XtCallCallbackList(w, ContainerSelectionCallback(w), (XtPointer)&cbs);
		}
	}
}

static void
ContainerBeginToggle(Widget w, XEvent *event, String *params, Cardinal *num_params)
{
	DEBUGOUT(_LtDebugAction(__FILE__, w, "ContainerBeginToggle", params, num_params));
}

/*
 * ContainerButtonMotion() :
 *	Processing depends on the value of XmNselectionPolicy, as follows: 
 *	XmSINGLE_SELECT
 *		This action simply returns to the caller. 
 *	XmBROWSE_SELECT
 *		Simply returns if this action follows a ContainerBeginExtend()
 *		action or ContainerBeginToggle() action. 
 *		If the pointer is no longer over the current anchor item, this
 *		action toggles the current anchor item and then toggles the item
 *		under the pointer (if any) and makes it the new anchor item for
 *		further processing. If XmNautomaticSelection is XmAUTO_SELECT and
 *		a change in any item's selection state is made, the
 *		XmNselectionCallback(s) is called with reason XmCR_BROWSE_SELECT
 *		and auto_selection_type XmAUTO_MOTION.
 *	XmMULTIPLE_SELECT
 *		If a previous action has set a Marquee rectangle start point, this
 *		action draws the Marquee rectangle between the current pointer
 *		position and the Marquee start point. If the XmNselectionTechnique
 *		is XmMARQUEE_EXTEND_BOTH and the pointer is over an item, the end
 *		point of the Marquee rectangle is extended to include the item. The
 *		selection states of all items within the Marquee rectangle are
 *		toggled to match the state of the anchor item.
 *		If no Marquee rectangle start point is set and the pointer is
 *		over an item, processing depends on the XmNlayoutType resource.
 *		The anchor item from the previous action is used. If XmNlayoutType
 *		is XmSPATIAL, the selection state of the item under the pointer is
 *		toggled to match the selection state of the anchor item. If
 *		XmNlayoutType is XmOUTLINE or XmDETAIL, the selection state of all
 *		items between the anchor item and the item under the pointer are
 *		toggled to match the selection state of the anchor item. 
 *		If XmNautomaticSelection is XmAUTO_SELECT and a change in any item's
 *		selection state is made, the XmNselectionCallback(s) is called with
 *		reason XmCR_MULTIPLE_SELECT and auto_selection_type XmAUTO_MOTION.
 *	XmEXTENDED_SELECT
 *		Processing is identical to the case where XmNselectionPolicy is
 *		XmMULTIPLE_SELECT, except that XmCR_EXTENDED_SELECT is the callback
 *		reason given if XmNselectionCallback is called. 
 *
 * Note : the fake function is to make the distinction between a call from
 *	ContainerEndSelect() and as the real ContainerButtonMotion() handler.
 *	The former case doesn't call the XmNselectionCallback, the latter does.
 */
static Boolean HandleContainerButtonMotion(Boolean doit,
	Widget w, XEvent *event, String *params, Cardinal *num_params)
{
	Widget		hit;
	Position	x, y;
	Boolean		ChangedSelection = False;

	DEBUGOUT(_LtDebugAction(__FILE__, w, "HandleContainerButtonMotion", params, num_params));

	switch (ContainerSelectionPolicy(w)) {
	case XmSINGLE_SELECT:
		return False;
	case XmBROWSE_SELECT:	/* FIX ME */
		break;
	case XmMULTIPLE_SELECT:
	case XmEXTENDED_SELECT:	/* This is the default */
	default:
		if (ContainerMarqueeMode(w)) {	/* If there's a marquee rectangle start */
			/* FIX ME */
		} else {	/* No marquee rectangle start */
			/* If the pointer is over an item */
			switch (event->type) {
			case MotionNotify:
				x = event->xmotion.x;
				y = event->xmotion.y;
				break;
			case ButtonPress:
			case ButtonRelease:
			default:	/* FIX ME */
				x = event->xbutton.x;
				y = event->xbutton.y;
				break;
			}
			hit = HitObject(w, x, y);
			DEBUGOUT(_LtDebug2(__FILE__, w, hit, "Yow ContainerButtonMotion\n"));
			switch (ContainerLayoutType(w)) {
			case XmSPATIAL:
				/* FIX ME */
			case XmOUTLINE:
			case XmDETAIL:
			default:
				if (hit) {
					if (ItemIsSelected(w, ContainerAnchorCWid(w))) {
						SelectItemRange(w, ContainerAnchorCWid(w), hit);
					} else {
						UnselectItemRange(w, ContainerAnchorCWid(w), hit);
					}
					ChangedSelection = True;
				}
			}
			
		}
		if (doit && ChangedSelection && ContainerAutomaticSelection(w) == XmAUTO_SELECT) {
			XmContainerSelectCallbackStruct	cbs;

			cbs.reason = XmCR_MULTIPLE_SELECT;
			cbs.event = event;
			cbs.auto_selection_type = XmAUTO_MOTION;
			cbs.selected_item_count = 0;
			cbs.selected_items = 0;

			XtCallCallbackList(w, ContainerSelectionCallback(w), (XtPointer)&cbs);
		}
	}
	return ChangedSelection;
}

static void
ContainerButtonMotion(Widget w, XEvent *event, String *params, Cardinal *num_params)
{
	(void) HandleContainerButtonMotion(True, w, event, params, num_params);
}

static void
ContainerCancel(Widget w, XEvent *event, String *params, Cardinal *num_params)
{
	DEBUGOUT(_LtDebugAction(__FILE__, w, "ContainerCancel", params, num_params));
}

static void
ContainerDeselectAll(Widget w, XEvent *event, String *params, Cardinal *num_params)
{
	DEBUGOUT(_LtDebugAction(__FILE__, w, "ContainerDeselectAll", params, num_params));
}

/*
 *	ContainerEndExtend() :
 *
 *	Simply returns if XmNselectionPolicy is XmSINGLE_SELECT or XmBROWSE_SELECT.
 *	Simply returns if XmNlayoutType is XmSPATIAL.
 *
 *	Otherwise, if XmNautomaticSelection is XmNO_AUTO_SELECT,
 *	XmNselectionCallback(s) is called with either XmCR_MULTIPLE_SELECT or
 *	XmCR_EXTENDED_SELECT as the reason depending on XmNselectionPolicy.
 *
 *	If XmNautomaticSelection is XmAUTO_SELECT and no change is made in any item's
 *	selection state by this action, XmNselectionCallback(s) is called with either
 *	XmCR_MULTIPLE_SELECT or XmCR_EXTENDED_SELECT as the reason depending
 *	on XmNselectionPolicy and auto_selection_type XmAUTO_CHANGE.
 *
 *	If XmNautomaticSelection is XmAUTO_SELECT and this action makes no change in any
 *	item's selection state, XmNselectionCallback(s) is called with either
 *	XmCR_MULTIPLE_SELECT or XmCR_EXTENDED_SELECT as the reason depending
 *	on XmNselectionPolicy and auto_selection_type XmAUTO_NO_CHANGE.
 */
static void
ContainerEndExtend(Widget w, XEvent *event, String *params, Cardinal *num_params)
{
	XmContainerSelectCallbackStruct	cbs;

	DEBUGOUT(_LtDebugAction(__FILE__, w, "ContainerEndExtend", params, num_params));

	if (ContainerSelectionPolicy(w) == XmSINGLE_SELECT
		|| ContainerSelectionPolicy(w) == XmBROWSE_SELECT
		|| ContainerLayoutType(w) == XmSPATIAL)
		return;

	if (ContainerAutomaticSelection(w) == XmNO_AUTO_SELECT) {
		if (ContainerSelectionPolicy(w) == XmMULTIPLE_SELECT)
			cbs.reason = XmCR_MULTIPLE_SELECT;
		else
			cbs.reason = XmCR_EXTENDED_SELECT;
		cbs.event = event;
		cbs.auto_selection_type = XmAUTO_MOTION;
		cbs.selected_item_count = 0;
		cbs.selected_items = 0;
		XtCallCallbackList(w, ContainerSelectionCallback(w), (XtPointer)&cbs);
	} else if (ContainerAutomaticSelection(w) == XmAUTO_SELECT) {
		if (0 /* FIX ME, if we've changed the selection */) {
			if (ContainerSelectionPolicy(w) == XmMULTIPLE_SELECT)
				cbs.reason = XmCR_MULTIPLE_SELECT;
			else
				cbs.reason = XmCR_EXTENDED_SELECT;
			cbs.event = event;
			cbs.auto_selection_type = XmAUTO_CHANGE;
			cbs.selected_item_count = ContainerSelectedItemCount(w);
			cbs.selected_items = ContainerSelectedItems(w);
			XtCallCallbackList(w, ContainerSelectionCallback(w), (XtPointer)&cbs);
		} else {	/* Haven't changed the selection */
			if (ContainerSelectionPolicy(w) == XmMULTIPLE_SELECT)
				cbs.reason = XmCR_MULTIPLE_SELECT;
			else
				cbs.reason = XmCR_EXTENDED_SELECT;
			cbs.event = event;
			cbs.auto_selection_type = XmAUTO_NO_CHANGE;
			cbs.selected_item_count = ContainerSelectedItemCount(w);
			cbs.selected_items = ContainerSelectedItems(w);
			XtCallCallbackList(w, ContainerSelectionCallback(w), (XtPointer)&cbs);
		}
	}
}

/*
 * ContainerEndSelect() :
 *	Processing depends on the value of XmNselectionPolicy, as follows:
 *	XmSINGLE_SELECT
 *		This action calls XmNselectionCallback with reason XmCR_SINGLE_SELECT. 
 *	XmBROWSE_SELECT
 *		If the pointer is no longer over the current anchor item,
 *		this action toggles the current anchor item and then toggles the item
 *		under the pointer (if any). If XmNautomaticSelection is XmNO_AUTO_SELECT,
 *		the XmNselectionCallback(s) is called with reason XmCR_BROWSE_SELECT.
 *		If XmNautomaticSelection is XmAUTO_SELECT and a change in any item's
 *		selection state is made, XmNselectionCallback(s) is called with reason
 *		XmCR_BROWSE_SELECT and auto_selection_type XmAUTO_CHANGE. If
 *		XmNautomaticSelection is XmAUTO_SELECT and no change is made in any
 *		item's selection state by this action, XmNselectionCallback(s) is
 *		called with reason XmCR_BROWSE_SELECT and auto_selection_type
 *		XmAUTO_NO_CHANGE. 
 *	XmMULTIPLE_SELECT
 *		This action first performs the same processing as the
 *		ContainerButtonMotion() action, except that XmNselectionCallback
 *		is not called. If XmNautomaticSelection is XmNO_AUTO_SELECT, the
 *		XmNselectionCallback(s) is called with reason XmCR_MULTIPLE_SELECT.
 *		If XmNautomaticSelection is XmAUTO_SELECT and a change in any item's
 *		selection state is made, the XmNselectionCallback(s) is called with
 *		reason XmCR_MULTIPLE_SELECT and auto_selection_type XmAUTO_CHANGE.
 *		If XmNautomaticSelection is XmAUTO_SELECT and this action makes no
 *		change in any item's selection state, XmNselectionCallback(s) is
 *		called with reason XmCR_MULTIPLE_SELECT and auto_selection_type
 *		XmAUTO_NO_CHANGE.
 *	XmEXTENDED_SELECT
 *		This action first performs the same processing as the ContainerButtonMotion()
 *		action, except that XmNselectionCallback is not called. If
 *		XmNautomaticSelection is XmNO_AUTO_SELECT, the XmNselectionCallback(s)
 *		is called with reason XmCR_EXTENDED_SELECT. If XmNautomaticSelection
 *		is XmAUTO_SELECT and a change in any item's selection state is made,
 *		XmNselectionCallback(s) is called with reason XmCR_EXTENDED_SELECT
 *		and auto_selection_type XmAUTO_CHANGE. If XmNautomaticSelection is
 *		XmAUTO_SELECT and this action makes no change in any item's selection
 *		state, XmNselectionCallback(s) is called with reason XmCR_EXTENDED_SELECT
 *		and auto_selection_type XmAUTO_NO_CHANGE.
 */
static void
ContainerEndSelect(Widget w, XEvent *event, String *params, Cardinal *num_params)
{
	XmContainerSelectCallbackStruct	cbs;
	Widget				l[2];
	Boolean				changed = False;

	DEBUGOUT(_LtDebugAction(__FILE__, w, "ContainerEndSelect", params, num_params));

#if 0
	/* FIX ME this comes not from documentation but it's a hack */
	ContainerActivate(w, event, params, num_params);
#endif
#if 1
	if (! _XmContainerToggleEvent(w, event)) {	/* FIX ME this is also a guess */
		return;
	}

	switch (ContainerSelectionPolicy(w)) {
	case XmSINGLE_SELECT:
	default:
		cbs.reason = XmCR_DEFAULT_ACTION;
		cbs.event = event;
		cbs.selected_items = l;	/* FIX ME */
		l[0] = ContainerIconHeader(w);
		l[1] = (Widget)0;
		if (l[0]) {
			cbs.selected_item_count = 1;	/* FIX ME */
		} else {
			cbs.selected_item_count = 0;	/* FIX ME */
		}
		cbs.auto_selection_type = 0;	/* FIX ME */

		XtCallCallbackList(w, ContainerSelectionCallback(w), (XtPointer)&cbs);
	/* ?? */
		ContainerIconHeader(w) = NULL;
		break;
	case XmBROWSE_SELECT:
		/* FIX ME */
	case XmMULTIPLE_SELECT:
	case XmEXTENDED_SELECT:
		changed = HandleContainerButtonMotion(False, w, event, params, num_params);
		break;
	}
#endif
}

static void
ContainerEndToggle(Widget w, XEvent *event, String *params, Cardinal *num_params)
{
	DEBUGOUT(_LtDebugAction(__FILE__, w, "ContainerEndToggle", params, num_params));
}

static void
ContainerEndTransfer(Widget w, XEvent *event, String *params, Cardinal *num_params)
{
	DEBUGOUT(_LtDebugAction(__FILE__, w, "ContainerEndTransfer", params, num_params));
}

static void
ContainerExpandOrCollapse(Widget w, XEvent *event, String *params, Cardinal *num_params)
{
	DEBUGOUT(_LtDebugAction(__FILE__, w, "ContainerExpandOrCollapse", params, num_params));
}

static void
ContainerExtend(Widget w, XEvent *event, String *params, Cardinal *num_params)
{
	DEBUGOUT(_LtDebugAction(__FILE__, w, "ContainerExtend", params, num_params));
}

static void
ContainerExtendCursor(Widget w, XEvent *event, String *params, Cardinal *num_params)
{
	DEBUGOUT(_LtDebugAction(__FILE__, w, "ContainerExtendCursor", params, num_params));
}

static void
ContainerHandleBtn1Down(Widget w, XEvent *event, String *params, Cardinal *num_params)
{
	DEBUGOUT(_LtDebugAction(__FILE__, w, "ContainerHandleBtn1Down", params, num_params));

	/* FIX ME - Do something depending on the state of XmNenableBtn1Transfer */

	/* If the pointer is over an unselected item or background, select it */

	/* Start transfer */

	/* FIX ME - somehow I think we must trigger this action */
	if (num_params) {
		XtCallActionProc(w, params[0], event, params + 1, (*num_params)-1);
	}
}

static void
ContainerHandleBtn1Motion(Widget w, XEvent *event, String *params, Cardinal *num_params)
{
	DEBUGOUT(_LtDebugAction(__FILE__, w, "ContainerHandleBtn1Motion", params, num_params));
}

/*
 * 
 */
static void
ContainerHandleBtn1Up(Widget w, XEvent *event, String *params, Cardinal *num_params)
{
	DEBUGOUT(_LtDebugAction(__FILE__, w, "ContainerHandleBtn1Up", params, num_params));

	if (0) {
		/* If a button 1 transfer is in progress, cancel it. */
		/* FIX ME */
	} else {
		/* Perform the action specified in the parameter */
		if (*num_params != 1) {
			_XmWarning(w, "ContainerHandleBtn1Up: #parameters should be 1");
			return;
		}

		XtCallActionProc(w, params[0], event, params, 0);
	}
}

static void
ContainerHandleBtn2Down(Widget w, XEvent *event, String *params, Cardinal *num_params)
{
	DEBUGOUT(_LtDebugAction(__FILE__, w, "ContainerHandleBtn2Down", params, num_params));
}

static void
ContainerHandleBtn2Motion(Widget w, XEvent *event, String *params, Cardinal *num_params)
{
	DEBUGOUT(_LtDebugAction(__FILE__, w, "ContainerHandleBtn2Motion", params, num_params));
}

static void
ContainerHandleBtn2Up(Widget w, XEvent *event, String *params, Cardinal *num_params)
{
	DEBUGOUT(_LtDebugAction(__FILE__, w, "ContainerHandleBtn2Up", params, num_params));
}

static void
ContainerMoveCursor(Widget w, XEvent *event, String *params, Cardinal *num_params)
{
	DEBUGOUT(_LtDebugAction(__FILE__, w, "ContainerMoveCursor", params, num_params));
}

static void
ContainerPrimaryCopy(Widget w, XEvent *event, String *params, Cardinal *num_params)
{
	DEBUGOUT(_LtDebugAction(__FILE__, w, "ContainerPrimaryCopy", params, num_params));
}

static void
ContainerPrimaryLink(Widget w, XEvent *event, String *params, Cardinal *num_params)
{
	DEBUGOUT(_LtDebugAction(__FILE__, w, "ContainerPrimaryLink", params, num_params));
}

static void
ContainerPrimaryMove(Widget w, XEvent *event, String *params, Cardinal *num_params)
{
	DEBUGOUT(_LtDebugAction(__FILE__, w, "ContainerPrimaryMove", params, num_params));
}

static void
ContainerSelect(Widget w, XEvent *event, String *params, Cardinal *num_params)
{
	DEBUGOUT(_LtDebugAction(__FILE__, w, "ContainerSelect", params, num_params));
}

static void
ContainerSelectAll(Widget w, XEvent *event, String *params, Cardinal *num_params)
{
	DEBUGOUT(_LtDebugAction(__FILE__, w, "ContainerSelectAll", params, num_params));
}

static void
ContainerStartTransfer(Widget w, XEvent *event, String *params, Cardinal *num_params)
{
	DEBUGOUT(_LtDebugAction(__FILE__, w, "ContainerStartTransfer", params, num_params));
}

static void
ContainerToggleMode(Widget w, XEvent *event, String *params, Cardinal *num_params)
{
	DEBUGOUT(_LtDebugAction(__FILE__, w, "ContainerToggleMode", params, num_params));
}
