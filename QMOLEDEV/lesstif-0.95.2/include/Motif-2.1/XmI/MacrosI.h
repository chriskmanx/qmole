/**
 *
 * $Id: MacrosI.h,v 1.1 2004/08/28 19:23:30 dannybackx Exp $
 * 
 * Copyright (C) 1995 Free Software Foundation, Inc.
 * Copyright © 1995, 1996, 1997, 1998, 1999, 2000, 2002 LessTif Development Team
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

#ifndef _XMI_MACROSI_H
#define _XMI_MACROSI_H

#include <Xm/LabelGP.h>
#include <Xm/LabelP.h>



/*
 * object shorthand
 */
#define ObjName(w) \
    (((ObjectRec *)(w))->object.xrm_name)

/*
 * Core shorthand
 */
#define CoreBeingDestroyed(w) \
    (((Widget)(w))->core.being_destroyed)

#define CoreConstraints(w) \
    (((Widget)(w))->core.constraints)

/* can't use XtScreen because that can be a function */
#define CoreScreen(w) \
    (((Widget)(w))->core.screen)

#define CoreDepth(w) \
    (((Widget)(w))->core.depth)

#define CoreColormap(w) \
    (((Widget)(w))->core.colormap)

#define CoreAccelerators(w) \
    (((Widget)(w))->core.accelerators)

/* can't use XtWindow because we want to be sure which window
 * gets set. */
#define CoreWindow(w) \
    (((Widget)(w))->core.window)

#define CoreMappedWhenManaged(w) \
    (((Widget)(w))->core.mapped_when_managed)

#define CorePopupList(w) \
    (((Widget)(w))->core.popup_list)

#define CoreNumPopups(w) \
    (((Widget)(w))->core.num_popups)

/* can't use XtBackground because that can be a function */
#define CoreBackground(w) \
    (((Widget)(w))->core.background_pixel)

#define CoreBackgroundPixmap(w) \
    (((Widget)(w))->core.background_pixmap)

/* class macros */
#define CoreClassTranslations(w) \
    (XtClass((Widget)(w))->core_class.tm_table)

/*
 * Shell shorthand
 */
#define	Shell_PopdownCallback(x) \
    (((ShellWidget)(x))->shell.popdown_callback)

#define	Shell_PoppedUp(x) \
    (((ShellWidget)(x))->shell.popped_up)

#define	Shell_GrabKind(x) \
    (((ShellWidget)(x))->shell.grab_kind)

#define Shell_AllowResize(x) \
    (((ShellWidget)(x))->shell.allow_shell_resize)

/*
 * one that Motif should have but doesn't
 */
#define XmIsMenuBar(w) \
    (XtIsSubclass(w, xmRowColumnWidgetClass) && RC_Type(w) == XmMENU_BAR)

#define XmIsOptionMenu(w) \
    (XtIsSubclass(w, xmRowColumnWidgetClass) && RC_Type(w) == XmMENU_OPTION)

/*
 * In the first set of macros, we have changed the names from what Motif has.
 */
/*
 * LABEL GADGET OVERRIDE MACROS
 */
#define LabG_AcceleratorText	LabG__acceleratorText
#define LabG_Label		LabG__label

#define LabGClass_SetOverrideCallback(c) \
    (((XmLabelGadgetClassRec *)(c))->label_class.setOverrideCallback)

#define LabGClass_MenuProcs(c) \
    (((XmLabelGadgetClassRec *)(c))->label_class.menuProcs)


/* 
 * PUSH BUTTON GADGET OVERRIDE MACROS
 */
#define PBG_DefaultButtonShadow PBG_DefaultButtonShadowThickness
#define PBG_FillGC		PBG_FillGc
#define PBG_BackgroundGC	PBG_BackgroundGc

/*
 * TOGGLE BUTTON GADGET OVERRIDE MACROS
 */
#define TBG_ArmCallback			TBG_ArmCB
#define TBG_ValueChangedCallback	TBG_ValueChangedCB
#define TBG_DisarmCallback		TBG_DisarmCB

/*
 * In the second set, these are macros that we have that Motif doesn't.
 */

/*
 * PRIMITIVE ACCESS MACROS
 */
#define Prim_Foreground(w) \
    (((XmPrimitiveWidget)(w))->primitive.foreground)

#define Prim_HighlightThickness(w) \
    (((XmPrimitiveWidget)(w))->primitive.highlight_thickness)

#define Prim_TopShadowGC(w) \
    (((XmPrimitiveWidget)(w))->primitive.top_shadow_GC)

#define Prim_TopShadowColor(w) \
    (((XmPrimitiveWidget)(w))->primitive.top_shadow_color)

#define Prim_TopShadowPixmap(w) \
    (((XmPrimitiveWidget)(w))->primitive.top_shadow_pixmap)

#define Prim_BottomShadowGC(w) \
    (((XmPrimitiveWidget)(w))->primitive.bottom_shadow_GC)

#define Prim_BottomShadowColor(w) \
    (((XmPrimitiveWidget)(w))->primitive.bottom_shadow_color)

#define Prim_BottomShadowPixmap(w) \
    (((XmPrimitiveWidget)(w))->primitive.bottom_shadow_pixmap)

#define Prim_HighlightGC(w) \
    (((XmPrimitiveWidget)(w))->primitive.highlight_GC)

#define Prim_HighlightColor(w) \
    (((XmPrimitiveWidget)(w))->primitive.highlight_color)

#define Prim_HighlightPixmap(w) \
    (((XmPrimitiveWidget)(w))->primitive.highlight_pixmap)

#define Prim_HighlightOnEnter(w) \
    (((XmPrimitiveWidget)(w))->primitive.highlight_on_enter)

#define Prim_HighlightDrawn(w) \
    (((XmPrimitiveWidget)(w))->primitive.highlight_drawn)

#define Prim_Highlighted(w) \
    (((XmPrimitiveWidget)(w))->primitive.highlighted)

#define Prim_TraversalOn(w) \
    (((XmPrimitiveWidget)(w))->primitive.traversal_on)

#define Prim_NavigationType(w) \
    (((XmPrimitiveWidget)(w))->primitive.navigation_type)

#define Prim_UnitType(w) \
    (((XmPrimitiveWidget)(w))->primitive.unit_type)

/*
 * some class macros
 */
#define PrimC_BorderHighlight(wc) \
    (((XmPrimitiveClassRec *)(wc))->primitive_class.border_highlight)

#define PrimC_BorderUnhighlight(wc) \
    (((XmPrimitiveClassRec *)(wc))->primitive_class.border_unhighlight)

#define PrimC_ArmAndActivate(wc) \
    (((XmPrimitiveClassRec *)(wc))->primitive_class.arm_and_activate)

/*
 * MANAGER/GADGET ACCESS MACROS
 */
#define XmParentForeground(w) (((XmManagerWidget) \
        (((XmGadget)(w))->object.parent))->manager.foreground)

#define XmParentBackground(w) (((XmManagerWidget) \
        (((XmGadget)(w))->object.parent))->core.background_pixel)

#define XmParentBottomShadowColor(w) (((XmManagerWidget) \
        (((XmGadget)(w))->object.parent))->manager.bottom_shadow_color)

#define XmParentHighlightColor(w) (((XmManagerWidget) \
        (((XmGadget)(w))->object.parent))->manager.highlight_color)

#define XmParentTopShadowColor(w) (((XmManagerWidget) \
        (((XmGadget)(w))->object.parent))->manager.top_shadow_color)

/*
 * MANAGER ACCESS MACROS
 */
#define MGR_InitialFocus(w) \
        (((XmManagerWidget)(w))->manager.initial_focus)
    
#define MGR_BackgroundGC(m) \
	(((XmManagerWidget)(m))->manager.background_GC)

#define MGR_Foreground(m) \
	(((XmManagerWidget)(m))->manager.foreground)

#define MGR_TopShadowGC(m) \
	(((XmManagerWidget)(m))->manager.top_shadow_GC)

#define MGR_TopShadowColor(m) \
	(((XmManagerWidget)(m))->manager.top_shadow_color)

#define MGR_TopShadowPixmap(m) \
	(((XmManagerWidget)(m))->manager.top_shadow_pixmap)

#define MGR_BottomShadowGC(m) \
	(((XmManagerWidget)(m))->manager.bottom_shadow_GC)

#define MGR_BottomShadowColor(m) \
	(((XmManagerWidget)(m))->manager.bottom_shadow_color)

#define MGR_BottomShadowPixmap(m) \
	(((XmManagerWidget)(m))->manager.bottom_shadow_pixmap)

#define MGR_HighlightGC(m) \
	(((XmManagerWidget)(m))->manager.highlight_GC)

#define MGR_HighlightColor(m) \
	(((XmManagerWidget)(m))->manager.highlight_color)

#define MGR_HighlightPixmap(m) \
	(((XmManagerWidget)(m))->manager.highlight_pixmap)

#define MGR_HelpCallbackList(m) \
	(((XmManagerWidget)(m))->manager.help_callback)

#define MGR_EventHandlerAdded(m) \
	(((XmManagerWidget)(m))->manager.event_handler_added)

#define MGR_HighlightedWidget(m) \
	(((XmManagerWidget)(m))->manager.highlighted_widget)

#define MGR_ActiveChild(m) \
	(((XmManagerWidget)(m))->manager.active_child)

#define MGR_SelectedGadget(m) \
	(((XmManagerWidget)(m))->manager.selected_gadget)

#define MGR_EligibleForMultiButtonEvent(m) \
	(((XmManagerWidget)(m))->manager.eligible_for_multi_button_event)

#define MGR_NavigationType(m) \
	(((XmManagerWidget)(m))->manager.navigation_type)

#define MGR_TraversalOn(m) \
	(((XmManagerWidget)(m))->manager.traversal_on)

#define MGR_UnitType(m) \
	(((XmManagerWidget)(m))->manager.unit_type)

#define MGR_StringDirection(m) \
	(((XmManagerWidget)(m))->manager.string_direction)

/* a couple of #defines to make composite stuff easier */

#define MGR_NumChildren(m) \
	(((XmManagerWidget)(m))->composite.num_children)

#define MGR_Children(m) \
	(((XmManagerWidget)(m))->composite.children)

#define MGR_NumSlots(m) \
	(((XmManagerWidget)(m))->composite.num_slots)

/*
 * GADGET ACCESS MACROS
 */
#define G_HelpCallback(g) \
       (((XmGadget)(g))->gadget.help_callback)

#define G_UserData(g) \
       (((XmGadget)(g))->gadget.have_traversal)

#define G_TraversalOn(g) \
       (((XmGadget)(g))->gadget.traversal_on)

#define G_HighlightOnEnter(g) \
       (((XmGadget)(g))->gadget.highlight_on_enter)

#define G_HaveTraversal(g) \
       (((XmGadget)(g))->gadget.have_traversal)

#define G_UnitType(g) \
       (((XmGadget)(g))->gadget.unit_type)

#define G_NavigationType(g) \
       (((XmGadget)(g))->gadget.navigation_type)

#define G_HighlightDrawn(g) \
       (((XmGadget)(g))->gadget.highlight_drawn)

#define G_Highlighted(g) \
       (((XmGadget)(g))->gadget.highlighted)

#define G_Visible(g) \
       (((XmGadget)(g))->gadget.visible)

#define G_EventMask(g) \
       (((XmGadget)(g))->gadget.event_mask)

/*
 * gadget class macros
 */
#define GC_BorderHighlight(wc) \
    (((XmGadgetClassRec *)(wc))->gadget_class.border_highlight)

#define GC_BorderUnhighlight(wc) \
    (((XmGadgetClassRec *)(wc))->gadget_class.border_unhighlight)

#define GC_ArmAndActivate(wc) \
    (((XmGadgetClassRec *)(wc))->gadget_class.arm_and_activate)


/*
 * EXT OBJECT ACCESS MACROS
 */
#define ExtObj_LogicalParent(w) \
    (((XmExtRec *)w)->ext.logicalParent)

#define ExtObj_ExtensionType(w) \
    (((XmExtRec *)w)->ext.extensionType)

/*
 * VENDOR SHELL EXT ACCESS MACROS
 */
#define VSEP_DefaultFontList(o) \
	(((XmVendorShellExtObject)o)->vendor.default_font_list)

#define VSEP_FocusPolicy(o) \
	(((XmVendorShellExtObject)o)->vendor.focus_policy)

#define VSEP_FocusData(o) \
	(((XmVendorShellExtObject)o)->vendor.focus_data)

#define VSEP_DeleteResponse(o) \
	(((XmVendorShellExtObject)o)->vendor.delete_response)

#define VSEP_UnitType(o) \
	(((XmVendorShellExtObject)o)->vendor.unit_type)

#define VSEP_MwmHints(o) \
	(((XmVendorShellExtObject)o)->vendor.mwm_hints)

#define VSEP_MwmInfo(o) \
	(((XmVendorShellExtObject)o)->vendor.mwm_info)

#define VSEP_MwmMenu(o) \
	(((XmVendorShellExtObject)o)->vendor.mwm_menu)

#define VSEP_FocusMovedCallback(o) \
	(((XmVendorShellExtObject)o)->vendor.focus_moved_callback)

#define VSEP_OldManaged(o) \
	(((XmVendorShellExtObject)o)->vendor.old_managed)

#define VSEP_XAtMap(o) \
	(((XmVendorShellExtObject)o)->vendor.xAtMap)

#define VSEP_YAtMap(o) \
	(((XmVendorShellExtObject)o)->vendor.yAtMap)

#define VSEP_XOffset(o) \
	(((XmVendorShellExtObject)o)->vendor.xOffset)

#define VSEP_YOffset(o) \
	(((XmVendorShellExtObject)o)->vendor.yOffset)

#define VSEP_LastOffsetSerial(o) \
	(((XmVendorShellExtObject)o)->vendor.lastOffsetSerial)

#define VSEP_LastMapRequest(o) \
	(((XmVendorShellExtObject)o)->vendor.lastMapRequest)

#define VSEP_ExternalReposition(o) \
	(((XmVendorShellExtObject)o)->vendor.externalReposition)

#define VSEP_MapStyle(o) \
	(((XmVendorShellExtObject)o)->vendor.mapStyle)

#define VSEP_RealizeCallback(o) \
	(((XmVendorShellExtObject)o)->vendor.realize_callback)

#define VSEP_GrabKind(o) \
	(((XmVendorShellExtObject)o)->vendor.grab_kind)

#define VSEP_AudibleWarning(o) \
	(((XmVendorShellExtObject)o)->vendor.audible_warning)

#define VSEP_LabelFontList(o) \
	(((XmVendorShellExtObject)o)->vendor.label_font_list)

#define VSEP_ButtonFontList(o) \
	(((XmVendorShellExtObject)o)->vendor.button_font_list)

#define VSEP_TextFontList(o) \
	(((XmVendorShellExtObject)o)->vendor.text_font_list)

#define VSEP_InputMethodString(o) \
	(((XmVendorShellExtObject)o)->vendor.input_method_string)

#define VSEP_PreeditTypeString(o) \
	(((XmVendorShellExtObject)o)->vendor.preedit_type_string)

#define VSEP_LightThreshold(o) \
	(((XmVendorShellExtObject)o)->vendor.light_threshold)

#define VSEP_DarkThreshold(o) \
	(((XmVendorShellExtObject)o)->vendor.dark_threshold)

#define VSEP_ForegroundThreshold(o) \
	(((XmVendorShellExtObject)o)->vendor.foreground_threshold)

#define VSEP_ImHeight(o) \
	(((XmVendorShellExtObject)o)->vendor.im_height)

#define VSEP_ImInfo(o) \
	(((XmVendorShellExtObject)o)->vendor.im_info)

#define VSEP_ImHeightSet(o) \
	(((XmVendorShellExtObject)o)->vendor.im_vs_height_set)

#define Vendor_IconPixmap(o) \
	(((WMShellWidget)o)->wm.wm_hints.icon_pixmap)

/*
 * ARROW BUTTON WIDGET ACCESS MACROS
 */
#define AB_ArmCallback(w) \
    (((XmArrowButtonWidget)(w))->arrowbutton.arm_callback)

#define AB_ActivateCallback(w) \
    (((XmArrowButtonWidget)(w))->arrowbutton.activate_callback)

#define AB_DisarmCallback(w) \
    (((XmArrowButtonWidget)(w))->arrowbutton.disarm_callback)

#define AB_ArrowGC(w) \
    (((XmArrowButtonWidget)(w))->arrowbutton.arrow_GC)

#define AB_InsensitiveGC(w) \
    (((XmArrowButtonWidget)(w))->arrowbutton.insensitive_GC)

#define AB_Direction(w) \
    (((XmArrowButtonWidget)(w))->arrowbutton.direction)

#define AB_Armed(w) \
    (((XmArrowButtonWidget)(w))->arrowbutton.selected)

#define AB_Timer(w) \
    (((XmArrowButtonWidget)(w))->arrowbutton.timer)

#define AB_MultiClick(w) \
    (((XmArrowButtonWidget)(w))->arrowbutton.multiClick)

#define AB_ClickCount(w) \
    (((XmArrowButtonWidget)(w))->arrowbutton.click_count)

#define AB_ArmTimeStamp(w) \
    (((XmArrowButtonWidget)(w))->arrowbutton.armTimeStamp)

#define AB_DetailShadowThickness(w) \
    (((XmArrowButtonWidget)(w))->arrowbutton.detail_shadow_thickness)

/*
 * ARROW BUTTON GADGET ACCESS MACROS
 */
#define ABG_ActivateCallback(w) \
    (((XmArrowButtonGadget)(w))->arrowbutton.activate_callback)

#define ABG_ArmCallback(w) \
    (((XmArrowButtonGadget)(w))->arrowbutton.arm_callback)

#define ABG_DisarmCallback(w) \
    (((XmArrowButtonGadget)(w))->arrowbutton.disarm_callback)

#define ABG_ArrowGC(w) \
    (((XmArrowButtonGadget)(w))->arrowbutton.arrow_GC)

#define ABG_InsensitiveGC(w) \
    (((XmArrowButtonGadget)(w))->arrowbutton.insensitive_GC)

#define ABG_Direction(w) \
    (((XmArrowButtonGadget)(w))->arrowbutton.direction)

#define ABG_Armed(w) \
    (((XmArrowButtonGadget)(w))->arrowbutton.selected)

#define ABG_Timer(w) \
    (((XmArrowButtonGadget)(w))->arrowbutton.timer)

#define ABG_MultiClick(w) \
    (((XmArrowButtonGadget)(w))->arrowbutton.multiClick)

#define ABG_ClickCount(w) \
    (((XmArrowButtonGadget)(w))->arrowbutton.click_count)

#define ABG_ArmTimeStamp(w) \
    (((XmArrowButtonGadget)(w))->arrowbutton.armTimeStamp)

#define ABG_DetailShadowThickness(w) \
    (((XmArrowButtonGadget)(w))->arrowbutton.detail_shadow_thickness)

/*
 * BULLETINBOARD WIDGET ACCESS MACROS
 */
#define BB_OldWidth(w) \
	(((XmBulletinBoardWidget)(w))->bulletin_board.old_width)

#define BB_OldHeight(w) \
	(((XmBulletinBoardWidget)(w))->bulletin_board.old_height)

#define BB_OldShadowThickness(w) \
	(((XmBulletinBoardWidget)(w))->bulletin_board.old_shadow_thickness)

#define BB_AutoUnmanage(w) \
	(((XmBulletinBoardWidget)(w))->bulletin_board.auto_unmanage)

#define BB_FocusCallback(w) \
	(((XmBulletinBoardWidget)(w))->bulletin_board.focus_callback)

#define BB_MapCallback(w) \
	(((XmBulletinBoardWidget)(w))->bulletin_board.map_callback)

#define BB_UnmapCallback(w) \
	(((XmBulletinBoardWidget)(w))->bulletin_board.unmap_callback)

#define BB_DefaultPosition(w) \
	(((XmBulletinBoardWidget)(w))->bulletin_board.default_position)

#define BB_ShadowType(w) \
	(((XmBulletinBoardWidget)(w))->bulletin_board.shadow_type)

#define BB_NoResize(w) \
	(((XmBulletinBoardWidget)(w))->bulletin_board.no_resize)

#define BB_DialogStyle(w) \
	(((XmBulletinBoardWidget)(w))->bulletin_board.dialog_style)

#define BB_DialogTitle(w) \
	(((XmBulletinBoardWidget)(w))->bulletin_board.dialog_title)

#define BB_AllowOverlap(w) \
	(((XmBulletinBoardWidget)(w))->bulletin_board.allow_overlap)

#define BB_GeoCache(w) \
	(((XmBulletinBoardWidget)(w))->bulletin_board.geo_cache)

/*
 * COMMAND WIDGET ACCESS MACROS
 */
#define Com_Command(w) \
     (((XmCommandRec *)(w))->selection_box.selection_label_string)

#define Com_HistoryItems(w) \
     (((XmCommandRec *)(w))->selection_box.list_items)

#define Com_HistoryItemCount(w) \
     (((XmCommandRec *)(w))->selection_box.list_item_count)

#define Com_HistoryMaxItems(w) \
     (((XmCommandRec *)(w))->command.history_max_items)

#define Com_HistoryVisibleItemCount(w) \
     (((XmCommandRec *)(w))->selection_box.list_visible_item_count)

#define Com_PromptString(w) \
     (((XmCommandRec *)(w))->selection_box.selection_label_string)

#define	Com_Error(w)	\
     (((XmCommandRec *)(w))->command.error)

/*
 * CASCADE BUTTON WIDGET ACCESS MACROS
 */
#define CB_Timer(cb) \
    (((XmCascadeButtonWidget)(cb))->cascade_button.timer)

#define CB_MapDelay(cb) \
    (((XmCascadeButtonWidget)(cb))->cascade_button.map_delay)

/*
 * DRAWING AREA WIDGET ACCESS MACROS
 */
#define DA_MarginWidth(w) \
    (((XmDrawingAreaWidget)w)->drawing_area.margin_width)

#define DA_MarginHeight(w) \
    (((XmDrawingAreaWidget)w)->drawing_area.margin_height)

#define DA_ResizePolicy(w) \
    (((XmDrawingAreaWidget)w)->drawing_area.resize_policy)

#define DA_ExposeCallback(w) \
    (((XmDrawingAreaWidget)w)->drawing_area.expose_callback)

#define DA_InputCallback(w) \
    (((XmDrawingAreaWidget)w)->drawing_area.input_callback)

#define DA_ResizeCallback(w) \
    (((XmDrawingAreaWidget)w)->drawing_area.resize_callback)

/*
 * DRAWN BUTTON ACCESS MACROS
 */
#define DB_PushButtonEnabled(w) \
    (((XmDrawnButtonWidget)(w))->drawnbutton.pushbutton_enabled)

#define DB_ShadowType(w) \
    (((XmDrawnButtonWidget)(w))->drawnbutton.shadow_type)

#define DB_ActivateCallback(w) \
    (((XmDrawnButtonWidget)(w))->drawnbutton.activate_callback)

#define DB_ArmCallback(w) \
    (((XmDrawnButtonWidget)(w))->drawnbutton.arm_callback)

#define DB_DisarmCallback(w) \
    (((XmDrawnButtonWidget)(w))->drawnbutton.disarm_callback)

#define DB_ExposeCallback(w) \
    (((XmDrawnButtonWidget)(w))->drawnbutton.expose_callback)

#define DB_ResizeCallback(w) \
    (((XmDrawnButtonWidget)(w))->drawnbutton.resize_callback)

#define DB_Armed(w) \
    (((XmDrawnButtonWidget)(w))->drawnbutton.armed)

#define DB_OldWidth(w) \
    (((XmDrawnButtonWidget)(w))->drawnbutton.old_width)

#define DB_OldHeight(w) \
    (((XmDrawnButtonWidget)(w))->drawnbutton.old_height)

#define DB_OldShadowThickness(w) \
    (((XmDrawnButtonWidget)(w))->drawnbutton.old_shadow_thickness)

#define DB_OldHighlightThickness(w) \
    (((XmDrawnButtonWidget)(w))->drawnbutton.old_highlight_thickness)

#define DB_Timer(w) \
    (((XmDrawnButtonWidget)(w))->drawnbutton.timer)

#define DB_MultiClick(w) \
    (((XmDrawnButtonWidget)(w))->drawnbutton.multiClick)

#define DB_ClickCount(w) \
    (((XmDrawnButtonWidget)(w))->drawnbutton.click_count)

#define DB_ArmTimeStamp(w) \
    (((XmDrawnButtonWidget)(w))->drawnbutton.armTimeStamp)


/*
 * DRAG CONTEXT WIDGET ACCESS MACROS
 */
#define DC_ExportTargets(w) \
    (((XmDragContextRec *)(w))->drag.exportTargets)

#define DC_NumExportTargets(w) \
    (((XmDragContextRec *)(w))->drag.numExportTargets)

#define DC_ConvertProc(w) \
    (((XmDragContextRec *)(w))->drag.convertProc)

#define DC_ClientData(w) \
    (((XmDragContextRec *)(w))->drag.clientData)

#define DC_SourceCursorIcon(w) \
    (((XmDragContextRec *)(w))->drag.sourceCursorIcon)

#define DC_StateCursorIcon(w) \
    (((XmDragContextRec *)(w))->drag.stateCursorIcon)

#define DC_OperationCursorIcon(w) \
    (((XmDragContextRec *)(w))->drag.operationCursorIcon)

#define DC_SourcePixmapIcon(w) \
    (((XmDragContextRec *)(w))->drag.sourcePixmapIcon)

#define DC_CursorBackground(w) \
    (((XmDragContextRec *)(w))->drag.cursorBackground)

#define DC_CursorForeground(w) \
    (((XmDragContextRec *)(w))->drag.cursorForeground)

#define DC_ValidCursorForeground(w) \
    (((XmDragContextRec *)(w))->drag.validCursorForeground)

#define DC_InvalidCursorForeground(w) \
    (((XmDragContextRec *)(w))->drag.invalidCursorForeground)

#define DC_NoneCursorForeground(w) \
    (((XmDragContextRec *)(w))->drag.noneCursorForeground)

#define DC_DragMotionCallback(w) \
    (((XmDragContextRec *)(w))->drag.dragMotionCallback)

#define DC_OperationChangedCallback(w) \
    (((XmDragContextRec *)(w))->drag.operationChangedCallback)

#define DC_SiteEnterCallback(w) \
    (((XmDragContextRec *)(w))->drag.siteEnterCallback)

#define DC_SiteLeaveCallback(w) \
    (((XmDragContextRec *)(w))->drag.siteLeaveCallback)

#define DC_TopLevelEnterCallback(w) \
    (((XmDragContextRec *)(w))->drag.topLevelEnterCallback)

#define DC_TopLevelLeaveCallback(w) \
    (((XmDragContextRec *)(w))->drag.topLevelLeaveCallback)

#define DC_DropStartCallback(w) \
    (((XmDragContextRec *)(w))->drag.dropStartCallback)

#define DC_DropFinishCallback(w) \
    (((XmDragContextRec *)(w))->drag.dropFinishCallback)

#define DC_DragDropFinishCallback(w) \
    (((XmDragContextRec *)(w))->drag.dragDropFinishCallback)

#define DC_DragOperations(w) \
    (((XmDragContextRec *)(w))->drag.dragOperations)

#define DC_Incremental(w) \
    (((XmDragContextRec *)(w))->drag.incremental)

#define DC_BlendModel(w) \
    (((XmDragContextRec *)(w))->drag.blendModel)

#define DC_SrcWindow(w) \
    (((XmDragContextRec *)(w))->drag.srcWindow)

#define DC_DragStartTime(w) \
    (((XmDragContextRec *)(w))->drag.dragStartTime)

#define DC_ICCHandle(w) \
    (((XmDragContextRec *)(w))->drag.iccHandle)

#define DC_SourceWidget(w) \
    (((XmDragContextRec *)(w))->drag.sourceWidget)

#define DC_SourceIsExternal(w) \
    (((XmDragContextRec *)(w))->drag.sourceIsExternal)

#define DC_TopWindowsFetched(w) \
    (((XmDragContextRec *)(w))->drag.topWindowsFetched)

#define DC_CommType(w) \
    (((XmDragContextRec *)(w))->drag.commType)

#define DC_AnimationType(w) \
    (((XmDragContextRec *)(w))->drag.animationType)

#define DC_Operation(w) \
    (((XmDragContextRec *)(w))->drag.operation)

#define DC_Operations(w) \
    (((XmDragContextRec *)(w))->drag.operations)

#define DC_LastEventState(w) \
    (((XmDragContextRec *)(w))->drag.lastEventState)

#define DC_DragCompletionStatus(w) \
    (((XmDragContextRec *)(w))->drag.dragCompletionStatus)

#define DC_DragDropCompletionStatus(w) \
    (((XmDragContextRec *)(w))->drag.dragDropCompletionStatus)

#define DC_ForceIPC(w) \
    (((XmDragContextRec *)(w))->drag.forceIPC)

#define DC_ServerGrabbed(w) \
    (((XmDragContextRec *)(w))->drag.serverGrabbed)

#define DC_UseLocal(w) \
    (((XmDragContextRec *)(w))->drag.useLocal)

#define DC_InDropSite(w) \
    (((XmDragContextRec *)(w))->drag.inDropSite)

#define DC_DragTimerId(w) \
    (((XmDragContextRec *)(w))->drag.dragTimerId)

#define DC_RoundOffTime(w) \
    (((XmDragContextRec *)(w))->drag.roundOffTime)

#define DC_LastChangeTime(w) \
    (((XmDragContextRec *)(w))->drag.lastChangeTime)

#define DC_CrossingTime(w) \
    (((XmDragContextRec *)(w))->drag.crossingTime)

#define DC_RoundOffTime(w) \
    (((XmDragContextRec *)(w))->drag.roundOffTime)

#define DC_DragFinishTime(w) \
    (((XmDragContextRec *)(w))->drag.dragFinishTime)

#define DC_DropFinishTime(w) \
    (((XmDragContextRec *)(w))->drag.dropFinishTime)

#define DC_DropSelection(w) \
    (((XmDragContextRec *)(w))->drag.dropSelection)

#define DC_SrcShell(w) \
    (((XmDragContextRec *)(w))->drag.srcShell)

#define DC_StartX(w) \
    (((XmDragContextRec *)(w))->drag.startX)

#define DC_StartY(w) \
    (((XmDragContextRec *)(w))->drag.startY)

#define DC_SiteID(w) \
    (((XmDragContextRec *)(w))->drag.siteID)

#define DC_CurrScreen(w) \
    (((XmDragContextRec *)(w))->drag.currScreen)

#define DC_CurrWmRoot(w) \
    (((XmDragContextRec *)(w))->drag.currWmRoot)

#define DC_CurDragOver(w) \
    (((XmDragContextRec *)(w))->drag.curDragOver)

#define DC_OrigDragOver(w) \
    (((XmDragContextRec *)(w))->drag.origDragOver)

#define DC_CurrReceiverInfo(w) \
    (((XmDragContextRec *)(w))->drag.currReceiverInfo)

#define DC_RootReceiverInfo(w) \
    (((XmDragContextRec *)(w))->drag.rootReceiverInfo)

#define DC_ReceiverInfos(w) \
    (((XmDragContextRec *)(w))->drag.receiverInfos)

#define DC_NumReceiverInfos(w) \
    (((XmDragContextRec *)(w))->drag.numReceiverInfos)

#define DC_MaxReceiverInfos(w) \
    (((XmDragContextRec *)(w))->drag.maxReceiverInfos)

#define DC_TrackingMode(w) \
    (((XmDragContextRec *)(w))->drag.trackingMode)

#define DC_ActiveProtocolStyle(w) \
    (((XmDragContextRec *)(w))->drag.activeProtocolStyle)

#define DC_ActiveBlendModel(w) \
    (((XmDragContextRec *)(w))->drag.activeBlendModel)

/*
 * and for the class
 */
#define DCC_DragStartProc(wc) \
    (((XmDragContextClassRec *)(wc))->drag_class.start)

#define DCC_DragCancelProc(wc) \
    (((XmDragContextClassRec *)(wc))->drag_class.cancel)


/*
 * DRAG ICON OBJECT ACCESS MACROS
 */
#define DI_Depth(w) \
    (((XmDragIconRec *)(w))->drag.depth)

#define DI_Pixmap(w) \
    (((XmDragIconRec *)(w))->drag.pixmap)

#define DI_Width(w) \
    (((XmDragIconRec *)(w))->drag.width)

#define DI_Height(w) \
    (((XmDragIconRec *)(w))->drag.height)

#define DI_Mask(w) \
    (((XmDragIconRec *)(w))->drag.mask)

#define DI_HotX(w) \
    (((XmDragIconRec *)(w))->drag.hot_x)

#define DI_HotY(w) \
    (((XmDragIconRec *)(w))->drag.hot_y)

#define DI_Attachment(w) \
    (((XmDragIconRec *)(w))->drag.attachment)

#define DI_IsDirty(w) \
    (((XmDragIconRec *)(w))->drag.isDirty)

#define DI_Region(w) \
    (((XmDragIconRec *)(w))->drag.region)

#define DI_RestoreRegion(w) \
    (((XmDragIconRec *)(w))->drag.restore_region)

#define DI_XOffset(w) \
    (((XmDragIconRec *)(w))->drag.x_offset)

#define DI_YOffset(w) \
    (((XmDragIconRec *)(w))->drag.y_offset)

#define DI_OffsetX(w) \
    (((XmDragIconRec *)(w))->drag.offset_x)

#define DI_OffsetY(w) \
    (((XmDragIconRec *)(w))->drag.offset_y)


/*
 * DRAG OVER WIDGET MACROS
 */
#define DO_HotX(w) \
    (((XmDragOverShellRec *)(w))->drag.hotX)

#define DO_HotY(w) \
    (((XmDragOverShellRec *)(w))->drag.hotY)

#define DO_CursorState(w) \
    (((XmDragOverShellRec *)(w))->drag.cursorState)

#define DO_Mode(w) \
    (((XmDragOverShellRec *)(w))->drag.mode)

#define DO_ActiveMode(w) \
    (((XmDragOverShellRec *)(w))->drag.activeMode)

#define DO_InitialX(w) \
    (((XmDragOverShellRec *)(w))->drag.initialX)

#define DO_InitialY(w) \
    (((XmDragOverShellRec *)(w))->drag.initialY)

#define DO_StateIcon(w) \
    (((XmDragOverShellRec *)(w))->drag.stateIcon)

#define DO_OpIcon(w) \
    (((XmDragOverShellRec *)(w))->drag.opIcon)

#define DO_CursorBlend(w) \
    (((XmDragOverShellRec *)(w))->drag.cursorBlend)

#define DO_RootBlend(w) \
    (((XmDragOverShellRec *)(w))->drag.rootBlend)

#define DO_CursorForeground(w) \
    (((XmDragOverShellRec *)(w))->drag.cursorForeground)

#define DO_CursorBackground(w) \
    (((XmDragOverShellRec *)(w))->drag.cursorBackground)

#define DO_NCCursor(w) \
    (((XmDragOverShellRec *)(w))->drag.ncCursor)

#define DO_ActiveCursor(w) \
    (((XmDragOverShellRec *)(w))->drag.activeCursor)

#define DO_Backing(w) \
    (((XmDragOverShellRec *)(w))->drag.backing)

#define DO_TmpPix(w) \
    (((XmDragOverShellRec *)(w))->drag.tmpPix)

#define DO_TmpBit(w) \
    (((XmDragOverShellRec *)(w))->drag.tmpBit)

#define DO_IsVisible(w) \
    (((XmDragOverShellRec *)(w))->drag.isVisible)


/*
 * DROP SITE MANAGER OBJECT MACROS
 */
#define DS_NotifyProc(w) \
    (((XmDropSiteManagerRec *)(w))->dropManager.notifyProc)

#define DS_TreeUpdateProc(w) \
    (((XmDropSiteManagerRec *)(w))->dropManager.treeUpdateProc)

#define DS_ClientData(w) \
    (((XmDropSiteManagerRec *)(w))->dropManager.client_data)

#define DS_DragUnderData(w) \
    (((XmDropSiteManagerRec *)(w))->dropManager.dragUnderData)

#define DS_CurInfo(w) \
    (((XmDropSiteManagerRec *)(w))->dropManager.curInfo)

#define DS_CurTime(w) \
    (((XmDropSiteManagerRec *)(w))->dropManager.curTime)

#define DS_CurX(w) \
    (((XmDropSiteManagerRec *)(w))->dropManager.curX)

#define DS_CurY(w) \
    (((XmDropSiteManagerRec *)(w))->dropManager.curY)

#define DS_OldX(w) \
    (((XmDropSiteManagerRec *)(w))->dropManager.oldX)

#define DS_OldY(w) \
    (((XmDropSiteManagerRec *)(w))->dropManager.oldY)

#define DS_CurDropSiteStatus(w) \
    (((XmDropSiteManagerRec *)(w))->dropManager.curDropSiteStatus)

#define DS_CurDragContext(w) \
    (((XmDropSiteManagerRec *)(w))->dropManager.curDragContext)

#define DS_CurAnimate(w) \
    (((XmDropSiteManagerRec *)(w))->dropManager.curAnimate)

#define DS_CurOperations(w) \
    (((XmDropSiteManagerRec *)(w))->dropManager.curOperations)

#define DS_CurOperation(w) \
    (((XmDropSiteManagerRec *)(w))->dropManager.curOperation)

#define DS_CurAncestorClipRegion(w) \
    (((XmDropSiteManagerRec *)(w))->dropManager.curAncestorClipRegion)

#define DS_NewAncestorClipRegion(w) \
    (((XmDropSiteManagerRec *)(w))->dropManager.curAncestorClipRegion)

#define DS_DSTable(w) \
    (((XmDropSiteManagerRec *)(w))->dropManager.dsTable)

#define DS_DSRoot(w) \
    (((XmDropSiteManagerRec *)(w))->dropManager.dsRoot)

#define DS_RootX(w) \
    (((XmDropSiteManagerRec *)(w))->dropManager.rootX)

#define DS_RootY(w) \
    (((XmDropSiteManagerRec *)(w))->dropManager.rootY)

#define DS_RootWidth(w) \
    (((XmDropSiteManagerRec *)(w))->dropManager.rootW)

#define DS_RootHeight(w) \
    (((XmDropSiteManagerRec *)(w))->dropManager.rootH)

#define DS_ClipperList(w) \
    (((XmDropSiteManagerRec *)(w))->dropManager.clipperList)

#define DS_UpdateInfo(w) \
    (((XmDropSiteManagerRec *)(w))->dropManager.updateInfo)

/*
 * a few for the class record
 */
#define DSC_CreateInfoProc(wc) \
    (((XmDropSiteManagerClassRec *)(wc))->dropManager_class.createInfo)

#define DSC_DestroyInfoProc(wc) \
    (((XmDropSiteManagerClassRec *)(wc))->dropManager_class.destroyInfo)

#define DSC_StartUpdateProc(wc) \
    (((XmDropSiteManagerClassRec *)(wc))->dropManager_class.startUpdate)

#define DSC_RetrieveInfoProc(wc) \
    (((XmDropSiteManagerClassRec *)(wc))->dropManager_class.retrieveInfo)

#define DSC_UpdateInfoProc(wc) \
    (((XmDropSiteManagerClassRec *)(wc))->dropManager_class.updateInfo)

#define DSC_EndUpdateProc(wc) \
    (((XmDropSiteManagerClassRec *)(wc))->dropManager_class.endUpdate)

#define DSC_UpdateProc(wc) \
    (((XmDropSiteManagerClassRec *)(wc))->dropManager_class.updateDSM)

#define DSC_ProcessMotionProc(wc) \
    (((XmDropSiteManagerClassRec *)(wc))->dropManager_class.processMotion)

#define DSC_ProcessDropProc(wc) \
    (((XmDropSiteManagerClassRec *)(wc))->dropManager_class.processDrop)

#define DSC_OperationChangedProc(wc) \
    (((XmDropSiteManagerClassRec *)(wc))->dropManager_class.operationChanged)

#define DSC_ChangeRootProc(wc) \
    (((XmDropSiteManagerClassRec *)(wc))->dropManager_class.changeRoot)

#define DSC_InsertInfoProc(wc) \
    (((XmDropSiteManagerClassRec *)(wc))->dropManager_class.insertInfo)

#define DSC_RemoveInfoProc(wc) \
    (((XmDropSiteManagerClassRec *)(wc))->dropManager_class.removeInfo)

#define DSC_SyncTreeProc(wc) \
    (((XmDropSiteManagerClassRec *)(wc))->dropManager_class.syncTree)

#define DSC_GetTreeFromDSMProc(wc) \
    (((XmDropSiteManagerClassRec *)(wc))->dropManager_class.getTreeFromDSM)

#define DSC_CreateDSInfoTable(wc) \
    (((XmDropSiteManagerClassRec *)(wc))->dropManager_class.createTable)

#define DSC_DestroyDSInfoTable(wc) \
    (((XmDropSiteManagerClassRec *)(wc))->dropManager_class.destroyTable)

#define DSC_RegisterInfoProc(wc) \
    (((XmDropSiteManagerClassRec *)(wc))->dropManager_class.registerInfo)

#define DSC_WidgetToInfoProc(wc) \
    (((XmDropSiteManagerClassRec *)(wc))->dropManager_class.widgetToInfo)

#define DSC_UnregisterInfoProc(wc) \
    (((XmDropSiteManagerClassRec *)(wc))->dropManager_class.unregisterInfo)


/*
 * DROP TRANSFER OBJECT ACCESS MACROS
 */
#define DT_DropTransfers(w) \
    (((XmDropTransferRec *)(w))->dropTransfer.drop_transfers)

#define DT_NumDropTransfers(w) \
    (((XmDropTransferRec *)(w))->dropTransfer.num_drop_transfers)

#define DT_Selection(w) \
    (((XmDropTransferRec *)(w))->dropTransfer.selection)

#define DT_DragContext(w) \
    (((XmDropTransferRec *)(w))->dropTransfer.dragContext)

#define DT_Timestamp(w) \
    (((XmDropTransferRec *)(w))->dropTransfer.timestamp)

#define DT_Incremental(w) \
    (((XmDropTransferRec *)(w))->dropTransfer.incremental)

#define DT_SourceWindow(w) \
    (((XmDropTransferRec *)(w))->dropTransfer.source_window)

#define DT_Tag(w) \
    (((XmDropTransferRec *)(w))->dropTransfer.tag)

#define DT_TransferCallback(w) \
    (((XmDropTransferRec *)(w))->dropTransfer.transfer_callback)

#define DT_TransferStatus(w) \
    (((XmDropTransferRec *)(w))->dropTransfer.transfer_status)

#define DT_MotifDropAtom(w) \
    (((XmDropTransferRec *)(w))->dropTransfer.motif_drop_atom)

#define DT_DropTransferLists(w) \
    (((XmDropTransferRec *)(w))->dropTransfer.drop_transfer_lists)

#define DT_NumDropTransferLists(w) \
    (((XmDropTransferRec *)(w))->dropTransfer.num_drop_transfer_lists)

#define DT_CurDropTransferList(w) \
    (((XmDropTransferRec *)(w))->dropTransfer.cur_drop_transfer_list)

#define DT_CurXfer(w) \
    (((XmDropTransferRec *)(w))->dropTransfer.cur_xfer)

#define DT_CurTargets(w) \
    (((XmDropTransferRec *)(w))->dropTransfer.cur_targets)

#define DT_CurClientData(w) \
    (((XmDropTransferRec *)(w))->dropTransfer.cur_client_data)

/*
 * a few for the class record
 */
#define DTC_StartTransferProc(wc) \
    (((XmDropTransferClassRec *)(wc))->dropTransfer_class.start_drop_transfer)

#define DTC_AddTransferProc(wc) \
    (((XmDropTransferClassRec *)(wc))->dropTransfer_class.add_drop_transfer)


/*
 * FORM MACROS
 */
#define Form_HorizSpacing(w) \
    (((XmFormRec *)(w))->form.horizontal_spacing)

#define Form_VertSpacing(w) \
    (((XmFormRec *)(w))->form.vertical_spacing)

#define Form_FractionBase(w) \
    (((XmFormRec *)(w))->form.fraction_base)

#define Form_RubberPos(w) \
    (((XmFormRec *)(w))->form.rubber_positioning)

#define Form_FirstChild(w) \
    (((XmFormRec *)(w))->form.first_child)

#define Form_InitialWidth(w) \
    (((XmFormRec *)(w))->form.initial_width)

#define Form_InitialHeight(w) \
    (((XmFormRec *)(w))->form.initial_height)

#define Form_ProcessingConstraints(w) \
    (((XmFormRec *)(w))->form.processing_constraints)

/* constraint_macros */
#define FormC_Atta(w, i) \
    (((XmFormConstraintRec *)((w)->core.constraints))->form.att[(i)])

#define FormC_NextSib(w) \
    (((XmFormConstraintRec *)((w)->core.constraints))->form.next_sibling)

#define FormC_Sorted(w) \
    (((XmFormConstraintRec *)((w)->core.constraints))->form.sorted)

#define FormC_Resizable(w) \
    (((XmFormConstraintRec *)((w)->core.constraints))->form.resizable)

#define FormC_PrefW(w) \
    (((XmFormConstraintRec *)((w)->core.constraints))->form.preferred_width)

#define FormC_PrefH(w) \
    (((XmFormConstraintRec *)((w)->core.constraints))->form.preferred_height)

#define FormC_X(w) \
    (((XmFormConstraintRec *)((w)->core.constraints))->form.att[0].value)

#define FormC_Y(w) \
    (((XmFormConstraintRec *)((w)->core.constraints))->form.att[2].value)

#define FormC_W(x) \
    (((XmFormConstraintRec *)((x)->core.constraints))->form.att[1].value)

#define FormC_H(w) \
    (((XmFormConstraintRec *)((w)->core.constraints))->form.att[3].value)

#define FormC_WFS(w) \
    (((XmFormConstraintRec *)((w)->core.constraints))->form.width_from_side)

#define FormC_HFS(w) \
    (((XmFormConstraintRec *)((w)->core.constraints))->form.height_from_side)

/* different look */
#define FCP_Atta(c, i) \
    (((XmFormConstraintRec *)(c))->form.att[(i)])

#define FCP_NextSib(c) \
    (((XmFormConstraintRec *)(c))->form.next_sibling)

#define FCP_Sorted(c) \
    (((XmFormConstraintRec *)(c))->form.sorted)

#define FCP_Resizable(c) \
    (((XmFormConstraintRec *)(c))->form.resizable)

#define FCP_PrefW(c) \
    (((XmFormConstraintRec *)(c))->form.preferred_width)

#define FCP_PrefH(c) \
    (((XmFormConstraintRec *)(c))->form.preferred_height)

#define FCP_X(c) \
    (((XmFormConstraintRec *)(c))->form.att[0].value)

#define FCP_Y(c) \
    (((XmFormConstraintRec *)(c))->form.att[2].value)

#define FCP_W(c) \
    (((XmFormConstraintRec *)(c))->form.att[1].value)

#define FCP_H(c) \
    (((XmFormConstraintRec *)(c))->form.att[3].value)

#if 0
#define FCP_WidthFromSide(c) \
    (((XmFormConstraintRec *)(c))->form.width_from_side)

#define FCP_HeightFromSide(c) \
    (((XmFormConstraintRec *)(c))->form.height_from_side)
#endif

/*
 * FILE SELECTIONBOX WIDGET ACCESS MACROS
 */
#define FS_DirSpec(w) \
    SB_TextString(w)

#define FS_FileListLabelString(w) \
    SB_ListLabelString(w)

/*
 * FRAME WIDGET ACCESS MACROS
 */
#define Frame_MarginWidth(w) \
    (((XmFrameWidget)w)->frame.margin_width)

#define Frame_MarginHeight(w) \
    (((XmFrameWidget)w)->frame.margin_height)

#define Frame_ShadowType(w) \
    (((XmFrameWidget)w)->frame.shadow_type)

#define Frame_OldWidth(w) \
    (((XmFrameWidget)w)->frame.old_width)

#define Frame_OldHeight(w) \
    (((XmFrameWidget)w)->frame.old_height)

#define Frame_OldShadowThickness(w) \
    (((XmFrameWidget)w)->frame.old_shadow_thickness)

#define Frame_OldShadowX(w) \
    (((XmFrameWidget)w)->frame.old_shadow_x)

#define Frame_OldShadowY(w) \
    (((XmFrameWidget)w)->frame.old_shadow_y)

#define Frame_WorkArea(w) \
    (((XmFrameWidget)w)->frame.work_area)

#define Frame_TitleArea(w) \
    (((XmFrameWidget)w)->frame.title_area)

#define Frame_ProcessingConstraints(w) \
    (((XmFrameWidget)w)->frame.processing_constraints)

/* constraint macros */
#define FrameC_ChildType(w) \
    (((XmFrameConstraintPtr)((w)->core.constraints))->frame.child_type)

#define FrameC_ChildHAlignment(w) \
    (((XmFrameConstraintPtr)((w)->core.constraints))->frame.child_h_alignment)

#define FrameC_ChildHSpacing(w) \
    (((XmFrameConstraintPtr)((w)->core.constraints))->frame.child_h_spacing)

#define FrameC_ChildVAlignment(w) \
    (((XmFrameConstraintPtr)((w)->core.constraints))->frame.child_v_alignment)

/*
 * LABEL WIDGET ACCESS MACROS
 */
#define Lab_RecomputeSize(w) \
       (((XmLabelWidget)(w))->label.recompute_size)

#define Lab_Label(w) \
       (((XmLabelWidget)(w))->label._label)

#define Lab_MnemonicCharset(w) \
       (((XmLabelWidget)(w))->label.mnemonicCharset)

#define Lab_Alignment(w) \
       (((XmLabelWidget)(w))->label.alignment)

#define Lab_Pixmap(w) \
       (((XmLabelWidget)(w))->label.pixmap)

#define Lab_PixmapInsensitive(w) \
       (((XmLabelWidget)(w))->label.pixmap_insen)

#define Lab_LabelType(w) \
       (((XmLabelWidget)(w))->label.label_type)

#define Lab_NormalGC(w) \
       (((XmLabelWidget)(w))->label.normal_GC)

#define Lab_InsensitiveGC(w) \
       (((XmLabelWidget)(w))->label.insensitive_GC)

#define Lab_StringDirection(w) \
       (((XmLabelWidget)(w))->label.string_direction)

#define Lab_AccTextRect(w) \
       (((XmLabelWidget)(w))->label.acc_TextRect)

#define Lab_SkipCallback(w) \
       (((XmLabelWidget)(w))->label.skipCallback)

/* Class macros */
#define LabClass_SetOverrideCallback(c) \
       (((XmLabelWidgetClass)(c))->label_class.setOverrideCallback)

#define LabClass_MenuProcs(c) \
       (((XmLabelWidgetClass)(c))->label_class.menuProcs)

#define LabClass_Translations(c) \
       (((XmLabelWidgetClass)(c))->label_class.translations)

/*
 * LIST WIDGET ACCESS MACROS
 */
#define List_InternalList(w) \
    (((XmListWidget)(w))->list.InternalList)

#define List_Event(w) \
    (((XmListWidget)(w))->list.Event)

#define List_LastItem(w) \
    (((XmListWidget)(w))->list.LastItem)

#define List_FontHeight(w) \
    (((XmListWidget)(w))->list.FontHeight)

#define List_AutoSelect(w) \
    (((XmListWidget)(w))->list.AutoSelect)

#define List_DidSelection(w) \
    (((XmListWidget)(w))->list.DidSelection)

#define List_FromSetSB(w) \
    (((XmListWidget)(w))->list.FromSetSB)

#define List_FromSetNewSize(w) \
    (((XmListWidget)(w))->list.FromSetNewSize)

#define List_LeaveDir(w) \
    (((XmListWidget)(w))->list.LeaveDir)

#define List_BrowseCallback(w) \
    (((XmListWidget)(w))->list.BrowseCallback)

#define List_ExtendCallback(w) \
    (((XmListWidget)(w))->list.ExtendCallback)

#define List_DefaultCallback(w) \
    (((XmListWidget)(w))->list.DefaultCallback)

#define List_ClickInterval(w) \
    (((XmListWidget)(w))->list.ClickInterval)

#define List_DragID(w) \
    (((XmListWidget)(w))->list.DragID)

#define List_Font(w) \
    (((XmListWidget)(w))->list.font)

#define List_ItemCount(w) \
    (((XmListWidget)(w))->list.itemCount)

#define List_SelectedItemCount(w) \
    (((XmListWidget)(w))->list.selectedItemCount)

#define List_VisibleItemCount(w) \
    (((XmListWidget)(w))->list.visibleItemCount)

#define List_LastSetVizCount(w) \
    (((XmListWidget)(w))->list.LastSetVizCount)

#define List_Items(w) \
    (((XmListWidget)(w))->list.items)

#define List_SelectedItems(w) \
    (((XmListWidget)(w))->list.selectedItems)

#if XmVERSION > 1
#define List_SelectedIndices(w) \
    (((XmListWidget)(w))->list.selectedPositions)
#else
#define List_SelectedIndices(w) \
    (((XmListWidget)(w))->list.selectedIndices)
#endif

#define List_MarginHeight(w) \
    (((XmListWidget)(w))->list.margin_height)

#define List_MarginWidth(w) \
    (((XmListWidget)(w))->list.margin_width)

#define List_SizePolicy(w) \
    (((XmListWidget)(w))->list.SizePolicy)

#define List_ItemSpacing(w) \
    (((XmListWidget)(w))->list.ItemSpacing)

#define List_Spacing(w) \
    (((XmListWidget)(w))->list.spacing)

#define List_MultipleCallback(w) \
    (((XmListWidget)(w))->list.MultipleCallback)

#define List_SingleCallback(w) \
    (((XmListWidget)(w))->list.SingleCallback)

#define List_SBDisplayPolicy(w) \
    (((XmListWidget)(w))->list.ScrollBarDisplayPolicy)

#define List_SelectionPolicy(w) \
    (((XmListWidget)(w))->list.SelectionPolicy)

#define List_StrDir(w) \
    (((XmListWidget)(w))->list.StrDir)

#define List_TopPosition(w) \
    (((XmListWidget)(w))->list.top_position)

#define List_AddMode(w) \
    (((XmListWidget)(w))->list.AddMode)

#define List_NormalGC(w) \
    (((XmListWidget)(w))->list.NormalGC)

#define List_InsensitiveGC(w) \
    (((XmListWidget)(w))->list.InsensitiveGC)

#define List_InverseGC(w) \
    (((XmListWidget)(w))->list.InverseGC)

#define List_HighlightGC(w) \
    (((XmListWidget)(w))->list.HighlightGC)

#define List_DashTile(w) \
    (((XmListWidget)(w))->list.DashTile)

#define List_HighlightThickness(w) \
    (((XmListWidget)(w))->list.HighlightThickness)

#define List_LastHLItem(w) \
    (((XmListWidget)(w))->list.LastHLItem)

#define List_StartItem(w) \
    (((XmListWidget)(w))->list.StartItem)

#define List_OldStartItem(w) \
    (((XmListWidget)(w))->list.OldStartItem)

#define List_EndItem(w) \
    (((XmListWidget)(w))->list.EndItem)

#define List_OldEndItem(w) \
    (((XmListWidget)(w))->list.OldEndItem)

#define List_BaseX(w) \
    (((XmListWidget)(w))->list.BaseX)

#define List_BaseY(w) \
    (((XmListWidget)(w))->list.BaseY)

#define List_MouseMoved(w) \
    (((XmListWidget)(w))->list.MouseMoved)

#define List_AppendInProgress(w) \
    (((XmListWidget)(w))->list.AppendInProgress)

#define List_Traversing(w) \
    (((XmListWidget)(w))->list.Traversing)

#define List_KbdSelection(w) \
    (((XmListWidget)(w))->list.KbdSelection)

#define List_DownCount(w) \
    (((XmListWidget)(w))->list.DownCount)

#define List_DownTime(w) \
    (((XmListWidget)(w))->list.DownTime)

#define List_CurrentKbdItem(w) \
    (((XmListWidget)(w))->list.CurrentKbdItem)

#define List_SelectionType(w) \
    (((XmListWidget)(w))->list.SelectionType)

#define List_MaxWidth(w) \
    (((XmListWidget)(w))->list.MaxWidth)

#define List_CharWidth(w) \
    (((XmListWidget)(w))->list.CharWidth)

#define List_XOrigin(w) \
    (((XmListWidget)(w))->list.XOrigin)

#define List_MaxItemHeight(w) \
    (((XmListWidget)(w))->list.MaxItemHeight)

#define List_IsScrolledList(w) \
    (((XmListWidget)(w))->list.Mom != NULL)

#define List_HSB(w) \
    (((XmListWidget)(w))->list.hScrollBar)

#define List_Hmax(w) \
    (((XmListWidget)(w))->list.hmax)

#define List_Horigin(w) \
    (((XmListWidget)(w))->list.hOrigin)

#define List_Hextent(w) \
    (((XmListWidget)(w))->list.hExtent)

#define List_Hmin(w) \
    (((XmListWidget)(w))->list.hmin)

#define List_Mom(w) \
    (((XmListWidget)(w))->list.Mom)

#define List_VSB(w) \
    (((XmListWidget)(w))->list.vScrollBar)

#define List_Vmax(w) \
    (((XmListWidget)(w))->list.vmax)

#define List_Vmin(w) \
    (((XmListWidget)(w))->list.vmin)

#define List_Vorigin(w) \
    (((XmListWidget)(w))->list.vOrigin)

#define List_Vextent(w) \
    (((XmListWidget)(w))->list.vExtent)

/*
 * MAINWINDOW WIDGET ACCESS MACROS
 */
#define MW_AreaWidth(w) \
    (((XmMainWindowWidget)w)->mwindow.AreaWidth)

#define MW_AreaHeight(w) \
    (((XmMainWindowWidget)w)->mwindow.AreaHeight)

#define MW_MarginWidth(w) \
    (((XmMainWindowWidget)w)->mwindow.margin_width)

#define MW_MarginHeight(w) \
    (((XmMainWindowWidget)w)->mwindow.margin_height)

#define MW_CommandWindow(w) \
    (((XmMainWindowWidget)w)->mwindow.CommandWindow)

#define MW_MenuBar(w) \
    (((XmMainWindowWidget)w)->mwindow.MenuBar)

#define MW_MessageWindow(w) \
    (((XmMainWindowWidget)w)->mwindow.Message)

#define MW_CommandLoc(w) \
    (((XmMainWindowWidget)w)->mwindow.CommandLoc)

#define MW_Sep1(w) \
    (((XmMainWindowWidget)w)->mwindow.Sep1)

#define MW_Sep2(w) \
    (((XmMainWindowWidget)w)->mwindow.Sep2)

#define MW_Sep3(w) \
    (((XmMainWindowWidget)w)->mwindow.Sep3)

#define MW_ShowSep(w) \
    (((XmMainWindowWidget)w)->mwindow.ShowSep)

/*
 * MESSAGE BOX WIDGET ACCESS MACROS
 */
#define	MB_DialogType(w) \
    (((XmMessageBoxWidget)w)->message_box.dialog_type)

#define	MB_DefaultType(w) \
    (((XmMessageBoxWidget)w)->message_box.default_type)

#define	MB_MessageString(w) \
    (((XmMessageBoxWidget)w)->message_box.message_string)

#define	MB_OKLabelString(w) \
    (((XmMessageBoxWidget)w)->message_box.ok_label_string)

#define	MB_OKButton(w) \
    (((XmMessageBoxWidget)w)->message_box.ok_button)

#define	MB_OKCall(w) \
    (((XmMessageBoxWidget)w)->message_box.ok_callback)

#define	MB_CancelLabelString(w) \
    (((XmMessageBoxWidget)w)->message_box.cancel_label_string)

#define	MB_CancelCall(w) \
    (((XmMessageBoxWidget)w)->message_box.cancel_callback)

#define	MB_HelpLabelString(w) \
    (((XmMessageBoxWidget)w)->message_box.help_label_string)

#define	MB_HelpButton(w) \
    (((XmMessageBoxWidget)w)->message_box.help_button)

#define	MB_Symbol(w) \
    (((XmMessageBoxWidget)w)->message_box.symbol_wid)

#define	MB_SymbolPixmap(w) \
    (((XmMessageBoxWidget)w)->message_box.symbol_pixmap)

#define	MB_Separator(w) \
    (((XmMessageBoxWidget)w)->message_box.separator)

#define	MB_MessageAlignment(w) \
    (((XmMessageBoxWidget)w)->message_box.message_alignment)

#define	MB_Message(w) \
    (((XmMessageBoxWidget)w)->message_box.message_wid)

#define	MB_MessageString(w) \
    (((XmMessageBoxWidget)w)->message_box.message_string)

#define	MB_MinimizeButtons(w) \
    (((XmMessageBoxWidget)w)->message_box.minimize_buttons)

/*
 * MENUSHELL WIDGET ACCESS MACROS
 */
#define MS_LabelFontList(w) \
    (((XmMenuShellWidget)w)->menu_shell.label_font_list)

#define MS_ButtonFontList(w) \
    (((XmMenuShellWidget)w)->menu_shell.button_font_list)

#define MS_DefaultFontList(w) \
    (((XmMenuShellWidget)w)->menu_shell.default_font_list)

#define MS_FocusData(w) \
    (((XmMenuShellWidget)w)->menu_shell.focus_data)

#define MS_PrivateShell(w) \
    (((XmMenuShellWidget)w)->menu_shell.private_shell)

/* class macros */
#define MSClass_PopdownOne(w) \
    (((XmMenuShellClassRec *)XtClass(w))->menu_shell_class.popdownOne)

#define MSClass_PopdownEveryone(w) \
    (((XmMenuShellClassRec *)XtClass(w))->menu_shell_class.popdownEveryone)

#define MSClass_PopdownDone(w) \
    (((XmMenuShellClassRec *)XtClass(w))->menu_shell_class.popdownDone)

#define MSClass_PopupSharedMenuPane(w) \
    (((XmMenuShellClassRec *)XtClass(w))->menu_shell_class.popupSharedMenupane)

/*
 * PANEDWINDOW WIDGET ACCESS MACROS
 */
#define PW_RefigureMode(w) \
    (((XmPanedWindowWidget)w)->paned_window.refiguremode)

#define PW_SeparatorOn(w) \
    (((XmPanedWindowWidget)w)->paned_window.separator_on)

#define PW_MarginWidth(w) \
    (((XmPanedWindowWidget)w)->paned_window.margin_width)

#define PW_MarginHeight(w) \
    (((XmPanedWindowWidget)w)->paned_window.margin_height)

#define PW_Spacing(w) \
    (((XmPanedWindowWidget)w)->paned_window.spacing)

#define PW_SashWidth(w) \
    (((XmPanedWindowWidget)w)->paned_window.sash_width)

#define PW_SashHeight(w) \
    (((XmPanedWindowWidget)w)->paned_window.sash_height)

#define PW_SashShadowThickness(w) \
    (((XmPanedWindowWidget)w)->paned_window.sash_shadow_thickness)

#define PW_SashIndent(w) \
    (((XmPanedWindowWidget)w)->paned_window.sash_indent)

#define PW_StartY(w) \
    (((XmPanedWindowWidget)w)->paned_window.starty)

#define PW_IncrementCount(w) \
    (((XmPanedWindowWidget)w)->paned_window.increment_count)

#define PW_PaneCount(w) \
    (((XmPanedWindowWidget)w)->paned_window.pane_count)

#define PW_NumSlots(w) \
    (((XmPanedWindowWidget)w)->paned_window.num_slots)

#define PW_NumManagedChildren(w) \
    (((XmPanedWindowWidget)w)->paned_window.num_managed_children)

#define PW_RecursivelyCalled(w) \
    (((XmPanedWindowWidget)w)->paned_window.recursively_called)

#define PW_ResizeAtRealize(w) \
    (((XmPanedWindowWidget)w)->paned_window.resize_at_realize)

#define PW_TopPane(w) \
    (((XmPanedWindowWidget)w)->paned_window.top_pane)

#define PW_BottomPane(w) \
    (((XmPanedWindowWidget)w)->paned_window.bottom_pane)

#define PW_FlipGC(w) \
    (((XmPanedWindowWidget)w)->paned_window.flipgc)

#define PW_ManagedChildren(w) \
    (((XmPanedWindowWidget)w)->paned_window.managed_children)


/*
 * constraint shorthand
 */
#define PWC_Position(w) \
    (((XmPanedWindowConstraintPtr)((w)->core.constraints))->panedw.position)

#define PWC_DHeight(w) \
    (((XmPanedWindowConstraintPtr)((w)->core.constraints))->panedw.dheight)

#define PWC_DY(w) \
    (((XmPanedWindowConstraintPtr)((w)->core.constraints))->panedw.dy)

#define PWC_OldDY(w) \
    (((XmPanedWindowConstraintPtr)((w)->core.constraints))->panedw.olddy)

#define PWC_PaneMaximum(w) \
    (((XmPanedWindowConstraintPtr)((w)->core.constraints))->panedw.max)

#define PWC_PaneMinimum(w) \
    (((XmPanedWindowConstraintPtr)((w)->core.constraints))->panedw.min)

#define PWC_IsPane(w) \
    (((XmPanedWindowConstraintPtr)((w)->core.constraints))->panedw.isPane)

#define PWC_AllowResize(w) \
    (((XmPanedWindowConstraintPtr)((w)->core.constraints))->panedw.allow_resize)

#define PWC_SkipAdjust(w) \
    (((XmPanedWindowConstraintPtr)((w)->core.constraints))->panedw.skip_adjust)

#define PWC_Sash(w) \
    (((XmPanedWindowConstraintPtr)((w)->core.constraints))->panedw.sash)

#define PWC_Separator(w) \
    (((XmPanedWindowConstraintPtr)((w)->core.constraints))->panedw.separator)

#define PWC_PositionIndex(w) \
    (((XmPanedWindowConstraintPtr)((w)->core.constraints))->panedw.position_index)

/*
 * PUSH BUTTON WIDGET ACCESS MACROS
 */
#define PB_ActivateCallback(w) \
    (((XmPushButtonWidget)(w))->pushbutton.activate_callback)

#define PB_ArmCallback(w) \
    (((XmPushButtonWidget)(w))->pushbutton.arm_callback)

#define PB_DisarmCallback(w) \
    (((XmPushButtonWidget)(w))->pushbutton.disarm_callback)

#define PB_Armed(w) \
    (((XmPushButtonWidget)(w))->pushbutton.armed)

#define PB_ClickCount(w) \
    (((XmPushButtonWidget)(w))->pushbutton.click_count)

#define PB_Compatible(w) \
    (((XmPushButtonWidget)(w))->pushbutton.compatible)

#define PB_ShowAsDefault(w) \
    (((XmPushButtonWidget)(w))->pushbutton.show_as_default)

#define PB_DefaultButtonShadow(w) \
    (((XmPushButtonWidget)(w))->pushbutton.default_button_shadow_thickness)

#define PB_FillOnArm(w) \
    (((XmPushButtonWidget)(w))->pushbutton.fill_on_arm)

#define PB_ArmColor(w) \
    (((XmPushButtonWidget)(w))->pushbutton.arm_color)

#define PB_FillGC(w) \
    (((XmPushButtonWidget)(w))->pushbutton.fill_gc)

#define PB_BackgroundGC(w) \
    (((XmPushButtonWidget)(w))->pushbutton.background_gc)

#define PB_Timer(w) \
    (((XmPushButtonWidget)(w))->pushbutton.timer)

#define PB_ArmTimeStamp(w) \
    (((XmPushButtonWidget)(w))->pushbutton.armTimeStamp)

#define PB_ArmPixmap(w) \
    (((XmPushButtonWidget)(w))->pushbutton.arm_pixmap)

#define PB_UnarmPixmap(w) \
    (((XmPushButtonWidget)(w))->pushbutton.unarm_pixmap)

#define PB_MultiClick(w) \
    (((XmPushButtonWidget)(w))->pushbutton.multiClick)

/*
 * PROTOCOL OBJECT ACCESS MACROS
 */
#define Protocol_PreHook(w) \
    (((XmProtocol)(w))->protocol.pre_hook)

#define Protocol_PostHook(w) \
    (((XmProtocol)(w))->protocol.post_hook)

#define Protocol_Callbacks(w) \
    (((XmProtocol)(w))->protocol.callbacks)

#define Protocol_Atom(w) \
    (((XmProtocol)(w))->protocol.atom)

#define Protocol_Active(w) \
    (((XmProtocol)(w))->protocol.active)

/*
 * ROWCOLUMN WIDGET ACCESS MACROS
 */
#define RC_OldWidth(m) \
    (((XmRowColumnWidget)(m))->row_column.old_width)

#define RC_OldHeight(m) \
    (((XmRowColumnWidget)(m))->row_column.old_height)

#define RC_OldShadowThickness(m) \
    (((XmRowColumnWidget)(m))->row_column.old_shadow_thickness)

#define RC_LastSelectToplevel(m) \
    (((XmRowColumnWidget)(m))->row_column.lastSelectToplevel)

#define RC_TearOffActivatedCallback(m) \
    (((XmRowColumnWidget)(m))->row_column.tear_off_activated_callback)

#define RC_TearOffDeactivatedCallback(m) \
    (((XmRowColumnWidget)(m))->row_column.tear_off_deactivated_callback)

#define RC_constraint_IndexPosition(w) \
    (((XmRowColumnConstraintRec *)(w)->core.constraints)->row_column.position_index)

#define RC_PostFromList(w) \
    (((XmRowColumnWidget)(w))->row_column.postFromList)

#define RC_PostFromCount(w) \
    (((XmRowColumnWidget)(w))->row_column.postFromCount)

#define RC_PostFromListSize(w) \
    (((XmRowColumnWidget)(w))->row_column.postFromListSize)

#define RC_PopupTimer(w) \
    (((XmRowColumnWidget)(w))->row_column.popup_timeout_timer)

#define	RC_IsAligned(w)	\
    (((XmRowColumnWidget)(w))->row_column.do_alignment)

#define RC_PopupMenuClick(m) \
    (((XmRowColumnWidget)(m))->row_column.popup_menu_click)


#define RCC_WasManaged(cw) \
    (((XmRowColumnConstraintRec *)(CoreConstraints(cw)))->row_column.was_managed)

#define RCC_MarginTop(cw) \
    (((XmRowColumnConstraintRec *)(CoreConstraints(cw)))->row_column.margin_top)

#define RCC_MarginBottom(cw) \
    (((XmRowColumnConstraintRec *)(CoreConstraints(cw)))->row_column.margin_bottom)

#define RCC_Baseline(cw) \
    (((XmRowColumnConstraintRec *)(CoreConstraints(cw)))->row_column.baseline)

#define RCC_PositionIndex(cw) \
    (((XmRowColumnConstraintRec *)(CoreConstraints(cw)))->row_column.position_index)

/* class macros */
#define RCClass_MenuProcs(c) \
    (((XmRowColumnClassRec *)(c))->row_column_class.menuProcedures)

#define RCClass_MenuTraverse(c, dir) \
    (((XmRowColumnClassRec *)(XtClass(c)))->row_column_class.menuProcedures)(XmMENU_TRAVERSAL, c, dir)

#define RCClass_ArmAndActivate(c) \
    (((XmRowColumnClassRec *)(c))->row_column_class.armAndActivate)

#define RCClass_TraversalHandler(c) \
    (((XmRowColumnClassRec *)(c))->row_column_class.traversalHandler)

/*
 * Short cuts to the Label class menu procedures
 */
#define LabC_MenuArm(w) (((XmLabelClassRec *)(XtClass(w)))->label_class.menuProcs)(XmMENU_ARM, w)
#define LabGC_MenuArm(w) (((XmLabelGadgetClassRec *)(XtClass(w)))->label_class.menuProcs)(XmMENU_ARM, w)
#define Lab_MenuArm(w) (XmIsGadget(w) ? LabGC_MenuArm(w): LabC_MenuArm(w))

#define LabC_MenuDisarm(w) (((XmLabelClassRec *)(XtClass(w)))->label_class.menuProcs)(XmMENU_DISARM, w)
#define LabGC_MenuDisarm(w) (((XmLabelGadgetClassRec *)(XtClass(w)))->label_class.menuProcs)(XmMENU_DISARM, w)
#define Lab_MenuDisarm(w) (XmIsGadget(w) ? LabGC_MenuDisarm(w): LabC_MenuDisarm(w))
/*
 * Short cuts to the RowColumn class menu procedures
 */
#define RC_MenuButton(w,e,v) (((XmRowColumnClassRec *)(XtClass(XtParent(w))))->row_column_class.menuProcedures)(XmMENU_BUTTON, w, e, v)
#define RC_MenuButtonPopdown(w,e,v) (((XmRowColumnClassRec *)(XtClass(XtParent(w))))->row_column_class.menuProcedures)(XmMENU_BUTTON_POPDOWN, w, e, v)
#define RC_MenuShellPopdown(w,e,v) (((XmRowColumnClassRec *)(XtClass(XtParent(w))))->row_column_class.menuProcedures)(XmMENU_SHELL_POPDOWN, w, e, v)
#define RC_MenuSubmenu(w) (((XmRowColumnClassRec *)(XtClass(XtParent(w))))->row_column_class.menuProcedures)(XmMENU_SUBMENU, w)
#define RC_MenuCascading(w,e) (((XmRowColumnClassRec *)(XtClass(XtParent(w))))->row_column_class.menuProcedures)(XmMENU_CASCADING, w, e)
#define RC_MenuMenuCallback(w,cbs) (((XmRowColumnClassRec *)(XtClass(XtParent(w))))->row_column_class.menuProcedures)(XmMENU_CALLBACK, w, cbs)

/*
 * SASH WIDGET ACCESS MACROS
 */
#define Sash_SashAction(w) \
    (((XmSashWidget)w)->sash.sash_action)

#define Sash_HasFocus(w) \
    (((XmSashWidget)w)->sash.has_focus)

/*
 * SCREEN WIDGET ACCESS MACROS
 */
#define Screen_Parent(w) \
    (((XmScreen)w)->desktop.parent)

#define Screen_Children(w) \
    (((XmScreen)w)->desktop.children)

#define Screen_NumChildren(w) \
    (((XmScreen)w)->desktop.num_children)

#define Screen_NumSlots(w) \
    (((XmScreen)w)->desktop.num_slots)

#define Screen_MwmPresent(w) \
    (((XmScreen)w)->screen.mwmPresent)

#define Screen_NumReparented(w) \
    (((XmScreen)w)->screen.numReparented)

#define Screen_DarkThreshold(w) \
    (((XmScreen)w)->screen.darkThreshold)

#define Screen_ForegroundThreshold(w) \
    (((XmScreen)w)->screen.foregroundThreshold)

#define Screen_LightThreshold(w) \
    (((XmScreen)w)->screen.lightThreshold)

#define Screen_DefaultNoneCursorIcon(w) \
    (((XmScreen)w)->screen.defaultNoneCursorIcon)

#define Screen_DefaultValidCursorIcon(w) \
    (((XmScreen)w)->screen.defaultValidCursorIcon)

#define Screen_DefaultInvalidCursorIcon(w) \
    (((XmScreen)w)->screen.defaultInvalidCursorIcon)

#define Screen_DefaultMoveCursorIcon(w) \
    (((XmScreen)w)->screen.defaultMoveCursorIcon)

#define Screen_DefaultCopyCursorIcon(w) \
    (((XmScreen)w)->screen.defaultCopyCursorIcon)

#define Screen_DefaultLinkCursorIcon(w) \
    (((XmScreen)w)->screen.defaultLinkCursorIcon)

#define Screen_DefaultSourceCursorIcon(w) \
    (((XmScreen)w)->screen.defaultSourceCursorIcon)

#define Screen_NullCursor(w) \
    (((XmScreen)w)->screen.nullCursor)

#define Screen_CursorCache(w) \
    (((XmScreen)w)->screen.cursorCache)

#define Screen_MaxCursorWidth(w) \
    (((XmScreen)w)->screen.maxCursorWidth)

#define Screen_MaxCursorHeight(w) \
    (((XmScreen)w)->screen.maxCursorHeight)

#define Screen_MenuCursor(w) \
    (((XmScreen)w)->screen.menuCursor)

#define Screen_UnpostBehavior(w) \
    (((XmScreen)w)->screen.unpostBehavior)

#define Screen_FontStruct(w) \
    (((XmScreen)w)->screen.font_struct)

#define Screen_HorizUnit(w) \
    (((XmScreen)w)->screen.h_unit)

#define Screen_VertUnit(w) \
    (((XmScreen)w)->screen.v_unit)

#define Screen_ScratchPixmaps(w) \
    (((XmScreen)w)->screen.scratchPixmaps)

#define Screen_MoveOpaque(w) \
    (((XmScreen)w)->screen.moveOpaque)

#define Screen_StateCursorIcon(w) \
    (((XmScreen)w)->screen.xmStateCursorIcon)

#define Screen_MoveCursorIcon(w) \
    (((XmScreen)w)->screen.xmMoveCursorIcon)

#define Screen_CopyCursorIcon(w) \
    (((XmScreen)w)->screen.xmCopyCursorIcon)

#define Screen_LinkCursorIcon(w) \
    (((XmScreen)w)->screen.xmLinkCursorIcon)

#define Screen_SourceCursorIcon(w) \
    (((XmScreen)w)->screen.xmSourceCursorIcon)

#define Screen_ImageGC(w) \
    (((XmScreen)w)->screen.imageGC)

#define Screen_ImageGCDepth(w) \
    (((XmScreen)w)->screen.imageGCDepth)

#define Screen_ImageForeground(w) \
    (((XmScreen)w)->screen.imageForeground)

#define Screen_ImageBackground(w) \
    (((XmScreen)w)->screen.imageBackground)

#define Screen_ScreenInfo(w) \
    (((XmScreen)w)->screen.screenInfo)

/*
 * SCALE WIDGET ACCESS MACROS
 */
#define Scale_HighlightThickness(x) \
    (((XmScaleWidget)(x))->scale.highlight_thickness)

#define Scale_HighlightOnEnter(x) \
    (((XmScaleWidget)(x))->scale.highlight_on_enter)

#define Scale_ForegroundGC(x) \
    (((XmScaleWidget)(x))->scale.foreground_GC)

#define Scale_Orientation(x) \
    (((XmScaleWidget)(x))->scale.orientation)

#define Scale_ProcessingDirection(x) \
    (((XmScaleWidget)(x))->scale.processing_direction)

#define Scale_DecimalPoints(x) \
    (((XmScaleWidget)(x))->scale.decimal_points)

#define Scale_ScaleWidth(x) \
    (((XmScaleWidget)(x))->scale.scale_width)

#define Scale_ScaleHeight(x) \
    (((XmScaleWidget)(x))->scale.scale_height)

#define Scale_Minimum(x) \
    (((XmScaleWidget)(x))->scale.minimum)

#define Scale_Maximum(x) \
    (((XmScaleWidget)(x))->scale.maximum)

#define Scale_Value(x) \
    (((XmScaleWidget)(x))->scale.value)

#define Scale_LastValue(x) \
    (((XmScaleWidget)(x))->scale.last_value)

#define Scale_ShowValue(x) \
    (((XmScaleWidget)(x))->scale.show_value)

#define Scale_ShowValueX(x) \
    (((XmScaleWidget)(x))->scale.show_value_x)

#define Scale_ShowValueY(x) \
    (((XmScaleWidget)(x))->scale.show_value_y)

#define Scale_ShowValueWidth(x) \
    (((XmScaleWidget)(x))->scale.show_value_width)

#define Scale_ShowValueHeight(x) \
    (((XmScaleWidget)(x))->scale.show_value_height)

#define Scale_FontList(x) \
    (((XmScaleWidget)(x))->scale.font_list)

#define Scale_FontStruct(x) \
    (((XmScaleWidget)(x))->scale.font_struct)

#define Scale_ScaleMultiple(x) \
    (((XmScaleWidget)(x))->scale.scale_multiple)

#define Scale_Title(x) \
    (((XmScaleWidget)(x))->scale.title)

#define Scale_SliderSize(x) \
    (((XmScaleWidget)(x))->scale.slider_size)

#define Scale_ValueChangedCallback(x) \
    (((XmScaleWidget)(x))->scale.value_changed_callback)

#define Scale_DragCallback(x) \
    (((XmScaleWidget)(x))->scale.drag_callback)

#define	Scale_Editable(x) \
    (((XmScaleWidget)(x))->scale.editable)

/*
 * SELECTIONBOX WIDGET ACCESS MACROS
 */
#define SB_DialogType(w) \
	(((XmSelectionBoxWidget) (w))->selection_box.dialog_type)

#define SB_ListItems(w) \
	(((XmSelectionBoxWidget)(w))->selection_box.list_items)

#define SB_ChildPlacement(w) \
	(((XmSelectionBoxWidget)(w))->selection_box.child_placement)

#define SB_OkCallback(w) \
	(((XmSelectionBoxWidget)(w))->selection_box.ok_callback)

#define SB_CancelCallback(w) \
	(((XmSelectionBoxWidget)(w))->selection_box.cancel_callback)

#define SB_ApplyCallback(w) \
	(((XmSelectionBoxWidget)(w))->selection_box.apply_callback)

#define SB_NoMatchCallback(w) \
	(((XmSelectionBoxWidget)(w))->selection_box.no_match_callback)

#define SB_ListLabelString(w) \
	(((XmSelectionBoxWidget) (w))->selection_box.list_label_string)

#define SB_TextString(w) \
	(((XmSelectionBoxWidget) (w))->selection_box.text_string)

#define SB_OkLabelString(w) \
	(((XmSelectionBoxWidget) (w))->selection_box.ok_label_string)

#define SB_CancelLabelString(w) \
	(((XmSelectionBoxWidget) (w))->selection_box.cancel_label_string)

#define SB_ApplyLabelString(w) \
	(((XmSelectionBoxWidget) (w))->selection_box.apply_label_string)

#define SB_HelpLabelString(w) \
	(((XmSelectionBoxWidget) (w))->selection_box.help_label_string)

#define SB_SelectionLabelString(w) \
	(((XmSelectionBoxWidget) (w))->selection_box.selection_label_string)

/*
 * SCROLLBAR WIDGET ACCESS MACROS
 */
#define SCB_Value(w) \
    (((XmScrollBarWidget)(w))->scrollBar.value)

#define SCB_Minimum(w) \
    (((XmScrollBarWidget)(w))->scrollBar.minimum)

#define SCB_Maximum(w) \
    (((XmScrollBarWidget)(w))->scrollBar.maximum)

#define SCB_SliderSize(w) \
    (((XmScrollBarWidget)(w))->scrollBar.slider_size)

#define SCB_Orientation(w) \
    (((XmScrollBarWidget)(w))->scrollBar.orientation)

#define SCB_ProcessingDirection(w) \
    (((XmScrollBarWidget)(w))->scrollBar.processing_direction)

#define SCB_ShowArrows(w) \
    (((XmScrollBarWidget)(w))->scrollBar.show_arrows)

#define SCB_Increment(w) \
    (((XmScrollBarWidget)(w))->scrollBar.increment)

#define SCB_PageIncrement(w) \
    (((XmScrollBarWidget)(w))->scrollBar.page_increment)

#define SCB_InitialDelay(w) \
    (((XmScrollBarWidget)(w))->scrollBar.initial_delay)

#define SCB_RepeatDelay(w) \
    (((XmScrollBarWidget)(w))->scrollBar.repeat_delay)

#define SCB_ValueChangedCallback(w) \
    (((XmScrollBarWidget)(w))->scrollBar.value_changed_callback)

#define SCB_IncrementCallback(w) \
    (((XmScrollBarWidget)(w))->scrollBar.increment_callback)

#define SCB_DecrementCallback(w) \
    (((XmScrollBarWidget)(w))->scrollBar.decrement_callback)

#define SCB_PageIncrementCallback(w) \
    (((XmScrollBarWidget)(w))->scrollBar.page_increment_callback)

#define SCB_PageDecrementCallback(w) \
    (((XmScrollBarWidget)(w))->scrollBar.page_decrement_callback)

#define SCB_ToTopCallback(w) \
    (((XmScrollBarWidget)(w))->scrollBar.to_top_callback)

#define SCB_ToBottomCallback(w) \
    (((XmScrollBarWidget)(w))->scrollBar.to_bottom_callback)

#define SCB_DragCallback(w) \
    (((XmScrollBarWidget)(w))->scrollBar.drag_callback)

#define SCB_ForegroundGC(w) \
    (((XmScrollBarWidget)(w))->scrollBar.foreground_GC)

#define SCB_TroughColor(w) \
    (((XmScrollBarWidget)(w))->scrollBar.trough_color)

#define SCB_Pixmap(w) \
    (((XmScrollBarWidget)(w))->scrollBar.pixmap)

#define SCB_SlidingOn(w) \
    (((XmScrollBarWidget)(w))->scrollBar.sliding_on)

#define SCB_EtchedSlider(w) \
    (((XmScrollBarWidget)(w))->scrollBar.etched_slider)

#define SCB_SavedValue(w) \
    (((XmScrollBarWidget)(w))->scrollBar.saved_value)

#define SCB_Flags(w) \
    (((XmScrollBarWidget)(w))->scrollBar.flags)

#define SCB_ChangeType(w) \
    (((XmScrollBarWidget)(w))->scrollBar.change_type)

#define SCB_Timer(w) \
    (((XmScrollBarWidget)(w))->scrollBar.timer)

#define SCB_InitialX(w) \
    (((XmScrollBarWidget)(w))->scrollBar.initial_x)

#define SCB_InitialY(w) \
    (((XmScrollBarWidget)(w))->scrollBar.initial_y)

#define SCB_SeparationX(w) \
    (((XmScrollBarWidget)(w))->scrollBar.separation_x)

#define SCB_SeparationY(w) \
    (((XmScrollBarWidget)(w))->scrollBar.separation_y)

#define SCB_SliderX(w) \
    (((XmScrollBarWidget)(w))->scrollBar.slider_x)

#define SCB_SliderY(w) \
    (((XmScrollBarWidget)(w))->scrollBar.slider_y)

#define SCB_SliderWidth(w) \
    (((XmScrollBarWidget)(w))->scrollBar.slider_width)

#define SCB_SliderHeight(w) \
    (((XmScrollBarWidget)(w))->scrollBar.slider_height)

#define SCB_SliderAreaX(w) \
    (((XmScrollBarWidget)(w))->scrollBar.slider_area_x)

#define SCB_SliderAreaY(w) \
    (((XmScrollBarWidget)(w))->scrollBar.slider_area_y)

#define SCB_SliderAreaWidth(w) \
    (((XmScrollBarWidget)(w))->scrollBar.slider_area_width)

#define SCB_SliderAreaHeight(w) \
    (((XmScrollBarWidget)(w))->scrollBar.slider_area_height)

#define SCB_Arrow1X(w) \
    (((XmScrollBarWidget)(w))->scrollBar.arrow1_x)

#define SCB_Arrow1Y(w) \
    (((XmScrollBarWidget)(w))->scrollBar.arrow1_y)

#define SCB_Arrow1Orientation(w) \
    (((XmScrollBarWidget)(w))->scrollBar.arrow1_orientation)

#define SCB_Arrow1Selected(w) \
    (((XmScrollBarWidget)(w))->scrollBar.arrow1_selected)

#define SCB_Arrow2X(w) \
    (((XmScrollBarWidget)(w))->scrollBar.arrow2_x)

#define SCB_Arrow2Y(w) \
    (((XmScrollBarWidget)(w))->scrollBar.arrow2_y)

#define SCB_Arrow2Orientation(w) \
    (((XmScrollBarWidget)(w))->scrollBar.arrow2_orientation)

#define SCB_Arrow2Selected(w) \
    (((XmScrollBarWidget)(w))->scrollBar.arrow2_selected)

#define SCB_ArrowWidth(w) \
    (((XmScrollBarWidget)(w))->scrollBar.arrow_width)

#define SCB_ArrowHeight(w) \
    (((XmScrollBarWidget)(w))->scrollBar.arrow_height)

#define SCB_UnavailableGC(w) \
    (((XmScrollBarWidget)(w))->scrollBar.unavailable_GC)

#define SCB_Offset(w) \
    (((XmScrollBarWidget)(w))->scrollBar.offset)

/*
 * SEPARATOR WIDGET ACCESS MACROS
 */
#define SEP_Margin(w) \
    (((XmSeparatorWidget)(w))->separator.margin)

#define SEP_Orientation(w) \
    (((XmSeparatorWidget)(w))->separator.orientation)

#define SEP_SeparatorType(w) \
    (((XmSeparatorWidget)(w))->separator.separator_type)

#define SEP_SeparatorGC(w) \
    (((XmSeparatorWidget)(w))->separator.separator_GC)

/*
 * SCROLLEDWINDOW WIDGET ACCESS MACROS
 */
#define SW_VSBMinimum(w) \
    (((XmScrolledWindowWidget)w)->swindow.vmin)

#define SW_VSBMaximum(w) \
    (((XmScrolledWindowWidget)w)->swindow.vmax)

#define SW_VSBValue(w) \
    (((XmScrolledWindowWidget)w)->swindow.vOrigin)

#define SW_VSBSliderSize(w) \
    (((XmScrolledWindowWidget)w)->swindow.vExtent)

#define SW_HSBMinimum(w) \
    (((XmScrolledWindowWidget)w)->swindow.hmin)

#define SW_HSBMaximum(w) \
    (((XmScrolledWindowWidget)w)->swindow.hmax)

#define SW_HSBValue(w) \
    (((XmScrolledWindowWidget)w)->swindow.hOrigin)

#define SW_HSBSliderSize(w) \
    (((XmScrolledWindowWidget)w)->swindow.hExtent)

#define SW_HSBX(w) \
    (((XmScrolledWindowWidget)w)->swindow.hsbX)

#define SW_HSBY(w) \
    (((XmScrolledWindowWidget)w)->swindow.hsbY)

#define SW_HSBWidth(w) \
    (((XmScrolledWindowWidget)w)->swindow.hsbWidth)

#define SW_HSBHeight(w) \
    (((XmScrolledWindowWidget)w)->swindow.hsbHeight)

#define SW_VSBX(w) \
    (((XmScrolledWindowWidget)w)->swindow.vsbX)

#define SW_VSBY(w) \
    (((XmScrolledWindowWidget)w)->swindow.vsbY)

#define SW_VSBWidth(w) \
    (((XmScrolledWindowWidget)w)->swindow.vsbWidth)

#define SW_VSBHeight(w) \
    (((XmScrolledWindowWidget)w)->swindow.vsbHeight)

#define SW_GivenWidth(w) \
    (((XmScrolledWindowWidget)w)->swindow.GivenWidth)

#define SW_GivenHeight(w) \
    (((XmScrolledWindowWidget)w)->swindow.GivenHeight)

#define SW_CWX(w) \
    (((XmScrolledWindowWidget)w)->swindow.XOffset)

#define SW_CWY(w) \
    (((XmScrolledWindowWidget)w)->swindow.YOffset)

#define SW_XOffset(w) \
    (((XmScrolledWindowWidget)w)->swindow.XOffset)

#define SW_YOffset(w) \
    (((XmScrolledWindowWidget)w)->swindow.YOffset)

#define SW_CWWidth(w) \
    (((XmScrolledWindowWidget)w)->swindow.AreaWidth)

#define SW_CWHeight(w) \
    (((XmScrolledWindowWidget)w)->swindow.AreaHeight)

#define SW_MarginWidth(w) \
    (((XmScrolledWindowWidget)w)->swindow.WidthPad)

#define SW_MarginHeight(w) \
    (((XmScrolledWindowWidget)w)->swindow.HeightPad)

#define SW_Spacing(w) \
    (((XmScrolledWindowWidget)w)->swindow.pad)

#define SW_HasHSB(w) \
    (((XmScrolledWindowWidget)w)->swindow.hasHSB)

#define SW_HasVSB(w) \
    (((XmScrolledWindowWidget)w)->swindow.hasVSB)

#define SW_InInit(w) \
    (((XmScrolledWindowWidget)w)->swindow.InInit)

#define SW_FromResize(w) \
    (((XmScrolledWindowWidget)w)->swindow.FromResize)

#define SW_VisualPolicy(w) \
    (((XmScrolledWindowWidget)w)->swindow.VisualPolicy)

#define SW_ScrollPolicy(w) \
    (((XmScrolledWindowWidget)w)->swindow.ScrollPolicy)

#define SW_ScrollBarPolicy(w) \
    (((XmScrolledWindowWidget)w)->swindow.ScrollBarPolicy)

#define SW_Placement(w) \
    (((XmScrolledWindowWidget)w)->swindow.Placement)

#define SW_HSB(w) \
    (((XmScrolledWindowWidget)w)->swindow.hScrollBar)

#define SW_VSB(w) \
    (((XmScrolledWindowWidget)w)->swindow.vScrollBar)

#define SW_ClipWindow(w) \
    (((XmScrolledWindowWidget)w)->swindow.ClipWindow)

#define SW_WorkWindow(w) \
    (((XmScrolledWindowWidget)w)->swindow.WorkWindow)

#define SW_TraverseObscuredCallback(w) \
    (((XmScrolledWindowWidget)w)->swindow.traverseObscuredCallback)

/*
 * TOGGLE BUTTON WIDGET ACCESS MACROS
 */
#define TB_IndType(w) \
    (((XmToggleButtonWidget)(w))->toggle.ind_type)

#define TB_Visible(w) \
    (((XmToggleButtonWidget)(w))->toggle.visible)

#define TB_Spacing(w) \
    (((XmToggleButtonWidget)(w))->toggle.spacing)

#define TB_IndicatorDim(w) \
    (((XmToggleButtonWidget)(w))->toggle.indicator_dim)

#define TB_IndicatorSet(w) \
    (((XmToggleButtonWidget)(w))->toggle.indicator_set)

#define TB_OnPixmap(w) \
    (((XmToggleButtonWidget)(w))->toggle.on_pixmap)

#define TB_InsenPixmap(w) \
    (((XmToggleButtonWidget)(w))->toggle.insen_pixmap)

#define TB_Set(w) \
    (((XmToggleButtonWidget)(w))->toggle.set)

#define TB_VisualSet(w) \
    (((XmToggleButtonWidget)(w))->toggle.visual_set)

#define TB_IndOn(w) \
    (((XmToggleButtonWidget)(w))->toggle.ind_on)

#define TB_FillOnSelect(w) \
    (((XmToggleButtonWidget)(w))->toggle.fill_on_select)

#define TB_SelectColor(w) \
    (((XmToggleButtonWidget)(w))->toggle.select_color)

#define TB_SelectGC(w) \
    (((XmToggleButtonWidget)(w))->toggle.select_GC)

#define TB_BackgroundGC(w) \
    (((XmToggleButtonWidget)(w))->toggle.background_gc)

#define TB_ArmCallback(w) \
    (((XmToggleButtonWidget)(w))->toggle.arm_CB)

#define TB_ValueChangedCallback(w) \
    (((XmToggleButtonWidget)(w))->toggle.value_changed_CB)

#define TB_DisarmCallback(w) \
    (((XmToggleButtonWidget)(w))->toggle.disarm_CB)

#define TB_Armed(w) \
    (((XmToggleButtonWidget)(w))->toggle.Armed)

/*
 * TEAROFF BUTTON WIDGET ACCESS MACROS
 */
#define TOB_Margin(w) \
   (((XmTearOffButtonWidget)(w))->tear_off_button.margin)

#define TOB_Orientation(w) \
   (((XmTearOffButtonWidget)(w))->tear_off_button.orientation)

#define TOB_SeparatorType(w) \
   (((XmTearOffButtonWidget)(w))->tear_off_button.separator_type)

#define TOB_SeparatorGC(w) \
   (((XmTearOffButtonWidget)(w))->tear_off_button.separator_GC)


/*
 * XmDISPLAY ACCESS MACROS
 */
#define Display_DragInitiatorProtocolStyle(w) \
   (((XmDisplay)(w))->display.dragInitiatorProtocolStyle)

#define Display_DragReceiverProtocolStyle(w) \
   (((XmDisplay)(w))->display.dragReceiverProtocolStyle)

#define Display_UserGrabbed(w) \
   (((XmDisplay)(w))->display.userGrabbed)

#define Display_DragContextClass(w) \
   (((XmDisplay)(w))->display.dragContextClass)

#define Display_DropTransferClass(w) \
   (((XmDisplay)(w))->display.dropTransferClass)

#define Display_DropSiteManagerClass(w) \
   (((XmDisplay)(w))->display.dropSiteManagerClass)

#define Display_ActiveDC(w) \
   (((XmDisplay)(w))->display.activeDC)

#define Display_DSM(w) \
   (((XmDisplay)(w))->display.dsm)

#define Display_LastDragTime(w) \
   (((XmDisplay)(w))->display.lastDragTime)

#define Display_ProxyWindow(w) \
   (((XmDisplay)(w))->display.proxyWindow)

#define Display_Modals(w) \
   (((XmDisplay)(w))->display.modals)

#define Display_NumModals(w) \
   (((XmDisplay)(w))->display.numModals)

#define Display_MaxModals(w) \
   (((XmDisplay)(w))->display.maxModals)

#define Display_Modals(w) \
   (((XmDisplay)(w))->display.modals)

#define Display_XmImInfo(w) \
   (((XmDisplay)(w))->display.xmim_info)

#define Display_BindingsString(w) \
   (((XmDisplay)(w))->display.bindingsString)

#define Display_Bindings(w) \
   (((XmDisplay)(w))->display.modals)

#define Display_LastKeyEvent(w) \
   (((XmDisplay)(w))->display.lastKeyEvent)

#define Display_KeycodeTag(w) \
   (((XmDisplay)(w))->display.keycode_tag)

#define Display_ShellCount(w) \
   (((XmDisplay)(w))->display.shellCount)

#define Display_DisplayInfo(w) \
   (((XmDisplay)(w))->display.displayInfo)


/*
 * Desktop class
 */
#define Desktop_Parent(w) \
   (((XmDesktopRec *)(w))->desktop.parent)

#define Desktop_Children(w) \
   (((XmDesktopRec *)(w))->desktop.children)

#define Desktop_NumChildren(w) \
   (((XmDesktopRec *)(w))->desktop.num_children)

#define Desktop_NumSlots(w) \
   (((XmDesktopRec *)(w))->desktop.num_slots)

/*
 * Convenient shorthands.
 */
#define ColormapOfObject(w) \
    (XmIsGadget(w) \
	? CoreColormap(((XmGadget)(w))->object.parent) \
	: CoreColormap(w))

/*
 * Sometimes XmStrings are NULL and sometimes they are (XmString)XmUNSPECIFIED,
 * both mean stay away.
 */
#define _XmStringIsSpecified(x) ((x) != NULL && (x) != (XmString)XmUNSPECIFIED)

#endif /* _XMI_MACROSI_H */
