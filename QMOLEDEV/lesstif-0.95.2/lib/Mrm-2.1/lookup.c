/* 
 * $Id: lookup.c,v 1.2 2005/11/01 08:47:19 dannybackx Exp $
 *
 * Copyright (C) 1995 Free Software Foundation, Inc.
 * Copyright (C) 1995-2001 LessTif Development Team
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
 *
 *  Original author:  Geoffrey W. Ritchey
 *                    codesmit@southwind.net
 **/

#include <LTconfig.h>

#include <stdio.h>

#if XtSpecificationRelease < 6
#include <ctype.h>
#include <X11/Xos.h>
#endif

#include <Mrm/MrmPublic.h>

#include <Xm/Xm.h>
#include <Xm/ArrowB.h>
#include <Xm/ArrowBG.h>
#include <Xm/AtomMgr.h>
#include <Xm/BulletinB.h>
#include <Xm/CascadeB.h>
#include <Xm/CascadeBG.h>
#include <Xm/Command.h>
#include <Xm/CutPaste.h>
#include <Xm/DialogS.h>
#include <Xm/Display.h>
#include <Xm/DragDrop.h>
#include <Xm/DrawingA.h>
#include <Xm/DrawnB.h>
#include <Xm/FileSB.h>
#include <Xm/Form.h>
#include <Xm/Frame.h>
#include <Xm/Label.h>
#include <Xm/LabelG.h>
#include <Xm/List.h>
#include <Xm/MainW.h>
#include <Xm/MenuShell.h>
#include <Xm/MessageB.h>
#include <Xm/MwmUtil.h>
#include <Xm/PanedW.h>
#include <Xm/Protocols.h>
#include <Xm/PushB.h>
#include <Xm/PushBG.h>
#include <Xm/RepType.h>
#include <Xm/RowColumn.h>
#include <Xm/Scale.h>
#include <Xm/ScrollBar.h>
#include <Xm/ScrolledW.h>
#include <Xm/SelectioB.h>
#include <Xm/SeparatoG.h>
#include <Xm/Separator.h>
#include <Xm/Text.h>
#include <Xm/TextF.h>
#include <Xm/ToggleB.h>
#include <Xm/ToggleBG.h>

#if XmVERSION > 1
#include <Xm/ComboBox.h>
#include <Xm/Container.h>
#include <Xm/IconG.h>
#include <Xm/Notebook.h>
#include <Xm/SpinB.h>
#endif

/* CSText disappeared in Motif 2.1 */
#if (XmVERSION == 2) && (XmREVISION == 0)
#include <Xm/CSText.h>
#endif

#include "misc.h"
#include "lookup.h"
#include "Mrm.h"

#include <XmI/DebugUtil.h>


enum
{
    false, true
};

CreateFunctionsType CreateFunctions[] =
{
    {"list", NULL},
    {"Xm_List", NULL},
    {"Xm_OK", NULL},
    {"Xm_Help", NULL},
    {"Xm_Message", NULL},
    {"XmArrowButton", XmCreateArrowButton},
    {"XmArrowButtonGadget", XmCreateArrowButtonGadget},
    {"XmBulletinBoard", XmCreateBulletinBoard},
    {"XmBulletinBoardDialog", XmCreateBulletinBoardDialog},
    {"XmCascadeButton", XmCreateCascadeButton},
    {"XmCascadeButtonGadget", XmCreateCascadeButtonGadget},
#if XmVERSION > 1
    {"XmComboBox", XmCreateComboBox},
#endif
    {"XmCommand", XmCreateCommand},
    {"XmCommandDialog", XmCreateCommandDialog},
#if XmVERSION > 1
    {"XmContainer", XmCreateContainer},
#endif
#if (XmVERSION == 2) && (XmREVISION == 0)
    {"XmCSText", XmCreateCSText},
#endif
    {"XmDialogShell", XmCreateDialogShell},
    {"XmDragIcon", XmCreateDragIcon},
    {"XmDrawingArea", XmCreateDrawingArea},
    {"XmDrawnButton", XmCreateDrawnButton},
    {"XmErrorDialog", XmCreateErrorDialog},
    {"XmFileSelectionBox", XmCreateFileSelectionBox},
    {"XmFileSelectionDialog", XmCreateFileSelectionDialog},
    {"XmForm", XmCreateForm},
    {"XmFormDialog", XmCreateFormDialog},
    {"XmFrame", XmCreateFrame},
#if XmVERSION > 1
    {"XmIconGadget", XmCreateIconGadget},
#endif
    {"XmInformationDialog", XmCreateInformationDialog},
    {"XmLabel", XmCreateLabel},
    {"XmLabelGadget", XmCreateLabelGadget},
    {"XmList", XmCreateList},
    {"XmMainWindow", XmCreateMainWindow},
    {"XmMenuBar", XmCreateMenuBar},
    {"XmMenuShell", XmCreateMenuShell},
    {"XmMessageBox", XmCreateMessageBox},
    {"XmMessageDialog", XmCreateMessageDialog},
#if XmVERSION > 1
    {"XmNotebook", XmCreateNotebook},
#endif
    {"XmOptionMenu", XmCreateOptionMenu},
    {"XmPanedWindow", XmCreatePanedWindow},
    {"XmPopupMenu", XmCreatePopupMenu},
    {"XmPromptDialog", XmCreatePromptDialog},
    {"XmPulldownMenu", XmCreatePulldownMenu},
    {"XmPushButton", XmCreatePushButton},
    {"XmPushButtonGadget", XmCreatePushButtonGadget},
    {"XmQuestionDialog", XmCreateQuestionDialog},
    {"XmRadioBox", XmCreateRadioBox},
    {"XmRowColumn", XmCreateRowColumn},
    {"XmScale", XmCreateScale},
    {"XmScrollBar", XmCreateScrollBar},
    {"XmScrolledList", XmCreateScrolledList},
#if (XmVERSION == 2) && (XmREVISION == 0)
    {"XmScrolledCSText", XmCreateScrolledCSText},
#endif
    {"XmScrolledText", XmCreateScrolledText},
    {"XmScrolledWindow", XmCreateScrolledWindow},
    {"XmSelectionBox", XmCreateSelectionBox},
    {"XmSelectionDialog", XmCreateSelectionDialog},
    {"XmSeparator", XmCreateSeparator},
    {"XmSeparatorGadget", XmCreateSeparatorGadget},
#if XmVERSION > 1
    {"XmSpinBox", XmCreateSpinBox},
#endif
    {"XmTemplateDialog", XmCreateTemplateDialog},
    {"XmText", XmCreateText},
    {"XmTextField", XmCreateTextField},
    {"XmToggleButton", XmCreateToggleButton},
    {"XmToggleButtonGadget", XmCreateToggleButtonGadget},
    {"XmWarningDialog", XmCreateWarningDialog},
    {"XmWorkingDialog", XmCreateWorkingDialog},
    {"XmWorkArea", XmCreateWorkArea},
};

ArgNamesType ArgNames[] =
{
    {"MrmNcreateCallback", MrmNcreateCallback, C},
    {"XmNaccelerator", XmNaccelerator, CSG},
    {"XmNacceleratorText", XmNacceleratorText, CSG},
    {"XmNaccelerators", XmNaccelerators, CSG},
    {"XmNactivateCallback", XmNactivateCallback, C},
    {"XmNadjustLast", XmNadjustLast, CSG},
    {"XmNadjustMargin", XmNadjustMargin, CSG},
    {"XmNalignment", XmNalignment, CSG},
    {"XmNallowOverlap", XmNallowOverlap, CSG},
    {"XmNallowResize", XmNallowResize, CSG},
    {"XmNallowShellResize", XmNallowShellResize, CG},
    {"XmNallowShellResize", XmNallowShellResize, G},
    {"XmNancestorSensitive", XmNancestorSensitive, G},
    {"XmNanimationMask", XmNanimationMask, CSG},
    {"XmNanimationPixmap", XmNanimationPixmap, CSG},
    {"XmNanimationPixmapDepth", XmNanimationPixmapDepth, CSG},
    {"XmNanimationStyle", XmNanimationStyle, CSG},
    {"XmNapplyCallback", XmNapplyCallback, C},
    {"XmNapplyLabelString", XmNapplyLabelString, CSG},
    {"XmNarmCallback", XmNarmCallback, C},
    {"XmNarmColor", XmNarmColor, CSG},
    {"XmNarmPixmap", XmNarmPixmap, CSG},
    {"XmNarrowDirection", XmNarrowDirection, CSG},
#if XmVERSION > 1
    {"XmNarrowLayout", XmNarrowLayout, CSG},
    {"XmNarrowSensitivity", XmNarrowSensitivity, CSG},
    {"XmNarrowSize", XmNarrowSize, CSG},
    {"XmNarrowSpacing", XmNarrowSpacing, CSG},
#endif
    {"XmNattachment", XmNattachment, CSG},
    {"XmNaudibleWarning", XmNaudibleWarning, CSG},
#if XmVERSION > 1
    {"XmNautoDragModel", XmNautoDragModel, CSG},
#endif
    {"XmNautoShowCursorPosition", XmNautoShowCursorPosition, CSG},
    {"XmNautoUnmanage", XmNautoUnmanage, CG},
    {"XmNautomaticSelection", XmNautomaticSelection, CSG},
#if XmVERSION > 1
    {"XmNbackPageBackground", XmNbackPageBackground, CSG},
    {"XmNbackPageForeground", XmNbackPageForeground, CSG},
    {"XmNbackPageNumber", XmNbackPageNumber, CSG},
    {"XmNbackPagePlacement", XmNbackPagePlacement, CSG},
    {"XmNbackPageSize", XmNbackPageSize, CSG},
#endif
    {"XmNbackground", XmNbackground, CSG},
    {"XmNbackgroundPixmap", XmNbackgroundPixmap, CSG},
    {"XmNbaseHeight", XmNbaseHeight, CSG},
    {"XmNbaseWidth", XmNbaseWidth, CSG},
#if XmVERSION > 1
    {"XmNbidirectionalCursor", XmNbidirectionalCursor, CSG},
    {"XmNbindingPixmap", XmNbindingPixmap, CSG},
    {"XmNbindingType", XmNbindingType, CSG},
    {"XmNbindingWidth", XmNbindingWidth, CSG},
    {"XmNbitmapConversionModel", XmNbitmapConversionModel, CSG},
#endif
    {"XmNblendModel", XmNblendModel, CG},
    {"XmNblinkRate", XmNblinkRate, CSG},
    {"XmNborderColor", XmNborderColor, CSG},
    {"XmNborderPixmap", XmNborderPixmap, CSG},
    {"XmNborderWidth", XmNborderWidth, CSG},
    {"XmNbottomAttachment", XmNbottomAttachment, CSG},
    {"XmNbottomOffset", XmNbottomOffset, CSG},
    {"XmNbottomPosition", XmNbottomPosition, CSG},
    {"XmNbottomShadowColor", XmNbottomShadowColor, CSG},
    {"XmNbottomShadowPixmap", XmNbottomShadowPixmap, CSG},
    {"XmNbottomWidget", XmNbottomWidget, 0 /* CSG */},
    {"XmNbrowseSelectionCallback", XmNbrowseSelectionCallback, C},
    {"XmNbuttonAcceleratorText", XmNbuttonAcceleratorText, C},
    {"XmNbuttonAccelerators", XmNbuttonAccelerators, C},
    {"XmNbuttonCount", XmNbuttonCount, C},
    {"XmNbuttonFontList", XmNbuttonFontList, CSG},
    {"XmNbuttonMnemonicCharSets", XmNbuttonMnemonicCharSets, C},
    {"XmNbuttonMnemonics", XmNbuttonMnemonics, C},
#if XmVERSION > 1
    {"XmNbuttonRenderTable", XmNbuttonRenderTable, CSG},
#endif
    {"XmNbuttonSet", XmNbuttonSet, C},
    {"XmNbuttonType", XmNbuttonType, C},
    {"XmNbuttons", XmNbuttons, C},
    {"XmNcancelButton", XmNcancelButton, SG},
    {"XmNcancelCallback", XmNcancelCallback, C},
    {"XmNcancelLabelString", XmNcancelLabelString, CSG},
    {"XmNcascadePixmap", XmNcascadePixmap, CSG},
    {"XmNcascadingCallback", XmNcascadingCallback, C},
    {"XmNchildHorizontalAlignment", XmNchildHorizontalAlignment, CSG},
    {"XmNchildHorizontalSpacing", XmNchildHorizontalSpacing, CSG},
    {"XmNchildPlacement", XmNchildPlacement, CSG},
    {"XmNchildType", XmNchildType, CSG},
    {"XmNchildVerticalAlignment", XmNchildVerticalAlignment, CSG},
    {"XmNchildren", XmNchildren, G},
    {"XmNclientData", XmNclientData, CSG},
    {"XmNclipWindow", XmNclipWindow, G},
#if XmVERSION > 1
    {"XmNcollapsedStatePixmap", XmNcollapsedStatePixmap, CSG},
    {"XmNcolorAllocationProc", XmNcolorAllocationProc, CSG},
    {"XmNcolorCalculationProc", XmNcolorCalculationProc, CSG},
#endif
    {"XmNcolormap", XmNcolormap, CG},
    {"XmNcolumns", XmNcolumns, CSG},
#if XmVERSION > 1
    {"XmNcomboBoxType", XmNcomboBoxType, CG},
#endif
    {"XmNcommand", XmNcommand, CSG},
    {"XmNcommandChangedCallback", XmNcommandChangedCallback, C},
    {"XmNcommandEnteredCallback", XmNcommandEnteredCallback, C},
    {"XmNcommandWindow", XmNcommandWindow, CSG},
#if XmVERSION > 1
    {"XmNconvertCallback", XmNconvertCallback, C},
#endif
    {"XmNconvertProc", XmNconvertProc, CSG},
    {"XmNcreatePopupChildProc", XmNcreatePopupChildProc, CSG},
#if (XmVERSION == 2 && XmREVISION == 0)
    {"XmNcstextValue", XmNcstextValue, CSG},
#endif
#if XmVERSION > 1
    {"XmNcurrentPageNumber", XmNcurrentPageNumber, CSG},
#endif
    {"XmNcursorBackground", XmNcursorBackground, CSG},
    {"XmNcursorForeground", XmNcursorForeground, CSG},
    {"XmNcursorPosition", XmNcursorPosition, CSG},
    {"XmNcursorPositionVisible", XmNcursorPositionVisible, CSG},
    {"XmNdarkThreshold", XmNdarkThreshold, C},
    {"XmNdecimalPoints", XmNdecimalPoints, CSG},
    {"XmNdecrementCallback", XmNdecrementCallback, C},
    {"XmNdefaultActionCallback", XmNdefaultActionCallback, C},
#if XmVERSION > 1
    {"XmNdefaultArrowSensitivity", XmNdefaultArrowSensitivity, CSG},
#endif
    {"XmNdefaultButton", XmNdefaultButton, 0},	/* SG */
#if XmVERSION > 1
    {"XmNdefaultButtonEmphasis", XmNdefaultButtonEmphasis, C},
#endif
    {"XmNdefaultButtonShadowThickness", XmNdefaultButtonShadowThickness, CSG},
    {"XmNdefaultButtonType", XmNdefaultButtonType, CSG},
    {"XmNdefaultCopyCursorIcon", XmNdefaultCopyCursorIcon, CSG},
    {"XmNdefaultFontList", XmNdefaultFontList, CG},
    {"XmNdefaultInvalidCursorIcon", XmNdefaultInvalidCursorIcon, CSG},
    {"XmNdefaultLinkCursorIcon", XmNdefaultLinkCursorIcon, CSG},
    {"XmNdefaultMoveCursorIcon", XmNdefaultMoveCursorIcon, CSG},
    {"XmNdefaultNoneCursorIcon", XmNdefaultNoneCursorIcon, CSG},
    {"XmNdefaultPosition", XmNdefaultPosition, CSG},
    {"XmNdefaultSourceCursorIcon", XmNdefaultSourceCursorIcon, CSG},
    {"XmNdefaultValidCursorIcon", XmNdefaultValidCursorIcon, CSG},
#if XmVERSION > 1
    {"XmNdefaultVirtualBindings", XmNdefaultVirtualBindings, C},
#endif
    {"XmNdeleteResponse", XmNdeleteResponse, CSG},
    {"XmNdepth", XmNdepth, CG},
#if XmVERSION > 1
    {"XmNdestinationCallback", XmNdestinationCallback, C},
#endif
    {"XmNdestroyCallback", XmNdestroyCallback, C},
#if XmVERSION > 1
    {"XmNdetail", XmNdetail, CSG},
    {"XmNdetailColumnHeading", XmNdetailColumnHeading, CSG},
    {"XmNdetailColumnHeadingCount", XmNdetailColumnHeadingCount, CSG},
    {"XmNdetailCount", XmNdetailCount, CSG},
    {"XmNdetailOrder", XmNdetailOrder, CSG},
    {"XmNdetailOrderCount", XmNdetailOrderCount, CSG},
    {"XmNdetailShadowThickness", XmNdetailShadowThickness, CSG},
    {"XmNdetailTabList", XmNdetailTabList, CSG},
#endif
    {"XmNdialogStyle", XmNdialogStyle, CSG},
    {"XmNdialogTitle", XmNdialogTitle, CSG},
    {"XmNdialogType", XmNdialogType, CG},
    {"XmNdialogType", XmNdialogType, CSG},
    {"XmNdialogType", XmNdialogType, G},
    {"XmNdirListItemCount", XmNdirListItemCount, SG},
    {"XmNdirListItems", XmNdirListItems, SG},
    {"XmNdirListLabelString", XmNdirListLabelString, CSG},
    {"XmNdirMask", XmNdirMask, CSG},
    {"XmNdirSearchProc", XmNdirSearchProc, CSG},
    {"XmNdirSpec", XmNdirSpec, CSG},
#if XmVERSION > 1
    {"XmNdirTextLabelString", XmNdirTextLabelString, C},
#endif
    {"XmNdirectory", XmNdirectory, CSG},
    {"XmNdirectoryValid", XmNdirectoryValid, SG},
    {"XmNdisarmCallback", XmNdisarmCallback, C},
    {"XmNdoubleClickInterval", XmNdoubleClickInterval, CSG},
    {"XmNdragCallback", XmNdragCallback, C},
    {"XmNdragDropFinishCallback", XmNdragDropFinishCallback, CSG},
    {"XmNdragInitiatorProtocolStyle", XmNdragInitiatorProtocolStyle, CG},
    {"XmNdragMotionCallback", XmNdragMotionCallback, C},
    {"XmNdragOperations", XmNdragOperations, C},
    {"XmNdragProc", XmNdragProc, CSG},
    {"XmNdragReceiverProtocolStyle", XmNdragReceiverProtocolStyle, CG},
#if XmVERSION > 1
    {"XmNdragStartCallback", XmNdragStartCallback, C},
#endif
    {"XmNdropFinishCallback", XmNdropFinishCallback, C},
    {"XmNdropProc", XmNdropProc, CSG},
    {"XmNdropRectangles", XmNdropRectangles, CSG},
    {"XmNdropSiteActivity", XmNdropSiteActivity, CSG},
    {"XmNdropSiteEnterCallback", XmNdropSiteEnterCallback, C},
    {"XmNdropSiteLeaveCallback", XmNdropSiteLeaveCallback, C},
    {"XmNdropSiteOperations", XmNdropSiteOperations, CSG},
    {"XmNdropSiteType", XmNdropSiteType, CG},
    {"XmNdropStartCallback", XmNdropStartCallback, C},
    {"XmNdropTransfers", XmNdropTransfers, CG},
    {"XmNeditMode", XmNeditMode, CSG},
    {"XmNeditable", XmNeditable, CSG},
#if XmVERSION > 1
    {"XmNenableBtn1Transfer", XmNenableBtn1Transfer, C},
    {"XmNenableButtonTab", XmNenableButtonTab, C},
    {"XmNenableDragIcon", XmNenableDragIcon, C},
    {"XmNenableEtchedInMenu", XmNenableEtchedInMenu, C},
    {"XmNenableToggleColor", XmNenableToggleColor, C},
    {"XmNenableToggleVisual", XmNenableToggleVisual, C},
    {"XmNenableUnselectableDrag", XmNenableUnselectableDrag, C},
    {"XmNenableWarp", XmNenableWarp, CSG},
#endif
    {"XmNentryAlignment", XmNentryAlignment, CSG},
    {"XmNentryBorder", XmNentryBorder, CSG},
    {"XmNentryCallback", XmNentryCallback, C},
    {"XmNentryClass", XmNentryClass, CSG},
#if XmVERSION > 1
    {"XmNentryParent", XmNentryParent, CSG},
#endif
    {"XmNentryVerticalAlignment", XmNentryVerticalAlignment, CSG},
#if XmVERSION > 1
    {"XmNentryViewType", XmNentryViewType, CSG},
    {"XmNexpandedStatePixmap", XmNexpandedStatePixmap, CSG},
#endif
    {"XmNexportTargets", XmNexportTargets, CSG},
    {"XmNexposeCallback", XmNexposeCallback, C},
    {"XmNextendedSelectionCallback", XmNextendedSelectionCallback, C},
#if XmVERSION > 1
    {"XmNfileFilterStyle", XmNfileFilterStyle, C},
#endif
    {"XmNfileListItemCount", XmNfileListItemCount, SG},
    {"XmNfileListItems", XmNfileListItems, SG},
    {"XmNfileListLabelString", XmNfileListLabelString, CSG},
    {"XmNfileSearchProc", XmNfileSearchProc, CSG},
    {"XmNfileTypeMask", XmNfileTypeMask, CSG},
    {"XmNfillOnArm", XmNfillOnArm, CSG},
    {"XmNfillOnSelect", XmNfillOnSelect, CSG},
    {"XmNfilterLabelString", XmNfilterLabelString, CSG},
#if XmVERSION > 1
    {"XmNfirstPageNumber", XmNfirstPageNumber, CSG},
#endif
    {"XmNfocusCallback", XmNfocusCallback, C},
    {"XmNfont", XmNfont, CSG},
    {"XmNfontList", XmNfontList, CSG},
#if XmVERSION > 1
    {"XmNfontName", XmNfontName, CSG},
    {"XmNfontType", XmNfontType, CSG},
#endif
    {"XmNforeground", XmNforeground, CSG},
    {"XmNforegroundThreshold", XmNforegroundThreshold, C},
    {"XmNfractionBase", XmNfractionBase, CSG},
#if XmVERSION > 1
    {"XmNframeBackground", XmNframeBackground, CSG},
    {"XmNframeChildType", XmNframeChildType, CSG},
    {"XmNframeShadowThickness", XmNframeShadowThickness, CSG},
#endif
    {"XmNgainPrimaryCallback", XmNgainPrimaryCallback, C},
    {"XmNgeometry", XmNgeometry, CSG},
    {"XmNheight", XmNheight, CSG},
    {"XmNheightInc", XmNheightInc, CSG},
    {"XmNhelpCallback", XmNhelpCallback, C},
    {"XmNhelpLabelString", XmNhelpLabelString, CSG},
    {"XmNhighlightColor", XmNhighlightColor, CSG},
    {"XmNhighlightOnEnter", XmNhighlightOnEnter, CSG},
    {"XmNhighlightPixmap", XmNhighlightPixmap, CSG},
    {"XmNhighlightThickness", XmNhighlightThickness, CSG},
    {"XmNhistoryItemCount", XmNhistoryItemCount, CSG},
    {"XmNhistoryItems", XmNhistoryItems, CSG},
    {"XmNhistoryMaxItems", XmNhistoryMaxItems, CSG},
    {"XmNhistoryVisibleItemCount", XmNhistoryVisibleItemCount, CSG},
    {"XmNhorizontalFontUnit", XmNhorizontalFontUnit, CSG},
    {"XmNhorizontalScrollBar", XmNhorizontalScrollBar, CSG},
    {"XmNhorizontalSpacing", XmNhorizontalSpacing, CSG},
    {"XmNhotX", XmNhotX, CSG},
    {"XmNhotY", XmNhotY, CSG},
    {"XmNiconMask", XmNiconMask, CSG},
    {"XmNiconPixmap", XmNiconPixmap, CSG},
    {"XmNiconWindow", XmNiconWindow, CSG},
    {"XmNiconX", XmNiconX, CSG},
    {"XmNiconY", XmNiconY, CSG},
    {"XmNimportTargets", XmNimportTargets, CSG},
#if XmVERSION > 1
    {"XmNincludeStatus", XmNincludeStatus, CSG},
#endif
    {"XmNincrement", XmNincrement, CSG},
    {"XmNincrementCallback", XmNincrementCallback, C},
#if XmVERSION > 1
    {"XmNincrementValue", XmNincrementValue, CSG},
#endif
    {"XmNincremental", XmNincremental, CSG},
#if XmVERSION > 1
    {"XmNindeterminatePixmap", XmNindeterminatePixmap, CSG},
#endif
    {"XmNindicatorOn", XmNindicatorOn, CSG},
    {"XmNindicatorSize", XmNindicatorSize, CSG},
    {"XmNindicatorType", XmNindicatorType, CSG},
    {"XmNinitialDelay", XmNinitialDelay, CSG},
    {"XmNinitialFocus", XmNinitialFocus, CSG},
    {"XmNinitialResourcesPersistent", XmNinitialResourcesPersistent, C},
    {"XmNinitialState", XmNinitialState, CSG},
#if XmVERSION > 1
    {"XmNinnerMarginHeight", XmNinnerMarginHeight, CSG},
    {"XmNinnerMarginWidth", XmNinnerMarginWidth, CSG},
#endif
    {"XmNinput", XmNinput, CSG},
    {"XmNinputCallback", XmNinputCallback, C},
    {"XmNinputMethod", XmNinputMethod, CSG},
#if XmVERSION > 1
    {"XmNinputPolicy", XmNinputPolicy, CSG},
    {"XmNinsensitiveStippleBitmap", XmNinsensitiveStippleBitmap, CSG},
#endif
    {"XmNinsertPosition", XmNinsertPosition, CSG},
    {"XmNinvalidCursorForeground", XmNinvalidCursorForeground, CSG},
#if XmVERSION > 1
    {"XmNinvokeParseProc", XmNinvokeParseProc, CSG},
#endif
    {"XmNisAligned", XmNisAligned, CSG},
    {"XmNisHomogeneous", XmNisHomogeneous, CG},
    {"XmNitemCount", XmNitemCount, CSG},
    {"XmNitems", XmNitems, CSG},
    {"XmNkeyboardFocusPolicy", XmNkeyboardFocusPolicy, CSG},
    {"XmNlabelFontList", XmNlabelFontList, CSG},
    {"XmNlabelInsensitivePixmap", XmNlabelInsensitivePixmap, CSG},
    {"XmNlabelPixmap", XmNlabelPixmap, CSG},
#if XmVERSION > 1
    {"XmNlabelRenderTable", XmNlabelRenderTable, CSG},
#endif
#if 0
    {"XmNlabelString", XmNlabelString, C},
#endif
    {"XmNlabelString", XmNlabelString, CSG},
    {"XmNlabelType", XmNlabelType, CSG},
#if XmVERSION > 1
    {"XmNlargeCellHeight", XmNlargeCellHeight, CSG},
    {"XmNlargeCellWidth", XmNlargeCellWidth, CSG},
    {"XmNlargeIconMask", XmNlargeIconMask, CSG},
    {"XmNlargeIconPixmap", XmNlargeIconPixmap, CSG},
    {"XmNlastPageNumber", XmNlastPageNumber, CSG},
    {"XmNlayoutDirection", XmNlayoutDirection, CG},
    {"XmNlayoutType", XmNlayoutType, CSG},
#endif
    {"XmNleftAttachment", XmNleftAttachment, CSG},
    {"XmNleftOffset", XmNleftOffset, CSG},
    {"XmNleftPosition", XmNleftPosition, CSG},
    {"XmNleftWidget", XmNleftWidget, 0 /* CSG */},
    {"XmNlightThreshold", XmNlightThreshold, C},
    {"XmNlistItemCount", XmNlistItemCount, CSG},
    {"XmNlistItems", XmNlistItems, CSG},
    {"XmNlistLabelString", XmNlistLabelString, CSG},
    {"XmNlistMarginHeight", XmNlistMarginHeight, CSG},
    {"XmNlistMarginWidth", XmNlistMarginWidth, CSG},
    {"XmNlistSizePolicy", XmNlistSizePolicy, CG},
    {"XmNlistSpacing", XmNlistSpacing, CSG},
    {"XmNlistUpdated", XmNlistUpdated, SG},
    {"XmNlistVisibleItemCount", XmNlistVisibleItemCount, CSG},
#if XmVERSION > 1
    {"XmNloadModel", XmNloadModel, CSG},
#endif
    {"XmNlosePrimaryCallback", XmNlosePrimaryCallback, C},
    {"XmNlosingFocusCallback", XmNlosingFocusCallback, C},
    {"XmNmainWindowMarginHeight", XmNmainWindowMarginHeight, CSG},
    {"XmNmainWindowMarginWidth", XmNmainWindowMarginWidth, CSG},
#if XmVERSION > 1
    {"XmNmajorTabSpacing", XmNmajorTabSpacing, CSG},
#endif
    {"XmNmapCallback", XmNmapCallback, C},
    {"XmNmappedWhenManaged", XmNmappedWhenManaged, CSG},
    {"XmNmappingDelay", XmNmappingDelay, CSG},
    {"XmNmargin", XmNmargin, CSG},
    {"XmNmarginBottom", XmNmarginBottom, CSG},
    {"XmNmarginHeight", XmNmarginHeight, CSG},
    {"XmNmarginLeft", XmNmarginLeft, CSG},
    {"XmNmarginRight", XmNmarginRight, CSG},
    {"XmNmarginTop", XmNmarginTop, CSG},
    {"XmNmarginWidth", XmNmarginWidth, CSG},
    {"XmNmask", XmNmask, CSG},
#if XmVERSION > 1
    {"XmNmatchBehavior", XmNmatchBehavior, CSG},
#endif
    {"XmNmaxAspectX", XmNmaxAspectX, CSG},
    {"XmNmaxAspectY", XmNmaxAspectY, CSG},
    {"XmNmaxHeight", XmNmaxHeight, CSG},
    {"XmNmaxLength", XmNmaxLength, CSG},
    {"XmNmaxWidth", XmNmaxWidth, CSG},
    {"XmNmaximum", XmNmaximum, CSG},
#if XmVERSION > 1
    {"XmNmaximumValue", XmNmaximumValue, CSG},
#endif
    {"XmNmenuAccelerator", XmNmenuAccelerator, CSG},
    {"XmNmenuBar", XmNmenuBar, CSG},
    {"XmNmenuCursor", XmNmenuCursor, C},
    {"XmNmenuHelpWidget", XmNmenuHelpWidget, CSG},
    {"XmNmenuHistory", XmNmenuHistory, CSG},
    {"XmNmenuPost", XmNmenuPost, CSG},
    {"XmNmessageAlignment", XmNmessageAlignment, CSG},
    {"XmNmessageString", XmNmessageString, CSG},
    {"XmNmessageWindow", XmNmessageWindow, CSG},
    {"XmNminAspectX", XmNminAspectX, CSG},
    {"XmNminAspectY", XmNminAspectY, CSG},
    {"XmNminHeight", XmNminHeight, CSG},
    {"XmNminWidth", XmNminWidth, CSG},
    {"XmNminimizeButtons", XmNminimizeButtons, CSG},
    {"XmNminimum", XmNminimum, CSG},
#if XmVERSION > 1
    {"XmNminimumValue", XmNminimumValue, CSG},
    {"XmNminorTabSpacing", XmNminorTabSpacing, CSG},
#endif
    {"XmNmnemonic", XmNmnemonic, C},	/* CHANGED   CSG -> C */
    {"XmNmnemonicCharSet", XmNmnemonicCharSet, CSG},
    {"XmNmodifyVerifyCallback", XmNmodifyVerifyCallback, C},
    {"XmNmodifyVerifyCallbackWcs", XmNmodifyVerifyCallbackWcs, C},
#if XmVERSION > 1
    {"XmNmotifVersion", XmNmotifVersion, CSG},
#endif
    {"XmNmotionVerifyCallback", XmNmotionVerifyCallback, C},
    {"XmNmoveOpaque", XmNmoveOpaque, CSG},
    {"XmNmultiClick", XmNmultiClick, CSG},
    {"XmNmultipleSelectionCallback", XmNmultipleSelectionCallback, C},
    {"XmNmustMatch", XmNmustMatch, CSG},
    {"XmNmwmDecorations", XmNmwmDecorations, CG},
    {"XmNmwmFunctions", XmNmwmFunctions, CG},
    {"XmNmwmInputMode", XmNmwmInputMode, CG},
    {"XmNmwmMenu", XmNmwmMenu, CG},
    {"XmNnavigationType", XmNnavigationType, CSG},
#if XmVERSION > 1
    {"XmNnoFontCallback", XmNnoFontCallback, C},
#endif
    {"XmNnoMatchCallback", XmNnoMatchCallback, C},
    {"XmNnoMatchString", XmNnoMatchString, CSG},
#if XmVERSION > 1
    {"XmNnoRenditionCallback", XmNnoRenditionCallback, C},
#endif
    {"XmNnoResize", XmNnoResize, CSG},
    {"XmNnoneCursorForeground", XmNnoneCursorForeground, CSG},
#if XmVERSION > 1
    {"XmNnotebookChildType", XmNnotebookChildType, CG},
#endif
    {"XmNnumChildren", XmNnumChildren, G},
    {"XmNnumColumns", XmNnumColumns, CSG},
    {"XmNnumDropRectangles", XmNnumDropRectangles, CSG},
    {"XmNnumDropTransfers", XmNnumDropTransfers, CSG},
    {"XmNnumExportTargets", XmNnumExportTargets, CSG},
    {"XmNnumImportTargets", XmNnumImportTargets, CSG},
#if XmVERSION > 1
    {"XmNnumValues", XmNnumValues, CSG},
#endif
    {"XmNoffsetX", XmNoffsetX, CSG},
    {"XmNoffsetY", XmNoffsetY, CSG},
    {"XmNokCallback", XmNokCallback, C},
    {"XmNokLabelString", XmNokLabelString, CSG},
    {"XmNoperationChangedCallback", XmNoperationChangedCallback, C},
    {"XmNoperationCursorIcon", XmNoperationCursorIcon, CSG},
    {"XmNoptionLabel", XmNoptionLabel, C},
    {"XmNoptionMnemonic", XmNoptionMnemonic, C},
    {"XmNorientation", XmNorientation, CSG},
#if XmVERSION > 1
    {"XmNoutlineButtonPolicy", XmNoutlineButtonPolicy, CSG},
    {"XmNoutlineChangedCallback", XmNoutlineChangedCallback, C},
    {"XmNoutlineColumnWidth", XmNoutlineColumnWidth, CSG},
    {"XmNoutlineIndentation", XmNoutlineIndentation, CSG},
    {"XmNoutlineLineStyle", XmNoutlineLineStyle, CSG},
    {"XmNoutlineState", XmNoutlineState, CSG},
#endif
    {"XmNoverrideRedirect", XmNoverrideRedirect, CSG},
    {"XmNpacking", XmNpacking, CSG},
#if XmVERSION > 1
    {"XmNpageChangedCallback", XmNpageChangedCallback, C},
#endif
    {"XmNpageDecrementCallback", XmNpageDecrementCallback, C},
    {"XmNpageIncrement", XmNpageIncrement, CSG},
    {"XmNpageIncrementCallback", XmNpageIncrementCallback, C},
#if XmVERSION > 1
    {"XmNpageNumber", XmNpageNumber, C},
#endif
    {"XmNpaneMaximum", XmNpaneMaximum, CSG},
    {"XmNpaneMinimum", XmNpaneMinimum, CSG},
#if XmVERSION > 1
    {"XmNpathMode", XmNpathMode, C},
#endif
    {"XmNpattern", XmNpattern, CSG},
#if XmVERSION > 1
    {"XmNpatternType", XmNpatternType, CSG},
#endif
    {"XmNpendingDelete", XmNpendingDelete, CSG},
    {"XmNpixmap", XmNpixmap, CSG},
    {"XmNpopdownCallback", XmNpopdownCallback, C},
    {"XmNpopupCallback", XmNpopupCallback, C},
    {"XmNpopupEnabled", XmNpopupEnabled, CSG},
#if XmVERSION > 1
    {"XmNpopupHandlerCallback", XmNpopupHandlerCallback, C},
    {"XmNposition", XmNposition, CSG},
#endif
    {"XmNpositionIndex", XmNpositionIndex, CSG},
    {"XmNpostFromButton", XmNpostFromButton, C},
    {"XmNpreeditType", XmNpreeditType, CSG},
#if XmVERSION > 1
    {"XmNprimaryOwnership", XmNprimaryOwnership, CSG},
#endif
    {"XmNprocessingDirection", XmNprocessingDirection, CSG},
    {"XmNpromptString", XmNpromptString, CSG},
    {"XmNpushButtonEnabled", XmNpushButtonEnabled, CSG},
    {"XmNqualifySearchDataProc", XmNqualifySearchDataProc, CSG},
    {"XmNradioAlwaysOne", XmNradioAlwaysOne, CSG},
    {"XmNradioBehavior", XmNradioBehavior, CSG},
    {"XmNrecomputeSize", XmNrecomputeSize, CSG},
    {"XmNrefigureMode", XmNrefigureMode, CSG},
#if XmVERSION > 1
    {"XmNrenderTable", XmNrenderTable, CSG},
    {"XmNrenditionBackground", XmNrenditionBackground, CSG},
    {"XmNrenditionForeground", XmNrenditionForeground, CSG},
#endif
    {"XmNrepeatDelay", XmNrepeatDelay, CSG},
    {"XmNresizable", XmNresizable, CSG},
    {"XmNresizeCallback", XmNresizeCallback, C},
    {"XmNresizeHeight", XmNresizeHeight, CSG},
    {"XmNresizePolicy", XmNresizePolicy, CSG},
    {"XmNresizeWidth", XmNresizeWidth, CSG},
    {"XmNrightAttachment", XmNrightAttachment, CSG},
    {"XmNrightOffset", XmNrightOffset, CSG},
    {"XmNrightPosition", XmNrightPosition, CSG},
    {"XmNrightWidget", XmNrightWidget, 0 /* CSG */},
    {"XmNrowColumnType", XmNrowColumnType, CG},
    {"XmNrows", XmNrows, CSG},
    {"XmNrubberPositioning", XmNrubberPositioning, CSG},
    {"XmNsashHeight", XmNsashHeight, CSG},
    {"XmNsashIndent", XmNsashIndent, CSG},
    {"XmNsashShadowThickness", XmNsashShadowThickness, CSG},
    {"XmNsashWidth", XmNsashWidth, CSG},
    {"XmNsaveUnder", XmNsaveUnder, CSG},
    {"XmNscaleHeight", XmNscaleHeight, CSG},
    {"XmNscaleMultiple", XmNscaleMultiple, CSG},
    {"XmNscaleWidth", XmNscaleWidth, CSG},
    {"XmNscreen", XmNscreen, CG},
    {"XmNscrollBarDisplayPolicy", XmNscrollBarDisplayPolicy, CSG},
    {"XmNscrollBarPlacement", XmNscrollBarPlacement, CSG},
    {"XmNscrollHorizontal", XmNscrollHorizontal, CG},
    {"XmNscrollLeftSide", XmNscrollLeftSide, CG},
    {"XmNscrollTopSide", XmNscrollTopSide, CG},
    {"XmNscrollVertical", XmNscrollVertical, CG},
#if XmVERSION > 1
    {"XmNscrolledWindowChildType", XmNscrolledWindowChildType, CSG},
#endif
    {"XmNscrolledWindowMarginHeight", XmNscrolledWindowMarginHeight, CSG},
    {"XmNscrolledWindowMarginWidth", XmNscrolledWindowMarginWidth, CSG},
    {"XmNscrollingPolicy", XmNscrollingPolicy, CG},
    {"XmNselectColor", XmNselectColor, CSG},
    {"XmNselectInsensitivePixmap", XmNselectInsensitivePixmap, CSG},
    {"XmNselectPixmap", XmNselectPixmap, CSG},
    {"XmNselectThreshold", XmNselectThreshold, CSG},
#if XmVERSION > 1
    {"XmNselectedItem", XmNselectedItem, CSG},
#endif
    {"XmNselectedItemCount", XmNselectedItemCount, CSG},
    {"XmNselectedItems", XmNselectedItems, CSG},
#if XmVERSION > 1
    {"XmNselectedObjectCount", XmNselectedObjectCount, SG},
    {"XmNselectedObjects", XmNselectedObjects, SG},
    {"XmNselectedPosition", XmNselectedPosition, CSG},
    {"XmNselectedPositionCount", XmNselectedPositionCount, CSG},
    {"XmNselectedPositions", XmNselectedPositions, CSG},
#endif
    {"XmNselectionArrayCount", XmNselectionArrayCount, CSG},
#if XmVERSION > 1
    {"XmNselectionCallback", XmNselectionCallback, C},
#endif
    {"XmNselectionLabelString", XmNselectionLabelString, CSG},
#if XmVERSION > 1
    {"XmNselectionMode", XmNselectionMode, CSG},
#endif
    {"XmNselectionPolicy", XmNselectionPolicy, CSG},
#if XmVERSION > 1
    {"XmNselectionTechnique", XmNselectionTechnique, CSG},
#endif
    {"XmNsensitive", XmNsensitive, CSG},
    {"XmNseparatorOn", XmNseparatorOn, CSG},
    {"XmNseparatorType", XmNseparatorType, CSG},
    {"XmNset", XmNset, CSG},
    {"XmNshadowThickness", XmNshadowThickness, CSG},
    {"XmNshadowType", XmNshadowType, CSG},
    {"XmNshellUnitType", XmNshellUnitType, CSG},
    {"XmNshowArrows", XmNshowArrows, CSG},
    {"XmNshowAsDefault", XmNshowAsDefault, CSG},
    {"XmNshowSeparator", XmNshowSeparator, CSG},
    {"XmNshowValue", XmNshowValue, CSG},
    {"XmNsimpleCallback", XmNsimpleCallback, C},
    {"XmNsingleSelectionCallback", XmNsingleSelectionCallback, C},
    {"XmNskipAdjust", XmNskipAdjust, CSG},
#if XmVERSION > 1
    {"XmNsliderMark", XmNsliderMark, CSG},
#endif
    {"XmNsliderSize", XmNsliderSize, CSG},
#if XmVERSION > 1
    {"XmNsliderVisual", XmNsliderVisual, CSG},
    {"XmNslidingMode", XmNslidingMode, CSG},
    {"XmNsmallCellHeight", XmNsmallCellHeight, CSG},
    {"XmNsmallCellWidth", XmNsmallCellWidth, CSG},
    {"XmNsmallIconMask", XmNsmallIconMask, CSG},
    {"XmNsmallIconPixmap", XmNsmallIconPixmap, CSG},
    {"XmNsnapBackMultiple", XmNsnapBackMultiple, CSG},
#endif
    {"XmNsource", XmNsource, CSG},
    {"XmNsourceCursorIcon", XmNsourceCursorIcon, CSG},
    {"XmNsourcePixmapIcon", XmNsourcePixmapIcon, CSG},
    {"XmNspacing", XmNspacing, CSG},
#if XmVERSION > 1
    {"XmNspatialIncludeModel", XmNspatialIncludeModel, CSG},
    {"XmNspatialResizeModel", XmNspatialResizeModel, CSG},
    {"XmNspatialSnapModel", XmNspatialSnapModel, CSG},
    {"XmNspatialStyle", XmNspatialStyle, CSG},
    {"XmNspinBoxChildType", XmNspinBoxChildType, CG},
#endif
    {"XmNstateCursorIcon", XmNstateCursorIcon, CSG},
#if XmVERSION > 1
    {"XmNstrikethruType", XmNstrikethruType, CSG},
#endif
    {"XmNstringDirection", XmNstringDirection, CG},
    {"XmNstringDirection", XmNstringDirection, CSG},
    {"XmNsubMenuId", XmNsubMenuId, CSG},
#if XmVERSION > 1
    {"XmNsubstitute", XmNsubstitute, CSG},
#endif
    {"XmNsymbolPixmap", XmNsymbolPixmap, CSG},
#if XmVERSION > 1
    {"XmNtabList", XmNtabList, CSG},
    {"XmNtag", XmNtag, G},
#endif
    {"XmNtearOffMenuActivateCallback", XmNtearOffMenuActivateCallback, C},
    {"XmNtearOffMenuDeactivateCallback", XmNtearOffMenuDeactivateCallback, C},
    {"XmNtearOffModel", XmNtearOffModel, CSG},
#if XmVERSION > 1
    {"XmNtearOffTitle", XmNtearOffTitle, CSG},
#endif
    {"XmNtextAccelerators", XmNtextAccelerators, C},
    {"XmNtextColumns", XmNtextColumns, CSG},
    {"XmNtextFontList", XmNtextFontList, CSG},
#if XmVERSION > 1
    {"XmNtextRenderTable", XmNtextRenderTable, CSG},
#endif
    {"XmNtextString", XmNtextString, CSG},
    {"XmNtextTranslations", XmNtextTranslations, C},
    {"XmNtitle", XmNtitle, CSG},
    {"XmNtitleEncoding", XmNtitleEncoding, CSG},
    {"XmNtitleString", XmNtitleString, CSG},
    {"XmNtoBottomCallback", XmNtoBottomCallback, C},
    {"XmNtoTopCallback", XmNtoTopCallback, C},
#if XmVERSION > 1
    {"XmNtoggleMode", XmNtoggleMode, CSG},
#endif
    {"XmNtopAttachment", XmNtopAttachment, CSG},
    {"XmNtopCharacter", XmNtopCharacter, CSG},
    {"XmNtopItemPosition", XmNtopItemPosition, CSG},
    {"XmNtopLevelEnterCallback", XmNtopLevelEnterCallback, C},
    {"XmNtopLevelLeaveCallback", XmNtopLevelLeaveCallback, C},
    {"XmNtopOffset", XmNtopOffset, CSG},
    {"XmNtopPosition", XmNtopPosition, CSG},
    {"XmNtopShadowColor", XmNtopShadowColor, CSG},
    {"XmNtopShadowPixmap", XmNtopShadowPixmap, CSG},
    {"XmNtopWidget", XmNtopWidget, 0 /* CSG */},
    {"XmNtransferProc", XmNtransferProc, CSG},
    {"XmNtransferStatus", XmNtransferStatus, CSG},
    {"XmNtransient", XmNtransient, CSG},
    {"XmNtransientFor", XmNtransientFor, CSG},
    {"XmNtranslations", XmNtranslations, CSG},
    {"XmNtraversalOn", XmNtraversalOn, CSG},
    {"XmNtraversalOn", XmNtraversalOn, G},
    {"XmNtraverseObscuredCallback", XmNtraverseObscuredCallback, CSG},
    {"XmNtroughColor", XmNtroughColor, CSG},
#if XmVERSION > 1
    {"XmNunderlineType", XmNunderlineType, CSG},
#endif
    {"XmNunitType", XmNunitType, CSG},
    {"XmNunmapCallback", XmNunmapCallback, C},
    {"XmNunpostBehavior", XmNunpostBehavior, CSG},
#if XmVERSION > 1
    {"XmNunselectColor", XmNunselectColor, CSG},
#endif
    {"XmNuseAsyncGeometry", XmNuseAsyncGeometry, CSG},
#if 0
    {"XmNuseColorObject", XmNuseColorObject, C},
#endif
    {"XmNuserData", XmNuserData, CSG},
    {"XmNvalidCursorForeground", XmNvalidCursorForeground, CSG},
    {"XmNvalue", XmNvalue, CSG},
    {"XmNvalueChangedCallback", XmNvalueChangedCallback, C},
    {"XmNvalueWcs", XmNvalueWcs, CSG},
#if XmVERSION > 1
    {"XmNvalues", XmNvalues, CSG},
#endif
    {"XmNverifyBell", XmNverifyBell, CSG},
    {"XmNverticalFontUnit", XmNverticalFontUnit, CSG},
    {"XmNverticalScrollBar", XmNverticalScrollBar, CSG},
    {"XmNverticalSpacing", XmNverticalSpacing, CSG},
#if XmVERSION > 1
    {"XmNviewType", XmNviewType, CSG},
#endif
    {"XmNvisibleItemCount", XmNvisibleItemCount, CSG},
    {"XmNvisibleWhenOff", XmNvisibleWhenOff, CSG},
    {"XmNvisual", XmNvisual, CSG},
#if XmVERSION > 1
    {"XmNvisualEmphasis", XmNvisualEmphasis, CSG},
#endif
    {"XmNvisualPolicy", XmNvisualPolicy, G},
    {"XmNwaitForWm", XmNwaitForWm, CSG},
    {"XmNwhichButton", XmNwhichButton, CSG},
    {"XmNwidth", XmNwidth, CSG},
    {"XmNwidthInc", XmNwidthInc, CSG},
    {"XmNwinGravity", XmNwinGravity, CSG},
    {"XmNwindowGroup", XmNwindowGroup, CSG},
    {"XmNwmTimeout", XmNwmTimeout, CSG},
    {"XmNwordWrap", XmNwordWrap, CSG},
    {"XmNworkWindow", XmNworkWindow, CSG},
    {"XmNx", XmNx, CSG},
    {"XmNy", XmNy, CSG},

};


PredefinedType Predefines[] =
{
    {"FMT16BIT", (XtPointer)FMT16BIT},
    {"FMT8BIT", (XtPointer)FMT8BIT},
    {"Xm1000TH_INCHES", (XtPointer)Xm1000TH_INCHES},
    {"Xm100TH_FONT_UNITS", (XtPointer)Xm100TH_FONT_UNITS},
    {"Xm100TH_MILLIMETERS", (XtPointer)Xm100TH_MILLIMETERS},
    {"Xm100TH_POINTS", (XtPointer)Xm100TH_POINTS},
    {"XmALIGNMENT_BASELINE_BOTTOM", (XtPointer)XmALIGNMENT_BASELINE_BOTTOM},
    {"XmALIGNMENT_BASELINE_TOP", (XtPointer)XmALIGNMENT_BASELINE_TOP},
    {"XmALIGNMENT_BEGINNING", (XtPointer)XmALIGNMENT_BEGINNING},
    {"XmALIGNMENT_CENTER", (XtPointer)XmALIGNMENT_CENTER},
    {"XmALIGNMENT_CONTENTS_BOTTOM", (XtPointer)XmALIGNMENT_CONTENTS_BOTTOM},
    {"XmALIGNMENT_CONTENTS_TOP", (XtPointer)XmALIGNMENT_CONTENTS_TOP},
    {"XmALIGNMENT_END", (XtPointer)XmALIGNMENT_END},
    {"XmALIGNMENT_WIDGET_BOTTOM", (XtPointer)XmALIGNMENT_WIDGET_BOTTOM},
    {"XmALIGNMENT_WIDGET_TOP", (XtPointer)XmALIGNMENT_WIDGET_TOP},
    {"XmAPPLICATION_DEFINED", (XtPointer)XmAPPLICATION_DEFINED},
#if XmVERSION > 1
    {"XmARROWS_SPLIT", (XtPointer)XmARROWS_SPLIT},
#endif
    {"XmARROW_DOWN", (XtPointer)XmARROW_DOWN},
    {"XmARROW_LEFT", (XtPointer)XmARROW_LEFT},
    {"XmARROW_RIGHT", (XtPointer)XmARROW_RIGHT},
    {"XmARROW_UP", (XtPointer)XmARROW_UP},
    {"XmAS_NEEDED", (XtPointer)XmAS_NEEDED},
    {"XmATTACH_FORM", (XtPointer)XmATTACH_FORM},
    {"XmATTACH_NONE", (XtPointer)XmATTACH_NONE},
    {"XmATTACH_OPPOSITE_FORM", (XtPointer)XmATTACH_OPPOSITE_FORM},
    {"XmATTACH_OPPOSITE_WIDGET", (XtPointer)XmATTACH_OPPOSITE_WIDGET},
    {"XmATTACH_POSITION", (XtPointer)XmATTACH_POSITION},
    {"XmATTACH_SELF", (XtPointer)XmATTACH_SELF},
    {"XmATTACH_WIDGET", (XtPointer)XmATTACH_WIDGET},
    {"XmAUTOMATIC", (XtPointer)XmAUTOMATIC},
    {"XmBELL", (XtPointer)XmBELL},
    {"XmBOTTOM_LEFT", (XtPointer)XmBOTTOM_LEFT},
    {"XmBOTTOM_RIGHT", (XtPointer)XmBOTTOM_RIGHT},
#if XmVERSION > 1
    {"XmBOTTOM_TO_TOP_LEFT_TO_RIGHT", (XtPointer)XmBOTTOM_TO_TOP_LEFT_TO_RIGHT},
    {"XmBOTTOM_TO_TOP_RIGHT_TO_LEFT", (XtPointer)XmBOTTOM_TO_TOP_RIGHT_TO_LEFT},
#endif
    {"XmBROWSE_SELECT", (XtPointer)XmBROWSE_SELECT},
    {"XmCASCADEBUTTON", (XtPointer)XmCASCADEBUTTON},
#if XmVERSION > 1
    {"XmCENTER", (XtPointer)XmCENTER},
#endif
    {"XmCHECKBUTTON", (XtPointer)XmCHECKBUTTON},
    {"XmCOMMAND_ABOVE_WORKSPACE", (XtPointer)XmCOMMAND_ABOVE_WORKSPACE},
    {"XmCOMMAND_BELOW_WORKSPACE", (XtPointer)XmCOMMAND_BELOW_WORKSPACE},
    {"XmCONSTANT", (XtPointer)XmCONSTANT},
    {"XmCOPY_FAILED", (XtPointer)XmCOPY_FAILED},
    {"XmCOPY_SUCCEEDED", (XtPointer)XmCOPY_SUCCEEDED},
    {"XmCOPY_TRUNCATED", (XtPointer)XmCOPY_TRUNCATED},
    {"XmCR_ACTIVATE", (XtPointer)XmCR_ACTIVATE},
    {"XmCR_APPLY", (XtPointer)XmCR_APPLY},
    {"XmCR_ARM", (XtPointer)XmCR_ARM},
    {"XmCR_BROWSE_SELECT", (XtPointer)XmCR_BROWSE_SELECT},
    {"XmCR_CANCEL", (XtPointer)XmCR_CANCEL},
    {"XmCR_CASCADING", (XtPointer)XmCR_CASCADING},
    {"XmCR_CLIPBOARD_DATA_DELETE", (XtPointer)XmCR_CLIPBOARD_DATA_DELETE},
    {"XmCR_CLIPBOARD_DATA_REQUEST", (XtPointer)XmCR_CLIPBOARD_DATA_REQUEST},
    {"XmCR_COMMAND_CHANGED", (XtPointer)XmCR_COMMAND_CHANGED},
    {"XmCR_COMMAND_ENTERED", (XtPointer)XmCR_COMMAND_ENTERED},
    {"XmCR_CREATE", (XtPointer)XmCR_CREATE},
    {"XmCR_DECREMENT", (XtPointer)XmCR_DECREMENT},
    {"XmCR_DEFAULT_ACTION", (XtPointer)XmCR_DEFAULT_ACTION},
    {"XmCR_DISARM", (XtPointer)XmCR_DISARM},
    {"XmCR_DRAG", (XtPointer)XmCR_DRAG},
    {"XmCR_EXECUTE", (XtPointer)XmCR_EXECUTE},
    {"XmCR_EXPOSE", (XtPointer)XmCR_EXPOSE},
    {"XmCR_EXTENDED_SELECT", (XtPointer)XmCR_EXTENDED_SELECT},
    {"XmCR_FOCUS", (XtPointer)XmCR_FOCUS},
    {"XmCR_GAIN_PRIMARY", (XtPointer)XmCR_GAIN_PRIMARY},
    {"XmCR_HELP", (XtPointer)XmCR_HELP},
    {"XmCR_INCREMENT", (XtPointer)XmCR_INCREMENT},
    {"XmCR_INPUT", (XtPointer)XmCR_INPUT},
    {"XmCR_LOSE_PRIMARY", (XtPointer)XmCR_LOSE_PRIMARY},
    {"XmCR_LOSING_FOCUS", (XtPointer)XmCR_LOSING_FOCUS},
    {"XmCR_MAP", (XtPointer)XmCR_MAP},
    {"XmCR_MODIFYING_TEXT_VALUE", (XtPointer)XmCR_MODIFYING_TEXT_VALUE},
    {"XmCR_MOVING_INSERT_CURSOR", (XtPointer)XmCR_MOVING_INSERT_CURSOR},
    {"XmCR_MULTIPLE_SELECT", (XtPointer)XmCR_MULTIPLE_SELECT},
    {"XmCR_NONE", (XtPointer)XmCR_NONE},
    {"XmCR_NO_MATCH", (XtPointer)XmCR_NO_MATCH},
    {"XmCR_OBSCURED_TRAVERSAL", (XtPointer)XmCR_OBSCURED_TRAVERSAL},
    {"XmCR_OK", (XtPointer)XmCR_OK},
    {"XmCR_PAGE_DECREMENT", (XtPointer)XmCR_PAGE_DECREMENT},
    {"XmCR_PAGE_INCREMENT", (XtPointer)XmCR_PAGE_INCREMENT},
    {"XmCR_PROTOCOLS", (XtPointer)XmCR_PROTOCOLS},
    {"XmCR_RESIZE", (XtPointer)XmCR_RESIZE},
    {"XmCR_SINGLE_SELECT", (XtPointer)XmCR_SINGLE_SELECT},
    {"XmCR_TEAR_OFF_ACTIVATE", (XtPointer)XmCR_TEAR_OFF_ACTIVATE},
    {"XmCR_TEAR_OFF_DEACTIVATE", (XtPointer)XmCR_TEAR_OFF_DEACTIVATE},
    {"XmCR_TO_BOTTOM", (XtPointer)XmCR_TO_BOTTOM},
    {"XmCR_TO_TOP", (XtPointer)XmCR_TO_TOP},
    {"XmCR_UNMAP", (XtPointer)XmCR_UNMAP},
    {"XmCR_VALUE_CHANGED", (XtPointer)XmCR_VALUE_CHANGED},
    {"XmCURSOR", (XtPointer)XmCURSOR},
    {"XmDEFAULT_BACKGROUND", (XtPointer)XmDEFAULT_BACKGROUND},
    {"XmDEFAULT_DARK_THRESHOLD", (XtPointer)XmDEFAULT_DARK_THRESHOLD},
    {"XmDEFAULT_FONT", (XtPointer)XmDEFAULT_FONT},
 {"XmDEFAULT_FOREGROUND_THRESHOLD", (XtPointer)XmDEFAULT_FOREGROUND_THRESHOLD},
    {"XmDEFAULT_LIGHT_THRESHOLD", (XtPointer)XmDEFAULT_LIGHT_THRESHOLD},
#if XmVERSION > 1
    {"XmDEFAULT_SELECT_COLOR", (XtPointer)XmDEFAULT_SELECT_COLOR},
#endif
    {"XmDESTROY", (XtPointer)XmDESTROY},
#if XmVERSION > 1
    {"XmDETAIL", (XtPointer)XmDETAIL},
#endif
    {"XmDIALOG_APPLICATION_MODAL", (XtPointer)XmDIALOG_APPLICATION_MODAL},
    {"XmDIALOG_APPLY_BUTTON", (XtPointer)XmDIALOG_APPLY_BUTTON},
    {"XmDIALOG_CANCEL_BUTTON", (XtPointer)XmDIALOG_CANCEL_BUTTON},
    {"XmDIALOG_COMMAND", (XtPointer)XmDIALOG_COMMAND},
    {"XmDIALOG_COMMAND_TEXT", (XtPointer)XmDIALOG_COMMAND_TEXT},
    {"XmDIALOG_DEFAULT_BUTTON", (XtPointer)XmDIALOG_DEFAULT_BUTTON},
    {"XmDIALOG_DIR_LIST", (XtPointer)XmDIALOG_DIR_LIST},
    {"XmDIALOG_DIR_LIST_LABEL", (XtPointer)XmDIALOG_DIR_LIST_LABEL},
    {"XmDIALOG_ERROR", (XtPointer)XmDIALOG_ERROR},
    {"XmDIALOG_FILE_LIST", (XtPointer)XmDIALOG_FILE_LIST},
    {"XmDIALOG_FILE_LIST_LABEL", (XtPointer)XmDIALOG_FILE_LIST_LABEL},
    {"XmDIALOG_FILE_SELECTION", (XtPointer)XmDIALOG_FILE_SELECTION},
    {"XmDIALOG_FILTER_LABEL", (XtPointer)XmDIALOG_FILTER_LABEL},
    {"XmDIALOG_FILTER_TEXT", (XtPointer)XmDIALOG_FILTER_TEXT},
{"XmDIALOG_FULL_APPLICATION_MODAL", (XtPointer)XmDIALOG_FULL_APPLICATION_MODAL},
    {"XmDIALOG_HELP_BUTTON", (XtPointer)XmDIALOG_HELP_BUTTON},
    {"XmDIALOG_HISTORY_LIST", (XtPointer)XmDIALOG_HISTORY_LIST},
    {"XmDIALOG_INFORMATION", (XtPointer)XmDIALOG_INFORMATION},
    {"XmDIALOG_LIST", (XtPointer)XmDIALOG_LIST},
    {"XmDIALOG_LIST_LABEL", (XtPointer)XmDIALOG_LIST_LABEL},
    {"XmDIALOG_MESSAGE", (XtPointer)XmDIALOG_MESSAGE},
    {"XmDIALOG_MESSAGE_LABEL", (XtPointer)XmDIALOG_MESSAGE_LABEL},
    {"XmDIALOG_MODELESS", (XtPointer)XmDIALOG_MODELESS},
    {"XmDIALOG_NONE", (XtPointer)XmDIALOG_NONE},
    {"XmDIALOG_OK_BUTTON", (XtPointer)XmDIALOG_OK_BUTTON},
    {"XmDIALOG_PRIMARY_APPLICATION_MODAL", (XtPointer)XmDIALOG_PRIMARY_APPLICATION_MODAL},
    {"XmDIALOG_PROMPT", (XtPointer)XmDIALOG_PROMPT},
    {"XmDIALOG_PROMPT_LABEL", (XtPointer)XmDIALOG_PROMPT_LABEL},
    {"XmDIALOG_QUESTION", (XtPointer)XmDIALOG_QUESTION},
    {"XmDIALOG_SELECTION", (XtPointer)XmDIALOG_SELECTION},
    {"XmDIALOG_SELECTION_LABEL", (XtPointer)XmDIALOG_SELECTION_LABEL},
    {"XmDIALOG_SEPARATOR", (XtPointer)XmDIALOG_SEPARATOR},
    {"XmDIALOG_SYMBOL_LABEL", (XtPointer)XmDIALOG_SYMBOL_LABEL},
    {"XmDIALOG_SYSTEM_MODAL", (XtPointer)XmDIALOG_SYSTEM_MODAL},
    {"XmDIALOG_TEMPLATE", (XtPointer)XmDIALOG_TEMPLATE},
    {"XmDIALOG_TEXT", (XtPointer)XmDIALOG_TEXT},
    {"XmDIALOG_VALUE_TEXT", (XtPointer)XmDIALOG_VALUE_TEXT},
    {"XmDIALOG_WARNING", (XtPointer)XmDIALOG_WARNING},
    {"XmDIALOG_WORKING", (XtPointer)XmDIALOG_WORKING},
    {"XmDIALOG_WORK_AREA", (XtPointer)XmDIALOG_WORK_AREA},
    {"XmDOUBLE_DASHED_LINE", (XtPointer)XmDOUBLE_DASHED_LINE},
    {"XmDOUBLE_LINE", (XtPointer)XmDOUBLE_LINE},
    {"XmDOUBLE_SEPARATOR", (XtPointer)XmDOUBLE_SEPARATOR},
    {"XmDO_NOTHING", (XtPointer)XmDO_NOTHING},
#if XmVERSION > 1
    {"XmDROP_DOWN_COMBO_BOX", (XtPointer)XmDROP_DOWN_COMBO_BOX},
    {"XmDROP_DOWN_LIST", (XtPointer)XmDROP_DOWN_LIST},
#endif
    {"XmDYNAMIC", (XtPointer)XmDYNAMIC},
    {"XmEXCLUSIVE_TAB_GROUP", (XtPointer)XmEXCLUSIVE_TAB_GROUP},
#if XmVERSION > 1
    {"XmEXPANDED", (XtPointer)XmEXPANDED},
#endif
    {"XmEXPLICIT", (XtPointer)XmEXPLICIT},
    {"XmEXTENDED_SELECT", (XtPointer)XmEXTENDED_SELECT},
    {"XmFALLBACK_CHARSET", (XtPointer)XmFALLBACK_CHARSET},
    {"XmFILE_ANY_TYPE", (XtPointer)XmFILE_ANY_TYPE},
    {"XmFILE_DIRECTORY", (XtPointer)XmFILE_DIRECTORY},
    {"XmFILE_REGULAR", (XtPointer)XmFILE_REGULAR},
    {"XmFIRST_POSITION", (XtPointer)XmFIRST_POSITION},
    {"XmFMT_16_BIT", (XtPointer)XmFMT_16_BIT},
    {"XmFMT_8_BIT", (XtPointer)XmFMT_8_BIT},
    {"XmFONT_IS_FONT", (XtPointer)XmFONT_IS_FONT},
    {"XmFONT_IS_FONTSET", (XtPointer)XmFONT_IS_FONTSET},
    {"XmFRAME_GENERIC_CHILD", (XtPointer)XmFRAME_GENERIC_CHILD},
    {"XmFRAME_TITLE_CHILD", (XtPointer)XmFRAME_TITLE_CHILD},
    {"XmFRAME_WORKAREA_CHILD", (XtPointer)XmFRAME_WORKAREA_CHILD},
    {"XmHIGHLIGHT_NORMAL", (XtPointer)XmHIGHLIGHT_NORMAL},
 {"XmHIGHLIGHT_SECONDARY_SELECTED", (XtPointer)XmHIGHLIGHT_SECONDARY_SELECTED},
    {"XmHIGHLIGHT_SELECTED", (XtPointer)XmHIGHLIGHT_SELECTED},
    {"XmHORIZONTAL", (XtPointer)XmHORIZONTAL},
#if XmVERSION > 1
    {"XmINDICATOR_CHECK_BOX", (XtPointer)XmINDICATOR_CHECK_BOX},
    {"XmINHERIT_POLICY", (XtPointer)XmINHERIT_POLICY},
#endif
    {"XmINVALID_SEPARATOR_TYPE", (XtPointer)XmINVALID_SEPARATOR_TYPE},
#if XmVERSION > 1
    {"XmLARGE_ICON", (XtPointer)XmLARGE_ICON},
#endif
    {"XmLAST_POSITION", (XtPointer)XmLAST_POSITION},
#if XmVERSION > 1
    {"XmLEFT_TO_RIGHT_BOTTOM_TO_TOP", (XtPointer)XmLEFT_TO_RIGHT_BOTTOM_TO_TOP},
    {"XmLEFT_TO_RIGHT_TOP_TO_BOTTOM", (XtPointer)XmLEFT_TO_RIGHT_TOP_TO_BOTTOM},
    {"XmMAJOR_TAB", (XtPointer)XmMAJOR_TAB},
#endif
    {"XmMAX_ON_BOTTOM", (XtPointer)XmMAX_ON_BOTTOM},
    {"XmMAX_ON_LEFT", (XtPointer)XmMAX_ON_LEFT},
    {"XmMAX_ON_RIGHT", (XtPointer)XmMAX_ON_RIGHT},
    {"XmMAX_ON_TOP", (XtPointer)XmMAX_ON_TOP},
    {"XmMENU_BAR", (XtPointer)XmMENU_BAR},
    {"XmMENU_OPTION", (XtPointer)XmMENU_OPTION},
    {"XmMENU_POPUP", (XtPointer)XmMENU_POPUP},
    {"XmMENU_PULLDOWN", (XtPointer)XmMENU_PULLDOWN},
#if XmVERSION > 1
    {"XmMINOR_TAB", (XtPointer)XmMINOR_TAB},
#endif
    {"XmMULTICLICK_DISCARD", (XtPointer)XmMULTICLICK_DISCARD},
    {"XmMULTICLICK_KEEP", (XtPointer)XmMULTICLICK_KEEP},
    {"XmMULTIPLE_SELECT", (XtPointer)XmMULTIPLE_SELECT},
    {"XmMULTI_LINE_EDIT", (XtPointer)XmMULTI_LINE_EDIT},
    {"XmNONE", (XtPointer)XmNONE},
    {"XmNO_LINE", (XtPointer)XmNO_LINE},
    {"XmNO_ORIENTATION", (XtPointer)XmNO_ORIENTATION},
    {"XmNO_PACKING", (XtPointer)XmNO_PACKING},
    {"XmN_OF_MANY", (XtPointer)XmN_OF_MANY},
    {"XmONE_OF_MANY", (XtPointer)XmONE_OF_MANY},
    {"XmPACK_COLUMN", (XtPointer)XmPACK_COLUMN},
    {"XmPACK_NONE", (XtPointer)XmPACK_NONE},
    {"XmPACK_TIGHT", (XtPointer)XmPACK_TIGHT},
#if XmVERSION > 1
    {"XmPAGE", (XtPointer)XmPAGE},
    {"XmPER_SHELL", (XtPointer)XmPER_SHELL},
    {"XmPER_WIDGET", (XtPointer)XmPER_WIDGET},
#endif
    {"XmPIXELS", (XtPointer)XmPIXELS},
    {"XmPIXMAP", (XtPointer)XmPIXMAP},
    {"XmPLACE_ABOVE_SELECTION", (XtPointer)XmPLACE_ABOVE_SELECTION},
    {"XmPLACE_BELOW_SELECTION", (XtPointer)XmPLACE_BELOW_SELECTION},
    {"XmPLACE_TOP", (XtPointer)XmPLACE_TOP},
    {"XmPOINTER", (XtPointer)XmPOINTER},
    {"XmPUSHBUTTON", (XtPointer)XmPUSHBUTTON},
    {"XmRADIOBUTTON", (XtPointer)XmRADIOBUTTON},
    {"XmRESIZE_ANY", (XtPointer)XmRESIZE_ANY},
    {"XmRESIZE_GROW", (XtPointer)XmRESIZE_GROW},
    {"XmRESIZE_IF_POSSIBLE", (XtPointer)XmRESIZE_IF_POSSIBLE},
    {"XmRESIZE_NONE", (XtPointer)XmRESIZE_NONE},
    {"XmREVISION", (XtPointer)XmREVISION},
#if XmVERSION > 1
    {"XmRIGHT_TO_LEFT_BOTTOM_TO_TOP", (XtPointer)XmRIGHT_TO_LEFT_BOTTOM_TO_TOP},
    {"XmRIGHT_TO_LEFT_TOP_TO_BOTTOM", (XtPointer)XmRIGHT_TO_LEFT_TOP_TO_BOTTOM},
#endif
    {"XmSELECT_ALL", (XtPointer)XmSELECT_ALL},
    {"XmSELECT_LINE", (XtPointer)XmSELECT_LINE},
    {"XmSELECT_PARAGRAPH", (XtPointer)XmSELECT_PARAGRAPH},
    {"XmSELECT_POSITION", (XtPointer)XmSELECT_POSITION},
    {"XmSELECT_WHITESPACE", (XtPointer)XmSELECT_WHITESPACE},
    {"XmSELECT_WORD", (XtPointer)XmSELECT_WORD},
    {"XmSEPARATOR", (XtPointer)XmSEPARATOR},
#if XmVERSION > 1
    {"XmSET", (XtPointer)XmSET},
#endif
    {"XmSINGLE_LINE", (XtPointer)XmSINGLE_LINE},
    {"XmSHADOW_ETCHED_IN", (XtPointer)XmSHADOW_ETCHED_IN},
    {"XmSHADOW_ETCHED_IN_DASH", (XtPointer)XmSHADOW_ETCHED_IN_DASH},
    {"XmSHADOW_ETCHED_OUT", (XtPointer)XmSHADOW_ETCHED_OUT},
    {"XmSHADOW_ETCHED_OUT_DASH", (XtPointer)XmSHADOW_ETCHED_OUT_DASH},
    {"XmSHADOW_IN", (XtPointer)XmSHADOW_IN},
    {"XmSHADOW_OUT", (XtPointer)XmSHADOW_OUT},
    {"XmSINGLE_DASHED_LINE", (XtPointer)XmSINGLE_DASHED_LINE},
    {"XmSINGLE_LINE", (XtPointer)XmSINGLE_LINE},
    {"XmSINGLE_LINE_EDIT", (XtPointer)XmSINGLE_LINE_EDIT},
    {"XmSINGLE_SELECT", (XtPointer)XmSINGLE_SELECT},
#if XmVERSION > 1
    {"XmSMALL_ICON", (XtPointer)XmSMALL_ICON},
    {"XmSPATIAL", (XtPointer)XmSPATIAL},
#endif
    {"XmSTATIC", (XtPointer)XmSTATIC},
#if XmVERSION > 1
    {"XmSTATUS_AREA", (XtPointer)XmSTATUS_AREA},
#endif
    {"XmSTICKY_TAB_GROUP", (XtPointer)XmSTICKY_TAB_GROUP},
    {"XmSTRING", (XtPointer)XmSTRING},
    {"XmSTRING_COMPONENT_CHARSET", (XtPointer)XmSTRING_COMPONENT_CHARSET},
    {"XmSTRING_COMPONENT_DIRECTION", (XtPointer)XmSTRING_COMPONENT_DIRECTION},
    {"XmSTRING_COMPONENT_END", (XtPointer)XmSTRING_COMPONENT_END},
 {"XmSTRING_COMPONENT_LOCALE_TEXT", (XtPointer)XmSTRING_COMPONENT_LOCALE_TEXT},
    {"XmSTRING_COMPONENT_SEPARATOR", (XtPointer)XmSTRING_COMPONENT_SEPARATOR},
    {"XmSTRING_COMPONENT_TEXT", (XtPointer)XmSTRING_COMPONENT_TEXT},
    {"XmSTRING_COMPONENT_UNKNOWN", (XtPointer)XmSTRING_COMPONENT_UNKNOWN},
    {"XmSTRING_COMPONENT_USER_BEGIN", (XtPointer)XmSTRING_COMPONENT_USER_BEGIN},
    {"XmSTRING_COMPONENT_USER_END", (XtPointer)XmSTRING_COMPONENT_USER_END},
    {"XmSTRING_DIRECTION_DEFAULT", (XtPointer)XmSTRING_DIRECTION_DEFAULT},
    {"XmSTRING_DIRECTION_L_TO_R", (XtPointer)XmSTRING_DIRECTION_L_TO_R},
    {"XmSTRING_DIRECTION_R_TO_L", (XtPointer)XmSTRING_DIRECTION_R_TO_L},
    {"XmSTRING_OS_CHARSET", (XtPointer)XmSTRING_OS_CHARSET},
    {"XmTAB_GROUP", (XtPointer)XmTAB_GROUP},
    {"XmTEAR_OFF_DISABLED", (XtPointer)XmTEAR_OFF_DISABLED},
    {"XmTEAR_OFF_ENABLED", (XtPointer)XmTEAR_OFF_ENABLED},
    {"XmTEXT_BACKWARD", (XtPointer)XmTEXT_BACKWARD},
    {"XmTEXT_FORWARD", (XtPointer)XmTEXT_FORWARD},
    {"XmTITLE", (XtPointer)XmTITLE},
    {"XmTOGGLEBUTTON", (XtPointer)XmTOGGLEBUTTON},
#if XmVERSION > 1
    {"XmTOGGLE_BOOLEAN", (XtPointer)XmTOGGLE_BOOLEAN},
#endif
    {"XmTOP_LEFT", (XtPointer)XmTOP_LEFT},
    {"XmTOP_RIGHT", (XtPointer)XmTOP_RIGHT},
#if XmVERSION > 1
    {"XmTOP_TO_BOTTOM_LEFT_TO_RIGHT", (XtPointer)XmTOP_TO_BOTTOM_LEFT_TO_RIGHT},
    {"XmTOP_TO_BOTTOM_RIGHT_TO_LEFT", (XtPointer)XmTOP_TO_BOTTOM_RIGHT_TO_LEFT},
#endif
    {"XmTRAVERSE_CURRENT", (XtPointer)XmTRAVERSE_CURRENT},
    {"XmTRAVERSE_DOWN", (XtPointer)XmTRAVERSE_DOWN},
    {"XmTRAVERSE_HOME", (XtPointer)XmTRAVERSE_HOME},
    {"XmTRAVERSE_LEFT", (XtPointer)XmTRAVERSE_LEFT},
    {"XmTRAVERSE_NEXT", (XtPointer)XmTRAVERSE_NEXT},
    {"XmTRAVERSE_NEXT_TAB_GROUP", (XtPointer)XmTRAVERSE_NEXT_TAB_GROUP},
    {"XmTRAVERSE_PREV", (XtPointer)XmTRAVERSE_PREV},
    {"XmTRAVERSE_PREV_TAB_GROUP", (XtPointer)XmTRAVERSE_PREV_TAB_GROUP},
    {"XmTRAVERSE_RIGHT", (XtPointer)XmTRAVERSE_RIGHT},
    {"XmTRAVERSE_UP", (XtPointer)XmTRAVERSE_UP},
    {"XmUNMAP", (XtPointer)XmUNMAP},
    {"XmUNPOST", (XtPointer)XmUNPOST},
    {"XmUNPOST_AND_REPLAY", (XtPointer)XmUNPOST_AND_REPLAY},
    {"XmUNSPECIFIED_PIXMAP", (XtPointer)XmUNSPECIFIED_PIXMAP},
    {"XmVARIABLE", (XtPointer)XmVARIABLE},
    {"XmVERSION", (XtPointer)XmVERSION},
    {"XmVERSION_STRING", (XtPointer)XmVERSION_STRING},
    {"XmVERTICAL", (XtPointer)XmVERTICAL},
    {"XmVISIBILITY_FULLY_OBSCURED", (XtPointer)XmVISIBILITY_FULLY_OBSCURED},
{"XmVISIBILITY_PARTIALLY_OBSCURED", (XtPointer)XmVISIBILITY_PARTIALLY_OBSCURED},
    {"XmVISIBILITY_UNOBSCURED", (XtPointer)XmVISIBILITY_UNOBSCURED},
    {"XmVersion", (XtPointer)XmVersion},
    {"XmWINDOW", (XtPointer)XmWINDOW},
    {"XmWORK_AREA", (XtPointer)XmWORK_AREA},
#if 0
    {"XmUNSPECIFIED", (XtPointer)XmUNSPECIFIED},
#endif
};

int ClassSize = sizeof(CreateFunctions) / sizeof(CreateFunctions[0]);
int ArgSize = sizeof(ArgNames) / sizeof(ArgNames[0]);
int PreSize = sizeof(Predefines) / sizeof(Predefines[0]);

extern int 
__MrmLookUpClassIndex(char *ClassName)
{
    int i = 0;

    for (i = 0; i < ClassSize; i++)
	if (strcmp(CreateFunctions[i].Name, ClassName) == 0)
	{
	    fprintf(stderr, "lookup %s = index of %d\n", ClassName, i);
	    return i;
	}
    __MrmWarn(LOC, "ZZ Undefined Class Name: %s\n", ClassName);
    return -1;
}

void 
__MrmGetArgValues(int index, char **theString, char *Access)
{
    *theString = ArgNames[index].theString;
    *Access = ArgNames[index].Access;
}

extern int 
__MrmLookUpArgIndex(char *ArgName)
{
    int i = 0;

    for (i = 0; i < ArgSize; i++)
	if (strcmp(ArgNames[i].Name, ArgName) == 0)
	{
	    return i;
	}
    __MrmWarn(LOC, "Undefined Arg Name: %s\n", ArgName);
    return -1;
}

/* XtPointer         String     */
char *
__MrmLookUpFunction(char *FunctionName)
{
    int i = 0;

    for (i = 0; i < NumberRegisteredFunctions; i++)
	if (strcmp(RegisteredFunctions[i].name, FunctionName) == 0)
	{
	    return (char *)RegisteredFunctions[i].value;
	}
    __MrmWarn(LOC, "Could not find %s\n", FunctionName);
    return NULL;
}

/* XtPointer           String */
extern int
__MrmLookUpPredefines(char *Name, long *value)
{
    int i = 0;

    for (i = 0; i < PreSize; i++)
	if (strcmp(Predefines[i].Name, Name) == 0)
	{
	    *value = (long)Predefines[i].value;
	    return 1;
	}
    return 0;
}

char *
__MrmArgNamesString(char *ArgName)
{
    return ArgNames[__MrmLookUpArgIndex(ArgName)].theString;
}

#if XtSpecificationRelease < 6

/*
 * this code is from X11R6 -- MLM
 */

/* $XConsortium: RdBitF.c,v 1.19 94/04/17 20:20:42 rws Exp $ */
/*

   Copyright (c) 1987  X Consortium

   Permission is hereby granted, free of charge, to any person obtaining
   a copy of this software and associated documentation files (the
   "Software"), to deal in the Software without restriction, including
   without limitation the rights to use, copy, modify, merge, publish,
   distribute, sublicense, and/or sell copies of the Software, and to
   permit persons to whom the Software is furnished to do so, subject to
   the following conditions:

   The above copyright notice and this permission notice shall be included
   in all copies or substantial portions of the Software.

   THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
   OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
   MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
   IN NO EVENT SHALL THE X CONSORTIUM BE LIABLE FOR ANY CLAIM, DAMAGES OR
   OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE,
   ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
   OTHER DEALINGS IN THE SOFTWARE.

   Except as contained in this notice, the name of the X Consortium shall
   not be used in advertising or otherwise to promote the sale, use or
   other dealings in this Software without prior written authorization
   from the X Consortium.

 */

/*
 *    Code to read bitmaps from disk files. Interprets 
 *      data from X10 and X11 bitmap files and creates
 *      Pixmap representations of files. Returns Pixmap
 *      ID and specifics about image.
 *
 *      Modified for speedup by Jim Becker, changed image
 *      data parsing logic (removed some fscanf()s). 
 *      Aug 5, 1988
 *
 * Note that this file and ../Xmu/RdBitF.c look very similar....  Keep them
 * that way (but don't use common source code so that people can have one 
 * without the other).
 */

/*
#include <X11/Xos.h>
#include <stdio.h>
#include <ctype.h>
*/


#define MAX_SIZE 255

/* shared data for the image read/parse logic */
static short hexTable[256];	/* conversion value */
static Bool initialized = False;	/* easier to fill in at run time */


/*
 *    Table index for the hex values. Initialized once, first time.
 *      Used for translation value or delimiter significance lookup.
 */
static void
initHexTable(void)
{
    /*
     * We build the table at run time for several reasons:
     *
     *     1.  portable to non-ASCII machines.
     *     2.  still reentrant since we set the init flag after setting table.
     *     3.  easier to extend.
     *     4.  less prone to bugs.
     */
    hexTable['0'] = 0;
    hexTable['1'] = 1;
    hexTable['2'] = 2;
    hexTable['3'] = 3;
    hexTable['4'] = 4;
    hexTable['5'] = 5;
    hexTable['6'] = 6;
    hexTable['7'] = 7;
    hexTable['8'] = 8;
    hexTable['9'] = 9;
    hexTable['A'] = 10;
    hexTable['B'] = 11;
    hexTable['C'] = 12;
    hexTable['D'] = 13;
    hexTable['E'] = 14;
    hexTable['F'] = 15;
    hexTable['a'] = 10;
    hexTable['b'] = 11;
    hexTable['c'] = 12;
    hexTable['d'] = 13;
    hexTable['e'] = 14;
    hexTable['f'] = 15;

    /* delimiters of significance are flagged w/ negative value */
    hexTable[' '] = -1;
    hexTable[','] = -1;
    hexTable['}'] = -1;
    hexTable['\n'] = -1;
    hexTable['\t'] = -1;

    initialized = True;
}

/*
 *    read next hex value in the input stream, return -1 if EOF
 */
static int
NextInt(FILE * fstream)
{
    int ch;
    int value = 0;
    int gotone = 0;
    int done = 0;

    /* loop, accumulate hex value until find delimiter  */
    /* skip any initial delimiters found in read stream */

    while (!done)
    {
	ch = getc(fstream);
	if (ch == EOF)
	{
	    value = -1;
	    done++;
	}
	else
	{
	    /* trim high bits, check type and accumulate */
	    ch &= 0xff;
	    if (isascii(ch) && isxdigit(ch))
	    {
		value = (value << 4) + hexTable[ch];
		gotone++;
	    }
	    else if ((hexTable[ch]) < 0 && gotone)
		done++;
	}
    }
    return value;
}

static int
XReadBitmapFileData(const char *filename,
		    unsigned int *width,
		    unsigned int *height, unsigned char **data,
		    int *x_hot, int *y_hot)
{
    FILE *fstream;		/* handle on file  */
    unsigned char *bits = NULL;	/* working variable */
    char line[MAX_SIZE];	/* input line from file */
    int size;			/* number of bytes of data */
    char name_and_type[MAX_SIZE];	/* an input line */
    char *type;			/* for parsing */
    int value;			/* from an input line */
    int version10p;		/* boolean, old format */
    int padding;		/* to handle alignment */
    int bytes_per_line;		/* per scanline of data */
    unsigned int ww = 0;	/* width */
    unsigned int hh = 0;	/* height */
    int hx = -1;		/* x hotspot */
    int hy = -1;		/* y hotspot */

    /* first time initialization */
    if (initialized == False)
	initHexTable();

    if (!(fstream = fopen(filename, "r")))
	return BitmapOpenFailed;

    /* error cleanup and return macro   */
#define	RETURN(code) \
{ if (bits) XtFree ((char *)bits); fclose (fstream); return code; }

    while (fgets(line, MAX_SIZE, fstream))
    {
	if (strlen(line) == MAX_SIZE - 1)
	    RETURN(BitmapFileInvalid);
	if (sscanf(line, "#define %s %d", name_and_type, &value) == 2)
	{
	    if (!(type = strrchr(name_and_type, '_')))
		type = name_and_type;
	    else
		type++;

	    if (!strcmp("width", type))
		ww = (unsigned int)value;
	    if (!strcmp("height", type))
		hh = (unsigned int)value;
	    if (!strcmp("hot", type))
	    {
		if (type-- == name_and_type || type-- == name_and_type)
		    continue;
		if (!strcmp("x_hot", type))
		    hx = value;
		if (!strcmp("y_hot", type))
		    hy = value;
	    }
	    continue;
	}

	if (sscanf(line, "static short %s = {", name_and_type) == 1)
	    version10p = 1;
	else if (sscanf(line, "static unsigned char %s = {", name_and_type) == 1)
	    version10p = 0;
	else if (sscanf(line, "static char %s = {", name_and_type) == 1)
	    version10p = 0;
	else
	    continue;

	if (!(type = strrchr(name_and_type, '_')))
	    type = name_and_type;
	else
	    type++;

	if (strcmp("bits[]", type))
	    continue;

	if (!ww || !hh)
	    RETURN(BitmapFileInvalid);

	if ((ww % 16) && ((ww % 16) < 9) && version10p)
	    padding = 1;
	else
	    padding = 0;

	bytes_per_line = (ww + 7) / 8 + padding;

	size = bytes_per_line * hh;
	bits = (unsigned char *)XtMalloc((unsigned int)size);
	if (!bits)
	    RETURN(BitmapNoMemory);

	if (version10p)
	{
	    unsigned char *ptr;
	    int bytes;

	    for (bytes = 0, ptr = bits; bytes < size; (bytes += 2))
	    {
		if ((value = NextInt(fstream)) < 0)
		    RETURN(BitmapFileInvalid);
		*(ptr++) = value;
		if (!padding || ((bytes + 2) % bytes_per_line))
		    *(ptr++) = value >> 8;
	    }
	}
	else
	{
	    unsigned char *ptr;
	    int bytes;

	    for (bytes = 0, ptr = bits; bytes < size; bytes++, ptr++)
	    {
		if ((value = NextInt(fstream)) < 0)
		    RETURN(BitmapFileInvalid);
		*ptr = value;
	    }
	}
    }				/* end while */

    fclose(fstream);
    if (!bits)
	return (BitmapFileInvalid);

    *data = bits;
    *width = ww;
    *height = hh;
    if (x_hot)
	*x_hot = hx;
    if (y_hot)
	*y_hot = hy;

    return (BitmapSuccess);
}
#endif /* XtSpecificationRelease < 6 */


extern int
__MrmReadBitmapFileData(char *Name, unsigned int *width, unsigned int *height,
                        char **data, int *x_hot, int *y_hot)
{
    int stat;

    stat = XReadBitmapFileData(Name, width, height,
                (unsigned char **)data, x_hot, y_hot);
    if (BitmapOpenFailed == stat)
	__MrmExit(LOC, "%s can't be opened\n", Name);
    if (BitmapFileInvalid == stat)
	__MrmExit(LOC, "%s is an invalid bitmap file\n", Name);
    return 1;
}
