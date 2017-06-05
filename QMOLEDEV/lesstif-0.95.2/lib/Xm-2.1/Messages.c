/**
 *
 * Copyright (C) 1997 Peter G. Williams, and Others
 * Copyright (C) 1997-2001 LessTif Development Team
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

#include <Xm/XmP.h>
#include <XmI/MessagesI.h>


const char *XME_WARNING =
    "XmeWarning";

const char *_XmMsgBaseClass_0000 =
    "No context found for extension";

const char *_XmMsgBaseClass_0001 =
    "_XmPopWidgetExtData; no extension found with XFindContext";

const char *_XmMsgBaseClass_0002 =
    "XmFreeWidgetExtData is an unsupported routine";

const char *_XmMsgBulletinB_0001 =
    "Incorrect dialog style; must be XmDIALOG_MODELESS.";

const char *_XmMsgCascadeB_0000 =
    "XmCascadeButton[Gadget] must have XmRowColumn parent with \n"
    "XmNrowColumnType XmMENU_PULLDOWN, XmMENU_POPUP, XmMENU_BAR or XmMENU_OPTION.";

const char *_XmMsgCascadeB_0001 =
    "Only XmRowColumn widgets of type XmMENU_PULLDOWN can be submenus.";

const char *_XmMsgCascadeB_0002 =
    "XmNmappingDelay must be greater than or equal to 0.";

const char *_XmMsgCascadeB_0003 =
    "XtGrabPointer failed";

const char *_XmMsgComboBox_0000 =
    "Applications cannot add children to XmComboBox widgets.";

const char *_XmMsgComboBox_0001 =
    "XmNcomboBoxType resource cannot be set by XtSetValues.";

const char *_XmMsgComboBox_0002 =
    "XmFontListGetNextFont failed.";

const char *_XmMsgComboBox_0003 =
    "XmFontListInitFontContext failed.";

const char *_XmMsgComboBox_0004 =
    "Internal widget has been destroyed.  Behavior is undefined";

const char *_XmMsgComboBox_0005 =
    "Internal widget has been unmanaged.  Behavior is undefined";

const char *_XmMsgComboBox_0006 =
    "XmQUICK_NAVIGATE is only valid for ComboBoxes of XmNcomboBoxType XmDROP_DOWN_LIST";

const char *_XmMsgComboBox_0007 =
    "Action invoked with the wrong number of parameters.";

const char *_XmMsgComboBox_0008 =
    "Action routine called from a widget that is not a descendant of ComboBox";

const char *_XmMsgCommand_0000 =
    "The dialog type must be XmDIALOG_COMMAND.";

const char *_XmMsgCommand_0001 =
    "Invalid child type; Command widget does not have this child.";

const char *_XmMsgCommand_0002 =
    "NULL or empty XmString";

const char *_XmMsgCommand_0003 =
    "NULL or empty XmString passed to XmCommandAppendValue.";

const char *_XmMsgCommand_0004 =
    "XmNmustMatch is always False for a Command widget.";

const char *_XmMsgCommand_0005 =
    "XmNhistoryMaxItems must be a positive integer greater than zero.";

const char *_XmMsgContainer_0000 =
    "Action invoked with the wrong number of parameters.";

const char *_XmMsgCutPaste_0000 =
    "Must call XmClipboardStartCopy() before XmClipboardCopy()";

const char *_XmMsgCutPaste_0001 =
    "Must call XmClipboardStartCopy() before XmClipboardEndCopy()";

const char *_XmMsgCutPaste_0002 =
    "Too many formats in XmClipboardCopy()";

const char *_XmMsgCutPaste_0003 =
    "ClipboardBadDataType";

const char *_XmMsgCutPaste_0004 =
    "bad data type";

const char *_XmMsgCutPaste_0005 =
    "ClipboardCorrupt";

const char *_XmMsgCutPaste_0006 =
    "internal error - corrupt data structure";

const char *_XmMsgCutPaste_0007 =
    "ClipboardBadFormat";

const char *_XmMsgCutPaste_0008 =
    "Error - registered format length must be 8, 16, or 32";

const char *_XmMsgCutPaste_0009 =
    "Error - registered format name must be nonnull";

const char *_XmMsgDialogS_0000 =
    "DialogShell widget only supports one RectObj child";

const char *_XmMsgDisplay_0001 =
    "Creating multiple XmDisplays for the same X display.  Only the\n"
    "first XmDisplay created for a particular X display can be referenced\n"
    "by calls to XmGetXmDisplay";

const char *_XmMsgDisplay_0002 =
    "Received TOP_LEVEL_LEAVE with no active DragContext";

const char *_XmMsgDisplay_0003 =
    "Cannot set XmDisplay class to a non-subclass of XmDisplay";

const char *_XmMsgDragBS_0000 =
    "_MOTIF_DRAG_WINDOW property has been destroyed";

const char *_XmMsgDragBS_0001 =
    "The protocol version levels do not match.";

const char *_XmMsgDragBS_0002 =
    "Unable to open display.";

const char *_XmMsgDragBS_0003 =
    "The atom table is empty.";

const char *_XmMsgDragBS_0004 =
    "The target table is empty.";

const char *_XmMsgDragBS_0005 =
    "The target table has an inconsistent property.";

const char *_XmMsgDragBS_0006 =
    "Invalid target table index.";

const char *_XmMsgDragC_0001 =
    "GenerateCallback does not expect XmCR_DROP_SITE_ENTER as a reason";

const char *_XmMsgDragC_0002 =
    "Invalid selection in DropConvertCallback";

const char *_XmMsgDragC_0003 =
    "The drop selection was lost.";

const char *_XmMsgDragC_0004 =
    "XGrabPointer failed";

const char *_XmMsgDragC_0005 =
    "ExternalNotifyHandler: the callback reason is not acceptable";

const char *_XmMsgDragC_0006 =
    "XmDragStart must be called as a result of a button press or motion event";

const char *_XmMsgDragICC_0000 =
    "Unknown drag and drop message type";

const char *_XmMsgDragICC_0001 =
    "The protocol version levels do not match.";

const char *_XmMsgDragIcon_0000 =
    "No geometry specified for dragIcon pixmap";

const char *_XmMsgDragIcon_0001 =
    "A dragIcon created with no pixmap";

const char *_XmMsgDragIcon_0002 =
    "String to Bitmap converter needs Screen argument";

const char *_XmMsgDragOverS_0000 =
    "Depth mismatch";

const char *_XmMsgDragOverS_0001 =
    "Unknown icon attachment";

const char *_XmMsgDragOverS_0002 =
    "Unknown drag state";

const char *_XmMsgDragOverS_0003 =
    "Unknown XmNblendModel";

const char *_XmMsgDragUnder_0000 =
    "Unable to get drop site window geometry";

const char *_XmMsgDragUnder_0001 =
    "Invalid animationPixmapDepth";

const char *_XmMsgDropSMgrI_0001 =
    "Cannot register a drop site that is a descendent of a simple drop site";

const char *_XmMsgDropSMgrI_0002 =
    "Cannot create a discontiguous child list for a composite drop site.";

const char *_XmMsgDropSMgrI_0003 =
    "%s is not a drop site child of %s";

const char *_XmMsgDropSMgr_0001 =
    "Cannot create drop sites that are children of a simple drop site.";

const char *_XmMsgDropSMgr_0002 =
    "Receiving motion events without an active drag context";

const char *_XmMsgDropSMgr_0003 =
    "Receiving operation changed without an active drag context.";

const char *_XmMsgDropSMgr_0004 =
    "Creating an active drop site with no drop procedure.";

const char *_XmMsgDropSMgr_0005 =
    "Cannot set rectangles or number of rectangles of composite drop sites.";

const char *_XmMsgDropSMgr_0006 =
    "Registering a widget as a drop site out of sequence.\n"
    "Ancestors must be registered before any of their \n"
    "descendants are registered.";

const char *_XmMsgDropSMgr_0007 =
    "Cannot register widget as a drop site more than once.";

const char *_XmMsgDropSMgr_0008 =
    "Drop site type may only be set at creation time.";

const char *_XmMsgDropSMgr_0009 =
    "Cannot change rectangles of non-simple drop site.";

const char *_XmMsgDropSMgr_0010 =
    "Cannot register a shell as a drop site.";

const char *_XmMsgForm_0000 =
    "Fraction base cannot be zero.";

const char *_XmMsgForm_0002 =
    "Circular dependency in Form children.";

const char *_XmMsgForm_0003 =
    "Bailed out of edge synchronization after 10,000 iterations.\n"
    "Check for contradictory constraints on the children of this form.";

const char *_XmMsgForm_0004 =
    "Attachment widget must have same parent as widget.";

const char *_XmMsgGadget_0000 =
    "Cannot change XmNlayoutDirection after initialization.";

const char *_XmMsgGeoUtils_0000 =
    "Failure of geometry request to \"almost\" reply";

const char *_XmMsgGeoUtils_0001 =
    "Invalid layout of children found";

const char *_XmMsgGeoUtils_0002 =
    "Invalid order found in XmSelectionBox";

const char *_XmMsgGetSecRes_0000 =
    "getLabelSecResData: Not enough memory";

const char *_XmMsgGrabS_0000 =
    "XmPopup requires a subclass of shellWidgetClass.";

const char *_XmMsgLabel_0003 =
    "Invalid XmNlabelString - must be a compound string";

const char *_XmMsgLabel_0004 =
    "Invalid XmNacceleratorText - must be a compound string";

const char *_XmMsgList_0000 =
    "When changed, XmNvisibleItemCount must be at least 1.";

const char *_XmMsgList_0005 =
    "Cannot change XmNlistSizePolicy after initialization.";

const char *_XmMsgList_0006 =
    "When changed, XmNitemCount must be nonnegative.";

const char *_XmMsgList_0007 =
    "Item(s) to be deleted are not present is list.";

const char *_XmMsgList_0008 =
    "XmNlistSpacing must be nonnegative.";

const char *_XmMsgList_0009 =
    "Cannot set XmNitems to NULL when XmNitemCount is positive.";

const char *_XmMsgList_0010 =
    "When changed, XmNselectedItemCount must be nonnegative.";

const char *_XmMsgList_0011 =
    "Cannot set XmNselectedItems to NULL when XmNselectedItemCount is positive.";

const char *_XmMsgList_0012 =
    "XmNtopItemPosition must be nonnegative.";

const char *_XmMsgList_0013 =
    "XmNitems and XmNitemCount mismatch!";

const char *_XmMsgList_0014 =
    "When changed, XmNselectedPositionCount must be nonnegative.";

const char *_XmMsgList_0015 =
    "Cannot set XmNselectedPositions to NULL when XmNselectedPositionCount is positive.";

const char *_XmMsgMainW_0000 =
    "The Menu Bar cannot be changed to NULL.";

const char *_XmMsgMainW_0001 =
    "The Command Window cannot be changed to NULL.";

const char *_XmMsgManager_0000 =
    "Widget class %s has invalid CompositeClassExtension record";

const char *_XmMsgManager_0001 =
    "Cannot change XmNlayoutDirection or XmNstringDirection after initialization.";

const char *_XmMsgMenuShell_0000 =
    "MenuShell widgets only accept XmRowColumn children.";

const char *_XmMsgMenuShell_0001 =
    "Attempting to manage a pulldown menu that is not attached \n"
    "to a cascade button.";

const char *_XmMsgMenuShell_0002 =
    "XmPopup requires a subclass of shellWidgetClass";

const char *_XmMsgMenuShell_0003 =
    "XmPopdown requires a subclass of shellWidgetClass";

const char *_XmMsgMenuShell_0004 =
    "XtMenuPopup wants exactly one argument";

const char *_XmMsgMenuShell_0005 =
    "XtMenuPopup only supports on ButtonPress, KeyPress or EnterNotify events.";

const char *_XmMsgMenuShell_0006 =
    "Cannot find popup widget \"%s\" in XtMenuPopup";

const char *_XmMsgMenuShell_0007 =
    "Cannot find popup widget \"%s\" in XtMenuPopdown";

const char *_XmMsgMenuShell_0008 =
    "XtMenuPopdown called with more than one argument";

const char *_XmMsgMenuShell_0009 =
    "Cannot change XmNlayoutDirection after initialization.";

const char *_XmMsgMessageB_0003 =
    "Invalid child type; widget does not have this child";

const char *_XmMsgMessageB_0004 =
    "Cancel button cannot be changed directly.";

const char *_XmMsgMotif_0000 =
    "\n"
    "Name: %s\n"
    "Class: %s\n"
    "";

const char *_XmMsgMotif_0001 =
    "Action invoked with the wrong number of parameters.";

const char *_XmMsgNavigMap_0000 =
    "_XmNavigate called with invalid direction";

const char *_XmMsgNotebook_0000 =
    "XmNnotebookChildType resource cannot be set by XtSetValues.";

const char *_XmMsgPanedW_0000 =
    "Invalid minimum value; must be greater than 0.";

const char *_XmMsgPanedW_0001 =
    "Invalid maximum value; must be greater than 0.";

const char *_XmMsgPanedW_0002 =
    "Invalid minimum/maximum value, minimum must be < maximum.";

const char *_XmMsgPanedW_0003 =
    "Constraints do not allow appropriate sizing.";

const char *_XmMsgPanedW_0004 =
    "Too few parameters in sash callback.";

const char *_XmMsgPanedW_0005 =
    "Invalid first parameter in sash callback.";

const char *_XmMsgPixConv_0000 =
    "Wrong number of parameters for CvtStringToPixmap";

const char *_XmMsgPrimitive_0000 =
    "Cannot change XmNlayoutDirection after initialization.";

const char *_XmMsgProtocols_0000 =
    "Widget must be a vendor shell";

const char *_XmMsgProtocols_0001 =
    "Protocol manager already exists.";

const char *_XmMsgProtocols_0002 =
    "There are more protocols than widget can handle";

const char *_XmMsgRegion_0000 =
    "Memory error";

const char *_XmMsgRepType_0001 =
    "Illegal representation type id";

const char *_XmMsgRepType_0002 =
    "Illegal value (%d) for rep type XmR%s";

const char *_XmMsgResConvert_0000 =
    "FetchUnitType: bad widget class";

const char *_XmMsgResConvert_0001 =
    "Improperly defined default list! exiting...";

const char *_XmMsgResConvert_0002 =
    "Missing colon in font string \"%s\"; any remaining fonts in list unparsed";

const char *_XmMsgResConvert_0003 =
    "Invalid delimiter in tag \"%s\"; any remaining fonts in list unparsed";

const char *_XmMsgResConvert_0004 =
    "Unmatched quotation marks in string \"%s\"; any remaining fonts in list unparsed";

const char *_XmMsgResConvert_0005 =
    "Unmatched quotation marks in tag \"%s\"; any remaining fonts in list unparsed";

const char *_XmMsgResConvert_0006 =
    "Null tag found when converting to type %s; any remaining fonts in list unparsed";

const char *_XmMsgResConvert_0007 =
    "Cannot convert XmString to compound text";

const char *_XmMsgResConvert_0008 =
    "Insufficient memory for XmbTextListToTextProperty";

const char *_XmMsgResConvert_0009 =
    "Locale not supported for XmbTextListToTextProperty";

const char *_XmMsgResConvert_0010 =
    "XmbTextListToTextProperty failed";

const char *_XmMsgResConvert_0011 =
    "Cannot convert widget name to Widget.";

const char *_XmMsgResConvert_0012 =
    "Cannot convert compound text to XmString";

const char *_XmMsgResConvert_0013 =
    "Cannot convert XmString to compound text";

const char *_XmMsgResConvert_0014 =
    "FetchUnitType called without a widget to reference";

const char *_XmMsgResConvert_0015 =
    "FetchDisplayArg called without a widget to reference";

const char *_XmMsgResConvert_0016 =
    "FetchWidgetArg called without a widget to reference";

const char *_XmMsgRowColText_0024 =
    "XtGrabKeyboard failed";

const char *_XmMsgRowColumn_0000 =
    "Attempt to set width to zero ignored";

const char *_XmMsgRowColumn_0001 =
    "Attempt to set height to zero ignored";

const char *_XmMsgRowColumn_0002 =
    "XmNhelpWidget not used by popup menus: forced to NULL";

const char *_XmMsgRowColumn_0003 =
    "XmNhelpWidget not used by pulldown menus: forced to NULL";

const char *_XmMsgRowColumn_0004 =
    "XmNhelpWidget not used by option menus: forced to NULL";

const char *_XmMsgRowColumn_0005 =
    "XmNhelpWidget not used by work areas: forced to NULL";

const char *_XmMsgRowColumn_0007 =
    "Widget hierarchy not appropriate for this XmNrowColumnType:\n"
    "defaulting to XmWORK_AREA";

const char *_XmMsgRowColumn_0008 =
    "Attempt to change XmNrowColumnType after initialization ignored";

const char *_XmMsgRowColumn_0015 =
    "Attempt to set XmNisHomogenous to FALSE for a RowColumn widget of type XmMENU_BAR ignored";

const char *_XmMsgRowColumn_0016 =
    "Attempt to change XmNentryClass for a RowColumn widget of type XmMENU_BAR ignored";

const char *_XmMsgRowColumn_0017 =
    "Attempt to change XmNwhichButton via XtSetValues for a RowColumn widget of type XmMENU_PULLDOWN ignored";

const char *_XmMsgRowColumn_0018 =
    "Attempt to change XmNmenuPost via XtSetValues for a RowColumn widget of type XmMENU_PULLDOWN ignored";

const char *_XmMsgRowColumn_0019 =
    "Attempt to set XmNmenuPost to an illegal value ignored";

const char *_XmMsgRowColumn_0020 =
    "Attempt to change XmNshadowThickness for a RowColumn widget not of type XmMENU_PULLDOWN or XmMENU_POPUP ignored";

const char *_XmMsgRowColumn_0022 =
    "Attempt to add wrong type child to a menu (that is, XmRowColumn) widget";

const char *_XmMsgRowColumn_0023 =
    "Attempt to add wrong type child to a homogeneous RowColumn widget";

const char *_XmMsgRowColumn_0025 =
    "Attempt to change XmNisHomogeneous for a RowColumn widget of type XmMENU_OPTION ignored";

const char *_XmMsgRowColumn_0026 =
    "Tear off enabled on a shared menupane: allowed but not recommended";

const char *_XmMsgRowColumn_0027 =
    "Illegal mnemonic character;  Could not convert X KEYSYM to a keycode";

const char *_XmMsgScaleScrBar_0004 =
    "Incorrect processing direction.";

const char *_XmMsgScale_0000 =
    "The scale minumum value is greater than or equal to the scale maximum value.";

const char *_XmMsgScale_0001 =
    "The specified scale value is less than the minimum scale value.";

const char *_XmMsgScale_0002 =
    "The specified scale value is greater than the maximum scale value.";

const char *_XmMsgScale_0005 =
    "Invalid highlight thickness.";

const char *_XmMsgScale_0006 =
    "Invalid XmNscaleMultiple; greater than (max - min)";

const char *_XmMsgScale_0007 =
    "Invalid XmNscaleMultiple; less than zero";

const char *_XmMsgScale_0008 =
    "(Maximum - minimum) cannot be greater than INT_MAX / 2;\n"
    "minimum has been set to zero, maximum may have been set to (INT_MAX/2).";

const char *_XmMsgScale_0009 =
    "XmNshowValue has an incorrect value";

const char *_XmMsgScreen_0000 =
    "Icon screen mismatch";

const char *_XmMsgScreen_0001 =
    "Cannot get XmScreen because XmDisplay was not found.";

const char *_XmMsgScrollBar_0000 =
    "The scrollbar minimum value is greater than or equal to\n"
    "the scrollbar maximum value.";

const char *_XmMsgScrollBar_0001 =
    "The specified slider size is less than 1";

const char *_XmMsgScrollBar_0002 =
    "The specified scrollbar value is less than the minimum\n"
    "scrollbar value.";

const char *_XmMsgScrollBar_0003 =
    "The specified scrollbar value is greater than the maximum\n"
    "scrollbar value minus the scrollbar slider size.";

const char *_XmMsgScrollBar_0004 =
    "The scrollbar increment is less than 1.";

const char *_XmMsgScrollBar_0005 =
    "The scrollbar page increment is less than 1.";

const char *_XmMsgScrollBar_0006 =
    "The scrollbar initial delay is less than 1.";

const char *_XmMsgScrollBar_0007 =
    "The scrollbar repeat delay is less than 1.";

const char *_XmMsgScrollBar_0008 =
    "Specified slider size is greater than the scrollbar maximum\n"
    "value minus the scrollbar minimum value.";

const char *_XmMsgScrollFrameT_0000 =
    "AssocNavigator requires a navigator trait";

const char *_XmMsgScrollFrameT_0001 =
    "DeAssocNavigator requires a navigator trait";

const char *_XmMsgScrollVis_0000 =
    "Wrong parameters passed to the XmScrollVisible function.";

const char *_XmMsgScrolledW_0004 =
    "Cannot change scrolling policy after initialization.";

const char *_XmMsgScrolledW_0005 =
    "Cannot change visual policy after initialization.";

const char *_XmMsgScrolledW_0006 =
    "Cannot set AS_NEEDED scrollbar policy with a\n"
    "visual policy of VARIABLE.";

const char *_XmMsgScrolledW_0007 =
    "Cannot change scrollbar widget in AUTOMATIC mode.";

const char *_XmMsgScrolledW_0008 =
    "Cannot change clip window";

const char *_XmMsgScrolledW_0009 =
    "Cannot set visual policy of CONSTANT in APPLICATION_DEFINED mode.";

const char *_XmMsgSelectioB_0001 =
    "Dialog type cannot be modified.";

const char *_XmMsgSelectioB_0002 =
    "Invalid child type; widget does not have this child.";

const char *_XmMsgSpinB_0001 =
    "Invalid value for XmNarrowLayout.";

const char *_XmMsgSpinB_0002 =
    "XmNminimumValue equals XmNmaximumValue.";

const char *_XmMsgSpinB_0003 =
    "No items supplied for XmSTRING child.";

const char *_XmMsgSpinB_0004 =
    "XmNincrementValue cannot be 0. A value of 1 will be used.";

const char *_XmMsgSpinB_0005 =
    "Spin direction specified by XmNincrementValue\n"
    "has been reversed to match the specified\n"
    "XmNminimumValue and XmNmaximumValue.";

const char *_XmMsgSpinB_0006 =
    "XmNposition out of range; minimum XmNposition used.";

const char *_XmMsgSpinB_0007 =
    "XmNposition out of range; maximum XmNposition used.";

const char *_XmMsgTextFWcs_0000 =
    "Character '%s' not supported in font.  Discarded.";

const char *_XmMsgTextFWcs_0001 =
    "Cannot use multibyte locale without a fontset.  Value discarded.";

const char *_XmMsgTextF_0000 =
    "Invalid cursor position; must be greater than or equal to 0.";

const char *_XmMsgTextF_0001 =
    "Invalid number of columns; must be greater than 0.";

const char *_XmMsgTextF_0002 =
    "XmFontListInitFontContext failed.";

const char *_XmMsgTextF_0003 =
    "XmFontListGetNextFont failed.";

const char *_XmMsgTextF_0004 =
    "Character '%s' not supported in font.  Discarded.";

const char *_XmMsgTextF_0005 =
    "XmNtraversalOn must always be true.";

const char *_XmMsgTextF_0006 =
    "Invalid number of columns; must be greater than or equal to 0.";

const char *_XmMsgTextIn_0000 =
    "Cannot find position while attempting to move to previous line.";

const char *_XmMsgTextOut_0000 =
    "Invalid rows; must be greater than 0";

const char *_XmMsgText_0000 =
    "Invalid source; source ignored.";

const char *_XmMsgText_0002 =
    "Text widget is editable; XmNtraversalOn must be true.";

const char *_XmMsgTransfer_0000 =
    "Calling SelectionCallbackWrapper when transfers should be finished";

const char *_XmMsgTransfer_0001 =
    "Cannot lock the clipboard; aborting transfer";

const char *_XmMsgTransfer_0002 =
    "The format and type of the callback supplied data does not match the data being merged";

const char *_XmMsgTransfer_0003 =
    "The status in the XmConvertCallbackStruct is not XmCONVERT_MERGE";

const char *_XmMsgTransfer_0004 =
    "CONVERT_MORE is not yet supported";

const char *_XmMsgTransfer_0005 =
    "Bad atom value found";

const char *_XmMsgTransfer_0006 =
    "Warning: Attempt to start a MULTIPLE transfer when one is in progress";

const char *_XmMsgTransfer_0007 =
    "Warning: Attempt to send a MULTIPLE transfer when one is not in progress";

const char *_XmMsgVaSimple_0000 =
    "XtVaTypedArg conversion needs nonnull widget handle";

const char *_XmMsgVaSimple_0001 =
    "Unable to find type of resource for conversion";

const char *_XmMsgVaSimple_0002 =
    "Type conversion failed";

const char *_XmMsgVendorE_0000 =
    "String to noop conversion needs no extra arguments";

const char *_XmMsgVendorE_0005 =
    "FetchUnitType called without a widget to reference";

const char *_XmMsgVendor_0000 =
    "Invalid value for XmNdeleteResponse";

const char *_XmMsgVendor_0001 =
    "Invalid value for XmNinputPolicy";

const char *_XmMsgVendor_0002 =
    "XmNlayoutDirection cannot be changed";

const char *_XmMsgVendor_0003 =
    "Fatal: \n"
    "_XmGetDefaultDisplay cannot be used prior to VendorS.Initialize, returns NULL";

const char *_XmMsgVisual_0000 =
    "Invalid color requested from _XmAccessColorData";

const char *_XmMsgVisual_0001 =
    "Cannot allocate colormap entry for default background";

const char *_XmMsgVisual_0002 =
    "Cannot parse default background color specification";

const char *_XmMsgXmIm_0000 =
    "Cannot open input method - using XLookupString";

const char *_XmMsgXmRenderT_0000 =
    "XmNtag cannot be NULL.  Setting to empty string.";

const char *_XmMsgXmRenderT_0001 =
    "Display is NULL.  Cannot load font.";

const char *_XmMsgXmRenderT_0002 =
    "XmNfontType invalid.  Cannot load font.";

const char *_XmMsgXmRenderT_0003 =
    "Conversion failed.  Cannot load font.";

const char *_XmMsgXmRenderT_0004 =
    "XmNfontType set to XmAS_IS.  Cannot load font.";

const char *_XmMsgXmRenderT_0005 =
    "XmNloadModel is XmLOAD_IMMEDIATE but XmNfont and XmNfontName not specified.\n"
    "Cannot load font.";

const char *_XmMsgXmSelect_0000 =
    "Internal error: no selection property context for display";

const char *_XmMsgXmSelect_0001 =
    "Selection owner returned type INCR property with format != 32";

const char *_XmMsgXmSelect_0002 =
    "XtGetSelectionRequest called for widget \"%s\" outside of ConvertSelection proc";

const char *_XmMsgXmString_0000 =
    "No font found.";

const char *_XmMsgXmTabList_0000 =
    "Tab value cannot be negative.";
