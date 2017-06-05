/**
 *
 * $Header: /cvsroot/lesstif/lesstif/lib/Xm-2.1/ResConvert.c,v 1.16 2006/04/19 18:42:22 dannybackx Exp $
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

static const char rcsid[] = "$Id: ResConvert.c,v 1.16 2006/04/19 18:42:22 dannybackx Exp $";

#include <LTconfig.h>

#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <ctype.h>
#include <string.h>

#include <X11/Xlib.h>
#include <X11/Xfuncproto.h>
#include <X11/Xresource.h>

#include <XmI/XmI.h>
#include <Xm/XmP.h>
#include <Xm/AtomMgr.h>
#include <Xm/RepType.h>
#include <Xm/BulletinBP.h>
#include <Xm/MenuShellP.h>
#include <Xm/VendorSEP.h>
#include <Xm/ExtObjectP.h>
#include <Xm/XmosP.h>
#include <XmI/LTmisc.h>

#include <XmI/DebugUtil.h>

#if	USE_XFT
#include <X11/Xft/Xft.h>
#endif

enum
{
    ST_ATOM_LIST, ST_BUTTON_TYPE, ST_KEY_SYM_TABLE, ST_STRING_TABLE,
    ST_XM_STRING_TABLE
};

static Boolean _XmCvtStringToXmString(Display *, XrmValue *,
		   Cardinal *, XrmValue *, XrmValue *, XtPointer *);
static Boolean _XmCvtStringToXmFontList(Display *, XrmValue *,
		   Cardinal *, XrmValue *, XrmValue *, XtPointer *);
static Boolean _XmCvtStringToCardinal(Display *, XrmValue *,
		   Cardinal *, XrmValue *, XrmValue *, XtPointer *);
static Boolean _XmCvtStringToKeySym(Display *, XrmValue *,
		   Cardinal *, XrmValue *, XrmValue *, XtPointer *);
static Boolean _XmCvtStringToResIndPosition(Display *, XrmValue *,
		   Cardinal *, XrmValue *, XrmValue *, XtPointer *);
static Boolean _XmCvtStringToResIndDimension(Display *, XrmValue *,
		   Cardinal *, XrmValue *, XrmValue *, XtPointer *);
static Boolean _XmCvtStringToResIndInt(Display *, XrmValue *,
		   Cardinal *, XrmValue *, XrmValue *, XtPointer *);
static Boolean _XmCvtStringToWidget(Display *, XrmValue *,
		   Cardinal *, XrmValue *, XrmValue *, XtPointer *);
static Boolean _XmCvtStringToWindow(Display *, XrmValue *,
		   Cardinal *, XrmValue *, XrmValue *, XtPointer *);
static Boolean _XmCvtStringToBooleanDimension(Display *, XrmValue *,
		   Cardinal *, XrmValue *, XrmValue *, XtPointer *);
static Boolean _XmCvtStringToChar(Display *, XrmValue *,
		   Cardinal *, XrmValue *, XrmValue *, XtPointer *);
static Boolean _XmCvtStringToTextPosition(Display *, XrmValue *,
		   Cardinal *, XrmValue *, XrmValue *, XtPointer *);
static Boolean _XmCvtStringToTopItemPosition(Display *, XrmValue *,
		   Cardinal *, XrmValue *, XrmValue *, XtPointer *);
static Boolean _XmCvtStringToSomeTable(Display *, XrmValue *,
		   Cardinal *, XrmValue *, XrmValue *, XtPointer *);

static void destroy_font_list(XtAppContext app, XrmValue *to,
			      XtPointer converer_data, XrmValue *args,
			      Cardinal *num_args);
static void destroy_table(XtAppContext app, XrmValue *to,
			  XtPointer converer_data, XrmValue *args,
			  Cardinal *num_args);
static void destroy_string_table(XtAppContext app, XrmValue *to,
				 XtPointer converer_data, XrmValue *args,
				 Cardinal *num_args);
static void destroy_xm_string_table(XtAppContext app, XrmValue *to,
				    XtPointer converer_data, XrmValue *args,
				    Cardinal *num_args);
static void get_unit_type(Widget w, Cardinal *size, XrmValue *val);

static Boolean _XmCvtStringToTabList(Display *display, XrmValue *args,
		       Cardinal *num_args, XrmValue *from, XrmValue *to,
		       XtPointer *converter_data);
static Boolean _XmCvtStringToRenderTable(Display *display, XrmValue *args,
		       Cardinal *num_args, XrmValue *from, XrmValue *to,
		       XtPointer *converter_data);
static void _XmCvtRenderTableFree(XtAppContext app, XrmValue *to,
		     XtPointer converter_data, XrmValue *args,
		     Cardinal *num_args);
#if	USE_XFT
static Boolean XmuCvtStringToXftColor(Display *dpy,
                       XrmValue *args, Cardinal *num_args,
                       XrmValue *fromVal, XrmValue *toVal,
                       XtPointer *converter_data);
static void XmuFreeXftColor(XtAppContext app, XrmValuePtr toVal, XtPointer closure,
                 XrmValuePtr args, Cardinal *num_args);
#endif	/* USE_XFT */


/*
   xmUseVersion is used for a sanity check by developers.  It is used within
   a M*tif applications to ensure the version of the (shared) M*tif library
   being used corresponds to the M*tif toolkit header the application was
   compiled against.  8/3/98, pgw@ma.ultranet.com
   amai: it seems that at least OM 2.1 initializes this number
         immediately, instead of waiting for XmRegisterConverters()
	 to be called. We try to stay as compatible as possible here ...
 */
#if XmVersion < 2001
int xmUseVersion = 0;
#else
int xmUseVersion = XmVersion;
#endif

extern char *XmVersionString(void)
{
	return LesstifVERSION_STRING;
}


/* please be careful if you're editting these arrays, as
 * the strings have to be in the _same_ order as the
 * enumerated constants in the include files.  Failure to comply
 * will bring about the death of your firstborn.  Have
 * a nice day.
 */

static char *multi_click[] =
{
    "multiclick_discard",
    "multiclick_keep"
};

static char *packing_styles[] =
{
    "no_packing",
    "pack_tight",
    "pack_column",
    "pack_none"
};

static char *focus_policies[] =
{
    "explicit",
    "pointer"
};

static char *label_types[] =
{
    "pixmap",
    "string"
};
static unsigned char label_type_values[] =
{
    XmPIXMAP,
    XmSTRING
};

static char *vertical_alignments[] =
{
    "alignment_baseline_top",
    "alignment_center",
    "alignment_baseline_bottom",
    "alignment_contents_top",
    "alignment_contents_bottom"
};

static char *child_vertical_alignments[] =
{
    "alignment_baseline_top",
    "alignment_center",
    "alignment_baseline_bottom",
    "alignment_widget_top",
    "alignment_widget_bottom"
};

static char *horizontal_alignments[] =
{
    "alignment_beginning",
    "alignment_center",
    "alignment_end"
};

static char *arrow_directions[] =
{
    "arrow_up",
    "arrow_down",
    "arrow_left",
    "arrow_right"
};

static char *attachments[] =
{
    "attach_none",
    "attach_form",
    "attach_opposite_form",
    "attach_widget",
    "attach_opposite_widget",
    "attach_position",
    "attach_self"
};

static char *audible_warnings[] =
{
    "none",
    "bell"
};

static char *frame_child_types[] =
{
    "frame_generic_child",
    "frame_workarea_child",
    "frame_title_child"
};

static char *delete_responses[] =
{
    "destroy",
    "unmap",
    "do_nothing"
};

static char *navigation_types[] =
{
    "none",
    "tab_group",
    "sticky_tab_group",
    "exclusive_tab_group"
};

static char *orientations[] =
{
    "vertical",
    "horizontal",
};
static unsigned char orientation_values[] =
{
    XmVERTICAL,
    XmHORIZONTAL
};

static char *protocol_styles[] =
{
    "drag_none",
    "drag_drop_only",
    "drag_prefer_preregister",
    "drag_preregister",
    "drag_prefer_dynamic",
    "drag_dynamic",
    "drag_prefer_receiver"
};

static char *scrollbar_placement[] =
{
    "bottom_right",
    "top_right",
    "bottom_left",
    "top_left"
};

static char *scrolling_policy[] =
{
    "automatic",
    "application_defined"
};

static char *scrollbar_policy[] =
{
    "static",
    "as_needed"
};

static char *edit_mode[] =
{
    "multi_line_edit",
    "single_line_edit"
};

static char *unit_types[] =
{
    "pixels",
    "100th_millimeters",
    "1000th_inches",
    "100th_points",
    "100th_font_units"
};

/*
 * These are the values of XmNdialogType for XmMessageBox
 *	This is the value type XmRDialogType.
 *	The class of this resource is XmCDialogType.
 */
static char *dialog_types[] =
{
    "dialog_template",
    "dialog_error",
    "dialog_information",
    "dialog_message",
    "dialog_question",
    "dialog_warning",
    "dialog_working"
};

/*
 * These are the values of XmNdialogType for XmSelectionBox
 *	This is the value type XmRSelectionType.
 *	The class of this resource is (also) XmCDialogType
 */
static char *selection_dialog_types[] = {
	"dialog_work_area",
	"dialog_prompt",
	"dialog_selection",
	"dialog_command",
	"dialog_file_selection",
};

static char *dialog_styles[] =
{
    "dialog_modeless",
    "dialog_primary_application_modal",
    "dialog_full_application_modal",
    "dialog_system_modal"
};

static char *shadow_types[] =
{
    "shadow_in",
    "shadow_out",
    "shadow_etched_in",
    "shadow_etched_out"
};
static unsigned char shadow_type_values[] =
{
    XmSHADOW_IN,
    XmSHADOW_OUT,
    XmSHADOW_ETCHED_IN,
    XmSHADOW_ETCHED_OUT
};

static char *separator_types[] =
{
    "no_line",
    "single_line",
    "double_line",
    "single_dashed_line",
    "double_dashed_line",
    "shadow_etched_in",
    "shadow_etched_out",
    "shadow_etched_in_dash",
    "shadow_etched_out_dash"
};

static char *row_column_types[] =
{
    "work_area",
    "menu_bar",
    "menu_pulldown",
    "menu_popup",
    "menu_option"
};
static unsigned char row_column_type_values[] =
{
    XmWORK_AREA,
    XmMENU_BAR,
    XmMENU_PULLDOWN,
    XmMENU_POPUP,
    XmMENU_OPTION
};

static char *indicator_types[] =
{
    "n_of_many",
    "one_of_many",
    "one_of_many_round",
    "one_of_many_diamond"
};
static unsigned char indicator_type_values[] =
{
    XmN_OF_MANY,
    XmONE_OF_MANY,
    XmONE_OF_MANY_ROUND,
    XmONE_OF_MANY_DIAMOND
};

static char *resize_policies[] =
{
    "resize_none",
    "resize_grow",
    "resize_any"
};

static char *extension_types[] =
{
    "cache_extension",
    "desktop_extension",
    "shell_extension",
    "protocol_extension",
    "default_extension"
};

static unsigned char extension_type_values[] =
{
    XmCACHE_EXTENSION,
    XmDESKTOP_EXTENSION,
    XmSHELL_EXTENSION,
    XmPROTOCOL_EXTENSION,
    XmDEFAULT_EXTENSION
};

static char *icon_attachments[] =
{
    "attach_north_west",
    "attach_north",
    "attach_north_east",
    "attach_east",
    "attach_south_east",
    "attach_south",
    "attach_south_west",
    "attach_west",
    "attach_center",
    "attach_hot"
};

static char *transfer_statuses[] =
{
    "transfer_failure",
    "transfer_success"
};

static char *file_types[] =
{
    "file_directory",
    "file_regular",
    "file_any_type"
};
static unsigned char file_type_values[] =
{
    XmFILE_DIRECTORY,
    XmFILE_REGULAR,
    XmFILE_ANY_TYPE
};

static char *string_directions[] =
{
    "string_direction_l_to_r",
    "string_direction_r_to_l"
};

static char *command_locations[] =
{
    "command_above_workspace",
    "command_below_workspace"
};

static char *default_button_types[] =
{
    "dialog_none",
    "dialog_cancel_button",
    "dialog_ok_button",
    "dialog_help_button"
};
static unsigned char default_button_type_values[] =
{
    XmDIALOG_NONE,
    XmDIALOG_CANCEL_BUTTON,
    XmDIALOG_OK_BUTTON,
    XmDIALOG_HELP_BUTTON
};

static char *processing_directions[] =
{
    "max_on_top",
    "max_on_bottom",
    "max_on_left",
    "max_on_right"
};

static char *unpost_behaviours[] =
{
    "unpost",
    "unpost_and_replay"
};

static char *visual_policies[] =
{
    "variable",
    "constant"
};

static char *child_placements[] =
{
    "place_top",
    "place_above_selection",
    "place_below_selection"
};

static char *selection_policies[] =
{
    "single_select",
    "multiple_select",
    "extended_select",
    "browse_select"
};

static char *list_size_policies[] =
{
    "variable",
    "constant",
    "resize_if_possible"
};

static XtConvertArgRec parentArgs[] = {
	{	XtBaseOffset,
		(XtPointer)XtOffsetOf(WidgetRec, core.parent),
		sizeof(Widget) }
};

static XtConvertArgRec selfArgs[] = {
	{	XtBaseOffset,
		(XtPointer)XtOffsetOf(WidgetRec, core.self),
		sizeof(Widget) }
};

static char *toggle_mode[] =
{
    "toggle_boolean",
    "toggle_indeterminate"
};

/* ComboBox */

static char *combobox_type[] =
{
   "combo_box",
   "drop_down_combo_box",
   "drop_down_list",
};

static unsigned char combobox_type_values[] =
{
   XmCOMBO_BOX,
   XmDROP_DOWN_COMBO_BOX,
   XmDROP_DOWN_LIST,
};

/* Spinbox */
static char *spb_child_types[] =
{
    "numeric",
    "string"
};

static unsigned char spb_child_type_values[] =
{
    XmNUMERIC,
    XmSTRING
};

static char *spb_arrow_layouts[] =
{
    "arrows_beginning",
    "arrows_end",
    "arrows_split"
};

static unsigned char spb_arrow_layout_values[] =
{
    XmARROWS_END,
    XmARROWS_BEGINNING,
    XmARROWS_SPLIT,
    XmARROWS_FLAT_END,
    XmARROWS_FLAT_BEGINNING
};

/* Notebook */
static char *nb_child_types[] =
{
    "none",
    "page",
    "major_tab",
    "minor_tab",
    "status_area",
    "page_scroller"
};

static unsigned char nb_child_type_values[] =
{
    XmNONE,
    XmPAGE,
    XmMAJOR_TAB,
    XmMINOR_TAB,
    XmSTATUS_AREA, 
    XmPAGE_SCROLLER
};

static char *indicator_on[] =
{
    "indicator_none",
    "indicator_fill",
    "indicator_check",
    "indicator_check_box",
    "indicator_cross",
    "indicator_cross_box",
    "false",			/* Support 1.2 compatible defaults */
    "true",			/* Support 1.2 compatible defaults */
    "off",			/* Boolean compatible values */
    "on",
    "0",
    "1",
    "no",
    "yes",
};

static unsigned char indicator_on_values[] =
{
    XmINDICATOR_NONE,
    XmINDICATOR_FILL,
    XmINDICATOR_CHECK,
    XmINDICATOR_CHECK_BOX,
    XmINDICATOR_CROSS,
    XmINDICATOR_CROSS_BOX,
    XmINDICATOR_NONE,		/* False */
    XmINDICATOR_FILL,		/* True */
    XmINDICATOR_NONE,		/* Off */
    XmINDICATOR_FILL,		/* On */
    XmINDICATOR_NONE,		/* 0 */
    XmINDICATOR_FILL,		/* 1 */
    XmINDICATOR_NONE,		/* no */
    XmINDICATOR_FILL,		/* yes */
};


#ifdef	USE_XFT
static XtConvertArgRec xftColorConvertArgs[] = {
  {XtWidgetBaseOffset, (XtPointer)XtOffsetOf(WidgetRec, core.screen),
	sizeof(Screen *)},
  {XtWidgetBaseOffset, (XtPointer)XtOffsetOf(WidgetRec, core.colormap),
	sizeof(Colormap)}
};

#define	XtRXftColor	"XftColor"
#endif	/* USE_XFT */

static char *filter_styles[] =
{
    "filter_none",
    "filter_hidden_files"
};

static char *path_mode_names[] =
{
	"path_mode_full",
	"path_mode_relative"
};

static char *font_types[] =
{
    "FONT_IS_FONT",
    "FONT_IS_FONTSET",
    "FONT_IS_XOC",
    "FONT_IS_XFT"
};
static unsigned char font_type_values[] =
{
    XmFONT_IS_FONT,
    XmFONT_IS_FONTSET,
    XmFONT_IS_XOC,
    XmFONT_IS_XFT
};
extern void
XmRegisterConverters(void)
{
    /* don't really know the difference between the two, so
       we'll just call the other from here for now. */

    _XmRegisterConverters();
}


extern void
_XmRegisterConverters(void)
{
	static Boolean first = True;
	static XtConvertArgRec immediate_arg = {XtImmediate, 0, sizeof(XtPointer)};
	static XtConvertArgRec horizontal_args[] = {
		{ XtWidgetBaseOffset, (XtPointer)XtOffset(Widget, core.screen), sizeof(Screen *) },
		{ XtImmediate, (XtPointer)XmHORIZONTAL, sizeof(XtPointer) },
		{ XtProcedureArg, (XtPointer)get_unit_type, 0 }
	};
	static XtConvertArgRec vertical_args[] = {
		{ XtWidgetBaseOffset, (XtPointer)XtOffset(Widget, core.screen), sizeof(Screen *) },
		{ XtImmediate, (XtPointer)XmVERTICAL, sizeof(XtPointer) },
		{ XtProcedureArg, (XtPointer)get_unit_type, 0 }
	};

	if (first) {
		first = False;
	} else {
		return;
	}

	/* See comment at declaration at beginning of this file */
#if XmVersion < 2001
	xmUseVersion = XmVersion;
#endif

#define REPTYPE_REGISTER(reptype,array) \
	XmRepTypeRegister((reptype), \
	(array), \
	NULL, XtNumber((array)));
#define REPTYPE_REGISTER_WITH_VALUES(reptype,array,values) \
	XmRepTypeRegister((reptype), \
	(array), \
	(values), XtNumber((array)));

	REPTYPE_REGISTER(XmRMultiClick, multi_click);
	REPTYPE_REGISTER(XmRPacking, packing_styles);
	REPTYPE_REGISTER(XmRKeyboardFocusPolicy, focus_policies);
	REPTYPE_REGISTER(XmRVerticalAlignment, vertical_alignments);
	REPTYPE_REGISTER(XmRChildVerticalAlignment, child_vertical_alignments);
	REPTYPE_REGISTER(XmRAlignment, horizontal_alignments);
	REPTYPE_REGISTER(XmRChildHorizontalAlignment, horizontal_alignments);
	REPTYPE_REGISTER(XmRArrowDirection, arrow_directions);
	REPTYPE_REGISTER(XmRAttachment, attachments);
	REPTYPE_REGISTER(XmRAudibleWarning, audible_warnings);
	REPTYPE_REGISTER(XmRChildType, frame_child_types);
	REPTYPE_REGISTER(XmRDeleteResponse, delete_responses);
	REPTYPE_REGISTER(XmRNavigationType, navigation_types);
	REPTYPE_REGISTER(XmRScrollBarPlacement, scrollbar_placement);
	REPTYPE_REGISTER(XmRScrollingPolicy, scrolling_policy);
	REPTYPE_REGISTER(XmRScrollBarDisplayPolicy, scrollbar_policy);
	REPTYPE_REGISTER(XmREditMode, edit_mode);
	REPTYPE_REGISTER(XmRDragInitiatorProtocolStyle, protocol_styles);
	REPTYPE_REGISTER(XmRDragReceiverProtocolStyle, protocol_styles);
	REPTYPE_REGISTER(XmRUnitType, unit_types);
	REPTYPE_REGISTER(XmRDialogType, dialog_types);
	REPTYPE_REGISTER(XmRSelectionType, selection_dialog_types);
	REPTYPE_REGISTER(XmRDialogStyle, dialog_styles);
	REPTYPE_REGISTER(XmRSeparatorType, separator_types);
	REPTYPE_REGISTER(XmRResizePolicy, resize_policies);
	REPTYPE_REGISTER(XmRIconAttachment, icon_attachments);
	REPTYPE_REGISTER(XmRTransferStatus, transfer_statuses);
	REPTYPE_REGISTER(XmRStringDirection, string_directions);
	REPTYPE_REGISTER(XmRCommandWindowLocation, command_locations);
	REPTYPE_REGISTER(XmRProcessingDirection, processing_directions);
	REPTYPE_REGISTER(XmRUnpostBehavior, unpost_behaviours);
	REPTYPE_REGISTER(XmRVisualPolicy, visual_policies);
	REPTYPE_REGISTER(XmRChildPlacement, child_placements);
	REPTYPE_REGISTER(XmRSelectionPolicy, selection_policies);
	REPTYPE_REGISTER(XmRListSizePolicy, list_size_policies);

	REPTYPE_REGISTER_WITH_VALUES(XmRLabelType, label_types, label_type_values);
	REPTYPE_REGISTER_WITH_VALUES(XmRShadowType, shadow_types, shadow_type_values);
	REPTYPE_REGISTER_WITH_VALUES(XmRRowColumnType, row_column_types, row_column_type_values);
	REPTYPE_REGISTER_WITH_VALUES(XmRExtensionType, extension_types, extension_type_values);
	REPTYPE_REGISTER_WITH_VALUES(XmRFileTypeMask, file_types, file_type_values);
	REPTYPE_REGISTER_WITH_VALUES(XmRDefaultButtonType, default_button_types,
			default_button_type_values);
	REPTYPE_REGISTER_WITH_VALUES(XmROrientation, orientations, orientation_values);
	REPTYPE_REGISTER_WITH_VALUES(XmRIndicatorType, indicator_types, indicator_type_values);

	/* Motif 2.x */
	REPTYPE_REGISTER(XmRToggleMode, toggle_mode);
	REPTYPE_REGISTER_WITH_VALUES(XmRArrowLayout, spb_arrow_layouts, spb_arrow_layout_values);
	REPTYPE_REGISTER_WITH_VALUES(XmRSpinBoxChildType, spb_child_types, spb_child_type_values);
	REPTYPE_REGISTER_WITH_VALUES(XmRIndicatorOn, indicator_on, indicator_on_values);
	REPTYPE_REGISTER_WITH_VALUES(XmRNotebookChildType, nb_child_types, nb_child_type_values);
	REPTYPE_REGISTER_WITH_VALUES(XmRComboBoxType, combobox_type, combobox_type_values);

	REPTYPE_REGISTER(XmRFileFilterStyle, filter_styles);
	REPTYPE_REGISTER(XmRPathMode, path_mode_names);

	XmRepTypeInstallTearOffModelConverter();

	/* now we install the other toolkit converters */

	XtSetTypeConverter(XtRString,	/* source type */
		XmRXmString,	/* target type */
		_XmCvtStringToXmString,	/* converter routine */
		NULL,	/* args for converter routine */
		0,	/* number of args to converter routine */
		XtCacheAll,	/* caching instructions */
		NULL);	/* destructor function */
	/* Don't cache */
	XtSetTypeConverter(XtRString,		/* source type */
		XmRFontList,			/* target type */
		_XmCvtStringToXmFontList,	/* converter routine */
		NULL,				/* args for converter routine */
		0,				/* number of args to converter routine */
		XtCacheByDisplay,		/* caching instructions */
		destroy_font_list);		/* destructor function */
	XtSetTypeConverter(XtRString,	/* source type */
		XmRHorizontalDimension,	/* target type */
		_XmCvtStringToResIndDimension,	/* converter routine */
		horizontal_args,	/* args for converter routine */
		XtNumber(horizontal_args),	/* number of args to converter routine */
		XtCacheNone,	/* caching instructions */
		NULL);	/* destructor function */

	XtSetTypeConverter(XtRString,	/* source type */
		XmRShellHorizDim,	/* target type */
		_XmCvtStringToResIndDimension,	/* converter routine */
		horizontal_args,	/* args for converter routine */
		XtNumber(horizontal_args),	/* number of args to converter routine */
		XtCacheNone,	/* caching instructions */
		NULL);	/* destructor function */

	XtSetTypeConverter(XtRString,	/* source type */
		XmRHorizontalInt,	/* target type */
		_XmCvtStringToResIndInt,	/* converter routine */
		horizontal_args,	/* args for converter routine */
		XtNumber(horizontal_args),	/* number of args to converter routine */
		XtCacheNone,	/* caching instructions */
		NULL);	/* destructor function */

	XtSetTypeConverter(XtRString,	/* source type */
		XmRHorizontalPosition,	/* target type */
		_XmCvtStringToResIndPosition, /* converter routine */
		horizontal_args,	/* args for converter routine */
		XtNumber(horizontal_args),	/* number of args to converter routine */
		XtCacheNone,	/* caching instructions */
		NULL);	/* destructor function */

	XtSetTypeConverter(XtRString,	/* source type */
		XmRShellHorizPos,	/* target type */
		_XmCvtStringToResIndPosition, /* converter routine */
		horizontal_args,	/* args for converter routine */
		XtNumber(horizontal_args),	/* number of args to converter routine */
		XtCacheNone,	/* caching instructions */
		NULL);	/* destructor function */

	XtSetTypeConverter(XtRString,	/* source type */
		XmRShellVertPos,	/* target type */
		_XmCvtStringToResIndPosition,	/* converter routine */
		vertical_args,	/* args for converter routine */
		XtNumber(vertical_args),	/* number of args to converter routine */
		XtCacheNone,	/* caching instructions */
		NULL);	/* destructor function */

	XtSetTypeConverter(XtRString,	/* source type */
		XmRVerticalPosition,	/* target type */
		_XmCvtStringToResIndPosition,	/* converter routine */
		vertical_args,	/* args for converter routine */
		XtNumber(vertical_args),	/* number of args to converter routine */
		XtCacheNone,	/* caching instructions */
		NULL);	/* destructor function */

	XtSetTypeConverter(XtRString,	/* source type */
		XmRShellVertDim,		/* target type */
		_XmCvtStringToResIndDimension,		/* converter routine */
		vertical_args,	/* args for converter routine */
		XtNumber(vertical_args),	/* number of args to converter routine */
		XtCacheNone,	/* caching instructions */
		NULL);	/* destructor function */

	XtSetTypeConverter(XtRString,	/* source type */
		XmRVerticalDimension,	/* target type */
		_XmCvtStringToResIndDimension,		/* converter routine */
		vertical_args,	/* args for converter routine */
		XtNumber(vertical_args),	/* number of args to converter routine */
		XtCacheNone,	/* caching instructions */
		NULL);	/* destructor function */

	XtSetTypeConverter(XtRString,	/* source type */
		XmRVerticalInt,	/* target type */
		_XmCvtStringToResIndInt,	/* converter routine */
		vertical_args,	/* args for converter routine */
		XtNumber(vertical_args),	/* number of args to converter routine */
		XtCacheNone,	/* caching instructions */
		NULL);	/* destructor function */

	XtSetTypeConverter(XtRString,	/* source type */
		XmRKeySym,	/* target type */
		_XmCvtStringToKeySym,	/* converter routine */
		NULL,	/* args for converter routine */
		0,	/* number of args to converter routine */
		XtCacheAll,	/* caching instructions */
		NULL);	/* destructor function */

#if 0
	XtSetTypeConverter(XtRString,	/* source type */
		XmRNavigation,	/* target type */
		_XmCvtStringToXmNavigation,	/* converter routine */
		NULL,	/* args for converter routine */
		0,	/* number of args to converter routine */
		XtCacheNone,	/* caching instructions */
		NULL);	/* destructor function */
#endif

	immediate_arg.address_id = (XtPointer)ST_ATOM_LIST;
	XtSetTypeConverter(XtRString,	/* source type */
		XmRAtomList,	/* target type */
		_XmCvtStringToSomeTable,	/* converter routine */
		&immediate_arg,	/* args for converter routine */
		1,	/* number of args to converter routine */
		XtCacheNone | XtCacheRefCount,	/* caching instructions */
		destroy_table);	/* destructor function */

	immediate_arg.address_id = (XtPointer)ST_BUTTON_TYPE;
	XtSetTypeConverter(XtRString,	/* source type */
		XmRButtonType,	/* target type */
		_XmCvtStringToSomeTable,	/* converter routine */
		&immediate_arg,	/* args for converter routine */
		1,	/* number of args to converter routine */
		XtCacheNone | XtCacheRefCount,	/* caching instructions */
		destroy_table);	/* destructor function */

	immediate_arg.address_id = (XtPointer)ST_STRING_TABLE;
	XtSetTypeConverter(XtRString,	/* source type */
		XmRCharSetTable,	/* target type */
		_XmCvtStringToSomeTable,	/* converter routine */
		&immediate_arg,	/* args for converter routine */
		1,	/* number of args to converter routine */
		XtCacheNone | XtCacheRefCount,	/* caching instructions */
		destroy_string_table);	/* destructor function */

	immediate_arg.address_id = (XtPointer)ST_KEY_SYM_TABLE;
	XtSetTypeConverter(XtRString,	/* source type */
		XmRKeySymTable,	/* target type */
		_XmCvtStringToSomeTable,	/* converter routine */
		&immediate_arg,	/* args for converter routine */
		1,	/* number of args to converter routine */
		XtCacheNone | XtCacheRefCount,	/* caching instructions */
		destroy_table);	/* destructor function */

	immediate_arg.address_id = (XtPointer)ST_STRING_TABLE;
	XtSetTypeConverter(XtRString,	/* source type */
		XmRStringTable,	/* target type */
		_XmCvtStringToSomeTable,	/* converter routine */
		&immediate_arg,	/* args for converter routine */
		1,	/* number of args to converter routine */
		XtCacheNone | XtCacheRefCount,	/* caching instructions */
		destroy_string_table);	/* destructor function */

	immediate_arg.address_id = (XtPointer)ST_XM_STRING_TABLE;
	XtSetTypeConverter(XtRString,	/* source type */
		XmRXmStringTable,	/* target type */
		_XmCvtStringToSomeTable,	/* converter routine */
		&immediate_arg,	/* args for converter routine */
		1,	/* number of args to converter routine */
		XtCacheNone | XtCacheRefCount,	/* caching instructions */
		destroy_xm_string_table);	/* destructor function */

	XtSetTypeConverter(XtRString,	/* source type */
		XmRWidget,	/* target type */
		_XmCvtStringToWidget,	/* converter routine */
		parentArgs,	/* args for converter routine */
		XtNumber(parentArgs),	/* number of args to converter routine */
		XtCacheNone,	/* caching instructions */
		NULL);	/* destructor function */

	XtSetTypeConverter(XtRString,	/* source type */
		XmRWindow,	/* target type */
		_XmCvtStringToWindow,	/* converter routine */
		parentArgs,	/* args for converter routine */
		XtNumber(parentArgs),	/* number of args to converter routine */
		XtCacheNone,	/* caching instructions */
		NULL);	/* destructor function */

	XtSetTypeConverter(XtRString,	/* source type */
		XmRCardinal,	/* target type */
		_XmCvtStringToCardinal,	/* converter routine */
		NULL,	/* args for converter routine */
		0,	/* number of args to converter routine */
		XtCacheNone,	/* caching instructions */
		NULL);	/* destructor function */

	XtSetTypeConverter(XtRString,	/* source type */
		XmRBooleanDimension,	/* target type */
		_XmCvtStringToBooleanDimension,	/* converter routine */
		horizontal_args,	/* args for converter routine */
		XtNumber(horizontal_args),	/* number of args to converter routine */
		XtCacheNone,	/* caching instructions */
		NULL);	/* destructor function */

	XtSetTypeConverter(XtRString,	/* source type */
		XmRChar,	/* target type */
		_XmCvtStringToChar,	/* converter routine */
		NULL,	/* args for converter routine */
		0,	/* number of args to converter routine */
		XtCacheNone,	/* caching instructions */
		NULL);	/* destructor function */

	XtSetTypeConverter(XtRString,	/* source type */
		XmRTextPosition,	/* target type */
		_XmCvtStringToTextPosition,	/* converter routine */
		NULL,	/* args for converter routine */
		0,	/* number of args to converter routine */
		XtCacheNone,	/* caching instructions */
		NULL);	/* destructor function */

	XtSetTypeConverter(XtRString,	/* source type */
		XmRTopItemPosition,	/* target type */
		_XmCvtStringToTopItemPosition,	/* converter routine */
		NULL,	/* args for converter routine */
		0,	/* number of args to converter routine */
		XtCacheNone,	/* caching instructions */
		NULL);	/* destructor function */
	XtSetTypeConverter(XtRString,	/* source type */
		XmRTabList,	/* target type */
		_XmCvtStringToTabList,	/* converter routine */
		NULL,	/* args for converter routine */
		0,	/* number of args to converter routine */
		XtCacheNone,	/* caching instructions */
		NULL);	/* destructor function */

	XtSetTypeConverter(XtRString,			/* source type */
		XmRRenderTable,			/* target type */
		_XmCvtStringToRenderTable,	/* converter routine */
		selfArgs,			/* args for converter routine */
		XtNumber(selfArgs),		/* number of args to converter routine */
		XtCacheByDisplay | XtCacheRefCount,	/* caching instructions */
		_XmCvtRenderTableFree);		/* destructor function */
#if	USE_XFT
	XtSetTypeConverter(XtRString,
		XtRXftColor,
		XmuCvtStringToXftColor,
		xftColorConvertArgs,
		XtNumber(xftColorConvertArgs),
		XtCacheByDisplay,
		XmuFreeXftColor);
#endif	/* USE_XFT */

#ifdef NONSTANDARD_CONVERTERS
	_XmRegisterNSEConverters();
#endif
	REPTYPE_REGISTER_WITH_VALUES(XmRFontType, font_types, font_type_values);

#undef REPTYPE_REGISTER
#undef REPTYPE_REGISTER_WITH_VALUES
}


/*
 * error convenience
 */
void
_XmError(Widget w, const char *fmt, ...)
{
	va_list arg_list;
	char buf[1024];
	int charsleft=sizeof(buf);

	va_start(arg_list, fmt);
#ifdef HAVE_VSNPRINTF
	charsleft -= vsnprintf(buf, sizeof(buf), fmt, arg_list);
#else
	charsleft -= vsprintf(buf, fmt, arg_list);
	/* one might add a catch here ... */
#endif
	va_end(arg_list);
	if (w) {
		XtAppError(XtWidgetToApplicationContext(w), buf);
	} else {
		XtError(buf);
	}
}


/*
 * Warning convenience
 *
 * Added _LtDebug stuff here to be able to avoid infinite loops in applications
 * that redirect stderr to a scrolledText. ML suffers from this, due to LessTif
 * bugs of course :-(
 */
static void
_LtWarning(Widget w, const char *fmt, va_list arg_list)
{
 /* Somewhere it's pointed out that we shouldn't exceed 1024 characters here.
    Let's reserve two chars for \n\000
    If we don't have (v)snprintf() we have a vulnerable piece of
    code here ...  */
#define BUFLEN 1026
    char buf[BUFLEN]="";
    int charsleft=BUFLEN;
    static Boolean redirect=False;
#ifndef	LESSTIF_PRODUCTION
    static Boolean init=False;


    if (!init) {
    	if (getenv("DEBUG_REDIRECT_XMWARNING"))
	   redirect = True;
	init=True;
    }
#endif /* !LESSTIF_PRODUCTION */

    if (w)
    {
#ifdef HAVE_SNPRINTF
	charsleft -= snprintf(buf, charsleft,
    	                      "\n    Name: %s\n    Class: %s\n    ",
		              XtName(w), XtClass(w)->core_class.class_name);
#else
#ifndef VOID_SPRINTF
	charsleft -=
#endif
	sprintf(buf, "\n    Name: %s\n    Class: %s\n    ",
		             XtName(w), XtClass(w)->core_class.class_name);
#ifdef VOID_SPRINTF
	charsleft -= strlen(buf);
#endif
        /* one might add a catch here ... */
#endif

#ifdef HAVE_VSNPRINTF
	charsleft -= vsnprintf(buf + strlen(buf), charsleft,
	                       fmt, arg_list);
#else
	charsleft -= vsprintf(buf + strlen(buf),
	                      fmt, arg_list);
        /* one might add a catch here ... */
#endif

	strcat(buf, "\n");

	if (redirect)
	{
	    _LtDebugPrintString(buf);
	}
	else
	{
	    XtAppWarning(XtWidgetToApplicationContext(w), buf);
	}
    }
    else
    {
#ifdef HAVE_VSNPRINTF
	charsleft -= vsnprintf(buf + strlen(buf), charsleft,
	          fmt, arg_list);
#else
	charsleft -= vsprintf(buf + strlen(buf),
	         fmt, arg_list);
#endif

	if (redirect)
	{
	    _LtDebugPrintString(buf);
	}
	else
	{
	    XtWarning(buf);
	}
    } /* if(w) */
}

/*
 * Warning convenience
 *
 * Added _LtDebug stuff here to be able to avoid infinite loops in applications
 * that redirect stderr to a scrolledText. ML suffers from this, due to LessTif
 * bugs of course :-(
 */

#ifdef _XmWarning
#undef _XmWarning
#endif

extern void
_XmWarning(Widget w, const char *fmt, ...)
{
    va_list arg_list;

    va_start(arg_list, fmt);
    _LtWarning(w, fmt, arg_list);
    va_end(arg_list);
}


extern void
XmeWarning(Widget w, const char *fmt, ...)
{
    va_list arg_list;

    va_start(arg_list, fmt);
    _LtWarning(w, fmt, arg_list);
    va_end(arg_list);
}


extern char *
XmRegisterSegmentEncoding(char *fontlist_tag, char *ct_encoding)
{
    return NULL;
}


extern char *
XmMapSegmentEncoding(char *fontlist_tag)
{
    return NULL;
}


extern XmString
XmCvtCTToXmString(char *text)
{
    return XmStringCreate(text, XmFONTLIST_DEFAULT_TAG);
}


extern Boolean
XmCvtTextToXmString(Display *display, XrmValuePtr args,
		    Cardinal *num_args, XrmValue *from_val, XrmValue *to_val,
		    XtPointer *converter_data)
{
    return False;
}


extern char *
XmCvtXmStringToCT(XmString string)
{
    char *text = (char *)NULL;
    XmStringContext context = NULL;
    char *pbuf = (char *)NULL, *pstr;
    XmStringCharSet tag;
    XmStringDirection direction;
    Boolean separator;

    DEBUGOUT(_LtDebug(__FILE__, NULL, "XmCvtXmStringToCT\n"));

    if (XmStringInitContext(&context, string) == True)
    {
	while (XmStringGetNextSegment(context,
				      &text,
				      &tag,
				      &direction,
				      &separator) == True)
	{
	    if (pbuf == NULL)
	    {
		/* allocate space for the buffer and init it */
		pbuf = XtMalloc(strlen(text) + 2);
		*pbuf = '\0';
	    }
	    else
	    {
		/* allocate more space for the buffer */
		pbuf = XtRealloc(pbuf, sizeof(pbuf) + strlen(text) + 2);
	    }

	    /* reset the pointer pstr to point back at pbuf */
	    pstr = pbuf;

	    pstr += (strlen(strcat(pstr, text)));
	    if (separator == True)
	    {			/* add newline */
		*pstr++ = '\n';
		*pstr = '\0';
	    }

	    /* free the text segment we just grabbed */
	    XtFree(text);
	}
	text = pbuf;
    }

    return text;
}

extern Boolean
XmCvtXmStringToText(Display *display, XrmValuePtr args,
		    Cardinal *num_args, XrmValue *from_val, XrmValue *to_val,
		    XtPointer *converter_data)
{
    char *value = XtNewString("");
    XmString m = (XmString)from_val->addr;
    XmStringContext c;
    XmStringComponentType t = XmSTRING_COMPONENT_UNKNOWN;

    if (*num_args != 0)
    {
	XtWarningMsg("wrongParameters", "XmCvtXmStringToText",
		     "XtToolkitError",
		     "XmString to String conversion needs no extra arguments",
		     (String *)NULL, (Cardinal *)NULL);
    }

    XmStringInitContext(&c, m);

    while (t != XmSTRING_COMPONENT_END)
    {
	char *s_text            = NULL;
	XmStringCharSet s_cs    = NULL;
	XmStringDirection d     = XmSTRING_DIRECTION_DEFAULT;
	XmStringComponentType u = XmSTRING_COMPONENT_UNKNOWN;
	unsigned short ul       = 0;
	unsigned char *s_uv     = NULL;
	char *text              = NULL;

	t = XmStringGetNextComponent(c, &s_text, &s_cs, &d, &u, &ul, &s_uv);

	switch (t)
	{
	case XmSTRING_COMPONENT_TEXT:
	case XmSTRING_COMPONENT_LOCALE_TEXT:
	    text = s_text;
	    break;

	case XmSTRING_COMPONENT_SEPARATOR:
	    text = "\n";
	    break;

	default:
	    /* Unknown component */
	    text = NULL;
	    break;
	}

	if (text != NULL)
	{
	    value = XtRealloc(value, strlen(value) + strlen(text) + 1);
	    strcat(value, text);
	}

	XtFree(s_text);
	XtFree(s_cs);
	XtFree((char *)s_uv);
    }

    to_val->addr = value;
    to_val->size = strlen(value);  /* FIXME: should this be sizeof(value)? */

    DEBUGOUT(_LtDebug(__FILE__, NULL,
		      "XmCvtXmStringToText(%p) => %s\n",
		      from_val->addr, to_val->addr));

    return True;
}


/*
 * Compare two strings to see if they're equivalent. This one is used
 * whenever comparing motif constants with resource string values. The
 * motif constants must have the Xm prefix stripped off and must be
 * in lower case.
 */
extern Boolean
_XmStringsAreEqual(char *in_str,
		   char *text_str)
{
    if (in_str[0] && (tolower(in_str[0]) == 'x') &&
	in_str[1] && (tolower(in_str[1]) == 'm'))
    {
	in_str += 2;		/* skip the Xm prefix */
    }

    while (*in_str)
    {
	if (tolower(*in_str) != *text_str)
	    return False;
	in_str++;
	text_str++;
    }

    return *text_str ? False : True;
}


/*
 * Fixed this thing to make a COPY of the fontlist it returns.
 * Danny 16/4/1996
 */
extern XmFontList
_XmGetDefaultFontList(Widget w, unsigned char fontListType)
{
    XmFontList labelFontList = NULL;
    XmFontList buttonFontList = NULL;
    XmFontList textFontList = NULL;
	XmFontListEntry	newEntry;

    Widget par = w;
    XmFontList fontlist, r;
    XmVendorShellExtObject ve = NULL;

    fontlist = NULL;
    switch (fontListType)
    {
    case XmTEXT_FONTLIST:
	while ((par = XtParent(par)) != NULL)
	{
	    if (XmIsBulletinBoard(par) && BB_TextFontList(par) != NULL)
	    {
		fontlist = BB_TextFontList(par);
		break;
	    }
	    else if (XmIsVendorShell(par))
	    {
		/* Danny, I found out the hard way that this function can
		 * get called before the VSEP child is valid, but the context
		 * has been set.  It was with that UIL compiler you sent.
		 * Thus the next three ve checks have been converted.
		 * - MLM
		 */
		ve = (XmVendorShellExtObject)_LtFindVendorExt((Widget)par);
		if (ve && VSEP_TextFontList(ve) != NULL)
		{
		    fontlist = VSEP_TextFontList(ve);
		    break;
		}
	    }
	}
	if (fontlist)
	{
		XmFontList	r = XmFontListCopy(fontlist);
		DEBUGOUT(_LtDebug(__FILE__, w, "_XmGetDefaultFontList(XmTEXT_FONTLIST) -> %p\n", r));
		return r;
	}

	newEntry = XmFontListEntryLoad(XtDisplay(w),
		XmDEFAULT_FONT,
		XmFONT_IS_FONT,
		XmFONTLIST_DEFAULT_TAG);
	textFontList = XmFontListAppendEntry(NULL, newEntry);
	XmFontListEntryFree(&newEntry);

	if (textFontList == NULL) {
		_XmWarning(w, "_XmGetDefaultFontList: textFontList NULL\n");
	}

	DEBUGOUT(_LtDebug(__FILE__, w, "_XmGetDefaultFontList(textFontList) -> %p\n",
		textFontList));
	return textFontList;

    case XmBUTTON_FONTLIST:
	while ((par = XtParent(par)) != NULL)
	{
	    if (XmIsBulletinBoard(par) && BB_ButtonFontList(par) != NULL)
	    {
		fontlist = BB_ButtonFontList(par);
		break;
	    }
	    else if (XmIsMenuShell(par) && MS_ButtonFontList(par) != NULL)
	    {
		fontlist = MS_ButtonFontList(par);
		break;
	    }
	    else if (XmIsVendorShell(par))
	    {
		ve = (XmVendorShellExtObject)_LtFindVendorExt((Widget)par);
		if (ve && VSEP_ButtonFontList(ve) != NULL)
		{
		    fontlist = VSEP_ButtonFontList(ve);
		    break;
		}
	    }
	}
	if (fontlist) {
		r = XmFontListCopy(fontlist);
		DEBUGOUT(_LtDebug(__FILE__, w, "_XmGetDefaultFontList(XmBUTTON_FONTLIST) -> %p\n", r));
		return r;
	}

	newEntry = XmFontListEntryLoad(XtDisplay(w),
		XmDEFAULT_FONT,
		XmFONT_IS_FONT,
		XmFONTLIST_DEFAULT_TAG);
	buttonFontList = XmFontListAppendEntry(NULL, newEntry);
	XmFontListEntryFree(&newEntry);

	if (buttonFontList == NULL) {
		_XmWarning(w, "_XmGetDefaultFontList: buttonFontlist NULL\n");
	}

	DEBUGOUT(_LtDebug(__FILE__, w, "_XmGetDefaultFontList(buttonFontList) -> %p\n",
		buttonFontList));
	return buttonFontList;

    default:
    case XmLABEL_FONTLIST:
	while ((par = XtParent(par)) != NULL)
	{
	    if (XmIsBulletinBoard(par) && BB_LabelFontList(par) != NULL)
	    {
		fontlist = BB_LabelFontList(par);
		break;
	    }
	    else if (XmIsMenuShell(par) && MS_LabelFontList(par) != NULL)
	    {
		fontlist = MS_LabelFontList(par);
		break;
	    }
	    else if (XmIsVendorShell(par))
	    {
		ve = (XmVendorShellExtObject)_LtFindVendorExt((Widget)par);
		if (ve && VSEP_LabelFontList(ve) != NULL)
		{
		    fontlist = VSEP_LabelFontList(ve);
		    break;
		}
	    }
	}

	if (fontlist)
	{
		r = XmFontListCopy(fontlist);
		DEBUGOUT(_LtDebug(__FILE__, w, "_XmGetDefaultFontList(XmLABEL_FONTLIST) -> %p\n", r));
		return r;
	}

	newEntry = XmFontListEntryLoad(XtDisplay(w),
		XmDEFAULT_FONT,
		XmFONT_IS_FONT,
		XmFONTLIST_DEFAULT_TAG);
	labelFontList = XmFontListAppendEntry(NULL, newEntry);
	XmFontListEntryFree(&newEntry);

	if (labelFontList == NULL) {
		_XmWarning(w, "_XmGetDefaultFontList: labelFontList NULL\n");
	}

	DEBUGOUT(_LtDebug(__FILE__, w, "_XmGetDefaultFontList(labelFontList) -> %p\n",
		labelFontList));
	return labelFontList;
    }
}

/* resource converters, of various types */

extern char *
_XmConvertCSToString(XmString cs)
{
    return NULL;
}


extern Boolean
_XmCvtXmStringToCT(XrmValue *from,
		   XrmValue *to)
{
    return False;
}

/*
 * One to convert from String (char *) to XmString
 */
static Boolean
_XmCvtStringToXmString(Display *display,
		       XrmValue *args,
		       Cardinal *num_args,
		       XrmValue *from,
		       XrmValue *to,
		       XtPointer *converter_data)
{
    static XmString newString = NULL;

    if (*num_args != 0)
    {
	XtWarningMsg("wrongParameters", "cvtStringToXmString",
		     "XtToolkitError",
		     "String to XmString conversion needs no extra arguments",
		     (String *)NULL, (Cardinal *)NULL);
    }
    newString = XmStringCreateLtoR(from->addr, XmFONTLIST_DEFAULT_TAG);

    if (newString)
    {
	if (to->addr == NULL)
	{
	    to->addr = (XPointer)&newString;
	    to->size = sizeof(XmString);
	}
	else
	{
	    if (to->size >= sizeof(XmString))
	    {
		*((XmString *)to->addr) = newString;
		to->size = sizeof(XmString);
	    }
	    else
	    {
		XtDisplayStringConversionWarning(display, (char *)from->addr,
						 XmRXmString);
	    }
	}
    }
    else
    {
	XtDisplayStringConversionWarning(display, (char *)from->addr,
					 XmRXmString);
    }

    DEBUGOUT(_LtDebug(__FILE__, NULL,
		      "_XmCvtStringToXmString(%s) => %p\n",
		      from->addr, to->addr));

    return True;
}

/*
 * This function is only used by the _XmCvtStringToXmFontlist resource
 * converter
 *
 * According to the docs, a font entry is :
 *      font_entry = font_name [ ";" font_name ]+ ":" [ tag ]
 *                 | font_name [ "=" tag ]
 *      font_name = XLFD font name
 *      tag = a string (no space, comma, colon, semicolon, equalsign)
 *
 * For a simple example, see test/Xm/international/test1.
 */
static XmFontList
__XmFontListResourceAddEntry(Display *display,
			     char *full_entry,
			     XmFontList oldFontList)
{
    XmFontList newFontList;
    XmFontListEntry newEntry;
    char *eq, *end, *fn, *ft, *tag, *font, *semi, *colon;

    DEBUGOUT(_LtDebug(__FILE__, NULL,
		      "__XmFontListResourceAddEntry(%s)\n", full_entry));

    eq = strchr(full_entry, (int)'=');

    /* there was an equal sign, the left side is the font,
       and the right side the tag. */
    if (eq)
    {
	/* set the equal sign to a null so we can XtNewString the font */
	*eq = 0;
	fn = font = XtNewString(full_entry);

	/* set it back again, and XtNewString the tag */
	*eq = '=';
	ft = tag = XtNewString(eq + 1);

	end = font + strlen(font);
	while (isspace(*font) && font < end)
	{
	    font++;
	}
	while (isspace(*end) && end > font)
	{
	    end--;
	}

	if (end != font + strlen(font))
	{
	    end++;
	    *end = 0;
	}

	end = tag + strlen(tag);
	while (isspace(*tag) && tag < end)
	{
	    tag++;
	}
	while (isspace(*end) && end > tag)
	{
	    end--;
	}

	if (end != tag + strlen(tag))
	{
	    end++;
	    *end = 0;
	}

        newEntry = XmFontListEntryLoad(display,
                                       font, XmFONT_IS_FONT, tag);
	newFontList = XmFontListAppendEntry(oldFontList, newEntry);

	XtFree(ft);
	XtFree(fn);

	return newFontList;
    }

    /* Now treat the case with no equal sign */
    semi = strchr(full_entry, ';');
    colon = strchr(full_entry, ':');

    if (semi || colon)
    {
	/* font_entry = font_name [ ";" font_name ]+ ":" [ tag ] */

	/* First find the tag if there is one */
	if (colon)
	{
	    tag = colon + 1;
	    while (tag && *tag != '\0' && isspace(*tag))
	    {
		tag++;
	    }
	    if (*tag == '\0')
	    {
		tag = XmFONTLIST_DEFAULT_TAG;
	    }
	}
	else
	{
	    tag = XmFONTLIST_DEFAULT_TAG;
	}

	while((semi = strchr(full_entry, ';')) != NULL)
	{
	    *semi = ',';
	}

	/* Find all the font names and process them */
	fn = font = XtNewString(full_entry);
	colon = strchr(fn, ':');

	if (colon)
	{
	    *colon = '\0';
	}
	newEntry = XmFontListEntryLoad(display,
                                       fn, XmFONT_IS_FONTSET, tag);
	newFontList = XmFontListAppendEntry(oldFontList, newEntry);

	XtFree(fn);

	return newFontList;
    }

    /* A simple font name */
    fn = font = XtNewString(full_entry);

    end = font + strlen(font);
    while (isspace(*font) && font < end)
    {
	font++;
    }

    while (isspace(*end) && end > font)
    {
	end--;
    }

    if (end != font + strlen(font))
    {
	end++;
	*end = 0;
    }
    newEntry = XmFontListEntryLoad(display,
                                   full_entry, XmFONT_IS_FONT, XmFONTLIST_DEFAULT_TAG);
    newFontList = XmFontListAppendEntry(oldFontList, newEntry);
    XtFree(fn);
    return newFontList;
}

/*
 * This thing should do reference counting, or make copies
 * Danny 16/4/1996
 */
/*
 * According to the docs, the following format should be converted :
 *      font_list = font_entry [ "," font_entry ]+
 *      font_entry = font_name [ ";" font_name ]+ ":" [ tag ]
 *                 | font_name [ "=" tag ]
 *      font_name = XLFD font name
 *      tag = a string (no space, comma, colon, semicolon, equalsign)
 *
 * For a simple example, see test/Xm/international/test1.
 */
static Boolean
_XmCvtStringToXmFontList(Display *display,
			 XrmValue *args,
			 Cardinal *num_args,
			 XrmValue *from,
			 XrmValue *to,
			 XtPointer *converter_data)
{
	XmFontList newFontList = NULL;
	char *p, *end, *font, *fn, *buf;

	if (*num_args != 0) {
		XtWarningMsg("wrongParameters", "cvtStringToFontlist",
			"XtToolkitError",
			"String to Fontlist conversion needs no extra arguments",
			(String *)NULL, (Cardinal *)NULL);
	}

    /* there is an implicit rule here.  strtok() modifies the passed in buffer
     * at least on Linux, so if a comma is found, a \0 is put there.  This is
     * bad, as this memory didn't come from us, but from the Intrinsics, and
     * may get used again (especially if it's in the resource cache)! */

    buf = XtNewString(from->addr);

    p = strtok(buf, ",");

    /* if there was actually a comma character in the string. */
    if (p)
    {
	do
	{
	    fn = font = XtNewString(p);

	    end = font + strlen(font);
	    while (isspace(*font) && font < end)
	    {
		font++;
	    }
	    while (isspace(*end) && end > font)
	    {
		end--;
	    }
	    if (end != font + strlen(font))
	    {
		end++;
		*end = 0;
	    }

	    newFontList = __XmFontListResourceAddEntry(display, font,
						       newFontList);

	    XtFree(fn);
	    p = strtok(NULL, ",");

	}
	while (p != NULL);

	XtFree(buf);
    }
    else
    {
	fn = font = buf;

	end = font + strlen(font);
	while (isspace(*font) && font < end)
	{
	    font++;
	}
	while (isspace(*end) && end > font)
	{
	    end--;
	}

	if (end != font + strlen(font))
	{
	    end++;
	    *end = 0;
	}
	newFontList = __XmFontListResourceAddEntry(display, font, newFontList);

	XtFree(buf);
    }

    if (newFontList)
    {
	if (to->addr == NULL)
	{
	    to->addr = (XPointer)&newFontList;
	    to->size = sizeof(XmFontList);
	}
	else
	{
	    if (to->size >= sizeof(XmFontList))
	    {
		*((XmFontList *)to->addr) = newFontList;
		to->size = sizeof(XmFontList);
	    }
	    else
	    {
		XtDisplayStringConversionWarning(display, (char *)from->addr,
						 XmRFontList);
	    }
	}
    }
    else
    {
	XtDisplayStringConversionWarning(display, (char *)from->addr,
					 XmRFontList);
    }

	DEBUGOUT(_LtDebug(__FILE__, NULL, "_XmCvtStringToXmFontList(%s) -> %p\n",
		from->addr, *((XmFontList *)to->addr)));

    return True;
}

#if 0
/* One from String to XmNavigationType */
static Boolean
_XmCvtStringToXmNavigation(Display *display,
			   XrmValue *args,
			   Cardinal *num_args,
			   XrmValue *from,
			   XrmValue *to,
			   XtPointer *converter_data)
{
    static XmNavigationType navType;

    navType = 10;

    if (*num_args != 0)
    {
	XtWarningMsg("wrongParameters", "cvtStringToXmNavigation",
		     "XtToolkitError",
		  "String to XmNavigation conversion needs no extra arguments",
		     (String *)NULL, (Cardinal *)NULL);
    }

    if (!strcmp((char *)from->addr, "NONE"))
    {
	navType = XmNONE;
    }
    else if (!strcmp((char *)from->addr, "TAB_GROUP"))
    {
	navType = XmTAB_GROUP;
    }
    else if (!strcmp((char *)from->addr, "STICKY_TAB_GROUP"))
    {
	navType = XmSTICKY_TAB_GROUP;
    }
    else if (!strcmp((char *)from->addr, "EXCLUSIVE_TAB_GROUP"))
    {
	navType = XmEXCLUSIVE_TAB_GROUP;
    }

    if (navType != 10)
    {
	if (to->addr == NULL)
	{
	    to->addr = (XtPointer)&navType;
	    to->size = sizeof(XmNavigationType);
	}
	else
	{
	    if (to->size >= sizeof(XmNavigationType))
	    {
		*((XmNavigationType *)to->addr) = navType;
		to->size = sizeof(XmNavigationType);
	    }
	    else
	    {
		XtDisplayStringConversionWarning(display, (char *)from->addr,
						 XmRNavigationType);
	    }
	}
    }
    else
    {
	XtDisplayStringConversionWarning(display, (char *)from->addr,
					 XmRNavigationType);
    }

    return True;
}
#endif

static Boolean
_XmCvtStringToCardinal(Display *display,
			XrmValue	*args,
			Cardinal	*num_args,
			XrmValue	*from,
			XrmValue	*to,
			XtPointer	*converter_data)
{
	static Cardinal	value;
	char		*end;

	value = 0;
	if (*num_args != 0)
	{
		XtWarningMsg("wrongParameters", "cvtStringToCardinal",
		    "XtToolkitError",
		    "String to Cardinal conversion needs no extra arguments",
		    (String *)NULL, (Cardinal *)NULL);
	}

	value = strtoul((char *)from->addr, &end, 10);

	if (*end == '\0')
	{
		if (to->addr == NULL)
		{
			to->addr = (XPointer)&value;
			to->size = sizeof(Cardinal);
		}
		else
		{
			if (to->size >= sizeof(Cardinal))
			{
				*((Cardinal *)to->addr) = value;
				to->size = sizeof(Cardinal);
			}
			else
			{
				XtDisplayStringConversionWarning(display,
					(char *)from->addr,
					XmRCardinal);
			}
		}
	}
	else
	{
		XtDisplayStringConversionWarning(display,
			(char *)from->addr,
			XmRCardinal);
	}

	return True;
}

static Boolean
_XmCvtStringToKeySym(Display *d, XrmValue *args, Cardinal *nargs,
		     XrmValue *from, XrmValue *to, XtPointer *data)
{
    static KeySym k;

    k = XStringToKeysym((char *)from->addr);

    if (k != NoSymbol)
    {
	if (to->addr == NULL)
	{
	    to->addr = (XPointer)&k;
	    to->size = sizeof(k);
	    return True;
	}
	else
	{
	    if (to->size >= sizeof(KeySym))
	    {
		*((KeySym *)to->addr) = k;
		to->size = sizeof(k);
		return True;
	    }
	    else
	    {
		XtDisplayStringConversionWarning(d, (char *)from->addr,
						 XmRKeySym);
		DEBUGOUT(_LtDebug(__FILE__, NULL,
				  "_XmCvtStringToKeySym fail at line %d\n",
				  __LINE__));
		return False;
	    }
	}
    }

    XtDisplayStringConversionWarning(d, (char *)from->addr, XmRKeySym);

    DEBUGOUT(_LtDebug(__FILE__, NULL,
		      "_XmCvtStringToKeySym fail at line %d\n", __LINE__));
    return False;
}


static Boolean
_XmCvtStringToResIndPosition(Display *display,
			     XrmValue *args,
			     Cardinal *num_args,
			     XrmValue *from,
			     XrmValue *to,
			     XtPointer *converter_data)
{
    static Position pos;

    if (*num_args != 3)
	XtWarningMsg("wrongParameters", "_XmCvtStringToResIndPosition",
		     "XtToolkitError",
"String to XmRHorizontal/VerticalPosition conversion needs no extra arguments",
		     (String *)NULL, (Cardinal *)NULL);

    pos = (Position)_XmConvertUnits(*(Screen **)args[0].addr,
				    (int)(long)*(XtPointer *)args[1].addr,
				    *(unsigned char *)args[2].addr,
				    atoi(from->addr),
				    XmPIXELS);

    if (to->addr == NULL)
    {
	to->addr = (XPointer)&pos;
	to->size = sizeof pos;
    }
    else
    {
	if (to->size < sizeof pos)
	{
	    XtDisplayStringConversionWarning(display, (char *)from->addr,
		(int)(long)*(XtPointer *)args[1].addr == XmHORIZONTAL
		? XmRHorizontalPosition : XmRVerticalPosition);
	    return False;
	}
	*(Position *)to->addr = pos;
	to->size = sizeof pos;
    }

    return True;
}


static Boolean
_XmCvtStringToResIndDimension(Display *display,
			      XrmValue *args,
			      Cardinal *num_args,
			      XrmValue *from,
			      XrmValue *to,
			      XtPointer *converter_data)
{
    static Dimension dim;

    if (*num_args != 3)
	XtWarningMsg("wrongParameters", "_XmCvtStringToResIndDimension",
		     "XtToolkitError",
"String to XmRHorizontal/VerticalDimension conversion needs no extra arguments",
		     (String *)NULL, (Cardinal *)NULL);

    dim = (Dimension)_XmConvertUnits(*(Screen **)args[0].addr,
				     (int)(long)*(XtPointer *)args[1].addr,
				     *(unsigned char *)args[2].addr,
				     atoi(from->addr),
				     XmPIXELS);

    if (to->addr == NULL)
    {
	to->addr = (XPointer)&dim;
	to->size = sizeof dim;
    }
    else
    {
	if (to->size < sizeof dim)
	{
	    XtDisplayStringConversionWarning(display, (char *)from->addr,
		(int)(long)*(XtPointer *)args[1].addr == XmHORIZONTAL
		? XmRHorizontalDimension : XmRVerticalDimension);
	    return False;
	}
	*(Dimension *)to->addr = dim;
	to->size = sizeof dim;
    }

    return True;
}


static Boolean
_XmCvtStringToResIndInt(Display *display,
			XrmValue *args,
			Cardinal *num_args,
			XrmValue *from,
			XrmValue *to,
			XtPointer *converter_data)
{
    static int pos;

    if (*num_args != 3)
	XtWarningMsg("wrongParameters", "_XmCvtStringToResIndInt",
		     "XtToolkitError",
     "String to XmRHorizontal/VerticalInt conversion needs no extra arguments",
		     (String *)NULL, (Cardinal *)NULL);

    pos = _XmConvertUnits(*(Screen **)args[0].addr,
			  (int)(long)*(XtPointer *)args[1].addr,
			  *(unsigned char *)args[2].addr,
			  atoi(from->addr),
			  XmPIXELS);

    if (to->addr == NULL)
    {
	to->addr = (XPointer)&pos;
	to->size = sizeof pos;
    }
    else
    {
	if (to->size < sizeof pos)
	{
	    XtDisplayStringConversionWarning(display, (char *)from->addr,
		(int)(long)*(XtPointer *)args[1].addr == XmHORIZONTAL
		? XmRHorizontalInt : XmRVerticalInt);
	    return False;
	}
	*(int *)to->addr = pos;
	to->size = sizeof pos;
    }

    return True;
}


static Boolean
_XmCvtStringToWidget(	Display *display,
			XrmValue *args,
			Cardinal *num_args,
			XrmValue *from,
			XrmValue *to,
			XtPointer *converter_data)
{
    Widget		parent;
    static Widget	child;
    char		*name;

    if (*num_args != 1)
	XtWarningMsg("wrongParameters", "cvtStringToWidget",
		     "XtToolkitError",
	 "String to Widget conversion needs an extra argument",
		     (String *)NULL, (Cardinal *)NULL);

    name = (char *)from->addr;

    parent = * (Widget *) args[0].addr;

    if (name != NULL)
    {
	child = XtNameToWidget(parent, name);

	if (to->addr == NULL)
	{
	    to->addr = (XPointer)&child;
	    to->size = sizeof(Widget);
	}
	else
	{
	    if (to->size >= sizeof(Widget))
	    {
		*((Widget *)to->addr) = child;
		to->size = sizeof(Widget);
	    }
	    else
	    {
		XtDisplayStringConversionWarning(display, (char *)from->addr,
						 XmRWidget);
	    }
	}
    }
    else
    {
	XtDisplayStringConversionWarning(display, (char *)from->addr,
					 XmRWidget);
    }

    return True;
}


static Boolean
_XmCvtStringToWindow(Display *display,
		     XrmValue *args,
		     Cardinal *num_args,
		     XrmValue *from,
		     XrmValue *to,
		     XtPointer *converter_data)
{
    static Window	win;

    win = atoi( from->addr);
    if (to->addr == NULL) {
	to->addr = (XPointer)&win;
	to->size = sizeof win;
    } else {
	if (!to->size) {
	    XtDisplayStringConversionWarning(display, from->addr, XmRWindow);
	    return False;
	}
	*to->addr = win;
	to->size = sizeof win;
    }

    return True;
}


static Boolean
_XmCvtStringToBooleanDimension( Display *display,
				XrmValue *args,
				Cardinal *num_args,
				XrmValue *from,
				XrmValue *to,
				XtPointer *converter_data)
{
    static Dimension	value;
    int			preval;
    String		str;

    str = (String)from->addr;

    if (*num_args != 3) {
	XtWarningMsg("wrongParameters", "cvtStringToBooleanDimension",
	    "XtToolkitError",
	    "String to BooleanDimension conversion needs no extra arguments",
	    (String *)NULL, (Cardinal *)NULL);
    }

    if (str != NULL) {
	if (isdigit(str[0])) {
	    preval = atoi(from->addr);
	    value = (Dimension)_XmConvertUnits(*(Screen **)args[0].addr,
					       (int)(long)*(XtPointer *)args[1].addr,
					       *(unsigned char *)args[2].addr,
					       preval,
					       XmPIXELS);
	    if (!value && preval)
		value = 1;
	} else {
	    if ((strcasecmp(str, "true") == 0) || (strcasecmp(str, "yes") == 0)
			|| (strcasecmp(str, "on") == 0)) {
		value = True;
	    } else if ((strcasecmp(str, "false") == 0)
			|| (strcasecmp(str, "no") == 0)
			|| (strcasecmp(str, "off") == 0)) {
		value = False;
	    } else {
		XtDisplayStringConversionWarning(display, str,
				XmRBooleanDimension);
		return False;
	    }
	}

	if (to->addr == NULL) {
	    to->addr = (XPointer)&value;
	    to->size = sizeof(value);
	} else {
	    if (to->size >= sizeof(value)) {
		*((Dimension *)to->addr) = value;
		to->size = sizeof(Dimension);
	    } else {
		XtDisplayStringConversionWarning(display, str,
				XmRBooleanDimension);
		return False;
	    }
	}
    } else {
	XtDisplayStringConversionWarning(display, str, XmRBooleanDimension);
	return False;
    }

    return True;
}


static Boolean
_XmCvtStringToChar( Display *display,
		    XrmValue *args,
		    Cardinal *num_args,
		    XrmValue *from,
		    XrmValue *to,
		    XtPointer *converter_data)
{
    static char	c;

    c = from->addr[0];
    if (to->addr == NULL) {
	to->addr = &c;
	to->size = 1;
    } else {
	if (!to->size) {
	    XtDisplayStringConversionWarning(display, from->addr, XmRChar);
	    return False;
	}
	*to->addr = c;
	to->size = 1;
    }

    return True;
}


static Boolean
_XmCvtStringToTextPosition( Display *display,
			    XrmValue *args,
			    Cardinal *num_args,
			    XrmValue *from,
			    XrmValue *to,
			    XtPointer *converter_data)
{
    static XmTextPosition	tp;

    tp = atol( from->addr);
    if (to->addr == NULL) {
	to->addr = (XPointer)&tp;
	to->size = sizeof tp;
    } else {
	if (to->size < sizeof tp) {
	    XtDisplayStringConversionWarning(display, from->addr,
					     XmRTextPosition);
	    return False;
	}
	*(XmTextPosition *)to->addr = tp;
	to->size = sizeof tp;
    }

    return True;
}


static Boolean
_XmCvtStringToTopItemPosition( Display *display,
			       XrmValue *args,
			       Cardinal *num_args,
			       XrmValue *from,
			       XrmValue *to,
			       XtPointer *converter_data)
{
    static int	tip;

    tip = atoi( from->addr);
    if (to->addr == NULL) {
	to->addr = (XPointer)&tip;
	to->size = sizeof tip;
    } else {
	if (to->size < sizeof tip) {
	    XtDisplayStringConversionWarning(display, from->addr,
					     XmRTopItemPosition);
	    return False;
	}
	*(XmTextPosition *)to->addr = tip;
	to->size = sizeof tip;
    }

    return True;
}


/* Converter for various tables of comma-separated strings,
 * with those strings converted each in their own way.
 */

static Boolean
_XmCvtStringToSomeTable( Display *display,
			 XrmValue *args,
			 Cardinal *num_args,
			 XrmValue *from,
			 XrmValue *to,
			 XtPointer *converter_data)
{
    int			nitems, tabsize, tabtype, escaped, anyescaped;
    char		*item, *p, *str, *sp;
    static XtPointer	*table;

    /* Separate the resource string at commas.
     * The commas may be escaped by a backslash
     * (leading whitespace and backslashes themselves may be escaped too).
     */

    table = (XtPointer *)XtMalloc((tabsize = 64) * sizeof(XtPointer));
    nitems = 0;
    item = NULL;
    escaped = anyescaped = False;
    tabtype = (int)(long)*(XtPointer *)args[0].addr;
    for (p = from->addr;; p++)
    {
	if (escaped)
	{
	    /* A previously escaped character.
	     * Treat it like a normal character (not comma, backslash or space)
	     */

	    escaped = False;
	    if (!item)
		item = p;
	}
	else if (p >= from->addr + from->size || !*p || *p == ',')
	{
	    /* End of a string in the table.
	     * Make a copy to work with, both to add a nul and unescape.
	     */

	    if (!item)
		item = p;
	    str = XtMalloc((p - item) + 1);
	    memcpy(str, item, p - item);
	    str[p - item] = 0;
	    if (anyescaped)
	    {
		/* Removed escape characters */

		anyescaped = False;
		for (sp = str; *sp; sp++)
		    if (*sp == '\\' && sp[1])
		    {
			memmove(sp, sp + 1, strlen(sp));
			sp++;
		    }
	    }

	    /* Encode the string into something and put it in the list */

	    switch (tabtype)
	    {
	    case ST_BUTTON_TYPE:
		/* This should use probably use RepType stuff.  But that
		 * converter isn't there, and if it did its name would be
		 * XmRButtonType, which this erroneously is.
		 */

		if (_XmStringsAreEqual( str, "pushbutton"))
		    ((XmButtonType *)table)[nitems] = XmPUSHBUTTON;
		else if (_XmStringsAreEqual( str, "togglebutton"))
		    ((XmButtonType *)table)[nitems] = XmTOGGLEBUTTON;
		else if (_XmStringsAreEqual( str, "radiobutton"))
		    ((XmButtonType *)table)[nitems] = XmRADIOBUTTON;
		else if (_XmStringsAreEqual( str, "cascadebutton"))
		    ((XmButtonType *)table)[nitems] = XmCASCADEBUTTON;
		else if (_XmStringsAreEqual( str, "separator"))
		    ((XmButtonType *)table)[nitems] = XmSEPARATOR;
		else if (_XmStringsAreEqual( str, "double_separator"))
		    ((XmButtonType *)table)[nitems] = XmDOUBLE_SEPARATOR;
		else if (_XmStringsAreEqual( str, "title"))
		    ((XmButtonType *)table)[nitems] = XmTITLE;
		else
		{
		    XtDisplayStringConversionWarning(display, from->addr,
						     XmRButtonType);
		    XtFree(str);
		    XtFree((char *)table);
		    return False;
		}
		break;

	    case ST_KEY_SYM_TABLE:
		if ((table[nitems] = (XtPointer)XStringToKeysym(str)) ==
		    NoSymbol)
		{
		    XtDisplayStringConversionWarning(display, from->addr,
						     XmRKeySymTable);
		    XtFree(str);
		    XtFree((char *)table);
		    return False;
		}
		XtFree(str);
		break;

	    case ST_ATOM_LIST:
		/* Convert to atoms later */
	    case ST_STRING_TABLE:
		table[nitems] = str;
		break;

	    case ST_XM_STRING_TABLE:
		table[nitems] =
		    (XtPointer)XmStringCreateLtoR(str, XmFONTLIST_DEFAULT_TAG);
		XtFree(str);
	    }

	    /* If this is the end of the string, stop looking */

	    nitems++;
	    if (p >= from->addr + from->size || !*p)
		break;
	    item = NULL;

	    /* Bump up the table size, re-allocating if needed */

	    if (nitems == tabsize)
		table = (XtPointer *)XtRealloc((char *)table,
		    (tabsize <<= 1) * sizeof(XtPointer));
	}
	else if (*p == '\\' && p < from->addr + from->size - 1 && p[1])
	{
	    /* Escape the next character after a blackslash
	     * (unless this is the end of the string)
	     */

	    escaped = anyescaped = True;
	}
	else if (!item && !isspace(*p))
	{
	    /* Skip leading whitespace for each item.
	     * Strangely, trailing whitespace stays.
	     */

	    item = p;
	}
    }

    /* Wrap up the conversion, including null-terminating the table */

    switch (tabtype)
    {
    case ST_ATOM_LIST:
	/* Now that we have all the atoms, convert them all at once */
	{
	    Atom *atoms = (Atom *)XtMalloc((nitems + 1) * sizeof(Atom));

	    atoms[nitems] = None;
#if XlibSpecificationRelease > 5
	    XInternAtoms(display, (char **)table, nitems, False, atoms);
#else
	    while (nitems--)
		XmInternAtom(display, ((char **)table)[nitems], False);
#endif
	    XtFree((char *)table);
	    table = (XtPointer *)atoms;
	}
	break;

    case ST_BUTTON_TYPE:
	table = (XtPointer *)
	    XtRealloc((char *)table, (nitems + 1) * sizeof(XmButtonType));
	((XmButtonType *)table)[nitems] = 0;
	break;

    default:
	table = (XtPointer *)
	    XtRealloc((char *)table, (nitems + 1) * sizeof(XtPointer));
	table[nitems] = NULL;
    }

    if (to->addr == NULL) {
	to->addr = (XPointer)&table;
	to->size = sizeof table;
    } else {
	if (to->size < sizeof table) {
	    XtDisplayStringConversionWarning(display, from->addr,
		tabtype == ST_ATOM_LIST ? XmRAtomList :
		tabtype == ST_KEY_SYM_TABLE ? XmRKeySymTable :
		tabtype == ST_STRING_TABLE ? XmRStringTable :
		XmRXmStringTable);
	    return False;
	}
	*(XtPointer **)to->addr = table;
	to->size = sizeof table;
    }

    return True;
}


static void
destroy_font_list(XtAppContext app, XrmValue *to,
		  XtPointer converer_data, XrmValue *args,
		  Cardinal *num_args)
{
	fprintf(stderr, "_XmCvtStringToXmFontList()\n");
    XmFontListFree(*(XmFontList *)to->addr);
}


static void
destroy_table(XtAppContext app, XrmValue *to,
	      XtPointer converer_data, XrmValue *args,
	      Cardinal *num_args)
{
    XtFree(*(char **)to->addr);
}


static void
destroy_string_table(XtAppContext app, XrmValue *to,
		     XtPointer converer_data, XrmValue *args,
		     Cardinal *num_args)
{
    int		i;
    String	*table = *(String **)to->addr;

    for (i = 0; table[i]; i++)
	XtFree(table[i]);
    XtFree((char *)table);
}


static void
destroy_xm_string_table(XtAppContext app, XrmValue *to,
		     XtPointer converer_data, XrmValue *args,
		     Cardinal *num_args)
{
    int			i;
    XmStringTable	table = *(XmStringTable *)to->addr;

    for (i = 0; table[i]; i++)
	XmStringFree(table[i]);
    XtFree((char *)table);
}


static void
get_unit_type(Widget w, Cardinal *size, XrmValue *val)
{
    static unsigned char ut;

    val->addr = (XPointer)&ut;
    val->size = sizeof ut;
    ut = _XmGetUnitType(w);
}

/*
 * XmConvertStringToUnits converts a string specification value and returns
 * the converted value as the return value from the function. This function
 * uses the specified screen's resolution to compute the number of units for
 * the string specification.
 */
extern int
XmConvertStringToUnits(Screen *screen, String spec, int orientation,
	int to_type, XtEnum *parse_error)
{
	/* FIX ME not implemented yet */
        _XmWarning(NULL, "XmConvertStringToUnits() is not implemented yet!\n");

	if (screen == NULL)
		return 0;

	if (orientation != XmHORIZONTAL && orientation != XmVERTICAL)
		return 0;

	switch (to_type) {
	case XmPIXELS:
	case XmMILLIMETERS:
	case Xm100TH_MILLIMETERS:
	case XmCENTIMETERS:
	case XmINCHES:
	case Xm1000TH_INCHES:
	case XmPOINTS:
	case Xm100TH_POINTS:
	case XmFONT_UNITS:
	case Xm100TH_FONT_UNITS:
		break;
	default:
		return 0;
	}
    return 0;
}

static Boolean
_XmCvtStringToTabList(Display *display,
		       XrmValue *args,
		       Cardinal *num_args,
		       XrmValue *from,
		       XrmValue *to,
		       XtPointer *converter_data)
{
	int			i, count;
	struct _XmTabRec	**tabs, *thistab;
	char			*c, *p, *q;
	struct _XmTabListRec	**d;

	DEBUGOUT(_LtDebug(__FILE__, NULL, "_XmCvtStringToTabList(%s)\n", from->addr));
	for (i=0, count=1; from->addr[i]; i++)
		if (from->addr[i] == ',')
			count++;

	tabs = (struct _XmTabRec **)XtCalloc(count, sizeof(struct _XmTabRec *));
	for (i=0; i<count; i++)
		tabs[i] = (struct _XmTabRec *)XtMalloc(sizeof(struct _XmTabRec));

	c = q = XtNewString(from->addr);
	i=0;
	while (q) {
		char	unit[20];

		thistab = tabs[i];
		p = strchr(q, ',');
		if (p) {
			*p = '\0';
		}
		/* Convert this into an entry */
		while (isspace(*q)) q++;
		thistab->offset_model = (*q == '+') ? XmRELATIVE : XmABSOLUTE;

		unit[0] = '\0';
		(void) sscanf(q, "%f%s", &thistab->value, &unit[0]);

		/* FIX ME I haven't a clue */
		if (strcasecmp("in", unit) == 0) {
			thistab->units = XmINCHES;
		} else if (strcasecmp("mm", unit) == 0) {
			thistab->units = XmMILLIMETERS;
		} else if (strcasecmp("cm", unit) == 0) {
			thistab->units = XmCENTIMETERS;
		} else {
			thistab->units = XmPIXELS;
		}

		thistab->alignment = XmALIGNMENT_BEGINNING;
		thistab->decimal = 0;

		if (p) {
			q = p+1;
		} else {
			break;
		}
		i++;
	}
	XtFree(c);

	if (to->addr == NULL) {
		to->size = sizeof(XtPointer);
		return False;
	} else {
		if (to->size < sizeof(XtPointer)) {
			to->size = sizeof(XtPointer);
			return False;
		} else {
			d = (struct _XmTabListRec **)to->addr;
			*d = (struct _XmTabListRec *)XtMalloc(sizeof(struct _XmTabListRec));
			(*d)->tabs = tabs;
			(*d)->count = count;
		}
	}

/*
	if (to->addr == NULL) {
		to->addr = (XPointer)&tip;
		to->size = sizeof tip;
	} else {
		if (to->size < sizeof tip) {
		    XtDisplayStringConversionWarning(display, from->addr,
					     XmRTopItemPosition);
		    return False;
		}
		*(XmTextPosition *)to->addr = tip;
		to->size = sizeof tip;
	}
*/
	return True;
}

#define	offset(x)	XtOffsetOf(struct __XmRenditionRec, x)

static XtResource rt_res[] = {
#if	USE_XFT
  { XmNrenditionForeground, XmCRenditionForeground, XtRXftColor,
    sizeof(XftColor), offset(xft_foreground.pixel),
    XmRImmediate, (XtPointer)XmUNSPECIFIED_PIXEL },
  { XmNrenditionBackground, XmCRenditionBackground, XtRXftColor,
    sizeof(XftColor), offset(xft_background.pixel),
    XmRImmediate, (XtPointer)XmUNSPECIFIED_PIXEL },
#else
  { XmNrenditionForeground, XmCRenditionForeground, XmRPixel,
    sizeof(Pixel), offset(rendition_foreground),
    XmRImmediate, (XtPointer)XmUNSPECIFIED_PIXEL },
  { XmNrenditionBackground, XmCRenditionBackground, XmRPixel,
    sizeof(Pixel), offset(rendition_background),
    XmRImmediate, (XtPointer)XmUNSPECIFIED_PIXEL },
#endif
  { XmNfont, XmCFont, XmRFontList,
    sizeof(XmFontList), offset(font),
    XmRImmediate, (XtPointer)XmAS_IS },
  { XmNfontName, XmCFontName, XmRString,
    sizeof(String), offset(font_name),
    XmRImmediate, (XtPointer)XmAS_IS },
#if	USE_XFT
  { XmNfontStyle, XmCFontStyle, XmRString,
    sizeof(String), offset(font_style),
    XmRImmediate, (XtPointer)NULL },
  { XmNfontFoundry, XmCFontFoundry, XmRString,
    sizeof(String), offset(font_foundry),
    XmRImmediate, (XtPointer)NULL },
  { XmNfontEncoding, XmCFontEncoding, XmRString,
    sizeof(String), offset(font_encoding),
    XmRImmediate, (XtPointer)NULL },
  { XmNfontSize, XmCFontSize, XmRInt,
    sizeof(int), offset(font_size),
    XmRImmediate, (XtPointer)0 },
#endif
  { XmNloadModel, XmCLoadModel, XmRUnsignedChar,
    sizeof(unsigned char), offset(load_model),
    XmRImmediate, (XtPointer)XmAS_IS },
  { XmNstrikethruType, XmCStrikethruType, XmRUnsignedChar,
    sizeof(unsigned char), offset(strike_thru_type),
    XmRImmediate, (XtPointer)XmAS_IS },
  { XmNtabList, XmCTabList, XmRTabList,
    sizeof(XmTabList), offset(tab_list),
    XmRImmediate, (XtPointer)XmAS_IS },
  { XmNunderlineType, XmCUnderlineType, XmRUnsignedChar,
    sizeof(unsigned char), offset(underline_type),
    XmRImmediate, (XtPointer)XmAS_IS },
  { XmNfontType, XmCFontType, XmRFontType,
    sizeof(XmFontType), offset(type), XmRImmediate, NULL },
};

/*
 * Recursively build a list of names/classes for this widget
 * Recursion necessary because we need them in the reverse order.
 */
static void InitNCList(Widget w, XrmName *names, XrmClass *classes, int *ix)
{
	Widget		p;
	XrmQuark	cn;

	if (w == NULL)
		return;
	p = XtParent(w);
	if (p) {
		InitNCList(p, names, classes, ix);
		cn = XrmStringToQuark(w->core.widget_class->core_class.class_name);
	} else {
		if (XtIsApplicationShell(w)) {
			/*
			 * Need to figure out the application class name
			 */
			cn = ((ApplicationShellWidget)w)->application.xrm_class;
		} else {
			/* Assuming that this doesn't happen,
			 * but the assignment below won't harm. */
			cn = XrmStringToQuark(w->core.widget_class->core_class.class_name);
		}
	}


	DEBUGOUT(_LtDebug(__FILE__, w, "InitNCList(%s, %s, %d)\n", XtName(w),
				XrmQuarkToString(cn), *ix));

	names[*ix] = XrmStringToQuark(XtName(w));
	classes[*ix] = cn;
	(*ix)++;
}
/*
 * Create a rendition from the resources.
 * Returns true if found.
 */
static Boolean
_XmRenditionFromResources(Display *dpy,
			Widget w,
			String name,
			String cl,
			XmStringTag tag,
			XmRendition *rp)
{
	XmRendition	r;
	XrmName		nn[10], *names = &nn[0];
	XrmClass	cll[10], *classes = &cll[0];
	XrmHashTable	*searchlist = 0;
	XrmDatabase	db = NULL;
	int		len = 8;	/* Means initially 16 */
	int		i, ix;
	Boolean		found = False;
	XrmRepresentation	tr;
	XrmValue		v, to;

	DEBUGOUT(_LtDebug(__FILE__, w,
		"_XmRenditionFromResources(name %s, class %s, tag %s)\n",
		name, cl, tag));

	/* Initialise names and classes to create a search list from */
	ix = 0;
	InitNCList(w, names, classes, &ix);

	names[ix] = XrmStringToQuark(name);
	classes[ix++] = XrmStringToQuark(cl);
	if (tag && strlen(tag)) {
		names[ix] = XrmStringToQuark(tag);
		classes[ix++] = XrmStringToQuark(XmCRendition);
	}
	names[ix] = NULLQUARK;
	classes[ix++] = NULLQUARK;

	/* Search the database in an efficient way, using a prepared search list */

	db = XtScreenDatabase(XtScreenOfObject(w));

	/* Set up a search list */
	do {
		len *= 2;
		searchlist = (XrmHashTable *)XtRealloc((char *)searchlist,
			sizeof(XrmHashTable) * len);
		for (i=0; i<len; i++)
			searchlist[i]=0;	/* Not necessary */
	} while (! XrmQGetSearchList(db, names, classes, searchlist, len));

	/* Create a rendition to fill up */
	r = XmRenditionCreate(w, tag, NULL, 0);

	/* Find resource values and convert them and store them */
	for (i=0; i<XtNumber(rt_res); i++) {
		if (XrmQGetSearchResource(searchlist,
				XrmStringToQuark(rt_res[i].resource_name),
				XrmStringToQuark(rt_res[i].resource_class),
				&tr, &v)) {
			Boolean	success;

			to.addr = ((char *)r) + rt_res[i].resource_offset;
			to.size = rt_res[i].resource_size;

			success = XtConvertAndStore(w,
				XmRString, &v,
				rt_res[i].resource_type, &to);
			if (!success) {
				/* Hack FIX ME This cannot be right */
				if (strcmp(rt_res[i].resource_type, XmRString) == 0) {
					/* String to string */
					char	**d = (char **)to.addr;
					*d = XtNewString(v.addr);
					found = True;
					DEBUGOUT(_LtDebug(__FILE__, w,
						"Convert from %s to %s value %s, %s\n",
						XmRString,
						rt_res[i].resource_type,
						v.addr,
						"fixed"));
					continue;
				}
				/* Conversion failure */
				DEBUGOUT(_LtDebug(__FILE__, w,
					"Convert from %s to %s value %s, %s\n",
					XmRString,
					rt_res[i].resource_type,
					v.addr,
					success ? "success" : "failed"));
				continue;
			} else {
				DEBUGOUT(_LtDebug(__FILE__, w,
					"Convert from %s to %s value %s, %s\n",
					XmRString,
					rt_res[i].resource_type,
					v.addr,
					success ? "success" : "failed"));
			}
			
			found = True;

			DEBUGOUT(_LtDebug(__FILE__, w,
				"_XmRenditionFromResources(%s,%s) -> %s (%p)\n",
				rt_res[i].resource_name,
				rt_res[i].resource_class,
				(char *)v.addr, r));
#if 0
		} else {
			DEBUGOUT(_LtDebug(__FILE__, w, "_XmRenditionFromResources: no %s, %s\n",
						rt_res[i].resource_name,
						rt_res[i].resource_class));
#endif
		}
	}

#if 0
	for (i=0; i<XtNumber(rt_res); i++) {
		char	st[32];
		if (XrmGetResource(db,
				rt_res[i].resource_name, rt_res[i].resource_class, &st, &v)) {
			fprintf(stderr, "Found %s (%s)\n", rt_res[i].resource_name, v);
		}
	}
#endif
	if (searchlist)
		XtFree((char *)searchlist);
	if (found && rp) {
		*rp = r;
		return True;
	}
	return False;
}

static Boolean
_XmCvtStringToRenderTable(Display *dpy,
                          XrmValue *args,
                          Cardinal *num_args,
                          XrmValue *from,
                          XrmValue *to,
                          XtPointer *converter_data)
{
	char		*list, *l;
	XmRendition	r;
	XmRenderTable	rt = NULL, ort;
	int		i, count = 0;
	Boolean		found;

	Widget	w = * (Widget *) args[0].addr;

	/*
	 *	*One.renderTable: green, variable
	 *	*One.renderTable.green.renditionForeground: Green
	 *	*One.renderTable.green.fontName: AS_IS
	 *	*One.renderTable.variable.underlineType: SINGLE_LINE
	 *	*One.renderTable.variable.renditionForeground: Red
	 *
	 *	_MOTIF_DEFAULT_LOCALE
	 *
	 *	If the search of the render table results in no font
	 *	or fontset, then if there is a rendition in the
	 *	render table with a tag of _MOTIF_DEFAULT_LOCALE,
	 *	and if that rendition specifies a font, then that
	 *	font will be used. If no font or fontset is specified
	 *	at this point, the text component will not be rendered
	 *	and a warning message will be displayed.
	 */
	/*
	 * We've got a comma-separated list in 'from',
	 * split it up and create renditions for it.
	 */
	l = list = XtMalloc(from->size + 1);
	strncpy(list, from->addr, from->size);
	list[from->size] = '\0';

	DEBUGOUT(_LtDebug(__FILE__, w, "_XmCvtStringToRenderTable(%s)\n", list));

	while (1) {
		char	*p = strchr(l, ',');
		int	last = 0;

		if (p)
			*p = '\0';
		else
			last = 1;
		/* We've got one, cut leading spaces */
		while (isspace(*l))
			l++;

		found = _XmRenditionFromResources(dpy, w,
			XmNrenderTable, XmCRenderTable,
			l, &r);
		if (found) {
			ort = rt;
			rt = XmRenderTableAddRenditions(ort, &r, 1, XmMERGE_REPLACE);
			XmRenderTableFree(ort);
			count++;
		}

		/* Look for more entries */
		l = p+1;
		if (last)
			break;
	}
	XtFree(list);

#if 1
	/*
	 * Is this an additional case or mutually exclusive with the loop above ??
	 * FIX ME
	 *
	 * Support the case of an empty renderTable list, as in
	 *	*One.renderTable:
	 *	*One.renderTable.renditionForeground: Green
	 *	*One.renderTable.fontName: AS_IS
	 */
	found = _XmRenditionFromResources(dpy, w,
			XmNrenderTable, XmCRenderTable,
			NULL, &r);
	if (found) {
		/* Now this rendition's tag is NULL, replace it by something sensible */
		if (r->tag == NULL)
			r->tag = _MOTIF_DEFAULT_LOCALE;

		ort = rt;
		rt = XmRenderTableAddRenditions(ort, &r, 1, XmMERGE_REPLACE);
		XmRenderTableFree(ort);
		count++;
	}
#endif
	/*
	 * Apart from the explicit list above, also scan for the
	 * _MOTIF_DEFAULT_LOCALE specifications.
	 */
	found = _XmRenditionFromResources(dpy, w,
			XmNrenderTable, XmCRenderTable,
			"_MOTIF_DEFAULT_LOCALE", &r);
	if (found) {
		ort = rt;
		rt = XmRenderTableAddRenditions(ort, &r, 1, XmMERGE_REPLACE);
		XmRenderTableFree(ort);
		count++;
	}

#if 0
	/*
	 * This crashes XBAE/examples/multibyte/multibyte
	 */
	XmRenditionFree(r);	/* ??? Leak removal ??? */
#endif

	/*
	 * The test below is a silly hack.
	 */
	if (!rt) {
		to->addr = NULL;
		to->size = sizeof(&rt);
		XmRenderTableFree(rt);
		DEBUGOUT(_LtDebug(__FILE__, w, "_XmCvtStringToRenderTable -> failed 1\n"));
		return False;
	}
#if 0
/*
 * Defining this gets us to have both an X Core font and an Xft font.
 * Unnecessary, time consuming, ..
 */
#define	XFT_FALLBACK
#endif

#if	defined(XFT_FALLBACK) || !defined(USE_XFT)
	/*
	 * Even though we want to implement font rendering through Xft,
	 * we do (currently still) need the font information to be present
	 * by loading the font.
	 * This should eventually only be executed when we're not using Xft.
	 * FIX ME
	 */
	for (i=0; i<rt->count; i++) {
		XmRendition	r = rt->renditions[i];
		if (r->font_name && r->font == (XFontSet)XmAS_IS) {
			if (r->type == XmFONT_IS_FONT) {
				XFontStruct	*fs = XLoadQueryFont(XtDisplay(w), r->font_name);
				if (fs) {
					r->font = (XFontSet)fs;
					DEBUGOUT(_LtDebug(__FILE__, w, "StringToFont(%s) -> %p\n",
						r->font_name, fs));
				} else {
					fs = XLoadQueryFont(XtDisplay(w), "fixed");
					if (fs) {
						r->font = (XFontSet)fs;
						DEBUGOUT(_LtDebug(__FILE__, w,
							"StringToFont(fixed) -> %p\n", fs));
					} else {
						_XmWarning(w, "Couldn't load default font");
					}
				}
			} else if (r->type == XmFONT_IS_FONTSET) {
				XFontSet	fs;
				char		**mclr = NULL;
				int		mccr;
				fs = XCreateFontSet(XtDisplay(w), r->font_name,
						&mclr, &mccr, NULL);
				DEBUGOUT(_LtDebug(__FILE__, w,
						"XCreateFontSet(%s) -> %p, %d missing\n",
							r->font_name, fs, mccr));
				r->font = fs;
			}
		}
	}
#endif

	/*
	 * Let's also do this for Xft here,
	 * even though I'm not at all sure that this is the best place.
	 */
#ifdef	USE_XFT
	for (i=0; i<rt->count; i++) {
		if (rt->renditions[i]->xft_font == 0 &&
				(rt->renditions[i]->pattern || rt->renditions[i]->font_name)) {
			XmRendition	r = rt->renditions[i];
			XftResult	res;
			XftPattern	*p;

			r->pattern = FcPatternCreate();
			if (r->font_name)
				FcPatternAddString(r->pattern, XFT_FAMILY, 
				  (unsigned char*)r->font_name);
			if (r->font_foundry)
				FcPatternAddString(r->pattern, XFT_FOUNDRY, 
				  (unsigned char*)r->font_foundry);
			if (r->font_encoding)
				FcPatternAddString(r->pattern, XFT_ENCODING, 
				  (unsigned char*)r->font_encoding);
			if (r->font_style)
				FcPatternAddString(r->pattern, XFT_STYLE, 
				  (unsigned char*)r->font_style);
			if (r->font_size)
				FcPatternAddInteger(r->pattern, XFT_SIZE, r->font_size);
			if (r->pixel_size)
				FcPatternAddInteger(r->pattern, XFT_PIXEL_SIZE, r->pixel_size);
			if (r->font_slant)
				FcPatternAddInteger(r->pattern, XFT_SLANT, r->font_slant);
			if (r->font_weight)
				FcPatternAddInteger(r->pattern, XFT_WEIGHT, r->font_weight);
			if (r->font_spacing)
				FcPatternAddInteger(r->pattern, XFT_SPACING, r->font_spacing);

			p = XftFontMatch(XtDisplay(w), 0, r->pattern, &res);
			r->xft_font = XftFontOpenPattern(XtDisplay(w), p);
			_XmXftFontAverageWidth(w,
					(XtPointer)r->xft_font,
					&r->font_average_width,
					&r->font_average_height);

			r->type = XmFONT_IS_XFT;
			r->font = NULL;		/* HACK */

			DEBUGOUT(_LtDebug(__FILE__, w,
				"Xft Find Font(fn %s) -> font %p\n",
				r->font_name, r->xft_font));
		}
	}
#endif

	/* Make sure we have a display */
	if (! rt->dpy)
		rt->dpy = dpy;

	/*
	 * Do the dance, as in any Xt converter, to return your info.
	 */
	if (to->size < sizeof(&rt)) {
		to->size = sizeof(&rt);
		XmRenderTableFree(rt);
		to->addr = NULL;
		DEBUGOUT(_LtDebug(__FILE__, w, "_XmCvtStringToRenderTable -> failed 2\n"));
		return False;
	}
	if (to->addr == NULL)
		to->addr = (XtPointer)&rt;
	else
		*(XmRenderTable *)(to->addr) = rt;

	to->addr = (XtPointer)&rt;

	DEBUGOUT(_LtDebug(__FILE__, w,
		"_XmCvtStringToRenderTable -> %p, %d elements\n", rt, count));

	return True;
}

static void
_XmCvtRenderTableFree(XtAppContext app, XrmValue *to,
		     XtPointer converter_data, XrmValue *args,
		     Cardinal *num_args)
{
	int		i;
	XmRenderTable	table = *(XmRenderTable *)to->addr;

	for (i=0; i<table->count; i++) {
		if (table->renditions[i]->type == XmFONT_IS_FONT) {
			XFreeFont(table->dpy, (XFontStruct *)table->renditions[i]->font);
		} else {
			/* FIX ME */
		}
	}

#if 0
	fprintf(stderr, "_XmCvtRenderTableFree(%p)\n", table);
#endif
	DEBUGOUT(_LtDebug(__FILE__, NULL, "_XmCvtRenderTableFree(%p)\n", table));
	XmRenderTableFree(table);
}

#if	USE_XFT
/*
 * The functions XmuCvtStringToXftColor() and XmuFreeXftColor() were
 * "stolen" from the xclock source (xc/programs/xclock/Clock.c).
 * The source file carried the following license text at the time :
 *
 * Copyright 1987, 1988, 1998  The Open Group
 *
 * Permission to use, copy, modify, distribute, and sell this software and its
 * documentation for any purpose is hereby granted without fee, provided that
 * the above copyright notice appear in all copies and that both that
 * copyright notice and this permission notice appear in supporting
 * documentation.
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.  IN NO EVENT SHALL THE
 * OPEN GROUP BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN
 * AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
 * CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 * 
 * Except as contained in this notice, the name of The Open Group shall not be
 * used in advertising or otherwise to promote the sale, use or other dealings
 * in this Software without prior written authorization from The Open Group.
 * 
 * 
 * Copyright 1987, 1988 by Digital Equipment Corporation, Maynard, Massachusetts.
 * 
 *                         All Rights Reserved
 * 
 * Permission to use, copy, modify, and distribute this software and its
 * documentation for any purpose and without fee is hereby granted,
 * provided that the above copyright notice appear in all copies and that
 * both that copyright notice and this permission notice appear in
 * supporting documentation, and that the name of Digital not be
 * used in advertising or publicity pertaining to distribution of the
 * software without specific, written prior permission.
 * 
 * DIGITAL DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING
 * ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO EVENT SHALL
 * DIGITAL BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR
 * ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS,
 * WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION,
 * ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS
 * SOFTWARE.
 */ 

#define donestr(type, value, tstr) \
        {                                                       \
            if (toVal->addr != NULL) {                          \
                if (toVal->size < sizeof(type)) {               \
                    toVal->size = sizeof(type);                 \
                    XtDisplayStringConversionWarning(dpy,       \
                        (char*) fromVal->addr, tstr);           \
                    return False;                               \
                }                                               \
                *(type*)(toVal->addr) = (value);                \
            }                                                   \
            else {                                              \
                static type static_val;                         \
                static_val = (value);                           \
                toVal->addr = (XPointer)&static_val;            \
            }                                                   \
            toVal->size = sizeof(type);                         \
            return True;                                        \
        }

static Boolean
XmuCvtStringToXftColor(Display *dpy,
                       XrmValue *args, Cardinal *num_args,
                       XrmValue *fromVal, XrmValue *toVal,
                       XtPointer *converter_data)
{
    char            *spec;
    XRenderColor    renderColor;
    XftColor        xftColor;
    Screen          *screen;
    Colormap        colormap;
    
    if (*num_args != 2)
    {
        XtAppErrorMsg (XtDisplayToApplicationContext (dpy),
                       "cvtStringToXftColor", "wrongParameters",
                       "XtToolkitError",
                       "String to render color conversion needs screen and colormap arguments",
                       (String *) NULL, (Cardinal *)NULL);
        return False;
    }
    
    screen = *((Screen **) args[0].addr);
    colormap = *((Colormap *) args[1].addr);

    spec = (char *) fromVal->addr; 
    if (strcasecmp (spec, XtDefaultForeground) == 0)
    {
        renderColor.red = 0;
        renderColor.green = 0;
        renderColor.blue = 0;
        renderColor.alpha = 0xffff;
    }
    else if (strcasecmp (spec, XtDefaultBackground) == 0)
    {
        renderColor.red = 0xffff;
        renderColor.green = 0xffff;
        renderColor.blue = 0xffff;
        renderColor.alpha = 0xffff;
    }
    else if (!XRenderParseColor (dpy, spec, &renderColor))
        return False;
    if (!XftColorAllocValue (dpy,
                             DefaultVisual (dpy,
                                            XScreenNumberOfScreen (screen)),
                             colormap,
                             &renderColor,
                             &xftColor))
        return False;

    donestr (XftColor, xftColor, XtRXftColor);
}

static void
XmuFreeXftColor (XtAppContext app, XrmValuePtr toVal, XtPointer closure,
                 XrmValuePtr args, Cardinal *num_args)
{
    Screen      *screen;
    Colormap    colormap;
    XftColor    *color;

    if (*num_args != 2)
    {
        XtAppErrorMsg (app,
                       "freeXftColor", "wrongParameters",
                       "XtToolkitError",
                       "Freeing an XftColor requires screen and colormap arguments",
                       (String *) NULL, (Cardinal *)NULL);
        return;
    }

    screen = *((Screen **) args[0].addr);
    colormap = *((Colormap *) args[1].addr);
    color = (XftColor *) toVal->addr;
    XftColorFree (DisplayOfScreen (screen),
                  DefaultVisual (DisplayOfScreen (screen),
                                 XScreenNumberOfScreen (screen)),
                  colormap, color);
}


#endif

extern unsigned int
XmCvtXmStringToByteStream(XmString string,
                          unsigned char **prop_return)
{
        _XmWarning(NULL, "XmConvertStringToUnits() is not implemented yet!\n");
	return 0;
}



extern XmString 
XmCvtByteStreamToXmString(unsigned char *property)
{
      _XmWarning(NULL, "XmCvtByteStreamToXmString() is not implemented yet!\n");
      return NULL;
}


extern int
XmCvtXmStringTableToTextProperty (Display *display,
                                  XmStringTable string_table,
                                  int count,
                                  XmICCEncodingStyle style,
                                  XTextProperty *text_prop_return)
{
        _XmWarning(NULL, "XmCvtXmStringTableToTextProperty() is not implemented yet!\n");
	return 0;
}				      
