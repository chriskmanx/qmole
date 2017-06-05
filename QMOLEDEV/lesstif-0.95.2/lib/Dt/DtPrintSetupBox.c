/**
 *
 * $Header: /cvsroot/lesstif/lesstif/lib/Dt/DtPrintSetupBox.c,v 1.47 2004/04/30 20:23:23 dannybackx Exp $
 *
 * Copyright © 2000, 2001 LessTif Development Team
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

static const char rcsid[] = "$Header: /cvsroot/lesstif/lesstif/lib/Dt/DtPrintSetupBox.c,v 1.47 2004/04/30 20:23:23 dannybackx Exp $";

#include <LTconfig.h>

#include <stdlib.h>
#include <stdio.h>	/* for debugging only ? */
#include <string.h>
#include <ctype.h>

#include <Xm/Xm.h>
#include <Xm/XmP.h>
#include <Xm/CommandP.h>
#include <Xm/List.h>
#include <Xm/PushBP.h>
#include <Xm/PushBGP.h>
#include <Xm/RowColumnP.h>
#include <Xm/TransltnsP.h>
#include <Xm/SeparatoGP.h>
#include <Xm/DialogS.h>
#include <Xm/TextF.h>
#include <Xm/MessageB.h>
#include <Xm/SpinB.h>
#include <Xm/ComboBox.h>
#include <Xm/SelectioB.h>
#include <Xm/ToggleBG.h>
#include <Xm/FileSB.h>
#include <Xm/RepType.h>

#include <XmI/XmI.h>
#include <XmI/MacrosI.h>

#include <Dt/Print.h>
#include <Dt/DtPrintSetupBoxP.h>


#include <XmI/DebugUtil.h>


/* Forward Declarations */
static void _DtDefaultPrinterInfoProc(Widget w, DtPrintSetupData *print_data);
static void _DtDefaultSelectFileProc(Widget w, DtPrintSetupData *print_data);
static XtEnum _DtDefaultSelectPrinterProc(Widget w, DtPrintSetupData *print_data);
static void _DtDefaultSetupProc(Widget w, DtPrintSetupData *print_data);
static void _DtDefaultVerifyPrinterProc(Widget w, DtPrintSetupData *print_data);

static void class_initialize(void);
static void class_part_initialize(WidgetClass w_class);
static void Initialize(Widget request, Widget new_w, ArgList args, Cardinal *num_args);
static void destroy(Widget w);
static Boolean SetValues(Widget current, Widget request, Widget new_w,
			  ArgList args, Cardinal *num_args);
static void InsertChild(Widget w);
static void DeleteChild(Widget w);

static XmGeoMatrix _DtPrintSetupBoxGeoMatrixCreate(Widget _w, Widget _from, XtWidgetGeometry *_pref);
Boolean _DtPrintSetupBoxNoGeoRequest(XmGeoMatrix _geoSpec);
void _DtPrintSetupBoxGetTextColumns(Widget wid, int resource_offset, XtArgVal *value);
void _DtPrintSetupBoxGetTextString(Widget wid, int resource_offset, XtArgVal *value);

static void _DtPrintSetupBoxCreateBottomWorkAreaSeparator(DtPrintSetupBoxWidget sb);
static void _DtPrintSetupBoxCreateTopWorkAreaSeparator(DtPrintSetupBoxWidget sb);
static void _DtPrintSetupBoxCreateButtonSeparator(DtPrintSetupBoxWidget sb);
static void _DtPrintSetupBoxCreateCopiesSpinBox(DtPrintSetupBoxWidget sb);
static void _DtPrintSetupBoxCreateCopiesLabel(DtPrintSetupBoxWidget sb);
static void _DtPrintSetupBoxCreateDescriptionLabel(DtPrintSetupBoxWidget sb);
static void _DtPrintSetupBoxCreateFileName(DtPrintSetupBoxWidget sb);
static void _DtPrintSetupBoxCreateFileNameCheckBox(DtPrintSetupBoxWidget sb);
static void _DtPrintSetupBoxCreateInfoButton(DtPrintSetupBoxWidget sb);
static void _DtPrintSetupBoxCreateSelectFileButton(DtPrintSetupBoxWidget sb);
static void _DtPrintSetupBoxCreateSelectPrinterButton(DtPrintSetupBoxWidget sb);
static void _DtPrintSetupBoxCreateSetupButton(DtPrintSetupBoxWidget sb);
static void _DtPrintSetupBoxCreateOkButton(DtPrintSetupBoxWidget sb);
static void _DtPrintSetupBoxCreateCancelButton(DtPrintSetupBoxWidget sb);
static void _DtPrintSetupBoxCreateHelpButton(DtPrintSetupBoxWidget sb);
static void _DtPrintSetupBoxCreatePrinterName(DtPrintSetupBoxWidget sb);
static void _DtPrintSetupBoxCreateToggle(DtPrintSetupBoxWidget sb);

static void _DtPsbUpdatePrinterList(Widget w);
static void _DtPsbSetDefaultPrinter(Widget w);
static void _DtPsbAddToPrinterList(Widget w, Display *d);
static void _DtSelectPrinterCancelCB(Widget w, XtPointer client, XtPointer call);

extern void _DtPrintSetupBoxGetSelectionLabelString(Widget w, int resource_offset,
				       XtArgVal *value);
extern void _DtPrintSetupBoxGetOkLabelString(Widget w, int resource_offset,
				XtArgVal *value);
extern void _DtPrintSetupBoxGetCancelLabelString(Widget w, int resource_offset,
				    XtArgVal *value);
extern void _DtPrintSetupBoxGetHelpLabelString(Widget w, int resource_offset,
				  XtArgVal *value);


/*
 * Resources for the DtPrintSetupBox class
 */
#define Offset(field) XtOffsetOf(DtPrintSetupBoxRec, printsetup_box.field)
static XtResource resources[] =
{
    {
	DtNclosePrintDisplayCallback, DtCClosePrintDisplayCallback, XmRCallback,
	sizeof(XtCallbackList), Offset(close_print_display_callback),
	XmRImmediate, (XtPointer)NULL
    },
    {
	DtNcopies, DtCCopies, XmRInt,
	sizeof(int), Offset(copies),
	XmRImmediate, (XtPointer)0
    },
    {
	DtNdescription, DtCDescription, XmRXmString,
	sizeof(XmString), Offset(description),
	XmRImmediate, (XtPointer)XmUNSPECIFIED
    },
    {
	DtNfileName, DtCPrintToFileName, XmRString,
	sizeof(String), Offset(file_name),
	XmRImmediate, (XtPointer)NULL
    },
    {
	DtNgetAttributesCallback, DtCGetAttributesCallback, XmRCallback,
	sizeof(XtCallbackList), Offset(get_attributes_callback),
	XmRImmediate, (XtPointer)NULL
    },
    {
	DtNminimizeButtons, DtCMinimizeButtons, XmRBoolean,
	sizeof(Boolean), Offset(minimize_buttons),
	XmRImmediate, (XtPointer)False
    },
    {
	DtNprintCallback, DtCPrintCallback, XmRCallback,
	sizeof(XtCallbackList), Offset(print_callback),
	XmRImmediate, (XtPointer)NULL
    },
    {
	DtNprintDestination, DtCPrintDestination, XmREnum,
	sizeof(XtEnum), Offset(print_destination),
	XmRImmediate, (XtPointer)DtPRINT_TO_PRINTER
    },
    {
	DtNprinterInfoProc, DtCPrinterInfoProc, DmRPrintSetupProc,
	sizeof(DtPrintSetupProc), Offset(printer_info_proc),
	XmRImmediate, (XtPointer)_DtDefaultPrinterInfoProc
    },
    {
	DtNprinterName, DtCPrinterName, XmRString,
	sizeof(String), Offset(printer_name),
	XmRImmediate, (XtPointer)NULL
    },
    {
	DtNprintSetupMode, DtCPrintSetupMode, XmREnum,
	sizeof(XtEnum), Offset(print_setup_mode),
	XmRImmediate, (XtPointer)DtPRINT_SETUP_XP
    },
    {
	DtNselectFileProc, DtCSelectFileProc, DmRPrintSetupProc,
	sizeof(DtPrintSetupProc), Offset(select_file_proc),
	XmRImmediate, (XtPointer)_DtDefaultSelectFileProc
    },
    {
	DtNsetAttributesCallback, DtCSetAttributesCallback, XmRCallback,
	sizeof(XtCallbackList), Offset(set_attributes_callback),
	XmRImmediate, (XtPointer)NULL
    },
    {
	DtNsetupProc, DtCSetupProc, DmRPrintSetupProc,
	sizeof(DtPrintSetupProc), Offset(setup_proc),
	XmRImmediate, (XtPointer)_DtDefaultSetupProc
    },
    {
	DtNverifyPrinterProc, DtCVerifyPrinterProc, DmRPrintSetupProc,
	sizeof(DtPrintSetupProc), Offset(verify_printer_proc),
	XmRImmediate, (XtPointer)_DtDefaultVerifyPrinterProc
    },
    {
	DtNworkAreaLocation, DtCWorkAreaLocation, XmREnum,
	sizeof(XtEnum), Offset(work_area_location),
	XmRImmediate, (XtPointer)DtWORK_AREA_BOTTOM
    },
    {
	DtNsetupCallback, DtCSetupCallback, XmRCallback,
	sizeof(XtCallbackList), Offset(setup_callback),
	XmRImmediate, (XtPointer)NULL
    },
};

static XmSyntheticResource syn_resources[] =
{
    {NULL}
#if 0
    {
	XmNlistLabelString,
	sizeof(XmString), Offset(list_label_string),
	_XmExportXmString, NULL	/* FIX ME */
    },
    {
	XmNokLabelString,
	sizeof(XmString), Offset(ok_label_string),
	_XmExportXmString, NULL
    },
    {
	XmNcancelLabelString,
	sizeof(XmString), Offset(cancel_label_string),
	_XmExportXmString, NULL
    },
    {
	XmNhelpLabelString,
	sizeof(XmString), Offset(help_label_string),
	_XmExportXmString, NULL
    }
#endif
};

static XtActionsRec actions[] =
{
  {NULL}
};

static char *dt_conversion_txt[] = {
	"work_area_top",
	"work_area_bottom",
	"work_area_top_and_bottom",
	"DtPRINT_TO_FILE",
	"DtPRINT_TO_PRINTER",
	"print_to_file",
	"print_to_printer",
	"print_setup_plain",
	"print_setup_xp"
};

static unsigned char dt_conversion_values[] = {
	DtWORK_AREA_TOP,
	DtWORK_AREA_BOTTOM,
	DtWORK_AREA_TOP_AND_BOTTOM,
	DtPRINT_TO_FILE,
	DtPRINT_TO_PRINTER,
	DtPRINT_TO_FILE,
	DtPRINT_TO_PRINTER,
	DtPRINT_SETUP_PLAIN,
	DtPRINT_SETUP_XP
};

DtPrintSetupBoxClassRec dtPrintSetupBoxClassRec = {
    /* Core class part */
    {
	/* superclass            */ (WidgetClass) &xmBulletinBoardClassRec,
        /* class_name            */ "DtPrintSetupBox",
	/* widget_size           */ sizeof(DtPrintSetupBoxRec),
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
	/* destroy               */ destroy,
	/* resize                */ XtInheritResize,
	/* expose                */ XtInheritExpose,
	/* set_values            */ SetValues,
	/* set_values_hook       */ NULL,
	/* set_values_almost     */ XtInheritSetValuesAlmost,
	/* get_values_hook       */ NULL,
	/* accept_focus          */ NULL,
	/* version               */ XtVersion,
	/* callback offsets      */ NULL,
	/* tm_table              */ XtInheritTranslations,
	/* query_geometry        */ XtInheritQueryGeometry,
	/* display_accelerator   */ NULL,
	/* extension             */ (XtPointer)NULL
    },
    /* Composite class part */
    {
	/* geometry manager */ XtInheritGeometryManager, 
        /* change_managed   */ XtInheritChangeManaged, 
        /* insert_child     */ InsertChild,
        /* delete_child     */ DeleteChild,
        /* extension        */ NULL,
    },
    /* Constraint class part */
    {
	/* subresources      */ NULL,
        /* subresource_count */ 0,
        /* constraint_size   */ 0,
        /* initialize        */ NULL,
        /* destroy           */ NULL,
        /* set_values        */ NULL,
        /* extension         */ NULL,
    },
    /* XmManager class part */
    {
        /* translations                 */ XmInheritTranslations,
        /* syn_resources                */ syn_resources,
        /* num_syn_resources            */ XtNumber(syn_resources),
        /* syn_constraint_resources     */ NULL,
        /* num_syn_constraint_resources */ 0,
        /* parent_process               */ XmInheritParentProcess,
        /* extension                    */ (XtPointer)NULL
    },
    /* XmBulletinBoard class part */
    {
	/* always_install_accelerators  */ True,
	/* geo_matrix_create            */ _DtPrintSetupBoxGeoMatrixCreate,
	/* focus_moved_proc             */ XmInheritFocusMovedProc,
	/* extension                    */ NULL,

    },
    /* DtPrintSetupBox part */
    {
	/* list_callback                */ NULL,
	/* extension                    */ NULL,
    }
};

WidgetClass dtPrintSetupBoxWidgetClass = (WidgetClass)&dtPrintSetupBoxClassRec;

static void
class_initialize(void)
{
#if 0
    _XmSelectionBCoreClassExtRec.record_type = XmQmotif;
#endif

	/* Type converters */
#define REPTYPE_REGISTER_WITH_VALUES(reptype,array,values) \
    XmRepTypeRegister((reptype), \
                     (array), \
                     (values), XtNumber((array)));

	REPTYPE_REGISTER_WITH_VALUES(XmREnum, dt_conversion_txt, dt_conversion_values);
}

static void
class_part_initialize(WidgetClass widget_class)
{
    DtPrintSetupBoxWidgetClass sbwc = (DtPrintSetupBoxWidgetClass)widget_class;
    DtPrintSetupBoxWidgetClass swc = (DtPrintSetupBoxWidgetClass)widget_class->core_class.superclass;

    if (sbwc->printsetup_box_class.list_callback == XmInheritCallbackProc)
    {
    	sbwc->printsetup_box_class.list_callback = swc->printsetup_box_class.list_callback;
    }
#if 0
    _XmFastSubclassInit(widget_class, XmSELECTION_BOX_BIT);
#endif
}

/*
 */
static void
Initialize(Widget request, Widget new_w, ArgList args, Cardinal *num_args)
{
	DtPrintSetupBoxWidget sb = (DtPrintSetupBoxWidget)new_w;

	PSB_BSeparator(sb) = NULL;

	/* core dump avoidance in GeoUtils */
	BB_CancelButton(sb) = NULL;
	PSB_OkButton(sb) = NULL;
	PSB_InfoButton(sb) = NULL;
	PSB_HelpButton(sb) = NULL;
	PSB_CancelLabelString(sb) = NULL;
	PSB_OkLabelString(sb) = NULL;
	PSB_HelpLabelString(sb) = NULL;
	PSB_PrinterSelectionDialog(new_w) = NULL;
	PSB_FileSelectionDialog(new_w) = NULL;

	PSB_PrinterName(new_w) = NULL;			/* Why ? */

	PSB_SetupLabelString(new_w) = NULL;
	PSB_SelectFileLabelString(new_w) = NULL;
	PSB_SelectPrinterLabelString(new_w) = NULL;
	PSB_NameComboBox(sb) = NULL;

	_DtPrintSetupBoxCreateBottomWorkAreaSeparator(sb);
	_DtPrintSetupBoxCreateTopWorkAreaSeparator(sb);
	_DtPrintSetupBoxCreateButtonSeparator(sb);
	_DtPrintSetupBoxCreateOkButton(sb);
	_DtPrintSetupBoxCreateCancelButton(sb);
	_DtPrintSetupBoxCreateHelpButton(sb);
	_DtPrintSetupBoxCreateCopiesSpinBox(sb);
	_DtPrintSetupBoxCreateCopiesLabel(sb);
	_DtPrintSetupBoxCreateDescriptionLabel(sb);
	_DtPrintSetupBoxCreateFileNameCheckBox(sb);
	_DtPrintSetupBoxCreateFileName(sb);
	_DtPrintSetupBoxCreateInfoButton(sb);
	_DtPrintSetupBoxCreateSelectFileButton(sb);
	_DtPrintSetupBoxCreateSelectPrinterButton(sb);
	_DtPrintSetupBoxCreateSetupButton(sb);

	_DtPrintSetupBoxCreatePrinterName(sb);
	_DtPrintSetupBoxCreateToggle(sb);

	_XmSetInitialOfTabGroup((Widget)sb, (Widget)PSB_PrinterNameTF(sb));

	BB_DefaultButton(new_w) = PSB_OkButton(sb);
	_XmBulletinBoardSetDynDefaultButton(new_w, PSB_OkButton(sb));

	PSB_PrintSetupData(new_w) = NULL;

	if (PSB_PrintSetupMode(new_w) == DtPRINT_SETUP_XP) {
		/*
		 * The widget must manage an X Printing display connection,
		 * a print context, and provide defaults for a number of X
		 * Printing operations such as printer selection and information
		 * dialogs, and printer verification.
		 */
		PSB_PrintSetupData(new_w) = (DtPrintSetupData *) XtMalloc(sizeof(DtPrintSetupData));
		memset(PSB_PrintSetupData(new_w), 0, sizeof(DtPrintSetupData));
		PSB_PrintSetupData(new_w)->print_display = NULL;
		DtPrintFillSetupData(NULL, PSB_PrintSetupData(new_w));

		PSB_SelectPrinterProc(new_w) = _DtDefaultSelectPrinterProc;

		_DtPsbSetDefaultPrinter(new_w);
	} else {
		PSB_SelectPrinterProc(new_w) = NULL;
	}

	PSB_WorkAreaTop(new_w) = NULL;
	PSB_WorkAreaBottom(new_w) = NULL;
}

static void
destroy(Widget w)
{
	DtPrintSetupCallbackStruct	cbs;

	DEBUGOUT(_LtDebug(__FILE__, w, "Destroy\n"));

	if (PSB_PrintSetupMode(w) == DtPRINT_SETUP_XP &&
	    PSB_ClosePrintDisplayCallback(w)) {
		cbs.reason = DtPRINT_CR_CLOSE_PRINT_DISPLAY;
		cbs.event = NULL;	/* tfm says invalid */
		cbs.print_data = PSB_PrintSetupData(w);
		XtCallCallbackList(w,
			PSB_ClosePrintDisplayCallback(w),
			&cbs);
	}

	DtPrintFreeSetupData(PSB_PrintSetupData(w));
}

/*
 * handle element updates
 */
#define	NE(x)	(x(o) != x(sb))
static Boolean
SetValues(Widget old,
	   Widget request,
	   Widget new_w,
	   ArgList args,
	   Cardinal *num_args)
{
    DtPrintSetupBoxWidget sb = (DtPrintSetupBoxWidget)new_w;
    DtPrintSetupBoxWidget o = (DtPrintSetupBoxWidget)old;
    Boolean r = False;		/* whether to redisplay */
    Arg al[10];
    int ac;

	DEBUGOUT(_LtDebug(__FILE__, new_w, "SetValues\n"));

    /* this is required for every BB subclass */
    BB_InSetValues(new_w) = True;

    if (NE(PSB_Copies)) {
	XtVaSetValues(PSB_CopiesSpinBoxTF(new_w), XmNposition, PSB_Copies(new_w), NULL);
    }

    if (NE(PSB_CopiesLabelString)) {
	XtVaSetValues(PSB_CopiesLabel(new_w), XmNlabelString, PSB_CopiesLabelString(new_w), NULL);
    }

    if (NE(PSB_DescriptionLabel)) {
	XtVaSetValues(PSB_Description(new_w), XmNlabelString, PSB_DescriptionLabel(new_w), NULL);
    }

    if (NE(PSB_FileName)) {
	/* Update the textfield */
	XmTextFieldSetString(PSB_FileNameTF(new_w), PSB_FileName(new_w));
    }

    if (NE(PSB_MinimizeButtons)) {
	/* No actions required ? May need to tweak layout matrix */
	r = True;
    }

    if (NE(PSB_PrintDestination)) {
	if (PSB_PrintDestination(new_w) == DtPRINT_TO_FILE) {
		DEBUGOUT(_LtDebug(__FILE__, new_w, "PSB: PrintToFile\n"));
		XmToggleButtonGadgetSetState(PSB_PrintToFileToggle(new_w), True, False);
		XmToggleButtonGadgetSetState(PSB_PrintToPrinterToggle(new_w), False, False);
		XtSetSensitive(PSB_FileNameTF(new_w), True);
		XtSetSensitive(PSB_FileNameTF(new_w), False);
	} else if (PSB_PrintDestination(new_w) == DtPRINT_TO_PRINTER) {
		DEBUGOUT(_LtDebug(__FILE__, new_w, "PSB: PrintToFile\n"));
		XmToggleButtonGadgetSetState(PSB_PrintToFileToggle(new_w), False, False);
		XmToggleButtonGadgetSetState(PSB_PrintToPrinterToggle(new_w), True, False);
		XtSetSensitive(PSB_FileNameTF(new_w), False);
		XtSetSensitive(PSB_FileNameTF(new_w), True);
	} else {
		/* Ignore error ? */
		_XmWarning(new_w, "SetValues: invalid value for DtNprintDestination");
	}
    }

    if (NE(PSB_PrinterName)) {
	if (PSB_PrinterName(old))
		XtFree(PSB_PrinterName(old));
	PSB_PrinterName(new_w) = XtNewString(PSB_PrinterName(new_w));
	XmTextFieldSetString(PSB_PrinterNameTF(new_w), PSB_PrinterName(new_w));
    }

    if (NE(PSB_PrintSetupMode)) {
    }

    if (NE(PSB_WorkAreaLocation)) {
	r = True;
    }

    if (NE(PSB_SetupLabelString)) {
	XtVaSetValues(PSB_SetupButton(new_w), XmNlabelString, PSB_SetupLabelString(new_w), NULL);
    }

    if (NE(PSB_SelectFileLabelString)) {
	XtVaSetValues(PSB_SelectFileButton(new_w), XmNlabelString, PSB_SelectFileLabelString(new_w), NULL);
    }

    if (NE(PSB_SelectPrinterLabelString)) {
	XtVaSetValues(PSB_SelectPrinterButton(new_w), XmNlabelString, PSB_SelectPrinterLabelString(new_w), NULL);
    }

    /* Labels : OK Button Label */
    ac = 0;
    if (NE(PSB_OkLabelString)) {
	PSB_OkLabelString(sb) = XmStringCopy(PSB_OkLabelString(sb));
	XmStringFree(PSB_OkLabelString(o));
	XtSetArg(al[ac], XmNlabelString, PSB_OkLabelString(sb)); ac++;
	r = True;
    }
    if (ac && PSB_OkButton(sb)) {
	XtSetValues(PSB_OkButton(sb), al, ac);
    }

    /* Labels : Cancel Button Label */
    ac = 0;
    if (NE(PSB_CancelLabelString)) {
	PSB_CancelLabelString(sb) = XmStringCopy(PSB_CancelLabelString(sb));
	XmStringFree(PSB_CancelLabelString(o));
	XtSetArg(al[ac], XmNlabelString, PSB_CancelLabelString(sb)); ac++;
	r = True;
    }
    if (ac && BB_CancelButton(sb)) {
	XtSetValues(BB_CancelButton(sb), al, ac);
    }

    /* Labels : Help Button Label */
    ac = 0;
    if (NE(PSB_HelpLabelString)) {
	PSB_HelpLabelString(sb) = XmStringCopy(PSB_HelpLabelString(sb));
	XmStringFree(PSB_HelpLabelString(o));
	XtSetArg(al[ac], XmNlabelString, PSB_HelpLabelString(sb)); ac++;
	r = True;
    }
    if (ac && PSB_HelpButton(sb)) {
	XtSetValues(PSB_HelpButton(sb), al, ac);
    }

    BB_InSetValues(new_w) = False;

    if (r && (XtClass(new_w) == xmSelectionBoxWidgetClass)) {
	_XmBulletinBoardSizeUpdate(new_w);

	r =  False;
    }

    return r;
}
/* PrinterName */

#define	BUTTON(v, nm, txt)						\
	{								\
		XmString	xms = XmStringCreateSimple(txt);	\
		v(sb)= _XmBB_CreateButtonG((Widget)sb, xms, nm);		\
		XmStringFree(xms);					\
	}
#define	LABEL(v, nm, txt)						\
	{								\
		XmString	xms = XmStringCreateSimple(txt);	\
		v(sb)= _XmBB_CreateLabelG((Widget)sb, xms, nm);		\
		XmStringFree(xms);					\
	}
		
/*
 * _XmPsbOkCB
 */
static void
_XmPsbOkCB(Widget w, XtPointer client, XtPointer call)
{
	DtPrintSetupCallbackStruct	cbs;
	Widget				b = XtParent(w);
	XmPushButtonCallbackStruct	*cbp = (XmPushButtonCallbackStruct *)call;

	if (! XtIsSubclass(b, dtPrintSetupBoxWidgetClass)) {
		DEBUGOUT(_LtDebug(__FILE__, w, "_XmPsbOkCB() - nop\n"));
		return;
	}

#if 0
	if (PSB_PrinterName(b) == NULL) {
		/* Should some warning be issued ? */
		DEBUGOUT(_LtDebug(__FILE__, w, "_XmPsbOkCB() - NULL\n"));
		_XmWarning(b, "_XmPsbOkCB: NULL printerName\n");
		return;
	}
#endif
	DEBUGOUT(_LtDebug(__FILE__, w, "_XmPsbOkCB()\n"));

	/* Select the printer, do all the necessary things */
	if (PSB_PrintSetupData(b)->printer_name) {
		XtFree(PSB_PrintSetupData(b)->printer_name);
		PSB_PrintSetupData(b)->printer_name = XtNewString(PSB_PrinterName(b));
	}

	if (PSB_PrintSetupData(b)) {
		if (PSB_PrintSetupMode(b) == DtPRINT_SETUP_XP &&
		    PSB_ClosePrintDisplayCallback(b)) {
			cbs.reason = DtPRINT_CR_CLOSE_PRINT_DISPLAY;
			cbs.event = NULL;	/* tfm says invalid */
			cbs.print_data = PSB_PrintSetupData(b);
			XtCallCallbackList(b,
				PSB_ClosePrintDisplayCallback(b),
				&cbs);
		}
		DtPrintFreeSetupData(PSB_PrintSetupData(b));
	}

	/* This should do the hard work */
	DtPrintFillSetupData(b, PSB_PrintSetupData(b));

	/* Fill the callback structure */
	cbs.reason = DtPRINT_CR_PRINT;
	cbs.event = cbp->event;
	cbs.print_data = PSB_PrintSetupData(b);

	/* Call the callback list */
	XtCallCallbackList(b, PSB_PrintCallback(b), &cbs);
}

static void
_DtPrintSetupBoxCreateButtonSeparator(DtPrintSetupBoxWidget sb)
{
    int ac;
    Arg al[2];

    ac = 0;
    XtSetArg(al[ac], XmNseparatorType, XmSHADOW_ETCHED_IN); ac++;
    XtSetArg(al[ac], XmNhighlightThickness, 0); ac++;
    PSB_BSeparator(sb) = XmCreateSeparatorGadget((Widget)sb, "ButtonSeparator", al, ac);
    XtManageChild(PSB_BSeparator(sb));
}


static void
_DtPrintSetupBoxCreateBottomWorkAreaSeparator(DtPrintSetupBoxWidget sb)
{
    int ac;
    Arg al[2];

    ac = 0;
    XtSetArg(al[ac], XmNseparatorType, XmSHADOW_ETCHED_IN); ac++;
    XtSetArg(al[ac], XmNhighlightThickness, 0); ac++;
    PSB_BWASeparator(sb) = XmCreateSeparatorGadget((Widget)sb, "BottomWorkAreaSeparator", al, ac);
    XtManageChild(PSB_BWASeparator(sb));
}


static void
_DtPrintSetupBoxCreateTopWorkAreaSeparator(DtPrintSetupBoxWidget sb)
{
    int ac;
    Arg al[2];

    ac = 0;
    XtSetArg(al[ac], XmNseparatorType, XmSHADOW_ETCHED_IN); ac++;
    XtSetArg(al[ac], XmNhighlightThickness, 0); ac++;
    PSB_TWASeparator(sb) = XmCreateSeparatorGadget((Widget)sb, "TopWorkAreaSeparator", al, ac);
    XtManageChild(PSB_TWASeparator(sb));
}

static void
_DtPrintSetupBoxCreateOkButton(DtPrintSetupBoxWidget sb)
{
    BUTTON(PSB_OkButton, "ok", "OK");
    XtManageChild(PSB_OkButton(sb));
    XtAddCallback(PSB_OkButton(sb), XmNactivateCallback, _XmPsbOkCB, NULL);
}

static void
_DtPrintSetupBoxCreateCancelButton(DtPrintSetupBoxWidget sb)
{
    BUTTON(BB_CancelButton, "cancel", "Cancel");
    XtManageChild(BB_CancelButton(sb));
}

/*
 * Process changes to Copies spinbox
 */
static void
_DtPSBSpinBoxCB(Widget w, XtPointer client, XtPointer call)
{
	Widget			psb = XtParent(w);
	XmSpinBoxCallbackStruct	*cbp = (XmSpinBoxCallbackStruct *) call;
	char			*s;

	if (! XmStringGetLtoR(cbp->value, XmFONTLIST_DEFAULT_TAG, &s)) {
		return;
	}

	PSB_Copies(psb) = atoi(s);
	XtFree(s);

	DEBUGOUT(_LtDebug(__FILE__, psb, "DtPSB: %d copies\n", PSB_Copies(psb)));
}

static void
_DtPrintSetupBoxCreateCopiesSpinBox(DtPrintSetupBoxWidget sb)
{
    Widget	tf;
    int		ac;
    Arg		al[8];

    ac = 0;
    XtSetArg(al[ac], XmNarrowLayout, XmARROWS_END); ac++;
    XtSetArg(al[ac], XmNarrowOrientation, XmARROWS_VERTICAL); ac++;
    PSB_CopiesSpinBox(sb) = XmCreateSpinBox((Widget)sb, "Copies", al, ac);
    XtManageChild(PSB_CopiesSpinBox(sb));
    XtAddCallback(PSB_CopiesSpinBox(sb), XmNvalueChangedCallback, _DtPSBSpinBoxCB, NULL);

    ac = 0;
    XtSetArg(al[ac], XmNspinBoxChildType, XmNUMERIC); ac++;
    XtSetArg(al[ac], XmNpositionType, XmPOSITION_VALUE); ac++;
    XtSetArg(al[ac], XmNdecimalPoints, 0); ac++;
    XtSetArg(al[ac], XmNminimumValue, 1); ac++;
    XtSetArg(al[ac], XmNmaximumValue, 1000); ac++;
    PSB_CopiesSpinBoxTF(sb) = tf = XmCreateTextField(PSB_CopiesSpinBox(sb), "tf", al, ac);
    XtManageChild(tf);
}

static void
_DtPrintSetupBoxCreateCopiesLabel(DtPrintSetupBoxWidget sb)
{
    LABEL(PSB_CopiesLabel, "copies", "Copies :");
    XtManageChild(PSB_CopiesLabel(sb));
}

static void
_DtPrintSetupBoxCreateDescriptionLabel(DtPrintSetupBoxWidget sb)
{
    LABEL(PSB_DescriptionLabel, "description", "Printer Description :");
    XtManageChild(PSB_DescriptionLabel(sb));

    LABEL(PSB_Description, "descript", " ");
    XtManageChild(PSB_Description(sb));
}

static void
_DtPrintSetupBoxCreateFileName(DtPrintSetupBoxWidget sb)
{
    PSB_FileNameTF(sb) = XmCreateTextField(PSB_FileNameCheckBox(sb), "FileName", NULL, 0);
    XtManageChild(PSB_FileNameTF(sb));
}

static void
_DtPrintSetupBoxCreateFileNameCheckBox(DtPrintSetupBoxWidget sb)
{
	Arg		al[5];
	int		ac = 0;

	XtSetArg(al[ac], XmNorientation, XmHORIZONTAL); ac++;	/* ?? FIX ME */
	PSB_FileNameCheckBox(sb) = XmCreateRowColumn((Widget)sb, "FileNameCheckBox", al, ac);
	XtManageChild(PSB_FileNameCheckBox(sb));

	{	/* Can't use the LABEL macro: different parent widget */
		XmString	xms = XmStringCreateSimple("File Name :");
		PSB_FileNameLabel(sb) =
			_XmBB_CreateLabelG(PSB_FileNameCheckBox(sb), xms, "filename");
		XmStringFree(xms);
	}
	XtManageChild(PSB_FileNameLabel(sb));
}

/*
 * The use has triggered the Info... button.
 */
static void
_DtPrintSetupBoxInfoButtonCB(Widget w, XtPointer client, XtPointer call)
{
	Widget	psb = (Widget)client;

	if (PSB_PrinterInfoProc(psb))
		(*PSB_PrinterInfoProc(psb))(psb, PSB_PrintSetupData(psb));
}

static void
_DtPrintSetupBoxCreateInfoButton(DtPrintSetupBoxWidget sb)
{
    BUTTON(PSB_InfoButton, "info", "Info...");
    XtManageChild(PSB_InfoButton(sb));
    XtRemoveAllCallbacks(PSB_InfoButton(sb), XmNactivateCallback);
    XtAddCallback(PSB_InfoButton(sb), XmNactivateCallback,
		_DtPrintSetupBoxInfoButtonCB, (XtPointer)sb);
}

static void
_DtPrintSetupBoxCreateHelpButton(DtPrintSetupBoxWidget sb)
{
    BUTTON(PSB_HelpButton, "help", "Help");
    XtManageChild(PSB_HelpButton(sb));
    /* get rid of auto_unmanage */
    XtRemoveAllCallbacks(PSB_HelpButton(sb), XmNactivateCallback);
}

static void
_DtSelectFileOkCB(Widget w, XtPointer client, XtPointer call)
{
	Widget	d = (Widget)client;
	char	*s;
	XmFileSelectionBoxCallbackStruct	*cbp = (XmFileSelectionBoxCallbackStruct *)call;

	if (! XmStringGetLtoR(cbp->value, XmFONTLIST_DEFAULT_TAG, &s)) {
		/* Can't convert it into a string */
		/* Note: we're not popping down the dialog ! */
		return;
	}

	XtVaSetValues(d,
			DtNfileName,		s,
			DtNprintDestination,	DtPRINT_TO_FILE,
		NULL);
	XtFree(s);

	XtUnmanageChild(w);
}

/*
 * This is the default for DtNselectFileProc
 *
 * Invokes a FileSelectionDialog for selecting a file name.
 * If the user makes a selection, then this is used to set DtNfileName.
 * The return value is ignored.
 */
static void
_DtDefaultSelectFileProc(Widget w, DtPrintSetupData *print_data)
{
	Widget	d;
	Arg	al[5];
	int	ac;

	DEBUGOUT(_LtDebug(__FILE__, w, "_DtDefaultSelectFileProc()\n"));

	if (PSB_FileSelectionDialog(w) == NULL) {
		ac = 0;
		XtSetArg(al[ac], XmNdeleteResponse, XmUNMAP); ac++;
		XtSetArg(al[ac], XmNautoUnmanage, False); ac++;
		PSB_FileSelectionDialog(w) = d =
			XmCreateFileSelectionDialog(XtParent(w), "select file", al, ac);
		XtAddCallback(d, XmNokCallback, _DtSelectFileOkCB, (XtPointer)w);
		XtAddCallback(d, XmNcancelCallback, _DtSelectPrinterCancelCB, (XtPointer)w);
	}
	XtManageChild(PSB_FileSelectionDialog(w));
}

static void
_DtPrintSetupBoxSelectFileCB(Widget w, XtPointer client, XtPointer call)
{
	Widget	psb = XtParent(w);

	if (PSB_SelectFileProc(psb)) {
		(*PSB_SelectFileProc(psb))(psb, PSB_PrintSetupData(psb));
	}
}

static void
_DtPrintSetupBoxCreateSelectFileButton(DtPrintSetupBoxWidget sb)
{
    BUTTON(PSB_SelectFileButton, "selectFile", "Select File");
    XtManageChild(PSB_SelectFileButton(sb));
    XtRemoveAllCallbacks(PSB_SelectFileButton(sb), XmNactivateCallback);
    XtAddCallback(PSB_SelectFileButton(sb), XmNactivateCallback, _DtPrintSetupBoxSelectFileCB, NULL);
}

/*
 * This gets called when the PrintSetupBox's "Select Printer" button is activated.
 */
static void
_DtPrintSetupBoxSelectPrinterCB(Widget w, XtPointer client, XtPointer call)
{
	Widget	psb = XtParent(w);

	if (PSB_SelectPrinterProc(psb)) {
		(*PSB_SelectPrinterProc(psb))(psb, PSB_PrintSetupData(psb));
	}
}

static void
_DtPrintSetupBoxCreateSelectPrinterButton(DtPrintSetupBoxWidget sb)
{
    BUTTON(PSB_SelectPrinterButton, "selectPrinter", "Select Printer");
    XtManageChild(PSB_SelectPrinterButton(sb));
    XtRemoveAllCallbacks(PSB_SelectPrinterButton(sb), XmNactivateCallback);
    XtAddCallback(PSB_SelectPrinterButton(sb), XmNactivateCallback, _DtPrintSetupBoxSelectPrinterCB, NULL);
}

static void
_DtPrintSetupBoxToggleCB(Widget w, XtPointer client, XtPointer call)
{
	Widget	sb = (Widget)client;
	XmToggleButtonCallbackStruct	*cbp = (XmToggleButtonCallbackStruct *)call;

	if (cbp->set)
		PSB_PrintDestination(sb) = DtPRINT_TO_FILE;
	else
		PSB_PrintDestination(sb) = DtPRINT_TO_PRINTER;

	DEBUGOUT(_LtDebug(__FILE__, sb, "_DtPrintSetupBoxToggleCB(%s)\n",
		cbp->set ? "DtPRINT_TO_FILE" : "DtPRINT_TO_PRINTER"));
}

static void
_DtPrintSetupBoxCreateToggle(DtPrintSetupBoxWidget sb)
{
    Widget	w;
    Arg		al[3];
    int		ac;

    ac = 0;
    XtSetArg(al[ac], XmNorientation, XmHORIZONTAL); ac++;
    XtSetArg(al[ac], XmNradioAlwaysOne, True); ac++;
    PSB_RadioBox(sb) = XmCreateRadioBox((Widget)sb, "radio", al, ac);
    XtManageChild(PSB_RadioBox(sb));

    ac = 0;
    XtSetArg(al[ac],
		XmNset, (PSB_PrintDestination(sb) == DtPRINT_TO_PRINTER) ? XmSET : XmUNSET);
	ac++;
    PSB_PrintToPrinterToggle(sb) = 
	w = XmCreateToggleButtonGadget(PSB_RadioBox(sb), "Print To Printer", al, ac);
    XtManageChild(w);

    ac = 0;
    XtSetArg(al[ac],
		XmNset, (PSB_PrintDestination(sb) == DtPRINT_TO_PRINTER) ? XmUNSET : XmSET);
	ac++;
    PSB_PrintToFileToggle(sb) = 
	w = XmCreateToggleButtonGadget(PSB_RadioBox(sb), "Print To File", al, ac);
    XtManageChild(w);

    XtAddCallback(w, XmNvalueChangedCallback, _DtPrintSetupBoxToggleCB, (XtPointer)sb);
}

static void _DtPrintSetupBoxCreatePrinterName(DtPrintSetupBoxWidget sb)
{
    LABEL(PSB_PrinterNameLabel, "printerName", "Printer Name:");
    XtManageChild(PSB_PrinterNameLabel(sb));

    PSB_PrinterNameTF(sb) = XmCreateTextField((Widget)sb, "printerNameTF", NULL, 0);
    XtManageChild(PSB_PrinterNameTF(sb));
}

/* For DtNsetupProc */
static void
_DtDefaultSetupProc(Widget w, DtPrintSetupData *print_data)
{
	DEBUGOUT(_LtDebug(__FILE__, w, "_DtDefaultSetupProc()\n"));

	if (PSB_PrintSetupMode(w) == DtPRINT_SETUP_XP) {
		/* Call XpNotifyPDM to present the setup dialog provided by the PDM */
	}
}

static void
_DtPrintSetupBoxSetupButtonCB(Widget w, XtPointer client, XtPointer call)
{
	Widget	psb = XtParent(w);

	if (PSB_SetupProc(psb)) {
		(*PSB_SetupProc(psb))(psb, PSB_PrintSetupData(psb));
	}
}

static void _DtPrintSetupBoxCreateSetupButton(DtPrintSetupBoxWidget sb)
{
    BUTTON(PSB_SetupButton, "setup", "Setup ...");
    XtManageChild(PSB_SetupButton(sb));
    XtRemoveAllCallbacks(PSB_SetupButton(sb), XmNactivateCallback);
    XtAddCallback(PSB_SetupButton(sb), XmNactivateCallback, _DtPrintSetupBoxSetupButtonCB, 0);
}

/*
 * I believe that most of the following are for syn_resource export procs
 */
extern void
_DtPrintSetupBoxGetSelectionLabelString(Widget w, int resource_offset,
				       XtArgVal *value)
{
}


extern void
_DtPrintSetupBoxGetOkLabelString(Widget w, int resource_offset,
				XtArgVal *value)
{
}


extern void
_DtPrintSetupBoxGetCancelLabelString(Widget w, int resource_offset,
				    XtArgVal *value)
{
}


extern void
_DtPrintSetupBoxGetHelpLabelString(Widget w, int resource_offset,
				  XtArgVal *value)
{
}


/*
 * Potential rows, as far as I can see from a screendump from the real DtPrint :
 *	1. optional work area
 *	2. separator
 *	3. printer description label, printer description, info button
 *	4. printer name label, printer name textfield, more printers button
 *	5. file name label, file name textfield, select file button
 *	6. print to printer/file radiobox, number of copies spinbox
 *	7. separator
 *	8. buttons
 *	?? separator + optional bottom work area
 *
 * Phases in this function :
 *	1. determine actual number of rows
 *	2. allocate geometry structure
 *	3. fill out all details in the structure
 *	4. be happy that the work's done and return
 */
/*
 * FIX ME layout stinks.
 * FIX ME doesn't currently address DtNminimizeButtons.
 */
extern XmGeoMatrix
_DtPrintSetupBoxGeoMatrixCreate(Widget _w, Widget _from, XtWidgetGeometry *_pref)
{
	XmGeoMatrix		geoSpec;
	XmGeoRowLayout		layoutPtr;
	XmKidGeometry		boxPtr;
	Cardinal		numKids;
	Boolean			newRow;
	int			nrows, i, nextras;
	Widget			*extras;

/*
 * Use macros to recognise a known child.
 */
#define	KNOWN_BUTTON(b, w) 			\
	(  w == PSB_OkButton(b)			\
	|| w == PSB_HelpButton(b)		\
	|| w == BB_CancelButton(b)		\
	|| w == PSB_SetupButton(b)		\
	|| w == PSB_SelectFileButton(b)		\
	|| w == PSB_SelectPrinterButton(b)	\
	|| w == PSB_InfoButton(b)		\
	)
#define	KNOWN_CHILD(b, w) 			\
	(	KNOWN_BUTTON(b, w)		\
	|| w == PSB_WorkAreaTop(b)		\
	|| w == PSB_WorkAreaBottom(b)		\
	|| w == PSB_BSeparator(b)		\
	|| w == PSB_TWASeparator(b)		\
	|| w == PSB_BWASeparator(b)		\
	|| w == PSB_Description(b)		\
	|| w == PSB_DescriptionLabel(b)		\
	|| w == PSB_RadioBox(b)			\
	|| w == PSB_CopiesSpinBox(b)		\
	|| w == PSB_CopiesLabel(b)		\
	|| w == PSB_FileNameLabel(b)		\
	|| w == PSB_FileNameTF(b)		\
	|| w == PSB_FileNameCheckBox(b)		\
	|| w == PSB_NameComboBox(b)		\
	|| w == PSB_PrinterSelectionDialog(b)	\
	|| w == PSB_PrinterNameLabel(b)		\
	|| w == PSB_PrinterNameTF(b)		\
	)

/*
 * Phase 1 - determine actual number of rows.
 */
    numKids = MGR_NumChildren(_w);

    nextras = 0;
    extras = (Widget *)XtMalloc(sizeof(Widget) * numKids);	/* enough */

    for (i=0; i<(int)numKids; i++) {
	Widget w = MGR_Children(_w)[i];

	if (KNOWN_CHILD(_w, w))
		continue;
	extras[nextras++] = w;
    }

    nrows = 0;

    /* note the starting from one.  The zero'th child is the "work area" */
    if (nextras > 0)
    {
	for (i = 1; i < (int)nextras; i++)
	{
	    if (XmIsMenuBar(extras[i]) && XtIsManaged(extras[i]))
		nrows++;
	}
	if (extras[0] && XtIsManaged(extras[0]))
	    nrows++;
    }

    if (PSB_WorkAreaTop(_w) && !PSB_WorkAreaBottom(_w))
	nrows += 2;	/* Both the work area and a separator */
    else if (PSB_WorkAreaBottom(_w) && !PSB_WorkAreaTop(_w))
	nrows += 2;	/* Both the work area and a separator */
    else if (PSB_WorkAreaBottom(_w) && PSB_WorkAreaTop(_w))
	nrows += 4;	/* Both the work areas and separators */

    if ((PSB_DescriptionLabel(_w) && XtIsManaged(PSB_DescriptionLabel(_w)))
		|| (PSB_Description(_w) && XtIsManaged(PSB_Description(_w)))
		|| (PSB_InfoButton(_w) && XtIsManaged(PSB_InfoButton(_w)))) {
	nrows++;
    }

    if ((PSB_PrinterNameLabel(_w) && XtIsManaged(PSB_PrinterNameLabel(_w)))
		|| (PSB_PrinterNameTF(_w) && XtIsManaged(PSB_PrinterNameTF(_w)))
		|| (PSB_SelectPrinterButton(_w) && XtIsManaged(PSB_SelectPrinterButton(_w)))) {
	nrows++;
    }

    if ((PSB_FileNameLabel(_w) && XtIsManaged(PSB_FileNameLabel(_w)))
		|| (PSB_FileNameTF(_w) && XtIsManaged(PSB_FileNameTF(_w)))
		|| (PSB_SelectFileButton(_w) && XtIsManaged(PSB_SelectFileButton(_w)))) {
	nrows++;
    }

    if ((PSB_FileNameCheckBox(_w) && XtIsManaged(PSB_FileNameCheckBox(_w)))
		|| (PSB_CopiesSpinBox(_w) && XtIsManaged(PSB_CopiesSpinBox(_w)))) {
	nrows++;
    }

    if (PSB_BSeparator(_w) && XtIsManaged(PSB_BSeparator(_w))) {
	nrows++;
    }

    if ((BB_CancelButton(_w) && XtIsManaged(BB_CancelButton(_w))) ||
	(PSB_SetupButton(_w) && XtIsManaged(PSB_SetupButton(_w))) ||
	(PSB_OkButton(_w) && XtIsManaged(PSB_OkButton(_w))) ||
	(PSB_HelpButton(_w) && XtIsManaged(PSB_HelpButton(_w))))
    {
	nrows++;
    }

    /* Danny */ nrows++;
    /*
     * Phase 2. allocate geometry structure
     */
    geoSpec = _XmGeoMatrixAlloc(nrows, numKids, 0);
    geoSpec->composite = (Widget)_w;
    geoSpec->instigator = (Widget)_from;
    if (_pref)
    {
	geoSpec->instig_request = *_pref;
    }
    geoSpec->margin_w = BB_MarginWidth(_w) + MGR_ShadowThickness(_w);
    geoSpec->margin_h = BB_MarginHeight(_w) + MGR_ShadowThickness(_w);
    geoSpec->no_geo_request = _DtPrintSetupBoxNoGeoRequest;

    /*
     * Phase 3. fill out all details in the structure
     */
    layoutPtr = &(geoSpec->layouts->row);
    boxPtr = geoSpec->boxes;

    /* First row : top work area */
    newRow = False;
    if (PSB_WorkAreaTop(_w) && XtIsManaged(PSB_WorkAreaTop(_w)) &&
	_XmGeoSetupKid(boxPtr++, PSB_WorkAreaTop(_w)))
    {
	layoutPtr->stretch_height = 1;
	layoutPtr->fill_mode = XmGEO_EXPAND;
#if 0
	layoutPtr->fit_mode = XmGEO_PROPORTIONAL;
#endif
	layoutPtr->even_width = 1;
	layoutPtr->even_height = 1;
	layoutPtr->space_above = BB_MarginHeight(_w);
	layoutPtr->space_between = BB_MarginWidth(_w);

	newRow = True;

	layoutPtr++;
	boxPtr++;
    }

    if (newRow) {
	boxPtr++;
	layoutPtr++;

	/* Have a separator */
	if (PSB_TWASeparator(_w) && XtIsManaged(PSB_TWASeparator(_w)) &&
			_XmGeoSetupKid(boxPtr++, PSB_TWASeparator(_w))) {
		layoutPtr->fix_up = _XmSeparatorFix;

		layoutPtr++;
		boxPtr++;
	}
    }

    /* Row 2 : label, printer description, info button */
    newRow = False;
    if (PSB_DescriptionLabel(_w) && XtIsManaged(PSB_DescriptionLabel(_w)) &&
	_XmGeoSetupKid(boxPtr++, PSB_DescriptionLabel(_w))) {
	
	layoutPtr->fill_mode = XmGEO_CENTER;
	layoutPtr->fit_mode = XmGEO_WRAP;
	layoutPtr->even_width = 6;
	layoutPtr->even_height = 6;
	layoutPtr->space_above = BB_MarginHeight(_w);
	newRow = True;
    }
    if (PSB_Description(_w) && XtIsManaged(PSB_Description(_w)) &&
	_XmGeoSetupKid(boxPtr++, PSB_Description(_w))) {
	
	layoutPtr->fill_mode = XmGEO_CENTER;
	layoutPtr->fit_mode = XmGEO_WRAP;
	layoutPtr->even_width = 6;
	layoutPtr->even_height = 6;
	layoutPtr->space_above = BB_MarginHeight(_w);
	newRow = True;
    }

    if (PSB_InfoButton(_w) && XtIsManaged(PSB_InfoButton(_w)) &&
	_XmGeoSetupKid(boxPtr++, PSB_InfoButton(_w)))
    {
	layoutPtr->fill_mode = XmGEO_CENTER;
	layoutPtr->fit_mode = XmGEO_WRAP;
	layoutPtr->even_width = 6;
	layoutPtr->even_height = 6;
	layoutPtr->space_above = BB_MarginHeight(_w);
	newRow = True;
    }
    if (newRow) {
	newRow = False;
	layoutPtr++;
	boxPtr++;
    }

    /* Row 3 : label, printer name, more-printers button */
    if (PSB_PrinterNameLabel(_w) && XtIsManaged(PSB_PrinterNameLabel(_w)) &&
	_XmGeoSetupKid(boxPtr++, PSB_PrinterNameLabel(_w))) {
	
	layoutPtr->fill_mode = XmGEO_CENTER;
	layoutPtr->fit_mode = XmGEO_WRAP;
	layoutPtr->even_width = 6;
	layoutPtr->even_height = 6;
	layoutPtr->space_above = BB_MarginHeight(_w);
	newRow = True;
    }

    if (PSB_PrinterNameTF(_w) && XtIsManaged(PSB_PrinterNameTF(_w)) &&
	_XmGeoSetupKid(boxPtr++, PSB_PrinterNameTF(_w))) {
	
	layoutPtr->fill_mode = XmGEO_CENTER;
	layoutPtr->fit_mode = XmGEO_WRAP;
	layoutPtr->even_width = 6;
	layoutPtr->even_height = 6;
	layoutPtr->space_above = BB_MarginHeight(_w);
	newRow = True;
    }

    if (PSB_SelectPrinterButton(_w) && XtIsManaged(PSB_SelectPrinterButton(_w)) &&
	_XmGeoSetupKid(boxPtr++, PSB_SelectPrinterButton(_w)))
    {
	layoutPtr->fill_mode = XmGEO_CENTER;
	layoutPtr->fit_mode = XmGEO_WRAP;
	layoutPtr->even_width = 6;
	layoutPtr->even_height = 6;
	layoutPtr->space_above = BB_MarginHeight(_w);
	newRow = True;
    }

    if (newRow) {
	layoutPtr++;
	boxPtr++;
    }

    /* Row 4 */
    newRow = False;
    if (PSB_FileNameCheckBox(_w) && XtIsManaged(PSB_FileNameCheckBox(_w)) &&
	_XmGeoSetupKid(boxPtr++, PSB_FileNameCheckBox(_w))) {
	
	layoutPtr->fill_mode = XmGEO_CENTER;
	layoutPtr->fit_mode = XmGEO_WRAP;
	layoutPtr->even_width = 6;
	layoutPtr->even_height = 6;
	layoutPtr->space_above = BB_MarginHeight(_w);
	newRow = True;
    }

    if (PSB_SelectFileButton(_w) && XtIsManaged(PSB_SelectFileButton(_w)) &&
	_XmGeoSetupKid(boxPtr++, PSB_SelectFileButton(_w)))
    {
	layoutPtr->fill_mode = XmGEO_CENTER;
	layoutPtr->fit_mode = XmGEO_WRAP;
	layoutPtr->even_width = 6;
	layoutPtr->even_height = 6;
	layoutPtr->space_above = BB_MarginHeight(_w);
	newRow = True;
    }

    if (newRow) {
	layoutPtr++;
	boxPtr++;
    }

    /* Row 5 : Radio box, Copies Label, Copies Spinbox */
    newRow = False;

    if (PSB_RadioBox(_w) && XtIsManaged(PSB_RadioBox(_w)) &&
	_XmGeoSetupKid(boxPtr++, PSB_RadioBox(_w))) {
	
	layoutPtr->fill_mode = XmGEO_CENTER;
	layoutPtr->fit_mode = XmGEO_WRAP;
	layoutPtr->even_width = 6;
	layoutPtr->even_height = 6;
	layoutPtr->space_above = BB_MarginHeight(_w);
	newRow = True;
    }
    if (PSB_CopiesLabel(_w) && XtIsManaged(PSB_CopiesLabel(_w)) &&
	_XmGeoSetupKid(boxPtr++, PSB_CopiesLabel(_w))) {
	
	layoutPtr->fill_mode = XmGEO_CENTER;
	layoutPtr->fit_mode = XmGEO_WRAP;
	layoutPtr->even_width = 6;
	layoutPtr->even_height = 6;
	layoutPtr->space_above = BB_MarginHeight(_w);
	newRow = True;
    }
    if (PSB_CopiesSpinBox(_w) && XtIsManaged(PSB_CopiesSpinBox(_w)) &&
	_XmGeoSetupKid(boxPtr++, PSB_CopiesSpinBox(_w))) {
	
	layoutPtr->fill_mode = XmGEO_CENTER;
	layoutPtr->fit_mode = XmGEO_WRAP;
	layoutPtr->even_width = 6;
	layoutPtr->even_height = 6;
	layoutPtr->space_above = BB_MarginHeight(_w);
	newRow = True;
    }

    if (newRow) {
	layoutPtr++;
	boxPtr++;
    }

    if (PSB_BSeparator(_w) && XtIsManaged(PSB_BSeparator(_w)) &&
	_XmGeoSetupKid(boxPtr++, PSB_BSeparator(_w)))
    {
	layoutPtr->fix_up = _XmSeparatorFix;
	layoutPtr->space_above = BB_MarginHeight(_w);
	boxPtr++;
	layoutPtr++;
    }

    newRow = False;
    if (PSB_WorkAreaBottom(_w) && XtIsManaged(PSB_WorkAreaBottom(_w)) &&
	_XmGeoSetupKid(boxPtr++, PSB_WorkAreaBottom(_w)))
    {
	layoutPtr->stretch_height = 1;
	layoutPtr->fill_mode = XmGEO_EXPAND;
	layoutPtr->even_width = 1;
	layoutPtr->even_height = 1;
	layoutPtr->space_above = BB_MarginHeight(_w);
	layoutPtr->space_between = BB_MarginWidth(_w);

	newRow = True;

	layoutPtr++;
	boxPtr++;
    }

    if (newRow) {
	layoutPtr++;

	/* Have a separator */
	if (PSB_BWASeparator(_w) && XtIsManaged(PSB_BWASeparator(_w)) &&
			_XmGeoSetupKid(boxPtr++, PSB_BWASeparator(_w))) {
		layoutPtr->fix_up = _XmSeparatorFix;

		layoutPtr++;
		boxPtr++;
	}
    }

    /* Buttons row */
    newRow = False;
    if (PSB_OkButton(_w) && XtIsManaged(PSB_OkButton(_w)) &&
	_XmGeoSetupKid(boxPtr++, PSB_OkButton(_w)))
    {
	layoutPtr->fill_mode = XmGEO_CENTER;
	layoutPtr->fit_mode = XmGEO_WRAP;
	layoutPtr->even_width = 6;
	layoutPtr->even_height = 6;
	layoutPtr->space_above = BB_MarginHeight(_w);
	newRow = True;
    }

    /*
     * Additional buttons not automatically created by the widget
     * are added here.
     */
#if 0
    for (i = 1; i < nextras; i++) {
	if (extras[i] && XtIsManaged(extras[i]) &&
	    (XmIsPushButton(extras[i]) || XmIsPushButtonGadget(extras[i])) &&
	    _XmGeoSetupKid(boxPtr++, extras[i]))
	{
	    _XmBulletinBoardSetDefaultShadow(extras[i]);
	    layoutPtr->fill_mode = XmGEO_CENTER;
	    layoutPtr->fit_mode = XmGEO_WRAP;
	    layoutPtr->even_width = 6;
	    layoutPtr->even_height = 6;
	    layoutPtr->space_above = BB_MarginHeight(_w);
	    newRow = True;
	}
    }
#endif

    if (PSB_SetupButton(_w) && XtIsManaged(PSB_SetupButton(_w)) &&
	_XmGeoSetupKid(boxPtr++, PSB_SetupButton(_w)))
    {
	layoutPtr->fill_mode = XmGEO_CENTER;
	layoutPtr->fit_mode = XmGEO_WRAP;
	layoutPtr->even_width = 6;
	layoutPtr->even_height = 6;
	layoutPtr->space_above = BB_MarginHeight(_w);
	newRow = True;
    }

    if (BB_CancelButton(_w) && XtIsManaged(BB_CancelButton(_w)) &&
	_XmGeoSetupKid(boxPtr++, BB_CancelButton(_w)))
    {
	layoutPtr->fill_mode = XmGEO_CENTER;
	layoutPtr->fit_mode = XmGEO_WRAP;
	layoutPtr->even_width = 6;
	layoutPtr->even_height = 6;
	layoutPtr->space_above = BB_MarginHeight(_w);
	newRow = True;
    }

    if (PSB_HelpButton(_w) && XtIsManaged(PSB_HelpButton(_w)) &&
	_XmGeoSetupKid(boxPtr++, PSB_HelpButton(_w)))
    {
	layoutPtr->fill_mode = XmGEO_CENTER;
	layoutPtr->fit_mode = XmGEO_WRAP;
	layoutPtr->even_width = 6;
	layoutPtr->even_height = 6;
	layoutPtr->space_above = BB_MarginHeight(_w);
	newRow = True;
    }

    if (newRow) {
	layoutPtr++;
	boxPtr++;
	newRow = False;
    }

    layoutPtr++;
    boxPtr++;

/*
 * Phase 4 : be happy that the work's done and return
 */
    layoutPtr->space_above = 0;	/* BB_MarginHeight(_w); */
    layoutPtr->end = True;

    if (nextras) {
	XtFree((char *)extras);
    }

    return (geoSpec);
}


extern Boolean
_DtPrintSetupBoxNoGeoRequest(XmGeoMatrix _geoSpec)
{
    if (BB_InSetValues(_geoSpec->composite) &&
	(XtClass(_geoSpec->composite) == xmSelectionBoxWidgetClass ||
	 XtClass(_geoSpec->composite) == xmCommandWidgetClass))
    {
	return True;
    }

    return False;
}


extern Widget
DtCreatePrintSetupBox(Widget parent,
		const String name,
		ArgList arglist,
		Cardinal argcount)
{
    return XtCreateWidget(name, dtPrintSetupBoxWidgetClass, parent,
			  arglist, argcount);
}


extern Widget
DtCreatePrintSetupDialog(Widget parent, char *name,
			Arg *argList, Cardinal argcount)
{
    Widget d, r;
    char *s;
    Arg *al = (Arg *)XtCalloc(argcount + 1, sizeof(Arg));
    int ac, i;

    s = XtMalloc(strlen(name) + 7);
    strcpy(s, name);
    strcat(s, "_popup");

    ac = 0;
    XtSetArg(al[ac], XmNallowShellResize, True); ac++;
    for (i = 0; i < (int)argcount; i++)
    {
	XtSetArg(al[ac], argList[i].name, argList[i].value); ac++;
    }

    d = XmCreateDialogShell(parent, s, al, ac);
    XtFree(s);

    r = XtCreateWidget(name, dtPrintSetupBoxWidgetClass, d, al, ac);
    XtFree((char *)al);
    return r;
}

/*      
 * Keep track of button children
 */
static void
InsertChild(Widget w)
{
	Widget	sb = XtParent(w);

#define superclass      (&xmBulletinBoardClassRec)

	(*superclass->composite_class.insert_child) (w);

	/*
	 * Work area children have strict rules to follow
	 */
	if ((! XmIsPushButton(w)) && (! XmIsPushButtonGadget(w))) {
		if (PSB_WorkAreaLocation(sb) == DtWORK_AREA_TOP)
			PSB_WorkAreaTop(sb) = w;
		else if (PSB_WorkAreaLocation(sb) == DtWORK_AREA_BOTTOM)
			PSB_WorkAreaBottom(sb) = w;
		else if (PSB_WorkAreaTop(sb) == NULL)
			PSB_WorkAreaTop(sb) = w;
		else
			PSB_WorkAreaBottom(sb) = w;
	}

#undef  superclass
}

static void
DeleteChild(Widget w)
{
	Widget	sb = XtParent(w);

#define superclass      (&xmBulletinBoardClassRec)
    (*superclass->composite_class.delete_child) (w);
#undef  superclass

	if (w == PSB_HelpButton(sb))
		PSB_HelpButton(sb) = NULL;
	if (w == PSB_OkButton(sb))
		PSB_OkButton(sb) = NULL;
	if (w == PSB_BSeparator(sb))
		PSB_BSeparator(sb) = NULL;
}


/*
 * These functions are the default ones
 */
/*
 * For DtNprinterInfoProc
 *
 * Get printer information.
 * Also called from default DtNselectPrinterProc.
 * If DtNprintSetupMode == DtPRINT_SETUP_XP, pop up a dialog showing the info.
 * Don't update the calling widget.
 */
void
_DtDefaultPrinterInfoProc(Widget w, DtPrintSetupData *print_data)
{
	XPPrinterList		plist;
	int			count, ac;
	Arg			al[5];
	XmString		xms;
	Widget			mb;

	DEBUGOUT(_LtDebug(__FILE__, w, "_DtDefaultPrinterInfoProc\n"));

	if (PSB_PrintSetupMode(w) != DtPRINT_SETUP_XP)
		return;

	if (print_data == 0 || print_data->print_display == 0 || print_data->printer_name == 0)
		return;

	if (print_data->dest_info) {
		XtFree(print_data->dest_info);
		print_data->dest_info = 0;
	}

	/* Must we look at print_data->destination (DtPRINT_TO_PRINTER, DtPRINT_TO_FILE) ? */

	/* Figure out the information */
	plist = XpGetPrinterList(print_data->print_display, print_data->printer_name, &count);
	if (plist == 0 || count <= 0)
		return;

	/* What to do with more than one entry in the list ? */
	print_data->dest_info = XtNewString(plist[0].desc);

	/* Show it */
	ac = 0;
	xms = XmStringCreateSimple(print_data->dest_info);
	XtSetArg(al[ac], XmNmessageString, xms); ac++;
	mb = XmCreateMessageDialog(XtParent(w), "printer info", al, ac);
	XmStringFree(xms);

#if 0
	/*
	 * FIX ME this creates a performance problem somewhere.
	 *
	 * The MessageDialog remains empty for several seconds,
	 * and then suddenly appears normal without user interaction.
	 */
	XtDestroyWidget(XmMessageBoxGetChild(mb, XmDIALOG_HELP_BUTTON));
	XtDestroyWidget(XmMessageBoxGetChild(mb, XmDIALOG_CANCEL_BUTTON));
#endif
	XpFreePrinterList(plist);
	XtManageChild(mb);
}

static void
_DtSelectPrinterOkCB(Widget w, XtPointer client, XtPointer call)
{
	Widget	d = (Widget)client;
	char	*s;
	XmSelectionBoxCallbackStruct	*cbp = (XmSelectionBoxCallbackStruct *)call;

	if (! XmStringGetLtoR(cbp->value, XmFONTLIST_DEFAULT_TAG, &s)) {
		/* Can't convert it into a string */
		/* Note: we're not popping down the dialog ! */
		return;
	}

	/* FIX ME !!! Shouldn't I just set the printerName textfield, instead of the
	 * widget resource ?
	 * Need to back out changes with a CANCEL button, don't I ?
	 */
	DEBUGOUT(_LtDebug(__FILE__, d, "_DtSelectPrinterOkCB: printer '%s'\n", s));

	XtVaSetValues(d,
			DtNprinterName,		s,
			DtNprintDestination,	DtPRINT_TO_PRINTER,
		NULL);
	XtFree(s);

	XtUnmanageChild(w);
}

/*
 * Used both for the printer and the file selection dialog's cancel action.
 */
static void
_DtSelectPrinterCancelCB(Widget w, XtPointer client, XtPointer call)
{
	/* Pop down */
	XtUnmanageChild(w);
}

/* For DtNselectPrinterProc */
/*
 * This is called in response to the activation of the Select Printer button.
 * If DtNprintSetupMode is DtPRINT_SETUP_XP, pops up a dialog to ask the user
 * to select a printer.
 * If the user presses cancel, nothing is updated; otherwise DtNprinterName
 * is set.
 */
static XtEnum
_DtDefaultSelectPrinterProc(Widget w, DtPrintSetupData *print_data)
{
	Widget		d, b;
	Arg		al[5];
	int		ac = 0;

	DEBUGOUT(_LtDebug(__FILE__, w, "_DtDefaultSelectPrinterProc()\n"));

	if (PSB_PrintSetupMode(w) != DtPRINT_SETUP_XP)
		return 0;	/* FIX ME return what ?? */

	if (! PSB_PrinterSelectionDialog(w)) {
		ac = 0;
		XtSetArg(al[ac], XmNdeleteResponse, XmUNMAP); ac++;
		XtSetArg(al[ac], XmNautoUnmanage, False); ac++;
		PSB_PrinterSelectionDialog(w) = d =
			XmCreateSelectionDialog(XtParent(w), "select printer", al, ac);
		XtDestroyWidget(XmSelectionBoxGetChild(d, XmDIALOG_APPLY_BUTTON));
		XtAddCallback(d, XmNokCallback, _DtSelectPrinterOkCB, (XtPointer)w);
		XtAddCallback(d, XmNcancelCallback, _DtSelectPrinterCancelCB, (XtPointer)w);

		b = XmCreatePushButtonGadget(d, "Info ...", NULL, 0);
		XtManageChild(b);
		XtAddCallback(b, XmNactivateCallback, _DtPrintSetupBoxInfoButtonCB, w);
	}
	_DtPsbUpdatePrinterList(w);
	XtManageChild(PSB_PrinterSelectionDialog(w));

	return 0;	/* FIX ME return what ?? */
}


/* For DtNverifyPrinterProc */
/*
 * Verify the printer name prior to any operation on it.
 */
static void
_DtDefaultVerifyPrinterProc(Widget w, DtPrintSetupData *print_data)
{
	DEBUGOUT(_LtDebug(__FILE__, w, "_DtDefaultVerifyPrinterProc()\n"));
}


static void
_DtPsbDefaultPrinter(Widget w, Display *d)
{
	XPPrinterList	plist;
	int		count, a = 0, b = 0;

	if (PSB_PrinterName(w))
		return;

	if (! XpQueryExtension(d, &a, &b)) {
		/* This display doesn't support the X Printing Extension */
		return;
	}

	plist = XpGetPrinterList(d, "", &count);
	if (count == 0)
		return;

	XmTextFieldSetString(PSB_PrinterNameTF(w), plist[0].name);
	PSB_PrinterName(w) = XtNewString(plist[0].name);

	DEBUGOUT(_LtDebug(__FILE__, w, "DtPSB: default printer %s\n", PSB_PrinterName(w)));

	XpFreePrinterList(plist);
}

/*
 * Initialize default printer
 * Algorithm is in the manual page.
 */
static void
_DtPsbSetDefaultPrinter(Widget w)
{
	char	*p, *q, *z, *d, *list;
	String	argv[] = { "this" };
	int	argc = 1;
	Display	*pdpy;

	if (PSB_PrinterName(w))
		XtFree(PSB_PrinterName(w));
	PSB_PrinterName(w) = NULL;

	/* Lots of info in PSB_PrintSetupData(w) */
	if (PSB_PrintSetupData(w)->print_display)
		_DtPsbAddToPrinterList(w, PSB_PrintSetupData(w)->print_display);

	/* Check this display */
	_DtPsbDefaultPrinter(w, XtDisplay(w));

	/* Check the list of print servers */
	list = getenv("XPSERVERLIST");
	if (list) {
		DEBUGOUT(_LtDebug(__FILE__, w, "Try XpServerlist [%s]\n", list));
		z = XtMalloc(strlen(list)+5);
		/* Cut the list into pieces */
		for (p=list; *p; p++) {
			while (*p && isspace(*p)) p++;
			if (*p == '\0')
				break;
			for (q=p, d=z; *q && !isspace(*q); q++)
				*(d++) = *q;
			*d = '\0';

			/* Now z contains a piece of the list */
			if (! strchr(z, ':'))
				strcat(z, ":0");
			DEBUGOUT(_LtDebug(__FILE__, w, "_DtPsbUpdatePrinterList try to open display <%s>\n", z));
			pdpy = XtOpenDisplay(XtWidgetToApplicationContext(w),
				z, "", "", NULL, 0, &argc, argv);
			if (pdpy) {
				_DtPsbDefaultPrinter(w, pdpy);
				XtCloseDisplay(pdpy);
			}

			if (*q)
				p=q;
			else
				break;
		}
		XtFree(z);
	}
}

static void
_DtPsbEmptyPrinterList(Widget w)
{
	Widget	l;

	if (! PSB_PrinterSelectionDialog(w))	/* This should not happen !! */
		return;
	l = XmSelectionBoxGetChild(PSB_PrinterSelectionDialog(w), XmDIALOG_LIST);
	if (! l)
		return;

	XmListDeleteAllItems(l);
}

/*
 * Add printers to the selection box.
 */
static void
_DtPsbAddToPrinterList(Widget w, Display *d)
{
	XPPrinterList	plist;
	int		count, i, a = 0, b = 0;
	XmString	xms;
	Widget		l;

	DEBUGOUT(_LtDebug(__FILE__, w, "_DtPsbAddToPrinterList()\n"));

	if (! PSB_PrinterSelectionDialog(w))	/* This should not happen !! */
		return;

	l = XmSelectionBoxGetChild(PSB_PrinterSelectionDialog(w), XmDIALOG_LIST);
	if (! l)
		return;

	if (! XpQueryExtension(d, &a, &b)) {
		/* This display doesn't support the X Printing Extension */
		return;
	}

	plist = XpGetPrinterList(d, "", &count);
	for (i=0; i<count; i++) {
		xms = XmStringCreateSimple(plist[i].name);
		if (! XmListItemExists(l, xms))				/* Remove doubles */
			XmListAddItemsUnselected(l, &xms, 1, 0);	/* Add at the end */
	}

	XpFreePrinterList(plist);
}

/*
 * Get a list of printers through libXp and put it in the combobox.
 *
 * FIX ME ??? Should we remove doubles ?
 *	An example of how they could get there is that some (print) Display gets
 *	in the list twice.
 */
static void
_DtPsbUpdatePrinterList(Widget w)
{
	char	*p, *q, *z, *d, *list;
	String	argv[] = { "this" };
	int	argc = 1;
	Display	*pdpy;

	if (! PSB_PrinterSelectionDialog(w))
		return;

	_DtPsbEmptyPrinterList(w);

	/* Lots of info in PSB_PrintSetupData(w) */
	if (PSB_PrintSetupData(w)->print_display)
		_DtPsbAddToPrinterList(w, PSB_PrintSetupData(w)->print_display);

	/* Check this display */
	_DtPsbAddToPrinterList(w, XtDisplay(w));

	/* Check the list of print servers */
	list = getenv("XPSERVERLIST");
	if (list) {
		DEBUGOUT(_LtDebug(__FILE__, w, "Try XpServerlist [%s]\n", list));
		z = XtMalloc(strlen(list)+5);
		/* Cut the list into pieces */
		for (p=list; *p; p++) {
			while (*p && isspace(*p)) p++;
			if (*p == '\0')
				break;
			for (q=p, d=z; *q && !isspace(*q); q++)
				*(d++) = *q;
			*d = '\0';

			/* Now z contains a piece of the list */
			if (! strchr(z, ':'))
				strcat(z, ":0");
			DEBUGOUT(_LtDebug(__FILE__, w, "_DtPsbUpdatePrinterList try to open display <%s>\n", z));
			pdpy = XtOpenDisplay(XtWidgetToApplicationContext(w),
				z, "", "", NULL, 0, &argc, argv);
			if (pdpy) {
				_DtPsbAddToPrinterList(w, pdpy);
				XtCloseDisplay(pdpy);
			}

			if (*q)
				p=q;
			else
				break;
		}
		XtFree(z);
	}
}
