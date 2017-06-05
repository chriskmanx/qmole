/**
 *
 * $Header: /cvsroot/lesstif/lesstif/lib/Dt/Print.h,v 1.11 2001/09/25 17:41:58 amai Exp $
 *
 * Copyright © 2000, 2001 Free Software Foundation, Inc.
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

#ifndef	_DT_PRINT_H_
#define	_DT_PRINT_H_

#include <X11/Intrinsic.h>
#include <X11/extensions/Print.h>

#ifdef __cplusplus
extern "C" {
#endif

/* FIX ME this can be optimised for better memory use */

#define	DtNcancelCallback		"cancelCallback"
#define	DtCCancelCallback		"CancelCallback"
#define	DtNclosePrintDisplayCallback	"closePrintDisplayCallback"
#define	DtCClosePrintDisplayCallback	"ClosePrintDisplayCallback"
#define	DtNcopies			"copies"
#define	DtCCopies			"Copies"
#define	DtNdescription			"description"
#define	DtCDescription			"Description"
#define	DtNfileName			"fileName"
#define	DtCPrintToFileName		"FileName"
#define	DtNgetAttributesCallback	"getAttributesCallback"
#define	DtCGetAttributesCallback	"GetAttributesCallback"
#define	DtNminimizeButtons		"minimizeButtons"
#define	DtCMinimizeButtons		"MinimizeButtons"
#define	DtNprintCallback		"printCallback"
#define	DtCPrintCallback		"PrintCallback"
#define	DtNprintDestination		"printDestination"
#define	DtCPrintDestination		"PrintDestination"
#define	DtNprinterInfoProc		"printerInfoProc"
#define	DtCPrinterInfoProc		"PrinterInfoProc"
#define	DtNprinterName			"printerName"
#define	DtCPrinterName			"PrinterName"
#define	DtNprintSetupMode		"printSetupMode"
#define	DtCPrintSetupMode		"PrintSetupMode"
#define	DtNselectFileProc		"selectFileProc"
#define	DtCSelectFileProc		"SelectFileProc"
#define	DtNselectPrinterProc		"selectPrinterProc"
#define	DtCSelectPrinterProc		"SelectPrinterProc"
#define	DtNsetAttributesCallback	"setAttributesCallback"
#define	DtCSetAttributesCallback	"SetAttributesCallback"
#define	DtNsetupProc			"setupProc"
#define	DtCSetupProc			"SetupProc"
#define	DtNverifyPrinterProc		"verifyPrinterProc"
#define	DtCVerifyPrinterProc		"VerifyPrinterProc"
#define	DtNworkAreaLocation		"workAreaLocation"
#define	DtCWorkAreaLocation		"WorkAreaLocation"

#define	DtNsetupCallback		"setupCallback"
#define	DtCSetupCallback		"SetupCallback"

#define	DmRPrintSetupProc		"printSetupProc"

typedef struct {
	String		printer_name;
	Display		*print_display;
	XPContext	print_context;
	XtEnum		destination;
	String		dest_info;
} DtPrintSetupData;

typedef struct {
	int			reason;
	XEvent			*event;
	DtPrintSetupData	*print_data;
} DtPrintSetupCallbackStruct;

enum {
	DtPRINT_CR_NONE,
	DtPRINT_CR_CANCEL,
	DtPRINT_CR_CLOSE_PRINT_DISPLAY,
	DtPRINT_CR_GET_ATTRIBUTES,
	DtPRINT_CR_GET_STATIC_ATTRIBUTES,
	DtPRINT_CR_PRINT,
	DtPRINT_CR_SET_ATTRIBUTES,
	DtPRINT_CR_SETUP
};

enum {
	DtSHORT_NAME,
	DtLONG_NAME,
	DtMEDIUM_NAME
};

enum {
	DtPRINT_SUCCESS,
	DtPRINT_FAILURE,
	DtPRINT_BAD_PARM,
	DtPRINT_INVALID_DISPLAY,
	DtPRINT_NOT_XP_DISPLAY,
	DtPRINT_NO_CONNECTION,
	DtPRINT_NO_DEFAULT,
	DtPRINT_NO_DEFAULT_DISPLAY,
	DtPRINT_NO_PRINTER,
	DtPRINT_PRINTER_MISSING
};

typedef enum {
	DtPRINT_CLOSE_CONNECTION,
	DtPRINT_RELEASE_CONNECTION
} DtPrintResetConnectionMode;

enum {
	DtPRINT_NO_DESTINATION,
	DtPRINT_TO_PRINTER,
	DtPRINT_TO_FILE
};

enum {
	DtPRINT_SETUP_XP,
	DtPRINT_SETUP_PLAIN
};

enum {
	DtWORK_AREA_NONE,
	DtWORK_AREA_BOTTOM,
	DtWORK_AREA_TOP,
	DtWORK_AREA_TOP_AND_BOTTOM
};
/*
 * The widget
 */
extern WidgetClass dtPrintSetupBoxWidgetClass;
typedef struct _DtPrintSetupBoxRec *DtPrintSetupBoxWidget;
typedef struct _DtPrintSetupBoxClassRec *DtPrintSetupBoxWidgetClass;

typedef XtEnum (*DtPrintSetupProc)(Widget wid, DtPrintSetupData *print_data);

Widget DtCreatePrintSetupDialog(Widget parent, const String name,
			ArgList arglist, Cardinal argcount);
Widget DtCreatePrintSetupBox(Widget parent, const String name,
			ArgList arglist, Cardinal argcount);
DtPrintSetupData *DtPrintCopySetupData(DtPrintSetupData *target, const DtPrintSetupData *source);
XtEnum DtPrintFillSetupData(Widget wid, DtPrintSetupData *print_data);
void DtPrintFreeSetupData(DtPrintSetupData *target);
XtEnum DtPrintResetConnection(Widget wid, DtPrintResetConnectionMode mode);

#ifdef __cplusplus
}
#endif

#endif /* _DT_PRINT_H_ */
