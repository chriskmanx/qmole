/**
 *
 * $Header: /cvsroot/lesstif/lesstif/lib/Dt/DtPrintSetupBoxP.h,v 1.20 2002/04/13 11:08:48 amai Exp $
 * 
 * Copyright © 2000, 2001, 2002 LessTif Development Team
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

#ifndef _DT_PRINT_SETUP_BOX_P_H_
#define _DT_PRINT_SETUP_BOX_P_H_

#include <Xm/BulletinBP.h>
#include <Dt/Print.h>

#ifdef __cplusplus
extern "C" {
#endif


/* Define the DtPrintSetupBox instance part */
typedef struct {
	/* DtPrintSetupBox */
	XtCallbackList		close_print_display_callback;
	int			copies;
	XmString		copies_label_string;
	XmString		description;
	String			file_name;
	XtCallbackList		get_attributes_callback;
	Boolean			minimize_buttons;
	XtCallbackList		print_callback;
	XtEnum			print_destination;
	DtPrintSetupProc	printer_info_proc;
	String			printer_name;
	XtEnum			print_setup_mode;
	DtPrintSetupProc	select_file_proc;
	DtPrintSetupProc	select_printer_proc;
	XtCallbackList		set_attributes_callback;
	DtPrintSetupProc	setup_proc;
	DtPrintSetupProc	verify_printer_proc;
	XtEnum			work_area_location;

	Widget			bottom_work_area_separator;
	Widget			top_work_area_separator;
	Widget			button_separator;
	Widget			copies_spinbox, copies_spinbox_tf;
	Widget			copies_label;
	Widget			description_widget;		/* FIX ME not in the docs */
	Widget			description_label;
	Widget			file_name_tf;
	Widget			file_name_checkbox;
	Widget			file_name_label;		/* FIX ME */
	Widget			info_button;
	Widget			name_combobox;
	XmString		name_label_string;
	Widget			name_label;
	Widget			work_area_top, work_area_bottom;

	DtPrintSetupData	*print_setup_data;
	Widget			printer_selection_dialog,
				file_selection_dialog;

	XmString		setup_label_string;
	Widget			setup_button;
	XmString		select_file_label_string;
	Widget			select_file_button;
	XmString		select_printer_label_string;
	Widget			select_printer_button;
	Widget			print_to_printer_toggle,
				print_to_file_toggle;
	Widget			ok_button;
	XmString		ok_label_string;
	Widget			apply_button;
	XmString		apply_label_string;
	XmString		cancel_label_string;
	Widget			help_button;
	XmString		help_label_string;
	XtCallbackList		ok_callback;
	XtCallbackList		apply_callback;
	XtCallbackList		cancel_callback;
	XtCallbackList		setup_callback;
	Widget			printer_name_label,
				printer_name_tf,
				radio;

} DtPrintSetupBoxPart;

#define	PSB_MinimizeButtons(w) \
        (((DtPrintSetupBoxWidget) (w))->printsetup_box.minimize_buttons)
#define	PSB_RadioBox(w) \
        (((DtPrintSetupBoxWidget) (w))->printsetup_box.radio)
#define	PSB_BWASeparator(w) \
        (((DtPrintSetupBoxWidget) (w))->printsetup_box.bottom_work_area_separator)
#define	PSB_TWASeparator(w) \
        (((DtPrintSetupBoxWidget) (w))->printsetup_box.top_work_area_separator)
#define	PSB_BSeparator(w) \
        (((DtPrintSetupBoxWidget) (w))->printsetup_box.button_separator)
#define	PSB_WorkAreaTop(w) \
        (((DtPrintSetupBoxWidget) (w))->printsetup_box.work_area_top)
#define	PSB_WorkAreaBottom(w) \
        (((DtPrintSetupBoxWidget) (w))->printsetup_box.work_area_bottom)
#define	PSB_WorkAreaLocation(w) \
        (((DtPrintSetupBoxWidget) (w))->printsetup_box.work_area_location)
#define	PSB_Copies(w) \
        (((DtPrintSetupBoxWidget) (w))->printsetup_box.copies)
#define	PSB_CopiesSpinBox(w) \
        (((DtPrintSetupBoxWidget) (w))->printsetup_box.copies_spinbox)
#define	PSB_CopiesSpinBoxTF(w) \
        (((DtPrintSetupBoxWidget) (w))->printsetup_box.copies_spinbox_tf)
#define	PSB_CopiesLabel(w) \
        (((DtPrintSetupBoxWidget) (w))->printsetup_box.copies_label)
#define	PSB_CopiesLabelString(w) \
        (((DtPrintSetupBoxWidget) (w))->printsetup_box.copies_label_string)
#define	PSB_Description(w) \
        (((DtPrintSetupBoxWidget) (w))->printsetup_box.description_widget)
#define	PSB_DescriptionLabel(w) \
        (((DtPrintSetupBoxWidget) (w))->printsetup_box.description_label)
#define	PSB_FileName(w) \
        (((DtPrintSetupBoxWidget) (w))->printsetup_box.file_name)
#define	PSB_FileNameLabel(w) \
        (((DtPrintSetupBoxWidget) (w))->printsetup_box.file_name_label)
#define	PSB_FileNameTF(w) \
        (((DtPrintSetupBoxWidget) (w))->printsetup_box.file_name_tf)
#define	PSB_FileNameCheckBox(w) \
        (((DtPrintSetupBoxWidget) (w))->printsetup_box.file_name_checkbox)
#define	PSB_InfoButton(w) \
        (((DtPrintSetupBoxWidget) (w))->printsetup_box.info_button)
#define	PSB_NameComboBox(w) \
        (((DtPrintSetupBoxWidget) (w))->printsetup_box.name_combobox)
#define	PSB_PrintSetupMode(w) \
        (((DtPrintSetupBoxWidget) (w))->printsetup_box.print_setup_mode)
#define	PSB_SelectPrinterProc(w) \
        (((DtPrintSetupBoxWidget) (w))->printsetup_box.select_printer_proc)
#define	PSB_SelectFileProc(w) \
        (((DtPrintSetupBoxWidget) (w))->printsetup_box.select_file_proc)
#define	PSB_PrinterInfoProc(w) \
        (((DtPrintSetupBoxWidget) (w))->printsetup_box.printer_info_proc)
#define	PSB_SetupProc(w) \
        (((DtPrintSetupBoxWidget) (w))->printsetup_box.setup_proc)
#define	PSB_PrintSetupData(w) \
        (((DtPrintSetupBoxWidget) (w))->printsetup_box.print_setup_data)
#define	PSB_PrinterSelectionDialog(w) \
        (((DtPrintSetupBoxWidget) (w))->printsetup_box.printer_selection_dialog)
#define	PSB_FileSelectionDialog(w) \
        (((DtPrintSetupBoxWidget) (w))->printsetup_box.file_selection_dialog)
#define	PSB_SelectFileButton(w) \
        (((DtPrintSetupBoxWidget) (w))->printsetup_box.select_file_button)
#define	PSB_SelectFileLabelString(w) \
        (((DtPrintSetupBoxWidget) (w))->printsetup_box.select_file_label_string)
#define	PSB_SelectPrinterButton(w) \
        (((DtPrintSetupBoxWidget) (w))->printsetup_box.select_printer_button)
#define	PSB_SelectPrinterLabelString(w) \
        (((DtPrintSetupBoxWidget) (w))->printsetup_box.select_printer_label_string)
#define	PSB_SetupButton(w) \
        (((DtPrintSetupBoxWidget) (w))->printsetup_box.setup_button)
#define	PSB_SetupLabelString(w) \
        (((DtPrintSetupBoxWidget) (w))->printsetup_box.setup_label_string)
#define	PSB_PrintCallback(w) \
        (((DtPrintSetupBoxWidget) (w))->printsetup_box.print_callback)
#define	PSB_PrinterName(w) \
        (((DtPrintSetupBoxWidget) (w))->printsetup_box.printer_name)
#define	PSB_PrinterNameLabel(w) \
        (((DtPrintSetupBoxWidget) (w))->printsetup_box.printer_name_label)
#define	PSB_PrinterNameTF(w) \
        (((DtPrintSetupBoxWidget) (w))->printsetup_box.printer_name_tf)
#define	PSB_ClosePrintDisplayCallback(w) \
        (((DtPrintSetupBoxWidget) (w))->printsetup_box.close_print_display_callback)

#define	PSB_HelpButton(w) \
        (((DtPrintSetupBoxWidget) (w))->printsetup_box.help_button)
#define	PSB_OkButton(w) \
        (((DtPrintSetupBoxWidget) (w))->printsetup_box.ok_button)
#define	PSB_OkLabelString(w) \
        (((DtPrintSetupBoxWidget) (w))->printsetup_box.ok_label_string)
#define	PSB_CancelLabelString(w) \
        (((DtPrintSetupBoxWidget) (w))->printsetup_box.cancel_label_string)
#define	PSB_HelpLabelString(w) \
        (((DtPrintSetupBoxWidget) (w))->printsetup_box.help_label_string)

#define	PSB_PrintToPrinterToggle(w) \
        (((DtPrintSetupBoxWidget) (w))->printsetup_box.print_to_printer_toggle)
#define	PSB_PrintToFileToggle(w) \
        (((DtPrintSetupBoxWidget) (w))->printsetup_box.print_to_file_toggle)
#define	PSB_PrintDestination(w) \
        (((DtPrintSetupBoxWidget) (w))->printsetup_box.print_destination)

/* Define the full instance record */
typedef struct _DtPrintSetupBoxRec {
    CorePart core;
    CompositePart composite;
    ConstraintPart constraint;
    XmManagerPart manager;
    XmBulletinBoardPart bulletin_board;
    DtPrintSetupBoxPart printsetup_box;
} DtPrintSetupBoxRec;

/* Define class part structure */
typedef struct {
    XtCallbackProc list_callback;
    XtPointer extension;
} DtPrintSetupBoxClassPart;

/* Defint the full class record */
typedef struct _DtPrintSetupBoxClassRec {
    CoreClassPart core_class;
    CompositeClassPart composite_class;
    ConstraintClassPart constraint_class;
    XmManagerClassPart manager_class;
    XmBulletinBoardClassPart bulletin_board_class;
    DtPrintSetupBoxClassPart printsetup_box_class;
} DtPrintSetupBoxClassRec;

extern DtPrintSetupBoxClassRec dtPrintSetupBoxClassRec;

#ifdef __cplusplus
}
#endif

#endif /* _DT_PRINT_SETUP_BOX_P_H_ */
