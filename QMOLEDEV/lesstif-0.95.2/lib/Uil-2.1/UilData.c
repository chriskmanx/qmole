/**
 *
 * $Id: UilData.c,v 1.1 2004/08/28 19:22:42 dannybackx Exp $
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
 **/

static const char rcsid[] = "$Id: UilData.c,v 1.1 2004/08/28 19:22:42 dannybackx Exp $";

#include <LTconfig.h>

#include <stdlib.h>

#include <uil/UilDBDef.h>
#include <uil/UilSymGl.h>

#include <XmI/DebugUtil.h>


unsigned char *constraint_tab;

unsigned char **allowed_argument_table;
unsigned char *argument_type_table;
unsigned char **allowed_child_table;
unsigned char *child_class_table;

unsigned char *charset_writing_direction_table;
unsigned char *charset_parsing_direction_table;
unsigned char *charset_character_size_table;

unsigned char **allowed_control_table;
UilEnumSetDescDef *enum_set_table;
unsigned short int *argument_enumset_table;
int *enumval_values_table;

unsigned short int *related_argument_table;
unsigned char **allowed_reason_table;

unsigned short int *uil_gadget_variants;
unsigned short int *uil_urm_nondialog_class;
unsigned short int *uil_urm_subtree_resource;

unsigned short int uil_sym_user_defined_object;
unsigned short int uil_sym_default_charset;
unsigned short int uil_sym_isolatin1_charset;

static key_keytable_entry_type
priv_key_table[] = {
    {
	tkn_k_class_charset,	   11,	 5,	  6,	"88591"
    },
    {
	tkn_k_class_charset,	   12,	 5,	  6,	"88592"
    },
    {
	tkn_k_class_charset,	   13,	 5,	  6,	"88593"
    },
    {
	tkn_k_class_charset,	   14,	 5,	  6,	"88594"
    },
    {
	tkn_k_class_charset,	   15,	 5,	  6,	"88595"
    },
    {
	tkn_k_class_charset,	    6,	 5,	  6,	"88596"
    },
    {
	tkn_k_class_charset,	    8,	 5,	  6,	"88597"
    },
    {
	tkn_k_class_charset,	    9,	 5,	  6,	"88598"
    },
    {
	tkn_k_class_charset,	   11,	 5,	  6,	"ASCII"
    },
    {
	tkn_k_class_charset,	   11,	 9,	  6,	"ISOLatin1"
    },
    {
	tkn_k_class_charset,	   12,	 9,	  6,	"ISOLatin2"
    },
    {
	tkn_k_class_charset,	   13,	 9,	  6,	"ISOLatin3"
    },
    {
	tkn_k_class_charset,	   14,	 9,	  6,	"ISOLatin4"
    },
    {
	tkn_k_class_charset,	   15,	 9,	  6,	"ISOLatin5"
    },
    {
	tkn_k_class_charset,	    6,	 9,	  6,	"ISOLatin6"
    },
    {
	tkn_k_class_charset,	    8,	 9,	  6,	"ISOLatin7"
    },
    {
	tkn_k_class_charset,	    9,	 9,	  6,	"ISOLatin8"
    },
    {
	tkn_k_class_enumval,	    1,	11,	  7,	"IconicState"
    },
    {
	tkn_k_class_reason,	    1,	18,	  5,	"MrmNcreateCallback"
    },
    {
	tkn_k_class_enumval,	    2,	11,	  7,	"NormalState"
    },
    {
	tkn_k_class_enumval,	    3,	15,	  7,	"Xm1000TH_INCHES"
    },
    {
	tkn_k_class_enumval,	    4,	18,	  7,	"Xm100TH_FONT_UNITS"
    },
    {
	tkn_k_class_enumval,	    5,	19,	  7,	"Xm100TH_MILLIMETERS"
    },
    {
	tkn_k_class_enumval,	    6,	14,	  7,	"Xm100TH_POINTS"
    },
    {
	tkn_k_class_enumval,	    7,	27,	  7,	"XmALIGNMENT_BASELINE_BOTTOM"
    },
    {
	tkn_k_class_enumval,	    8,	24,	  7,	"XmALIGNMENT_BASELINE_TOP"
    },
    {
	tkn_k_class_enumval,	    9,	21,	  7,	"XmALIGNMENT_BEGINNING"
    },
    {
	tkn_k_class_enumval,	   10,	18,	  7,	"XmALIGNMENT_CENTER"
    },
    {
	tkn_k_class_enumval,	   11,	27,	  7,	"XmALIGNMENT_CONTENTS_BOTTOM"
    },
    {
	tkn_k_class_enumval,	   12,	24,	  7,	"XmALIGNMENT_CONTENTS_TOP"
    },
    {
	tkn_k_class_enumval,	   13,	15,	  7,	"XmALIGNMENT_END"
    },
    {
	tkn_k_class_enumval,	   14,	25,	  7,	"XmALIGNMENT_WIDGET_BOTTOM"
    },
    {
	tkn_k_class_enumval,	   15,	22,	  7,	"XmALIGNMENT_WIDGET_TOP"
    },
    {
	tkn_k_class_enumval,	   16,	21,	  7,	"XmAPPLICATION_DEFINED"
    },
    {
	tkn_k_class_enumval,	   17,	12,	  7,	"XmARROW_DOWN"
    },
    {
	tkn_k_class_enumval,	   18,	12,	  7,	"XmARROW_LEFT"
    },
    {
	tkn_k_class_enumval,	   19,	13,	  7,	"XmARROW_RIGHT"
    },
    {
	tkn_k_class_enumval,	   20,	10,	  7,	"XmARROW_UP"
    },
    {
	tkn_k_class_enumval,	   21,	11,	  7,	"XmAS_NEEDED"
    },
    {
	tkn_k_class_enumval,	   22,	13,	  7,	"XmATTACH_FORM"
    },
    {
	tkn_k_class_enumval,	   23,	13,	  7,	"XmATTACH_NONE"
    },
    {
	tkn_k_class_enumval,	   24,	22,	  7,	"XmATTACH_OPPOSITE_FORM"
    },
    {
	tkn_k_class_enumval,	   25,	24,	  7,	"XmATTACH_OPPOSITE_WIDGET"
    },
    {
	tkn_k_class_enumval,	   26,	17,	  7,	"XmATTACH_POSITION"
    },
    {
	tkn_k_class_enumval,	   27,	13,	  7,	"XmATTACH_SELF"
    },
    {
	tkn_k_class_enumval,	   28,	15,	  7,	"XmATTACH_WIDGET"
    },
    {
	tkn_k_class_enumval,	   29,	11,	  7,	"XmAUTOMATIC"
    },
    {
	tkn_k_class_class,	    1,	13,	  8,	"XmArrowButton"
    },
    {
	tkn_k_class_class,	    2,	19,	  8,	"XmArrowButtonGadget"
    },
    {
	tkn_k_class_enumval,	   30,	 6,	  7,	"XmBELL"
    },
    {
	tkn_k_class_enumval,	   31,	13,	  7,	"XmBOTTOM_LEFT"
    },
    {
	tkn_k_class_enumval,	   32,	14,	  7,	"XmBOTTOM_RIGHT"
    },
    {
	tkn_k_class_enumval,	   33,	15,	  7,	"XmBROWSE_SELECT"
    },
    {
	tkn_k_class_class,	    3,	15,	  8,	"XmBulletinBoard"
    },
    {
	tkn_k_class_class,	    4,	21,	  8,	"XmBulletinBoardDialog"
    },
    {
	tkn_k_class_enumval,	   34,	25,	  7,	"XmCOMMAND_ABOVE_WORKSPACE"
    },
    {
	tkn_k_class_enumval,	   35,	25,	  7,	"XmCOMMAND_BELOW_WORKSPACE"
    },
    {
	tkn_k_class_enumval,	   36,	10,	  7,	"XmCONSTANT"
    },
    {
	tkn_k_class_class,	    5,	15,	  8,	"XmCascadeButton"
    },
    {
	tkn_k_class_class,	    6,	21,	  8,	"XmCascadeButtonGadget"
    },
    {
	tkn_k_class_class,	    7,	 9,	  8,	"XmCommand"
    },
    {
	tkn_k_class_enumval,	   37,	 9,	  7,	"XmDESTROY"
    },
    {
	tkn_k_class_enumval,	   38,	26,	  7,	"XmDIALOG_APPLICATION_MODAL"
    },
    {
	tkn_k_class_enumval,	   39,	22,	  7,	"XmDIALOG_CANCEL_BUTTON"
    },
    {
	tkn_k_class_enumval,	   40,	23,	  7,	"XmDIALOG_DEFAULT_BUTTON"
    },
    {
	tkn_k_class_enumval,	   41,	14,	  7,	"XmDIALOG_ERROR"
    },
    {
	tkn_k_class_enumval,	   42,	23,	  7,	"XmDIALOG_FILE_SELECTION"
    },
    {
	tkn_k_class_enumval,	   43,	31,	  7,	"XmDIALOG_FULL_APPLICATION_MODAL"
    },
    {
	tkn_k_class_enumval,	   44,	20,	  7,	"XmDIALOG_HELP_BUTTON"
    },
    {
	tkn_k_class_enumval,	   45,	20,	  7,	"XmDIALOG_INFORMATION"
    },
    {
	tkn_k_class_enumval,	   46,	16,	  7,	"XmDIALOG_MESSAGE"
    },
    {
	tkn_k_class_enumval,	   47,	22,	  7,	"XmDIALOG_MESSAGE_LABEL"
    },
    {
	tkn_k_class_enumval,	   48,	17,	  7,	"XmDIALOG_MODELESS"
    },
    {
	tkn_k_class_enumval,	   49,	18,	  7,	"XmDIALOG_OK_BUTTON"
    },
    {
	tkn_k_class_enumval,	   50,	34,	  7,	"XmDIALOG_PRIMARY_APPLICATION_MODAL"
    },
    {
	tkn_k_class_enumval,	   51,	15,	  7,	"XmDIALOG_PROMPT"
    },
    {
	tkn_k_class_enumval,	   52,	17,	  7,	"XmDIALOG_QUESTION"
    },
    {
	tkn_k_class_enumval,	   53,	18,	  7,	"XmDIALOG_SELECTION"
    },
    {
	tkn_k_class_enumval,	   54,	18,	  7,	"XmDIALOG_SEPARATOR"
    },
    {
	tkn_k_class_enumval,	   55,	21,	  7,	"XmDIALOG_SYMBOL_LABEL"
    },
    {
	tkn_k_class_enumval,	   56,	21,	  7,	"XmDIALOG_SYSTEM_MODAL"
    },
    {
	tkn_k_class_enumval,	   57,	17,	  7,	"XmDIALOG_TEMPLATE"
    },
    {
	tkn_k_class_enumval,	   58,	16,	  7,	"XmDIALOG_WARNING"
    },
    {
	tkn_k_class_enumval,	   59,	16,	  7,	"XmDIALOG_WORKING"
    },
    {
	tkn_k_class_enumval,	   60,	18,	  7,	"XmDIALOG_WORK_AREA"
    },
    {
	tkn_k_class_enumval,	   61,	20,	  7,	"XmDOUBLE_DASHED_LINE"
    },
    {
	tkn_k_class_enumval,	   62,	13,	  7,	"XmDOUBLE_LINE"
    },
    {
	tkn_k_class_enumval,	   63,	12,	  7,	"XmDO_NOTHING"
    },
    {
	tkn_k_class_class,	    8,	13,	  8,	"XmDialogShell"
    },
    {
	tkn_k_class_class,	    9,	13,	  8,	"XmDrawingArea"
    },
    {
	tkn_k_class_class,	   10,	13,	  8,	"XmDrawnButton"
    },
    {
	tkn_k_class_enumval,	   64,	21,	  7,	"XmEXCLUSIVE_TAB_GROUP"
    },
    {
	tkn_k_class_enumval,	   65,	10,	  7,	"XmEXPLICIT"
    },
    {
	tkn_k_class_enumval,	   66,	17,	  7,	"XmEXTENDED_SELECT"
    },
    {
	tkn_k_class_class,	   11,	13,	  8,	"XmErrorDialog"
    },
    {
	tkn_k_class_enumval,	   67,	16,	  7,	"XmFILE_DIRECTORY"
    },
    {
	tkn_k_class_enumval,	   68,	14,	  7,	"XmFILE_REGULAR"
    },
    {
	tkn_k_class_enumval,	   69,	16,	  7,	"XmFIRST_POSITION"
    },
    {
	tkn_k_class_enumval,	   70,	21,	  7,	"XmFRAME_GENERIC_CHILD"
    },
    {
	tkn_k_class_enumval,	   71,	19,	  7,	"XmFRAME_TITLE_CHILD"
    },
    {
	tkn_k_class_enumval,	   72,	22,	  7,	"XmFRAME_WORKAREA_CHILD"
    },
    {
	tkn_k_class_class,	   12,	18,	  8,	"XmFileSelectionBox"
    },
    {
	tkn_k_class_class,	   13,	21,	  8,	"XmFileSelectionDialog"
    },
    {
	tkn_k_class_class,	   14,	 6,	  8,	"XmForm"
    },
    {
	tkn_k_class_class,	   15,	12,	  8,	"XmFormDialog"
    },
    {
	tkn_k_class_class,	   16,	 7,	  8,	"XmFrame"
    },
    {
	tkn_k_class_enumval,	   73,	12,	  7,	"XmHORIZONTAL"
    },
    {
	tkn_k_class_class,	   17,	19,	  8,	"XmInformationDialog"
    },
    {
	tkn_k_class_enumval,	   74,	15,	  7,	"XmLAST_POSITION"
    },
    {
	tkn_k_class_class,	   18,	 7,	  8,	"XmLabel"
    },
    {
	tkn_k_class_class,	   19,	13,	  8,	"XmLabelGadget"
    },
    {
	tkn_k_class_class,	   20,	 6,	  8,	"XmList"
    },
    {
	tkn_k_class_enumval,	   75,	15,	  7,	"XmMAX_ON_BOTTOM"
    },
    {
	tkn_k_class_enumval,	   76,	13,	  7,	"XmMAX_ON_LEFT"
    },
    {
	tkn_k_class_enumval,	   77,	14,	  7,	"XmMAX_ON_RIGHT"
    },
    {
	tkn_k_class_enumval,	   78,	12,	  7,	"XmMAX_ON_TOP"
    },
    {
	tkn_k_class_enumval,	   79,	10,	  7,	"XmMENU_BAR"
    },
    {
	tkn_k_class_enumval,	   80,	13,	  7,	"XmMENU_OPTION"
    },
    {
	tkn_k_class_enumval,	   81,	12,	  7,	"XmMENU_POPUP"
    },
    {
	tkn_k_class_enumval,	   82,	15,	  7,	"XmMENU_PULLDOWN"
    },
    {
	tkn_k_class_enumval,	   83,	20,	  7,	"XmMULTICLICK_DISCARD"
    },
    {
	tkn_k_class_enumval,	   84,	17,	  7,	"XmMULTICLICK_KEEP"
    },
    {
	tkn_k_class_enumval,	   85,	17,	  7,	"XmMULTIPLE_SELECT"
    },
    {
	tkn_k_class_enumval,	   86,	17,	  7,	"XmMULTI_LINE_EDIT"
    },
    {
	tkn_k_class_class,	   21,	12,	  8,	"XmMainWindow"
    },
    {
	tkn_k_class_class,	   22,	 9,	  8,	"XmMenuBar"
    },
    {
	tkn_k_class_class,	   23,	11,	  8,	"XmMenuShell"
    },
    {
	tkn_k_class_class,	   24,	12,	  8,	"XmMessageBox"
    },
    {
	tkn_k_class_class,	   25,	15,	  8,	"XmMessageDialog"
    },
    {
	tkn_k_class_enumval,	   87,	 6,	  7,	"XmNONE"
    },
    {
	tkn_k_class_enumval,	   88,	 9,	  7,	"XmNO_LINE"
    },
    {
	tkn_k_class_enumval,	   89,	16,	  7,	"XmNO_ORIENTATION"
    },
    {
	tkn_k_class_enumval,	   90,	12,	  7,	"XmNO_PACKING"
    },
    {
	tkn_k_class_enumval,	   91,	11,	  7,	"XmN_OF_MANY"
    },
    {
	tkn_k_class_argument,	    1,	14,	  4,	"XmNaccelerator"
    },
    {
	tkn_k_class_argument,	    2,	18,	  4,	"XmNacceleratorText"
    },
    {
	tkn_k_class_argument,	    3,	15,	  4,	"XmNaccelerators"
    },
    {
	tkn_k_class_reason,	    2,	19,	  5,	"XmNactivateCallback"
    },
    {
	tkn_k_class_argument,	    4,	13,	  4,	"XmNadjustLast"
    },
    {
	tkn_k_class_argument,	    5,	15,	  4,	"XmNadjustMargin"
    },
    {
	tkn_k_class_argument,	    6,	12,	  4,	"XmNalignment"
    },
    {
	tkn_k_class_argument,	    7,	15,	  4,	"XmNallowOverlap"
    },
    {
	tkn_k_class_argument,	    8,	14,	  4,	"XmNallowResize"
    },
    {
	tkn_k_class_argument,	    9,	19,	  4,	"XmNallowShellResize"
    },
    {
	tkn_k_class_argument,	   10,	20,	  4,	"XmNancestorSensitive"
    },
    {
	tkn_k_class_reason,	    3,	16,	  5,	"XmNapplyCallback"
    },
    {
	tkn_k_class_argument,	   11,	19,	  4,	"XmNapplyLabelString"
    },
    {
	tkn_k_class_reason,	    4,	14,	  5,	"XmNarmCallback"
    },
    {
	tkn_k_class_argument,	   12,	11,	  4,	"XmNarmColor"
    },
    {
	tkn_k_class_argument,	   13,	12,	  4,	"XmNarmPixmap"
    },
    {
	tkn_k_class_argument,	   14,	17,	  4,	"XmNarrowDirection"
    },
    {
	tkn_k_class_argument,	   15,	17,	  4,	"XmNaudibleWarning"
    },
    {
	tkn_k_class_argument,	   16,	25,	  4,	"XmNautoShowCursorPosition"
    },
    {
	tkn_k_class_argument,	   17,	15,	  4,	"XmNautoUnmanage"
    },
    {
	tkn_k_class_argument,	   18,	21,	  4,	"XmNautomaticSelection"
    },
    {
	tkn_k_class_argument,	   19,	13,	  4,	"XmNbackground"
    },
    {
	tkn_k_class_argument,	   20,	19,	  4,	"XmNbackgroundPixmap"
    },
    {
	tkn_k_class_argument,	   21,	13,	  4,	"XmNbaseHeight"
    },
    {
	tkn_k_class_argument,	   22,	12,	  4,	"XmNbaseWidth"
    },
    {
	tkn_k_class_argument,	   23,	12,	  4,	"XmNblinkRate"
    },
    {
	tkn_k_class_argument,	   24,	14,	  4,	"XmNborderColor"
    },
    {
	tkn_k_class_argument,	   25,	15,	  4,	"XmNborderPixmap"
    },
    {
	tkn_k_class_argument,	   26,	14,	  4,	"XmNborderWidth"
    },
    {
	tkn_k_class_argument,	   27,	19,	  4,	"XmNbottomAttachment"
    },
    {
	tkn_k_class_argument,	   28,	15,	  4,	"XmNbottomOffset"
    },
    {
	tkn_k_class_argument,	   29,	17,	  4,	"XmNbottomPosition"
    },
    {
	tkn_k_class_argument,	   30,	20,	  4,	"XmNbottomShadowColor"
    },
    {
	tkn_k_class_argument,	   31,	21,	  4,	"XmNbottomShadowPixmap"
    },
    {
	tkn_k_class_argument,	   32,	15,	  4,	"XmNbottomWidget"
    },
    {
	tkn_k_class_reason,	    5,	26,	  5,	"XmNbrowseSelectionCallback"
    },
    {
	tkn_k_class_argument,	   33,	17,	  4,	"XmNbuttonFontList"
    },
    {
	tkn_k_class_argument,	   34,	15,	  4,	"XmNcancelButton"
    },
    {
	tkn_k_class_reason,	    6,	17,	  5,	"XmNcancelCallback"
    },
    {
	tkn_k_class_argument,	   35,	20,	  4,	"XmNcancelLabelString"
    },
    {
	tkn_k_class_argument,	   36,	16,	  4,	"XmNcascadePixmap"
    },
    {
	tkn_k_class_reason,	    7,	20,	  5,	"XmNcascadingCallback"
    },
    {
	tkn_k_class_argument,	   37,	27,	  4,	"XmNchildHorizontalAlignment"
    },
    {
	tkn_k_class_argument,	   38,	25,	  4,	"XmNchildHorizontalSpacing"
    },
    {
	tkn_k_class_argument,	   39,	17,	  4,	"XmNchildPlacement"
    },
    {
	tkn_k_class_argument,	   40,	12,	  4,	"XmNchildType"
    },
    {
	tkn_k_class_argument,	   41,	25,	  4,	"XmNchildVerticalAlignment"
    },
    {
	tkn_k_class_argument,	   42,	13,	  4,	"XmNclipWindow"
    },
    {
	tkn_k_class_argument,	   43,	11,	  4,	"XmNcolormap"
    },
    {
	tkn_k_class_argument,	   44,	10,	  4,	"XmNcolumns"
    },
    {
	tkn_k_class_argument,	   45,	10,	  4,	"XmNcommand"
    },
    {
	tkn_k_class_reason,	    8,	25,	  5,	"XmNcommandChangedCallback"
    },
    {
	tkn_k_class_reason,	    9,	25,	  5,	"XmNcommandEnteredCallback"
    },
    {
	tkn_k_class_argument,	   46,	16,	  4,	"XmNcommandWindow"
    },
    {
	tkn_k_class_argument,	   47,	24,	  4,	"XmNcommandWindowLocation"
    },
    {
	tkn_k_class_argument,	   48,	23,	  4,	"XmNcreatePopupChildProc"
    },
    {
	tkn_k_class_argument,	   49,	17,	  4,	"XmNcursorPosition"
    },
    {
	tkn_k_class_argument,	   50,	24,	  4,	"XmNcursorPositionVisible"
    },
    {
	tkn_k_class_argument,	   51,	16,	  4,	"XmNdecimalPoints"
    },
    {
	tkn_k_class_reason,	   10,	20,	  5,	"XmNdecrementCallback"
    },
    {
	tkn_k_class_reason,	   11,	24,	  5,	"XmNdefaultActionCallback"
    },
    {
	tkn_k_class_argument,	   52,	16,	  4,	"XmNdefaultButton"
    },
    {
	tkn_k_class_argument,	   53,	31,	  4,	"XmNdefaultButtonShadowThickness"
    },
    {
	tkn_k_class_argument,	   54,	20,	  4,	"XmNdefaultButtonType"
    },
    {
	tkn_k_class_argument,	   55,	18,	  4,	"XmNdefaultFontList"
    },
    {
	tkn_k_class_argument,	   56,	18,	  4,	"XmNdefaultPosition"
    },
    {
	tkn_k_class_argument,	   57,	17,	  4,	"XmNdeleteResponse"
    },
    {
	tkn_k_class_argument,	   58,	 8,	  4,	"XmNdepth"
    },
    {
	tkn_k_class_reason,	   12,	18,	  5,	"XmNdestroyCallback"
    },
    {
	tkn_k_class_argument,	   59,	14,	  4,	"XmNdialogStyle"
    },
    {
	tkn_k_class_argument,	   60,	14,	  4,	"XmNdialogTitle"
    },
    {
	tkn_k_class_argument,	   61,	13,	  4,	"XmNdialogType"
    },
    {
	tkn_k_class_argument,	   62,	19,	  4,	"XmNdirListItemCount"
    },
    {
	tkn_k_class_argument,	   63,	15,	  4,	"XmNdirListItems"
    },
    {
	tkn_k_class_argument,	   64,	21,	  4,	"XmNdirListLabelString"
    },
    {
	tkn_k_class_argument,	   65,	10,	  4,	"XmNdirMask"
    },
    {
	tkn_k_class_argument,	   66,	16,	  4,	"XmNdirSearchProc"
    },
    {
	tkn_k_class_argument,	   67,	10,	  4,	"XmNdirSpec"
    },
    {
	tkn_k_class_argument,	   68,	12,	  4,	"XmNdirectory"
    },
    {
	tkn_k_class_reason,	   13,	17,	  5,	"XmNdisarmCallback"
    },
    {
	tkn_k_class_argument,	   69,	22,	  4,	"XmNdoubleClickInterval"
    },
    {
	tkn_k_class_reason,	   14,	15,	  5,	"XmNdragCallback"
    },
    {
	tkn_k_class_argument,	   70,	11,	  4,	"XmNeditMode"
    },
    {
	tkn_k_class_argument,	   71,	11,	  4,	"XmNeditable"
    },
    {
	tkn_k_class_argument,	   72,	17,	  4,	"XmNentryAlignment"
    },
    {
	tkn_k_class_argument,	   73,	14,	  4,	"XmNentryBorder"
    },
    {
	tkn_k_class_reason,	   15,	16,	  5,	"XmNentryCallback"
    },
    {
	tkn_k_class_argument,	   74,	13,	  4,	"XmNentryClass"
    },
    {
	tkn_k_class_argument,	   75,	25,	  4,	"XmNentryVerticalAlignment"
    },
    {
	tkn_k_class_reason,	   16,	17,	  5,	"XmNexposeCallback"
    },
    {
	tkn_k_class_reason,	   17,	28,	  5,	"XmNextendedSelectionCallback"
    },
    {
	tkn_k_class_argument,	   76,	20,	  4,	"XmNfileListItemCount"
    },
    {
	tkn_k_class_argument,	   77,	16,	  4,	"XmNfileListItems"
    },
    {
	tkn_k_class_argument,	   78,	22,	  4,	"XmNfileListLabelString"
    },
    {
	tkn_k_class_argument,	   79,	17,	  4,	"XmNfileSearchProc"
    },
    {
	tkn_k_class_argument,	   80,	15,	  4,	"XmNfileTypeMask"
    },
    {
	tkn_k_class_argument,	   81,	12,	  4,	"XmNfillOnArm"
    },
    {
	tkn_k_class_argument,	   82,	15,	  4,	"XmNfillOnSelect"
    },
    {
	tkn_k_class_argument,	   83,	20,	  4,	"XmNfilterLabelString"
    },
    {
	tkn_k_class_reason,	   18,	16,	  5,	"XmNfocusCallback"
    },
    {
	tkn_k_class_argument,	   84,	11,	  4,	"XmNfontList"
    },
    {
	tkn_k_class_argument,	   85,	13,	  4,	"XmNforeground"
    },
    {
	tkn_k_class_argument,	   86,	15,	  4,	"XmNfractionBase"
    },
    {
	tkn_k_class_reason,	   19,	22,	  5,	"XmNgainPrimaryCallback"
    },
    {
	tkn_k_class_argument,	   87,	11,	  4,	"XmNgeometry"
    },
    {
	tkn_k_class_argument,	   88,	 9,	  4,	"XmNheight"
    },
    {
	tkn_k_class_argument,	   89,	12,	  4,	"XmNheightInc"
    },
    {
	tkn_k_class_reason,	   20,	15,	  5,	"XmNhelpCallback"
    },
    {
	tkn_k_class_argument,	   90,	18,	  4,	"XmNhelpLabelString"
    },
    {
	tkn_k_class_argument,	   91,	17,	  4,	"XmNhighlightColor"
    },
    {
	tkn_k_class_argument,	   92,	19,	  4,	"XmNhighlightOnEnter"
    },
    {
	tkn_k_class_argument,	   93,	18,	  4,	"XmNhighlightPixmap"
    },
    {
	tkn_k_class_argument,	   94,	21,	  4,	"XmNhighlightThickness"
    },
    {
	tkn_k_class_argument,	   95,	19,	  4,	"XmNhistoryItemCount"
    },
    {
	tkn_k_class_argument,	   96,	15,	  4,	"XmNhistoryItems"
    },
    {
	tkn_k_class_argument,	   97,	18,	  4,	"XmNhistoryMaxItems"
    },
    {
	tkn_k_class_argument,	   98,	26,	  4,	"XmNhistoryVisibleItemCount"
    },
    {
	tkn_k_class_argument,	   99,	22,	  4,	"XmNhorizontalScrollBar"
    },
    {
	tkn_k_class_argument,	  100,	20,	  4,	"XmNhorizontalSpacing"
    },
    {
	tkn_k_class_argument,	  101,	11,	  4,	"XmNiconMask"
    },
    {
	tkn_k_class_argument,	  102,	11,	  4,	"XmNiconName"
    },
    {
	tkn_k_class_argument,	  103,	19,	  4,	"XmNiconNameEncoding"
    },
    {
	tkn_k_class_argument,	  104,	13,	  4,	"XmNiconPixmap"
    },
    {
	tkn_k_class_argument,	  105,	13,	  4,	"XmNiconWindow"
    },
    {
	tkn_k_class_argument,	  106,	 8,	  4,	"XmNiconX"
    },
    {
	tkn_k_class_argument,	  107,	 8,	  4,	"XmNiconY"
    },
    {
	tkn_k_class_argument,	  108,	 9,	  4,	"XmNiconic"
    },
    {
	tkn_k_class_argument,	  109,	12,	  4,	"XmNincrement"
    },
    {
	tkn_k_class_reason,	   21,	20,	  5,	"XmNincrementCallback"
    },
    {
	tkn_k_class_argument,	  110,	14,	  4,	"XmNindicatorOn"
    },
    {
	tkn_k_class_argument,	  111,	16,	  4,	"XmNindicatorSize"
    },
    {
	tkn_k_class_argument,	  112,	16,	  4,	"XmNindicatorType"
    },
    {
	tkn_k_class_argument,	  113,	15,	  4,	"XmNinitialDelay"
    },
    {
	tkn_k_class_argument,	  114,	15,	  4,	"XmNinitialFocus"
    },
    {
	tkn_k_class_argument,	  115,	29,	  4,	"XmNinitialResourcesPersistent"
    },
    {
	tkn_k_class_argument,	  116,	15,	  4,	"XmNinitialState"
    },
    {
	tkn_k_class_argument,	  117,	 8,	  4,	"XmNinput"
    },
    {
	tkn_k_class_reason,	   22,	16,	  5,	"XmNinputCallback"
    },
    {
	tkn_k_class_argument,	  118,	14,	  4,	"XmNinputMethod"
    },
    {
	tkn_k_class_argument,	  119,	17,	  4,	"XmNinsertPosition"
    },
    {
	tkn_k_class_argument,	  120,	12,	  4,	"XmNisAligned"
    },
    {
	tkn_k_class_argument,	  121,	16,	  4,	"XmNisHomogeneous"
    },
    {
	tkn_k_class_argument,	  122,	12,	  4,	"XmNitemCount"
    },
    {
	tkn_k_class_argument,	  123,	 8,	  4,	"XmNitems"
    },
    {
	tkn_k_class_argument,	  124,	22,	  4,	"XmNkeyboardFocusPolicy"
    },
    {
	tkn_k_class_argument,	  125,	16,	  4,	"XmNlabelFontList"
    },
    {
	tkn_k_class_argument,	  126,	25,	  4,	"XmNlabelInsensitivePixmap"
    },
    {
	tkn_k_class_argument,	  127,	14,	  4,	"XmNlabelPixmap"
    },
    {
	tkn_k_class_argument,	  128,	14,	  4,	"XmNlabelString"
    },
    {
	tkn_k_class_argument,	  129,	12,	  4,	"XmNlabelType"
    },
    {
	tkn_k_class_argument,	  130,	17,	  4,	"XmNleftAttachment"
    },
    {
	tkn_k_class_argument,	  131,	13,	  4,	"XmNleftOffset"
    },
    {
	tkn_k_class_argument,	  132,	15,	  4,	"XmNleftPosition"
    },
    {
	tkn_k_class_argument,	  133,	13,	  4,	"XmNleftWidget"
    },
    {
	tkn_k_class_argument,	  134,	16,	  4,	"XmNlistItemCount"
    },
    {
	tkn_k_class_argument,	  135,	12,	  4,	"XmNlistItems"
    },
    {
	tkn_k_class_argument,	  136,	18,	  4,	"XmNlistLabelString"
    },
    {
	tkn_k_class_argument,	  137,	19,	  4,	"XmNlistMarginHeight"
    },
    {
	tkn_k_class_argument,	  138,	18,	  4,	"XmNlistMarginWidth"
    },
    {
	tkn_k_class_argument,	  139,	17,	  4,	"XmNlistSizePolicy"
    },
    {
	tkn_k_class_argument,	  140,	14,	  4,	"XmNlistSpacing"
    },
    {
	tkn_k_class_argument,	  141,	14,	  4,	"XmNlistUpdated"
    },
    {
	tkn_k_class_argument,	  142,	23,	  4,	"XmNlistVisibleItemCount"
    },
    {
	tkn_k_class_reason,	   23,	22,	  5,	"XmNlosePrimaryCallback"
    },
    {
	tkn_k_class_reason,	   24,	22,	  5,	"XmNlosingFocusCallback"
    },
    {
	tkn_k_class_argument,	  143,	25,	  4,	"XmNmainWindowMarginHeight"
    },
    {
	tkn_k_class_argument,	  144,	24,	  4,	"XmNmainWindowMarginWidth"
    },
    {
	tkn_k_class_reason,	   25,	14,	  5,	"XmNmapCallback"
    },
    {
	tkn_k_class_argument,	  145,	20,	  4,	"XmNmappedWhenManaged"
    },
    {
	tkn_k_class_argument,	  146,	15,	  4,	"XmNmappingDelay"
    },
    {
	tkn_k_class_argument,	  147,	 9,	  4,	"XmNmargin"
    },
    {
	tkn_k_class_argument,	  148,	15,	  4,	"XmNmarginBottom"
    },
    {
	tkn_k_class_argument,	  149,	15,	  4,	"XmNmarginHeight"
    },
    {
	tkn_k_class_argument,	  150,	13,	  4,	"XmNmarginLeft"
    },
    {
	tkn_k_class_argument,	  151,	14,	  4,	"XmNmarginRight"
    },
    {
	tkn_k_class_argument,	  152,	12,	  4,	"XmNmarginTop"
    },
    {
	tkn_k_class_argument,	  153,	14,	  4,	"XmNmarginWidth"
    },
    {
	tkn_k_class_argument,	  154,	13,	  4,	"XmNmaxAspectX"
    },
    {
	tkn_k_class_argument,	  155,	13,	  4,	"XmNmaxAspectY"
    },
    {
	tkn_k_class_argument,	  156,	12,	  4,	"XmNmaxHeight"
    },
    {
	tkn_k_class_argument,	  157,	12,	  4,	"XmNmaxLength"
    },
    {
	tkn_k_class_argument,	  158,	11,	  4,	"XmNmaxWidth"
    },
    {
	tkn_k_class_argument,	  159,	10,	  4,	"XmNmaximum"
    },
    {
	tkn_k_class_argument,	  160,	18,	  4,	"XmNmenuAccelerator"
    },
    {
	tkn_k_class_argument,	  161,	10,	  4,	"XmNmenuBar"
    },
    {
	tkn_k_class_argument,	  162,	17,	  4,	"XmNmenuHelpWidget"
    },
    {
	tkn_k_class_argument,	  163,	14,	  4,	"XmNmenuHistory"
    },
    {
	tkn_k_class_argument,	  164,	11,	  4,	"XmNmenuPost"
    },
    {
	tkn_k_class_argument,	  165,	19,	  4,	"XmNmessageAlignment"
    },
    {
	tkn_k_class_argument,	  166,	16,	  4,	"XmNmessageString"
    },
    {
	tkn_k_class_argument,	  167,	16,	  4,	"XmNmessageWindow"
    },
    {
	tkn_k_class_argument,	  168,	13,	  4,	"XmNminAspectX"
    },
    {
	tkn_k_class_argument,	  169,	13,	  4,	"XmNminAspectY"
    },
    {
	tkn_k_class_argument,	  170,	12,	  4,	"XmNminHeight"
    },
    {
	tkn_k_class_argument,	  171,	11,	  4,	"XmNminWidth"
    },
    {
	tkn_k_class_argument,	  172,	18,	  4,	"XmNminimizeButtons"
    },
    {
	tkn_k_class_argument,	  173,	10,	  4,	"XmNminimum"
    },
    {
	tkn_k_class_argument,	  174,	11,	  4,	"XmNmnemonic"
    },
    {
	tkn_k_class_argument,	  175,	18,	  4,	"XmNmnemonicCharSet"
    },
    {
	tkn_k_class_reason,	   26,	23,	  5,	"XmNmodifyVerifyCallback"
    },
    {
	tkn_k_class_reason,	   27,	26,	  5,	"XmNmodifyVerifyCallbackWcs"
    },
    {
	tkn_k_class_reason,	   28,	23,	  5,	"XmNmotionVerifyCallback"
    },
    {
	tkn_k_class_argument,	  176,	13,	  4,	"XmNmultiClick"
    },
    {
	tkn_k_class_reason,	   29,	28,	  5,	"XmNmultipleSelectionCallback"
    },
    {
	tkn_k_class_argument,	  177,	12,	  4,	"XmNmustMatch"
    },
    {
	tkn_k_class_argument,	  178,	17,	  4,	"XmNmwmDecorations"
    },
    {
	tkn_k_class_argument,	  179,	15,	  4,	"XmNmwmFunctions"
    },
    {
	tkn_k_class_argument,	  180,	15,	  4,	"XmNmwmInputMode"
    },
    {
	tkn_k_class_argument,	  181,	10,	  4,	"XmNmwmMenu"
    },
    {
	tkn_k_class_argument,	  182,	17,	  4,	"XmNnavigationType"
    },
    {
	tkn_k_class_reason,	   30,	18,	  5,	"XmNnoMatchCallback"
    },
    {
	tkn_k_class_argument,	  183,	16,	  4,	"XmNnoMatchString"
    },
    {
	tkn_k_class_argument,	  184,	11,	  4,	"XmNnoResize"
    },
    {
	tkn_k_class_argument,	  185,	13,	  4,	"XmNnumColumns"
    },
    {
	tkn_k_class_reason,	   31,	13,	  5,	"XmNokCallback"
    },
    {
	tkn_k_class_argument,	  186,	16,	  4,	"XmNokLabelString"
    },
    {
	tkn_k_class_argument,	  187,	14,	  4,	"XmNorientation"
    },
    {
	tkn_k_class_argument,	  188,	19,	  4,	"XmNoverrideRedirect"
    },
    {
	tkn_k_class_argument,	  189,	10,	  4,	"XmNpacking"
    },
    {
	tkn_k_class_reason,	   32,	24,	  5,	"XmNpageDecrementCallback"
    },
    {
	tkn_k_class_argument,	  190,	16,	  4,	"XmNpageIncrement"
    },
    {
	tkn_k_class_reason,	   33,	24,	  5,	"XmNpageIncrementCallback"
    },
    {
	tkn_k_class_argument,	  191,	14,	  4,	"XmNpaneMaximum"
    },
    {
	tkn_k_class_argument,	  192,	14,	  4,	"XmNpaneMinimum"
    },
    {
	tkn_k_class_argument,	  193,	10,	  4,	"XmNpattern"
    },
    {
	tkn_k_class_argument,	  194,	16,	  4,	"XmNpendingDelete"
    },
    {
	tkn_k_class_reason,	   34,	18,	  5,	"XmNpopdownCallback"
    },
    {
	tkn_k_class_reason,	   35,	16,	  5,	"XmNpopupCallback"
    },
    {
	tkn_k_class_argument,	  195,	15,	  4,	"XmNpopupEnabled"
    },
    {
	tkn_k_class_argument,	  196,	16,	  4,	"XmNpositionIndex"
    },
    {
	tkn_k_class_argument,	  197,	16,	  4,	"XmNpostFromCount"
    },
    {
	tkn_k_class_argument,	  198,	15,	  4,	"XmNpostFromList"
    },
    {
	tkn_k_class_argument,	  199,	14,	  4,	"XmNpreeditType"
    },
    {
	tkn_k_class_argument,	  200,	22,	  4,	"XmNprocessingDirection"
    },
    {
	tkn_k_class_argument,	  201,	15,	  4,	"XmNpromptString"
    },
    {
	tkn_k_class_argument,	  202,	20,	  4,	"XmNpushButtonEnabled"
    },
    {
	tkn_k_class_argument,	  203,	24,	  4,	"XmNqualifySearchDataProc"
    },
    {
	tkn_k_class_argument,	  204,	17,	  4,	"XmNradioAlwaysOne"
    },
    {
	tkn_k_class_argument,	  205,	16,	  4,	"XmNradioBehavior"
    },
    {
	tkn_k_class_argument,	  206,	16,	  4,	"XmNrecomputeSize"
    },
    {
	tkn_k_class_argument,	  207,	15,	  4,	"XmNrefigureMode"
    },
    {
	tkn_k_class_argument,	  208,	14,	  4,	"XmNrepeatDelay"
    },
    {
	tkn_k_class_argument,	  209,	12,	  4,	"XmNresizable"
    },
    {
	tkn_k_class_reason,	   36,	17,	  5,	"XmNresizeCallback"
    },
    {
	tkn_k_class_argument,	  210,	15,	  4,	"XmNresizeHeight"
    },
    {
	tkn_k_class_argument,	  211,	15,	  4,	"XmNresizePolicy"
    },
    {
	tkn_k_class_argument,	  212,	14,	  4,	"XmNresizeWidth"
    },
    {
	tkn_k_class_argument,	  213,	18,	  4,	"XmNrightAttachment"
    },
    {
	tkn_k_class_argument,	  214,	14,	  4,	"XmNrightOffset"
    },
    {
	tkn_k_class_argument,	  215,	16,	  4,	"XmNrightPosition"
    },
    {
	tkn_k_class_argument,	  216,	14,	  4,	"XmNrightWidget"
    },
    {
	tkn_k_class_argument,	  217,	16,	  4,	"XmNrowColumnType"
    },
    {
	tkn_k_class_argument,	  218,	 7,	  4,	"XmNrows"
    },
    {
	tkn_k_class_argument,	  219,	20,	  4,	"XmNrubberPositioning"
    },
    {
	tkn_k_class_argument,	  220,	13,	  4,	"XmNsashHeight"
    },
    {
	tkn_k_class_argument,	  221,	13,	  4,	"XmNsashIndent"
    },
    {
	tkn_k_class_argument,	  222,	22,	  4,	"XmNsashShadowThickness"
    },
    {
	tkn_k_class_argument,	  223,	12,	  4,	"XmNsashWidth"
    },
    {
	tkn_k_class_argument,	  224,	12,	  4,	"XmNsaveUnder"
    },
    {
	tkn_k_class_argument,	  225,	14,	  4,	"XmNscaleHeight"
    },
    {
	tkn_k_class_argument,	  226,	16,	  4,	"XmNscaleMultiple"
    },
    {
	tkn_k_class_argument,	  227,	13,	  4,	"XmNscaleWidth"
    },
    {
	tkn_k_class_argument,	  228,	 9,	  4,	"XmNscreen"
    },
    {
	tkn_k_class_argument,	  229,	25,	  4,	"XmNscrollBarDisplayPolicy"
    },
    {
	tkn_k_class_argument,	  230,	21,	  4,	"XmNscrollBarPlacement"
    },
    {
	tkn_k_class_argument,	  231,	19,	  4,	"XmNscrollHorizontal"
    },
    {
	tkn_k_class_argument,	  232,	17,	  4,	"XmNscrollLeftSide"
    },
    {
	tkn_k_class_argument,	  233,	16,	  4,	"XmNscrollTopSide"
    },
    {
	tkn_k_class_argument,	  234,	17,	  4,	"XmNscrollVertical"
    },
    {
	tkn_k_class_argument,	  235,	29,	  4,	"XmNscrolledWindowMarginHeight"
    },
    {
	tkn_k_class_argument,	  236,	28,	  4,	"XmNscrolledWindowMarginWidth"
    },
    {
	tkn_k_class_argument,	  237,	18,	  4,	"XmNscrollingPolicy"
    },
    {
	tkn_k_class_argument,	  238,	14,	  4,	"XmNselectColor"
    },
    {
	tkn_k_class_argument,	  239,	26,	  4,	"XmNselectInsensitivePixmap"
    },
    {
	tkn_k_class_argument,	  240,	15,	  4,	"XmNselectPixmap"
    },
    {
	tkn_k_class_argument,	  241,	18,	  4,	"XmNselectThreshold"
    },
    {
	tkn_k_class_argument,	  242,	20,	  4,	"XmNselectedItemCount"
    },
    {
	tkn_k_class_argument,	  243,	16,	  4,	"XmNselectedItems"
    },
    {
	tkn_k_class_argument,	  244,	17,	  4,	"XmNselectionArray"
    },
    {
	tkn_k_class_argument,	  245,	22,	  4,	"XmNselectionArrayCount"
    },
    {
	tkn_k_class_argument,	  246,	23,	  4,	"XmNselectionLabelString"
    },
    {
	tkn_k_class_argument,	  247,	18,	  4,	"XmNselectionPolicy"
    },
    {
	tkn_k_class_argument,	  248,	12,	  4,	"XmNsensitive"
    },
    {
	tkn_k_class_argument,	  249,	14,	  4,	"XmNseparatorOn"
    },
    {
	tkn_k_class_argument,	  250,	16,	  4,	"XmNseparatorType"
    },
    {
	tkn_k_class_argument,	  251,	 6,	  4,	"XmNset"
    },
    {
	tkn_k_class_argument,	  252,	18,	  4,	"XmNshadowThickness"
    },
    {
	tkn_k_class_argument,	  253,	13,	  4,	"XmNshadowType"
    },
    {
	tkn_k_class_argument,	  254,	16,	  4,	"XmNshellUnitType"
    },
    {
	tkn_k_class_argument,	  255,	13,	  4,	"XmNshowArrows"
    },
    {
	tkn_k_class_argument,	  256,	16,	  4,	"XmNshowAsDefault"
    },
    {
	tkn_k_class_argument,	  257,	16,	  4,	"XmNshowSeparator"
    },
    {
	tkn_k_class_argument,	  258,	12,	  4,	"XmNshowValue"
    },
    {
	tkn_k_class_reason,	   37,	26,	  5,	"XmNsingleSelectionCallback"
    },
    {
	tkn_k_class_argument,	  259,	13,	  4,	"XmNskipAdjust"
    },
    {
	tkn_k_class_argument,	  260,	13,	  4,	"XmNsliderSize"
    },
    {
	tkn_k_class_argument,	  261,	 9,	  4,	"XmNsource"
    },
    {
	tkn_k_class_argument,	  262,	10,	  4,	"XmNspacing"
    },
    {
	tkn_k_class_argument,	  263,	18,	  4,	"XmNstringDirection"
    },
    {
	tkn_k_class_argument,	  264,	12,	  4,	"XmNsubMenuId"
    },
    {
	tkn_k_class_argument,	  265,	15,	  4,	"XmNsymbolPixmap"
    },
    {
	tkn_k_class_reason,	   38,	30,	  5,	"XmNtearOffMenuActivateCallback"
    },
    {
	tkn_k_class_reason,	   39,	32,	  5,	"XmNtearOffMenuDeactivateCallback"
    },
    {
	tkn_k_class_argument,	  266,	15,	  4,	"XmNtearOffModel"
    },
    {
	tkn_k_class_argument,	  267,	19,	  4,	"XmNtextAccelerators"
    },
    {
	tkn_k_class_argument,	  268,	14,	  4,	"XmNtextColumns"
    },
    {
	tkn_k_class_argument,	  269,	15,	  4,	"XmNtextFontList"
    },
    {
	tkn_k_class_argument,	  270,	13,	  4,	"XmNtextString"
    },
    {
	tkn_k_class_argument,	  271,	19,	  4,	"XmNtextTranslations"
    },
    {
	tkn_k_class_argument,	  272,	 8,	  4,	"XmNtitle"
    },
    {
	tkn_k_class_argument,	  273,	16,	  4,	"XmNtitleEncoding"
    },
    {
	tkn_k_class_argument,	  274,	14,	  4,	"XmNtitleString"
    },
    {
	tkn_k_class_reason,	   40,	19,	  5,	"XmNtoBottomCallback"
    },
    {
	tkn_k_class_reason,	   41,	16,	  5,	"XmNtoTopCallback"
    },
    {
	tkn_k_class_argument,	  275,	16,	  4,	"XmNtopAttachment"
    },
    {
	tkn_k_class_argument,	  276,	15,	  4,	"XmNtopCharacter"
    },
    {
	tkn_k_class_argument,	  277,	18,	  4,	"XmNtopItemPosition"
    },
    {
	tkn_k_class_argument,	  278,	12,	  4,	"XmNtopOffset"
    },
    {
	tkn_k_class_argument,	  279,	14,	  4,	"XmNtopPosition"
    },
    {
	tkn_k_class_argument,	  280,	17,	  4,	"XmNtopShadowColor"
    },
    {
	tkn_k_class_argument,	  281,	18,	  4,	"XmNtopShadowPixmap"
    },
    {
	tkn_k_class_argument,	  282,	12,	  4,	"XmNtopWidget"
    },
    {
	tkn_k_class_argument,	  283,	12,	  4,	"XmNtransient"
    },
    {
	tkn_k_class_argument,	  284,	15,	  4,	"XmNtransientFor"
    },
    {
	tkn_k_class_argument,	  285,	15,	  4,	"XmNtranslations"
    },
    {
	tkn_k_class_reason,	   42,	20,	  5,	"XmNtraversalCallback"
    },
    {
	tkn_k_class_argument,	  286,	14,	  4,	"XmNtraversalOn"
    },
    {
	tkn_k_class_reason,	   43,	27,	  5,	"XmNtraverseObscuredCallback"
    },
    {
	tkn_k_class_argument,	  287,	14,	  4,	"XmNtroughColor"
    },
    {
	tkn_k_class_argument,	  288,	11,	  4,	"XmNunitType"
    },
    {
	tkn_k_class_reason,	   44,	16,	  5,	"XmNunmapCallback"
    },
    {
	tkn_k_class_argument,	  289,	17,	  4,	"XmNunpostBehavior"
    },
    {
	tkn_k_class_argument,	  290,	19,	  4,	"XmNuseAsyncGeometry"
    },
    {
	tkn_k_class_argument,	  291,	11,	  4,	"XmNuserData"
    },
    {
	tkn_k_class_argument,	  292,	 8,	  4,	"XmNvalue"
    },
    {
	tkn_k_class_reason,	   45,	23,	  5,	"XmNvalueChangedCallback"
    },
    {
	tkn_k_class_argument,	  293,	11,	  4,	"XmNvalueWcs"
    },
    {
	tkn_k_class_argument,	  294,	13,	  4,	"XmNverifyBell"
    },
    {
	tkn_k_class_argument,	  295,	20,	  4,	"XmNverticalScrollBar"
    },
    {
	tkn_k_class_argument,	  296,	18,	  4,	"XmNverticalSpacing"
    },
    {
	tkn_k_class_argument,	  297,	19,	  4,	"XmNvisibleItemCount"
    },
    {
	tkn_k_class_argument,	  298,	17,	  4,	"XmNvisibleWhenOff"
    },
    {
	tkn_k_class_argument,	  299,	 9,	  4,	"XmNvisual"
    },
    {
	tkn_k_class_argument,	  300,	15,	  4,	"XmNvisualPolicy"
    },
    {
	tkn_k_class_argument,	  301,	12,	  4,	"XmNwaitForWm"
    },
    {
	tkn_k_class_argument,	  302,	14,	  4,	"XmNwhichButton"
    },
    {
	tkn_k_class_argument,	  303,	 8,	  4,	"XmNwidth"
    },
    {
	tkn_k_class_argument,	  304,	11,	  4,	"XmNwidthInc"
    },
    {
	tkn_k_class_argument,	  305,	13,	  4,	"XmNwinGravity"
    },
    {
	tkn_k_class_argument,	  306,	14,	  4,	"XmNwindowGroup"
    },
    {
	tkn_k_class_argument,	  307,	12,	  4,	"XmNwmTimeout"
    },
    {
	tkn_k_class_argument,	  308,	11,	  4,	"XmNwordWrap"
    },
    {
	tkn_k_class_argument,	  309,	13,	  4,	"XmNworkWindow"
    },
    {
	tkn_k_class_argument,	  310,	 4,	  4,	"XmNx"
    },
    {
	tkn_k_class_argument,	  311,	 4,	  4,	"XmNy"
    },
    {
	tkn_k_class_enumval,	   92,	13,	  7,	"XmONE_OF_MANY"
    },
    {
	tkn_k_class_class,	   26,	12,	  8,	"XmOptionMenu"
    },
    {
	tkn_k_class_enumval,	   93,	13,	  7,	"XmPACK_COLUMN"
    },
    {
	tkn_k_class_enumval,	   94,	11,	  7,	"XmPACK_NONE"
    },
    {
	tkn_k_class_enumval,	   95,	12,	  7,	"XmPACK_TIGHT"
    },
    {
	tkn_k_class_enumval,	   96,	 8,	  7,	"XmPIXELS"
    },
    {
	tkn_k_class_enumval,	   97,	 8,	  7,	"XmPIXMAP"
    },
    {
	tkn_k_class_enumval,	   98,	23,	  7,	"XmPLACE_ABOVE_SELECTION"
    },
    {
	tkn_k_class_enumval,	   99,	23,	  7,	"XmPLACE_BELOW_SELECTION"
    },
    {
	tkn_k_class_enumval,	  100,	11,	  7,	"XmPLACE_TOP"
    },
    {
	tkn_k_class_enumval,	  101,	 9,	  7,	"XmPOINTER"
    },
    {
	tkn_k_class_class,	   27,	13,	  8,	"XmPanedWindow"
    },
    {
	tkn_k_class_class,	   28,	11,	  8,	"XmPopupMenu"
    },
    {
	tkn_k_class_class,	   29,	14,	  8,	"XmPromptDialog"
    },
    {
	tkn_k_class_class,	   30,	14,	  8,	"XmPulldownMenu"
    },
    {
	tkn_k_class_class,	   31,	12,	  8,	"XmPushButton"
    },
    {
	tkn_k_class_class,	   32,	18,	  8,	"XmPushButtonGadget"
    },
    {
	tkn_k_class_class,	   33,	16,	  8,	"XmQuestionDialog"
    },
    {
	tkn_k_class_enumval,	  102,	12,	  7,	"XmRESIZE_ANY"
    },
    {
	tkn_k_class_enumval,	  103,	13,	  7,	"XmRESIZE_GROW"
    },
    {
	tkn_k_class_enumval,	  104,	20,	  7,	"XmRESIZE_IF_POSSIBLE"
    },
    {
	tkn_k_class_enumval,	  105,	13,	  7,	"XmRESIZE_NONE"
    },
    {
	tkn_k_class_class,	   34,	10,	  8,	"XmRadioBox"
    },
    {
	tkn_k_class_class,	   35,	11,	  8,	"XmRowColumn"
    },
    {
	tkn_k_class_enumval,	  106,	18,	  7,	"XmSHADOW_ETCHED_IN"
    },
    {
	tkn_k_class_enumval,	  107,	23,	  7,	"XmSHADOW_ETCHED_IN_DASH"
    },
    {
	tkn_k_class_enumval,	  108,	19,	  7,	"XmSHADOW_ETCHED_OUT"
    },
    {
	tkn_k_class_enumval,	  109,	24,	  7,	"XmSHADOW_ETCHED_OUT_DASH"
    },
    {
	tkn_k_class_enumval,	  110,	11,	  7,	"XmSHADOW_IN"
    },
    {
	tkn_k_class_enumval,	  111,	12,	  7,	"XmSHADOW_OUT"
    },
    {
	tkn_k_class_enumval,	  112,	20,	  7,	"XmSINGLE_DASHED_LINE"
    },
    {
	tkn_k_class_enumval,	  113,	13,	  7,	"XmSINGLE_LINE"
    },
    {
	tkn_k_class_enumval,	  114,	18,	  7,	"XmSINGLE_LINE_EDIT"
    },
    {
	tkn_k_class_enumval,	  115,	15,	  7,	"XmSINGLE_SELECT"
    },
    {
	tkn_k_class_enumval,	  116,	 8,	  7,	"XmSTATIC"
    },
    {
	tkn_k_class_enumval,	  117,	18,	  7,	"XmSTICKY_TAB_GROUP"
    },
    {
	tkn_k_class_enumval,	  118,	 8,	  7,	"XmSTRING"
    },
    {
	tkn_k_class_enumval,	  119,	25,	  7,	"XmSTRING_DIRECTION_L_TO_R"
    },
    {
	tkn_k_class_enumval,	  120,	25,	  7,	"XmSTRING_DIRECTION_R_TO_L"
    },
    {
	tkn_k_class_class,	   36,	 7,	  8,	"XmScale"
    },
    {
	tkn_k_class_class,	   37,	11,	  8,	"XmScrollBar"
    },
    {
	tkn_k_class_class,	   38,	14,	  8,	"XmScrolledList"
    },
    {
	tkn_k_class_class,	   39,	14,	  8,	"XmScrolledText"
    },
    {
	tkn_k_class_class,	   40,	16,	  8,	"XmScrolledWindow"
    },
    {
	tkn_k_class_class,	   41,	14,	  8,	"XmSelectionBox"
    },
    {
	tkn_k_class_class,	   42,	17,	  8,	"XmSelectionDialog"
    },
    {
	tkn_k_class_class,	   43,	11,	  8,	"XmSeparator"
    },
    {
	tkn_k_class_class,	   44,	17,	  8,	"XmSeparatorGadget"
    },
    {
	tkn_k_class_enumval,	  121,	11,	  7,	"XmTAB_GROUP"
    },
    {
	tkn_k_class_enumval,	  122,	19,	  7,	"XmTEAR_OFF_DISABLED"
    },
    {
	tkn_k_class_enumval,	  123,	18,	  7,	"XmTEAR_OFF_ENABLED"
    },
    {
	tkn_k_class_enumval,	  124,	10,	  7,	"XmTOP_LEFT"
    },
    {
	tkn_k_class_enumval,	  125,	11,	  7,	"XmTOP_RIGHT"
    },
    {
	tkn_k_class_class,	   45,	15,	  8,	"XmTearOffButton"
    },
    {
	tkn_k_class_class,	   46,	16,	  8,	"XmTemplateDialog"
    },
    {
	tkn_k_class_class,	   47,	 6,	  8,	"XmText"
    },
    {
	tkn_k_class_class,	   48,	11,	  8,	"XmTextField"
    },
    {
	tkn_k_class_class,	   49,	14,	  8,	"XmToggleButton"
    },
    {
	tkn_k_class_class,	   50,	20,	  8,	"XmToggleButtonGadget"
    },
    {
	tkn_k_class_enumval,	  126,	 7,	  7,	"XmUNMAP"
    },
    {
	tkn_k_class_enumval,	  127,	 8,	  7,	"XmUNPOST"
    },
    {
	tkn_k_class_enumval,	  128,	19,	  7,	"XmUNPOST_AND_REPLAY"
    },
    {
	tkn_k_class_enumval,	  129,	10,	  7,	"XmVARIABLE"
    },
    {
	tkn_k_class_enumval,	  130,	10,	  7,	"XmVERTICAL"
    },
    {
	tkn_k_class_enumval,	  131,	11,	  7,	"XmWORK_AREA"
    },
    {
	tkn_k_class_class,	   51,	15,	  8,	"XmWarningDialog"
    },
    {
	tkn_k_class_class,	   52,	10,	  8,	"XmWorkArea"
    },
    {
	tkn_k_class_class,	   53,	15,	  8,	"XmWorkingDialog"
    },
    {
	tkn_k_class_child,	    1,	 8,	 96,	"Xm_Apply"
    },
    {
	tkn_k_class_child,	    2,	 9,	 96,	"Xm_Cancel"
    },
    {
	tkn_k_class_child,	    3,	 6,	 96,	"Xm_Dir"
    },
    {
	tkn_k_class_child,	    4,	10,	 96,	"Xm_DirList"
    },
    {
	tkn_k_class_child,	    5,	 9,	 96,	"Xm_Filter"
    },
    {
	tkn_k_class_child,	    6,	14,	 96,	"Xm_FilterLabel"
    },
    {
	tkn_k_class_child,	    7,	13,	 96,	"Xm_FilterText"
    },
    {
	tkn_k_class_child,	    8,	 7,	 96,	"Xm_Help"
    },
    {
	tkn_k_class_child,	    9,	15,	 96,	"Xm_HorScrollBar"
    },
    {
	tkn_k_class_child,	   10,	 8,	 96,	"Xm_Items"
    },
    {
	tkn_k_class_child,	   11,	12,	 96,	"Xm_ItemsList"
    },
    {
	tkn_k_class_child,	   12,	10,	 96,	"Xm_Message"
    },
    {
	tkn_k_class_child,	   13,	 5,	 96,	"Xm_OK"
    },
    {
	tkn_k_class_child,	   14,	15,	 96,	"Xm_OptionButton"
    },
    {
	tkn_k_class_child,	   15,	14,	 96,	"Xm_OptionLabel"
    },
    {
	tkn_k_class_child,	   16,	12,	 96,	"Xm_Selection"
    },
    {
	tkn_k_class_child,	   17,	12,	 96,	"Xm_Separator"
    },
    {
	tkn_k_class_child,	   18,	13,	 96,	"Xm_Separator1"
    },
    {
	tkn_k_class_child,	   19,	13,	 96,	"Xm_Separator2"
    },
    {
	tkn_k_class_child,	   20,	13,	 96,	"Xm_Separator3"
    },
    {
	tkn_k_class_child,	   21,	 9,	 96,	"Xm_Symbol"
    },
    {
	tkn_k_class_child,	   22,	17,	 96,	"Xm_TearOffControl"
    },
    {
	tkn_k_class_child,	   23,	 7,	 96,	"Xm_Text"
    },
    {
	tkn_k_class_child,	   24,	 8,	 96,	"Xm_Title"
    },
    {
	tkn_k_class_child,	   25,	16,	 96,	"Xm_VertScrollBar"
    },
    {
	tkn_k_class_keyword,	    0,	 3,	 71,	"any"
    },
    {
	tkn_k_class_keyword,	    0,	 8,	 52,	"argument"
    },
    {
	tkn_k_class_reserved,	    0,	 9,	 42,	"arguments"
    },
    {
	tkn_k_class_keyword,	    0,	18,	 84,	"asciz_string_table"
    },
    {
	tkn_k_class_keyword,	    0,	11,	 82,	"asciz_table"
    },
    {
	tkn_k_class_keyword,	    0,	10,	 76,	"background"
    },
    {
	tkn_k_class_charset,	    2,	 4,	  6,	"big5"
    },
    {
	tkn_k_class_keyword,	    0,	 7,	 73,	"boolean"
    },
    {
	tkn_k_class_reserved,	    0,	 9,	 43,	"callbacks"
    },
    {
	tkn_k_class_keyword,	    0,	16,	 59,	"case_insensitive"
    },
    {
	tkn_k_class_keyword,	    0,	14,	 58,	"case_sensitive"
    },
    {
	tkn_k_class_keyword,	    0,	13,	 57,	"character_set"
    },
    {
	tkn_k_class_keyword,	    0,	14,	 81,	"class_rec_name"
    },
    {
	tkn_k_class_keyword,	    0,	 5,	 55,	"color"
    },
    {
	tkn_k_class_keyword,	    0,	11,	 78,	"color_table"
    },
    {
	tkn_k_class_keyword,	    0,	15,	 69,	"compound_string"
    },
    {
	tkn_k_class_keyword,	    0,	21,	 85,	"compound_string_table"
    },
    {
	tkn_k_class_reserved,	    0,	 8,	 45,	"controls"
    },
    {
	tkn_k_class_reserved,	    0,	 3,	 33,	"end"
    },
    {
	tkn_k_class_charset,	    3,	 3,	  6,	"euc"
    },
    {
	tkn_k_class_reserved,	    0,	 8,	 65,	"exported"
    },
    {
	tkn_k_class_reserved,	    0,	 5,	 36,	"false"
    },
    {
	tkn_k_class_keyword,	    0,	 4,	 66,	"file"
    },
    {
	tkn_k_class_keyword,	    0,	 5,	 79,	"float"
    },
    {
	tkn_k_class_keyword,	    0,	 4,	 51,	"font"
    },
    {
	tkn_k_class_keyword,	    0,	10,	 70,	"font_table"
    },
    {
	tkn_k_class_keyword,	    0,	 7,	 95,	"fontset"
    },
    {
	tkn_k_class_keyword,	    0,	10,	 77,	"foreground"
    },
    {
	tkn_k_class_reserved,	    0,	 6,	 50,	"gadget"
    },
    {
	tkn_k_class_charset,	    4,	10,	  6,	"gb_chinese"
    },
    {
	tkn_k_class_charset,	    4,	 8,	  6,	"gb_hanzi"
    },
    {
	tkn_k_class_charset,	    4,	11,	  6,	"gb_hanzi_gl"
    },
    {
	tkn_k_class_charset,	    5,	11,	  6,	"gb_hanzi_gr"
    },
    {
	tkn_k_class_keyword,	    0,	 4,	 74,	"icon"
    },
    {
	tkn_k_class_reserved,	    0,	10,	 32,	"identifier"
    },
    {
	tkn_k_class_keyword,	    0,	 8,	 64,	"imported"
    },
    {
	tkn_k_class_reserved,	    0,	 7,	 37,	"include"
    },
    {
	tkn_k_class_keyword,	    0,	 7,	 80,	"integer"
    },
    {
	tkn_k_class_keyword,	    0,	13,	 83,	"integer_table"
    },
    {
	tkn_k_class_charset,	    6,	10,	  6,	"iso_arabic"
    },
    {
	tkn_k_class_charset,	    7,	12,	  6,	"iso_cyrillic"
    },
    {
	tkn_k_class_charset,	    8,	 9,	  6,	"iso_greek"
    },
    {
	tkn_k_class_charset,	    9,	10,	  6,	"iso_hebrew"
    },
    {
	tkn_k_class_charset,	   10,	13,	  6,	"iso_hebrew_lr"
    },
    {
	tkn_k_class_charset,	   11,	10,	  6,	"iso_latin1"
    },
    {
	tkn_k_class_charset,	   12,	10,	  6,	"iso_latin2"
    },
    {
	tkn_k_class_charset,	   13,	10,	  6,	"iso_latin3"
    },
    {
	tkn_k_class_charset,	   14,	10,	  6,	"iso_latin4"
    },
    {
	tkn_k_class_charset,	   15,	10,	  6,	"iso_latin5"
    },
    {
	tkn_k_class_charset,	    6,	10,	  6,	"iso_latin6"
    },
    {
	tkn_k_class_charset,	    8,	10,	  6,	"iso_latin7"
    },
    {
	tkn_k_class_charset,	    9,	10,	  6,	"iso_latin8"
    },
    {
	tkn_k_class_charset,	   10,	13,	  6,	"iso_latin8_lr"
    },
    {
	tkn_k_class_charset,	   16,	12,	  6,	"jis_japanese"
    },
    {
	tkn_k_class_charset,	   16,	 9,	  6,	"jis_kanji"
    },
    {
	tkn_k_class_charset,	   16,	12,	  6,	"jis_kanji_gl"
    },
    {
	tkn_k_class_charset,	   17,	12,	  6,	"jis_kanji_gr"
    },
    {
	tkn_k_class_charset,	   18,	12,	  6,	"jis_katakana"
    },
    {
	tkn_k_class_keyword,	    0,	 6,	 90,	"keysym"
    },
    {
	tkn_k_class_charset,	   19,	10,	  6,	"ksc_hangul"
    },
    {
	tkn_k_class_charset,	   19,	13,	  6,	"ksc_hangul_gl"
    },
    {
	tkn_k_class_charset,	   20,	13,	  6,	"ksc_hangul_gr"
    },
    {
	tkn_k_class_charset,	   19,	10,	  6,	"ksc_korean"
    },
    {
	tkn_k_class_reserved,	    0,	 4,	 31,	"list"
    },
    {
	tkn_k_class_reserved,	    0,	 5,	 38,	"macro"
    },
    {
	tkn_k_class_keyword,	    0,	 7,	 61,	"managed"
    },
    {
	tkn_k_class_reserved,	    0,	 6,	 34,	"module"
    },
    {
	tkn_k_class_keyword,	    0,	 5,	 56,	"names"
    },
    {
	tkn_k_class_reserved,	    0,	 6,	 47,	"object"
    },
    {
	tkn_k_class_keyword,	    0,	 7,	 48,	"objects"
    },
    {
	tkn_k_class_reserved,	    0,	 3,	 40,	"off"
    },
    {
	tkn_k_class_reserved,	    0,	 2,	 39,	"on"
    },
    {
	tkn_k_class_keyword,	    0,	 6,	 54,	"pixmap"
    },
    {
	tkn_k_class_reserved,	    0,	 7,	 63,	"private"
    },
    {
	tkn_k_class_reserved,	    0,	 9,	 46,	"procedure"
    },
    {
	tkn_k_class_reserved,	    0,	10,	 44,	"procedures"
    },
    {
	tkn_k_class_keyword,	    0,	 6,	 53,	"reason"
    },
    {
	tkn_k_class_keyword,	    0,	 3,	 92,	"rgb"
    },
    {
	tkn_k_class_keyword,	    0,	13,	 75,	"right_to_left"
    },
    {
	tkn_k_class_keyword,	    0,	 8,	 87,	"separate"
    },
    {
	tkn_k_class_keyword,	    0,	12,	 91,	"single_float"
    },
    {
	tkn_k_class_keyword,	    0,	11,	 88,	"sixteen_bit"
    },
    {
	tkn_k_class_keyword,	    0,	 6,	 72,	"string"
    },
    {
	tkn_k_class_keyword,	    0,	12,	 67,	"string_table"
    },
    {
	tkn_k_class_keyword,	    0,	17,	 68,	"translation_table"
    },
    {
	tkn_k_class_reserved,	    0,	 4,	 35,	"true"
    },
    {
	tkn_k_class_keyword,	    0,	 9,	 62,	"unmanaged"
    },
    {
	tkn_k_class_class,	   54,	12,	  8,	"user_defined"
    },
    {
	tkn_k_class_reserved,	    0,	 5,	 41,	"value"
    },
    {
	tkn_k_class_keyword,	    0,	 7,	 60,	"version"
    },
    {
	tkn_k_class_keyword,	    0,	14,	 93,	"wide_character"
    },
    {
	tkn_k_class_reserved,	    0,	 6,	 49,	"widget"
    },
    {
	tkn_k_class_keyword,	    0,	11,	 86,	"xbitmapfile"
    },
};

key_keytable_entry_type *key_table = priv_key_table;
/* 675 */
int key_k_keyword_count = sizeof(priv_key_table) / sizeof(key_keytable_entry_type);

int key_k_keyword_max_length = 34;

static key_keytable_entry_type
priv_key_table_case_ins[] = {
    {
	tkn_k_class_charset,	   11,	 5,	  6,	"88591"
    },
    {
	tkn_k_class_charset,	   12,	 5,	  6,	"88592"
    },
    {
	tkn_k_class_charset,	   13,	 5,	  6,	"88593"
    },
    {
	tkn_k_class_charset,	   14,	 5,	  6,	"88594"
    },
    {
	tkn_k_class_charset,	   15,	 5,	  6,	"88595"
    },
    {
	tkn_k_class_charset,	    6,	 5,	  6,	"88596"
    },
    {
	tkn_k_class_charset,	    8,	 5,	  6,	"88597"
    },
    {
	tkn_k_class_charset,	    9,	 5,	  6,	"88598"
    },
    {
	tkn_k_class_keyword,	    0,	 3,	 71,	"ANY"
    },
    {
	tkn_k_class_keyword,	    0,	 8,	 52,	"ARGUMENT"
    },
    {
	tkn_k_class_reserved,	    0,	 9,	 42,	"ARGUMENTS"
    },
    {
	tkn_k_class_charset,	   11,	 5,	  6,	"ASCII"
    },
    {
	tkn_k_class_keyword,	    0,	18,	 84,	"ASCIZ_STRING_TABLE"
    },
    {
	tkn_k_class_keyword,	    0,	11,	 82,	"ASCIZ_TABLE"
    },
    {
	tkn_k_class_keyword,	    0,	10,	 76,	"BACKGROUND"
    },
    {
	tkn_k_class_charset,	    2,	 4,	  6,	"BIG5"
    },
    {
	tkn_k_class_keyword,	    0,	 7,	 73,	"BOOLEAN"
    },
    {
	tkn_k_class_reserved,	    0,	 9,	 43,	"CALLBACKS"
    },
    {
	tkn_k_class_keyword,	    0,	16,	 59,	"CASE_INSENSITIVE"
    },
    {
	tkn_k_class_keyword,	    0,	14,	 58,	"CASE_SENSITIVE"
    },
    {
	tkn_k_class_keyword,	    0,	13,	 57,	"CHARACTER_SET"
    },
    {
	tkn_k_class_keyword,	    0,	14,	 81,	"CLASS_REC_NAME"
    },
    {
	tkn_k_class_keyword,	    0,	 5,	 55,	"COLOR"
    },
    {
	tkn_k_class_keyword,	    0,	11,	 78,	"COLOR_TABLE"
    },
    {
	tkn_k_class_keyword,	    0,	15,	 69,	"COMPOUND_STRING"
    },
    {
	tkn_k_class_keyword,	    0,	21,	 85,	"COMPOUND_STRING_TABLE"
    },
    {
	tkn_k_class_reserved,	    0,	 8,	 45,	"CONTROLS"
    },
    {
	tkn_k_class_reserved,	    0,	 3,	 33,	"END"
    },
    {
	tkn_k_class_charset,	    3,	 3,	  6,	"EUC"
    },
    {
	tkn_k_class_reserved,	    0,	 8,	 65,	"EXPORTED"
    },
    {
	tkn_k_class_reserved,	    0,	 5,	 36,	"FALSE"
    },
    {
	tkn_k_class_keyword,	    0,	 4,	 66,	"FILE"
    },
    {
	tkn_k_class_keyword,	    0,	 5,	 79,	"FLOAT"
    },
    {
	tkn_k_class_keyword,	    0,	 4,	 51,	"FONT"
    },
    {
	tkn_k_class_keyword,	    0,	 7,	 95,	"FONTSET"
    },
    {
	tkn_k_class_keyword,	    0,	10,	 70,	"FONT_TABLE"
    },
    {
	tkn_k_class_keyword,	    0,	10,	 77,	"FOREGROUND"
    },
    {
	tkn_k_class_reserved,	    0,	 6,	 50,	"GADGET"
    },
    {
	tkn_k_class_charset,	    4,	10,	  6,	"GB_CHINESE"
    },
    {
	tkn_k_class_charset,	    4,	 8,	  6,	"GB_HANZI"
    },
    {
	tkn_k_class_charset,	    4,	11,	  6,	"GB_HANZI_GL"
    },
    {
	tkn_k_class_charset,	    5,	11,	  6,	"GB_HANZI_GR"
    },
    {
	tkn_k_class_keyword,	    0,	 4,	 74,	"ICON"
    },
    {
	tkn_k_class_enumval,	    1,	11,	  7,	"ICONICSTATE"
    },
    {
	tkn_k_class_reserved,	    0,	10,	 32,	"IDENTIFIER"
    },
    {
	tkn_k_class_keyword,	    0,	 8,	 64,	"IMPORTED"
    },
    {
	tkn_k_class_reserved,	    0,	 7,	 37,	"INCLUDE"
    },
    {
	tkn_k_class_keyword,	    0,	 7,	 80,	"INTEGER"
    },
    {
	tkn_k_class_keyword,	    0,	13,	 83,	"INTEGER_TABLE"
    },
    {
	tkn_k_class_charset,	   11,	 9,	  6,	"ISOLATIN1"
    },
    {
	tkn_k_class_charset,	   12,	 9,	  6,	"ISOLATIN2"
    },
    {
	tkn_k_class_charset,	   13,	 9,	  6,	"ISOLATIN3"
    },
    {
	tkn_k_class_charset,	   14,	 9,	  6,	"ISOLATIN4"
    },
    {
	tkn_k_class_charset,	   15,	 9,	  6,	"ISOLATIN5"
    },
    {
	tkn_k_class_charset,	    6,	 9,	  6,	"ISOLATIN6"
    },
    {
	tkn_k_class_charset,	    8,	 9,	  6,	"ISOLATIN7"
    },
    {
	tkn_k_class_charset,	    9,	 9,	  6,	"ISOLATIN8"
    },
    {
	tkn_k_class_charset,	    6,	10,	  6,	"ISO_ARABIC"
    },
    {
	tkn_k_class_charset,	    7,	12,	  6,	"ISO_CYRILLIC"
    },
    {
	tkn_k_class_charset,	    8,	 9,	  6,	"ISO_GREEK"
    },
    {
	tkn_k_class_charset,	    9,	10,	  6,	"ISO_HEBREW"
    },
    {
	tkn_k_class_charset,	   10,	13,	  6,	"ISO_HEBREW_LR"
    },
    {
	tkn_k_class_charset,	   11,	10,	  6,	"ISO_LATIN1"
    },
    {
	tkn_k_class_charset,	   12,	10,	  6,	"ISO_LATIN2"
    },
    {
	tkn_k_class_charset,	   13,	10,	  6,	"ISO_LATIN3"
    },
    {
	tkn_k_class_charset,	   14,	10,	  6,	"ISO_LATIN4"
    },
    {
	tkn_k_class_charset,	   15,	10,	  6,	"ISO_LATIN5"
    },
    {
	tkn_k_class_charset,	    6,	10,	  6,	"ISO_LATIN6"
    },
    {
	tkn_k_class_charset,	    8,	10,	  6,	"ISO_LATIN7"
    },
    {
	tkn_k_class_charset,	    9,	10,	  6,	"ISO_LATIN8"
    },
    {
	tkn_k_class_charset,	   10,	13,	  6,	"ISO_LATIN8_LR"
    },
    {
	tkn_k_class_charset,	   16,	12,	  6,	"JIS_JAPANESE"
    },
    {
	tkn_k_class_charset,	   16,	 9,	  6,	"JIS_KANJI"
    },
    {
	tkn_k_class_charset,	   16,	12,	  6,	"JIS_KANJI_GL"
    },
    {
	tkn_k_class_charset,	   17,	12,	  6,	"JIS_KANJI_GR"
    },
    {
	tkn_k_class_charset,	   18,	12,	  6,	"JIS_KATAKANA"
    },
    {
	tkn_k_class_keyword,	    0,	 6,	 90,	"KEYSYM"
    },
    {
	tkn_k_class_charset,	   19,	10,	  6,	"KSC_HANGUL"
    },
    {
	tkn_k_class_charset,	   19,	13,	  6,	"KSC_HANGUL_GL"
    },
    {
	tkn_k_class_charset,	   20,	13,	  6,	"KSC_HANGUL_GR"
    },
    {
	tkn_k_class_charset,	   19,	10,	  6,	"KSC_KOREAN"
    },
    {
	tkn_k_class_reserved,	    0,	 4,	 31,	"LIST"
    },
    {
	tkn_k_class_reserved,	    0,	 5,	 38,	"MACRO"
    },
    {
	tkn_k_class_keyword,	    0,	 7,	 61,	"MANAGED"
    },
    {
	tkn_k_class_reserved,	    0,	 6,	 34,	"MODULE"
    },
    {
	tkn_k_class_reason,	    1,	18,	  5,	"MRMNCREATECALLBACK"
    },
    {
	tkn_k_class_keyword,	    0,	 5,	 56,	"NAMES"
    },
    {
	tkn_k_class_enumval,	    2,	11,	  7,	"NORMALSTATE"
    },
    {
	tkn_k_class_reserved,	    0,	 6,	 47,	"OBJECT"
    },
    {
	tkn_k_class_keyword,	    0,	 7,	 48,	"OBJECTS"
    },
    {
	tkn_k_class_reserved,	    0,	 3,	 40,	"OFF"
    },
    {
	tkn_k_class_reserved,	    0,	 2,	 39,	"ON"
    },
    {
	tkn_k_class_keyword,	    0,	 6,	 54,	"PIXMAP"
    },
    {
	tkn_k_class_reserved,	    0,	 7,	 63,	"PRIVATE"
    },
    {
	tkn_k_class_reserved,	    0,	 9,	 46,	"PROCEDURE"
    },
    {
	tkn_k_class_reserved,	    0,	10,	 44,	"PROCEDURES"
    },
    {
	tkn_k_class_keyword,	    0,	 6,	 53,	"REASON"
    },
    {
	tkn_k_class_keyword,	    0,	 3,	 92,	"RGB"
    },
    {
	tkn_k_class_keyword,	    0,	13,	 75,	"RIGHT_TO_LEFT"
    },
    {
	tkn_k_class_keyword,	    0,	 8,	 87,	"SEPARATE"
    },
    {
	tkn_k_class_keyword,	    0,	12,	 91,	"SINGLE_FLOAT"
    },
    {
	tkn_k_class_keyword,	    0,	11,	 88,	"SIXTEEN_BIT"
    },
    {
	tkn_k_class_keyword,	    0,	 6,	 72,	"STRING"
    },
    {
	tkn_k_class_keyword,	    0,	12,	 67,	"STRING_TABLE"
    },
    {
	tkn_k_class_keyword,	    0,	17,	 68,	"TRANSLATION_TABLE"
    },
    {
	tkn_k_class_reserved,	    0,	 4,	 35,	"TRUE"
    },
    {
	tkn_k_class_keyword,	    0,	 9,	 62,	"UNMANAGED"
    },
    {
	tkn_k_class_class,	   54,	12,	  8,	"USER_DEFINED"
    },
    {
	tkn_k_class_reserved,	    0,	 5,	 41,	"VALUE"
    },
    {
	tkn_k_class_keyword,	    0,	 7,	 60,	"VERSION"
    },
    {
	tkn_k_class_keyword,	    0,	14,	 93,	"WIDE_CHARACTER"
    },
    {
	tkn_k_class_reserved,	    0,	 6,	 49,	"WIDGET"
    },
    {
	tkn_k_class_keyword,	    0,	11,	 86,	"XBITMAPFILE"
    },
    {
	tkn_k_class_enumval,	    3,	15,	  7,	"XM1000TH_INCHES"
    },
    {
	tkn_k_class_enumval,	    4,	18,	  7,	"XM100TH_FONT_UNITS"
    },
    {
	tkn_k_class_enumval,	    5,	19,	  7,	"XM100TH_MILLIMETERS"
    },
    {
	tkn_k_class_enumval,	    6,	14,	  7,	"XM100TH_POINTS"
    },
    {
	tkn_k_class_enumval,	    7,	27,	  7,	"XMALIGNMENT_BASELINE_BOTTOM"
    },
    {
	tkn_k_class_enumval,	    8,	24,	  7,	"XMALIGNMENT_BASELINE_TOP"
    },
    {
	tkn_k_class_enumval,	    9,	21,	  7,	"XMALIGNMENT_BEGINNING"
    },
    {
	tkn_k_class_enumval,	   10,	18,	  7,	"XMALIGNMENT_CENTER"
    },
    {
	tkn_k_class_enumval,	   11,	27,	  7,	"XMALIGNMENT_CONTENTS_BOTTOM"
    },
    {
	tkn_k_class_enumval,	   12,	24,	  7,	"XMALIGNMENT_CONTENTS_TOP"
    },
    {
	tkn_k_class_enumval,	   13,	15,	  7,	"XMALIGNMENT_END"
    },
    {
	tkn_k_class_enumval,	   14,	25,	  7,	"XMALIGNMENT_WIDGET_BOTTOM"
    },
    {
	tkn_k_class_enumval,	   15,	22,	  7,	"XMALIGNMENT_WIDGET_TOP"
    },
    {
	tkn_k_class_enumval,	   16,	21,	  7,	"XMAPPLICATION_DEFINED"
    },
    {
	tkn_k_class_class,	    1,	13,	  8,	"XMARROWBUTTON"
    },
    {
	tkn_k_class_class,	    2,	19,	  8,	"XMARROWBUTTONGADGET"
    },
    {
	tkn_k_class_enumval,	   17,	12,	  7,	"XMARROW_DOWN"
    },
    {
	tkn_k_class_enumval,	   18,	12,	  7,	"XMARROW_LEFT"
    },
    {
	tkn_k_class_enumval,	   19,	13,	  7,	"XMARROW_RIGHT"
    },
    {
	tkn_k_class_enumval,	   20,	10,	  7,	"XMARROW_UP"
    },
    {
	tkn_k_class_enumval,	   21,	11,	  7,	"XMAS_NEEDED"
    },
    {
	tkn_k_class_enumval,	   22,	13,	  7,	"XMATTACH_FORM"
    },
    {
	tkn_k_class_enumval,	   23,	13,	  7,	"XMATTACH_NONE"
    },
    {
	tkn_k_class_enumval,	   24,	22,	  7,	"XMATTACH_OPPOSITE_FORM"
    },
    {
	tkn_k_class_enumval,	   25,	24,	  7,	"XMATTACH_OPPOSITE_WIDGET"
    },
    {
	tkn_k_class_enumval,	   26,	17,	  7,	"XMATTACH_POSITION"
    },
    {
	tkn_k_class_enumval,	   27,	13,	  7,	"XMATTACH_SELF"
    },
    {
	tkn_k_class_enumval,	   28,	15,	  7,	"XMATTACH_WIDGET"
    },
    {
	tkn_k_class_enumval,	   29,	11,	  7,	"XMAUTOMATIC"
    },
    {
	tkn_k_class_enumval,	   30,	 6,	  7,	"XMBELL"
    },
    {
	tkn_k_class_enumval,	   31,	13,	  7,	"XMBOTTOM_LEFT"
    },
    {
	tkn_k_class_enumval,	   32,	14,	  7,	"XMBOTTOM_RIGHT"
    },
    {
	tkn_k_class_enumval,	   33,	15,	  7,	"XMBROWSE_SELECT"
    },
    {
	tkn_k_class_class,	    3,	15,	  8,	"XMBULLETINBOARD"
    },
    {
	tkn_k_class_class,	    4,	21,	  8,	"XMBULLETINBOARDDIALOG"
    },
    {
	tkn_k_class_class,	    5,	15,	  8,	"XMCASCADEBUTTON"
    },
    {
	tkn_k_class_class,	    6,	21,	  8,	"XMCASCADEBUTTONGADGET"
    },
    {
	tkn_k_class_class,	    7,	 9,	  8,	"XMCOMMAND"
    },
    {
	tkn_k_class_enumval,	   34,	25,	  7,	"XMCOMMAND_ABOVE_WORKSPACE"
    },
    {
	tkn_k_class_enumval,	   35,	25,	  7,	"XMCOMMAND_BELOW_WORKSPACE"
    },
    {
	tkn_k_class_enumval,	   36,	10,	  7,	"XMCONSTANT"
    },
    {
	tkn_k_class_enumval,	   37,	 9,	  7,	"XMDESTROY"
    },
    {
	tkn_k_class_class,	    8,	13,	  8,	"XMDIALOGSHELL"
    },
    {
	tkn_k_class_enumval,	   38,	26,	  7,	"XMDIALOG_APPLICATION_MODAL"
    },
    {
	tkn_k_class_enumval,	   39,	22,	  7,	"XMDIALOG_CANCEL_BUTTON"
    },
    {
	tkn_k_class_enumval,	   40,	23,	  7,	"XMDIALOG_DEFAULT_BUTTON"
    },
    {
	tkn_k_class_enumval,	   41,	14,	  7,	"XMDIALOG_ERROR"
    },
    {
	tkn_k_class_enumval,	   42,	23,	  7,	"XMDIALOG_FILE_SELECTION"
    },
    {
	tkn_k_class_enumval,	   43,	31,	  7,	"XMDIALOG_FULL_APPLICATION_MODAL"
    },
    {
	tkn_k_class_enumval,	   44,	20,	  7,	"XMDIALOG_HELP_BUTTON"
    },
    {
	tkn_k_class_enumval,	   45,	20,	  7,	"XMDIALOG_INFORMATION"
    },
    {
	tkn_k_class_enumval,	   46,	16,	  7,	"XMDIALOG_MESSAGE"
    },
    {
	tkn_k_class_enumval,	   47,	22,	  7,	"XMDIALOG_MESSAGE_LABEL"
    },
    {
	tkn_k_class_enumval,	   48,	17,	  7,	"XMDIALOG_MODELESS"
    },
    {
	tkn_k_class_enumval,	   49,	18,	  7,	"XMDIALOG_OK_BUTTON"
    },
    {
	tkn_k_class_enumval,	   50,	34,	  7,	"XMDIALOG_PRIMARY_APPLICATION_MODAL"
    },
    {
	tkn_k_class_enumval,	   51,	15,	  7,	"XMDIALOG_PROMPT"
    },
    {
	tkn_k_class_enumval,	   52,	17,	  7,	"XMDIALOG_QUESTION"
    },
    {
	tkn_k_class_enumval,	   53,	18,	  7,	"XMDIALOG_SELECTION"
    },
    {
	tkn_k_class_enumval,	   54,	18,	  7,	"XMDIALOG_SEPARATOR"
    },
    {
	tkn_k_class_enumval,	   55,	21,	  7,	"XMDIALOG_SYMBOL_LABEL"
    },
    {
	tkn_k_class_enumval,	   56,	21,	  7,	"XMDIALOG_SYSTEM_MODAL"
    },
    {
	tkn_k_class_enumval,	   57,	17,	  7,	"XMDIALOG_TEMPLATE"
    },
    {
	tkn_k_class_enumval,	   58,	16,	  7,	"XMDIALOG_WARNING"
    },
    {
	tkn_k_class_enumval,	   59,	16,	  7,	"XMDIALOG_WORKING"
    },
    {
	tkn_k_class_enumval,	   60,	18,	  7,	"XMDIALOG_WORK_AREA"
    },
    {
	tkn_k_class_enumval,	   61,	20,	  7,	"XMDOUBLE_DASHED_LINE"
    },
    {
	tkn_k_class_enumval,	   62,	13,	  7,	"XMDOUBLE_LINE"
    },
    {
	tkn_k_class_enumval,	   63,	12,	  7,	"XMDO_NOTHING"
    },
    {
	tkn_k_class_class,	    9,	13,	  8,	"XMDRAWINGAREA"
    },
    {
	tkn_k_class_class,	   10,	13,	  8,	"XMDRAWNBUTTON"
    },
    {
	tkn_k_class_class,	   11,	13,	  8,	"XMERRORDIALOG"
    },
    {
	tkn_k_class_enumval,	   64,	21,	  7,	"XMEXCLUSIVE_TAB_GROUP"
    },
    {
	tkn_k_class_enumval,	   65,	10,	  7,	"XMEXPLICIT"
    },
    {
	tkn_k_class_enumval,	   66,	17,	  7,	"XMEXTENDED_SELECT"
    },
    {
	tkn_k_class_class,	   12,	18,	  8,	"XMFILESELECTIONBOX"
    },
    {
	tkn_k_class_class,	   13,	21,	  8,	"XMFILESELECTIONDIALOG"
    },
    {
	tkn_k_class_enumval,	   67,	16,	  7,	"XMFILE_DIRECTORY"
    },
    {
	tkn_k_class_enumval,	   68,	14,	  7,	"XMFILE_REGULAR"
    },
    {
	tkn_k_class_enumval,	   69,	16,	  7,	"XMFIRST_POSITION"
    },
    {
	tkn_k_class_class,	   14,	 6,	  8,	"XMFORM"
    },
    {
	tkn_k_class_class,	   15,	12,	  8,	"XMFORMDIALOG"
    },
    {
	tkn_k_class_class,	   16,	 7,	  8,	"XMFRAME"
    },
    {
	tkn_k_class_enumval,	   70,	21,	  7,	"XMFRAME_GENERIC_CHILD"
    },
    {
	tkn_k_class_enumval,	   71,	19,	  7,	"XMFRAME_TITLE_CHILD"
    },
    {
	tkn_k_class_enumval,	   72,	22,	  7,	"XMFRAME_WORKAREA_CHILD"
    },
    {
	tkn_k_class_enumval,	   73,	12,	  7,	"XMHORIZONTAL"
    },
    {
	tkn_k_class_class,	   17,	19,	  8,	"XMINFORMATIONDIALOG"
    },
    {
	tkn_k_class_class,	   18,	 7,	  8,	"XMLABEL"
    },
    {
	tkn_k_class_class,	   19,	13,	  8,	"XMLABELGADGET"
    },
    {
	tkn_k_class_enumval,	   74,	15,	  7,	"XMLAST_POSITION"
    },
    {
	tkn_k_class_class,	   20,	 6,	  8,	"XMLIST"
    },
    {
	tkn_k_class_class,	   21,	12,	  8,	"XMMAINWINDOW"
    },
    {
	tkn_k_class_enumval,	   75,	15,	  7,	"XMMAX_ON_BOTTOM"
    },
    {
	tkn_k_class_enumval,	   76,	13,	  7,	"XMMAX_ON_LEFT"
    },
    {
	tkn_k_class_enumval,	   77,	14,	  7,	"XMMAX_ON_RIGHT"
    },
    {
	tkn_k_class_enumval,	   78,	12,	  7,	"XMMAX_ON_TOP"
    },
    {
	tkn_k_class_class,	   22,	 9,	  8,	"XMMENUBAR"
    },
    {
	tkn_k_class_class,	   23,	11,	  8,	"XMMENUSHELL"
    },
    {
	tkn_k_class_enumval,	   79,	10,	  7,	"XMMENU_BAR"
    },
    {
	tkn_k_class_enumval,	   80,	13,	  7,	"XMMENU_OPTION"
    },
    {
	tkn_k_class_enumval,	   81,	12,	  7,	"XMMENU_POPUP"
    },
    {
	tkn_k_class_enumval,	   82,	15,	  7,	"XMMENU_PULLDOWN"
    },
    {
	tkn_k_class_class,	   24,	12,	  8,	"XMMESSAGEBOX"
    },
    {
	tkn_k_class_class,	   25,	15,	  8,	"XMMESSAGEDIALOG"
    },
    {
	tkn_k_class_enumval,	   83,	20,	  7,	"XMMULTICLICK_DISCARD"
    },
    {
	tkn_k_class_enumval,	   84,	17,	  7,	"XMMULTICLICK_KEEP"
    },
    {
	tkn_k_class_enumval,	   85,	17,	  7,	"XMMULTIPLE_SELECT"
    },
    {
	tkn_k_class_enumval,	   86,	17,	  7,	"XMMULTI_LINE_EDIT"
    },
    {
	tkn_k_class_argument,	    1,	14,	  4,	"XMNACCELERATOR"
    },
    {
	tkn_k_class_argument,	    3,	15,	  4,	"XMNACCELERATORS"
    },
    {
	tkn_k_class_argument,	    2,	18,	  4,	"XMNACCELERATORTEXT"
    },
    {
	tkn_k_class_reason,	    2,	19,	  5,	"XMNACTIVATECALLBACK"
    },
    {
	tkn_k_class_argument,	    4,	13,	  4,	"XMNADJUSTLAST"
    },
    {
	tkn_k_class_argument,	    5,	15,	  4,	"XMNADJUSTMARGIN"
    },
    {
	tkn_k_class_argument,	    6,	12,	  4,	"XMNALIGNMENT"
    },
    {
	tkn_k_class_argument,	    7,	15,	  4,	"XMNALLOWOVERLAP"
    },
    {
	tkn_k_class_argument,	    8,	14,	  4,	"XMNALLOWRESIZE"
    },
    {
	tkn_k_class_argument,	    9,	19,	  4,	"XMNALLOWSHELLRESIZE"
    },
    {
	tkn_k_class_argument,	   10,	20,	  4,	"XMNANCESTORSENSITIVE"
    },
    {
	tkn_k_class_reason,	    3,	16,	  5,	"XMNAPPLYCALLBACK"
    },
    {
	tkn_k_class_argument,	   11,	19,	  4,	"XMNAPPLYLABELSTRING"
    },
    {
	tkn_k_class_reason,	    4,	14,	  5,	"XMNARMCALLBACK"
    },
    {
	tkn_k_class_argument,	   12,	11,	  4,	"XMNARMCOLOR"
    },
    {
	tkn_k_class_argument,	   13,	12,	  4,	"XMNARMPIXMAP"
    },
    {
	tkn_k_class_argument,	   14,	17,	  4,	"XMNARROWDIRECTION"
    },
    {
	tkn_k_class_argument,	   15,	17,	  4,	"XMNAUDIBLEWARNING"
    },
    {
	tkn_k_class_argument,	   18,	21,	  4,	"XMNAUTOMATICSELECTION"
    },
    {
	tkn_k_class_argument,	   16,	25,	  4,	"XMNAUTOSHOWCURSORPOSITION"
    },
    {
	tkn_k_class_argument,	   17,	15,	  4,	"XMNAUTOUNMANAGE"
    },
    {
	tkn_k_class_argument,	   19,	13,	  4,	"XMNBACKGROUND"
    },
    {
	tkn_k_class_argument,	   20,	19,	  4,	"XMNBACKGROUNDPIXMAP"
    },
    {
	tkn_k_class_argument,	   21,	13,	  4,	"XMNBASEHEIGHT"
    },
    {
	tkn_k_class_argument,	   22,	12,	  4,	"XMNBASEWIDTH"
    },
    {
	tkn_k_class_argument,	   23,	12,	  4,	"XMNBLINKRATE"
    },
    {
	tkn_k_class_argument,	   24,	14,	  4,	"XMNBORDERCOLOR"
    },
    {
	tkn_k_class_argument,	   25,	15,	  4,	"XMNBORDERPIXMAP"
    },
    {
	tkn_k_class_argument,	   26,	14,	  4,	"XMNBORDERWIDTH"
    },
    {
	tkn_k_class_argument,	   27,	19,	  4,	"XMNBOTTOMATTACHMENT"
    },
    {
	tkn_k_class_argument,	   28,	15,	  4,	"XMNBOTTOMOFFSET"
    },
    {
	tkn_k_class_argument,	   29,	17,	  4,	"XMNBOTTOMPOSITION"
    },
    {
	tkn_k_class_argument,	   30,	20,	  4,	"XMNBOTTOMSHADOWCOLOR"
    },
    {
	tkn_k_class_argument,	   31,	21,	  4,	"XMNBOTTOMSHADOWPIXMAP"
    },
    {
	tkn_k_class_argument,	   32,	15,	  4,	"XMNBOTTOMWIDGET"
    },
    {
	tkn_k_class_reason,	    5,	26,	  5,	"XMNBROWSESELECTIONCALLBACK"
    },
    {
	tkn_k_class_argument,	   33,	17,	  4,	"XMNBUTTONFONTLIST"
    },
    {
	tkn_k_class_argument,	   34,	15,	  4,	"XMNCANCELBUTTON"
    },
    {
	tkn_k_class_reason,	    6,	17,	  5,	"XMNCANCELCALLBACK"
    },
    {
	tkn_k_class_argument,	   35,	20,	  4,	"XMNCANCELLABELSTRING"
    },
    {
	tkn_k_class_argument,	   36,	16,	  4,	"XMNCASCADEPIXMAP"
    },
    {
	tkn_k_class_reason,	    7,	20,	  5,	"XMNCASCADINGCALLBACK"
    },
    {
	tkn_k_class_argument,	   37,	27,	  4,	"XMNCHILDHORIZONTALALIGNMENT"
    },
    {
	tkn_k_class_argument,	   38,	25,	  4,	"XMNCHILDHORIZONTALSPACING"
    },
    {
	tkn_k_class_argument,	   39,	17,	  4,	"XMNCHILDPLACEMENT"
    },
    {
	tkn_k_class_argument,	   40,	12,	  4,	"XMNCHILDTYPE"
    },
    {
	tkn_k_class_argument,	   41,	25,	  4,	"XMNCHILDVERTICALALIGNMENT"
    },
    {
	tkn_k_class_argument,	   42,	13,	  4,	"XMNCLIPWINDOW"
    },
    {
	tkn_k_class_argument,	   43,	11,	  4,	"XMNCOLORMAP"
    },
    {
	tkn_k_class_argument,	   44,	10,	  4,	"XMNCOLUMNS"
    },
    {
	tkn_k_class_argument,	   45,	10,	  4,	"XMNCOMMAND"
    },
    {
	tkn_k_class_reason,	    8,	25,	  5,	"XMNCOMMANDCHANGEDCALLBACK"
    },
    {
	tkn_k_class_reason,	    9,	25,	  5,	"XMNCOMMANDENTEREDCALLBACK"
    },
    {
	tkn_k_class_argument,	   46,	16,	  4,	"XMNCOMMANDWINDOW"
    },
    {
	tkn_k_class_argument,	   47,	24,	  4,	"XMNCOMMANDWINDOWLOCATION"
    },
    {
	tkn_k_class_argument,	   48,	23,	  4,	"XMNCREATEPOPUPCHILDPROC"
    },
    {
	tkn_k_class_argument,	   49,	17,	  4,	"XMNCURSORPOSITION"
    },
    {
	tkn_k_class_argument,	   50,	24,	  4,	"XMNCURSORPOSITIONVISIBLE"
    },
    {
	tkn_k_class_argument,	   51,	16,	  4,	"XMNDECIMALPOINTS"
    },
    {
	tkn_k_class_reason,	   10,	20,	  5,	"XMNDECREMENTCALLBACK"
    },
    {
	tkn_k_class_reason,	   11,	24,	  5,	"XMNDEFAULTACTIONCALLBACK"
    },
    {
	tkn_k_class_argument,	   52,	16,	  4,	"XMNDEFAULTBUTTON"
    },
    {
	tkn_k_class_argument,	   53,	31,	  4,	"XMNDEFAULTBUTTONSHADOWTHICKNESS"
    },
    {
	tkn_k_class_argument,	   54,	20,	  4,	"XMNDEFAULTBUTTONTYPE"
    },
    {
	tkn_k_class_argument,	   55,	18,	  4,	"XMNDEFAULTFONTLIST"
    },
    {
	tkn_k_class_argument,	   56,	18,	  4,	"XMNDEFAULTPOSITION"
    },
    {
	tkn_k_class_argument,	   57,	17,	  4,	"XMNDELETERESPONSE"
    },
    {
	tkn_k_class_argument,	   58,	 8,	  4,	"XMNDEPTH"
    },
    {
	tkn_k_class_reason,	   12,	18,	  5,	"XMNDESTROYCALLBACK"
    },
    {
	tkn_k_class_argument,	   59,	14,	  4,	"XMNDIALOGSTYLE"
    },
    {
	tkn_k_class_argument,	   60,	14,	  4,	"XMNDIALOGTITLE"
    },
    {
	tkn_k_class_argument,	   61,	13,	  4,	"XMNDIALOGTYPE"
    },
    {
	tkn_k_class_argument,	   68,	12,	  4,	"XMNDIRECTORY"
    },
    {
	tkn_k_class_argument,	   62,	19,	  4,	"XMNDIRLISTITEMCOUNT"
    },
    {
	tkn_k_class_argument,	   63,	15,	  4,	"XMNDIRLISTITEMS"
    },
    {
	tkn_k_class_argument,	   64,	21,	  4,	"XMNDIRLISTLABELSTRING"
    },
    {
	tkn_k_class_argument,	   65,	10,	  4,	"XMNDIRMASK"
    },
    {
	tkn_k_class_argument,	   66,	16,	  4,	"XMNDIRSEARCHPROC"
    },
    {
	tkn_k_class_argument,	   67,	10,	  4,	"XMNDIRSPEC"
    },
    {
	tkn_k_class_reason,	   13,	17,	  5,	"XMNDISARMCALLBACK"
    },
    {
	tkn_k_class_argument,	   69,	22,	  4,	"XMNDOUBLECLICKINTERVAL"
    },
    {
	tkn_k_class_reason,	   14,	15,	  5,	"XMNDRAGCALLBACK"
    },
    {
	tkn_k_class_argument,	   71,	11,	  4,	"XMNEDITABLE"
    },
    {
	tkn_k_class_argument,	   70,	11,	  4,	"XMNEDITMODE"
    },
    {
	tkn_k_class_argument,	   72,	17,	  4,	"XMNENTRYALIGNMENT"
    },
    {
	tkn_k_class_argument,	   73,	14,	  4,	"XMNENTRYBORDER"
    },
    {
	tkn_k_class_reason,	   15,	16,	  5,	"XMNENTRYCALLBACK"
    },
    {
	tkn_k_class_argument,	   74,	13,	  4,	"XMNENTRYCLASS"
    },
    {
	tkn_k_class_argument,	   75,	25,	  4,	"XMNENTRYVERTICALALIGNMENT"
    },
    {
	tkn_k_class_reason,	   16,	17,	  5,	"XMNEXPOSECALLBACK"
    },
    {
	tkn_k_class_reason,	   17,	28,	  5,	"XMNEXTENDEDSELECTIONCALLBACK"
    },
    {
	tkn_k_class_argument,	   76,	20,	  4,	"XMNFILELISTITEMCOUNT"
    },
    {
	tkn_k_class_argument,	   77,	16,	  4,	"XMNFILELISTITEMS"
    },
    {
	tkn_k_class_argument,	   78,	22,	  4,	"XMNFILELISTLABELSTRING"
    },
    {
	tkn_k_class_argument,	   79,	17,	  4,	"XMNFILESEARCHPROC"
    },
    {
	tkn_k_class_argument,	   80,	15,	  4,	"XMNFILETYPEMASK"
    },
    {
	tkn_k_class_argument,	   81,	12,	  4,	"XMNFILLONARM"
    },
    {
	tkn_k_class_argument,	   82,	15,	  4,	"XMNFILLONSELECT"
    },
    {
	tkn_k_class_argument,	   83,	20,	  4,	"XMNFILTERLABELSTRING"
    },
    {
	tkn_k_class_reason,	   18,	16,	  5,	"XMNFOCUSCALLBACK"
    },
    {
	tkn_k_class_argument,	   84,	11,	  4,	"XMNFONTLIST"
    },
    {
	tkn_k_class_argument,	   85,	13,	  4,	"XMNFOREGROUND"
    },
    {
	tkn_k_class_argument,	   86,	15,	  4,	"XMNFRACTIONBASE"
    },
    {
	tkn_k_class_reason,	   19,	22,	  5,	"XMNGAINPRIMARYCALLBACK"
    },
    {
	tkn_k_class_argument,	   87,	11,	  4,	"XMNGEOMETRY"
    },
    {
	tkn_k_class_argument,	   88,	 9,	  4,	"XMNHEIGHT"
    },
    {
	tkn_k_class_argument,	   89,	12,	  4,	"XMNHEIGHTINC"
    },
    {
	tkn_k_class_reason,	   20,	15,	  5,	"XMNHELPCALLBACK"
    },
    {
	tkn_k_class_argument,	   90,	18,	  4,	"XMNHELPLABELSTRING"
    },
    {
	tkn_k_class_argument,	   91,	17,	  4,	"XMNHIGHLIGHTCOLOR"
    },
    {
	tkn_k_class_argument,	   92,	19,	  4,	"XMNHIGHLIGHTONENTER"
    },
    {
	tkn_k_class_argument,	   93,	18,	  4,	"XMNHIGHLIGHTPIXMAP"
    },
    {
	tkn_k_class_argument,	   94,	21,	  4,	"XMNHIGHLIGHTTHICKNESS"
    },
    {
	tkn_k_class_argument,	   95,	19,	  4,	"XMNHISTORYITEMCOUNT"
    },
    {
	tkn_k_class_argument,	   96,	15,	  4,	"XMNHISTORYITEMS"
    },
    {
	tkn_k_class_argument,	   97,	18,	  4,	"XMNHISTORYMAXITEMS"
    },
    {
	tkn_k_class_argument,	   98,	26,	  4,	"XMNHISTORYVISIBLEITEMCOUNT"
    },
    {
	tkn_k_class_argument,	   99,	22,	  4,	"XMNHORIZONTALSCROLLBAR"
    },
    {
	tkn_k_class_argument,	  100,	20,	  4,	"XMNHORIZONTALSPACING"
    },
    {
	tkn_k_class_argument,	  108,	 9,	  4,	"XMNICONIC"
    },
    {
	tkn_k_class_argument,	  101,	11,	  4,	"XMNICONMASK"
    },
    {
	tkn_k_class_argument,	  102,	11,	  4,	"XMNICONNAME"
    },
    {
	tkn_k_class_argument,	  103,	19,	  4,	"XMNICONNAMEENCODING"
    },
    {
	tkn_k_class_argument,	  104,	13,	  4,	"XMNICONPIXMAP"
    },
    {
	tkn_k_class_argument,	  105,	13,	  4,	"XMNICONWINDOW"
    },
    {
	tkn_k_class_argument,	  106,	 8,	  4,	"XMNICONX"
    },
    {
	tkn_k_class_argument,	  107,	 8,	  4,	"XMNICONY"
    },
    {
	tkn_k_class_argument,	  109,	12,	  4,	"XMNINCREMENT"
    },
    {
	tkn_k_class_reason,	   21,	20,	  5,	"XMNINCREMENTCALLBACK"
    },
    {
	tkn_k_class_argument,	  110,	14,	  4,	"XMNINDICATORON"
    },
    {
	tkn_k_class_argument,	  111,	16,	  4,	"XMNINDICATORSIZE"
    },
    {
	tkn_k_class_argument,	  112,	16,	  4,	"XMNINDICATORTYPE"
    },
    {
	tkn_k_class_argument,	  113,	15,	  4,	"XMNINITIALDELAY"
    },
    {
	tkn_k_class_argument,	  114,	15,	  4,	"XMNINITIALFOCUS"
    },
    {
	tkn_k_class_argument,	  115,	29,	  4,	"XMNINITIALRESOURCESPERSISTENT"
    },
    {
	tkn_k_class_argument,	  116,	15,	  4,	"XMNINITIALSTATE"
    },
    {
	tkn_k_class_argument,	  117,	 8,	  4,	"XMNINPUT"
    },
    {
	tkn_k_class_reason,	   22,	16,	  5,	"XMNINPUTCALLBACK"
    },
    {
	tkn_k_class_argument,	  118,	14,	  4,	"XMNINPUTMETHOD"
    },
    {
	tkn_k_class_argument,	  119,	17,	  4,	"XMNINSERTPOSITION"
    },
    {
	tkn_k_class_argument,	  120,	12,	  4,	"XMNISALIGNED"
    },
    {
	tkn_k_class_argument,	  121,	16,	  4,	"XMNISHOMOGENEOUS"
    },
    {
	tkn_k_class_argument,	  122,	12,	  4,	"XMNITEMCOUNT"
    },
    {
	tkn_k_class_argument,	  123,	 8,	  4,	"XMNITEMS"
    },
    {
	tkn_k_class_argument,	  124,	22,	  4,	"XMNKEYBOARDFOCUSPOLICY"
    },
    {
	tkn_k_class_argument,	  125,	16,	  4,	"XMNLABELFONTLIST"
    },
    {
	tkn_k_class_argument,	  126,	25,	  4,	"XMNLABELINSENSITIVEPIXMAP"
    },
    {
	tkn_k_class_argument,	  127,	14,	  4,	"XMNLABELPIXMAP"
    },
    {
	tkn_k_class_argument,	  128,	14,	  4,	"XMNLABELSTRING"
    },
    {
	tkn_k_class_argument,	  129,	12,	  4,	"XMNLABELTYPE"
    },
    {
	tkn_k_class_argument,	  130,	17,	  4,	"XMNLEFTATTACHMENT"
    },
    {
	tkn_k_class_argument,	  131,	13,	  4,	"XMNLEFTOFFSET"
    },
    {
	tkn_k_class_argument,	  132,	15,	  4,	"XMNLEFTPOSITION"
    },
    {
	tkn_k_class_argument,	  133,	13,	  4,	"XMNLEFTWIDGET"
    },
    {
	tkn_k_class_argument,	  134,	16,	  4,	"XMNLISTITEMCOUNT"
    },
    {
	tkn_k_class_argument,	  135,	12,	  4,	"XMNLISTITEMS"
    },
    {
	tkn_k_class_argument,	  136,	18,	  4,	"XMNLISTLABELSTRING"
    },
    {
	tkn_k_class_argument,	  137,	19,	  4,	"XMNLISTMARGINHEIGHT"
    },
    {
	tkn_k_class_argument,	  138,	18,	  4,	"XMNLISTMARGINWIDTH"
    },
    {
	tkn_k_class_argument,	  139,	17,	  4,	"XMNLISTSIZEPOLICY"
    },
    {
	tkn_k_class_argument,	  140,	14,	  4,	"XMNLISTSPACING"
    },
    {
	tkn_k_class_argument,	  141,	14,	  4,	"XMNLISTUPDATED"
    },
    {
	tkn_k_class_argument,	  142,	23,	  4,	"XMNLISTVISIBLEITEMCOUNT"
    },
    {
	tkn_k_class_reason,	   23,	22,	  5,	"XMNLOSEPRIMARYCALLBACK"
    },
    {
	tkn_k_class_reason,	   24,	22,	  5,	"XMNLOSINGFOCUSCALLBACK"
    },
    {
	tkn_k_class_argument,	  143,	25,	  4,	"XMNMAINWINDOWMARGINHEIGHT"
    },
    {
	tkn_k_class_argument,	  144,	24,	  4,	"XMNMAINWINDOWMARGINWIDTH"
    },
    {
	tkn_k_class_reason,	   25,	14,	  5,	"XMNMAPCALLBACK"
    },
    {
	tkn_k_class_argument,	  145,	20,	  4,	"XMNMAPPEDWHENMANAGED"
    },
    {
	tkn_k_class_argument,	  146,	15,	  4,	"XMNMAPPINGDELAY"
    },
    {
	tkn_k_class_argument,	  147,	 9,	  4,	"XMNMARGIN"
    },
    {
	tkn_k_class_argument,	  148,	15,	  4,	"XMNMARGINBOTTOM"
    },
    {
	tkn_k_class_argument,	  149,	15,	  4,	"XMNMARGINHEIGHT"
    },
    {
	tkn_k_class_argument,	  150,	13,	  4,	"XMNMARGINLEFT"
    },
    {
	tkn_k_class_argument,	  151,	14,	  4,	"XMNMARGINRIGHT"
    },
    {
	tkn_k_class_argument,	  152,	12,	  4,	"XMNMARGINTOP"
    },
    {
	tkn_k_class_argument,	  153,	14,	  4,	"XMNMARGINWIDTH"
    },
    {
	tkn_k_class_argument,	  154,	13,	  4,	"XMNMAXASPECTX"
    },
    {
	tkn_k_class_argument,	  155,	13,	  4,	"XMNMAXASPECTY"
    },
    {
	tkn_k_class_argument,	  156,	12,	  4,	"XMNMAXHEIGHT"
    },
    {
	tkn_k_class_argument,	  159,	10,	  4,	"XMNMAXIMUM"
    },
    {
	tkn_k_class_argument,	  157,	12,	  4,	"XMNMAXLENGTH"
    },
    {
	tkn_k_class_argument,	  158,	11,	  4,	"XMNMAXWIDTH"
    },
    {
	tkn_k_class_argument,	  160,	18,	  4,	"XMNMENUACCELERATOR"
    },
    {
	tkn_k_class_argument,	  161,	10,	  4,	"XMNMENUBAR"
    },
    {
	tkn_k_class_argument,	  162,	17,	  4,	"XMNMENUHELPWIDGET"
    },
    {
	tkn_k_class_argument,	  163,	14,	  4,	"XMNMENUHISTORY"
    },
    {
	tkn_k_class_argument,	  164,	11,	  4,	"XMNMENUPOST"
    },
    {
	tkn_k_class_argument,	  165,	19,	  4,	"XMNMESSAGEALIGNMENT"
    },
    {
	tkn_k_class_argument,	  166,	16,	  4,	"XMNMESSAGESTRING"
    },
    {
	tkn_k_class_argument,	  167,	16,	  4,	"XMNMESSAGEWINDOW"
    },
    {
	tkn_k_class_argument,	  168,	13,	  4,	"XMNMINASPECTX"
    },
    {
	tkn_k_class_argument,	  169,	13,	  4,	"XMNMINASPECTY"
    },
    {
	tkn_k_class_argument,	  170,	12,	  4,	"XMNMINHEIGHT"
    },
    {
	tkn_k_class_argument,	  172,	18,	  4,	"XMNMINIMIZEBUTTONS"
    },
    {
	tkn_k_class_argument,	  173,	10,	  4,	"XMNMINIMUM"
    },
    {
	tkn_k_class_argument,	  171,	11,	  4,	"XMNMINWIDTH"
    },
    {
	tkn_k_class_argument,	  174,	11,	  4,	"XMNMNEMONIC"
    },
    {
	tkn_k_class_argument,	  175,	18,	  4,	"XMNMNEMONICCHARSET"
    },
    {
	tkn_k_class_reason,	   26,	23,	  5,	"XMNMODIFYVERIFYCALLBACK"
    },
    {
	tkn_k_class_reason,	   27,	26,	  5,	"XMNMODIFYVERIFYCALLBACKWCS"
    },
    {
	tkn_k_class_reason,	   28,	23,	  5,	"XMNMOTIONVERIFYCALLBACK"
    },
    {
	tkn_k_class_argument,	  176,	13,	  4,	"XMNMULTICLICK"
    },
    {
	tkn_k_class_reason,	   29,	28,	  5,	"XMNMULTIPLESELECTIONCALLBACK"
    },
    {
	tkn_k_class_argument,	  177,	12,	  4,	"XMNMUSTMATCH"
    },
    {
	tkn_k_class_argument,	  178,	17,	  4,	"XMNMWMDECORATIONS"
    },
    {
	tkn_k_class_argument,	  179,	15,	  4,	"XMNMWMFUNCTIONS"
    },
    {
	tkn_k_class_argument,	  180,	15,	  4,	"XMNMWMINPUTMODE"
    },
    {
	tkn_k_class_argument,	  181,	10,	  4,	"XMNMWMMENU"
    },
    {
	tkn_k_class_argument,	  182,	17,	  4,	"XMNNAVIGATIONTYPE"
    },
    {
	tkn_k_class_reason,	   30,	18,	  5,	"XMNNOMATCHCALLBACK"
    },
    {
	tkn_k_class_argument,	  183,	16,	  4,	"XMNNOMATCHSTRING"
    },
    {
	tkn_k_class_argument,	  184,	11,	  4,	"XMNNORESIZE"
    },
    {
	tkn_k_class_argument,	  185,	13,	  4,	"XMNNUMCOLUMNS"
    },
    {
	tkn_k_class_reason,	   31,	13,	  5,	"XMNOKCALLBACK"
    },
    {
	tkn_k_class_argument,	  186,	16,	  4,	"XMNOKLABELSTRING"
    },
    {
	tkn_k_class_enumval,	   87,	 6,	  7,	"XMNONE"
    },
    {
	tkn_k_class_argument,	  187,	14,	  4,	"XMNORIENTATION"
    },
    {
	tkn_k_class_argument,	  188,	19,	  4,	"XMNOVERRIDEREDIRECT"
    },
    {
	tkn_k_class_enumval,	   88,	 9,	  7,	"XMNO_LINE"
    },
    {
	tkn_k_class_enumval,	   89,	16,	  7,	"XMNO_ORIENTATION"
    },
    {
	tkn_k_class_enumval,	   90,	12,	  7,	"XMNO_PACKING"
    },
    {
	tkn_k_class_argument,	  189,	10,	  4,	"XMNPACKING"
    },
    {
	tkn_k_class_reason,	   32,	24,	  5,	"XMNPAGEDECREMENTCALLBACK"
    },
    {
	tkn_k_class_argument,	  190,	16,	  4,	"XMNPAGEINCREMENT"
    },
    {
	tkn_k_class_reason,	   33,	24,	  5,	"XMNPAGEINCREMENTCALLBACK"
    },
    {
	tkn_k_class_argument,	  191,	14,	  4,	"XMNPANEMAXIMUM"
    },
    {
	tkn_k_class_argument,	  192,	14,	  4,	"XMNPANEMINIMUM"
    },
    {
	tkn_k_class_argument,	  193,	10,	  4,	"XMNPATTERN"
    },
    {
	tkn_k_class_argument,	  194,	16,	  4,	"XMNPENDINGDELETE"
    },
    {
	tkn_k_class_reason,	   34,	18,	  5,	"XMNPOPDOWNCALLBACK"
    },
    {
	tkn_k_class_reason,	   35,	16,	  5,	"XMNPOPUPCALLBACK"
    },
    {
	tkn_k_class_argument,	  195,	15,	  4,	"XMNPOPUPENABLED"
    },
    {
	tkn_k_class_argument,	  196,	16,	  4,	"XMNPOSITIONINDEX"
    },
    {
	tkn_k_class_argument,	  197,	16,	  4,	"XMNPOSTFROMCOUNT"
    },
    {
	tkn_k_class_argument,	  198,	15,	  4,	"XMNPOSTFROMLIST"
    },
    {
	tkn_k_class_argument,	  199,	14,	  4,	"XMNPREEDITTYPE"
    },
    {
	tkn_k_class_argument,	  200,	22,	  4,	"XMNPROCESSINGDIRECTION"
    },
    {
	tkn_k_class_argument,	  201,	15,	  4,	"XMNPROMPTSTRING"
    },
    {
	tkn_k_class_argument,	  202,	20,	  4,	"XMNPUSHBUTTONENABLED"
    },
    {
	tkn_k_class_argument,	  203,	24,	  4,	"XMNQUALIFYSEARCHDATAPROC"
    },
    {
	tkn_k_class_argument,	  204,	17,	  4,	"XMNRADIOALWAYSONE"
    },
    {
	tkn_k_class_argument,	  205,	16,	  4,	"XMNRADIOBEHAVIOR"
    },
    {
	tkn_k_class_argument,	  206,	16,	  4,	"XMNRECOMPUTESIZE"
    },
    {
	tkn_k_class_argument,	  207,	15,	  4,	"XMNREFIGUREMODE"
    },
    {
	tkn_k_class_argument,	  208,	14,	  4,	"XMNREPEATDELAY"
    },
    {
	tkn_k_class_argument,	  209,	12,	  4,	"XMNRESIZABLE"
    },
    {
	tkn_k_class_reason,	   36,	17,	  5,	"XMNRESIZECALLBACK"
    },
    {
	tkn_k_class_argument,	  210,	15,	  4,	"XMNRESIZEHEIGHT"
    },
    {
	tkn_k_class_argument,	  211,	15,	  4,	"XMNRESIZEPOLICY"
    },
    {
	tkn_k_class_argument,	  212,	14,	  4,	"XMNRESIZEWIDTH"
    },
    {
	tkn_k_class_argument,	  213,	18,	  4,	"XMNRIGHTATTACHMENT"
    },
    {
	tkn_k_class_argument,	  214,	14,	  4,	"XMNRIGHTOFFSET"
    },
    {
	tkn_k_class_argument,	  215,	16,	  4,	"XMNRIGHTPOSITION"
    },
    {
	tkn_k_class_argument,	  216,	14,	  4,	"XMNRIGHTWIDGET"
    },
    {
	tkn_k_class_argument,	  217,	16,	  4,	"XMNROWCOLUMNTYPE"
    },
    {
	tkn_k_class_argument,	  218,	 7,	  4,	"XMNROWS"
    },
    {
	tkn_k_class_argument,	  219,	20,	  4,	"XMNRUBBERPOSITIONING"
    },
    {
	tkn_k_class_argument,	  220,	13,	  4,	"XMNSASHHEIGHT"
    },
    {
	tkn_k_class_argument,	  221,	13,	  4,	"XMNSASHINDENT"
    },
    {
	tkn_k_class_argument,	  222,	22,	  4,	"XMNSASHSHADOWTHICKNESS"
    },
    {
	tkn_k_class_argument,	  223,	12,	  4,	"XMNSASHWIDTH"
    },
    {
	tkn_k_class_argument,	  224,	12,	  4,	"XMNSAVEUNDER"
    },
    {
	tkn_k_class_argument,	  225,	14,	  4,	"XMNSCALEHEIGHT"
    },
    {
	tkn_k_class_argument,	  226,	16,	  4,	"XMNSCALEMULTIPLE"
    },
    {
	tkn_k_class_argument,	  227,	13,	  4,	"XMNSCALEWIDTH"
    },
    {
	tkn_k_class_argument,	  228,	 9,	  4,	"XMNSCREEN"
    },
    {
	tkn_k_class_argument,	  229,	25,	  4,	"XMNSCROLLBARDISPLAYPOLICY"
    },
    {
	tkn_k_class_argument,	  230,	21,	  4,	"XMNSCROLLBARPLACEMENT"
    },
    {
	tkn_k_class_argument,	  235,	29,	  4,	"XMNSCROLLEDWINDOWMARGINHEIGHT"
    },
    {
	tkn_k_class_argument,	  236,	28,	  4,	"XMNSCROLLEDWINDOWMARGINWIDTH"
    },
    {
	tkn_k_class_argument,	  231,	19,	  4,	"XMNSCROLLHORIZONTAL"
    },
    {
	tkn_k_class_argument,	  237,	18,	  4,	"XMNSCROLLINGPOLICY"
    },
    {
	tkn_k_class_argument,	  232,	17,	  4,	"XMNSCROLLLEFTSIDE"
    },
    {
	tkn_k_class_argument,	  233,	16,	  4,	"XMNSCROLLTOPSIDE"
    },
    {
	tkn_k_class_argument,	  234,	17,	  4,	"XMNSCROLLVERTICAL"
    },
    {
	tkn_k_class_argument,	  238,	14,	  4,	"XMNSELECTCOLOR"
    },
    {
	tkn_k_class_argument,	  242,	20,	  4,	"XMNSELECTEDITEMCOUNT"
    },
    {
	tkn_k_class_argument,	  243,	16,	  4,	"XMNSELECTEDITEMS"
    },
    {
	tkn_k_class_argument,	  239,	26,	  4,	"XMNSELECTINSENSITIVEPIXMAP"
    },
    {
	tkn_k_class_argument,	  244,	17,	  4,	"XMNSELECTIONARRAY"
    },
    {
	tkn_k_class_argument,	  245,	22,	  4,	"XMNSELECTIONARRAYCOUNT"
    },
    {
	tkn_k_class_argument,	  246,	23,	  4,	"XMNSELECTIONLABELSTRING"
    },
    {
	tkn_k_class_argument,	  247,	18,	  4,	"XMNSELECTIONPOLICY"
    },
    {
	tkn_k_class_argument,	  240,	15,	  4,	"XMNSELECTPIXMAP"
    },
    {
	tkn_k_class_argument,	  241,	18,	  4,	"XMNSELECTTHRESHOLD"
    },
    {
	tkn_k_class_argument,	  248,	12,	  4,	"XMNSENSITIVE"
    },
    {
	tkn_k_class_argument,	  249,	14,	  4,	"XMNSEPARATORON"
    },
    {
	tkn_k_class_argument,	  250,	16,	  4,	"XMNSEPARATORTYPE"
    },
    {
	tkn_k_class_argument,	  251,	 6,	  4,	"XMNSET"
    },
    {
	tkn_k_class_argument,	  252,	18,	  4,	"XMNSHADOWTHICKNESS"
    },
    {
	tkn_k_class_argument,	  253,	13,	  4,	"XMNSHADOWTYPE"
    },
    {
	tkn_k_class_argument,	  254,	16,	  4,	"XMNSHELLUNITTYPE"
    },
    {
	tkn_k_class_argument,	  255,	13,	  4,	"XMNSHOWARROWS"
    },
    {
	tkn_k_class_argument,	  256,	16,	  4,	"XMNSHOWASDEFAULT"
    },
    {
	tkn_k_class_argument,	  257,	16,	  4,	"XMNSHOWSEPARATOR"
    },
    {
	tkn_k_class_argument,	  258,	12,	  4,	"XMNSHOWVALUE"
    },
    {
	tkn_k_class_reason,	   37,	26,	  5,	"XMNSINGLESELECTIONCALLBACK"
    },
    {
	tkn_k_class_argument,	  259,	13,	  4,	"XMNSKIPADJUST"
    },
    {
	tkn_k_class_argument,	  260,	13,	  4,	"XMNSLIDERSIZE"
    },
    {
	tkn_k_class_argument,	  261,	 9,	  4,	"XMNSOURCE"
    },
    {
	tkn_k_class_argument,	  262,	10,	  4,	"XMNSPACING"
    },
    {
	tkn_k_class_argument,	  263,	18,	  4,	"XMNSTRINGDIRECTION"
    },
    {
	tkn_k_class_argument,	  264,	12,	  4,	"XMNSUBMENUID"
    },
    {
	tkn_k_class_argument,	  265,	15,	  4,	"XMNSYMBOLPIXMAP"
    },
    {
	tkn_k_class_reason,	   38,	30,	  5,	"XMNTEAROFFMENUACTIVATECALLBACK"
    },
    {
	tkn_k_class_reason,	   39,	32,	  5,	"XMNTEAROFFMENUDEACTIVATECALLBACK"
    },
    {
	tkn_k_class_argument,	  266,	15,	  4,	"XMNTEAROFFMODEL"
    },
    {
	tkn_k_class_argument,	  267,	19,	  4,	"XMNTEXTACCELERATORS"
    },
    {
	tkn_k_class_argument,	  268,	14,	  4,	"XMNTEXTCOLUMNS"
    },
    {
	tkn_k_class_argument,	  269,	15,	  4,	"XMNTEXTFONTLIST"
    },
    {
	tkn_k_class_argument,	  270,	13,	  4,	"XMNTEXTSTRING"
    },
    {
	tkn_k_class_argument,	  271,	19,	  4,	"XMNTEXTTRANSLATIONS"
    },
    {
	tkn_k_class_argument,	  272,	 8,	  4,	"XMNTITLE"
    },
    {
	tkn_k_class_argument,	  273,	16,	  4,	"XMNTITLEENCODING"
    },
    {
	tkn_k_class_argument,	  274,	14,	  4,	"XMNTITLESTRING"
    },
    {
	tkn_k_class_reason,	   40,	19,	  5,	"XMNTOBOTTOMCALLBACK"
    },
    {
	tkn_k_class_argument,	  275,	16,	  4,	"XMNTOPATTACHMENT"
    },
    {
	tkn_k_class_argument,	  276,	15,	  4,	"XMNTOPCHARACTER"
    },
    {
	tkn_k_class_argument,	  277,	18,	  4,	"XMNTOPITEMPOSITION"
    },
    {
	tkn_k_class_argument,	  278,	12,	  4,	"XMNTOPOFFSET"
    },
    {
	tkn_k_class_argument,	  279,	14,	  4,	"XMNTOPPOSITION"
    },
    {
	tkn_k_class_argument,	  280,	17,	  4,	"XMNTOPSHADOWCOLOR"
    },
    {
	tkn_k_class_argument,	  281,	18,	  4,	"XMNTOPSHADOWPIXMAP"
    },
    {
	tkn_k_class_argument,	  282,	12,	  4,	"XMNTOPWIDGET"
    },
    {
	tkn_k_class_reason,	   41,	16,	  5,	"XMNTOTOPCALLBACK"
    },
    {
	tkn_k_class_argument,	  283,	12,	  4,	"XMNTRANSIENT"
    },
    {
	tkn_k_class_argument,	  284,	15,	  4,	"XMNTRANSIENTFOR"
    },
    {
	tkn_k_class_argument,	  285,	15,	  4,	"XMNTRANSLATIONS"
    },
    {
	tkn_k_class_reason,	   42,	20,	  5,	"XMNTRAVERSALCALLBACK"
    },
    {
	tkn_k_class_argument,	  286,	14,	  4,	"XMNTRAVERSALON"
    },
    {
	tkn_k_class_reason,	   43,	27,	  5,	"XMNTRAVERSEOBSCUREDCALLBACK"
    },
    {
	tkn_k_class_argument,	  287,	14,	  4,	"XMNTROUGHCOLOR"
    },
    {
	tkn_k_class_argument,	  288,	11,	  4,	"XMNUNITTYPE"
    },
    {
	tkn_k_class_reason,	   44,	16,	  5,	"XMNUNMAPCALLBACK"
    },
    {
	tkn_k_class_argument,	  289,	17,	  4,	"XMNUNPOSTBEHAVIOR"
    },
    {
	tkn_k_class_argument,	  290,	19,	  4,	"XMNUSEASYNCGEOMETRY"
    },
    {
	tkn_k_class_argument,	  291,	11,	  4,	"XMNUSERDATA"
    },
    {
	tkn_k_class_argument,	  292,	 8,	  4,	"XMNVALUE"
    },
    {
	tkn_k_class_reason,	   45,	23,	  5,	"XMNVALUECHANGEDCALLBACK"
    },
    {
	tkn_k_class_argument,	  293,	11,	  4,	"XMNVALUEWCS"
    },
    {
	tkn_k_class_argument,	  294,	13,	  4,	"XMNVERIFYBELL"
    },
    {
	tkn_k_class_argument,	  295,	20,	  4,	"XMNVERTICALSCROLLBAR"
    },
    {
	tkn_k_class_argument,	  296,	18,	  4,	"XMNVERTICALSPACING"
    },
    {
	tkn_k_class_argument,	  297,	19,	  4,	"XMNVISIBLEITEMCOUNT"
    },
    {
	tkn_k_class_argument,	  298,	17,	  4,	"XMNVISIBLEWHENOFF"
    },
    {
	tkn_k_class_argument,	  299,	 9,	  4,	"XMNVISUAL"
    },
    {
	tkn_k_class_argument,	  300,	15,	  4,	"XMNVISUALPOLICY"
    },
    {
	tkn_k_class_argument,	  301,	12,	  4,	"XMNWAITFORWM"
    },
    {
	tkn_k_class_argument,	  302,	14,	  4,	"XMNWHICHBUTTON"
    },
    {
	tkn_k_class_argument,	  303,	 8,	  4,	"XMNWIDTH"
    },
    {
	tkn_k_class_argument,	  304,	11,	  4,	"XMNWIDTHINC"
    },
    {
	tkn_k_class_argument,	  306,	14,	  4,	"XMNWINDOWGROUP"
    },
    {
	tkn_k_class_argument,	  305,	13,	  4,	"XMNWINGRAVITY"
    },
    {
	tkn_k_class_argument,	  307,	12,	  4,	"XMNWMTIMEOUT"
    },
    {
	tkn_k_class_argument,	  308,	11,	  4,	"XMNWORDWRAP"
    },
    {
	tkn_k_class_argument,	  309,	13,	  4,	"XMNWORKWINDOW"
    },
    {
	tkn_k_class_argument,	  310,	 4,	  4,	"XMNX"
    },
    {
	tkn_k_class_argument,	  311,	 4,	  4,	"XMNY"
    },
    {
	tkn_k_class_enumval,	   91,	11,	  7,	"XMN_OF_MANY"
    },
    {
	tkn_k_class_enumval,	   92,	13,	  7,	"XMONE_OF_MANY"
    },
    {
	tkn_k_class_class,	   26,	12,	  8,	"XMOPTIONMENU"
    },
    {
	tkn_k_class_enumval,	   93,	13,	  7,	"XMPACK_COLUMN"
    },
    {
	tkn_k_class_enumval,	   94,	11,	  7,	"XMPACK_NONE"
    },
    {
	tkn_k_class_enumval,	   95,	12,	  7,	"XMPACK_TIGHT"
    },
    {
	tkn_k_class_class,	   27,	13,	  8,	"XMPANEDWINDOW"
    },
    {
	tkn_k_class_enumval,	   96,	 8,	  7,	"XMPIXELS"
    },
    {
	tkn_k_class_enumval,	   97,	 8,	  7,	"XMPIXMAP"
    },
    {
	tkn_k_class_enumval,	   98,	23,	  7,	"XMPLACE_ABOVE_SELECTION"
    },
    {
	tkn_k_class_enumval,	   99,	23,	  7,	"XMPLACE_BELOW_SELECTION"
    },
    {
	tkn_k_class_enumval,	  100,	11,	  7,	"XMPLACE_TOP"
    },
    {
	tkn_k_class_enumval,	  101,	 9,	  7,	"XMPOINTER"
    },
    {
	tkn_k_class_class,	   28,	11,	  8,	"XMPOPUPMENU"
    },
    {
	tkn_k_class_class,	   29,	14,	  8,	"XMPROMPTDIALOG"
    },
    {
	tkn_k_class_class,	   30,	14,	  8,	"XMPULLDOWNMENU"
    },
    {
	tkn_k_class_class,	   31,	12,	  8,	"XMPUSHBUTTON"
    },
    {
	tkn_k_class_class,	   32,	18,	  8,	"XMPUSHBUTTONGADGET"
    },
    {
	tkn_k_class_class,	   33,	16,	  8,	"XMQUESTIONDIALOG"
    },
    {
	tkn_k_class_class,	   34,	10,	  8,	"XMRADIOBOX"
    },
    {
	tkn_k_class_enumval,	  102,	12,	  7,	"XMRESIZE_ANY"
    },
    {
	tkn_k_class_enumval,	  103,	13,	  7,	"XMRESIZE_GROW"
    },
    {
	tkn_k_class_enumval,	  104,	20,	  7,	"XMRESIZE_IF_POSSIBLE"
    },
    {
	tkn_k_class_enumval,	  105,	13,	  7,	"XMRESIZE_NONE"
    },
    {
	tkn_k_class_class,	   35,	11,	  8,	"XMROWCOLUMN"
    },
    {
	tkn_k_class_class,	   36,	 7,	  8,	"XMSCALE"
    },
    {
	tkn_k_class_class,	   37,	11,	  8,	"XMSCROLLBAR"
    },
    {
	tkn_k_class_class,	   38,	14,	  8,	"XMSCROLLEDLIST"
    },
    {
	tkn_k_class_class,	   39,	14,	  8,	"XMSCROLLEDTEXT"
    },
    {
	tkn_k_class_class,	   40,	16,	  8,	"XMSCROLLEDWINDOW"
    },
    {
	tkn_k_class_class,	   41,	14,	  8,	"XMSELECTIONBOX"
    },
    {
	tkn_k_class_class,	   42,	17,	  8,	"XMSELECTIONDIALOG"
    },
    {
	tkn_k_class_class,	   43,	11,	  8,	"XMSEPARATOR"
    },
    {
	tkn_k_class_class,	   44,	17,	  8,	"XMSEPARATORGADGET"
    },
    {
	tkn_k_class_enumval,	  106,	18,	  7,	"XMSHADOW_ETCHED_IN"
    },
    {
	tkn_k_class_enumval,	  107,	23,	  7,	"XMSHADOW_ETCHED_IN_DASH"
    },
    {
	tkn_k_class_enumval,	  108,	19,	  7,	"XMSHADOW_ETCHED_OUT"
    },
    {
	tkn_k_class_enumval,	  109,	24,	  7,	"XMSHADOW_ETCHED_OUT_DASH"
    },
    {
	tkn_k_class_enumval,	  110,	11,	  7,	"XMSHADOW_IN"
    },
    {
	tkn_k_class_enumval,	  111,	12,	  7,	"XMSHADOW_OUT"
    },
    {
	tkn_k_class_enumval,	  112,	20,	  7,	"XMSINGLE_DASHED_LINE"
    },
    {
	tkn_k_class_enumval,	  113,	13,	  7,	"XMSINGLE_LINE"
    },
    {
	tkn_k_class_enumval,	  114,	18,	  7,	"XMSINGLE_LINE_EDIT"
    },
    {
	tkn_k_class_enumval,	  115,	15,	  7,	"XMSINGLE_SELECT"
    },
    {
	tkn_k_class_enumval,	  116,	 8,	  7,	"XMSTATIC"
    },
    {
	tkn_k_class_enumval,	  117,	18,	  7,	"XMSTICKY_TAB_GROUP"
    },
    {
	tkn_k_class_enumval,	  118,	 8,	  7,	"XMSTRING"
    },
    {
	tkn_k_class_enumval,	  119,	25,	  7,	"XMSTRING_DIRECTION_L_TO_R"
    },
    {
	tkn_k_class_enumval,	  120,	25,	  7,	"XMSTRING_DIRECTION_R_TO_L"
    },
    {
	tkn_k_class_enumval,	  121,	11,	  7,	"XMTAB_GROUP"
    },
    {
	tkn_k_class_class,	   45,	15,	  8,	"XMTEAROFFBUTTON"
    },
    {
	tkn_k_class_enumval,	  122,	19,	  7,	"XMTEAR_OFF_DISABLED"
    },
    {
	tkn_k_class_enumval,	  123,	18,	  7,	"XMTEAR_OFF_ENABLED"
    },
    {
	tkn_k_class_class,	   46,	16,	  8,	"XMTEMPLATEDIALOG"
    },
    {
	tkn_k_class_class,	   47,	 6,	  8,	"XMTEXT"
    },
    {
	tkn_k_class_class,	   48,	11,	  8,	"XMTEXTFIELD"
    },
    {
	tkn_k_class_class,	   49,	14,	  8,	"XMTOGGLEBUTTON"
    },
    {
	tkn_k_class_class,	   50,	20,	  8,	"XMTOGGLEBUTTONGADGET"
    },
    {
	tkn_k_class_enumval,	  124,	10,	  7,	"XMTOP_LEFT"
    },
    {
	tkn_k_class_enumval,	  125,	11,	  7,	"XMTOP_RIGHT"
    },
    {
	tkn_k_class_enumval,	  126,	 7,	  7,	"XMUNMAP"
    },
    {
	tkn_k_class_enumval,	  127,	 8,	  7,	"XMUNPOST"
    },
    {
	tkn_k_class_enumval,	  128,	19,	  7,	"XMUNPOST_AND_REPLAY"
    },
    {
	tkn_k_class_enumval,	  129,	10,	  7,	"XMVARIABLE"
    },
    {
	tkn_k_class_enumval,	  130,	10,	  7,	"XMVERTICAL"
    },
    {
	tkn_k_class_class,	   51,	15,	  8,	"XMWARNINGDIALOG"
    },
    {
	tkn_k_class_class,	   52,	10,	  8,	"XMWORKAREA"
    },
    {
	tkn_k_class_class,	   53,	15,	  8,	"XMWORKINGDIALOG"
    },
    {
	tkn_k_class_enumval,	  131,	11,	  7,	"XMWORK_AREA"
    },
    {
	tkn_k_class_child,	    1,	 8,	 96,	"XM_APPLY"
    },
    {
	tkn_k_class_child,	    2,	 9,	 96,	"XM_CANCEL"
    },
    {
	tkn_k_class_child,	    3,	 6,	 96,	"XM_DIR"
    },
    {
	tkn_k_class_child,	    4,	10,	 96,	"XM_DIRLIST"
    },
    {
	tkn_k_class_child,	    5,	 9,	 96,	"XM_FILTER"
    },
    {
	tkn_k_class_child,	    6,	14,	 96,	"XM_FILTERLABEL"
    },
    {
	tkn_k_class_child,	    7,	13,	 96,	"XM_FILTERTEXT"
    },
    {
	tkn_k_class_child,	    8,	 7,	 96,	"XM_HELP"
    },
    {
	tkn_k_class_child,	    9,	15,	 96,	"XM_HORSCROLLBAR"
    },
    {
	tkn_k_class_child,	   10,	 8,	 96,	"XM_ITEMS"
    },
    {
	tkn_k_class_child,	   11,	12,	 96,	"XM_ITEMSLIST"
    },
    {
	tkn_k_class_child,	   12,	10,	 96,	"XM_MESSAGE"
    },
    {
	tkn_k_class_child,	   13,	 5,	 96,	"XM_OK"
    },
    {
	tkn_k_class_child,	   14,	15,	 96,	"XM_OPTIONBUTTON"
    },
    {
	tkn_k_class_child,	   15,	14,	 96,	"XM_OPTIONLABEL"
    },
    {
	tkn_k_class_child,	   16,	12,	 96,	"XM_SELECTION"
    },
    {
	tkn_k_class_child,	   17,	12,	 96,	"XM_SEPARATOR"
    },
    {
	tkn_k_class_child,	   18,	13,	 96,	"XM_SEPARATOR1"
    },
    {
	tkn_k_class_child,	   19,	13,	 96,	"XM_SEPARATOR2"
    },
    {
	tkn_k_class_child,	   20,	13,	 96,	"XM_SEPARATOR3"
    },
    {
	tkn_k_class_child,	   21,	 9,	 96,	"XM_SYMBOL"
    },
    {
	tkn_k_class_child,	   22,	17,	 96,	"XM_TEAROFFCONTROL"
    },
    {
	tkn_k_class_child,	   23,	 7,	 96,	"XM_TEXT"
    },
    {
	tkn_k_class_child,	   24,	 8,	 96,	"XM_TITLE"
    },
    {
	tkn_k_class_child,	   25,	16,	 96,	"XM_VERTSCROLLBAR"
    },
};

key_keytable_entry_type *key_table_case_ins = priv_key_table_case_ins;

/********************************************************/

char **charset_xmstring_names_table = NULL;

static char
*priv_charset_lang_names_table[] = {
    "BIG5",
    "",
    "EUC",
    "",
    "GB_HANZI",
    "GB2312.1980-0",
    "GB_CHINESE",
    "GB_HANZI_GL",
    "GB_HANZI_GR",
    "GB2312.1980-1",
    "ISO_ARABIC",
    "ISO8859-6",
    "ISO_LATIN6",
    "ISOLATIN6",
    "88596",
    "ISO_CYRILLIC",
    "ISO8859-5",
    "ISO_GREEK",
    "ISO8859-7",
    "ISO_LATIN7",
    "ISOLATIN7",
    "88597",
    "ISO_HEBREW",
    "ISO8859-8",
    "ISO_LATIN8",
    "ISOLATIN8",
    "88598",
    "ISO_HEBREW_LR",
    "ISO8859-8",
    "ISO_LATIN8_LR",
    "ISO_LATIN1",
    "ISO8859-1",
    "ISOLATIN1",
    "88591",
    "ASCII",
    "ISO_LATIN2",
    "ISO8859-2",
    "ISOLATIN2",
    "88592",
    "ISO_LATIN3",
    "ISO8859-3",
    "ISOLATIN3",
    "88593",
    "ISO_LATIN4",
    "ISO8859-4",
    "ISOLATIN4",
    "88594",
    "ISO_LATIN5",
    "ISO8859-5",
    "ISOLATIN5",
    "88595",
    "JIS_KANJI",
    "JISX0208.1983-0",
    "JIS_JAPANESE",
    "JIS_KANJI_GL",
    "JIS_KANJI_GR",
    "JISX0208.1983-1",
    "JIS_KATAKANA",
    "JISX0201.1976-0",
    "KSC_HANGUL",
    "KSC5601.1987-0",
    "KSC_KOREAN",
    "KSC_HANGUL_GL",
    "KSC_HANGUL_GR",
    "KSC5601.1987-1",
};

char **charset_lang_names_table = priv_charset_lang_names_table;

static unsigned short
priv_charset_lang_codes_table[] = {
    2,
    2,
    3,
    3,
    4,
    4,
    4,
    4,
    5,
    5,
    6,
    6,
    6,
    6,
    6,
    7,
    7,
    8,
    8,
    8,
    8,
    8,
    9,
    9,
    9,
    9,
    9,
    10,
    10,
    10,
    11,
    11,
    11,
    11,
    11,
    12,
    12,
    12,
    12,
    13,
    13,
    13,
    13,
    14,
    14,
    14,
    14,
    15,
    15,
    15,
    15,
    16,
    16,
    16,
    16,
    17,
    17,
    18,
    18,
    19,
    19,
    19,
    19,
    20,
    20,
};

unsigned short int *charset_lang_codes_table = priv_charset_lang_codes_table;

unsigned short int charset_lang_table_max = 65;

/********************************************************/

int uil_max_object = 54;

static char
*priv_uil_widget_names[] = {
    "",
    "XmArrowButton",
    "XmArrowButtonGadget",
    "XmBulletinBoard",
    "XmBulletinBoardDialog",
    "XmCascadeButton",
    "XmCascadeButtonGadget",
    "XmCommand",
    "XmDialogShell",
    "XmDrawingArea",
    "XmDrawnButton",
    "XmErrorDialog",
    "XmFileSelectionBox",
    "XmFileSelectionDialog",
    "XmForm",
    "XmFormDialog",
    "XmFrame",
    "XmInformationDialog",
    "XmLabel",
    "XmLabelGadget",
    "XmList",
    "XmMainWindow",
    "XmMenuBar",
    "XmMenuShell",
    "XmMessageBox",
    "XmMessageDialog",
    "XmOptionMenu",
    "XmPanedWindow",
    "XmPopupMenu",
    "XmPromptDialog",
    "XmPulldownMenu",
    "XmPushButton",
    "XmPushButtonGadget",
    "XmQuestionDialog",
    "XmRadioBox",
    "XmRowColumn",
    "XmScale",
    "XmScrollBar",
    "XmScrolledList",
    "XmScrolledText",
    "XmScrolledWindow",
    "XmSelectionBox",
    "XmSelectionDialog",
    "XmSeparator",
    "XmSeparatorGadget",
    "XmTearOffButton",
    "XmTemplateDialog",
    "XmText",
    "XmTextField",
    "XmToggleButton",
    "XmToggleButtonGadget",
    "XmWarningDialog",
    "XmWorkArea",
    "XmWorkingDialog",
};

char **uil_widget_names = priv_uil_widget_names;

/********************************************************/

int uil_max_arg = 311;

static char
*priv_uil_argument_names[] = {
    "",
    "XmNaccelerator",
    "XmNacceleratorText",
    "XmNaccelerators",
    "XmNadjustLast",
    "XmNadjustMargin",
    "XmNalignment",
    "XmNallowOverlap",
    "XmNallowResize",
    "XmNallowShellResize",
    "XmNancestorSensitive",
    "XmNapplyLabelString",
    "XmNarmColor",
    "XmNarmPixmap",
    "XmNarrowDirection",
    "XmNaudibleWarning",
    "XmNautoShowCursorPosition",
    "XmNautoUnmanage",
    "XmNautomaticSelection",
    "XmNbackground",
    "XmNbackgroundPixmap",
    "XmNbaseHeight",
    "XmNbaseWidth",
    "XmNblinkRate",
    "XmNborderColor",
    "XmNborderPixmap",
    "XmNborderWidth",
    "XmNbottomAttachment",
    "XmNbottomOffset",
    "XmNbottomPosition",
    "XmNbottomShadowColor",
    "XmNbottomShadowPixmap",
    "XmNbottomWidget",
    "XmNbuttonFontList",
    "XmNcancelButton",
    "XmNcancelLabelString",
    "XmNcascadePixmap",
    "XmNchildHorizontalAlignment",
    "XmNchildHorizontalSpacing",
    "XmNchildPlacement",
    "XmNchildType",
    "XmNchildVerticalAlignment",
    "XmNclipWindow",
    "XmNcolormap",
    "XmNcolumns",
    "XmNcommand",
    "XmNcommandWindow",
    "XmNcommandWindowLocation",
    "XmNcreatePopupChildProc",
    "XmNcursorPosition",
    "XmNcursorPositionVisible",
    "XmNdecimalPoints",
    "XmNdefaultButton",
    "XmNdefaultButtonShadowThickness",
    "XmNdefaultButtonType",
    "XmNdefaultFontList",
    "XmNdefaultPosition",
    "XmNdeleteResponse",
    "XmNdepth",
    "XmNdialogStyle",
    "XmNdialogTitle",
    "XmNdialogType",
    "XmNdirListItemCount",
    "XmNdirListItems",
    "XmNdirListLabelString",
    "XmNdirMask",
    "XmNdirSearchProc",
    "XmNdirSpec",
    "XmNdirectory",
    "XmNdoubleClickInterval",
    "XmNeditMode",
    "XmNeditable",
    "XmNentryAlignment",
    "XmNentryBorder",
    "XmNentryClass",
    "XmNentryVerticalAlignment",
    "XmNfileListItemCount",
    "XmNfileListItems",
    "XmNfileListLabelString",
    "XmNfileSearchProc",
    "XmNfileTypeMask",
    "XmNfillOnArm",
    "XmNfillOnSelect",
    "XmNfilterLabelString",
    "XmNfontList",
    "XmNforeground",
    "XmNfractionBase",
    "XmNgeometry",
    "XmNheight",
    "XmNheightInc",
    "XmNhelpLabelString",
    "XmNhighlightColor",
    "XmNhighlightOnEnter",
    "XmNhighlightPixmap",
    "XmNhighlightThickness",
    "XmNhistoryItemCount",
    "XmNhistoryItems",
    "XmNhistoryMaxItems",
    "XmNhistoryVisibleItemCount",
    "XmNhorizontalScrollBar",
    "XmNhorizontalSpacing",
    "XmNiconMask",
    "XmNiconName",
    "XmNiconNameEncoding",
    "XmNiconPixmap",
    "XmNiconWindow",
    "XmNiconX",
    "XmNiconY",
    "XmNiconic",
    "XmNincrement",
    "XmNindicatorOn",
    "XmNindicatorSize",
    "XmNindicatorType",
    "XmNinitialDelay",
    "XmNinitialFocus",
    "XmNinitialResourcesPersistent",
    "XmNinitialState",
    "XmNinput",
    "XmNinputMethod",
    "XmNinsertPosition",
    "XmNisAligned",
    "XmNisHomogeneous",
    "XmNitemCount",
    "XmNitems",
    "XmNkeyboardFocusPolicy",
    "XmNlabelFontList",
    "XmNlabelInsensitivePixmap",
    "XmNlabelPixmap",
    "XmNlabelString",
    "XmNlabelType",
    "XmNleftAttachment",
    "XmNleftOffset",
    "XmNleftPosition",
    "XmNleftWidget",
    "XmNlistItemCount",
    "XmNlistItems",
    "XmNlistLabelString",
    "XmNlistMarginHeight",
    "XmNlistMarginWidth",
    "XmNlistSizePolicy",
    "XmNlistSpacing",
    "XmNlistUpdated",
    "XmNlistVisibleItemCount",
    "XmNmainWindowMarginHeight",
    "XmNmainWindowMarginWidth",
    "XmNmappedWhenManaged",
    "XmNmappingDelay",
    "XmNmargin",
    "XmNmarginBottom",
    "XmNmarginHeight",
    "XmNmarginLeft",
    "XmNmarginRight",
    "XmNmarginTop",
    "XmNmarginWidth",
    "XmNmaxAspectX",
    "XmNmaxAspectY",
    "XmNmaxHeight",
    "XmNmaxLength",
    "XmNmaxWidth",
    "XmNmaximum",
    "XmNmenuAccelerator",
    "XmNmenuBar",
    "XmNmenuHelpWidget",
    "XmNmenuHistory",
    "XmNmenuPost",
    "XmNmessageAlignment",
    "XmNmessageString",
    "XmNmessageWindow",
    "XmNminAspectX",
    "XmNminAspectY",
    "XmNminHeight",
    "XmNminWidth",
    "XmNminimizeButtons",
    "XmNminimum",
    "XmNmnemonic",
    "XmNmnemonicCharSet",
    "XmNmultiClick",
    "XmNmustMatch",
    "XmNmwmDecorations",
    "XmNmwmFunctions",
    "XmNmwmInputMode",
    "XmNmwmMenu",
    "XmNnavigationType",
    "XmNnoMatchString",
    "XmNnoResize",
    "XmNnumColumns",
    "XmNokLabelString",
    "XmNorientation",
    "XmNoverrideRedirect",
    "XmNpacking",
    "XmNpageIncrement",
    "XmNpaneMaximum",
    "XmNpaneMinimum",
    "XmNpattern",
    "XmNpendingDelete",
    "XmNpopupEnabled",
    "XmNpositionIndex",
    "XmNpostFromCount",
    "XmNpostFromList",
    "XmNpreeditType",
    "XmNprocessingDirection",
    "XmNpromptString",
    "XmNpushButtonEnabled",
    "XmNqualifySearchDataProc",
    "XmNradioAlwaysOne",
    "XmNradioBehavior",
    "XmNrecomputeSize",
    "XmNrefigureMode",
    "XmNrepeatDelay",
    "XmNresizable",
    "XmNresizeHeight",
    "XmNresizePolicy",
    "XmNresizeWidth",
    "XmNrightAttachment",
    "XmNrightOffset",
    "XmNrightPosition",
    "XmNrightWidget",
    "XmNrowColumnType",
    "XmNrows",
    "XmNrubberPositioning",
    "XmNsashHeight",
    "XmNsashIndent",
    "XmNsashShadowThickness",
    "XmNsashWidth",
    "XmNsaveUnder",
    "XmNscaleHeight",
    "XmNscaleMultiple",
    "XmNscaleWidth",
    "XmNscreen",
    "XmNscrollBarDisplayPolicy",
    "XmNscrollBarPlacement",
    "XmNscrollHorizontal",
    "XmNscrollLeftSide",
    "XmNscrollTopSide",
    "XmNscrollVertical",
    "XmNscrolledWindowMarginHeight",
    "XmNscrolledWindowMarginWidth",
    "XmNscrollingPolicy",
    "XmNselectColor",
    "XmNselectInsensitivePixmap",
    "XmNselectPixmap",
    "XmNselectThreshold",
    "XmNselectedItemCount",
    "XmNselectedItems",
    "XmNselectionArray",
    "XmNselectionArrayCount",
    "XmNselectionLabelString",
    "XmNselectionPolicy",
    "XmNsensitive",
    "XmNseparatorOn",
    "XmNseparatorType",
    "XmNset",
    "XmNshadowThickness",
    "XmNshadowType",
    "XmNshellUnitType",
    "XmNshowArrows",
    "XmNshowAsDefault",
    "XmNshowSeparator",
    "XmNshowValue",
    "XmNskipAdjust",
    "XmNsliderSize",
    "XmNsource",
    "XmNspacing",
    "XmNstringDirection",
    "XmNsubMenuId",
    "XmNsymbolPixmap",
    "XmNtearOffModel",
    "XmNtextAccelerators",
    "XmNtextColumns",
    "XmNtextFontList",
    "XmNtextString",
    "XmNtextTranslations",
    "XmNtitle",
    "XmNtitleEncoding",
    "XmNtitleString",
    "XmNtopAttachment",
    "XmNtopCharacter",
    "XmNtopItemPosition",
    "XmNtopOffset",
    "XmNtopPosition",
    "XmNtopShadowColor",
    "XmNtopShadowPixmap",
    "XmNtopWidget",
    "XmNtransient",
    "XmNtransientFor",
    "XmNtranslations",
    "XmNtraversalOn",
    "XmNtroughColor",
    "XmNunitType",
    "XmNunpostBehavior",
    "XmNuseAsyncGeometry",
    "XmNuserData",
    "XmNvalue",
    "XmNvalueWcs",
    "XmNverifyBell",
    "XmNverticalScrollBar",
    "XmNverticalSpacing",
    "XmNvisibleItemCount",
    "XmNvisibleWhenOff",
    "XmNvisual",
    "XmNvisualPolicy",
    "XmNwaitForWm",
    "XmNwhichButton",
    "XmNwidth",
    "XmNwidthInc",
    "XmNwinGravity",
    "XmNwindowGroup",
    "XmNwmTimeout",
    "XmNwordWrap",
    "XmNworkWindow",
    "XmNx",
};

char **uil_argument_names = priv_uil_argument_names;

/********************************************************/

int uil_max_child = 25;

static char
*priv_uil_child_names[] = {
    "",
    "Xm_Apply",
    "Xm_Cancel",
    "Xm_Dir",
    "Xm_DirList",
    "Xm_Filter",
    "Xm_FilterLabel",
    "Xm_FilterText",
    "Xm_Help",
    "Xm_HorScrollBar",
    "Xm_Items",
    "Xm_ItemsList",
    "Xm_Message",
    "Xm_OK",
    "Xm_OptionButton",
    "Xm_OptionLabel",
    "Xm_Selection",
    "Xm_Separator",
    "Xm_Separator1",
    "Xm_Separator2",
    "Xm_Separator3",
    "Xm_Symbol",
    "Xm_TearOffControl",
    "Xm_Text",
    "Xm_Title",
};

char **uil_child_names = priv_uil_child_names;

/********************************************************/

int uil_max_reason = 45;

static char
*priv_uil_reason_names[] = {
    "",
    "MrmNcreateCallback",
    "XmNactivateCallback",
    "XmNapplyCallback",
    "XmNarmCallback",
    "XmNbrowseSelectionCallback",
    "XmNcancelCallback",
    "XmNcascadingCallback",
    "XmNcommandChangedCallback",
    "XmNcommandEnteredCallback",
    "XmNdecrementCallback",
    "XmNdefaultActionCallback",
    "XmNdestroyCallback",
    "XmNdisarmCallback",
    "XmNdragCallback",
    "XmNentryCallback",
    "XmNexposeCallback",
    "XmNextendedSelectionCallback",
    "XmNfocusCallback",
    "XmNgainPrimaryCallback",
    "XmNhelpCallback",
    "XmNincrementCallback",
    "XmNinputCallback",
    "XmNlosePrimaryCallback",
    "XmNlosingFocusCallback",
    "XmNmapCallback",
    "XmNmodifyVerifyCallback",
    "XmNmodifyVerifyCallbackWcs",
    "XmNmotionVerifyCallback",
    "XmNmultipleSelectionCallback",
    "XmNnoMatchCallback",
    "XmNokCallback",
    "XmNpageDecrementCallback",
    "XmNpageIncrementCallback",
    "XmNpopdownCallback",
    "XmNpopupCallback",
    "XmNresizeCallback",
    "XmNsingleSelectionCallback",
    "XmNtearOffMenuActivateCallback",
    "XmNtearOffMenuDeactivateCallback",
    "XmNtoBottomCallback",
    "XmNtoTopCallback",
    "XmNtraversalCallback",
    "XmNtraverseObscuredCallback",
    "XmNunmapCallback",
};

char **uil_reason_names = priv_uil_reason_names;

/********************************************************/

int uil_max_enumset = 39;

int uil_max_enumval = 131;

static char
*priv_uil_enumval_names[] = {
    "",
    "IconicState",
    "NormalState",
    "Xm1000TH_INCHES",
    "Xm100TH_FONT_UNITS",
    "Xm100TH_MILLIMETERS",
    "Xm100TH_POINTS",
    "XmALIGNMENT_BASELINE_BOTTOM",
    "XmALIGNMENT_BASELINE_TOP",
    "XmALIGNMENT_BEGINNING",
    "XmALIGNMENT_CENTER",
    "XmALIGNMENT_CONTENTS_BOTTOM",
    "XmALIGNMENT_CONTENTS_TOP",
    "XmALIGNMENT_END",
    "XmALIGNMENT_WIDGET_BOTTOM",
    "XmALIGNMENT_WIDGET_TOP",
    "XmAPPLICATION_DEFINED",
    "XmARROW_DOWN",
    "XmARROW_LEFT",
    "XmARROW_RIGHT",
    "XmARROW_UP",
    "XmAS_NEEDED",
    "XmATTACH_FORM",
    "XmATTACH_NONE",
    "XmATTACH_OPPOSITE_FORM",
    "XmATTACH_OPPOSITE_WIDGET",
    "XmATTACH_POSITION",
    "XmATTACH_SELF",
    "XmATTACH_WIDGET",
    "XmAUTOMATIC",
    "XmBELL",
    "XmBOTTOM_LEFT",
    "XmBOTTOM_RIGHT",
    "XmBROWSE_SELECT",
    "XmCOMMAND_ABOVE_WORKSPACE",
    "XmCOMMAND_BELOW_WORKSPACE",
    "XmCONSTANT",
    "XmDESTROY",
    "XmDIALOG_APPLICATION_MODAL",
    "XmDIALOG_CANCEL_BUTTON",
    "XmDIALOG_DEFAULT_BUTTON",
    "XmDIALOG_ERROR",
    "XmDIALOG_FILE_SELECTION",
    "XmDIALOG_FULL_APPLICATION_MODAL",
    "XmDIALOG_HELP_BUTTON",
    "XmDIALOG_INFORMATION",
    "XmDIALOG_MESSAGE",
    "XmDIALOG_MESSAGE_LABEL",
    "XmDIALOG_MODELESS",
    "XmDIALOG_OK_BUTTON",
    "XmDIALOG_PRIMARY_APPLICATION_MODAL",
    "XmDIALOG_PROMPT",
    "XmDIALOG_QUESTION",
    "XmDIALOG_SELECTION",
    "XmDIALOG_SEPARATOR",
    "XmDIALOG_SYMBOL_LABEL",
    "XmDIALOG_SYSTEM_MODAL",
    "XmDIALOG_TEMPLATE",
    "XmDIALOG_WARNING",
    "XmDIALOG_WORKING",
    "XmDIALOG_WORK_AREA",
    "XmDOUBLE_DASHED_LINE",
    "XmDOUBLE_LINE",
    "XmDO_NOTHING",
    "XmEXCLUSIVE_TAB_GROUP",
    "XmEXPLICIT",
    "XmEXTENDED_SELECT",
    "XmFILE_DIRECTORY",
    "XmFILE_REGULAR",
    "XmFIRST_POSITION",
    "XmFRAME_GENERIC_CHILD",
    "XmFRAME_TITLE_CHILD",
    "XmFRAME_WORKAREA_CHILD",
    "XmHORIZONTAL",
    "XmLAST_POSITION",
    "XmMAX_ON_BOTTOM",
    "XmMAX_ON_LEFT",
    "XmMAX_ON_RIGHT",
    "XmMAX_ON_TOP",
    "XmMENU_BAR",
    "XmMENU_OPTION",
    "XmMENU_POPUP",
    "XmMENU_PULLDOWN",
    "XmMULTICLICK_DISCARD",
    "XmMULTICLICK_KEEP",
    "XmMULTIPLE_SELECT",
    "XmMULTI_LINE_EDIT",
    "XmNONE",
    "XmNO_LINE",
    "XmNO_ORIENTATION",
    "XmNO_PACKING",
    "XmN_OF_MANY",
    "XmONE_OF_MANY",
    "XmPACK_COLUMN",
    "XmPACK_NONE",
    "XmPACK_TIGHT",
    "XmPIXELS",
    "XmPIXMAP",
    "XmPLACE_ABOVE_SELECTION",
    "XmPLACE_BELOW_SELECTION",
    "XmPLACE_TOP",
    "XmPOINTER",
    "XmRESIZE_ANY",
    "XmRESIZE_GROW",
    "XmRESIZE_IF_POSSIBLE",
    "XmRESIZE_NONE",
    "XmSHADOW_ETCHED_IN",
    "XmSHADOW_ETCHED_IN_DASH",
    "XmSHADOW_ETCHED_OUT",
    "XmSHADOW_ETCHED_OUT_DASH",
    "XmSHADOW_IN",
    "XmSHADOW_OUT",
    "XmSINGLE_DASHED_LINE",
    "XmSINGLE_LINE",
    "XmSINGLE_LINE_EDIT",
    "XmSINGLE_SELECT",
    "XmSTATIC",
    "XmSTICKY_TAB_GROUP",
    "XmSTRING",
    "XmSTRING_DIRECTION_L_TO_R",
    "XmSTRING_DIRECTION_R_TO_L",
    "XmTAB_GROUP",
    "XmTEAR_OFF_DISABLED",
    "XmTEAR_OFF_ENABLED",
    "XmTOP_LEFT",
    "XmTOP_RIGHT",
    "XmUNMAP",
    "XmUNPOST",
    "XmUNPOST_AND_REPLAY",
    "XmVARIABLE",
    "XmVERTICAL",
};

char **uil_enumval_names = priv_uil_enumval_names;

/********************************************************/

int uil_max_charset = 20;

static char
*priv_uil_charset_names[] = {
    "",
    "<userdefined>",
    "big5",
    "euc",
    "gb_hanzi",
    "gb_hanzi_gr",
    "iso_arabic",
    "iso_cyrillic",
    "iso_greek",
    "iso_hebrew",
    "iso_hebrew_lr",
    "iso_latin1",
    "iso_latin2",
    "iso_latin3",
    "iso_latin4",
    "iso_latin5",
    "jis_kanji",
    "jis_kanji_gr",
    "jis_katakana",
    "ksc_hangul",
};

char **uil_charset_names = priv_uil_charset_names;
/********************************************************/

int tok_num_tokens = 97;

static char
*priv_tok_token_name_table[] = {
    "UILEOF",
    "NAME",
    "FONT_NAME",
    "COLOR_NAME",
    "ARGUMENT_NAME",
    "REASON_NAME",
    "CHARSET_NAME",
    "ENUMVAL_NAME",
    "CLASS_NAME",
    "UNS_FLOAT_LITERAL",
    "COMP_STRING",
    "CHAR_8_LITERAL",
    "UNS_INT_LITERAL",
    "LEFT_PAREN",
    "RIGHT_PAREN",
    "COLON",
    "SEMICOLON",
    "LEFT_BRACE",
    "RIGHT_BRACE",
    "COMMA",
    "EQUAL_SIGN",
    "NOT",
    "PLUS",
    "MINUS",
    "AND",
    "OR",
    "XOR",
    "MULTIPLY",
    "DIVIDE",
    "LEFT_SHIFT",
    "RIGHT_SHIFT",
    "LIST",
    "IDENTIFIER",
    "END",
    "MODULE",
    "UILTRUE",
    "UILFALSE",
    "INCLUDE",
    "MACRO",
    "ON",
    "OFF",
    "VALUE",
    "ARGUMENTS",
    "CALLBACKS",
    "PROCEDURES",
    "CONTROLS",
    "PROCEDURE",
    "OBJECT",
    "OBJECTS",
    "WIDGET",
    "GADGET",
    "FONT",
    "ARGUMENT",
    "REASON",
    "PIXMAP",
    "COLOR",
    "NAMES",
    "CHARACTER_SET",
    "CASE_SENSITIVE",
    "CASE_INSENSITIVE",
    "VERSION",
    "MANAGED",
    "UNMANAGED",
    "PRIVATE",
    "IMPORTED",
    "EXPORTED",
    "UILFILE",
    "STRING_TABLE",
    "TRANSLATION_TABLE",
    "COMPOUND_STRING",
    "FONT_TABLE",
    "ANY",
    "STRING",
    "BOOLEAN",
    "ICON",
    "RIGHT_TO_LEFT",
    "BACKGROUND",
    "FOREGROUND",
    "COLOR_TABLE",
    "FLOAT",
    "INTEGER",
    "CLASS_REC_NAME",
    "ASCIZ_TABLE",
    "INTEGER_TABLE",
    "ASCIZ_STRING_TABLE",
    "COMPOUND_STRING_TABLE",
    "XBITMAPFILE",
    "SEPARATE",
    "SIXTEEN_BIT",
    "POUND",
    "KEYSYM",
    "SINGLE_FLOAT",
    "RGB",
    "WIDE_CHARACTER",
    "LOC_STRING",
    "FONTSET",
    "CHILD_NAME",
};

char **tok_token_name_table = priv_tok_token_name_table;

/********************************************************/

static char
*priv_uil_widget_funcs[] = {
    "",
    "XmCreateArrowButton",
    "XmCreateArrowButtonGadget",
    "XmCreateBulletinBoard",
    "XmCreateBulletinBoardDialog",
    "XmCreateCascadeButton",
    "XmCreateCascadeButtonGadget",
    "XmCreateCommand",
    "XmCreateDialogShell",
    "XmCreateDrawingArea",
    "XmCreateDrawnButton",
    "XmCreateErrorDialog",
    "XmCreateFileSelectionBox",
    "XmCreateFileSelectionDialog",
    "XmCreateForm",
    "XmCreateFormDialog",
    "XmCreateFrame",
    "XmCreateInformationDialog",
    "XmCreateLabel",
    "XmCreateLabelGadget",
    "XmCreateList",
    "XmCreateMainWindow",
    "XmCreateMenuBar",
    "XmCreateMenuShell",
    "XmCreateMessageBox",
    "XmCreateMessageDialog",
    "XmCreateOptionMenu",
    "XmCreatePanedWindow",
    "XmCreatePopupMenu",
    "XmCreatePromptDialog",
    "XmCreatePulldownMenu",
    "XmCreatePushButton",
    "XmCreatePushButtonGadget",
    "XmCreateQuestionDialog",
    "XmCreateRadioBox",
    "XmCreateRowColumn",
    "XmCreateScale",
    "XmCreateScrollBar",
    "XmCreateScrolledList",
    "XmCreateScrolledText",
    "XmCreateScrolledWindow",
    "XmCreateSelectionBox",
    "XmCreateSelectionDialog",
    "XmCreateSeparator",
    "XmCreateSeparatorGadget",
    "",
    "XmCreateTemplateDialog",
    "XmCreateText",
    "XmCreateTextField",
    "XmCreateToggleButton",
    "XmCreateToggleButtonGadget",
    "XmCreateWarningDialog",
    "XmCreateWorkArea",
    "XmCreateWorkingDialog",
};

char **uil_widget_funcs = priv_uil_widget_funcs;

/********************************************************/

static char
*priv_uil_argument_toolkit_names[] = {
    "",
    "accelerator",
    "acceleratorText",
    "accelerators",
    "adjustLast",
    "adjustMargin",
    "alignment",
    "allowOverlap",
    "allowResize",
    "allowShellResize",
    "ancestorSensitive",
    "applyLabelString",
    "armColor",
    "armPixmap",
    "arrowDirection",
    "audibleWarning",
    "autoShowCursorPosition",
    "autoUnmanage",
    "automaticSelection",
    "background",
    "backgroundPixmap",
    "baseHeight",
    "baseWidth",
    "blinkRate",
    "borderColor",
    "borderPixmap",
    "borderWidth",
    "bottomAttachment",
    "bottomOffset",
    "bottomPosition",
    "bottomShadowColor",
    "bottomShadowPixmap",
    "bottomWidget",
    "buttonFontList",
    "cancelButton",
    "cancelLabelString",
    "cascadePixmap",
    "childHorizontalAlignment",
    "childHorizontalSpacing",
    "childPlacement",
    "childType",
    "childVerticalAlignment",
    "clipWindow",
    "colormap",
    "columns",
    "command",
    "commandWindow",
    "commandWindowLocation",
    "createPopupChildProc",
    "cursorPosition",
    "cursorPositionVisible",
    "decimalPoints",
    "defaultButton",
    "defaultButtonShadowThickness",
    "defaultButtonType",
    "defaultFontList",
    "defaultPosition",
    "deleteResponse",
    "depth",
    "dialogStyle",
    "dialogTitle",
    "dialogType",
    "dirListItemCount",
    "dirListItems",
    "dirListLabelString",
    "dirMask",
    "dirSearchProc",
    "dirSpec",
    "directory",
    "doubleClickInterval",
    "editMode",
    "editable",
    "entryAlignment",
    "entryBorder",
    "entryClass",
    "entryVerticalAlignment",
    "fileListItemCount",
    "fileListItems",
    "fileListLabelString",
    "fileSearchProc",
    "fileTypeMask",
    "fillOnArm",
    "fillOnSelect",
    "filterLabelString",
    "fontList",
    "foreground",
    "fractionBase",
    "geometry",
    "height",
    "heightInc",
    "helpLabelString",
    "highlightColor",
    "highlightOnEnter",
    "highlightPixmap",
    "highlightThickness",
    "historyItemCount",
    "historyItems",
    "historyMaxItems",
    "historyVisibleItemCount",
    "horizontalScrollBar",
    "horizontalSpacing",
    "iconMask",
    "iconName",
    "iconNameEncoding",
    "iconPixmap",
    "iconWindow",
    "iconX",
    "iconY",
    "iconic",
    "increment",
    "indicatorOn",
    "indicatorSize",
    "indicatorType",
    "initialDelay",
    "initialFocus",
    "initialResourcesPersistent",
    "initialState",
    "input",
    "inputMethod",
    "insertPosition",
    "isAligned",
    "isHomogeneous",
    "itemCount",
    "items",
    "keyboardFocusPolicy",
    "labelFontList",
    "labelInsensitivePixmap",
    "labelPixmap",
    "labelString",
    "labelType",
    "leftAttachment",
    "leftOffset",
    "leftPosition",
    "leftWidget",
    "listItemCount",
    "listItems",
    "listLabelString",
    "listMarginHeight",
    "listMarginWidth",
    "listSizePolicy",
    "listSpacing",
    "listUpdated",
    "listVisibleItemCount",
    "mainWindowMarginHeight",
    "mainWindowMarginWidth",
    "mappedWhenManaged",
    "mappingDelay",
    "margin",
    "marginBottom",
    "marginHeight",
    "marginLeft",
    "marginRight",
    "marginTop",
    "marginWidth",
    "maxAspectX",
    "maxAspectY",
    "maxHeight",
    "maxLength",
    "maxWidth",
    "maximum",
    "menuAccelerator",
    "menuBar",
    "menuHelpWidget",
    "menuHistory",
    "menuPost",
    "messageAlignment",
    "messageString",
    "messageWindow",
    "minAspectX",
    "minAspectY",
    "minHeight",
    "minWidth",
    "minimizeButtons",
    "minimum",
    "mnemonic",
    "mnemonicCharSet",
    "multiClick",
    "mustMatch",
    "mwmDecorations",
    "mwmFunctions",
    "mwmInputMode",
    "mwmMenu",
    "navigationType",
    "noMatchString",
    "noResize",
    "numColumns",
    "okLabelString",
    "orientation",
    "overrideRedirect",
    "packing",
    "pageIncrement",
    "paneMaximum",
    "paneMinimum",
    "pattern",
    "pendingDelete",
    "popupEnabled",
    "positionIndex",
    "postFromCount",
    "postFromList",
    "preeditType",
    "processingDirection",
    "promptString",
    "pushButtonEnabled",
    "qualifySearchDataProc",
    "radioAlwaysOne",
    "radioBehavior",
    "recomputeSize",
    "refigureMode",
    "repeatDelay",
    "resizable",
    "resizeHeight",
    "resizePolicy",
    "resizeWidth",
    "rightAttachment",
    "rightOffset",
    "rightPosition",
    "rightWidget",
    "rowColumnType",
    "rows",
    "rubberPositioning",
    "sashHeight",
    "sashIndent",
    "sashShadowThickness",
    "sashWidth",
    "saveUnder",
    "scaleHeight",
    "scaleMultiple",
    "scaleWidth",
    "screen",
    "scrollBarDisplayPolicy",
    "scrollBarPlacement",
    "scrollHorizontal",
    "scrollLeftSide",
    "scrollTopSide",
    "scrollVertical",
    "scrolledWindowMarginHeight",
    "scrolledWindowMarginWidth",
    "scrollingPolicy",
    "selectColor",
    "selectInsensitivePixmap",
    "selectPixmap",
    "selectThreshold",
    "selectedItemCount",
    "selectedItems",
    "selectionArray",
    "selectionArrayCount",
    "selectionLabelString",
    "selectionPolicy",
    "sensitive",
    "separatorOn",
    "separatorType",
    "set",
    "shadowThickness",
    "shadowType",
    "shellUnitType",
    "showArrows",
    "showAsDefault",
    "showSeparator",
    "showValue",
    "skipAdjust",
    "sliderSize",
    "source",
    "spacing",
    "stringDirection",
    "subMenuId",
    "symbolPixmap",
    "tearOffModel",
    "textAccelerators",
    "textColumns",
    "textFontList",
    "textString",
    "textTranslations",
    "title",
    "titleEncoding",
    "titleString",
    "topAttachment",
    "topCharacter",
    "topItemPosition",
    "topOffset",
    "topPosition",
    "topShadowColor",
    "topShadowPixmap",
    "topWidget",
    "transient",
    "transientFor",
    "translations",
    "traversalOn",
    "troughColor",
    "unitType",
    "unpostBehavior",
    "useAsyncGeometry",
    "userData",
    "value",
    "valueWcs",
    "verifyBell",
    "verticalScrollBar",
    "verticalSpacing",
    "visibleItemCount",
    "visibleWhenOff",
    "visual",
    "visualPolicy",
    "waitforwm",
    "whichButton",
    "width",
    "widthInc",
    "winGravity",
    "windowGroup",
    "wmTimeout",
    "wordWrap",
    "workWindow",
    "x",
};

char **uil_argument_toolkit_names = priv_uil_argument_toolkit_names;

/********************************************************/

static char
*priv_uil_reason_toolkit_names[] = {
    "",
    "createCallback",
    "activateCallback",
    "applyCallback",
    "armCallback",
    "browseSelectionCallback",
    "cancelCallback",
    "cascadingCallback",
    "commandChangedCallback",
    "commandEnteredCallback",
    "decrementCallback",
    "defaultActionCallback",
    "destroyCallback",
    "disarmCallback",
    "dragCallback",
    "entryCallback",
    "exposeCallback",
    "extendedSelectionCallback",
    "focusCallback",
    "gainPrimaryCallback",
    "helpCallback",
    "incrementCallback",
    "inputCallback",
    "losePrimaryCallback",
    "losingFocusCallback",
    "mapCallback",
    "modifyVerifyCallback",
    "modifyVerifyCallbackWcs",
    "motionVerifyCallback",
    "multipleSelectionCallback",
    "noMatchCallback",
    "okCallback",
    "pageDecrementCallback",
    "pageIncrementCallback",
    "popdownCallback",
    "popupCallback",
    "resizeCallback",
    "singleSelectionCallback",
    "tearOffMenuActivateCallback",
    "tearOffMenuDeactivateCallback",
    "toBottomCallback",
    "toTopCallback",
    "traversalCallback",
    "traverseObscuredCallback",
    "unmapCallback",
};

char **uil_reason_toolkit_names = priv_uil_reason_toolkit_names;

/********************************************************/

int uil_max_value = 29;

char *uil_datatype_names[] = {
    "",
    "any",
    "argument",
    "asciz_table",
    "boolean",
    "string",
    "class_rec_name",
    "color",
    "color_table",
    "compound_string",
    "float",
    "font",
    "font_table",
    "icon",
    "identifier",
    "integer",
    "integer_table",
    "keysym",
    "pixmap",
    "reason",
    "rgb",
    "single_float",
    "string_table",
    "translation_table",
    "widget_ref",
    "xbitmapfile",
    "localized_string",
    "wchar_string",
    "fontset",
};
