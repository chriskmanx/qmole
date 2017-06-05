/**
 *
 * $Id: MessageB.c,v 1.1 2004/08/28 19:22:44 dannybackx Exp $
 *
 * Copyright (C) 1996 Free Software Foundation, Inc.
 * Copyright (C) 1996-2001 LessTif Development Team
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

static const char rcsid[] = "$Id: MessageB.c,v 1.1 2004/08/28 19:22:44 dannybackx Exp $";

#include <LTconfig.h>

#include <XmI/XmI.h>
#include <Xm/XmP.h>
#include <Xm/MessageBP.h>
#ifdef USE_WIDGETS
#include <Xm/SeparatorP.h>
#else
#include <Xm/SeparatoGP.h>
#endif
#include <Xm/RowColumn.h>
#include <Xm/RowColumnP.h>
#include <Xm/DialogS.h>
#include <Xm/LabelGP.h>
#include <Xm/RepType.h>

#include <XmI/DebugUtil.h>

/* Forward Declarations */

static void class_initialize(void);

static void class_part_initialize(WidgetClass w_class);

static void initialize(Widget request, Widget new_w,
		       ArgList args, Cardinal *num_args);

static void destroy(Widget w);

static Boolean set_values(Widget current, Widget request, Widget new_w,
			  ArgList args, Cardinal *num_args);

static void delete_child(Widget w);

XmGeoMatrix _XmMessageBoxGeoMatrixCreate(Widget _w, Widget _from,
					 XtWidgetGeometry *_pref);

Boolean _XmMessageBoxNoGeoRequest(XmGeoMatrix _geoSpec);

static void _XmMbButton(Widget w, XtPointer client, XtPointer call);

static Boolean XmMbSetSubresources(Widget new_w, Widget request,
				   ArgList args, Cardinal *nargs);

static Pixmap _XmMessageBoxPixmap(Widget w, unsigned char dt);

static void _XmMessageBoxExportPixmap(Widget w, int offset, XtArgVal *value);

/*
 * Resources for the Message Box class
 */
#define Offset(field) XtOffsetOf(XmMessageBoxRec, message_box.field)
static XtResource resources[] =
{
    {
	XmNdialogType, XmCDialogType, XmRDialogType,
	sizeof(unsigned char), Offset(dialog_type),
	XmRImmediate, (XtPointer)XmDIALOG_MESSAGE
    },
    {
	XmNminimizeButtons, XmCMinimizeButtons, XmRBoolean,
	sizeof(Boolean), Offset(minimize_buttons),
	XmRImmediate, (XtPointer)False
    },
    {
	XmNdefaultButtonType, XmCDefaultButtonType, XmRDefaultButtonType,
	sizeof(unsigned char), Offset(default_type),
	XtRImmediate, (XtPointer)XmDIALOG_OK_BUTTON
    },
    {
	XmNmessageString, XmCXmString, XmRXmString,
	sizeof(XmString), Offset(message_string),
	XmRXmString, (XtPointer)NULL
    },
    {
	XmNmessageAlignment, XmCAlignment, XmRAlignment,
	sizeof(unsigned char), Offset(message_alignment),
	XtRImmediate, (XtPointer)XmALIGNMENT_BEGINNING
    },
    {
	XmNsymbolPixmap, XmCPixmap, XmRManForegroundPixmap,
	sizeof(Pixmap), Offset(symbol_pixmap),
	XmRImmediate, (XtPointer)XmUNSPECIFIED_PIXMAP
    },
    {
	XmNokLabelString, XmCXmString, XmRXmString,
	sizeof(XmString), Offset(ok_label_string),
	XmRXmString, (XtPointer)NULL
    },
    {
	XmNokCallback, XmCCallback, XmRCallback,
	sizeof(XtCallbackList), Offset(ok_callback),
	XmRCallback, (XtPointer)NULL
    },
    {
	XmNcancelLabelString, XmCXmString, XmRXmString,
	sizeof(XmString), Offset(cancel_label_string),
	XmRXmString, (XtPointer)NULL
    },
    {
	XmNcancelCallback, XmCCallback, XmRCallback,
	sizeof(XtCallbackList), Offset(cancel_callback),
	XmRCallback, (XtPointer)NULL
    },
    {
	XmNhelpLabelString, XmCXmString, XmRXmString,
	sizeof(XmString), Offset(help_label_string),
	XmRXmString, (XtPointer)NULL
    },
#if 0
/* Initialize children widgets to NULL */
    {
	".", ".", XmRWidget,
	sizeof(Widget), Offset(message_wid),
	XmRImmediate, (XtPointer)NULL
    },
    {
	".", ".", XmRWidget,
	sizeof(Widget), Offset(symbol_wid),
	XmRImmediate, (XtPointer)NULL
    },
    {
	".", ".", XmRWidget,
	sizeof(Widget), Offset(ok_button),
	XmRImmediate, (XtPointer)NULL
    },
    {
	".", ".", XmRWidget,
	sizeof(Widget), Offset(help_button),
	XmRImmediate, (XtPointer)NULL
    },
    {
	".", ".", XmRWidget,
	sizeof(Widget), Offset(separator),
	XmRImmediate, (XtPointer)NULL
    },
#endif
};

static XmSyntheticResource syn_resources[] =
{
    {
	XmNmessageString,
	sizeof(XmString), Offset(message_string),
	_XmExportXmString, NULL
    },
    {
	XmNsymbolPixmap,
	sizeof(Pixmap), Offset(symbol_pixmap),
	_XmMessageBoxExportPixmap, NULL
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
};
/* *INDENT-OFF* */
#if 0
static XmBaseClassExtRec _XmMessageBCoreClassExtRec = {
    /* next_extension            */ NULL,
    /* record_type               */ NULLQUARK,                             
    /* version                   */ XmBaseClassExtVersion,
    /* size                      */ sizeof(XmBaseClassExtRec),
    /* initialize_prehook        */ NULL,
    /* set_values_prehook        */ NULL,
    /* initialize_posthook       */ NULL,
    /* set_values_posthook       */ NULL,
    /* secondary_object_class    */ NULL,
    /* secondary_object_create   */ NULL,
    /* get_secondary_resources   */ NULL,
    /* fast_subclass             */ { 0 },
    /* get_values_prehook        */ NULL,
    /* get_values_posthook       */ NULL,
    /* class_part_init_prehook   */ NULL,
    /* class_part_init_posthook  */ NULL,
    /* ext_resources             */ NULL,
    /* compiled_ext_resources    */ NULL,
    /* num_ext_resources         */ 0,
    /* use_sub_resources         */ False,
    /* widget_navigable          */ XmInheritWidgetNavigable,
    /* focus_change              */ XmInheritFocusChange,
    /* wrapper_data              */ NULL
};
#endif

XmMessageBoxClassRec xmMessageBoxClassRec = {
    /* Core class part */
    {
	/* superclass            */ (WidgetClass) &xmBulletinBoardClassRec,
        /* class_name            */ "XmMessageBox",
	/* widget_size           */ sizeof(XmMessageBoxRec),
	/* class_initialize      */ class_initialize,
	/* class_part_initialize */ class_part_initialize,
	/* class_inited          */ False,
	/* initialize            */ initialize,
	/* initialize_hook       */ NULL,
	/* realize               */ XtInheritRealize,
	/* actions               */ NULL,
	/* num_actions           */ 0,
	/* resources             */ resources,
	/* num_resources         */ XtNumber(resources),
	/* xrm_class             */ NULLQUARK,
	/* compress_motion       */ True,
	/* compress_exposure     */ XtExposeCompressMaximal,
	/* compress_enterleave   */ False,
	/* visible_interest      */ False,
	/* destroy               */ destroy,
	/* resize                */ XtInheritResize,
	/* expose                */ XtInheritExpose,
	/* set_values            */ set_values,
	/* set_values_hook       */ NULL,
	/* set_values_almost     */ XtInheritSetValuesAlmost,
	/* get_values_hook       */ NULL,
	/* accept_focus          */ XtInheritAcceptFocus,
	/* version               */ XtVersion,
	/* callback offsets      */ NULL,
	/* tm_table              */ XtInheritTranslations,
	/* query_geometry        */ XtInheritQueryGeometry,
	/* display_accelerator   */ NULL,
	/* extension             */ (XtPointer)NULL /*&_XmMessageBCoreClassExtRec*/
    },
    /* Composite class part */
    {
	/* geometry manager */ XtInheritGeometryManager, 
        /* change_managed   */ XtInheritChangeManaged, 
        /* insert_child     */ XtInheritInsertChild,
        /* delete_child     */ delete_child,
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
	/* geo_matrix_create            */ _XmMessageBoxGeoMatrixCreate,
	/* focus_moved_proc             */ XmInheritFocusMovedProc,
	/* extension                    */ NULL,
    },
    /* XmMessageBox part */
    {
	/* extension */ NULL,
    }
};
/* *INDENT-ON* */


WidgetClass xmMessageBoxWidgetClass = (WidgetClass)&xmMessageBoxClassRec;

static void
class_initialize(void)
{
#if 0
    _XmMessageBCoreClassExtRec.record_type = XmQmotif;
#endif
}

static void
class_part_initialize(WidgetClass widget_class)
{
    _XmFastSubclassInit(widget_class, XmMESSAGE_BOX_BIT);
}

static void
initialize(Widget request, Widget new_w,
	   ArgList args, Cardinal *num_args)
{
    Widget default_button;
    Arg al[3];

    DEBUGOUT(_LtDebug(__FILE__, new_w,
		      "initialize: %i args\n"
		      "\trequest X %5i Y %5i W %5i H %5i\n"
		      "\t  new_w X %5i Y %5i W %5i H %5i\n",
		      *num_args,
		      XtX(request), XtY(request),
		      XtWidth(request), XtHeight(request),
		      XtX(new_w), XtY(new_w),
		      XtWidth(new_w), XtHeight(new_w)));
    DEBUGOUT(_LtDebugPrintArgList(__FILE__, new_w, args, *num_args, False));

    /* core dump avoidance */
    MB_Message(new_w) = NULL;
    MB_Symbol(new_w) = NULL;

    if (!XmRepTypeValidValue(XmRepTypeGetId(XmRDefaultButtonType),
			     MB_DefaultType(new_w), new_w))
    {
	MB_DefaultType(new_w) = XmDIALOG_OK_BUTTON;
    }

    if (!XmRepTypeValidValue(XmRepTypeGetId(XmRDialogType),
			     MB_DialogType(new_w), new_w))
    {
	MB_DialogType(new_w) = XmDIALOG_MESSAGE;
    }

    if (!XmRepTypeValidValue(XmRepTypeGetId(XmRAlignment),
			     MB_MessageAlignment(new_w), new_w))
    {
	MB_MessageAlignment(new_w) = XmALIGNMENT_BEGINNING;
    }

    if (MB_DialogType(new_w) != XmDIALOG_TEMPLATE ||
	(MB_SymbolPixmap(new_w) != None &&
	 MB_SymbolPixmap(new_w) != XmUNSPECIFIED_PIXMAP))
    {
	MB_Symbol(new_w) = _XmBB_CreateLabelG(new_w, NULL, "Symbol");
	if (MB_SymbolPixmap(new_w) == XmUNSPECIFIED_PIXMAP)
	{
	    MB_SymbolPixmap(new_w) = _XmMessageBoxPixmap(new_w,
							 MB_DialogType(new_w));
	}

	XtSetArg(al[0], XmNlabelPixmap, MB_SymbolPixmap(new_w));
	XtSetArg(al[1], XmNlabelType, XmPIXMAP);
	XtSetValues(MB_Symbol(new_w), al, 2);
    }
    else
    {
	MB_Symbol(new_w) = NULL;
    }

    if (MB_MessageString(new_w)) {
	MB_MessageString(new_w) = XmStringCopy(MB_MessageString(new_w));
    }

    if (MB_DialogType(new_w) != XmDIALOG_TEMPLATE ||
	MB_MessageString(new_w) != NULL)
    {
	if (MB_MessageString(new_w) == NULL)
	{
	    MB_MessageString(new_w) =
		XmStringCreateLtoR("", XmFONTLIST_DEFAULT_TAG);
	}

	MB_Message(new_w) = _XmBB_CreateLabelG(new_w, MB_MessageString(new_w),
					       "Message");

	XtSetArg(al[0], XmNalignment, MB_MessageAlignment(new_w));
	XtSetValues(MB_Message(new_w), al, 1);
    }
    else
    {
	MB_Message(new_w) = NULL;
    }

#ifdef USE_WIDGETS
    MB_Separator(new_w) = XmCreateSeparator(new_w, "Separator", NULL, 0);
#else
    MB_Separator(new_w) = XmCreateSeparatorGadget(new_w, "Separator", NULL, 0);
#endif

    if (MB_DialogType(new_w) != XmDIALOG_TEMPLATE ||
	MB_OKLabelString(new_w) != NULL ||
	MB_OKCall(new_w) != NULL)
    {
	MB_OKButton(new_w) = _XmBB_CreateButtonG(new_w,
						 MB_OKLabelString(new_w),
						 "OK");
    }
    else
    {
	MB_OKButton(new_w) = NULL;
    }
    if (MB_OKLabelString(new_w) != NULL)
    {
        MB_OKLabelString(new_w) = XmStringCopy(MB_OKLabelString(new_w));
    }

    if (MB_DialogType(new_w) != XmDIALOG_TEMPLATE ||
	MB_CancelLabelString(new_w) != NULL ||
	MB_CancelCall(new_w))
    {
	BB_CancelButton(new_w) =
			_XmBB_CreateButtonG(new_w,
					    MB_CancelLabelString(new_w),
					    "Cancel");
    }
    else
    {
	BB_CancelButton(new_w) = NULL;
    }
    if (MB_CancelLabelString(new_w) != NULL)
    {
        MB_CancelLabelString(new_w) = XmStringCopy(MB_CancelLabelString(new_w));
    }

    if (MB_DialogType(new_w) != XmDIALOG_TEMPLATE ||
	MB_HelpLabelString(new_w) != NULL ||
	MGR_HelpCallbackList(new_w) != NULL)
    {
	MB_HelpButton(new_w) = _XmBB_CreateButtonG(new_w,
						   MB_HelpLabelString(new_w),
						   "Help");
    }
    else
    {
	MB_HelpButton(new_w) = NULL;
    }
    if (MB_HelpLabelString(new_w) != NULL)
    {
        MB_HelpLabelString(new_w) = XmStringCopy(MB_HelpLabelString(new_w));
    }

    /* Remove auto_unmanage */
    if (MB_HelpButton(new_w))
    {
	XtRemoveAllCallbacks(MB_HelpButton(new_w), XmNactivateCallback);
    }

    /* Add Activate handler */
    if (MB_OKButton(new_w))
    {
	XtAddCallback(MB_OKButton(new_w),
		      XmNactivateCallback, _XmMbButton, NULL);
    }
    if (BB_CancelButton(new_w))
    {
	XtAddCallback(BB_CancelButton(new_w),
		      XmNactivateCallback, _XmMbButton, NULL);
    }
    if (MB_HelpButton(new_w))
    {
	XtAddCallback(MB_HelpButton(new_w),
		      XmNactivateCallback, _XmMbButton, NULL);
    }

    if (MB_OKButton(new_w))
    {
	XtManageChild(MB_OKButton(new_w));
    }

    if (BB_CancelButton(new_w))
    {
	XtManageChild(BB_CancelButton(new_w));
    }

    if (MB_HelpButton(new_w))
    {
	XtManageChild(MB_HelpButton(new_w));
    }

    XtManageChild(MB_Separator(new_w));

    if (MB_Symbol(new_w))
    {
	XtManageChild(MB_Symbol(new_w));
    }

    if (MB_Message(new_w))
    {
	XtManageChild(MB_Message(new_w));
    }

    switch (MB_DefaultType(new_w))
    {
    case XmDIALOG_OK_BUTTON:
	default_button = MB_OKButton(new_w);
	break;

    case XmDIALOG_CANCEL_BUTTON:
	default_button = BB_CancelButton(new_w);
	break;

    case XmDIALOG_HELP_BUTTON:
	default_button = MB_HelpButton(new_w);
	break;

    case XmDIALOG_NONE:
    default:
	default_button = NULL;
	break;
    }

    /*
     * By putting this assignment last, we can check in insert_child
     * whether the newly inserted widget is one created in Initialize.
     */
    XtVaSetValues(new_w, XmNdefaultButton, default_button, NULL);
    /* Don't add anything after this !! */
}

static void
destroy(Widget w)
{
    if (MB_MessageString(w))
    {
	XmStringFree(MB_MessageString(w));
    }
    if (MB_HelpLabelString(w))
    {
        XmStringFree(MB_HelpLabelString(w));
    }
    if (MB_CancelLabelString(w))
    {
        XmStringFree(MB_CancelLabelString(w));
    }
    if (MB_OKLabelString(w))
    {
        XmStringFree(MB_OKLabelString(w));
    }
}

static void
delete_child(Widget w)
{
    Widget mb = XtParent(w);

    DEBUGOUT(_LtDebug2(__FILE__, mb, w, "delete_child\n"));

#define superclass (&xmBulletinBoardClassRec)
    (*superclass->composite_class.delete_child) (w);
#undef superclass

    if (w == MB_OKButton(mb))
    {
	MB_OKButton(mb) = NULL;
    }
    if (w == MB_HelpButton(mb))
    {
	MB_HelpButton(mb) = NULL;
    }
    /* The Cancel Button is dealt with in BulletinBoard - that's where it
     * belongs. */
}

/*
 * The ORIG_BITMAPS are created by the LessTif authors.
 * The others (ORIG_BITMAPS == 0) look an awful lot like the ones used in M*tif.
 */
#define ORIG_BITMAPS	0

/*
 * I just included some bitmaps here...
 *
 *      #include "warning.bm"
 */
#if ORIG_BITMAPS
#define warning_width 9
#define warning_height 22
static unsigned char warning_bits[] =
{
    0x00, 0x00, 0x10, 0x00, 0x38, 0x00, 0x6c, 0x00, 0xee, 0x00, 0xc6, 0x00,
    0xc6, 0x00, 0x6c, 0x00, 0x6c, 0x00, 0x6c, 0x00, 0x7c, 0x00, 0x38, 0x00,
    0x38, 0x00, 0x38, 0x00, 0x38, 0x00, 0x10, 0x00, 0x10, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x10, 0x00, 0x38, 0x00, 0x10, 0x00};
#else
#define warning_width 9
#define warning_height 22
static unsigned char warning_bits[] =
{
    0x00, 0x00, 0x18, 0x00, 0x2c, 0x00, 0x56, 0x00, 0x2a, 0x00, 0x56, 0x00,
    0x2a, 0x00, 0x56, 0x00, 0x2c, 0x00, 0x14, 0x00, 0x2c, 0x00, 0x14, 0x00,
    0x2c, 0x00, 0x14, 0x00, 0x00, 0x00, 0x00, 0x00, 0x3c, 0x00, 0x14, 0x00,
    0x2c, 0x00, 0x14, 0x00, 0x00, 0x00, 0x00, 0x00};
#endif

/* Question bitmap */
#if ORIG_BITMAPS
#define question_width 23
#define question_height 22
static unsigned char question_bits[] =
{
    0x00, 0x18, 0x00, 0x00, 0x18, 0x00, 0x7e, 0x18, 0x00, 0x78, 0x18, 0x0f,
    0x78, 0x98, 0x1f, 0x78, 0xd8, 0x39, 0x40, 0xd8, 0x30, 0x40, 0x18, 0x30,
    0x00, 0x18, 0x30, 0x00, 0x18, 0x38, 0x00, 0x18, 0x1c, 0x00, 0x18, 0x0e,
    0x00, 0x1f, 0x06, 0x00, 0x0c, 0x06, 0x00, 0x0c, 0x06, 0x00, 0x0c, 0x06,
    0x00, 0x0f, 0x00, 0x00, 0x0c, 0x06, 0x00, 0x0c, 0x0f, 0x00, 0x0c, 0x0f,
    0xf0, 0x03, 0x06, 0xc0, 0x03, 0x00, 0xc0, 0x03, 0x00};
#else
#define question_width 22
#define question_height 22
static unsigned char question_bits[] =
{
   0xf0, 0x3f, 0x00, 0x58, 0x55, 0x00, 0xac, 0xaa, 0x00, 0xd6, 0x5f, 0x01,
   0xea, 0xbf, 0x02, 0xf6, 0x7f, 0x01, 0xea, 0xba, 0x02, 0xf6, 0x7d, 0x05,
   0xea, 0xba, 0x0a, 0x56, 0x7d, 0x15, 0xaa, 0xbe, 0x1e, 0x56, 0x5f, 0x01,
   0xac, 0xaf, 0x02, 0x58, 0x57, 0x01, 0xb0, 0xaf, 0x00, 0x60, 0x55, 0x01,
   0xa0, 0xaa, 0x00, 0x60, 0x17, 0x00, 0xa0, 0x2f, 0x00, 0x60, 0x57, 0x00,
   0xb0, 0xaa, 0x00, 0x50, 0x55, 0x01};
#endif
#if 0
#define question_width 32
#define question_height 32
static unsigned char question_bits[] =
{
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0xfe, 0x07, 0x00,
    0x00, 0xab, 0x0a, 0x00, 0x80, 0x55, 0x15, 0x00, 0xc0, 0xfa, 0x2b, 0x00,
    0x40, 0xfd, 0x5f, 0x00, 0xc0, 0xfe, 0x2f, 0x00, 0x40, 0x5d, 0x5f, 0x00,
    0xc0, 0xbe, 0xaf, 0x00, 0x40, 0x5d, 0x5f, 0x01, 0xc0, 0xaa, 0xaf, 0x02,
    0x40, 0xd5, 0x57, 0x00, 0xc0, 0xea, 0x2b, 0x00, 0x80, 0xf5, 0x55, 0x00,
    0x00, 0xeb, 0x2a, 0x00, 0x00, 0xf6, 0x15, 0x00, 0x00, 0xac, 0x2a, 0x00,
    0x00, 0x54, 0x15, 0x00, 0x00, 0xec, 0x02, 0x00, 0x00, 0xf4, 0x05, 0x00,
    0x00, 0xec, 0x02, 0x00, 0x00, 0x56, 0x05, 0x00, 0x00, 0xaa, 0x0a, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00};
#endif

/* Information bitmap */
#if ORIG_BITMAPS
#define info_width 8
#define info_height 15
static unsigned char info_bits[] =
{
    0x08, 0x1c, 0x08, 0x00, 0x1e, 0x1e, 0x18, 0x18, 0x18, 0x18, 0x18, 0x18,
    0x18, 0xff, 0xff};
#else
#define info_width 11
#define info_height 24
static unsigned char info_bits[] =
{
    0x00, 0x00, 0x38, 0x00, 0x54, 0x00, 0x2c, 0x00, 0x54, 0x00, 0x28, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x7e, 0x00, 0x2a, 0x00, 0x5e, 0x00, 0x28, 0x00,
    0x58, 0x00, 0x28, 0x00, 0x58, 0x00, 0x28, 0x00, 0x58, 0x00, 0x28, 0x00,
    0x58, 0x00, 0xae, 0x01, 0x56, 0x01, 0xaa, 0x00, 0x00, 0x00, 0x00, 0x00};
#endif

/* Error bitmap */
#if ORIG_BITMAPS
#define error_width 16
#define error_height 16
static unsigned char error_bits[] =
{
    0x00, 0x00, 0xc0, 0x01, 0xf0, 0x07, 0x1c, 0x1c, 0x0c, 0x18, 0x1e, 0x30,
    0x32, 0x20, 0x63, 0x60, 0xc3, 0x61, 0x83, 0x63, 0x02, 0x26, 0x06, 0x3c,
    0x0c, 0x18, 0x1c, 0x1c, 0xf0, 0x07, 0xc0, 0x01};
#else
#define error_width 20
#define error_height 20
static unsigned char error_bits[] =
{
    0x00, 0x00, 0x00, 0xc0, 0x0f, 0x00, 0xf0, 0x3a, 0x00, 0x58, 0x55, 0x00,
    0x2c, 0xa0, 0x00, 0x56, 0x40, 0x01, 0xaa, 0x80, 0x02, 0x46, 0x81, 0x01,
    0x8a, 0x82, 0x02, 0x06, 0x85, 0x01, 0x0a, 0x8a, 0x02, 0x06, 0x94, 0x01,
    0x0a, 0xe8, 0x02, 0x14, 0x50, 0x01, 0x28, 0xb0, 0x00, 0xd0, 0x5f, 0x00,
    0xa0, 0x2a, 0x00, 0x40, 0x15, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00};
#endif

/* Working bitmap */
#if ORIG_BITMAPS
#define working_width 16
#define working_height 16
static unsigned char working_bits[] =
{
    0xff, 0xff, 0xf2, 0x4f, 0xf2, 0x4f, 0xe2, 0x47, 0xe2, 0x47, 0xc2, 0x43,
    0xc2, 0x43, 0x82, 0x41, 0x82, 0x41, 0xc2, 0x43, 0xc2, 0x43, 0xe2, 0x47,
    0xe2, 0x47, 0xf2, 0x4f, 0xf2, 0x4f, 0xff, 0xff};
/* end bitmap(s) */
#else
#define working_width 21
#define working_height 23
static unsigned char working_bits[] =
{
    0x00, 0x00, 0x00, 0xfe, 0xff, 0x0f, 0xaa, 0xaa, 0x0a, 0x44, 0x55, 0x06,
    0xcc, 0x2a, 0x02, 0x44, 0x55, 0x06, 0xcc, 0x2a, 0x02, 0x84, 0x15, 0x06,
    0x8c, 0x2a, 0x02, 0x04, 0x15, 0x06, 0x0c, 0x0a, 0x02, 0x04, 0x06, 0x06,
    0x0c, 0x0b, 0x02, 0x84, 0x15, 0x06, 0xcc, 0x2a, 0x02, 0x44, 0x55, 0x06,
    0xcc, 0x2a, 0x02, 0x44, 0x55, 0x06, 0xcc, 0x2a, 0x02, 0x44, 0x55, 0x06,
    0xfe, 0xff, 0x0f, 0x56, 0x55, 0x05, 0x00, 0x00, 0x00};
#endif

void
_XmMessageBoxInstallImages(Widget w)
{
    XImage *information_image;
    XImage *warning_image;
    XImage *question_image;
    XImage *error_image;
    XImage *working_image;

    _XmCreateImage(information_image, XtDisplay(w), (char *)info_bits,
		   info_width, info_height, LSBFirst);
    _XmCreateImage(warning_image, XtDisplay(w), (char *)warning_bits,
		   warning_width, warning_height, LSBFirst);
    _XmCreateImage(question_image, XtDisplay(w), (char *)question_bits,
		   question_width, question_height, LSBFirst);
    _XmCreateImage(error_image, XtDisplay(w), (char *)error_bits,
		   error_width, error_height, LSBFirst);
    _XmCreateImage(working_image, XtDisplay(w), (char *)working_bits,
		   working_width, working_height, LSBFirst);

    XmInstallImage(information_image, "xm_information");
    XmInstallImage(warning_image, "xm_warning");
    XmInstallImage(question_image, "xm_question");
    XmInstallImage(error_image, "xm_error");
    XmInstallImage(working_image, "xm_working");
}

/*
 * We use the Image Cache now.
 * Installed above - should be called from the XmDisplay initialize method.
 */
static Pixmap
_XmMessageBoxPixmap(Widget w, unsigned char dt)
{
    char *code;

    switch (dt)
    {
    case XmDIALOG_MESSAGE:
    case XmDIALOG_TEMPLATE:
    default:
	return (Pixmap)XmUNSPECIFIED_PIXMAP;

    case XmDIALOG_ERROR:
	code = "xm_error";
	break;

    case XmDIALOG_INFORMATION:
	code = "xm_information";
	break;

    case XmDIALOG_QUESTION:
	code = "xm_question";
	break;

    case XmDIALOG_WORKING:
	code = "xm_working";
	break;

    case XmDIALOG_WARNING:
	code = "xm_warning";
	break;
    }

    return XmGetPixmap(XtScreen(w), code,
		       MGR_Foreground(w), XtBackground(w));
}

static Boolean
XmMbSetSubresources(Widget new_w, Widget old, ArgList args, Cardinal *nargs)
{
    Boolean refresh_needed = False;

    /* Make sure you can compare */
    if (old == NULL)
    {
	old = new_w;
    }

    if (MB_Message(new_w) &&
	MB_MessageString(new_w) != MB_MessageString(old) &&
	!XmStringCompare(MB_MessageString(new_w), MB_MessageString(old)))
    {
	Arg al[2];

	if (MB_MessageString(new_w) != NULL && !XtIsManaged(MB_Message(new_w)))
	{
	    XtManageChild(MB_Message(new_w));
	}
	else if (MB_MessageString(new_w) == NULL &&
		 XtIsManaged(MB_Message(new_w)))
	{
	    XtUnmanageChild(MB_Message(new_w));
	}

	XtSetArg(al[0], XmNlabelString, MB_MessageString(new_w));
	XtSetArg(al[1], XmNlabelType, XmSTRING);
	XtSetValues(MB_Message(new_w), al, 2);

	refresh_needed = True;
    }

    if (MB_Symbol(new_w) &&
	MB_DialogType(old) != MB_DialogType(new_w))
    {
	Arg al[2];

	MB_SymbolPixmap(new_w) = _XmMessageBoxPixmap(new_w,
							 MB_DialogType(new_w));

	XtSetArg(al[0], XmNlabelPixmap, MB_SymbolPixmap(new_w));
	XtSetArg(al[1], XmNlabelType, XmPIXMAP);
	XtSetValues(MB_Symbol(new_w), al, 2);

	if (MB_DialogType(new_w) == XmDIALOG_MESSAGE &&
	    XtIsManaged(MB_Symbol(new_w)))
	{
	    XtUnmanageChild(MB_Symbol(new_w));
	}
	else if (MB_Symbol(new_w) && MB_DialogType(new_w) != XmDIALOG_MESSAGE &&
		 !XtIsManaged(MB_Symbol(new_w)))
	{
	    XtUnmanageChild(MB_Symbol(new_w));
	}

	refresh_needed = True;
    }

    if (MB_Symbol(new_w) &&
	MB_SymbolPixmap(old) != MB_SymbolPixmap(new_w))
    {
	Arg al[2];

	if (MB_SymbolPixmap(new_w) == XmUNSPECIFIED_PIXMAP)
	{
	    XtSetArg(al[1], XmNlabelType, XmSTRING);
	    XtSetValues(MB_Symbol(new_w), al, 1);
	}
	else
	{
	    if (!XtIsManaged(MB_Symbol(new_w)))
	    {
		XtManageChild(MB_Symbol(new_w));
	    }
	    if (!XtIsManaged(MB_Message(new_w)))
	    {
		XtManageChild(MB_Message(new_w));
	    }

	    XtSetArg(al[0], XmNlabelPixmap, MB_SymbolPixmap(new_w));
	    XtSetArg(al[1], XmNlabelType, XmPIXMAP);
	    XtSetValues(MB_Symbol(new_w), al, 2);
	}

	refresh_needed = True;
    }

    if (MB_HelpButton(new_w) &&
	MB_HelpLabelString(new_w) != MB_HelpLabelString(old))
    {
        if (!XmStringCompare(MB_HelpLabelString(new_w), MB_HelpLabelString(old)))
        {
            Arg al[2];

            XtSetArg(al[0], XmNlabelString, MB_HelpLabelString(new_w));
            XtSetArg(al[1], XmNlabelType, XmSTRING);
            XtSetValues(MB_HelpButton(new_w), al, 2);

            refresh_needed = True;
        }
        MB_HelpLabelString(new_w) = XmStringCopy(MB_HelpLabelString(new_w));
        XmStringFree(MB_HelpLabelString(old));
    }

    if (BB_CancelButton(new_w) &&
	MB_CancelLabelString(new_w) != MB_CancelLabelString(old))
    {
	if (!XmStringCompare(MB_CancelLabelString(new_w),
                             MB_CancelLabelString(old)))
        {
            Arg al[2];

            XtSetArg(al[0], XmNlabelString, MB_CancelLabelString(new_w));
            XtSetArg(al[1], XmNlabelType, XmSTRING);
            XtSetValues(BB_CancelButton(new_w), al, 2);

            refresh_needed = True;
        }
        MB_CancelLabelString(new_w) = XmStringCopy(MB_CancelLabelString(new_w));
        XmStringFree(MB_CancelLabelString(old));
    }

    if (MB_OKButton(new_w) &&
	MB_OKLabelString(new_w) != MB_OKLabelString(old))
    {
        if (!XmStringCompare(MB_OKLabelString(new_w), MB_OKLabelString(old)))
        {
            Arg al[2];

            XtSetArg(al[0], XmNlabelString, MB_OKLabelString(new_w));
            XtSetArg(al[1], XmNlabelType, XmSTRING);
            XtSetValues(MB_OKButton(new_w), al, 2);

            refresh_needed = True;
        }
        MB_OKLabelString(new_w) = XmStringCopy(MB_OKLabelString(new_w));
        XmStringFree(MB_OKLabelString(old));
    }

    return refresh_needed;
}

static Boolean
set_values(Widget old, Widget request, Widget new_w,
	   ArgList args, Cardinal *num_args)
{
    Boolean refresh_needed;

    DEBUGOUT(_LtDebug(__FILE__, new_w,
		      "set_values: %i args\n"
		      "\t    old X %5i Y %5i W %5i H %5i\n"
		      "\trequest X %5i Y %5i W %5i H %5i\n"
		      "\t  new_w X %5i Y %5i W %5i H %5i\n",
		      *num_args,
		      XtX(old), XtY(old),
		      XtWidth(old), XtHeight(old),
		      XtX(request), XtY(request),
		      XtWidth(request), XtHeight(request),
		      XtX(new_w), XtY(new_w),
		      XtWidth(new_w), XtHeight(new_w)));
    DEBUGOUT(_LtDebugPrintArgList(__FILE__, new_w, args, *num_args, False));

    BB_InSetValues(new_w) = True;

    /* Must do this call first */
    refresh_needed = XmMbSetSubresources(new_w, old, args, num_args);

    /* Copy MB_MessageString */
    if ((MB_MessageString(new_w) != MB_MessageString(old)) && MB_MessageString(new_w)) {
	MB_MessageString(new_w) = XmStringCopy(MB_MessageString(new_w));
	if (MB_MessageString(old))
	    XmStringFree(MB_MessageString(old));
    }
    if (refresh_needed || XtWidth(old) != XtWidth(new_w) ||
	XtHeight(old) != XtHeight(new_w))
    {
	refresh_needed = True;
    }

    BB_InSetValues(new_w) = False;

    if (refresh_needed && (XtClass(new_w) == xmMessageBoxWidgetClass))
    {
	_XmBulletinBoardSizeUpdate(new_w);
	return False;
    }

    return refresh_needed;
}

XmGeoMatrix
_XmMessageBoxGeoMatrixCreate(Widget _w, Widget _from, XtWidgetGeometry *_pref)
{
    XmGeoMatrix geoSpec;
    register XmGeoRowLayout layoutPtr;
    register XmKidGeometry boxPtr;
    Cardinal numKids, i, nextras;
    Boolean newRow, tookChild;
    int nrows;
    Widget *extras;

    numKids = MGR_NumChildren(_w);

    nextras = 0;
    extras = NULL;
    for (i = 0; i < numKids; i++)
    {
	if (MGR_Children(_w)[i] != MB_Symbol(_w) &&
	    MGR_Children(_w)[i] != MB_Message(_w) &&
	    MGR_Children(_w)[i] != MB_Separator(_w) &&
	    MGR_Children(_w)[i] != MB_OKButton(_w) &&
	    MGR_Children(_w)[i] != MB_HelpButton(_w) &&
	    MGR_Children(_w)[i] != BB_CancelButton(_w))
	{
	    nextras++;
	}
    }

    if (nextras)
    {
	extras = (Widget *)XtMalloc(sizeof(Widget) * nextras);
    }

    nextras = 0;
    for (i = 0; i < numKids; i++)
    {
	if (MGR_Children(_w)[i] != MB_Symbol(_w) &&
	    MGR_Children(_w)[i] != MB_Message(_w) &&
	    MGR_Children(_w)[i] != MB_Separator(_w) &&
	    MGR_Children(_w)[i] != MB_OKButton(_w) &&
	    MGR_Children(_w)[i] != MB_HelpButton(_w) &&
	    MGR_Children(_w)[i] != BB_CancelButton(_w))
	{
	    extras[nextras] = MGR_Children(_w)[i];
	    nextras++;
	}
    }

    nrows = 0;

    numKids = MGR_NumChildren(_w);

    if (nextras > 0)
    {
	/* look for menu bar */
	for (i = 0; i < nextras; i++)
	{
	    if (XmIsMenuBar(extras[i]) && XtIsManaged(extras[i]))
		nrows++;
	}
    }

#if 0
/* rws 15 Nov 1998
_XmMessageBoxGeoMatrixCreate() - When counting the rows that we need to 
	allocate we were putting the work area on the same row as the
	message and symbol, however we were doing the actual layout with
	the work area on it's own row _iff_ there is a message.  So, if there
	is a message and a work area we are laying out for 4 rows but only
	allocating 3, which is not such a good idea.
 */
    if ((MB_Message(_w) && XtIsManaged(MB_Message(_w))) ||
	(MB_Symbol(_w) && XtIsManaged(MB_Symbol(_w))))
    {
	nrows++;
    }
    /* work area.  Algorithm per Programmer's reference 1-673 */
    else if (nextras > 0)
    {
	/* look for work area */
	for (i = 0; i < nextras; i++)
	{
	    if (extras[i] && XtIsManaged(extras[i]) &&
		!XmIsMenuBar(extras[i]) && !XmIsPushButton(extras[i]) &&
		!XmIsPushButtonGadget(extras[i]))
	    {
		if (MB_Message(_w) && XtIsManaged(MB_Message(_w)))
		{
		    nrows++;
		    break;
		}
		else if ((MB_Symbol(_w) && XtIsManaged(MB_Symbol(_w))))
		{
		    break;
		}
		else
		{
		    nrows++;
		    break;
		}
	    }
	}
    }
#else
    if ((MB_Message(_w) && XtIsManaged(MB_Message(_w))) ||
	(MB_Symbol(_w) && XtIsManaged(MB_Symbol(_w))))
    {
	/* message and symbol are always on there own row */
	nrows++;
    }
    if (nextras > 0)
    {
	/* look for work area */
	for (i = 0; i < nextras; i++)
	{
	    if (extras[i] && XtIsManaged(extras[i]) &&
		!XmIsMenuBar(extras[i]) && !XmIsPushButton(extras[i]) &&
		!XmIsPushButtonGadget(extras[i]))
	    {
		if (MB_Message(_w) && XtIsManaged(MB_Message(_w)))
		{
		    /* work area is always below a message */
		    nrows++;
		    break;
		}
		else if ((MB_Symbol(_w) && XtIsManaged(MB_Symbol(_w))))
		{
		    /* work area is beside a symbol */
		    break;
		}
		else
		{
		    /* work area is alone on the first row */
		    nrows++;
		    break;
		}
	    }
	}
    }
#endif

    if (MB_Separator(_w) && XtIsManaged(MB_Separator(_w)))
    {
	nrows++;
    }

    if ((BB_CancelButton(_w) && XtIsManaged(BB_CancelButton(_w))) ||
	(MB_OKButton(_w) && XtIsManaged(MB_OKButton(_w))) ||
	(MB_HelpButton(_w) && XtIsManaged(MB_HelpButton(_w))))
    {
	nrows++;
    }
    else
    {
	for (i = 0; i < nextras; i++)
	{
	    if (extras[i] && XtIsManaged(extras[i]) &&
		(XmIsPushButton(extras[i]) || XmIsPushButtonGadget(extras[i])))
	    {
		nrows++;
		break;
	    }
	}
    }

    geoSpec = _XmGeoMatrixAlloc(nrows, numKids, 0);
    geoSpec->composite = (Widget)_w;
    geoSpec->instigator = (Widget)_from;
    if (_pref)
    {
	geoSpec->instig_request = *_pref;
    }
    geoSpec->margin_w = BB_MarginWidth(_w) + MGR_ShadowThickness(_w);
    geoSpec->margin_h = BB_MarginHeight(_w) + MGR_ShadowThickness(_w);
    geoSpec->no_geo_request = _XmMessageBoxNoGeoRequest;

    layoutPtr = &(geoSpec->layouts->row);
    boxPtr = geoSpec->boxes;

    for (i = 0; i < nextras; i++)
    {
	if (XmIsMenuBar(extras[i]) && XtIsManaged(extras[i]) &&
	    _XmGeoSetupKid(boxPtr, extras[i]))
	{
	    layoutPtr->fix_up = _XmMenuBarFix;
	    layoutPtr->space_above = 0;
	    boxPtr += 2;
	    layoutPtr++;
	    break;
	}
    }

    newRow = False;
    tookChild = False;
    if (MB_Symbol(_w) && XtIsManaged(MB_Symbol(_w)) &&
	_XmGeoSetupKid(boxPtr, MB_Symbol(_w)) &&
	 MB_DialogType(_w) != XmDIALOG_MESSAGE &&
	 MB_DialogType(_w) != XmDIALOG_TEMPLATE)
    {
	layoutPtr->even_width = 0;
	layoutPtr->even_height = 0;
	layoutPtr->space_above = BB_MarginHeight(_w);
	layoutPtr->space_between = BB_MarginWidth(_w);
	newRow = True;
	boxPtr++;
    }

    if (MB_Message(_w) && XtIsManaged(MB_Message(_w)) &&
	_XmGeoSetupKid(boxPtr, MB_Message(_w)))
    {
	layoutPtr->space_end = 0;
	layoutPtr->even_width = 0;
	layoutPtr->even_height = 0;
	layoutPtr->space_above = BB_MarginHeight(_w);
	layoutPtr->space_between = BB_MarginWidth(_w);
	newRow = True;
	boxPtr++;
    }
    /* Adjacent to Symbol */
    else if (newRow && nextras)
    {
	for (i = 0; i < nextras; i++)
	{
	    if (XtIsManaged(extras[i]) &&
		!XmIsMenuBar(extras[i]) &&
		!(XmIsPushButton(extras[i]) ||
		  XmIsPushButtonGadget(extras[i])) &&
		_XmGeoSetupKid(boxPtr, extras[i]))
	    {
		layoutPtr->even_width = 0;
		layoutPtr->stretch_height = 1;
		layoutPtr->even_height = 1;
		layoutPtr->space_above = BB_MarginHeight(_w);
		layoutPtr->space_between = BB_MarginWidth(_w);
		newRow = True;
		tookChild = True;
		boxPtr++;
		break;
	    }
	}
    }

    
    if (newRow)
    {
	layoutPtr++;
	boxPtr++;
    }

    /* work area on own line, either because not adjacent to Symbol,
     * or no symbol and no message given. */
    if (nextras && !tookChild)
    {
	for (i = 0; i < nextras; i++)
	{
	    if (XtIsManaged(extras[i]) &&
		!(XmIsPushButton(extras[i]) ||
		  XmIsPushButtonGadget(extras[i])) &&
		!XmIsMenuBar(extras[i]) && _XmGeoSetupKid(boxPtr, extras[i]))
	    {
		layoutPtr->fill_mode = XmGEO_EXPAND;
		layoutPtr->even_width = 0;
		layoutPtr->stretch_height = 1;
		layoutPtr->space_above = BB_MarginHeight(_w);
		layoutPtr++;
		boxPtr += 2;
		break;
	    }
	}
    }

    if (MB_Separator(_w) && XtIsManaged(MB_Separator(_w)) &&
	_XmGeoSetupKid(boxPtr, MB_Separator(_w)))
    {
	layoutPtr->fix_up = _XmSeparatorFix;
	layoutPtr->space_above = BB_MarginHeight(_w);
	boxPtr += 2;
	layoutPtr++;
    }

    newRow = False;
    /* rws 28 Mar 1999
       messagebox/test18 shows this wackyness. If we don't have an OK button
       these are 6, if we do, they are 4. (go figure!!!)
       messagebox/test10 shows that extra buttons are also 4.
     */
    layoutPtr->even_width = 6;
    layoutPtr->even_height = 6;
    if (MB_OKButton(_w) && XtIsManaged(MB_OKButton(_w)) &&
	_XmGeoSetupKid(boxPtr++, MB_OKButton(_w)))
    {
	layoutPtr->fill_mode = XmGEO_CENTER;
	layoutPtr->fit_mode = XmGEO_WRAP;
	layoutPtr->even_width = 4;
	layoutPtr->even_height = 4;
	layoutPtr->space_above = BB_MarginHeight(_w);
	newRow = True;
    }
    for (i = 0; i < nextras; i++)
    {
	if (extras[i] && XtIsManaged(extras[i]) &&
	    (XmIsPushButton(extras[i]) || XmIsPushButtonGadget(extras[i])) &&
	    _XmGeoSetupKid(boxPtr++, extras[i]))
	{
	    _XmBulletinBoardSetDefaultShadow(extras[i]);
	    layoutPtr->fill_mode = XmGEO_CENTER;
	    layoutPtr->fit_mode = XmGEO_WRAP;
	    layoutPtr->even_width = 4;
	    layoutPtr->even_height = 4;
	    layoutPtr->space_above = BB_MarginHeight(_w);
	    newRow = True;
	}
    }

    if (BB_CancelButton(_w) && XtIsManaged(BB_CancelButton(_w)) &&
	_XmGeoSetupKid(boxPtr++, BB_CancelButton(_w)))
    {
	layoutPtr->fill_mode = XmGEO_CENTER;
	layoutPtr->fit_mode = XmGEO_WRAP;
	/*
	layoutPtr->even_width = 4;
	layoutPtr->even_height = 4;
	*/
	layoutPtr->space_above = BB_MarginHeight(_w);
	newRow = True;
    }
    if (MB_HelpButton(_w) && XtIsManaged(MB_HelpButton(_w)) &&
	_XmGeoSetupKid(boxPtr++, MB_HelpButton(_w)))
    {
	layoutPtr->fill_mode = XmGEO_CENTER;
	layoutPtr->fit_mode = XmGEO_WRAP;
	/*
	layoutPtr->even_width = 4;
	layoutPtr->even_height = 4;
	*/
	layoutPtr->space_above = BB_MarginHeight(_w);
	newRow = True;
    }

    if (newRow)
    {
	layoutPtr++;
	boxPtr++;
    }

    layoutPtr->space_above = 0;	/* BB_MarginHeight(_w); */
    layoutPtr->end = True;
    if (nextras)
    {
	XtFree((char *)extras);
    }

    return (geoSpec);
}

Boolean
_XmMessageBoxNoGeoRequest(XmGeoMatrix _geoSpec)
{
    if (BB_InSetValues(_geoSpec->composite) &&
	(XtClass(_geoSpec->composite) == xmMessageBoxWidgetClass))
    {
	return (True);
    }

    return (False);
}

/*
 * This routine is called for any button child of message box for the
 * XmNactivateCallback. Make mapping to XmNokCallback etc.
 */
static void 
_XmMbButton(Widget w, XtPointer client, XtPointer call)
{
    XmMessageBoxWidget sb = (XmMessageBoxWidget)XtParent(w);
    XmPushButtonCallbackStruct *pbs = (XmPushButtonCallbackStruct *)call;
    XmAnyCallbackStruct cbs;

    DEBUGOUT(_LtDebug(__FILE__, w, "Button Press\n"));

    if (w == MB_OKButton(sb))
    {
	cbs.reason = XmCR_OK;
	if (call)
	{
	    cbs.event = pbs->event;
	}
	XtCallCallbackList((Widget)sb, MB_OKCall(sb), (XtPointer)&cbs);
    }
    else if (w == BB_CancelButton(sb))
    {
	cbs.reason = XmCR_CANCEL;
	if (call)
	{
	    cbs.event = pbs->event;
	}
	XtCallCallbackList((Widget)sb, MB_CancelCall(sb),
			   (XtPointer)&cbs);
    }
    else if (w == MB_HelpButton(sb))
    {
	cbs.reason = XmCR_HELP;
	if (call)
	{
	    cbs.event = pbs->event;
	}
	XtCallCallbackList((Widget)sb, MGR_HelpCallbackList(sb),
			   (XtPointer)&cbs);
    }
}

static void
_XmMessageBoxExportPixmap(Widget w, int offset, XtArgVal *value)
{
    Pixmap pix = XmUNSPECIFIED_PIXMAP;

    DEBUGOUT(_LtDebug(__FILE__, w, "_XmMessageBoxExportPixmap\n"));

    XtVaGetValues(MB_Symbol(w), XmNlabelPixmap, &pix, NULL);
    *value = (XtArgVal)pix;
}

Widget
XmCreateMessageBox(Widget parent,
		   char *name,
		   Arg *arglist,
		   Cardinal argcount)
{
    return XtCreateWidget(name,
			  xmMessageBoxWidgetClass,
			  parent,
			  arglist,
			  argcount);
}

Widget
XmCreateErrorDialog(Widget parent,
		    char *name,
		    Arg *arglist,
		    Cardinal argcount)
{
    Widget shell, r;
    char *shell_name;
    Arg *al;
    Cardinal nargs, i;

    shell_name = _XmMakeDialogName(name);

    al = (Arg *)XtCalloc(argcount + 2, sizeof(Arg));

    nargs = 0;
    XtSetArg(al[nargs], XmNallowShellResize, True);
    nargs++;
    XtSetArg(al[nargs], XmNdialogType, XmDIALOG_ERROR);
    nargs++;

    for (i = 0; i < argcount; i++)
    {
	XtSetArg(al[nargs], arglist[i].name, arglist[i].value);
	nargs++;
    }

    shell = XmCreateDialogShell(parent, shell_name, al, nargs);
    XtFree(shell_name);

    r = XmCreateMessageBox(shell, name, al, nargs);
    XtFree((char *)al);

    return r;
}

Widget
XmCreateInformationDialog(Widget parent,
			  char *name,
			  Arg *arglist,
			  Cardinal argcount)
{
    Widget shell, r;
    char *shell_name;
    Arg *al;
    Cardinal nargs, i;

    shell_name = _XmMakeDialogName(name);

    al = (Arg *)XtCalloc(argcount + 2, sizeof(Arg));

    nargs = 0;
    XtSetArg(al[nargs], XmNallowShellResize, True);
    nargs++;
    XtSetArg(al[nargs], XmNdialogType, XmDIALOG_INFORMATION);
    nargs++;

    for (i = 0; i < argcount; i++)
    {
	XtSetArg(al[nargs], arglist[i].name, arglist[i].value);
	nargs++;
    }

    shell = XmCreateDialogShell(parent, shell_name, al, nargs);
    XtFree(shell_name);

    r = XmCreateMessageBox(shell, name, al, nargs);
    XtFree((char *)al);

    return r;
}

Widget
XmCreateMessageDialog(Widget parent,
		      char *name,
		      Arg *arglist,
		      Cardinal argcount)
{
    Widget shell, r;
    char *shell_name;
    Arg *al;
    Cardinal nargs, i;

    shell_name = _XmMakeDialogName(name);

    al = (Arg *)XtCalloc(argcount + 2, sizeof(Arg));

    nargs = 0;
    XtSetArg(al[nargs], XmNallowShellResize, True); nargs++;

    for (i = 0; i < argcount; i++)
    {
	XtSetArg(al[nargs], arglist[i].name, arglist[i].value); nargs++;
    }

    shell = XmCreateDialogShell(parent, shell_name, al, nargs);
    XtFree(shell_name);

    r = XmCreateMessageBox(shell, name, al, nargs);
    XtAddCallback(r, XmNdestroyCallback,
		  _XmDestroyParentCallback,
		  (XtPointer)r);
    XtFree((char *)al);

    return r;
}


Widget
XmCreateQuestionDialog(Widget parent,
		       char *name,
		       Arg *arglist,
		       Cardinal argcount)
{
    Widget shell, r;
    char *shell_name;
    Arg *al;
    Cardinal nargs, i;

    shell_name = _XmMakeDialogName(name);

    al = (Arg *)XtCalloc(argcount + 2, sizeof(Arg));

    nargs = 0;
    XtSetArg(al[nargs], XmNallowShellResize, True);
    nargs++;
    XtSetArg(al[nargs], XmNdialogType, XmDIALOG_QUESTION);
    nargs++;

    for (i = 0; i < argcount; i++)
    {
	XtSetArg(al[nargs], arglist[i].name, arglist[i].value);
	nargs++;
    }

    shell = XmCreateDialogShell(parent, shell_name, al, nargs);
    XtFree(shell_name);

    r = XmCreateMessageBox(shell, name, al, nargs);
    XtFree((char *)al);

    return r;
}


Widget
XmCreateTemplateDialog(Widget parent,
		       char *name,
		       Arg *arglist,
		       Cardinal argcount)
{
    Widget shell, r;
    char *shell_name;
    Arg *al;
    Cardinal nargs, i;

    shell_name = _XmMakeDialogName(name);

    al = (Arg *)XtCalloc(argcount + 2, sizeof(Arg));

    nargs = 0;
    XtSetArg(al[nargs], XmNallowShellResize, True); nargs++;
    XtSetArg(al[nargs], XmNdialogType, XmDIALOG_TEMPLATE); nargs++;

    for (i = 0; i < argcount; i++)
    {
	XtSetArg(al[nargs], arglist[i].name, arglist[i].value);
	nargs++;
    }

    shell = XmCreateDialogShell(parent, shell_name, al, nargs);
    XtFree(shell_name);

    r = XmCreateMessageBox(shell, name, al, nargs);
    XtFree((char *)al);
    return r;
}


Widget
XmCreateWarningDialog(Widget parent,
		      char *name,
		      Arg *arglist,
		      Cardinal argcount)
{
    Widget shell, r;
    char *shell_name;
    Arg *al;
    Cardinal nargs, i;

    shell_name = _XmMakeDialogName(name);

    al = (Arg *)XtCalloc(argcount + 2, sizeof(Arg));

    nargs = 0;
    XtSetArg(al[nargs], XmNallowShellResize, True);
    nargs++;
    XtSetArg(al[nargs], XmNdialogType, XmDIALOG_WARNING);
    nargs++;

    for (i = 0; i < argcount; i++)
    {
	XtSetArg(al[nargs], arglist[i].name, arglist[i].value);
	nargs++;
    }

    shell = XmCreateDialogShell(parent, shell_name, al, nargs);
    XtFree(shell_name);

    r = XmCreateMessageBox(shell, name, al, nargs);
    XtFree((char *)al);

    return r;
}

Widget
XmCreateWorkingDialog(Widget parent,
		      char *name,
		      Arg *arglist,
		      Cardinal argcount)
{
    Widget shell, r;
    char *shell_name;
    Arg *al;
    Cardinal nargs, i;

    shell_name = _XmMakeDialogName(name);

    al = (Arg *)XtCalloc(argcount + 2, sizeof(Arg));

    nargs = 0;
    XtSetArg(al[nargs], XmNallowShellResize, True);
    nargs++;
    XtSetArg(al[nargs], XmNdialogType, XmDIALOG_WORKING);
    nargs++;

    for (i = 0; i < argcount; i++)
    {
	XtSetArg(al[nargs], arglist[i].name, arglist[i].value);
	nargs++;
    }

    shell = XmCreateDialogShell(parent, shell_name, al, nargs);
    XtFree(shell_name);

    r = XmCreateMessageBox(shell, name, al, nargs);
    XtFree((char *)al);

    return r;
}

Widget
XmMessageBoxGetChild(Widget parent,
		     unsigned char child)
{
    XmMessageBoxWidget mb = (XmMessageBoxWidget)parent;

    switch (child)
    {
    case XmDIALOG_CANCEL_BUTTON:
	return BB_CancelButton(mb);

    case XmDIALOG_DEFAULT_BUTTON:
	return BB_DefaultButton(mb);

    case XmDIALOG_HELP_BUTTON:
	return MB_HelpButton(mb);

    case XmDIALOG_MESSAGE_LABEL:
	return MB_Message(mb);

    case XmDIALOG_OK_BUTTON:
	return MB_OKButton(mb);

    case XmDIALOG_SEPARATOR:
	return MB_Separator(mb);

    case XmDIALOG_SYMBOL_LABEL:
	return MB_Symbol(mb);

    default:
	/* invalid child */
	return NULL;
    }
}
