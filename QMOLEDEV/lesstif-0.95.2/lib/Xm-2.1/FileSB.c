/**
 *
 * $Header: /cvsroot/lesstif/lesstif/lib/Xm-2.1/FileSB.c,v 1.5 2005/11/01 08:14:56 dannybackx Exp $
 *
 * Copyright (C) 1997 Free Software Foundation, Inc.
 * Copyright © 1997, 1998, 1999, 2000, 2001, 2002, 2004 LessTif Development Team
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

static const char rcsid[] = "$Header: /cvsroot/lesstif/lesstif/lib/Xm-2.1/FileSB.c,v 1.5 2005/11/01 08:14:56 dannybackx Exp $";

#include <LTconfig.h>

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#ifdef HAVE_LIBGEN_H
#include <libgen.h>    /* for basename() */
#endif


#include <XmI/XmI.h>
#include <Xm/XmP.h>
#include <Xm/List.h>
#include <Xm/SelectioBP.h>
#include <Xm/ListP.h>
#include <Xm/FileSBP.h>
#include <Xm/RowColumnP.h>
#include <Xm/ScrolledWP.h>
#include <Xm/ScrollBarP.h>
#include <Xm/XmosP.h>
#include <Xm/TextF.h>
#include <Xm/DialogS.h>
#include <Xm/TransltnsP.h>

#include <XmI/LTmisc.h>
#include <XmI/MacrosI.h>

#include <XmI/DebugUtil.h>


/* #defines being used: */

/* see below for more info */
#define MOZILLA

/* Forward Declarations */

static void class_initialize(void);
static void class_part_initialize(WidgetClass w_class);
static void initialize(Widget request, Widget new_w,
		       ArgList args, Cardinal *num_args);
static void destroy(Widget w);
static Boolean set_values(Widget current, Widget request, Widget new_w,
			  ArgList args, Cardinal *num_args);

#if 0
/* Motif has one of these */
static void get_values_hook(Widget w, ArgList args, Cardinal *num_args);
#endif

static XmGeoMatrix geoMatrixCreate(Widget _w, Widget _from,
				   XtWidgetGeometry *_pref);
static void focus_moved_proc(Widget wid, XtPointer client_data, XtPointer data);
static Boolean noGeoRequest(XmGeoMatrix _geoSpec);
static void defaultDirSearchProc(Widget widget, XtPointer search_data);
static void defaultFileSearchProc(Widget widget, XtPointer search_data);
static void defaultQualifySearchDataProc(Widget widget, XtPointer input_data, XtPointer output_data);
static void _XmFileSelectionSearch(Widget w);
static void _XmFsbButton(Widget w, XtPointer client, XtPointer call);
static void _XmFsbFileSelect(Widget w, XtPointer client, XtPointer call);
static XtAccelerators text_accelerators_parsed = NULL;


/*
 * Resources for the FileSelection Box class
 */
#define Offset(field) XtOffsetOf(XmFileSelectionBoxRec, file_selection_box.field)
#define SBOffset(field) XtOffsetOf(XmFileSelectionBoxRec, selection_box.field)
static XtResource resources[] =
{
    {
	XmNdirectory, XmCDirectory, XmRXmString,
	sizeof(XmString), Offset(directory),
	XmRXmString, (XtPointer)NULL
    },
    {
	XmNpattern, XmCPattern, XmRXmString,
	sizeof(XmString), Offset(pattern),
	XmRImmediate, (XtPointer)NULL
    },
    {
	XmNdirListLabelString, XmCDirListLabelString, XmRXmString,
	sizeof(XmString), Offset(dir_list_label_string),
	XmRImmediate, (XtPointer)XmUNSPECIFIED
    },
    {
	XmNdirListItems, XmCDirListItems, XmRXmStringTable,
	sizeof(XmStringTable), Offset(dir_list_items),
	XmRImmediate, (XtPointer)NULL
    },
    {
	XmNdirListItemCount, XmCDirListItemCount, XmRInt,
	sizeof(int), Offset(dir_list_item_count),
	XmRImmediate, (XtPointer)XmUNSPECIFIED
    },
    {
	XmNfilterLabelString, XmCFilterLabelString, XmRXmString,
	sizeof(XmString), Offset(filter_label_string),
	XmRImmediate, (XtPointer)XmUNSPECIFIED
    },
    {
	XmNdirMask, XmCDirMask, XmRXmString,
	sizeof(XmString), Offset(dir_mask),
	XmRImmediate, (XtPointer)XmUNSPECIFIED
    },
    {
	XmNnoMatchString, XmCNoMatchString, XmRXmString,
	sizeof(XmString), Offset(no_match_string),
	XmRImmediate, (XtPointer)XmUNSPECIFIED
    },
    {
	XmNqualifySearchDataProc, XmCQualifySearchDataProc, XmRProc,
	sizeof(XmQualifyProc), Offset(qualify_search_data_proc),
	XmRImmediate, (XtPointer)defaultQualifySearchDataProc
    },
    {
	XmNdirSearchProc, XmCDirSearchProc, XmRProc,
	sizeof(XmSearchProc), Offset(dir_search_proc),
	XmRImmediate, (XtPointer)defaultDirSearchProc
    },
    {
	XmNfileSearchProc, XmCFileSearchProc, XmRProc,
	sizeof(XmSearchProc), Offset(file_search_proc),
	XmRImmediate, (XtPointer)defaultFileSearchProc
    },
    {
	XmNfileTypeMask, XmCFileTypeMask, XmRFileTypeMask,
	sizeof(unsigned char), Offset(file_type_mask),
	XmRImmediate, (XtPointer)XmFILE_REGULAR
    },
    {
	XmNlistUpdated, XmCListUpdated, XmRBoolean,
	sizeof(Boolean), Offset(list_updated),
	XmRImmediate, (XtPointer)True
    },
    {
	XmNdirectoryValid, XmCDirectoryValid, XmRBoolean,
	sizeof(Boolean), Offset(directory_valid),
	XmRImmediate, (XtPointer)True
    },
    {
	XmNdirSpec, XmCDirSpec, XmRXmString,
	sizeof(XmString), SBOffset(text_string),
	XmRImmediate, (XtPointer)XmUNSPECIFIED
    },
    {
	XmNautoUnmanage, XmCAutoUnmanage, XmRBoolean,
	sizeof(Boolean), XtOffsetOf(XmFileSelectionBoxRec, bulletin_board.auto_unmanage),
	XmRImmediate, (XtPointer)False
    },
    {
	XmNfileListLabelString, XmCFileListLabelString, XmRXmString,
	sizeof(XmString), SBOffset(list_label_string),
	XmRImmediate, (XtPointer)XmUNSPECIFIED
    },
    {
	XmNapplyLabelString, XmCApplyLabelString, XmRXmString,
	sizeof(XmString), SBOffset(apply_label_string),
	XmRImmediate, (XtPointer)XmUNSPECIFIED /*"Filter"*/
    },
    {
	XmNdialogType, XmCDialogType, XmRSelectionType,
	sizeof(unsigned char), SBOffset(dialog_type),
	XmRImmediate, (XtPointer)XmDIALOG_FILE_SELECTION
    },
    {
	XmNfileListItems, XmCItems, XmRXmStringTable,
	sizeof(XmStringTable), SBOffset(list_items),
	XmRImmediate, (XtPointer)NULL
    },
    {
	XmNfileListItemCount, XmCItemCount, XmRInt,
	sizeof(int), SBOffset(list_item_count),
	XmRImmediate, (XtPointer)XmUNSPECIFIED
    },
/*
 *	XmNpathMode
 *	Specifies whether or not an additional text field will be used 
 *	to display and edit the filter. The possible values are
 *
 *	XmPATH_MODE_FULL
 *		Specifies that no additional text field will be used
 *		to display the filter. There will just be a single
 *		text field to display XmNdirMask.
 *
 *	XmPATH_MODE_RELATIVE
 *		Specifies that there will be two text field displays,
 *		one to display the XmNdirectory and one to display
 *		the XmNpattern. In this instance, the XmNfilterLabelString
 *		resource applies to the text field for XmNpattern and
 *		XmNdirTextLabelString applies to the text field for
 *		XmNdirectory.
 */
    {
	XmNpathMode, XmCPathMode, XmRPathMode,
	sizeof(XtEnum), Offset(path_mode),
	XmRImmediate, (XtPointer)XmPATH_MODE_FULL
    },
    {
	XmNfileFilterStyle, XmCFileFilterStyle, XmRFileFilterStyle,
	sizeof(XtEnum), Offset(file_filter_style),
	XmRImmediate, (XtPointer)XmFILTER_NONE
    },
    {
	XmNdirTextLabelString, XmCDirTextLabelString, XmRXmString,
	sizeof(XmString), Offset(dir_text_label_string),
	XmRImmediate, (XtPointer)XmUNSPECIFIED
    },
};

/* Synthetic resources */
static void FSBGetDirListItemCount(Widget wid, int resource_offset, XtArgVal *value);
static void FSBGetDirListItems(Widget wid, int resource_offset, XtArgVal *value);
static void FSBGetDirListLabelString(Widget wid, int resource_offset, XtArgVal *value);
static void FSBGetDirMask(Widget wid, int resource_offset, XtArgVal *value);
static void FSBGetDirectory(Widget wid, int resource_offset, XtArgVal *value);
static void FSBGetFilterLabelString(Widget wid, int resource_offset, XtArgVal *value);
static void FSBGetListItemCount(Widget wid, int resource_offset, XtArgVal *value);
static void FSBGetListItems(Widget wid, int resource_offset, XtArgVal *value);
static void FSBGetNoMatchString(Widget wid, int resource_offset, XtArgVal *value);
static void FSBGetPattern(Widget wid, int resource_offset, XtArgVal *value);


static XmSyntheticResource syn_resources[] =
{
    {
	XmNdirectory,
	sizeof(XmString), Offset(directory),
	FSBGetDirectory, NULL
    },
    {
	XmNdirListLabelString,
	sizeof(XmString), Offset(dir_list_label_string),
	FSBGetDirListLabelString, NULL
    },
    {
	XmNdirListItems,
	sizeof(XmStringTable), Offset(dir_list_items),
	FSBGetDirListItems, NULL
    },
    {
	XmNdirListItemCount,
	sizeof(int), Offset(dir_list_item_count),
	FSBGetDirListItemCount, NULL
    },
    {
	XmNfilterLabelString,
	sizeof(XmString), Offset(filter_label_string),
	FSBGetFilterLabelString, NULL
    },
    {
	XmNdirMask,
	sizeof(XmString), Offset(dir_mask),
	FSBGetDirMask, NULL
    },
    {
	XmNdirSpec,
	sizeof(XmString), SBOffset(text_string),
	_XmExportXmString, NULL
    },
    {
	XmNfileListLabelString,
	sizeof(XmString), SBOffset(list_label_string),
	_XmExportXmString, NULL
    },
    {
	XmNfileListItems,
	sizeof(XmStringTable), SBOffset(list_items),
	FSBGetListItems, NULL
    },
    {
	XmNfileListItemCount,
	sizeof(XmStringTable), SBOffset(list_item_count),
	FSBGetListItemCount, NULL
    },
    {
	XmNnoMatchString,
	sizeof(XmString), Offset(no_match_string),
	FSBGetNoMatchString, NULL
    },
    {
	XmNpattern,
	sizeof(XmString), Offset(pattern),
	FSBGetPattern, NULL
    }
};

static void FileSelectionBoxUpOrDown(Widget, XEvent *, String *, Cardinal *);
static void FileSelectionBoxRestore(Widget, XEvent *, String *, Cardinal *);

static XtActionsRec actions[] =
{
	{"UpOrDown", FileSelectionBoxUpOrDown},
	{"SelectionBoxUpOrDown", FileSelectionBoxUpOrDown},
	{"SelectionBoxRestore", FileSelectionBoxRestore},
};

#if 0
static XmBaseClassExtRec _XmFileSBCoreClassExtRec = {
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

XmFileSelectionBoxClassRec xmFileSelectionBoxClassRec = {
    /* Core class part */
    {
	/* superclass            */ (WidgetClass) &xmSelectionBoxClassRec,
        /* class_name            */ "XmFileSelectionBox",
	/* widget_size           */ sizeof(XmFileSelectionBoxRec),
	/* class_initialize      */ class_initialize,
	/* class_part_initialize */ class_part_initialize,
	/* class_inited          */ False,
	/* initialize            */ initialize,
	/* initialize_hook       */ NULL,
	/* realize               */ XtInheritRealize,
	/* actions               */ actions /*NULL*/,
	/* num_actions           */ XtNumber(actions),
	/* resources             */ resources,
	/* num_resources         */ XtNumber(resources),
	/* xrm_class             */ NULLQUARK,
	/* compress_motion       */ True,
	/* compress_exposure     */ XtExposeCompressMaximal,
	/* compress_enterleave   */ False /*True*/,
	/* visible_interest      */ False,
	/* destroy               */ destroy,
	/* resize                */ XtInheritResize,
	/* expose                */ XtInheritExpose,
	/* set_values            */ set_values,
	/* set_values_hook       */ NULL,
	/* set_values_almost     */ XtInheritSetValuesAlmost,
	/* get_values_hook       */ NULL /*get_values_hook*/, /* Motif has one */
	/* accept_focus          */ NULL,
	/* version               */ XtVersion,
	/* callback offsets      */ NULL,
	/* tm_table              */ XtInheritTranslations,
	/* query_geometry        */ XtInheritQueryGeometry,
	/* display_accelerator   */ NULL,
	/* extension             */ (XtPointer)NULL /*&_XmFileSBCoreClassExtRec*/
    },
    /* Composite class part */
    {
	/* geometry manager */ XtInheritGeometryManager, /* Motif has one */
        /* change_managed   */ XtInheritChangeManaged,  /* Motif has one */
        /* insert_child     */ XtInheritInsertChild,
        /* delete_child     */ XtInheritDeleteChild, /* Motif has one */
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
	/* always_install_accelerators  */ True /*False*/,
	/* geo_matrix_create            */ geoMatrixCreate,
	/* focus_moved_proc             */ focus_moved_proc,
	/* extension                    */ NULL,
    },
    /* XmSelectionBox part */
    {
	/* Motif has something called a list_callback here */
	/* list_callback                */ NULL,
	/* extension                    */ NULL,
    },
    /* XmFileSelectionBox part */
    {
	/* extension                    */ NULL,
    }
};


WidgetClass xmFileSelectionBoxWidgetClass = (WidgetClass)&xmFileSelectionBoxClassRec;

#define DEFAULT_NUM_VIS_ITEMS	XmFSB_MAX_WIDGETS_VERT	/* FIX ME -- can someone confirm this? */
#define DEFAULT_LIST_WIDTH	100

static void
class_initialize(void)
{
    if (text_accelerators_parsed == NULL)
    {
	text_accelerators_parsed = 
	    XtParseAcceleratorTable(_XmSelectioB_defaultTextAccelerators);
    }
    
#if 0
    _XmFileSBCoreClassExtRec.record_type = XmQmotif;
#endif
}


static void
class_part_initialize(WidgetClass widget_class)
{
    _XmFastSubclassInit(widget_class, XmFILE_SELECTION_BOX_BIT);
}


static void
initialize(Widget request, Widget new_w,
	   ArgList args, Cardinal *num_args)
{
    Arg argl[7];
    int argc;
    XmString files = NULL;
    XtTranslations save_acc;
    int dir_set_flag;

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

    SB_DialogType(new_w) = XmDIALOG_FILE_SELECTION;
    FS_StateFlags(new_w) = 0;
    
    XtManageChild(SB_ApplyButton(new_w));
    XtManageChild(SB_HelpButton(new_w));

    if (SB_ListItemCount(new_w) == XmUNSPECIFIED)
    {
	SB_ListItemCount(new_w) = 0;
    }

    if (FS_DirListItemCount(new_w) == XmUNSPECIFIED)
    {
	FS_DirListItemCount(new_w) = 0;
    }

    /*
     * dir list label
     */
    if (FS_DirListLabelString(new_w) == (XmString)XmUNSPECIFIED)
    {
	FS_DirListLabelString(new_w) =
	    _XmOSGetLocalizedString(NULL,	/* FIX ME */
				    new_w,
				    XmNdirListLabelString,
				    (String)"Directories");
    }
    else if (FS_DirListLabelString(new_w) != NULL)
    {
	FS_DirListLabelString(new_w) =
		XmStringCopy(FS_DirListLabelString(new_w));
    }
    if (FS_Pattern(new_w) != NULL) {
	FS_Pattern(new_w) = XmStringCopy(FS_Pattern(new_w));
    }

    /* rws file list always has a scroll bar
     * file list
     */
    XtVaSetValues(SB_List(new_w),
		  XmNscrollBarDisplayPolicy, XmSTATIC,
		  XmNlistSizePolicy, XmCONSTANT,
		  NULL);

    /*
     * file list label
     */
    if (FS_FileListLabelString(new_w) == (XmString)XmUNSPECIFIED)
    {
	files = _XmOSGetLocalizedString(NULL,	/* FIX ME */
					new_w,
					XmNfileListLabelString,
					"Files");
    }
    else if (FS_FileListLabelString(new_w) != NULL)
    {
	files =	XmStringCopy(FS_FileListLabelString(new_w));
    }

    FS_FileListLabelString(new_w) = files;
    argc = 0;
    XtSetArg(argl[argc], XmNlabelString, FS_FileListLabelString(new_w));
    argc++;
    XtSetValues(SB_ListLabel(new_w), argl, argc);

    /*
     * directory label
     */
    if (FS_DirTextLabelString(new_w) == (XmString)XmUNSPECIFIED)
    {
	FS_DirTextLabelString(new_w) =
	    _XmOSGetLocalizedString(NULL,	/* FIX ME */
				    new_w,
				    XmNdirTextLabelString,
				    "Directory");
    }
    else
    {
	FS_DirTextLabelString(new_w) = XmStringCopy(FS_DirTextLabelString(new_w));
    }

    /*
     * filter label
     */
    if (FS_FilterLabelString(new_w) == (XmString)XmUNSPECIFIED)
    {
	FS_FilterLabelString(new_w) =
	    _XmOSGetLocalizedString(NULL,	/* FIX ME */
				    new_w,
				    XmNfilterLabelString,
				    "Filter");
    }
    else
    {
	FS_FilterLabelString(new_w) = XmStringCopy(FS_FilterLabelString(new_w));
    }

    /*
     * apply label
     */
    if (SB_ApplyLabelString(new_w) == (XmString)XmUNSPECIFIED)
    {
	SB_ApplyLabelString(new_w) =
	    _XmOSGetLocalizedString(NULL,	/* FIX ME */  /* leaks */
				    new_w,
				    XmNfilterLabelString,
				    "Filter");
    }
    else
    {
	SB_ApplyLabelString(new_w) = XmStringCopy(SB_ApplyLabelString(new_w));
    }
    XtVaSetValues(SB_ApplyButton(new_w),
    	XmNlabelString, SB_ApplyLabelString(new_w),
    	NULL);

    if (FS_PathMode(new_w) == XmPATH_MODE_RELATIVE) {
	FS_DirTextLabel(new_w) = _XmBB_CreateLabelG(new_w,
					       FS_DirTextLabelString(new_w),
					       "DirTextLabel");
	argc = 0;
	XtSetArg(argl[argc], XmNalignment, XmALIGNMENT_BEGINNING);
	argc++;
	XtSetValues(FS_DirTextLabel(new_w), argl, argc);
	XtManageChild(FS_DirTextLabel(new_w));
    }

    FS_FilterLabel(new_w) = _XmBB_CreateLabelG(new_w,
					       FS_FilterLabelString(new_w),
					       "FilterLabel");
    argc = 0;
    XtSetArg(argl[argc], XmNalignment, XmALIGNMENT_BEGINNING);
    argc++;
    XtSetValues(FS_FilterLabel(new_w), argl, argc);
    XtManageChild(FS_FilterLabel(new_w));

    FS_DirListLabel(new_w) = _XmBB_CreateLabelG(new_w,
						FS_DirListLabelString(new_w),
						"Dir");
    argc = 0;
    XtSetArg(argl[argc], XmNalignment, XmALIGNMENT_BEGINNING);
    argc++;
    XtSetValues(FS_DirListLabel(new_w), argl, argc);
    XtManageChild(FS_DirListLabel(new_w));

    if (FS_PathMode(new_w) == XmPATH_MODE_RELATIVE) {
	/*
	 * directory text
	 */
	argc = 0;
	FS_DirText(new_w) = XmCreateTextField(new_w, "DirText", argl, argc);
	save_acc = CoreAccelerators(new_w);
	if (SB_TextAccelerators(new_w) == NULL) {
	    CoreAccelerators(new_w) = text_accelerators_parsed;
	} else {
	    CoreAccelerators(new_w) = SB_TextAccelerators(new_w);
	}
	XtInstallAccelerators(FS_DirText(new_w), (Widget) new_w);
	CoreAccelerators(new_w) = save_acc;

	XtManageChild(FS_DirText(new_w));
     }

    /*
     * filter text
     */
    argc = 0;
    FS_FilterText(new_w) = XmCreateTextField(new_w, "FilterText", argl, argc);
    save_acc = CoreAccelerators(new_w);
    if (SB_TextAccelerators(new_w) == NULL)
    {
	CoreAccelerators(new_w) = text_accelerators_parsed;
    }
    else
    {
	CoreAccelerators(new_w) = SB_TextAccelerators(new_w);
    }
    XtInstallAccelerators(FS_FilterText(new_w), (Widget) new_w);
    CoreAccelerators(new_w) = save_acc;

    XtManageChild(FS_FilterText(new_w));

    /*
     * dir list
     */
    argc = 0;
    XtSetArg(argl[argc], XmNvisibleItemCount, SB_ListVisibleItemCount(new_w));
    argc++;
    XtSetArg(argl[argc], XmNselectionPolicy, XmBROWSE_SELECT);
    argc++;
    XtSetArg(argl[argc], XmNscrollBarDisplayPolicy, XmSTATIC);
    argc++;
    XtSetArg(argl[argc], XmNlistSizePolicy, XmCONSTANT);
    argc++;

    FS_DirList(new_w) = XmCreateScrolledList(new_w, "DirList", argl, argc);

    XtManageChild(FS_DirList(new_w));
    XtAddCallback(FS_DirList(new_w), XmNbrowseSelectionCallback, _XmFsbButton, NULL);
    XtAddCallback(FS_DirList(new_w), XmNdefaultActionCallback, _XmFsbButton, NULL);

    /* rws
     *  Override callbacks same as the list
     *  get rid of BBautoUnmanage
     */
    XtRemoveAllCallbacks(FS_FilterText(new_w), XmNactivateCallback);
    XtAddCallback(FS_FilterText(new_w), XmNactivateCallback, _XmFsbButton, NULL);

    /*
     * Override callback on child widgets - need our own version of _XmSbButton -> _XmFsbButton
     */
    if (SB_List(new_w))
    {
	/*
	XtRemoveAllCallbacks(SB_List(new_w), XmNdefaultActionCallback);
	XtAddCallback(SB_List(new_w), XmNdefaultActionCallback, _XmFsbButton, NULL);
	*/

	/*
	 * We also need to override the selection callback for the
	 * file list since an "empty" file list (which has a single fake
	 * entry of " [ .. ] ") cannot be selected from.
	 */
	XtRemoveAllCallbacks(SB_List(new_w), XmNbrowseSelectionCallback);
	XtAddCallback(SB_List(new_w), XmNbrowseSelectionCallback, _XmFsbFileSelect, (XtPointer)new_w);
    }
    if (SB_OkButton(new_w))
    {
	XtRemoveAllCallbacks(SB_OkButton(new_w), XmNactivateCallback);
	XtAddCallback(SB_OkButton(new_w), XmNactivateCallback, _XmFsbButton, NULL);
    }
    if (SB_ApplyButton(new_w))
    {
	XtRemoveAllCallbacks(SB_ApplyButton(new_w), XmNactivateCallback);
	XtAddCallback(SB_ApplyButton(new_w), XmNactivateCallback, _XmFsbButton, NULL);
    }
    if (BB_CancelButton(new_w))
    {
	XtRemoveAllCallbacks(BB_CancelButton(new_w), XmNactivateCallback);
	XtAddCallback(BB_CancelButton(new_w), XmNactivateCallback, _XmFsbButton, NULL);
    }
    if (SB_HelpButton(new_w))
    {
	XtRemoveAllCallbacks(SB_HelpButton(new_w), XmNactivateCallback);
	XtAddCallback(SB_HelpButton(new_w), XmNactivateCallback, _XmFsbButton, NULL);
    }

    if (FS_NoMatchString(new_w) == (XmString)XmUNSPECIFIED)
    {
	FS_NoMatchString(new_w) = XmStringCreateLocalized(" [ .. ] ");
    }
    else if (FS_NoMatchString(new_w) != NULL)
    {
	FS_NoMatchString(new_w) = XmStringCopy(FS_NoMatchString(new_w));
    }

    /*
     * If the XmNdirMask has been set but neither the XmNdirectory nor the
     * XmNpattern has been set then we'll bust up the XmNdirMask to
     * initialize the XmNdirectory and XmNpattern.  Motif appears to do it
     * this way (see mgv 2.4.3 and previous, I was setting XmNdirMask to
     * "*.*ps*" rather than setting XmNpattern but Motif figured out what
     * I meant anyway).  If either XmNpattern or XmNdirectory has been set
     * then we'll leave well enough alone.  The _XmFileSelectionSearch (at
     * least with defaultQualifySearchDataProc) will figure everything out
     * after this.
     *
     * When dirMask is set but directory or pattern isn't, Motif
     * extracts the unset information from the dirMask but leaves
     * the set information alone.
     *
     * See filesb/test11 if you want to fiddle with this.
     *
     *		--mu@trends.net, 1998.07.04
     */
    dir_set_flag = 0;
    if (_XmStringIsSpecified(FS_DirMask(new_w)))
    {
	String mask;

        if (XmStringGetLtoR(FS_DirMask(new_w), XmFONTLIST_DEFAULT_TAG, &mask))
        {
            String pat = _XmOSFindPatternPart(mask);
	    if (!_XmStringIsSpecified(FS_Pattern(new_w))) {
		FS_Pattern(new_w) = XmStringCreateLocalized(pat);
	    }

            if (pat > mask && !_XmStringIsSpecified(FS_Directory(new_w)))
	    {
        	/*
        	 * Remember that _XmOSFindPattern will give us something
        	 * that points into its argument.  Also remember that
        	 * XmNdirectory should contain the trailing directory
        	 * separator.
        	 */
        	*pat = '\0';
        	FS_Directory(new_w) = XmStringCreateLocalized(mask);
                dir_set_flag = 1;
            }

            XtFree(mask);
        }

        FS_DirMask(new_w) = XmStringCopy(FS_DirMask(new_w));
    }

    if (dir_set_flag == 0) { /* FS_Directory was not set above */
        if (_XmStringIsSpecified(FS_Directory(new_w))) {
            FS_Directory(new_w) = XmStringCopy(FS_Directory(new_w));
        }
        else
            FS_Directory(new_w) = NULL;
    }

    _XmFileSelectionSearch(new_w);
    XmStringFree(FS_DirSpec(new_w));
    FS_DirSpec(new_w) = XmStringCopy(FS_Directory(new_w));
}


static void
destroy(Widget w)
{
    DEBUGOUT(_LtDebug("RWS", w,"%s:destroy(%d)\n",
    	__FILE__, __LINE__
    	));

    XmStringFree(FS_DirListLabelString(w));
    XmStringFree(FS_FileListLabelString(w));
    XmStringFree(FS_FilterLabelString(w));
    XmStringFree(FS_NoMatchString(w));
	/* T. Straumann: don't forget to free these */
    XmStringFree(FS_DirMask(w));
    XmStringFree(FS_Pattern(w));
    XmStringFree(FS_Directory(w));
}


static Boolean
set_values(Widget old, Widget request, Widget new_w,
	   ArgList args, Cardinal *num_args)
{
    Arg al[6];
    int ac;
    Boolean refresh = False;
    Boolean reSearch;

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

    /* this is required */
    BB_InSetValues(new_w) = True;

    /* rws 24 Jun 1998
       This needs splain'n
       Since the dirSpec and the textString share storage, if textString is
       specified as a resource it will get updated in the new_w structure
       while Xt is updating the resources for SelectionBox and then put
       back to its original value when Xt updates the resources for
       FileSelectionBox. Therefore if dirSpec is not given we must buz the
       argList to see if textString was given and use that value for the
       dirSpec.

       All this could have been avoided by Motif _not_ saying that dirSpec is a
       synonym for textString!

       If someone can find a better way, I will listen.
     */
    if (FS_DirSpec(old) == FS_DirSpec(new_w))
    {
    Cardinal i;

    	for (i=0; i<*num_args; i++)
    	{
    		if (strcmp(args[i].name, XmNtextString) == 0)
    		{
    			if (FS_DirSpec(new_w))
    			{
			    XmStringFree(FS_DirSpec(new_w));
    			}
    			FS_DirSpec(new_w) = XmStringCopy((XmString)(args[i].value));
    		}
    	}
    }

    /* dir list */
    if (FS_DirListItems(new_w) != FS_DirListItems(old) ||
	FS_DirListItemCount(new_w) != FS_DirListItemCount(old))
    {
	/* FIX ME: the items should be copied */
	ac = 0;
	XtSetArg(al[ac], XmNitems, FS_DirListItems(new_w)); ac++;
	XtSetArg(al[ac], XmNitemCount, FS_DirListItemCount(new_w)); ac++;
	XtSetValues(FS_DirList(new_w), al, ac);

	refresh = True;
    }

    /* dir list label string */
    if (FS_DirListLabelString(new_w) != FS_DirListLabelString(old))
    {
	FS_DirListLabelString(new_w) =
		XmStringCopy(FS_DirListLabelString(new_w));
	XmStringFree(FS_DirListLabelString(old));

	ac = 0;
	XtSetArg(al[ac], XmNlabelString, FS_DirListLabelString(new_w)); ac++;
	XtSetValues(FS_DirListLabel(new_w),al,ac);

	refresh = True;
    }

	/* T. Straumann: this is a little weird:
	 *
	 * FS_FileLabelString() is actually SB_ListLabelString().
	 *
	 * NOTE: I don't like this; set_values on FS_FileLabelString
	 *	is done by the SB widget.
	 *	However we, i.e. FSB does the initialization and
	 *	the destruction.
	 *	IMHO this is quite confusing and inelegant.
	 */

    /* label string */
    if (FS_FilterLabelString(new_w) != FS_FilterLabelString(old))
    {
	ac = 0;
	FS_FilterLabelString(new_w) = XmStringCopy(FS_FilterLabelString(new_w));
	XmStringFree(FS_FilterLabelString(old));

	XtSetArg(al[ac], XmNlabelString, FS_FilterLabelString(new_w)); ac++;
	XtSetValues(FS_FilterLabel(new_w), al, ac);

	refresh = True;
    }

    if (FS_DirSpec(new_w) != FS_DirSpec(old))
    {
	String value;

		if (XmStringGetLtoR(FS_DirSpec(new_w), XmFONTLIST_DEFAULT_TAG, &value))
		{
		XtVaSetValues(SB_Text(new_w), XmNvalue, value, NULL);
		XtFree(value);
		refresh = True;
		}

    }

	/* T. Straumann: if any new string is supplied, we must 
	 *				 free the old one and make a copy of the user
	 *				 supplied value.
	 *				 If the strings are equal, we MUST move the
	 *				 old copy to the new_w and let the user's
	 *				 copy alone!!!
	 */
	reSearch = False;
    if ( !XmStringCompare(FS_Directory(new_w), FS_Directory(old)))
	{
		XmStringFree(FS_Directory(old));
		FS_Directory(new_w) = XmStringCopy(FS_Directory(new_w));
		reSearch = True;
	}
	else
	{
		FS_Directory(new_w) = FS_Directory(old);
	}

	if (!XmStringCompare(FS_DirMask(new_w), FS_DirMask(old))) {
		XmStringFree(FS_DirMask(old));
		FS_DirMask(new_w) = XmStringCopy(FS_DirMask(new_w));
#if 1
	/* This hopefully fixes bug #470233. */
		XmStringFree(FS_Pattern(old));
		FS_Pattern(new_w) = XmStringCopy(FS_DirMask(new_w));

#endif
		reSearch = True;
	} else {
		FS_DirMask(new_w) = FS_DirMask(old);
	}

	if (!XmStringCompare(FS_Pattern(new_w), FS_Pattern(old))) {
		XmStringFree(FS_Pattern(old));
		FS_Pattern(new_w) = XmStringCopy(FS_Pattern(new_w));
		reSearch = True;
	} else {
		FS_Pattern(new_w) = FS_Pattern(old);
	}

	if (!XmStringCompare(FS_NoMatchString(new_w), FS_NoMatchString(old))) {
		XmStringFree(FS_NoMatchString(old));
		FS_NoMatchString(new_w) = XmStringCopy(FS_NoMatchString(new_w));
		reSearch = True;
	} else {
		FS_NoMatchString(new_w) = FS_NoMatchString(old);
	}

	if (FS_FileSearchProc(new_w) != FS_FileSearchProc(old))
		reSearch = True;

	if (FS_DirSearchProc(new_w) != FS_DirSearchProc(old))
		reSearch = True;

	if ( reSearch || (FS_FileTypeMask(new_w) != FS_FileTypeMask(old))) {
		_XmFileSelectionSearch(new_w);
	}

	/* the next 8 lines are required for every BB subclass that uses
	 * the GeoMatrix */
	BB_InSetValues(new_w) = False;

	if (refresh && (XtClass(new_w) == xmFileSelectionBoxWidgetClass)) {
		_XmBulletinBoardSizeUpdate(new_w);
		return False;
	}

	return refresh;
}

#if 0
/* Motif has one of these */
static void get_values_hook(Widget w, ArgList args, Cardinal *num_args)
{
    DEBUGOUT(_LtDebug(__FILE__, w,
		      "%s:get_values_hook() - %i args\n",
		      __FILE__,
		      *num_args));
    DEBUGOUT(_LtDebugPrintArgList(__FILE__, w, args, *num_args, False));

}
#endif


static Dimension
GetPreferredListWidth(Widget w)
{
    int i;
    String Item;
    String tmp;
    XmString Xmtmp;
    Dimension tmpWidth;
    Dimension Width = 20;
    XmFontList fl;
    int NumItems;
    XmStringTable Items;
    Dimension spacing = 0;
    Dimension sbwidth = 0;
    Dimension sbmargin = 0;
    Dimension listmargin = 0;
    Dimension highlight = 0;
    Widget sb;

    XtVaGetValues(w,
		  XmNhighlightThickness, &highlight,
		  XmNlistMarginWidth, &listmargin,
		  XmNitemCount, &NumItems,
		  XmNitems, &Items,	/* This is _not_ a copy. Do not free */
		  XmNfontList, &fl,
		  NULL);

    for (i = 0; i < NumItems; i++)
    {
	if ( ! XmStringGetLtoR(Items[i], XmFONTLIST_DEFAULT_TAG, &Item) )
	{
	continue;
	}

	if ((tmp = strrchr(Item, '/')) != NULL)
	{
	    tmp++;
	}
	else
	{
	    tmp = Item;
	}

	Xmtmp = XmStringCreateLtoR(tmp, XmFONTLIST_DEFAULT_TAG);

	tmpWidth = XmStringWidth(fl, Xmtmp);

	Width = Width < tmpWidth ? tmpWidth : Width;

	XmStringFree(Xmtmp);

	XtFree(Item);
    }

    Width += 6;			/* Can someone explain this? What am I missing */
    Width += (listmargin * 2);
    Width += (highlight * 2);

    XtVaGetValues(XtParent(w),
		  XmNspacing, &spacing,
		  XmNscrolledWindowMarginWidth, &sbmargin,
		  XmNverticalScrollBar, &sb,
		  NULL);

    Width += (sbmargin * 2);

    if (XtIsManaged(sb))
    {
	XtVaGetValues(sb,
		      XmNwidth, &sbwidth,
		      NULL);
	Width += sbwidth + spacing;
    }

    return (Width);
}


/*
 * XmGeoSegmentFixUpProc
 */
static void
_XmTextFix(XmGeoMatrix geoSpec,
	   int action,
	   XmGeoMajorLayout layoutPtr,
	   XmKidGeometry rowPtr)
{

    if (!XmIsTextField(rowPtr->kid))
    {
	return;
    }

    switch (action)
    {
    case XmGET_PREFERRED_SIZE:
	rowPtr->box.width = 0;
	break;

    case XmGEO_POST_SET:
	XmTextFieldSetInsertionPosition(rowPtr->kid,
					XmTextFieldGetLastPosition(rowPtr->kid));
	break;

    default:
	break;
    }
    DEBUGOUT(_LtDebug2(__FILE__, XtParent(rowPtr->kid), rowPtr->kid, "_XmTextFix() - %s %dx%d%+d%+d\n",
    		_LtDebugGeoAction2String(action),
    		rowPtr->box.width,
    		rowPtr->box.height,
    		rowPtr->box.x,
    		rowPtr->box.y ));
}


/*
 * XmGeoSegmentFixUpProc
 */
static void
_XmDirListFix(XmGeoMatrix geoSpec,
	      int action,
	      XmGeoMajorLayout layoutPtr,
	      XmKidGeometry rowPtr)
{
    DEBUGOUT(_LtDebug2(__FILE__, XtParent(rowPtr->kid), rowPtr->kid, "_XmDirListFix() - %s\n",
    		_LtDebugGeoAction2String(action)));

    switch (action)
    {
    case XmGET_ACTUAL_SIZE:
	break;

    case XmGET_PREFERRED_SIZE:
	while (rowPtr->kid)
	{
	    if (XmIsScrolledWindow(rowPtr->kid))
	    {
		rowPtr->box.width =
		    GetPreferredListWidth(SW_WorkWindow(rowPtr->kid));
		rowPtr->box.height = XtHeight(rowPtr->kid);
		/* rws 18 Mar 1998
		   Do not let the list be any narrower than its
		   label
		 */
		if ((&rowPtr[-3])->kid == FS_DirListLabel(geoSpec->composite))
		{
		if ((&rowPtr[-3])->box.width > rowPtr->box.width)
		{
		    rowPtr->box.width = (&rowPtr[-3])->box.width;
		}
		}
    DEBUGOUT(_LtDebug2(__FILE__, XtParent(rowPtr->kid), rowPtr->kid, "_XmDirListFix() - %s %dx%d%+d%+d\n",
    		_LtDebugGeoAction2String(action),
    		rowPtr->box.width,
    		rowPtr->box.height,
    		rowPtr->box.x,
    		rowPtr->box.y ));
	    }
	    rowPtr++;
	}
	break;

    case XmGEO_POST_SET:
	/*
	 * fix up the Horizontal scroll bars in here
	 */
	while (rowPtr->kid)
	{
	    if (XmIsScrolledWindow(rowPtr->kid))
	    {
		Widget hsb;

		hsb = (Widget)SW_HSB(rowPtr->kid);
		if (hsb && XtIsManaged(hsb))
		{
		    XmListSetHorizPos(SW_WorkWindow(rowPtr->kid),
				      SCB_Maximum(hsb) - SCB_SliderSize(hsb) -
			    GetPreferredListWidth(SW_WorkWindow(rowPtr->kid)) +
				      rowPtr->box.width);
		}
    DEBUGOUT(_LtDebug2(__FILE__, XtParent(rowPtr->kid), rowPtr->kid, "_XmDirListFix() - %s %dx%d%+d%+d\n",
    		_LtDebugGeoAction2String(action),
    		rowPtr->box.width,
    		rowPtr->box.height,
    		rowPtr->box.x,
    		rowPtr->box.y ));
	    }
	    rowPtr++;
	}
	break;

    case XmGEO_PRE_SET:
	{
	    XmKidGeometry fileList = &rowPtr[1];
	    XmKidGeometry fileListLabel = &rowPtr[-2];
	    XmKidGeometry dirList = &rowPtr[0];
	    XmKidGeometry dirListLabel = &rowPtr[-3];
	    Dimension DirPrefWidth;
	    Dimension FilePrefWidth;

	    if (dirList->kid)
	    {
		DirPrefWidth = GetPreferredListWidth(FS_DirList(XtParent(dirList->kid)));
	    }
	    else
	    {
	    	DirPrefWidth = 0;
	    }
	    /*
	     * Position the file list
	     * It is the width that it wants to be
	     */
	    if (fileList->kid)
	    {
		FilePrefWidth = GetPreferredListWidth(SB_List(XtParent(fileList->kid)));
		fileList->box.width = FilePrefWidth;
		fileList->box.x = geoSpec->width
		    - fileList->box.width
		    - geoSpec->margin_w
		    - 2 * fileList->box.border_width - 1;
	    }
	    else
	    {
	    	FilePrefWidth = 0;
		fileList->box.width = FilePrefWidth;
		fileList->box.x = geoSpec->width - 1;
	    }

	    /*
	     * Position the file list label
	     */
	     if (fileListLabel->kid == SB_ListLabel(geoSpec->composite))
	     {
		if (fileList->kid)
		{
		    fileListLabel->box.x = fileList->box.x;
		    fileListLabel->box.width = fileList->box.width;
		}
		else
		{
		    fileListLabel->box.x = geoSpec->width
			- XtWidth(fileListLabel->kid)
			- geoSpec->margin_w
			- 2 * fileList->box.border_width;
		    fileListLabel->box.width = XtWidth(fileListLabel->kid);
		}
	     }
	    /*
	     * Position the dir list
	     * It takes up the rest of the space in this row
	     */
	    dirList->box.width = fileList->box.x
		- geoSpec->margin_w
		- layoutPtr->row.space_between
		- 2 * dirList->box.border_width;
    DEBUGOUT(_LtDebug2(__FILE__, XtParent(rowPtr->kid), rowPtr->kid, "_XmDirListFix() - %s %dx%d%+d%+d\n",
    		_LtDebugGeoAction2String(action),
    		rowPtr->box.width,
    		rowPtr->box.height,
    		rowPtr->box.x,
    		rowPtr->box.y ));
    DEBUGOUT(_LtDebug2(__FILE__, XtParent(rowPtr->kid), fileList->kid, "_XmDirListFix() - %s %dx%d%+d%+d\n",
    		_LtDebugGeoAction2String(action),
    		fileList->box.width,
    		fileList->box.height,
    		fileList->box.x,
    		fileList->box.y ));
	    /*
	     * Position the dir list label
	     */
	     if (dirListLabel->kid == FS_DirListLabel(geoSpec->composite))
	     {
		if (fileList->kid)
		{
		    dirListLabel->box.width = dirList->box.width;
		}
		else
		{
		    dirListLabel->box.width = XtWidth(dirListLabel->kid);
		}
	     }
	}
	break;

    default:
	break;
    }
}


static XmGeoMatrix
geoMatrixCreate(Widget _w, Widget _from, XtWidgetGeometry *_pref)
{
    XmGeoMatrix geoSpec;
    register XmGeoRowLayout layoutPtr;
    register XmKidGeometry boxPtr;
    Cardinal numKids, i, nextras;
    Boolean newRow;
    int nrows;
    Widget *extras = NULL;

    numKids = MGR_NumChildren(_w);

    nextras = 0;
    extras = NULL;
    for (i = 0; i < numKids; i++)
    {
	if (MGR_Children(_w)[i] != SB_ListLabel(_w) &&
	    (SB_List(_w)
	     ? MGR_Children(_w)[i] != XtParent(SB_List(_w))
	     : True) &&
	    MGR_Children(_w)[i] != SB_SelectionLabel(_w) &&
	    MGR_Children(_w)[i] != SB_Text(_w) &&
	    MGR_Children(_w)[i] != SB_Separator(_w) &&
	    MGR_Children(_w)[i] != SB_OkButton(_w) &&
	    MGR_Children(_w)[i] != SB_ApplyButton(_w) &&
	    MGR_Children(_w)[i] != SB_HelpButton(_w) &&
	    MGR_Children(_w)[i] != BB_CancelButton(_w) &&
	    MGR_Children(_w)[i] != FS_FilterLabel(_w) &&
	    MGR_Children(_w)[i] != FS_FilterText(_w) &&
	    MGR_Children(_w)[i] != FS_DirListLabel(_w) &&
	    MGR_Children(_w)[i] != FS_DirText(_w) &&
	    MGR_Children(_w)[i] != FS_DirTextLabel(_w) &&
	    (FS_DirList(_w)
	     ? MGR_Children(_w)[i] != XtParent(FS_DirList(_w))
	     : True))
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
	if (MGR_Children(_w)[i] != SB_ListLabel(_w) &&
	    (SB_List(_w)
	     ? MGR_Children(_w)[i] != XtParent(SB_List(_w))
	     : True) &&
	    MGR_Children(_w)[i] != SB_SelectionLabel(_w) &&
	    MGR_Children(_w)[i] != SB_Text(_w) &&
	    MGR_Children(_w)[i] != SB_Separator(_w) &&
	    MGR_Children(_w)[i] != SB_OkButton(_w) &&
	    MGR_Children(_w)[i] != SB_ApplyButton(_w) &&
	    MGR_Children(_w)[i] != SB_HelpButton(_w) &&
	    MGR_Children(_w)[i] != BB_CancelButton(_w) &&
	    MGR_Children(_w)[i] != FS_FilterLabel(_w) &&
	    MGR_Children(_w)[i] != FS_FilterText(_w) &&
	    MGR_Children(_w)[i] != FS_DirListLabel(_w) &&
	    MGR_Children(_w)[i] != FS_DirText(_w) &&
	    MGR_Children(_w)[i] != FS_DirTextLabel(_w) &&
	    (FS_DirList(_w)
	     ? MGR_Children(_w)[i] != XtParent(FS_DirList(_w))
	     : True))
	{
	    extras[nextras] = MGR_Children(_w)[i];
	    nextras++;
	}
    }

    nrows = 0;

    numKids = MGR_NumChildren(_w);

    /* note the starting from one.  The zero'th child is the "work area" */
    if (nextras > 0)
    {
	for (i = 1; i < nextras; i++)
	{
	    if (XmIsMenuBar(extras[i]) && XtIsManaged(extras[i]))
	    {
		nrows++;
	    }
	}
	if (extras[0] && XtIsManaged(extras[0]))
	{
	    nrows++;
	}
    }

    if (FS_PathMode(_w) == XmPATH_MODE_RELATIVE) {
	if (FS_DirTextLabel(_w) && XtIsManaged(FS_DirTextLabel(_w))) {
		nrows++;
	}
	if (FS_DirText(_w) && XtIsManaged(FS_DirText(_w))) {
		nrows++;
	}
    }
#if 0
/* Keep it simple for now - FIX ME */
     else
#endif
    {
	if (FS_FilterLabel(_w) && XtIsManaged(FS_FilterLabel(_w))) {
		nrows++;
	}
	if (FS_FilterText(_w) && XtIsManaged(FS_FilterText(_w))) {
		nrows++;
	}
    }

    if ((FS_DirListLabel(_w) && XtIsManaged(FS_DirListLabel(_w))) ||
	(SB_ListLabel(_w) && XtIsManaged(SB_ListLabel(_w))))
    {
	nrows++;
    }

    if ((FS_DirList(_w) && XtIsManaged(FS_DirList(_w))) ||
	(SB_List(_w) && XtIsManaged(XtParent(SB_List(_w)))))
    {
	nrows++;
    }

    if (SB_SelectionLabel(_w) && XtIsManaged(SB_SelectionLabel(_w)))
    {
	nrows++;
    }

    if (SB_Text(_w) && XtIsManaged(SB_Text(_w)))
    {
	nrows++;
    }

    if (SB_Separator(_w) && XtIsManaged(SB_Separator(_w)))
    {
	nrows++;
    }

    if ((BB_CancelButton(_w) && XtIsManaged(BB_CancelButton(_w))) ||
	(SB_OkButton(_w) && XtIsManaged(SB_OkButton(_w))) ||
	(SB_ApplyButton(_w) && XtIsManaged(SB_ApplyButton(_w))) ||
	(SB_HelpButton(_w) && XtIsManaged(SB_HelpButton(_w))))
    {
	nrows++;
    }
    else
    {
	for (i = 1; i < nextras; i++)
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
    geoSpec->no_geo_request = noGeoRequest;
    geoSpec->uniform_border = True;
    geoSpec->border = XtBorderWidth(_w);

    layoutPtr = &(geoSpec->layouts->row);
    boxPtr = geoSpec->boxes;

    for (i = 1; i < nextras; i++)
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

    if (SB_ChildPlacement(_w) == XmPLACE_TOP && nextras &&
	extras[0] && XtIsManaged(extras[0]) &&
	_XmGeoSetupKid(boxPtr, extras[0]))
    {
	layoutPtr->stretch_height = 1;
	layoutPtr->fill_mode = XmGEO_EXPAND;
	layoutPtr->even_width = 1;
	layoutPtr->even_height = 1;
	layoutPtr->space_above = BB_MarginHeight(_w);
	layoutPtr++;
	boxPtr += 2;
	nrows++;
    }

#if XmVERSION > 1
	/* Danny FIX ME FIXME
	 * For now, just move this above the other fields.
	 * We really need to move the filter text+label when
	 * DirText is present.
	 */
    newRow = False;
    if (FS_PathMode(_w) == XmPATH_MODE_RELATIVE) {
	if (FS_DirTextLabel(_w) && XtIsManaged(FS_DirTextLabel(_w)) &&
	    _XmGeoSetupKid(boxPtr, FS_DirTextLabel(_w)))
	{
	    layoutPtr->fill_mode = XmGEO_EXPAND;
	    layoutPtr->even_width = 0;
	    layoutPtr->even_height = 1;
	    layoutPtr->space_above = BB_MarginHeight(_w);
	    layoutPtr++;
	    boxPtr += 2;
	    newRow = True;
	}

	if (FS_DirText(_w) && XtIsManaged(FS_DirText(_w)) &&
	    _XmGeoSetupKid(boxPtr, FS_DirText(_w)))
	{
	    layoutPtr->fix_up = _XmTextFix;
	    layoutPtr->fill_mode = XmGEO_EXPAND;
	    layoutPtr->space_above = 0;	/* BB_MarginHeight(_w); */
	    layoutPtr++;
	    boxPtr += 2;
	    newRow = True;
	}
    }
/*    if (newRow)
    {
	layoutPtr++;
	boxPtr++;
    }
*/
#endif

    if (FS_FilterLabel(_w) && XtIsManaged(FS_FilterLabel(_w)) &&
	_XmGeoSetupKid(boxPtr, FS_FilterLabel(_w)))
    {
	layoutPtr->fill_mode = XmGEO_EXPAND;
	layoutPtr->even_width = 0;
	layoutPtr->even_height = 1;
	layoutPtr->space_above = BB_MarginHeight(_w);
	layoutPtr++;
	boxPtr += 2;
    }

    if (FS_FilterText(_w) && XtIsManaged(FS_FilterText(_w)) &&
	_XmGeoSetupKid(boxPtr, FS_FilterText(_w)))
    {
	layoutPtr->fix_up = _XmTextFix;
	layoutPtr->fill_mode = XmGEO_EXPAND;
	layoutPtr->space_above = 0;	/* BB_MarginHeight(_w); */
	layoutPtr++;
	boxPtr += 2;
    }

    newRow = False;
    if (FS_DirListLabel(_w) && XtIsManaged(FS_DirListLabel(_w)) &&
	_XmGeoSetupKid(boxPtr, FS_DirListLabel(_w)))
    {
	layoutPtr->fill_mode = XmGEO_EXPAND;
	layoutPtr->fit_mode = XmGEO_PROPORTIONAL;
	layoutPtr->even_width = 0;
	layoutPtr->even_height = 1;
	layoutPtr->space_above = BB_MarginHeight(_w);
	layoutPtr->space_between = BB_MarginWidth(_w);
	newRow = True;
	boxPtr++;
    }

    if (SB_ListLabel(_w) && XtIsManaged(SB_ListLabel(_w)) &&
	_XmGeoSetupKid(boxPtr, SB_ListLabel(_w)))
    {
	layoutPtr->fill_mode = XmGEO_EXPAND;
	layoutPtr->fit_mode = XmGEO_PROPORTIONAL;
	layoutPtr->even_width = 0;
	layoutPtr->even_height = 1;
	layoutPtr->space_above = BB_MarginHeight(_w);
	layoutPtr->space_between = BB_MarginWidth(_w);
	newRow = True;
	boxPtr++;
    }

    if (newRow)
    {
	layoutPtr++;
	boxPtr++;
    }

    newRow = False;
    if (FS_DirList(_w) && XtIsManaged(FS_DirList(_w)) &&
	_XmGeoSetupKid(boxPtr, XtParent(FS_DirList(_w))))
    {
	layoutPtr->fix_up = _XmDirListFix;
	layoutPtr->stretch_height = 1;
	layoutPtr->fill_mode = XmGEO_PACK;
	layoutPtr->fit_mode = XmGEO_PROPORTIONAL;
	layoutPtr->even_width = (Dimension)False;
	layoutPtr->even_height = 1;
	layoutPtr->sticky_end = True;
	layoutPtr->space_above =
	    (FS_DirListLabel(_w) && XtIsManaged(FS_DirListLabel(_w)))
	    ? 0 : BB_MarginHeight(_w) ;
	layoutPtr->min_height = 40;
	layoutPtr->space_between = BB_MarginWidth(_w);
	newRow = True;
	boxPtr++;
    }

    if (SB_List(_w) && XtIsManaged(XtParent(SB_List(_w))) &&
	_XmGeoSetupKid(boxPtr, XtParent(SB_List(_w))))
    {
	layoutPtr->fix_up = _XmDirListFix;
	layoutPtr->stretch_height = 1;
	layoutPtr->fill_mode = XmGEO_PACK;
	layoutPtr->fit_mode = XmGEO_PROPORTIONAL;
	layoutPtr->even_width = (Dimension)False;
	layoutPtr->even_height = 1;
	layoutPtr->sticky_end = True;
	layoutPtr->space_above =
	    (SB_ListLabel(_w) && XtIsManaged(SB_ListLabel(_w)))
	    ? 0 : BB_MarginHeight(_w) ;
	layoutPtr->min_height = 40;
	layoutPtr->space_between = BB_MarginWidth(_w);
	newRow = True;
	boxPtr++;
    }

    if (newRow)
    {
	layoutPtr++;
	boxPtr++;
    }

    if (SB_ChildPlacement(_w) == XmPLACE_ABOVE_SELECTION && nextras &&
	extras[0] && XtIsManaged(extras[0]) &&
	_XmGeoSetupKid(boxPtr, extras[0]))
    {
	layoutPtr->stretch_height = 1;
	layoutPtr->fill_mode = XmGEO_EXPAND;
	layoutPtr->even_width = 1;
	layoutPtr->even_height = 1;
	layoutPtr->space_above = BB_MarginHeight(_w);
	layoutPtr++;
	boxPtr += 2;
	nrows++;
    }

    if (SB_SelectionLabel(_w) && XtIsManaged(SB_SelectionLabel(_w)) &&
	_XmGeoSetupKid(boxPtr, SB_SelectionLabel(_w)))
    {
	layoutPtr->fill_mode = XmGEO_EXPAND;
	layoutPtr->even_width = 0;
	layoutPtr->even_height = 1;
	layoutPtr->space_above = BB_MarginHeight(_w);
	layoutPtr++;
	boxPtr += 2;
    }

    if (SB_Text(_w) && XtIsManaged(SB_Text(_w)) &&
	_XmGeoSetupKid(boxPtr, SB_Text(_w)))
    {
	layoutPtr->fix_up = _XmTextFix;
	layoutPtr->fill_mode = XmGEO_EXPAND;
	layoutPtr->even_height = 1;
	layoutPtr->even_width = 0;
	layoutPtr->space_above = 0;	/* BB_MarginHeight(_w); */
	layoutPtr++;
	boxPtr += 2;
    }

    if (SB_ChildPlacement(_w) == XmPLACE_BELOW_SELECTION && nextras &&
	extras[0] && XtIsManaged(extras[0]) &&
	_XmGeoSetupKid(boxPtr, extras[0]))
    {
	layoutPtr->stretch_height = 1;
	layoutPtr->fill_mode = XmGEO_EXPAND;
	layoutPtr->even_width = 1;
	layoutPtr->even_height = 1;
	layoutPtr->space_above = BB_MarginHeight(_w);
	layoutPtr++;
	boxPtr += 2;
	nrows++;
    }

    if (SB_Separator(_w) && XtIsManaged(SB_Separator(_w)) &&
	_XmGeoSetupKid(boxPtr, SB_Separator(_w)))
    {
	layoutPtr->fix_up = _XmSeparatorFix;
	layoutPtr->space_above = BB_MarginHeight(_w);
	boxPtr += 2;
	layoutPtr++;
    }

    newRow = False;
    if (SB_OkButton(_w) && XtIsManaged(SB_OkButton(_w)) &&
	_XmGeoSetupKid(boxPtr++, SB_OkButton(_w)))
    {
	layoutPtr->fill_mode = XmGEO_CENTER;
	layoutPtr->fit_mode = XmGEO_WRAP;
	if (LabG_RecomputeSize(SB_OkButton(_w)))
	{
	    layoutPtr->even_width = 6;
	    layoutPtr->even_height = 6;
	}
	else
	{
	    layoutPtr->even_width = 1;
	    layoutPtr->even_height = 1;
	}
	layoutPtr->space_above = BB_MarginHeight(_w);
	newRow = True;
    }

    for (i = 1; i < nextras; i++)
    {
	if (extras[i] && XtIsManaged(extras[i]) &&
	    (XmIsPushButton(extras[i]) || XmIsPushButtonGadget(extras[i])) &&
	    _XmGeoSetupKid(boxPtr++, extras[i]))
	{
	    _XmBulletinBoardSetDefaultShadow(extras[i]);
	    layoutPtr->fill_mode = XmGEO_CENTER;
	    layoutPtr->fit_mode = XmGEO_WRAP;
	    if ((XmIsPushButton(extras[i]) && Lab_RecomputeSize(extras[i])) || 
	        (XmIsPushButtonGadget(extras[i]) && LabG_RecomputeSize(extras[i])))
	    {
		layoutPtr->even_width = 6;
		layoutPtr->even_height = 6;
	    }
	    else
	    {
		layoutPtr->even_width = 1;
		layoutPtr->even_height = 1;
	    }
	    layoutPtr->space_above = BB_MarginHeight(_w);
	    newRow = True;
	}
    }

    if (SB_ApplyButton(_w) && XtIsManaged(SB_ApplyButton(_w)) &&
	_XmGeoSetupKid(boxPtr++, SB_ApplyButton(_w)))
    {
	layoutPtr->fill_mode = XmGEO_CENTER;
	layoutPtr->fit_mode = XmGEO_WRAP;
	if (LabG_RecomputeSize(SB_ApplyButton(_w)))
	{
	    layoutPtr->even_width = 6;
	    layoutPtr->even_height = 6;
	}
	else
	{
	    layoutPtr->even_width = 1;
	    layoutPtr->even_height = 1;
	}
	layoutPtr->space_above = BB_MarginHeight(_w);
	newRow = True;
    }

    if (BB_CancelButton(_w) && XtIsManaged(BB_CancelButton(_w)) &&
	_XmGeoSetupKid(boxPtr++, BB_CancelButton(_w)))
    {
	layoutPtr->fill_mode = XmGEO_CENTER;
	layoutPtr->fit_mode = XmGEO_WRAP;
	if (LabG_RecomputeSize(SB_CancelButton(_w)))
	{
	    layoutPtr->even_width = 6;
	    layoutPtr->even_height = 6;
	}
	else
	{
	    layoutPtr->even_width = 1;
	    layoutPtr->even_height = 1;
	}
	layoutPtr->space_above = BB_MarginHeight(_w);
	newRow = True;
    }

    if (SB_HelpButton(_w) && XtIsManaged(SB_HelpButton(_w)) &&
	_XmGeoSetupKid(boxPtr++, SB_HelpButton(_w)))
    {
	layoutPtr->fill_mode = XmGEO_CENTER;
	layoutPtr->fit_mode = XmGEO_WRAP;
	if (LabG_RecomputeSize(SB_HelpButton(_w)))
	{
	    /* rws 8 Jul 2000
	       filesb/test6
	     */
	    if (SB_List(_w) && XtIsManaged(XtParent(SB_List(_w))))
	    {
		layoutPtr->even_width = 6;
		layoutPtr->even_height = 6;
	    }
	    else
	    {
		layoutPtr->even_width = 4;
		layoutPtr->even_height = 4;
	    }
	}
	else
	{
	    layoutPtr->even_width = 1;
	    layoutPtr->even_height = 1;
	}
	/* rws 8 Jul 2000
	   filesb/test15 when pressing Filter
	 */
	if (SB_OkButton(_w) && !LabG_RecomputeSize(SB_OkButton(_w)))
	{
	    layoutPtr->even_width = 1;
	    if (XtIsRealized(_w))
	    {
		layoutPtr->even_height = 1;
	    }
	    else
	    {
		layoutPtr->even_height = 4;
	    }
	}
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


static void 
focus_moved_proc(Widget wid, XtPointer client_data, XtPointer data)
{
    XmFocusMovedCallback cbs = (XmFocusMovedCallback)data;
    Widget bb = (Widget)client_data;

    DEBUGOUT(_LtDebug2(__FILE__, wid, bb, "%s:focus_moved_proc() - %s -> %s\n",
    	__FILE__,
    	cbs->old_focus ? XtName(cbs->old_focus) : "NULL",
    	cbs->new_focus ? XtName(cbs->new_focus) : "NULL"));
    /* rws 21 Feb 1998
       I suspect that when the focus leaves the FilterText an update should
       be done. Can anyone verify this??
     */
    if (cbs->new_focus && cbs->new_focus == FS_FilterText(bb))
    {
	BB_DefaultButton(bb) = SB_ApplyButton(bb);
    }
    else if (cbs->new_focus && cbs->new_focus == FS_DirList(bb))
    {
	BB_DefaultButton(bb) = SB_ApplyButton(bb);
    }
    else
    {
	BB_DefaultButton(bb) = SB_OkButton(bb);
    }
    _XmBulletinBoardFocusMoved(wid, client_data, data);
}


static Boolean
noGeoRequest(XmGeoMatrix _geoSpec)
{
	/*
	return True;
	*/
    DEBUGOUT(_LtDebug("FSB", _geoSpec->composite, "%s:noGeoRequest(%d) - instigator %s\n",
    	__FILE__, __LINE__,
    	_geoSpec->instigator ? XtName(_geoSpec->instigator) : "NULL"));

    if (BB_InSetValues(_geoSpec->composite) &&
	(XtClass(_geoSpec->composite) == xmFileSelectionBoxWidgetClass))
    {
	return True;
    }
    if (_geoSpec->instigator && FS_DirList(_geoSpec->composite) && 
        (XtParent(FS_DirList(_geoSpec->composite)) == _geoSpec->instigator))
    {
	return True;
    }
    if (_geoSpec->instigator && SB_List(_geoSpec->composite) && 
        (XtParent(SB_List(_geoSpec->composite)) == _geoSpec->instigator))
    {
	return True;
    }

    return False;
}

/******************************* SEARCHING **********************************/


/* Keep it short and simple - make it a function (instead of a macro)
   for easier debugging purposes but give it a more than reasonable chance
   to be optimized out (or inlined in :-)  ... */
/* Microsoft's compiler won't allow static and inline to be combined */
static Boolean
FileIsHidden(String name)
{
	const char *ptr;

	ptr=basename(name);
	if (*ptr=='.') {
		ptr++;
		if ( (*ptr=='\0') || ((*ptr=='.')&&(*(ptr+1)=='\0')) ) {
			/* keep special entries visible */
			DEBUGOUT(_LtDebug(__FILE__, NULL, "FileIsHidden(%s) -> dot-False\n", name));
			return False;
		} else {
			DEBUGOUT(_LtDebug(__FILE__, NULL, "FileIsHidden(%s) -> True\n", name));
			return True;
		}
	} else {
		DEBUGOUT(_LtDebug(__FILE__, NULL, "FileIsHidden(%s) -> False\n", name));
		return False;
	}
}

static void
defaultDirSearchProc(Widget widget,
		     XtPointer data)
{
    String *entries, dir, pat;
    XmFileSelectionBoxCallbackStruct *cbs = (XmFileSelectionBoxCallbackStruct *)data;
    unsigned int numents, numalloc, argc, i, max;
    Arg argl[3];
    XmString *table;

    DEBUGOUT(_LtDebug(__FILE__, widget, "defaultDirSearchProc\n"));

    if (!XmStringGetLtoR(cbs->dir, XmFONTLIST_DEFAULT_TAG, &dir))
    {
	dir = XtNewString("");
    }

    entries  = NULL;
    numents  = 0;
    numalloc = 0;

    /*
     * I think pattern should always be "*" for directories.
     * It's what happens if you run testXm/filesb/test1 in its directory.
     * At startup the widget shows the "." ".." and "CVS" directories.
     * After appending "*.c" to the filter, the directory list is NOT changed,
     * but the file list is.
     *
     * Danny 25/5/1996
     */
    pat = XtNewString("*");
    _XmOSGetDirEntries(dir, pat, XmFILE_DIRECTORY, False, True,
		       &entries, &numents, &numalloc);
    XtFree(dir);
    XtFree(pat);

    max = 64;
    table = (XmString *)XtCalloc(max, sizeof(XmString));
   
    /* we use two versions of this loop for better performance */
    if (FS_FileFilterStyle(widget)==XmFILTER_HIDDEN_FILES) {

        int visible=0;
    
        for (i = 0; i < numents; i++)
         {
	 if (FileIsHidden(entries[i])) {
	    XtFree(entries[i]);
	 }
	 else {
	   if (i == max)
	    {
	    max *= 2;
	    table = (XmString *)XtRealloc((char *)table, max * sizeof(XmString));
	    }
	    table[visible] = XmStringCreateLtoR(entries[i], XmFONTLIST_DEFAULT_TAG);
	    visible++;
	    XtFree(entries[i]);
         } /* not hidden */
        } /* for */
	numents = visible;
    }  /* XmFILTER_HIDDEN_FILES */
    else
    for (i = 0; i < numents; i++)
    {
	if (i == max)
	{
	    max *= 2;
	    table = (XmString *)XtRealloc((char *)table, max * sizeof(XmString));
	}
	table[i] = XmStringCreateLtoR(entries[i], XmFONTLIST_DEFAULT_TAG);
	XtFree(entries[i]);
    }

    if (entries)
    {
	XtFree((char *)entries);
    }

    argc = 0;
    XtSetArg(argl[argc], XmNitems, table); argc++;
    XtSetArg(argl[argc], XmNitemCount, numents); argc++;
    XtSetValues(FS_DirList(widget), argl, argc); 
    for (i = 0; i < numents; i++)
    {
    	XmStringFree(table[i]);
    }
    XtFree((String)table);

    FS_DirectoryValid(widget) = True;
    FS_ListUpdated(widget) = True;
}


static void
defaultFileSearchProc(Widget widget, XtPointer data)
{
    String *entries, dir, pat;
    XmFileSelectionBoxCallbackStruct *cbs =
	(XmFileSelectionBoxCallbackStruct *)data;
    unsigned int numents, numalloc, argc, i, max;
    Arg argl[3];
    XmString *table;

    DEBUGOUT(_LtDebug(__FILE__, widget, "defaultFileSearchProc\n"));

    if (!XmStringGetLtoR(cbs->dir, XmFONTLIST_DEFAULT_TAG, &dir))
    {
	dir = XtNewString("");
	DEBUGOUT(_LtDebug(__FILE__, widget,
			  "defaultFileSearchProc: empty directory\n"));
    }
    else
    {
	DEBUGOUT(_LtDebug(__FILE__, widget,
			  "defaultFileSearchProc: directory '%s'\n", dir));
    }

    if (!XmStringGetLtoR(cbs->pattern, XmFONTLIST_DEFAULT_TAG, &pat))
    {
	pat = XtNewString("*");
	DEBUGOUT(_LtDebug(__FILE__, widget,
			  "defaultFileSearchProc: default pattern '*'\n"));
    }
    else
    {
	DEBUGOUT(_LtDebug(__FILE__, widget,
			  "defaultFileSearchProc: pattern '%s'\n", pat));
    }

    entries  = NULL;
    numents  = 0;
    numalloc = 0;

    _XmOSBuildFileList(dir, pat, FS_FileTypeMask(widget),  /* FIX ME: leaks */
		       &entries, &numents, &numalloc);
    XtFree(dir);
    XtFree(pat);

    if (numents > 0)
	qsort((void *)entries, numents, sizeof(String), _XmOSFileCompare);

    max = 64;
    table = (XmString *)XtCalloc(max, sizeof(XmString));

    /* we use two versions of this loop for better performance */
    if (FS_FileFilterStyle(widget)==XmFILTER_HIDDEN_FILES) {

        int visible=0;

        for (i = 0; i < numents; i++)
        {
	 if (FileIsHidden(entries[i])) {
	    XtFree(entries[i]);
	 } else {
   	   if (i == max)
 	    {
	     max *= 2;
	     table = (XmString *)XtRealloc((char *)table,
					  max * sizeof(XmString));
 	    }
	    table[visible] = XmStringCreateLtoR(entries[i], XmFONTLIST_DEFAULT_TAG);
	    visible++;
	    XtFree(entries[i]);
         } /* not hidden */
	 } /* for */
    numents = visible;
    } /* XmFILTER_HIDDEN_FILES */
    else
     for (i = 0; i < numents; i++)
        {
   	 if (i == max)
 	{
	     max *= 2;
	     table = (XmString *)XtRealloc((char *)table,
					  max * sizeof(XmString));
 	  }
	  table[i] = XmStringCreateLtoR(entries[i], XmFONTLIST_DEFAULT_TAG);
	  XtFree(entries[i]);
        }


    if (entries)
    {
	XtFree((char *)entries);
    }

    argc = 0;
    XtSetArg(argl[argc], XmNfileListItems, table); argc++;
    XtSetArg(argl[argc], XmNfileListItemCount, numents); argc++;
    XtSetValues((widget), argl, argc); 

    for (i = 0; i < numents; i++)
    {
	XmStringFree(table[i]);
    }
    XtFree((String)table);

    FS_ListUpdated(widget) = True;
}


static void
defaultQualifySearchDataProc(Widget w,
			     XtPointer input_data,
			     XtPointer output_data)
{
/* rws 7 Apr 1998
   use filesb/test9 if you are going to screw with this!!! If you do not
   know how to use it don't screw with it.
 */
    XmFileSelectionBoxCallbackStruct *in =
	(XmFileSelectionBoxCallbackStruct *)input_data;
    XmFileSelectionBoxCallbackStruct *out =
	(XmFileSelectionBoxCallbackStruct *)output_data;
    String dir = NULL;
    String pat = NULL;
    String mask = NULL;
    String pdir, ppat;

    DEBUGOUT(_LtDebug(__FILE__, w, "defaultQualifySearchDataProc\n"));

    if (in->dir)
    {
	if (!XmStringGetLtoR(in->dir, XmFONTLIST_DEFAULT_TAG, &dir))
	{
	    dir = XtNewString("");
	}
    }
    else
    {
	if (in->mask)
	{
	String mask, pat;

	    if (XmStringGetLtoR(in->mask, XmFONTLIST_DEFAULT_TAG, &mask))
	    {
		pat = _XmOSFindPatternPart(mask);
		if (pat > mask)
		{
		    pat[-1] = '\0';
		    if (*mask)
		    {
			dir = XtNewString(mask);
		    }
		    else
		    {
			dir = XtNewString("/");
		    }
		}
		else
		{
		    dir = XtNewString("");
		}
		XtFree(mask);
	    }
	    else
	    {
		dir = XtNewString("");
	    }
	}
	else
	{
	    if (!XmStringGetLtoR(FS_Directory(w), XmFONTLIST_DEFAULT_TAG, &dir))
	    {
		dir = XtNewString("");
	    }
	}
    }
    if (in->pattern)
    {
	if (!XmStringGetLtoR(in->pattern, XmFONTLIST_DEFAULT_TAG, &pat))
	{
	    pat = XtNewString("*");
	}
    }
    else
    {
	if (in->mask)
	{
	String mask;

	    if (XmStringGetLtoR(in->mask, XmFONTLIST_DEFAULT_TAG, &mask))
	    {
	    String tmp;

		tmp = _XmOSFindPatternPart(mask);
		if (strlen(tmp) == 0)
		{
		    pat = XtNewString("*");
		}
		else
		{
		    pat = XtNewString(tmp);
		}
		XtFree(mask);
	    }
	    else
	    {
		pat = XtNewString("*");
	    }
	}
	else
	{
	    if (!XmStringGetLtoR(FS_Pattern(w), XmFONTLIST_DEFAULT_TAG, &pat))
	    {
		pat = XtNewString("*");
	    }
	}
    }
    _XmOSQualifyFileSpec(dir, pat, &pdir, &ppat);
    XtFree(dir);
    XtFree(pat);
    out->dir = XmStringCreateSimple(pdir);
    out->dir_length = XmStringLength(out->dir);
    out->pattern = XmStringCreateSimple(ppat);
    out->pattern_length = XmStringLength(out->pattern);

    mask = XtMalloc(strlen(pdir) + strlen(ppat) + 1);
    strcpy(mask, pdir);
    strcat(mask, ppat);
    XtFree(pdir);
    XtFree(ppat);
    out->mask = XmStringCreateSimple(mask);
    out->mask_length = XmStringLength(out->mask);
    XtFree(mask);

    if (in->value)
    {
    	out->value = XmStringCopy(in->value);
    	out->length = XmStringLength(out->value);
    }
    else if (_XmStringIsSpecified(FS_DirSpec(w)))
    {
    	out->value = XmStringCopy(FS_DirSpec(w));
    	out->length = XmStringLength(out->value);
    }
    else
    {
	out->value = NULL;
	out->length = 0;
    }

    out->reason = in->reason;
    out->event = in->event;
}

/*
 * search procedure.  Algorithm per
 * OSF/M*tif Programmer's Reference Release 1.2, 1-446
 */
static void
_XmFileSelectionSearch(Widget w)
{
    XmFileSelectionBoxCallbackStruct in, out;
    Arg argl[5];
    int argc;
    char *value;

    DEBUGOUT(_LtDebug(__FILE__, w, "_XmFileSelectionSearch()\n"));

    /*
     * setup the callback structures
     */
    in.reason = 0;
    in.event = NULL;
    in.value = NULL;
    in.length = 0;
    in.dir_length = 0;
    in.mask_length = 0;
    in.pattern_length = 0;

    in.mask = FS_DirMask(w);
    if (!_XmStringIsSpecified(in.mask))
    {
    	FS_DirMask(w) = XmStringCreateSimple("");
	in.mask = FS_DirMask(w);
    }
    in.mask_length = XmStringLength(in.mask);
    DEBUGOUT(_LtDebug(__FILE__, w, "\tdirMask %s (%i)\n", 
    		_LtDebugXmString2String(FS_DirMask(w)),in.mask_length));

    in.dir = FS_Directory(w);
    if (!_XmStringIsSpecified(in.dir))
    {
    	FS_Directory(w) = XmStringCreateSimple("");
	in.dir = FS_Directory(w);
    }
    in.dir_length = XmStringLength(in.dir);
    DEBUGOUT(_LtDebug(__FILE__, w, "\tdirectory %s (%i)\n", 
    		_LtDebugXmString2String(FS_Directory(w)),in.dir_length));

    in.pattern = FS_Pattern(w);
    if (!_XmStringIsSpecified(in.pattern))
    {
    	FS_Pattern(w) = XmStringCreateSimple("");
	in.pattern = FS_Pattern(w);
    }
    in.pattern_length = XmStringLength(in.pattern);
    DEBUGOUT(_LtDebug(__FILE__, w, "\tpattern %s (%i)\n", 
    		_LtDebugXmString2String(FS_Pattern(w)),in.pattern_length));

    /*
     * qualification
     */
    DEBUGOUT(_LtDebug(__FILE__, w, "_XmFileSelectionSearch() - Calling Qualify\n"));
    (*FS_QualifySearchDataProc(w)) (w, (XtPointer)&in, (XtPointer)&out);

#ifdef MOZILLA
    /* rws 21 Jun 1998
       Mozilla installs its own dir search proc.  The first thing it does is
       to get the XmNdirectory and compare this to the last time it was 
       called.  If it is the same, the search is not done.
     */
    DEBUGOUT(_LtDebug(__FILE__, w, "_XmFileSelectionSearch()\n\tdirectory %s\n",
		      _LtDebugXmString2String(FS_Directory(w))));
    DEBUGOUT(_LtDebug(__FILE__, w, "_XmFileSelectionSearch()\n\tin.dir %s\n",
		      _LtDebugXmString2String(in.dir)));
    DEBUGOUT(_LtDebug(__FILE__, w, "_XmFileSelectionSearch()\n\tout.dir %s\n",
		      _LtDebugXmString2String(out.dir)));
    if (FS_Directory(w))
    {
	XmStringFree(FS_Directory(w));
    }
    FS_Directory(w) = XmStringCreateSimple("");
#endif

    /*
     * dir search
     *
     * I'm guessing that the XmFS_DIR_SEARCH_PROC bit is a flag
     * to keep the directory list selection callback from causing
     * an infinite loop.
     *		-- mu@echo-on.net 1998.03.17
     */
    FS_DirectoryValid(w) = False;
    FS_ListUpdated(w) = False;
    if ((FS_StateFlags(w) & XmFS_DIR_SEARCH_PROC) == 0)
    {
	FS_StateFlags(w) |= XmFS_DIR_SEARCH_PROC;
	DEBUGOUT(_LtDebug(__FILE__, w, "_XmFileSelectionSearch() - Calling DirSearch\n"));
	(*FS_DirSearchProc(w)) (w, (XtPointer)&out);
	if (FS_ListUpdated(w))
	{
	    XmListSelectPos(FS_DirList(w), 1, False);	/* True; Danny */
	    XmListSetKbdItemPos(FS_DirList(w), 1);
	}
	FS_StateFlags(w) &= ~XmFS_DIR_SEARCH_PROC;
    }

    /*
     * file search
     */
    if (FS_DirectoryValid(w))
    {
	FS_ListUpdated(w) = False;
	if ((FS_StateFlags(w) & XmFS_IN_FILE_SEARCH) == 0)
	{
	    FS_StateFlags(w) |= XmFS_IN_FILE_SEARCH;
	    DEBUGOUT(_LtDebug(__FILE__, w, "_XmFileSelectionSearch() - Calling FileSearch\n"));
	    (*FS_FileSearchProc(w)) (w, (XtPointer)&out);
	    if (FS_ListUpdated(w) && SB_ListItemCount(w) == 0)
	    {
	    XmString *table;
	    unsigned int numents;

		/* rws 1 Mar 1997
		 * This does actually get into the list but, you have to left
		 * click on the list hsb before it appears?????
		 */
		FS_StateFlags(w) |= XmFS_NO_MATCH;
		table = (XmString *)XtCalloc(1, sizeof(XmString));
		table[0] = XmStringCopy(FS_NoMatchString(w));
		numents = 1;
		argc = 0;
		XtSetArg(argl[argc], XmNfileListItemCount, numents); argc++;
		XtSetArg(argl[argc], XmNfileListItems, table); argc++;
		XtSetValues((w), argl, argc);
		XmStringFree(table[0]);
		XtFree((String)table);
	    }
	    else
	    {
		FS_StateFlags(w) &= ~XmFS_NO_MATCH;
	    }
	    if (FS_ListUpdated(w))
	    {
		if (FS_DirSpec(w))
		{
		    XmStringFree(FS_DirSpec(w));
		}
		FS_DirSpec(w) = XmStringCopy(out.dir);
			if (XmStringGetLtoR(out.dir, XmFONTLIST_DEFAULT_TAG, &value))
			{

			argc = 0;
			XtSetArg(argl[argc], XmNvalue, value); argc++;
			XtSetValues(SB_Text(w), argl, argc);

			XtFree(value);
			}
	    }
	    FS_StateFlags(w) &= ~XmFS_IN_FILE_SEARCH;
	}
    }

    if (FS_DirMask(w))
    {
	XmStringFree(FS_DirMask(w));
    }
    FS_DirMask(w) = XmStringCopy(out.mask);

    if (XmStringGetLtoR(FS_DirMask(w), XmFONTLIST_DEFAULT_TAG, &value))
    {
	argc = 0;
	XtSetArg(argl[argc], XmNvalue, value); argc++;
	XtSetValues(FS_FilterText(w), argl, argc);
	XtFree(value);
    }

    if (FS_Directory(w))
    {
	XmStringFree(FS_Directory(w));
    }
    FS_Directory(w) = XmStringCopy(out.dir);
    if (FS_Pattern(w))
    {
	XmStringFree(FS_Pattern(w));
    }
    FS_Pattern(w) = XmStringCopy(out.pattern);

    if (out.value)
    {
    	XmStringFree(out.value);
    }
    if (out.dir)
    {
    	XmStringFree(out.dir);
    }
    if (out.mask)
    {
    	XmStringFree(out.mask);
    }
	/* T. Straumann: fixed memory leak */
	if (out.pattern)
	{
		XmStringFree(out.pattern);
	}
    if (XtClass(w) == xmFileSelectionBoxWidgetClass)
    {
	_XmBulletinBoardSizeUpdate(w);
    }
}

/************************** END WIDGET METHODS ****************************/

/************************** EXTERNAL FUNCTIONS ****************************/
Widget
XmCreateFileSelectionBox(Widget parent,
			 char *name,
			 Arg *argList,
			 Cardinal argcount)
{
    return XtCreateWidget(name,
			  xmFileSelectionBoxWidgetClass,
			  parent,
			  argList,
			  argcount);
}


Widget
XmCreateFileSelectionDialog(Widget parent,
			    char *name,
			    Arg *argList,
			    Cardinal argcount)
{
    Widget d, ret;
    char *s;
    Arg *al;
    Cardinal ac, i;

    s = _XmMakeDialogName(name);

    al = (Arg *)XtCalloc(argcount + 2, sizeof(Arg));

    ac = 0;
    XtSetArg(al[ac], XmNallowShellResize, True); ac++;
    for (i = 0; i < argcount; i++)
    {
	XtSetArg(al[ac], argList[i].name, argList[i].value); ac++;
    }

    /*
    d = XtCreateWidget(s, xmDialogShellWidgetClass, parent, al, ac);
    */
    d = XmCreateDialogShell(parent, s, al, ac);
    XtFree(s);

    XtSetArg(al[ac], XmNdialogType, XmDIALOG_FILE_SELECTION); ac++;
    ret = XtCreateWidget(name, xmFileSelectionBoxWidgetClass, d, al, ac);
    XtAddCallback(ret, XmNdestroyCallback,
		  _XmDestroyParentCallback,
		  (XtPointer)ret);
    XtFree((char *)al);

    return ret;
}

/* This routine is marked as obsolete as of Motif 2.1 */
Widget
XmFileSelectionBoxGetChild(Widget w,
			   unsigned char child)
{
    switch (child)
    {

    case XmDIALOG_APPLY_BUTTON:
	return SB_ApplyButton(w);

    case XmDIALOG_CANCEL_BUTTON:
	return SB_CancelButton(w);
    
    case XmDIALOG_DEFAULT_BUTTON:
	return BB_DefaultButton(w);

    case XmDIALOG_DIR_LIST:
	return FS_DirList(w);

    case XmDIALOG_DIR_LIST_LABEL:
	return FS_DirListLabel(w);

    case XmDIALOG_FILTER_LABEL:
	return FS_FilterLabel(w);

    case XmDIALOG_FILTER_TEXT:
	return FS_FilterText(w);

    case XmDIALOG_HELP_BUTTON:
	return SB_HelpButton(w);

    case XmDIALOG_LIST:
	return SB_List(w);

    case XmDIALOG_LIST_LABEL:
	return SB_ListLabel(w);

    case XmDIALOG_OK_BUTTON:
	return SB_OkButton(w);

    case XmDIALOG_SELECTION_LABEL:
	return SB_SelectionLabel(w);

    case XmDIALOG_SEPARATOR:
	return SB_Separator(w);

    case XmDIALOG_TEXT:
	return SB_Text(w);

    case XmDIALOG_WORK_AREA:
	return SB_WorkArea(w);

    /* Still have to treat these.
       amai: dumb question - why? Motif manpage doesn't show them!? */
#if 0
    case XmDIALOG_MESSAGE_LABEL:
    case XmDIALOG_DEFAULT_BUTTON:
    case XmDIALOG_SYMBOL_LABEL:
	return BB_DefaultButton(w);
#endif

    default:
      return NULL;
    }
}


void
XmFileSelectionDoSearch(Widget w, XmString dirmask)
{
     	XmFileSelectionBoxCallbackStruct in, out;
     	String mask;

    DEBUGOUT(_LtDebug(__FILE__, w, "XmFileSelectionDoSearch() - %s\n",
    		_LtDebugXmString2String(dirmask)));

	in.reason = 0;
	in.event = NULL;
	in.value = NULL;
	in.length = 0;
	in.dir = NULL;
	in.dir_length = 0;
	in.mask = NULL;
	in.mask_length = 0;
	in.pattern = NULL;
	in.pattern_length = 0;
	if (dirmask != (XmString)NULL)
	{
		in.mask = XmStringCopy(dirmask);
		in.mask_length = XmStringLength(in.mask);
	}
	else
	{
		if (XtIsManaged(FS_FilterText(w)))
		{
		    mask = XmTextFieldGetString(FS_FilterText(w));
		    in.mask = XmStringCreateSimple(mask);
		    in.mask_length = XmStringLength(in.mask);
		    XtFree(mask);
		}
		else
		{
		    mask = XmTextFieldGetString(SB_Text(w));
		    in.dir = XmStringCreateSimple(mask);
		    in.dir_length = XmStringLength(in.dir);
		    XtFree(mask);
		}
	}
	(*FS_QualifySearchDataProc(w)) ((Widget)w, (XtPointer)&in, (XtPointer)&out);
	if (in.dir)
	{
		XmStringFree(in.dir);
	}
	if (in.mask)
	{
		XmStringFree(in.mask);
	}
	XmStringFree(FS_DirMask(w));
	XmStringFree(FS_Pattern(w));
	XmStringFree(FS_Directory(w));
	FS_DirMask(w) = out.mask;
	FS_Pattern(w) = out.pattern;
	FS_Directory(w) = out.dir;
	if (out.value)
	{
		XmStringFree(out.value);
	}

	if (XmStringGetLtoR(out.mask, XmFONTLIST_DEFAULT_TAG, &mask))
	{
	    XmTextFieldSetString(FS_FilterText(w), mask);
	    XmTextFieldSetInsertionPosition(FS_FilterText(w),
		    XmTextFieldGetLastPosition(FS_FilterText(w)));
	    XtFree(mask);
	}
	_XmFileSelectionSearch((Widget)w);
}


/*
 * Stolen from SelectionBox.c
 *
 * We need a private routine for this here, because we need to pass the
 * callback structures specific to FileSelectionBox not SelectionBox.
 *
 * This routine is called for any button child of selectionbox for the
 * XmNactivateCallback. Make mapping to XmNokCallback etc.
 * Also called for double-click in list.
 *
 * Need to implement BulletinBoard's AutoUnmanage though.
 */
static void
_XmFsbButton(Widget w, XtPointer client, XtPointer call)
{
	XmFileSelectionBoxWidget sb = (XmFileSelectionBoxWidget)XtParent(w);
	XmAnyCallbackStruct *a = (XmAnyCallbackStruct *)call;
	XmFileSelectionBoxCallbackStruct cbs;

	if (XmIsList(w)) {	/* The lists are grandchildren... fix sb */
		sb = (XmFileSelectionBoxWidget)XtParent(sb);
	}

	cbs.event = a->event;
	cbs.value = (XmString)0;
	cbs.length = 0;

	/*
	* I think these should always be in initialised :
	*	dir, pattern, mask, and their _length companions
	* Make sure to free these at the end !
	*/
	cbs.mask = XmStringCopy(FS_DirMask(sb));
	cbs.mask_length = XmStringLength(cbs.mask);
	cbs.dir = XmStringCopy(FS_Directory(sb));
	cbs.dir_length = XmStringLength(cbs.dir);
	cbs.pattern = XmStringCopy(FS_Pattern(sb));
	cbs.pattern_length = XmStringLength(cbs.pattern);

	if (w == SB_OkButton(sb) /* || w == SB_List(sb) */) {
		if (w == SB_OkButton(sb)) {
			char *s = XmTextFieldGetString(SB_Text(sb));

			DEBUGOUT(_LtDebug(__FILE__, w, "_XmFsbButton OK '%s'\n", s));

			cbs.value = XmStringCreateSimple(s);

			XtFree(s);
		} else {
			/* List double-click */
			XmListCallbackStruct *ls = (XmListCallbackStruct *) call;

			if (FS_StateFlags(sb) & XmFS_NO_MATCH) {
				XmListDeselectAllItems(w);
				goto BailOut;
			}

			cbs.value = XmStringCopy(ls->item);

			DEBUGOUT(_LtDebug(__FILE__, w, "_XmFsbButton LIST '%s'\n",
				_LtDebugXmString2String(cbs.value)));
		}

		cbs.length = XmStringLength(cbs.value);

		if (SB_TextString(sb)) {
			XmStringFree(SB_TextString(sb));
		}

		SB_TextString(sb) = XmStringCopy(cbs.value);

		if (SB_MustMatch(sb)) {
			if (_XmSelectionBoxMatch((XmSelectionBoxWidget)sb)) {
				cbs.reason = XmCR_OK;
				XtCallCallbackList((Widget)sb, SB_OkCallback(sb), &cbs);
			} else {
				cbs.reason = XmCR_NO_MATCH;
				XtCallCallbackList((Widget)sb, SB_NoMatchCallback(sb), &cbs);
			}
		} else {
			cbs.reason = XmCR_OK;
			XtCallCallbackList((Widget)sb, SB_OkCallback(sb), &cbs);
		}
	} else if (w == BB_CancelButton(sb)) {
		cbs.reason = XmCR_CANCEL;

		DEBUGOUT(_LtDebug(__FILE__, w, "_XmFsbButton CANCEL\n"));

		XtCallCallbackList((Widget)sb, SB_CancelCallback(sb), &cbs);
	} else if (w == FS_FilterText(sb) || w == SB_ApplyButton(sb)) {
		XmFileSelectionBoxCallbackStruct in, out;
		String mask;

		DEBUGOUT(_LtDebug(__FILE__, w, "_XmFsbButton FILTER\n"));

		in.reason = a->reason;
		in.event = a->event;
		in.value = NULL;
		in.length = 0;
		in.dir = NULL;
		in.dir_length = 0;
		mask = XmTextFieldGetString(FS_FilterText(sb));
		in.mask = XmStringCreateSimple(mask);
		in.mask_length = XmStringLength(in.mask);
		XtFree(mask);
		in.pattern = NULL;
		in.pattern_length = 0;
		(*FS_QualifySearchDataProc(sb)) ((Widget)sb, (XtPointer)&in, (XtPointer)&out);
		XmStringFree(FS_DirMask(sb));
		XmStringFree(FS_Pattern(sb));
		XmStringFree(FS_Directory(sb));
		FS_DirMask(sb) = out.mask;
		FS_Pattern(sb) = out.pattern;
		FS_Directory(sb) = out.dir;

		if (XmStringGetLtoR(out.mask, XmFONTLIST_DEFAULT_TAG, &mask)) {
			XmTextFieldSetString(FS_FilterText(sb), mask);
			XmTextFieldSetInsertionPosition(FS_FilterText(sb),
			XmTextFieldGetLastPosition(FS_FilterText(sb)));
			XtFree(mask);
		}
		_XmFileSelectionSearch((Widget)sb);
		XmStringFree(in.mask);
		XmStringFree(out.value);
	} else if (w == SB_HelpButton(sb)) {
		cbs.reason = XmCR_HELP;
		DEBUGOUT(_LtDebug(__FILE__, w, "_XmFsbButton HELP\n"));
		XtCallCallbackList((Widget)sb, sb->manager.help_callback, &cbs);
	} else if (w == FS_DirList(sb)) {
		/* Somebody selected a directory */
		XmListCallbackStruct *ls = (XmListCallbackStruct *) call;
		XmFileSelectionBoxCallbackStruct in, out;
		String mask;

		DEBUGOUT(_LtDebug(__FILE__, w, "_XmFsbButton DirList\n"));

		in.reason = ls->reason;
		in.event = ls->event;
		in.value = NULL;
		in.length = 0;
		in.dir = ls->item;
		in.dir_length = XmStringLength(in.dir);
		mask = XmTextFieldGetString(FS_FilterText(sb));
		in.mask = XmStringCreateSimple(mask);
		in.mask_length = XmStringLength(in.mask);
		XtFree(mask);
		in.pattern = NULL;
		in.pattern_length = 0;
		(*FS_QualifySearchDataProc(sb)) ((Widget)sb, (XtPointer)&in, (XtPointer)&out);

		if (XmStringGetLtoR(out.mask, XmFONTLIST_DEFAULT_TAG, &mask)) {
			XmTextFieldSetString(FS_FilterText(sb), mask);
			/*
			XmTextFieldSetInsertionPosition(FS_FilterText(sb),
				XmTextFieldGetLastPosition(FS_FilterText(sb)));
			*/
			XtFree(mask);
		}
		if (ls->reason == XmCR_BROWSE_SELECT) {
			/*
			 * The results of the searchDataProc aren't going to get copied
			 * to the widget, so free them
			 */
			XmStringFree(out.value);	/* ?? */
			XmStringFree(out.mask);
			XmStringFree(out.pattern);
			XmStringFree(out.dir);
		} else {
#ifdef MOZILLA
			XmStringFree(FS_DirMask(sb));
			XmStringFree(FS_Pattern(sb));
			XmStringFree(FS_Directory(sb));
			FS_DirMask(sb) = out.mask;
			FS_Pattern(sb) = out.pattern;
			FS_Directory(sb) = out.dir;
			XmStringFree(out.value);	/* ?? */
#endif
			_XmFileSelectionSearch((Widget)sb);
		}
		XmTextFieldSetInsertionPosition(FS_FilterText(sb),
			XmTextFieldGetLastPosition(FS_FilterText(sb)));
		XmStringFree(in.mask);
	} else {
		DEBUGOUT(_LtDebug(__FILE__, w, "_XmFsbButton : untreated case !!\n"));
	}

	/* AutoUnmanage */
	if (BB_AutoUnmanage(sb)) {
		if (w == SB_OkButton(sb) || w == SB_List(sb) || w == BB_CancelButton(sb)) {
			Widget	shell = XtParent(sb);

			if (XtIsManaged((Widget)sb) &&
				XtIsSubclass(shell, xmDialogShellWidgetClass) &&
				!CoreBeingDestroyed(sb))
			{
				XtUnmanageChild((Widget)sb);
				DEBUGOUT(_LtDebug(__FILE__, (Widget)sb, "FSB AutoUnmanage\n"));

				XtCallCallbackList(shell, Shell_PopdownCallback(shell), NULL);
			}
		}
		/* Not in these cases :
		*	SB_HelpButton(sb)
		*	FS_FilterText(sb)
		*	SB_ApplyButton(sb)
		*	FS_DirList(sb)
		*/
	}

BailOut:
	/* Free what you've allocated */
	if (cbs.value != NULL)
		XmStringFree(cbs.value);
	XmStringFree(cbs.mask);
	XmStringFree(cbs.dir);
	XmStringFree(cbs.pattern);
}

/*
 * Stolen from SelectionBox.c
 *
 * We need a private routine for this here, because we need to keep the
 * artificial entry in empty file lists from being selected.
 */
static void
_XmFsbFileSelect(Widget w, XtPointer client, XtPointer call)
{
    XmFileSelectionBoxWidget fsb = (XmFileSelectionBoxWidget)client;
    char *s;
    XmListCallbackStruct *lp = (XmListCallbackStruct *) call;

    /*
     * Disallow the selection if there was no match.
     * This will happen if the glob pattern didn't match anything,
     * in this case, the file list will contain a single entry
     * which no one is supposed to be able to select.
     */
    if (FS_StateFlags(fsb) & XmFS_NO_MATCH)
    {
	DEBUGOUT(_LtDebug(__FILE__, w,
			  "_XmFsbFileSelect (Selection with XmFS_NO_MATCH)\n"));

        /*
	 * The lp->item_position should always be one so we'll just assume
	 * that it is.
	 */
        XmListDeselectPos(w, 1);
	return;
    }

    if (SB_Text(fsb) == NULL)
    {
	DEBUGOUT(_LtDebug(__FILE__, w,
			  "_XmFsbFileSelect (No TextField)\n"));
	return;
    }

    if (lp == NULL || lp->item == NULL)
    {
	DEBUGOUT(_LtDebug(__FILE__, w,
			  "_XmFsbFileSelect (NULL)\n"));
	return;
    }

    if (!XmStringGetLtoR(lp->item, XmFONTLIST_DEFAULT_TAG, &s))
    {
	DEBUGOUT(_LtDebug(__FILE__, w,
			  "_XmFsbFileSelect (Couldn't convert to string)\n"));
	return;
    }

    DEBUGOUT(_LtDebug(__FILE__, w,
		      "_XmFsbFileSelect '%s'\n", s));
    XmTextFieldSetString(SB_Text(fsb), s);
    XmTextFieldSetInsertionPosition(SB_Text(fsb),
				    XmTextFieldGetLastPosition(SB_Text(fsb)));
    XtFree(s);
    if (SB_TextString(fsb))
    {
    	XmStringFree(SB_TextString(fsb));
    }
    SB_TextString(fsb) = XmStringCopy(lp->item);
    if (FS_DirMask(fsb))
    {
    	XmStringFree(FS_DirMask(fsb));
    }
    FS_DirMask(fsb) = XmStringCopy(lp->item);
}


/*
 * I assume that the following can be used in synthetic resources :
 *      FSBGetDirListItemCount
 *      FSBGetDirListItems
 *      FSBGetDirListLabelString
 *      FSBGetDirMask
 *      FSBGetDirectory
 *      FSBGetFilterLabelString
 *      FSBGetListItemCount
 *      FSBGetListItems
 *      FSBGetNoMatchString
 *      FSBGetPattern
 *
 * Rules for the copy versus reallocate question have been taken from Kenton
 * Lee's article "Avoiding Motif Memory Leaks" in his column "Advanced
 * Application Development" in The X Advisor, March 1996.
 * The article can be located under http://landru.unx.com/DD/advisor/ .
 * Other articles by Ken are available at http://www.rahul.net/kenton/bib.html
 * or http://www.rahul.net/kenton/index.shtml .
 */
static void 
FSBGetDirListItemCount(Widget wid, int resource_offset, XtArgVal *value)
{
    /* rws 20 Jun 1998
       We are getting the ListItems directly out of the SelectionBox so....
       get the DirListItems directly from the List
     */
    *value = List_ItemCount(FS_DirList(wid));
}

static void 
FSBGetDirListItems(Widget wid, int resource_offset, XtArgVal *value)
{
    /* According to Ken this is copied in SetValues, but not in GetValues */
    /* rws 20 Jun 1998
       We are getting the ListItems directly out of the SelectionBox so....
       get the DirListItems directly from the List
     */
    *value = (XtArgVal)List_Items(FS_DirList(wid));
}

static void 
FSBGetDirListLabelString(Widget wid, int resource_offset, XtArgVal *value)
{
    /* According to Ken this is always a copy */
    *value = (XtArgVal)XmStringCopy(FS_DirListLabelString(wid));
}

static void 
FSBGetDirMask(Widget wid, int resource_offset, XtArgVal *value)
{
    /* According to Ken this is always a copy */
    *value = (XtArgVal)XmStringCopy(FS_DirMask(wid));
}

static void 
FSBGetDirectory(Widget wid, int resource_offset, XtArgVal *value)
{
    /* According to Ken this is always a copy */
    DEBUGOUT(_LtDebug(__FILE__, wid, "FSBGetDirectory()\n"));
    *value = (XtArgVal)XmStringCopy(FS_Directory(wid));
}

static void 
FSBGetFilterLabelString(Widget wid, int resource_offset, XtArgVal *value)
{
    /* According to Ken this is always a copy */
    *value = (XtArgVal)XmStringCopy(FS_FilterLabelString(wid));
}

static void 
FSBGetListItemCount(Widget wid, int resource_offset, XtArgVal *value)
{
    *value = (XtArgVal)SB_ListItemCount(wid);
}

static void 
FSBGetListItems(Widget wid, int resource_offset, XtArgVal *value)
{
    /* According to Ken this is copied in SetValues, but not in GetValues */
    /*
    *value = (XtArgVal)SB_ListItems(wid);
    */
    _XmSelectionBoxGetListItems(wid, resource_offset, value);
}

static void 
FSBGetNoMatchString(Widget wid, int resource_offset, XtArgVal *value)
{
    /* Return a copy of the internal string because the */
    /* application expects to be able to free the returned string. */
    *value = (XtArgVal)XmStringCopy(FS_NoMatchString(wid));
}

static void 
FSBGetPattern(Widget wid, int resource_offset, XtArgVal *value)
{
    /* Return a copy of the internal string because the */
    /* application expects to be able to free the returned string. */
    *value = (XtArgVal)XmStringCopy(FS_Pattern(wid));
}


static void
FileSelectionBoxUpOrDown(Widget w, XEvent *event,
                         String *params, Cardinal *num_params)
{
    Arg args[4];
    Widget text, list, focus;
    int type, top_item_position, visible_item_count, item_count;
    XmStringTable items;
    int *position_list, position_count, new_position, old_position;
    /* char *selected_text; */
    /* XmFileSelectionBoxCallbackStruct in, out; */
    DEBUGOUT(_LtDebug(__FILE__, w, "FileSelectionBoxUpOrDown %s\n",
		      params ? *params: "NULL"));

    if (*num_params != 1)
    {
	return;
    }

    focus = XmGetFocusWidget(w);
    if (focus == FS_FilterText(w))
    {
	text = FS_FilterText(w);
	list = FS_DirList(w);
    }
    else if (focus == SB_Text(w))
    {
	text = SB_Text(w);
	list = SB_List(w);
    }
    else
    {
	return;
    }

    if (list != NULL && text != NULL)
    {
	if (*num_params != 1)
	{
	    return;
	}
	type = atoi(*params);
	XtSetArg(args[0], XmNtopItemPosition, &top_item_position);
	XtSetArg(args[1], XmNvisibleItemCount, &visible_item_count);
	XtSetArg(args[2], XmNitemCount, &item_count);
	XtSetArg(args[3], XmNitems, &items);
	XtGetValues(list, args, 4);
	if (item_count == 0)
	{
	    return;
	}
	if (XmListGetSelectedPos(list, &position_list, &position_count))
	{
	    old_position = *position_list;
	    XtFree((char *) position_list);
	}
	else
	{
	    old_position = 0;
	}
	
	switch (type)
	{
	case 0:
	    /* move up */
	    if (old_position > 1)
	    {
		new_position = old_position - 1;
	    }
	    else
	    {
		new_position = 1;
	    }
	    break;
	case 1:
	    /* move down */
	    if (old_position < item_count)
	    {
		new_position = old_position + 1;
	    }
	    else
	    {
		new_position = item_count;
	    }
	    break;
	case 2:
	    /* move to first item */
	    new_position = 1;
	    break;
	case 3:
	    /* move to last item */
	    new_position = item_count;
	    break;
	default:
	    new_position = old_position;
	    break;
	}
	if (new_position != old_position)
	{
	    if (new_position < top_item_position)
	    {
		XmListSetPos(list, new_position);
	    }
	    else if (new_position >= top_item_position + visible_item_count)
	    {
		XmListSetBottomPos(list, new_position);
	    }
	    XmListSelectPos(list, new_position, True);
	}
    }
}


static void
FileSelectionBoxRestore(Widget w, XEvent *event,
                        String *params, Cardinal *num_params)
{
    Widget text, list, focus;
    /* XmStringTable items; */
    /* int item_count, *position_list, position_count; */
    char *selected_text;
    /* XmAnyCallbackStruct in; */

    DEBUGOUT(_LtDebug(__FILE__, w, "FileSelectionBoxRestore\n"));

    focus = XmGetFocusWidget(w);
    if (focus == FS_FilterText(w))
    {
	text = FS_FilterText(w);
	list = FS_DirList(w);
    }
    else if (focus == SB_Text(w))
    {
	_XmSelectionBoxRestore(w, event, params, num_params);
	return;
    }
    else
    {
	return;
    }

    if (text != NULL && list != NULL)
    {
	if (!XmStringGetLtoR(FS_DirMask(w),
			     XmFONTLIST_DEFAULT_TAG,
			     &selected_text))
	{
	    selected_text = XtMalloc(sizeof(char));
	    *selected_text = '\0';
	}

	XmTextFieldSetString(text, selected_text);
	XtFree(selected_text);
	XmTextFieldSetInsertionPosition(text,
					XmTextFieldGetLastPosition(text));
    }
}
