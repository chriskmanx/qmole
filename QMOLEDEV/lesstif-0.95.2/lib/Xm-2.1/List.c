/**
 *
 * $Header: /cvsroot/lesstif/lesstif/lib/Xm-2.1/List.c,v 1.4 2008/01/02 19:42:57 dannybackx Exp $
 *
 * Copyright (C) 1995 Free Software Foundation, Inc.
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

static const char rcsid[] = "$Header: /cvsroot/lesstif/lesstif/lib/Xm-2.1/List.c,v 1.4 2008/01/02 19:42:57 dannybackx Exp $";

#include <LTconfig.h>

#include <stdio.h>
#include <string.h>

#include <XmI/XmI.h>

#include <Xm/XmP.h>
#include <Xm/AtomMgr.h>
#include <Xm/DragC.h>
#include <Xm/ListP.h>
#include <Xm/ScrollBarP.h>
#include <Xm/ScrolledW.h>
#include <XmI/AtomMgrI.h>
#include <XmI/DragDropI.h>
#include <Xm/TransltnsP.h>

#include <XmI/DebugUtil.h>

/*
 *    Some #defines to select different patches within this file.
 *    We should try to avoid such things ...
 */


/*
 * JKF: comment this out when the XmStringDrawXXX functions
 * implement their clip mask arguments.
 */
#define	JONS_CLIP_MASK_KLUDGE

/*
 * JKF: someday I won't need this
 */
#define USE_SEPARATE_ITEMS_LIST

/*
 * amai: IXI Motif extensions. You might turn them on, but we have
         only stubs and even don't know their signatures ...
 */
#undef IXI_STUFF

/*
 * ?
 */
#define HSB_FIX

/*
 * lots of others ... :-(
 */


/* Forward Declarations */

static void class_initialize(void);
static void class_part_initialize(WidgetClass widget_class);
static void destroy(Widget w);
static void resize(Widget w);
static void expose(Widget w, XEvent *event, Region region);
static void initialize(Widget request, Widget new_w,
		       ArgList args, Cardinal *num_args);
static Boolean set_values(Widget current, Widget request, Widget new_w,
			  ArgList args, Cardinal *num_args);
static XtGeometryResult query_geometry(Widget w,
				       XtWidgetGeometry *proposed,
				       XtWidgetGeometry *answer);
static void list_border_highlight(Widget w);
static void list_border_unhighlight(Widget w);

static XtTranslations list_trans;

/* prototypes for drag-drop */
static Boolean drag_selected_proc(Widget w,
				  Atom *selection,
				  Atom *target,
				  Atom *type_return,
				  XtPointer *value_return,
				  unsigned long *length_return,
				  int *format_return);

static Boolean drag_unselected_proc(Widget w,
				    Atom *selection,
				    Atom *target,
				    Atom *type_return,
				    XtPointer *value_return,
				    unsigned long *length_return,
				    int *format_return);
static void drag_drop_finish(Widget w,
			     XtPointer client_data,
			     XtPointer call_data);

static void _XmListSetSBManageVert(Widget w, Boolean *manage_changed);
static void _XmListSetSBManageHoriz(Widget w, Boolean *manage_changed);
static void _XmListRedraw(Widget w, Boolean redraw_all_visible);
static void _XmListSetRenderTable(Widget, int, XrmValue *);

/*
 * Resources for the list class
 */
#define Offset(field) XtOffsetOf(XmListRec, list.field)

static XtResource resources[] =
{
    {
	XmNlistSpacing, XmCListSpacing, XmRVerticalDimension,
	sizeof(Dimension), Offset(ItemSpacing),
	XmRImmediate, (XtPointer)0
    },
    {
	XmNlistMarginWidth, XmCListMarginWidth, XmRHorizontalDimension,
	sizeof(Dimension), Offset(margin_width),
	XmRImmediate, (XtPointer)0
    },
    {
	XmNlistMarginHeight, XmCListMarginHeight, XmRVerticalDimension,
	sizeof(Dimension), Offset(margin_height),
	XmRImmediate, (XtPointer)0
    },
    {
	XmNstringDirection, XmCStringDirection, XmRStringDirection,
	sizeof(XmStringDirection), Offset(StrDir),
	XmRImmediate, (XtPointer)((XmStringDirection)XmUNSPECIFIED)
	/* this was STRING_L_TO_R */
    },
    {
	XmNitems, XmCItems, XmRXmStringTable,
	sizeof(XmStringTable), Offset(items),
	XmRStringTable, (XtPointer)NULL
    },
    {
	XmNitemCount, XmCItemCount, XmRInt,
	sizeof(int), Offset(itemCount),
	XmRImmediate, (XtPointer)0
    },
    {
	XmNselectedItems, XmCSelectedItems, XmRXmStringTable,
	sizeof(XmStringTable), Offset(selectedItems),
	XmRStringTable, (XtPointer)NULL
    },
    {
	XmNselectedItemCount, XmCSelectedItemCount, XmRInt,
	sizeof(int), Offset(selectedItemCount),
	XmRImmediate, (XtPointer)0
    },
    {
	XmNvisibleItemCount, XmCVisibleItemCount, XmRInt,
	sizeof(int), Offset(visibleItemCount),
	XmRImmediate, (XtPointer)0
    },
    {
	XmNtopItemPosition, XmCTopItemPosition, XmRTopItemPosition,
	sizeof(int), Offset(top_position),
	XmRImmediate, (XtPointer)1
    },
    {
	XmNselectionPolicy, XmCSelectionPolicy, XmRSelectionPolicy,
	sizeof(unsigned char), Offset(SelectionPolicy),
	XmRImmediate, (XtPointer)XmBROWSE_SELECT
    },
    {
	XmNlistSizePolicy, XmCListSizePolicy, XmRListSizePolicy,
	sizeof(unsigned char), Offset(SizePolicy),
	XmRImmediate, (XtPointer)XmVARIABLE
    },
    {
	XmNscrollBarDisplayPolicy, XmCScrollBarDisplayPolicy, XmRScrollBarDisplayPolicy,
	sizeof(unsigned char), Offset(ScrollBarDisplayPolicy),
	XmRImmediate, (XtPointer)XmAS_NEEDED
	/* This should be a CallProc */
    },
    {
	XmNautomaticSelection, XmCAutomaticSelection, XmRBoolean,
	sizeof(Boolean), Offset(AutoSelect),
	XmRImmediate, (XtPointer)False
    },
    {
	XmNdoubleClickInterval, XmCDoubleClickInterval, XmRInt,
	sizeof(int), Offset(ClickInterval),
	XmRImmediate, (XtPointer)XmUNSPECIFIED
	/* was XmRCallProc, (XtPointer)_XmDoubelClickIntervalDefault */
    },
    {
	XmNsingleSelectionCallback, XmCCallback, XmRCallback,
	sizeof(XtCallbackList), Offset(SingleCallback),
	XmRCallback, (XtPointer)NULL
    },
    {
	XmNmultipleSelectionCallback, XmCCallback, XmRCallback,
	sizeof(XtCallbackList), Offset(MultipleCallback),
	XmRCallback, (XtPointer)NULL
    },
    {
	XmNextendedSelectionCallback, XmCCallback, XmRCallback,
	sizeof(XtCallbackList), Offset(ExtendCallback),
	XmRCallback, (XtPointer)NULL
    },
    {
	XmNbrowseSelectionCallback, XmCCallback, XmRCallback,
	sizeof(XtCallbackList), Offset(BrowseCallback),
	XmRCallback, (XtPointer)NULL
    },
    {
	XmNdefaultActionCallback, XmCCallback, XmRCallback,
	sizeof(XtCallbackList), Offset(DefaultCallback),
	XmRCallback, (XtPointer)NULL
    },
    {
	XmNhorizontalScrollBar, XmCHorizontalScrollBar, XmRWidget,
	sizeof(Widget), Offset(hScrollBar),
	XmRImmediate, (XtPointer)NULL
    },
    {
	XmNverticalScrollBar, XmCVerticalScrollBar, XmRWidget,
	sizeof(Widget), Offset(vScrollBar),
	XmRImmediate, (XtPointer)NULL
    },
    {
	XmNnavigationType, XmCNavigationType, XmRNavigationType,
	sizeof(XmNavigationType), XtOffsetOf(XmListRec, primitive.navigation_type),
	XmRImmediate, (XtPointer)XmTAB_GROUP
    },
    {
	/*
	 * Note !!
	 * Order is important. This resource must be specified before the
	 * XmNfontList and XmNrenderTable, otherwise the check_set_render_table
	 * flag is not initialised before use in _XmListSetRenderTable().
	 */
	"keep.off", "Keep.off", XmRBoolean,
	sizeof(Boolean), Offset(check_set_render_table),
	XmRImmediate, (XtPointer)NULL
    },
    {
	XmNfontList, XmCFontList, XmRFontList,
	sizeof(XmFontList), Offset(font),
	XmRCallProc, (XtPointer)_XmListSetRenderTable
    },
    {
	XmNrenderTable, XmCRenderTable, XmRRenderTable,
	sizeof(XmFontList), Offset(font),
	XmRCallProc, (XtPointer)_XmListSetRenderTable
    },
};

static XmSyntheticResource syn_resources[] =
{
    {
	XmNlistSpacing,
	sizeof(Dimension), Offset(ItemSpacing),
	_XmFromVerticalPixels, _XmToVerticalPixels
    },
    {
	XmNlistMarginWidth,
	sizeof(Dimension), Offset(margin_width),
	_XmFromHorizontalPixels, _XmToHorizontalPixels
    },
    {
	XmNlistMarginHeight,
	sizeof(Dimension), Offset(margin_height),
	_XmFromVerticalPixels, _XmToVerticalPixels
    },
};


/* local prototypes */
static void ListAddMode(Widget, XEvent *, String *, Cardinal *);
static void ListBeginData(Widget, XEvent *, String *, Cardinal *);
static void ListBeginDataExtend(Widget, XEvent *, String *, Cardinal *);
static void ListBeginExtend(Widget, XEvent *, String *, Cardinal *);
static void ListBeginLine(Widget, XEvent *, String *, Cardinal *);
static void ListBeginSelect(Widget, XEvent *, String *, Cardinal *);
static void ListBeginToggle(Widget, XEvent *, String *, Cardinal *);
static void ListButtonMotion(Widget, XEvent *, String *, Cardinal *);
static void ListCopyToClipboard(Widget, XEvent *, String *, Cardinal *);
static void ListDefaultAction(Widget, XEvent *, String *, Cardinal *);
static void ListEndData(Widget, XEvent *, String *, Cardinal *);
static void ListEndDataExtend(Widget, XEvent *, String *, Cardinal *);
static void ListEndExtend(Widget, XEvent *, String *, Cardinal *);
static void ListEndLine(Widget, XEvent *, String *, Cardinal *);
static void ListEndSelect(Widget, XEvent *, String *, Cardinal *);
static void ListEndToggle(Widget, XEvent *, String *, Cardinal *);
static void ListEnter(Widget, XEvent *, String *, Cardinal *);
static void ListExtendNextItem(Widget, XEvent *, String *, Cardinal *);
static void ListExtendPrevItem(Widget, XEvent *, String *, Cardinal *);
static void ListFocusIn(Widget, XEvent *, String *, Cardinal *);
static void ListFocusOut(Widget, XEvent *, String *, Cardinal *);
static void ListKbdActivate(Widget, XEvent *, String *, Cardinal *);
static void ListKbdBeginExtend(Widget, XEvent *, String *, Cardinal *);
static void ListKbdBeginSelect(Widget, XEvent *, String *, Cardinal *);
static void ListKbdCancel(Widget, XEvent *, String *, Cardinal *);
static void ListKbdDeSelectAll(Widget, XEvent *, String *, Cardinal *);
static void ListKbdEndExtend(Widget, XEvent *, String *, Cardinal *);
static void ListKbdEndSelect(Widget, XEvent *, String *, Cardinal *);
static void ListKbdSelectAll(Widget, XEvent *, String *, Cardinal *);
static void ListLeftChar(Widget, XEvent *, String *, Cardinal *);
static void ListLeave(Widget, XEvent *, String *, Cardinal *);
static void ListLeftPage(Widget, XEvent *, String *, Cardinal *);
static void ListNextItem(Widget, XEvent *, String *, Cardinal *);
static void ListNextPage(Widget, XEvent *, String *, Cardinal *);
static void ListPrevItem(Widget, XEvent *, String *, Cardinal *);
static void ListPrevPage(Widget, XEvent *, String *, Cardinal *);
static void ListProcessDrag(Widget, XEvent *, String *, Cardinal *);
static void ListRightChar(Widget, XEvent *, String *, Cardinal *);
static void ListRightPage(Widget, XEvent *, String *, Cardinal *);


static XtActionsRec actions[] =
{
    {"ListButtonMotion", ListButtonMotion},


    {"ListBeginExtend", ListBeginExtend},
    {"ListEndExtend", ListEndExtend},


    {"ListBeginToggle", ListBeginToggle},
    {"ListEndToggle", ListEndToggle},





    {"ListBeginSelect", ListBeginSelect},
    {"ListEndSelect", ListEndSelect},
    {"ListKbdBeginSelect", ListKbdBeginSelect},
    {"ListKbdEndSelect", ListKbdEndSelect},





    {"ListKbdBeginExtend", ListKbdBeginExtend},
    {"ListKbdEndExtend", ListKbdEndExtend},



    {"ListKbdSelectAll", ListKbdSelectAll},
    {"ListKbdDeSelectAll", ListKbdDeSelectAll},
    {"ListKbdActivate", ListKbdActivate},
    {"ListKbdCancel", ListKbdCancel},
    {"ListAddMode", ListAddMode},
    {"ListPrevItem", ListPrevItem},
    {"ListNextItem", ListNextItem},
    {"ListPrevPage", ListPrevPage},
    {"ListNextPage", ListNextPage},
    {"ListLeftChar", ListLeftChar},
    {"ListLeftPage", ListLeftPage},
    {"ListRightChar", ListRightChar},
    {"ListRightPage", ListRightPage},









    {"ListExtendPrevItem", ListExtendPrevItem},
    {"ListExtendNextItem", ListExtendNextItem},



    {"ListBeginLine", ListBeginLine},
    {"ListEndLine", ListEndLine},
    {"ListBeginData", ListBeginData},
    {"ListEndData", ListEndData},
    {"ListBeginDataExtend", ListBeginDataExtend},
    {"ListEndDataExtend", ListEndDataExtend},
    {"ListFocusIn", ListFocusIn},
    {"ListFocusOut", ListFocusOut},
    {"ListEnter", ListEnter},
    {"ListLeave", ListLeave},



    {"ListCopyToClipboard", ListCopyToClipboard},
    {"ListProcessDrag", ListProcessDrag},

    {"ListDefaultAction", ListDefaultAction},
};


static XmBaseClassExtRec _XmListCoreClassExtRec = {
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


XmPrimitiveClassExtRec _XmListPrimClassExtRec = {
    /* next_extension      */ NULL,
    /* record_type         */ NULLQUARK,
    /* version             */ XmPrimitiveClassExtVersion,
    /* record_size         */ sizeof(XmPrimitiveClassExtRec),
    /* widget_baseline     */ NULL, /* FIX ME */
    /* widget_display_rect */ NULL, /* FIX ME */
    /* widget_margins      */ NULL  /* FIX ME */
};


XmListClassRec xmListClassRec = {
    /* Core class part */
    {
      /* superclass            */ (WidgetClass) &xmPrimitiveClassRec,
      /* class_name            */ "XmList",
      /* widget_size           */ sizeof(XmListRec),
      /* class_initialize      */ class_initialize,
      /* class_part_initialize */ class_part_initialize,
      /* class_inited          */ False,
      /* initialize            */ initialize,
      /* initialize_hook       */ NULL,
      /* realize               */ XtInheritRealize,
      /* actions               */ actions,
      /* num_actions           */ XtNumber(actions),
      /* resources             */ resources,
      /* num_resources         */ XtNumber(resources),
      /* xrm_class             */ NULLQUARK,
      /* compress_motion       */ False /*True*/,
      /* compress_exposure     */ XtExposeCompressMaximal,
      /* compress_enterleave   */ True,
      /* visible_interest      */ False,
      /* destroy               */ destroy,
      /* resize                */ resize,
      /* expose                */ expose,
      /* set_values            */ set_values,
      /* set_values_hook       */ NULL,
      /* set_values_almost     */ XtInheritSetValuesAlmost,
      /* get_values_hook       */ NULL,
      /* accept_focus          */ NULL,
      /* version               */ XtVersion,
      /* callback offsets      */ NULL,
      /* tm_table              */ _XmList_ListXlations1,
      /* query_geometry        */ query_geometry,
      /* display_accelerator   */ NULL,
      /* extension             */ (XtPointer)&_XmListCoreClassExtRec
    },
    /* Primitive Class part */
    {
	/* border_highlight      */ list_border_highlight,
       	/* border_unhighlight    */ list_border_unhighlight,
       	/* translations          */ NULL /*XtInheritTranslations*/,
       	/* arm_and_activate_proc */ NULL,
       	/* synthetic resources   */ syn_resources, 
        /* num syn res           */ XtNumber(syn_resources),
	/* extension             */ (XtPointer)&_XmListPrimClassExtRec
    },
    /* List Class part */
    {
      /* extension */ NULL
    }
};


WidgetClass xmListWidgetClass = (WidgetClass)&xmListClassRec;


/************************ local functions ************************/

/************************************/
/* List item memory [de]allocation */
/**********************************/
/*
 * This function allocates or reallocates the item list
 */
static void
_XmListReallocItems(Widget w)
{
    int item_count = List_ItemCount(w);

    if (item_count == 0)
    {
	XtFree((char *)List_Items(w));

	List_Items(w) = NULL;

	XtFree((char *)List_InternalList(w));

	List_InternalList(w) = NULL;
    }
    else
    {
	List_Items(w) =
	    (XmStringTable)XtRealloc((char *)List_Items(w),
				     item_count * sizeof(XmString *));

	List_InternalList(w) =
	    (ElementPtr *)XtRealloc((char *)List_InternalList(w),
				    item_count * sizeof(ElementPtr));
    }
}

/*
 * This function allocates or reallocates the selected item list
 */
static void
_XmListReallocSelectedItems(Widget w)
{
    int item_count = List_ItemCount(w);

    /*
     * the selected items list is the same size as the items list
     */
    if (item_count == 0) {
	XtFree((char *)List_SelectedItems(w));
	List_SelectedItems(w) = NULL;
	XtFree((char *)List_SelectedIndices(w));
	List_SelectedIndices(w) = NULL;
    } else {
	List_SelectedItems(w) = (XmStringTable)XtRealloc((char *)List_SelectedItems(w),
				     item_count * sizeof(XmString));

	List_SelectedIndices(w) = (int *)XtRealloc((char *)List_SelectedIndices(w),
			     item_count * sizeof(int));
    }
}


static void
_XmListFreeItems(Widget w)
{
    int i;

    for (i = 0; i < List_ItemCount(w); i++)
    {
	XmStringFree((XmString)List_Items(w)[i]);

	_XmStringFree((_XmString)List_InternalList(w)[i]->name);
	XtFree((char *)List_InternalList(w)[i]);
    }

    XtFree((char *)List_Items(w));
    XtFree((char *)List_InternalList(w));
}


static void
_XmListFreeSelectedItems(Widget w)
{

    XtFree((char *)List_SelectedItems(w));
    XtFree((char *)List_SelectedIndices(w));
}


/**********************************/
/* Geometry management functions */
/********************************/
static Dimension
_XmListBestHeight(Widget w)
{
    Dimension best_height;
    Dimension highlight;
    Dimension spacing = 0;
    int hi_mult = 2;

    highlight = Prim_HighlightThickness(w);
    /*
    *  6/24/98 pgw:  
    *
    *  The highlight is incremented by 1 under 1.2.3 
    *  if not zero.  Found this when playing with the 
    *  values of highlightThickness, 
    *  shadowThickness and listMarginHeight in a resource file. 
    *  This is a very effective method for finding geo defects.
    */
    if (highlight != 0)
    {
       highlight++;
    }
#if XmVERSION > 1 
    /* do nothing */
#else
    /*
    *  6/24/98 pgw:
    * 
    *  This is a 1.2.3 ism..ick. 
    */
    if (List_ItemCount(w) == 0 && List_VisibleItemCount(w) <= 1)
    {
       hi_mult++;
    }
#endif

    if (List_VisibleItemCount(w) > 1)
    {
       spacing = (List_VisibleItemCount(w)-1)*(List_ItemSpacing(w)+highlight);
    }
    best_height = (List_VisibleItemCount(w) * List_MaxItemHeight(w))
                  + (2*(List_MarginHeight(w) + Prim_ShadowThickness(w)))
                  + (hi_mult*highlight)
                  + spacing;  

    return best_height;
#if 0
    /* 
       Note: This is correct for M*tif 2.0.  However, I saw different
       results under M*tif 1.2.3 when item count is 0.  Someone should
       check this against 1.2.4 and above. 
    */
    /*
     * JKF - Added code to support special case when VisibleItemCount == 1 && Itemcount == 0.
     * This is part of the fix for test7, test8 and test14.
     */
    return (List_VisibleItemCount(w) * (List_MaxItemHeight(w) + highlight + 1) +
	    highlight + 1 +
	    2 * (List_MarginHeight(w) + Prim_ShadowThickness(w)) +
	    (List_VisibleItemCount(w) <= 1 && List_ItemCount(w) == 0 ?  
		highlight + 1 : 
	    	(List_VisibleItemCount(w) - 1) * (List_ItemSpacing(w))));
#endif
}


static Dimension
_XmListBestWidth(Widget w)
{
    Dimension best_width;
    Dimension highlight;

    highlight = Prim_HighlightThickness(w);
    /*
    *  6/24/98 pgw:  
    *
    *  The highlight is incremented by 1 under 1.2.3 
    *  if not zero.  Found this when playing with the 
    *  values of highlightThickness, 
    *  shadowThickness and listMarginWidth in a resource file. 
    *  This is a very effective method for finding geo defects.
    */
    if (highlight != 0)
    {
       highlight++;
    }
    best_width = List_MaxWidth(w) 
                 + (2*(List_MarginWidth(w)+Prim_ShadowThickness(w)+highlight));
    return best_width;
#if 0
    return (List_MaxWidth(w) + 2 * (Prim_ShadowThickness(w) + 
			List_MarginWidth(w) + highlight + 1));
#endif
#if 0
    if (List_ItemCount(w) > 0)
    {
	return (List_MaxWidth(w) + 2 * (Prim_ShadowThickness(w) +
					List_MarginWidth(w) + highlight + 1));
    }
    /* special case, if there are no items in list */
    else
    {
        /* 
           Note: This is correct for M*tif 2.0.  However, I saw different
           results under M*tif 1.2.3 when item count is 0.  Someone should
           check this against 1.2.4 and above. 
        */
       /*
        * JKF - Added code to support special case when VisibleItemCount == 1 && Itemcount == 0.
        * This is part of the fix for test7, test8, and test14.
        */
	return (List_VisibleItemCount(w) * (List_MaxItemHeight(w)) + 
	(List_VisibleItemCount(w) <= 1 ?  
			highlight + 1 : 
			(List_VisibleItemCount(w) - 1) * (List_ItemSpacing(w) + highlight + 1))  ) / 2 +
	    2 * (Prim_ShadowThickness(w) + List_MarginWidth(w) + highlight + 1);
    }
#endif
}


static Dimension
_XmListAvailableWidth(Widget w)
{
    return (XtWidth(w) - 2 * (Prim_ShadowThickness(w) + List_MarginWidth(w) +
			      Prim_HighlightThickness(w) + 1));
}


/*
  FUNCTION: _XmListEstimateItemSize
  SYNOPSIS: static void _XmListEstimateItemSize(Widget w)
  DESCRIPTION:
  This function estimates the List_MaxWidth(w) and List_MaxItemHeight(w)
  when no items are in the list.  
  END:
*/
static void 
_XmListEstimateItemSize(Widget w)
{
    Dimension max_height = 0, max_width = 0;
    XmString x;
    Dimension adjustment;

    if (List_ItemCount(w) != 0) return;

    /*
     *  This is probably a kluge, but it seems to work.
     *
     *  6/25/98 pgw: 
     *
     *  This works like a charm.
     */
    x = XmStringCreateSimple("W");
    XmStringExtent(List_Font(w), x, &max_width, &max_height);
    XmStringFree(x);
    x = NULL;

    /* check for zero just in case something does not do what we expect */
    if(max_width == 0)
    {
        max_width = 6;
    }

    /* At this point the visible item count should never be zero:
    assert(List_VisibleItemCount(w) != 0); */

    max_width = (max_width * List_VisibleItemCount(w)) 
                + (List_VisibleItemCount(w)/2); 
    max_height += List_ItemSpacing(w);

    adjustment = 0;

#if XmVERSION > 1
    if(List_VisibleItemCount(w) > 1)
    {
       /* 
        * 7/1/98 pgw:
        * No floating point calcs needed as in 1.2.x.
        */
       adjustment+=((Prim_HighlightThickness(w)>0)?
                     (((Prim_HighlightThickness(w)+1)
                      *(List_VisibleItemCount(w)-1))/2)
                     :0);
    }
#else
    /*
     * 6/25/98 pgw:
     *
     * Spacing adjustment calculation for width. 
     * The following is 1.2ish behavior.
     */
    if(List_VisibleItemCount(w) > 1)
    {
       Dimension foo;

       /*
        * 7/13/98 pgw:
        * Simplified calculation to use integer math again.
        */
       foo = ((List_ItemSpacing(w)>0)?
                     ((List_VisibleItemCount(w)-1) * List_ItemSpacing(w)):0)
           + ((Prim_HighlightThickness(w)>0)?
                     ((Prim_HighlightThickness(w)+1) * (List_VisibleItemCount(w)-1)):0);

       if (foo > 0)
       {
          adjustment += foo/2;
       }
    }
    else
    {
       if(List_ItemSpacing(w) != 0 || Prim_HighlightThickness(w) !=0)
       {
          adjustment += ((List_ItemSpacing(w)+1)+(Prim_HighlightThickness(w)+1))/2;
       }
    }
#endif

    max_width += adjustment;

    List_MaxItemHeight(w) = max_height;
    List_MaxWidth(w) = max_width;

}


static void
_XmListRecalcItemSize(Widget w)
{
    int max_width = 0;
    int max_height = 0;
	
    int i;
    /*
     * If there are no items, don't do anything.
     */
    if (List_ItemCount(w) == 0) return;	

    for (i = 0; i < List_ItemCount(w); i++)
    {
	max_width = (Dimension)_XmMax(List_InternalList(w)[i]->width, max_width);
	max_height = (Dimension)_XmMax(List_InternalList(w)[i]->height, max_height);
    }
    List_MaxWidth(w) = max_width;
    List_MaxItemHeight(w) = max_height;
}


/*
  FUNCTION: _XmListDetermineItemSize
  SYNOPSIS: static void _XmListDetermineItemSize(Widget w)
  DESCRIPTION:
  This function determines the item size by calling the
  apropriate routine.
  END:
*/
static void 
_XmListDetermineItemSize(Widget w)
{
    if (List_ItemCount(w) == 0)
    {
        _XmListEstimateItemSize(w);
    }
    else 
    {
        _XmListRecalcItemSize(w);
    }

    DEBUGOUT(_LtDebug(__FILE__, w, 
                      "_XmListDetermineItemSize: MaxWidth = %d, MaxItemHeight = %d\n",
                      List_MaxWidth(w),
                      List_MaxItemHeight(w)));
}


static void
_XmListCalcVisibleItemCount(Widget w, Boolean *redraw_all)
{
    /* Sets the VisibleItemCount based on the current geometry */
    int nitems;
    int highlight = Prim_HighlightThickness(w);
    Dimension available_height = (XtHeight(w) -
				  2 * (highlight + List_MarginHeight(w)));

    nitems = (available_height - (highlight + 1) + List_ItemSpacing(w)) /
	(List_MaxItemHeight(w) + List_ItemSpacing(w) + highlight + 1);

    DEBUGOUT(_LtDebug(__FILE__, w,
		      "XmListCalcVisibleItemCount => nitems: %d item height: %d\n",
		      nitems, List_MaxItemHeight(w)));

    if (nitems <= 0)
    {
	nitems = 1;
    }

    if (XtHeight(w) > List_MaxItemHeight(w) )
    {
	/*
	 * This used to update List_VisibleItemCount only when
	 * nitems was larger than List_VisibleItemCount; however,
	 * List_VisibleItemCount must be nitems at all times since
	 * the visible item count is used to configure the vertical
	 * scrollbar (in particular, the visible item count is used
	 * to set the slider size).
	 */
	List_VisibleItemCount(w) = nitems;
	*redraw_all = True;
    }

    DEBUGOUT(_LtDebug(__FILE__, w, "XmListCalcVisibleItemCount => w: %d h: %d, "
		      "VisibleItemCount %d\n", XtWidth(w), XtHeight(w),
		      List_VisibleItemCount(w)));
}


static void
_XmListCalculateInitialGeometry(Widget new_w, Widget request)
{
    Boolean redraw_all = False;	/* Dummy var */

    DEBUGOUT(_LtDebug(__FILE__, new_w,
		    "_XmListCalculateInitialGeometry => VisibleItemCount %d request %dx%d\n",
		      List_VisibleItemCount(new_w),
		      XtWidth(request), XtHeight(request)));

    if (XtWidth(request) == 0)
    {
	XtWidth(new_w) = _XmListBestWidth(new_w);
    }
    if (XtHeight(request) == 0)
    {
	XtHeight(new_w) = _XmListBestHeight(new_w);
    }
    else
    {
	_XmListCalcVisibleItemCount(new_w, &redraw_all);
    }
}


static void
_XmListCalcHeight(Widget w, Dimension *ret_height)
{
    Dimension calc_height;

    calc_height = XtHeight(w);

    /*
     * 7/1/98, pgw: Only do this if not realized.  
     * Test15 requires this.
     */
    if(!XtIsRealized(w)) 
    {
        /*
         * 6/24/98, pgw: The height can change in 1.2 due to 
         * the item count going from zero to one.  The problem
         * is the estimated height in 1.2 is too large.
         */
        calc_height = _XmListBestHeight(w);
    }

    *ret_height = calc_height;
}


static void
_XmListCalcWidthBasedOnSizePolicy(Widget w, Dimension *ret_width) 
{
    Dimension calc_width, best_width;

    DEBUGOUT(_LtDebug(__FILE__, w, 
	"_XmListCalcWidthBasedOnSizePolicy() original w: %d, h: %d\n", 
		XtWidth(w), XtHeight(w)));

    /* 
       REMARK:
       A new width value may have been specified in set_values.  
       END:
    */
    calc_width = XtWidth(w);

    /*
       REMARK:
       Interesting..things don't change after being realized. See test11, test12, 
       and test13.  Test14 shows that the list widget still remains constant after
       being realized (when parent is a scrolled window).
       END:
    */
/* MGV used to be #defined unconditionally
#ifndef MGV
    if(!XtIsRealized(w)) 
#endif
*/
    { 
       best_width = _XmListBestWidth(w);
       /* 
          REMARK:
          If set to XmRESIZE_IF_POSSIBLE means we grow or shrink.
          If set to XmVARIABLE the width grows only..according to the docs.
          END:
       */
       if ((List_SizePolicy(w) == XmRESIZE_IF_POSSIBLE)
           || (List_SizePolicy(w) == XmVARIABLE && best_width > XtWidth(w)))
       {
          calc_width = best_width;
       }
    }

    *ret_width = calc_width;
}


static void
_XmListSetGeometry(Widget w)
{
    XtGeometryResult result;
    XtWidgetGeometry geo;
    Dimension desired_width, desired_height;

    DEBUGOUT(_LtDebug(__FILE__, w, 
	"_XmListSetGeometry() original w: %d, h: %d\n", 
		XtWidth(w), XtHeight(w)));

    /* NOTE: This function needs to be Single Entry/Single Exit */
    List_FromSetNewSize(w) = True;

    /* Calculate the dimensions needed to display VisibleItemCount items */
    desired_height = _XmListBestHeight(w);
    _XmListCalcWidthBasedOnSizePolicy(w, &desired_width); 

    DEBUGOUT(_LtDebug(__FILE__, w,
		      "_XmListSetGeometry() desired w: %d, h: %d\n",
		      (int)desired_width, (int)desired_height));

    geo.request_mode = CWWidth | CWHeight;
    geo.width = desired_width;
    geo.height = desired_height;

    result = _XmMakeGeometryRequest(w, &geo);

    List_FromSetNewSize(w) = False;
}


/* 
   REMARK:
   Call this function only from routines that add and delete items.
   It determines if a width change is really needed.  DO NOT CALL
   THIS from set_values. 
   END:
*/
static void
_XmListSetGeometryIfNeeded(Widget w)
{
    Dimension calc_width, calc_height;

    /*
     * JKF - Added code to fix scrollbar management problems.
     * This is part of the fix for test7, test8 and test14.
     */
    if (List_IsScrolledList(w))
    {
	Boolean manage_changed;

	_XmListSetSBManageVert(w, &manage_changed);
	_XmListSetSBManageHoriz(w, &manage_changed);
    }
    _XmListCalcWidthBasedOnSizePolicy(w, &calc_width);
    _XmListCalcHeight(w, &calc_height);
 
    /* Check to see if we needed to change the geometry */
    if(calc_width != XtWidth(w) || calc_height != XtHeight(w))
    {
        /*
         * 6/25/98 pgw:
         * we only need to change the width..should we call _XmListSetGeometry?
         * If we do its going to recalculate things again.  For now we will be lazy.
         */
        _XmListSetGeometry(w);
    }
    {
    Boolean redraw_all;

	_XmListCalcVisibleItemCount(w, &redraw_all);
    }
}


static void
_XmListHighlight(Widget w)
{
    int line_type = LineSolid;

    DEBUGOUT(_LtDebug(__FILE__, w, "_XmListHighlight()\n"));

    if (List_SelectionPolicy(w) == XmSINGLE_SELECT ||
	List_SelectionPolicy(w) == XmMULTIPLE_SELECT)
    {
	line_type = LineOnOffDash;

	/* this is a special case  */
	if (List_LastHLItem(w) < 1 || List_LastHLItem(w) > List_ItemCount(w))
	{
	    List_LastHLItem(w) = 0;
	}
    }
    else if (List_SelectionPolicy(w) == XmBROWSE_SELECT ||
	     List_SelectionPolicy(w) == XmEXTENDED_SELECT)
    {
	line_type = LineSolid;
    }

    if (List_ItemCount(w) == 0 ||
	List_LastHLItem(w) > (List_TopPosition(w) +
			      List_VisibleItemCount(w) - 1) ||
	List_LastHLItem(w) < List_TopPosition(w))
    {
	_XmDrawHighlight(XtDisplay(w), XtWindow(w), Prim_HighlightGC(w),
			 (Position)(Prim_ShadowThickness(w) +
				    List_MarginWidth(w)),
			 (Position)(Prim_ShadowThickness(w) +
				    List_MarginHeight(w)),
			 (Dimension)(XtWidth(w) - 2 * (List_MarginWidth(w) +
					  Prim_ShadowThickness(w))),
			 (Dimension)(XtHeight(w) - 2 * (List_MarginHeight(w) +
						     Prim_ShadowThickness(w))),
			 Prim_HighlightThickness(w),
			 line_type);
    }
    else
    {
	int itemHeight = List_MaxItemHeight(w) + Prim_HighlightThickness(w) +
			1 + List_ItemSpacing(w);

	_XmDrawHighlight(XtDisplay(w),
			 XtWindow(w),
			 Prim_HighlightGC(w),
			 (Position)(Prim_ShadowThickness(w) +
				    List_MarginWidth(w)),
			 (Position)(Prim_ShadowThickness(w) +
				    List_MarginHeight(w)
				 + (List_LastHLItem(w) - List_TopPosition(w)) *
				    itemHeight),
			 (Dimension)(XtWidth(w) - 2 * (List_MarginWidth(w) +
						     Prim_ShadowThickness(w))),
			 (Dimension)(List_MaxItemHeight(w) +
				     2 * (Prim_HighlightThickness(w) + 1)),
			 Prim_HighlightThickness(w),
			 line_type);
    }
    DEBUGOUT(_LtDebug(__FILE__, w, "_XmListHighlight() - done\n"));
}


static void
_XmListUnhighlight(Widget w)
{

    DEBUGOUT(_LtDebug(__FILE__, w, "_XmListUnhighlight()\n"));

    if (List_ItemCount(w) == 0 ||
	List_LastHLItem(w) > (List_TopPosition(w) +
			      List_VisibleItemCount(w) - 1) ||
	List_LastHLItem(w) < List_TopPosition(w))
    {
	DEBUGOUT(_LtDebug(__FILE__, w, "_XmListUnhighlight() border\n"));

	_XmClearBorder(XtDisplay(w),
		       XtWindow(w),
		       (Position)(Prim_ShadowThickness(w) +
				  List_MarginWidth(w)),
		       (Position)(Prim_ShadowThickness(w) +
				  List_MarginHeight(w)),
		       (Dimension)(XtWidth(w) - 2 * (List_MarginWidth(w) +
						     Prim_ShadowThickness(w))),
		       (Dimension)(XtHeight(w) - 2 * (List_MarginHeight(w) +
						      Prim_ShadowThickness(w))),
		       Prim_HighlightThickness(w));
    }
    else
    {
	int itemHeight = List_MaxItemHeight(w) + Prim_HighlightThickness(w) +
			1 + List_ItemSpacing(w);

	DEBUGOUT(_LtDebug(__FILE__, w, "_XmListUnhighlight() item\n"));

	/*
	 * This call used to clear the entire list again in many cases when
	 * a list is used as a ScrolledList, especially in a dialog box.
	 * The reason was that the final parameter, if 0, was used in Xlib
	 * calls which translate that into "take the entire window".
	 * Danny 28/2/1997
	 */
	DEBUGOUT(_LtDebug(__FILE__, w,
			  "_XmClearBorder(x %d, y %d, w %d, h %d, st %d)\n",
			  (Position)(Prim_ShadowThickness(w) +
				     List_MarginWidth(w)),
			  (Position)(Prim_ShadowThickness(w) +
				     List_MarginHeight(w) +
				     (List_LastHLItem(w) -
				      List_TopPosition(w)) * itemHeight),
			  (Dimension)(XtWidth(w) - 2 * (List_MarginWidth(w) +
						     Prim_ShadowThickness(w))),
			  (Dimension)(List_MaxItemHeight(w) +
				      2 * (Prim_HighlightThickness(w) + 1)),
			  Prim_HighlightThickness(w)));

	_XmClearBorder(XtDisplay(w), XtWindow(w),
		       (Position)(Prim_ShadowThickness(w) +
				  List_MarginWidth(w)),
		       (Position)(Prim_ShadowThickness(w) +
				  List_MarginHeight(w) +
				  (List_LastHLItem(w) - List_TopPosition(w)) *
				  itemHeight),
		       (Dimension)(XtWidth(w) - 2 * (List_MarginWidth(w) +
						     Prim_ShadowThickness(w))),
		       (Dimension)(List_MaxItemHeight(w) +
				   2 * (Prim_HighlightThickness(w) + 1)),
		       Prim_HighlightThickness(w));

    }
}


/************************************/
/* Scrollbar init/update functions */
/**********************************/

/* Manage the vertical, if necessary */
static void
_XmListSetSBManageVert(Widget w, Boolean *manage_changed)
{

    DEBUGOUT(_LtDebug(__FILE__, w,
		      "_XmListSetSBManageVert(%s) DisplayPolicy=%s\n",
		      XtIsManaged(List_VSB(w)) ? "managed" : "unmanaged",
		      (List_SBDisplayPolicy(w) == XmAS_NEEDED)
		      ? "AS_NEEDED"
		      : "STATIC"));

    *manage_changed = False;

    if (List_SBDisplayPolicy(w) == XmAS_NEEDED)
    {

	if (XtIsManaged(List_VSB(w)) &&
	    List_VisibleItemCount(w) >= List_ItemCount(w))
	{
	    XtUnmanageChild((Widget)List_VSB(w));

	    *manage_changed = True;
	}
	else if (!XtIsManaged(List_VSB(w)) &&
		 List_VisibleItemCount(w) < List_ItemCount(w))
	{
	    XtManageChild((Widget)List_VSB(w));

	    *manage_changed = True;

	    DEBUGOUT(_LtDebug(__FILE__, w, "_XmListSetSBManageVert(%s) 1\n",
			      XtIsManaged(List_VSB(w))
			      ? "managed"
			      : "unmanaged"));
	}
    }
    else
    {
	if (!XtIsManaged(List_VSB(w)))
	{
	    XtManageChild((Widget)List_VSB(w));

	    *manage_changed = True;

	    DEBUGOUT(_LtDebug(__FILE__, w, "_XmListSetSBManageVert(%s) 2\n",
			      XtIsManaged(List_VSB(w))
			      ? "managed"
			      : "unmanaged"));
	}
    }

#if 0
    XmUpdateDisplay((Widget)List_VSB(w));
#endif

    DEBUGOUT(_LtDebug(__FILE__, w, "_XmListSetSBManageVert(%s) leaving\n",
		      XtIsManaged(List_VSB(w))
		      ? "managed"
		      : "unmanaged"));
}


/*************************************************************************/
/* Manage the horizontal, if necessary                                   */
/*                                                                       */
/* Motif 1.2.x has a bug/feature related to the resources                */
/* XmNscrollBarDisplayPolicy and XmNlistSizePolicy.  The value of        */
/* XmNscrollBarDisplayPolicy seems to have some effect on the value      */
/* of XmNlistSizePolicy.                                                 */
/*                                                                       */
/* if XmNscrollBarDisplayPolicy                                          */
/* is this, and the value of XmNlistSizePolicy is this,                  */
/*     |                                            |                    */
/*     |                                            |                    */
/*     |            .------------.---------------------------.           */
/*     |            |            |                           |           */
/*     |            V            V                           V           */
/*     |        XmVARIABLE   XmCONSTANT             XmRESIZE_IF_POSSIBLE */
/*     V       --------------------------------------------------------- */
/*             |                                                         */
/* XmSTATIC    |XmVARIABLE   XmCONSTANT             XmCONSTANT           */
/*             |                                                         */
/*             |                                                         */
/* XmAS_NEEDED |XmVARIABLE   XmRESIZE_IF_POSSIBLE,  XmRESIZE_IF_POSSIBLE */
/*             |             but reserves space                          */
/*             |             for horiz scrollbar                         */
/*             --------------------------------------------------------- */
/*                  ^            ^                           ^           */
/*              then the effective XmNlistSizePolicy value is here       */
/*                                                                       */
/*************************************************************************/
static void
_XmListSetSBManageHoriz(Widget w, Boolean *manage_changed)
{
    DEBUGOUT(_LtDebug(__FILE__, w,
		    "_XmListSetSBManageHoriz(%s) - avail, max width = %d %d\n",
		      XtIsManaged(List_HSB(w)) ? "managed" : "unmanaged",
		      _XmListAvailableWidth(w),
		      List_MaxWidth(w)));

    *manage_changed = False;

    if (List_SizePolicy(w) == XmRESIZE_IF_POSSIBLE)
    {
	if (XtIsManaged(List_HSB(w)) &&
	    _XmListAvailableWidth(w) >= List_MaxWidth(w))
	{
	    DEBUGOUT(_LtDebug(__FILE__, w,
			      "_XmListSetSBManageHoriz(%s) - unmanaging\n",
			   XtIsManaged(List_HSB(w)) ? "managed" : "unmanaged");

		     XtUnmanageChild((Widget)List_HSB(w)));

	    *manage_changed = True;
	}
	else if (!XtIsManaged(List_HSB(w)) &&
		 _XmListAvailableWidth(w) < List_MaxWidth(w))
	{
	    DEBUGOUT(_LtDebug(__FILE__, w,
			      "_XmListSetSBManageHoriz(%s) - managing\n",
			  XtIsManaged(List_HSB(w)) ? "managed" : "unmanaged"));

	    XtManageChild((Widget)List_HSB(w));

	    *manage_changed = True;
	}
    }
    else if (List_SizePolicy(w) == XmCONSTANT)
    {
	if (List_SBDisplayPolicy(w) == XmAS_NEEDED)
	{
	    if (_XmListAvailableWidth(w) >= List_MaxWidth(w))
	    {
		if (XtIsManaged(List_HSB(w)))
		{
		    XtUnmanageChild((Widget)List_HSB(w));

		    *manage_changed = True;
		}
	    }
	    else
	    {
		if (!XtIsManaged(List_HSB(w)))
		{
		    XtManageChild((Widget)List_HSB(w));

		    *manage_changed = True;
		}
	    }
	}
	else
	{
	    if (!XtIsManaged(List_HSB(w)))
	    {
		XtManageChild((Widget)List_HSB(w));

		*manage_changed = True;
	    }
	}
    }
    else if (List_SizePolicy(w) == XmVARIABLE)
    {
	if (XtIsManaged(List_HSB(w)) &&
	    _XmListAvailableWidth(w) >= List_MaxWidth(w))
	{
	    DEBUGOUT(_LtDebug(__FILE__, w,
			      "_XmListSetSBManageHoriz(%s) - unmanaging\n",
			  XtIsManaged(List_HSB(w)) ? "managed" : "unmanaged"));

	    XtUnmanageChild((Widget)List_HSB(w));

	    *manage_changed = True;
	}
	else if (!XtIsManaged(List_HSB(w)) &&
		 _XmListAvailableWidth(w) < List_MaxWidth(w))
	{
	    DEBUGOUT(_LtDebug(__FILE__, w,
			      "_XmListSetSBManageHoriz(%s) - managing\n",
			  XtIsManaged(List_HSB(w)) ? "managed" : "unmanaged"));

	    XtManageChild((Widget)List_HSB(w));

	    *manage_changed = True;
	}
    }
}


/* Initialises the scrollbars' min, max and extent values
 * The scrollbars need to be updated together to prevent re-entrancy 
 * problems */
static void
_XmListInitScrollBars(Widget w, Boolean horiz, Boolean vert)
{
    Boolean manage_changed = False;
    int old_Horigin;

    DEBUGOUT(_LtDebug(__FILE__, w, "_XmListInitScrollBars() %s %s %s\n",
		      horiz ? "Horizontal" : "",
		      vert ? "Vertical" : "",
		      List_FromSetSB(w) ? "... Re-entered!" : ""));

    /*
     * As managing the scrollbars causes the Scrolled Window parent to 
     * resize the list, this function can be called recursively.  This 
     * bit of code avoids race conditions caused by that.
     */
    if (!List_IsScrolledList(w) || List_FromSetSB(w))
    {
	return;
    }

    List_FromSetSB(w) = True;

    /*
     * Changing the management of one scrollbar can require a change in
     * the other... this should handle all cases though.... I think
     */
    _XmListSetSBManageVert(w, &manage_changed);
    _XmListSetSBManageHoriz(w, &manage_changed);
    _XmListSetSBManageVert(w, &manage_changed);

    if (manage_changed)
    {
	horiz = vert = True;
    }

    if (horiz)
    {
	if (List_ItemCount(w) == 0 || List_MaxWidth(w) == 0)
	{
	    List_Hmin(w) = 0;
	    List_Hmax(w) = 1;
	    List_Hextent(w) = 1;
	    List_Horigin(w) = 0;
	}
	else
	{
	    List_Hmin(w) = 0;
#ifndef HSB_FIX
	    List_Hmax(w) = List_MaxWidth(w) /*- 1*/;
#else
	    List_Hmax(w) = List_MaxWidth(w) +
		2 * (Prim_ShadowThickness(w) + Prim_HighlightThickness(w) + 1);
#endif	    
	    /*
	     * Here's a weird situation: if the list is created with 0
	     * elements, XtWidth(w) is equal to the sum of all the border
	     * areas, like the shadow, highlight, etc..., so the available
	     * width is actually ZERO.  In this case, I think we should
	     * set Hextent(w) to be the same as Hmax(w).
	     */

#ifndef HSB_FIX
	    if (List_Hmax(w) > _XmListAvailableWidth(w) &&
		_XmListAvailableWidth(w) > 0)
#else
	    if (List_Hmax(w) > XtWidth(w) && XtWidth(w) > 0)
#endif
	    {
		/*
		 * List_Hmax is larger than the visible width, so
		 * make the slider size represent the visible width.
		 */
#ifndef HSB_FIX
		/*
		 * set slider size to be the available width of the list
		 */
		List_Hextent(w) = _XmListAvailableWidth(w);
#else
		/*
		 * set slider size to be the width of the list
		 */	
		List_Hextent(w) = XtWidth(w);
#endif
	    }
	    else
	    {
		/*
		 * whole list is visible horizontally, so slider size
		 * is set to Hmax - Hmin, which equals Hmax
		 */
		List_Hextent(w) = List_Hmax(w);
	    }
	}

	old_Horigin = List_Horigin(w);

	if (List_Horigin(w) < 0)
	{
	    List_Horigin(w) = 0;
	}
	else if (List_Horigin(w) > List_Hmax(w) - List_Hextent(w))
	{
	    List_Horigin(w) = List_Hmax(w) - List_Hextent(w);
	}

	XtVaSetValues((Widget)List_HSB(w),
		      XmNmaximum, List_Hmax(w),
		      XmNminimum, List_Hmin(w),
		      XmNincrement, 10 /*List_CharWidth(w) */ ,
		      XmNsliderSize, List_Hextent(w),
		      XmNvalue, List_Horigin(w),
		      NULL);
	/*
	 * this is a kluge.  I call the valueChanged callback manually
	 */
	if (List_Horigin(w) != old_Horigin)
	{
	    XmScrollBarCallbackStruct cbs;
	    Widget sb = (Widget)List_HSB(w);

	    cbs.event = NULL;
	    cbs.value = List_Horigin(w);
	    cbs.reason = XmCR_VALUE_CHANGED;
	    cbs.pixel = 0;

	    XtCallCallbackList(sb, SCB_ValueChangedCallback(sb), &cbs);
	}
	DEBUGOUT(_LtDebug(__FILE__, w,
			  "_XmListInitScrollBars() horiz - max, min, "
		     "slider, val = %d %d %d %d\n", List_Hmax(w), List_Hmin(w),
			  List_Hextent(w), List_Horigin(w)));

    }

    if (vert)
    {
	int page_inc;

	List_Vmin(w) = 0;

	if (List_ItemCount(w) == 0 || List_VisibleItemCount(w) == 0)
	{
	    List_Vorigin(w) = 0;
	    List_Vextent(w) = 100;
	    List_Vmax(w) = 100;
	    page_inc = 10;
	}
	else
	{
	    if (List_TopPosition(w) < 0)
	    {
		List_TopPosition(w) = 0;
	    }

	    List_Vorigin(w) = List_TopPosition(w) - 1;

	    if (List_Vorigin(w) < 0)
	    {
		List_Vorigin(w) = 0;
	    }

	    List_Vextent(w) = List_VisibleItemCount(w);

	    if (List_ItemCount(w) < List_VisibleItemCount(w))
	    {
		List_Vmax(w) = List_VisibleItemCount(w);
	    }
	    else
	    {
		List_Vmax(w) = List_ItemCount(w);
	    }

	    if (List_Vorigin(w) > List_Vmax(w) - List_Vextent(w))
	    {
		List_Vorigin(w) = List_Vmax(w) - List_Vextent(w);
	    }
	    page_inc = List_Vextent(w) - 1;

	}

	/* rws 21 Jun 1997
	 * page_inc is set in the if above, so let's just check its range
	 * and use it.
	 */
	if (page_inc < 1)
	{
	    page_inc = 1;
	}

	DEBUGOUT(_LtDebug(__FILE__, w,
			  "_XmListInitScrollBars: slider, page, value, max,"
			  " count, visible, top =%d, %d, %d, %d, %d, %d, %d\n",
			List_Vextent(w), page_inc /* rws List_Vextent(w)-1 */ ,
			  List_Vorigin(w), List_Vmax(w),
			  List_ItemCount(w), List_VisibleItemCount(w),
			  List_TopPosition(w)));

	XtVaSetValues((Widget)List_VSB(w),
		      XmNmaximum, List_Vmax(w),
		      XmNminimum, List_Vmin(w),
		      XmNsliderSize, List_Vextent(w),
		      XmNvalue, List_Vorigin(w),
		      XmNpageIncrement, page_inc /* rws List_Vextent(w)-1 */ ,
		      NULL);
    }

    List_FromSetSB(w) = False;

#if 0
    XmUpdateDisplay(w);
#endif

}


/* Set the position of the horizontal scrollbar */
static void
_XmListUpdateHorizScrollBar(Widget w, int origin, Boolean *redraw_all)
{
    if (!List_IsScrolledList(w))
    {
	return;
    }
    DEBUGOUT(_LtDebug(__FILE__, w, "_XmListUpdateHorizScrollBar: origin = %i\n",
		      origin));


    if (origin < List_Hmin(w))
    {
	origin = List_Hmin(w);
    }
    else if (origin > List_Hmax(w) - List_Hextent(w))
    {
	origin = List_Hmax(w) - List_Hextent(w);
    }

    if (origin != List_Horigin(w))
    {
	List_XOrigin(w) = List_Horigin(w) = origin;

	XtVaSetValues((Widget)List_HSB(w), XmNvalue, List_Horigin(w), NULL);

	*redraw_all = True;
    }
}


/* Updates the origin of the vertical scrollbar based on the TopPosition */
static void
_XmListUpdateVertScrollBar(Widget w)
{
    if (!List_IsScrolledList(w))
    {
	return;
    }

    DEBUGOUT(_LtDebug(__FILE__, w, "_XmListUpdateVertScrollBar: value = %d\n",
		      List_TopPosition(w)));

    if (List_Vorigin(w) != List_TopPosition(w) - 1)
    {
	List_Vorigin(w) = List_TopPosition(w) - 1;

	if (List_Vorigin(w) < 0)
	{
	    List_Vorigin(w) = 0;
	}

	XtVaSetValues((Widget)List_VSB(w), XmNvalue, List_Vorigin(w), NULL);
    }
}


/*******************************/
/* List item addition/removal */
/*****************************/

/* Add items to the list in an unselected state */
static void
_XmListAddItemsUnselected(Widget w, XmString *items, int item_count, int position)
{
    int i, n;
    Dimension width, height; 

    DEBUGOUT(_LtDebug(__FILE__, w, "_XmListAddItemsUnselected (itemCount %d)\n",
		      List_ItemCount(w)));

    if (List_LastHLItem(w) == 0)
	List_LastHLItem(w) = 1;

    if (List_ItemCount(w) == 0)
    {
	position = 0;
    }
    else if (position < 0 || position > (List_ItemCount(w) + 1))
    {
	DEBUGOUT(_LtDebug(__FILE__, w,
			  "_XmListAddItemsUnselected (itemCount %d) : "
			  "illegal position %d\n",
			  List_ItemCount(w), position));
	return;
    }

    List_ItemCount(w) += item_count;

    /*
     *  resize List_Items(w) and List_SelectedItems(w) to accomodate
     *  the added items
     */
    _XmListReallocItems(w);
    _XmListReallocSelectedItems(w);

    if (position == 0) {
	position = List_ItemCount(w) - item_count + 1;
    } else {
	/* Shuffle the items down the list */
	for (i = List_ItemCount(w) - 1; i >= position - 1 + item_count; i--) {
	    List_Items(w)[i] = List_Items(w)[i - item_count];
	    List_InternalList(w)[i] = List_InternalList(w)[i - item_count];
	    List_InternalList(w)[i]->LastTimeDrawn = False;
	}
	/* Move the contents of SelectedIndices */
	for (i=0; i<List_SelectedItemCount(w); i++) {
		if (List_SelectedIndices(w)[i] >= position)
			List_SelectedIndices(w)[i] += item_count;
	}
	if (List_LastHLItem(w) && List_LastHLItem(w) >= position) {
	   List_LastHLItem(w)++;
	}
    }

    /* Insert the elements */
    for (i = position - 1, n = 0; n < item_count; i++, n++)
    {
	XmStringExtent(List_Font(w), items[n], &width, &height);

	List_Items(w)[i] = XmStringCopy(items[n]);
	List_InternalList(w)[i] = (ElementPtr)XtMalloc(sizeof(Element));
	List_InternalList(w)[i]->name = _XmStringCreate(items[n]);
	List_InternalList(w)[i]->selected =
	    List_InternalList(w)[i]->last_selected =
	    List_InternalList(w)[i]->LastTimeDrawn = False;

	/*
	 *   Don't forget that each "line" can consist of multiple lines.
	 *   I don't think I handle that properly yet.
	 */
	List_InternalList(w)[i]->NumLines = XmStringLineCount(items[n]);
	List_InternalList(w)[i]->length = XmStringLength(items[n]);
	List_InternalList(w)[i]->height = height;
	List_InternalList(w)[i]->width = width;

#ifdef USE_CUM_HEIGHT
	List_InternalList(w)[i]->CumHeight = height +
	    ((i == 0) ? Prim_ShadowThickness(w) :
	     List_InternalList(w)[i - 1]->CumHeight +
	     2 * Prim_HighlightThickness(w) +
	     List_ItemSpacing(w));
#endif
    }

    /* 
       6/30/98 pgw:
       If initial height was an estimate and max item height was estimated
       too large then we won't get the correct settings here.  We need to
       do a recalculation of the max width and height.
    */
    /*
     * Recalculate the max width, because it could have changed.
     */
    _XmListRecalcItemSize(w);

#ifdef USE_CUM_HEIGHT
    /* Update CumHeight for all elements below the inserted ones */
    for (i = position - 1 + item_count; i < List_ItemCount(w); i++)
    {
	List_InternalList(w)[i]->CumHeight = ((i == 0)
		     ? Prim_ShadowThickness(w) + Prim_HighlightThickness(w) + 1
				    : List_InternalList(w)[i - 1]->CumHeight) +
	    List_InternalList(w)[i]->height +
	    1 + Prim_HighlightThickness(w) + List_ItemSpacing(w);
    }
#endif

    /* List_ItemCount has changed, so re-initialise the scrollbars */
    _XmListInitScrollBars(w, True, True);
}

/* Inserts an item in the list */
static void
_XmListAddItemUnselected(Widget w, XmString item, int position)
{
    _XmListAddItemsUnselected(w, &item, 1, position);

    DEBUGOUT(_LtDebug(__FILE__, w, "_XmListAddItemUnselected(): about to call _XmListRedraw()\n"));
    
    _XmListRedraw(w, False);
}


/* Deletes the item at position <position> */
static void
_XmListDeletePos(Widget w, int position)
{
    int i;
    Dimension height;
    Boolean is_selected;

    if (position < 0 || position > List_ItemCount(w))
    {
	return;
    }

    if (!position)
    {
	position = List_ItemCount(w);
    }

    /*
     * This seems to be a quirk of Motif.  When the last selected item is
     * deleted, the last selected item position is decremented, unless it
     * is the first item
     */
    if (position == List_LastHLItem(w) && position != 1)
    {
	/*
	 * Looks like last_selected is not yet implemented, but I'll do
	 * this anyway
	 */
	List_InternalList(w)[List_LastHLItem(w) - 1]->last_selected = False;

	List_LastHLItem(w)--;

	List_InternalList(w)[List_LastHLItem(w) - 1]->last_selected = True;
    }

    height = List_InternalList(w)[position - 1]->height;

    /* Free the item's memory */
    XmStringFree(List_Items(w)[position - 1]);

    _XmStringFree((_XmString)List_InternalList(w)[position - 1]->name);
    XtFree((char *)List_InternalList(w)[position - 1]);

    /* Shuffle the other items down */
    for (i = position - 1; i < List_ItemCount(w) - 1; i++)
    {
	List_Items(w)[i] = List_Items(w)[i + 1];
	List_InternalList(w)[i] = List_InternalList(w)[i + 1];

#ifdef USE_CUM_HEIGHT
	List_InternalList(w)[i]->CumHeight -=
	    height + 2 * Prim_HighlightThickness(w) + List_ItemSpacing(w);
#endif
    }

    /* Handle case where item is selected */
    if (List_LastItem(w) == position)
    {
	List_LastItem(w) = 0;
    }

    for (is_selected = False, i = 0; i < List_SelectedItemCount(w); i++)
    {
	/* If we haven't found the item yet, test for it, else shuffle */
	if (!is_selected)
	{
	    if (List_SelectedIndices(w)[i] == position)
	    {
		List_SelectedItemCount(w)--;
		is_selected = True;
		List_SelectedIndices(w)[i] = List_SelectedIndices(w)[i+1];
		List_SelectedItems(w)[i] = List_SelectedItems(w)[i+1];
	    }
	}
	else
	{
	    List_SelectedIndices(w)[i] = List_SelectedIndices(w)[i + 1];
	    List_SelectedItems(w)[i] = List_SelectedItems(w)[i + 1];
	}
	/* Need to shuffle indices too */
	if (List_SelectedIndices(w)[i] > position) {
	    List_SelectedIndices(w)[i]--;
	}
    }

    /*
     * Realloc items.  I could optimize this by not reallocating unless the
     * list needs to be larger. 
     */
    --List_ItemCount(w);
    _XmListReallocItems(w);
    _XmListReallocSelectedItems(w);

    /*
     * Determine Item Size
     */
    _XmListDetermineItemSize(w);

    if (List_ItemCount(w) &&
	List_TopPosition(w) + List_VisibleItemCount(w) - 1 > List_ItemCount(w))
    {
	List_TopPosition(w) = 1;
    }
 
    _XmListInitScrollBars(w, True, True);
}


/* Deletes an item with value <item>, if one exists */
static Boolean
_XmListDeleteItem(Widget w, XmString item)
{
    int i;

    for (i = 0; i < List_ItemCount(w); i++) {
	if (XmStringCompare(item, List_Items(w)[i])) {
	    _XmListDeletePos(w, i + 1);
	    return True;
	}
    }

    return False;
}


/*
 * Clones the items and InternalList instance variable for the widget
 * and reallocates the selected items arrays. 
 */
static void
_XmListInstallItems(Widget w)
{
    XmString *new_items, *list_items = List_Items(w);

    if (Prim_Highlighted(w))
	_XmListUnhighlight(w);
    List_LastHLItem(w) = 1;

    DEBUGOUT(_LtDebug(__FILE__, w, "_XmListInstallItems: entering\n"));

    /* this looks like the value M*tif uses */
    List_CharWidth(w) = 10;

    DEBUGOUT(_LtDebug(__FILE__, w, "_XmListInstallItems: CharWidth = %d\n", List_CharWidth(w)));

    List_Items(w) = NULL;
    List_InternalList(w) = NULL;
	/* T. Straumann: setting this to NULL may cause a memory leak;
	 *				 it will be reallocated down there anyway
     * List_SelectedIndices(w) = NULL;
	 */

    if (List_ItemCount(w) != 0) {
	int i;
	ElementPtr *new_elements;
#ifdef USE_CUM_HEIGHT
	Dimension cum_height = Prim_ShadowThickness(w);
#endif

	_XmListReallocItems(w);
	_XmListReallocSelectedItems(w);

	new_items = List_Items(w);
	new_elements = List_InternalList(w);

	for (i = 0; i < List_ItemCount(w); i++) {
	    new_items[i] = XmStringCopy(list_items[i]);

	    new_elements[i] = (ElementPtr)XtMalloc(sizeof(Element));

	    new_elements[i]->name = _XmStringCreate(new_items[i]);
	    new_elements[i]->selected = False;
	    new_elements[i]->last_selected = False;
	    new_elements[i]->LastTimeDrawn = False;

	    new_elements[i]->NumLines = XmStringLineCount(new_items[i]);
	    new_elements[i]->length = XmStringLength(new_items[i]);

	    XmStringExtent(List_Font(w), new_items[i],
			   &new_elements[i]->width, &new_elements[i]->height);

#ifdef USE_CUM_HEIGHT
	    cum_height += (new_elements[i]->height + 2 * Prim_HighlightThickness(w)
			   + List_ItemSpacing(w));

	    new_elements[i]->CumHeight = cum_height;
#endif
	}
    }

#if 0
    /* Bug #916711 : don't wipe out selection on SetValues */
    List_SelectedItemCount(w) = 0;
#endif
    _XmListDetermineItemSize(w);
    _XmListInitScrollBars(w, True, True);

    DEBUGOUT(_LtDebug(__FILE__, w, "_XmListInstallItems: exiting\n"));
}

/*******************************/
/* Item selection/deselection */
/*****************************/


/* Selects the item at position <position> */
static void
_XmListSelectPos(Widget w, int position)
{
    int i;

    if (position < 0 || position > List_ItemCount(w))
    {
	return;
    }

    if (!position)
    {
	position = List_ItemCount(w);
    }

    if (List_InternalList(w)[position - 1]->selected == True)
    {
	return;
    }

    List_InternalList(w)[position - 1]->selected = True;
    List_InternalList(w)[position - 1]->LastTimeDrawn = False;

    if (List_SelectionPolicy(w) == XmSINGLE_SELECT ||
	List_SelectionPolicy(w) == XmBROWSE_SELECT)
    {
	List_SelectedItems(w)[0] = List_Items(w)[position - 1];
	List_SelectedIndices(w)[0] = position;
	List_SelectedItemCount(w) = 1;
    }
    else
    {
	for (i = 0; i < List_SelectedItemCount(w); i++)
	{
	    if (position == List_SelectedIndices(w)[i])
	    {
		/* it's already in the list.  Don't add it again */
		return;
	    }
	}

	/* can't select any more items? */
	if (List_SelectedItemCount(w) == List_ItemCount(w))
	{
	    return;
	}

	/*
	 * set as StartItem incase this is the first item selected 
	 */ 
	if (List_SelectedItemCount(w) == 0) 
		List_StartItem(w) = position; 

	/*
	 * Bart Schaefer <schaefer@zanshin.com> says List_SelectedItems(w)
	 * can be NULL here, so let's protect against it.
	 */
	if (List_SelectedItems(w)) {
	    List_SelectedItems(w)[List_SelectedItemCount(w)] =
		List_Items(w)[position - 1];
	    List_SelectedIndices(w)[List_SelectedItemCount(w)] = position;
	    List_SelectedItemCount(w)++;
	}
    }

    List_LastItem(w) = position;
}


/* Selects all unselected items */
static void
_XmListSelectAll(Widget w)
{
    int pos;

    for (pos = 1; pos <= List_ItemCount(w); pos++)
    {
	_XmListSelectPos(w, pos);
    }
}


static void
_XmListSelectPosIfMatch(Widget w, int position)
{
    int i;

    /* position == 0 means last position */
    if (position == 0)
    {
	position = List_ItemCount(w);
    }

    /* do nothing if already selected */
    if (List_InternalList(w)[position - 1]->selected)
    {
	return;
    }

    /* if the item at the specified position matches any of 
     * the items in XmNselectedItems, then select the item at
     * the specified position */
    for (i = 0; i < List_SelectedItemCount(w); ++i)
    {
	if (XmStringCompare(List_Items(w)[position - 1],
			    List_SelectedItems(w)[i]))
	{
	    _XmListSelectPos(w, position);

	    break;
	}
    }
}


/* Deselects the item at position <position> */
static Boolean
_XmListDeselectPos(Widget w,
		   int position)
{
    int i, j;

    DEBUGOUT(_LtDebug(__FILE__, w,
		      "_XmListDeselectPos() last_item = %d\n",
		      List_LastItem(w)));

    if (position < 0 || position > List_ItemCount(w))
    {
	return False;
    }

    if (!position)
    {
	position = List_ItemCount(w);
    }

    if (List_SelectionPolicy(w) == XmSINGLE_SELECT ||
	List_SelectionPolicy(w) == XmBROWSE_SELECT)
    {
	if (List_LastItem(w) == position)
	{
	    List_InternalList(w)[position - 1]->selected = False;
	    List_InternalList(w)[position - 1]->LastTimeDrawn = False;
	    List_SelectedItemCount(w) = 0;
	    List_LastItem(w) = 0;

	    return True;
	}
    }
    /* XmMULTIPLE_SELECT or XmEXTENDED_SELECT */
    else
    {
	if (List_SelectedItemCount(w) == 0)
	{
	    return False;
	}

	List_InternalList(w)[position - 1]->selected = False;
	List_InternalList(w)[position - 1]->LastTimeDrawn = False;
	if (List_LastItem(w) == position)
	{
	    List_LastItem(w) = 0;
	}

	for (i = 0; i < List_SelectedItemCount(w); i++)
	{
	    if (position == List_SelectedIndices(w)[i])
	    {
		for (j = i; j < List_SelectedItemCount(w) - 1; j++)
		{
		    List_SelectedIndices(w)[j] = List_SelectedIndices(w)[j + 1];
		    /* toed: the selected elements need to be moved up too */
		    List_SelectedItems(w)[j] = List_SelectedItems(w)[j + 1];
		}

		List_SelectedItemCount(w)--;
		break;
	    }
	}

    }
    return True;
}


/* Deselects all selected items */
static void
_XmListDeselectAll(Widget w)
{
    int i;

    DEBUGOUT(_LtDebug(__FILE__, w, "_XmListDeselectAll()\n"));

    for (i = 0; i < List_ItemCount(w); i++)
    {
	if (List_InternalList(w)[i]->selected)
	{
	    List_InternalList(w)[i]->selected = False;
	    List_InternalList(w)[i]->LastTimeDrawn = False;
	}
    }

    /* should I deallocate the array here? */
    List_SelectedItemCount(w) = 0;
    List_LastItem(w) = 0;
}


/* Toggles the selection state of position <position> */
static void
_XmListTogglePos(Widget w, int position)
{
    if (List_InternalList(w)[position - 1]->selected)
    {
	_XmListDeselectPos(w, position);
    }
    else
    {
	_XmListSelectPos(w, position);
    }
}


static void
_XmListInvokeCallbacks(Widget w,
		       XEvent *event,
		       Boolean default_action)
{
    int *selected_item_positions = NULL, selected_item_count;
    XmString *selected_items = NULL;
    XmListCallbackStruct call_data;
    XtCallbackList callbacks = NULL;

    DEBUGOUT(_LtDebug(__FILE__, w,
		      "_XmListInvokeCallbacks(%d)\n", default_action));

    /* information that is always present in callbacks */
    call_data.event = event;
    if (List_LastItem(w) > 0)
    {
DEBUGOUT(_LtDebug(__FILE__, w, "_XmListInvokeCallbacks() - last item > 0\n"));
	call_data.item = List_Items(w)[List_LastItem(w) - 1];
	call_data.item_position = List_LastItem(w);
	call_data.item_length = XmStringLength(call_data.item);
    }
    else
    {
	if (List_LastHLItem(w) > 0)
	{
	    call_data.item = List_Items(w)[List_LastHLItem(w) - 1];
	    call_data.item_position = List_LastHLItem(w);
	    call_data.item_length = XmStringLength(call_data.item);
	}
	else
	{
	    call_data.item = NULL;
	    call_data.item_position = 0;
	    call_data.item_length = 0;
	}
    }

    /* defaultActionCallback */
    if (default_action && List_DefaultCallback(w) != NULL)
    {
DEBUGOUT(_LtDebug(__FILE__, w, "_XmListInvokeCallbacks() - default action\n"));
	call_data.reason = XmCR_DEFAULT_ACTION;
	callbacks = List_DefaultCallback(w);
    }
    /* one of the selection policy callbacks */
    else
    {
	switch (List_SelectionPolicy(w))
	{
	case XmBROWSE_SELECT:
DEBUGOUT(_LtDebug(__FILE__, w, "_XmListInvokeCallbacks() - BROWSE_SELECT\n"));
	    call_data.reason = XmCR_BROWSE_SELECT;
	    callbacks = List_BrowseCallback(w);
	    break;

	case XmSINGLE_SELECT:
DEBUGOUT(_LtDebug(__FILE__, w, "_XmListInvokeCallbacks() - SINGLE_SELECT\n"));
	    call_data.reason = XmCR_SINGLE_SELECT;
	    callbacks = List_SingleCallback(w);
	    break;

	case XmMULTIPLE_SELECT:
DEBUGOUT(_LtDebug(__FILE__, w, "_XmListInvokeCallbacks() - MULTIPLE_SELECT\n"));
	    call_data.reason = XmCR_MULTIPLE_SELECT;
	    callbacks = List_MultipleCallback(w);
	    break;

	case XmEXTENDED_SELECT:
DEBUGOUT(_LtDebug(__FILE__, w, "_XmListInvokeCallbacks() - EXTENDED_SELECT\n"));
	    call_data.reason = XmCR_EXTENDED_SELECT;
	    callbacks = List_ExtendCallback(w);
	    break;
	}
    }

    if (callbacks == NULL)
    {
DEBUGOUT(_LtDebug(__FILE__, w, "_XmListInvokeCallbacks() - no callbacks\n"));
	return;
    }

    if (call_data.reason == XmCR_EXTENDED_SELECT ||
	call_data.reason == XmCR_MULTIPLE_SELECT)
    {
	int i;

	selected_item_positions =
	    (int *)XtMalloc(List_SelectedItemCount(w) * sizeof(int));
	selected_items = (XmString *)XtMalloc(List_SelectedItemCount(w) *
					      sizeof(XmString));

	for (i = 0; i < List_SelectedItemCount(w); i++)
	    selected_items[i] = XmStringCopy(List_SelectedItems(w)[i]);

	memcpy(selected_item_positions,
              List_SelectedIndices(w),
	      List_SelectedItemCount(w) * sizeof(int));

	call_data.selected_items = selected_items;
	call_data.selected_item_positions = selected_item_positions;
	call_data.selected_item_count = List_SelectedItemCount(w);

	if (call_data.reason == XmCR_EXTENDED_SELECT)
	{
	    call_data.selection_type = List_SelectionType(w);
	}
    }

    selected_item_count = List_SelectedItemCount(w);

DEBUGOUT(_LtDebug(__FILE__, w, "_XmListInvokeCallbacks() - calling callbacks\n"));
    XtCallCallbackList(w, callbacks, (XtPointer)&call_data);
DEBUGOUT(_LtDebug(__FILE__, w, "_XmListInvokeCallbacks() - called callbacks\n"));

    if (call_data.reason == XmCR_EXTENDED_SELECT ||
	call_data.reason == XmCR_MULTIPLE_SELECT)
    {
	int i;

	for (i = 0; i < selected_item_count; i++)
	{
	    XmStringFree(selected_items[i]);
	}

	XtFree((char *)selected_items);

	XtFree((char *)selected_item_positions);
    }
DEBUGOUT(_LtDebug(__FILE__, w, "_XmListInvokeCallbacks() - done\n"));
}

/**********************/
/* Drawing functions */
/********************/

/*
 * set the redraw_all_visible to True if all the items visible should be
 * redrawn.  If you're changing just one item, set this to false,
 * but make sure you set the LastTimeDrawn in that item's InternalList entry
 * to False
 */
static void
_XmListRedraw(Widget w,
	      Boolean redraw_all_visible)
{
    Position x, y;
    int i;
    GC myGC = XtSensitive(w) ? List_NormalGC(w) : List_InsensitiveGC(w);
    Dimension fill_width = (XtWidth(w) - 2 * (Prim_ShadowThickness(w) +
					      List_MarginWidth(w) +
					      Prim_HighlightThickness(w) + 1));

    DEBUGOUT(_LtDebug(__FILE__, w, "_XmListRedraw(): width = %d, visible = %d\n",
		      (int)XtWidth(w), List_VisibleItemCount(w)));


    if (!XtIsRealized(w))
    {
	return;
    }

    if (redraw_all_visible)
    {
	XClearWindow(XtDisplay(w), XtWindow(w));
    }

    x = (List_MarginWidth(w) + Prim_ShadowThickness(w) +
	 Prim_HighlightThickness(w) + 1);	/* why is this + 1??? */
    y = (List_MarginHeight(w) + Prim_ShadowThickness(w) +
	 Prim_HighlightThickness(w) + 1);	/* why is this + 1??? */

    for (i = List_TopPosition(w) - 1;
	 (i < List_ItemCount(w)) &&
	 (i < List_VisibleItemCount(w) + List_TopPosition(w) - 1);
	 i++)
    {

	if (i < 0)
	{
	    continue;
	}

	if (List_InternalList(w)[i]->LastTimeDrawn == False ||
	    redraw_all_visible == True)
	{
	    XRectangle clip_rect;

	    DEBUGOUT(_LtDebug(__FILE__, w,
			 "  _XmListRedraw() redrawing item %d...yes\n", i + 1));

	    /* we don't want to continually redraw this item... */
	    List_InternalList(w)[i]->LastTimeDrawn = True;

	    clip_rect.x = (Position)x;
	    clip_rect.y = (Position)y;
	    clip_rect.width = (Dimension)(fill_width);
	    clip_rect.height = (Dimension)List_MaxItemHeight(w);


	    XFillRectangle(XtDisplay(w), XtWindow(w),
			   (List_InternalList(w)[i]->selected
			    ? List_NormalGC(w) : List_InverseGC(w)),
			   x, y,
			   fill_width,
			   List_MaxItemHeight(w));

	    if (List_InternalList(w)[i]->selected)
	    {
#ifdef JONS_CLIP_MASK_KLUDGE
		XSetClipRectangles(XtDisplay(w), List_InverseGC(w),
				   0, 0, &clip_rect, 1, Unsorted);
#endif
		_XmStringDraw(XtDisplay(w),
			      XtWindow(w),
			      List_Font(w),
			      List_InternalList(w)[i]->name,
			      List_InverseGC(w),
			      x - List_Horigin(w),
			      y + (List_MaxItemHeight(w) -
				   List_InternalList(w)[i]->height) / 2,
			      fill_width,
			      XmALIGNMENT_BEGINNING,
			      List_StrDir(w),
			      &clip_rect);

#ifdef JONS_CLIP_MASK_KLUDGE
		/* remove clip mask */
		XSetClipMask(XtDisplay(w), List_InverseGC(w), None);
#endif
	    }

	    if (!List_InternalList(w)[i]->selected || !XtSensitive(w))
	    {
#ifdef JONS_CLIP_MASK_KLUDGE
		XSetClipRectangles(XtDisplay(w), myGC,
				   0, 0, &clip_rect, 1, Unsorted);
#endif

		_XmStringDraw(XtDisplay(w),
			      XtWindow(w),
			      List_Font(w),
			      List_InternalList(w)[i]->name,
			      myGC,
			      x - List_Horigin(w),
			      y + (List_MaxItemHeight(w) -
				   List_InternalList(w)[i]->height) / 2,
			      fill_width,
			      XmALIGNMENT_BEGINNING,
			      List_StrDir(w),
			      &clip_rect);

#ifdef JONS_CLIP_MASK_KLUDGE
		/* remove clip mask */
		XSetClipMask(XtDisplay(w), myGC, None);
#endif
	    }
	}
	else
	{
	    DEBUGOUT(_LtDebug(__FILE__, w,
			  "  _XmListRedraw() redrawing item %d...no\n", i + 1));
	}

	y += (List_MaxItemHeight(w) + Prim_HighlightThickness(w) + 1 +
	      List_ItemSpacing(w));
    }

    _XmDrawShadows(XtDisplay(w),
		   XtWindow(w),
		   Prim_TopShadowGC(w),
		   Prim_BottomShadowGC(w),
		   0, 0,
		   XtWidth(w),
		   XtHeight(w),
		   Prim_ShadowThickness(w), XmSHADOW_IN);

    if (Prim_Highlighted(w))
    {
	_XmListHighlight(w);
    }
}


/******************/
/* Miscellaneous */
/****************/


/* Set the current cursor position. Position must be visible */
static void
_XmListSetCursorPos(Widget w, int position)
{
    DEBUGOUT(_LtDebug(__FILE__, w,
		      "_XmListSetCursorPos(): position: %d, HL: %d\n",
		      position, List_LastHLItem(w)));

    /*
     * return if cursor position is out of visible range
     */
    if (position < List_TopPosition(w) ||
	position > List_TopPosition(w) + List_VisibleItemCount(w) - 1)
    {
	return;
    }

    if (List_LastHLItem(w) > List_ItemCount(w))
	List_LastHLItem(w) = 0;

    if (List_LastHLItem(w))
    {
	List_InternalList(w)[List_LastHLItem(w) - 1]->LastTimeDrawn = False;
	if (Prim_Highlighted(w))
	{
	    _XmListUnhighlight(w);
	}
    }

    List_LastHLItem(w) = position;
    List_InternalList(w)[position - 1]->LastTimeDrawn = False;
}



/*
 * Set which item is displayed at the top of the list
 * NOTE: this function should not alter the last highlighted
 * item!
 */
static void
_XmListSetTopPos(Widget w, int position, Boolean *redraw_all)
{
    DEBUGOUT(_LtDebug(__FILE__, w,
		      "_XmListSetTopPos(): position: %d\n", position));

    *redraw_all = False;

    if (List_TopPosition(w) != position)
    {
	if (Prim_Highlighted(w))
	{
	    _XmListUnhighlight(w);
	}

	List_TopPosition(w) = position;
	*redraw_all = True;

	_XmListUpdateVertScrollBar(w);
    }
}


/* Restores the old selection for any items in the current select range */
static void
_XmListRestoreSelectRange(Widget w)
{
    int pos, direction, last_pos;

    /* Restore items from the anchor point to the old position */
    last_pos = List_LastItem(w);
    direction = (last_pos > List_StartItem(w)) ? 1 : -1;

    for (pos = List_StartItem(w) + direction;
	 pos > 0 &&
	 pos != last_pos + direction;
	 pos += direction)
    {
	DEBUGOUT(_LtDebug(__FILE__, w,
			  "In for loop. pos: %d, direction: %d\n",
			  pos, direction));
	if (List_InternalList(w)[pos - 1]->saved_select)
	{
	    _XmListSelectPos(w, pos);
	}
	else
	{
	    _XmListDeselectPos(w, pos);
	}
    }
}


/* Changes the current selection range from anchor->List_LastItem(w) to
 * anchor->new-position */
static void
_XmListSetSelectRange(Widget w, int new_position)
{
    int pos, direction;

    DEBUGOUT(_LtDebug(__FILE__, w,
		      "_XmListSetSelectRange() LastItem: %d, "
		      "position: %d, anchor: %d\n",
		      List_LastItem(w), new_position,
		      List_StartItem(w)));

    _XmListRestoreSelectRange(w);

    /* Save items from the anchor point to here and set their 
     * selection state to the same as the anchor */
    direction = (new_position > List_StartItem(w)) ? 1 : -1;
    for (pos = List_StartItem(w) + direction;
	 pos != new_position + direction;
	 pos += direction)
    {
	List_InternalList(w)[pos - 1]->saved_select =
	    List_InternalList(w)[pos - 1]->selected;

	if (List_InternalList(w)[pos - 1]->selected)
	{
	    _XmListDeselectPos(w, pos);
	}
	else
	{
	    _XmListSelectPos(w, pos);
	}
    }

    /* Eeek!  We have to do this so that the focus will move when
     * the mouse button is released, but if the anchor item is
     * unselected then LastItem will not point to the last selected
     * item, as it should.
     */
    List_LastItem(w) = new_position;
}


/***********************/
/* internal callbacks */
/*********************/

static void
_XmListHorizontalScrollBarCallback(Widget w,
				   XtPointer client_data,
				   XtPointer call_data)

{
    XmScrollBarCallbackStruct *cbs = (XmScrollBarCallbackStruct *)call_data;
    Widget list = (Widget)client_data;
	int	old_Horigin = List_Horigin(list);

    DEBUGOUT(_LtDebug(__FILE__, w, "_XmListHorizontalScrollBarCallback()\n"));
	
    List_Horigin(list) = List_XOrigin(list) = cbs->value;
	
    if (List_Horigin(list) != old_Horigin)
	_XmListRedraw(list, True);

}


static void
_XmListVerticalScrollBarCallback(Widget w,
				 XtPointer client_data,
				 XtPointer call_data)
{
    Boolean redraw_all = False;
    XmScrollBarCallbackStruct *cbs = (XmScrollBarCallbackStruct *)call_data;
    Widget list = (Widget)client_data;

    /*  cbs->value must be incremented because scrollbar values
     * are 0..ItemCount */
    _XmListSetTopPos(list, cbs->value + 1, &redraw_all);
    _XmListRedraw(list, redraw_all);
}


/*******************/
/* widget methods */
/*****************/

static void
class_initialize(void)
{
    _XmListCoreClassExtRec.record_type = XmQmotif;
    /* try to limit leaks by calling this once here.  See BulletinBoard.c */
    list_trans = XtParseTranslationTable(_XmList_ListXlations2);
}


static void
class_part_initialize(WidgetClass widget_class)
{
    _XmFastSubclassInit(widget_class, XmLIST_BIT);
}


static void
destroy(Widget w)
{
    DEBUGOUT(_LtDebug(__FILE__, w, "Destroy()\n"));

    _XmListFreeItems(w);
/* T. Straumann: who reported that xmbdfed crashed?
 *				 why introduce a memory leak to fix this ??
 */
#if 1
    /* This crashes xmbdfed */
    _XmListFreeSelectedItems(w);
#endif
#if 0
    XtFree((XtPointer)List_SelectedItems(w));	/* moved out of _XmListFreeItems */
#endif

    XtReleaseGC(w, List_NormalGC(w));
    XtReleaseGC(w, List_InsensitiveGC(w));
    XtReleaseGC(w, List_InverseGC(w));
    XtReleaseGC(w, List_HighlightGC(w));

    if (List_Mom(w) && !CoreBeingDestroyed(List_Mom(w)))
    {
	XtDestroyWidget((Widget)List_Mom(w));
    }
    XmFontListFree(List_Font(w));
}


static void
resize(Widget w)
{
    int new_top;
    Boolean redraw_all = False;

    DEBUGOUT(_LtDebug(__FILE__, w,
		      "resize() (%d, %d)\n", XtWidth(w), XtHeight(w)));


    if (Prim_Highlighted(w))
    {
	_XmListUnhighlight(w);
    }

    /*
     * This is a Motif 1.2 feature.  Motif 1.1.x did not do this.
     */
    _XmListCalcVisibleItemCount(w, &redraw_all);

    if (List_TopPosition(w) > 1 &&
	List_TopPosition(w) > List_ItemCount(w) - List_VisibleItemCount(w) + 1)
    {
	new_top = List_ItemCount(w) - List_VisibleItemCount(w) + 1;
	if (new_top < 1)
	{
	    new_top = 1;
	}

	DEBUGOUT(_LtDebug(__FILE__, w,
			  "TopPos(old,new) = %d, %d\n",
			  List_TopPosition(w), new_top));

	_XmListSetTopPos(w, new_top, &redraw_all);
    }

    _XmListInitScrollBars(w, True, True);
}


static void
expose(Widget w, XEvent *event, Region region)
{
    XmListWidgetClass wc = (XmListWidgetClass)XtClass(w);

    DEBUGOUT(_LtDebug(__FILE__, w, "entering expose()\n"));

    _XmListRedraw(w, True);
    if (Prim_Highlighted(w))
    {
	(*wc->primitive_class.border_highlight) (w);
    }
    else
    {
	(*wc->primitive_class.border_unhighlight) (w);
    }
    DEBUGOUT(_LtDebug(__FILE__, w, "leaving expose()\n"));
#undef superclass
}


static void
CreateNormalGC(Widget w)
{
    XGCValues values;

    values.foreground = Prim_Foreground(w);
    values.background = XtBackground(w);

    List_NormalGC(w) = XtGetGC(w, GCForeground | GCBackground, &values);
}


static void
CreateHighlightGC(Widget w)
{
    unsigned long mask = GCForeground | GCBackground;
    XGCValues values;

    values.foreground = Prim_HighlightColor(w);
    values.background = XtBackground(w);

    if (List_AddMode(w))
    {
#if 0
/* URK!  DashTile isn't initialized! */
	mask |= GCTile | GCFillStyle;

	values.tile = List_DashTile(w);
	values.fill_style = FillTiled;
#endif
    }

    List_HighlightGC(w) = XtGetGC(w, mask, &values);
}


static void
CreateInsensitiveGC(Widget w)
{
    XGCValues values;
    XtGCMask mask;

    mask = GCForeground | GCBackground | GCFillStyle | GCFunction |
	GCStipple | GCPlaneMask | GCSubwindowMode |
	GCGraphicsExposures | GCTileStipXOrigin | GCTileStipYOrigin;

    values.function = GXcopy;
    values.plane_mask = -1;
    values.subwindow_mode = ClipByChildren;
    values.graphics_exposures = False;
    values.foreground = Prim_Foreground(w);
    values.background = XtBackground(w);
    values.fill_style = FillStippled;
    values.ts_x_origin = values.ts_y_origin = 0;
    values.stipple = XmGetPixmapByDepth(XtScreen(w),
					XmODD_STIPPLE_IMAGE,
					_XmWhitePixelOfObject(w),
					_XmBlackPixelOfObject(w),
					1);

    List_InsensitiveGC(w) = XtGetGC(w, mask, &values);
}


static void
CreateInverseGC(Widget w)
{
    XGCValues values;

    values.foreground = XtBackground(w);
    values.background = Prim_Foreground(w);

    List_InverseGC(w) = XtGetGC(w, GCForeground | GCBackground, &values);
}


static void
CreateScrollBars(Widget new_w)
{
    /* Below this point calls to parent methods, which may invoke callbacks
     * occur, so we have to have everything else sorted by here */
    if (List_IsScrolledList(new_w))
    {
#ifdef USE_INCORRECT_SCROLLBAR_NAME
	char *name = XtMalloc(strlen(XtName(new_w)) + 4);

	strcpy(name, XtName(new_w));
	strcat(name, "VSB");
#else
	char *name = "VertScrollBar"; 
#endif
	List_VSB(new_w) =
	    (XmScrollBarWidget)XtVaCreateWidget(name,
						xmScrollBarWidgetClass,
						XtParent(new_w),
						NULL);

	if (List_SBDisplayPolicy(new_w) == XmSTATIC ||
	    (List_SBDisplayPolicy(new_w) == XmAS_NEEDED &&
	     List_ItemCount(new_w) > List_VisibleItemCount(new_w)))
	{
	    XtManageChild((Widget)List_VSB(new_w));
	}
	else
	{
	    DEBUGOUT(_LtDebug(__FILE__, new_w, "  VSB not managed\n"));
	}

	XtAddCallback((Widget)List_VSB(new_w), XmNdecrementCallback,
		      _XmListVerticalScrollBarCallback,
		      (XtPointer)new_w);
	XtAddCallback((Widget)List_VSB(new_w), XmNdragCallback,
		      _XmListVerticalScrollBarCallback,
		      (XtPointer)new_w);
	XtAddCallback((Widget)List_VSB(new_w), XmNincrementCallback,
		      _XmListVerticalScrollBarCallback,
		      (XtPointer)new_w);
	XtAddCallback((Widget)List_VSB(new_w), XmNpageDecrementCallback,
		      _XmListVerticalScrollBarCallback,
		      (XtPointer)new_w);
	XtAddCallback((Widget)List_VSB(new_w), XmNpageIncrementCallback,
		      _XmListVerticalScrollBarCallback,
		      (XtPointer)new_w);
	XtAddCallback((Widget)List_VSB(new_w), XmNtoBottomCallback,
		      _XmListVerticalScrollBarCallback,
		      (XtPointer)new_w);
	XtAddCallback((Widget)List_VSB(new_w), XmNtoTopCallback,
		      _XmListVerticalScrollBarCallback,
		      (XtPointer)new_w);
	XtAddCallback((Widget)List_VSB(new_w), XmNvalueChangedCallback,
		      _XmListVerticalScrollBarCallback,
		      (XtPointer)new_w);

#ifdef USE_INCORRECT_SCROLLBAR_NAME
	strcpy(name, XtName(new_w));
	strcat(name, "HSB");
#else
	name = "HorScrollBar"; 
#endif
	List_HSB(new_w) =
	    (XmScrollBarWidget)XtVaCreateWidget(name,
						xmScrollBarWidgetClass,
						(Widget)List_Mom(new_w),
						XmNorientation, XmHORIZONTAL,
						NULL);

	XtAddCallback((Widget)List_HSB(new_w), XmNdecrementCallback,
		      _XmListHorizontalScrollBarCallback,
		      (XtPointer)new_w);
	XtAddCallback((Widget)List_HSB(new_w), XmNdragCallback,
		      _XmListHorizontalScrollBarCallback,
		      (XtPointer)new_w);
	XtAddCallback((Widget)List_HSB(new_w), XmNincrementCallback,
		      _XmListHorizontalScrollBarCallback,
		      (XtPointer)new_w);
	XtAddCallback((Widget)List_HSB(new_w), XmNpageDecrementCallback,
		      _XmListHorizontalScrollBarCallback,
		      (XtPointer)new_w);
	XtAddCallback((Widget)List_HSB(new_w), XmNpageIncrementCallback,
		      _XmListHorizontalScrollBarCallback,
		      (XtPointer)new_w);
	XtAddCallback((Widget)List_HSB(new_w), XmNtoBottomCallback,
		      _XmListHorizontalScrollBarCallback,
		      (XtPointer)new_w);
	XtAddCallback((Widget)List_HSB(new_w), XmNtoTopCallback,
		      _XmListHorizontalScrollBarCallback,
		      (XtPointer)new_w);
	XtAddCallback((Widget)List_HSB(new_w), XmNvalueChangedCallback,
		      _XmListHorizontalScrollBarCallback,
		      (XtPointer)new_w);

	XmScrolledWindowSetAreas((Widget)List_Mom(new_w),
				 (Widget)List_HSB(new_w),
				 (Widget)List_VSB(new_w), new_w);

	/*
	 * XtVaSetValues((Widget)List_Mom(new_w),
	 * XmNscrollingPolicy, XmAPPLICATION_DEFINED,
	 * XmNvisualPolicy, XmVARIABLE,
	 * NULL);
	 */

	if (XtIsManaged(List_VSB(new_w)))
	{
	    DEBUGOUT(_LtDebug(__FILE__, new_w, "  VSB is now managed\n"));
	}

	_XmListInitScrollBars(new_w, True, True);

#ifdef USE_INCORRECT_SCROLLBAR_NAME
	XtFree(name);
#endif
    }
}


static void
initialize(Widget request, Widget new_w,
	   ArgList args, Cardinal *num_args)
{
	int i;
    	XmStringTable new_selected_items = NULL;
	int		new_selected_item_count = 0;

	XtAugmentTranslations(new_w, list_trans);

	DEBUGOUT(_LtDebug(__FILE__, new_w, "initialize() Visible: %d\n",
		List_VisibleItemCount(new_w)));
	DEBUGOUT(_LtDebug(__FILE__, new_w,
		"initialize: %i args Visible: %d\n"
		"\trequest X %5i Y %5i W %5i H %5i\n"
		"\t  new_w X %5i Y %5i W %5i H %5i\n",
		*num_args, List_VisibleItemCount(new_w),
		XtX(request), XtY(request),
		XtWidth(request), XtHeight(request),
		XtX(new_w), XtY(new_w),
		XtWidth(new_w), XtHeight(new_w)));
	DEBUGOUT(_LtDebugPrintArgList(__FILE__, new_w, args, *num_args, False));
	DEBUGOUT(_LtDebug(__FILE__, new_w, "Selected Item Count = %d\n",
		List_SelectedItemCount(new_w)));
	DEBUGOUT(_LtDebug(__FILE__, new_w, "Shadow Thickness = %d\n",
		Prim_ShadowThickness(new_w)));
	
	/* rws 24 Nov 1998
	 *
	 * list/test20 indicates that setting resizePolicy does the same thing
	 * as what listSizePolicy is supposed to do.
	 */
#if 0
	/*
	 * Two names for the same resource means we need some tricks to avoid
	 * being assigned a value for renderTable first and then initialised
	 * to NULL for fontList later.
	 */
	for (i=0; i<*num_args; i++) {
#if (XmVERSION > 1)
		if (strcmp(args[i].name, XmNrenderTable) == 0) {
			List_Font(new_w) = XmFontListCopy((XmFontList)args[i].value);
			break;
		} else
#endif
		if (strcmp(args[i].name, XmNfontList) == 0) {
			List_Font(new_w) = XmFontListCopy((XmFontList)args[i].value);
		}
	}
#endif
	if (List_Font(new_w) == NULL || List_Font(new_w) == (XmFontList)XmUNSPECIFIED) {
		/* Use the trait to find a default renderTable */
		/* FIX ME currently same as 1.2 */
		List_Font(new_w) = _XmGetDefaultFontList(new_w, XmTEXT_FONTLIST);
	}
	else {
		List_Font(new_w) = XmFontListCopy(List_Font(new_w));
		}


#if (XmVERSION > 1)
	/* Make the rendertable suitable */
	for (i=0; i<List_ItemCount(new_w); i++)
		_XmRenderTableFinalise(new_w, List_Font(new_w),
			List_Items(new_w)[i]);
#endif
	List_AddMode(new_w) = False;

	if (List_ItemCount(new_w) == XmUNSPECIFIED) {
		List_ItemCount(new_w) = 1;
	}

	List_StartItem(new_w) = 0;
	List_LastItem(new_w) = 0;
	List_LastHLItem(new_w) = 0;
	List_DragID(new_w) = (XtIntervalId)NULL;

	if (List_VisibleItemCount(new_w) == 0) {
	/*
	 * REMARK:
	 * See: scrolledwindow/test14 & list/test9
	 * If visible item count is not specified then set to 1..
	 * also set LastViz to the before change value.  6/17/98 pgw      
	 * END:
	 */
		List_LastSetVizCount(new_w) = List_VisibleItemCount(new_w);
		List_VisibleItemCount(new_w) = 1;
	}

	/* So InstallItems doesn't try to do any scrollbar things */
	List_Mom(new_w) = NULL;
	List_FromSetSB(new_w) = False;
	List_FromSetNewSize(new_w) = False;

	/* Save SelectedItems and SelectedItemCount, because InstallItems will
	 * clear it.
	 */
	if (List_SelectedItemCount(new_w) > 0) {
		new_selected_item_count = List_SelectedItemCount(new_w);
		/* T. Straumann: no need to copy the whole string table */
		new_selected_items = List_SelectedItems(new_w);
		List_SelectedItems(new_w) = NULL;
		/* InstallItems will create a new list */
	}

	/* Initialise Items, InternalList, SelectedItems, SelectedIndices,
	 * SelectedItemCount, MaxWidth, MaxItemHeight */
	/* T. Straumann: to avoid memory leaks the SelectedIndices is 
	 *				 always reused and hence must be initialized to NULL
	 */
	List_SelectedIndices(new_w) = NULL;
	_XmListInstallItems(new_w);

	if (new_selected_item_count > 0) {
		/* T. Straumann: let XmListSelectItem do this ...
		 * List_SelectedItemCount(new_w) = new_selected_item_count;
		 */

		for (i = 0; i < new_selected_item_count; i++) {
		/* T. Straumann: no copy; XmListSelect will let List_SelectedItems(new_w)[i]
		 *				 point to the correct string.
	    List_SelectedItems(new_w)[i] = XmStringCopy(new_selected_items[i]);    
		 */
			XmListSelectItem(new_w, new_selected_items[i], False);
		}
	} else {
		DEBUGOUT(_LtDebug(__FILE__, new_w, "No items selected\n"));
	}

	if (List_TopPosition(new_w) < 1 ||
			List_TopPosition(new_w) > List_ItemCount(new_w)) {
		List_TopPosition(new_w) = 1;
	}

	List_LastHLItem(new_w) = List_TopPosition(new_w);

    List_XOrigin(new_w) = 0;	/* initialize x coordinate */
    List_Horigin(new_w) = 0;
    List_Hmin(new_w) = 0;
    List_Vorigin(new_w) = 0;
    List_Vmin(new_w) = 0;

    CreateNormalGC(new_w);
    CreateInsensitiveGC(new_w);
    CreateInverseGC(new_w);
    CreateHighlightGC(new_w);

    _XmListCalculateInitialGeometry(new_w, request);

    if (XmIsScrolledWindow(XtParent(new_w)))
    {
	List_Mom(new_w) = (XmScrolledWindowWidget)XtParent(new_w);
    }
    else
    {
	List_Mom(new_w) = NULL;
    }

    CreateScrollBars(new_w);

    /* This is needed to initialize double-click detection */
    List_DownCount(new_w) = 0;
    List_DownTime(new_w) = 0;

    if (List_ClickInterval(new_w) == XmUNSPECIFIED)
    {
	List_ClickInterval(new_w) = XtGetMultiClickTime(XtDisplay(new_w));
    }

    /*
     * I'd like to have XmListCalculateInitialGeometry() here
     */
}


static Boolean
set_values(Widget old, Widget request, Widget new_w, ArgList args, Cardinal *num_args)
{
    Boolean need_refresh = False, need_newgeo = False;
    int i;
    XmStringTable new_selected_items;
    int new_selected_item_count;

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

    if (List_SizePolicy(old) != List_SizePolicy(new_w)) {
	_XmWarning(new_w, "Cannot change XmNlistSizePolicy after initialization.\n");

	List_SizePolicy(new_w) = List_SizePolicy(old);
    }

    if (XtIsSensitive(old) != XtIsSensitive(new_w)) {
	need_refresh = True;
    }

    if (XtBackground(old) != XtBackground(new_w) ||
        Prim_Foreground(old) != Prim_Foreground(new_w) ||
        Prim_HighlightColor(old) != Prim_HighlightColor(new_w))
    {
	XtReleaseGC(new_w, List_InverseGC(new_w));
	XtReleaseGC(new_w, List_InsensitiveGC(new_w));
	XtReleaseGC(new_w, List_HighlightGC(new_w));
	XtReleaseGC(new_w, List_NormalGC(new_w));
	CreateInverseGC(new_w);
	CreateInsensitiveGC(new_w);
	CreateHighlightGC(new_w);
	CreateNormalGC(new_w);
	need_refresh = True;
    }

    if (List_Font(old) != List_Font(new_w))
    {
	XmFontListFree(List_Font(old));

	List_Font(new_w) = XmFontListCopy(List_Font(request));

	need_newgeo = True;
	need_refresh = True;
    }

    /* The list of selected items needs to be saved here, because
     * InstallItems will trample on it */
    new_selected_items = List_SelectedItems(new_w);
	/* T. Straumann: don't use the user supplied list, but reuse the
	 *				 old (internal) one
	 */
	if (List_SelectedItems(old) != List_SelectedItems(new_w))
	{
		List_SelectedItems(new_w) = List_SelectedItems(old);
		_XmListReallocSelectedItems(new_w);
	}
    new_selected_item_count = List_SelectedItemCount(new_w);

    if (List_ItemCount(old) != List_ItemCount(new_w) &&
	List_Items(old) == List_Items(new_w))
    {
	/*
	 * ItemCount changed, but Items didn't, so force ItemCount
	 * back to original value.
	 */
	List_ItemCount(new_w) = List_ItemCount(old);

	_XmWarning(new_w,
	   "XmNitemCount and XmNitems must be set by the same XtSetValues.\n");
    }
    else if (List_Items(old) != List_Items(new_w))
    {
	/*
	 * Items changed, and we assume ItemCount changed as well.
	 */

	_XmListInstallItems(new_w);

	/* T. Straumann: try this one... */
	_XmListFreeItems(old);

	need_newgeo = True;
	need_refresh = True;
    }

    if (List_MarginHeight(old) != List_MarginHeight(new_w) ||
	List_MarginWidth(old) != List_MarginWidth(new_w) ||
	List_ItemSpacing(old) != List_ItemSpacing(new_w) ||
	List_SBDisplayPolicy(old) != List_SBDisplayPolicy(new_w) ||
	List_StrDir(old) != List_StrDir(new_w))
    {
	need_newgeo = True;
	need_refresh = True;
    }

    if (List_VisibleItemCount(old) != List_VisibleItemCount(new_w))
    {
	if (List_VisibleItemCount(new_w) == 0)
	{
	    List_VisibleItemCount(new_w) = 1;
	}

	DEBUGOUT(_LtDebug(__FILE__, new_w,
			  "VisibleItemCount now %d\n",
			  List_VisibleItemCount(new_w)));

	need_newgeo = True;
	need_refresh = True;
    }

    /*
     * We need to make sure that TopPosition is a valid value.
     * This needs to be done whether or not the value has changed. 
     * Valid values are 1..(ItemCount - VisibleItemCount + 1).
     *
     */

    if (List_TopPosition(new_w) < 1)
    {
	/*
	 * Show the start of the list.
	 * M*tif doesn't display a warning here, so we won't either.
	 */
	DEBUGOUT(_LtDebug(__FILE__, new_w,
		"set_values: topPosition(%d) < 1, changed to 1.\n",
		List_TopPosition(new_w)));
	
	List_TopPosition(new_w) = 1;
	need_newgeo = True;
	need_refresh = True;
    }
    else if (List_TopPosition(new_w) > List_ItemCount(new_w) - List_VisibleItemCount(new_w) + 1)
    {
	/*
	 * Move TopPosition to display the end of the list.
	 * M*tif doesn't display a warning here, so we won't either.
	 */
	DEBUGOUT(_LtDebug(__FILE__, new_w,
		"set_values: topPosition(%d) too high, changed to %d.\n",
		List_TopPosition(new_w), 
		List_ItemCount(new_w) - List_VisibleItemCount(new_w) + 1));

	List_TopPosition(new_w) = 
	     List_ItemCount(new_w) - List_VisibleItemCount(new_w) + 1 < 1 ? 1 : List_ItemCount(new_w) - List_VisibleItemCount(new_w) + 1;
	need_newgeo = True;
	need_refresh = True;

    }
    else if (List_TopPosition(old) != List_TopPosition(new_w)) 
    {
    	need_newgeo = True;
	need_refresh = True;
	_XmListUpdateVertScrollBar(new_w);
    }

    if (List_SelectedItemCount(old) != new_selected_item_count &&
	List_SelectedItems(old) == new_selected_items)
    {
	if (new_selected_item_count == 0)
	{
	    XmListDeselectAllItems(new_w);
	}
	else
	{
	    List_SelectedItemCount(new_w) = List_SelectedItemCount(old);
	    _XmWarning(new_w,
		   "XmNselectedItemCount and XmNselectedItems must be "
		   "set by the same XtSetValues.\n");
	}
    }
	/* T. Straumann: compare to new_selected_items; new_w's field has possibly
	 *				 been realloced!
	 */
    else if (List_SelectedItems(old) != new_selected_items)
    {
	if (List_SelectionPolicy(new_w) == XmBROWSE_SELECT ||
	    List_SelectionPolicy(new_w) == XmSINGLE_SELECT)
	{
	    for (i = 0;
		 i < new_selected_item_count;
/*
   amai: Outcommenting this fixes test/Xm/list/test23!
             && List_SelectedItemCount(new_w) == 0;
*/
		 i++)
	    {
		/* T. Straumann: note that new_w is using the old list of
		 *				 selected items.
		 */

		if (new_selected_items[i])
		     XmListSelectItem(new_w, new_selected_items[i], False);
		else
		    _XmWarning(new_w, "Trying to select a NULL item\n");
	    }
	}
	else
	{
	    List_SelectedItemCount(new_w) = 0;
	    _XmListDeselectAll(new_w);

	    for (i = 0; i < new_selected_item_count; i++)
	    {
		XmListSelectItem(new_w, new_selected_items[i], False);
	    }

	    need_refresh = True;
	}
    }

    if (List_SelectionPolicy(old) != List_SelectionPolicy(new_w))
    {
	switch (List_SelectionPolicy(new_w))
	{
	case XmBROWSE_SELECT:
	case XmEXTENDED_SELECT:
	    List_AddMode(new_w) = False;
	    break;

	case XmSINGLE_SELECT:
	case XmMULTIPLE_SELECT:
	    List_AddMode(new_w) = True;
	    break;

	default:
	    _XmWarning(new_w, "Invalid selectionPolicy.\n");
	}

	/* create the new highlight gc */
	XtReleaseGC(new_w, List_HighlightGC(new_w));
	CreateHighlightGC(new_w);
    }

    if (need_newgeo)
    {
	_XmListSetGeometry(new_w);
    }

    return need_refresh;
} /* set_values() */



/************/
/* Actions */
/**********/
static void
ListAddMode(Widget w, XEvent *event, String *params, Cardinal *num_params)
{
    DEBUGOUT(_LtDebug(__FILE__, w, "ListAddMode action\n"));

    if (List_SelectionPolicy(w) == XmEXTENDED_SELECT)
    {
	List_AddMode(w) = !List_AddMode(w);
    }
}


static void
ListBeginData(Widget w, XEvent *event, String *params, Cardinal *num_params)
{
    Boolean redraw_all = False;
    DEBUGOUT(_LtDebug(__FILE__, w, "ListBeginData action\n"));

    List_TopPosition(w) = 1;

    if (!List_AddMode(w))
    {
	_XmListDeselectAll(w);

	_XmListSetTopPos(w, 1, &redraw_all);

	_XmListSelectPos(w, 1);

	_XmListRedraw(w, redraw_all);

	/* send out callbacks */
	List_SelectionType(w) = XmINITIAL;

	_XmListInvokeCallbacks(w, event, False);
    }
}


static void
ListBeginDataExtend(Widget w, XEvent *event,
		    String *params, Cardinal *num_params)
{
    Boolean redraw_all = False;

    DEBUGOUT(_LtDebug(__FILE__, w, "ListBeginDataExtend action\n"));

    if (List_ItemCount(w) == 0)
    {
	return;
    }

    if (List_SelectionPolicy(w) == XmMULTIPLE_SELECT ||
	List_SelectionPolicy(w) == XmEXTENDED_SELECT)
    {
	if (List_SelectionPolicy(w) == XmEXTENDED_SELECT)
	{
	    _XmListSetSelectRange(w, 1);
	}

	_XmListSetTopPos(w, 1, &redraw_all);
	_XmListSetCursorPos(w, 1);
	_XmListRedraw(w, redraw_all);

	if (List_SelectionPolicy(w) == XmEXTENDED_SELECT)
	{
	    _XmListInvokeCallbacks(w, event, False);
	}
    }
}


static void
ListBeginExtend(Widget w, XEvent *event, String *params, Cardinal *num_params)
{
    XButtonEvent *bevent = (XButtonEvent *)event;
    int position;

    DEBUGOUT(_LtDebug(__FILE__, w, "ListBeginExtend action\n"));

    if (List_ItemCount(w) == 0)
    {
	return;
    }

    if (List_SelectionPolicy(w) == XmEXTENDED_SELECT)
    {
	XmProcessTraversal(w, XmTRAVERSE_CURRENT);
	position = XmListYToPos(w, bevent->y);

	if (List_StartItem(w))
	{
	    _XmListSetSelectRange(w, position);
	    _XmListRedraw(w, False);

	    if (List_AutoSelect(w))
	    {
		List_SelectionType(w) = XmMODIFICATION;

		_XmListInvokeCallbacks(w, event, False);
	    }
	}
    }
}


static void
ListBeginLine(Widget w, XEvent *event, String *params, Cardinal *num_params)
{
    Boolean redraw_all = False;
    DEBUGOUT(_LtDebug(__FILE__, w, "ListBeginList action\n"));

    _XmListUpdateHorizScrollBar(w, 0, &redraw_all);
    if (redraw_all)
    {
	_XmListRedraw(w, True);
    }
}


static void
ListBeginSelect(Widget w, XEvent *event, String *params, Cardinal *num_params)
{
    XButtonEvent *bevent = (XButtonEvent *)event;
    int position;

    DEBUGOUT(_LtDebug(__FILE__, w, "ListBeginSelect action\n"));

    if (List_ItemCount(w) == 0)
    {
	return;
    }

    XmProcessTraversal(w, XmTRAVERSE_CURRENT);
    position = XmListYToPos(w, bevent->y);

    DEBUGOUT(_LtDebug(__FILE__, w, "Selected position %d\n", position));

    switch (List_SelectionPolicy(w))
    {
    case XmBROWSE_SELECT:
	if (List_LastItem(w))
	{
	    /*	
	     * Deselect all, in case the selection policy was changed
	     * from XmMULTIPLE_SELECT or XmEXTENDED_SELECT
	     */
	    _XmListDeselectAll(w);
	    /*_XmListDeselectPos(w, List_LastItem(w));*/
	}
	_XmListSelectPos(w, position);
	break;

    case XmSINGLE_SELECT:
	{
	int old_pos = List_LastItem(w);

	    if (List_LastItem(w))
	    {
		/*	
		 * Deselect all, in case the selection policy was changed
		 * from XmMULTIPLE_SELECT or XmEXTENDED_SELECT
		 */
		_XmListDeselectAll(w);
		/*_XmListDeselectPos(w, List_LastItem(w));*/
	    }
	    if (old_pos != position)
	    {
		_XmListSelectPos(w, position);
	    }
	}
	break;

    case XmMULTIPLE_SELECT:
	_XmListTogglePos(w, position);
	List_LastItem(w) = position;
	break;

    case XmEXTENDED_SELECT:
	_XmListDeselectAll(w);
	_XmListSelectPos(w, position);
	List_StartItem(w) = position;
	break;
    }

    _XmListRedraw(w, False);

    /* Callback only if browse or extended select */
    if ((List_SelectionPolicy(w) == XmBROWSE_SELECT ||
	 List_SelectionPolicy(w) == XmEXTENDED_SELECT) && List_AutoSelect(w))
    {
	_XmListInvokeCallbacks(w, event, False);
    }
}


static void
ListBeginToggle(Widget w, XEvent *event, String *params, Cardinal *num_params)
{
    DEBUGOUT(_LtDebug(__FILE__, w, "ListBeginToggle()\n"));

    if (List_ItemCount(w) == 0)
    {
	return;
    }

    if (List_SelectionPolicy(w) == XmEXTENDED_SELECT)
    {
	XButtonEvent *bevent = (XButtonEvent *)event;
	int position;

	XmProcessTraversal(w, XmTRAVERSE_CURRENT);
	position = XmListYToPos(w, bevent->y);

	if (List_InternalList(w)[position - 1]->selected)
	{
	    _XmListDeselectPos(w, position);
	}
	else
	{
	    _XmListSelectPos(w, position);
	}

	List_StartItem(w) = position;
	_XmListRedraw(w, False);

	if (List_AutoSelect(w))
	{
	    _XmListInvokeCallbacks(w, event, False);
	}
    }
}

#define XmListDRAG_DOWN 0
#define XmListDRAG_UP 1
#define XmListDRAG_TIMEOUT 125


static void
ListDragToPos(Widget w, XEvent *event, int new_pos, Boolean *redraw_all)
{
    DEBUGOUT(_LtDebug(__FILE__, w, "ListDragToPos() new pos = %d\n", new_pos));

    switch (List_SelectionPolicy(w))
    {
    case XmBROWSE_SELECT:
	{
	    _XmListDeselectPos(w, List_LastItem(w));
	    _XmListSelectPos(w, new_pos);

	    /* if automaticselect is True, call XmNbrowseSelectionCallback */
	    if (event && List_AutoSelect(w))
	    {
		_XmListInvokeCallbacks(w, event, False);
	    }
	}
	break;

    case XmEXTENDED_SELECT:
	{
	    _XmListSetSelectRange(w, new_pos);
	    if (event && List_AutoSelect(w))
		_XmListInvokeCallbacks(w, event, False);
	}
	break;
    }
}


static void
ListDragTimeout(XtPointer closure,
		XtIntervalId *id)
{
    Widget w = (Widget)closure;
    int new_pos;
    Boolean redraw_all;

    DEBUGOUT(_LtDebug(__FILE__, w,
		      "ListDragTimeout() Dragging %s\n",
		      List_LeaveDir(w) == XmListDRAG_DOWN ? "down" : "up"));

    /* If we've dragged as far as we're gettin' */
    if ((List_LeaveDir(w) == XmListDRAG_DOWN &&
	 List_TopPosition(w) + List_VisibleItemCount(w) > List_ItemCount(w)) ||
	(List_LeaveDir(w) == XmListDRAG_UP && List_TopPosition(w) <= 1))
    {
	/* No more timeouts */
	List_DragID(w) = 0;
    }
    else
    {
	if (List_LeaveDir(w) == XmListDRAG_DOWN)
	{
	    new_pos = List_TopPosition(w) + List_VisibleItemCount(w);
	}
	else
	{
	    new_pos = List_TopPosition(w) - 1;
	}

	/* FIX ME - Where should we get the event causing the callback from? */
	ListDragToPos(w, NULL, new_pos, &redraw_all);
	_XmListSetTopPos(w, (List_TopPosition(w) +
			     (List_LeaveDir(w) == XmListDRAG_DOWN ? 1 : -1)),
			 &redraw_all);

	_XmListRedraw(w, redraw_all);
	List_DragID(w) = XtAppAddTimeOut(XtWidgetToApplicationContext(w),
					 XmListDRAG_TIMEOUT, ListDragTimeout,
					 (XtPointer)w);
    }
}


static void
ListButtonMotion(Widget w, XEvent *event, String *params, Cardinal *num_params)
{
    XButtonEvent *bevent = (XButtonEvent *)event;
    int new_pos;
    Boolean direction, redraw_all = False;

    DEBUGOUT(_LtDebug(__FILE__, w, "ListButtonMotion() action\n"));

    if (List_ItemCount(w) == 0)
    {
	return;
    }

    if (List_SelectionPolicy(w) == XmBROWSE_SELECT ||
	List_SelectionPolicy(w) == XmEXTENDED_SELECT)
    {

	int itemHeight = List_MaxItemHeight(w) +
	Prim_HighlightThickness(w) + 1 +
	List_ItemSpacing(w);

	/* If the pointer is below or above the list area */
	if ((direction = (int)event->xbutton.y >
	     (int)(Prim_ShadowThickness(w) + List_MarginHeight(w) +
		   itemHeight * List_VisibleItemCount(w))) ||
	    ((int)event->xbutton.y <
	     (int)(Prim_ShadowThickness(w) + List_MarginHeight(w))))
	{
	    /* Add timeout */
	    if (!List_DragID(w))
	    {
		List_LeaveDir(w) = direction ? XmListDRAG_DOWN : XmListDRAG_UP;
		List_DragID(w) =
		    XtAppAddTimeOut(XtWidgetToApplicationContext(w),
				    XmListDRAG_TIMEOUT, ListDragTimeout,
				    (XtPointer)w);
	    }
	}
	else
	{
	    if (List_DragID(w))
	    {
		/* Remove timeout */
		XtRemoveTimeOut(List_DragID(w));
		List_DragID(w) = 0;
	    }
	    new_pos = XmListYToPos(w, bevent->y);
	    if (List_LastItem(w) != new_pos)
		ListDragToPos(w, event, new_pos, &redraw_all);
	    _XmListRedraw(w, redraw_all);
	}
    }
}


static void
ListCopyToClipboard(Widget w, XEvent *event,
		    String *params, Cardinal *num_params)
{
    DEBUGOUT(_LtDebug(__FILE__, w, "ListCopyToClipboard()\n"));

    if (List_ItemCount(w) == 0 || List_SelectedItemCount(w) == 0)
    {
	return;
    }

    /* FIX ME */
}


static void
ListDefaultAction(Widget w, XEvent *event, String *params, Cardinal *num_params)
{
    DEBUGOUT(_LtDebug(__FILE__, w, "ListDefaultAction()\n"));

    _XmListInvokeCallbacks(w, event, True);
    {
    XmParentProcessDataRec data;

	data.input_action.process_type = XmINPUT_ACTION;
	data.input_action.event = event;
	data.input_action.action = XmPARENT_ACTIVATE;
	data.input_action.params = params;
	data.input_action.num_params = num_params;

	if (XmIsManager(XtParent(w)) && 
	(((XmManagerWidgetClass)(XtClass(XtParent(w))))->manager_class.parent_process) != NULL)
	{
	    (((XmManagerWidgetClass)(XtClass(XtParent(w))))->manager_class.parent_process)
		(XtParent(w), &data);
	}
    }
}


static void
ListEndData(Widget w, XEvent *event, String *params, Cardinal *num_params)
{
    Boolean redraw_all = False;
    int top_pos;
    DEBUGOUT(_LtDebug(__FILE__, w, "ListEndData()\n"));

    if (List_ItemCount(w) == 0)
    {
	return;
    }

    top_pos = List_ItemCount(w) - List_VisibleItemCount(w) + 1;
    if (top_pos < 1)
    {
	top_pos = 1;
    }

    _XmListSetTopPos(w, top_pos, &redraw_all);
    _XmListSetCursorPos(w, List_ItemCount(w));

    if (!List_AddMode(w))
    {
	_XmListDeselectAll(w);
	_XmListSelectPos(w, List_ItemCount(w));
	_XmListInvokeCallbacks(w, event, False);
    }

    _XmListRedraw(w, redraw_all);
}


static void
ListEndDataExtend(Widget w, XEvent *event, String *params, Cardinal *num_params)
{
    Boolean redraw_all = False;
    int top_pos;

    DEBUGOUT(_LtDebug(__FILE__, w, "ListEndDataExtend()\n"));

    if (List_ItemCount(w) == 0)
    {
	return;
    }

    if (List_SelectionPolicy(w) == XmMULTIPLE_SELECT ||
	List_SelectionPolicy(w) == XmEXTENDED_SELECT)
    {
	top_pos = List_ItemCount(w) - List_VisibleItemCount(w) + 1;
	if (top_pos < 1)
	    top_pos = 1;

	_XmListSetTopPos(w, top_pos, &redraw_all);
	_XmListSetCursorPos(w, List_ItemCount(w));

	if (List_SelectionPolicy(w) == XmEXTENDED_SELECT)
	{
	    _XmListSetSelectRange(w, List_ItemCount(w));

	    if (List_AutoSelect(w))
	    {
		_XmListInvokeCallbacks(w, event, False);
	    }
	}

	_XmListRedraw(w, redraw_all);
    }
}


static void
ListEndExtend(Widget w, XEvent *event, String *params, Cardinal *num_params)
{
    int new_top;
    Boolean redraw_all;
    DEBUGOUT(_LtDebug(__FILE__, w, "ListEndExtend()\n"));

    if (List_ItemCount(w) == 0)
    {
	return;
    }

    if (List_SelectionPolicy(w) == XmEXTENDED_SELECT)
    {
	new_top = List_LastItem(w) - List_VisibleItemCount(w) + 1;

	if (new_top < 1)
	{
	    new_top = 1;
	}

	_XmListSetTopPos(w, new_top, &redraw_all);
	_XmListSetCursorPos(w, List_LastItem(w));
	_XmListInvokeCallbacks(w, event, False);
	_XmListRedraw(w, redraw_all);
    }
}


static void
ListEndLine(Widget w, XEvent *event, String *params, Cardinal *num_params)
{
    Boolean redraw_all = False;

    DEBUGOUT(_LtDebug(__FILE__, w, "ListEndLine()\n"));

    _XmListUpdateHorizScrollBar(w, List_Hmax(w), &redraw_all);

    if (redraw_all)
    {
	_XmListRedraw(w, True);
    }
}


/*
 * There must be a technical reason why (no time to dive into that), but
 * putting double-click in the translation table seems not to work. Too
 * complicated I guess.
 * Anyway, the consequence is we have to measure time to detect double-click
 * ourselves in here.
 * Motif has DownTime and DownCount fields in its structure so it must also be
 * doing this...
 */
static void
ListEndSelect(Widget w,
	      XEvent *event,
	      String *params,
	      Cardinal *num_params)
{
    XButtonEvent *bevent = (XButtonEvent *)event;
    Boolean redraw_all = False;
    DEBUGOUT(_LtDebug(__FILE__, w, "ListEndSelect() action\n"));

    if (List_ItemCount(w) == 0)
    {
	return;
    }

    /* Try to find double-click */
    if (List_DownCount(w) > 0 &&
	bevent->time < List_DownTime(w) + List_ClickInterval(w))
    {
	List_DownCount(w)++;

	ListDefaultAction(w, event, params, num_params);

	List_DownCount(w) = 0;

	return;			/* needed ? FIX ME */
    }
    else
    {
	List_DownTime(w) = bevent->time;
	List_DownCount(w) = 1;
    }

    if (Prim_Highlighted(w) /*&& List_LastHLItem(w) != 0*/)
    {
	_XmListUnhighlight(w);
    }

    if (List_DragID(w))
    {
	XtRemoveTimeOut(List_DragID(w));
	List_DragID(w) = 0;
    }

    if (List_SelectionPolicy(w) == XmBROWSE_SELECT ||
	List_SelectionPolicy(w) == XmEXTENDED_SELECT)
    {
	List_LastHLItem(w) = XmListYToPos(w, bevent->y);
    }
    else
    {
	List_LastHLItem(w) = XmListYToPos(w, bevent->y);
    }

    if (List_LastItem(w))
    {
	_XmListSetCursorPos(w, List_LastItem(w));
    }

    _XmListRedraw(w, redraw_all);

    if ((List_SelectionPolicy(w) == XmSINGLE_SELECT ||
	 List_SelectionPolicy(w) == XmMULTIPLE_SELECT) ||
	((List_SelectionPolicy(w) == XmBROWSE_SELECT ||
	  List_SelectionPolicy(w) == XmEXTENDED_SELECT) &&
	 List_AutoSelect(w) == False))
    {
	_XmListInvokeCallbacks(w, event, False);
    }
}


static void
ListEndToggle(Widget w, XEvent *event, String *params, Cardinal *num_params)
{
    Boolean redraw_all = False;

    DEBUGOUT(_LtDebug(__FILE__, w,
		      "ListEndToggle() last_item = %d\n", List_LastItem(w)));

    if (List_ItemCount(w) == 0)
    {
	return;
    }

    if (List_SelectionPolicy(w) == XmEXTENDED_SELECT)
    {
#if 0
	/* Michel Bardiaux suggests this is undesired */
	int new_top;
	new_top = List_LastItem(w) - List_VisibleItemCount(w) + 1;

	if (new_top < 1)
	{
	    new_top = 1;
	}

	_XmListSetTopPos(w, new_top, &redraw_all);
#endif
	_XmListSetCursorPos(w, List_LastItem(w));

	if (!List_AutoSelect(w))
	{
	    _XmListInvokeCallbacks(w, event, False);
	}

	_XmListRedraw(w, redraw_all);
    }
}


static void
ListExtendNextItem(Widget w, XEvent *event,
		   String *params, Cardinal *num_params)
{
    Boolean redraw_all = False;

    DEBUGOUT(_LtDebug(__FILE__, w, "ListExtendNextItem()\n"));

    if (List_ItemCount(w) == 0 || List_LastHLItem(w) >= List_ItemCount(w))
    {
	return;
    }

    if (List_SelectionPolicy(w) == XmEXTENDED_SELECT)
    {
	if (List_StartItem(w) == 0)
	{
	    List_StartItem(w) = List_LastHLItem(w);
	}

	_XmListSetCursorPos(w, List_LastHLItem(w) + 1);

	if (List_LastHLItem(w) > (List_TopPosition(w) +
				  List_VisibleItemCount(w) - 1))
	{
	    _XmListSetTopPos(w, List_TopPosition(w) + 1, &redraw_all);
	}

	_XmListSetSelectRange(w, List_LastHLItem(w));
	_XmListRedraw(w, redraw_all);

	_XmListInvokeCallbacks(w, event, False);
    }
}


static void
ListExtendPrevItem(Widget w, XEvent *event,
		   String *params, Cardinal *num_params)
{
    Boolean redraw_all = False;

    DEBUGOUT(_LtDebug(__FILE__, w, "ListExtendPrevItem()\n"));

    if (List_ItemCount(w) == 0 || List_LastHLItem(w) <= 1)
    {
	return;
    }

    if (List_SelectionPolicy(w) == XmEXTENDED_SELECT)
    {
	if (List_StartItem(w) == 0)
	{
	    List_StartItem(w) = List_LastHLItem(w);
	}
	_XmListSetCursorPos(w, List_LastHLItem(w) - 1);

	if (List_LastHLItem(w) < List_TopPosition(w))
	{
	    _XmListSetTopPos(w, List_TopPosition(w) - 1, &redraw_all);
	}

	_XmListSetSelectRange(w, List_LastHLItem(w));
	_XmListRedraw(w, redraw_all);

	_XmListInvokeCallbacks(w, event, False);
    }
}


static void
ListFocusIn(Widget w, XEvent *event, String *params, Cardinal *num_params)
{
    DEBUGOUT(_LtDebug(__FILE__, w, "ListFocusIn() action\n"));

    XtCallActionProc(w, "PrimitiveFocusIn", event, params, *num_params);
}


static void
ListFocusOut(Widget w, XEvent *event, String *params, Cardinal *num_params)
{
    DEBUGOUT(_LtDebug(__FILE__, w, "ListFocusOut() action\n"));

    XtCallActionProc(w, "PrimitiveFocusOut", event, params, *num_params);
}


static void
ListKbdActivate(Widget w, XEvent *event, String *params, Cardinal *num_params)
{
    DEBUGOUT(_LtDebug(__FILE__, w, "ListKbdActivate() action\n"));

    if (List_ItemCount(w) == 0)
    {
	return;
    }

    ListDefaultAction(w, event, params, num_params);
    /*
    _XmListInvokeCallbacks(w, event, True);
    {
    XmParentProcessDataRec data;

	data.input_action.process_type = XmINPUT_ACTION;
	data.input_action.event = event;
	data.input_action.action = XmPARENT_ACTIVATE;
	data.input_action.params = params;
	data.input_action.num_params = num_params;

	if (XmIsManager(XtParent(w)) && 
	(((XmManagerWidgetClass)(XtClass(XtParent(w))))->manager_class.parent_process) != NULL)
	{
	    (((XmManagerWidgetClass)(XtClass(XtParent(w))))->manager_class.parent_process)(w, &data);
	}
    }
    */
}


static void
ListKbdBeginExtend(Widget w, XEvent *event, String *params, Cardinal *num_params)
{
    DEBUGOUT(_LtDebug(__FILE__, w, "ListKbdBeginExtend()\n"));

    if (List_ItemCount(w) == 0)
    {
	return;
    }

    _XmListSetSelectRange(w, List_LastHLItem(w));
    _XmListRedraw(w, False);
}


static void
ListKbdBeginSelect(Widget w, XEvent *event,
		   String *params, Cardinal *num_params)
{
    DEBUGOUT(_LtDebug(__FILE__, w, "ListKbdBeginSelect()\n"));

    if (List_ItemCount(w) == 0)
    {
	return;
    }

    switch (List_SelectionPolicy(w))
    {
    case XmSINGLE_SELECT:
	if (List_LastItem(w) != List_LastHLItem(w))
	{
	    if (List_LastItem(w))
	    {
		_XmListDeselectPos(w, List_LastItem(w));
	    }

	    _XmListSelectPos(w, List_LastHLItem(w));
	}
	break;

    case XmBROWSE_SELECT:
	if (List_LastItem(w))
	{
	    _XmListDeselectPos(w, List_LastItem(w));
	}

	_XmListSelectPos(w, List_LastHLItem(w));
	break;

    case XmMULTIPLE_SELECT:
	_XmListTogglePos(w, List_LastHLItem(w));
	break;

    case XmEXTENDED_SELECT:
	List_StartItem(w) = List_LastHLItem(w);
	if (List_AddMode(w))
	{
	    _XmListTogglePos(w, List_LastHLItem(w));
	}
	else
	{
	    _XmListDeselectAll(w);
	    _XmListSelectPos(w, List_LastHLItem(w));
	}

	if (List_AutoSelect(w))
	{
	    _XmListInvokeCallbacks(w, event, False);
	}

	break;
    }

    _XmListRedraw(w, False);
}


static void
ListKbdCancel(Widget w, XEvent *event, String *params, Cardinal *num_params)
{
    DEBUGOUT(_LtDebug(__FILE__, w, "ListKbdCancel()\n"));

    if (List_ItemCount(w) == 0)
    {
	return;
    }

    if (List_SelectionPolicy(w) == XmEXTENDED_SELECT)
    {
	_XmListRestoreSelectRange(w);

	List_StartItem(w) = 0;

	_XmListRedraw(w, False);
    }
    {
    XmParentProcessDataRec data;

	data.input_action.process_type = XmINPUT_ACTION;
	data.input_action.event = event;
	data.input_action.action = XmPARENT_CANCEL;
	data.input_action.params = params;
	data.input_action.num_params = num_params;

	if (XmIsManager(XtParent(w)) && 
	(((XmManagerWidgetClass)(XtClass(XtParent(w))))->manager_class.parent_process) != NULL)
	{
	    (((XmManagerWidgetClass)(XtClass(XtParent(w))))->manager_class.parent_process)(w, &data);
	}
    }
}


static void
ListKbdDeSelectAll(Widget w, XEvent *event,
		   String *params, Cardinal *num_params)
{
    DEBUGOUT(_LtDebug(__FILE__, w, "ListKbdDeSelectAll()\n"));

    if (List_ItemCount(w) == 0 || List_SelectionPolicy(w) == XmBROWSE_SELECT)
    {
	return;
    }

    if (List_SelectionPolicy(w) == XmSINGLE_SELECT ||
	List_SelectionPolicy(w) == XmMULTIPLE_SELECT ||
	(List_SelectionPolicy(w) == XmEXTENDED_SELECT &&
	 List_AddMode(w)))
    {
	_XmListDeselectAll(w);
    }
    else if (List_SelectionPolicy(w) == XmEXTENDED_SELECT)
    {
	/* XXX - we are meant to leave the item at the cursor position alone
	 * here if the shell's XmNkeyboardFocusPolicy is XmEXPLICIT */
	_XmListDeselectAll(w);
    }

    _XmListInvokeCallbacks(w, event, False);
    _XmListRedraw(w, False);
}


static void
ListKbdEndExtend(Widget w, XEvent *event, String *params, Cardinal *num_params)
{
    DEBUGOUT(_LtDebug(__FILE__, w, "ListKbdEndExtend()\n"));

    if (List_ItemCount(w) == 0)
    {
	return;
    }

    if (List_SelectionPolicy(w) == XmEXTENDED_SELECT && !List_AutoSelect(w))
    {
	_XmListInvokeCallbacks(w, event, False);
    }
}


static void
ListKbdEndSelect(Widget w, XEvent *event, String *params, Cardinal *num_params)
{
    DEBUGOUT(_LtDebug(__FILE__, w, "ListKbdEndSelect()\n"));

    if (List_ItemCount(w) == 0)
    {
	return;
    }

    if (List_SelectionPolicy(w) == XmSINGLE_SELECT ||
	List_SelectionPolicy(w) == XmMULTIPLE_SELECT ||
	!List_AutoSelect(w))
    {
	_XmListInvokeCallbacks(w, event, False);
    }
}


static void
ListKbdSelectAll(Widget w, XEvent *event, String *params, Cardinal *num_params)
{
    DEBUGOUT(_LtDebug(__FILE__, w, "ListKbdSelectAll()\n"));

    if (List_ItemCount(w) == 0)
    {
	return;
    }

    switch (List_SelectionPolicy(w))
    {
    case XmSINGLE_SELECT:
    case XmBROWSE_SELECT:
	if (List_LastItem(w) != List_LastHLItem(w))
	{
	    if (List_LastItem(w))
	    {
		XmListDeselectPos(w, List_LastItem(w));
	    }

	    _XmListSelectPos(w, List_LastHLItem(w));
	}
	break;

    case XmMULTIPLE_SELECT:
    case XmEXTENDED_SELECT:
	_XmListSelectAll(w);
	break;
    }

    _XmListRedraw(w, False);
    _XmListInvokeCallbacks(w, event, False);
}


static void
ListLeftChar(Widget w, XEvent *event, String *params, Cardinal *num_params)
{
    Boolean redraw_all = False;

    DEBUGOUT(_LtDebug(__FILE__, w, "ListLeftChar()\n"));

    _XmListUpdateHorizScrollBar(w, List_Horigin(w) - List_CharWidth(w),
				&redraw_all);
    if (redraw_all)
    {
	_XmListRedraw(w, True);
    }
}


static void
ListLeftPage(Widget w, XEvent *event, String *params, Cardinal *num_params)
{
    DEBUGOUT(_LtDebug(__FILE__, w, "ListLeftPage()\n"));

/**starving**/
}


static void
ListNextItem(Widget w, XEvent *event, String *params, Cardinal *num_params)
{
    Boolean redraw_all = False;

    DEBUGOUT(_LtDebug(__FILE__, w, "ListNextItem()\n"));

    if (List_ItemCount(w) == 0 || List_LastHLItem(w) == List_ItemCount(w))
    {
	return;
    }

    /* assert(List_LastHLItem(w) > 0 && List_LastHLItem(w) <= List_ItemCount(w)); */

    if (List_LastHLItem(w)+1 > (List_TopPosition(w) +
			      List_VisibleItemCount(w) - 1))
    {
	_XmListSetTopPos(w, List_TopPosition(w) + 1, &redraw_all);
    }
    _XmListSetCursorPos(w, List_LastHLItem(w) + 1);


    if (List_SelectionPolicy(w) == XmBROWSE_SELECT)
    {
	_XmListDeselectPos(w, List_LastHLItem(w) - 1);
	_XmListSelectPos(w, List_LastHLItem(w));
    }

    if (List_SelectionPolicy(w) == XmEXTENDED_SELECT && !List_AddMode(w))
    {
	_XmListDeselectAll(w);
	List_StartItem(w) = List_LastHLItem(w);
	_XmListSelectPos(w, List_LastHLItem(w));
    }
    if (List_SelectionPolicy(w) == XmBROWSE_SELECT ||
    	 List_SelectionPolicy(w) == XmEXTENDED_SELECT)
    	     {
    	     	_XmListInvokeCallbacks(w, event, False);
    	     	    }
    _XmListRedraw(w, redraw_all);
}


static void
ListNextPage(Widget w, XEvent *event, String *params, Cardinal *num_params)
{
    Boolean redraw_all = False;
    int position, new_top;

    DEBUGOUT(_LtDebug(__FILE__, w, "ListNextPage()\n"));

    if (List_ItemCount(w) == 0)
    {
	return;
    }

    /* Move down a page - 1 item.  Move the top of the list the same amount,
     * so the cursor stays at the same point on the screen */
    position = List_LastHLItem(w) + List_VisibleItemCount(w) - 1;
    if (position > List_ItemCount(w))
    {
	position = List_ItemCount(w);
    }

    new_top = List_TopPosition(w) + List_VisibleItemCount(w) - 1;

    if (List_VisibleItemCount(w) == 1)
    {
	position++;
	new_top++;
    }

    if (new_top > List_ItemCount(w) - List_VisibleItemCount(w) + 1)
    {
	new_top = List_ItemCount(w) - List_VisibleItemCount(w) + 1;
	if (new_top < 1)
	{
	    new_top = 1;
	}
    }

    if (List_SelectionPolicy(w) == XmBROWSE_SELECT)
    {
	_XmListDeselectPos(w, List_LastHLItem(w));
	_XmListSelectPos(w, position);
    }

    if (List_SelectionPolicy(w) == XmEXTENDED_SELECT && !List_AddMode(w))
    {
	_XmListDeselectAll(w);
	List_StartItem(w) = position;
	_XmListSelectPos(w, position);
    }

    _XmListSetTopPos(w, new_top, &redraw_all);
    _XmListSetCursorPos(w, position);
    if (List_SelectionPolicy(w) == XmBROWSE_SELECT
		    || List_SelectionPolicy(w) == XmEXTENDED_SELECT) {
	    _XmListInvokeCallbacks(w, event, False);
    }
    _XmListRedraw(w, redraw_all);
}


static void
ListPrevItem(Widget w, XEvent *event, String *params, Cardinal *num_params)
{
    Boolean redraw_all = False;
    DEBUGOUT(_LtDebug(__FILE__, w, "ListPrevItem()\n"));

    if (List_ItemCount(w) == 0 || List_LastHLItem(w) == 1)
    {
	return;
    }

    /* assert(List_LastHLItem(w) > 0 && List_LastHLItem(w) <= List_ItemCount(w)); */

    if (List_LastHLItem(w)-1 < List_TopPosition(w))
    {
	_XmListSetTopPos(w, List_LastHLItem(w)-1, &redraw_all);
    }
    _XmListSetCursorPos(w, List_LastHLItem(w) - 1);

    if (List_SelectionPolicy(w) == XmBROWSE_SELECT)
    {
	_XmListDeselectPos(w, List_LastHLItem(w) + 1);
	_XmListSelectPos(w, List_LastHLItem(w));
    }

    if (List_SelectionPolicy(w) == XmEXTENDED_SELECT && !List_AddMode(w))
    {
	_XmListDeselectAll(w);
	List_StartItem(w) = List_LastHLItem(w);
	_XmListSelectPos(w, List_LastHLItem(w));
    }

    if (List_SelectionPolicy(w) == XmBROWSE_SELECT
		    || List_SelectionPolicy(w) == XmEXTENDED_SELECT) {
	    _XmListInvokeCallbacks(w, event, False);
    }
    _XmListRedraw(w, redraw_all);
}


static void
ListPrevPage(Widget w, XEvent *event, String *params, Cardinal *num_params)
{
    Boolean redraw_all = False;
    int position, new_top;

    DEBUGOUT(_LtDebug(__FILE__, w, "ListPrevPage()\n"));

    if (List_ItemCount(w) == 0 || List_LastHLItem(w) == 1)
    {
	return;
    }

    position = List_LastHLItem(w) - List_VisibleItemCount(w) + 1;
    if (List_VisibleItemCount(w) == 1)
    {
	position--;
    }
    if (position < 1)
    {
	position = 1;
    }

    new_top = List_TopPosition(w) - List_VisibleItemCount(w) + 1;
    if (List_VisibleItemCount(w) == 1)
    {
	new_top--;
    }
    if (new_top < 1)
    {
	new_top = 1;
    }

    if (List_SelectionPolicy(w) == XmBROWSE_SELECT)
    {
	_XmListDeselectPos(w, List_LastHLItem(w));

	_XmListSelectPos(w, position);
    }

    if (List_SelectionPolicy(w) == XmEXTENDED_SELECT && !List_AddMode(w))
    {
	_XmListDeselectAll(w);

	List_StartItem(w) = position;

	_XmListSelectPos(w, position);
    }

    _XmListSetCursorPos(w, position);
    _XmListSetTopPos(w, new_top, &redraw_all);

    if (List_SelectionPolicy(w) == XmBROWSE_SELECT
		    || List_SelectionPolicy(w) == XmEXTENDED_SELECT) {
	    _XmListInvokeCallbacks(w, event, False);
    }
    _XmListRedraw(w, redraw_all);
}


static void
ListProcessDrag(Widget w, XEvent *event, String *params, Cardinal *num_params)
{
    /* CHECK TO SEE IF THE EXPORT TARGET IS REALLY COMPOUND_TEXT -- FIX ME ! */
    int position;
    Atom export_target[3];
    Arg args[10];
    int n = 0;
    Widget dc;

    DEBUGOUT(_LtDebug(__FILE__, w, "ListProcessDrag()\n"));

    /* If the list is empty, they couldn't really have dragged from it,
     * now could they? */
    if (List_ItemCount(w) == 0)
    {
	return;
    }

    /* set up some stuff */
    export_target[0] = XmInternAtom(XtDisplay(w), _XA_COMPOUND_TEXT, False);
    export_target[1] = XmInternAtom(XtDisplay(w), _XA_TEXT, False);
    export_target[2] = XA_STRING;

    XtSetArg(args[n], XmNexportTargets, export_target); n++;
    XtSetArg(args[n], XmNnumExportTargets, 3); n++;
    XtSetArg(args[n], XmNdragOperations, XmDROP_COPY); n++;
    XtSetArg(args[n], XmNclientData, w); n++;

    /* determine the item that they dragged from */
    position = XmListYToPos(w, event->xbutton.y);

    if (position > List_ItemCount(w))
    {
	position = List_ItemCount(w);
    }

    /* now decide whether the item was selected or not.  If it was, we want
       to transfer all the selected items.  If not, we only want to transfer
       the item they dragged from.

       position is +1 since XmListYToPos can return 0 */

    if (XmListPosSelected(w, position /* + 1 */ ))
    {
	/* the position was selected.  drag all the selected positions */
	XtSetArg(args[n], XmNconvertProc, drag_selected_proc); n++;
    }
    else
    {
	/* the position wasn't selected.  drag only the one item */
	XtSetArg(args[n], XmNconvertProc, drag_unselected_proc); n++;
    }
    dc = XmDragStart(w, event, args, n);

    if (dc)
    {
	XtAddCallback(dc, XmNdragDropFinishCallback, drag_drop_finish, NULL);
    }
}


static void
ListRightChar(Widget w, XEvent *event, String *params, Cardinal *num_params)
{
    Boolean redraw_all = False;
    DEBUGOUT(_LtDebug(__FILE__, w, "ListRightChar()\n"));

    _XmListUpdateHorizScrollBar(w, List_Horigin(w) + List_CharWidth(w),
				&redraw_all);
    if (redraw_all)
    {
	_XmListRedraw(w, True);
    }
}


static void
ListRightPage(Widget w, XEvent *event, String *params, Cardinal *num_params)
{
    DEBUGOUT(_LtDebug(__FILE__, w, "ListRightPage()\n"));

    /**starving**/
}


/*
 * This function needs to update List_MaxWidth(w) and List_MaxItemHeight(w)
 * because the new item might affect both.
 */
static void
_XmListReplaceItemPos(Widget w, int _pos, XmString new_item)
{
    Dimension height, width;

    /* Check the range, see bug #906329 */
    if (_pos < 0 || _pos >= List_ItemCount(w)) {
	    _XmWarning(w,
			    "_XmListReplaceItemPos() called with invalid position %d,"
			    "range from 0 to %d\n",
			    _pos, List_ItemCount(w));
	    return;
    }

    /* deallocate the item */
    XmStringFree(List_Items(w)[_pos]);
    _XmStringFree(List_InternalList(w)[_pos]->name);

    /* replace with new_item */
    List_Items(w)[_pos] = XmStringCopy(new_item);
    List_InternalList(w)[_pos]->name = _XmStringCreate(new_item);

    XmStringExtent(List_Font(w), new_item, &width, &height);
    List_InternalList(w)[_pos]->width = width;
    List_InternalList(w)[_pos]->height = height;
    List_InternalList(w)[_pos]->length = XmStringLength(new_item);
    List_InternalList(w)[_pos]->LastTimeDrawn=False; /* Peter Stein 2000/09/04: mark as not drawn */

    /*
     * Recalculate the max width, because it could have changed.
     */
    _XmListRecalcItemSize(w);
}


/*        ****** PUBLIC INTERFACES ******                   */


extern Widget
XmCreateList(Widget parent, char *name, Arg *arglist, Cardinal argCount)
{
    return XtCreateWidget(name, xmListWidgetClass, parent, arglist, argCount);
}


extern Widget
XmCreateScrolledList(Widget parent, char *name, Arg *arglist, Cardinal argCount)
{
    Widget sw, list;
    char *sname;
    Cardinal i;
    Arg *al;

    sname = XtMalloc(strlen(name) + 3);
    strcpy(sname, name);
    strcat(sname, "SW");

    al = (Arg *)XtCalloc(argCount + 4, sizeof(Arg));
    for (i = 0; i < argCount; i++)
    {
	al[i].name = arglist[i].name;
	al[i].value = arglist[i].value;
    }

    XtSetArg(al[i], XmNscrollingPolicy, XmAPPLICATION_DEFINED); i++;
    XtSetArg(al[i], XmNvisualPolicy, XmVARIABLE); i++;
    XtSetArg(al[i], XmNscrollBarDisplayPolicy, XmSTATIC); i++;
    XtSetArg(al[i], XmNshadowThickness, 0); i++;

    sw = XtCreateManagedWidget(sname, xmScrolledWindowWidgetClass, parent,
			       al, i);
    XtFree((char *)al);
    XtFree((char *)sname);

    list = XtCreateWidget(name, xmListWidgetClass, sw, arglist, argCount);
	XtVaSetValues(sw, XmNworkWindow, list, NULL);
    XtAddCallback(list, XmNdestroyCallback,
		  _XmDestroyParentCallback,
		  (XtPointer)list);
	
    if (XtIsRealized(parent))
    {
	XtRealizeWidget(sw);
    }

	return list;
}


extern void
XmListAddItem(Widget w,
	      XmString item,
	      int position)
{
    DEBUGOUT(_LtDebug(__FILE__, w, "XmListAddItem()\n"));

    if (position < 0)
    {
	/* do some warning type stuff here... */
	position = 0;
    }
    if (position > (List_ItemCount(w) + 1))
    {
	/* do some warning type stuff here... */
	position = 0;
    }

    _XmListAddItemUnselected(w, item, position);
    _XmListSelectPosIfMatch(w, position);
    _XmListSetGeometryIfNeeded(w);
    _XmListRedraw(w, True);
}


extern void
XmListAddItems(Widget w,
	       XmString *items,
	       int item_count,
	       int position)
{
    int i;

    DEBUGOUT(_LtDebug(__FILE__, w, "XmListAddItems item_count = %d position = %d\n",
    		item_count, position));

    if (position <= 0 || (position > List_ItemCount(w) + 1))
    {
	position = List_ItemCount(w) + 1;
    }

    _XmListAddItemsUnselected(w, items, item_count, position);

    for (i = 0; i < item_count; ++i)
    {
	_XmListSelectPosIfMatch(w, position + i);
    }

    _XmListSetGeometryIfNeeded(w);

    _XmListRedraw(w, True);
}


extern void
XmListAddItemUnselected(Widget w, XmString item, int position)
{
    /*DEBUGOUT(_LtDebug(__FILE__, w,
     *                        "XmListAddItemUnselected(%s, %d)\n",
     *                  _LtDebugXmString2String(item), position)); */

    _XmListAddItemUnselected(w, item, position);
    _XmListSetGeometryIfNeeded(w);
}


extern void
XmListAddItemsUnselected(Widget w, XmString *items, int item_count,
			 int position)
{
    DEBUGOUT(_LtDebug(__FILE__, w,
		      "XmListAddItemsUnselected(_, count %d, pos %d)\n",
		      item_count, position));

    _XmListAddItemsUnselected(w, items, item_count, position);

    _XmListSetGeometryIfNeeded(w);

    _XmListRedraw(w, True);
}


extern void
XmListDeleteAllItems(Widget w)
{
    int i;

    DEBUGOUT(_LtDebug(__FILE__, w, "XmListDeleteAllItems()\n"));

    for (i = 0; i < List_ItemCount(w); ++i)
    {
	XmStringFree(List_Items(w)[i]);
    }

    List_ItemCount(w) = List_SelectedItemCount(w) = List_LastItem(w) = 0;
    List_TopPosition(w) = 1;
    List_LastHLItem(w) = 0;

    _XmListDetermineItemSize(w);
    /*
        Note: The width is recalculated according to the docs if the 
              size policy is XmRESIZE_IF_POSSIBLE and the widget has 
              not been realized yet.  See test15.  pgw - 6/18/98
    */
    _XmListSetGeometryIfNeeded(w);

    _XmListRedraw(w, True);
}


extern void
XmListDeleteItem(Widget w, XmString item)
{
    DEBUGOUT(_LtDebug(__FILE__, w, "XmListDeleteItem()\n"));

    if (_XmListDeleteItem(w, item))
    {
	_XmListSetGeometryIfNeeded(w);

	_XmListRedraw(w, True);
    }
    else
    {
	XtWarning("XmListDeleteItem: item not found in list.\n");
    }
}


extern void
XmListDeleteItems(Widget w, XmString *items, int item_count)
{
    Boolean need_refresh = False;
    int i;

    DEBUGOUT(_LtDebug(__FILE__, w, "XmListDeleteItems()\n"));

    for (i = 0; i < item_count; ++i)
    {
	need_refresh |= _XmListDeleteItem(w, items[i]);
    }

    if (need_refresh)
    {
        /*
           The _XmListSetGeometryIfNeeded routine is called here rather
           then in _XmListDeletePos because _XmListDeletePos could be called
           multiple times from the caller.  Hence this will compress the geometry calls.
        */
        _XmListSetGeometryIfNeeded(w);

	_XmListRedraw(w, True);
    }
}


extern void
XmListDeleteItemsPos(Widget w, int item_count, int position)
{
    int i = 0;

    DEBUGOUT(_LtDebug(__FILE__, w, "XmListDeleteItemsPos()\n"));

    if (position < 0 || position > List_ItemCount(w))
    {
	XtWarning("XmDeleteItemsPos: position not in list bounds.\n");
	return;
    }

    if (position)
    {
	while (i++ < item_count && position <= List_ItemCount(w))
	    _XmListDeletePos(w, position);
    }
    else
    {
	_XmListDeletePos(w, List_ItemCount(w));
    }

    /*
       The _XmListSetGeometryIfNeeded routine is called here rather
       then in _XmListDeletePos because _XmListDeletePos could be called
       multiple times from the caller.  Hence this will compress the geometry calls.
    */
    _XmListSetGeometryIfNeeded(w);

    _XmListRedraw(w, True);
}


extern void
XmListDeletePos(Widget w, int position)
{
    DEBUGOUT(_LtDebug(__FILE__, w, "XmListDeletePos()\n"));

    if (position < 0 || position > List_ItemCount(w))
    {
	XtWarning("XmDeletePos: position is not within list bounds.\n");
	return;
    }

    if (position == 0)
    {
	position = List_ItemCount(w);
    }

    _XmListDeletePos(w, position);
    _XmListSetGeometryIfNeeded(w);

    _XmListRedraw(w, True);
}


extern void
XmListDeletePositions(Widget w, int *position_list, int position_count)
{
    int i, j;

    DEBUGOUT(_LtDebug(__FILE__, w, "XmListDeletePositions()\n"));

    for (i = 0; i < position_count; ++i)
    {
	position_list[i] = (position_list[i] ? position_list[i] :
			    List_ItemCount(w));
    }

    for (i = List_ItemCount(w); i > 0; --i)
    {
	for (j = 0; j < position_count; ++j)
	{
	    if (position_list[j] == i)
	    {
		_XmListDeletePos(w, i);
		break;
	    }
	}
    }

    _XmListSetGeometryIfNeeded(w);
    _XmListRedraw(w, True);
}


extern void
XmListDeselectAllItems(Widget w)
{
    DEBUGOUT(_LtDebug(__FILE__, w, "XmListDeselectAllItems()\n"));

    _XmListDeselectAll(w);
    _XmListRedraw(w, False);
}


extern void
XmListDeselectItem(Widget w, XmString item)
{
    int i;
    _XmString str;

    DEBUGOUT(_LtDebug(__FILE__, w, "XmListDeselectItem()\n"));

    str = _XmStringCreate(item);

    for (i = 0; i < List_ItemCount(w); i++)
    {
	if (_XmStringByteCompare(str, List_InternalList(w)[i]->name))
	{
	    XmListDeselectPos(w, i + 1);
	}
    }

    _XmStringFree(str);
}


extern void
XmListDeselectPos(Widget w, int position)
{
    DEBUGOUT(_LtDebug(__FILE__, w, "XmListDeselectPos()\n"));

    if (position == 0)
    {
	position = List_ItemCount(w);
    }

    if (position < 0)
    {
	position = List_ItemCount(w) - 1;	/* Not in the specs */
    }

    if (_XmListDeselectPos(w, position))
    {
	_XmListRedraw(w, False);
    }
    else
    {
	_XmWarning(w,
		   "XmListDeselectPos(%d) : item not found in selectedItems.\n",
		   position);
    }
}


extern int
XmListGetKbdItemPos(Widget w)
{
    DEBUGOUT(_LtDebug(__FILE__, w, "XmListGetKbdItemPos()\n"));

    return List_LastHLItem(w);
}


extern Boolean
XmListGetMatchPos(Widget w, XmString item,
		  int **position_list, int *position_count)
{
    int i;

    DEBUGOUT(_LtDebug(__FILE__, w, "XmListGetMatchPos()\n"));

    *position_count = 0;
    *position_list = NULL;

    for (i = 0; i < List_ItemCount(w); ++i)
    {
	if (XmStringCompare(item, List_Items(w)[i]))
	{
	    *position_list = (int *)XtRealloc((char *)*position_list,
					      ((*position_count)+1) * sizeof(int));

	    (*position_list)[(*position_count)++] = i + 1;
	}
    }

    return *position_count ? True : False;
}

/*
 * This routine is obsolete.
 * It is replaced by calling XtGetValues for the List resources XmNselectedPositions
 * and XmNselectedPositionCount. XmListGetSelectedPos is a Boolean function that returns
 * an array of the positions of the selected items in a List.
 * The position of the first item in the list is 1; the position of the second item is 2;
 * and so on. When the return value is True, XmListGetSelectedPos allocates memory for
 * this array. The caller is responsible for freeing this memory. The caller can recover
 * the allocated memory by calling XtFree. 
 */
extern Boolean
XmListGetSelectedPos(Widget w, int **position_list, int *position_count)
{
    int			i, j;
    XmListWidget	lw = (XmListWidget)w;

    DEBUGOUT(_LtDebug(__FILE__, w, "XmListGetSelectPos()\n"));

    if (position_count == NULL || position_list == NULL)
	    return False;
    *position_count = List_SelectedItemCount(w);
    *position_list = (int *)XtMalloc(List_SelectedItemCount(w) * sizeof(int));
    for (i=0; i<List_SelectedItemCount(w); i++)
	    (*position_list)[i] = List_SelectedIndices(w)[i];
    return True;
}


extern Boolean
XmListItemExists(Widget w, XmString item)
{
    int i;

    DEBUGOUT(_LtDebug(__FILE__, w, "XmListItemExists()\n"));

    for (i = 0; i < List_ItemCount(w); ++i)
    {
	if (XmStringCompare(item, List_Items(w)[i]))
	{
	    return True;
	}
    }

    return False;
}


extern int
XmListItemPos(Widget w, XmString item)
{
    int i;

    DEBUGOUT(_LtDebug(__FILE__, w, "XmListItemPos()\n"));

    for (i = 0; i < List_ItemCount(w); ++i)
    {
	if (XmStringCompare(item, List_Items(w)[i]))
	{
	    return i + 1;
	}
    }

    return 0;
}


extern Boolean
XmListPosSelected(Widget w, int position)
{
    DEBUGOUT(_LtDebug(__FILE__, w, "XmListPosSelected()\n"));

    if (position < 0 || position > List_ItemCount(w))
    {
	return False;
    }

    if (position == 0)
    {
	position = List_ItemCount(w);
    }

    return List_InternalList(w)[position - 1]->selected;
}


extern Boolean
XmListPosToBounds(Widget w,
		  int position,
		  Position *x,
		  Position *y,
		  Dimension *width,
		  Dimension *height)
{
    DEBUGOUT(_LtDebug(__FILE__, w, "XmListPosToBounds()\n"));

    if (!position)
    {
	position = List_ItemCount(w);
    }

    if (position < List_TopPosition(w) ||
	position >= List_TopPosition(w) + List_VisibleItemCount(w))
    {
	return False;
    }

    if (x)
    {
	*x = (List_MarginWidth(w)
	      + Prim_ShadowThickness(w)
	      + Prim_HighlightThickness(w));
    }

    if (width)
    {
	*width = List_InternalList(w)[position - 1]->width;
    }

    if (height)
    {
	*height = List_InternalList(w)[position - 1]->height;
    }

    if (y)
    {
	*y = ((Prim_ShadowThickness(w)
	       + List_MarginHeight(w)
	       + Prim_HighlightThickness(w))
#ifdef USE_CUM_HEIGHT
	      + (List_InternalList(w)[position - 1]->CumHeight
		 - List_InternalList(w)[position - 1]->height
#else
	      + (List_MaxItemHeight(w) * (position - 1)
#endif
		 - List_Vorigin(w)));
    }

    return True;
}


extern void
XmListReplaceItems(Widget w, XmString *old_items, int item_count,
		   XmString *new_items)
{
    int i, j;
    Boolean need_refresh = False;

    DEBUGOUT(_LtDebug(__FILE__, w, "XmListReplaceItems()\n"));

    for (i = 0; i < item_count; ++i)
    {

	for (j = 0; j < List_ItemCount(w); ++j)
	{

	    /* see if item in old_items[] matches item in list */
	    if (XmStringCompare(old_items[i], List_Items(w)[j]))
	    {

		/* we have a match */
		need_refresh = True;

		/* deselect the item */
		(void)_XmListDeselectPos(w, j);

		_XmListReplaceItemPos(w, j, new_items[i]);

		/* set selected if match item in selected items list */
		_XmListSelectPosIfMatch(w, j);
	    }
	}
    }

    if (need_refresh)
    {
        _XmListSetGeometryIfNeeded(w);
	_XmListRedraw(w, True);
    }
}


extern void
XmListReplaceItemsPos(Widget w, XmString *new_items, int item_count, int position)
{
    int i, j = (position ? position : List_ItemCount(w)) - 1;

    DEBUGOUT(_LtDebug(__FILE__, w, "XmListReplaceItemsPos(%d)\n", j));

    /* rws 22 May 1998
       We must be careful here! j is zero based _XmListDeselectPos and
       _XmListSelectPosIfMatch are one based but _XmListReplaceItemPos
       is zero based.
     */
    for (i = 0; i < item_count && j < List_ItemCount(w); ++i)
    {
	(void)_XmListDeselectPos(w, j + 1);
	_XmListReplaceItemPos(w, j, new_items[i]);

	/* FIX ME : The two statements below can be combined to be more
	 * efficient, if the semantics of _XmListSelectPosIfMatch can be
	 * changed so it'll deselect stuff as well.
	 * There's a good chance that this can be done */

	(void)_XmListDeselectPos(w, j + 1);		/* Danny 23/7/97 */

	_XmListSelectPosIfMatch(w, j + 1);

	j++;
    }
    /*
     * This is needed because the width of the list may need to
     * be changed.
     */
    _XmListSetGeometryIfNeeded(w);

	_XmListRedraw(w, False); /* Peter Stein 2000/09/04: redraw only changed items */
}


extern void
XmListReplaceItemsPosUnselected(Widget w, XmString *new_items, int item_count,
				int position)
{
    int i;

    DEBUGOUT(_LtDebug(__FILE__, w, "XmListReplaceItemsPosUnselected()\n"));

    if (!position)
    {
	position = List_ItemCount(w);
    }

    for (i = 0; i < item_count && position <= List_ItemCount(w); i++, position++)
    {
	(void)_XmListDeselectPos(w, position);

	_XmListReplaceItemPos(w, position - 1, new_items[i]);
    }

    _XmListSetGeometryIfNeeded(w);
    _XmListRedraw(w, True);
}


extern void
XmListReplaceItemsUnselected(Widget w, XmString *old_items, int item_count, XmString *new_items)
{
    Boolean need_refresh = False;
    int i, j;

    DEBUGOUT(_LtDebug(__FILE__, w, "XmListReplaceItemsUnselected()\n"));

    for (i = 0; i < item_count; ++i)
    {
	for (j = 0; j < List_ItemCount(w); ++j)
	{
	    if (XmStringCompare(old_items[i], List_Items(w)[j]))
	    {
		need_refresh = True;
		(void)_XmListDeselectPos(w, j);
		_XmListReplaceItemPos(w, j, new_items[i]);
	    }
	}
    }

    if (need_refresh)
    {
        _XmListSetGeometryIfNeeded(w);
	_XmListRedraw(w, True);
    }
}


extern void
XmListReplacePositions(Widget w, int *position_list, XmString *item_list, int item_count)
{
    int i, j;

    DEBUGOUT(_LtDebug(__FILE__, w, "XmListReplacePosition()\n"));

    for (i = 0; i < item_count; ++i)
    {
	j = (position_list[i] ? position_list[i] : List_ItemCount(w)) - 1;
	(void)_XmListDeselectPos(w, j);

	if (position_list[i] > List_ItemCount(w))
	{
	    /* print a warning */
	}
	else
	{
	    _XmListReplaceItemPos(w, j, item_list[i]);
	    (void)_XmListDeselectPos(w, j);	/* Danny 23/7/97 */
	    _XmListSelectPosIfMatch(w, j);
	}
    }

    _XmListSetGeometryIfNeeded(w);
    _XmListRedraw(w, False); /* Peter Stein 2000/09/04: redraw only changed items */
}


extern void
XmListSelectItem(Widget w, XmString item, Boolean notify)
{
    int i;

    DEBUGOUT(_LtDebug(__FILE__, w, "XmListSelectItem()\n"));

    for (i = 0; i < List_ItemCount(w); ++i)
    {
	if (XmStringCompare(item, List_Items(w)[i]))
	{
	    XmListSelectPos(w, i + 1, notify);

	    return;
	}
    }
}


extern void
XmListSelectPos(Widget w, int position, Boolean notify)
{
	switch (List_SelectionPolicy(w))
	{
	case XmBROWSE_SELECT:
		DEBUGOUT(_LtDebug(__FILE__, w, "XmListSelectPos() - BROWSE_SELECT\n"));
		break;

	case XmSINGLE_SELECT:
		DEBUGOUT(_LtDebug(__FILE__, w, "XmListSelectPos() - SINGLE_SELECT\n"));
		break;

	case XmMULTIPLE_SELECT:
		DEBUGOUT(_LtDebug(__FILE__, w, "XmListSelectPos() - MULTIPLE_SELECT\n"));
		break;

	case XmEXTENDED_SELECT:
		DEBUGOUT(_LtDebug(__FILE__, w, "XmListSelectPos() - EXTENDED_SELECT\n"));
		break;
	}

    if (position < 0 || position > List_ItemCount(w) || 
	(position == 0 && List_ItemCount(w) == 0)) {
	return;
    }

    if (position == 0) {
	position = List_ItemCount(w);
    }

    if ((List_SelectionPolicy(w) == XmBROWSE_SELECT ||
	 List_SelectionPolicy(w) == XmSINGLE_SELECT) && List_LastItem(w) != 0) {
        _XmListDeselectPos(w, List_LastItem(w));
    }

    if ( (List_SelectionPolicy(w) == XmMULTIPLE_SELECT) ||
         (List_SelectionPolicy(w) == XmEXTENDED_SELECT) ) {
	 _XmListTogglePos(w, position);
    } else {
      _XmListSelectPos(w, position);
    }
    _XmListSetCursorPos(w, position);
    _XmListRedraw(w, False);

    if (notify)
    {
	XAnyEvent *event = (XAnyEvent *)XtMalloc(sizeof(XAnyEvent));
	event->type = 0;
	event->serial = 0;
	event->send_event = False;
	event->display = XtDisplay(w);
	event->window = XtWindow(w);

	_XmListInvokeCallbacks(w, (XEvent *)event, False);

	XtFree((char *)event);
    }
}

extern void
XmListSetAddMode(Widget w, Boolean mode)
{
    DEBUGOUT(_LtDebug(__FILE__, w, "XmListSetAddMode()\n"));

    if (List_SelectionPolicy(w) == XmEXTENDED_SELECT)
    {
	List_AddMode(w) = mode;
    }
}


extern void
XmListSetBottomItem(Widget w, XmString item)
{
    Boolean redraw_all = False;
    int i;

    DEBUGOUT(_LtDebug(__FILE__, w, "XmListSetBottomItem()\n"));

    for (i = 0; i < List_ItemCount(w); ++i)
    {
	if (XmStringCompare(item, List_Items(w)[i]))
	{
	int new_pos;

	    new_pos = i - List_VisibleItemCount(w) + 2;
	    if (new_pos < 1)
	    {
	    	new_pos = 1;
	    }
	    _XmListSetTopPos(w, new_pos, &redraw_all);
	    _XmListRedraw(w, redraw_all);
	    break;
	}
    }
}


extern void
XmListSetBottomPos(Widget w, int position)
{
    Boolean redraw_all = False;
    DEBUGOUT(_LtDebug(__FILE__, w, "XmListSetBottomPos() - position %i count %i visible %i\n",
    	position, 
    	List_ItemCount(w),
    	List_VisibleItemCount(w)
    	));

    if (position < 0 || position > List_ItemCount(w))
    {
	return;
    }

    if (position == 0)
    {
	position = List_ItemCount(w);
    }

    /* Ensure that position - List_VisibleItemCount(w) + 1 >= 1 */
    if (position < List_VisibleItemCount(w))
    {
	_XmListSetTopPos(w, 1, &redraw_all);
    }
    else
    {
	_XmListSetTopPos(w, position - List_VisibleItemCount(w) + 1,
			 &redraw_all);
    }

    _XmListRedraw(w, redraw_all);
}


extern void
XmListSetHorizPos(Widget w, int position)
{
    Boolean redraw_all = False;

    DEBUGOUT(_LtDebug(__FILE__, w,
		      "XmListSetHorizPos()      List_MaxWidth() = %d\n",
		      List_MaxWidth(w)));

    if (!List_IsScrolledList(w) || !XtIsManaged(List_HSB(w)))
    {
	return;
    }

    if (position < List_Hmin(w))
    {
	position = List_Hmin(w);
    }
    else if (position > List_Hmax(w))
    {
	position = List_Hmax(w);
    }

    _XmListUpdateHorizScrollBar(w, position, &redraw_all);

    if (redraw_all)
    {
	_XmListRedraw(w, True);
    }
}


extern void
XmListSetItem(Widget w, XmString item)
{
    Boolean redraw_all = False;
    int i;

    DEBUGOUT(_LtDebug(__FILE__, w, "XmListSetItem()\n"));

    for (i = 0; i < List_SelectedItemCount(w); ++i)
    {
	if (XmStringCompare(item, List_Items(w)[i]))
	{
	    _XmListSetTopPos(w, i + 1, &redraw_all);
	    _XmListRedraw(w, redraw_all);
	    break;
	}
    }
}


extern Boolean
XmListSetKbdItemPos(Widget w, int position)
{
    DEBUGOUT(_LtDebug(__FILE__, w, "XmListSetKbdItemPos()\n"));

    if (position < 0 || position > List_ItemCount(w) || !List_ItemCount(w))
    {
	return False;
    }

    if (!position)
    {
	position = List_ItemCount(w);
    }

    _XmListSetCursorPos(w, position);
    _XmListRedraw(w, False);

    return True;
}


extern void
XmListSetPos(Widget w, int position)
{
    Boolean redraw_all = False;
    DEBUGOUT(_LtDebug(__FILE__, w, "XmListSetPos()\n"));

    if (position < 0 || position > List_ItemCount(w))
    {
	return;
    }

    if (!position)
    {
	position = List_ItemCount(w);
    }

    _XmListSetTopPos(w, position, &redraw_all);
    _XmListRedraw(w, redraw_all);
}


/* 
 * This function frees the existing XmNselectedItems list, then
 * recreates it.  It is used to resynch the XmNselectedItems
 * if you have messed with some internal XmList structures and
 * possibly screwed things up.  This function is clearly a 
 * kluge.
 */
extern void
XmListUpdateSelectedList(Widget w)
{
    int i;

    DEBUGOUT(_LtDebug(__FILE__, w, "XmListUpdateSelectedList()\n"));

    /* reallocate the XmNselectedItems */
	/* T. Straumann: use this routine */
	_XmListReallocSelectedItems(w);

    /* reset the selected count to zero */
    List_SelectedItemCount(w) = 0;

    for (i = 0; i < List_ItemCount(w); ++i)
    {
	if (List_InternalList(w)[i]->selected)
	{
		/* T. Straumann: fixed memory leak, selected items must not contain
		 *				 copies.
		 */
	    List_SelectedItems(w)[List_SelectedItemCount(w)] = List_Items(w)[i];
		/* T. Straumann: IMHO, the List_SelectedItemIndex shoud be fixed as well */
		List_SelectedIndices(w)[List_SelectedItemCount(w)] = i+1;
		List_SelectedItemCount(w)++;
	}
    }

    _XmListRedraw(w, True);
}


extern int
XmListYToPos(Widget w, Position y)
{
    int calculated_pos;
    int item_height;
    int margin = List_MarginHeight(w) +
	List_ItemSpacing(w) +
	Prim_ShadowThickness(w) +
	Prim_HighlightThickness(w);

    DEBUGOUT(_LtDebug(__FILE__, w, "XmListYToPos()\n"));

    if (List_ItemCount(w) == 0)
    {
	return 0;
    }

    item_height = List_MaxItemHeight(w) + Prim_HighlightThickness(w) + 1 +
	List_ItemSpacing(w);
    calculated_pos = (y - margin) / item_height + List_TopPosition(w);
 
    if (calculated_pos > List_ItemCount(w))
    {
	calculated_pos = List_ItemCount(w);
    }
    if (calculated_pos < List_TopPosition(w))
    {
	calculated_pos = List_TopPosition(w);
    }
    if (calculated_pos > List_TopPosition(w) + List_VisibleItemCount(w) - 1)
    {
	calculated_pos = List_TopPosition(w) + List_VisibleItemCount(w) - 1;
    }

    if (calculated_pos < 1)
    {
	_XmWarning(w, "XmListYToPos: impossible position %d in %s %d\n",
		   calculated_pos, __FILE__, __LINE__);
	calculated_pos = 0;
    }

    return calculated_pos;
}


/* 
 * This routine determines the preferred size of List.
 */
static XtGeometryResult
query_geometry(Widget w, XtWidgetGeometry *proposed, XtWidgetGeometry *answer)
{
    DEBUGOUT(_LtDebug(__FILE__, w, "%s:query_geometry(%d) - proposed %s\n",
    	__FILE__, __LINE__,
    	_LtDebugWidgetGeometry2String(proposed)));

    answer->request_mode = CWWidth | CWHeight;

    /* Fix for defect found by test15 */
    _XmListCalcWidthBasedOnSizePolicy(w, &answer->width); 
    /* 
     * 7/1/98 pgw: 
     * Only recalculate best height if not realized.  Defect found by test15. 
     */
    _XmListCalcHeight(w, &answer->height);

    DEBUGOUT(_LtDebug(__FILE__, w,
		      "query_geometry  => W %d H %d (items %d visible %d)\n",
		      answer->width, answer->height,
		      List_ItemCount(w), List_VisibleItemCount(w)));

#if 0
    if (((proposed->request_mode & CWWidth) &&
	 proposed->width >= answer->width) &&
	((proposed->request_mode & CWHeight) &&
	 proposed->height >= answer->height))
    {
	return XtGeometryYes;
    }

    if (proposed->width == XtWidth(w) && proposed->height == XtHeight(w))
    {
	if (answer)
	{
	    answer->request_mode = 0;
	}
	return XtGeometryNo;
    }
    else
    {
	return XtGeometryAlmost;
    }
#else
    return _XmGMReplyToQueryGeometry(w, proposed, answer);
#endif
}


static void
list_border_highlight(Widget w)
{
    DEBUGOUT(_LtDebug(__FILE__, w,
		      "list_border_highlight() LastHLItem: %d Top: %d\n",
		      List_LastHLItem(w), List_TopPosition(w)));

    Prim_Highlighted(w) = True;
    Prim_HighlightDrawn(w) = True;

    _XmListHighlight(w);
}


static void
list_border_unhighlight(Widget w)
{
    DEBUGOUT(_LtDebug(__FILE__, w,
		      "list_border_unhighlight() LastHLItem: %d\n",
		      List_LastHLItem(w)));

    if (!XtIsManaged(w))
    {
	return;
    }

    Prim_Highlighted(w) = False;
    Prim_HighlightDrawn(w) = False;

    _XmListUnhighlight(w);
}


static void
ListLeave(Widget w,
	  XEvent *event,
	  String *params,
	  Cardinal *num_params)
{
    DEBUGOUT(_LtDebug(__FILE__, w, "XmListLeave()\n"));

    XtCallActionProc(w, "PrimitiveLeave", event, params, *num_params);
}


static void
ListEnter(Widget w,
	  XEvent *event,
	  String *params,
	  Cardinal *num_params)
{
    DEBUGOUT(_LtDebug(__FILE__, w, "XmListEnter()\n"));

    XtCallActionProc(w, "PrimitiveEnter", event, params, *num_params);
}


static Boolean
drag_selected_proc(Widget w,
		   Atom *selection,
		   Atom *target,
		   Atom *type_return,
		   XtPointer *value_return,
		   unsigned long *length_return,
		   int *format_return)
{
    Atom COMPOUND_TEXT;
    Atom MOTIF_DROP;

    COMPOUND_TEXT = XmInternAtom(XtDisplay(w), _XA_COMPOUND_TEXT, False);
    MOTIF_DROP = XmInternAtom(XtDisplay(w), _XA_MOTIF_DROP, False);

    if (*selection != MOTIF_DROP)
    {
	return False;
    }

    DEBUGOUT(_LtDebug(__FILE__, w, "We're dealing with a motif drop\n"));

    return False;
}


static Boolean
drag_unselected_proc(Widget w,
		     Atom *selection,
		     Atom *target,
		     Atom *type_return,
		     XtPointer *value_return,
		     unsigned long *length_return,
		     int *format_return)
{
    Atom COMPOUND_TEXT;
    Atom MOTIF_DROP;

    COMPOUND_TEXT = XmInternAtom(XtDisplay(w), _XA_COMPOUND_TEXT, False);
    MOTIF_DROP = XmInternAtom(XtDisplay(w), _XA_MOTIF_DROP, False);

    if (*selection != MOTIF_DROP)
    {
	return False;
    }

    DEBUGOUT(_LtDebug(__FILE__, w, "We're dealing with a motif drop\n"));

    return False;
}


static void
drag_drop_finish(Widget w,
		 XtPointer client_data,
		 XtPointer call_data)
{
}


#ifdef IXI_STUFF
/*
     amai: Finally I've found at least some info in this in the web:

     "The IXI Motif 1.2.2 toolkit, which was available for Solaris 2.3 software
     development, contains some incompatibilities with standard OSF/Motif 1.2.2.
     These features are not part of the OSF/Motif 1.2 specification,
     and are not present in the Solaris 2.4 and later Motif toolkits. 

     The following "XmList convenience functions" are the non-standard functions
     in IXI Motif. Remove them from your application code if you have
     used them"

     We provide some stubs which may be commented out. Note that we still
     don't know about their full signature.
*/

extern void
XmListRecolorItem()
{
    XtWarning("XmListRecolorItem() is not implemented. Function is obsolete!\n");
}


extern void
XmListRecolorPos()
{
    XtWarning("XmListRecolorPos() is not implemented. Function is obsolete!\n");
}


extern void
XmListSetClientDataPos()
{
    XtWarning("XmListSetClientDataPos() is not implemented. Function is obsolete!\n");
}


extern void
XmListSetClientDatasPos()
{
    XtWarning("XmListSetClientDatasPos() is not implemented. Function is obsolete!\n");
}
#endif /* IXI_STUFF */

static void
_XmListSetRenderTable(Widget w, int o, XrmValue *v)
{
	XmListWidget		lw = (XmListWidget)w;
	static const XmFontList	nullfl = 0;

	DEBUGOUT(_LtDebug(__FILE__, w, "_XmListSetRenderTable (%d)\n",
		lw->list.check_set_render_table));

	++lw->list.check_set_render_table;
	switch (lw->list.check_set_render_table)
	{
        case 1:
                /*
                 * Either the font list or render table resource has
                 * not been set, but do not know yet if both have not
                 * been set.  For now, preserve the value in case one
                 * of the resources has been set.
                 */
                v->addr = (char *)&(lw->list.font);
                break;

        case 2:
                /*
                 * Neither the font list nor render table resource has
                 * been set.  To avoid relying on the structure having
                 * been zero filled by the Xt library, ensure the
                 * font element is set to NULL.
                 */
                v->addr = (char*)&nullfl;
                break;

        default:
                /* This should never happen. */
                v->addr = NULL;
		break;
	}
}
