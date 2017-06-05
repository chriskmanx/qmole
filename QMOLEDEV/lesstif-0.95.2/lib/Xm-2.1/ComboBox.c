/**
 *
 * $Header: /cvsroot/lesstif/lesstif/lib/Xm-2.1/ComboBox.c,v 1.1 2004/08/28 19:22:43 dannybackx Exp $
 *
 * Copyright (C) 1997 Free Software Foundation, Inc.
 * Copyright © 1997, 1998, 1999, 2000, 2001, 2002 LessTif Development Team
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

static const char rcsid[] = "$Header: /cvsroot/lesstif/lesstif/lib/Xm-2.1/ComboBox.c,v 1.1 2004/08/28 19:22:43 dannybackx Exp $";

#include <LTconfig.h>

#include <XmI/XmI.h>
#include <Xm/XmP.h>
#include <Xm/ListP.h>
#include <Xm/TextFP.h>
#include <Xm/GrabShell.h>
#include <Xm/ComboBoxP.h>
#include <Xm/TransltnsP.h>
#include <Xm/VendorSEP.h>

#include <XmI/MessagesI.h>
#include <XmI/PopupUtil.h>

#include <XmI/DebugUtil.h>


/* 
  MODULE: CombBox.c
  DESCRIPTION:
  This widget is under construction.
  END:
*/

/* Forward Declarations */
static void CallSelectionCallbacks(Widget w, 
   XtPointer client_data, 
   XtPointer data);
static void CBActivate(Widget w, 
   XEvent *xev, 
   String *params, 
   Cardinal *num_params);
static void CBArmAndDropDownList(Widget w, 
   XEvent *xev, 
   String *params, 
   Cardinal *num_params); 
static void CBCancel(Widget w,
   XEvent *xev, 
   String *params, 
   Cardinal *num_params);
static void CBDisarm(Widget w,
   XEvent *xev, 
   String *params, 
   Cardinal *num_params);
static void CBDropDownList(Widget w, 
   XEvent *xev, 
   String *params, 
   Cardinal *num_params);
static void CBFocusIn(Widget w,
   XEvent *xev, 
   String *params, 
   Cardinal *num_params);
static void CBFocusOut(Widget w,
   XEvent *xev, 
   String *params, 
   Cardinal *num_params);
#if 0
static void CBGetSelectedItem(Widget w,
   XEvent *xev, 
   String *params, 
   Cardinal *num_params);
static void CBGetSelectedPos(Widget w,
   XEvent *xev, 
   String *params, 
   Cardinal *num_params);
#endif
static void CBListAction(Widget w,
   XEvent *xev, 
   String *params, 
   Cardinal *num_params);
#if 0
static void CBSetSelectedItem(Widget w,
   XEvent *xev, 
   String *params, 
   Cardinal *num_params);
static void CBSetSelectedPos(Widget w,
   XEvent *xev, 
   String *params, 
   Cardinal *num_params);
#endif
static void CBTextFocusOut(Widget w,
   XEvent *xev, 
   String *params, 
   Cardinal *num_params);
static void ChangeManaged(Widget w);
static void CheckMinimalSize(Widget w, XtWidgetGeometry *reply);
#if 0
static void CheckSetRenderTable(Widget w);
#endif
static void ClassInitialize(void);
static void ClassPartInitialize(WidgetClass widget_class);
static void ComboBoxParentProcess(Widget w);
static void ComputeSize(Widget w, XtWidgetGeometry *reply);
static void ConstraintDestroy(Widget w);
static void CreateArrowGC(Widget w);
static void CreateChildren(Widget w);
static void CreateEditBox(Widget parent);
static void CreatePulldown(Widget parent);
static void CreateScrolledList(Widget cb);
static void Destroy(Widget w);
static void DoLayout(Widget w);
static void DrawArrow(Widget w);
static void DrawBorder(Widget w);
static void DrawShadows(Widget w);
static Widget _FindComboBox(Widget w);
static Widget FindComboBox(Widget w);
static Widget _FindVendorShell(Widget w);
static XtGeometryResult GeometryManager(Widget w, 
   XtWidgetGeometry *request, 
   XtWidgetGeometry *reply);
static XmString GetEditBoxValue(Widget w);
static void HighlightBorder(Widget w);
static void Initialize(Widget request, 
   Widget c_new,
   ArgList args,
   Cardinal *num_args);
static void InsertChild(Widget w);
static void ListSelectionCB(Widget w, XtPointer client_data, XtPointer data);
static void PopdownList(Widget w, XEvent *event);
static void PopupEH(Widget w, XtPointer udata, XEvent *xev, Boolean *pb);
static XtGeometryResult QueryGeometry(Widget w, 
   XtWidgetGeometry *proposed, 
   XtWidgetGeometry *answer);
static void Redisplay(Widget w, XEvent *event, Region region);
static void RemoveFocusChangeHandler(Widget w);
static void RemoveHitAreaHandler(Widget w);
static void Resize(Widget w);
#if 0
static void SBBtnDownEH(Widget w, XtPointer udata, XEvent *xev, Boolean *pb);
static void SBBtnUpEH(Widget w, XtPointer udata, XEvent *xev, Boolean *pb);
#endif
static void SetEditBoxValue(Widget w, XmString s);
static void SetFocusChangeHandler(Widget w);
static void SetHitArea(Widget w);
static void SetHitAreaSize(Widget w);
static void SetHitAreaHandler(Widget w);
static void SetIdealTextSize(Widget w);
static Boolean SetValues(Widget current, 
   Widget request, 
   Widget c_new, 
   ArgList args,
   Cardinal *num_args);
static void ShellFocusMovedCB(Widget w, XtPointer client_data, XtPointer data);
static void ShellPopdownCB(Widget w, XtPointer client_data, XtPointer data);
static void ShellPopupCB(Widget w, XtPointer client_data, XtPointer data);
static void TextChangedCB(Widget w, XtPointer client_data, XtPointer data);
static void UnhighlightBorder(Widget w);
static void AdjustKids(Widget w);
static void AdjustKidsComboBox(Widget w);
static void AdjustKidsDropDown(Widget w);
static void CalcCBLineHeight(Widget w, Dimension *height);
static Boolean IsEventInRectangle(XEvent *xev, XRectangle *rect);
static Boolean KidsValid(Widget w);
static void ComboBoxFocusMovedCB(Widget w, XtPointer client_data, XtPointer data);
static void _LTTranslateCoords(Widget w, Position x, Position y, Position *rootx, Position *rooty);
static void _LTShellSmartPlacement(Widget w);
static void __LTShellSmartPlacement(Widget w, int *ret_x, int *ret_y);
static void GetValuesHook(Widget w, ArgList args, Cardinal *num_args);


#define VALID(w) (w && XtIsManaged(w))
#define Unused(x) (void)(x)
#define superclass (&xmManagerClassRec)
#define Offset(field) XtOffsetOf(XmComboBoxRec, combo_box.field)

/* class names for private widgets */
#define CB_LIST_CN "List"
#define CB_LIST_SHELL_CN "GrabShell"
#define CB_TEXT_CN "Text"

/* FIXTHIS: This belongs in MacrosI.h */
#define CB_ArrowGC(w) \
    (((XmComboBoxWidget)(w))->combo_box.arrow_GC)

#define CB_IdealEBHeight(w) \
    (((XmComboBoxWidget)(w))->combo_box.ideal_ebheight)

#define CB_IdealEBWidth(w) \
    (((XmComboBoxWidget)(w))->combo_box.ideal_ebwidth)

/* ========================================================================== */
/* Resources for the ComboBox class */
static XtResource resources[] = {

   {
     XmNarrowSize,
     XmCArrowSize,
     XmRHorizontalDimension,
     sizeof(Dimension),
     Offset(arrow_size),
     XmRImmediate,
     (XtPointer)0
   },

   {
     XmNarrowSpacing,
     XmCArrowSpacing,
     XmRHorizontalDimension,
     sizeof(Dimension),
     Offset(arrow_spacing),
     XmRImmediate,
     (XtPointer)0
   },

   {
     XmNcomboBoxType,
     XmCComboBoxType,
     XmRComboBoxType,
     sizeof(unsigned char),
     Offset(type),
     XmRImmediate, 
     (XtPointer)XmCOMBO_BOX
   },

   {
     XmNfontList,
     XmCFontList,
     XmRFontList,
     sizeof(XmFontList),
     Offset(render_table),
     XmRImmediate,
     (XtPointer)NULL
   },

   {
     XmNhighlightThickness,
     XmCHighlightThickness,
     XmRHorizontalDimension,
     sizeof(Dimension),
     Offset(highlight_thickness),
     XmRImmediate,
     (XtPointer)2
   },

   {
     XmNmarginWidth,
     XmCMarginWidth,
     XmRHorizontalDimension,
     sizeof(Dimension),
     Offset(margin_width),
     XmRImmediate, 
     (XtPointer)2
   },

   {
     XmNmarginHeight,
     XmCMarginHeight, 
     XmRVerticalDimension,
     sizeof(Dimension), 
     Offset(margin_height),
     XmRImmediate, 
     (XtPointer)2
   },

   {
     XmNmatchBehavior,
     XmCMatchBehavior,
     XmRMatchBehavior,
     sizeof(unsigned char),
     Offset(match_behavior),
     XmRImmediate,
     (XtPointer)XmNONE
   },

   {
     XmNrenderTable,
     XmCRenderTable,
     XmRRenderTable,
     sizeof(XmRenderTable), 
     Offset(render_table),
     XmRImmediate,
     (XtPointer)NULL
   },

   {
     XmNselectedItem,
     XmCSelectedItem,
     XmRXmString,
     sizeof(XmString), 
     Offset(selected_item),
     XmRImmediate,
     (XtPointer)NULL
   },

   {
     XmNselectedPosition,
     XmCSelectedPosition,
     XmRInt,
     sizeof(int), 
     Offset(selected_position),
     XmRImmediate,
     (XtPointer)0
   },

   {
     XmNselectionCallback,
     XmCCallback, 
     XtRCallback,
     sizeof(XtCallbackList),
     Offset(selection_callback),
     XmRImmediate,
     (XtPointer)NULL
   },

   {
     XmNshadowThickness,
     XmCShadowThickness,
     XmRHorizontalDimension,
     sizeof(Dimension),
     XtOffsetOf(XmComboBoxRec, manager.shadow_thickness),
     XmRImmediate,
     (XtPointer)2
   },

#if XmVersion >= 2001
   {
     XmNpositionMode,
     XmCPositionMode,
     XmRPositionMode,
     sizeof(XtEnum),
     Offset(position_mode),
     XmRImmediate,
     (XtPointer)XmZERO_BASED
   },
#endif
#if 0
	/*
	 * We should override XmList's XmNvisibleItemCount's default
	 * value. In XmList, this is dynamic, here it should be 10.
	 */
   {
	XmNvisibleItemCount, XmCVisibleItemCount, XmRInt, sizeof(
   },
#endif
};


static XmSyntheticResource syn_resources[] = {
   {
      XmNselectedItem,
      sizeof(XmString),
      Offset(selected_item),
      _XmExportXmString,
      NULL
   },

   {
      XmNselectedPosition,
      sizeof(int), 
      Offset(selected_position),
      NULL, /* FIX ME */
      NULL  /* FIX ME */
   },

   {
      XmNmarginWidth,
      sizeof(Dimension), 
      Offset(margin_width),
      _XmFromHorizontalPixels, 
      _XmToHorizontalPixels
   },

   {
      XmNmarginHeight,
      sizeof(Dimension), 
      Offset(margin_height),
      _XmFromVerticalPixels,
      _XmToVerticalPixels
   },

   {
      XmNhighlightThickness,
      sizeof(Dimension), 
      Offset(highlight_thickness),
      _XmFromHorizontalPixels, 
      _XmToHorizontalPixels
   },

   {
      XmNshadowThickness,
      sizeof(Dimension), 
      XtOffsetOf(XmManagerRec, manager.shadow_thickness),
      _XmFromHorizontalPixels, 
      _XmToHorizontalPixels
   },

   {
      XmNarrowSize,
      sizeof(Dimension), 
      Offset(arrow_size),
      _XmFromHorizontalPixels,
      _XmToHorizontalPixels
   },

   {
      XmNarrowSpacing,
      sizeof(Dimension), 
      Offset(arrow_spacing),
      _XmFromHorizontalPixels,
      _XmToHorizontalPixels
   },

};

/* these get set in one of the initialize functions */
static XtTranslations parsed_list_translations;
static XtTranslations parsed_text_focus_translations;

static XtAccelerators parsed_accelerators;
static XtAccelerators parsed_list_accelerators;

/* Action table */
static XtActionsRec actionsList[] = {
   {"CBFocusIn", (XtActionProc) CBFocusIn},
   {"CBFocusOut", (XtActionProc) CBFocusOut},
   {"CBActivate", (XtActionProc) CBActivate},
   {"CBArmAndDropDownList", (XtActionProc) CBArmAndDropDownList},
   {"CBCancel", (XtActionProc) CBCancel},
   {"CBDisarm", (XtActionProc) CBDisarm},
   {"CBDropDownList", (XtActionProc) CBDropDownList},
   {"CBListAction", (XtActionProc) CBListAction},
   {"CBTextFocusOut", (XtActionProc) CBTextFocusOut},
};

/* ========================================================================== */
/* Class record */

XmComboBoxClassRec xmComboBoxClassRec = {
   /* Core class part */
   {
      (WidgetClass) &xmManagerClassRec,       /* superclass */
      "XmComboBox",                           /* class_name */
      sizeof(XmComboBoxRec),                  /* widget_size */
      ClassInitialize,                        /* class_initialize */
      ClassPartInitialize,                    /* class_part_initialize */
      FALSE,                                  /* class_inited */
      Initialize,                             /* initialize */
      NULL,                                   /* initialize_hook */
      XtInheritRealize,                       /* realize */
      actionsList,                            /* actions */
      XtNumber(actionsList),                  /* num_actions */
      resources,                              /* resources */
      XtNumber(resources),                    /* num_resources */
      NULLQUARK,                              /* xrm_class */
      TRUE,                                   /* compress_motion */
      XtExposeCompressMaximal,                /* compress_exposure */
      TRUE,                                   /* compress_enterleave */
      FALSE,                                  /* visible_interest */
      Destroy,                                /* destroy */
      Resize, /* XtInheritResize,  */         /* resize */
      Redisplay, /* XtInheritExpose,  */      /* expose */
      SetValues,                              /* set_values */
      NULL,                                   /* set_values_hook */
      XtInheritSetValuesAlmost,               /* set_values_almost */
      GetValuesHook,                          /* get_values_hook */
      NULL,                                   /* accept_focus */
      XtVersion,                              /* version */
      NULL,                                   /* callback offsets */
      _XmComboBox_defaultTranslations,        /* tm_table */
      QueryGeometry,                          /* query_geometry */
      NULL,                                   /* display_accelerator */
      NULL                                    /* extension */ 
   },
   /* Composite class part */
   {
      GeometryManager,        /* geometry manager */
      ChangeManaged,          /* ChangeManaged */
      InsertChild,            /* insert_child */ 
      XtInheritDeleteChild,   /* delete_child */ 
      NULL                    /* extension */ 
   },
   /* Constraint class part */
   {
      NULL,      /* FIXTHIS: */ /* subresources */
      0,         /* FIXTHIS: */ /* subresource_count */
      0,         /* FIXTHIS: */ /* constraint_size */
      NULL,      /* FIXTHIS: */ /* Initialize */
      ConstraintDestroy,        /* Destroy */
      NULL,      /* FIXTHIS: */ /* set_values */
      NULL                      /* extension */ 
   },
   /* XmManager class part */
   {
      XtInheritTranslations,  /* translations */
      syn_resources,          /* syn_resources */ 
      XtNumber(syn_resources),/* num_syn_resources */
      NULL,                   /* syn_constraint_resources */ 
      0,                      /* num_syn_constraint_resources */
      XmInheritParentProcess, /* parent process */
      NULL                    /* extension */ 
   },
   /* XmComboBox class part */
   {
      NULL                    /* extension */ 
   }
};

WidgetClass xmComboBoxWidgetClass = (WidgetClass) &xmComboBoxClassRec;

/* ========================================================================== */
/* functions */
/*
  FUNCTION: CallSelectionCallbacks
  SYNOPSIS: static void CallSelectionCallbacks(Widget w, XtPointer client_data, XtPointer data)
  DESCRIPTION:
  This function calls all XmNselectionCallback routines set by the application.
  END:

  FIXTHIS: Does this need to have a XtCallbackProc signature?
*/
static void
CallSelectionCallbacks(Widget w, XtPointer client_data, XtPointer data)
{
   XmComboBoxCallbackStruct cbs;
   XmString v; 

   Unused(client_data);

   DEBUGOUT(_LtDebug(__FILE__, w, "%s:CallSelectionCallbacks ...\n",
   	__FILE__));

   if (CB_SelectionCB(w))
   {
      /* This is stored in a local because I don't trust programmers. ;^) */
      v = GetEditBoxValue(w); 

      /* Set default values. */
      cbs.reason = XmCR_SELECT;
      cbs.event = NULL;
      cbs.item_or_text = v;
      cbs.item_position = 0;

      /* Override default values if data was passed in. */
      if (data)
      {
         cbs.event = ((XmAnyCallbackStruct *) data)->event;

         if (((XmAnyCallbackStruct *) data)->reason == XmCR_BROWSE_SELECT)
         {
            cbs.item_position = ((XmListCallbackStruct *)data)->item_position;
         }
      }

      XtCallCallbackList(w,CB_SelectionCB(w),(XtPointer)&cbs);

      XmStringFree(v);
   }
}


static void 
CBActivate(
   Widget w, 
   XEvent *event, 
   String *params, 
   Cardinal *num_params)
{
   DEBUGOUT(_LtDebug(__FILE__, w, "%s:CBActivate ...\n",
   	__FILE__));
#if 0
   FindComboBox();
   CallSelectionCallbacks(); 
   if (CB_Type(w) == XmDROP_DOWN_COMBO_BOX ||
       CB_Type(w) == XmDROP_DOWN_LIST) 
   {
   	PopdownList();
   }
   ComboBoxParentProcess();
#endif
}


static void 
CBArmAndDropDownList(
   Widget w, 
   XEvent *event,
   String *params, 
   Cardinal *num_params)
{
   Widget w_passed;

   DEBUGOUT(_LtDebug(__FILE__, w, "%s:CBArmAndDropDownList ...\n",
   	__FILE__));
   w_passed = w;
   /* Find self because child may have generated event */
   w = FindComboBox(w);

   /* Was the button hit? */
   if (!IsEventInRectangle(event, &CB_HitRect(w)))
   {
      return;
   }

   CBDropDownList(w_passed,event,params,num_params);
}


static void 
CBCancel(
   Widget w, 
   XEvent *event,
   String *params, 
   Cardinal *num_params)
{
#if 0
   FindComboBox();
   ComboBoxParentProcess();
#endif
}


static void 
CBDisarm(
   Widget w,
   XEvent *event,
   String *params, 
   Cardinal *num_params)
{
#if 0
   FindComboBox();
   DrawArrow();
#endif
}


static void 
CBDropDownList(
   Widget w, 
   XEvent *event,
   String *params, 
   Cardinal *num_params)
{
   Widget w_passed;
   Position x, y, rootx, rooty;

   /* I did this because I keep wanting to use w instead of the local */
   w_passed = w;
   w = FindComboBox(w);
   if(!XmIsComboBox(w))
   {
      return;
   }

   /* need to adjust for highlight */
   x = CB_HighlightThickness(w);
   y = XtHeight(w) - CB_HighlightThickness(w); 

   /* translate the coordinates of the x and y position + offsets of combo */
   _LTTranslateCoords(w, x, y, &rootx, &rooty);

   /* set x and y position of popup shell */
   XtVaSetValues(CB_ListShell(w), XmNx, rootx, XmNy, rooty, NULL);

   /* lets be smart about the placement */
   _LTShellSmartPlacement(CB_ListShell(w));

   /* pop it up */
   _XmPopupSpringLoaded(CB_ListShell(w));

#if 0
   XtPopup(CB_ListShell(w), XtGrabNone);
#endif

#if 0
   PopdownList();
   CBDisarm();
#endif
}


static void 
CBFocusIn(Widget w, XEvent *event, String *params, Cardinal *num_params)
{
   DEBUGOUT(_LtDebug(__FILE__, w, "CBFocusIn ...\n"));

   w = FindComboBox(w);
   if(!XmIsComboBox(w))
   {
      return;
   }

   HighlightBorder(w);
}


static void 
CBFocusOut(Widget w, XEvent *event, String *params, Cardinal *num_params)
{
   DEBUGOUT(_LtDebug(__FILE__, w, "CBFocusOut ...\n"));

   w = FindComboBox(w);
   if(!XmIsComboBox(w))
   {
      return;
   }

   UnhighlightBorder(w);

#if 0
   CallSelectionCallbacks(); 
#endif
}


#if 0
static void 
CBGetSelectedItem(
   Widget w, 
   XEvent *event,
   String *params, 
   Cardinal *num_params)
{
#if 0
   GetEditBoxValue();
#endif
}
#endif


#if 0
static void 
CBGetSelectedPos(
   Widget w, 
   XEvent *event,
   String *params, 
   Cardinal *num_params)
{
}
#endif
   

static void 
CBListAction(
   Widget w, 
   XEvent *event,
   String *params, 
   Cardinal *num_params)
{
   char *action;
   int pos;
   int selected_pos_cnt;
   int *selected_pos;
   int item_cnt;

   DEBUGOUT(_LtDebug(__FILE__, w, "CBListAction ...\n"));

   w = FindComboBox(w);
   if(!XmIsComboBox(w))
   {
      return;
   }


/*
 CBListAction(Up)
 CBListAction(Down)
 CBListAction(ListBeginData)
 CBListAction(ListEndData)
 CBListAction(ListPrevPage)
 CBListAction(ListNextPage)
*/

   /* Is the number of parameters == 1 */
   if(num_params == NULL || *num_params != 1)
   {
      XmeWarning(w, _XmMsgComboBox_0007);
      return;
   }

   switch( *(params[0]) ) {
   case 'U' /* "Up" */:
   case 'D' /* "Down" */:

      XtVaGetValues(CB_List(w), XmNitemCount, &item_cnt,
                                NULL);
#if 0
      /* FIXME: these resources need to be implemented in the list box */
      XtVaGetValues(CB_List(w), XmNselectedPositions, &selected_pos,
                                XmNselectedPositionCount, &selected_pos_cnt,
                                NULL);

#else
      if (XmListGetSelectedPos(CB_List(w), &selected_pos, &selected_pos_cnt) == FALSE)
      {
         selected_pos_cnt = 0;
      }

#endif
      if(selected_pos_cnt <= 0)
      {
         pos = 1;
      }
      else
      {
         /* Check to see if we have a pointer (for the paranoid) */
         if (selected_pos)
         {
            pos = *selected_pos; /* pick first item */
#if 0  /* FIXME: XmNselectedPositions is not implemented. */
#else
            /* Free allocates memory by XmListGetSelectedPos for the selected_pos array */
            XtFree((char *)selected_pos);
#endif
         }
         else
         {
            pos = 1;
         }

         switch( *(params[0]) ) {
         case 'U' /* "Up" */: pos--; break;
         case 'D' /* "Down" */: pos++; if(pos > item_cnt) { pos = 1; } break;
         }
      }

      DEBUGOUT(_LtDebug(__FILE__, w, "XmListSelectPos(%s,%d,True)\n",XtName(CB_List(w)),pos));
      XmListSelectPos(CB_List(w), pos, True);
      break;

   case 'L' /* "ListBeginData" */:
            /* "ListEndData" */
            /* "ListPrevPage" */
            /* "ListNextPage" */
      action = params[0];
      XtCallActionProc(CB_List(w), action, event, NULL, 0);
      break;
   }

   DEBUGOUT(_LtDebug(__FILE__, w, "CBListAction return\n"));
}


#if 0
static void 
CBSetSelectedItem(
   Widget w, 
   XEvent *event,
   String *params, 
   Cardinal *num_params)
{
#if 0
   GetEditBoxValue();
   SetEditBoxValue();
#endif
}
#endif


#if 0
static void 
CBSetSelectedPos(
   Widget w, 
   XEvent *event,
   String *params, 
   Cardinal *num_params)
{
}
#endif


static void 
CBTextFocusOut(
   Widget w,
   XEvent *event,
   String *params, 
   Cardinal *num_params)
{
   w = FindComboBox(w);
   if(!XmIsComboBox(w))
   {
      return;
   }

   XtCallActionProc(w, "CBFocusOut", event, NULL, 0);
}


/*
  FUNCTION: ChangeManaged
  SYNOPSIS: static void ChangeManaged(Widget w)
  DESCRIPTION:
  This function performs a layout if the combo box is managed.  
  Otherwise it just returns. 
  END:
*/
static void 
ChangeManaged(Widget w)
{
   XtWidgetGeometry geo;
   XtGeometryResult result;

   DEBUGOUT(_LtDebug(__FILE__, w, "ChangeManaged ...\n"));

#if 0
   if (!XtIsManaged(w))
   {
      return;
   }
#endif

   /* Compute our preferred size */
   ComputeSize(w, &geo);

   /* Make geometry request to parent */
   result = _XmMakeGeometryRequest(w, &geo);

   if (result != XtGeometryNo)
   {
      XtWidth(w) = geo.width;
      XtHeight(w) = geo.height;
   }

   /* Layout children */
   DoLayout(w);
}


/*
  FUNCTION: CheckMinimalSize
  SYNOPSIS: static void CheckMinimalSize(Widget w, XtWidgetGeometry *reply);
  DESCRIPTION:
  This function does not check but returns the minimal size.
  END:
*/
static void
CheckMinimalSize(Widget w, XtWidgetGeometry *reply)
{
   reply->request_mode = (CWWidth|CWHeight);

   reply->width = 0;
   reply->height = 0;

   if (CB_Type(w) != XmCOMBO_BOX) 
   {
      reply->width += CB_HitRect(w).width;
      reply->height += CB_HitRect(w).height;

      /* don't forget to include the arrow spacing to the width */
      reply->width += CB_ArrowSpacing(w);
   }
}


#if 0
static void
CheckSetRenderTable(Widget w)
{
   /* FIXTHIS: CheckSetRenderTable needs to be implemented */
   Unused(w);
}
#endif


/*
  FUNCTION: ClassInitialize
  SYNOPSIS: static void ClassInitialize()
  DESCRIPTION:
  This function initialize class data.
  END:
*/
static void
ClassInitialize(void)
{
   /* parse translations */
   parsed_list_translations = XtParseTranslationTable(
      _XmComboBox_dropDownListTranslations);

   parsed_text_focus_translations = XtParseTranslationTable(
      _XmComboBox_textFocusTranslations);

   /* parse accelerators */
   parsed_accelerators = XtParseAcceleratorTable(
      _XmComboBox_defaultAccelerators);

   parsed_list_accelerators = XtParseAcceleratorTable(
      _XmComboBox_dropDownComboBoxAccelerators);
}


static void
ClassPartInitialize(WidgetClass widget_class)
{
   Unused(widget_class);
}


static void
ComboBoxParentProcess(Widget w)
{
   /* FIXTHIS: ComboBoxParentProcess needs to be implemented. */
   Unused(w);

#if 0
   PopdownList();
   CBDisarm();
   GetEditBoxValue();
   CallSelectionCallbacks();
#endif
}


/*
  FUNCTION: ComputeArrowSize
  SYNOPSIS: static Dimension ComputeArrowSize(Widget w)
  DESCRIPTION:
  This function does some magic to determine the size of the ComboBox's arrow.
  END:
*/
static Dimension
ComputeArrowSize(Widget w)
{
   Dimension arrowSize = 0;
   int fudge_factor = 1;

   DEBUGOUT(_LtDebug(__FILE__, w, "ComputeArrowSize ...\n"));

   if (CB_Type(w) == XmDROP_DOWN_LIST
       || CB_Type(w) == XmDROP_DOWN_COMBO_BOX)
   {
      if (Prim_ShadowThickness(CB_EditBox(w)) != 0)
      {
         fudge_factor = Prim_ShadowThickness(CB_EditBox(w));
      }

      arrowSize = XtHeight(CB_EditBox(w))
                - ((2*(TextF_MarginHeight(CB_EditBox(w)))) 
                  + (2*(XtBorderWidth(CB_EditBox(w))))
                  + (2*(Prim_HighlightThickness(CB_EditBox(w))))
                  );

      /* This calculation was a pain to figure out. */
      arrowSize -= ((TextF_FontAscent(CB_EditBox(w))
                    + fudge_factor
                    ) 
                   - (TextF_FontAverageWidth(CB_EditBox(w)) 
                     + (2*(TextF_MarginHeight(CB_EditBox(w))))
                     )
                   ); 
   }

   DEBUGOUT(_LtDebug(__FILE__, w, "ComputeArrowSize: text margin height(%d) ...\n",
      TextF_MarginHeight(CB_EditBox(w))));
   DEBUGOUT(_LtDebug(__FILE__, w, "ComputeArrowSize: text margin width(%d) ...\n",
      TextF_MarginWidth(CB_EditBox(w))));
   DEBUGOUT(_LtDebug(__FILE__, w, "ComputeArrowSize: text height(%d) ...\n",
      XtHeight(CB_EditBox(w))));
   DEBUGOUT(_LtDebug(__FILE__, w, "ComputeArrowSize: text width(%d) ...\n",
      XtWidth(CB_EditBox(w))));
   DEBUGOUT(_LtDebug(__FILE__, w, "ComputeArrowSize: text border width(%d) ...\n",
      XtBorderWidth(CB_EditBox(w))));
   DEBUGOUT(_LtDebug(__FILE__, w, "ComputeArrowSize: text highlight thickness(%d) ...\n",
      Prim_HighlightThickness(CB_EditBox(w))));
   DEBUGOUT(_LtDebug(__FILE__, w, "ComputeArrowSize: text shadow thickness(%d) ...\n",
      Prim_ShadowThickness(CB_EditBox(w))));
   DEBUGOUT(_LtDebug(__FILE__, w, "ComputeArrowSize: text ascent (%d) ...\n",
      TextF_FontAscent(CB_EditBox(w))));
   DEBUGOUT(_LtDebug(__FILE__, w, "ComputeArrowSize: text average char width (%d) ...\n",
      TextF_FontAverageWidth(CB_EditBox(w)))); 

   DEBUGOUT(_LtDebug(__FILE__, w, "ComputeArrowSize: arrow_size(%d) ...\n", arrowSize));

   return arrowSize;
}


/*
  FUNCTION: PreferredGeo
  SYNOPSIS: static void PreferredGeo(Widget w, XtWidgetGeometry *)
  DESCRIPTION:
  This function returns the preferred size of the widget.
  END:
*/
static void
PreferredGeo(Widget w, XtWidgetGeometry *reply)
{
   XtQueryGeometry(w, NULL, reply);
   DEBUGOUT(_LtDebug(__FILE__, w,
                     "get preferred size of child %s\n",
                     _LtDebugWidgetGeometry2String(reply)));
}


/*
  FUNCTION: AdjustWidth
  SYNOPSIS: static Dimension AdjustWidth(Widget w)
  DESCRIPTION:
  This function returns the adjustment needed to the width
  to allow space for the margin, shadow, and highlight.
  END:
*/
static Dimension
AdjustWidth(Widget w)
{
   Dimension width = 0;

   /* adjust for margins */
   width += 2 * CB_MarginWidth(w);

   /* adjust for highlight thickness */
   width += 2 * CB_HighlightThickness(w);

   /* adjust for shadow thickness */
   width += 2 * MGR_ShadowThickness(w);

   return width;
}


/*
  FUNCTION: AdjustHeight
  SYNOPSIS: static Dimension AdjustHeight(Widget w)
  DESCRIPTION:
  This function returns the adjustment needed to the height
  to allow space for the margin, shadow, and highlight.
  END:
*/
static Dimension
AdjustHeight(Widget w)
{
   Dimension height = 0;

   /* adjust for margins */
   height += 2 * CB_MarginHeight(w);

   /* adjust for highlight thickness */
   height += 2 * CB_HighlightThickness(w);

   /* adjust for shadow thickness */
   height += 2 * MGR_ShadowThickness(w);

   return height;
}


/*
  FUNCTION: ComputeSizeComboBox
  SYNOPSIS: static void ComputeSizeComboBox(Widget w, XtWidgetGeometry *)
  DESCRIPTION:
  This function determines how big the ComboBox needs to be.
  END:
*/
static void
ComputeSizeComboBox(Widget w, XtWidgetGeometry *reply)
{
   XtWidgetGeometry e_pref, s_pref;

   PreferredGeo(CB_EditBox(w), &e_pref);
   PreferredGeo(CB_ScrolledW(w), &s_pref);

   /* start with the size of edit box */
   reply->width = e_pref.width;
   reply->height = e_pref.height;

#if 0
   /* start with the size of edit box */
   reply->width = CB_IdealEBWidth(w);
   reply->height = CB_IdealEBHeight(w);
#endif

   /* add the height of scrolled window if type is XmCOMBO_BOX */
   /*
      REMARK:
      06/2/97: The initial scrolled window widget geometry is incorrect.  
      Hence, the size computed by this function will be incorrect.  Don't
      try to fix this function.  The problem is with the scrolled window 
      geometry.  Test 14 in directory test/Xm/scrolledwindow demonstrates 
      the problem.
      END:
   */
   reply->height += s_pref.height; 

   /* add adjustments for other special effects */
   reply->width += AdjustWidth(w);
   reply->height += AdjustHeight(w);
}


/*
  FUNCTION: ComputeSizeDropDown
  SYNOPSIS: static void ComputeSizeDropDown(Widget w, XtWidgetGeometry *)
  DESCRIPTION:
  This function determines how big the DropDown needs to be.
  END:
*/
static void 
ComputeSizeDropDown(Widget w, XtWidgetGeometry *reply)
{
   XtWidgetGeometry e_pref, s_pref;

   /* Get the preferred size of the edit box child */
   PreferredGeo(CB_EditBox(w), &e_pref);
   PreferredGeo(CB_ScrolledW(w), &s_pref);
   PreferredGeo(CB_ListShell(w), &s_pref);

   /* start with the size of edit box */
   reply->width = e_pref.width;
   reply->height = e_pref.height;

#if 0
   /* start with the size of edit box */
   reply->width = CB_IdealEBWidth(w);
   reply->height = CB_IdealEBHeight(w);
#endif

   /*
      REMARK:
      If a drop down and arrow size is greater then ideal
      edit box height then use the arrow size as the edit box height.
      END:
   */
   if (reply->height < CB_ArrowSize(w))
   {
      reply->height = CB_ArrowSize(w);
   }

   DEBUGOUT(_LtDebug(__FILE__, w, "ComputeSize: arrow_spacing(%d) ...\n", 
            CB_ArrowSpacing(w)));
   DEBUGOUT(_LtDebug(__FILE__, w, "ComputeSize: arrow_size(%d) ...\n",
            CB_ArrowSize(w)));

   /* add the width and spacing for the arrow button */
   reply->width += CB_ArrowSpacing(w);
   reply->width += CB_ArrowSize(w);

   /* add adjustments for other special effects */
   reply->width += AdjustWidth(w);
   reply->height += AdjustHeight(w);
}


/*
  FUNCTION: ComputeSize
  SYNOPSIS: static void ComputeSize(Widget w, XtWidgetGeometry *)
  DESCRIPTION:
  This function determines how big the ComboBox needs to be.
  END:
*/
static void
ComputeSize(Widget w, XtWidgetGeometry *reply)
{
   DEBUGOUT(_LtDebug(__FILE__, w, "ComputeSize ...\n"));

   if (CB_Type(w) == XmCOMBO_BOX)
   {
      ComputeSizeComboBox(w, reply);
   }
   else
   {  /* assume calculation for XmDROP_DOWN_LIST and XmDROP_DOWN_COMBO_BOX
         are the same.
      */
      ComputeSizeDropDown(w, reply);
   }

   reply->request_mode = (CWWidth|CWHeight);
}


static void
ConstraintDestroy(Widget w)
{
   DEBUGOUT(_LtDebug(__FILE__, w, "ConstraintDestroy ...\n"));

   /* FIXTHIS: ConstraintDestroy needs to be implemented. */
   Unused(w);
}


/*
  FUNCTION: CreateArrowGC
  SYNOPSIS: static void CreateArrowGC(Widget w)
  DESCRIPTION:
  This function creates the arrow GC.
  END:
*/
static void
CreateArrowGC(Widget w)
{
   XGCValues values;
   XtGCMask mask;

   DEBUGOUT(_LtDebug(__FILE__, w, "CreateArrowGC ...\n"));

   mask = GCForeground | GCBackground;

   /* values.foreground = MGR_Foreground(w); */
   values.foreground = XtBackground(w);
   values.background = XtBackground(w);

   /* these GC's get used for shadow drawing, so set 'em up */
   mask |= GCLineWidth | GCLineStyle | GCJoinStyle | GCCapStyle;
   values.line_width = 1;
   values.line_style = LineSolid;
   values.join_style = JoinMiter;
   values.cap_style = CapButt;

   CB_ArrowGC(w) = XtGetGC(w, mask, &values);
}


/*
  FUNCTION: CreateChildren
  SYNOPSIS: static void CreateChildren(Widget w)
  DESCRIPTION:
  This function calls other functions which create the children 
  of the combobox widget.
  END:
*/
static void
CreateChildren(Widget w)
{
   DEBUGOUT(_LtDebug(__FILE__, w, "CreateChildren ...\n"));

   CreateEditBox(w);
   CreatePulldown(w);
   CreateScrolledList(w);
}


/*
  FUNCTION: CreateEditBox
  SYNOPSIS: static void CreateEditBox(Widget w)
  DESCRIPTION:
  This function creates the text field widget for the combobox widget.
  END:
*/
static void
CreateEditBox(Widget w)
{
   Widget text;
   Arg args[10];
   int n = 0;
 
   DEBUGOUT(_LtDebug(__FILE__, w, "CreateEditBox ...\n"));

   if (CB_Type(w) == XmDROP_DOWN_LIST)
   {  /* force these settings */
      XtSetArg(args[n], XmNeditable, False); n++;
      XtSetArg(args[n], XmNcursorPositionVisible, False); n++;
      XtSetArg(args[n], XmNshadowThickness, 0); n++;
   }
   if (CB_Type(w) == XmCOMBO_BOX
       || CB_Type(w) == XmDROP_DOWN_COMBO_BOX)
   {  /* force these settings */
      XtSetArg(args[n], XmNeditable, True); n++;
      XtSetArg(args[n], XmNcursorPositionVisible, True); n++;
      XtSetArg(args[n], XmNeditMode, XmSINGLE_LINE_EDIT); n++;
   }

   XtSetArg(args[n], XmNnavigationType, XmNONE); n++;
   XtSetArg(args[n], XmNhighlightThickness, 0); n++;
   XtSetArg(args[n], XmNborderWidth, 0); n++;
            
   text = XmCreateTextField(w, CB_TEXT_CN, args, n);

   SetIdealTextSize(w);

   XtManageChild(text);

   XtAddCallback(text,
                 XmNvalueChangedCallback, 
                 (XtCallbackProc) TextChangedCB,
                 (XtPointer) w);
}

/*
  FUNCTION: CreatePulldown
  SYNOPSIS: static void CreatePulldown(Widget w)
  DESCRIPTION:
  This function creates a GrabShell widget for the combobox widget if
  the type is not XmCOMBO_BOX.  If the type is XmCOMBO_BOX the list_shell 
  member in the XmComboBoxPart structure will be set to the combobox widget. 
  END:

  FIXTHIS: Confirm if Motif 2.0 does it this way.
*/
static void
CreatePulldown(Widget w)
{
   Arg args[10];
   int n = 0;

   DEBUGOUT(_LtDebug(__FILE__, w, "CreatePulldown ...\n"));

   if (CB_Type(w) == XmDROP_DOWN_LIST
       || CB_Type(w) == XmDROP_DOWN_COMBO_BOX)
   {
      XtSetArg(args[n], XmNownerEvents, True); n++;
      
#if 0
      /* FIXTHIS: GrabModeSync handling is broken in the grab shell. */
      XtSetArg(args[n], XmNgrabStyle, GrabModeSync); n++;
#endif

      /* Despite the function name a GrabShell widget is used */
      CB_ListShell(w) = XmCreateGrabShell(w,CB_LIST_SHELL_CN,args,n);

#if 1
      /* Why am I doing this? */

      /* Add focus handler */
      XmeAddFocusChangeCallback(CB_ListShell(w), 
                                (XtCallbackProc) ShellFocusMovedCB, 
                                (XtPointer) w);
#endif

      XtAddCallback(CB_ListShell(w),
                    XmNpopdownCallback, 
                    (XtCallbackProc) ShellPopdownCB,
                    (XtPointer) w);

      XtAddCallback(CB_ListShell(w),
                    XmNpopupCallback, 
                    (XtCallbackProc) ShellPopupCB,
                    (XtPointer) w);

   }
   else /* if (CB_Type(w) == XmCOMBO_BOX) then make self the list shell */
   {
      CB_ListShell(w) = w;
   }
}


/*
  FUNCTION: CreateScrolledList
  SYNOPSIS: static void CreateScrolledList(Widget parent)
  DESCRIPTION:
  This function creates the scrolled list widget for the combobox widget.
  END:
*/
static void
CreateScrolledList(Widget w)
{
   Arg args[15];
   int n = 0;

   DEBUGOUT(_LtDebug(__FILE__, w, "CreateScrolledList ...\n"));

   /* force settings */
   if (CB_Type(w) == XmCOMBO_BOX)
   {  
      XtSetArg(args[n], XmNtraversalOn, False); n++;
   }

   if (CB_Type(w) == XmDROP_DOWN_COMBO_BOX)
   {
      XtSetArg(args[n], XmNhighlightThickness, 2); n++;
   }
   else
   {
      XtSetArg(args[n], XmNhighlightThickness, 0); n++;
   }

   XtSetArg(args[n], XmNborderWidth, 0); n++;
   XtSetArg(args[n], XmNnavigationType, XmNONE); n++;
   XtSetArg(args[n], XmNselectionPolicy, XmBROWSE_SELECT); n++;
   XtSetArg(args[n], XmNlistSizePolicy, XmVARIABLE); n++;
   XtSetArg(args[n], XmNspacing, 0); n++;
   XtSetArg(args[n], XmNvisualPolicy, XmVARIABLE); n++;
   XtSetArg(args[n], XmNselectedPositions, NULL); n++;
   XtSetArg(args[n], XmNselectedItemCount, 0); n++;

   XtSetArg(args[n], XmNshadowThickness, 0); n++;

   /* 
      REMARK:
      XmCreateScrolledList returns an unmanage list. 
      The parent of the list is a managed scrolled window.
      END:
    */
   CB_List(w) = XmCreateScrolledList(CB_ListShell(w),CB_LIST_CN,args,n);
   XtManageChild(CB_List(w));

   /* This may cause a problem if XtParent is a macro */
   CB_ScrolledW(w) = XtParent(CB_List(w));

   /* Setup callback for combobox. */
   XtAddCallback(CB_List(w),
                 XmNbrowseSelectionCallback, 
                 (XtCallbackProc) ListSelectionCB,
                 (XtPointer) w);

#if 0
   SetEditBoxValue();
#endif
}


/*
  FUNCTION: Destroy
  SYNOPSIS: static void Destroy(Widget w)
  DESCRIPTION:
  This function releases allocated memory done by the widgets instance.
  END:
*/
static void
Destroy(Widget w)
{
   DEBUGOUT(_LtDebug(__FILE__, w, "Destroy ...\n"));

   if (CB_SelectedItem(w))
   {
      XmStringFree(CB_SelectedItem(w));
   }

#if 0
   /* 
      FIX ME: ???
      CB_ListShell(w) is a child of the combobox so it will clean 
      itself up when destroyed.  Please correct me if I'm wrong.
      pgw@hungry.com
    */
   /* 
      REMARK:
      Remove a focus change handler
      if we are a DROP_DOWN_LIST or DROP_DOWN_COMBO_BOX 
      END:
   */
   if (CB_Type(w) == XmDROP_DOWN_LIST
       || CB_Type(w) == XmDROP_DOWN_COMBO_BOX)
   {
      /* remove focus handler */
      XmeRemoveFocusChangeCallback(CB_ListShell(w),
                                   (XtCallbackProc)ShellFocusMovedCB, 
                                   (XtPointer) w);
   }
#endif

   /* Remove event handler */
   RemoveHitAreaHandler(w);

   /* Remove focus handler */
   RemoveFocusChangeHandler(w);

   /* Release GC's */
   XtReleaseGC(w,CB_ArrowGC(w));
}


/*
  FUNCTION: DetectAndFixBadSettings
  SYNOPSIS: static void DetectAndFixBadSettings(Widget w)
  DESCRIPTION:
  This function detects and fixes bad settings.
  END:
*/
static void
DetectAndFixBadSettings(Widget w)
{
   if (CB_Type(w) != XmDROP_DOWN_LIST) 
   {
      /* Only drop down lists can have the following: */
      if(CB_MatchBehavior(w) == XmQUICK_NAVIGATE)
      {
         CB_MatchBehavior(w) = (unsigned char) XmNONE;
         XmeWarning(w, _XmMsgComboBox_0006);
      }
   }
}


/*
  FUNCTION: DetectAndFixChangesToReadOnlySettings
  SYNOPSIS: static void DetectAndFixChangesToReadOnlySettings(Widget ow, Widget nw)
  DESCRIPTION:
  This function detects and fixes bad settings.
  END:
*/
static void
DetectAndFixChangesToReadOnlySettings(Widget ow, Widget nw)
{
   if (CB_Type(ow) != CB_Type(nw))
   {
      CB_Type(nw) = CB_Type(ow);

      XmeWarning(ow, _XmMsgComboBox_0001);
   }
}


/*
  FUNCTION: DoLayout
  SYNOPSIS: static void DoLayout(Widget w)
  DESCRIPTION:
  This function repositions child windows according to its own geometry.
  END:
*/
static void
DoLayout(Widget w)
{
   DEBUGOUT(_LtDebug(__FILE__, w, "DoLayout ...\n"));

   /* if the kids are not valid then return */
   if(!KidsValid(w))
   {
      return;
   }

   /* adjust size and location of the kids */
   AdjustKids(w);
}


/*
  FUNCTION: DrawArrow
  SYNOPSIS: static void DrawArrow(Widget w)
  DESCRIPTION:
  This function draws the arrow button.  The combo box widget simulates 
  its own arrow button. 
  END:
*/
static void
DrawArrow(Widget w)
{
   Position normal_arrow_x, 
            normal_arrow_y, 
            normal_shadow_x, 
            normal_shadow_y;
   Dimension normal_arrow_width, 
             normal_arrow_height, 
             normal_arrow_thickness, 
             normal_shadow_width, 
             normal_shadow_height,
             normal_shadow_thickness;
   Position margin_left, margin_top;


   DEBUGOUT(_LtDebug(__FILE__, w, "DrawArrow ...\n"));

   /*
      REMARK:
      Assume the arrow's x, y, width and height values have not changed.  
      They will be updated by a resize event which also causes a layout.
      END:
   */

   DEBUGOUT(_LtDebug(__FILE__, 
            w, 
            "DrawArrow: x(%d), y(%d), width(%d), height(%d) ...\n",
            (int) CB_HitRect(w).x,
            (int) CB_HitRect(w).y,
            (int) CB_HitRect(w).width,
            (int) CB_HitRect(w).height));

   /* Calculate the line height */
   CalcCBLineHeight(w, &normal_shadow_height);

   /* Calculate the arrow size */
   normal_arrow_width = CB_ArrowSize(w) - normal_shadow_height;
   normal_arrow_height = CB_ArrowSize(w) - normal_shadow_height;

   /*
      REMARK:
      Either the ArrowSpacing or the margin width and height
      or both effect the actual size.  Need to test this.
      END:
   */

   /* Calculate arrow position */ 
   margin_top = CB_HitRect(w).height - 
                (normal_arrow_height + normal_shadow_height + 1);
   if(margin_top) margin_top = margin_top / 2;
   margin_left = (CB_HitRect(w).width - normal_arrow_width);
   if(margin_left) margin_left = margin_left / 2;

   normal_arrow_y = CB_HitRect(w).y + margin_top;
   normal_arrow_x = CB_HitRect(w).x + margin_left;

   /* make sure that the arrow will be visible */
   normal_arrow_thickness = MGR_ShadowThickness(w);
   if (normal_arrow_thickness < 1)
   {
      normal_arrow_thickness = 1;
   }

   DEBUGOUT(_LtDebug(__FILE__, 
            w, 
            "DrawArrow: computed: x(%d), y(%d), width(%d), height(%d)\n",
            (int) normal_arrow_x,
            (int) normal_arrow_y,
            (int) normal_arrow_width,
            (int) normal_arrow_height));

   XmeDrawArrow(XtDisplay(w),               /* display */
                XtWindow(w),                /* window */
                MGR_TopShadowGC(w),         /* top gc */
                MGR_BottomShadowGC(w),      /* bottom gc */
                CB_ArrowGC(w),              /* fill gc */
                normal_arrow_x,             /* position x */
                normal_arrow_y,             /* position y */
                normal_arrow_width,         /* width */
                normal_arrow_height,        /* height */
                normal_arrow_thickness,     /* shadow thickness */
                XmARROW_DOWN);

   normal_shadow_width = normal_arrow_width;

   normal_shadow_y = normal_arrow_height + normal_arrow_y;
   normal_shadow_x = normal_arrow_x;

   /*
      REMARK:
      11/14/97 (pgw)
      The line shadow thickness must starts at 1 otherwise we would not
      be drawing a line.
      END:
   */
   normal_shadow_thickness = 1; /* starts at 1 */

   if (normal_shadow_height > 3)
   {
      normal_shadow_thickness ++; /* become 2 */
   }

   /* Draw shadows */
   XmeDrawShadows(XtDisplay(w),               /* display */
                  XtWindow(w),                /* window */
                  MGR_TopShadowGC(w),         /* top gc */
                  MGR_BottomShadowGC(w),      /* bottom gc */
                  normal_shadow_x,            /* position x */
                  normal_shadow_y,            /* position y */
                  normal_shadow_width,        /* width */
                  normal_shadow_height,       /* height */
                  normal_shadow_thickness,    /* shadow thickness */
                  XmSHADOW_ETCHED_OUT_DASH);  /* shadow type */
}


/*
  FUNCTION: DrawBorder
  SYNOPSIS: static void DrawBorder(Widget w)
  DESCRIPTION:
  This function draws the border around the combobox by calling
  either HighlightBorder or UnhighlightBorder.  Typically called from
  routine Redisplay.
  END:
*/
static void
DrawBorder(Widget w)
{
   DEBUGOUT(_LtDebug(__FILE__, w, "DrawBorder ...\n"));

   if (CB_Highlighted(w) == True)
   {
      HighlightBorder(w);
   }
   else
   {
      UnhighlightBorder(w);
   }
}


static void
DrawComboBox(Widget w)
{
   /* clear the window before doing the redraw */
   XClearWindow(XtDisplayOfObject(w), XtWindowOfObject(w));

   /* Draw the arrow if I'm not a XmCOMBO_BOX */
   if (CB_Type(w) != XmCOMBO_BOX)
   {
      DrawArrow(w);
   }

   DrawShadows(w);

   DrawBorder(w);
}


/*
  FUNCTION: DrawShadows
  SYNOPSIS: static void DrawShadows(Widget w)
  DESCRIPTION:
  This function draws the shadow around the combobox by calling
  XmeDrawShadows.
  END:
*/
static void
DrawShadows(Widget w)
{
   Position normal_shadow_x, normal_shadow_y;
   Dimension normal_shadow_width, normal_shadow_height;

   DEBUGOUT(_LtDebug(__FILE__, w, "DrawShadows ...\n"));

   /* adjust for highlight and margins */
   normal_shadow_x = CB_HighlightThickness(w);
   normal_shadow_y = CB_HighlightThickness(w);
   normal_shadow_width = XtWidth(w) - 2*normal_shadow_x;
   normal_shadow_height = XtHeight(w) - 2*normal_shadow_y;

   DEBUGOUT(_LtDebug(__FILE__, 
            w, 
            "DrawShadows: x(%d), y(%d), width(%d), height(%d) ...\n",
            (int) normal_shadow_x,
            (int) normal_shadow_y,
            (int) normal_shadow_width,
            (int) normal_shadow_height));

   XmeDrawShadows(XtDisplayOfObject(w),       /* display */
                  XtWindowOfObject(w),        /* window */
                  MGR_TopShadowGC(w),         /* top gc */
                  MGR_BottomShadowGC(w),      /* bottom gc */
                  normal_shadow_x,            /* position x */
                  normal_shadow_y,            /* position y */
                  normal_shadow_width,        /* width */
                  normal_shadow_height,       /* height */
                  MGR_ShadowThickness(w),     /* shadow thickness */
                  XmSHADOW_ETCHED_OUT_DASH);  /* shadow type */
}


/*
  FUNCTION: _FindComboBox
  SYNOPSIS: static Widget _FindComboBox(Widget w)
  DESCRIPTION:
  This function finds the combobox widget from the supplied widget.
  The supplied widget must be a child or descendent of the combobox
  otherwise NULL is returned.
  END:
*/
static Widget
_FindComboBox(Widget w)
{
   Widget ww = w;

   DEBUGOUT(_LtDebug(__FILE__, w, "_FindComboBox ...\n"));

   while(ww && !XmIsComboBox(ww))
   {
      w = ww;
      ww = XtParent(w); 
   }

   return ww;
}


/*
  FUNCTION: FindComboBox
  SYNOPSIS: static Widget FindComboBox(Widget w)
  DESCRIPTION:
  This function finds the combobox widget from the supplied widget.
  The supplied widget must be a child or descendent of the combobox
  otherwise an error is reported.
  END:
*/
static Widget
FindComboBox(Widget w)
{
   Widget self;

   self = _FindComboBox(w); 

   /* Check for error */
   if (self == NULL)
   {  /* How could something like this possibly happen? */
      /* I'm guessing this generates message 0008 */
      XmeWarning(w, _XmMsgComboBox_0008);
      self = w;
   }

   return self;
}


/*
  FUNCTION: _FindVendorShell
  SYNOPSIS: static Widget _FindVendorShell(Widget w)
  DESCRIPTION:
  This function finds the vendor shell widget from the supplied widget.
  The supplied widget must be a child or descendent of the combobox
  otherwise NULL is returned.
  END:
*/
static Widget
_FindVendorShell(Widget w)
{
   Widget ww = w;

   DEBUGOUT(_LtDebug(__FILE__, w, "_FindVendorShell ...\n"));

   while(ww && !XtIsVendorShell(ww))
   {
      w = ww;
      ww = XtParent(w); 
   }

   return ww;
}


static XtGeometryResult 
GeometryManager(Widget w, XtWidgetGeometry *request, XtWidgetGeometry *reply)
{
   DEBUGOUT(_LtDebug2(__FILE__, XtParent(w), w, "%s:GeometryManager %s\n",
   		__FILE__,
   		_LtDebugWidgetGeometry2String(request)));

   Unused(w);
   Unused(request);
   Unused(reply);

   /* FIXTHIS: GeometryManager needs to be implemented. */
   if (reply)
       *reply = *request;
   return (XtGeometryResult) XtGeometryYes;

#if 0
   CheckMinimalSize();
#endif
}


/*
  FUNCTION: GetEditBoxValue
  SYNOPSIS: static XmString GetEditBoxValue(Widget w)
  DESCRIPTION:
  This function returns the current value of the edit box.
  It is the responsibility of the call function to free the
  pointer returned by this function.
  END:
*/
static XmString
GetEditBoxValue(Widget w)
{
   XmString s;
   char *v;

   XtVaGetValues(CB_EditBox(w),XmNvalue, &v, NULL);
   s = XmCvtCTToXmString(v);
   return s;
}

/*
  FUNCTION: HighlightBorder
  SYNOPSIS: static void HighlightBorder(Widget w)
  DESCRIPTION:
  This function draws the highlight around the combobox. 
  END:
*/
static void
HighlightBorder(Widget w)
{
   int highlight_offset = 0; /* adjust as needed (<0 makes it bigger) */

   /* check to see if thickness is zero */
   if(CB_HighlightThickness(w) == 0)
   {
      return;
   }

   DEBUGOUT(_LtDebug(__FILE__, w, "HighlightBorder ...\n"));

   XmeDrawHighlight(XtDisplayOfObject(w),
                    XtWindowOfObject(w),
                    MGR_HighlightGC(w),
                    highlight_offset,
                    highlight_offset,
                    XtWidth(w) - 2*highlight_offset,
                    XtHeight(w) - 2*highlight_offset,
                    CB_HighlightThickness(w));

   CB_Highlighted(w) = True;
}


/*
  FUNCTION: Initialize
  SYNOPSIS: static void Initialize(Widget,Widget,ArgList,Cardinal *)
  DESCRIPTION:
  This function initializes the instance data.
  END:
*/
static void
Initialize(Widget request,
           Widget c_new,
           ArgList args,
           Cardinal *num_args)
{
	int	i;

	DEBUGOUT(_LtDebug(__FILE__, c_new,
		"%s:Initialize: %i args\n"
		"\trequest X %5i Y %5i W %5i H %5i\n"
		"\t  c_new X %5i Y %5i W %5i H %5i\n",
		__FILE__,
		*num_args,
		XtX(request), XtY(request),
		XtWidth(request), XtHeight(request),
		XtX(c_new), XtY(c_new),
		XtWidth(c_new), XtHeight(c_new)));
	DEBUGOUT(_LtDebugPrintArgList(__FILE__, c_new, args, *num_args, False));

	/* initialize internal data */
	CB_Highlighted(c_new) = False;
	CB_ArrowPressed(c_new) = False;

	CreateArrowGC(c_new);

	/* Fix bad settings */
	DetectAndFixBadSettings(c_new);

	/* FIXTHIS: Some of the args maybe needed by the children. */
	CreateChildren(c_new);

	/* Add Hit Area Handler only once */
	SetHitAreaHandler(c_new);

	/* Set focus change handler */
	SetFocusChangeHandler(c_new);

	DEBUGOUT(_LtDebug(__FILE__, c_new,
		"Initialize: IdealEBHeight(%d) ...\n",
		CB_IdealEBHeight(c_new)));
	DEBUGOUT(_LtDebug(__FILE__, c_new,
		"Initialize: IdealEBWidth(%d) ...\n",
		CB_IdealEBWidth(c_new)));

	/* The following must be done after creating the children */
	/* set the default values if not set */
	if (CB_ArrowSize(c_new) == 0) {
		CB_ArrowSize(c_new) = ComputeArrowSize(c_new);
	}
	if (CB_ArrowSpacing(c_new) == 0) {
	/*
	 * REMARK:
	 * The default value for XmNarrowSpacing is obtained from
	 * the XmNmarginWidth resource.
	 * END:
	 */
		if (CB_Type(c_new) == XmDROP_DOWN_LIST
			|| CB_Type(c_new) == XmDROP_DOWN_COMBO_BOX) {
			CB_ArrowSpacing(c_new) = CB_MarginWidth(c_new);
		}
	}

	for (i=0; i<*num_args; i++) {
		if (strcmp(args[i].name, XmNvisibleItemCount) == 0) {
			XtVaSetValues(CB_List(c_new),
				XmNvisibleItemCount, args[i].value,
				NULL);;
		}
	}
}


/*
  FUNCTION: InsertChild
  SYNOPSIS: static void InsertChild(Widget w)
  DESCRIPTION:
  This function reports any errors and then calls the composite's insert 
  child routine to do the actual insert.
  END:
*/
static void
InsertChild(Widget w)
{
   Widget parent = XtParent(w);

   DEBUGOUT(_LtDebug2(__FILE__, (Widget)parent, w, "InsertChild\n"));

   /*
      REMARK: 
      The combobox in 2.0 checks to see if the child count is greater then 
      two before inserting another child.  If it is greater then it prints 
      a warning.
      END:
    */ 
   if (MGR_NumChildren(parent) > 2)
   {
      XmeWarning(parent, _XmMsgComboBox_0000);
   }

   (*superclass->composite_class.insert_child)(w);
}


/*
  FUNCTION: ListSelectionCB
  SYNOPSIS: static void ListSelectionCB(Widget w, XtPointer client_data, XtPointer data)
  DESCRIPTION:
  This function handles a list selection.  The parameter client_data should
  always be the combo box widget. 
  END:
*/
static void
ListSelectionCB(Widget w, XtPointer client_data, XtPointer data)
{
   XmListCallbackStruct *cbs = (XmListCallbackStruct *) data;
   Widget cb = (Widget) client_data;
   XmString v;

   DEBUGOUT(_LtDebug(__FILE__, w, "%s:ListSelectionCB ...\n",
   	__FILE__));

   v = GetEditBoxValue(cb);

   /* See if the edit box value needs to change. */
   if (XmStringCompare(v,cbs->item) == FALSE)
   {
      /* set string in edit box */
      SetEditBoxValue(cb, cbs->item);
   }

   /* Squash leak */
   XmStringFree(v);

   /* Set XmNselectedItem */
   if (CB_SelectedItem(w))
   {    
      XmStringFree(CB_SelectedItem(w));
   }
   /* 
    * Need to make a copy here because we are not
    * managing the memory allocated to the cbs structure.
    */
   CB_SelectedItem(w) = XmStringCopy(cbs->item);

   /* Call internal selection callback list */
   CallSelectionCallbacks(cb, client_data, data);

   if (CB_Type(cb) == XmCOMBO_BOX)
   {
      return;
   }

   PopdownList(cb, cbs->event);

   CBDisarm(cb,cbs->event,NULL,NULL);
} 


static void
PopdownList(Widget w, XEvent *event)
{
   DEBUGOUT(_LtDebug(__FILE__, w, "%s:PopdownList ...\n",
   	__FILE__));

   XtCallActionProc(CB_ListShell(w), "GrabShellPopdown", event, NULL, 0);
}


/*
  FUNCTION: PopupEH
  SYNOPSIS: static void PopupEH(Widget, XtPointer, XEvent *, Boolean *)
  DESCRIPTION:
  Popup event handler.
  END:
*/
static void
PopupEH(Widget w, XtPointer udata, XEvent *xev, Boolean *pb)
{
   XButtonPressedEvent *xbp = (XButtonPressedEvent *)xev;
   XRectangle *rect;

   DEBUGOUT(_LtDebug(__FILE__, w, "PopupEH ...\n"));

   Unused(udata);

   *pb = TRUE;
   rect = &CB_HitRect(w);

   if(xbp->button != Button1)
   {
      return;
   }

   DEBUGOUT(_LtDebug(__FILE__, w, "PopupEH: hit at: x(%d), y(%d)\n",
            (int)xbp->x,(int)xbp->y));
   DEBUGOUT(_LtDebug(__FILE__, w, 
            "PopupEH: hit area: x(%d), y(%d), width(%d), height(%d)\n",
            (int) CB_HitRect(w).x,
            (int) CB_HitRect(w).y,
            (int) CB_HitRect(w).width,
            (int) CB_HitRect(w).height));

   /* check to see if arrow button was hit */
   if(!IsEventInRectangle(xev, rect))
   {
      return;
   }

   /* Call our action procedure */
   XtCallActionProc(w, "CBArmAndDropDownList", xev, NULL, 0);
   
#if 0
   /* Is the grab shell managed? */
   if (!XtIsManaged(CB_ListShell(w)))
   {
      DEBUGOUT(_LtDebug(__FILE__, w, "PopupEH: Not managed so show grab shell\n"));

      /* display grab shell */
      XtPopup(CB_ListShell(w), XtGrabNone);
      /*
      CBArmAndDropDownList(w,xev,NULL,NULL);
      */
   }
   else
   {
      DEBUGOUT(_LtDebug(__FILE__, w, "PopupEH: Managed so hide grab shell\n"));

      XtUnmanageChild(CB_ListShell(w));
      /*
      CBDisarm(w,xev,NULL,NULL);
      */
   }
#endif
}


/*
  FUNCTION: QueryGeometry
  SYNOPSIS: static XtGeometryResult QueryGeometry(Widget,XtWidgetGeometry *,XtWidgetGeometry *)
  DESCRIPTION:
  END:
*/
static XtGeometryResult 
QueryGeometry(Widget w, XtWidgetGeometry *proposed, XtWidgetGeometry *answer)
{
   XtGeometryResult result = XtGeometryYes;
   XtWidgetGeometry geo;

   DEBUGOUT(_LtDebug(__FILE__, w, "%s:QueryGeometry %s\n",
		      __FILE__,
		      _LtDebugWidgetGeometry2String(proposed)));

   proposed->request_mode &= CWWidth | CWHeight;
   if(proposed->request_mode == 0) 
   {
      return result;
   }

   ComputeSize(w, &geo);

   if(proposed->request_mode & CWWidth)
   {
      if(proposed->width < geo.width)
      {
         result = XtGeometryAlmost;
         answer->width = geo.width;
         answer->request_mode |= CWWidth;
      }
   }

   if(proposed->request_mode & CWHeight)
   {
      if(proposed->height < geo.height)
      {
         result = XtGeometryAlmost;
         answer->height = geo.height;
         answer->request_mode |= CWHeight;
      }
   }

   Unused(w);
   Unused(proposed);
   Unused(answer);

   return result;
}


/* 
  FUNCTION: Redisplay
  SYNOPSIS: static void Redisplay(Widget w)
  DESCRIPTION:
  This routine handles expose events.
  END:
*/
static void 
Redisplay(Widget w, XEvent *event, Region region)
{
   DEBUGOUT(_LtDebug(__FILE__, w, "%s:Redisplay ...\n",
   	__FILE__));

   Unused(event);
   Unused(region);

   DrawComboBox(w);
}

/* 
  FUNCTION: Resize
  SYNOPSIS: static void Resize(Widget w)
  DESCRIPTION:
  This routine handles resizing.
  END:
*/
static void 
Resize(Widget w)
{
   DEBUGOUT(_LtDebug(__FILE__, w, "%s:Resize %ix%i\n",
   	__FILE__,
   	XtWidth(w), XtHeight(w)));

   /*
   if(XtIsRealized(w))
   */
   {  /* widgets must be resized */
      /* rws 12 Jul 1998
         must be done if we are realized or not otherwise they will have
         the wrong size when they are realized (maxwell).
       */
      DoLayout(w);
   }

   (*superclass->core_class.resize)(w);
}


#if 0
/*
  FUNCTION: SBBtnDownEH()
  SYNOPSIS: static void SBBtnDownEH()
  DESCRIPTION:
  Scrolled Button down event handler.
  END:
*/
static void
SBBtnDownEH(Widget w, XtPointer udata, XEvent *xev, Boolean *pb)
{
   Unused(w);
   Unused(udata);
   Unused(xev);
   Unused(pb);
}
#endif

#if 0
/*
  FUNCTION: SBBtnUpEH()
  SYNOPSIS: static void SBBtnDownEH()
  DESCRIPTION:
  Scrolled Button up event handler.
  END:
*/
static void
SBBtnUpEH(Widget w, XtPointer udata, XEvent *xev, Boolean *pb)
{
   Unused(w);
   Unused(udata);
   Unused(xev);
   Unused(pb);
}
#endif


static void
SetEditBoxValue(Widget w, XmString s)
{
   char *v;

   /* Convert XmString to a string */
   v = XmCvtXmStringToCT(s);

   XtVaSetValues(CB_EditBox(w),XmNvalue, v, NULL);

   /* free memory */
   XtFree(v);
}

/*
  FUNCTION: SetFocusChangeHandler
  SYNOPSIS: static void SetFocusChangeHandler(Widget w)
  DESCRIPTION:
  This routine will setup the focus change handler routine for
  the combobox.
  END:
*/
static void
SetFocusChangeHandler(Widget w)
{
   Widget shell;
 
   shell = _FindVendorShell(w);

   /* Add focus handler */
   XmeAddFocusChangeCallback(shell, 
                             (XtCallbackProc) ComboBoxFocusMovedCB, 
                             (XtPointer) w);
}


static void
RemoveFocusChangeHandler(Widget w)
{
   Widget shell;

   shell = _FindVendorShell(w);

   /* Remove focus change handler */
   XmeRemoveFocusChangeCallback(shell,
                                (XtCallbackProc)ComboBoxFocusMovedCB, 
                                (XtPointer) w);
}

/* 
  FUNCTION: SetHitArea
  SYNOPSIS: static void SetHitArea(Widget w)
  DESCRIPTION:
  This routine will setup the hit area by setting the size and then
  installing an event handler for button events.
  END:
*/
static void
SetHitArea(Widget w)
{
   SetHitAreaSize(w);
}


/*
  FUNCTION: SetHitAreaHandler
  SYNOPSIS: static void SetHitAreaHandler(Widget w)
  DESCRIPTION:
  This routine will setup the hit area by installing an event handler
  for button events.
  END:
*/
static void
SetHitAreaHandler(Widget w)
{
   EventMask mask;

   DEBUGOUT(_LtDebug(__FILE__, w, "SetHitAreaHandler ...\n"));

   if (CB_Type(w) == XmDROP_DOWN_LIST
       || CB_Type(w) == XmDROP_DOWN_COMBO_BOX)
   {

      mask = ButtonPressMask;
      XtAddEventHandler(w, mask, FALSE, (XtEventHandler)PopupEH, (XtPointer) NULL);
   }
}


static void
RemoveHitAreaHandler(Widget w)
{
   EventMask mask;

   DEBUGOUT(_LtDebug(__FILE__, w, "RemoveHitAreaHandler ...\n"));

   if (CB_Type(w) == XmDROP_DOWN_LIST
       || CB_Type(w) == XmDROP_DOWN_COMBO_BOX)
   {
      mask = ButtonPressMask;
      XtRemoveEventHandler(w, mask, FALSE, (XtEventHandler)PopupEH, (XtPointer) NULL);
   }
}

/*
  FUNCTION: SetHitAreaSize
  SYNOPSIS: static void SetHitAreaSize(Widget w)
  DESCRIPTION:
  This function fills in the x, y, width and height for the arrow button.
  Must only be called after adjustments to the edit box have been completed.
  END:
*/
static void
SetHitAreaSize(Widget w)
{
   DEBUGOUT(_LtDebug(__FILE__, w, "SetHitAreaSize ...\n"));

   /* Always use CB_ArrowSize for the width */
   CB_HitRect(w).width = CB_ArrowSize(w);

   /* Always use the edit box height at this point in time */
   CB_HitRect(w).height = XtHeight(CB_EditBox(w));

   /* Y is the same as the edit box Y value */
   CB_HitRect(w).y = XtY(CB_EditBox(w));

   /* X is the X value of the edit box plus its width plus the arrow spacing */
   CB_HitRect(w).x = XtX(CB_EditBox(w)) + XtWidth(CB_EditBox(w))
                     + CB_ArrowSpacing(w);
}


/*
  FUNCTION: SetIdealTextSize
  SYNOPSIS: static void SetIdealTextSize(Widget w)
  DESCRIPTION:
  This function fills in the ideal width and height based on the
  current width and height of the text field.  This function should
  only be called after creating the text field.
  END:
*/
static void
SetIdealTextSize(Widget w)
{
   DEBUGOUT(_LtDebug(__FILE__, w, "SetIdealTextSize ...\n"));

   CB_IdealEBHeight(w) = XtHeight(CB_EditBox(w));
   CB_IdealEBWidth(w) = XtWidth(CB_EditBox(w));
}


/* 
  FUNCTION: SetValues
  SYNOPSIS: static Boolean SetValues(Widget,Widget,Widget,ArgList,Cardinal *)
  DESCRIPTION:
  This routine handles the setting of widget resources.
  END:
*/
static Boolean 
SetValues(Widget old, 
          Widget request, 
          Widget c_new, 
          ArgList args,
          Cardinal *num_args)
{
	Boolean need_refresh = False;
	int	i;
#if 0
	XmComboBoxWidget ow = (XmComboBoxWidget) old;
	XmComboBoxWidget nw = (XmComboBoxWidget) c_new;
#endif

	/* FIXME: There should be more code here :-) */

	 DEBUGOUT(_LtDebug(__FILE__, c_new,
		"%s:SetValues: %i args\n"
		"\t    old X %5i Y %5i W %5i H %5i\n"
		"\trequest X %5i Y %5i W %5i H %5i\n"
		"\t  c_new X %5i Y %5i W %5i H %5i\n",
		__FILE__,
		*num_args,
		XtX(old), XtY(old),
		XtWidth(old), XtHeight(old),
		XtX(request), XtY(request),
		XtWidth(request), XtHeight(request),
		XtX(c_new), XtY(c_new),
		XtWidth(c_new), XtHeight(c_new)));
	DEBUGOUT(_LtDebugPrintArgList(__FILE__, c_new, args, *num_args, False));

	DetectAndFixChangesToReadOnlySettings(old, c_new);
	DetectAndFixBadSettings(c_new);

	for (i=0; i<*num_args; i++) {
		if (strcmp(args[i].name, XmNvisibleItemCount) == 0) {
			XtVaSetValues(CB_List(c_new),
				XmNvisibleItemCount, args[i].value,
				NULL);;
		}
	}

	Unused(request);
	Unused(args);
	Unused(num_args);

	return need_refresh;
}


/*
  FUNCTION: ShellFocusMovedCB
  SYNOPSIS: static void ShellFocusMovedCB(Widget, XtPointer, XtPointer)
  DESCRIPTION:
  This function is called when the list shell focus changes.
  END:
*/
static void
ShellFocusMovedCB(Widget w, XtPointer client_data, XtPointer call_data)
{
   Widget self = (Widget) client_data; 
   Widget find_self;
   XmFocusMovedCallbackStruct *cbs = (XmFocusMovedCallbackStruct *) call_data;

   DEBUGOUT(_LtDebug(__FILE__, self, "ShellFocusMovedCB ...\n"));

   /* we will find ourself if new focus widget is a child */
   find_self = _FindComboBox(cbs->new_focus);
   if(self == find_self)
   {
      CBFocusIn(self, cbs->event, NULL, NULL);
   }
   else
   {
      CBFocusOut(self, cbs->event, NULL, NULL);

      PopdownList(self, cbs->event);

      CBDisarm(self, cbs->event,NULL,NULL);
   }

   Unused(w);
   Unused(client_data);
   Unused(call_data);
} 


static void
ShellPopdownCB(Widget w, XtPointer client_data, XtPointer data) 
{
   /* FIXTHIS: */

   Unused(w);
   Unused(client_data);
   Unused(data);

#if 0
   CBFocusOut();
#endif
} 


static void
ShellPopupCB(Widget w, XtPointer client_data, XtPointer data)
{
   /* FIXTHIS: */

   Unused(w);
   Unused(client_data);
   Unused(data);
} 


/*
  FUNCTION: TextChangedCB
  SYNOPSIS: static void TextChangedCB(Widget w, XtPointer cdata, XtPointer data)
  DESCRIPTION:
  Apparently, this function does absolutely nothing. 
  END:
*/
static void
TextChangedCB(Widget w, XtPointer client_data, XtPointer data)
{
   DEBUGOUT(_LtDebug(__FILE__, w, "TextChangedCB ...\n"));

   Unused(w);
   Unused(client_data);
   Unused(data);
} 

/*
  FUNCTION: UnhighlightBorder
  SYNOPSIS: static void UnhighlightBorder(Widget w)
  DESCRIPTION:
  This function removes the highlight around the combobox. 
  END:
*/
static void
UnhighlightBorder(Widget w)
{
   int highlight_offset = 0; /* adjust as needed (<0 makes it bigger) */

   /* check to see if thickness is zero */
   if(CB_HighlightThickness(w) == 0)
   {
      return;
   }

   DEBUGOUT(_LtDebug(__FILE__, w, "UnhighlightBorder ...\n"));

   DEBUGOUT(_LtDebug(__FILE__, 
            w, 
            "UnhighlightBorder: x(%d), y(%d), width(%d), height(%d) ...\n",
            (int) highlight_offset,
            (int) highlight_offset,
            (int) XtWidth(w) - 2*highlight_offset,
            (int) XtHeight(w) - 2*highlight_offset));

   /* FIXTHIS: Should I be using my own background GC? */
   XmeDrawHighlight(XtDisplayOfObject(w),
                    XtWindowOfObject(w),
                    MGR_BackgroundGC(w),
                    highlight_offset,
                    highlight_offset,
                    XtWidth(w) - 2*highlight_offset,
                    XtHeight(w) - 2*highlight_offset,
                    CB_HighlightThickness(w));

   CB_Highlighted(w) = False;
}


/*
  FUNCTION: AdjustKids
  SYNOPSIS: static Boolean AdjustKids(Widget w)
  DESCRIPTION:
  Determine type of widget and call appropriate adjustment routine.
  END:
*/
static void
AdjustKids(Widget w)
{
   if (CB_Type(w) == XmCOMBO_BOX)
   {
      AdjustKidsComboBox(w);
   }
   else
   {  /* assume calculation for XmDROP_DOWN_LIST and XmDROP_DOWN_COMBO_BOX
         are the same.
      */
      AdjustKidsDropDown(w);
   }
}


/*
  FUNCTION: AdjustKidsComboBox
  SYNOPSIS: static Boolean AdjustKidsComboBox(Widget w)
  DESCRIPTION:
  Determine position and size of kids for the combo box widget.
  END:
*/
static void
AdjustKidsComboBox(Widget w)
{
   int edit_x, edit_y, edit_height, edit_width;
   int scrolledw_x, scrolledw_y, scrolledw_width, scrolledw_height;

   DEBUGOUT(_LtDebug(__FILE__, w, "AdjustKidsComboBox ...\n"));

   /* Perform adjustments to the edit box */
   edit_x = edit_y = 0;

   edit_x += CB_MarginWidth(w);
   edit_y += CB_MarginHeight(w);

   edit_x += CB_HighlightThickness(w);
   edit_y += CB_HighlightThickness(w);

   edit_x += MGR_ShadowThickness(w);
   edit_y += MGR_ShadowThickness(w);

#if 0 
   /* should be taking the ideal height here not the XtHeight of the edit box */
   edit_height = XtHeight(CB_EditBox(w));
#endif
#if 0
   /* Assume the height of the scrolled window will not increase or decrease
      unless the programmer specifically changes the visibleItemCount.
    */
   edit_height = XtHeight(w)  
                 - (XtHeight(CB_ScrolledW(w))
                    + (2*(CB_HighlightThickness(w)
                          +MGR_ShadowThickness(w)
                          +CB_MarginHeight(w))));
#endif
   /*
      980929 - pgw@hungry.com
      Test 10 demonstrates the height of the scrolled window
      will increase or decrease if the form is resized.
   */
   edit_height = XtHeight(CB_EditBox(w));

   /* calculate edit width based on current size */
   edit_width = XtWidth(w) - 
                (CB_ArrowSize(w) + CB_ArrowSpacing(w) 
                 + (2*(CB_HighlightThickness(w)
                       +MGR_ShadowThickness(w)
                       +CB_MarginWidth(w))));


   /* configure the edit box */
   XmeConfigureObject(CB_EditBox(w),
                      edit_x,
                      edit_y,
                      edit_width,
                      edit_height,
                      XtBorderWidth(CB_EditBox(w)));

   /* Perform adjustments to the scrolled window */
   scrolledw_x = edit_x;
   scrolledw_y = edit_y + XtHeight(CB_EditBox(w));
   scrolledw_width = XtWidth(CB_EditBox(w));

   scrolledw_height = XtHeight(w) 
                    - (scrolledw_y
                       + (CB_HighlightThickness(w)
                          +MGR_ShadowThickness(w)
                          +CB_MarginHeight(w)));
#if 0
   scrolledw_height = XtHeight(w) 
                 - (scrolledw_y
                    + (2*(CB_HighlightThickness(w)
                          +MGR_ShadowThickness(w)
                          +CB_MarginHeight(w))));
#endif

   XmeConfigureObject(CB_ScrolledW(w),
                      scrolledw_x,
                      scrolledw_y,
                      scrolledw_width,
                      scrolledw_height,
                      XtBorderWidth(CB_ScrolledW(w)));
}

/*
  FUNCTION: AdjustKidsDropDown
  SYNOPSIS: static Boolean AdjustKidsDropDown(Widget w)
  DESCRIPTION:
  Determine position and size of kids for the drop down widget.
  END:
*/
static void
AdjustKidsDropDown(Widget w)
{
   int edit_x, edit_y, edit_height, edit_width;
   int shell_x, shell_y, shell_height, shell_width;

   DEBUGOUT(_LtDebug(__FILE__, w, "AdjustKidsDropDown ...\n"));
   /*
      FIXTHIS: Confirm if docs are correct.
      REMARK:
      The placement of the arrow button depends on the resource
      XmNlayoutDirection according to the documentation.
      This resource is new to 2.0 and lives in the manager widget.
      This will be implemented at a later date.  For now the button will
      always be on the right side.
      END:
   */

   /* perform x,y adjustments to edit box */
   edit_x = edit_y = 0;

   edit_x += CB_MarginWidth(w);
   edit_y += CB_MarginHeight(w);

   edit_x += CB_HighlightThickness(w);
   edit_y += CB_HighlightThickness(w);

   edit_x += MGR_ShadowThickness(w);
   edit_y += MGR_ShadowThickness(w);

   /*
      REMARK:
      The height of the edit box is dependent on the size of the arrowSize
      resource.  If the arrowSize is greater then the calculated text height
      then use the arrowSize to determine the height of the edit box.
      END:
   */

   edit_height = XtHeight(w)  
                 - (2*(CB_HighlightThickness(w)
                       +MGR_ShadowThickness(w)
                       +CB_MarginHeight(w)));

   if (CB_ArrowSize(w) > edit_height)
   {
      edit_height = CB_ArrowSize(w);
   }

   /* 
      REMARK:
      Test 8 demonstrates the width and height of the ComboBox can change.
      FIX ME: when resizing need to redraw self.
      END:
   */
   /* calculate edit width based on current size */
   edit_width = XtWidth(w) - 
                (CB_ArrowSize(w) + CB_ArrowSpacing(w) 
                 + (2*(CB_HighlightThickness(w)
                       +MGR_ShadowThickness(w) 
                       +CB_MarginWidth(w))));

   /* configure the edit box */
   XmeConfigureObject(CB_EditBox(w),
                      edit_x,
                      edit_y,
                      edit_width,
                      edit_height,
                      XtBorderWidth(CB_EditBox(w)));

   /* resize the grab shell width to equal the width of the drop down */
   shell_x = XtX(CB_ListShell(w));
   shell_y = XtY(CB_ListShell(w));
   /* The shell width is the width of the dropdown - the highlight thickness */
   shell_width = edit_width + CB_ArrowSize(w) + CB_ArrowSpacing(w)
                 + (2*(MGR_ShadowThickness(w) + CB_MarginWidth(w)));

#if 0
   shell_width = XtWidth(w)
                 - (2*CB_HighlightThickness(w));
#endif

   shell_height = XtHeight(CB_ListShell(w));

   /* configure the shell */
   XmeConfigureObject(CB_ListShell(w),
                      shell_x,
                      shell_y,
                      shell_width,
                      shell_height,
                      XtBorderWidth(CB_ListShell(w)));

   /* Setup the hit area for the pseudo arrow button */
   SetHitArea(w);
}

/*
  FUNCTION: CalcCBLineHeight
  SYNOPSIS: static void CalcCBLineHeight(Widget w, Dimension *height)
  DESCRIPTION:
  This function calculates the height of the combo box line.
  The line is the visual effect located below the arrow.
  END:
*/
static void
CalcCBLineHeight(Widget w, Dimension *height)
{
   int level;
   int change_on;
   int fudge_factor;

   /*
      REMARK:
      11/14/97 (pgw)
      Haven't figured out how to calculate this fudge factor yet but I know
      it is seven.
      END:
   */
   fudge_factor = 7; /* arrow spacing * 2 + 3 for the line? why is this 7 */

   /*
      REMARK:
      Determine how many times the arrow size is divisible by the fudge factor.
      This seems too complex..there must be a better way of doing this calc.
      END:
   */
   level = CB_ArrowSize(w) / fudge_factor;
   change_on = (level * fudge_factor) + ((((level*10)/2)+5)/10);

   if (CB_ArrowSize(w) < change_on)
   {
      level--;
   }

   *height = level + 1;
}

/*
  FUNCTION: IsEventInRectangle
  SYNOPSIS: static Boolean IsEventInRectangle(XEvent *xev, XRectangle *rect)
  DESCRIPTION:
  This function determines if the combobox has been hit within the
  rectangle.
  END:
*/
static Boolean
IsEventInRectangle(XEvent *xev, XRectangle *rect)
{
   /* 
    * Expression checks to see if the event is not within the rectangle.
    * Then negate expression to determine if it is within the rectangle. 
    */

   switch(xev->type)
   {
   case ButtonPress:
   case ButtonRelease:
      return (Boolean) !(  (xev->xbutton.x < rect->x
                            || xev->xbutton.x > (rect->x + rect->width))
                         ||(xev->xbutton.y < rect->y
                            || xev->xbutton.y > (rect->y + rect->height)));
   case LeaveNotify:
   case EnterNotify:
      return (Boolean) !(  (xev->xcrossing.x < rect->x
                            || xev->xcrossing.x > (rect->x + rect->width))
                         ||(xev->xcrossing.y < rect->y
                            || xev->xcrossing.y > (rect->y + rect->height)));
   }

   return False;
}

/*
  FUNCTION: KidsValid
  SYNOPSIS: static Boolean KidsValid(Widget w)
  DESCRIPTION:
  Determine if the children are still valid.
  END:
*/
static Boolean
KidsValid(Widget w)
{
   /* Report warning if the listbox or edit box are not managed */
   if (!XtIsManaged(CB_EditBox(w)) || !XtIsManaged(CB_List(w)))
   {
      /* report warning, user should know better */
      XmeWarning(w, _XmMsgComboBox_0005);
      return False;
   }
   /* Report warning if the listbox or edit box have been destroyed */
   if (CoreBeingDestroyed(CB_EditBox(w)) || CoreBeingDestroyed(CB_List(w)))
   {
      /* report warning, user getting ready for a core dump ;-) */
      XmeWarning(w, _XmMsgComboBox_0004);
      return False;
   }

   return True;
}

/*
  FUNCTION: ComboBoxFocusMovedCB
  SYNOPSIS: static void ComboBoxFocusMovedCB(Widget, XtPointer, XtPointer)
  DESCRIPTION:
  This function is called when the focus changes on an ancestors vendor shell.
  END:
*/
static void
ComboBoxFocusMovedCB(Widget w, XtPointer client_data, XtPointer call_data)
{
   Widget self = (Widget) client_data; 
   Widget find_self;
   Boolean to_self = False, from_self = False;
   XmFocusMovedCallbackStruct *cbs = (XmFocusMovedCallbackStruct *) call_data;

   DEBUGOUT(_LtDebug(__FILE__, self, "ComboBoxFocusMovedCB ...\n"));

   /* continue ? */
   if(!cbs->cont)
   {
      return;
   }

   /* we will find ourself if new focus widget is a child */
   find_self = _FindComboBox(cbs->new_focus);
   if(self == find_self)
   {
      to_self = True;
   }
   find_self = _FindComboBox(cbs->old_focus);
   if(self == find_self)
   {
      from_self = True;
   }

   if(to_self)
   {
      CBFocusIn(self, cbs->event, NULL, NULL);
   }
   else if(from_self)
   {
      CBFocusOut(self, cbs->event, NULL, NULL);
   }

   Unused(w);
   Unused(client_data);
   Unused(call_data);
} 

/* ================================================================ */
/* Public routines */
/* 
  FUNCTION: XmCreateComboBox
  SYNOPSIS: Widget XmCreateComboBox(Widget,char *,ArgList,Cardinal)
  DESCRIPTION:
  Creates an instance of a XmComboxWidget.
  END:
*/
extern Widget 
XmCreateComboBox(Widget parent, char *name, ArgList arglist,
                 Cardinal cnt)
{
   return XtCreateWidget(name, 
                         xmComboBoxWidgetClass,
                         parent, 
                         arglist, 
                         cnt);
}

/* 
  FUNCTION: XmCreateDropDownComboBox
  SYNOPSIS: Widget XmCreateDropDownComboBox(Widget,char *,ArgList,Cardinal)
  DESCRIPTION:
  Creates an instance of a XmComboxWidget with XmNcomboBoxType 
  set to XmDROP_DOWN_COMBO_BOX.
  END:
*/
extern Widget 
XmCreateDropDownComboBox(Widget parent,
                         char *name,
                         Arg *arglist,
                         Cardinal cnt)
{
   Widget w;
   Arg myArgList[1];
   int n = 0;

   ArgList combined;

   XtSetArg(myArgList[n], XmNcomboBoxType, XmDROP_DOWN_COMBO_BOX); n++;

   combined = XtMergeArgLists(myArgList, n, arglist, cnt);

   w = XtCreateWidget(name,
                      xmComboBoxWidgetClass,
                      parent,
                      combined, 
                      n + cnt);

   XtFree((char *)combined);

   return w;
}


/* 
  FUNCTION: XmCreateDropDownList
  SYNOPSIS: Widget XmCreateDropDownList(Widget,char *,ArgList,Cardinal)
  DESCRIPTION:
  Creates an instance of a XmComboxWidget with XmNcomboBoxType 
  set to XmDROP_DOWN_LIST.
  END:
*/
extern Widget 
XmCreateDropDownList(Widget parent,
                     char *name,
                     Arg *arglist,
                     Cardinal cnt)
{
   Widget w;
   Arg myArgList[1];
   int n = 0;

   ArgList combined;

   XtSetArg(myArgList[n], XmNcomboBoxType, XmDROP_DOWN_LIST); n++;

   combined = XtMergeArgLists(myArgList, n, arglist, cnt);

   w = XtCreateWidget(name,
                      xmComboBoxWidgetClass,
                      parent,
                      combined, 
                      n + cnt);

   XtFree((char *)combined);

   return w;
}



/*
  REMARK:

* Determine if OSF/XmComboBox Widget actual creates a arrow button or
simulates it some how?

It simulates it.  It does not seem to ever resize the width
but does allow resizing of the height of the button along with the text field.

* Is the arrow button in the XmComboBox a gadget?

No.  It draws its own.

* Determine how children are layed out.  This is dependent on the previous
question.

Only one child, the text field.  The popup shell and the scrolled list 
will handle there own geometry.  The use of only one child simplies things.

* What is the name of the text field widget?

The text field widget is called "Text".

  END:
*/

#if XmVersion >= 2001
extern void
XmComboBoxAddItem(Widget w, XmString xms, int a, Boolean unique)
{
	char    *s = NULL;
	XmStringGetLtoR(xms, XmFONTLIST_DEFAULT_TAG, &s);
#if 0
	fprintf(stderr, "XmComboBoxAddItem(%s,%d,%d)\n", s, a, (int)unique);
#endif
	XtFree(s);
      /* FIXME: what about 'unique' ?? */
	XmListAddItemUnselected(CB_List(w), xms, a);
}


extern void
XmComboBoxDeletePos(Widget w, int pos)
{
  _XmWarning(w, "XmComboBoxDeletePos is not yet implemented!");
}


extern void
XmComboBoxSelectItem(Widget w, XmString item)
{
  _XmWarning(w, "XmComboBoxSelectItem is not yet implemented!");
}


extern void
XmComboBoxSetItem(Widget w, XmString item)
{
  _XmWarning(w, "XmComboBoxSetItem is not yet implemented!");
}

#endif /* XmVersion >= 2001 */


/* 
  FUNCTION: XmComboBoxUpdate
  SYNOPSIS: void XmComboBoxUpdate(Widget w)
  DESCRIPTION:
  Communicates any dependent changes between child widgets.
  END:
*/
extern void 
XmComboBoxUpdate(Widget w)
{
   /* FIXTHIS: XmComboBoxUpdate needs to be implemented. */
   Unused(w);
}


/* ================================================================ */
/* Utility routines the we may want to make public */
/*
  FUNCTION: _LTTranslateCoords
  SYNOPSIS: static void _LTTranslateCoords(Widget w, Position x, Position y, Position *rootx, Position *rooty)
  DESCRIPTION:
  This function is a safe version of XtTranslateCoords.
  END:
*/
static void
_LTTranslateCoords(Widget w, Position x, Position y, Position *rootx, Position *rooty)
{
   int w_rootx, w_rooty;
   Window junk;

   /*
    * Usually, it is safe here to use XtTranslateCoords.  However,
    * there is a certain amount of misbehaviour that can confuse the
    * Intrinsics about the actual widget location.  In particular,
    * if the widget window has been reparented, there is no way to tell
    * the intrinsics about this, and thus, the Intinsics do not really
    * know where the widget is located.  Thus, we have to ask the X server
    * directly for this info. This causes more traffic on the wire, but...
    * I really need this.  -- Linas
    *
    * XtTranslateCoords(w, x, y, &rootx, &rooty);
    */
   XTranslateCoordinates (XtDisplay(w), XtWindow(w),
                          XRootWindowOfScreen(XtScreen(w)),
                          x, y,  &w_rootx, &w_rooty, &junk);

   *rootx = w_rootx;
   *rooty = w_rooty;
}

/*
  FUNCTION: __LTShellSmartPlacement
  SYNOPSIS: static void __LTShellSmartPlacement(Widget w)
  DESCRIPTION:
  This function attempts to be smart about placement of the shell on the
  display.  It calculates the ideal x and y placement values if possible to
  display shell completely on the screen.
  END:
*/
static void
__LTShellSmartPlacement(Widget w, int *ret_x, int *ret_y)
{
    Display *dpy;
    int x,y,width,height;
    int dpy_width, dpy_height, scr;

    DEBUGOUT(_LtDebug(__FILE__, w, "__LTShellSmartPlacement...\n"));

    /* Is shell realized? */
    if(!XtIsRealized(w))
    {
        /* Force size calculation */
        XtRealizeWidget(w);
    }

    dpy = XtDisplay(w);
    scr = XDefaultScreen(dpy);

    dpy_width = DisplayWidth(dpy,scr);
    dpy_height = DisplayHeight(dpy,scr);

    x = XtX(w);
    y = XtY(w);

    width = XtWidth(w);
    height = XtHeight(w);

    /* adjust based on default width and height of display */
    if (y+height > dpy_height) y = dpy_height - height;
    if (x+width > dpy_width) x = dpy_width - width;

    /* last check always keep x and y visible */
    if (x < 0) x = 0;
    if (y < 0) y = 0;

    *ret_x = x;
    *ret_y = y;

    DEBUGOUT(_LtDebug(__FILE__, w, "__LTShellSmartPlacement...x=%d,y=%d\n",x,y));
}

/*
  FUNCTION: _LTShellSmartPlacement
  SYNOPSIS: static void _LTShellSmartPlacement(Widget w)
  DESCRIPTION:
  This function attempts to be smart about placement of the shell on the
  display.  It adjusts the suggested x and y placement values if possible to
  display shell completely on the screen.
  END:
*/

static void
_LTShellSmartPlacement(Widget w)
{
    int x,y;

#if 0
    /* Is shell realized? */
    if(!XtIsRealized(w))
    {
        /* Force size calculation */
        XtRealizeWidget(w);
    }
#endif

    __LTShellSmartPlacement(w, &x, &y);

    /* if we changed something then reset X and Y */
    if(XtX(w) != x || XtY(w) != y) XtVaSetValues(w, XmNx, x, XmNy, y, NULL);
}

static void
GetValuesHook(Widget w, ArgList args, Cardinal *num_args)
{
	Cardinal i;
	XrmQuark tf = XrmStringToQuark(XmNtextField), q,
		    l = XrmStringToQuark(XmNlist);
	XtPointer *p;

	for (i=0; i<*num_args; i++) {
		q = XrmStringToQuark(args[i].name);
		if (q == tf) {	/* XmNtextField */
			p = (XtPointer *) args[i].value;
			*p = (XtPointer)XtNameToWidget(w, "Text");
		} else if (q == l) {	/* XmNlist */
			p = (XtPointer *) args[i].value;
			*p = (XtPointer)CB_List(w);
		} else {
		}
	}
}
