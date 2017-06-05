/*
 *  $Header: /cvsroot/lesstif/lesstif/test/common/XdbPrintResources.c,v 1.4 2002/05/01 16:01:26 amai Exp $
 *
 * Copyright (C) 1995 Free Software Foundation, Inc.
 * Copyright (C) 1996-2002 LessTif Development Team
 */
 
#include "LTTconfig.h"

#include <stdio.h>

#include <Xm/ScrolledWP.h>
#include <Xm/ListP.h>

#define LIB_LTTEST
#include "Test.h"

#ifndef List_ItemCount
#define List_ItemCount(w) \
    (((XmListWidget)(w))->list.itemCount)

#define List_SelectedItemCount(w) \
    (((XmListWidget)(w))->list.selectedItemCount)

#define List_VisibleItemCount(w) \
    (((XmListWidget)(w))->list.visibleItemCount)

#define List_LastSetVizCount(w) \
    (((XmListWidget)(w))->list.LastSetVizCount)


#define List_Items(w) \
    (((XmListWidget)(w))->list.items)

#define List_SelectedItems(w) \
    (((XmListWidget)(w))->list.selectedItems)

#define List_SelectedIndices(w) \
    (((XmListWidget)(w))->list.selectedIndices)

#define List_MarginHeight(w) \
    (((XmListWidget)(w))->list.margin_height)

#define List_MarginWidth(w) \
    (((XmListWidget)(w))->list.margin_width)

#define List_SizePolicy(w) \
    (((XmListWidget)(w))->list.SizePolicy)

#define List_ItemSpacing(w) \
    (((XmListWidget)(w))->list.ItemSpacing)

#define List_Spacing(w) \
    (((XmListWidget)(w))->list.spacing)

#define List_SBDisplayPolicy(w) \
    (((XmListWidget)(w))->list.ScrollBarDisplayPolicy)

#define List_SelectionPolicy(w) \
    (((XmListWidget)(w))->list.SelectionPolicy)

#define List_BaseX(w) \
    (((XmListWidget)(w))->list.BaseX)

#define List_BaseY(w) \
    (((XmListWidget)(w))->list.BaseY)

#endif


#ifndef SW_VSBMinimum
#define SW_VSBMinimum(w) \
    (((XmScrolledWindowWidget)w)->swindow.vmin)

#define SW_VSBMaximum(w) \
    (((XmScrolledWindowWidget)w)->swindow.vmax)

#define SW_VSBValue(w) \
    (((XmScrolledWindowWidget)w)->swindow.vOrigin)

#define SW_VSBSliderSize(w) \
    (((XmScrolledWindowWidget)w)->swindow.vExtent)

#define SW_HSBMinimum(w) \
    (((XmScrolledWindowWidget)w)->swindow.hmin)

#define SW_HSBMaximum(w) \
    (((XmScrolledWindowWidget)w)->swindow.hmax)

#define SW_HSBValue(w) \
    (((XmScrolledWindowWidget)w)->swindow.hOrigin)


#define SW_HSBSliderSize(w) \
    (((XmScrolledWindowWidget)w)->swindow.hExtent)

#define SW_HSBX(w) \
    (((XmScrolledWindowWidget)w)->swindow.hsbX)

#define SW_HSBY(w) \
    (((XmScrolledWindowWidget)w)->swindow.hsbY)

#define SW_HSBWidth(w) \
    (((XmScrolledWindowWidget)w)->swindow.hsbWidth)

#define SW_HSBHeight(w) \
    (((XmScrolledWindowWidget)w)->swindow.hsbHeight)

#define SW_VSBX(w) \
    (((XmScrolledWindowWidget)w)->swindow.vsbX)

#define SW_VSBY(w) \
    (((XmScrolledWindowWidget)w)->swindow.vsbY)

#define SW_VSBWidth(w) \
    (((XmScrolledWindowWidget)w)->swindow.vsbWidth)

#define SW_VSBHeight(w) \
    (((XmScrolledWindowWidget)w)->swindow.vsbHeight)

#define SW_GivenWidth(w) \
    (((XmScrolledWindowWidget)w)->swindow.GivenWidth)

#define SW_GivenHeight(w) \
    (((XmScrolledWindowWidget)w)->swindow.GivenHeight)

#define SW_CWX(w) \
    (((XmScrolledWindowWidget)w)->swindow.XOffset)

#define SW_CWY(w) \
    (((XmScrolledWindowWidget)w)->swindow.YOffset)

#define SW_CWWidth(w) \
    (((XmScrolledWindowWidget)w)->swindow.AreaWidth)

#define SW_CWHeight(w) \
    (((XmScrolledWindowWidget)w)->swindow.AreaHeight)

#define SW_MarginWidth(w) \
    (((XmScrolledWindowWidget)w)->swindow.WidthPad)

#define SW_MarginHeight(w) \
    (((XmScrolledWindowWidget)w)->swindow.HeightPad)

#define SW_Spacing(w) \
    (((XmScrolledWindowWidget)w)->swindow.pad)

#define SW_HasHSB(w) \
    (((XmScrolledWindowWidget)w)->swindow.hasHSB)

#define SW_HasVSB(w) \
    (((XmScrolledWindowWidget)w)->swindow.hasVSB)

#define SW_InInit(w) \
    (((XmScrolledWindowWidget)w)->swindow.InInit)

#define SW_FromResize(w) \
    (((XmScrolledWindowWidget)w)->swindow.FromResize)

#define SW_VisualPolicy(w) \
    (((XmScrolledWindowWidget)w)->swindow.VisualPolicy)

#define SW_ScrollPolicy(w) \
    (((XmScrolledWindowWidget)w)->swindow.ScrollPolicy)

#define SW_ScrollBarPolicy(w) \
    (((XmScrolledWindowWidget)w)->swindow.ScrollBarPolicy)

#define SW_Placement(w) \
    (((XmScrolledWindowWidget)w)->swindow.Placement)

#define SW_HSB(w) \
    (((XmScrolledWindowWidget)w)->swindow.hScrollBar)

#define SW_VSB(w) \
    (((XmScrolledWindowWidget)w)->swindow.vScrollBar)

#endif


static const char *visualPolicy(int p)
{
   switch(p)
   {
      case XmVARIABLE:
         return "XmVARIABLE";
      case XmCONSTANT:
         return "XmCONSTANT";
      default:
         return "???";
    }
}


static const char *scrollBarDisplayPolicy(int p)
{
   switch(p)
   {
      case XmAS_NEEDED:
         return "XmAS_NEEDED";
      case XmSTATIC:
         return "XmSTATIC";
      default:
         return "???";
    }
}


static const char *scrollingPolicy(int p)
{
   switch(p)
   {
      case XmAUTOMATIC:
         return "XmAUTOMATIC";
      case XmAPPLICATION_DEFINED:
         return "XmAPPLICATION_DEFINED";
      default:
         return "???";
    }
}


static const char *scrollBarPlacement(int p)
{
   switch(p)
   {
      case XmTOP_LEFT:
         return "XmTOP_LEFT";
      case XmTOP_RIGHT:
         return "XmTOP_RIGHT";
      case XmBOTTOM_LEFT:
         return "XmBOTTOM_LEFT";
      case XmBOTTOM_RIGHT:
         return "XmBOTTOM_RIGHT";
      default:
         return "???";
    }
}


static const char *selectionPolicy(int p)
{
   switch(p)
   {
      case XmSINGLE_SELECT:
         return "XmSINGLE_SELECT";
      case XmMULTIPLE_SELECT:
         return "XmMULTIPLE_SELECT";
      case XmEXTENDED_SELECT:
         return "XmEXTENDED_SELECT";
      case XmBROWSE_SELECT:
         return "XmBROWSE_SELECT";
      default:
         return "???";
    }
}


static const char *listSizePolicy(int p)
{
   switch(p)
   {
      case XmCONSTANT:
         return "XmCONSTANT";
      case XmVARIABLE:
         return "XmVARIABLE";
      case XmRESIZE_IF_POSSIBLE:
         return "XmRESIZE_IF_POSSIBLE";
      default:
         return "???";
    }
}


static void _XdbPrintScrolledWindowResources(Widget w)
{
  fprintf(stderr,"ScrolledWindow Resource(s):\n");
#if 0
  fprintf(stderr,"GivenWidth=(%d)\n",(int) SW_GivenWidth(w));
  fprintf(stderr,"GivenHeight=(%d)\n",(int) SW_GivenHeight(w));
#endif
  fprintf(stderr,"AreaWidth=(%d)\n",(int) SW_CWWidth(w));
  fprintf(stderr,"AreaHeight=(%d)\n",(int) SW_CWHeight(w));
  fprintf(stderr,"WidthPad=(%d)\n",(int) SW_MarginWidth(w));
  fprintf(stderr,"HeightPad=(%d)\n",(int) SW_MarginHeight(w));
  fprintf(stderr,"XOffset=(%d)\n",(int) SW_CWX(w));
  fprintf(stderr,"YOffset=(%d)\n",(int) SW_CWY(w));
  fprintf(stderr,"pad=(%d)\n",(int) SW_Spacing(w));
#if 0
  fprintf(stderr,"hasHSB=(%d)\n",(int) SW_HasHSB(w));
  fprintf(stderr,"hasVSB=(%d)\n",(int) SW_HasVSB(w));
#endif
  fprintf(stderr,"InInit=(%d)\n",(int) SW_InInit(w)); 
  fprintf(stderr,"FromResize=(%d)\n",(int) SW_FromResize(w));
  fprintf(stderr,"VisualPolicy=(%s)\n", visualPolicy(SW_VisualPolicy(w)));
  fprintf(stderr,"ScrollPolicy=(%s)\n", scrollingPolicy(SW_ScrollPolicy(w)));
  fprintf(stderr,"ScrollBarPolicy=(%s)\n", scrollBarDisplayPolicy(SW_ScrollBarPolicy(w)));
  fprintf(stderr,"Placement=(%s)\n",scrollBarPlacement(SW_Placement(w)));
#if 0
  fprintf(stderr,"vmin=(%d)\n",(int) SW_VSBMinimum(w));
  fprintf(stderr,"vmax=(%d)\n",(int) SW_VSBMaximum(w));
  fprintf(stderr,"vOrigin=(%d)\n",(int) SW_VSBValue(w));
  fprintf(stderr,"vExtent=(%d)\n",(int) SW_VSBSliderSize(w));
  fprintf(stderr,"hmin=(%d)\n",(int) SW_HSBMinimum(w));
  fprintf(stderr,"hmax=(%d)\n",(int) SW_HSBMaximum(w));
  fprintf(stderr,"hOrigin=(%d)\n",(int) SW_HSBValue(w));
  fprintf(stderr,"hExtent=(%d)\n",(int) SW_HSBSliderSize(w));
  fprintf(stderr,"hsbX=(%d)\n",(int) SW_HSBX(w));
  fprintf(stderr,"hsbY=(%d)\n",(int) SW_HSBY(w));
  fprintf(stderr,"hsbWidth=(%d)\n",(int) SW_HSBWidth(w));
  fprintf(stderr,"hsbHeight=(%d)\n",(int) SW_HSBHeight(w));
  fprintf(stderr,"vsbX=(%d)\n",(int) SW_VSBX(w));
  fprintf(stderr,"vsbY=(%d)\n",(int) SW_VSBY(w));
  fprintf(stderr,"vsbWidth=(%d)\n",(int) SW_VSBWidth(w));
  fprintf(stderr,"vsbHeight=(%d)\n",(int) SW_VSBHeight(w));
#endif
}


static void _XdbPrintListResources(Widget w)
{
  fprintf(stderr,"List Resource(s):\n");
  fprintf(stderr,"spacing(%d):\n",  List_Spacing(w));
  fprintf(stderr,"ItemSpacing(%d):\n", List_ItemSpacing(w));
  fprintf(stderr,"margin_width(%d):\n", List_MarginWidth(w));
  fprintf(stderr,"margin_height(%d):\n", List_MarginHeight(w));
  fprintf(stderr,"itemCount(%d):\n", List_ItemCount(w));
  fprintf(stderr,"selectedItemCount(%d):\n", List_SelectedItemCount(w));
  fprintf(stderr,"visibleItemCount(%d):\n", List_VisibleItemCount(w));
  fprintf(stderr,"LastSetVizCount(%d):\n", List_LastSetVizCount(w));
  fprintf(stderr,"SelectionPolicy(%s):\n",selectionPolicy(List_SelectionPolicy(w)));
  fprintf(stderr,"ScrollBarDisplayPolicy(%s):\n",scrollBarDisplayPolicy(List_SBDisplayPolicy(w)));
  fprintf(stderr,"SizePolicy(%s):\n",listSizePolicy(List_SizePolicy(w)));
}


void XdbPrintResources(Widget w)
{
   if (XmIsScrolledWindow(w))
   {
      _XdbPrintScrolledWindowResources(w);
   }
   else if (XmIsList(w))
   {
      _XdbPrintListResources(w);
   }
}
