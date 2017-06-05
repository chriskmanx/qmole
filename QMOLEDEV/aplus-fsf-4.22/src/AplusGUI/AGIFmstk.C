//////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved.
// See .../src/LICENSE for terms of distribution.
//
//
///////////////////////////////////////////////////////////////////////////////

#if HAVE_IOSTREAM
#include <iostream>
#else
#include <iostream.h>
#endif
#if HAVE_NEW
#include <new>
#else
#include <new.h>
#endif
#include <math.h>
#include <X11/Xlib.h>
#include <X11/Xatom.h>
#include <AplusGUI/AGIFmstk.H>
#include <AplusGUI/EnumTables.H>
#include <AplusGUI/AplusConvert.H>
#include <AplusGUI/AplusTrace.H>
#include <AplusGUI/AplusPrintText.H>
#include <AplusGUI/AplusParagraph.H>

extern const int MSPointsPerInch;
extern void asTitleStringVector(MSStringVector &, A);
extern AVariableData *getVarData(A);
extern int AplusEvaluationDepth;

static A cdipv(AClientData *ac_,A data_,A index_,A pick_,V v_)
{
//   ++AplusEvaluationDepth;  fix in mstk

  if(AScbTraceHook::function()) 
    {
      AScbTraceHook::run(ac_->function(),(I)ac_->data(),(I)data_,(I)index_,(I)pick_,v_);
    }

  A r=af4(ac_->function(),(I)ac_->data(),(I)data_,(I)index_,(I)pick_,v_);
//   --AplusEvaluationDepth;  fix in mstk
  if (r==0) showError(qs);
  return r;
}

A getVarFunc(AClientData *ac_)
{ A r=aplus_nl;
  if (ac_!=0)
   { 
     if (isNull(ac_->actualData())==MSFalse) r=(A)ic(ac_->actualData());
     else
      {
	r=gv(Et,2);
	r->p[0]=ic(ac_->function());
	r->p[1]=ic(ac_->data());
      }
   }
  return (A)r;
}

//  MSWidgetView 'A' interface functions
static void s_width(MSWidgetView *pWidgetView_,int w_) { pWidgetView_->width(w_); }
static I g_width(MSWidgetView *pWidgetView_) { return (I) pWidgetView_->width(); }
static void s_height(MSWidgetView *pWidgetView_,int h_) { pWidgetView_->height(h_); }
static I g_height(MSWidgetView *pWidgetView_) { return (I) pWidgetView_->height(); }
static void s_x(MSWidgetView *pWidgetView_,int x_) {pWidgetView_->moveTo(x_,pWidgetView_->y_origin());}
static I g_x(MSWidgetView *pWidgetView_) {return (I) pWidgetView_->x_origin();}
static void s_y(MSWidgetView *pWidgetView_,int y_) {pWidgetView_->moveTo(pWidgetView_->x_origin(),y_);}
static I g_y(MSWidgetView *pWidgetView_) {return (I) pWidgetView_->y_origin();}
static void s_acceptFocus(MSWidgetView *pWidgetView_,MSBoolean b_) {pWidgetView_->acceptFocus(b_);}
static I g_acceptFocus(MSWidgetView *pWidgetView_) 
{return MSTrue==pWidgetView_->acceptFocus() ? 1 : 0;}

static void s_defaultLeader(MSShell *leader_) {MSShell::defaultLeader(leader_);}
static MSShell *g_defaultLeader(void) {return MSShell::defaultLeader();}


static I s_saveYourselfWindow(MSTopLevel *tl_) 
{
  static MSTopLevel *_saveYourSelf = 0;
  if (0!=_saveYourSelf) return -1;
  _saveYourSelf = tl_;
  // Need to get around protected MSToplevel::setWMSaveYourself()
  if (tl_->widgetType() == AplusShell::symbol())
   {
     ((AplusShell *) tl_)->setWMSaveYourself();
   }
  else   if (tl_->widgetType() == AplusPopup::symbol())
   {
     ((AplusPopup *) tl_)->setWMSaveYourself();
   }
  return 0;
}


static MSTopLevel *g_saveYourselfWindow(void) { return (MSTopLevel *) 0; }
//static MSTopLevel *g_saveYourselfWindow(void) {          /* FIX THIS */
//  return TopLevelW::saveYourselfWindow();}

static void s_parent(MSWidgetView *pWidgetView_,MSWidgetView *p_)
{ if (p_==0) 
   { MSShell *p=new AplusShell;(void)p->windowGroup(g_defaultLeader()); p_=p; }
  else if (p_==(MSWidgetView *)-1) 
   { MSPopup *p=new AplusPopup;(void)p->windowGroup(g_defaultLeader()); p_=p; }
  pWidgetView_->reparent(p_); 
}
static void s_resizeOptions(MSWidgetView *pWidgetView_,unsigned long opts_)
{
  enum ResizeOptions { Center=0,Left=4,Right=8,Top=16,Bottom=32,
		       SmallWidth=64,SmallHeight=128,LikeWidth=256,LikeHeight=512};
  unsigned long constraint = At::Default;

  if (opts_ & Left       ) constraint |= At::Left;
  if (opts_ & Right      ) constraint |= At::Right;
  if (opts_ & Top        ) constraint |= At::Top;
  if (opts_ & Bottom     ) constraint |= At::Bottom;
  if (opts_ & SmallWidth ) constraint |= At::MinimizeWidth;
  if (opts_ & SmallHeight) constraint |= At::MinimizeHeight;
  if (opts_ & LikeWidth  ) constraint |= At::MaintainWidth;
  if (opts_ & LikeHeight ) constraint |= At::MaintainHeight;

  pWidgetView_->resizeConstraints(constraint);
}

static unsigned long g_resizeOptions(MSWidgetView *pWidgetView_)
{
  enum ResizeOptions { Center=0,Left=4,Right=8,Top=16,Bottom=32,
		       SmallWidth=64,SmallHeight=128,LikeWidth=256,LikeHeight=512};
  unsigned long constraint = pWidgetView_->resizeConstraints();
  unsigned long result = Center;
  
  if (constraint & At::Left          ) result |= Left;
  if (constraint & At::Right         ) result |= Right;
  if (constraint & At::Top           ) result |= Top;
  if (constraint & At::Bottom        ) result |= Bottom;
  if (constraint & At::MinimizeWidth ) result |= SmallWidth;
  if (constraint & At::MinimizeHeight) result |= SmallHeight;
  if (constraint & At::MaintainWidth ) result |= LikeWidth;
  if (constraint & At::MaintainHeight) result |= LikeHeight;

  return result;
}

static void s_sflags(MSWidgetCommon *pWidgetView_,A flags_)
{ if (pWidgetView_->model()) ((AplusModel*)pWidgetView_->model())->sflags(flags_); }
static A g_sflags(MSWidgetCommon *pWidgetView_)
{
  if (pWidgetView_->model()) return ((AplusModel*)pWidgetView_->model())->sflags();
  else return aplus_nl;
}

/* TO MAKE THINGS COMPILABLE */
static void s_colorBgPixel(void *,unsigned long ) {}
static void s_colorBgString(void *,char *) {}
static unsigned long g_colorBgPixel(void *) { return 0;}
static A g_colorBgString(void *) { return aplus_nl; }
static void s_colorFgPixel(void *,unsigned long) {}
static void s_colorFgString(void *,char *) {}
static unsigned long g_colorFgPixel(void *) { return 0;}
static A g_colorFgString(void *) {return aplus_nl;}

//  ColorObject is non-existent in MSTK                                            /* FIX THIS */
/*
static void s_colorBgPixel(ColorObject *win_,unsigned long pixel_) {win_->setColorBg(pixel_);}
static void s_colorBgString(ColorObject *win_,char *str_) {win_->setColorBg(str_);}
static unsigned long g_colorBgPixel(ColorObject *win_) {return win_->pixelBgValue();}
static A g_colorBgString(ColorObject *win_) {return (A)(gsv(0,(char *)win_->colorBg()));}

static void s_colorFgPixel(ColorObject *win_,unsigned long pixel_) {win_->setColorFg(pixel_);}
static void s_colorFgString(ColorObject *win_,char *str_) {win_->setColorFg(str_);}
static unsigned long g_colorFgPixel(ColorObject *win_) {return win_->pixelFgValue();}
static A g_colorFgString(ColorObject *win_) {return (A)(gsv(0,(char *)win_->colorFg()));}
*/

int bServerParseColor(MSDisplayServer *s_,char * cp_) 
{ 
  XColor rgb;
  XParseColor(s_->display(),s_->colorManager()->colormap(),cp_,&rgb); 
  return rgb.pixel;
}

int bAllocColor(MSDisplayServer *s_,char *cp_) 
{ 
  static XColor rgb;
  XParseColor(s_->display(),s_->colorManager()->colormap(),cp_,&rgb); 
  int status=XAllocColor(s_->display(),s_->colorManager()->colormap(),&rgb);
  return (status!=0)?(int)rgb.pixel:-1;
}

static void bFreeColor(MSDisplayServer *s_,unsigned long pixel_) 
{ 
  XFreeColors(s_->display(),s_->colorManager()->colormap(),&pixel_,1,0); 
}

static MSWidgetView *g_parent(MSWidgetView *pWidgetView_)
{ return (MSWidgetView *)pWidgetView_->owner(); }

static MSWidgetView *g_focus(MSWidgetView *pWidgetView_)
{
  MSWidget *pFocusWidget=pWidgetView_->inputFocus();
  //
  // Make sure that we have an A+ widget here, not an MStk widget.  This
  // especially applies to composite A+ widgets, such as AplusSlot, since
  // in MStk every field is also a widget.  If pFocusWidget is an MStk
  // widget (e.g., a slot field), then go up the widget hierarchy until
  // we find an A+ widget.
  //
  while (pFocusWidget!=0 && isAplusWidget(pFocusWidget)==MSFalse)
    {
      pFocusWidget = pFocusWidget->owner();
    }

  // Either the A+ widget that has the focus or 0 will be returned to s;
  // s should handle the null pointer case properly.
  //
  return (MSWidgetView *)pFocusWidget;
}

static MSWidgetView *g_focusWindow(void) { return (MSWidgetView *)MSWidget::focusWindow(); }

static I g_freeze(MSWidgetView *pWidgetView_) 
{ return MSTrue==pWidgetView_->frozen() ? 1 : 0; }
static I g_visible(MSWidgetView *pWidgetView_) 
{ return MSTrue==pWidgetView_->visible() ? 1 : 0; }
static I g_sensitive(MSWidgetView *pWidgetView_) 
{ return MSTrue==pWidgetView_->sensitive() ? 1 : 0; }
static I g_dynamic(MSWidgetView *pWidgetView_) 
{ return MSTrue==pWidgetView_->dynamic() ? 1 : 0; }
static void s_sensitive(MSWidgetView *pWidgetView_,MSBoolean b_) { pWidgetView_->sensitive(b_); }
static void s_dynamic(MSWidgetView *pWidgetView_,MSBoolean b_) { pWidgetView_->dynamic(b_); }
static void s_background(MSWidgetView *pWidgetView_,unsigned long p_)
{pWidgetView_->background(p_);}
static unsigned long g_background(MSWidgetView *pWidgetView_) {return pWidgetView_->background();}
static void s_foreground(MSWidgetView *pWidgetView_,unsigned long p_)  
{ pWidgetView_->foreground(p_); }
static unsigned long g_foreground(MSWidgetView *pWidgetView_) {return pWidgetView_->foreground();}

static void s_hlColor(MSWidgetView *pWidgetView_,unsigned long p_)
{ if (pWidgetView_!=0) pWidgetView_->highlightColor(p_); }

static unsigned long g_hlColor(MSWidgetView *pWidgetView_)
{return (pWidgetView_!=0) ? pWidgetView_->highlightColor() : 0;}

static void s_hlThickness(MSWidgetView *pWidgetView_,int ht_)
{ if (pWidgetView_!=0) pWidgetView_->highlightThickness(ht_); }

static I g_hlThickness(MSWidgetView *pWidgetView_)
{return (pWidgetView_!=0) ? (I) pWidgetView_->highlightThickness() : 0;}

static void s_shadowThickness(MSWidgetView *pWidgetView_,int st_)
{ if (pWidgetView_!=0) pWidgetView_->shadowThickness(st_); }

static int g_shadowThickness(MSWidgetView *pWidgetView_)
{return (pWidgetView_!=0) ? pWidgetView_->shadowThickness() : 0;}

static void s_shadowStyle(MSWidgetView *pWidgetView_, A value_)
{
  AplusShadowStyleConverter converter;
  unsigned long style = converter(value_);
  if (style!=converter.enumNotFound())
    {
      pWidgetView_->shadowStyle((MSShadowStyle)style);
    }
}

static A g_shadowStyle(MSWidgetView *pWidgetView_)
{
  AplusShadowStyleConverter converter;
  return converter(pWidgetView_->shadowStyle());
}

static void s_titleAlign(MSWidgetView *pWidgetView_, A value_)
{
  const MSSymbol& type(pWidgetView_->widgetType());
  //
  // need RTTI here to effectively determine if pWidgetView_ is derived
  // from MSWidgetCommon since that's where titleAlignment() is defined;
  // AplusTableColumn and AplusTraceSet are the only A+ widgets *not*
  // derived from MSWidgetCommon
  //
  if (type==AplusTableColumn::symbol() || type==AplusTraceSet::symbol())
    {
      // the way it's set up in s this should never happen because `titlejustify
      // attribute is defined only for `real classes, which `tableField and `graphTrace
      // are *not*, but we check here just in case
      //
      return;
    }

  AplusAlignmentConverter converter;
  unsigned long justify = converter(value_);
  if (justify!=converter.enumNotFound())
    {
      ((MSWidgetCommon *)pWidgetView_)->titleAlignment(justify);
    }
}

static A g_titleAlign(MSWidgetView *pWidgetView_)
{
  const MSSymbol& type(pWidgetView_->widgetType());
  //
  // need RTTI here to effectively determine if pWidgetView_ is derived
  // from MSWidgetCommon since that's where titleAlignment() is defined;
  // AplusTableColumn and AplusTraceSet are the only A+ widgets *not*
  // derived from MSWidgetCommon
  //
  if (type==AplusTableColumn::symbol() || type==AplusTraceSet::symbol())
    {
      // the way it's set up in s this should never happen because `titlejustify
      // attribute is defined only for `real classes, which `tableField and `graphTrace
      // are *not*, but we check here just in case
      //
      return aplus_nl;
    }

  AplusAlignmentConverter converter;
  return converter(((MSWidgetCommon *)pWidgetView_)->titleAlignment());
}

static void s_font(MSWidgetView *pWidgetView_,Font id_) {pWidgetView_->font(id_);}
static Font g_font(MSWidgetView *pWidgetView_) { return pWidgetView_->font(); }
static void s_windowName(MSTopLevel *pWidgetView_,char *name_) { pWidgetView_->windowTitle(name_); }
static void s_iconName(MSTopLevel *pWidgetView_,char *name_) { pWidgetView_->iconTitle(name_); }
static A g_windowName(MSTopLevel *pWidgetView_)              
{
  MSString name(pWidgetView_->windowTitle());
  A r=gsv(0,(char *)name.string());
  return r;
}
static A g_iconName(MSTopLevel *pWidgetView_)
{
  MSString name(pWidgetView_->iconTitle());
  A r=gsv(0,(char *)name.string());
  return r;
}
static void s_freeze(MSWidgetView *pWidgetView_,MSBoolean b_) 
{
  if (b_==MSTrue) pWidgetView_->freeze();
  else
  {
    pWidgetView_->unfreeze();
    pWidgetView_->redraw();
  }
}

//  MSDisplayServer 'A' interface functions
static Display *g_serverDisplay(MSDisplayServer *server_) {return server_->display();}
static I 	g_screenWidth(MSDisplayServer *server_)   {return (I) server_->width();}
static I 	g_screenHeight(MSDisplayServer *server_)  {return (I) server_->height();}


// AplusCollapsible interface functions
static void s_handleBg(MSCollapsibleLayout *cl_, unsigned long bg_)
{ cl_->handleBackground(bg_); }
static unsigned long g_handleBg(MSCollapsibleLayout *cl_)
{ return cl_->handleBackground(); }
static void s_handleSize(MSCollapsibleLayout *cl_, unsigned long size_)
{ cl_->handleSize(size_); }
static unsigned long g_handleSize(MSCollapsibleLayout *cl_)
{ return cl_->handleSize(); }

static void s_open(MSCollapsibleLayout *cl_, A open_)
{ 
  if( open_ && QA( open_) )
    {
      int childCount=cl_->children().length();
	if( open_->t==Et && open_->n==childCount )
	  {
	    for(int i=0; i<open_->n; i++)
	      {
		A wrk=(A)open_->p[i];
		if( wrk && QA(wrk) && wrk->t==It && wrk->n==1)
		  wrk->p[0] ? 
		    cl_->open(cl_->children()(i)): 
		    cl_->close(cl_->children()(i));
	      }
	    
	  }
	else if(open_->t==It && open_->n==1)
	  {
	    for(int i=0; i<childCount; i++)
	      open_->p[0] ? 
		cl_->open(cl_->children()(i)):
	        cl_->close(cl_->children()(i));
		
	  }
    }
}
static A g_open(MSCollapsibleLayout *cl_)
{ 
  int childCount=cl_->children().length();
  A z=gv(Et,childCount);

  for(int i=0; i<childCount; i++)
      z->p[i]=(I)gi( MSTrue==cl_->isOpened(cl_->children()(i)) ? 1:0);

  return z;
}

static void s_toolTip(MSCollapsibleLayout *cl_, A toolTip_)
{ 
  if( toolTip_ && QA( toolTip_) )
    {
      int childCount=cl_->children().length();
	if( toolTip_->t==Et && toolTip_->n==childCount )
	  {
	    for(int i=0; i<toolTip_->n; i++)
	      {
		A wrk=(A)toolTip_->p[i];
		if( wrk && QA(wrk) && wrk->t==Ct)
		  {
		    MSString tip((char *)wrk->p,wrk->n);
		    MSStringVector tips;
		    tips<<tip;
		    cl_->handleToolTip(cl_->children()(i),tips);
		  }

	      }
	    
	  }
	else if(toolTip_->t==Ct)
	  {
	    for(int i=0; i<childCount; i++)
	      {
		MSString tip((char *)toolTip_->p,toolTip_->n);
		MSStringVector tips;
		tips<<tip;
		cl_->handleToolTip(cl_->children()(i),tips);
	      }
		
	  }
    }
}

static A g_toolTip(MSCollapsibleLayout *cl_)
{ 
  int childCount=cl_->children().length();
  A z=gv(Et,childCount);

  for(int i=0; i<childCount; i++)
    {
      MSStringVector tips=cl_->handleToolTip(cl_->children()(i));
      MSString tip;
      for(int j=0; j<tips.length();j++) tip<<tips(i);
      z->p[i]=(I)gsv(0,(char *)tip.string());
    }
  return z;
}

// AXPassword interface functions
static I g_passwordValidity(AplusPassword *o_) 	       
{ return MSTrue==o_->valid() ? 1 : 0; }
static I  	 g_passwordFillChar(AplusPassword *o_) 	       { return (I)o_->fillChar(); }
static void 	 s_passwordFillChar(AplusPassword *o_,int ch_) { o_->fillChar((char)ch_); }


// AplusTreeView interface functions

static void s_treeSelectedNode(AplusTreeView *tv_, A value_) { tv_->selectedNodeA(value_); }
static A g_treeSelectedNode(AplusTreeView *tv_) { return tv_->selectedNodeA(); }

static void s_treeNodeFg(AplusTreeView *tv_,unsigned long fg_) {tv_->nodeForeground(fg_);}
static unsigned long g_treeNodeFg(AplusTreeView *tv_) {return tv_->nodeForeground();}
static void s_treeNodeBg(AplusTreeView *tv_,unsigned long bg_) {tv_->nodeBackground(bg_);}
static unsigned long g_treeNodeBg(AplusTreeView *tv_) {return tv_->nodeBackground();}
static void s_treeSelectedNodeFg(AplusTreeView *tv_,unsigned long fg_) {tv_->selectedNodeForeground(fg_);}
static unsigned long g_treeSelectedNodeFg(AplusTreeView *tv_) {return tv_->selectedNodeForeground();}
static void s_treeSelectedNodeBg(AplusTreeView *tv_,unsigned long bg_) {tv_->selectedNodeBackground(bg_);}
static unsigned long g_treeSelectedNodeBg(AplusTreeView *tv_) {return tv_->selectedNodeBackground();}

static void s_treeVerticalSpace(AplusTreeView *tv_,int num_)
{tv_->verticalSpacing(0<num_?num_:0);}
static I g_treeVerticalSpace(AplusTreeView *tv_) {return (I) tv_->verticalSpacing();}
static void s_treeHorizontalSpace(AplusTreeView *tv_,int num_)
{tv_->horizontalSpacing(0<num_?num_:0);}
static I g_treeHorizontalSpace(AplusTreeView *tv_) {return (I) tv_->horizontalSpacing();}

static void s_treeOrientation(AplusTreeView *tv_, A sym_)
{
  if (!QS(sym_)&&(sym_->t==Et&&sym_->n>0&&QS(*sym_->p)))
  {
    if(XS(*sym_->p)==si("vertical")) tv_->orientation(AplusTreeView::Vertical);
    else if(XS(*sym_->p)==si("horizontal")) tv_->orientation(AplusTreeView::Horizontal);
  }

}
static A g_treeOrientation(AplusTreeView *tv_)
{
  return gsym((AplusTreeView::Vertical==tv_->orientation())?"vertical":"horizontal");
}


static void s_treeShowButtons(AplusTreeView *tv_, MSBoolean b_)
{ tv_->showButtons(b_); }

static I g_treeShowButtons(AplusTreeView *tv_)
{ return MSTrue==tv_->showButtons() ? 1 : 0; }

static void s_treeLineColor(AplusTreeView *tv_, unsigned long color_)
{ tv_->lineForeground(color_); }

static unsigned long g_treeLineColor(AplusTreeView *tv_)
{ return tv_->lineForeground(); }

// AplusTextM interface functions
extern "C" A gst(I x, C *s);
static A g_textBuffer(AplusText *o_)                      { return (A)gst(0,(char *)o_->string()); }
static I g_textNumVisibleRows(AplusText *mp_)           { return (I) mp_->rows();}
static void s_textNumVisibleRows(AplusText *mp_,int num_) { mp_->rows(num_); }
static I g_textNumVisibleCols(AplusText *mp_)           { return (I) mp_->columns();}
static void s_textNumVisibleCols(AplusText *mp_,int num_) { mp_->columns(num_); }
static I g_textCursorPosition(AplusText *o_)            { return (I) o_->cursorPosition();}
static void s_textCursorPosition(AplusText *o_,int cp_)   { o_->cursorPosition(cp_);}

// AXCommand interface functions
static char *g_commandBuffer(AplusCommand *o_) {return (char *)o_->buffer(); }
static void s_commandBuffer(AplusCommand *o_,A a_) { o_->buffer(a_); }
static I g_commandCursorPosition(AplusCommand *o_) {return (I) o_->cursorPosition();}
static void s_commandCursorPosition(AplusCommand *o_,int cp_) 
{ o_->cursorPosition(cp_); }

// AXPage interface functions
static A g_pageCursor(AplusPage *p_) { return p_->cursorPosition(); }
static unsigned long g_pageBlinkRate(AplusPage *p_) { return p_->blinkRate(); }
static void s_pageBlinkRate(AplusPage *p_,unsigned long r_) { p_->blinkRate(r_); }
static A g_pageColorTable(AplusPage *p_) { return p_->colorTable(); }
static void s_pageColorTable(AplusPage *p_,A ct_) { p_->colorTable(ct_); }
static A g_pageBoxes(AplusPage *p_) { return p_->boxes(); }
static void s_pageBoxes(AplusPage *p_,A b_) { p_->boxes(b_); }
static A g_pageLines(AplusPage *p_) { return p_->lines(); }
static void s_pageLines(AplusPage *p_,A b_) { p_->lines(b_); }
static I g_pageLineWidth(AplusPage *p_) { return (I) p_->lineWidth(); }
static void s_pageLineWidth(AplusPage *p_,int lw_) { p_->lineWidth(lw_); }
static A g_pageBoxColors(AplusPage *p_) { return p_->boxColorVector(); }
static void s_pageBoxColors(AplusPage *p_,A b_) { p_->boxColorVector(b_); }
static A g_pageRubberBand(AplusPage *p_) { return p_->rBand(); }
static A g_pageKeyBuffer(AplusPage *p_) { return p_->keyBuf(); }

static void s_pageIndexFunc(AplusPage *page_,A fc_)
{ if (fc_->t==Et && fc_->n==2) 
   { I f=fc_->p[0];
     I c=fc_->p[1];
     AClientData *ac=new AClientData((A)f,(A)c);
     page_->indexFunc((AFunc)cdipv,ac);     
   }
  else if (isNull(fc_)==MSTrue) page_->indexFunc((AFunc)0,0);     
  else showError("Invalid 'pageIndex' Function Specification");
}
static A g_pageIndexFunc(AplusPage *page_)
{  return (A)getVarFunc((AClientData *)page_->indexFunc()->arg()); }

static void s_pageBoldFunc(AplusPage *page_,A fc_)
{ if (fc_->t==Et && fc_->n==2) 
   { I f=fc_->p[0];
     I c=fc_->p[1];
     AClientData *ac=new AClientData((A)f,(A)c);
     page_->boldFunc((AFunc)cdipv,ac);     
   }
  else if (isNull(fc_)==MSTrue) page_->boldFunc((AFunc)0,0);     
  else showError("Invalid 'pageBold' Function Specification");
}
static A g_pageBoldFunc(AplusPage *page_)
{  return (A)getVarFunc((AClientData *)page_->boldFunc()->arg()); }

static void s_pageBlinkFunc(AplusPage *page_,A fc_)
{ if (fc_->t==Et && fc_->n==2) 
   { I f=fc_->p[0];
     I c=fc_->p[1];
     AClientData *ac=new AClientData((A)f,(A)c);
     page_->blinkFunc((AFunc)cdipv,ac);     
   }
  else if (isNull(fc_)==MSTrue) page_->blinkFunc((AFunc)0,0);     
  else showError("Invalid 'pageBlink' Function Specification");
}
static A g_pageBlinkFunc(AplusPage *page_)
{  return (A)getVarFunc((AClientData *)page_->blinkFunc()->arg()); }

static void s_pageUnderlineFunc(AplusPage *page_,A fc_)
{ if (fc_->t==Et && fc_->n==2) 
   { I f=fc_->p[0];
     I c=fc_->p[1];
     AClientData *ac=new AClientData((A)f,(A)c);
     page_->underlineFunc((AFunc)cdipv,ac);     
   }
  else if (isNull(fc_)==MSTrue) page_->underlineFunc((AFunc)0,0);     
  else showError("Invalid 'pageUnderline' Function Specification");
}
static A g_pageUnderlineFunc(AplusPage *page_)
{  return (A)getVarFunc((AClientData *)page_->underlineFunc()->arg()); }


//  XManager 'A' interface functions
static I g_numChildren(AplusManager *mp_) { return (I) mp_->childCount(); }
static char *g_serverName(MSDisplayServer *server_) {return (char *) server_->name();}


// AXBtnBox 'A' interface functions  /* maps to MSButtonBox */
static void s_actionBoxOptions(AplusButtonBox *ab_,A sym_)
{ ab_->alignment(GUIEnum.alignFormat(sym_));}
static A g_actionBoxOptions(AplusButtonBox *ab_)
{ return GUIEnum.alignFormat((unsigned long)ab_->alignment()); }

static void s_actionHlColor(AplusButtonBox *o_, unsigned long c_) { o_->highlightColor(c_); }
static unsigned long g_actionHlColor(AplusButtonBox *o_) { return o_->highlightColor(); }
static void s_actionHlThickness(AplusButtonBox *o_, int t_) { o_->buttonHighlightThickness(t_); }
static I g_actionHlThickness(AplusButtonBox *o_) { return (I) o_->buttonHighlightThickness(); }
static void s_actionButtonShadowThickness(AplusButtonBox *o_, int t_) { o_->buttonShadowThickness(t_); }
static I g_actionButtonShadowThickness(AplusButtonBox *o_) { return (I) o_->buttonShadowThickness(); }

static void s_checkHlColor(AplusCheckBox *o_, unsigned long c_) { o_->highlightColor(c_); }
static unsigned long g_checkHlColor(AplusCheckBox *o_) { return o_->highlightColor(); }
static void s_checkHlThickness(AplusCheckBox *o_, int t_) { o_->buttonHighlightThickness(t_); }
static I g_checkHlThickness(AplusCheckBox *o_) { return (I) o_->buttonHighlightThickness(); }

static void s_radioHlColor(AplusRadioBox *o_, unsigned long c_) { o_->highlightColor(c_); }
static unsigned long g_radioHlColor(AplusRadioBox *o_) { return o_->highlightColor(); }
static void s_radioHlThickness(AplusRadioBox *o_, int t_) { o_->buttonHighlightThickness(t_); }
static I g_radioHlThickness(AplusRadioBox *o_) { return (I) o_->buttonHighlightThickness(); }

// XTextObject 'A' interface functions /* maps to MSPrimitiveText */
static void s_labelOptions(MSPrimitiveText *pt_,A x_)
{ pt_->alignment(GUIEnum.alignFormat(x_)); }
static A g_labelOptions(MSPrimitiveText *pt_)
{ return GUIEnum.alignFormat((unsigned long) pt_->alignment()); }


// XManager attribute access functions
static void s_mgrDoubleClickTime(AplusManager *,unsigned long time_) 
{ MSApplication::doubleClickInterval(time_); }
static unsigned long g_mgrDoubleClickTime(AplusManager *) 
{ return MSApplication::doubleClickInterval(); }


static void s_mgrLongPressTime(AplusManager *,unsigned long) {}
static unsigned long g_mgrLongPressTime(AplusManager *) { return 0;}

//static void s_mgrLongPressTime(XManager *mgr_,unsigned long time_)  /* Non-existent */
//{ mgr_->longPressTime(time_); }
//static unsigned long g_mgrLongPressTime(XManager *mgr_) 
//{ return (unsigned long) mgr_->longPressTime(); }

/*
 *  XScale access functions
 */
static void s_scaleValueMin(MSScale *sp_,A value_) 
{ if (!QS(value_)&&(value_->t==It||value_->t==Ft))
   { P p; p.i=value_->p; sp_->valueMin(value_->t==Ft?p.f[0]:(double)p.i[0]); } }
static A g_scaleValueMin(MSScale *sp_) { A r=gf(sp_->valueMin()); return r; }

static void s_scaleValueMax(MSScale *sp_,A value_) 
{ if (!QS(value_)&&(value_->t==It||value_->t==Ft))
   { P p; p.i=value_->p; sp_->valueMax(value_->t==Ft?p.f[0]:(double)p.i[0]); } }
static A g_scaleValueMax(MSScale *sp_) { A r=gf(sp_->valueMax()); return r; }

static void s_scaleValueInc(MSScale *sp_,A value_) 
{ if (!QS(value_)&&(value_->t==It||value_->t==Ft))
   { P p; p.i=value_->p; sp_->valueInc(value_->t==Ft?p.f[0]:(double)p.i[0]); } }
static A g_scaleValueInc(MSScale *sp_) { A r=gf(sp_->valueInc()); return r; }

static void s_scaleValuePageInc(MSScale *sp_,A value_) 
{ if (!QS(value_)&&(value_->t==It||value_->t==Ft))
   { P p; p.i=value_->p; sp_->valuePageInc(value_->t==Ft?p.f[0]:(double)p.i[0]); } }
static A g_scaleValuePageInc(MSScale *sp_) { A r=gf(sp_->valuePageInc()); return r; }

static void s_scaleValueFont(MSScale *sp_,Font font_) { sp_->valueFont(font_); }
static Font g_scaleValueFont(MSScale *sp_) { return (Font)sp_->valueFont(); }

static void s_scaleValueFg(MSScale *sp_,unsigned long fg_) { sp_->valueForeground(fg_); }
static unsigned long g_scaleValueFg(MSScale *sp_) { return sp_->valueForeground(); }

static void s_scaleValueAlign(MSScale *sp_,A sym_){ sp_->valueAlignment(GUIEnum.alignFormat(sym_)); }
static A g_scaleValueAlign(MSScale *sp_) { return GUIEnum.alignFormat(sp_->valueAlignment()); }

static void s_scaleSliderBg(MSScale *sp_,unsigned long bg_) { sp_->sliderBackground(bg_); }
static unsigned long g_scaleSliderBg(MSScale *sp_) { return sp_->sliderBackground(); }

// SliderHeight and sliderwidth will call sliderSize
static void s_scaleSliderHeight(MSScale *sp_,A value_) 
{ if (!QS(value_)&&(value_->t==It||value_->t==Ft))
   { P p; p.i=value_->p; sp_->sliderSize((int)(value_->t==Ft?p.f[0]:p.i[0])); } }
static A g_scaleSliderHeight(MSScale *sp_) { A r=gf(sp_->sliderSize()); return r; }

static void s_scaleSliderWidth(MSScale *sp_,A value_) 
{ if (!QS(value_)&&(value_->t==It||value_->t==Ft))
   { P p; p.i=value_->p; sp_->sliderSize((int)(value_->t==Ft?p.f[0]:p.i[0])); } }
static A g_scaleSliderWidth(MSScale *sp_) { A r=gf(sp_->sliderSize()); return r; }

static void s_scaleTitleAlign(MSScale *sp_,A x_){ sp_->titleAlignment(GUIEnum.alignFormat(x_));}
static A g_scaleTitleAlign(MSScale *sp_) { return GUIEnum.alignFormat(sp_->titleAlignment()); }

static void s_scaleSubtitle(MSScale *sp_,A title_)
{
  MSStringVector t;
  asTitleStringVector(t, title_);
  sp_->subtitle(t);
}
static A g_scaleSubtitle(MSScale *sp_)
{ 
  A ap = aplus_nl;
  const MSStringVector &t=sp_->subtitle();
  long d[MAXR]={ 0, 0, 0, 0, 0, 0, 0, 0, 0 };
  long xrho=t.length();
  d[0]=xrho;
  ap = ga(Et, 1, xrho, d);
  for (long i=0; i<xrho; i++)
  {
    d[0]=t.elementAt(i).length();
    ap->p[i]=(long) gc(Ct, 1, t.elementAt(i).length(), d, (long *)t.elementAt(i).string());
  }
  return ap;
}

static void s_scaleSubtitleFont(MSScale *sp_,Font font_) { sp_->subtitleFont(font_); }
static Font g_scaleSubtitleFont(MSScale *sp_) { return (Font)sp_->subtitleFont(); }

static void s_scaleSubtitleAlign(MSScale *sp_,A x_) { sp_->subtitleAlignment(GUIEnum.alignFormat(x_)); }
static A g_scaleSubtitleAlign(MSScale *sp_) { return GUIEnum.alignFormat(sp_->subtitleAlignment()); }

static void s_scaleSubtitleFg(MSScale *sp_,unsigned long fg_) { sp_->subtitleForeground(fg_); }
static unsigned long g_scaleSubtitleFg(MSScale *sp_) { return sp_->subtitleForeground(); }

static void s_scaleMintitle(MSScale *sp_,A title_)
{
  MSStringVector t;
  asTitleStringVector(t, title_);
  sp_->mintitle(t);
}

static A g_scaleMintitle(MSScale *sp_)
{ 
  A ap = aplus_nl;
  const MSStringVector &t=sp_->mintitle();
  long d[MAXR]={ 0, 0, 0, 0, 0, 0, 0, 0, 0 };
  long xrho=t.length();
  d[0]=xrho;
  ap = ga(Et, 1, xrho, d);
  for (long i=0; i<xrho; i++)
  {
    d[0]=t.elementAt(i).length();
    ap->p[i]=(long) gc(Ct, 1, t.elementAt(i).length(), d, (long *)t.elementAt(i).string());
  }
  return ap;
}


static void s_scaleMintitleFg(MSScale *sp_,unsigned long fg_) { sp_->mintitleForeground(fg_); }
static unsigned long g_scaleMintitleFg(MSScale *sp_) { return sp_->mintitleForeground(); }

static void s_scaleMintitleAlign(MSScale *sp_,A x_) { sp_->mintitleAlignment(GUIEnum.alignFormat(x_)); }
static A g_scaleMintitleAlign(MSScale *sp_) { return GUIEnum.alignFormat(sp_->mintitleAlignment()); }

static void s_scaleMintitleFont(MSScale *sp_,Font font_) { sp_->mintitleFont(font_); }
static Font g_scaleMintitleFont(MSScale *sp_) { return (Font)sp_->mintitleFont(); }

static void s_scaleMaxtitle(MSScale *sp_,A title_)
{
  MSStringVector t;
  asTitleStringVector(t, title_);
  sp_->maxtitle(t);
}
static A g_scaleMaxtitle(MSScale *sp_)
{
  A ap = aplus_nl;
  const MSStringVector &t=sp_->maxtitle();
  long d[MAXR]={ 0, 0, 0, 0, 0, 0, 0, 0, 0 };
  long xrho=t.length();
  d[0]=xrho;
  ap = ga(Et, 1, xrho, d);
  for (long i=0; i<xrho; i++)
  {
    d[0]=t.elementAt(i).length();
    ap->p[i]=(long) gc(Ct, 1, t.elementAt(i).length(), d, (long *)t.elementAt(i).string());
  }
  return ap;
}

static void s_scaleMaxtitleFg(MSScale *sp_,unsigned long fg_) { sp_->maxtitleForeground(fg_); }
static unsigned long g_scaleMaxtitleFg(MSScale *sp_) { return sp_->maxtitleForeground(); }

static void s_scaleMaxtitleAlign(MSScale *sp_,A x_) { sp_->maxtitleAlignment(GUIEnum.alignFormat(x_)); }
static A g_scaleMaxtitleAlign(MSScale *sp_) { return GUIEnum.alignFormat(sp_->maxtitleAlignment()); }

static void s_scaleMaxtitleFont(MSScale *sp_,Font font_) { sp_->maxtitleFont(font_); }
static Font g_scaleMaxtitleFont(MSScale *sp_) { return (Font)sp_->maxtitleFont(); }

static void s_scaleLabelInc(MSScale *sp_,A value_) 
{ if (!QS(value_)&&(value_->t==It||value_->t==Ft))
   { P p; p.i=value_->p; sp_->labelInc(value_->t==Ft?p.f[0]:(double)p.i[0]); } }
static A g_scaleLabelInc(MSScale *sp_) { A r=gf(sp_->labelInc()); return r; }

static void s_scaleLabelFont(MSScale *sp_,Font font_) { sp_->labelFont(font_); }
static Font g_scaleLabelFont(MSScale *sp_) { return (Font)sp_->labelFont(); }

static void s_scaleLabelFg(MSScale *sp_,unsigned long fg_) { sp_->labelForeground(fg_); }
static unsigned long g_scaleLabelFg(MSScale *sp_) { return sp_->labelForeground(); }

static void s_scaleLabelAlign(MSScale *sp_,A x_) { sp_->labelAlignment(GUIEnum.alignFormat(x_)); }
static A g_scaleLabelAlign(MSScale *sp_) { return GUIEnum.alignFormat(sp_->labelAlignment()); }

static void s_scaleLabelOut(MSScale *,A ) {}

//void s_scaleLabelOut(MSScale *sp_,A fc_)
//{ 
//  if (!QS(fc_)&&(fc_->t==Et&&fc_->n>0&&QS(*fc_->p)))
//   {
//     sp_->labelFormat(fc_);
//   }
//  else if (fc_->t==Et&&fc_->n==2)
//   {
//     P p; p.i = fc_->p;
//     if (QA(p.a[0])&&(p.a[0]->t==Et&&p.a[0]->n>0&&QS(*p.a[0]->p)))
//      {
//	sp_->labelFormat(p.a[0]);
//	P pp; pp.i = p.a[1]->p;
//	if (pp.i!=0) sp_->labelPrecision((int)pp.i[0]);
//      }
//     else sp_->labelFormatFunc((AFunc)cdipv,fc_);
//   }
//  else if (isNull(fc_)==MSTrue) sp_->labelFormatFunc((AFunc)0,0);
//  else showError("Invalid 'label format' Function Specification");
//}

static A g_scaleLabelOut(MSScale *) { return aplus_nl; }

//A g_scaleLabelOut(MSScale *sp_)
//{ 
//  A ac=(A)getVarFunc((AClientData *)sp_->labelFormatFunc()->arg());
//  return (isNull(ac)==MSTrue) ? sp_->labelFormatSym() : ac; 
//}

static void s_scaleMajorTickSize(MSScale *sp_,int val_) { sp_->majorTickSize(val_); }
static I g_scaleMajorTickSize(MSScale *sp_) { return (I)sp_->majorTickSize(); }
static void s_scaleMinorTickSize(MSScale *sp_,int val_) { sp_->minorTickSize(val_); }
static I g_scaleMinorTickSize(MSScale *sp_) { return (I)sp_->minorTickSize(); }
static void s_scaleMinorTickCt(MSScale *sp_,int val_) { sp_->minorTickCount(val_); }
static I g_scaleMinorTickCt(MSScale *sp_) { return (I)sp_->minorTickCount(); }

static void s_scaleEditorBg(MSScale *,unsigned long) { }
static void s_scaleEditorFg(MSScale *,unsigned long) { }
static unsigned long g_scaleEditorBg(MSScale *) { return 0; }
static unsigned long g_scaleEditorFg(MSScale *) { return 0; }

// void s_scaleEditorBg(MSScale *sp_,unsigned long fg_) { sp_->editor()->background(fg_); } 
// void s_scaleEditorFg(MSScale *sp_,unsigned long fg_) { sp_->editor()->foreground(fg_); } 
// unsigned long g_scaleEditorBg(MSScale *sp_) { return sp_->editor()->background(); } 
// unsigned long g_scaleEditorFg(MSScale *sp_) { return sp_->editor()->foreground(); } 

static void s_scaleEdit(MSScale *,MSBoolean)
{
//   if (b_==MSTrue)
//   {
//     sp_->editor()->font(sp_->valueFont());
//     sp_->editor()->resize(sp_->valueWin()->width(),sp_->valueWin()->height());
//     sp_->editor()->moveTo(sp_->valueWin()->x_origin(),sp_->valueWin()->y_origin());
//     sp_->editor()->string(sp_->valueStr());
//     sp_->editor()->map();
//     sp_->editor()->focusIn();
//     sp_->editor()->obtainFocus();
//   }
//   else
//   {
//     sp_->editorActivate();
//   }
}

static I g_scaleEdit(MSScale *){ return 0; } // MSFalse

// MSBoolean g_scaleEdit(MSScale *sp_){ return sp_->editor()->mapped(); }




//  AXTableVar 'A' interface functions   /* MSTableColumn */

static void s_fieldColumnAlignment(MSWidgetView *o_, A sym_)
{
  AplusTableColumn *tc = (AplusTableColumn *)o_;
  if (tc) tc->columnAlignment((MSAlignment)GUIEnum.alignFormat(sym_));
}

static A g_fieldColumnAlignment(MSWidgetView *o_)
{
  A result=aplus_nl;
  AplusTableColumn *tc = (AplusTableColumn *)o_;
  if (tc) result = GUIEnum.alignFormat((unsigned long)tc->columnAlignment());
  return result;
}

static void s_fieldCycleFunc(MSWidgetView *o_, A fc_)
{
  AplusTableColumn *tc = (AplusTableColumn *)o_;
  
  V v = (tc->model()!=0)?((AplusModel*)tc->model())->aplusVar():0;
  AVariableData *varData = pAVarDataFromV(v);
 
  if (varData)
   {
     if (fc_->t==Et&&fc_->n==2) 
      {
	AClientData *ac=new AClientData((A)fc_->p[0],(A)fc_->p[1]);
	varData->cycleFunc((AFunc)cdipv,ac);     
      }
     else if (isNull(fc_)==MSTrue) varData->cycleFunc((AFunc)0,0);     
     else showError("Invalid 'array' Cycle Function Specification");
   }
}

static A g_fieldCycleFunc(MSWidgetView *o_)
{
  AplusTableColumn *tc = (AplusTableColumn *)o_;
  A out = aplus_nl;
  V v = (tc->model()!=0)?((AplusModel*)tc->model())->aplusVar():0;
  AVariableData *varData = pAVarDataFromV(v);

  if (varData)
   {
     out = (A)getVarFunc(varData->cycleFunc()->arg());
   }
  
  return out;
}

static void s_fieldCycleColors(MSWidgetView *o_,A a_)
{
  AplusTableColumn *tc = (AplusTableColumn *)o_;
  if (QA(a_))
   {
     MSUnsignedLongVector uv;
     for (unsigned i = 0; i < a_->n; i++)
       uv << (unsigned long) a_->p[i];
     tc->cycleColors(uv);
   }
}
static A g_fieldCycleColors(MSWidgetView *o_)
{
  AplusTableColumn *tc = (AplusTableColumn *)o_;
  A cv = aplus_nl;
  MSUnsignedLongVector uv= tc->cycleColors();
  
  if (uv.length() > 0)
   {
     long d[MAXR]={ 0, 0, 0, 0, 0, 0, 0, 0, 0 };
     d[0]=uv.length();
     cv = ga(It, 1, uv.length(), d);
     for(unsigned i = 0; i < uv.length(); i++) { cv->p[i] = (unsigned long)uv(i); }
   }
  return cv;
}

static void s_fieldBreakText(MSWidgetView *o_, A sym_, A printTextVtr_)
{
  AplusTableColumn *tc = (AplusTableColumn *)o_;
  if (QA(printTextVtr_) && printTextVtr_->t==It && printTextVtr_->n>0)
   {
     tc->removeAllBreakText();
     for (unsigned i = 0; i < printTextVtr_->n; i++)
      {
	P p; p.i=printTextVtr_->p;
	AplusPrintText *pt = (AplusPrintText *) p.i[i];
	tc->addBreakText(*pt);
      }
     tc->breakText((A) ic(sym_));
   }
}
static A g_fieldBreakText(MSWidgetView *o_)
{
  AplusTableColumn *tc = (AplusTableColumn *)o_;
  return (A) ic(tc->breakText());
}
//////////////////////////////////////////////////////////////////////////////
static void s_fieldBgGrayScale(MSWidgetView *o_, A value_)
{
  AplusTableColumn *tc = (AplusTableColumn *)o_;
  if(!QS(value_)&&(value_->t==It || value_->t==Ft))
   {
     P p; p.i=value_->p; 
     tc->bgGrayScale(value_->t==Ft?p.f[0]:(double)p.i[0]);
   }
}
static A g_fieldBgGrayScale(MSWidgetView *o_)
{
  AplusTableColumn *tc = (AplusTableColumn *)o_;
  return (A) gf(tc->bgGrayScale());
}
static void s_fieldBgGrayScaleFunc(MSWidgetView *o_, A fc_)
{
  AplusTableColumn *tc = (AplusTableColumn *)o_;
  V v = (tc->model()!=0)?((AplusModel*)tc->model())->aplusVar():0;
  if (fc_->t==Et&&fc_->n==2) 
      {
	AClientData *ac=new AClientData((A)fc_->p[0],(A)fc_->p[1]);
	tc->bgGrayScaleFunc((AFunc)cdipv,ac);
      }
  else if (isNull(fc_)==MSTrue) tc->bgGrayScaleFunc((AFunc)0,0);
  else showError("Invalid 'field' BgGrayScale Function Specification");
}
static A g_fieldBgGrayScaleFunc(MSWidgetView *o_)
{
  A out = aplus_nl;
  AplusTableColumn *tc = (AplusTableColumn *)o_;
  V v = (tc->model()!=0)?((AplusModel*)tc->model())->aplusVar():0;
  out = (A)getVarFunc(tc->bgGrayScaleFunc()->arg());
  return out;
}
//////////////////////////////////////////////////////////////////////////////
static void s_fieldBreakBgGrayScale(MSWidgetView *o_, A value_)
{
  AplusTableColumn *tc = (AplusTableColumn *)o_;
  if(!QS(value_)&&(value_->t==It || value_->t==Ft))
   {
     P p; p.i=value_->p; 
     tc->breakBgGrayScale(value_->t==Ft?p.f[0]:(double)p.i[0]);
   }
}
static A g_fieldBreakBgGrayScale(MSWidgetView *o_)
{
  AplusTableColumn *tc = (AplusTableColumn *)o_;
  return (A) gf(tc->breakBgGrayScale());
}
static void s_fieldBreakBgGrayScaleFunc(MSWidgetView *o_, A fc_)
{
  AplusTableColumn *tc = (AplusTableColumn *)o_;
  V v = (tc->model()!=0)?((AplusModel*)tc->model())->aplusVar():0;
  if (fc_->t==Et&&fc_->n==2) 
      {
	AClientData *ac=new AClientData((A)fc_->p[0],(A)fc_->p[1]);
	tc->breakBgGrayScaleFunc((AFunc)cdipv,ac);
      }
  else if (isNull(fc_)==MSTrue) tc->bgGrayScaleFunc((AFunc)0,0);
  else showError("Invalid 'field' BreakBgGrayScale Function Specification");
}
static A g_fieldBreakBgGrayScaleFunc(MSWidgetView *o_)
{
  A out = aplus_nl;
  AplusTableColumn *tc = (AplusTableColumn *)o_;
  V v = (tc->model()!=0)?((AplusModel*)tc->model())->aplusVar():0;
  out = (A)getVarFunc(tc->breakBgGrayScaleFunc()->arg());
  return out;
}
//////////////////////////////////////////////////////////////////////////////
static void s_fieldBreakFont(MSWidgetView *o_, A a_)
{
  AplusTableColumn *tc = (AplusTableColumn *)o_;
  tc->breakFont((const char *)AplusConvert::asMSString(a_));
}
static A g_fieldBreakFont(MSWidgetView *o_)
{
  AplusTableColumn *tc = (AplusTableColumn *)o_;
  return AplusConvert::asA(MSString(tc->breakFont()));
}
static void s_fieldBreakFontFunc(MSWidgetView *o_, A fc_)
{
  AplusTableColumn *tc = (AplusTableColumn *)o_;
  V v = (tc->model()!=0)?((AplusModel*)tc->model())->aplusVar():0;
  if (fc_->t==Et&&fc_->n==2) 
   {
     AClientData *ac=new AClientData((A)fc_->p[0],(A)fc_->p[1]);
     tc->breakFontFunc((AFunc)cdipv,ac);
   }
  else if (isNull(fc_)==MSTrue) tc->breakFontFunc((AFunc)0,0);
  else showError("Invalid 'field' BreakFont Function Specification");
}
static A g_fieldBreakFontFunc(MSWidgetView *o_)
{
  A out = aplus_nl;
  AplusTableColumn *tc = (AplusTableColumn *)o_;
  V v = (tc->model()!=0)?((AplusModel*)tc->model())->aplusVar():0;
  out = (A)getVarFunc(tc->breakFontFunc()->arg());
  return out;
}
//////////////////////////////////////////////////////////////////////////////
static void s_fieldBreakLeading(MSWidgetView *o_, int x_)
{
  AplusTableColumn *tc = (AplusTableColumn *)o_;
  tc->breakLeading(x_);
}
static I g_fieldBreakLeading(MSWidgetView *o_)
{
  AplusTableColumn *tc = (AplusTableColumn *)o_;
  return (I) tc->breakLeading();
}
static void s_fieldBreakLeadingFunc(MSWidgetView *o_, A fc_)
{
  AplusTableColumn *tc = (AplusTableColumn *)o_;
  V v = (tc->model()!=0)?((AplusModel*)tc->model())->aplusVar():0;
  if (fc_->t==Et&&fc_->n==2) 
   {
     AClientData *ac=new AClientData((A)fc_->p[0],(A)fc_->p[1]);
     tc->breakLeadingFunc((AFunc)cdipv,ac);
   }
  else if (isNull(fc_)==MSTrue) tc->breakLeadingFunc((AFunc)0,0);
  else showError("Invalid 'field' BreakLeading Function Specification");
}
static A g_fieldBreakLeadingFunc(MSWidgetView *o_)
{
  A out = aplus_nl;
  AplusTableColumn *tc = (AplusTableColumn *)o_;
  V v = (tc->model()!=0)?((AplusModel*)tc->model())->aplusVar():0;
  out = (A)getVarFunc(tc->breakLeadingFunc()->arg());
  return out;
}
//////////////////////////////////////////////////////////////////////////////
static void s_fieldBreakOffset(MSWidgetView *o_, int x_)
{
  AplusTableColumn *tc = (AplusTableColumn *)o_;
  tc->breakOffset(x_);
}
static I g_fieldBreakOffset(MSWidgetView *o_)
{
  AplusTableColumn *tc = (AplusTableColumn *)o_;
  return (I) tc->breakOffset();
}
static void s_fieldBreakOffsetFunc(MSWidgetView *o_, A fc_)
{
  AplusTableColumn *tc = (AplusTableColumn *)o_;
  V v = (tc->model()!=0)?((AplusModel*)tc->model())->aplusVar():0;
  if (fc_->t==Et&&fc_->n==2) 
   {
     AClientData *ac=new AClientData((A)fc_->p[0],(A)fc_->p[1]);
     tc->breakOffsetFunc((AFunc)cdipv,ac);
   }
  else if (isNull(fc_)==MSTrue) tc->breakOffsetFunc((AFunc)0,0);
  else showError("Invalid 'field' BreakOffset Function Specification");
}
static A g_fieldBreakOffsetFunc(MSWidgetView *o_)
{
  A out = aplus_nl;
  AplusTableColumn *tc = (AplusTableColumn *)o_;
  V v = (tc->model()!=0)?((AplusModel*)tc->model())->aplusVar():0;
  out = (A)getVarFunc(tc->breakOffsetFunc()->arg());
  return out;
}
//////////////////////////////////////////////////////////////////////////////
static void s_fieldBreakOn(MSWidgetView *o_, MSBoolean a_)
{
  AplusTableColumn *tc = (AplusTableColumn *)o_;
  tc->breakOn(a_);
}
static I g_fieldBreakOn(MSWidgetView *o_)
{
  AplusTableColumn *tc = (AplusTableColumn *)o_;
  return MSTrue==tc->breakOn() ? 1 : 0;
}

static void s_fieldBreakProcessOn(MSWidgetView *o_, MSBoolean a_)
{
  AplusTableColumn *tc = (AplusTableColumn *)o_;
  tc->breakProcessOn(a_);
}
static I g_fieldBreakProcessOn(MSWidgetView *o_)
{
  AplusTableColumn *tc = (AplusTableColumn *)o_;
  return MSTrue==tc->breakProcessOn() ? 1 : 0;
}
//////////////////////////////////////////////////////////////////////////////
static void s_fieldBreakStyle(MSWidgetView *o_, A a_)
{
  AplusTableColumn *tc = (AplusTableColumn *)o_;
  tc->breakStyle(GUIEnum.formatStyle(a_));
}
static A g_fieldBreakStyle(MSWidgetView *o_)
{
  AplusTableColumn *tc = (AplusTableColumn *)o_;
  return GUIEnum.formatStyle(tc->breakStyle());
}
static void s_fieldBreakStyleFunc(MSWidgetView *o_, A fc_)
{
  AplusTableColumn *tc = (AplusTableColumn *)o_;
  V v = (tc->model()!=0)?((AplusModel*)tc->model())->aplusVar():0;
  if (fc_->t==Et&&fc_->n==2) 
   {
     AClientData *ac=new AClientData((A)fc_->p[0],(A)fc_->p[1]);
     tc->breakStyleFunc((AFunc)cdipv,ac);
   }
  else if (isNull(fc_)==MSTrue) tc->breakStyleFunc((AFunc)0,0);
  else showError("Invalid 'field' BreakStyle Function Specification");
}
static A g_fieldBreakStyleFunc(MSWidgetView *o_)
{
  A out = aplus_nl;
  AplusTableColumn *tc = (AplusTableColumn *)o_;
  V v = (tc->model()!=0)?((AplusModel*)tc->model())->aplusVar():0;
  out = (A)getVarFunc(tc->breakStyleFunc()->arg());
  return out;
}
//////////////////////////////////////////////////////////////////////////////
static void s_fieldFgGrayScale(MSWidgetView *o_, A value_)
{
  AplusTableColumn *tc = (AplusTableColumn *)o_;
  if(!QS(value_)&&(value_->t==It || value_->t==Ft))
   {
     P p; p.i=value_->p; 
     tc->fgGrayScale(value_->t==Ft?p.f[0]:(double)p.i[0]);
   }
}
static A g_fieldFgGrayScale(MSWidgetView *o_)
{
  AplusTableColumn *tc = (AplusTableColumn *)o_;
  return (A) gf(tc->fgGrayScale());
}
static void s_fieldFgGrayScaleFunc(MSWidgetView *o_, A fc_)
{
  AplusTableColumn *tc = (AplusTableColumn *)o_;
  V v = (tc->model()!=0)?((AplusModel*)tc->model())->aplusVar():0;
  if (fc_->t==Et&&fc_->n==2) 
   {
     AClientData *ac=new AClientData((A)fc_->p[0],(A)fc_->p[1]);
     tc->fgGrayScaleFunc((AFunc)cdipv,ac);
   }
  else if (isNull(fc_)==MSTrue) tc->fgGrayScaleFunc((AFunc)0,0);
  else showError("Invalid 'field' FgGrayScale Function Specification");
}
static A g_fieldFgGrayScaleFunc(MSWidgetView *o_)
{
  A out = aplus_nl;
  AplusTableColumn *tc = (AplusTableColumn *)o_;
  V v = (tc->model()!=0)?((AplusModel*)tc->model())->aplusVar():0;
  out = (A)getVarFunc(tc->fgGrayScaleFunc()->arg());
  return out;
}
//////////////////////////////////////////////////////////////////////////////
static void s_fieldBreakFgGrayScale(MSWidgetView *o_, A value_)
{
  AplusTableColumn *tc = (AplusTableColumn *)o_;
  if(!QS(value_)&&(value_->t==It || value_->t==Ft))
   {
     P p; p.i=value_->p; 
     tc->breakFgGrayScale(value_->t==Ft?p.f[0]:(double)p.i[0]);
   }
}
static A g_fieldBreakFgGrayScale(MSWidgetView *o_)
{
  AplusTableColumn *tc = (AplusTableColumn *)o_;
  return (A) gf(tc->breakFgGrayScale());
}
static void s_fieldBreakFgGrayScaleFunc(MSWidgetView *o_, A fc_)
{
  AplusTableColumn *tc = (AplusTableColumn *)o_;
  V v = (tc->model()!=0)?((AplusModel*)tc->model())->aplusVar():0;
  if (fc_->t==Et&&fc_->n==2) 
   {
     AClientData *ac=new AClientData((A)fc_->p[0],(A)fc_->p[1]);
     tc->fgGrayScaleFunc((AFunc)cdipv,ac);
   }
  else if (isNull(fc_)==MSTrue) tc->breakFgGrayScaleFunc((AFunc)0,0);
  else showError("Invalid 'field' BreakFgGrayScale Function Specification");
}
static A g_fieldBreakFgGrayScaleFunc(MSWidgetView *o_)
{
  A out = aplus_nl;
  AplusTableColumn *tc = (AplusTableColumn *)o_;
  V v = (tc->model()!=0)?((AplusModel*)tc->model())->aplusVar():0;
  out = (A)getVarFunc(tc->breakFgGrayScaleFunc()->arg());
  return out;
}
//////////////////////////////////////////////////////////////////////////////
static void s_fieldBreakProcessFunc(MSWidgetView *o_, A fc_)
{
  AplusTableColumn *tc = (AplusTableColumn *)o_;
  V v = (tc->model()!=0)?((AplusModel*)tc->model())->aplusVar():0;
  if (fc_->t==Et&&fc_->n==2) 
   {
     AClientData *ac=new AClientData((A)fc_->p[0],(A)fc_->p[1]);
     tc->breakProcessFunc((AFunc)cdipv,ac);
   }
  else if (isNull(fc_)==MSTrue) tc->breakProcessFunc((AFunc)0,0);
  else showError("Invalid 'field' BreakProcess Function Specification");
}
static A g_fieldBreakProcessFunc(MSWidgetView *o_)
{
  A out = aplus_nl;
  AplusTableColumn *tc = (AplusTableColumn *)o_;
  V v = (tc->model()!=0)?((AplusModel*)tc->model())->aplusVar():0;
  out = (A)getVarFunc(tc->breakProcessFunc()->arg());
  return out;
}
static void s_fieldFormatBreakFunc(MSWidgetView *o_, A fc_)
{
  AplusTableColumn *tc = (AplusTableColumn *)o_;
  V v = (tc->model()!=0)?((AplusModel*)tc->model())->aplusVar():0;
  if (fc_->t==Et&&fc_->n==2) 
   {
     AClientData *ac=new AClientData((A)fc_->p[0],(A)fc_->p[1]);
     tc->formatBreakFunc((AFunc)cdipv,ac);
   }
  else if (isNull(fc_)==MSTrue) tc->formatBreakFunc((AFunc)0,0);
  else showError("Invalid 'field' FormatBreak Function Specification");
}
static A g_fieldFormatBreakFunc(MSWidgetView *o_)
{
  A out = aplus_nl;
  AplusTableColumn *tc = (AplusTableColumn *)o_;
  V v = (tc->model()!=0)?((AplusModel*)tc->model())->aplusVar():0;
  out = (A)getVarFunc(tc->formatBreakFunc()->arg());
  return out;
}
static void s_fieldBreakCriteriaFunc(MSWidgetView *o_, A fc_)
{
  AplusTableColumn *tc = (AplusTableColumn *)o_;
  V v = (tc->model()!=0)?((AplusModel*)tc->model())->aplusVar():0;
  if (fc_->t==Et&&fc_->n==2) 
   {
     AClientData *ac=new AClientData((A)fc_->p[0],(A)fc_->p[1]);
     tc->breakCriteriaFunc((AFunc)cdipv,ac);
   }
  else if (isNull(fc_)==MSTrue) tc->breakCriteriaFunc((AFunc)0,0);
  else showError("Invalid 'field' FormatBreak Function Specification");
}
static A g_fieldBreakCriteriaFunc(MSWidgetView *o_)
{
  A out = aplus_nl;
  AplusTableColumn *tc = (AplusTableColumn *)o_;
  V v = (tc->model()!=0)?((AplusModel*)tc->model())->aplusVar():0;
  out = (A)getVarFunc(tc->breakCriteriaFunc()->arg());
  return out;
}
//////////////////////////////////////////////////////////////////////////////
static void s_fieldHeadingStyle(MSWidgetView *o_, A a_)
{
  AplusTableColumn *tc = (AplusTableColumn *)o_;
  tc->headingStyle(GUIEnum.formatStyle(a_));
}
static A g_fieldHeadingStyle(MSWidgetView *o_)
{
  AplusTableColumn *tc = (AplusTableColumn *)o_;
  return GUIEnum.formatStyle(tc->headingStyle());
}
static void s_fieldHeadingFgGrayScale(MSWidgetView *o_, A value_)
{
  AplusTableColumn *tc = (AplusTableColumn *)o_;
  if(!QS(value_)&&(value_->t==It || value_->t==Ft))
   {
     P p; p.i=value_->p; 
     tc->headingFgGrayScale(value_->t==Ft?p.f[0]:(double)p.i[0]);
   }
}
static A g_fieldHeadingFgGrayScale(MSWidgetView *o_)
{
  AplusTableColumn *tc = (AplusTableColumn *)o_;
  return (A) gf(tc->headingFgGrayScale());
}
static void s_fieldHeadingBgGrayScale(MSWidgetView *o_, A value_)
{
  AplusTableColumn *tc = (AplusTableColumn *)o_;
  if(!QS(value_)&&(value_->t==It || value_->t==Ft))
   {
     P p; p.i=value_->p; 
     tc->headingBgGrayScale(value_->t==Ft?p.f[0]:(double)p.i[0]);
   }
}
static A g_fieldHeadingBgGrayScale(MSWidgetView *o_)
{
  AplusTableColumn *tc = (AplusTableColumn *)o_;
  return (A) gf(tc->headingBgGrayScale());
}

static void s_fieldPageBreakOn(MSWidgetView *o_, MSBoolean a_)
{
  AplusTableColumn *tc = (AplusTableColumn *)o_;
  tc->pageBreakOn(a_);
}
static I g_fieldPageBreakOn(MSWidgetView *o_)
{
  AplusTableColumn *tc = (AplusTableColumn *)o_;
  return MSTrue==tc->pageBreakOn() ? 1 : 0;
}
//////////////////////////////////////////////////////////////////////////////
static void s_fieldReportFont(MSWidgetView *o_, A a_)
{
  AplusTableColumn *tc = (AplusTableColumn *)o_;
  tc->reportFont(AplusConvert::asMSString(a_));
}
static A g_fieldReportFont(MSWidgetView *o_)
{
  AplusTableColumn *tc = (AplusTableColumn *)o_;
  return AplusConvert::asA(MSString(tc->reportFont()));
}
static void s_fieldReportFontFunc(MSWidgetView *o_, A fc_)
{
  AplusTableColumn *tc = (AplusTableColumn *)o_;
  V v = (tc->model()!=0)?((AplusModel*)tc->model())->aplusVar():0;
  if (fc_->t==Et&&fc_->n==2) 
   {
     AClientData *ac=new AClientData((A)fc_->p[0],(A)fc_->p[1]);
     tc->reportFontFunc((AFunc)cdipv,ac);
   }
  else if (isNull(fc_)==MSTrue) tc->reportFontFunc((AFunc)0,0);
  else showError("Invalid 'field' ReportFont Function Specification");
}
static A g_fieldReportFontFunc(MSWidgetView *o_)
{
  A out = aplus_nl;
  AplusTableColumn *tc = (AplusTableColumn *)o_;
  V v = (tc->model()!=0)?((AplusModel*)tc->model())->aplusVar():0;
  out = (A)getVarFunc(tc->reportFontFunc()->arg());
  return out;
}

//////////////////////////////////////////////////////////////////////////////
static void s_fieldReportHeadingFont(MSWidgetView *o_, A a_)
{
  AplusTableColumn *tc = (AplusTableColumn *)o_;
  tc->reportHeadingFont(AplusConvert::asMSString(a_));
}
static A g_fieldReportHeadingFont(MSWidgetView *o_)
{
  AplusTableColumn *tc = (AplusTableColumn *)o_;
  return AplusConvert::asA(tc->reportHeadingFont());
}
//////////////////////////////////////////////////////////////////////////////
static void s_fieldStyle(MSWidgetView *o_, A a_)
{
  AplusTableColumn *tc = (AplusTableColumn *)o_;
  tc->style(GUIEnum.formatStyle(a_));
}
static A g_fieldStyle(MSWidgetView *o_)
{
  AplusTableColumn *tc = (AplusTableColumn *)o_;
  return GUIEnum.formatStyle(tc->style());
}
static void s_fieldStyleFunc(MSWidgetView *o_, A fc_)
{
  AplusTableColumn *tc = (AplusTableColumn *)o_;
  V v = (tc->model()!=0)?((AplusModel*)tc->model())->aplusVar():0;
  if (fc_->t==Et&&fc_->n==2) 
   {
     AClientData *ac=new AClientData((A)fc_->p[0],(A)fc_->p[1]);
     tc->styleFunc((AFunc)cdipv,ac);
   }
  else if (isNull(fc_)==MSTrue) tc->styleFunc((AFunc)0,0);
  else showError("Invalid 'field' Style Function Specification");
}
static A g_fieldStyleFunc(MSWidgetView *o_)
{
  A out = aplus_nl;
  AplusTableColumn *tc = (AplusTableColumn *)o_;
  V v = (tc->model()!=0)?((AplusModel*)tc->model())->aplusVar():0;
  out = (A)getVarFunc(tc->styleFunc()->arg());
  return out;
}
static void s_fieldSuppressDuplicate(MSWidgetView *o_, MSBoolean suppress_)
{
  AplusTableColumn *tc = (AplusTableColumn *)o_;
  tc->suppressDuplicate(suppress_);
}
static I g_fieldSuppressDuplicate(MSWidgetView *o_)
{
  AplusTableColumn *tc = (AplusTableColumn *)o_;
  return MSTrue==tc->suppressDuplicate()? 1 : 0;
}
static void s_fieldComputationMode(MSWidgetView *o_, A cm_)
{
  AplusTableColumn *tc = (AplusTableColumn *)o_;
  tc->compMode(GUIEnum.formatCompMode(cm_));
}
static A g_fieldComputationMode(MSWidgetView *o_)
{
  AplusTableColumn *tc = (AplusTableColumn *)o_;
  return GUIEnum.formatCompMode(tc->compMode());
}

static void s_fieldHeadingFg(MSTableColumn *pColumn_, unsigned long value_) { pColumn_->headingForeground(value_); }
static unsigned long g_fieldHeadingFg(MSTableColumn *pColumn_) { return pColumn_->headingForeground(); } 

static void s_fieldHeading(MSTableColumn *pColumn_, A value_) { pColumn_->heading(AplusConvert::asMSStringVector(value_)); }
static A g_fieldHeading(MSTableColumn *pColumn_) { return AplusConvert::asA(pColumn_->heading()); }

// Table Column Reporting functions

static void s_fieldBreakString(MSTableColumn *pColumn_, A value_)
{ pColumn_->breakString(AplusConvert::asMSStringVector(value_)); }
static A g_fieldBreakString(MSTableColumn *pColumn_) { return AplusConvert::asA(pColumn_->breakString()); }

static A g_fieldBreakIndex(const MSTableColumn *pColumn_) { return AplusConvert::asA(pColumn_->breakIndex()); }

static void s_fieldBreakFg(MSTableColumn *pColumn_, A value_)
{
  if (value_->t==Et && value_->n==1 && QS(*value_->p))
    {
      pColumn_->breakFg(((S)XS(*value_->p))->n);
    }
  else if (value_->t==Ct)
    {
      pColumn_->breakFg((char *)value_->p);
    }
}

static A g_fieldBreakFg(MSTableColumn *pColumn_) { return AplusConvert::asA(pColumn_->breakFgString()); }

static void s_fieldBreakBg(MSTableColumn *pColumn_, A value_)
{
  if (value_->t==Et && value_->n==1 && QS(*value_->p))
    {
      pColumn_->breakBg(((S)XS(*value_->p))->n);
    }
  else if (value_->t==Ct)
    {
      pColumn_->breakBg((char *)value_->p);
    }
}

static A g_fieldBreakBg(MSTableColumn *pColumn_) { return AplusConvert::asA(pColumn_->breakBgString()); }

//////////////////////////////////////////////////////////////////////////////

//  AXHMenu 'A' interface functions
static void s_hmenuMnemonics(AplusMenu *o_, A mnemonics_) { o_->mnemonics(mnemonics_); }
static A g_hmenuMnemonics(AplusMenu *o_) { return o_->mnemonics(); }


//  AXChoice 'A' interface functions   /* MSOptionMenu */
static void s_choiceNumCols(AplusChoice *o_,int n_) { o_->columns(n_); }
static I g_choiceNumCols(AplusChoice *o_) { return (I) o_->columns(); }


//  AXCross 'A' interface functions  /* AplusMatrix */
static void s_matrixRowIndex(AplusMatrix *o_,A a_) { o_->rowIndex(a_); }
static A g_matrixRowIndex(AplusMatrix *o_) { return o_->rowIndexVector(); }
static void s_matrixColIndex(AplusMatrix *o_,A a_) { o_->colIndex(a_); }
static A g_matrixColIndex(AplusMatrix *o_) { return o_->colIndexVector(); }
static void s_matrixCornerIndex(AplusMatrix *o_,MSBoolean b_){o_->cornerIndex(b_);}
static I g_matrixCornerIndex(AplusMatrix *o_) 
{ return MSTrue==o_->cornerIndex() ? 1 : 0; }
static void s_matrixRowIndexBg(AplusMatrix *o_,unsigned long bg_) { o_->rowIndexBg(bg_); }
static unsigned long g_matrixRowIndexBg(AplusMatrix *o_) {return o_->rowIndexBg();}
static void s_matrixColIndexBg(AplusMatrix *o_,unsigned long bg_) { o_->colIndexBg(bg_); }
static unsigned long g_matrixColIndexBg(AplusMatrix *o_) {return o_->colIndexBg();}
static void s_matrixCornerIndexBg(AplusMatrix *o_,unsigned long bg_) { o_->cornerIndexBg(bg_); }
static unsigned long g_matrixCornerIndexBg(AplusMatrix *o_) { return o_->cornerIndexBg(); }

static I g_matrixNumHeadingRows(AplusMatrix *mp_) { return (I) mp_->numHeadings(); }
static void s_matrixNumHeadingRows(AplusMatrix *mp_,int num_) { mp_->numHeadings(num_); }
static A g_matrixColSpace(AplusMatrix *mp_) { return mp_->colSpace(); }
static void s_matrixColSpace(AplusMatrix *mp_,A a_) { mp_->colSpace(a_); }
static void s_matrixColSpaceFunc(AplusMatrix *o_,A fc_)
{ if (fc_->t==Et&&fc_->n==2) 
   { I f=fc_->p[0];
     I c=fc_->p[1];
     AClientData *ac=new AClientData((A)f,(A)c);
     o_->spaceFunc((AFunc)cdipv,ac);     
   }
  else if (isNull(fc_)==MSTrue) o_->spaceFunc((AFunc)0,0);     
  else showError("Invalid 'colSpace' Function Specification");
}
static A g_matrixColSpaceFunc(AplusMatrix *o_) {  return (A)getVarFunc((AClientData *)o_->spaceFunc()->arg()); }



//  AXSObject 'A' interface functions    /* MSRowColumnView */
static I g_numVisibleRows(MSRowColumnView *mp_) { return (I) mp_->rows(); }
static void s_numVisibleRows(MSRowColumnView *mp_,int num_) {mp_->rows(num_); }
static I g_numVisibleCols(MSRowColumnView *mp_) { return (I) mp_->columns(); }
static void s_numVisibleCols(MSRowColumnView *mp_,int num_) {mp_->columns(num_); }

static void s_arrayCycleFunc(MSRowColumnView *o_, A fc_)
{
  V v = (o_->model()!=0)?((AplusModel*)o_->model())->aplusVar():0;
  AVariableData *varData = pAVarDataFromV(v);
 
  if (varData)
   {
     if (fc_->t==Et&&fc_->n==2) 
      {
	AClientData *ac=new AClientData((A)fc_->p[0],(A)fc_->p[1]);
	varData->cycleFunc((AFunc)cdipv,ac);     
      }
     else if (isNull(fc_)==MSTrue) varData->cycleFunc((AFunc)0,0);     
     else showError("Invalid 'array' Cycle Function Specification");
   }
}

static A g_arrayCycleFunc(MSRowColumnView *o_)
{
  A out = aplus_nl;
  V v = (o_->model()!=0)?((AplusModel*)o_->model())->aplusVar():0;
  AVariableData *varData = pAVarDataFromV(v);

  if (varData)
   {
     out = (A)getVarFunc(varData->cycleFunc()->arg());
   }
  
  return out;
}

static void s_arrayCycleColors(MSRowColumnView *o_,A a_)
{
  if (QA(a_))
   {
     MSUnsignedLongVector uv;
     for (unsigned i = 0; i < a_->n; i++)
       uv << (unsigned long) a_->p[i];
     o_->cycleColors(uv);
   }
}
static A g_arrayCycleColors(MSRowColumnView *o_)
{
  A cv = aplus_nl;
  MSUnsignedLongVector uv= o_->cycleColors();
  if (uv.length() > 0)
   {
     long d[MAXR]={ 0, 0, 0, 0, 0, 0, 0, 0, 0 };
     d[0]=uv.length();
     cv = ga(It, 1, uv.length(), d);
     for(unsigned i = 0; i < uv.length(); i++) { cv->p[i] = (unsigned long)uv(i); }
   }
  return cv;
}

static void s_arrayCycleInterval(MSRowColumnView *o_,unsigned long i_) 
{ o_->cycleInterval(i_); }
static unsigned long g_arrayCycleInterval(MSRowColumnView *o_)
{ return o_->cycleInterval(); }
static void s_arrayCycleColorMode(MSRowColumnView *o_, A cm_)
{
  MSCycleColorMode mode=GUIEnum.cycleColorMode(cm_);
  if (-1!=(int)mode) o_->cycleColorMode(mode);
}
static A g_arrayCycleColorMode(MSRowColumnView *o_)
{
  return GUIEnum.cycleColorMode(o_->cycleColorMode());
}


static void s_arrayVsbBg(MSRowColumnView *o_,unsigned long pixel_) 
{ o_->vsbBackground(pixel_); } 
static void s_arrayHsbBg(MSRowColumnView *o_,unsigned long pixel_) 
{ o_->hsbBackground(pixel_); } 
static unsigned long g_arrayVsbBg(MSRowColumnView *o_) {return o_->vsbBackground();}
static unsigned long g_arrayHsbBg(MSRowColumnView *o_) {return o_->hsbBackground();}

static void s_arrayRowBg(MSRowColumnView *o_,unsigned long p_)
{ o_->selectedRowBackground(p_); } 

static void s_arraySelectVectorBg(MSRowColumnView *o_,unsigned long p_)   // Supported by Selection Vector
{ o_->selectedRowBackground(p_); } 

static unsigned long g_arrayRowBg(MSRowColumnView *o_){return o_->selectedRowBackground();}

static unsigned long g_arraySelectVectorBg(MSRowColumnView *o_) 
{ return o_->selectedRowBackground();} 

static void s_arrayEditorBg(MSRowColumnView *o_,unsigned long p_) 
{ o_->editorBackground(p_); } 
static void s_arrayEditorFg(MSRowColumnView *o_,unsigned long p_) 
{ o_->editorForeground(p_); } 
static unsigned long g_arrayEditorBg(MSRowColumnView *o_) 
{ return o_->editorBackground(); } 
static unsigned long g_arrayEditorFg(MSRowColumnView *o_) 
{ return o_->editorForeground(); } 
static void s_arraySelectRow(MSRowColumnView *o_,int row_) 
{
  if(MSFalse==o_->editing())o_->selectedRow(row_);
  else showError("Invalid row selection: array in edit mode");
}
static void s_arrayFirstRow(MSRowColumnView *o_,int row_) { o_->firstRow(row_); }
static void s_arrayFirstCol(MSRowColumnView *o_,int col_) { o_->firstColumn(col_); }
static void s_arraySbSize(MSRowColumnView *o_,int size_) { o_->vsbSize(size_); o_->hsbSize(size_); }
static void s_arrayHsbSize(MSRowColumnView *o_,int size_) { o_->hsbSize(size_); }
static void s_arrayVsbSize(MSRowColumnView *o_,int size_) { o_->vsbSize(size_); }
static I g_arraySelectRow(MSRowColumnView *o_) { return (I) o_->selectedRow(); }
static I g_arrayFirstRow(MSRowColumnView *o_) { return (I) o_->firstRow(); }
static I g_arrayFirstCol(MSRowColumnView *o_) { return (I) o_->firstColumn(); }
static I g_arraySbSize(MSRowColumnView *o_) { return (I) o_->hsbSize(); }
static I g_arrayHsbSize(MSRowColumnView *o_) { return (I) o_->hsbSize(); }
static I g_arrayVsbSize(MSRowColumnView *o_) { return (I) o_->vsbSize(); }

static void s_arraySelectionMode(MSRowColumnView *o_,A sym_)
{
  if (!QS(sym_)&&(sym_->t==Et&&sym_->n>0&&QS(*sym_->p)))
  {
    if (strcmp((char *) XS(sym_->p[0])->n, "single") == 0)
      o_->selectionMode(MSSingle);
    else if (strcmp((char *) XS(sym_->p[0])->n, "multiple") == 0)
      o_->selectionMode(MSMultiple);
  }
}

static A g_arraySelectionMode(MSRowColumnView *o_)
{
  MSSelectionMode mod = o_->selectionMode();
  A a=gs(Et);
  if (mod == MSMultiple)
    *a->p=MS(si("multiple"));
  else
    *a->p=MS(si("single"));

  return a;
}

static I g_arrayEdit(MSRowColumnView *o_) 
{return MSTrue==o_->editing() ? 1 : 0;}

static void s_arrayEdit(MSRowColumnView *o_,MSBoolean b_) 
{
  if (b_==MSTrue) o_->edit();
  else
   {
     o_->editorActivate();
   }
  return;
}
static void s_arraySelectVector(MSRowColumnView *o_,A a_)
{
  MSIndexVector iv;
  unsigned *d = (a_&&(a_->t == It))?(unsigned*)a_->p:0;
  int n = ((int) a_->n > 0)? (int) a_->n: 0;
  
  if (d) for(;n>0;n--,d++) iv<<(unsigned)*d;
  o_->selectionVector(iv);
}
static A g_arraySelectVector(MSRowColumnView *o_)   // Convert IndexVector to A
{
  A sv = aplus_nl;
  MSIndexVector uv= o_->selectionVector();

  long d[MAXR]={ 0, 0, 0, 0, 0, 0, 0, 0, 0 };
  d[0] = uv.length();
  sv = ga(It, 1, uv.length(), d);
  for(unsigned i = 0; i < uv.length(); i++) { sv->p[i] = (unsigned long)uv(i); }
  return sv;
}


// /* MSArrayView */

static void s_arraySelectBg(MSArrayView *o_,unsigned long p_)
{o_->selectedCellBackground(p_);}

static unsigned long g_arraySelectBg(MSArrayView *o_)
{return o_->selectedCellBackground(); } 

static void s_arraySelectCol(MSArrayView *o_,int col_) 
{
  if(MSFalse==o_->editing())o_->selectedColumn(col_);
  else showError("Invalid column selection: array in edit mode");
}
static I g_arraySelectCol(MSArrayView *o_) { return (I) o_->selectedColumn(); }

static void s_arraySeparatorInterval(MSArrayView *o_,int inc_) {o_->rowSeparator(inc_); }
static I g_arraySeparatorInterval(MSArrayView *o_) {return (I) o_->rowSeparator();}

static void s_arrayColSeparator(MSArrayView *o_,int inc_) {o_->columnSeparator(inc_);}
static I g_arrayColSeparator(MSArrayView *o_) {return (I) o_->columnSeparator();}


// AXTable fns    /* MSTable */
static void s_tableNumFixedFields(MSArrayView *o_,int n_) { o_->fixedColumns(n_); }
static I g_tableNumFixedFields(MSArrayView *o_) {return (I) o_->fixedColumns(); }
static void s_tableColumnDragDrop(MSArrayView *o_, MSBoolean b_) { ((MSTable *)o_)->columnDragDrop(b_); }
static I g_tableColumnDragDrop(MSArrayView *o_) 
{ return MSTrue==((MSTable *)o_)->columnDragDrop() ? 1 : 0; }
static void s_tableColumnResize(MSArrayView *o_, MSBoolean b_) { ((MSTable *)o_)->columnResize(b_); }
static I g_tableColumnResize(MSArrayView *o_) 
{ return MSTrue==((MSTable *)o_)->columnResize() ? 1 : 0; }

static void s_tableBreakFont(MSArrayView *o_, A x_) { ((MSTable *) o_)->breakFont(AplusConvert::asMSString(x_)); }
static A g_tableBreakFont(MSArrayView *o_) { return AplusConvert::asA(((MSTable *) o_)->breakFont()); }
static void s_tableBreakStyle(MSArrayView *o_, A x_) { ((MSTable *) o_)->breakStyle(GUIEnum.formatStyle(x_)); }
static A g_tableBreakStyle(MSArrayView *o_) { return GUIEnum.formatStyle(((MSTable *) o_)->breakStyle()); }

static void s_tableFixedReportColumns(MSArrayView *o_, int x_) { ((MSTable *) o_)->MSReportTable::fixedReportColumns(x_); }
static I g_tableFixedReportColumns(MSArrayView *o_) { return (I) ((MSTable *) o_)->fixedReportColumns(); }

static void s_tableGroupHeading(MSArrayView *o_, A x_) 
{ 
  if(o_ && x_ && QA(x_))
    if( isNull(x_)==MSTrue )
      {
	// add a dummy entry so can do a removeAllGroupHeadngs
	((MSTable *) o_)->MSReportTable::addGroupHeading("dummy");
	((MSTable *) o_)->MSReportTable::removeAllGroupHeadings();
      }
    else
      if(x_->t==Et && QS(x_->p[0])) // single (`groupheading;`a)
	{

	  // add a dummy entry so can do a removeAllGroupHeadngs
	  ((MSTable *) o_)->MSReportTable::addGroupHeading("dummy");
	  ((MSTable *) o_)->MSReportTable::removeAllGroupHeadings();

	  AplusParagraph *ap = 
	    ((AplusParagraph *)getVarData(x_)->pWidgetView());

	  if(ap && ap->widgetType()==AplusParagraph::symbol())
	    {
	      ((MSTable *) o_)->MSReportTable::addGroupHeading(*ap).
		text(AplusConvert::asMSStringVector(ap->model()->a())).
		column(ap->column()).
		columnSpan(ap->columnSpan());
	    }
	}
      else			 // vector (`groupheading;(`a;`b;`c))
	{
	  int i;
	  // add a dummy entry so can do a removeAllGroupHeadngs
	  ((MSTable *) o_)->MSReportTable::addGroupHeading("dummy");
	  ((MSTable *) o_)->MSReportTable::removeAllGroupHeadings();

	  for( i=0; i<x_->n; i++)
	    {
	      A ax=(A)x_->p[i];
	      if( ax && QA(ax) && ax->t==Et && QS(ax->p[0]) )
		{
		  AplusParagraph *ap = 
		    ((AplusParagraph *)getVarData(ax)->pWidgetView());

		  if(ap && ap->widgetType()==AplusParagraph::symbol())
		    {
		      ((MSTable *) o_)->MSReportTable::addGroupHeading(*ap).
			text(AplusConvert::asMSStringVector(ap->model()->a())).
			column(ap->column()).
			columnSpan(ap->columnSpan());
		    }
		}
	    }
	}
}
static A g_tableGroupHeading(MSArrayView *o_) { return aplus_nl; }

static I g_tableGrandTotal(MSArrayView *o_) { return (I) ((MSTable *) o_)->grandTotal(); }
static I g_tableCurrentBreakColumn(MSArrayView *o_)
{
  MSTable *t=(MSTable *) o_;
  if (t->breakColumn().length()>0) return (I) t->breakColumn().lastElement();
  return -1;
}
static void s_tableGrandTotalText(MSArrayView *o_, A sym_, A printText_)
{
  AplusTable *t = (AplusTable *) o_;
  if (QA(printText_) && printText_->t==It && printText_->n==1)
   {
     P p; p.i=printText_->p;
     AplusPrintText *pt = (AplusPrintText *) p.i[0];
     t->grandTotalText() =*pt;  // FIX THIS
     t->grandTotalTextA((A) ic(sym_));
     delete pt;
   }
}
static A g_tableGrandTotalText(MSArrayView *o_) { return (A) ic(((AplusTable *)o_)->grandTotalTextA()); }
static void s_tableHeadingStyle(MSArrayView *o_, A x_) { ((MSTable *) o_)->headingStyle(GUIEnum.formatStyle(x_)); }
static A g_tableHeadingStyle(MSArrayView *o_) { return GUIEnum.formatStyle(((MSTable *) o_)->headingStyle()); }
static void s_tableReportFont(MSArrayView *o_, A x_) { ((MSTable *) o_)->reportFont(AplusConvert::asMSString(x_)); }
static A g_tableReportFont(MSArrayView *o_) { return AplusConvert::asA(((MSTable *) o_)->reportFont()); }
static void s_tableReportGrandTotalOn(MSArrayView *o_, MSBoolean x_) { ((MSTable *) o_)->reportGrandTotalOn(x_); }
static I g_tableReportGrandTotalOn(MSArrayView *o_) 
{ return MSTrue==((MSTable *) o_)->reportGrandTotalOn() ? 1 : 0; }
static void s_tableReportHeadingFont(MSArrayView *o_, A x_) { ((MSTable *) o_)->reportHeadingFont(AplusConvert::asMSString(x_)); }
static A g_tableReportHeadingFont(MSArrayView *o_) { return AplusConvert::asA(((MSTable *) o_)->reportHeadingFont()); }
static void s_tableReportTotalOn(MSArrayView *o_, MSBoolean x_) { ((MSTable *) o_)->reportTotalOn(x_); }
static I g_tableReportTotalOn(MSArrayView *o_) 
{ return  MSTrue==((MSTable *) o_)->reportTotalOn() ? 1 : 0; }
static void s_tableReportTotalFont(MSArrayView *o_, A x_) { ((MSTable *) o_)->reportTotalFont(AplusConvert::asMSString(x_)); }
static A g_tableReportTotalFont(MSArrayView *o_) { return AplusConvert::asA(((MSTable *) o_)->reportTotalFont()); }
static void s_tableReportTotalLeading(MSArrayView *o_, int x_) { ((MSTable *) o_)->reportTotalLeading(x_); }
static I g_tableReportTotalLeading(MSArrayView *o_) { return (I) ((MSTable *) o_)->reportTotalLeading(); }
static void s_tableReportTotalStyle(MSArrayView *o_, A x_) { ((MSTable *) o_)->reportTotalStyle(GUIEnum.formatStyle(x_)); }
static A g_tableReportTotalStyle(MSArrayView *o_) { return GUIEnum.formatStyle(((MSTable *) o_)->reportTotalStyle()); }
static void s_tableStyle(MSArrayView *o_, A x_) { ((MSTable *) o_)->style(GUIEnum.formatStyle(x_)); }
static A g_tableStyle(MSArrayView *o_) { return GUIEnum.formatStyle(((MSTable *) o_)->style()); }
static void s_tableRowControl(MSArrayView *o_, A vals_) { ((MSTable *)o_)->rowControl(AplusConvert::asMSUnsignedVector(vals_)); }
static A g_tableRowControl(MSArrayView *o_) { return AplusConvert::asA(((MSTable *)o_)->rowControl()); }
static void s_tableRowPageSpan(MSArrayView *o_, int rowPageSpan_) { ((MSTable *)o_)->rowPageSpan(rowPageSpan_); }
static I g_tableRowPageSpan(MSArrayView *o_) { return (I) ((MSTable *)o_)->rowPageSpan(); }
static void s_tableColumnControl(MSArrayView *o_, A vals_) { ((MSTable *)o_)->columnControl(AplusConvert::asMSUnsignedVector(vals_)); }
static A g_tableColumnControl(MSArrayView *o_) { return AplusConvert::asA(((MSTable *)o_)->columnControl()); }
static void s_tableColumnPageSpan(MSArrayView *o_, unsigned columnPageSpan_) { ((MSTable *)o_)->columnPageSpan(columnPageSpan_); }
static I g_tableColumnPageSpan(MSArrayView *o_) { return (I) ((MSTable *)o_)->columnPageSpan(); }
static void s_tableNewspaperColumn(MSArrayView *o_, int x_) { ((MSTable *) o_)->newspaperColumns(x_); }
static I g_tableNewspaperColumn(MSArrayView *o_) { return (I) ((MSTable *) o_)->newspaperColumns(); }
static A g_tableColumnSpacing(MSArrayView *o_) { return AplusConvert::asA(((MSTable *)o_)->reportColumnSpacing()); }
static void s_tableColumnSpacing(MSArrayView *o_, A floats_) { ((MSTable *)o_)->reportColumnSpacing(AplusConvert::asMSFloatVector(floats_)); }
static I g_tableFrameLineWidth(MSArrayView *o_) { return (I) ((MSTable *)o_)->frameLineWidth(); }
static void s_tableFrameLineWidth(MSArrayView *o_, int flw_) { ((MSTable *)o_)->frameLineWidth(flw_); }
static I g_tableFrameOffset(MSArrayView *o_) { return (I) ((MSTable *)o_)->frameOffset(); }
static void s_tableFrameOffset(MSArrayView *o_, int fo_) { ((MSTable *)o_)->frameOffset(fo_); }
static A g_tableFrameStyle(MSArrayView *o_) { return GUIEnum.formatStyle(((MSTable *)o_)->frameStyle()); }
static void s_tableFrameStyle(MSArrayView *o_, A sym_) { ((MSTable *)o_)->frameStyle(GUIEnum.formatStyle(sym_)); }
static A g_tableLeading(MSArrayView *o_) { return AplusConvert::asA(((MSTable *)o_)->leading()); }
static void s_tableLeading(MSArrayView *o_, A vals_) { ((MSTable *)o_)->leading(AplusConvert::asMSUnsignedVector(vals_)); }
static A g_tableOutputStyle(MSArrayView *o_)
{
  A   r=aplus_nl;
  int i,j,count=0;
  char *str;
  MSTable *t=(MSTable *) o_;
  unsigned long style = t->outputStyle();
  
  for (i=2; i<=MSP::NoHeadings; i*=2)
   {
     if (GUIEnum.reportStyleEnumHashTable()->lookup((unsigned long)(style&i))!=0) count++;
   }
  if (count>0)
   {
     r=gv(Et,count);
     for (j=0,i=2; i<=MSP::NoHeadings; i*=2)
      {
	if ((str=(char*)GUIEnum.reportStyleEnumHashTable()->lookup((unsigned long)(style&i)))!=0)
	  r->p[j++]=MS(si(str));
      }
   }
  return r;
}
static void s_tableOutputStyle(MSArrayView *o_, A sym_)
{
  MSTable *t=(MSTable *)o_;
  unsigned long style=0;
  if (sym_!=0)
   {
     A *p=(A *)sym_->p;
     for (int i=0; i<(int)sym_->n; i++)
      {
	char *s = (char *) XS(sym_->p[i])->n;
	if (QS(p[i])) style|=(unsigned long)GUIEnum.reportStyleStringHashTable()->lookup(s);
	else
	 {
	   cerr << "ã ! ";
	   if (s!=0) cerr << s;
	   cerr << ": invalid symbol" << endl;
	 }
      }
   }
  t->outputStyle(style);
}


// Reporting methods

static void s_tableLeftMargin(MSWidgetView *pWidget_, A value_)
{
  if (!QS(value_))
    {
      AplusTable *pTable = (AplusTable *)pWidget_;
      if (value_->t==Ft)	// if it's a floating point value
	{
	  pTable->leftMargin(*(double*)value_->p);
	}
      else if (value_->t==It)  // it's an integer
	{
	  pTable->leftMargin((double)*(I*)value_->p);
	}
    }
}

static A g_tableLeftMargin(MSWidgetView *pWidget_)
{
  // A fix for a small MStk problem - to be fixed in MStk for release 2.6:
  // margins in inches are calculated based on the corresponding pixel offset values;
  // the default pixel offset is -1 (meaning that it's inherited from the report's pixel offset);
  // thus, the default margin is -0.0139, which is confusing to the user - it should also be -1.
  double margin = ((AplusTable *)pWidget_)->leftMargin();
  return gf((margin<0)?-1:margin);
}

static void s_tableRightMargin(MSWidgetView *pWidget_, A value_)
{
  if (!QS(value_))
    {
      AplusTable *pTable = (AplusTable *)pWidget_;
      if (value_->t==Ft)	// if it's a floating point value
	{
	  pTable->rightMargin(*(double*)value_->p);
	}
      else if (value_->t==It)  // it's an integer
	{
	  pTable->rightMargin((double)*(I*)value_->p);
	}
    }
}

static A g_tableRightMargin(MSWidgetView *pWidget_)
{
  // A fix for a small MStk problem - to be fixed in MStk for release 2.6:
  // margins in inches are calculated based on the corresponding pixel offset values;
  // the default pixel offset is -1 (meaning that it's inherited from the report's pixel offset);
  // thus, the default margin is -0.0139, which is confusing to the user - it should also be -1.
  double margin = ((AplusTable *)pWidget_)->rightMargin();
  return gf((margin<0)?-1:margin);
}

static void s_tableTopOffset(MSWidgetView *pWidget_, A value_)
{
  if (!QS(value_))
    {
      AplusTable *pTable = (AplusTable *)pWidget_;
      if (value_->t==Ft)	// if it's a floating point value
	{
	  pTable->topOffset(*(double*)value_->p);
	}
      else if (value_->t==It)  // it's an integer
	{
	  pTable->topOffset((double)*(I*)value_->p);
	}
    }
}

static A g_tableTopOffset(MSWidgetView *pWidget_)
{
  // A fix for an MStk bug to be fixed in MStk 2.6:
  // MSPrintItem::topOffset() and MSPrintItem::bottomOffset() methods do the cast to double incorrectly
  // so the decimal part gets truncated
  double offset = ((AplusTable *)pWidget_)->topPixel();
  return gf(offset/MSPointsPerInch);
}

static void s_tableBottomOffset(MSWidgetView *pWidget_, A value_)
{
  if (!QS(value_))
    {
      AplusTable *pTable = (AplusTable *)pWidget_;
      if (value_->t==Ft)	// if it's a floating point value
	{
	  pTable->bottomOffset(*(double*)value_->p);
	}
      else if (value_->t==It)  // it's an integer
	{
	  pTable->bottomOffset((double)*(I*)value_->p);
	}
    }
}

static A g_tableBottomOffset(MSWidgetView *pWidget_)
{
  // A fix for an MStk bug to be fixed in MStk 2.6:
  // MSPrintItem::topOffset() and MSPrintItem::bottomOffset() methods do the cast to double incorrectly
  // so the decimal part gets truncated
  double offset = ((AplusTable *)pWidget_)->bottomPixel();
  return gf(offset/MSPointsPerInch);
}

static void s_tablePrintRow(MSWidgetView *pWidget_, int value_) { ((AplusTable *)pWidget_)->printRow(value_); }
static I  g_tablePrintRow(MSWidgetView *pWidget_) { return (I) ((AplusTable *)pWidget_)->printRow(); }

static void s_tablePrintColumn(MSWidgetView *pWidget_, int value_) { ((AplusTable *)pWidget_)->printColumn(value_); }
static I  g_tablePrintColumn(MSWidgetView *pWidget_) { return (I) ((AplusTable *)pWidget_)->printColumn(); }

static void s_tableJustify(MSWidgetView *pWidget_, A value_)
{
  AplusAlignmentConverter converter;
  unsigned long justify = converter(value_);
  if (justify!=converter.enumNotFound())
    {
      ((AplusTable *)pWidget_)->justification(justify);
    }
}

static A g_tableJustify(MSWidgetView *pWidget_)
{
  AplusAlignmentConverter converter;
  return converter(((AplusTable *)pWidget_)->justification());
}  

static void s_tablePageAlign(MSWidgetView *pWidget_, A value_)
{
  AplusAlignmentConverter converter;
  unsigned long align = converter(value_);
  if (align!=converter.enumNotFound())
    {
      ((AplusTable *)pWidget_)->pageAlignment(align);
    }
}  

static A g_tablePageAlign(MSWidgetView *pWidget_)
{
  AplusAlignmentConverter converter;
  return converter(((AplusTable *)pWidget_)->pageAlignment());
}


static A g_tableBreakTextColumn(MSWidgetView *pWidget_)
{ return AplusConvert::asA(((AplusTable *)pWidget_)->breakTextColumn()); }
static A g_tableBreakColumn(MSWidgetView *pWidget_) { return AplusConvert::asA(((AplusTable *)pWidget_)->breakColumn()); }
static A g_tableBreakIndex(MSWidgetView *pWidget_) { return AplusConvert::asA(((AplusTable *)pWidget_)->breakIndex()); }
static A g_tablePageBreakIndex(MSWidgetView *pWidget_)
{ return AplusConvert::asA(((AplusTable *)pWidget_)->pageBreakIndex()); }

static void s_tableFgGrayScale(MSWidgetView *pWidget_, A value_)
{
  if (QS(value_)==0)
    {
      AplusTable *pTable = (AplusTable *)pWidget_;
      if (value_->t==Ft)	// if it's a floating point value
	{
	  pTable->fgGrayScale(*(double*)value_->p);
	}
      else if (value_->t==It)  // it's an integer
	{
	  pTable->fgGrayScale((double)*(I*)value_->p);
	}
    }
}
static A g_tableFgGrayScale(MSWidgetView *pWidget_) { return gf(((AplusTable *)pWidget_)->fgGrayScale()); }

static void s_tableBgGrayScale(MSWidgetView *pWidget_, A value_)
{
  if (QS(value_)==0)
    {
      AplusTable *pTable = (AplusTable *)pWidget_;
      if (value_->t==Ft)	// if it's a floating point value
	{
	  pTable->bgGrayScale(*(double*)value_->p);
	}
      else if (value_->t==It)  // it's an integer
	{
	  pTable->bgGrayScale((double)*(I*)value_->p);
	}
    }
}
static A g_tableBgGrayScale(MSWidgetView *pWidget_) { return gf(((AplusTable *)pWidget_)->bgGrayScale()); }

static void s_tableHeadingFgGrayScale(MSWidgetView *pWidget_, A value_)
{
  if (QS(value_)==0)
    {
      AplusTable *pTable = (AplusTable *)pWidget_;
      if (value_->t==Ft)	// if it's a floating point value
	{
	  pTable->headingFgGrayScale(*(double*)value_->p);
	}
      else if (value_->t==It)  // it's an integer
	{
	  pTable->headingFgGrayScale((double)*(I*)value_->p);
	}
    }
}
static A g_tableHeadingFgGrayScale(MSWidgetView *pWidget_) { return gf(((AplusTable *)pWidget_)->headingFgGrayScale()); }

static void s_tableHeadingBgGrayScale(MSWidgetView *pWidget_, A value_)
{
  if (QS(value_)==0)
    {
      AplusTable *pTable = (AplusTable *)pWidget_;
      if (value_->t==Ft)	// if it's a floating point value
	{
	  pTable->headingBgGrayScale(*(double*)value_->p);
	}
      else if (value_->t==It)  // it's an integer
	{
	  pTable->headingBgGrayScale((double)*(I*)value_->p);
	}
    }
}
static A g_tableHeadingBgGrayScale(MSWidgetView *pWidget_) { return gf(((AplusTable *)pWidget_)->headingBgGrayScale()); }

static void s_tableBreakOffset(MSWidgetView *pWidget_, int value_) { ((AplusTable *)pWidget_)->breakOffset(value_); }
static I  g_tableBreakOffset(MSWidgetView *pWidget_) { return (I) ((AplusTable *)pWidget_)->breakOffset(); }

static void s_tableBreakLeading(MSWidgetView *pWidget_, int value_) { ((AplusTable *)pWidget_)->breakLeading(value_); }
static I  g_tableBreakLeading(MSWidgetView *pWidget_) { return (I) ((AplusTable *)pWidget_)->breakLeading(); }

static void s_tableReportHeadingOffset(MSWidgetView *pWidget_, int value_)
{ ((AplusTable *)pWidget_)->reportHeadingOffset(value_); }
static I  g_tableReportHeadingOffset(MSWidgetView *pWidget_) { return (I) ((AplusTable *)pWidget_)->reportHeadingOffset(); }

static void s_tableComputeBreaks(MSWidgetView *pWidget_) { ((AplusTable *)pWidget_)->computeBreaks(); }

static void s_tableDynamicRecompute(MSWidgetView *pWidget_, MSBoolean value_)
{ ((AplusTable *)pWidget_)->dynamicRecompute(value_); }
static I g_tableDynamicRecompute(MSWidgetView *pWidget_) 
{ return MSTrue==((AplusTable *)pWidget_)->dynamicRecompute() ? 1 : 0; }

static void s_tableShowBreaks(MSWidgetView *pWidget_, MSBoolean value_) { ((AplusTable *)pWidget_)->showBreaks(value_); }
static I g_tableShowBreaks(MSWidgetView *pWidget_) 
{ return MSTrue==((AplusTable *)pWidget_)->showBreaks() ? 1 : 0; }

static void s_tableHeadingFg(MSWidgetView *pWidget_, unsigned long value_)
{ ((AplusTable *)pWidget_)->headingForeground(value_); }
static unsigned long g_tableHeadingFg(MSWidgetView *pWidget_) { return ((AplusTable *)pWidget_)->headingForeground(); }

static void s_tableDelimiter(MSWidgetView *pWidget_, A value_)
{
  AplusTable *pTable=(AplusTable *)pWidget_;

  if (isNull(value_)==MSTrue)
    {
      pTable->delimiter(" ");
    }
  else
    {
      pTable->delimiter(AplusConvert::asMSString(value_));
    }
}

static A g_tableDelimiter(MSWidgetView *pWidget_) { return AplusConvert::asA(((AplusTable *)pWidget_)->delimiter()); }

/*** MSReportTable::breakTextIndex() is protected in MStk 2.6; uncomment if it is made public in the future ***/
//
//static A g_tableBreakTextIndex(MSWidgetView *pWidget_)
//{ return AplusConvert::asA(((AplusTable *)pWidget_)->breakTextIndex()); }

// XScrollW fns    /* MSScrolledWindow */
static I g_windowSbSize(MSScrolledWindow *w_) { return (I) w_->scrollBarSize(); }
static void s_windowSbSize(MSScrolledWindow *w_,int s_) { w_->scrollBarSize(s_); }
static void s_windowVsbBg(MSScrolledWindow *w_,unsigned long p_) {w_->vsbBackground(p_); } 
static void s_windowHsbBg(MSScrolledWindow *w_,unsigned long p_) { w_->hsbBackground(p_); } 
static unsigned long g_windowVsbBg(MSScrolledWindow *) {return 0;}
static unsigned long g_windowHsbBg(MSScrolledWindow *) {return 0;}
//static unsigned long g_windowVsbBg(MSScrolledWindow *w_) {return w_->vsbBackground();}
//static unsigned long g_windowHsbBg(MSScrolledWindow *w_) {return w_->hsbBackground();}

// AXPushB fns   /* MSButton */
static void s_buttonMargin(MSButton *btn_,int m_) { btn_->margin(m_); }
static I g_buttonMargin(MSButton *btn_) { return (I) btn_->margin(); }


// AXLabelM fns  /* MSLabel */
static void s_labelMargin(AplusLabel *btn_,int m_) { btn_->margin(m_); }
static I g_labelMargin(AplusLabel *btn_) { return (I) btn_->margin(); }
   

// AXScalar fns  /* MSEntryField */
static void s_scalarEditorBg(AplusEntryField *o_,unsigned long p_) { o_->editorBackground(p_); } 
static void s_scalarEditorFg(AplusEntryField *o_,unsigned long p_) { o_->editorForeground(p_); } 
static unsigned long g_scalarEditorBg(AplusEntryField *o_) { return o_->editorBackground(); } 
static unsigned long g_scalarEditorFg(AplusEntryField *o_){ return o_->editorForeground(); } 
static void s_scalarEdit(AplusEntryField *o_,MSBoolean b_){ (b_==MSTrue)?o_->edit():(void)o_->activateEditor(); }
static I g_scalarEdit(AplusEntryField *o_) 
{ return MSTrue==o_->editing() ? 1 : 0; }
static void s_scalarArrowButtons(AplusEntryField *o_, MSBoolean b_) { o_->arrowButtons(b_); }
static I g_scalarArrowButtons(AplusEntryField *o_) 
{ return MSTrue==o_->arrowButtons() ? 1 : 0; }

static void s_scalarCycleFunc(AplusEntryField *o_,A fc_)
{
  V v = (o_->model()!=0)?((AplusModel*)o_->model())->aplusVar():0;
  AVariableData *varData = pAVarDataFromV(v);
 
  if (varData)
   {
     if (fc_->t==Et&&fc_->n==2) 
      {
	AClientData *ac=new AClientData((A)fc_->p[0],(A)fc_->p[1]);
	varData->cycleFunc((AFunc)cdipv,ac);     
      }
     else if (isNull(fc_)==MSTrue) varData->cycleFunc((AFunc)0,0);     
     else showError("Invalid 'scalar' Cycle Function Specification");
   }
}
static A g_scalarCycleFunc(AplusEntryField *o_)
{
  A out = aplus_nl;
  V v = (o_->model()!=0)?((AplusModel*)o_->model())->aplusVar():0;
  AVariableData *varData = pAVarDataFromV(v);

  if (varData)
   {
     out = (A)getVarFunc(varData->cycleFunc()->arg());
   }
  
  return out;
}

static void s_scalarCycleColors(AplusEntryField *o_,A a_)
{
  if (QA(a_))
   {
     MSUnsignedLongVector uv;
     for (unsigned i = 0; i < a_->n; i++)
       uv << (unsigned long) a_->p[i];
     o_->cycleColors(uv);
   }
}
static A g_scalarCycleColors(AplusEntryField *o_)
{
  A cv = aplus_nl;
  MSUnsignedLongVector uv= o_->cycleColors();
  if (uv.length() > 0)
   {
     long d[MAXR]={ 0, 0, 0, 0, 0, 0, 0, 0, 0 };
     d[0]=uv.length();
     cv = ga(It, 1, uv.length(), d);
     for(unsigned i = 0; i < uv.length(); i++) { cv->p[i] = (unsigned long)uv(i); }
   }
  return cv;
}

static void s_scalarCycleInterval(AplusEntryField *o_,unsigned long i_) 
{ o_->cycleInterval(i_); }
static unsigned long g_scalarCycleInterval(AplusEntryField *o_) 
{ return o_->cycleInterval(); }

static void s_scalarValueBg(AplusEntryField *ef_, unsigned long bg_) { ef_->valueBackground(bg_); }
static unsigned long g_scalarValueBg(AplusEntryField *ef_) { return ef_->valueBackground(); }
static void s_scalarValueFont(AplusEntryField *ef_, Font font_) { ef_->valueFont(font_); }
static Font g_scalarValueFont(AplusEntryField *ef_) { return ef_->valueFont(); }

static void s_scalarValueAlign(AplusEntryField *ef_, A alignment_)
{
  AplusAlignmentConverter converter;
  unsigned long align = converter(alignment_);
  if (align!=converter.enumNotFound())
    {
      ef_->valueAlignment((MSAlignment)align);
    }
}

static A g_scalarValueAlign(AplusEntryField *ef_)
{
  AplusAlignmentConverter converter;
  return converter(ef_->valueAlignment());
}

static void s_scalarValueShadowStyle(AplusEntryField *ef_, A value_)
{
  AplusShadowStyleConverter converter;
  unsigned long style = converter(value_);
  if (style!=converter.enumNotFound())
    {
      ef_->valueShadowStyle((MSShadowStyle)style);
    }
}

static A g_scalarValueShadowStyle(AplusEntryField *ef_)
{
  AplusShadowStyleConverter converter;
  return converter(ef_->valueShadowStyle());
}

static void s_scalarValueShadowThickness(AplusEntryField *ef_, int value_) { ef_->valueShadowThickness(value_); }
static I g_scalarValueShadowThickness(AplusEntryField *ef_) { return (I) ef_->valueShadowThickness(); }

// cycleColorMode() method does not yet exist for MSEntryField, but will.
// static void s_scalarCycleColorMode(AplusEntryField *o_, A cm_)
// { o_->cycleColorMode(GUIEnum.cycleColorMode(cm_)); }
// static A g_scalarCycleColorMode(AplusEntryField *o_)
// { return GUIEnum.cycleColorMode(o_->cycleColorMode()); }


// AXSlot fns
static void s_slotCycleFunc(AplusSlot *o_,A fc_)
{ if (fc_->t==Et&&fc_->n==2) 
   { AClientData *ac=new AClientData((A)fc_->p[0],(A)fc_->p[1]);
     o_->cycleFunc((AFunc)cdipv,ac);     
   }
  else if (isNull(fc_)==MSTrue) o_->cycleFunc((AFunc)0,0);     
  else showError("Invalid 'slot' Cycle Function Specification");
}
static A g_slotCycleFunc(AplusSlot *o_)
{ return (A)getVarFunc((AClientData *)o_->cycleFunc()->arg()); }
static void s_slotCycleColors(AplusSlot *o_,A a_) { o_->cycleColors(a_); }
static A g_slotCycleColors(AplusSlot *o_) { return o_->cycleColors(); }
static void s_slotCycleInterval(AplusSlot *o_,unsigned long i_) 
{ o_->cycleInterval(i_); }
static unsigned long g_slotCycleInterval(AplusSlot *o_)
{return o_->cycleInterval(); }
// cycleColorMode() method does not yet exist for MSEntryField, but will.
// static void s_slotCycleColorMode(AplusSlot *o_, A cm_)
// { o_->cycleColorMode(GUIEnum.cycleColorMode(cm_)); }
// static A g_slotCycleColorMode(AplusSlot *o_)
// { return GUIEnum.cycleColorMode(o_->cycleColorMode()); }

static void s_slotEditorBg(AplusSlot *o_,unsigned long p_)
{ o_->editorBackground(p_); } 
static void s_slotEditorFg(AplusSlot *o_,unsigned long p_)
{ o_->editorForeground(p_); } 
static unsigned long g_slotEditorBg(AplusSlot *o_){return o_->editorBackground();}
static unsigned long g_slotEditorFg(AplusSlot *o_){return o_->editorForeground();}
static void s_slotEdit(AplusSlot *o_,MSBoolean b_) { (b_==MSTrue)?o_->startEditing():o_->stopEditing(); }
static I g_slotEdit(AplusSlot *o_) 
{ return MSTrue==o_->editing() ? 1 : 0; }
static void s_slotSelected(AplusSlot *o_,int index_) { o_->selectedItem(index_); }
static I g_slotSelected(AplusSlot *o_) { return (I) o_->selectedItem(); }
static void s_slotHlColor(AplusSlot *o_, unsigned long c_) { o_->highlightColor(c_); }
static unsigned long g_slotHlColor(AplusSlot *o_) { return o_->highlightColor(); }
static void s_slotHlThickness(AplusSlot *o_, int t_) { o_->highlightThickness(t_); }
static I g_slotHlThickness(AplusSlot *o_) { return (I) o_->highlightThickness(); }
static void s_slotShadowThickness(AplusSlot *o_, int t_) { o_->shadowThickness(t_); }
static I g_slotShadowThickness(AplusSlot *o_) { return (I) o_->shadowThickness(); }
static void s_slotArrowButtons(AplusSlot *o_, A states_) { o_->arrowButtons((A)ic(states_)); }
static A g_slotArrowButtons(AplusSlot *o_) { return (A)ic(o_->arrowButtons()); }
  

// AXBox fns 
static void s_boxSelected(AplusButtonBox *o_,int index_) { o_->selectedItem(index_); }
static I g_boxSelected(AplusButtonBox *o_) { return (I) o_->selectedItem(); }


//////////////////////////////////////////////////////////////////////////////
// XTable extractor functions    /* MSLayoutManager */

static void s_layoutRowSpace(MSLayoutManager *tp_,A x_) { if (!QS(x_)&&(x_->t==It||x_->t==Ft))
  { P p; p.i=x_->p; tp_->rowSpacing((int)(x_->t==Ft?p.f[0]:p.i[0])); } }
static A g_layoutRowSpace(MSLayoutManager *tp_) { A r=gf(tp_->rowSpacing()); return r; }

static void s_layoutColSpace(MSLayoutManager *tp_,A x_) { if (!QS(x_)&&(x_->t==It||x_->t==Ft))
  { P p; p.i=x_->p; tp_->columnSpacing((int)(x_->t==Ft?p.f[0]:p.i[0])); } }
static A g_layoutColSpace(MSLayoutManager *tp_) { A r=gf(tp_->columnSpacing()); return r; }

static I g_layoutRow(MSWidgetView *pWidgetView_)
{
  MSLayoutManager *tp=(MSLayoutManager *) pWidgetView_->owner();
  return (I) tp->row((MSWidgetView *)pWidgetView_);
}
static I g_layoutCol(MSWidgetView *pWidgetView_)
{
  MSLayoutManager *tp=(MSLayoutManager *) pWidgetView_->owner();
  return (I) tp->column((MSWidgetView *)pWidgetView_);
}
static I g_layoutHspan(MSWidgetView *pWidgetView_)
{
  MSLayoutManager *tp=(MSLayoutManager *) pWidgetView_->owner();
  return (I) tp->columnSpan((MSWidgetView *)pWidgetView_);
}
static I g_layoutVspan(MSWidgetView *pWidgetView_)
{
  MSLayoutManager *tp=(MSLayoutManager *) pWidgetView_->owner();
  return (I) tp->rowSpan((MSWidgetView *)pWidgetView_);
}
static unsigned long g_layoutOptions(MSWidgetView *pWidgetView_)
{
  MSLayoutManager *tp=(MSLayoutManager *) pWidgetView_->owner();
  return tp->options(pWidgetView_);
}

static I g_layoutUniformRows(MSLayoutManager *tb_) 
{ return MSTrue==tb_->uniformRows() ? 1 : 0; }
static I g_layoutUniformCols(MSLayoutManager *tb_) 
{ return  MSTrue==tb_->uniformColumns() ? 1 : 0; }
static I g_layoutLockSize(MSLayoutManager *tb_) 
{ return MSTrue==tb_->lockSize() ? 1 : 0; }
static I g_layoutLockPositions(MSLayoutManager *tb_)
{return MSTrue==tb_->lockPositions() ? 1 : 0;}
static void s_layoutUniformRows(MSLayoutManager *tb_,MSBoolean b_)
{ tb_->uniformRows(b_);}
static void s_layoutUniformCols(MSLayoutManager *tb_,MSBoolean b_)
{tb_->uniformColumns(b_);}
static void s_layoutLockSize(MSLayoutManager *tb_,MSBoolean b_)
{tb_->lockSize(b_);}
static void s_layoutLockPositions(MSLayoutManager *tb_,MSBoolean b_)
{tb_->lockPositions(b_); }

static void s_layoutMargin(MSLayoutManager *pWidgetView_, int margin_) { pWidgetView_->margin(margin_); }
static I g_layoutMargin(MSLayoutManager *pWidgetView_) { return (I) pWidgetView_->margin(); }

//  AplusNotebook 'A' interface functions

static A g_notebookOrientation(AplusNotebook *nb_)
{
  return gsym((MSNotebook::Vertical==nb_->orientation())?"vertical":"horizontal");
}

static I g_notebookShowBinding(AplusNotebook *nb_) 
{ return MSTrue== nb_->showBinding() ? 1 : 0; }
static unsigned g_notebookBindingWidth(AplusNotebook *nb_) 
{ return nb_->bindingWidth(); }
static unsigned g_notebookFrameThickness(AplusNotebook *nb_) 
{ return nb_->frameThickness(); }
static unsigned g_notebookMarginHeight(AplusNotebook *nb_)
{ return nb_->marginHeight(); }
static unsigned g_notebookMarginWidth(AplusNotebook *nb_)
{ return nb_->marginWidth(); }
static unsigned g_notebookBackpages(AplusNotebook *nb_)
{ return nb_->backpages(); }
static unsigned g_notebookBackpageThickness(AplusNotebook *nb_) 
{ return nb_->backpageThickness(); }
static unsigned long g_notebookBackpageFg(AplusNotebook *nb_)
{ return nb_->backpageForeground(); }
static unsigned long g_notebookBackpageBg(AplusNotebook *nb_)
{ return nb_->backpageBackground(); }
static unsigned long g_notebookFrameBackground(AplusNotebook *nb_)
{ return nb_->frameBackground(); }   
static A g_notebookCurrentPage(AplusNotebook *nb_)
{
  MSWidgetView *pWidgetView = (MSWidgetView *)nb_->currentWidget();
  if (pWidgetView)
    {
      AplusModel *m = (AplusModel *)pWidgetView->model();
      if (m)
	{
	  V v = m->aplusVar();
	  if (v!=0)
	    {
	      A a=(A)gs(Et);	// create an A object to store the symbol
	      a->p[0]=(I)MS(symjoin(v->cx->s,v->s)); // join the context and the symbol into dotted pair and store in a
	      return a;
	    }
	}
    }

  return aplus_nl;
}

static char *g_notebookPageTitle(AplusNotebook *nb_, MSWidgetView *page_)
{
  return (char *)nb_->titleFromWidget(page_);
}

static I g_notebookPageSelection(AplusNotebook *nb_,
					 MSWidgetView *page_)
{ return  MSTrue==nb_->tabAttribute(page_).sensitive() ? 1 : 0; }
static I g_notebookShowTabs(AplusNotebook *nb_)
{ return MSTrue==nb_->showTabs() ? 1 : 0; }
static I g_notebookShowPopup(AplusNotebook *nb_)
{ return MSTrue==nb_->showPopup() ? 1 : 0; }
static unsigned g_notebookBorderWidth(AplusNotebook *nb_) 
{ return nb_->borderWidth(); }
static unsigned g_notebookBorderHeight(AplusNotebook *nb_)
{ return nb_->borderHeight(); }
static unsigned long g_notebookSelectedPageFg(AplusNotebook *nb_)
{ return nb_->selectedPageForeground(); }
static unsigned long g_notebookSelectedPageBg(AplusNotebook *nb_)
{ return nb_->selectedPageBackground(); }
static I g_notebookLockSize(AplusNotebook *nb_) 
{ return MSTrue==nb_->lockSize() ? 1 : 0; }

static void s_notebookOrientation(AplusNotebook *nb_, A sym_)
{
  if (!QS(sym_)&&(sym_->t==Et&&sym_->n>0&&QS(*sym_->p)))
  {
    if(XS(*sym_->p)==si("vertical")) nb_->orientation(MSNotebook::Vertical);
    else if(XS(*sym_->p)==si("horizontal")) 
      nb_->orientation(MSNotebook::Horizontal);
  }

}

static void s_notebookShowBinding(AplusNotebook *nb_, MSBoolean b_)
{ nb_->showBinding(b_); }       
static void s_notebookBindingWidth(AplusNotebook *nb_, unsigned w_)
{ nb_->bindingWidth(w_); }      
static void s_notebookFrameThickness(AplusNotebook *nb_, unsigned t_)
{ nb_->frameThickness(t_); }    
static void s_notebookMarginHeight(AplusNotebook *nb_, unsigned h_)
{ nb_->marginHeight(h_); }      
static void s_notebookMarginWidth(AplusNotebook *nb_, unsigned w_)
{ nb_->marginWidth(w_); }       
static void s_notebookBackpages(AplusNotebook *nb_, unsigned n_)	
{ nb_->backpages(n_); }         
static void s_notebookBackpageThickness(AplusNotebook *nb_, unsigned t_)
{ nb_->backpageThickness(t_); } 
static void s_notebookBackpageFg(AplusNotebook *nb_, unsigned long fg_)
{ nb_->backpageForeground(fg_); }
static void s_notebookBackpageBg(AplusNotebook *nb_, unsigned long bg_)
{ nb_->backpageBackground(bg_); }
static void s_notebookFrameBackground(AplusNotebook *nb_, unsigned long bg_)
{ nb_->frameBackground(bg_); }   


static void s_notebookCurrentPage(AplusNotebook *nb_, A sym_)
{
  if (isNull(sym_)==MSTrue)	// make none of the pages selected
    {
      nb_->currentWidget(0);
    }
  else
    {
      AVariableData *pVarData=getVarData(sym_);
      if (pVarData!=0)
	{
	  MSWidgetView *pWidgetView=pVarData->pWidgetView();
	  if (pWidgetView!=0)
	    {
	      nb_->currentWidget(pVarData->pWidgetView());
	      return;
	    }
	}

      showError("Invalid notebook page");
    }
}

static void s_notebookPageTitle(AplusNotebook *nb_, MSWidgetView *widget_, A title_)
{
  if (QS(title_))
    {
      return;
    }

  if (title_->t==Ct)
    {
      nb_->pageTitle(widget_,(char *)title_->p);
    }
  else if (title_->t==Et)	 // nested array of strings
    {
      MSStringVector sv(AplusConvert::asMSStringVector(title_));
      if (sv.length()>0)
	{
	  // since notebook's pageTitle() method accepts only (const char *),
	  // we need to break up the string vector into a single string with
	  // embedded '\n' separators
	  //
	  nb_->pageTitle(widget_,sv.asString('\n').string());
	}
    }
}

static void s_notebookPageSelection(AplusNotebook *nb_, 
				    MSWidgetView *widget_, MSBoolean flag_)
{
  MSNotebookTabAttribute tab(nb_);
  tab.sensitive(flag_);
  nb_->tabAttribute(widget_, tab);
}

static void s_notebookShowTabs(AplusNotebook *nb_, MSBoolean flag_)
{ nb_->showTabs(flag_); }
static void s_notebookShowPopup(AplusNotebook *nb_, MSBoolean flag_)
{ nb_->showPopup(flag_); }
static void s_notebookBorderWidth(AplusNotebook *nb_, unsigned width_)
{ nb_->borderWidth(width_); }
static void s_notebookBorderHeight(AplusNotebook *nb_, unsigned height_)
{ nb_->borderHeight(height_); }
static void s_notebookSelectedPageFg(AplusNotebook *nb_, unsigned long fg_)
{ nb_->selectedPageForeground(fg_); }
static void s_notebookSelectedPageBg(AplusNotebook *nb_, unsigned long bg_)
{ nb_->selectedPageBackground(bg_); }
static void s_notebookLockSize(AplusNotebook *nb_, MSBoolean flag_) 
{ nb_->lockSize(flag_); }

static MSWidgetVector AIntArrayToWidgetVector(A aobj_)
{
  MSWidgetVector svec;
  if(It!=aobj_->t) return svec;
  for (int i=0;i<aobj_->n;++i)
    svec.append((MSWidget *)aobj_->p[i]);
  return svec;
}

static void bNotebookPermute(AplusNotebook *nb_, A wids_)
{
  nb_->permuteWidgets(AIntArrayToWidgetVector(wids_));
}

//////////////////////////////////////////////////////////////////////////////

// Widget Creation Functions
static AplusDisplayServer *c_XServer(char *dpyName_)
{
  AplusDisplayServer *server;
  if (dpyName_!=0) server=new AplusDisplayServer(dpyName_);
  else server=new AplusDisplayServer;
  return server;
}

static MSShell *c_ShellW(MSDisplayServer *server_)
{ MSShell *sp;
  if (server_==0) sp=new AplusShell;
  else sp=new AplusShell(server_); 
  (void)sp->windowGroup(g_defaultLeader());
  return sp;
}
static MSPopup *c_XPopupW(MSDisplayServer *server_)
{ MSPopup *pw;
  if (server_==0) pw=new AplusPopup;
  else pw=new AplusPopup(server_); 
  (void)pw->windowGroup(g_defaultLeader());
  return pw;
}

// generate a shell as the parent if parent==0 -- ref: zero parent
MSWidgetView *validateParent(MSWidgetView *parent_)
{
  MSWidgetView *pp=parent_;
  if (pp==0) 
    { MSShell *sp=new AplusShell; (void)sp->windowGroup(g_defaultLeader()); pp=sp;}
  else if (pp==(MSWidgetView *)-1) 
    { MSPopup *po=new AplusPopup; (void)po->windowGroup(g_defaultLeader()); pp=po;}
  return pp; 
}

static MSWidgetView *c_AXReference(MSWidgetView *parent_)
{ return (MSWidgetView *) new AplusReference(validateParent(parent_)); }
static MSWidgetView *c_AXLabelM   (MSWidgetView *parent_)
{ return (MSWidgetView *) new AplusLabel(validateParent(parent_)); }
static MSWidgetView *c_AXScalar(MSWidgetView *parent_)
{ return (MSWidgetView *) new AplusEntryField(validateParent(parent_)); }
static MSWidgetView *c_AXPushB(MSWidgetView *parent_)
{ return (MSWidgetView *) new AplusButton(validateParent(parent_)); }
static MSWidgetView *c_AXLayout(MSWidgetView *parent_)
{ return (MSWidgetView *) new AplusLayout(validateParent(parent_)); }
static MSWidgetView *c_AXArray(MSWidgetView *parent_)
{ return (MSWidgetView *) new AplusArray(validateParent(parent_)); }
static MSWidgetView *c_AXTextM    (MSWidgetView *parent_)
{ return (MSWidgetView *) new AplusText(validateParent(parent_)); }
static MSWidgetView *c_XScrollW(MSWidgetView *parent_)
{ return (MSWidgetView *) new AplusScrolledWindow(validateParent(parent_)); }
static MSWidgetView *c_AXField(MSWidgetView *parent_)
{ return (MSWidgetView *) new AplusTableColumn((AplusTable *)validateParent(parent_)); }
static MSWidgetView *c_AXTable(MSWidgetView *parent_)
{ return (MSWidgetView *) new AplusTable(validateParent(parent_)); }
static MSWidgetView *c_AXCollapsibleW(MSWidgetView *parent_)
{ return (MSWidgetView *) new AplusCollapsible(validateParent(parent_)); }
static MSWidgetView *c_AXPaneW(MSWidgetView *parent_)
{ return (MSWidgetView *) new AplusPane(validateParent(parent_)); }
static MSWidgetView *c_AXVPaneW(MSWidgetView *parent_)
{ return (MSWidgetView *) new AplusVPane(validateParent(parent_)); }
static MSWidgetView *c_AXHPaneW(MSWidgetView *parent_)
{ return (MSWidgetView *) new AplusHPane(validateParent(parent_)); }
static MSWidgetView *c_Notebook(MSWidgetView *parent_)
{ return (MSWidgetView *) new AplusNotebook(validateParent(parent_)); }
static MSWidgetView *c_AXBtnBox(MSWidgetView *parent_)
{ return (MSWidgetView *) new AplusButtonBox(validateParent(parent_)); }
static MSWidgetView *c_AXCheckBox(MSWidgetView *parent_)
{ return (MSWidgetView *) new AplusCheckBox(validateParent(parent_)); }
static MSWidgetView *c_AXRadioBox(MSWidgetView *parent_)
{ return (MSWidgetView *) new AplusRadioBox(validateParent(parent_)); }
static MSWidgetView *c_AXPage(MSWidgetView *parent_)
{ return (MSWidgetView *) new AplusPage(validateParent(parent_)); }
static MSWidgetView *c_AXTree(MSWidgetView *parent_)
{ return (MSWidgetView *) new AplusTreeView(validateParent(parent_)); }
static MSWidgetView *c_AXPassword(MSWidgetView *parent_)
{ return (MSWidgetView *) new AplusPassword(validateParent(parent_)); }
static MSWidgetView *c_AXSlot(MSWidgetView *parent_)
{ return (MSWidgetView *) new AplusSlot(validateParent(parent_)); }
static MSWidgetView *c_AXCommand(MSWidgetView *parent_)
{ return (MSWidgetView *) new AplusCommand(validateParent(parent_)); }
static MSWidgetView *c_AXView(MSWidgetView *parent_)
{ return (MSWidgetView *) new AplusView(validateParent(parent_)); }
static MSWidgetView *c_AXChoice(MSWidgetView *parent_)
{ return (MSWidgetView *) new AplusChoice(validateParent(parent_)); }
static MSWidgetView *c_AXHMenu(MSWidgetView *parent_)
{ return (MSWidgetView *) new AplusMenu(validateParent(parent_)); }
static MSWidgetView *c_AXVMenu(MSWidgetView *parent_)
{ return (MSWidgetView *) new AplusMenu(validateParent(parent_),AplusMenu::Vertical); }
static MSWidgetView *c_XManager(MSWidgetView *parent_)
{ return (MSWidgetView *) new AplusManager(validateParent(parent_)); }
static MSWidgetView *c_AXCross(MSWidgetView *parent_)
{ return (MSWidgetView *) new AplusMatrix(validateParent(parent_)); }
static MSWidgetView *c_AXVScale(MSWidgetView *parent_)
{ return (MSWidgetView *) new AplusVScale(validateParent(parent_)); }
static MSWidgetView *c_AXHScale(MSWidgetView *parent_)
{ return (MSWidgetView *) new AplusHScale(validateParent(parent_)); }
static MSWidgetView *c_AXHGauge(MSWidgetView *parent_)
{ return (MSWidgetView *) new AplusHGauge(validateParent(parent_)); }
static MSWidgetView *c_AXVGauge(MSWidgetView *parent_)
{ return (MSWidgetView *) new AplusVGauge(validateParent(parent_)); }

static void		  *c_AColorObject(MSWidgetView *)    { return (void *) 0; }

/*
static ColorObject *c_AColorObject(MSWidgetView *parent_)
{ return (ColorObject *) new ColorObject(validateParent(parent_)); }
*/


static void installC_Commands(void)
{
  install((PFI)c_XServer,"c_XServer",IV,1,CP,0,0,0,0,0,0,0);
  install((PFI)c_ShellW,"c_ShellW",IV,1,IV,0,0,0,0,0,0,0);
  install((PFI)c_XPopupW,"c_XPopupW",IV,1,IV,0,0,0,0,0,0,0);
  install((PFI)c_AXReference,"c_AXReference",IV,1,IV,0,0,0,0,0,0,0);
  install((PFI)c_AXVMenu,"c_AXVMenu",IV,1,IV,0,0,0,0,0,0,0);
  install((PFI)c_AXHMenu,"c_AXHMenu",IV,1,IV,0,0,0,0,0,0,0);
  install((PFI)c_AXTree,"c_AXTree",IV,1,IV,0,0,0,0,0,0,0);
  install((PFI)c_AXCommand,"c_AXCommand",IV,1,IV,0,0,0,0,0,0,0);
  install((PFI)c_AXPassword,"c_AXPassword",IV,1,IV,0,0,0,0,0,0,0);
  install((PFI)c_XManager,"c_XManager",IV,1,IV,0,0,0,0,0,0,0);
  install((PFI)c_AXLayout,"c_XLayout",IV,1,IV,0,0,0,0,0,0,0);
  install((PFI)c_AXPage,"c_AXPage",IV,1,IV,0,0,0,0,0,0,0);
  install((PFI)c_AXCollapsibleW,"c_AXCollapsibleW",IV,1,IV,0,0,0,0,0,0,0);
  install((PFI)c_AXPaneW,"c_AXPaneW",IV,1,IV,0,0,0,0,0,0,0);
  install((PFI)c_AXHPaneW,"c_AXHPaneW",IV,1,IV,0,0,0,0,0,0,0);
  install((PFI)c_AXVPaneW,"c_AXVPaneW",IV,1,IV,0,0,0,0,0,0,0);
  install((PFI)c_AXTextM,"c_AXTextM",IV,1,IV,0,0,0,0,0,0,0);
  install((PFI)c_AXLabelM,"c_AXLabelM",IV,1,IV,0,0,0,0,0,0,0);
  install((PFI)c_AXBtnBox,"c_AXBtnBox",IV,1,IV,0,0,0,0,0,0,0);
  install((PFI)c_XScrollW,"c_XScrollW",IV,1,IV,0,0,0,0,0,0,0);
  install((PFI)c_AXTable,"c_AXTable",IV,1,IV,0,0,0,0,0,0,0);
  install((PFI)c_AXField,"c_AXField",IV,1,IV,0,0,0,0,0,0,0);
  install((PFI)c_AXCross,"c_AXCross",IV,1,IV,0,0,0,0,0,0,0);
  install((PFI)c_AXArray,"c_AXArray",IV,1,IV,0,0,0,0,0,0,0);
  install((PFI)c_AXView,"c_AXView",IV,1,IV,0,0,0,0,0,0,0);
  install((PFI)c_AXScalar,"c_AXScalar",IV,1,IV,0,0,0,0,0,0,0);
  install((PFI)c_AXChoice,"c_AXChoice",IV,1,IV,0,0,0,0,0,0,0);
  install((PFI)c_AColorObject,"c_AColorObject",IV,1,IV,0,0,0,0,0,0,0);
  install((PFI)c_AXHScale,"c_AXHScale",IV,1,IV,0,0,0,0,0,0,0);
  install((PFI)c_AXVScale,"c_AXVScale",IV,1,IV,0,0,0,0,0,0,0);
  install((PFI)c_AXHGauge,"c_AXHGauge",IV,1,IV,0,0,0,0,0,0,0);
  install((PFI)c_AXVGauge,"c_AXVGauge",IV,1,IV,0,0,0,0,0,0,0);
  install((PFI)c_AXPushB,"c_AXPushB",IV,1,IV,0,0,0,0,0,0,0);
  install((PFI)c_AXRadioBox,"c_AXRadioBox",IV,1,IV,0,0,0,0,0,0,0);
  install((PFI)c_AXCheckBox,"c_AXCheckBox",IV,1,IV,0,0,0,0,0,0,0);
  install((PFI)c_AXSlot,"c_AXSlot",IV,1,IV,0,0,0,0,0,0,0);
  install((PFI)c_Notebook,"c_Notebook",IV,1,IV,0,0,0,0,0,0,0);
}

static void installTextCommands(void)
{
  install((PFI)g_textBuffer,"g_textBuffer",A_,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_textNumVisibleRows,"s_textNumVisibleRows",V_,2,IV,IV,0,0,0,0,0,0);
  install((PFI)s_textNumVisibleCols,"s_textNumVisibleCols",V_,2,IV,IV,0,0,0,0,0,0);
  install((PFI)g_textNumVisibleRows,"g_textNumVisibleRows",IV,1,IV,0,0,0,0,0,0,0);
  install((PFI)g_textNumVisibleCols,"g_textNumVisibleCols",IV,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_textCursorPosition,"s_textCursorPosition",V_,2,IV,IV,0,0,0,0,0,0);
  install((PFI)g_textCursorPosition,"g_textCursorPosition",IV,1,IV,0,0,0,0,0,0,0);
}			 

static void installWidgetCommands(void)
{
  install((PFI)s_width,"s_width",V_,2,IV,IV,0,0,0,0,0,0);
  install((PFI)s_height,"s_height",V_,2,IV,IV,0,0,0,0,0,0);
  install((PFI)s_x,"s_x",V_,2,IV,IV,0,0,0,0,0,0);
  install((PFI)s_y,"s_y",V_,2,IV,IV,0,0,0,0,0,0);
  install((PFI)s_acceptFocus,"s_acceptFocus",V_,2,IV,IV,0,0,0,0,0,0);
  install((PFI)g_width,"g_width",IV,1,IV,0,0,0,0,0,0,0);
  install((PFI)g_height,"g_height",IV,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_parent,"s_parent",V_,2,IV,IV,0,0,0,0,0,0);
  install((PFI)g_parent,"g_parent",IV,1,IV,0,0,0,0,0,0,0);
  install((PFI)g_x,"g_x",IV,1,IV,0,0,0,0,0,0,0);
  install((PFI)g_y,"g_y",IV,1,IV,0,0,0,0,0,0,0);
  install((PFI)g_acceptFocus,"g_acceptFocus",IV,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_sflags,"s_sflags",V_,2,IV,A_,0,0,0,0,0,0); 
  install((PFI)g_sflags,"g_sflags",A_,1,IV,0,0,0,0,0,0,0); 

  install((PFI)s_resizeOptions,"s_resizeOptions",V_,2,IV,IV,0,0,0,0,0,0); 
  install((PFI)g_resizeOptions,"g_resizeOptions",IV,1,IV,0,0,0,0,0,0,0); 
  install((PFI)s_background,"s_background",V_,2,IV,IV,0,0,0,0,0,0);
  install((PFI)g_background,"g_background",IV,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_foreground,"s_foreground",V_,2,IV,IV,0,0,0,0,0,0);
  install((PFI)g_foreground,"g_foreground",IV,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_hlColor,"s_hlColor",V_,2,IV,IV,0,0,0,0,0,0);
  install((PFI)g_hlColor,"g_hlColor",IV,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_hlThickness,"s_hlThickness",V_,2,IV,IV,0,0,0,0,0,0);
  install((PFI)g_hlThickness,"g_hlThickness",IV,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_shadowThickness,"s_shadowThickness",V_,2,IV,IV,0,0,0,0,0,0);
  install((PFI)g_shadowThickness,"g_shadowThickness",IV,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_shadowStyle,"s_shadowStyle",V_,2,IV,A_,0,0,0,0,0,0);
  install((PFI)g_shadowStyle,"g_shadowStyle",A_,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_titleAlign,"s_titleAlign",V_,2,IV,A_,0,0,0,0,0,0);
  install((PFI)g_titleAlign,"g_titleAlign",A_,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_font,"s_font",V_,2,IV,IV,0,0,0,0,0,0);
  install((PFI)g_font,"g_font",IV,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_freeze,"s_freeze",V_,2,IV,IV,0,0,0,0,0,0);
  install((PFI)g_freeze,"g_freeze",IV,1,IV,0,0,0,0,0,0,0);
  install((PFI)g_focus,"g_focus",IV,1,IV,0,0,0,0,0,0,0);
  install((PFI)g_focusWindow,"g_focusWindow",IV,0,0,0,0,0,0,0,0,0);
  install((PFI)g_visible,"g_visible",IV,1,IV,0,0,0,0,0,0,0);
  install((PFI)g_sensitive,"g_sensitive",IV,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_sensitive,"s_sensitive",V_,2,IV,IV,0,0,0,0,0,0);
  install((PFI)g_dynamic,"g_dynamic",IV,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_dynamic,"s_dynamic",V_,2,IV,IV,0,0,0,0,0,0);
  install((PFI)s_defaultLeader,"s_defaultLeader",V_,1,IV,0,0,0,0,0,0,0);
  install((PFI)g_defaultLeader,"g_defaultLeader",IV,0,0,0,0,0,0,0,0,0);
  install((PFI)s_saveYourselfWindow,"s_savewmWindow",IV,1,IV,0,0,0,0,0,0,0);
  install((PFI)g_saveYourselfWindow,"g_savewmWindow",IV,0,0,0,0,0,0,0,0,0);

}

static void installPasswordCommands(void)
{
  install((PFI)g_passwordValidity,"g_passwordValidity",IV,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_passwordFillChar,"s_passwordFillChar",V_,2,IV,IV,0,0,0,0,0,0);
  install((PFI)g_passwordFillChar,"g_passwordFillChar",IV,1,IV,0,0,0,0,0,0,0);
}

static void installTreeCommands(void)
{
  install((PFI)s_treeSelectedNode,"s_treeSelectedNode",V_,2,IV,A_,0,0,0,0,0,0);
  install((PFI)g_treeSelectedNode,"g_treeSelectedNode",A_,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_treeOrientation,"s_treeOrientation",V_,2,IV,A_,0,0,0,0,0,0);
  install((PFI)g_treeOrientation,"g_treeOrientation",A_,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_treeVerticalSpace,"s_treeVerticalSpace",V_,2,IV,IV,0,0,0,0,0,0);
  install((PFI)g_treeVerticalSpace,"g_treeVerticalSpace",IV,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_treeHorizontalSpace,"s_treeHorizontalSpace",V_,2,IV,IV,0,0,0,0,0,0);
  install((PFI)g_treeHorizontalSpace,"g_treeHorizontalSpace",IV,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_treeNodeBg,"s_treeNodeBg",V_,2,IV,IV,0,0,0,0,0,0);
  install((PFI)s_treeNodeFg,"s_treeNodeFg",V_,2,IV,IV,0,0,0,0,0,0);
  install((PFI)g_treeNodeBg,"g_treeNodeBg",IV,1,IV,0,0,0,0,0,0,0);
  install((PFI)g_treeNodeFg,"g_treeNodeFg",IV,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_treeSelectedNodeBg,"s_treeSelectedNodeBg",V_,2,IV,IV,0,0,0,0,0,0);
  install((PFI)s_treeSelectedNodeFg,"s_treeSelectedNodeFg",V_,2,IV,IV,0,0,0,0,0,0);
  install((PFI)g_treeSelectedNodeBg,"g_treeSelectedNodeBg",IV,1,IV,0,0,0,0,0,0,0);
  install((PFI)g_treeSelectedNodeFg,"g_treeSelectedNodeFg",IV,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_treeShowButtons,"s_treeShowButtons",V_,2,IV,IV,0,0,0,0,0,0);
  install((PFI)g_treeShowButtons,"g_treeShowButtons",IV,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_treeLineColor,"s_treeLineColor",V_,2,IV,IV,0,0,0,0,0,0);
  install((PFI)g_treeLineColor,"g_treeLineColor",IV,1,IV,0,0,0,0,0,0,0);
}

static void installCommandCommands(void)
{
  install((PFI)s_commandCursorPosition,"s_commandCursorPosition",V_,2,IV,IV,0,0,0,0,0,0);
  install((PFI)g_commandCursorPosition,"g_commandCursorPosition",IV,1,IV,0,0,0,0,0,0,0);
  install((PFI)g_commandBuffer,"g_commandBuffer",CP,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_commandBuffer,"s_commandBuffer",V_,2,IV,A_,0,0,0,0,0,0);
}

static void installPageCommands(void)
{
  install((PFI)g_pageCursor,"g_pageCursor",A_,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_pageBlinkRate,"s_pageBlinkRate",V_,2,IV,IV,0,0,0,0,0,0);
  install((PFI)g_pageBlinkRate,"g_pageBlinkRate",IV,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_pageColorTable,"s_pageColorTable",V_,2,IV,A_,0,0,0,0,0,0);
  install((PFI)g_pageColorTable,"g_pageColorTable",A_,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_pageBoxes,"s_pageBoxes",V_,2,IV,A_,0,0,0,0,0,0);
  install((PFI)g_pageBoxes,"g_pageBoxes",A_,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_pageLines,"s_pageLines",V_,2,IV,A_,0,0,0,0,0,0);
  install((PFI)g_pageLines,"g_pageLines",A_,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_pageLineWidth,"s_pageLineWidth",V_,2,IV,IV,0,0,0,0,0,0);
  install((PFI)g_pageLineWidth,"g_pageLineWidth",IV,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_pageBoxColors,"s_pageBoxColors",V_,2,IV,A_,0,0,0,0,0,0);
  install((PFI)g_pageBoxColors,"g_pageBoxColors",A_,1,IV,0,0,0,0,0,0,0);
  install((PFI)g_pageRubberBand,"g_pageRubberBand",A_,1,IV,0,0,0,0,0,0,0);
  install((PFI)g_pageKeyBuffer,"g_pageKeyBuffer",A_,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_pageIndexFunc,"s_pageIndexFunc",V_,2,IV,A_,0,0,0,0,0,0);
  install((PFI)g_pageIndexFunc,"g_pageIndexFunc",A_,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_pageBoldFunc,"s_pageBoldFunc",V_,2,IV,A_,0,0,0,0,0,0);
  install((PFI)g_pageBoldFunc,"g_pageBoldFunc",A_,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_pageBlinkFunc,"s_pageBlinkFunc",V_,2,IV,A_,0,0,0,0,0,0);
  install((PFI)g_pageBlinkFunc,"g_pageBlinkFunc",A_,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_pageUnderlineFunc,"s_pageUnderlineFunc",V_,2,IV,A_,0,0,0,0,0,0);
  install((PFI)g_pageUnderlineFunc,"g_pageUnderlineFunc",A_,1,IV,0,0,0,0,0,0,0);
}  

static void installArrayCommands(void)
{
  install((PFI)s_numVisibleRows,"s_numVisibleRows",V_,2,IV,IV,0,0,0,0,0,0);
  install((PFI)s_numVisibleCols,"s_numVisibleCols",V_,2,IV,IV,0,0,0,0,0,0);
  install((PFI)g_numVisibleRows,"g_numVisibleRows",IV,1,IV,0,0,0,0,0,0,0);
  install((PFI)g_numVisibleCols,"g_numVisibleCols",IV,1,IV,0,0,0,0,0,0,0);
  install((PFI)g_numChildren,"g_numChildren",IV,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_arrayCycleFunc,"s_arrayCycleFunc",V_,2,IV,A_,0,0,0,0,0,0);
  install((PFI)g_arrayCycleFunc,"g_arrayCycleFunc",A_,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_arrayCycleColors,"s_arrayCycleColors",V_,2,IV,A_,0,0,0,0,0,0);
  install((PFI)g_arrayCycleColors,"g_arrayCycleColors",A_,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_arrayCycleInterval,"s_arrayCycleInterval",V_,2,IV,IV,0,0,0,0,0,0);
  install((PFI)g_arrayCycleInterval,"g_arrayCycleInterval",IV,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_arrayCycleColorMode,"s_arrayCycleColorMode",V_,2,IV,A_,0,0,0,0,0,0);
  install((PFI)g_arrayCycleColorMode,"g_arrayCycleColorMode",A_,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_arraySelectVector,"s_arraySelectVector",V_,2,IV,A_,0,0,0,0,0,0);
  install((PFI)g_arraySelectVector,"g_arraySelectVector",A_,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_arrayRowBg,"s_arrayRowBg",V_,2,IV,IV,0,0,0,0,0,0);
  install((PFI)g_arrayRowBg,"g_arrayRowBg",IV,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_arraySelectBg,"s_arraySelectBg",V_,2,IV,IV,0,0,0,0,0,0);
  install((PFI)g_arraySelectBg,"g_arraySelectBg",IV,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_arraySelectVectorBg,"s_arraySelectVectorBg",V_,2,IV,IV,0,0,0,0,0,0);
  install((PFI)g_arraySelectVectorBg,"g_arraySelectVectorBg",IV,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_arrayVsbBg,"s_arrayVsbBg",V_,2,IV,IV,0,0,0,0,0,0);
  install((PFI)s_arrayHsbBg,"s_arrayHsbBg",V_,2,IV,IV,0,0,0,0,0,0);
  install((PFI)g_arrayVsbBg,"g_arrayVsbBg",IV,1,IV,0,0,0,0,0,0,0);
  install((PFI)g_arrayHsbBg,"g_arrayHsbBg",IV,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_arrayEditorBg,"s_arrayEditorBg",V_,2,IV,IV,0,0,0,0,0,0);
  install((PFI)s_arrayEditorFg,"s_arrayEditorFg",V_,2,IV,IV,0,0,0,0,0,0);
  install((PFI)g_arrayEditorBg,"g_arrayEditorBg",IV,1,IV,0,0,0,0,0,0,0);
  install((PFI)g_arrayEditorFg,"g_arrayEditorFg",IV,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_arraySelectRow,"s_arraySelectRow",V_,2,IV,IV,0,0,0,0,0,0);
  install((PFI)s_arraySelectCol,"s_arraySelectCol",V_,2,IV,IV,0,0,0,0,0,0);
  install((PFI)s_arrayFirstRow,"s_arrayFirstRow",V_,2,IV,IV,0,0,0,0,0,0);
  install((PFI)s_arrayFirstCol,"s_arrayFirstCol",V_,2,IV,IV,0,0,0,0,0,0);
  install((PFI)s_arraySbSize,"s_arraySbSize",V_,2,IV,IV,0,0,0,0,0,0);
  install((PFI)s_arrayVsbSize,"s_arrayVsbSize",V_,2,IV,IV,0,0,0,0,0,0);
  install((PFI)s_arrayHsbSize,"s_arrayHsbSize",V_,2,IV,IV,0,0,0,0,0,0);
  install((PFI)s_arraySeparatorInterval,"s_arraySeparatorInterval",V_,2,IV,IV,0,0,0,0,0,0);
  install((PFI)s_arrayColSeparator,"s_arrayColSeparator",V_,2,IV,IV,0,0,0,0,0,0);
  install((PFI)g_arraySelectRow,"g_arraySelectRow",IV,1,IV,0,0,0,0,0,0,0);
  install((PFI)g_arraySelectCol,"g_arraySelectCol",IV,1,IV,0,0,0,0,0,0,0);
  install((PFI)g_arrayFirstRow,"g_arrayFirstRow",IV,1,IV,0,0,0,0,0,0,0);
  install((PFI)g_arrayFirstCol,"g_arrayFirstCol",IV,1,IV,0,0,0,0,0,0,0);
  install((PFI)g_arraySbSize,"g_arraySbSize",IV,1,IV,0,0,0,0,0,0,0);
  install((PFI)g_arrayVsbSize,"g_arrayVsbSize",IV,1,IV,0,0,0,0,0,0,0);
  install((PFI)g_arrayHsbSize,"g_arrayHsbSize",IV,1,IV,0,0,0,0,0,0,0);
  install((PFI)g_arraySeparatorInterval,"g_arraySeparatorInterval",IV,1,IV,0,0,0,0,0,0,0);
  install((PFI)g_arrayColSeparator,"g_arrayColSeparator",IV,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_arrayEdit,"s_arrayEdit",V_,2,IV,IV,0,0,0,0,0,0);
  install((PFI)g_arrayEdit,"g_arrayEdit",IV,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_arraySelectionMode,"s_arraySelectionMode",V_,2,IV,A_,0,0,0,0,0,0);
  install((PFI)g_arraySelectionMode,"g_arraySelectionMode",A_,1,IV,0,0,0,0,0,0,0);
}

static void installPrimitiveTextCommands(void)
{
  install((PFI)s_labelOptions,"s_labelOptions",V_,2,IV,A_,0,0,0,0,0,0);
  install((PFI)g_labelOptions,"g_labelOptions",A_,1,IV,0,0,0,0,0,0,0);
}

static void installButtonBoxCommands(void)
{
  install((PFI)s_actionBoxOptions,"s_actionBoxOptions",V_,2,IV,A_,0,0,0,0,0,0);
  install((PFI)g_actionBoxOptions,"g_actionBoxOptions",A_,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_boxSelected,"s_boxSelected",V_,2,IV,IV,0,0,0,0,0,0);
  install((PFI)g_boxSelected,"g_boxSelected",IV,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_actionHlColor,"s_actionHlColor",V_,2,IV,IV,0,0,0,0,0,0);
  install((PFI)g_actionHlColor,"g_actionHlColor",IV,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_actionHlThickness,"s_actionHlThickness",V_,2,IV,IV,0,0,0,0,0,0);
  install((PFI)g_actionHlThickness,"g_actionHlThickness",IV,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_actionButtonShadowThickness,"s_actionButtonShadowThickness",V_,2,IV,IV,0,0,0,0,0,0);
  install((PFI)g_actionButtonShadowThickness,"g_actionButtonShadowThickness",IV,1,IV,0,0,0,0,0,0,0);
}

static void installCheckBoxCommands(void)
{
  install((PFI)s_checkHlColor,"s_checkHlColor",V_,2,IV,IV,0,0,0,0,0,0);
  install((PFI)g_checkHlColor,"g_checkHlColor",IV,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_checkHlThickness,"s_checkHlThickness",V_,2,IV,IV,0,0,0,0,0,0);
  install((PFI)g_checkHlThickness,"g_checkHlThickness",IV,1,IV,0,0,0,0,0,0,0);
}

static void installRadioBoxCommands(void)
{
  install((PFI)s_radioHlColor,"s_radioHlColor",V_,2,IV,IV,0,0,0,0,0,0);
  install((PFI)g_radioHlColor,"g_radioHlColor",IV,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_radioHlThickness,"s_radioHlThickness",V_,2,IV,IV,0,0,0,0,0,0);
  install((PFI)g_radioHlThickness,"g_radioHlThickness",IV,1,IV,0,0,0,0,0,0,0);
}


static void installDisplayServerCommands(void)
{
  install((PFI)g_serverDisplay,"g_serverDisplay",IV,1,IV,0,0,0,0,0,0,0);
  install((PFI)g_screenWidth,"g_screenWidth",IV,1,IV,0,0,0,0,0,0,0);
  install((PFI)g_screenHeight,"g_screenHeight",IV,1,IV,0,0,0,0,0,0,0);
}

static void installManagerCommands(void)
{
  install((PFI)s_mgrDoubleClickTime,"s_mgrDoubleClickTime",V_,2,IV,IV,0,0,0,0,0,0);
  install((PFI)g_mgrDoubleClickTime,"g_mgrDoubleClickTime",IV,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_mgrLongPressTime,"s_mgrLongPressTime",V_,2,IV,IV,0,0,0,0,0,0);
  install((PFI)g_mgrLongPressTime,"g_mgrLongPressTime",IV,1,IV,0,0,0,0,0,0,0);
  install((PFI)g_serverName,"g_serverName",CP,1,IV,0,0,0,0,0,0,0);
}

static void installColorObjectCommands(void)
{
  install((PFI)s_colorBgString,"s_colorBgString",V_,2,IV,CP,0,0,0,0,0,0);
  install((PFI)s_colorBgPixel,"s_colorBgPixel",V_,2,IV,IV,0,0,0,0,0,0);
  install((PFI)g_colorBgString,"g_colorBgString",A_,1,IV,0,0,0,0,0,0,0);
  install((PFI)g_colorBgPixel,"g_colorBgPixel",IV,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_colorFgString,"s_colorFgString",V_,2,IV,CP,0,0,0,0,0,0);
  install((PFI)s_colorFgPixel,"s_colorFgPixel",V_,2,IV,IV,0,0,0,0,0,0);
  install((PFI)g_colorFgString,"g_colorFgString",A_,1,IV,0,0,0,0,0,0,0);
  install((PFI)g_colorFgPixel,"g_colorFgPixel",IV,1,IV,0,0,0,0,0,0,0);
  install((PFI)bServerParseColor,"bServerParseColor",IV,2,IV,CP,0,0,0,0,0,0);
  install((PFI)bAllocColor,"bAllocColor",IV,2,IV,CP,0,0,0,0,0,0);
  install((PFI)bFreeColor,"bFreeColor",V_,2,IV,IV,0,0,0,0,0,0);
}

static void installScaleCommands(void)
{
  install((PFI)s_scaleValueMin,"s_scaleValueMin",V_,2,IV,A_,0,0,0,0,0,0);
  install((PFI)g_scaleValueMin,"g_scaleValueMin",A_,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_scaleValueMax,"s_scaleValueMax",V_,2,IV,A_,0,0,0,0,0,0);
  install((PFI)g_scaleValueMax,"g_scaleValueMax",A_,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_scaleValueInc,"s_scaleValueInc",V_,2,IV,A_,0,0,0,0,0,0);
  install((PFI)g_scaleValueInc,"g_scaleValueInc",A_,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_scaleValuePageInc,"s_scaleValuePageInc",V_,2,IV,A_,0,0,0,0,0,0);
  install((PFI)g_scaleValuePageInc,"g_scaleValuePageInc",A_,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_scaleValueFont,"s_scaleValueFont",V_,2,IV,IV,0,0,0,0,0,0);
  install((PFI)g_scaleValueFont,"g_scaleValueFont",IV,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_scaleValueFg,"s_scaleValueFg",V_,2,IV,IV,0,0,0,0,0,0);
  install((PFI)g_scaleValueFg,"g_scaleValueFg",IV,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_scaleValueAlign,"s_scaleValueAlign",V_,2,IV,A_,0,0,0,0,0,0);
  install((PFI)g_scaleValueAlign,"g_scaleValueAlign",A_,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_scaleSliderBg,"s_scaleSliderBg",V_,2,IV,IV,0,0,0,0,0,0);
  install((PFI)g_scaleSliderBg,"g_scaleSliderBg",IV,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_scaleSliderHeight,"s_scaleSliderHeight",V_,2,IV,A_,0,0,0,0,0,0);
  install((PFI)g_scaleSliderHeight,"g_scaleSliderHeight",A_,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_scaleSliderWidth,"s_scaleSliderWidth",V_,2,IV,A_,0,0,0,0,0,0);
  install((PFI)g_scaleSliderWidth,"g_scaleSliderWidth",A_,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_scaleTitleAlign,"s_scaleTitleAlign",V_,2,IV,A_,0,0,0,0,0,0);
  install((PFI)g_scaleTitleAlign,"g_scaleTitleAlign",A_,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_scaleSubtitle,"s_scaleSubtitle",V_,2,IV,A_,0,0,0,0,0,0);
  install((PFI)g_scaleSubtitle,"g_scaleSubtitle",A_,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_scaleSubtitleFont,"s_scaleSubtitleFont",V_,2,IV,IV,0,0,0,0,0,0);
  install((PFI)g_scaleSubtitleFont,"g_scaleSubtitleFont",IV,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_scaleSubtitleFg,"s_scaleSubtitleFg",V_,2,IV,IV,0,0,0,0,0,0);
  install((PFI)g_scaleSubtitleFg,"g_scaleSubtitleFg",IV,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_scaleSubtitleAlign,"s_scaleSubtitleAlign",V_,2,IV,A_,0,0,0,0,0,0);
  install((PFI)g_scaleSubtitleAlign,"g_scaleSubtitleAlign",A_,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_scaleMintitle,"s_scaleMintitle",V_,2,IV,A_,0,0,0,0,0,0);
  install((PFI)g_scaleMintitle,"g_scaleMintitle",A_,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_scaleMintitleFg,"s_scaleMintitleFg",V_,2,IV,IV,0,0,0,0,0,0);
  install((PFI)g_scaleMintitleFg,"g_scaleMintitleFg",IV,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_scaleMintitleAlign,"s_scaleMintitleAlign",V_,2,IV,A_,0,0,0,0,0,0);
  install((PFI)g_scaleMintitleAlign,"g_scaleMintitleAlign",A_,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_scaleMintitleFont,"s_scaleMintitleFont",V_,2,IV,IV,0,0,0,0,0,0);
  install((PFI)g_scaleMintitleFont,"g_scaleMintitleFont",IV,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_scaleMaxtitle,"s_scaleMaxtitle",V_,2,IV,A_,0,0,0,0,0,0);
  install((PFI)g_scaleMaxtitle,"g_scaleMaxtitle",A_,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_scaleMaxtitleFg,"s_scaleMaxtitleFg",V_,2,IV,IV,0,0,0,0,0,0);
  install((PFI)g_scaleMaxtitleFg,"g_scaleMaxtitleFg",IV,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_scaleMaxtitleAlign,"s_scaleMaxtitleAlign",V_,2,IV,A_,0,0,0,0,0,0);
  install((PFI)g_scaleMaxtitleAlign,"g_scaleMaxtitleAlign",A_,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_scaleMaxtitleFont,"s_scaleMaxtitleFont",V_,2,IV,IV,0,0,0,0,0,0);
  install((PFI)g_scaleMaxtitleFont,"g_scaleMaxtitleFont",IV,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_scaleLabelInc,"s_scaleLabelInc",V_,2,IV,A_,0,0,0,0,0,0);
  install((PFI)g_scaleLabelInc,"g_scaleLabelInc",A_,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_scaleLabelFont,"s_scaleLabelFont",V_,2,IV,IV,0,0,0,0,0,0);
  install((PFI)g_scaleLabelFont,"g_scaleLabelFont",IV,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_scaleLabelFg,"s_scaleLabelFg",V_,2,IV,IV,0,0,0,0,0,0);
  install((PFI)g_scaleLabelFg,"g_scaleLabelFg",IV,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_scaleLabelAlign,"s_scaleLabelAlign",V_,2,IV,A_,0,0,0,0,0,0);
  install((PFI)g_scaleLabelAlign,"g_scaleLabelAlign",A_,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_scaleLabelOut,"s_scaleLabelOut",V_,2,IV,A_,0,0,0,0,0,0);
  install((PFI)g_scaleLabelOut,"g_scaleLabelOut",A_,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_scaleMajorTickSize,"s_scaleMajorTickSize",V_,2,IV,IV,0,0,0,0,0,0);
  install((PFI)g_scaleMajorTickSize,"g_scaleMajorTickSize",IV,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_scaleMinorTickSize,"s_scaleMinorTickSize",V_,2,IV,IV,0,0,0,0,0,0);
  install((PFI)g_scaleMinorTickSize,"g_scaleMinorTickSize",IV,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_scaleMinorTickCt,"s_scaleMinorTickCt",V_,2,IV,IV,0,0,0,0,0,0);
  install((PFI)g_scaleMinorTickCt,"g_scaleMinorTickCt",IV,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_scaleEditorBg,"s_scaleEditorBg",V_,2,IV,IV,0,0,0,0,0,0);
  install((PFI)s_scaleEditorFg,"s_scaleEditorFg",V_,2,IV,IV,0,0,0,0,0,0);
  install((PFI)g_scaleEditorBg,"g_scaleEditorBg",IV,1,IV,0,0,0,0,0,0,0);
  install((PFI)g_scaleEditorFg,"g_scaleEditorFg",IV,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_scaleEdit,"s_scaleEdit",V_,2,IV,IV,0,0,0,0,0,0);
  install((PFI)g_scaleEdit,"g_scaleEdit",IV,1,IV,0,0,0,0,0,0,0);
}

static void installMenuCommands(void)
{
  install((PFI)s_hmenuMnemonics,"s_hmenuMnemonics",V_,2,IV,A_,0,0,0,0,0,0);
  install((PFI)g_hmenuMnemonics,"g_hmenuMnemonics",A_,1,IV,0,0,0,0,0,0,0);
}

static void installChoiceCommands(void)
{
  install((PFI)s_choiceNumCols,"s_choiceNumCols",V_,2,IV,IV,0,0,0,0,0,0);
  install((PFI)g_choiceNumCols,"g_choiceNumCols",IV,1,IV,0,0,0,0,0,0,0);
}

static void installMatrixCommands(void)
{
  install((PFI)s_matrixRowIndex,"s_matrixRowIndex",V_,2,IV,A_,0,0,0,0,0,0);
  install((PFI)g_matrixRowIndex,"g_matrixRowIndex",A_,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_matrixColIndex,"s_matrixColIndex",V_,2,IV,A_,0,0,0,0,0,0);
  install((PFI)g_matrixColIndex,"g_matrixColIndex",A_,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_matrixCornerIndex,"s_matrixCornerIndex",V_,2,IV,IV,0,0,0,0,0,0);
  install((PFI)g_matrixCornerIndex,"g_matrixCornerIndex",IV,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_matrixRowIndexBg,"s_matrixRowIndexBg",V_,2,IV,IV,0,0,0,0,0,0);
  install((PFI)g_matrixRowIndexBg,"g_matrixRowIndexBg",IV,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_matrixColIndexBg,"s_matrixColIndexBg",V_,2,IV,IV,0,0,0,0,0,0);
  install((PFI)g_matrixColIndexBg,"g_matrixColIndexBg",IV,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_matrixCornerIndexBg,"s_matrixCornerIndexBg",V_,2,IV,IV,0,0,0,0,0,0);
  install((PFI)g_matrixCornerIndexBg,"g_matrixCornerIndexBg",IV,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_matrixColSpace,"s_matrixColSpace",V_,2,IV,A_,0,0,0,0,0,0);
  install((PFI)g_matrixColSpace,"g_matrixColSpace",A_,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_matrixNumHeadingRows,"s_matrixNumHeadingRows",V_,2,IV,IV,0,0,0,0,0,0);
  install((PFI)g_matrixNumHeadingRows,"g_matrixNumHeadingRows",IV,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_matrixColSpaceFunc,"s_matrixColSpaceFunc",V_,2,IV,A_,0,0,0,0,0,0);
  install((PFI)g_matrixColSpaceFunc,"g_matrixColSpaceFunc",A_,1,IV,0,0,0,0,0,0,0);
}

static void installTableColumnCommands(void)
{
  install((PFI)s_fieldColumnAlignment, "s_fieldColumnAlignment", V_,2,IV,A_,0,0,0,0,0,0);
  install((PFI)g_fieldColumnAlignment, "g_fieldColumnAlignment", A_,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_fieldCycleFunc,"s_fieldCycleFunc",V_,2,IV,A_,0,0,0,0,0,0);
  install((PFI)g_fieldCycleFunc,"g_fieldCycleFunc",A_,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_fieldCycleColors,"s_fieldCycleColors",V_,2,IV,A_,0,0,0,0,0,0);
  install((PFI)g_fieldCycleColors,"g_fieldCycleColors",A_,1,IV,0,0,0,0,0,0,0);

  // Report Commands
  install((PFI)s_fieldBgGrayScale,"s_fieldBgGrayScale",V_,2,IV,A_,0,0,0,0,0,0);
  install((PFI)g_fieldBgGrayScale,"g_fieldBgGrayScale",A_,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_fieldBgGrayScaleFunc,"s_fieldBgGrayScaleFunc",V_,2,IV,A_,0,0,0,0,0,0);
  install((PFI)g_fieldBgGrayScaleFunc,"g_fieldBgGrayScaleFunc",A_,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_fieldBreakBgGrayScale,"s_fieldBreakBgGrayScale",V_,2,IV,A_,0,0,0,0,0,0);
  install((PFI)g_fieldBreakBgGrayScale,"g_fieldBreakBgGrayScale",A_,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_fieldBreakBgGrayScaleFunc,"s_fieldBreakBgGrayScaleFunc",V_,2,IV,A_,0,0,0,0,0,0);
  install((PFI)g_fieldBreakBgGrayScaleFunc,"g_fieldBreakBgGrayScaleFunc",A_,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_fieldBreakText,"s_fieldBreakText",V_,3,IV,A_,A_,0,0,0,0,0);
  install((PFI)g_fieldBreakText,"g_fieldBreakText",A_,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_fieldBreakFont,"s_fieldBreakFont",V_,2,IV,A_,0,0,0,0,0,0);
  install((PFI)g_fieldBreakFont,"g_fieldBreakFont",A_,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_fieldBreakFontFunc,"s_fieldBreakFontFunc",V_,2,IV,A_,0,0,0,0,0,0);
  install((PFI)g_fieldBreakFontFunc,"g_fieldBreakFontFunc",A_,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_fieldBreakLeading,"s_fieldBreakLeading",V_,2,IV,IV,0,0,0,0,0,0);
  install((PFI)g_fieldBreakLeading,"g_fieldBreakLeading",IV,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_fieldBreakLeadingFunc,"s_fieldBreakLeadingFunc",V_,2,IV,A_,0,0,0,0,0,0);
  install((PFI)g_fieldBreakLeadingFunc,"g_fieldBreakLeadingFunc",A_,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_fieldBreakOffset,"s_fieldBreakOffset",V_,2,IV,IV,0,0,0,0,0,0);
  install((PFI)g_fieldBreakOffset,"g_fieldBreakOffset",IV,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_fieldBreakOffsetFunc,"s_fieldBreakOffsetFunc",V_,2,IV,A_,0,0,0,0,0,0);
  install((PFI)g_fieldBreakOffsetFunc,"g_fieldBreakOffsetFunc",A_,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_fieldBreakOn,"s_fieldBreakOn",V_,2,IV,IV,0,0,0,0,0,0);
  install((PFI)g_fieldBreakOn,"g_fieldBreakOn",IV,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_fieldBreakProcessOn,"s_fieldBreakProcessOn",V_,2,IV,IV,0,0,0,0,0,0);
  install((PFI)g_fieldBreakProcessOn,"g_fieldBreakProcessOn",IV,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_fieldBreakStyle,"s_fieldBreakStyle",V_,2,IV,A_,0,0,0,0,0,0);
  install((PFI)g_fieldBreakStyle,"g_fieldBreakStyle",A_,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_fieldBreakStyleFunc,"s_fieldBreakStyleFunc",V_,2,IV,A_,0,0,0,0,0,0);
  install((PFI)g_fieldBreakStyleFunc,"g_fieldBreakStyleFunc",A_,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_fieldFgGrayScale,"s_fieldFgGrayScale",V_,2,IV,A_,0,0,0,0,0,0);
  install((PFI)g_fieldFgGrayScale,"g_fieldFgGrayScale",A_,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_fieldFgGrayScaleFunc,"s_fieldFgGrayScaleFunc",V_,2,IV,A_,0,0,0,0,0,0);
  install((PFI)g_fieldFgGrayScaleFunc,"g_fieldFgGrayScaleFunc",A_,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_fieldBreakFgGrayScale,"s_fieldBreakFgGrayScale",V_,2,IV,A_,0,0,0,0,0,0);
  install((PFI)g_fieldBreakFgGrayScale,"g_fieldBreakFgGrayScale",A_,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_fieldBreakFgGrayScaleFunc,"s_fieldBreakFgGrayScaleFunc",V_,2,IV,A_,0,0,0,0,0,0);
  install((PFI)g_fieldBreakFgGrayScaleFunc,"g_fieldBreakFgGrayScaleFunc",A_,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_fieldBreakProcessFunc,"s_fieldBreakProcessFunc",V_,2,IV,A_,0,0,0,0,0,0);
  install((PFI)g_fieldBreakProcessFunc,"g_fieldBreakProcessFunc",A_,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_fieldFormatBreakFunc,"s_fieldFormatBreakFunc",V_,2,IV,A_,0,0,0,0,0,0);
  install((PFI)g_fieldFormatBreakFunc,"g_fieldFormatBreakFunc",A_,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_fieldHeadingStyle,"s_fieldHeadingStyle",V_,2,IV,A_,0,0,0,0,0,0);
  install((PFI)g_fieldHeadingStyle,"g_fieldHeadingStyle",A_,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_fieldHeadingFgGrayScale,"s_fieldHeadingFgGrayScale",V_,2,IV,A_,0,0,0,0,0,0);
  install((PFI)g_fieldHeadingFgGrayScale,"g_fieldHeadingFgGrayScale",A_,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_fieldHeadingBgGrayScale,"s_fieldHeadingBgGrayScale",V_,2,IV,A_,0,0,0,0,0,0);
  install((PFI)g_fieldHeadingBgGrayScale,"g_fieldHeadingBgGrayScale",A_,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_fieldPageBreakOn,"s_fieldPageBreakOn",V_,2,IV,IV,0,0,0,0,0,0);
  install((PFI)g_fieldPageBreakOn,"g_fieldPageBreakOn",IV,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_fieldReportFont,"s_fieldReportFont",V_,2,IV,A_,0,0,0,0,0,0);
  install((PFI)g_fieldReportFont,"g_fieldReportFont",A_,1,IV,0,0,0,0,0,0,0);
  install((PFI)g_fieldReportFontFunc,"g_fieldReportFontFunc",A_,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_fieldReportFontFunc,"s_fieldReportFontFunc",V_,2,IV,A_,0,0,0,0,0,0);
  install((PFI)s_fieldReportHeadingFont,"s_fieldReportHeadingFont",V_,2,IV,A_,0,0,0,0,0,0);
  install((PFI)g_fieldReportHeadingFont,"g_fieldReportHeadingFont",A_,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_fieldStyle,"s_fieldStyle",V_,2,IV,A_,0,0,0,0,0,0);
  install((PFI)g_fieldStyle,"g_fieldStyle",A_,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_fieldStyleFunc,"s_fieldStyleFunc",V_,2,IV,A_,0,0,0,0,0,0);
  install((PFI)g_fieldStyleFunc,"g_fieldStyleFunc",A_,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_fieldSuppressDuplicate,"s_fieldSuppressDuplicate",V_,2,IV,IV,0,0,0,0,0,0);
  install((PFI)g_fieldSuppressDuplicate,"g_fieldSuppressDuplicate",IV,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_fieldComputationMode,"s_fieldComputationMode",V_,2,IV,A_,0,0,0,0,0,0);
  install((PFI)g_fieldComputationMode,"g_fieldComputationMode",A_,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_fieldHeading,"s_fieldHeading",V_,2,IV,A_,0,0,0,0,0,0);
  install((PFI)g_fieldHeading,"g_fieldHeading",A_,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_fieldHeadingFg,"s_fieldHeadingFg",V_,2,IV,IV,0,0,0,0,0,0);
  install((PFI)g_fieldHeadingFg,"g_fieldHeadingFg",IV,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_fieldBreakCriteriaFunc,"s_fieldBreakCriteriaFunc",V_,2,IV,A_,0,0,0,0,0,0);
  install((PFI)g_fieldBreakCriteriaFunc, "g_fieldBreakCriteriaFunc",A_,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_fieldBreakString,"s_fieldBreakString",V_,2,IV,A_,0,0,0,0,0,0);
  install((PFI)g_fieldBreakString,"g_fieldBreakString",A_,1,IV,0,0,0,0,0,0,0);
  install((PFI)g_fieldBreakIndex,"g_fieldBreakIndex",A_,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_fieldBreakFg,"s_fieldBreakFg",V_,2,IV,A_,0,0,0,0,0,0);
  install((PFI)g_fieldBreakFg,"g_fieldBreakFg",A_,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_fieldBreakBg,"s_fieldBreakBg",V_,2,IV,A_,0,0,0,0,0,0);
  install((PFI)g_fieldBreakBg,"g_fieldBreakBg",A_,1,IV,0,0,0,0,0,0,0);
}

static void installTableCommands(void)
{
  install((PFI)s_tableNumFixedFields,"s_tableNumFixedFields",V_,2,IV,IV,0,0,0,0,0,0);
  install((PFI)g_tableNumFixedFields,"g_tableNumFixedFields",IV,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_tableColumnDragDrop, "s_tableColumnDragDrop", V_,2,IV,IV,0,0,0,0,0,0);
  install((PFI)s_tableColumnResize, "s_tableColumnResize", V_,2,IV,IV,0,0,0,0,0,0);
  install((PFI)g_tableColumnDragDrop, "g_tableColumnDragDrop",IV,1,IV,0,0,0,0,0,0,0);
  install((PFI)g_tableColumnResize, "g_tableColumnResize", IV,1,IV,0,0,0,0,0,0,0);

  // Report Commands
  install((PFI)s_tableBreakFont, "s_tableBreakFont",V_,2,IV,A_,0,0,0,0,0,0);
  install((PFI)g_tableBreakFont,"g_tableBreakFont",A_,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_tableBreakStyle,"s_tableBreakStyle",V_,2,IV,A_,0,0,0,0,0,0);
  install((PFI)g_tableBreakStyle,"g_tableBreakStyle",A_,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_tableFixedReportColumns,"s_tableFixedReportColumns",V_,2,IV,IV,0,0,0,0,0,0);
  install((PFI)g_tableFixedReportColumns,"g_tableFixedReportColumns",IV,1,IV,0,0,0,0,0,0,0);

  install((PFI)s_tableGroupHeading,"s_tableGroupHeading",V_,2,IV,A_,0,0,0,0,0,0);
  install((PFI)g_tableGroupHeading,"g_tableGroupHeading",A_,1,IV,0,0,0,0,0,0,0);

  install((PFI)g_tableGrandTotal,"g_tableGrandTotal",IV,1,IV,0,0,0,0,0,0,0);
  install((PFI)g_tableCurrentBreakColumn, "g_tableCurrentBreakColumn",IV,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_tableGrandTotalText,"s_tableGrandTotalText",V_,3,IV,A_,A_,0,0,0,0,0);
  install((PFI)g_tableGrandTotalText,"g_tableGrandTotalText",A_,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_tableHeadingStyle,"s_tableHeadingStyle",V_,2,IV,A_,0,0,0,0,0,0);
  install((PFI)g_tableHeadingStyle,"g_tableHeadingStyle",A_,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_tableReportFont,"s_tableReportFont",V_,2,IV,A_,0,0,0,0,0,0);
  install((PFI)g_tableReportFont,"g_tableReportFont",A_,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_tableReportGrandTotalOn,"s_tableReportGrandTotalOn",V_,2,IV,IV,0,0,0,0,0,0);
  install((PFI)g_tableReportGrandTotalOn,"g_tableReportGrandTotalOn",IV,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_tableReportHeadingFont,"s_tableReportHeadingFont",V_,2,IV,A_,0,0,0,0,0,0);
  install((PFI)g_tableReportHeadingFont,"g_tableReportHeadingFont",A_,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_tableReportTotalOn,"s_tableReportTotalOn",V_,2,IV,IV,0,0,0,0,0,0);
  install((PFI)g_tableReportTotalOn,"g_tableReportTotalOn",IV,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_tableReportTotalFont,"s_tableReportTotalFont",V_,2,IV,A_,0,0,0,0,0,0);
  install((PFI)g_tableReportTotalFont,"g_tableReportTotalFont",A_,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_tableReportTotalLeading,"s_tableReportTotalLeading",V_,2,IV,IV,0,0,0,0,0,0);
  install((PFI)g_tableReportTotalLeading,"g_tableReportTotalLeading",IV,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_tableReportTotalStyle,"s_tableReportTotalStyle",V_,2,IV,A_,0,0,0,0,0,0);
  install((PFI)g_tableReportTotalStyle,"g_tableReportTotalStyle",A_,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_tableStyle,"s_tableStyle",V_,2,IV,A_,0,0,0,0,0,0);
  install((PFI)g_tableStyle,"g_tableStyle",A_,1,IV,0,0,0,0,0,0,0);
  install((PFI)g_tableColumnControl,"g_tableColumnControl",A_,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_tableColumnControl,"s_tableColumnControl",V_,2,IV,A_,0,0,0,0,0,0);
  install((PFI)g_tableColumnPageSpan,"g_tableColumnPageSpan",IV,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_tableColumnPageSpan,"s_tableColumnPageSpan",V_,2,IV,IV,0,0,0,0,0,0);
  install((PFI)g_tableRowControl,"g_tableRowControl",A_,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_tableRowControl,"s_tableRowControl",V_,2,IV,A_,0,0,0,0,0,0);
  install((PFI)g_tableRowPageSpan,"g_tableRowPageSpan",IV,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_tableRowPageSpan,"s_tableRowPageSpan",V_,2,IV,IV,0,0,0,0,0,0);
  install((PFI)s_tableNewspaperColumn,"s_tableNewspaperColumn",V_,2,IV,IV,0,0,0,0,0,0);
  install((PFI)g_tableNewspaperColumn,"g_tableNewspaperColumn",IV,1,IV,0,0,0,0,0,0,0);
  install((PFI)g_tableColumnSpacing,"g_tableColumnSpacing",A_,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_tableColumnSpacing,"s_tableColumnSpacing",V_,2,IV,A_,0,0,0,0,0,0);

  install((PFI)g_tableFrameLineWidth,"g_tableFrameLineWidth",IV,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_tableFrameLineWidth,"s_tableFrameLineWidth",V_,2,IV,IV,0,0,0,0,0,0);
  install((PFI)g_tableFrameOffset,"g_tableFrameOffset",IV,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_tableFrameOffset,"s_tableFrameOffset",V_,2,IV,IV,0,0,0,0,0,0);
  install((PFI)g_tableFrameStyle,"g_tableFrameStyle",A_,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_tableFrameStyle,"s_tableFrameStyle",V_,2,IV,A_,0,0,0,0,0,0);

  install((PFI)g_tableLeading,"g_tableLeading",A_,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_tableLeading,"s_tableLeading",V_,2,IV,A_,0,0,0,0,0,0);
  install((PFI)g_tableOutputStyle,"g_tableOutputStyle",A_,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_tableOutputStyle,"s_tableOutputStyle",V_,2,IV,A_,0,0,0,0,0,0);

  install((PFI)s_tableLeftMargin,"s_tableLeftMargin",V_,2,IV,A_,0,0,0,0,0,0);
  install((PFI)g_tableLeftMargin,"g_tableLeftMargin",A_,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_tableRightMargin,"s_tableRightMargin",V_,2,IV,A_,0,0,0,0,0,0);
  install((PFI)g_tableRightMargin,"g_tableRightMargin",A_,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_tableTopOffset,"s_tableTopOffset",V_,2,IV,A_,0,0,0,0,0,0);
  install((PFI)g_tableTopOffset,"g_tableTopOffset",A_,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_tableBottomOffset,"s_tableBottomOffset",V_,2,IV,A_,0,0,0,0,0,0);
  install((PFI)g_tableBottomOffset,"g_tableBottomOffset",A_,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_tablePrintRow,"s_tablePrintRow",V_,2,IV,IV,0,0,0,0,0,0);
  install((PFI)g_tablePrintRow,"g_tablePrintRow",IV,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_tablePrintColumn,"s_tablePrintColumn",V_,2,IV,IV,0,0,0,0,0,0);
  install((PFI)g_tablePrintColumn,"g_tablePrintColumn",IV,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_tableJustify,"s_tableJustify",V_,2,IV,A_,0,0,0,0,0,0);
  install((PFI)g_tableJustify,"g_tableJustify",A_,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_tablePageAlign,"s_tablePageAlign",V_,2,IV,A_,0,0,0,0,0,0);
  install((PFI)g_tablePageAlign,"g_tablePageAlign",A_,1,IV,0,0,0,0,0,0,0);
  install((PFI)g_tableBreakTextColumn,"g_tableBreakTextColumn",A_,1,IV,0,0,0,0,0,0,0);
  install((PFI)g_tableBreakColumn,"g_tableBreakColumn",A_,1,IV,0,0,0,0,0,0,0);
  install((PFI)g_tableBreakIndex,"g_tableBreakIndex",A_,1,IV,0,0,0,0,0,0,0);
  install((PFI)g_tablePageBreakIndex,"g_tablePageBreakIndex",A_,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_tableFgGrayScale,"s_tableFgGrayScale",V_,2,IV,A_,0,0,0,0,0,0);
  install((PFI)g_tableFgGrayScale,"g_tableFgGrayScale",A_,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_tableBgGrayScale,"s_tableBgGrayScale",V_,2,IV,A_,0,0,0,0,0,0);
  install((PFI)g_tableBgGrayScale,"g_tableBgGrayScale",A_,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_tableHeadingFgGrayScale,"s_tableHeadingFgGrayScale",V_,2,IV,A_,0,0,0,0,0,0);
  install((PFI)g_tableHeadingFgGrayScale,"g_tableHeadingFgGrayScale",A_,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_tableHeadingBgGrayScale,"s_tableHeadingBgGrayScale",V_,2,IV,A_,0,0,0,0,0,0);
  install((PFI)g_tableHeadingBgGrayScale,"g_tableHeadingBgGrayScale",A_,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_tableBreakOffset,"s_tableBreakOffset",V_,2,IV,IV,0,0,0,0,0,0);
  install((PFI)g_tableBreakOffset,"g_tableBreakOffset",IV,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_tableBreakLeading,"s_tableBreakLeading",V_,2,IV,IV,0,0,0,0,0,0);
  install((PFI)g_tableBreakLeading,"g_tableBreakLeading",IV,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_tableReportHeadingOffset,"s_tableReportHeadingOffset",V_,2,IV,IV,0,0,0,0,0,0);
  install((PFI)g_tableReportHeadingOffset,"g_tableReportHeadingOffset",IV,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_tableComputeBreaks,"s_tableComputeBreaks",V_,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_tableDynamicRecompute,"s_tableDynamicRecompute",V_,2,IV,IV,0,0,0,0,0,0);
  install((PFI)g_tableDynamicRecompute,"g_tableDynamicRecompute",IV,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_tableShowBreaks,"s_tableShowBreaks",V_,2,IV,IV,0,0,0,0,0,0);
  install((PFI)g_tableShowBreaks,"g_tableShowBreaks",IV,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_tableHeadingFg,"s_tableHeadingFg",V_,2,IV,IV,0,0,0,0,0,0);
  install((PFI)g_tableHeadingFg,"g_tableHeadingFg",IV,1,IV,0,0,0,0,0,0,0);
  install((PFI)g_tableDelimiter,"g_tableDelimiter",A_,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_tableDelimiter,"s_tableDelimiter",V_,2,IV,A_,0,0,0,0,0,0);
  //  install((PFI)g_tableBreakTextIndex,"g_tableBreakTextIndex",A_,1,IV,0,0,0,0,0,0,0);
}

static void installScrolledWindowCommands(void)
{
  install((PFI)s_windowVsbBg,"s_windowVsbBg",V_,2,IV,IV,0,0,0,0,0,0);
  install((PFI)s_windowHsbBg,"s_windowHsbBg",V_,2,IV,IV,0,0,0,0,0,0);
  install((PFI)g_windowVsbBg,"g_windowVsbBg",IV,1,IV,0,0,0,0,0,0,0);
  install((PFI)g_windowHsbBg,"g_windowHsbBg",IV,1,IV,0,0,0,0,0,0,0);
  install((PFI)g_windowSbSize,"g_windowSbSize",IV,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_windowSbSize,"s_windowSbSize",V_,2,IV,IV,0,0,0,0,0,0);
}

static void installButtonCommands(void)
{
  install((PFI)s_buttonMargin,"s_buttonMargin",V_,2,IV,IV,0,0,0,0,0,0);
  install((PFI)g_buttonMargin,"g_buttonMargin",IV,1,IV,0,0,0,0,0,0,0);
}

static void installLabelCommands(void)
{
  install((PFI)s_labelMargin,"s_labelMargin",V_,2,IV,IV,0,0,0,0,0,0);
  install((PFI)g_labelMargin,"g_labelMargin",IV,1,IV,0,0,0,0,0,0,0);
}

static void installScalarCommands(void)
{
  install((PFI)s_scalarEditorBg,"s_scalarEditorBg",V_,2,IV,IV,0,0,0,0,0,0);
  install((PFI)s_scalarEditorFg,"s_scalarEditorFg",V_,2,IV,IV,0,0,0,0,0,0);
  install((PFI)g_scalarEditorBg,"g_scalarEditorBg",IV,1,IV,0,0,0,0,0,0,0);
  install((PFI)g_scalarEditorFg,"g_scalarEditorFg",IV,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_scalarEdit,"s_scalarEdit",V_,2,IV,IV,0,0,0,0,0,0);
  install((PFI)g_scalarEdit,"g_scalarEdit",IV,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_scalarArrowButtons, "s_scalarArrowButtons",V_,2,IV,IV,0,0,0,0,0,0);
  install((PFI)g_scalarArrowButtons, "g_scalarArrowButtons",IV,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_scalarCycleFunc,"s_scalarCycleFunc",V_,2,IV,A_,0,0,0,0,0,0);
  install((PFI)g_scalarCycleFunc,"g_scalarCycleFunc",A_,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_scalarCycleColors,"s_scalarCycleColors",V_,2,IV,A_,0,0,0,0,0,0);
  install((PFI)g_scalarCycleColors,"g_scalarCycleColors", A_,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_scalarCycleInterval,"s_scalarCycleInterval",V_,2,IV,IV,0,0,0,0,0,0);
  install((PFI)g_scalarCycleInterval,"g_scalarCycleInterval",IV,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_scalarValueBg,"s_scalarValueBg",V_,2,IV,IV,0,0,0,0,0,0);	
  install((PFI)g_scalarValueBg,"g_scalarValueBg",IV,1,IV,0,0,0,0,0,0,0);	
  install((PFI)s_scalarValueFont,"s_scalarValueFont",V_,2,IV,IV,0,0,0,0,0,0);	
  install((PFI)g_scalarValueFont,"g_scalarValueFont",IV,1,IV,0,0,0,0,0,0,0);	
  install((PFI)s_scalarValueAlign,"s_scalarValueAlign",V_,2,IV,A_,0,0,0,0,0,0);
  install((PFI)g_scalarValueAlign,"g_scalarValueAlign",A_,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_scalarValueShadowStyle,"s_scalarValueShadowStyle",V_,2,IV,A_,0,0,0,0,0,0);	
  install((PFI)g_scalarValueShadowStyle,"g_scalarValueShadowStyle",A_,1,IV,0,0,0,0,0,0,0);	
  install((PFI)s_scalarValueShadowThickness,"s_scalarValueShadowThickness",V_,2,IV,IV,0,0,0,0,0,0);	
  install((PFI)g_scalarValueShadowThickness,"g_scalarValueShadowThickness",IV,1,IV,0,0,0,0,0,0,0);	

// cycleColorMode() method does not yet exist for MSEntryField, but will.
//  install((PFI)s_scalarCycleColorMode,"s_scalarCycleColorMode",V_,2,IV,A_,0,0,0,0,0,0);
//  install((PFI)g_scalarCycleColorMode,"g_scalarCycleColorMode",A_,1,IV,0,0,0,0,0,0,0);
}

static void installSlotCommands(void)
{
  install((PFI)s_slotCycleFunc,"s_slotCycleFunc",V_,2,IV,A_,0,0,0,0,0,0);
  install((PFI)g_slotCycleFunc,"g_slotCycleFunc",A_,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_slotCycleColors,"s_slotCycleColors",V_,2,IV,A_,0,0,0,0,0,0);
  install((PFI)g_slotCycleColors,"g_slotCycleColors",A_,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_slotCycleInterval,"s_slotCycleInterval",V_,2,IV,IV,0,0,0,0,0,0);
  install((PFI)g_slotCycleInterval,"g_slotCycleInterval",IV,1,IV,0,0,0,0,0,0,0);
// cycleColorMode() method does not yet exist for MSEntryField, but will.
//  install((PFI)s_slotCycleColorMode,"s_slotCycleColorMode",V_,2,IV,A_,0,0,0,0,0,0);
//  install((PFI)g_slotCycleColorMode,"g_slotCycleColorMode",A_,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_slotEditorBg,"s_slotEditorBg",V_,2,IV,IV,0,0,0,0,0,0);
  install((PFI)s_slotEditorFg,"s_slotEditorFg",V_,2,IV,IV,0,0,0,0,0,0);
  install((PFI)g_slotEditorBg,"g_slotEditorBg",IV,1,IV,0,0,0,0,0,0,0);
  install((PFI)g_slotEditorFg,"g_slotEditorFg",IV,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_slotEdit,"s_slotEdit",V_,2,IV,IV,0,0,0,0,0,0);
  install((PFI)g_slotEdit,"g_slotEdit",IV,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_slotSelected,"s_slotSelected",V_,2,IV,IV,0,0,0,0,0,0);
  install((PFI)g_slotSelected,"g_slotSelected",IV,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_slotHlColor,"s_slotHlColor",V_,2,IV,IV,0,0,0,0,0,0);
  install((PFI)g_slotHlColor,"g_slotHlColor",IV,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_slotHlThickness,"s_slotHlThickness",V_,2,IV,IV,0,0,0,0,0,0);
  install((PFI)g_slotHlThickness,"g_slotHlThickness",IV,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_slotShadowThickness,"s_slotShadowThickness",V_,2,IV,IV,0,0,0,0,0,0);
  install((PFI)g_slotShadowThickness,"g_slotShadowThickness",IV,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_slotArrowButtons,"s_slotArrowButtons",V_,2,IV,A_,0,0,0,0,0,0);
  install((PFI)g_slotArrowButtons,"g_slotArrowButtons",A_,1,IV,0,0,0,0,0,0,0);
}

static void installTopLevelCommands(void)
{
  install((PFI)s_windowName,"s_windowName",V_,2,IV,CP,0,0,0,0,0,0);
  install((PFI)g_windowName,"g_windowName",A_,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_iconName,"s_iconName",V_,2,IV,CP,0,0,0,0,0,0);
  install((PFI)g_iconName,"g_iconName",A_,1,IV,0,0,0,0,0,0,0);
}

static void installCollapsibleCommands(void)
{
  install((PFI)s_handleBg,"s_handleBg",V_,2,IV,IV,0,0,0,0,0,0);
  install((PFI)g_handleBg,"g_handleBg",IV,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_handleSize,"s_handleSize",V_,2,IV,IV,0,0,0,0,0,0);
  install((PFI)g_handleSize,"g_handleSize",IV,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_open,"s_open",V_,2,IV,A_,0,0,0,0,0,0);
  install((PFI)g_open,"g_open",A_,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_toolTip,"s_toolTip",V_,2,IV,A_,0,0,0,0,0,0);
  install((PFI)g_toolTip,"g_toolTip",A_,1,IV,0,0,0,0,0,0,0);
}

static void installLayoutCommands(void)
{
  install((PFI)s_layoutRowSpace,"s_layoutRowSpace",V_,2,IV,A_,0,0,0,0,0,0);
  install((PFI)s_layoutColSpace,"s_layoutColSpace",V_,2,IV,A_,0,0,0,0,0,0);
  install((PFI)g_layoutRowSpace,"g_layoutRowSpace",A_,1,IV,0,0,0,0,0,0,0);
  install((PFI)g_layoutColSpace,"g_layoutColSpace",A_,1,IV,0,0,0,0,0,0,0);
  install((PFI)g_layoutRow,"g_layoutRow",IV,1,IV,0,0,0,0,0,0,0);
  install((PFI)g_layoutCol,"g_layoutCol",IV,1,IV,0,0,0,0,0,0,0);
  install((PFI)g_layoutHspan,"g_layoutHspan",IV,1,IV,0,0,0,0,0,0,0);
  install((PFI)g_layoutVspan,"g_layoutVspan",IV,1,IV,0,0,0,0,0,0,0);
  install((PFI)g_layoutOptions,"g_layoutOptions",IV,1,IV,0,0,0,0,0,0,0);
  install((PFI)g_layoutUniformRows,"g_layoutUniformRows",IV,1,IV,0,0,0,0,0,0,0);
  install((PFI)g_layoutUniformCols,"g_layoutUniformCols",IV,1,IV,0,0,0,0,0,0,0);
  install((PFI)g_layoutLockSize,"g_layoutLockSize",IV,1,IV,0,0,0,0,0,0,0);
  install((PFI)g_layoutLockPositions,"g_layoutLockPositions",IV,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_layoutUniformRows,"s_layoutUniformRows",V_,2,IV,IV,0,0,0,0,0,0);
  install((PFI)s_layoutUniformCols,"s_layoutUniformCols",V_,2,IV,IV,0,0,0,0,0,0);
  install((PFI)s_layoutLockSize,"s_layoutLockSize",V_,2,IV,IV,0,0,0,0,0,0);
  install((PFI)s_layoutLockPositions,"s_layoutLockPositions",V_,2,IV,IV,0,0,0,0,0,0);
  install((PFI)s_layoutMargin,"s_layoutMargin",V_,2,IV,IV,0,0,0,0,0,0);
  install((PFI)g_layoutMargin,"g_layoutMargin",IV,1,IV,0,0,0,0,0,0,0);
}

static void installNoteBookCommands(void)
{
  install((PFI)g_notebookOrientation,"g_notebookOrientation",A_,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_notebookOrientation,"s_notebookOrientation",V_,2,IV,A_,0,0,0,0,0,0);
  install((PFI)g_notebookShowBinding,"g_notebookShowBinding",IV,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_notebookShowBinding,"s_notebookShowBinding",V_,2,IV,IV,0,0,0,0,0,0);
  install((PFI)g_notebookBindingWidth,"g_notebookBindingWidth",IV,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_notebookBindingWidth,"s_notebookBindingWidth",V_,2,IV,IV,0,0,0,0,0,0);
  install((PFI)g_notebookFrameThickness,"g_notebookFrameThickness",IV,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_notebookFrameThickness,"s_notebookFrameThickness",V_,2,IV,IV,0,0,0,0,0,0);
  install((PFI)g_notebookMarginHeight,"g_notebookMarginHeight",IV,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_notebookMarginHeight,"s_notebookMarginHeight",V_,2,IV,IV,0,0,0,0,0,0);
  install((PFI)g_notebookMarginWidth,"g_notebookMarginWidth",IV,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_notebookMarginWidth,"s_notebookMarginWidth",V_,2,IV,IV,0,0,0,0,0,0);
  install((PFI)g_notebookBackpages,"g_notebookBackpages",IV,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_notebookBackpages,"s_notebookBackpages",V_,2,IV,IV,0,0,0,0,0,0);
  install((PFI)g_notebookBackpageThickness,"g_notebookBackpageThickness",IV,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_notebookBackpageThickness,"s_notebookBackpageThickness",V_,2,IV,IV,0,0,0,0,0,0);
  install((PFI)g_notebookBackpageFg,"g_notebookBackpageFg",IV,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_notebookBackpageFg,"s_notebookBackpageFg",V_,2,IV,IV,0,0,0,0,0,0);
  install((PFI)g_notebookBackpageBg,"g_notebookBackpageBg",IV,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_notebookBackpageBg,"s_notebookBackpageBg",V_,2,IV,IV,0,0,0,0,0,0);
  install((PFI)g_notebookFrameBackground,"g_notebookFrameBackground",IV,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_notebookFrameBackground,"s_notebookFrameBackground",V_,2,IV,IV,0,0,0,0,0,0);
  install((PFI)g_notebookCurrentPage,"g_notebookCurrentPage",A_,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_notebookCurrentPage,"s_notebookCurrentPage",V_,2,IV,A_,0,0,0,0,0,0);
  install((PFI)g_notebookPageTitle,"g_notebookPageTitle",CP,2,IV,IV,0,0,0,0,0,0);
  install((PFI)s_notebookPageTitle,"s_notebookPageTitle",V_,3,IV,IV,A_,0,0,0,0,0);
  install((PFI)g_notebookPageSelection,"g_notebookPageSelection",IV,2,IV,IV,0,0,0,0,0,0);
  install((PFI)s_notebookPageSelection,"s_notebookPageSelection",V_,3,IV,IV,IV,0,0,0,0,0);
  install((PFI)g_notebookShowTabs,"g_notebookShowTabs",IV,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_notebookShowTabs,"s_notebookShowTabs",V_,2,IV,IV,0,0,0,0,0,0);
  install((PFI)g_notebookShowPopup,"g_notebookShowPopup",IV,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_notebookShowPopup,"s_notebookShowPopup",V_,2,IV,IV,0,0,0,0,0,0);
  install((PFI)g_notebookBorderWidth,"g_notebookBorderWidth",IV,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_notebookBorderWidth,"s_notebookBorderWidth",V_,2,IV,IV,0,0,0,0,0,0);
  install((PFI)g_notebookBorderHeight,"g_notebookBorderHeight",IV,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_notebookBorderHeight,"s_notebookBorderHeight",V_,2,IV,IV,0,0,0,0,0,0);
  install((PFI)g_notebookSelectedPageFg,"g_notebookSelectedPageFg",IV,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_notebookSelectedPageFg,"s_notebookSelectedPageFg",V_,2,IV,IV,0,0,0,0,0,0);
  install((PFI)g_notebookSelectedPageBg,"g_notebookSelectedPageBg",IV,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_notebookSelectedPageBg,"s_notebookSelectedPageBg",V_,2,IV,IV,0,0,0,0,0,0);
  install((PFI)g_notebookLockSize,"g_notebookLockSize",IV,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_notebookLockSize,"s_notebookLockSize",V_,2,IV,IV,0,0,0,0,0,0);
  install((PFI)bNotebookPermute,"bNotebookPermute",V_,2,IV,A_,0,0,0,0,0,0);
}

void AGIFmstkInstall(void)
{
  CX context=Cx;
  Cx=cx("s");

  installC_Commands();
  installTextCommands();
  installWidgetCommands();
  installPasswordCommands();
  installTreeCommands();
  installPageCommands();
  installArrayCommands();
  installPrimitiveTextCommands();
  installButtonBoxCommands();
  installCheckBoxCommands();
  installRadioBoxCommands();
  installDisplayServerCommands();
  installMatrixCommands();
  installManagerCommands();
  installScaleCommands();
  installMenuCommands();
  installChoiceCommands();
  installTableColumnCommands();
  installTableCommands();
  installScrolledWindowCommands();
  installButtonCommands();
  installLabelCommands();
  installScalarCommands();
  installSlotCommands();
  installTopLevelCommands();
  installLayoutCommands();
  installNoteBookCommands();
  installCommandCommands();
  installCollapsibleCommands();
  Cx=context;
}



