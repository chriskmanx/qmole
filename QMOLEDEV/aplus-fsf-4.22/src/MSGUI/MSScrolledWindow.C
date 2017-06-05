///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////

#include <MSGUI/MSScrolledWindow.H>

static const int DefaultScrollBarSize=15;
static const int DefaultViewHeight=300;
static const int DefaultViewWidth=300;

MSScrolledWindow::MSScrolledWindow(MSWidget *owner_) : MSManager(owner_)
{
  init();
}

MSScrolledWindow::~MSScrolledWindow(void)
{
  if (hsb()!=0)        safeDestroy(hsb());
  if (vsb()!=0)        safeDestroy(vsb());
  if (clipWindow()!=0) safeDestroy(clipWindow());
  _hsb=0,_vsb=0,_clipWindow=0;
  if (scrollWindow()!=0) safeDestroy(scrollWindow());
  _scrollWindow=0;
}

void MSScrolledWindow::init(void)
{
  _hsb=0,_vsb=0,_clipWindow=0,_scrollWindow=0;
  _shadowThickness=0;    
  _highlightThickness=0; 
  _clipWindow=new MSPrimitive(this);
  clipWindow()->highlightThickness(0);
  clipWindow()->shadowThickness(0);  
  clipWindow()->sensitive(MSFalse);
  clipWindow()->acceptFocus(MSFalse);
//  clipWindow()->map();
  _vsb=new Vsb(this);
  _hsb=new Hsb(this);
  _viewHeight=DefaultViewHeight;
  _viewWidth=DefaultViewWidth;
  _scrollBarDisplayPolicy=Static;
  computeSize();
}

MSScrolledWindow::Hsb::Hsb(MSWidget *owner_) : MSHScrollBar(owner_)
{
  inc(2);
  highlightThickness(0);
  acceptFocus(MSFalse);  
  height(DefaultScrollBarSize);
//  map();
}

MSScrolledWindow::Hsb::~Hsb(void)
{}

MSScrolledWindow::Vsb::Vsb(MSWidget *owner_) : MSVScrollBar(owner_)
{
  inc(2);  
  highlightThickness(0);
  acceptFocus(MSFalse);
  width(DefaultScrollBarSize);
//  map();
}

MSScrolledWindow::Vsb::~Vsb(void)
{}

void MSScrolledWindow::show(void)
{
  if (mapped()==MSFalse)
   {
     if (scrollWindow()!=0) scrollWindow()->show();
     map();
   }
}

// _clipWindow will be zero when this gets called for it
void MSScrolledWindow::childInsert(MSWidget *pWidget_)
{
  if (clipWindow()!=0&&vsb()!=0&&hsb()!=0)
   {
     if (clipWindow()!=pWidget_&&vsb()!=pWidget_&&hsb()!=pWidget_&&scrollWindow()==0)
      {
	
	_scrollWindow=pWidget_;
	placement();
      }
   }
}

void MSScrolledWindow::childRemove(MSWidget *pWidget_)
{
  if (scrollWindow()==pWidget_)
   {
     _scrollWindow=0;
     placement();
   }
}

void MSScrolledWindow::childCreate(MSWidget *pWidget_)  
{ childInsert(pWidget_); }
void MSScrolledWindow::childDestroy(MSWidget *pWidget_) 
{ childRemove(pWidget_); }

void MSScrolledWindow::visibilityObscured(void)
{
  visible(MSFalse);
  if (scrollWindow()!=0) visibilityObscuredNotify(scrollWindow());
}

void MSScrolledWindow::visibilityUnobscured(void)
{
  visible(MSTrue);
  if (scrollWindow()!=0) visibilityUnobscuredNotify(scrollWindow());
}

void MSScrolledWindow::focusIn(void)  
{ highlight(); }
void MSScrolledWindow::focusOut(void) 
{ unHighlight(); }

void MSScrolledWindow::placement(void) 
{
  if (clipWindow()==0||vsb()==0||hsb()==0) return;

  int offset=highlightThickness()+shadowThickness();
  int sH=1;
  int sW=1;
  if (scrollWindow()!=0)
   {
     sW=scrollWindow()->width();
     sH=scrollWindow()->height(); 
     scrollWindow()->moveTo(offset,offset);
   }
  int availableWidth=width()-2*offset;
  int availableHeight=height()-2*offset;
  if (scrollBarDisplayPolicy()==Static)
   {
     clipWindow()->map();
     vsb()->map();
     hsb()->map();
     _viewHeight=availableHeight-hsb()->height();
     _viewWidth=availableWidth-vsb()->width();
   }
  else
   {
     if (sW>availableWidth||sH>availableHeight)
      {
	clipWindow()->map();
	vsb()->map();
	hsb()->map();
	_viewHeight=availableHeight-hsb()->height();
	_viewWidth=availableWidth-vsb()->width();
      }
     else
      {
	clipWindow()->unmap();
	vsb()->unmap();
	hsb()->unmap();
	_viewHeight=availableHeight;
	_viewWidth=availableWidth;
      }
   }

  if (vsb()->mapped())
   {
     int vH=height()-hsb()->height();
     int vW=width()-vsb()->width();
    
     hsb()->moveTo(offset,vH);
     hsb()->min(0);
     hsb()->valueChange(0);

     if (sW-vW>0) hsb()->max(sW);
     else hsb()->max(vW);     

     hsb()->pageInc(vW);
     hsb()->viewSize(vW);
     hsb()->width(vW);
     hsb()->raise();

     vsb()->moveTo(vW,offset);
     vsb()->min(0);
     vsb()->valueChange(0);

     if (sH-vH>0) vsb()->max(sH);
     else vsb()->max(vH);     

     vsb()->pageInc(vH);
     vsb()->viewSize(vH);
     vsb()->height(vH);
     vsb()->raise();

     vsb()->value(0);  
     hsb()->value(0);  

     clipWindow()->resize(vsb()->width(),hsb()->height());
     clipWindow()->moveTo(hsb()->x_origin()+vW,vsb()->y_origin()+vH);
     clipWindow()->raise();
   }
}

void MSScrolledWindow::childConfigure(MSWidget *pWidget_)
{ if (pWidget_==scrollWindow()) placement(); }

void MSScrolledWindow::computeSize(void)
{
  MSBoolean needScrollBar=MSFalse;
  if (clipWindow()!=0&&vsb()!=0&&hsb()!=0)
   {
     int offset=highlightThickness()+shadowThickness();
     int cumWidth=viewWidth();
     int cumHeight=viewHeight();
     if (scrollBarDisplayPolicy()==Static) needScrollBar=MSTrue;
     else
      {
	if (scrollWindow()!=0)
	 {
	   if (viewWidth()<scrollWindow()->width()||viewHeight()<scrollWindow()->height())
	     needScrollBar=MSTrue;
	 }
      }
     // If we need scroll bar and the current policy is AsNeeded, we have to
     // temporarily set the scroll bar policy to Static so the placement()
     // method triggered by the resize() will do the right thing.
     ScrollBarDisplayPolicy policy=scrollBarDisplayPolicy();
     if (needScrollBar==MSTrue)
      {
	if(policy==AsNeeded) _scrollBarDisplayPolicy=Static;
	cumWidth+=vsb()->width();
	cumHeight+=hsb()->height();
      }
     resize(cumWidth+2*offset,cumHeight+2*offset);
     _scrollBarDisplayPolicy=policy;
   }
}

void MSScrolledWindow::updateBackground(unsigned long oldbg_)
{
  MSWidgetCommon::updateBackground(oldbg_); 
  clipWindow()->background(background());
  if (vsb()->background()==oldbg_) vsbBackground(background());
  if (hsb()->background()==oldbg_) hsbBackground(background());
}

void MSScrolledWindow::viewHeight(int height_) 
{ 
  if (viewHeight()!=height_) 
   { 
     _viewHeight=height_; 
     computeSize();
   }
}

void MSScrolledWindow::viewWidth(int width_)  
{ 
  if (viewWidth()!=width_) 
   { 
     _viewWidth=width_; 
     computeSize();
   }
}

void MSScrolledWindow::scrollBarSize(int size_)
{ 
  vsb()->width(size_); 
  hsb()->height(size_); 
  computeSize(); 
}

void MSScrolledWindow::Vsb::change(void)
{
  if (scrolledWindow()->scrollWindow()!=0)
   {
     scrolledWindow()->scrollWindow()->moveTo(scrolledWindow()->scrollWindow()->x_origin(),
					      min()-value());
     XFlush(display());
   }
}

void MSScrolledWindow::Vsb::drag(void)
{
  if (scrolledWindow()->scrollWindow()!=0)
   {
     scrolledWindow()->scrollWindow()->moveTo(scrolledWindow()->scrollWindow()->x_origin(),
					      min()-value());
     server()->dispatch();
   }
}

void MSScrolledWindow::Hsb::change(void)
{
  if (scrolledWindow()->scrollWindow()!=0)
   {
     scrolledWindow()->scrollWindow()->moveTo(min()-value(),
					      scrolledWindow()->scrollWindow()->y_origin());
     XFlush(display());
   }
}

void MSScrolledWindow::Hsb::drag(void)
{
  if (scrolledWindow()->scrollWindow()!=0)
   {
     scrolledWindow()->scrollWindow()->moveTo(min()-value(),
					      scrolledWindow()->scrollWindow()->y_origin());
     server()->dispatch();
   }
}

void MSScrolledWindow::naturalSize(void)
{
  _viewHeight=DefaultViewHeight;
  _viewWidth=DefaultViewWidth;
  computeSize();
  if (scrollWindow()!=0) scrollWindow()->naturalSize();
}

MSWidgetVector MSScrolledWindow::children(void)
{
  MSWidgetVector vector;
  if (scrollWindow()!=0) vector.append(scrollWindow());
  return vector;
}

void MSScrolledWindow::scrollBarDisplayPolicy(ScrollBarDisplayPolicy scrollBarDisplayPolicy_)
{
  if (_scrollBarDisplayPolicy!=scrollBarDisplayPolicy_)
   {
     _scrollBarDisplayPolicy=scrollBarDisplayPolicy_;
     computeSize();
   }
}

void MSScrolledWindow::print(const char *file_)
{
  MSBoolean   fileOpen=MSFalse;

  if (outputMode()==Draw)
   {
     if (file_!=0) displayPrintFileName(file_);
     if (displayPrintOpen(this)==MSTrue) 
      {
	fileOpen=MSTrue;
	outputMode(Print);
	displayPrintXorigin(0);
	displayPrintYorigin(0);
      }
     else return;
   }
  if (mapped()==MSTrue) redraw();
  if(scrollWindow()!=0&&scrollWindow()->mapped()==MSTrue)
   {
     displayPrintOriginInc(scrollWindow());
     scrollWindow()->print();
     displayPrintOriginDec(scrollWindow());
   }
  if (fileOpen==MSTrue) 
   {
     displayPrintClose();
     outputMode(Draw);
   }
}

void MSScrolledWindow::set(MSAttrValueList& avList_)
{
  MSManager::set(avList_);
  MSIndexVector index;
  for (unsigned i=0;i<avList_.length();i++)
   {
     if (avList_[i].attribute()=="viewHeight") viewHeight(avList_[i].value().asInt()),index<<i;  
     else if (avList_[i].attribute()=="viewWidth") viewWidth(avList_[i].value().asInt()) ,index<<i;
     else if (avList_[i].attribute()=="scrollBarDisplayPolicy")
      {
	MSString val=avList_[i].value();
	scrollBarDisplayPolicy(val=="Static"?Static:AsNeeded);
	index<<i;
      }
   }
  avList_.remove(index);
}

MSAttrValueList& MSScrolledWindow::get(MSAttrValueList& avList_)
{
  avList_<<MSAttrValue("viewHeight", MSString(viewHeight()));
  avList_<<MSAttrValue("viewWidth",  MSString(viewWidth()));
  avList_<<MSAttrValue("scrollBarDisplayPolicy",
		       scrollBarDisplayPolicy()==Static?"Static":"AsNeeded",
		       MSStringVector("Static\nAsNeeded"));
  return MSManager::get(avList_);
}

void MSScrolledWindow::pageUp(void) {}
void MSScrolledWindow::pageDown(void) {}
void MSScrolledWindow::home(void) {}
void MSScrolledWindow::end(void) {}










