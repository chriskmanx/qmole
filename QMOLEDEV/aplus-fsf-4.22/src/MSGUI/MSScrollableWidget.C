///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////

#include <MSGUI/MSScrollableWidget.H>

// MSScrollableWidget is a composite widget which manages four different widgets:
// a vertical scrollbar, a horizontal scrollbar, a viewport widget, and
// a draw area widget.  The draw area widget is a child of the viewport widget.
// MSScrollableWiget is a subclass of MSWidgetCommon but adds no new public interface.
// It is meant to be subclassed, and the subclass is responsible to control the size of
// the draw area widget as well as its content.
//
// Subclass should overrides the redrawDrawArea() method to redraw its content.
//

const int MSScrollableWidgetDefaultShadowThickness=2;
const int MSScrollableWidgetDefaultHighlightThickness=2;
const int MSScrollableWidgetDefaultNaturalWidth=300;
const int MSScrollableWidgetDefaultNaturalHeight=300;
const int MSScrollableWidgetDefaultScrollBarSize=15;

MSScrollableWidget::Vsb::Vsb(MSWidget *owner_) : MSVScrollBar(owner_)
{
  inc(2);  
  highlightThickness(0);
  acceptFocus(MSFalse);
  width(MSScrollableWidgetDefaultScrollBarSize);
}

MSScrollableWidget::Vsb::~Vsb(void)
{}

void MSScrollableWidget::Vsb::change(void)
{
  scrollableWidget()->vsbChange();
  XFlush(display());
}

void MSScrollableWidget::Vsb::drag(void)
{
  scrollableWidget()->vsbChange();
  server()->dispatch();
}

MSScrollableWidget::Hsb::Hsb(MSWidget *owner_) : MSHScrollBar(owner_)
{
  inc(2);
  highlightThickness(0);
  acceptFocus(MSFalse);  
  height(MSScrollableWidgetDefaultScrollBarSize);
}

MSScrollableWidget::Hsb::~Hsb(void)
{}

void MSScrollableWidget::Hsb::change(void)
{
  scrollableWidget()->hsbChange();
  XFlush(display());
}

void MSScrollableWidget::Hsb::drag(void)
{
  scrollableWidget()->hsbChange();
  server()->dispatch();
}

MSScrollableWidget::ViewPort::ViewPort(MSWidget *owner_) : MSWidgetCommon(owner_)
{
  _shadowThickness=0;
  _highlightThickness=0;
  selectInput(ExposureMask);
}

void MSScrollableWidget::ViewPort::redraw(void)
{scrollableWidget()->redrawViewPort();}

MSScrollableWidget::DrawAreaWidget::DrawAreaWidget(MSWidget *owner_) : MSWidgetCommon(owner_)
{
  _shadowThickness=0;
  _highlightThickness=0;
  selectInput(ExposureMask);
}

void MSScrollableWidget::DrawAreaWidget::redraw(void)
{scrollableWidget()->redrawDrawArea();}
  
MSScrollableWidget::MSScrollableWidget(MSWidget *owner_) : MSWidgetCommon(owner_)
{init();}

MSScrollableWidget::~MSScrollableWidget(void)
{
  vsb()->destroy();
  hsb()->destroy();
  drawAreaWidget()->destroy();
  viewPort()->destroy();
}

void MSScrollableWidget::init(void)
{
  _shadowStyle=MSSunken;
  _shadowThickness=MSScrollableWidgetDefaultShadowThickness;
  _highlightThickness=MSScrollableWidgetDefaultHighlightThickness;
  _vsb=new Vsb(this);
  _hsb=new Hsb(this);
  _viewPort=new ViewPort(this);
  int offset=highlightThickness()+shadowThickness();
  _viewPort->moveTo(offset,offset);
  _drawAreaWidget=new DrawAreaWidget(_viewPort);
  _naturalWidth=MSScrollableWidgetDefaultNaturalWidth;
  _naturalHeight=MSScrollableWidgetDefaultNaturalHeight;
  addToFocusList();
  selectInput(ExposureMask);
}

void MSScrollableWidget::show(void)
{
  if (mapped()==MSFalse)
   {
     drawAreaWidget()->show();
     viewPort()->show();
     map();
   }
}

void MSScrollableWidget::redraw(void)
{
  if (mapped()==MSTrue)
   {
     redrawDrawAreaImmediately();
     redrawViewPortImmediately();
     redrawScrollableWidgetImmediately();
   }
}

void MSScrollableWidget::print(const char *file_)
{
  MSBoolean fileOpen=MSFalse;
  MSBoolean open=MSTrue; 
  if (outputMode()==Draw)
   {
     if (file_!=0) displayPrintFileName(file_);
     if ((open=displayPrintOpen(this))==MSTrue)
      {
	fileOpen=MSTrue;
	outputMode(Print);
	displayPrintXorigin(0);
	displayPrintYorigin(0);
      }
   }
  if (open==MSTrue)
   {
     displayPrintOriginInc(drawAreaWidget());
     redrawDrawAreaImmediately();
     displayPrintOriginDec(drawAreaWidget());

     displayPrintOriginInc(viewPort());
     redrawViewPortImmediately();
     displayPrintOriginDec(viewPort());

     redrawScrollableWidgetImmediately();
     if (vsb()->mapped())
      {
        displayPrintOriginInc(vsb());
        vsb()->redraw();
        displayPrintOriginDec(vsb());
      }
     if (hsb()->mapped())
      {
        displayPrintOriginInc(hsb());
        hsb()->redraw();
        displayPrintOriginDec(hsb());
      }
     if (fileOpen==MSTrue) 
      {
	displayPrintClose();
	outputMode(Draw);
      }
   }
}

void MSScrollableWidget::expose(const XEvent *pEvent_)
{
  if (pEvent_->xexpose.count==0)
   {
     XEvent aEvent;
     while (XCheckWindowEvent(display(),_window,ExposureMask,&aEvent)==True);
     redrawScrollableWidget();
   }
}

void MSScrollableWidget::focusIn(void)  
{ highlight(); }
void MSScrollableWidget::focusOut(void) 
{ unHighlight(); }

void MSScrollableWidget::updateBackground(unsigned long oldBg_)
{
  MSWidgetCommon::updateBackground(oldBg_);
  vsb()->background(background());
  hsb()->background(background());
  viewPort()->background(background());
  drawAreaWidget()->background(background());
  redraw();
}

void MSScrollableWidget::redrawScrollableWidgetImmediately(void)
{
  if (mapped()==MSTrue) redrawScrollableWidget();
}

void MSScrollableWidget::redrawScrollableWidget(void)
{
  if (highlightThickness()>0)
   {
     GC gc=(highlighted()==MSTrue)?highlightGC():backgroundShadowGC();
     drawFlatShadow(window(),MSRect(0,topShadowOffset(),width(),height()-topShadowOffset()),
		    highlightThickness(),gc);
   }
  if (shadowThickness()>0)
   {
     int viewWidth=width()-highlightThickness()*2;
     viewWidth-=(vsb()->mapped()==MSTrue)?vsb()->width():0;
     int viewHeight=height()-highlightThickness()*2;
     viewHeight-=(hsb()->mapped()==MSTrue)?hsb()->height():0;
     drawBevel(window(),MSRect(highlightThickness(),highlightThickness(),viewWidth,viewHeight),shadowStyle(),shadowThickness());
   }
}

void MSScrollableWidget::redrawDrawAreaImmediately(void)
{ if (mapped()==MSTrue) redrawDrawArea(); }

void MSScrollableWidget::redrawDrawArea(void)
{}

void MSScrollableWidget::redrawViewPortImmediately(void)
{ if (mapped()==MSTrue) redrawViewPort(); }

void MSScrollableWidget::redrawViewPort(void)
{}


void MSScrollableWidget::firstMapNotify(void)
{ naturalSize(); }

void MSScrollableWidget::naturalSize(void)
{
  if (firstMap()==MSTrue)
   {
     int w,h;
     int dx,dy,dw,dh;
     drawAreaGeometry(dx,dy,dw,dh);
     int margin=shadowThickness()*2+highlightThickness()*2;
     if (dw<=0||dw>naturalWidth()) w=naturalWidth()+margin;
     else w=dw+margin;
     if (dh<=0||dh>naturalHeight()) h=naturalHeight()+margin;
     else h=dh+margin;
     resize(w,h);
   }
}

void MSScrollableWidget::computeSize(void)
{
  viewPort()->shadowThickness(shadowThickness()); 
  int offset=highlightThickness()+shadowThickness();
  viewPort()->moveTo(offset,offset);
  configure();
}

void MSScrollableWidget::configure(void)
{
  if (firstMap()==MSTrue)
   {
     //First figure out if we will be needing either
     //the horizontal or vertical scroll bars.
     int dx,dy,dw,dh;
     drawAreaGeometry(dx,dy,dw,dh);
     int offset=shadowThickness()+highlightThickness();
     int margin=offset*2;
     int requiredWidth=dw+margin;
     int requiredHeight=dh+margin;
     if (width()>=requiredWidth)
      {
        hsb()->hide();
      }
     else
      {
        hsb()->show();
        requiredHeight+=hsb()->height();
      }
     if (height()>=requiredHeight)
      {
        vsb()->hide();
      }
     else
      {
        vsb()->show(); 
        if (hsb()->mapped()==MSFalse)
         {
           requiredWidth+=vsb()->width();
           if (width()<requiredWidth) hsb()->show();
         }
      }
     //Reposition the draw area if necessary.  We're using 
     //a NorthWest gravity policy here in positioning the draw area,
     //meaning that we're going to preverse the (x,y) location as
     //much as possible
     int xpos=dx;
     int ypos=dy;
     int viewableWidth=width()-margin;
     if (vsb()->mapped()) viewableWidth-=vsb()->width();
     int viewableHeight=height()-margin;
     if (hsb()->mapped()) viewableHeight-=hsb()->height();
     if (hsb()->mapped()==MSTrue)
      {
        if ((dw+dx)<viewableWidth)
         {
           xpos=viewableWidth-dw;
         }
      }
     else xpos=0;
     if (vsb()->mapped()==MSTrue)
      {
        if ((dh+dy)<viewableHeight)
         {
           ypos=viewableHeight-dh;
         }
      }
     else ypos=0;

     //Move the draw area
     moveDrawArea(xpos,ypos);

     //Update the scroll bars
     if (hsb()->mapped()==MSTrue)
      {
        int w=(vsb()->mapped()==MSTrue)?
          width()-vsb()->width()-highlightThickness()*2:
          width()-highlightThickness()*2;
        w=(w>0)?w:1;
        hsb()->width(w);
        hsb()->moveTo(highlightThickness(),height()-highlightThickness()-hsb()->height());
        hsb()->min(0);
        hsb()->max(dw);
        hsb()->viewSize(viewableWidth);
        hsb()->pageInc(viewableWidth);
//        int value;
//        if (dx>=0) value=offset-dx;
//        else value=-dx+offset;
//        hsb()->valueChange(value);
        hsb()->valueChange(-xpos);
      }
     else hsb()->valueChange(0);
     if (vsb()->mapped()==MSTrue)
      {
        int h=(hsb()->mapped()==MSTrue)?height()-hsb()->height()-highlightThickness()*2:height()-highlightThickness()*2;
        h=(h>0)?h:1;
        vsb()->height(h);
        vsb()->moveTo(width()-highlightThickness()-vsb()->width(),highlightThickness());
        vsb()->min(0);
        vsb()->max(dh);
        vsb()->viewSize(viewableHeight);
        vsb()->pageInc(viewableHeight);
//        int value;
//        if (dy>=0) value=offset-dy;
//        else value=-dy+offset;
//        vsb()->valueChange(value);
        vsb()->valueChange(-ypos);        
      }
     else vsb()->valueChange(0);

     // Resize the viewport
     int w=viewableWidth;
     int h=viewableHeight;
     w=(w>0)?w:1;
     h=(h>0)?h:1;
     resizeViewPort(w,h);
   }
}

void MSScrollableWidget::drawAreaXY(int &x_,int &y_)
{
  int offset=highlightThickness()+shadowThickness();
  x_=x_-offset-drawAreaWidget()->x();
  y_=y_-offset-drawAreaWidget()->y();
}

void MSScrollableWidget::viewPortXY(int &x_,int &y_)
{
  int offset=highlightThickness()+shadowThickness();
  x_=x_-offset;
  y_=y_-offset;
}

void MSScrollableWidget::moveDrawArea(int x_,int y_)
{drawAreaWidget()->moveTo(x_,y_);}

void MSScrollableWidget::resizeDrawArea(int w_,int h_)
{
  drawAreaWidget()->resize(w_,h_);
  configure();
}

void MSScrollableWidget::resizeViewPort(int w_,int h_)
{viewPort()->resize(w_,h_);}


void MSScrollableWidget::vsbChange(void)
{moveDrawArea(drawAreaWidget()->x_origin(),vsb()->min()-vsb()->value());}

void MSScrollableWidget::hsbChange(void)
{moveDrawArea(hsb()->min()-hsb()->value(),drawAreaWidget()->y_origin());}

void MSScrollableWidget::drawAreaGeometry(int &x_,int &y_,int &w_,int &h_)
{
  x_=drawAreaWidget()->x();
  y_=drawAreaWidget()->y();
  w_=drawAreaWidget()->width();
  h_=drawAreaWidget()->height();
}
