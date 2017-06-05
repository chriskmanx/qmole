///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////

#include <MSGUI/MSToolTip.H>
#include <MSGUI/MSPixmap.H>
#include <MSIPC/MSIntervalTimer.H>
#if HAVE_IOSTREAM
#include <iostream>
#else
#include <iostream.h>
#endif

const char * const MSToolTipDefaultForeground="black";
const char * const MSToolTipDefaultBackground="yellow";
unsigned MSToolTipDefaultMarginHeightForBubble=8;
unsigned MSToolTipDefaultMarginWidthForBubble=0;
unsigned MSToolTipDefaultMarginHeightForBox=3;
unsigned MSToolTipDefaultMarginWidthForBox=3;
unsigned long MSToolTipDefaultDelay=500;

MSBoolean MSToolTip::_displayToolTip(MSTrue);

class ToolTipTimer : public MSIntervalTimer
{
private:
  MSToolTip *_toolTip;
public:
  ToolTipTimer(unsigned long,MSToolTip*);
  ~ToolTipTimer(void);  
  virtual void process(void);
};


MSToolTip::MSToolTip(MSDisplayServer *server_)
: MSWidget(server_)
{
  _displayFor=0;
  _tailPosition=Top|Left;
  _style=Bubble;
  _fg=server()->pixel(MSToolTipDefaultForeground);
  _bg=server()->pixel(MSToolTipDefaultBackground);
  _marginHeight=MSToolTipDefaultMarginHeightForBubble;
  _marginWidth=MSToolTipDefaultMarginWidthForBubble;
  _bubbleMarginWidth=0;
  _bubbleMarginHeight=0;
  _delay=MSToolTipDefaultDelay;
  _timer=new ToolTipTimer(delay(),this);
  timer()->stop();
  _fontObject.fontStruct(server()->fontStruct(font()));
  _eventMask=ExposureMask;

  XSetWindowAttributes attributes;
  unsigned long valueMask=CWOverrideRedirect|CWSaveUnder|CWBackPixmap|CWEventMask;
  attributes.save_under=True;
  attributes.override_redirect=True;
  attributes.background_pixmap=None;
  attributes.event_mask=ExposureMask;
  _window=XCreateWindow(display(),server()->root(),0,0,1,1,0,
			CopyFromParent,InputOutput,CopyFromParent,valueMask,
			&attributes);
  XGCValues values;
  valueMask=GCFont;
  values.font=font();
  _gc=XCreateGC(display(),window(),valueMask,&values);

  server()->widgetHashTable()->add(window(),this);
}

MSToolTip::~MSToolTip(void)
{
  XFreeGC(display(),gc());
  if (timer()!=0) delete _timer;
}

void MSToolTip::toolTip(const MSStringVector &toolTip_,MSWidget *displayFor_)
{
  _displayFor=displayFor_;
  _toolTip=toolTip_;
  computeSize();
}

void MSToolTip::style(Style style_)
{
  if (_style!=style_)
   {
     _style=style_;
     if (_style==Bubble)
       {
         _marginHeight=MSToolTipDefaultMarginHeightForBubble;
         _marginWidth=MSToolTipDefaultMarginWidthForBubble;
       }
     else
      {
        _marginHeight=MSToolTipDefaultMarginHeightForBox;
        _marginWidth=MSToolTipDefaultMarginWidthForBox;
      }
     computeSize();
   }
}

void MSToolTip::redraw(void)
{
  int startX,startY;
  if (style()==Bubble)
   {
     XPoint points[3];
     if (tailPosition()&Right) points[0].x=width();
     else points[0].x=0;
     if (tailPosition()&Bottom) points[0].y=height();
     else points[0].y=0;

     points[1].x=width()-bubbleMarginWidth();
     points[2].x=bubbleMarginWidth();
     points[1].y=height()/2;
     points[2].y=points[1].y;
  
     XSetForeground(display(),gc(),background());
     XFillPolygon(display(),window(),gc(),points,3,Nonconvex,CoordModeOrigin);
     XFillArc(display(),window(),gc(),0,0,width(),height(),0,360*64);
     startX=bubbleMarginWidth()+marginWidth();
     startY=bubbleMarginHeight()+marginHeight()+fontObject().textAscent();
   }
  else
   {
     XSetForeground(display(),gc(),background());
     XFillRectangle(display(),window(),gc(),0,0,width(),height());
     XSetForeground(display(),gc(),foreground());
     XDrawRectangle(display(),window(),gc(),0,0,width()-1,height()-1);
     startX=marginWidth();
     startY=marginHeight()+fontObject().textAscent();
   }
  XSetForeground(display(),gc(),foreground());
  for (unsigned i=0;i<toolTip().length();i++)
   {
     XDrawString(display(),window(),gc(),startX,startY,toolTip()(i),toolTip()(i).length());
     startY+=fontObject().textHeight();
   }
}

void MSToolTip::map(void)
{
  if (displayToolTip()==MSTrue)
   {
     if (mapped()==MSFalse) timer()->reset();
   }
}

void MSToolTip::unmap(void)
{
  timer()->stop();
  if (mapped()==MSTrue)
   {
     _mapped=MSFalse;
     XUnmapWindow(display(),window());
   }
}

void MSToolTip::computeSize(void)
{
  int oldW=width();
  int oldH=height();
  int newW=0;
  int newH=0;
  for (unsigned i=0;i<toolTip().length();i++)
   {
     int w=fontObject().textWidth(toolTip()(i));
     newW=(w>newW)?w:newW;
     newH+=fontObject().textHeight();
   }
  newW+=(marginWidth()*2);
  newH+=(marginHeight()*2);
  if (style()==Bubble)
   {
     int bubbleW=(int)(newW*1.4142);
     int bubbleH=(int)(newH*1.4142);
     _bubbleMarginWidth=(bubbleW-newW)/2;
     _bubbleMarginHeight=(bubbleH-newH)/2;
     newW=bubbleW;
     newH=bubbleH;
   }
  if (oldW!=newW||oldH!=newH)
   {
     //It is necessary to force an unmap and then remap to get rid of the
     //background because of the transparent background that we're using 
     if (mapped()==MSTrue) XUnmapWindow(display(),window());
     resize(newW,newH);
     if (mapped()==MSTrue) XMapWindow(display(),window());
   }
  else if (mapped()==MSTrue) redraw();
}

void MSToolTip::updateFont(Font)
{
  fontObject().fontStruct(server()->fontStruct(font()));
  XSetFont(display(),gc(),font());
  computeSize();
}

void MSToolTip::updateForeground(unsigned long)
{
  if (mapped()==MSTrue) redraw();
}

void MSToolTip::updateBackground(unsigned long)
{
  XSetWindowBackgroundPixmap(display(),window(),None);
  if (mapped()==MSTrue) redraw();
}

void MSToolTip::delay(unsigned long delay_)
{
  if (_delay!=delay_)
   {
     _delay=delay_;
     if (timer()!=0) delete _timer;
     _timer=new ToolTipTimer(delay(),this);
     timer()->stop();
   }
}

ToolTipTimer::ToolTipTimer(unsigned long interval_,MSToolTip *toolTip_) :
MSIntervalTimer(interval_),
_toolTip(toolTip_)
{}

ToolTipTimer::~ToolTipTimer(void)
{}

void ToolTipTimer::process(void)
{
  _toolTip->_mapped=MSTrue;
  XMapRaised(_toolTip->display(),_toolTip->window());
  stop();
}

MSBoolean MSToolTip::displayToolTip(void)
{ return _displayToolTip; }

void MSToolTip::displayToolTip(MSBoolean displayToolTip_)
{ _displayToolTip=displayToolTip_; }

