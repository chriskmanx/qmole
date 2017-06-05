///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1998-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////

#include <MSGUI/MSArrowButton.H>

const int MSArrowButtonDefaultWidth=10;
const int MSArrowButtonDefaultHeight=10;
const int MSArrowButtonDefaultShadowThickness=2;
const int MSArrowButtonDefaultHighlightThickness=0;
const int MSArrowButtonEventMask=(ExposureMask|ButtonPressMask|ButtonReleaseMask);

MSArrowButton::MSArrowButton(MSWidget *owner_,MSArrow::ArrowType type_)
: MSWidgetCommon(owner_)
{
  _arrow=new MSArrow(this,type_);
  init();
}

MSArrowButton::MSArrowButton(MSWidget *owner_)
: MSWidgetCommon(owner_)
{
  _arrow=new MSArrow(this);
  init();
}

void MSArrowButton::init(void)
{
  _highlightThickness=MSArrowButtonDefaultHighlightThickness;
  _shadowThickness=MSArrowButtonDefaultShadowThickness;
  _shadowStyle=MSSunken;
  _acceptFocus=MSFalse;
  _arrowTimer=0;
  _repeatInterval=0;
  _repeatThreshold=0;
  selectInput(MSArrowButtonEventMask);
}

MSArrowButton::~MSArrowButton(void)
{
  delete _arrow;
  if (arrowTimer()!=0) delete _arrowTimer;
}

void MSArrowButton::configure(void)
{
  int offset=(highlightThickness()+shadowThickness())*2;
  arrow()->resize(width()-offset,height()-offset);
}

void MSArrowButton::computeSize(void)
{
  int offset=highlightThickness()+shadowThickness();
  arrow()->moveTo(offset,offset);
  resize(arrow()->width()+offset*2,arrow()->height()+offset*2);
}

void MSArrowButton::naturalSize(void)
{
  arrow()->resize(MSArrowButtonDefaultWidth,MSArrowButtonDefaultHeight);
  computeSize();
}

void MSArrowButton::firstMapNotify(void)
{
  if (arrow()->width()==0||arrow()->height()==0) naturalSize();
  else computeSize();
}

void MSArrowButton::repeatThreshold(unsigned long repeatThreshold_)
{ _repeatThreshold=repeatThreshold_; }

void MSArrowButton::repeatInterval(unsigned long repeatInterval_)
{_repeatInterval=repeatInterval_;}

void MSArrowButton::activate(void)
{
  if (isProtected()==MSFalse) activateCallback(MSWidgetCallback::activate);
}

void MSArrowButton::buttonPress(const XEvent *event_)
{
  if (isProtected()==MSFalse) buttonPressNotify(this,event_);
}

void MSArrowButton::button1Press(const XEvent *)
{
  arrow()->select(MSTrue);
  activate();
  if (repeatInterval()>0)
   {
     if (arrowTimer()==0) _arrowTimer=new MSArrowTimer(repeatThreshold(),this);
     if (repeatThreshold()==0) arrowTimer()->expirationInterval(repeatInterval());
     else arrowTimer()->expirationInterval(repeatThreshold());
     arrowTimer()->reset();
   }
}

void MSArrowButton::buttonRelease(const XEvent *event_)
{
  buttonReleaseNotify(this,event_);
}

void MSArrowButton::button1Release(const XEvent *)
{
  arrow()->select(MSFalse);
  if (arrowTimer()!=0) arrowTimer()->stop();
}

void MSArrowButton::updateBackground(unsigned long oldBG_)
{
  MSWidgetCommon::updateBackground(oldBG_);
  redraw();
}

void MSArrowButton::type(MSArrow::ArrowType type_)
{
  arrow()->type(type_);
  drawBackground();
  redraw();
}

void MSArrowButton::redraw(void)
{
  if (mapped()==MSTrue)
   {
     drawShadow();
     arrow()->draw();
   }
}

void MSArrowButton::arrowColor(const char *color_)
{ arrowColor(server()->pixel(color_)); }

void MSArrowButton::arrowColor(unsigned long pixel_)
{
  arrow()->color(pixel_);
  arrow()->draw();
}

MSArrowButton::MSArrowTimer::MSArrowTimer(unsigned long msec_,MSArrowButton *owner_)
: MSIntervalTimer(msec_), _owner(owner_)
{}

void MSArrowButton::MSArrowTimer::process(void)
{
  owner()->activate();
  if (owner()->repeatInterval()!=0) expirationInterval(owner()->repeatInterval());
}

void MSArrowButton::set(MSAttrValueList& avList_)
{
  MSWidgetCommon::set(avList_);
  MSIndexVector index;
  for (unsigned i=0;i<avList_.length();i++)
   {
     if (avList_[i].attribute()=="type")
      {
        MSStringVector typeVector("MSArrow::Left\nMSArrow::Right\nMSArrow::Up\nMSArrow::Down");
        int t=typeVector.indexOf(avList_[i].value());
        if(t!=typeVector.length()) type((MSArrow::ArrowType)t);
        index<<i;
      }
     else if(avList_[i].attribute()=="repeatThreshold")
         repeatThreshold(avList_[i].value().asInt()),index<<i;
     else if(avList_[i].attribute()=="repeatInterval")
         repeatInterval(avList_[i].value().asInt()),index<<i;
     else if(avList_[i].attribute()=="arrowColor")
         arrowColor(avList_[i].value()),index<<i;
   }
  avList_.remove(index);

}


MSAttrValueList& MSArrowButton::get(MSAttrValueList& avList_)
{
  MSStringVector typeVector("MSArrow::Left\nMSArrow::Right\nMSArrow::Up\nMSArrow::Down");
  avList_<<MSAttrValue("activate","",MSAttrValue::Callback);
  avList_<<MSAttrValue("type",typeVector(type()),typeVector);
  avList_<<MSAttrValue("repeatThreshold",MSString(repeatThreshold()));
  avList_<<MSAttrValue("repeatInterval",MSString(repeatInterval()));
  avList_<<MSAttrValue("arrowColor",server()->colorName(arrowColor()),MSAttrValue::Color);
  return MSWidgetCommon::get(avList_);
}
