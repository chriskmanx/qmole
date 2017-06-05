///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////

#include <MSGUI/MSHGauge.H>

extern const int SliderAreaShadowThickness;

MSHGauge::MSHGauge(MSWidget *owner_,const char *title_) : 
MSHScale(owner_,title_)
{ init(); }
MSHGauge::MSHGauge(MSWidget *owner_,const MSStringVector& title_) : 
MSHScale(owner_,title_)
{ init(); }
MSHGauge::MSHGauge(MSWidget *owner_,MSFloat& model_,const char *title_) : 
MSHScale(owner_,model_,title_)
{ init(); }
MSHGauge::MSHGauge(MSWidget *owner_,MSFloat& model_,const MSStringVector& title_) : 
MSHScale(owner_,model_,title_)
{ init(); }
MSHGauge::MSHGauge(MSWidget *owner_,MSInt& model_,const char *title_) : 
MSHScale(owner_,model_,title_)
{ init(); }
MSHGauge::MSHGauge(MSWidget *owner_,MSInt& model_,const MSStringVector& title_) : 
MSHScale(owner_,model_,title_)
{ init(); }

void MSHGauge::init(void)
{
  acceptFocus(MSFalse);
  sensitive(MSFalse);
  _gaugeWidth=0;
  _startValue.unset();
}

MSHGauge::~MSHGauge(void) {}

void MSHGauge::redraw(void)
{
  MSHScale::redraw();
  drawGauge();
}

void MSHGauge::drawSubWindows(void)
{
  if (outputMode()<Print)
   {
     if (valueAlignment()!=MSNone)
      {
        valueWin()->map();
        valueWin()->raise();
      }
     else valueWin()->unmap();
     valueWin()->moveTo(x_org(),y_org()-valueWin()->height());
   }
}

void MSHGauge::update(const MSIndexVector&)
{
  updateSliderSize(currentValue());
}

void MSHGauge::sliderRedrawNotify(void)
{
  drawGauge();
}

void MSHGauge::updateSliderSize(double value_)
{
  if (gaugeWidth()<=slider()->shadowThickness()||value_<=valueMin()||value_>=valueMax()) 
   {
     drawSliderArea();
     drawGauge();
   }
  else updateGauge(value_);
  MSString buffer;
  valueWin()->updateValue(formatValue(buffer,value_));
}

void MSHGauge::drawGauge(void)
{
  if (mapped()==MSTrue)
   {
     int x=sliderAreaRect().x();
     int y=sliderAreaRect().y()+SliderAreaShadowThickness;
     int curValue=valueToPixel(currentValue());
     int startValue;
     if(_startValue.isSet()==MSTrue)
      {
        double sv=_startValue;
        if(sv<valueMin()) sv=valueMin();
	else if(sv>valueMax()) sv=valueMax();
	startValue=valueToPixel(sv);
      }
     else startValue=x+SliderAreaShadowThickness;
     Direction direction=curValue>startValue?Right:Left;
     int w=abs(startValue-curValue); //-slider()->offset();
     int thickness=slider()->shadowThickness();
     int startx=(direction==Right)?startValue:curValue;
     if (w>=thickness&&slider()->height()>thickness*2)
      {
        int width=w-(w>thickness*2?thickness*2:thickness);
        XBFillRectangle(display(),window(),slider()->backgroundShadowGC(),
                        startx+thickness,y+thickness,width,slider()->height()-thickness*2);
      }
     if (w>thickness&&thickness>0)
      {
        drawGaugeShadow(startx,y,w,thickness,direction);
      }
     gaugeWidth(w);
     _direction=direction;
   }
}

void MSHGauge::drawGaugeShadow(int x,int y, int w, int thickness, Direction direction)
{
  // top
  XBFillRectangle(display(),window(),slider()->topShadowGC(),x,y,w,thickness);

  int startx=direction==Right?x:x+w-thickness;

  // left or right
  XBFillRectangle(display(),window(),slider()->topShadowGC(),startx,y,
                  w>thickness?thickness:w,slider()->height());

  // bottom shadow
  if (w>thickness)
   {
     XPoint points[6];
     points[0].x=points[5].x=direction==Right?x:x+w;
     points[4].x=points[3].x=direction==Right?x+w-thickness:x+thickness;
     points[1].x=points[2].x=direction==Right?x+w:x;
     points[0].y=points[1].y=y+slider()->height();
     points[5].y=points[4].y=y+slider()->height()-thickness;
     points[3].y=y+thickness;
     points[2].y=y;
     XBFillPolygon(display(),window(),slider()->bottomShadowGC(),points,6,Nonconvex,CoordModeOrigin);
   }
}


void MSHGauge::updateGauge(double value_)
{
  int x=sliderAreaRect().x();
  int y=sliderAreaRect().y()+SliderAreaShadowThickness;
  int curValue=valueToPixel(value_);
  int startValue;
  if(_startValue.isSet()==MSTrue)
   {
     double sv=_startValue;
     if(sv<valueMin()) sv=valueMin();
     else if(sv>valueMax()) sv=valueMax();
     startValue=valueToPixel(sv);
   }
  else startValue=x+SliderAreaShadowThickness;
  Direction direction=curValue>startValue?Right:Left;
  int w=abs(startValue-curValue); //-slider()->offset();
  int thickness=slider()->shadowThickness();
  int width=abs(gaugeWidth()-w);
  int startx=startValue;
  XPoint points[7];
  
  if (w==gaugeWidth() && direction==_direction) return;
  else if (direction==_direction)
   {
     if (w<gaugeWidth())
      {
        // value is smaller, clear area
        startx=direction==Right?startx+w:startx-gaugeWidth();
        XFillRectangle(display(),window(),selectShadowGC(),startx,y,width,slider()->height());
      }
     else
      {
        // value is bigger, add area
        startx=direction==Right?startx+gaugeWidth()-thickness:startx-w+thickness;
        XFillRectangle(display(),window(),slider()->backgroundShadowGC(),startx,y,width,slider()->height());
      }
     if (w>thickness&&thickness>0) updateGaugeShadow(startx,y,w,thickness,direction);
   }
  else
   {
     drawSliderArea();
     drawGauge();
   }
  gaugeWidth(w);
  _direction=direction;
}

void MSHGauge::updateGaugeShadow(int x,int y, int w, int thickness, Direction direction)
{
  int width=abs(gaugeWidth()-w)+thickness;
  XPoint points[6];
  if (w<gaugeWidth())
   {
     // value is smaller
     points[0].x=points[1].x=direction==Right?x-thickness:x+width;
     points[2].x=points[3].x=direction==Right?x:x+width-thickness;
     points[0].y=points[3].y=y+slider()->height()-thickness;
     points[1].y=y+thickness;
     points[2].y=y;
     XBFillPolygon(display(),window(),slider()->bottomShadowGC(),points,4,Nonconvex,CoordModeOrigin);
   }
  else
   {
     // value is bigger
     XFillRectangle(display(),window(),slider()->topShadowGC(),x,y,width,thickness);
     points[0].x=points[5].x=direction==Right?x:x+width-thickness;
     points[4].x=points[3].x=direction==Right?x+width-thickness:x;
     points[1].x=points[2].x=direction==Right?x+width:x-thickness;
     points[0].y=points[1].y=y+slider()->height();
     points[5].y=points[4].y=y+slider()->height()-thickness;
     points[3].y=y+thickness;
     points[2].y=y;
     XBFillPolygon(display(),window(),slider()->bottomShadowGC(),points,6,Nonconvex,CoordModeOrigin);
   }
}


// #########################################################
// default virtual methods - prevents gratuitous inlining
// #########################################################
void MSHGauge::buttonPress(const XEvent *) {}
void MSHGauge::keyPress(const XEvent *,KeySym,unsigned int,const char *) {}
void MSHGauge::drawSliderEtch(void) {}

void MSHGauge::computeLabelOffset(void)
{ labelOffset(0); }


void MSHGauge::startValue(const MSFloat& value_ )
{
  _startValue=value_;
  drawSliderArea();
  drawGauge();
}

void MSHGauge::set(MSAttrValueList& avList_)
{
  MSScale::set(avList_);
  for (unsigned i=0;i<avList_.length();i++)
   {
     if (avList_[i].attribute()=="startValue")
      {
        MSFloat value;
        if (avList_[i].value().length()==0) startValue(value);
	else
	 {
	   if (value.set(avList_[i].value())==MSError::MSSuccess) startValue(value);
	 }
      }
   }
}

MSAttrValueList& MSHGauge::get(MSAttrValueList& avList_)
{
  if (_startValue.isSet()==MSTrue)
   {
     avList_<<MSAttrValue("startValue",_startValue.asString());
   }
  else avList_<<MSAttrValue("startValue","");
  return MSScale::get(avList_);
}
