///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////

#include <MSGUI/MSVGauge.H>

extern const int SliderAreaShadowThickness;

MSVGauge::MSVGauge(MSWidget *owner_,const char *title_) : 
MSVScale(owner_,title_) { init(); }
MSVGauge::MSVGauge(MSWidget *owner_,const MSStringVector& title_) : 
MSVScale(owner_,title_) { init(); }
MSVGauge::MSVGauge(MSWidget *owner_,MSFloat& model_,const char *title_) : 
MSVScale(owner_,model_,title_) { init(); }
MSVGauge::MSVGauge(MSWidget *owner_,MSFloat& model_,const MSStringVector& title_) : 
MSVScale(owner_,model_,title_) { init(); }
MSVGauge::MSVGauge(MSWidget *owner_,MSInt& model_,const char *title_) : 
MSVScale(owner_,model_,title_) { init(); }
MSVGauge::MSVGauge(MSWidget *owner_,MSInt& model_,const MSStringVector& title_) : 
MSVScale(owner_,model_,title_) { init(); }

void MSVGauge::init(void)
{
  acceptFocus(MSFalse);
  sensitive(MSFalse);
  _gaugeHeight=0;
  _startValue.unset();
}

MSVGauge::~MSVGauge(void) {}

void MSVGauge::redraw(void)
{
  MSVScale::redraw();
  drawGauge();
}

void MSVGauge::drawSubWindows(void)
{
  if (outputMode()<Print)
   {
     if (valueAlignment()!=MSNone)
      {
        valueWin()->map();
        valueWin()->raise();
      }
     else valueWin()->unmap();
     valueWin()->moveTo(x_end(),y_org());
   }
}

void MSVGauge::update(const MSIndexVector&)
{
  updateSliderSize(currentValue());
}

void MSVGauge::sliderRedrawNotify(void)
{
  drawGauge();
}

void MSVGauge::updateSliderSize(double value_)
{
  if (gaugeHeight()<=slider()->shadowThickness()||value_<=valueMin()||value_>=valueMax()) 
   {
     drawSliderArea();
     drawGauge();
   }
  else updateGauge((int)(value_));
  MSString buffer;
  valueWin()->updateValue(formatValue(buffer,value_));
}

void MSVGauge::drawGauge(void)
{
  if (mapped()==MSTrue)
   {
     int x=sliderAreaRect().x()+SliderAreaShadowThickness;
     int y=y_end();
     int curValue=valueToPixel(currentValue())+slider()->height();
     int startValue;
     if(_startValue.isSet()==MSTrue)
      {
        double sv=_startValue;
        if(sv<valueMin()) sv=valueMin();
	else if(sv>valueMax()) sv=valueMax();
	startValue=valueToPixel(sv)+slider()->height();
      }
     else startValue=y-SliderAreaShadowThickness;
     Direction direction=curValue<startValue?Up:Down;
     int thickness=slider()->shadowThickness();
     int h=abs(startValue-curValue); 
     int starty=(direction==Up)?startValue:curValue;
     
     if (h>=thickness&&slider()->width()>thickness*2)
      {
        int height=h-(h>thickness*2?thickness*2:thickness);
        XBFillRectangle(display(),window(),slider()->backgroundShadowGC(),
                        x+thickness,starty-h+thickness,slider()->width()-2*thickness,height);
      }
     if (h>thickness&&thickness>0)
      {
        drawGaugeShadow(x,starty,h,thickness,direction);
      }
     gaugeHeight(h);
     _direction=direction;
   }
}

void MSVGauge::drawGaugeShadow(int x,int y, int h, int thickness, Direction direction)
{
  // left
  XBFillRectangle(display(),window(),slider()->topShadowGC(),x,y-h,thickness,h);

  // top or bottom
  int starty=direction==Up?y:y+h-thickness;

  XBFillRectangle(display(),window(),slider()->topShadowGC(),x,starty-h,slider()->width(),
                  h>thickness?thickness:h);

  if (h>thickness)
   {
     XPoint points[6];
     points[0].y=points[1].y=direction==Up?y:y-h;
     points[4].y=points[5].y=direction==Up?y-thickness:y-h+thickness;
     points[2].y=direction==Up?y-h:y;
     points[3].y=direction==Up?y-h+thickness:y-thickness;
     points[0].x=x;
     points[1].x=points[2].x=x+slider()->width();
     points[3].x=points[4].x=x+slider()->width()-thickness;
     points[5].x=x+thickness;
     XBFillPolygon(display(),window(),slider()->bottomShadowGC(),points,6,Nonconvex,CoordModeOrigin);
   }
}

void MSVGauge::updateGauge(int h_)
{
  if (mapped()==MSTrue)
   {
     int x=sliderAreaRect().x()+SliderAreaShadowThickness;
     int y=y_end();
     int curValue=valueToPixel(h_)+slider()->height();
     int startValue;
     if(_startValue.isSet()==MSTrue)
      {
        double sv=_startValue;
        if(sv<valueMin()) sv=valueMin();
	else if(sv>valueMax()) sv=valueMax();
	startValue=valueToPixel(sv)+slider()->height();
      }
     else startValue=y-SliderAreaShadowThickness;
     Direction direction=curValue<startValue?Up:Down;
     int thickness=slider()->shadowThickness();
     int h=abs(startValue-curValue); 
     int starty=startValue;
     int height=abs(h-gaugeHeight());
             
     if (h==gaugeHeight() && direction==_direction) return;
     else if (direction==_direction)
      {
        if (h<gaugeHeight())
         {
           // value is smaller, clear area
           starty=direction==Up?starty-gaugeHeight():starty+h-thickness;
           XFillRectangle(display(),window(),selectShadowGC(),x,starty,slider()->width(),height+thickness);
         }
        else
         {
           // value is bigger, add area
           starty=direction==Up?starty-h+thickness:starty+gaugeHeight()-thickness;
           XFillRectangle(display(),window(),slider()->backgroundShadowGC(),x+thickness,
                          starty,slider()->width()-2*thickness,height);
         }
        if (h>thickness&&thickness>0) updateGaugeShadow(x,starty,h,thickness,direction);
      }
     else
      {
        // direction changed, redraw
        drawSliderArea();
        drawGauge();
      }
     gaugeHeight(h);
     _direction=direction;
   }
}

void MSVGauge::updateGaugeShadow(int x,int y,int h, int thickness, Direction direction)
{
  int height=abs(h-gaugeHeight());
  XPoint points[6];

  if (h<gaugeHeight())
   {
     if (direction==Up)
      {
        points[0].y=points[1].y=y+height+thickness;
        points[2].y=points[3].y=y+height;
        points[0].x=points[3].x=x;
        points[1].x=x+slider()->width()-thickness;
        points[2].x=x+slider()->width();
      }
     else
      {
        points[0].y=points[1].y=y+thickness;
        points[2].y=points[3].y=y;
        points[0].x=points[3].x=x;
        points[2].x=x+slider()->width()-thickness;
        points[1].x=x+slider()->width();
      }
     XBFillPolygon(display(),window(),slider()->topShadowGC(),points,4,Nonconvex,CoordModeOrigin);
   }
  else 
   {
     if (direction==Up)
      {
        XFillRectangle(display(),window(),slider()->bottomShadowGC(),
                       x+slider()->width()-thickness,y-thickness,thickness,height+thickness);
        points[0].y=points[1].y=y-thickness;
        points[2].y=points[3].y=y;
        points[4].y=points[5].y=y+height;
        points[0].x=points[5].x=x;
        points[3].x=points[4].x=x+thickness;
        points[1].x=x+slider()->width();
        points[2].x=x+slider()->width()-thickness;
      }
     else
      {
        XFillRectangle(display(),window(),slider()->bottomShadowGC(),x+slider()->width()-thickness,
                       y,thickness,height+thickness);
        points[0].y=points[1].y=y+height+thickness;
        points[2].y=points[3].y=y+height;
        points[4].y=points[5].y=y;
        points[0].x=points[5].x=x;
        points[3].x=points[4].x=x+thickness;
        points[2].x=x+slider()->width()-thickness;
        points[1].x=x+slider()->width();
      }
     XBFillPolygon(display(),window(),slider()->topShadowGC(),points,6,Nonconvex,CoordModeOrigin);
   }
}


void MSVGauge::startValue(const MSFloat& value_ )
{
  _startValue=value_;
  drawSliderArea();
  drawGauge();
}

void MSVGauge::set(MSAttrValueList& avList_)
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

MSAttrValueList& MSVGauge::get(MSAttrValueList& avList_)
{
  if (_startValue.isSet()==MSTrue)
   {
     avList_<<MSAttrValue("startValue",_startValue.asString());
   }
  else avList_<<MSAttrValue("startValue","");
  return MSScale::get(avList_);
}



// #########################################################
// default virtual methods - prevents gratuitous inlining
// #########################################################

void MSVGauge::computeLabelOffset(void)
{ labelOffset(0); }

void MSVGauge::moveValueWin(int,int) {}
void MSVGauge::buttonPress(const XEvent *) {}
void MSVGauge::keyPress(const XEvent *,KeySym,unsigned int,const char *) {}
void MSVGauge::drawSliderEtch(void) {}


