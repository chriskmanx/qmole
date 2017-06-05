///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////

#include <stdlib.h>
#include <MSGUI/MSHScrollBar.H>
#include <MSGUI/MSArrow.H>

static const int DefaultScrollBarWidth=100;

//static const int MinHSBWidth=39;
static const int MinimumMotifSliderWidth=4;
static const int DefaultElevatorBoxWidth=15;
static const int DefaultElevatorWidth=49;
static const int MinElevatorWidth=33;

static const int MSHScrollBarMinPropLength=3;
static const int MSHScrollBarDefaultArrowWidth=9;
static const int MSHScrollBarDefaultArrowOffset=3;
static const int MSHScrollBarDefaultMarkerWidth=6;
static const int MSHScrollBarDefaultMarkerGap=2;
static const int MSHScrollBarDefaultCableWidth=3;
static const int MSHScrollBarSmallThickness=17;

MSHScrollBar::HElevator::HElevator(MSHScrollBar *owner_) : MSScrollBar::Elevator(owner_)
{
  _arrow1=new MSArrow(this,MSArrow::Left);
  _arrow2=new MSArrow(this,MSArrow::Right);
}

MSHScrollBar::HElevator::~HElevator(void)
{}

void MSHScrollBar::HElevator::configure(void)
{
  if (scrollBar()->style()==MSScrollBar::Openlook)
   {
     if (_arrow1!=0&&_arrow2!=0)
      {
	int offset=highlightThickness()+shadowThickness();
	int aw=MSHScrollBarDefaultArrowWidth;
	int ah=height()-2*offset;
	
	_arrow1->resize(aw,ah);
	_arrow2->resize(aw,ah);
	_arrow1->moveTo(offset+MSHScrollBarDefaultArrowOffset,offset);
	_arrow2->moveTo(width()-offset-_arrow2->width()-MSHScrollBarDefaultArrowOffset,offset);
	draw();
      }
   }
}

void MSHScrollBar::HElevator::unselect(void)
{
  if (mapped()==MSTrue&&owner()->mapped()==MSTrue&&owner()->owner()->mapped()==MSTrue)
   {
     if (scrollBar()->style()==MSScrollBar::Openlook)
      {
	if (width()==DefaultElevatorWidth)
	 {
	   int offset=highlightThickness()+shadowThickness();
	   int eh=height()-2*offset;
	   int ew=DefaultElevatorBoxWidth-2;     
	   int x=offset+DefaultElevatorBoxWidth+1;     
	   int y=offset;
	   XFillRectangle(display(),window(),backgroundShadowGC(),x,y,ew,eh);
	 }
      }
   }
}

void MSHScrollBar::HElevator::select(void)
{
  if (mapped()==MSTrue&&owner()->mapped()==MSTrue&&owner()->owner()->mapped()==MSTrue)
   {
     if (scrollBar()->style()==MSScrollBar::Openlook)
      {
	if (width()==DefaultElevatorWidth)
	 {
	   int offset=highlightThickness()+shadowThickness();
	   int eh=height()-2*offset;
	   int ew=DefaultElevatorBoxWidth;     
	   int x=offset+DefaultElevatorBoxWidth;     
	   int y=offset;
	   int w=ew>>1;
	   int h=eh>>1;
	   int xd=(ew-w)>>1;
	   int yd=(eh-h)>>1;
	   XFillArc(display(),window(),bottomShadowGC(),x+xd,y+yd,w,h,0,360*64);
	 }
      }
   }
}

void MSHScrollBar::HElevator::draw(void)
{
  if (mapped()==MSTrue&&owner()->mapped()==MSTrue&&owner()->owner()->mapped()==MSTrue)
   {
     int ht=highlightThickness();
     int sht=shadowThickness();
     int offset=ht+sht;
     int x=offset+DefaultElevatorBoxWidth;     

     if (scrollBar()->style()==MSScrollBar::Openlook)
      {
	drawBackground();
	drawShadow(MSRaised);
	
	if (width()==DefaultElevatorWidth)
	 {
	   XDrawLine(display(),window(),topShadowGC(),x,offset,x,height()-2*offset);
	   x+=DefaultElevatorBoxWidth;
	   XDrawLine(display(),window(),bottomShadowGC(),x,offset,x,height()-2*offset);
	   drawArrows();
	 }
	else if (width()==MinElevatorWidth)
	 {
	   XDrawLine(display(),window(),bottomShadowGC(),x,offset,x,height()-2*offset);
	   drawArrows();
	 }
      }
     else
      {
        drawRaised();
	XFillRectangle(display(),window(),backgroundShadowGC(),
		       offset,offset,width()-2*offset,height()-2*offset);
        drawArrows();
      }
   }
}

MSHScrollBar::MSHScrollBar(MSWidget *owner_,int min_,int max_,int inc_) : 
MSScrollBar(owner_,min_,max_,inc_)
{ init(); }

MSHScrollBar::~MSHScrollBar(void)
{}

void MSHScrollBar::init(void)
{
  _elevator=new HElevator(this);
  resize(DefaultScrollBarWidth,MSHScrollBarSmallThickness+2*highlightThickness());
  _elevator->map();
}

void MSHScrollBar::adjustPointer(void)
{
  if (style()==MSScrollBar::Openlook)
   {
     if (changeType()==Dec)
      {
	XWarpPointer(display(),None,_elevator->window(),0,0,0,0,
		     DefaultElevatorBoxWidth>>1,_elevator->height()>>1); 
      }
     else if (changeType()==Inc)
      {
	XWarpPointer(display(),None,_elevator->window(),0,0,0,0,
		     _elevator->width()-(DefaultElevatorBoxWidth>>1),
		     _elevator->height()>>1); 
      }
   }
}

void MSHScrollBar::configureElevator(void)
{
  if (style()!=MSScrollBar::Openlook&&_elevator!=0)
   {
     int offset=highlightThickness()+shadowThickness();
     int arrowWidth=_elevator->_arrow1->width();

     if (arrowWidth==0) arrowWidth=-1;
     arrowWidth+=(offset+1);
     
     double userSize=(max()-min());
     double trueSize=sliderAreaRect().width();
     double factor;
     factor=(userSize!=0)?trueSize/userSize:0;
     double slideSize=(double)(viewSize())*factor; 
     int w=(int)(slideSize+0.5);

     _elevator->resize(w>MinimumMotifSliderWidth?w:MinimumMotifSliderWidth,height()-2*offset);
   }
}

void MSHScrollBar::configureForOpenlook(void)
{
  _shadowThickness=1;
  if (_elevator!=0)
   {
     _elevator->shadowThickness(shadowThickness());
     _elevator->_arrow1->owner(_elevator);
     _elevator->_arrow2->owner(_elevator);  
   }
  
  int ht=highlightThickness();  
  int sht=shadowThickness();
  int mw=MSHScrollBarDefaultMarkerWidth;
  int mg=MSHScrollBarDefaultMarkerGap;
  int ew=DefaultElevatorWidth;
  int w=width()-2*ht;

  if (w>=ew+2*(mw+mg))
   {
     _markersOn=MSTrue;
     _elevatorOn=MSTrue;
   }
  else
   {
     ew=MinElevatorWidth;
     if (w>=ew+2*(mw+mg))
      {
        _markersOn=MSTrue;
	_elevatorOn=MSTrue;
      }
     else if (w>=ew)
      {
        mw=0;
        mg=0;
	_markersOn=MSFalse;
	_elevatorOn=MSTrue;
      }
     else
      {
        mw=0;
        mg=0;
	_markersOn=MSFalse;
	_elevatorOn=MSFalse;
      }
   }

  sliderAreaRect().x(ht+mw+mg);
  sliderAreaRect().width(w-2*(mw+mg));
  sliderAreaRect().y(ht);
  sliderAreaRect().height(height()-2*ht);

  double factor=0.0;
  if (max()-min()>0)
   {
     double userSize=(max()-min());
     double trueSize=sliderAreaRect().width()-_elevator->width();
     factor=trueSize/userSize;
   }
  double slideStart=(double)(value()-min())*factor;
  int xx=(int)(slideStart+0.5)+sliderAreaRect().x();

  if (_elevator!=0)
   {
     _elevator->moveTo(boundsCheckX(xx),ht);
     _elevator->resize(ew,height()-2*ht);
   }
}

void MSHScrollBar::configureForMotif(void)
{
  if (_elevator!=0)
   {
     _shadowThickness=2;
     _elevator->shadowThickness(shadowThickness());     
     _elevator->_arrow1->owner(this);
     _elevator->_arrow2->owner(this);  

     const int MinScrollBarLength=6;
     int offset=shadowThickness()+highlightThickness();  
     int w,h;
     
     w=h=height()-2*offset;
     if (width()<2*(w+offset)+MinScrollBarLength+2) w=(width()-(MinScrollBarLength+2+2*offset))/2;

     sliderAreaRect().y(offset);
     sliderAreaRect().x(offset+w+1);
     sliderAreaRect().width(width()-2*(offset+w+1));
     sliderAreaRect().height(height()-2*offset);

     _elevator->_arrow1->configure(offset,offset,w,h); 
     _elevator->_arrow2->configure(offset+w+1+sliderAreaRect().width()+1,offset,w,h);

     int arrowWidth=_elevator->_arrow1->width();

     if (arrowWidth==0) arrowWidth=-1;
     arrowWidth+=(offset+1);
     
     double userSize=(max()-min());
     double trueSize=sliderAreaRect().width();
     double factor=(userSize!=0)?trueSize/userSize:0;
     double slideStart=(double)(value()-min())*factor+arrowWidth;
     double slideSize=(double)(viewSize())*factor; 
     int xx=(int)(slideStart+0.5);
     w=(int)(slideSize+0.5);

     _elevator->moveTo(boundsCheckX(xx),offset);
     _elevator->resize(w>MinimumMotifSliderWidth?w:MinimumMotifSliderWidth,height()-2*offset);
     redrawElevator();
   }
}

void MSHScrollBar::motionLoop(void)
{
  unsigned int mask=Button1Mask|Button2Mask;
  unsigned int keys=mask;
  int ix=0,iy=0;
  int rx=0,ry=0;
  int button_x;
  int button_y;
  int newX;
  int realX;
  int slideVal;
  Window root,child;

  freeze();  // prevent redraw from expose when elevator is moved.
  _elevator->select();  
  while (keys&mask)
   {
     XQueryPointer(display(),window(),&root,&child,&rx,&ry,&ix,&iy,&keys);
     button_y=iy;
     button_x=ix;
     
     // Force button_x and button_y to be within the slider_area.
     if (button_x<sliderAreaRect().x()) button_x=sliderAreaRect().x();
     
     if (button_x>sliderAreaRect().x()+sliderAreaRect().width())
      button_x=sliderAreaRect().x()+sliderAreaRect().width();
     
     if (button_y<sliderAreaRect().y()) button_y=sliderAreaRect().y();
     
     if (button_y>sliderAreaRect().y()+sliderAreaRect().height())
      button_y=sliderAreaRect().y()+sliderAreaRect().height();
     
     // Calculate the new origin of the slider.  
     // Bound the values with the slider area.
     newX=realX=button_x-_separation_x;
     
     if (newX<sliderAreaRect().x()) newX=sliderAreaRect().x();
     
     if (newX+_elevator->width()>sliderAreaRect().x()+sliderAreaRect().width())
      newX=sliderAreaRect().x()+sliderAreaRect().width()-_elevator->width();
     
     if (realX!=_initial_x)
      {
	slideVal=calcSliderValue(button_x,button_y);
	if (slideVal==value()) _changeType=NoChange;
	if (slideVal>=max()-viewSize())
	 {
	   slideVal=max()-viewSize();
	   newX=calcXValue(slideVal);
	 }
	int oldValue=value();
	_value=slideVal;
	moveElevator(newX,_elevator->y());
	_initial_x=_elevator->x();	
        if (oldValue!=_value) drag();
      }
   }
  _elevator->unselect();
  unfreeze();
}

void MSHScrollBar::motifButton1Press(const XEvent *pXEvent_)
{
  int xx=pXEvent_->xbutton.x;
  int yy=pXEvent_->xbutton.y;

  _savedValue=value();
  _separation_x=0;
  _separation_y=0;
  _changeType=NoChange;
  
  //  Calculate whether the selection point is in the slider  
  if ((xx>=_elevator->x())&&
      (xx<=_elevator->x()+_elevator->width())&&
      (yy>=_elevator->y())&&
      (yy<=_elevator->y()+_elevator->height()))
   {
     _separation_x=xx-_elevator->x();
     _separation_y=yy-_elevator->y();
     _initial_x=_elevator->x();
     _initial_y=_elevator->y();
     _slidingOn=MSTrue;
     _changeType=Drag;
     motionLoop();
   }
  // ... in arrow 1 
  else if ((xx>=_elevator->_arrow1->x())&&
	   (yy>=_elevator->_arrow1->y())&&
	   (xx<=_elevator->_arrow1->x()+_elevator->_arrow1->width())&&
	   (yy<=_elevator->_arrow1->y()+_elevator->_arrow1->height()))
   {
     _changeType=Dec;
     _elevator->_arrow1->select(MSTrue);
   }
  // ... in arrow 2 
  else if ((xx>=_elevator->_arrow2->x())&&
	   (yy>=_elevator->_arrow2->y())&&
	   (xx<=_elevator->_arrow2->x()+_elevator->_arrow2->width())&&
	   (yy<=_elevator->_arrow2->y()+_elevator->_arrow2->height()))
   {
     _changeType=Inc;
     _elevator->_arrow2->select(MSTrue);
   }
  // ... in the trough (i.e. slider area)... 
  else if ((xx>=sliderAreaRect().x())&&
	   (yy>=sliderAreaRect().y())&&
	   (xx<=sliderAreaRect().x()+sliderAreaRect().width())&&
	   (yy<=sliderAreaRect().y()+sliderAreaRect().height()))
   {
     // Page the slider up or down 
     if (xx<_elevator->x()) _changeType=PageUp;
     else _changeType=PageDown;
   }
  else return; //... in the highlight area.  
  startDelayTimer();
  updateSliderValue();
}

void MSHScrollBar::motifButton2Press(const XEvent *pXEvent_)
{
  int xx=pXEvent_->xbutton.x;
  int yy=pXEvent_->xbutton.y;

  _savedValue=value();
  _separation_x=0;
  _separation_y=0;
  _changeType=NoChange;
  
  //  Calculate whether the selection point is in the slider  
  if ((xx>=_elevator->x())&&
      (xx<=_elevator->x()+_elevator->width())&&
      (yy>=_elevator->y())&&
      (yy<=_elevator->y()+_elevator->height()))
   {
     _separation_x=xx-_elevator->x();
     _separation_y=yy-_elevator->y();
     _initial_x=_elevator->x();
     _initial_y=_elevator->y();
     _slidingOn=MSTrue;
     _changeType=Drag;
   }
  else if ((xx>=sliderAreaRect().x())&&
	   (yy>=sliderAreaRect().y())&&
	   (xx<=sliderAreaRect().x()+sliderAreaRect().width())&&
	   (yy<=sliderAreaRect().y()+sliderAreaRect().height()))
   {
     // Warp the slider to the cursor, and then drag 
     _separation_x=_elevator->width()/2;
     _initial_x=_elevator->x();
     _initial_y=_elevator->y();
     _slidingOn=MSTrue;
     _changeType=Drag;	
   }
  motionLoop();
}

void MSHScrollBar::openlookButton1Press(const XEvent *pXEvent_)
{
  int ht=highlightThickness();
  int xx=pXEvent_->xbutton.x;
  int yy=pXEvent_->xbutton.y;

  _savedValue=value();
  _separation_x=0;
  _changeType=NoChange;
  
  //  Calculate whether the selection point is in the slider  
  if ((_elevator->width()==DefaultElevatorWidth)&&
       (xx>=_elevator->x()+DefaultElevatorBoxWidth)&&
       (xx<=_elevator->x()+_elevator->width()-DefaultElevatorBoxWidth)&&
       (yy>=_elevator->y())&&
       (yy<=_elevator->y()+_elevator->height()))
   {
     _separation_x=xx-_elevator->x();
     _initial_x=_elevator->x();
     _slidingOn=MSTrue;
     _changeType=Drag;
     _elevator->select();
     motionLoop();
     return;
   }
  // ... in arrow 1 
  else if ((yy>=_elevator->_arrow1->y())&&
	    (xx>=_elevator->x())&&
	    (yy<=_elevator->_arrow1->y()+_elevator->_arrow1->height())&&
	    (xx<=_elevator->x()+DefaultElevatorBoxWidth))
   {
     _changeType=Dec;
     _elevator->_arrow1->select(MSTrue);
   }
  // ... in arrow 2 
  else if ((yy>=_elevator->_arrow2->y())&&
	    (xx>=_elevator->x()+_elevator->width()-DefaultElevatorBoxWidth)&&
	    (yy<=_elevator->_arrow2->y()+_elevator->_arrow2->height())&&
	    (xx<=_elevator->x()+_elevator->width()))
   {
     _changeType=Inc;
     _elevator->_arrow2->select(MSTrue);
   }
  // ... in top marker
  else if ((yy>=sliderAreaRect().y())&&
	    (xx>=ht)&&
	    (yy<=sliderAreaRect().y()+sliderAreaRect().height())&&
	    (xx<=ht+MSHScrollBarDefaultMarkerWidth))
   {
     _changeType=Home;
   }
  // ... in bottom marker
  else if ((yy>=sliderAreaRect().y())&&
	    (xx>=width()-ht-MSHScrollBarDefaultMarkerWidth)&&
	    (yy<=sliderAreaRect().y()+sliderAreaRect().height())&&
	    (xx<=width()-ht))
   {
     _changeType=End;
   }
  // ... in the trough (i.e. slider area)... 
  else if ((xx>=sliderAreaRect().x())&&
	    (yy>=sliderAreaRect().y())&&
	    (xx<=sliderAreaRect().x()+sliderAreaRect().width())&&
	    (yy<=sliderAreaRect().y()+sliderAreaRect().height ()))
   {
     /* Page the slider up or down */
     if (xx<_elevator->x()+DefaultElevatorBoxWidth) _changeType=PageUp;
     else _changeType=PageDown;
   }
  else return; //... in the highlight area.  
  startDelayTimer();
  updateSliderValue();
}

void MSHScrollBar::openlookButton2Press(const XEvent *pXEvent_)
{
  int xx=pXEvent_->xbutton.x;
  int yy=pXEvent_->xbutton.y;

  _savedValue=value();
  _separation_x=0;
  _changeType=NoChange;
  
  //  Calculate whether the selection point is in the slider  
  if ((_elevator->width()==DefaultElevatorWidth)&&
       (xx>=_elevator->x()+DefaultElevatorBoxWidth)&&
       (xx<=_elevator->x()+_elevator->width()-DefaultElevatorBoxWidth)&&
       (yy>=_elevator->y())&&
       (yy<=_elevator->y()+_elevator->height()))
   {
     _separation_x=xx-_elevator->x();
     _initial_x=_elevator->x();
     _slidingOn=MSTrue;
     _changeType=Drag;
   }
  // ... in the trough (i.e. slider area)... 
  else if ((xx>=sliderAreaRect().x())&&
            (yy>=sliderAreaRect().y())&&
            (xx<=sliderAreaRect().x()+sliderAreaRect().width())&&
            (yy<=sliderAreaRect().y()+sliderAreaRect().height()))
   {
     // Warp the slider to the cursor,and then drag 
     _separation_x=_elevator->width()/ 2;
     _initial_x=_elevator->x();
     _slidingOn=MSTrue;
     _changeType=Drag;	
   }
  motionLoop();
}

int MSHScrollBar::boundsCheckX(int x_)
{
  if (x_<sliderAreaRect().x()) x_=sliderAreaRect().x();
  if (x_+_elevator->width()>sliderAreaRect().x()+sliderAreaRect().width())
   { x_=sliderAreaRect().x()+sliderAreaRect().width()-_elevator->width(); }
  return x_;
}

int MSHScrollBar::calcSliderValue(int x_,int)  
{
  if (x_<=sliderAreaRect().x())
   {
     return min();
   }
  else if (x_>=sliderAreaRect().x()+sliderAreaRect().width())
   {
     return max()-viewSize();
   }
  else
   {
     double referencePoint=(double)x_-_separation_x;
     double userSize=max()-min();
     double temp;

     if (style()==MSScrollBar::Openlook)
      {
        double trueSize=sliderAreaRect().width()-_elevator->width();
        temp=referencePoint/trueSize;
      }
     else
      {
        int offset=highlightThickness()+shadowThickness();
        double trueSize=sliderAreaRect().width();
        int arrowSize=(_elevator->_arrow1!=0)?_elevator->_arrow1->width():0;
        temp=(referencePoint-arrowSize-offset)/trueSize;
      }

     temp=temp*userSize+min()+0.5;
     int intTemp=(int)temp;

     if (intTemp<min()) intTemp=min();
     else if (intTemp>max()-viewSize()) intTemp=max()-viewSize();
     return (intTemp);
   }
}

int MSHScrollBar::calcXValue(int value_)  
{
  double factor=0.0;
  double userSize=(max()-min());
  int xx=0;
  
  if (style()==MSScrollBar::Openlook)
   {
     if (userSize>0)
      {
	double trueSize=sliderAreaRect().width()-_elevator->width();
	factor=trueSize/userSize;
      }
     double slideStart=(double)(value_-min())*factor;
     xx=(int)(slideStart+0.5)+sliderAreaRect().x();
   }
  else
   {
     int offset=shadowThickness()+highlightThickness();  
     int arrowWidth=_elevator->_arrow1->width();
     if (arrowWidth==0) arrowWidth=-1;
     arrowWidth+=(offset+1);
     if (userSize>0)
      {
	double trueSize=sliderAreaRect().width();
	factor=trueSize/userSize;
      }
     double slideStart=(double)(value_-min())*factor+arrowWidth;
     xx=(int)(slideStart+0.5);
   }
  return boundsCheckX(xx);
}

void MSHScrollBar::redrawElevator(void)  
{
  if (mapped()==MSTrue&&owner()->mapped()==MSTrue)
   {
     _elevator->moveTo(calcXValue(value()),_elevator->y());
     if (style()==MSScrollBar::Openlook)
      {
	drawElevatorCable();
	drawPropIndicator();
      }
     else
      {
        drawElevator();
      }
   }
}

void MSHScrollBar::moveElevator(int x_,int y_)  
{
  if (x_!=_elevator->x())
   {
     _elevator->moveTo(x_,y_);
     drawElevatorCable();
     drawPropIndicator();
   }
}

void MSHScrollBar::drawElevatorCable(void) 
{
  if (mapped()==MSTrue&&owner()->mapped()==MSTrue)
   {
     if (style()==MSScrollBar::Openlook)
      {
	int y=((height()-MSHScrollBarDefaultCableWidth)>>1);
	int x=sliderAreaRect().x();
	int w=sliderAreaRect().width();
	
	XSetFillStyle(display(),bottomShadowGC(),FillTiled);
	XFillRectangle(display(),window(),bottomShadowGC(),x,y,
		       w,MSHScrollBarDefaultCableWidth);
	XSetFillStyle(display(),bottomShadowGC(),FillSolid);
      }
   }
}

void MSHScrollBar::drawPropIndicator(void) 
{
  if (mapped()==MSTrue&&owner()->mapped()==MSTrue)
   {
     if (style()==MSScrollBar::Openlook)
      {
	XRectangle rect[2];
	double factor=0.0;
	if (max()-min()>0)
	 {
	   double userSize=(max()-min());
	   double trueSize=sliderAreaRect().width()-_elevator->width();
	   factor=trueSize/userSize;
	 }
	double propSize=(double)(viewSize())*factor; 
	int propLength=(int)(propSize+0.5);
	int n=1;
	
	propLength= (propLength<MSHScrollBarMinPropLength)?MSHScrollBarMinPropLength:propLength;
	
	if (viewSize()>=(max()-min())) sensitive(MSFalse);
	else sensitive(MSTrue);
	
	int y=((height()-MSHScrollBarDefaultCableWidth) >> 1);
	int x=_elevator->x()+_elevator->width();
	
	rect[0].y=rect[1].y=y;
	rect[0].height=rect[1].height=MSHScrollBarDefaultCableWidth;
	
	if (value()==max()-viewSize())
	 {
	   rect[0].x=x;
	   rect[0].width=sliderAreaRect().width()+sliderAreaRect().x()-x;
	 }
	else
	 {
	   if (x+propLength>sliderAreaRect().x()+sliderAreaRect().width())
	    {
	      if (_elevator->x()-propLength>sliderAreaRect().x())
	       {
		 int w=sliderAreaRect().width()+sliderAreaRect().x()-x;
		 if (w>0)
		  {
		    n=2;	   
		    rect[1].x=_elevator->x()+_elevator->width();
		    rect[1].width=w;
		  }
		 rect[0].width=propLength-w;
		 rect[0].x=_elevator->x()-rect[0].width;
	       }   
	      else
	       {
		 rect[0].x=sliderAreaRect().x();
		 rect[0].width=sliderAreaRect().width();  
	       }
	    }
	   else
	    {
	      rect[0].width=propLength;   
	      rect[0].x=x;
	    }
	 }  
	XFillRectangles(display(),window(),bottomShadowGC(),&rect[0],n);
      }
   }
}

void MSHScrollBar::drawMarkers(void) 
{
  if (mapped()==MSTrue&&owner()->mapped()==MSTrue&&markersOn()==MSTrue)
   {
     if (style()==MSScrollBar::Openlook)
      {
	int  ht=highlightThickness();
	MSRect rect(ht,ht,MSHScrollBarDefaultMarkerWidth,_elevator->height());
	
	drawBevel(rect,MSRaised,shadowThickness());
	rect.x(width()-ht-MSHScrollBarDefaultMarkerWidth);
	drawBevel(rect,MSRaised,shadowThickness());
      }
   }
}









