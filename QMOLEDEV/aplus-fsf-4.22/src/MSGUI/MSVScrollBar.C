///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////

#include <stdlib.h>
#include <MSGUI/MSVScrollBar.H>
#include <MSGUI/MSArrow.H>

static const int DefaultScrollBarHeight=100; 

//static const int MinVSBHeight=39;
static const int MinimumMotifSliderHeight=4;
static const int DefaultElevatorBoxHeight=15;
static const int DefaultElevatorHeight=49;
static const int MinElevatorHeight=33;

static const int MSVScrollBarMinPropLength=3;
static const int MSVScrollBarDefaultArrowHeight=9;
static const int MSVScrollBarDefaultArrowOffset=3;
static const int MSVScrollBarDefaultMarkerHeight=6;
static const int MSVScrollBarDefaultMarkerGap=2;
static const int MSVScrollBarDefaultCableWidth=3;
static const int MSVScrollBarSmallThickness=17;
 
MSVScrollBar::VElevator::VElevator(MSVScrollBar *owner_) : MSScrollBar::Elevator(owner_)
{
  _arrow1=new MSArrow(this,MSArrow::Up);
  _arrow2=new MSArrow(this,MSArrow::Down);
}

MSVScrollBar::VElevator::~VElevator(void)
{}

void MSVScrollBar::VElevator::configure(void)
{
  if (scrollBar()->style()==MSScrollBar::Openlook)
   {
     if (_arrow1!=0||_arrow2!=0)
      {
	int offset=highlightThickness()+shadowThickness();
	int ah=MSVScrollBarDefaultArrowHeight;
	int aw=width()-2*offset;
	
	_arrow1->resize(aw,ah);
	_arrow2->resize(aw,ah);
	_arrow1->moveTo(offset,offset+MSVScrollBarDefaultArrowOffset);
	_arrow2->moveTo(offset,height()-offset-_arrow2->height()-MSVScrollBarDefaultArrowOffset);
	draw();
      }
   }
}

void MSVScrollBar::VElevator::unselect(void)
{
  if (mapped()==MSTrue&&owner()->mapped()==MSTrue&&owner()->owner()->mapped()==MSTrue)
   {
     if (scrollBar()->style()==MSScrollBar::Openlook)
      {
	if (height()==DefaultElevatorHeight)
	 {
	   int offset=highlightThickness()+shadowThickness();
	   int ew=width()-2*offset;
	   int eh=DefaultElevatorBoxHeight-2;     
	   int y=offset+DefaultElevatorBoxHeight+1;     
	   int x=offset;
	   XFillRectangle(display(),window(),backgroundShadowGC(),x,y,ew,eh);
	 }
      }
   }
}

void MSVScrollBar::VElevator::select(void)
{
  if (mapped()==MSTrue&&owner()->mapped()==MSTrue&&owner()->owner()->mapped()==MSTrue)
   {
     if (scrollBar()->style()==MSScrollBar::Openlook)
      {
	if (height()==DefaultElevatorHeight)
	 {
	   int offset=highlightThickness()+shadowThickness();
	   int ew=width()-2*offset;
	   int eh=DefaultElevatorBoxHeight;     
	   int y=offset+DefaultElevatorBoxHeight;     
	   int x=offset;
	   int w=ew>>1;
	   int h=eh>>1;
	   int xd=(ew-w)>>1;
	   int yd=(eh-h)>>1;
	   XFillArc(display(),window(),bottomShadowGC(),x+xd,y+yd,w,h,0,360*64);
	 }
      }
   }
}

void MSVScrollBar::VElevator::draw(void)
{
  if (mapped()==MSTrue&&owner()->mapped()==MSTrue&&owner()->owner()->mapped()==MSTrue)
   {
     int ht=highlightThickness();
     int sht=shadowThickness();
     int offset=ht+sht;
     int y=offset+DefaultElevatorBoxHeight;     

     if (scrollBar()->style()==MSScrollBar::Openlook)
      {
	drawBackground();
	drawShadow(MSRaised);
	
	if (height()==DefaultElevatorHeight)
	 {
	   XDrawLine(display(),window(),topShadowGC(),offset,y,width()-2*offset,y);
	   y+=DefaultElevatorBoxHeight;
	   XDrawLine(display(),window(),bottomShadowGC(),offset,y,width()-2*offset,y);
	   drawArrows();
	 }
	else if (height()==MinElevatorHeight)
	 {
	   XDrawLine(display(),window(),bottomShadowGC(),offset,y,width()-2*offset,y);
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

MSVScrollBar::MSVScrollBar(MSWidget *owner_,int min_,int max_,int inc_) : 
MSScrollBar(owner_,min_,max_,inc_)
{ init(); }

MSVScrollBar::~MSVScrollBar(void)
{}

void MSVScrollBar::init(void)
{
  _elevator=new VElevator(this);
  resize(MSVScrollBarSmallThickness+2*highlightThickness(),DefaultScrollBarHeight);
  _elevator->map();
}

void MSVScrollBar::adjustPointer(void)
{
  if (style()==MSScrollBar::Openlook)
   {
     if (changeType()==Dec)
      {
	XWarpPointer(display(),None,_elevator->window(),
		     0,0,0,0,_elevator->width()>>1,DefaultElevatorBoxHeight>>1); 
      }
     else if (changeType()==Inc)
      {
	XWarpPointer(display(),None,_elevator->window(),
		     0,0,0,0,_elevator->width()>>1,
		     _elevator->height()-(DefaultElevatorBoxHeight>>1)); 
      }
     XFlush(display());
   }
}

void MSVScrollBar::configureElevator(void)
{
  if (style()!=MSScrollBar::Openlook&&_elevator!=0)
   {
     int offset=highlightThickness()+shadowThickness();
     int arrowHeight=_elevator->_arrow1->height();

     if (arrowHeight==0) arrowHeight=-1;
     arrowHeight+=(offset+1);
     
     double userSize=(max()-min());
     double trueSize=sliderAreaRect().height();
     double factor;
     factor=(userSize!=0)?trueSize/userSize:0;
     double slideSize=(double)(viewSize())*factor; 
     int h=(int)(slideSize+0.5);

     _elevator->resize(width()-2*offset,h>MinimumMotifSliderHeight?h:MinimumMotifSliderHeight);
   }
}

void MSVScrollBar::configureForOpenlook(void)
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
  int mh=MSVScrollBarDefaultMarkerHeight;
  int mg=MSVScrollBarDefaultMarkerGap;
  int eh=DefaultElevatorHeight;
  int h=height()-2*ht;

  if (h>=eh+2*(mh+mg))
   {
     _markersOn=MSTrue;
     _elevatorOn=MSTrue;
   }
  else
   {
     eh=MinElevatorHeight;
     if (h>=eh+2*(mh+mg))
      {
	_markersOn=MSTrue;
	_elevatorOn=MSTrue;
      }
     else if (h>=eh)
      {
	mh=0;
	mg=0;
	_markersOn=MSFalse;
	_elevatorOn=MSTrue;
      }
     else
      {
	mh=0;
	mg=0;
	_markersOn=MSFalse;
	_elevatorOn=MSFalse;
      }
   }

  sliderAreaRect().y(ht+mh+mg);
  sliderAreaRect().height(h-2*(mh+mg));
  sliderAreaRect().x(ht);
  sliderAreaRect().width(width()-2*ht);

  double factor=0.0;
  if (max()-min()>0)
   {
     double userSize=(max()-min());
     double trueSize=sliderAreaRect().height()-_elevator->height();
     factor=trueSize/userSize;
   }
  double slideStart=(double)(value()-min())*factor;
  int yy=(int)(slideStart+0.5)+sliderAreaRect().y();

  if (_elevator!=0)
   {
     _elevator->moveTo(ht,boundsCheckY(yy));
     _elevator->resize(width()-2*ht,eh);
   }
}

void MSVScrollBar::configureForMotif(void)
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
     
     w=h=width()-2*offset;
     if (height()<2*(h+offset)+MinScrollBarLength+2) h=(height()-(MinScrollBarLength+2+2*offset))/2;

     sliderAreaRect().y(offset+h+1);
     sliderAreaRect().x(offset);
     sliderAreaRect().width(width()-2*offset);
     sliderAreaRect().height(height()-2*(offset+h+1));

     _elevator->_arrow1->configure(offset,offset,w,h); 
     _elevator->_arrow2->configure(offset,offset+h+1+sliderAreaRect().height()+1,w,h);

     int arrowHeight=_elevator->_arrow1->height();

     if (arrowHeight==0) arrowHeight=-1;
     arrowHeight+=(offset+1);
     
     double userSize=(max()-min());
     double trueSize=sliderAreaRect().height();
     double factor=(userSize!=0)?trueSize/userSize:0;
     double slideStart=(double)(value()-min())*factor+arrowHeight;
     double slideSize=(double)(viewSize())*factor; 
     int yy=(int)(slideStart+0.5);
     h=(int)(slideSize+0.5);

     _elevator->moveTo(offset,boundsCheckY(yy));
     _elevator->resize(width()-2*offset,h>MinimumMotifSliderHeight?h:MinimumMotifSliderHeight);
     redrawElevator();
   }
}

void MSVScrollBar::motifButton1Press(const XEvent *pXEvent_)
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
     if (yy<_elevator->y()) _changeType=PageUp;
     else _changeType=PageDown;
   }
  else return; //... in the highlight area.  
  startDelayTimer();
  updateSliderValue();
}

void MSVScrollBar::motifButton2Press(const XEvent *pXEvent_)
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
     _separation_y=_elevator->height()/2;
     _initial_x=_elevator->x();
     _initial_y=_elevator->y();
     _slidingOn=MSTrue;
     _changeType=Drag;	
   }
  motionLoop();
}


void MSVScrollBar::openlookButton1Press(const XEvent *pXEvent_)
{
  int ht=highlightThickness();
  int xx=pXEvent_->xbutton.x;
  int yy=pXEvent_->xbutton.y;

  _savedValue=value();
  _separation_y=0;
  _changeType=NoChange;
  
  //  Calculate whether the selection point is in the slider  
  if ((_elevator->height()==DefaultElevatorHeight)&&
      (xx>=_elevator->x())&&
      (xx<=_elevator->x()+_elevator->width())&&
      (yy>=_elevator->y()+DefaultElevatorBoxHeight)&&
      (yy<=_elevator->y()+_elevator->height()-DefaultElevatorBoxHeight))
   {
     _separation_y=yy-_elevator->y();
     _initial_y=_elevator->y();
     _slidingOn=MSTrue;
     _changeType=Drag;
     motionLoop();
     return;
   }
  // ... in arrow 1 
  else if ((xx>=_elevator->_arrow1->x())&&
	   (yy>=_elevator->y())&&
	   (xx<=_elevator->_arrow1->x()+_elevator->_arrow1->width())&&
	   (yy<=_elevator->y()+DefaultElevatorBoxHeight))
   {
     _changeType=Dec;
     _elevator->_arrow1->select(MSTrue);
   }
  // ... in arrow 2 
  else if ((xx>=_elevator->_arrow2->x())&&
	   (yy>=_elevator->y()+_elevator->height()-DefaultElevatorBoxHeight)&&
	   (xx<=_elevator->_arrow2->x()+_elevator->_arrow2->width())&&
	   (yy<=_elevator->y()+_elevator->height()))
   {
     _changeType=Inc;
     _elevator->_arrow2->select(MSTrue);
   }
  // ... in top marker
  else if ((xx>=sliderAreaRect().x())&&
	   (yy>=ht)&&
	   (xx<=sliderAreaRect().x()+sliderAreaRect().width())&&
	   (yy<=ht+MSVScrollBarDefaultMarkerHeight))
   {
     _changeType=Home;
   }
  // ... in bottom marker
  else if ((xx>=sliderAreaRect().x())&&
	   (yy>=height()-ht-MSVScrollBarDefaultMarkerHeight)&&
	   (xx<=sliderAreaRect().x()+sliderAreaRect().width())&&
	   (yy<=height()-ht))
   {
     _changeType=End;
   }
  // ... in the trough (i.e. slider area)... 
  else if ((xx>=sliderAreaRect().x())&&
	   (yy>=sliderAreaRect().y())&&
	   (xx<=sliderAreaRect().x()+sliderAreaRect().width())&&
	   (yy<=sliderAreaRect().y()+sliderAreaRect().height()))
   {
     // Page the slider up or down 
     if (yy<_elevator->y()+DefaultElevatorBoxHeight) _changeType=PageUp;
     else _changeType=PageDown;
   }
  else return; //... in the highlight area.  
  startDelayTimer();
  updateSliderValue();
}

void MSVScrollBar::openlookButton2Press(const XEvent *pXEvent_)
{
  int xx=pXEvent_->xbutton.x;
  int yy=pXEvent_->xbutton.y;

  _savedValue=value();
  _separation_y=0;
  _changeType=NoChange;
  
  //  Calculate whether the selection point is in the slider  
  if ((_elevator->height()==DefaultElevatorHeight)&&
      (xx>=_elevator->x())&&
      (xx<=_elevator->x()+_elevator->width())&&
      (yy>=_elevator->y()+DefaultElevatorBoxHeight)&&
      (yy<=_elevator->y()+_elevator->height()-DefaultElevatorBoxHeight))
   {
     _separation_y=yy-_elevator->y();
     _initial_y=_elevator->y();
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
     _separation_y=_elevator->height()/2;
     _initial_y=_elevator->y();
     _slidingOn=MSTrue;
     _changeType=Drag;	
   }
  motionLoop();
}

void MSVScrollBar::motionLoop(void)
{
  unsigned int mask=Button1Mask|Button2Mask;
  unsigned int keys=mask;
  int ix=0,iy=0;
  int rx=0,ry=0;
  int button_x,button_y;
  int newY,realY;
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
     newY=realY=button_y-_separation_y;
     
     if (newY<sliderAreaRect().y()) newY=sliderAreaRect().y();
     
     if (newY+_elevator->height()>sliderAreaRect().y()+sliderAreaRect().height())
      newY=sliderAreaRect().y()+sliderAreaRect().height()-_elevator->height();

     if (realY!=_initial_y)
      {
	slideVal=calcSliderValue(button_x,button_y);
	if (slideVal==value()) _changeType=NoChange;
	if (slideVal>=max()-viewSize())
	 {
	   slideVal=max()-viewSize();
	   newY=calcYValue(slideVal);
	 }
	int oldValue=value();
	_value=slideVal;    
	moveElevator(_elevator->x(),newY);
	_initial_y=_elevator->y();
        if (oldValue!=_value) drag();
      }
   }
  _elevator->unselect();
  unfreeze();
}

int MSVScrollBar::boundsCheckY(int y_)
{
  if (y_<sliderAreaRect().y()) y_=sliderAreaRect().y();
  if (y_+_elevator->height()>sliderAreaRect().y()+sliderAreaRect().height())
   { y_=sliderAreaRect().y()+sliderAreaRect().height()-_elevator->height(); }
  return y_;
}

int MSVScrollBar::calcSliderValue(int,int y_)  
{
  if (y_<=sliderAreaRect().y())
   {
     return min();
   }
  else if (y_>=sliderAreaRect().y()+sliderAreaRect().height())
   {
     return max()-viewSize();
   }
  else
   {
     double referencePoint=(double)y_-_separation_y;
     double userSize=max()-min();
     double temp;

     if (style()==MSScrollBar::Openlook)
      {
        double trueSize=sliderAreaRect().height()-_elevator->height();
        temp=referencePoint/trueSize;
      }
     else
      {
        int offset=highlightThickness()+shadowThickness();
        double trueSize=sliderAreaRect().height();
        int arrowSize=(_elevator->_arrow1!=0)?_elevator->_arrow1->height():0;
        temp=(referencePoint-arrowSize-offset)/trueSize;
      }

     temp=temp*userSize+min()+0.5;
     int intTemp=(int)temp;

     if (intTemp<min()) intTemp=min();
     else if (intTemp>max()-viewSize()) intTemp=max()-viewSize();
     return (intTemp);
   }
}

int MSVScrollBar::calcYValue(int value_)  
{
  double factor=0.0;
  double userSize=(max()-min());
  int yy=0;
  
  if (style()==MSScrollBar::Openlook)
   {
     if (userSize>0)
      {
	double trueSize=sliderAreaRect().height()-_elevator->height();
	factor=trueSize/userSize;
      }
     double slideStart=(double)(value_-min())*factor;
     yy=(int)(slideStart+0.5)+sliderAreaRect().y();
   }
  else
   {
     int offset=shadowThickness()+highlightThickness();  
     int arrowHeight=_elevator->_arrow1->height();
     if (arrowHeight==0) arrowHeight=-1;
     arrowHeight+=(offset+1);
     if (userSize>0)
      {
	double trueSize=sliderAreaRect().height();
	factor=trueSize/userSize;
      }
     double slideStart=(double)(value_-min())*factor+arrowHeight;
     yy=(int)(slideStart+0.5);
   }
  return boundsCheckY(yy);
}

void MSVScrollBar::redrawElevator(void)  
{
  if (mapped()==MSTrue&&owner()->mapped()==MSTrue)
   {
     _elevator->moveTo(_elevator->x_origin(),calcYValue(value()));
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

void MSVScrollBar::moveElevator(int x_,int y_)  
{
  if (y_!=_elevator->y())
   {
     _elevator->moveTo(x_,y_);
     drawElevatorCable();
     drawPropIndicator();
   }
}

void MSVScrollBar::drawElevatorCable(void) 
{
  if (mapped()==MSTrue&&owner()->mapped()==MSTrue)
   {
     if (style()==MSScrollBar::Openlook)
      {
	int x=((width()-MSVScrollBarDefaultCableWidth)>>1);
	int y=sliderAreaRect().y();
	int h=sliderAreaRect().height();
	
	XSetFillStyle(display(),bottomShadowGC(),FillTiled);
	XFillRectangle(display(),window(),bottomShadowGC(),x,y,MSVScrollBarDefaultCableWidth,h);
	XSetFillStyle(display(),bottomShadowGC(),FillSolid);
      }
   }
}

void MSVScrollBar::drawPropIndicator(void) 
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
	   double trueSize=sliderAreaRect().height()-_elevator->height();
	   factor=trueSize/userSize;
	 }
	double propSize=(double)(viewSize())*factor; 
	int propLength=(int)(propSize+0.5);
	int n=1;
	
	propLength=(propLength<MSVScrollBarMinPropLength)?MSVScrollBarMinPropLength:propLength;
	
	if (viewSize()>=(max()-min())) sensitive(MSFalse);
	else sensitive(MSTrue);
	
	int x=((width()-MSVScrollBarDefaultCableWidth) >> 1);
	int y=_elevator->y()+_elevator->height();
	
	rect[0].x=rect[1].x=x;
	rect[0].width=rect[1].width=MSVScrollBarDefaultCableWidth;
	
	if (value()==max()-viewSize())
	 {
	   rect[0].y=y;
	   rect[0].height=sliderAreaRect().height()+sliderAreaRect().y()-y;
	 }
	else
	 {
	   if (y+propLength>sliderAreaRect().y()+sliderAreaRect().height())
	    {
	      if (_elevator->y()-propLength>sliderAreaRect().y())
	       {
		 int h=sliderAreaRect().height()+sliderAreaRect().y()-y;
		 if (h>0)
		  {
		    n=2;	   
		    rect[1].y=_elevator->y()+_elevator->height();
		    rect[1].height=h;
		  }
		 rect[0].height=propLength-h;
		 rect[0].y=_elevator->y()-rect[0].height;
	       }   
	      else
	       {
		 rect[0].y=sliderAreaRect().y();
		 rect[0].height=sliderAreaRect().height();  
	       }
	    }
	   else
	    {
	      rect[0].height=propLength;   
	      rect[0].y=y;
	    }
	 }
	XFillRectangles(display(),window(),bottomShadowGC(),&rect[0],n);
      }
   }
}

void MSVScrollBar::drawMarkers(void) 
{
  if (mapped()==MSTrue&&owner()->mapped()==MSTrue&&markersOn()==MSTrue)
   {
     if (style()==MSScrollBar::Openlook)
      {
	int  ht=highlightThickness();
	MSRect rect(ht,ht,_elevator->width(),MSVScrollBarDefaultMarkerHeight);
	
	drawBevel(rect,MSRaised,shadowThickness());
	rect.y(height()-ht-MSVScrollBarDefaultMarkerHeight);
	drawBevel(rect,MSRaised,shadowThickness());
      }
   }
}

















