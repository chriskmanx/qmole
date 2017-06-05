///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////

#include <stdlib.h>
#include <MSGUI/MSScrollBar.H>
#include <MSGUI/MSArrow.H>

static const unsigned long MSScrollBarDefaultRepeatInterval=50;
static const unsigned long MSScrollBarDefaultInitialDelay=250;
static const unsigned long MSScrollBarEventMask=(ExposureMask|ButtonPressMask|ButtonReleaseMask);

MSScrollBar::Style MSScrollBar::_defaultStyle=MSScrollBar::Motif;

MSScrollBar::Elevator::Elevator(MSScrollBar *owner_) : MSWidgetCommon(owner_)
{
  _acceptFocus=MSFalse;
  _highlightThickness=0;
  _shadowThickness=1;
  _arrow1=0;
  _arrow2=0;
}

MSScrollBar::Elevator::~Elevator(void)
{
  if (_arrow1!=0) delete _arrow1;
  if (_arrow2!=0) delete _arrow2;
}

MSArrow *MSScrollBar::Elevator::arrow1(void) const
{ return _arrow1; } 

MSArrow *MSScrollBar::Elevator::arrow2(void) const
{ return _arrow2; }

MSScrollBar *MSScrollBar::Elevator::scrollBar(void) const
{ return (MSScrollBar *)_owner; }

void MSScrollBar::Elevator::updateBackground(unsigned long oldbg_)
{
  MSWidgetCommon::updateBackground(oldbg_);
  redraw();
}

void MSScrollBar::Elevator::configure(void) 
{ draw(); }

void MSScrollBar::Elevator::redraw(void) 
{ draw(); }

void MSScrollBar::Elevator::drawArrows(void)
{
  if (mapped()==MSTrue&&_arrow1!=0&&_arrow2!=0)
   {
     _arrow1->draw();
     _arrow2->draw();
   }
}

MSScrollBar::DelayTimer::DelayTimer(MSScrollBar *scrollBar_,unsigned long msec_) : 
MSRegularTimer(msec_,0)
{ _scrollbar=scrollBar_; }

MSScrollBar::DelayTimer::~DelayTimer(void)
{}

void MSScrollBar::DelayTimer::process(void)
{ _scrollbar->processDelayTimer(); }

MSScrollBar::RepeatTimer::RepeatTimer(MSScrollBar *scrollBar_,unsigned long interval_) : 
MSIntervalTimer(interval_)
{ _scrollbar=scrollBar_; }

MSScrollBar::RepeatTimer::~RepeatTimer(void)
{}

void MSScrollBar::RepeatTimer::process(void)
{ _scrollbar->processRepeatTimer(); }

void MSScrollBar::processRepeatTimer(void)
{ repeat(); }
void MSScrollBar::processDelayTimer(void)
{ delay(); }

MSScrollBar::PopupMenu::PopupMenu(MSScrollBar *scrollbar_) : MSPopupMenu(scrollbar_->server())
{
  new MSMenuItem(this,"Openlook",'O',Openlook);
  new MSMenuItem(this,"Motif",'M',Motif);
//  new MSMenuItem(this,"Windows",'W',Windows); -- for the future
  new MSMenuItem(this,"Home",'H',GotoTop);
  new MSMenuItem(this,"End",'E',GotoBottom);  
}

void MSScrollBar::PopupMenu::activate(void)
{
  int choice=activeMenuItem()->tag();
  done();
  if (_scrollbar!=0)
   {
     switch (choice)
      {
      case Openlook:   _scrollbar->style(MSScrollBar::Openlook); break;
      case Motif:      _scrollbar->style(MSScrollBar::Motif);    break;
      case Windows:    _scrollbar->style(MSScrollBar::Windows);  break;
      case GotoTop:    _scrollbar->value(_scrollbar->min());     break;
      case GotoBottom: _scrollbar->value(_scrollbar->max());     break;
      default:                                                   break;
      }
   }
}

void MSScrollBar::PopupMenu::scrollbar(MSScrollBar *scrollbar_)
{
  _scrollbar=scrollbar_;
  background(_scrollbar->background());
  foreground(_scrollbar->foreground());
}

MSScrollBar::MSScrollBar(MSWidget *owner_,int min_,int max_,int inc_) : 
MSPrimitive(owner_)
{
  _style=defaultStyle();
  _elevator=0;
  _cablePixmap=0;
  _min=min_;
  _max=max_;
  _inc=inc_;

  int delta=_max-_min;
  _viewSize=(int)delta/10;
  if (delta<100) _viewSize=delta<10?delta:10;
  if (viewSize()<1) _viewSize=1;
  if (viewSize()>(max()-min())) _viewSize=(max()-min());
  init();
}

MSScrollBar::~MSScrollBar(void)
{
  if (_elevator!=0)    delete _elevator;
  if (_cablePixmap!=0) delete _cablePixmap;
  if (_delayTimer!=0)  delete _delayTimer;
  if (_repeatTimer!=0) delete _repeatTimer;
}

MSScrollBar::PopupMenu *MSScrollBar::popupMenu(void)
{
  PopupMenu *menu;
  if ((menu=(PopupMenu*)server()->scrollBarMenu())==0)
   {
     menu=new PopupMenu(this);
     server()->scrollBarMenu(menu);
   }
  return menu;
}

void MSScrollBar::init(void)
{
  _acceptFocus=MSFalse;
  _changeType=NoChange;
  _pageInc=(_max>10)?10:_max;
  _value=0;
  _savedValue=0;  
  _initial_x=0;
  _initial_y=0;
  _separation_x=0;
  _separation_y=0;
  _highlightThickness=0;
  _shadowThickness=1;
  _slidingOn=MSFalse;
  _markersOn=MSTrue;
  _elevatorOn=MSTrue;
  _repeatTimer=0;
  _repeatTimer=new RepeatTimer(this,MSScrollBarDefaultRepeatInterval);
  _repeatOn=MSTrue;
  stopRepeatTimer();
  _delayTimer=0;
  if (style()==MSScrollBar::Motif)
   {
     XSetWindowBackground(display(),window(),selectShadowColor());
   }
  selectInput(MSScrollBarEventMask);
}

void MSScrollBar::firstMapNotify(void) 
{ setCablePixmap(); }

void MSScrollBar::updateBackground(unsigned long oldbg_)
{
  MSWidgetCommon::updateBackground(oldbg_); 
  _elevator->background(background());
  if (style()==MSScrollBar::Motif)
   {
     XSetWindowBackground(display(),window(),selectShadowColor());
   }
  else
   {
     XSetWindowBackground(display(),window(),background());
   }
  setCablePixmap();
  redraw(); 
}

void MSScrollBar::setCablePixmap(void)
{
  if (firstMap()==MSTrue)
   {
     if (cablePixmap()!=0) delete _cablePixmap;
     if (style()==MSScrollBar::Openlook)
      {
	_cablePixmap=new MSPixmap(server(),MSPixmap::ForegroundFiftyPixmap,
				  bottomShadowColor(),background());
	XSetTile(display(),bottomShadowGC(),cablePixmap()->pixmap());
      }
     else _cablePixmap=0;
   }
}

void MSScrollBar::style(MSScrollBar::Style style_)
{
  if (style()!=style_)
   {
     _style=style_;
     if (style()==MSScrollBar::Motif)
      {
	XSetWindowBackground(display(),window(),selectShadowColor());
      }
     else
      {
	XSetWindowBackground(display(),window(),background());
      }
     setCablePixmap();
     configure();
     XClearWindow(display(),window());
     redraw();
   }
}

void MSScrollBar::defaultStyle(MSScrollBar::Style style_)
{ _defaultStyle=style_; }

int MSScrollBar::calcSliderValue(int,int)
{ return 0; }

void MSScrollBar::inc(int inc_)      
{ if (inc_>0&&inc_<=(max()-min())) _inc=inc_; }
void MSScrollBar::pageInc(int pInc_) 
{ if (pInc_>0&&pInc_<=(max()-min())) _pageInc=pInc_; }

void MSScrollBar::viewSize(int size_)
{
  if (size_==viewSize()) return;
  int delta=(max()-min());

  if (size_<1)
   {
     if (delta<viewSize()) _viewSize=delta;    
   }
  else if (size_>delta)
   {
     if (delta<viewSize()) _viewSize=delta;    
   }
  else _viewSize=size_;
  if (value()>max()-viewSize())
   {
     _value=max()-viewSize();
     _changeType=ValueChange;
     notify(value());
   }
  configureElevator();     
  redrawElevator();
}

void MSScrollBar::view(int start_,int size_)
{
  if (value()!=start_||viewSize()!=size_)
   {
     if (start_>=min()&&start_+size_<=max())
      {
	_value=start_;
	_viewSize=size_;
	configureElevator();
	redrawElevator();
      }
   }
}

void MSScrollBar::valueChange(int value_)
{
  if (value_==value()&&value()<=max()-viewSize()) return;
  
  if (value_<min()) _value=min();   
  else if (value_>max()-viewSize()) _value=max()-viewSize();
  else _value=value_; 
  redrawElevator(); 
}

void MSScrollBar::min(int min_)
{
  if (min_!=min()&&min_<=max())
   {
     _min=min_;
     if (value()<min()) _value=min();
     configureElevator();
     redrawElevator();   
   }
}

void MSScrollBar::max(int max_)
{
  if (max_!=max()&&max_>=min()) 
   {
     _max=max_;
     if (value()>max()-viewSize()) _value=max()-viewSize();
     configureElevator();
     redrawElevator();   
   }
}

void MSScrollBar::value(int value_)
{
  if (value_!=value())
   {
     valueChange(value_);
     _changeType=ValueChange;
     notify(value()); // invoke callback
   }
}

void MSScrollBar::expose(const XEvent *pEvent_)
{
  if (mapped()==MSTrue&&owner()->mapped()==MSTrue&&frozen()==MSFalse)
   {
     int offset=highlightThickness()+shadowThickness();
     if (pEvent_->xexpose.width==width()||pEvent_->xexpose.height==height())
      {
	redraw();
      }
     else if ((pEvent_->xexpose.x>=sliderAreaRect().x())&&
	 (pEvent_->xexpose.y>=sliderAreaRect().y())&&
	 (pEvent_->xexpose.width<=sliderAreaRect().width())&&
	 (pEvent_->xexpose.height<=sliderAreaRect().height()))
      {
	// nothing to do at this time
      }
     else
      {
	if (style()==MSScrollBar::Openlook)
	 {
	   drawMarkers();
	   drawElevatorCable();
	   drawPropIndicator();
	 }
	else
	 {
	   drawArrows();
	   drawSunken();	
	 }
      }
   }
}

void MSScrollBar::redraw(void)
{
  if (mapped()==MSTrue&&owner()->mapped()==MSTrue&&frozen()==MSFalse)
   {
     drawMarkers();
     redrawElevator();
     drawSunken();
   }
}

void MSScrollBar::drawElevator(void) 
{ if (mapped()==MSTrue&&owner()->mapped()==MSTrue) _elevator->draw(); }

void MSScrollBar::drawArrows(void) 
{
  if (style()==MSScrollBar::Motif&&mapped()==MSTrue&&owner()->mapped()==MSTrue)
   {
     _elevator->drawArrows();
   }
}

void MSScrollBar::configure(void)
{
  switch (style())
   {
   case Openlook: configureForOpenlook(); break;
   case Motif:    configureForMotif();    break;
   case Windows:  configureForWindows();  break;
   }
}

void MSScrollBar::configureForWindows(void)
{ configureForMotif(); }

void MSScrollBar::updateSliderValue(void)
{
  int changeAmount=0;
  int tempValue;
  int savedValue=value();

  switch (changeType())
   {
   case Inc:         changeAmount=inc();                      break;   
   case Dec:         changeAmount=-inc();                     break;   
   case PageDown:    changeAmount=pageInc();                  break;   
   case PageUp:      changeAmount=-pageInc();                 break;   
   case Home:        changeAmount=min()-value();              break;
   case End:         changeAmount=(max()-viewSize())-value(); break;
   case NoChange:
   case Drag:
   case ValueChange: changeAmount=0;                          break;
   }

  //  tempValue contains the possible new scroll bar value  
  //  Depending on the change type,adjust this new value    
  //  if it extends beyound the bounds of the value range.   

  tempValue=value()+changeAmount;
  
  if (changeType()==Inc||changeType()==PageDown)
   {
     if (tempValue>max()-viewSize()) tempValue=max()-viewSize();
     if (tempValue<=value()) return;
   }
  else if (changeType()==Dec||changeType()==PageUp)
   {
     if (tempValue<min()) tempValue=min();
     if (tempValue>=value()) return;
   }
  else 
   {
     if (tempValue>max()-viewSize()) tempValue=max()-viewSize();
     if (tempValue<min()) tempValue=min();
   }
  if (tempValue!=_savedValue)
   {
     _value=tempValue; // force the slider to be redrawn
     redrawElevator();
     // need to warp pointer when inc and dec'ing
     if (changeType()==Inc||changeType()==Dec) adjustPointer(); 
     notify(value());  // drive callbacks
   }
}

void MSScrollBar::release(void)
{
  if (_elevator==0) return;
  if (_elevator->_arrow1->selected()==MSTrue) _elevator->_arrow1->select(MSFalse);
  if (_elevator->_arrow2->selected()==MSTrue) _elevator->_arrow2->select(MSFalse);
  if (_delayTimer!=0)
   {
     _delayTimer->stop();
     _delayTimer=0;
   }
  stopRepeatTimer();
  if (slidingOn()==MSTrue&&changeType()==Drag)
   {
     _changeType=ValueChange;
     _slidingOn=MSFalse;
     notify(_value);
   }
  _slidingOn=MSFalse;  
  _changeType=NoChange;
}

void MSScrollBar::startDelayTimer(void)
{
  if (_delayTimer!=0)
   {
     _delayTimer->stop();
     _delayTimer=0;
   }
  _delayTimer=new DelayTimer(this,MSScrollBarDefaultInitialDelay);
}

void MSScrollBar::startRepeatTimer(void)
{
  if (repeatOn()!=MSTrue)
   {
     _repeatOn=MSTrue;
     _repeatTimer->reset();
   }
}

void MSScrollBar::stopRepeatTimer(void)
{
  if (repeatOn()==MSTrue)
   {
     _repeatTimer->stop();
     _repeatOn=MSFalse;
   }
}

void MSScrollBar::increment(void)
{ activateCallback(MSWidgetCallback::increment); change(); }
void MSScrollBar::decrement(void)
{ activateCallback(MSWidgetCallback::decrement); change(); }
void MSScrollBar::pageDown(void)
{ activateCallback(MSWidgetCallback::pagedecrement); change(); }
void MSScrollBar::pageUp(void)
{ activateCallback(MSWidgetCallback::pageincrement); change(); }
void MSScrollBar::drag(void)
{ activateCallback(MSWidgetCallback::drag); change(); }
void MSScrollBar::home(void)
{ activateCallback(MSWidgetCallback::home); change(); }
void MSScrollBar::end(void)
{ activateCallback(MSWidgetCallback::end); change(); }
void MSScrollBar::change(void)
{ activateCallback(MSWidgetCallback::valuechange); }

void MSScrollBar::notify(int)
{
  switch(changeType())
   {
   case Inc:         increment(); break;   
   case Dec:         decrement(); break;   
   case PageDown:    pageDown();  break;   
   case PageUp:      pageUp();    break;   
   case Drag:        drag();      break;   
   case ValueChange: change();    break;
   case Home:        home();      break;
   case End:         end();       break;
   case NoChange:                 break;
   }
}

void MSScrollBar::repeat(void)
{
  unsigned int mask=Button1Mask;
  unsigned int keys=mask;
  int ix=0,iy=0;
  int rx=0,ry=0;
  Window root,child;
  
  XQueryPointer(display(),window(),&root,&child,&rx,&ry,&ix,&iy,&keys);
  if ((keys&mask)==mask) updateSliderValue(); 
  else release();
}

void MSScrollBar::delay(void)
{
  _delayTimer=0;
  startRepeatTimer();
}

void MSScrollBar::buttonPress(const XEvent *pXEvent_)
{ if (isProtected()==MSFalse) buttonPressNotify(this,pXEvent_); }

void MSScrollBar::buttonRelease(const XEvent *pXEvent_)
{ 
  if (isProtected()==MSFalse)
   {
     if (pXEvent_->xbutton.button==Button1||pXEvent_->xbutton.button==Button2) release(); 
   }
}

void MSScrollBar::button1Press(const XEvent *pXEvent_)
{
  switch (style())
   {
   case Openlook: openlookButton1Press(pXEvent_); break;
   case Windows:  
   case Motif:    motifButton1Press(pXEvent_);    break;
   }
}

void MSScrollBar::button2Press(const XEvent *pXEvent_)
{
  switch (style())
   {
   case Openlook: openlookButton2Press(pXEvent_); break;
   case Windows:  
   case Motif:    motifButton2Press(pXEvent_);    break;
   }
}

void MSScrollBar::button3Press(const XEvent *)
{
  int xx,yy;
  pointerXY(xx,yy);
  PopupMenu *menu=popupMenu(); // force creation of popupMenu - this is ugly
  menu->scrollbar(this);
  menu->moveTo(xx,yy);
  menu->show();
}

// #########################################################
// default virtual methods - prevents gratuitous inlining
// #########################################################

void MSScrollBar::Elevator::draw(void) {}
void MSScrollBar::Elevator::select(void) {}
void MSScrollBar::Elevator::unselect(void) {}

void MSScrollBar::redrawElevator(void) {}
void MSScrollBar::configureElevator(void) {}  
void MSScrollBar::drawElevatorCable(void) {}
void MSScrollBar::drawPropIndicator(void) {}
void MSScrollBar::drawMarkers(void) {}
void MSScrollBar::adjustPointer(void) {}
void MSScrollBar::calcPropIndicator(void) {}
void MSScrollBar::openlookButton1Press(const XEvent*) {}
void MSScrollBar::openlookButton2Press(const XEvent*) {}
void MSScrollBar::motifButton1Press(const XEvent*) {}
void MSScrollBar::motifButton2Press(const XEvent*) {}
void MSScrollBar::configureForOpenlook(void) {}
void MSScrollBar::configureForMotif(void) {}

// #########################################################
// inline methods
// #########################################################

int MSScrollBar::min(void) const
{ return _min; }
int MSScrollBar::max(void) const
{ return _max; }
int MSScrollBar::inc(void) const
{ return _inc; }
int MSScrollBar::pageInc(void) const
{ return _pageInc; }
int MSScrollBar::value(void) const
{ return _value; }
int MSScrollBar::viewSize(void) const
{ return _viewSize; }

MSScrollBar::Style MSScrollBar::style(void) const
{ return _style; }
MSScrollBar::Style MSScrollBar::defaultStyle(void)
{ return _defaultStyle; }
MSRect& MSScrollBar::sliderAreaRect(void)
{ return _sliderAreaRect; }
MSPixmap *MSScrollBar::cablePixmap(void) const
{ return _cablePixmap; }
MSScrollBar::Elevator *MSScrollBar::elevator(void) const
{ return _elevator; }
MSScrollBar::Change MSScrollBar::changeType(void) const
{ return _changeType; }
MSTimer *MSScrollBar::delayTimer(void) const
{ return _delayTimer; }
MSTimer *MSScrollBar::repeatTimer(void) const
{ return _repeatTimer; }
MSBoolean MSScrollBar::repeatOn(void) const
{ return _repeatOn; }
MSBoolean MSScrollBar::slidingOn(void) const
{ return _slidingOn; }
MSBoolean MSScrollBar::markersOn(void) const
{ return _markersOn; }
MSBoolean MSScrollBar::elevatorOn(void) const
{ return _elevatorOn; }


