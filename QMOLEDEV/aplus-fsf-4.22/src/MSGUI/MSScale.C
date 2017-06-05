///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////

#include <stdlib.h>
#include <MSGUI/MSScale.H>
#include <MSTypes/MSMessageLog.H>

static const unsigned long DefaultEventMask=ExposureMask|ButtonPressMask|ButtonReleaseMask|KeyReleaseMask;
static const unsigned long DefaultRepeatInterval    =50;
static const unsigned long DefaultInitialDelay      =250;
static const int       	   DefaultMinorTickCount    =1;
static const int           DefaultMinorTickSize     =6;
static const int           DefaultMajorTickSize     =10;
extern const int           SliderAreaShadowThickness=2;

MSLabelFormat MSScale::_outFormat;
MSLabelOutPtr    MSScale::_defaultLabelOut(new MSLabelOut(),MSInit);

// #########################################################
// DelayTimer
// #########################################################
MSScale::DelayTimer::DelayTimer(MSScale *s_,unsigned long ms_):MSRegularTimer(ms_,0)
{ _scale=s_;}

MSScale::DelayTimer::~DelayTimer(void)
{}

void MSScale::DelayTimer::process(void)
{ _scale->processDelayTimer(); }

// #########################################################
// RepeatTimer
// #########################################################
MSScale::RepeatTimer::RepeatTimer(MSScale *s_,unsigned long interval_):MSIntervalTimer(interval_)
{ _scale=s_;}

MSScale::RepeatTimer::~RepeatTimer(void)
{}

void MSScale::RepeatTimer::process(void)
{ _scale->processRepeatTimer(); }

// #########################################################
// Editor
// #########################################################
MSScale::Editor::Editor(MSWidget *owner_):MSTextField(owner_)
{
  _highlightThickness=0;
  _shadowThickness=0;
  margin(0);  
}

MSScale::Editor::~Editor(void)
{}

void MSScale::Editor::activate(void)
{ ((MSScale *)owner())->editorActivate(); }
void MSScale::Editor::escape(void)
{ ((MSScale *)owner())->editorEscape(); }

// #########################################################
// ValueWin
// #########################################################
MSScale::ValueWin::ValueWin(MSWidget *owner_):MSWidgetCommon(owner_)
{ 
  _highlightThickness=0;
  _shadowThickness=0;
  _offset=0;
  _x_org=0;
  _y_org=0;
  sensitive(MSFalse);
  
  XGCValues values;    
  values.foreground=foreground();
  values.background=background();
  values.font=font();
  _gc=XCreateGC(display(),window(),(GCForeground|GCBackground|GCFont),&values);
  _fontInfo=(XFontStruct *)server()->fontStruct(font());
//  backingStore(WhenMapped);
  selectInput(ExposureMask);
}

MSScale::ValueWin::~ValueWin(void)
{ XFreeGC(display(),_gc); }

void MSScale::ValueWin::updateValue(const char *pString_)
{
  drawBackground();
  if (pString_!=0)
   {
     MSScale *scale=(MSScale *)owner();
     unsigned long align=scale->valueAlignment();
     int ww=XTextWidth(fontInfo(),pString_,strlen(pString_));
     int xx=align&MSLeft?0:align&MSRight?width()-ww:(width()-ww)/2;
     XDrawString(display(),window(),gc(),fontInfo(),xx,fontInfo()->ascent,pString_,strlen(pString_));
   }
}

void MSScale::ValueWin::updateFont(Font oldfid_)            
{ 
  MSWidgetCommon::updateFont(oldfid_);
  _fontInfo=(XFontStruct *)server()->fontStruct(font());
  XSetFont(display(),_gc,font()); 
}

void MSScale::ValueWin::updateForeground(unsigned long oldfg_)
{ 
  MSWidgetCommon::updateForeground(oldfg_); 
  XSetForeground(display(),gc(),foreground());
}

void MSScale::ValueWin::redraw(void)
{
  MSScale *scale=(MSScale *)owner();
  MSString buffer;
  updateValue(scale->formatValue(buffer,scale->currentValue()));
}


// #########################################################
// Slider
// #########################################################
MSScale::Slider::Slider(MSWidget *owner_):MSWidgetCommon(owner_)
{ 
  _gc=XCreateGC(display(),window(),0,0);
  _highlightThickness=0;
  _shadowThickness=2;
  shadowStyle(MSRaised);
  saveUnder(MSTrue);
//  backingStore(Always);
  selectInput(ExposureMask);
}

MSScale::Slider::~Slider(void)
{ XFreeGC(display(),_gc); }

void MSScale::Slider::setValueWinYCoord(int y_)
{
  MSScale::ValueWin *win=scale()->valueWin();
  win->moveTo(win->x_org(),y_+(height()-win->height())/2);
}

void MSScale::Slider::setValueWinXCoord(int x_)
{
  MSScale::ValueWin *win=scale()->valueWin();
  win->moveTo(x_+win->offset(),win->y_org());
}

void MSScale::Slider::button1Press(const XEvent *event_) 
{ if (event_->xbutton.button==Button1) scale()->moveSlider(event_); }

void MSScale::Slider::adjustSize(int w_,int h_) 
{
  if (width()!=w_||height()!=h_)
   {     
     MSRect::width(w_<=0?1:w_>=USHRT_MAX?width():w_);
     MSRect::height(h_<=0?1:h_>=USHRT_MAX?height():h_);
     if (window()!=0)
      {
        selectInput();
        XResizeWindow(display(),window(),width(),height());
        redraw();
        selectInput(ExposureMask);        
      }
   }
}

void MSScale::Slider::configure(void)
{ if (mapped()==MSTrue) redraw(); }
void MSScale::Slider::redraw(void)
{
  if (mapped()==MSTrue)
   {
     drawShadow();
     ((MSScale*)owner())->drawSliderEtch();
     XFlush(display());
   }
   ((MSScale*)owner())->sliderRedrawNotify();
}

void MSScale::Slider::drawSliderEtch(void) {}
void MSScale::Slider::moveTo(int,int) {}

// #########################################################
// MSScale
// #########################################################

MSScale::MSScale(MSWidget *owner_,const char *title_):MSPrimitive(owner_,title_) 
{
  init();
  _modelType=MSFloat::symbol();
  internalCouple(new MSFloat);
}

MSScale::MSScale(MSWidget *owner_,const MSStringVector& title_):MSPrimitive(owner_,title_) 
{
  init();
  _modelType=MSFloat::symbol();
  internalCouple(new MSFloat);
}

MSScale::MSScale(MSWidget *owner_,MSFloat& model_,const char *title_):MSPrimitive(owner_,title_)
{
  init();
  _modelType=MSFloat::symbol();
  couple(&model_);
}

MSScale::MSScale(MSWidget *owner_,MSFloat& model_,const MSStringVector& title_):MSPrimitive(owner_,title_)
{
  init();
  _modelType=MSFloat::symbol();
  couple(&model_);
}

MSScale::MSScale(MSWidget *owner_,MSInt& model_,const char *title_):MSPrimitive(owner_,title_)
{
  init();
  _modelType=MSInt::symbol();
  couple(&model_);
  format(MSFormat(MSInt::WithoutCommas));
}

MSScale::MSScale(MSWidget *owner_,MSInt& model_,const MSStringVector& title_):MSPrimitive(owner_,title_)
{
  init();
  _modelType=MSInt::symbol();
  couple(&model_);
  format(MSFormat(MSInt::WithoutCommas));
}

void MSScale::init(void)
{
  freeze();
  _base=0;
  _scale=0;
  _valueMin=0.;
  _valueMax=100.;
  _valueInc=1.;
  _valuePageInc=10.;
  _valueAlignment=(unsigned long)(MSCenter|MSTop);
  
  _titleAlignment=(unsigned long)(MSCenter|MSTop);
  _subtitleFont=MSPrimitive::font();
  _subtitleForeground=MSPrimitive::foreground();
  _subtitleAlignment=_titleAlignment;

  _mintitleForeground=MSPrimitive::foreground();
  _mintitleAlignment=(unsigned long)MSCenter;
  _mintitleHeight=0;
  _mintitleWidth=0;
  _mintitleFont=MSPrimitive::font();

  _maxtitleForeground=MSPrimitive::foreground();
  _maxtitleAlignment=_mintitleAlignment;
  _maxtitleHeight=0;
  _maxtitleWidth=0;
  _maxtitleFont=MSPrimitive::font();

  _labelForeground=MSPrimitive::foreground();
  _labelFont=MSPrimitive::font();
  _labelInc=0.;
  _labelSpacing=2;
  _labelOffset=0;
  _labelOut=_defaultLabelOut;

  _majorTickSize=DefaultMajorTickSize;
  _minorTickSize=DefaultMinorTickSize;
  _minorTickCount=DefaultMinorTickCount;
  _internalUpdateStatus=MSFalse;

  _gc=XCreateGC(display(),window(),0,0);
  _valueWin=new ValueWin(this);
  _editor=new Editor(this);
  _repeatTimer=new RepeatTimer(this,DefaultRepeatInterval);
  _repeatOn=MSTrue;
  _delayTimer=0;
  stopRepeatTimer();
  _shadowThickness=0;
  _highlightThickness=2;
  shadowStyle(MSRaised);
  acceptFocus(MSTrue);
  selectInput(DefaultEventMask);
  backingStore(WhenMapped);
  addToFocusList();
  format(MSFormat(MSFloat::Decimal2));
}

MSScale::~MSScale(void) 
{
  freeze();
  if (_delayTimer!=0)  delete _delayTimer;
  if (_repeatTimer!=0) delete _repeatTimer;
  if (_slider!=0)      delete _slider;
  if (_valueWin!=0)    delete _valueWin;
  if (_editor!=0)      delete _editor;
  if (_gc!=0)          XFreeGC(display(),_gc);
}

int MSScale::sliderSize(void) const
{return 0;}

void MSScale::editorActivate(void)
{ 
  if (editor()->mapped()==MSTrue)
   {
     if (validate((char *)editor()->string())==MSTrue) editorEscape(); 
   }
}

void MSScale::editorEscape(void)
{ 
  if (editor()->mapped()==MSTrue) 
   {
     focusOutNotify(editor());
     editor()->unmap(); 
   }
}

MSBoolean MSScale::validate(const char *pString_)
{
  MSBoolean status=MSFalse;
  if (MSView::model()!=0)
   {
     if (modelType()==MSFloat::symbol())
      {
        MSFloat temp;
        if (temp.set(pString_)==MSError::MSSuccess)
         {
           *((MSFloat *)MSView::model())=temp;
           status=MSTrue; 
         }
      }
     else if (modelType()==MSInt::symbol())
      {
        MSInt temp;
        if (temp.set(pString_)==MSError::MSSuccess)
         {
           *((MSInt *)MSView::model())=temp;
           status=MSTrue;
         }
      }
   }
  return status;
}

void MSScale::model(const MSFloat& model_)
{
  _modelType=MSFloat::symbol();
  constCouple(&model_);
  if (format().formatType()!=MSFormat::Float)
   {
     format(MSFormat(MSFloat::Decimal2));
     computeSize();
     redraw();
   }
}

void MSScale::model(const MSInt& model_)
{
  _modelType=MSInt::symbol();
  constCouple(&model_);
  if (format().formatType()!=MSFormat::Int)
   {
     format(MSFormat(MSInt::WithoutCommas));
     computeSize();
     redraw();
   }
}

void MSScale::model(MSFloat& model_)
{
  _modelType=MSFloat::symbol();
  couple(&model_);
  if (format().formatType()!=MSFormat::Float)
   {
     format(MSFormat(MSFloat::Decimal2));
     computeSize();
     redraw();
   }
}

void MSScale::model(MSInt& model_)
{
  _modelType=MSInt::symbol();
  couple(&model_);
  if (format().formatType()!=MSFormat::Int)
   {
     format(MSFormat(MSInt::WithoutCommas));
     computeSize();
     redraw();
   }
}

void MSScale::firstMapNotify(void)
{
  freeze(); 
  computeSize(); 
  unfreeze();
}

void MSScale::configure(void) 
{
  redraw();
  if (editor()->mapped()==MSTrue) 
   { editor()->moveTo(valueWin()->x_origin(),valueWin()->y_origin()); }
}

void MSScale::processRepeatTimer(void) 
{ 
  unsigned int mask=Button1Mask|(unsigned int)KeyPressMask;
  unsigned int keys=mask;
  int ix=0,iy=0;
  int rx=0,ry=0;
  Window root,child;
  
  XQueryPointer(display(),window(),&root,&child,&rx,&ry,&ix,&iy,&keys);
  if (keys&mask) updateSliderValue(); 
}

void MSScale::processDelayTimer(void)
{ 
  _delayTimer=0;
  startRepeatTimer();
}

void MSScale::editValue(const char *pString_)
{
  if (MSView::model()!=0)
   {
     editor()->color(background(),foreground());
     editor()->font(valueWin()->font());
     editor()->resize(valueWin()->width(),valueWin()->height());
     editor()->moveTo(valueWin()->x_origin(),valueWin()->y_origin());
     if (pString_!=0) editor()->string(pString_);
     editor()->map();
     editor()->raise();
     focusInNotify(editor());
   }
}

MSBoolean MSScale::assignValue(double x_)
{
  if (MSView::model()!=0)
   {
     double value=x_>valueMax()?valueMax():x_<valueMin()?valueMin():x_;
     if (modelType()==MSFloat::symbol()) *((MSFloat *)MSView::model())=value;
     else if (modelType()==MSInt::symbol()) *((MSInt *)MSView::model())=(int)(value);
     return MSTrue;
   }
  return MSFalse;
}

double MSScale::currentValue(void)               
{
  double value=0.0;
  if (MSView::model()!=0)
   {
     if (modelType()==MSFloat::symbol()) value=(double)*((MSFloat*)MSView::model());
     else if (modelType()==MSInt::symbol()) value=(double)*((MSInt*)MSView::model());
   }
  return value>valueMax()?valueMax():value<valueMin()?valueMin():value;
}

double MSScale::currentValue(void) const
{
  double value=0.0;
  if (MSView::model()!=0)
   {
     if (modelType()==MSFloat::symbol()) value=(double)*((MSFloat *)MSView::model());
     else if (modelType()==MSInt::symbol()) value=(double)*((MSInt *)MSView::model());
   }
  return value>valueMax()?valueMax():value<valueMin()?valueMin():value;
}

void MSScale::setValue(double x_)              
{
  internalUpdateStatus(MSTrue); 
  assignValue(x_);
}

void MSScale::value(const MSFloat& v_)
{ setValue(v_); }

void MSScale::value(const MSInt& v_)
{ setValue(v_); }

void MSScale::value(double v_)
{ setValue(v_); }

void MSScale::value(int v_)
{ setValue((double)v_); }

void MSScale::update(const MSIndexVector&)
{
  if (internalUpdateStatus()==MSFalse)
   {
     double value=currentValue();
     setSliderPosition(valueToPixel(value));
   }
  else internalUpdateStatus(MSFalse);
  MSString buffer;
  valueWin()->updateValue(formatValue(buffer,currentValue()));
}

void MSScale::buttonPress(const XEvent *event_)
{
  if (isProtected()==MSFalse)
   {
#ifndef MS_WINDOWS     
     selectInput(DefaultEventMask^ExposureMask);
#endif     
     MSBoolean focus=traverseFocus(this);
     Window sw=event_->xbutton.subwindow;
     XEvent *ev=(XEvent *)event_;
     if (sw==slider()->window()&&editor()->mapped()==MSFalse) 
      {
	ev->xbutton.x-=slider()->x_origin();
	ev->xbutton.y-=slider()->y_origin();	
	buttonPressNotify(slider(),ev);
      }
     else if (focus==MSTrue&&sw==valueWin()->window()&&valueWin()->sensitive()==MSTrue) 
      {
	ev->xbutton.x-=valueWin()->x_origin();
	ev->xbutton.y-=valueWin()->y_origin();	
	MSString buffer;
	if (editor()->mapped()==MSFalse) editValue(formatValue(buffer,currentValue()));
	buttonPressNotify(editor(),ev);
      }
     else if (focus==MSTrue&&sw==editor()->window()) 
      {
	ev->xbutton.x-=editor()->x_origin();
	ev->xbutton.y-=editor()->y_origin();	
	buttonPressNotify(editor(),ev);
      }
     else if (editor()!=0&&editor()->mapped()!=MSTrue) buttonPressNotify(this,event_);
   }
}

void MSScale::button1Press(const XEvent *event_) 
{
  int bx=event_->xbutton.x;
  int by=event_->xbutton.y;
  int x=slider()->x_origin();
  int y=slider()->y_origin();
  if (bx>x_org()&&bx<x_end()&&by>y_org()&&by<y_end())
   {
     valueChange(incFactor(bx,by)*valueInc());
     startDelayTimer();
     updateSliderValue();
   }
}

void MSScale::button2Press(const XEvent *event_) 
{
  int bx=event_->xbutton.x;
  int by=event_->xbutton.y;
  if (bx>x_org()&&bx<x_end()&&by>y_org()&&by<y_end()) moveSlider(event_);
}

void MSScale::buttonRelease(const XEvent *event_)
{
  if (event_->xbutton.button!=Button1&&event_->xbutton.button!=Button2) return;
  if (_delayTimer!=0)
   {
     _delayTimer->stop();
     _delayTimer=0;
   }
  stopRepeatTimer();
  selectInput(DefaultEventMask);
}


void MSScale::keyRelease(const XEvent *e_,KeySym keysym_,unsigned int state_,const char *pString_)
{ MSPrimitive::keyRelease(e_,keysym_,state_,pString_); }

void MSScale::keyPress(const XEvent *e_,KeySym keysym_,unsigned int state_,const char *pString_)
{ 
  if (isProtected()==MSFalse&& hasModel()==MSTrue)
   {
     MSKeyPress keyPress(keysym_, state_);
     if(keyTranslate(keyPress)==MSTrue)
       {
	 if (editor()->mapped()==MSTrue) keyPressNotify(editor(),e_,keysym_,state_,pString_);
	 else
	   {
	     switch(keysym_)
	       {
	       case XK_Home:
	       case XK_F27:   home();      break;
	       case XK_End:
	       case XK_R13:   end();       break;
	       case XK_Prior:
	       case XK_F29:   pageUp();    break;
	       case XK_Next:
	       case XK_F35:   pageDown();  break;
	       case XK_Left:  left();      break;
	       case XK_Right: right();     break;
	       case XK_Up:    up();        break;
	       case XK_Down:  down();      break;
	       default:                    
		 editor()->string("");
		 keyPressNotify(editor(),e_,keysym_,state_,pString_);
		 if (editor()->length()>0) editValue(0);
		 break;
	       }
	   }
       }
   }
}

void MSScale::home(void)     { assignValue(valueMin()); }
void MSScale::end(void)      { assignValue(valueMax()); }
void MSScale::up(void)       { valueChange(valueInc());      startTimedUpdate(); }
void MSScale::down(void)     { valueChange(-valueInc());     startTimedUpdate(); }
void MSScale::left(void)     { valueChange(-valueInc());     startTimedUpdate(); }
void MSScale::right(void)    { valueChange(valueInc());      startTimedUpdate(); }
void MSScale::pageUp(void)   { valueChange(valuePageInc());  startTimedUpdate(); }
void MSScale::pageDown(void) { valueChange(-valuePageInc()); startTimedUpdate(); }

void MSScale::keyRelease(const XEvent *)
{
  if (_delayTimer!=0)
   {
     _delayTimer->stop();
     _delayTimer=0;
   }
  stopRepeatTimer();
}

void MSScale::startTimedUpdate(void)
{
  startDelayTimer();
  updateSliderValue();
}

void MSScale::startDelayTimer(void)
{
  if (delayTimer()!=0)
   {
     delayTimer()->stop();
     _delayTimer=0;
   }
  _delayTimer=new DelayTimer(this,DefaultInitialDelay);
}

void MSScale::startRepeatTimer(void)
{
  if (_repeatOn!=MSTrue)
   {
     _repeatOn=MSTrue;
     _repeatTimer->reset();
   }
}

void MSScale::stopRepeatTimer(void)
{
  if (_repeatOn==MSTrue)
   {
     _repeatTimer->stop();
     _repeatOn=MSFalse;
   }
}

void MSScale::updateSliderValue(void)
{
  if (MSView::model()!=0)
   {
     double x=currentValue()+valueChange();
     x=x>valueMax()?valueMax():x<valueMin()?valueMin():x;
     assignValue(x);
   }
}

void MSScale::redraw(void)
{
  if (mapped()==MSTrue&&frozen()==MSFalse)
   {
     if (highlighted()==MSTrue) drawHighlight();
     drawBackground();
     drawShadow();
     computeValueWinSize();
     computeLabelOffset();
     computeSliderAreaSize();
     computeSliderScale();
     computeTickInc();
     drawTitles();
     drawSliderTitles();
     drawTickLabels();
     drawSliderArea();
     drawSubWindows();
   }
}

void MSScale::drawSubWindows(void)
{
  if (outputMode()==Draw)
   {
     setSliderPosition(valueToPixel(currentValue()));
     slider()->map();
     slider()->raise();
     slider()->redraw();
     if (valueAlignment()!=MSNone)
      {
        valueWin()->map();
        valueWin()->raise();
      }
     else valueWin()->unmap();
     MSString buffer;
     valueWin()->updateValue(formatValue(buffer,currentValue()));
     if (editor()->mapped()==MSTrue) editor()->raise();
   }
}

void MSScale::computeValueWinSize(void)
{
  int wmin,wmax;
  MSString buffer;
  formatValue(buffer,valueMin());
  wmin=XTextWidth(valueWin()->fontInfo(),buffer,buffer.length());
  formatValue(buffer.removeAll(),valueMax());
  wmax=XTextWidth(valueWin()->fontInfo(),buffer,buffer.length());

  formatValue(buffer.removeAll(),currentValue());
  int w=XTextWidth(valueWin()->fontInfo(),buffer,buffer.length());
  w=(w=w>wmin?w:wmin)>wmax?w:wmax;
  valueWin()->offset((slider()->width()-w)/2);
  valueWin()->resize(w,valueWin()->textHeight());
}

void MSScale::drawSliderArea(void)
{
   XFillRectangle(display(),window(),selectShadowGC(),
 		 x_org(),y_org(),sliderAreaRect().width(),sliderAreaRect().height());
   drawBevel(_sliderAreaRect,MSSunken,SliderAreaShadowThickness);
}

void MSScale::drawTitles(void)
{
  int offset=highlightThickness()+shadowThickness();
  int x,len,y=MSBottom&titleAlignment()?height()-titleHeight():offset;
  XFontStruct *fontInfo;
  GC gc=XCreateGC(display(),window(),0,0);
  char *pString;
  
  if (title().maxLength()>0) 
   {
     fontInfo=(XFontStruct *)server()->fontStruct(titleFont());
     XSetFont(display(),gc,titleFont());
     XSetForeground(display(),gc,titleForeground());
     y+=fontInfo->ascent;
     for (int i=0;i<title().length();i++) 
      {
        len=title()[i].length();
        pString=(char *)title()[i].string();
	x=MSLeft&titleAlignment()?x_org():(MSRight&titleAlignment()?
	    x_end()-XTextWidth(fontInfo,pString,len):(width()-XTextWidth(fontInfo,pString,len))/2);
	XDrawString(display(),window(),gc,fontInfo,x,y,pString,len);
	y+=fontInfo->ascent+fontInfo->descent;
      }
     y-=fontInfo->ascent;
   }
  if (subtitle().maxLength()>0)
   {
     fontInfo=(XFontStruct *)server()->fontStruct(subtitleFont());
     XSetFont(display(),gc,subtitleFont());
     XSetForeground(display(),gc,subtitleForeground());
     y+=fontInfo->ascent;
     for (int i=0;i<subtitle().length();i++) 
      {
	len=subtitle()[i].length();
	pString=(char *)subtitle()[i].string();
	x=MSLeft&subtitleAlignment()?x_org():(MSRight&subtitleAlignment()?
	  x_end()-XTextWidth(fontInfo,pString,len):(width()-XTextWidth(fontInfo,pString,len))/2);
	XDrawString(display(),window(),gc,fontInfo,x,y,pString,len);
	y+=fontInfo->ascent+fontInfo->descent;
      }
   }
  XFreeGC(display(),gc);
}

void MSScale::print(const char *file_)
{
  MSBoolean fileOpen=MSFalse;
  busyOn();
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
  redraw();
  if (slider()->mapped()==MSTrue)
   {
     displayPrintOriginInc(slider());
     slider()->redraw();
     displayPrintOriginDec(slider());
     if (valueWin()->mapped()==MSTrue)
      {
	displayPrintOriginInc(valueWin());
	MSString buffer;
	valueWin()->updateValue(formatValue(buffer,currentValue()));
	displayPrintOriginDec(valueWin());
      }
   }
  if (fileOpen==MSTrue) 
   {
     displayPrintClose();
     outputMode(Draw);
   }
  busyOff();
}

void MSScale::updateTitle(void)
{
  computeSize();
  redraw(); 
}

void MSScale::updateBackground(unsigned long oldbg_)
{ 
  if (slider()->background()==oldbg_)   slider()->background(background());
  if (valueWin()->background()==oldbg_) valueWin()->background(background());
  MSPrimitive::updateBackground(oldbg_);
  redraw(); 
}

void MSScale::updateForeground(unsigned long oldfg_)
{ 
  if (valueWin()->foreground()==oldfg_) valueWin()->foreground(foreground());
  if (labelForeground()==oldfg_)        _labelForeground=foreground();
  if (subtitleForeground()==oldfg_)     _subtitleForeground=foreground();
  if (mintitleForeground()==oldfg_)     _mintitleForeground=foreground();
  if (maxtitleForeground()==oldfg_)     _maxtitleForeground=foreground();
  MSPrimitive::updateForeground(oldfg_);
  XSetForeground(display(),gc(),foreground());
  redraw(); 
}

void MSScale::updateFont(Font oldfid_)
{
  Font fid=MSWidget::font();
  if (fid!=oldfid_)
   { 
     if (oldfid_==subtitleFont()) _subtitleFont=fid;
     if (oldfid_==mintitleFont()) _mintitleFont=fid;
     if (oldfid_==maxtitleFont()) _maxtitleFont=fid;
     if (oldfid_==labelFont()) _labelFont=fid;
     if (oldfid_==valueFont()) valueWin()->font(fid);
     computeSize();
     redraw(); 
   }
}

void MSScale::focusIn(void) 
{
  highlight();
  if (editor()->mapped()==MSTrue) focusInNotify(editor());
}

void MSScale::focusOut(void) 
{
  unHighlight();
  if (editor()->mapped()==MSTrue) focusOutNotify(editor());
}

MSBoolean MSScale::loseFocus(void) 
{
  if (editor()->mapped()==MSTrue) editorActivate(); 
  if (editor()->mapped()==MSTrue) return MSFalse;
  else
   {
     unHighlight();
     return MSTrue;
   }
}


void MSScale::set(MSAttrValueList& avList_)
{
  MSPrimitive::set(avList_);
  MSIndexVector index;
  for (unsigned i=0;i<avList_.length();i++)
   {
     if (avList_[i].attribute()=="labelFont") labelFont(avList_[i].value()),index<<i;  
     else if (avList_[i].attribute()=="labelForeground") labelForeground(avList_[i].value()),index<<i;
     else if (avList_[i].attribute()=="labelInc") labelInc(avList_[i].value().asDouble()),index<<i;
     else if (avList_[i].attribute()=="labelAlignment") labelAlignment(MSAttrValue::stringToAlignment(avList_[i].value())),index<<i;
     else if (avList_[i].attribute()=="valueMin") valueMin(avList_[i].value().asDouble()),index<<i;
     else if (avList_[i].attribute()=="valueMax") valueMax(avList_[i].value().asDouble()),index<<i;
     else if (avList_[i].attribute()=="valueInc") valueInc(avList_[i].value().asDouble()),index<<i;
     else if (avList_[i].attribute()=="valueFont") valueFont(avList_[i].value()),index<<i;
     else if (avList_[i].attribute()=="valueForeground") valueForeground(avList_[i].value()),index<<i;
     else if (avList_[i].attribute()=="valueAlignment") valueAlignment(MSAttrValue::stringToAlignment(avList_[i].value())),index<<i;
     else if (avList_[i].attribute()=="subtitle") subtitle(MSAttrValue::stringToStringVector(avList_[i].value())),index<<i;
     else if (avList_[i].attribute()=="subtitleFont") subtitleFont(avList_[i].value()),index<<i;
     else if (avList_[i].attribute()=="subtitleForeground") subtitleForeground(avList_[i].value()),index<<i;
     else if (avList_[i].attribute()=="subtitleAlignment") subtitleAlignment(MSAttrValue::stringToAlignment(avList_[i].value())),index<<i;
     else if (avList_[i].attribute()=="mintitle") mintitle(MSAttrValue::stringToStringVector(avList_[i].value())),index<<i;
     else if (avList_[i].attribute()=="mintitleFont") mintitleFont(avList_[i].value()),index<<i;
     else if (avList_[i].attribute()=="mintitleForeground") mintitleForeground(avList_[i].value()),index<<i;
     else if (avList_[i].attribute()=="mintitleAlignment") mintitleAlignment(MSAttrValue::stringToAlignment(avList_[i].value())),index<<i;
     else if (avList_[i].attribute()=="maxtitle") maxtitle(MSAttrValue::stringToStringVector(avList_[i].value())),index<<i;
     else if (avList_[i].attribute()=="maxtitleFont") maxtitleFont(avList_[i].value()),index<<i;
     else if (avList_[i].attribute()=="maxtitleForeground") maxtitleForeground(avList_[i].value()),index<<i;
     else if (avList_[i].attribute()=="maxtitleAlignment") maxtitleAlignment(MSAttrValue::stringToAlignment(avList_[i].value())),index<<i;
     else if (avList_[i].attribute()=="sliderBackground") sliderBackground(avList_[i].value()),index<<i;
     else if (avList_[i].attribute()=="majorTickSize") majorTickSize(avList_[i].value().asInt()),index<<i;
     else if (avList_[i].attribute()=="minorTickSize") minorTickSize(avList_[i].value().asInt()),index<<i;
     else if (avList_[i].attribute()=="minorTickCount") minorTickCount(avList_[i].value().asInt()),index<<i;
   }
  avList_.remove(index);
}

MSAttrValueList& MSScale::get(MSAttrValueList& avList_)
{
  MSStringVector alignmentVector("MSNone\nMSCenter\nMSTop\nMSBottom\nMSLeft\nMSRight");  

  avList_<<MSAttrValue("labelAlignment",MSAttrValue::alignmentToString(labelAlignment()),
                       alignmentVector,MSAttrValue::List);
  avList_<<MSAttrValue("labelFont",server()->fontName(labelFont()),MSAttrValue::Font);
  avList_<<MSAttrValue("labelForeground",server()->colorName(labelForeground()),MSAttrValue::Color);
  avList_<<MSAttrValue("labelInc",MSString(labelInc()));

  avList_<<MSAttrValue("majorTickSize",MSString(majorTickSize()));
  avList_<<MSAttrValue("minorTickCount",MSString(minorTickCount()));
  avList_<<MSAttrValue("minorTickSize",MSString(minorTickSize()));

  avList_<<MSAttrValue("maxtitle",MSAttrValue::stringVectorToString(maxtitle()),MSAttrValue::String);
  avList_<<MSAttrValue("maxtitleAlignment",MSAttrValue::alignmentToString(maxtitleAlignment()),
                       alignmentVector,MSAttrValue::List);
  avList_<<MSAttrValue("maxtitleFont",server()->fontName(maxtitleFont()),MSAttrValue::Font);
  avList_<<MSAttrValue("maxtitleForeground",server()->colorName(maxtitleForeground()),MSAttrValue::Color);

  avList_<<MSAttrValue("mintitle",MSAttrValue::stringVectorToString(mintitle()),MSAttrValue::String);
  avList_<<MSAttrValue("mintitleAlignment",MSAttrValue::alignmentToString(mintitleAlignment()),
                       alignmentVector,MSAttrValue::List);
  avList_<<MSAttrValue("mintitleFont",server()->fontName(mintitleFont()),MSAttrValue::Font);
  avList_<<MSAttrValue("mintitleForeground",server()->colorName(mintitleForeground()),MSAttrValue::Color);

  avList_<<MSAttrValue("sliderBackground",server()->colorName(sliderBackground()),MSAttrValue::Color);

  avList_<<MSAttrValue("subtitle",MSAttrValue::stringVectorToString(subtitle()),MSAttrValue::String);
  avList_<<MSAttrValue("subtitleAlignment",MSAttrValue::alignmentToString(subtitleAlignment()),
                       alignmentVector,MSAttrValue::List);
  avList_<<MSAttrValue("subtitleFont",server()->fontName(subtitleFont()),MSAttrValue::Font);
  avList_<<MSAttrValue("subtitleForeground",server()->colorName(subtitleForeground()),MSAttrValue::Color);

  avList_<<MSAttrValue("valueAlignment",MSAttrValue::alignmentToString(valueAlignment()),
                       alignmentVector,MSAttrValue::List);
  avList_<<MSAttrValue("valueInc",MSString(valueInc()));
  avList_<<MSAttrValue("valueFont",server()->fontName(valueFont()),MSAttrValue::Font);
  avList_<<MSAttrValue("valueForeground",server()->colorName(valueForeground()),MSAttrValue::Color);
  avList_<<MSAttrValue("valueMin",MSString(valueMin()));
  avList_<<MSAttrValue("valueMax",MSString(valueMax()));

  return MSPrimitive::get(avList_);
}

void MSScale::drawSliderValue(void)
{}

// #########################################################
// mutator methods 
// #########################################################

void MSScale::titleAlignment(unsigned long x_)
{
  if (x_!=titleAlignment())
   {
     _titleAlignment=x_;
     computeSize();
     redraw();
   }
}
void MSScale::subtitle(const MSStringVector& x_)
{
  if (x_!=subtitle())
   {
     _subtitle=x_;
     computeSize();
     redraw();
   }
}
void MSScale::subtitleAlignment(unsigned long x_)
{
  if (x_!=subtitleAlignment())
   {
     _subtitleAlignment=x_;
     computeSize();
     redraw();
   }
}
void MSScale::subtitleForeground(unsigned long x_)
{ _subtitleForeground=x_; redraw(); }
void MSScale::subtitleForeground(const char *x_)
{ subtitleForeground(server()->pixel(x_)); }
void MSScale::subtitleFont(const char *x_)
{ subtitleFont(server()->fontID(x_)); }
void MSScale::subtitleFont(Font x_)
{
  if (x_!=subtitleFont())
   {
     _subtitleFont=x_;
     computeSize();
     redraw();
   }
}

void MSScale::mintitle(const MSStringVector& x_)
{
  if (x_!=mintitle())
   {
     _mintitle=x_;
     computeSize();
     redraw();
   }
}
void MSScale::mintitleAlignment(unsigned long x_)
{
  if (x_!=mintitleAlignment())
   {
     _mintitleAlignment=x_;
     computeSize();
     redraw();
   }
}
void MSScale::mintitleForeground(const char *x_)
{ mintitleForeground(server()->pixel(x_)); }
void MSScale::mintitleForeground(unsigned long x_)
{
  _mintitleForeground=x_;
  redraw();
}
void MSScale::mintitleFont(const char *x_)
{ mintitleFont(server()->fontID(x_)); }
void MSScale::mintitleFont(Font x_)
{
  if (x_!=mintitleFont())
   {
     _mintitleFont=x_;
     computeSize();
     redraw();
   }
}

void MSScale::maxtitle(const MSStringVector& x_)
{
  if (x_!=maxtitle())
   {
     _maxtitle=x_;
     computeSize();
     redraw();
   }
}
void MSScale::maxtitleAlignment(unsigned long x_)
{
  if (x_!=maxtitleAlignment())
   {
     _maxtitleAlignment=x_;
     computeSize();
     redraw();
   }
}
void MSScale::maxtitleForeground(unsigned long x_)
{ _maxtitleForeground=x_; redraw(); }
void MSScale::maxtitleForeground(const char *x_)
{ maxtitleForeground(server()->pixel(x_)); }
void MSScale::maxtitleFont(const char *x_)
{ maxtitleFont(server()->fontID(x_)); }
void MSScale::maxtitleFont(Font x_)
{
  if (x_!=maxtitleFont())
   {
     _maxtitleFont=x_;
     computeSize();
     redraw();
   }
}

void MSScale::labelAlignment(unsigned long x_)
{
  if (x_!=labelAlignment())
   {
     _labelAlignment=x_;
     computeSize();
     redraw();
   }
}
void MSScale::labelForeground(unsigned long x_)
{ _labelForeground=x_; redraw(); }
void MSScale::labelForeground(const char *x_)
{ labelForeground(server()->pixel(x_)); }
void MSScale::labelFont(const char *x_)
{ labelFont(server()->fontID(x_)); }
void MSScale::labelFont(Font x_)
{
  if (x_!=labelFont())
   {
     _labelFont=x_;
     computeSize();
     redraw(); 
   }
}
void MSScale::labelInc(double x_)
{ _labelInc=x_; redraw(); }

void MSScale::labelOut(MSLabelOut *x_)
{
  labelOut(MSLabelOutPtr(x_,MSInit));
}

void MSScale::labelOut(MSLabelOutPtr x_)
{
  x_->owner(this);
  _labelOut=x_;
  computeSize();
  redraw();
}

void MSScale::sliderForeground(unsigned long x_)
{
  _slider->foreground(x_);
  _slider->redraw();
}
void MSScale::sliderBackground(unsigned long x_)
{
  _slider->background(x_);
  _slider->redraw();
}
void MSScale::sliderForeground(const char *x_)
{ sliderForeground(server()->pixel(x_)); }
void MSScale::sliderBackground(const char *x_)
{
  sliderBackground(server()->pixel(x_));
}

void MSScale::valueMin(double x_)
{
  if (x_<_valueMax&&x_!=_valueMin)
   {
     _valueMin=x_;
     computeSize();
     redraw();
   }
}
void MSScale::valueMax(double x_)
{
  if (x_>_valueMin&&x_!=_valueMax)
   {
     _valueMax=x_;
     computeSize();
     redraw();
   }
}

void MSScale::valueInc(double x_)
{ if (x_>0.&&x_<=(_valueMax-_valueMin)) _valueInc=x_; }
void MSScale::valuePageInc(double x_)
{ if (x_>0.&&x_<=(_valueMax-_valueMin)) _valuePageInc=x_; }

void MSScale::valueChange(double x_)
{ _valueChange=x_; }
void MSScale::valueAlignment(unsigned long x_)
{
  _valueAlignment=x_;
  computeSize();
  redraw();
}
void MSScale::valueForeground(unsigned long x_)
{ _valueWin->foreground(x_); }
void MSScale::valueForeground(const char *x_)
{ valueForeground(server()->pixel(x_)); }
void MSScale::valueFont(Font x_)
{
  _valueWin->font(x_);
  computeSize();
  redraw();
}
void MSScale::valueFont(const char *x_)
{ valueFont(server()->fontID(x_)); }
void MSScale::valueSensitive(MSBoolean x_)
{_valueWin->sensitive(x_);}
void MSScale::majorTickSize(int x_)
{
  _majorTickSize=x_<30?x_:30;
  computeSize();
  redraw();
}
void MSScale::minorTickSize(int x_)
{
  _minorTickSize=x_<30?x_:30;
  computeSize();
  redraw();
}
void MSScale::minorTickCount(int x_)
{ _minorTickCount=x_; redraw(); }

// #########################################################
// default virtual methods - prevents gratuitous inlining
// #########################################################
void MSScale::drawTickLabels(void) {}
void MSScale::drawSliderTitles(void) {}
void MSScale::moveSlider(const XEvent *) {}
void MSScale::computeLabelOffset(void) {}
void MSScale::computeSliderAreaSize(void) {}
void MSScale::computeSliderScale(void) {}
void MSScale::computeTickInc(void) {}
void MSScale::button3Press(const XEvent *) {}
void MSScale::computeSize(void) {}
void MSScale::setSliderPosition(int) {}
void MSScale::sliderSize(int) {}
void MSScale::drawSliderEtch(void) {}
void MSScale::sliderRedrawNotify(void) {}

double MSScale::pixelToValue(int)
{ return 0.; }
int MSScale::valueToPixel(double)
{ return 0; }
int MSScale::incFactor(int,int)
{ return 1; }

void MSScale::updateFormat(void)
{ redraw(); }

void MSScale::updateData(void)
{ redraw(); }

void MSScale::naturalSize(void)
{ computeSize(); }

const char *MSScale::formatValue(MSString &buffer_,double data_)
{
  if (format().formatType()==MSFormat::Float)
   {
     MSFloat aFloat(data_);
     return aFloat.format(buffer_,format());
   }
  else 
   {
     MSInt aInt((int)(data_));
     return aInt.format(buffer_,format());
   }
}

MSFloat MSScale::asFloat(void) const
{ return MSFloat(currentValue()); }

MSInt MSScale::asInt(void) const
{
  if (currentValue()>INT_MAX||currentValue()<-INT_MAX)
   {
     MSString message("Warning: value exceeds INT_MAX, unable to assign value ");
     message += MSString(currentValue());
     MSMessageLog::warningMessage(message.string());
   }
  return MSInt((int)(currentValue()));
}

unsigned long MSScale::addEditorKeyCallback( const char* pString_,MSKeyCallback* keyCallback_)
{ return editor()->addKeyCallback(pString_,keyCallback_);}

void MSScale::removeEditorKeyCallback(unsigned long id_)
{ editor()->removeKeyCallback(id_); }

void MSScale::removeEditorKeyCallback(const char* pString_)
{ editor()->removeKeyCallback(pString_); }
