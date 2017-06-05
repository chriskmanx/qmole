///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////

#include <MSGUI/MSEntryFieldCombo.H>
#include <MSGUI/MSArrow.H>

static const int MSEntryFieldComboDefaultSpacing=3;
static const int MSEntryFieldComboRepeatInterval=100;
static const int MSEntryFieldComboInitialDelay=200;

// #########################################################
// DelayTimer
// #########################################################
MSEntryFieldCombo::DelayTimer::DelayTimer(MSEntryFieldCombo *ef_,unsigned long ms_,TimerDirection direction_):
MSRegularTimer(ms_,0),_entryField(ef_),_direction(direction_)
{}

void MSEntryFieldCombo::DelayTimer::process(void)
{ _entryField->processDelayTimer(); }

void MSEntryFieldCombo::DelayTimer::direction(TimerDirection direction_)
{ _direction=direction_; }

MSEntryFieldCombo::TimerDirection MSEntryFieldCombo::DelayTimer::direction(void) const
{ return _direction; }

MSEntryFieldCombo::RepeatTimer::RepeatTimer(MSEntryFieldCombo *ef_,
		    unsigned long interval_, TimerDirection direction_) :
MSIntervalTimer(interval_),_entryField(ef_),_direction(direction_)
{}

void MSEntryFieldCombo::RepeatTimer::direction(TimerDirection direction_)
{ _direction=direction_; }

MSEntryFieldCombo::TimerDirection MSEntryFieldCombo::RepeatTimer::direction(void) const
{ return _direction; }

MSEntryFieldCombo *MSEntryFieldCombo::RepeatTimer::entryField(void)
{ return _entryField; }

void MSEntryFieldCombo::RepeatTimer::process(void)
{
  if (_direction==Up) _entryField->increment();
  else _entryField->decrement();
}

// ##################################################################
// MSEntryFieldCombo
// ##################################################################
MSEntryFieldCombo::MSEntryFieldCombo(MSWidget *owner_,const MSSymbol& tag_):
MSEntryField(owner_,tag_)
{ init(); }

MSEntryFieldCombo::MSEntryFieldCombo(MSWidget *owner_,const char *label_,const MSSymbol& tag_):
MSEntryField(owner_,label_,tag_)
{ init(); }

void MSEntryFieldCombo::init(void)
{
  _buttonSpacing=MSEntryFieldComboDefaultSpacing;
  _buttonFlags = 0;
  _upArrow=0;
  _downArrow=0;
  _comboArrow=0;
  _repeatTimer=0;
  _delayTimer=0;
  _buttonLabel = "";
  _buttonSelected = MSFalse;
}

MSEntryFieldCombo::~MSEntryFieldCombo(void)
{
  if (_upArrow!=0) delete _upArrow;
  if (_downArrow!=0) delete _downArrow;
  if (_comboArrow!=0) delete _comboArrow;
  if (_delayTimer!=0) delete _delayTimer;
  if (_repeatTimer!=0) delete _repeatTimer;
}

void MSEntryFieldCombo::buttonColor(ButtonFlag flag_, const char * color_)
{ buttonColor(flag_, server()->pixel(color_)); }

void MSEntryFieldCombo::buttonColor(ButtonFlag flag_, unsigned long color_)
{
  if (color_ != buttonColor(flag_))
   {
     switch(flag_)
      {
      case ComboButton:
        if(_comboArrow != 0) _comboArrow->color(color_);
        break;

      case UpDownArrows:
        if(_upArrow != 0) _upArrow->color(color_);
        if(_downArrow != 0) _downArrow->color(color_);
        break;

      case TextButton:        
      default:
        break;
        
      }   
     redraw();
   }
}

unsigned long MSEntryFieldCombo::buttonColor(ButtonFlag flag_) const
{
  switch(flag_)
   {
   case ComboButton:
     if (_comboArrow != 0) return  _comboArrow->color();
     break;
   case UpDownArrows:
     if (_upArrow != 0) return _upArrow->color();
     break;
   case TextButton:
   default:
     break;
   }
  return background();
}
  
    

void MSEntryFieldCombo::buttonState(ButtonFlag button_, MSBoolean flag_)
{
  if(button_ == TextButton) return; // use comboButtonLabel instead
  if(bitState(button_) != flag_)
   {
     if(flag_ == MSTrue) setBit(button_);
     else unsetBit(button_);
     
     switch(button_)
      {
      case UpDownArrows:
	if(_upArrow == 0)   _upArrow=new MSArrow(this,MSArrow::Up);
	if(_downArrow == 0) _downArrow=new MSArrow(this,MSArrow::Down);
	break;
      case ComboButton:
        if (_comboArrow == 0) _comboArrow = new MSArrow(this, MSArrow::Down);
      default:
	break;
      }
   }
  placement();
}

void MSEntryFieldCombo::comboButtonLabel(const MSString& label_)
{
  _buttonLabel = label_;
  if(_buttonLabel == "") unsetBit(TextButton);
  else setBit(TextButton);
  placement();
}

void MSEntryFieldCombo::buttonPress(const XEvent *pEvent_)
{
  if (sensitive()==MSTrue)
   {
     if(pEvent_->xbutton.x<_fieldValue->x()+_fieldValue->width())
      {
        MSEntryField::buttonPress(pEvent_);
      }
     else 
      {
        activate();
        if (_editor->mapped()==MSFalse)
         {
           MSBoolean focusOK=MSTrue;
           if (acceptFocus()==MSTrue) focusOK=traverseFocus(this);
           if (focusOK==MSTrue)
            {
              if (pEvent_->xbutton.button==Button1&&_buttonFlags!=0)
               {
                 if (comboOrTextButton()==MSTrue&&
                     pEvent_->xbutton.x>=buttonRect().x()&&
                     pEvent_->xbutton.y>=buttonRect().y())
                  {
                    if(buttonState(ComboButton) == MSTrue)
                     {
                       buttonActivate();
                     }
                    else
                     {
                       drawTextButton(MSTrue);
                       _buttonSelected = MSTrue;
                     }
                  }
                 else if(buttonState(UpDownArrows) == MSTrue)
                  {
                    if (isProtected()==MSFalse)
                     {
                       if(pEvent_->xbutton.y <_fieldValue->y()
                          +_fieldValue->height()/2) armUpArrow();
                       else armDownArrow();
                     }
                  }
               }
            }
         }
      }
   }
}

void MSEntryFieldCombo::buttonRelease(const XEvent *pEvent_)
{
  if (buttonState(UpDownArrows)==MSTrue&&
      (_upArrow->selected()==MSTrue||_downArrow->selected()==MSTrue))
   {
     if (_upArrow->selected()) _upArrow->select(MSFalse);
     else if (_downArrow->selected()) _downArrow->select(MSFalse);
     stopTimers();
   }
  else if(comboOrTextButton()==MSTrue&&_buttonSelected==MSTrue)
   {
     _buttonSelected=MSFalse;
     drawTextButton(MSFalse);
     if(pEvent_->xbutton.x >= buttonRect().x()&&
	pEvent_->xbutton.x <= buttonRect().x() + buttonRect().width()&&
	pEvent_->xbutton.y >= buttonRect().y() &&
	pEvent_->xbutton.y <= buttonRect().y() + buttonRect().height())
      {
	textButtonActivate();
      }
   }
  else MSEntryField::buttonRelease(pEvent_);
}

void MSEntryFieldCombo::armUpArrow(void)
{
  _upArrow->select(MSTrue);
  increment();
  startDelayTimer(Up);
}

void MSEntryFieldCombo::armDownArrow(void)
{
  _downArrow->select(MSTrue);
  decrement();
  startDelayTimer(Down);
}


void MSEntryFieldCombo::drawArrows(void)
{
  _upArrow->draw();
  _downArrow->draw();
}


void MSEntryFieldCombo::drawTextButton(MSBoolean armed_)
{
  if(frozen()==MSFalse&&mapped()==MSTrue)
   {
     drawBevel(buttonRect(),(armed_==MSTrue)?MSSunken:MSRaised,2);
     XFillRectangle(display(),window(),
		    (armed_==MSTrue)?selectShadowGC():backgroundShadowGC(),
		    buttonRect().x()+2,buttonRect().y()+2,
		    buttonRect().width()-4,buttonRect().height()-4);

     int offset = 2 + 1;
     int x = buttonRect().x() + offset;
     int y = buttonRect().y() + 2;
  
     XDrawString(display(), window(), fieldValue()->textGC(),
		 fieldValue()->textFontStruct(),
		 x, y + fieldValue()->textAscent(),
		 comboButtonLabel().string(),
		 comboButtonLabel().length());
   }
}
	     
void MSEntryFieldCombo::drawComboButton(MSBoolean armed_)
{
  if (frozen()==MSFalse&&mapped()==MSTrue)
   {
     
     drawBevel(buttonRect(),(armed_==MSTrue)?MSSunken:MSRaised,2);
     XFillRectangle(display(),window(),
		    (armed_==MSTrue)?selectShadowGC():backgroundShadowGC(),
		    buttonRect().x()+2,buttonRect().y()+2,
		    buttonRect().width()-4,buttonRect().height()-4);
     
     int mw=buttonRect().width()>>1;  // x midpoint of button
     int mh=buttonRect().height()>>1; // y midpoint of button 
     int xoff=buttonRect().x();
     int yoff=buttonRect().y();     
     
     int ah = _fieldValue->height()/2;
     int aw = _fieldValue->textHeight();
     _comboArrow->configure(xoff+mw-aw/2, yoff+mh-ah/2, aw, ah);
     _comboArrow->select(armed_);
     _comboArrow->draw();
   }
}

void MSEntryFieldCombo::redraw(void)
{
  if (frozen()==MSFalse&&mapped()==MSTrue)
   {
     drawBackground();
     drawFieldLabel();
     drawFieldValue();
     if (buttonState(UpDownArrows)==MSTrue) drawArrows();
     if (buttonState(ComboButton) ==MSTrue) drawComboButton(MSFalse);
     else if(buttonState(TextButton)==MSTrue) drawTextButton(_buttonSelected);
     drawShadow();
     if (highlighted()==MSTrue) drawHighlight();
   }
}

unsigned MSEntryFieldCombo::computeDecorPixelWidth(void)
{
  unsigned width = 0;
  if (buttonState(UpDownArrows) == MSTrue)
  {
    width = computeArrowPixelWidth();
    if (comboOrTextButton() == MSTrue)
      width += computeButtonPixelWidth() + buttonSpacing();
  }
  else if(comboOrTextButton() == MSTrue) width = computeButtonPixelWidth();

  return width;
}

unsigned MSEntryFieldCombo::computeValuePixelWidth(void)
{
  if (_buttonFlags == 0) return (MSEntryField::computeValuePixelWidth());
  else return valueWidth()*_fieldValue->charWidth()+2*_fieldValue->offset()+
  computeDecorPixelWidth()+buttonSpacing();
}

unsigned MSEntryFieldCombo::computeArrowPixelWidth(void)
{ return _fieldValue->textHeight(); }

unsigned MSEntryFieldCombo::computeButtonPixelWidth(void)
{
  if (buttonState(ComboButton) == MSTrue)
   {
     return _fieldValue->textHeight();
   }
  else // textButton
   {
     int offset = 6;
     
     return fieldValue()->textWidth(comboButtonLabel().string(),
                                    comboButtonLabel().length()) + offset;
   }
}

void MSEntryFieldCombo::placement(void) 
{
  if (_buttonFlags == 0) MSEntryField::placement();
  else if (_fieldValue!=0&&_fieldLabel!=0)
   {
     if (_editor->mapped()==MSTrue) unmapEditor();
     if(buttonState(UpDownArrows) == MSTrue)
      {
	_upArrow->width(computeArrowPixelWidth());
	_downArrow->width(computeArrowPixelWidth());
      }
     if (comboOrTextButton() == MSTrue)
      {
        buttonRect().width(computeButtonPixelWidth());
      }
     
     int extraWidth = computeDecorPixelWidth();
     
     int offset=highlightThickness()+shadowThickness();
     int offset2=offset<<1;
     int vh=_fieldValue->textHeight()+
     2*(marginHeight()+_fieldValue->shadowThickness()+_fieldValue->highlightThickness());
     int lh=_fieldLabel->textHeight()+
     2*(_fieldLabel->shadowThickness()+_fieldLabel->highlightThickness());
     int lw=_fieldLabel->width();
     int vw=_fieldValue->width();
     int trueWidth=width()-offset2-extraWidth-int(buttonSpacing());
     int h;

     if (labelAlignment()==MSTop)
      {
	vw+=buttonSpacing()+extraWidth;
	_fieldLabel->moveTo(offset,offset);
	_fieldValue->moveTo(offset,offset+lh+labelSpacing());
	_fieldLabel->width(lw);
	_fieldValue->width(trueWidth);
      }
     else
      {
	vw=trueWidth-_fieldLabel->width();
	h=(vh>lh)?vh:lh;
	if (vh==lh) height(h+offset2);
	else if (h+offset2>height()) height(h+offset2);   
	
	_fieldLabel->height(h);
	_fieldValue->resize(vw,h);
	_fieldLabel->moveTo(offset,offset);
	_fieldValue->moveTo(offset+_fieldLabel->width(),offset);
      }

     int xPos = fieldValue()->x() + fieldValue()->width();
     
     if(buttonState(UpDownArrows) == MSTrue)
      {
	_upArrow->configure(xPos+buttonSpacing(),
			    _fieldValue->y(),computeArrowPixelWidth(),
			    _fieldValue->height()/2);
	_downArrow->configure(xPos+buttonSpacing(),
			      _fieldValue->y()+_fieldValue->height()/2,
			      computeArrowPixelWidth(),
			      _fieldValue->height()/2);
	xPos +=computeArrowPixelWidth() +buttonSpacing();	
      }
     if(comboOrTextButton() == MSTrue)
      {
	buttonRect().configuration(xPos+buttonSpacing(),
				   fieldValue()->y(),
				   computeButtonPixelWidth(),
				   fieldValue()->height());
      }
      
     redraw();
   }
}

void MSEntryFieldCombo::labelPixelWidth(unsigned labelPixelWidth_) 
{
  if (_fieldLabel!=0)
   {
     if (_fieldLabel->length()==0) labelPixelWidth_=0;
     if (_fieldLabel->width()!=labelPixelWidth_)
      {
	int offset=highlightThickness()+shadowThickness();

	if (labelAlignment()==MSTop)
	 {
	   unsigned lh=_fieldLabel->textHeight()+
	            2*(_fieldLabel->shadowThickness()+_fieldLabel->highlightThickness());

	   _fieldLabel->moveTo(offset,offset);
	   _fieldValue->moveTo(offset,offset+lh);
	   _fieldLabel->width(labelPixelWidth_);
	   int vw=width()-2*offset;
	   if (_buttonFlags != 0)
	    {
	      vw-=(computeDecorPixelWidth()+buttonSpacing());
	    }
	   _fieldValue->width(vw);
	 }
	else
	 {
	   int trueWidth=width()-2*offset;
	   int vw=trueWidth-int(labelPixelWidth_);
	   if (_buttonFlags != 0)
	    {
	      vw-=(computeDecorPixelWidth()+buttonSpacing());
	    }
	   _fieldLabel->width(labelPixelWidth_);
	   _fieldValue->width(vw);
	   _fieldLabel->moveTo(offset,offset);
	   _fieldValue->moveTo(offset+labelPixelWidth_,offset);
	 }
	redraw();
      }
   }
}

void MSEntryFieldCombo::reference(void)
{
  if(buttonState(ComboButton) == MSTrue) buttonActivate();
  else MSEntryField::reference();
}

void MSEntryFieldCombo::startDelayTimer(TimerDirection dir_)
{
  if (_delayTimer!=0)
   {
     _delayTimer->stop();
     _delayTimer=0;
   }
  _delayTimer=new DelayTimer(this,MSEntryFieldComboInitialDelay,dir_);
}

void MSEntryFieldCombo::processDelayTimer(void)
{ 
  startRepeatTimer(_delayTimer->direction());
  _delayTimer=0;
}

void MSEntryFieldCombo::startRepeatTimer(TimerDirection dir_)
{
  if (_repeatTimer==0) _repeatTimer=new RepeatTimer(this,MSEntryFieldComboRepeatInterval,dir_);
  else
   {
     _repeatTimer->direction(dir_);
     _repeatTimer->reset();
   }
}

void MSEntryFieldCombo::stopTimers(void)
{
  if(_delayTimer!=0)
   {
     _delayTimer->stop();
     _delayTimer=0;
   }
  if (_repeatTimer!=0) _repeatTimer->stop();
}

// ##################################################################
// inline methods
// ##################################################################

unsigned MSEntryFieldCombo::buttonSpacing(void) const
{ return _buttonSpacing; }

MSArrow *MSEntryFieldCombo::upArrow(void)
{ return _upArrow; }

MSArrow *MSEntryFieldCombo::downArrow(void)
{ return _downArrow; }

MSRect& MSEntryFieldCombo::buttonRect(void)
{ return _buttonRect; }

MSEntryFieldCombo::RepeatTimer *MSEntryFieldCombo::repeatTimer(void)
{ return _repeatTimer; }

MSBoolean MSEntryFieldCombo::bitState(unsigned flag_) const
{ return (_buttonFlags&flag_)?MSTrue:MSFalse; }

void MSEntryFieldCombo::setBit(ButtonFlag flag_)
{ _buttonFlags|=flag_; }

void MSEntryFieldCombo::unsetBit(ButtonFlag flag_)
{ _buttonFlags&=~flag_; }

MSBoolean MSEntryFieldCombo::buttonState(ButtonFlag flag_) const
{ return (_buttonFlags&flag_)?MSTrue:MSFalse; }

MSBoolean MSEntryFieldCombo::comboOrTextButton(void) const
{ return bitState(ComboButton|TextButton); }


void MSEntryFieldCombo::buttonActivate(void)
{}

void MSEntryFieldCombo::textButtonActivate(void)
{}


void MSEntryFieldCombo::down(void)
{ if (buttonState(ComboButton) == MSTrue) buttonActivate(); }

void MSEntryFieldCombo::up(void)
{ if (buttonState(ComboButton) == MSTrue) buttonActivate(); }

void MSEntryFieldCombo::buttonSpacing(unsigned buttonSpacing_)
{ _buttonSpacing = buttonSpacing_; }


const MSString& MSEntryFieldCombo::comboButtonLabel(void) const
{ return _buttonLabel; }

