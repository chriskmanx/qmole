///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////

#include <MSGUI/MSDateEntryField.H>

static const unsigned MSDateEntryFieldDefaultValueWidth=10;

static const unsigned int grabMask=(unsigned int)(ButtonPressMask|ButtonReleaseMask);

MSDateEntryField::DateMonthView::DateMonthView(MSWidget *owner_)
: MSMonthView(owner_)
{
  arrowButtons(MSTrue);
  acceptFocus(MSFalse);
}

void MSDateEntryField::DateMonthView::activate(void)
{
  MSString buffer;
  // We have to use Slash4 to guarantee that the date gets parsed correctly
  // Ideally we may want to set the date directly, but this is problematic as
  // it is possible for users to rely on the fact that validate is always called.
    dateField()->fieldEditor()->string(viewDate().format(buffer,MSDate::Slash4));
//  dateField()->fieldEditor()->string(viewDate().format(buffer,dateField()->format()));
    dateShell()->activate();
}

void MSDateEntryField::DateMonthView::escape(void)
{ dateShell()->escape(); }

void MSDateEntryField::DateMonthView::nextMonth(void)
{
  const MSDate & max = dateField()->maximumValue();
  
  if(max.isSet() == MSTrue)
   {
     MSDate aDate = viewDate() + MSTerm(0, 1);
     if (aDate <= max) viewDate() = aDate;
     else
      {
	aDate.setFirstDayOfMonth();
	if(aDate <= max) viewDate() = max;
      }
   }
  else MSMonthView::nextMonth();
}

void MSDateEntryField::DateMonthView::prevMonth(void)
{
  const MSDate & min = dateField()->minimumValue();
  
  if(min.isSet() == MSTrue)
   {
     MSDate aDate = viewDate() - MSTerm(0, 1);
     if (aDate >= min) viewDate() = aDate;
     else
      {
	aDate.setLastDayOfMonth();
	if(aDate >= min) viewDate() = min;
      }
   }
  else MSMonthView::prevMonth();
}

void MSDateEntryField::DateMonthView::home(void)
{
  if(dateField()->minimumValue().isSet() == MSTrue)
    viewDate(dateField()->minimumValue());
} 

void MSDateEntryField::DateMonthView::end(void)
{
  if(dateField()->maximumValue().isSet() == MSTrue)
    viewDate(dateField()->maximumValue());
}

void MSDateEntryField::DateMonthView::keyPress(const XEvent * ev,
	       KeySym keysym_, unsigned int state_, const char* string_)
{
  MSKeyPress keyPress(keysym_, state_);
  if(keyTranslate(keyPress)==MSFalse) MSMonthView::keyPress(ev, keysym_, state_, string_);
}

void MSDateEntryField::DateMonthView::buttonPress(const XEvent *pEvent_)
{
  Window win = pEvent_->xbutton.subwindow;
  if(win == 0)
  {
    //test if we our on our own window or not.
    Window root, child;
    int ix =0, iy=0, rx=0, ry=0;
    unsigned keys;

    XQueryPointer(display(),dateShell()->window(),&root,&child,&rx,&ry,&ix,&iy,&keys);
    if(child == window()) MSMonthView::buttonPress(pEvent_);
    else escape();
  }
  else if(win == _leftArrow->window()) buttonPressNotify(_leftArrow, pEvent_);
  else if(win == _rightArrow->window()) buttonPressNotify(_rightArrow, pEvent_);
  else MSMonthView::buttonPress(pEvent_);
}

void MSDateEntryField::DateMonthView::buttonRelease(const XEvent* pEvent_)
{
  // just in case they are waiting for button Release but wont get since
  // we're grabbing pointer
  // this doesn't have any effect if arrows are inactive at the moment.
  if(_leftArrow != 0)  buttonReleaseNotify(_leftArrow, pEvent_);
  if(_rightArrow != 0) buttonReleaseNotify(_rightArrow, pEvent_);
}


MSDateEntryField::DateShell::DateShell(MSDateEntryField *owner_)
: MSWidget(owner_->server())
{
  _dateField = owner_;
  _monthView = 0;
  _bg=dateField()->background();
  _fg=dateField()->foreground();
  _fontID=dateField()->font();
  XSetWindowAttributes attributes;
  attributes.background_pixel=background();
  attributes.border_pixel=foreground();
  attributes.override_redirect=(int)MSTrue;
  _window=(Window)XCreateWindow(display(),
                    server()->root(),
                    MSRect::x(),MSRect::y(),MSRect::width(),MSRect::height(),
                    1,(int)CopyFromParent,InputOutput,CopyFromParent,
		    (unsigned long)(CWBackPixel|CWBorderPixel|CWOverrideRedirect),
		    (XSetWindowAttributes *)&attributes);  
  _eventMask=0;
  server()->widgetHashTable()->add(window(),this);
  _monthView=new DateMonthView(this);

}

MSDateEntryField::DateShell::~DateShell()
{ safeDestroy(_monthView);}

void MSDateEntryField::DateShell::showAt(int x, int y)
{
  moveTo(x, y);
  show();
}

void MSDateEntryField::DateShell::show(void)
{
 if (mapped()==MSFalse)
   {
     monthView()->show();
     map();
     XFlush(display());
     raise();
     grab();
   }
}

void MSDateEntryField::DateShell::hide(void)
{
  if (mapped()==MSTrue)
   {
     ungrab();
     unmap();
     monthView()->hide();
   }
}


void MSDateEntryField::DateShell::grab(void)
{
  server()->grabKeyboard(monthView()->window(),False,GrabModeAsync,GrabModeAsync,CurrentTime,MSTrue);
  server()->grabPointer(monthView()->window(),False,
			grabMask,GrabModeAsync,GrabModeAsync,None,None,CurrentTime,MSTrue);
  XFlush(display());
}

void MSDateEntryField::DateShell::ungrab(void)
{
  server()->ungrabPointer(monthView()->window(),CurrentTime);
  server()->ungrabKeyboard(monthView()->window(),CurrentTime);
  XFlush(display());
}

void MSDateEntryField::DateShell::activate(void)
{
  hide();
  dateField()->monthViewActivate();
}

void MSDateEntryField::DateShell::escape(void)
{
  hide();
  dateField()->monthViewEscape();
}


void MSDateEntryField::DateShell::childConfigure(MSWidget *pWidget_)
{ resize(pWidget_->width(),pWidget_->height()); }

void MSDateEntryField::DateShell::configure(void)
{ if (monthView()!=0) monthView()->resize(width(),height()); }


MSDateEntryField::MSDateEntryField(MSWidget *owner_,
				   const char *label_,const MSSymbol& tag_) :
MSEntryFieldPlus(owner_,label_,tag_) 
{
  internalCouple(new MSDate(MSDate::today()));
  init();
}

MSDateEntryField::MSDateEntryField(MSWidget *owner_,MSDate& model_,
				   const char *label_,const MSSymbol& tag_) :
MSEntryFieldPlus(owner_,label_,tag_) 
{
  model(model_);
  init();
}

MSDateEntryField::~MSDateEntryField(void)
{
  if(_dateShell != 0) safeDestroy(_dateShell);
}

void MSDateEntryField::init(void)
{
  _minimumValue.unset();
  _maximumValue.unset();  
  _format=MSFormat(MSDate::Slash4);
  _incrementValue=MSTerm(0,0,1);
  _dateShell = 0;
  _valueWidth=MSDateEntryFieldDefaultValueWidth;
}

void MSDateEntryField::monthDropDown(MSBoolean dropDown_)
{
  buttonState(ComboButton, dropDown_);
  if(dropDown_ == MSTrue && dateShell() == 0) _dateShell = new DateShell(this);
}

void MSDateEntryField::model(MSDate& model_)
{ couple(&model_); }

void MSDateEntryField::model(const MSDate& model_)
{ constCouple(&model_); }

void MSDateEntryField::updateData(void)
{
  if (MSView::model()==0) internalCouple(new MSDate(MSDate::today()));
  MSEntryFieldPlus::updateData();
}

void MSDateEntryField::buttonActivate(void)
{ showMonthView(); }

void MSDateEntryField::showMonthView(void)
{
  drawComboButton(MSTrue);
  if(value().isSet()==MSFalse) monthView()->viewDate(MSDate::today());
  else monthView()->viewDate(value());
  clearEditor();
  mapEditor();
  MSString buffer;
  fieldEditor()->string(monthView()->viewDate().format(buffer,format()));
  fieldEditor()->selectAll();
  //show outside screen to make it compute size.
  dateShell()->showAt(server()->width(), server()->height());
  
  int x,y;
  rootXY(x,y);
  x+=buttonRect().x()-dateShell()->width();
  if(x<0) x=0;
  y+=height();
  if(y+dateShell()->height() >server()->height()) y -= height()+dateShell()->height();
  dateShell()->showAt(x, y);
}

void MSDateEntryField::monthViewEscape(void)
{
  drawComboButton(MSFalse);
  escape();
}

void MSDateEntryField::monthViewActivate(void)
{
  drawComboButton(MSFalse);
  activate();
}

MSMonthView *MSDateEntryField::monthView(void)
{ return (dateShell()==0)?0:dateShell()->monthView(); }

const MSMonthView *MSDateEntryField::monthView(void) const
{ return (dateShell()==0)?0:dateShell()->monthView(); }

void MSDateEntryField::updateFont(Font oldfid_)
{
  MSEntryFieldPlus::updateFont(oldfid_);
  if (monthView() != 0) monthView()->font(font());
}

void MSDateEntryField::updateForeground(unsigned long oldFg_)
{
  MSEntryFieldPlus::updateForeground(oldFg_);
  if (monthView() != 0) monthView()->foreground(foreground());
}
  
void MSDateEntryField::updateBackground(unsigned long oldBg_)
{
  MSEntryFieldPlus::updateBackground(oldBg_);
  if (monthView() != 0) monthView()->background(background());
}

const char *MSDateEntryField::formatOutput(MSString &buffer_)
{
  if (MSView::model()!=0) value().format(buffer_,format());
  return buffer_.string();
}

MSBoolean MSDateEntryField::validate(const char *pString_)
{
  if (MSView::model()!=0)
   {
     MSDate aDate;
     if (aDate.set(pString_)==MSError::MSSuccess)
      {
	if (minimumValue().isSet()==MSTrue&&maximumValue().isSet()==MSTrue)
	 {
	   if (aDate>=minimumValue()&&aDate<=maximumValue())
	    {
	      value()=aDate;
	      return MSTrue;
	    }
	 }
	else if (minimumValue().isSet()==MSTrue)
	 {
	   if (aDate>=minimumValue())
	    {
	      value()=aDate;
	      return MSTrue;
	    }
	 }
	else if (maximumValue().isSet()==MSTrue)
	 {
	   if (aDate<=maximumValue())
	    {
	      value()=aDate;
	      return MSTrue;
	    }
	 }
	else
	 {
	   value()=aDate;
	   return MSTrue;
	 }
      }
   }
  return MSFalse;
}

void MSDateEntryField::increment(void)
{
  if (MSView::model()!=0)
   {
     if (maximumValue().isSet()==MSTrue)
      {
	MSDate aDate=value();
	aDate+=incrementValue();
	if (aDate<=maximumValue())
	 {
	   value()=aDate;
	   valueChange();
	 }
      }
     else
      {
	value()+=incrementValue();
	valueChange();
      }
   }
}

void MSDateEntryField::decrement(void)
{
  if (MSView::model()!=0)
   {
     if (minimumValue().isSet()==MSTrue)
      {
	MSDate aDate=value();
	aDate-=incrementValue();
	if (aDate>=minimumValue())
	 {
	   value()=aDate;
	   valueChange();
	 }
      }
     else
      {
	value()-=incrementValue();
	valueChange();
      }
   }
}

void MSDateEntryField::generateInputMask(void)
{
  MSString mask;
  switch(format().dateFormat())
   {
   case MSDate::Slash        : mask = "--/--/--";    break;
   case MSDate::Slash4       : mask = "--/--/----"; break;
   case MSDate::EuropeanDot  : mask = "--.--.--";    break;
   case MSDate::EuropeanDot4 : mask = "--.--.----";  break;
   default:                    mask = ""; break;
   }

  if(inputMaskCharacter()!='-' && mask != "") mask.change('-', inputMaskCharacter());
  fieldEditor()->inputMask(mask);
}

void MSDateEntryField::set(MSAttrValueList& avList_)
{
  MSEntryFieldPlus::set(avList_);
  MSIndexVector index;
  for (unsigned i=0;i<avList_.length();i++)
   {
     const MSString& attr=avList_[i].attribute();
     const MSString& aValue=avList_[i].value(); 
     
     if (attr=="incrementValue")
      {
	MSTerm aTerm;
	if (aTerm.set(aValue)==MSError::MSSuccess)
	 {
	   incrementValue(aTerm),index<<i;
	 }
      }
     else if (attr=="minimumValue")
      {
	MSDate aDate;	   
	if (aDate.set(aValue)==MSError::MSSuccess)
	 {
	   if (aValue=="") _minimumValue.unset();
	   else _minimumValue=aDate;
	   index<<i;
	 }
      }
     else if (attr=="maximumValue")
      {
	MSDate aDate;
	if (aDate.set(aValue)==MSError::MSSuccess)
	 {
	   if (aValue=="") _maximumValue.unset();
	   else _maximumValue=aDate;
	   index<<i;
	 }
      }
     else if (attr=="monthDropDown")
      {
	monthDropDown(aValue.asBoolean());
	index << i;
      }
     else if (attr == "comboArrowColor")
      {
        comboArrowColor(aValue);
        index << i;
      }
   }
  avList_.remove(index);
}

MSAttrValueList& MSDateEntryField::get(MSAttrValueList& avList_)
{
  avList_ << MSAttrValue("monthDropDown",
			 (monthDropDown() == MSTrue) ?"MSTrue":"MSFalse",
			 MSStringVector("MSFalse\nMSTrue"));
  avList_ << MSAttrValue("comboArrowColor",
                         server()->colorName(comboArrowColor()), MSAttrValue::Color);
  
  avList_<<MSAttrValue("incrementValue",_incrementValue.asString(),MSAttrValue::String);
  if (_minimumValue.isSet()==MSTrue)
   {
     avList_<<MSAttrValue("minimumValue",_minimumValue.asString(),MSAttrValue::String);
   }
  else avList_<<MSAttrValue("minimumValue","",MSAttrValue::String);
  if (_maximumValue.isSet()==MSTrue)
   {
     avList_<<MSAttrValue("maximumValue",_maximumValue.asString(),MSAttrValue::String);
   }
  else avList_<<MSAttrValue("maximumValue","",MSAttrValue::String);
  return MSEntryFieldPlus::get(avList_);
}



