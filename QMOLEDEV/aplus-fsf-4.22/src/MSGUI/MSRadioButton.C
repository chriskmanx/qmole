///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////

#include <MSGUI/MSRadioButton.H>
#include <MSGUI/MSRadioBox.H>

MSRadioButton::MSRadioButton(MSRadioBox *owner_,const char *label_,const MSSymbol& tag_) : 
MSToggleButtonBase(owner_,label_,tag_)
{}

MSRadioButton::MSRadioButton(MSRadioBox *owner_,const MSStringVector& label_,
			     const MSSymbol& tag_) : 
MSToggleButtonBase(owner_,label_,tag_)
{}

MSRadioButton::MSRadioButton(MSRadioBox *owner_,const MSPixmap &pixmap_,
			     const MSPixmap &insensitivePixmap_,
			     const MSPixmap &armedPixmap_,const MSSymbol& tag_) :
MSToggleButtonBase(owner_,pixmap_,insensitivePixmap_,armedPixmap_,tag_)
{}

MSRadioButton::MSRadioButton(MSRadioBox *owner_,const char *label_,int integerTag_) :
MSToggleButtonBase(owner_,label_,integerTag_)
{}

MSRadioButton::MSRadioButton(MSRadioBox *owner_,const MSStringVector& label_,int integerTag_) :
MSToggleButtonBase(owner_,label_,integerTag_)
{}

MSRadioButton::MSRadioButton(MSRadioBox *owner_,const MSPixmap &pixmap_,
			     const MSPixmap &insensitivePixmap_,
			     const MSPixmap &armedPixmap_,int integerTag_) :
MSToggleButtonBase(owner_,pixmap_,insensitivePixmap_,armedPixmap_,integerTag_)
{}

MSRadioButton::~MSRadioButton(void) {}

// widgetType will go away when dynamic_cast (RTTI) operator becomes available
// this method is suppose to emulate the dynamic_cast operator
MSRadioBox *MSRadioButton::radioBox(void) const
{
  if (parentWidgetType()==MSRadioBox::symbol()) return (MSRadioBox *)owner();
  else return 0;
}

void MSRadioButton::radioBoxArm(void) 
{
  MSRadioBox *rbox=radioBox();
  if (rbox!=0) rbox->arm(this);
}

void MSRadioButton::radioBoxDisarm(void) 
{
  MSRadioBox *rbox=radioBox();
  if (rbox!=0) rbox->disarm();
}

void MSRadioButton::radioBoxActivate(void) 
{
  MSRadioBox *rbox=radioBox();
  if (rbox!=0) rbox->activate();
}

void MSRadioButton::disarm(void) 
{}

void MSRadioButton::arm(MSBoolean callback_)
{
  if (armed()==MSFalse)
   {
     radioBoxDisarm();
     setArmState();
     radioBoxArm(); 
     if (callback_==MSTrue)
      {
	if (activateCallback(MSWidgetCallback::arm)==MSFalse) 
	radioBoxActivate();
      }
   }
}

void MSRadioButton::arm(void) 
{ arm(MSTrue); }

void MSRadioButton::drawSymbol(void) 
{ drawDiamond(); }

void MSRadioButton::drawDiamond(void)
{
  if (mapped()==MSTrue&&owner()->mapped()==MSTrue)
   {
     int offset=highlightThickness()+shadowThickness();
     int x=offset+margin();
     int size=textHeight();
     
     if (size%2==0) size--;
     
     int delta=(height()-2*(offset+margin())-size);
     delta=(delta>0)?delta>>1:0;
     int y=offset+margin()+delta;

     if (armed()==MSTrue) selectMSGC().foreground(selectColor());
     drawDiamondShadow(window(),MSRect(x,y,size,size),armed(),
		       topShadowGC(),bottomShadowGC(),
		       backgroundShadowGC(),selectGC());
   }
}

void MSRadioButton::up(void) { if (radioBox()) radioBox()->up(); }
void MSRadioButton::down(void) { if (radioBox()) radioBox()->down(); }
void MSRadioButton::left(void) { if (radioBox()) radioBox()->left(); }
void MSRadioButton::right(void) { if (radioBox()) radioBox()->right(); }

void MSRadioButton::set(MSAttrValueList& avList_)
{ MSToggleButtonBase::set(avList_); }

MSAttrValueList& MSRadioButton::get(MSAttrValueList& avList_)
{
  avList_<<MSAttrValue("arm","",MSAttrValue::Callback);
  return MSToggleButtonBase::get(avList_);
}





