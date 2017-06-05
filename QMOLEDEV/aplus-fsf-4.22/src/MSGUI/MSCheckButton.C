///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////

#include <MSGUI/MSCheckButton.H>
#include <MSGUI/MSCheckBox.H>

MSCheckButton::MSCheckButton(MSWidget *owner_,const char *label_,const MSSymbol& tag_) : 
MSToggleButtonBase(owner_,label_,tag_) 
{}
MSCheckButton::MSCheckButton(MSWidget *owner_,const MSStringVector& label_,const MSSymbol& tag_) : 
MSToggleButtonBase(owner_,label_,tag_) 
{}
MSCheckButton::MSCheckButton(MSWidget *owner_,const MSPixmap &pixmap_,
			     const MSPixmap &insensitivePixmap_,
			     const MSPixmap &armedPixmap_,const MSSymbol& tag_) :
MSToggleButtonBase(owner_,pixmap_,insensitivePixmap_,armedPixmap_,tag_)
{}
MSCheckButton::MSCheckButton(MSWidget *owner_,const char *label_,int integerTag_) :
MSToggleButtonBase(owner_,label_,integerTag_) 
{}

MSCheckButton::MSCheckButton(MSWidget *owner_,const MSStringVector& label_,int integerTag_) :
MSToggleButtonBase(owner_,label_,integerTag_) 
{}
MSCheckButton::MSCheckButton(MSWidget *owner_,const MSPixmap &pixmap_,
			     const MSPixmap &insensitivePixmap_,
			     const MSPixmap &armedPixmap_,int integerTag_) :
MSToggleButtonBase(owner_,pixmap_,insensitivePixmap_,armedPixmap_,integerTag_)
{}

MSCheckButton::~MSCheckButton(void) {}

void MSCheckButton::drawSymbol(void) 
{ drawCheckButton(); }

void MSCheckButton::drawCheckButton(void)
{
  if (mapped()==MSTrue&&owner()->mapped()==MSTrue)
   {
     if (armed()==MSTrue) selectMSGC().foreground(selectColor());
     int offset=highlightThickness()+shadowThickness()+margin();
     int tw=textHeight();
     int xx=(int)(offset+tw*.1);
     int size=(int)(tw*.8);
     
     if (size%2==0) size--;
     
     int delta=(height()-2*offset-size);
     delta=(delta>0)?delta>>1:0;
     int yy=offset+delta;
     
     drawBevel(MSRect(xx,yy,size,size),
               (armed()==MSTrue)?MSSunken:MSRaised,toggleShadowThickness());
     
     if (size>2*shadowThickness()+1)
      {
	if(outputMode()==Draw || armed()==MSTrue)
	  { 
	    XBFillRectangle(display(),window(),
			    (armed()==MSTrue)?selectGC():backgroundShadowGC(),
			    xx+toggleShadowThickness(),yy+toggleShadowThickness(),
			    size-2*toggleShadowThickness(),size-2*toggleShadowThickness());
      }
      }
   }
}

// widgetType will go away when dynamic_cast (RTTI) operator becomes available
// this method is suppose to emulate the dynamic_cast operator
MSCheckBox *MSCheckButton::checkBox(void) const
{
  if (parentWidgetType()==MSCheckBox::symbol()) return (MSCheckBox *)owner();
  else return 0;
}

void MSCheckButton::checkBoxArm(void)
{
  MSCheckBox *cbox=checkBox();
  if (cbox!=0) cbox->arm(this);
}

void MSCheckButton::checkBoxDisarm(void)
{
  MSCheckBox *cbox=checkBox();
  if (cbox!=0) cbox->disarm(this);
}

void MSCheckButton::arm(void)
{
  if (armed()==MSFalse)
   {
     setArmState();
     if (activateCallback(MSWidgetCallback::arm)==MSFalse) checkBoxArm();
   }
}

void MSCheckButton::disarm(void)
{
  if (armed()==MSTrue)
   {
     setDisarmState();
     if (activateCallback(MSWidgetCallback::disarm)==MSFalse) checkBoxDisarm();
   }
}

void MSCheckButton::up(void) { if (checkBox()) checkBox()->up(); }
void MSCheckButton::down(void) { if (checkBox()) checkBox()->down(); }
void MSCheckButton::left(void) { if (checkBox()) checkBox()->left(); }
void MSCheckButton::right(void) { if (checkBox()) checkBox()->right(); }

void MSCheckButton::set(MSAttrValueList& avList_)
{ MSToggleButtonBase::set(avList_); }

MSAttrValueList& MSCheckButton::get(MSAttrValueList& avList_)
{
  avList_<<MSAttrValue("arm","",MSAttrValue::Callback);
  avList_<<MSAttrValue("disarm","",MSAttrValue::Callback);
  return MSToggleButtonBase::get(avList_);
}

