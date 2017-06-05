///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////

#include <MSGUI/MSActionButton.H>
#include <MSGUI/MSActionBox.H>
#include <MSGUI/MSPixmap.H>

static const unsigned long MSActionButtonEventMask=(ExposureMask|ButtonPressMask|
                                                    ButtonReleaseMask|Button1MotionMask);

MSActionButton::MSActionButton(MSWidget *owner_,const char *label_,const MSSymbol& tag_) : 
MSButton(owner_,label_),_tag(tag_),
_integerTag(0)
{
  init();
}

MSActionButton::MSActionButton(MSWidget *owner_,const MSStringVector& label_,const MSSymbol& tag_) : 
MSButton(owner_,label_),_tag(tag_),
_integerTag(0)
{
  init();
}

MSActionButton::MSActionButton(MSWidget *owner_,const MSPixmap &pixmap_,
			       const MSPixmap &insensitivePixmap_,
			       const MSPixmap &armedPixmap_,const MSSymbol& tag_) :
MSButton(owner_,pixmap_,insensitivePixmap_,armedPixmap_),
_tag(tag_),_integerTag(0)
{
  init();
}

MSActionButton::MSActionButton(MSWidget *owner_,const char *label_,int integerTag_) : 
MSButton(owner_,label_),
_integerTag(integerTag_),_tag(MSSymbol::nullSymbol())
{
  init();
}

MSActionButton::MSActionButton(MSWidget *owner_,const MSStringVector& label_,int integerTag_) : 
MSButton(owner_,label_),
_integerTag(integerTag_),_tag(MSSymbol::nullSymbol())
{
  init();
}

MSActionButton::MSActionButton(MSWidget *owner_,const MSPixmap &pixmap_,
			       const MSPixmap &insensitivePixmap_,
			       const MSPixmap &armedPixmap_,int integerTag_) :
MSButton(owner_,pixmap_,insensitivePixmap_,armedPixmap_),
_integerTag(integerTag_),_tag(MSSymbol::nullSymbol())
{
  init();
}

MSActionButton::~MSActionButton(void)
{
}

void MSActionButton::init(void)
{
  selectInput(MSActionButtonEventMask);
}

void MSActionButton::state(MSBoolean state_) 
{ 
  if (state_!=armed()) 
   { 
     _armed=state_; 
     change(); 
   } 
}

void MSActionButton::tag(const MSSymbol& tag_) 
{ _tag=tag_; }

void MSActionButton::integerTag(int integerTag_) 
{ _integerTag=integerTag_; }

void MSActionButton::redraw(void)
{
  if (owner()->mapped()==MSTrue&&owner()->frozen()==MSFalse)
   { 
     drawBackground(); 
     drawSymbol(); 
     if (pixmap()==0) drawLabel(); 
     else drawPixmap();
     (armed()==MSTrue)?drawSunken():drawRaised();
   }
}

void MSActionButton::change(void) 
{ 
   drawSymbol(); 
   boolModel()=state();
   if (pixmap()!=0) drawPixmap();
}

void MSActionButton::drawSymbol(void) {}

// widgetType will go away when dynamic_cast (RTTI) operator becomes available
// this method is suppose to emulate the dynamic_cast operator
MSActionBox *MSActionButton::actionBox(void) const
{
  if (parentWidgetType()==MSActionBox::symbol()) return (MSActionBox *)owner();
  else return 0;
}

void MSActionButton::actionBoxActivate(void)
{
  MSActionBox *abox=actionBox();
  if (abox!=0) abox->activate(this);
}

void MSActionButton::defaultActivate(void) 
{ 
  if (armed()==MSTrue) 
   {
     if (activateCallback(MSWidgetCallback::activate)==MSFalse) actionBoxActivate(); 
   }
}

void MSActionButton::activate(void) 
{ defaultActivate(); }

void MSActionButton::setArmState(void)
{
  if (armed()==MSFalse)
   {
     _armed=MSTrue;
     if (boolModel()!=state()) boolModel()=state();
     if (pixmap()!=0) redraw();
     else drawSymbol();
   }
}

void MSActionButton::setDisarmState(void)
{
  if (armed()==MSTrue)
   {
     _armed=MSFalse;
     if (boolModel()!=state()) boolModel()=state();
     if (pixmap()!=0) redraw();
     drawSymbol();
   }
}

void MSActionButton::arm(void)
{
  if (armed()==MSFalse)
   {
      _armed=MSTrue;
      if (boolModel()!=state()) boolModel()=state();
      redraw();
   }
}

void MSActionButton::disarm(void)
{
  if (armed()==MSTrue)
   {
      _armed=MSFalse;
      if (boolModel()!=state()) boolModel()=state();
      redraw();
   }
}

void MSActionButton::up(void) { if (actionBox()) actionBox()->up(); }
void MSActionButton::down(void) { if (actionBox()) actionBox()->down(); }
void MSActionButton::left(void) { if (actionBox()) actionBox()->left(); }
void MSActionButton::right(void) { if (actionBox()) actionBox()->right(); }


void MSActionButton::boolModelChanged(MSEvent &)
{
  if (boolModel()!=state())
   { 
     if (boolModel()==MSBool(MSTrue))
      {
        arm();
      }
     else
      {
        disarm();
      }
   }
}

void MSActionButton::newBoolModelNotify(void)
{state(boolModel());}

void MSActionButton::set(MSAttrValueList& avList_)
{  
  MSButton::set(avList_);
  MSIndexVector index;
  for (unsigned i=0;i<avList_.length();i++)
    {
      if(avList_[i].attribute()=="tag")
       {
         if(avList_[i].value().length()==0) tag(MSSymbol::nullSymbol());
         else tag(MSSymbol(avList_[i].value()));
         index<<i;
       }
      else if(avList_[i].attribute()=="integerTag") integerTag(avList_[i].value().asInt()),index<<i;
    }
  avList_.remove(index);
}


MSAttrValueList& MSActionButton::get(MSAttrValueList& avList_)
{
  avList_<<MSAttrValue("tag",tag().symbolName(),MSAttrValue::Control|MSAttrValue::String);
  avList_<<MSAttrValue("integerTag",MSString(integerTag()),MSAttrValue::Control);
  return MSButton::get(avList_);
}




