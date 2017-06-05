///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////

#include <MSToggleButton.H>

MSToggleButton::MSToggleButton(MSWidget *owner_,const char *label_,const MSSymbol& tag_)
    : MSToggleButtonBase(owner_,label_,tag_)
{init();}

MSToggleButton::MSToggleButton(MSWidget *owner_,const MSStringVector& label_,
                               const MSSymbol& tag_) : 
    MSToggleButtonBase(owner_,label_,tag_)
{init();}

MSToggleButton::MSToggleButton(MSWidget *owner_,const MSPixmap &pixmap_,
                               const MSPixmap &insensitivePixmap_,
                               const MSPixmap &armedPixmap_,const MSSymbol& tag_) :
    MSToggleButtonBase(owner_,pixmap_,insensitivePixmap_,armedPixmap_,tag_)
{init();}

MSToggleButton::MSToggleButton(MSWidget *owner_,const char *label_,int integerTag_) :
    MSToggleButtonBase(owner_,label_,integerTag_)
{init();}

MSToggleButton::MSToggleButton(MSWidget *owner_,const MSStringVector& label_,int integerTag_) :
    MSToggleButtonBase(owner_,label_,integerTag_)
{init();}

MSToggleButton::MSToggleButton(MSWidget *owner_,const MSPixmap &pixmap_,
                               const MSPixmap &insensitivePixmap_,
                               const MSPixmap &armedPixmap_,int integerTag_) :
    MSToggleButtonBase(owner_,pixmap_,insensitivePixmap_,armedPixmap_,integerTag_)
{init();}

MSToggleButton::~MSToggleButton() 
{
}

void MSToggleButton::init(void)
{
  _spacing=0;
  _toggleShadowThickness =0;
}

void MSToggleButton::redraw(void)
{
  if (owner()->mapped()==MSTrue)
   {
     int ht=highlightThickness();
     XFillRectangle(display(),window(),
                    (armed()==MSTrue)?selectGC():backgroundShadowGC(),
                    ht,ht,width()-(ht<<1),height()-(ht<<1));
     
     if(highlighted()==MSTrue) drawHighlight();
     else undrawHighlight();
     if (pixmap()==0) drawLabel();
     else drawPixmap();

     (armed()==MSTrue)?drawSunken():drawRaised();
   }
}


void MSToggleButton::disarm(void)
{
  if (armed()==MSTrue)
   {
     setDisarmState();
     redraw();
   }
}

void MSToggleButton::arm(void)
{
  if (armed()==MSFalse)
   {
     setArmState();
     redraw();
   }
}
