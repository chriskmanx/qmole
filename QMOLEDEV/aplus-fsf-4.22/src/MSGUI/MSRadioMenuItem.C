///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1998-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////

#include <MSGUI/MSFontObject.H>
#include <MSGUI/MSMenu.H>
#include <MSGUI/MSRadioMenuItem.H>
#include <MSGUI/MSPixmap.H>

MSRadioMenuItem::MSRadioMenuItem(MSMenu *owner_) : 
MSToggleMenuItem(owner_)
{}

MSRadioMenuItem::MSRadioMenuItem(MSMenu *owner_,const char *label_,char mnemonic_,int tag_) : 
MSToggleMenuItem(owner_,label_,mnemonic_,tag_)
{}

MSRadioMenuItem::MSRadioMenuItem(MSMenu *owner_,const MSString& label_,
				 char mnemonic_,int tag_) : 
MSToggleMenuItem(owner_,label_,mnemonic_,tag_)
{}

MSRadioMenuItem::MSRadioMenuItem(MSMenu *owner_,const MSPixmap &pixmap_,
				 const MSPixmap& insensitivePixmap_,int tag_) :
MSToggleMenuItem(owner_,pixmap_,insensitivePixmap_,tag_)
{}

MSRadioMenuItem::MSRadioMenuItem(MSMenu *menu_,const MSString &label_,const MSPixmap &pixmap_,const MSPixmap &armedPixmap_,const MSPixmap &insensitivePixmap_,char mnemonic_,int tag_):
  MSToggleMenuItem(menu_,label_,pixmap_,armedPixmap_,insensitivePixmap_,mnemonic_,tag_){}

MSRadioMenuItem::MSRadioMenuItem(MSMenu *menu_,const MSString &label_,const MSPixmap &pixmap_,const MSPixmap &armedPixmap_,char mnemonic_,int tag_):
  MSToggleMenuItem(menu_,label_,pixmap_,armedPixmap_,pixmap_,mnemonic_,tag_){}

MSRadioMenuItem::MSRadioMenuItem(MSMenu *menu_,const char *label_,const MSPixmap &pixmap_,const MSPixmap &armedPixmap_,const MSPixmap &insensitivePixmap_,char mnemonic_,int tag_):
  MSToggleMenuItem(menu_,label_,pixmap_,armedPixmap_,insensitivePixmap_,mnemonic_,tag_){}

MSRadioMenuItem::MSRadioMenuItem(MSMenu *menu_,const char *label_,const MSPixmap &pixmap_,const MSPixmap &armedPixmap_,char mnemonic_,int tag_):
  MSToggleMenuItem(menu_,label_,pixmap_,armedPixmap_,pixmap_,mnemonic_,tag_){}

MSRadioMenuItem::~MSRadioMenuItem(void) {}

void MSRadioMenuItem::activate(void) 
{ 
  if (menu()!=0)
   {
     menu()->releaseGrab();
     state(MSTrue);
     if (showState()==ShowBoth) drawPixmap();
     else drawSymbol();
     menu()->enforceRadioBehavior();
     if (activateCallback(MSWidgetCallback::activate)==MSFalse) menu()->activate();
     else menu()->done();
   }
}

void MSRadioMenuItem::radioDisarm(void)
{
  if (state()==MSTrue)
   {
     state(MSFalse);
     if (showState()==ShowBoth) drawPixmap();
     else drawSymbol();
   }
}

void MSRadioMenuItem::drawSymbol(void)
{
  if (owner()->mapped()==MSTrue && (showDisarmState()==MSTrue || armed()==MSTrue) )
   {
     int offset=highlightThickness()+shadowThickness();
     int x=x_origin()+marginWidth()+offset;
     int size=fontObject()->textHeight();
     
     if (size%2==0) size--;
     
     int delta=(height()-2*(offset+marginHeight())-size);
     delta=(delta>0)?delta>>1:0;
     int y=y_origin()+marginHeight()+offset+delta;

     drawDiamondShadow(owner()->window(),
		       MSRect(x,y,size,size),armed(),
		       topShadowGC(),bottomShadowGC(),
		       backgroundShadowGC(),_selectMSGC.gc()); // selectShadowGC());
   }
}


int MSRadioMenuItem::symbolHeight(void) const
{  return fontObject()->textHeight(); }
int MSRadioMenuItem::symbolWidth(void) const
{  return fontObject()->textHeight(); }
