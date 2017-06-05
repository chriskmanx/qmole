///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1998-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////

#include <MSGUI/MSFontObject.H>
#include <MSGUI/MSCheckMenuItem.H>
#include <MSGUI/MSPixmap.H>


MSCheckMenuItem::MSCheckMenuItem(MSMenu *owner_) : 
    MSToggleMenuItem(owner_) {}

MSCheckMenuItem::MSCheckMenuItem(MSMenu *owner_,const char *label_,char mnemonic_,int tag_) : 
    MSToggleMenuItem(owner_,label_,mnemonic_,tag_) {}

MSCheckMenuItem::MSCheckMenuItem(MSMenu *owner_,const MSString& label_,char mnemonic_,int tag_) : 
    MSToggleMenuItem(owner_,label_,mnemonic_,tag_) {}

MSCheckMenuItem::MSCheckMenuItem(MSMenu *owner_,const MSPixmap &pixmap_,const MSPixmap &insensitivePixmap_,int tag_) :
    MSToggleMenuItem(owner_,pixmap_,insensitivePixmap_,tag_) {}

MSCheckMenuItem::MSCheckMenuItem(MSMenu *menu_,const MSString &label_,const MSPixmap &pixmap_,const MSPixmap &armedPixmap_,
                                 const MSPixmap &insensitivePixmap_,char mnemonic_,int tag_):
    MSToggleMenuItem(menu_,label_,pixmap_,armedPixmap_,insensitivePixmap_,mnemonic_,tag_){}

MSCheckMenuItem::MSCheckMenuItem(MSMenu *menu_,const MSString &label_,const MSPixmap &pixmap_,const MSPixmap &armedPixmap_,
                                 char mnemonic_,int tag_):
    MSToggleMenuItem(menu_,label_,pixmap_,armedPixmap_,pixmap_,mnemonic_,tag_){}

MSCheckMenuItem::MSCheckMenuItem(MSMenu *menu_,const char *label_,const MSPixmap &pixmap_,const MSPixmap &armedPixmap_,
                                 const MSPixmap &insensitivePixmap_,char mnemonic_,int tag_):
    MSToggleMenuItem(menu_,label_,pixmap_,armedPixmap_,insensitivePixmap_,mnemonic_,tag_){}

MSCheckMenuItem::MSCheckMenuItem(MSMenu *menu_,const char *label_,const MSPixmap &pixmap_,const MSPixmap &armedPixmap_,
                                 char mnemonic_,int tag_):
    MSToggleMenuItem(menu_,label_,pixmap_,armedPixmap_,pixmap_,mnemonic_,tag_){}

MSCheckMenuItem::~MSCheckMenuItem(void) {}

void MSCheckMenuItem::drawSymbol(void)
{
  if (owner()->mapped()==MSTrue &&  (showDisarmState()==MSTrue || armed()==MSTrue)  )
    {
      int offset=highlightThickness()+shadowThickness();
      int tw=fontObject()->textHeight();
      int x=(int)(x_origin()+marginWidth()+offset+tw*.1);
      int size=(int)(tw*.8);
      int sht=2;
      
      if (size%2==0) size--;
       
      int delta=(height()-2*(offset+marginHeight())-size);
      delta=(delta>0)?delta>>1:0;
      int y=y_origin()+marginHeight()+offset+delta;
      MSRect rect(x,y,size,size);
      drawBevel(rect,(armed()==MSTrue)?MSSunken:MSRaised,sht);
      
      if (size>2*shadowThickness()+1)
	{
	  XFillRectangle(display(),owner()->window(),
			 (armed()==MSTrue)?_selectMSGC.gc():backgroundShadowGC(),
			 x+sht,y+sht,size-2*sht,size-2*sht);
	}
    }
}

int MSCheckMenuItem::symbolHeight(void) const
{  return fontObject()->textHeight(); }
int MSCheckMenuItem::symbolWidth(void) const
{  return fontObject()->textHeight(); }

