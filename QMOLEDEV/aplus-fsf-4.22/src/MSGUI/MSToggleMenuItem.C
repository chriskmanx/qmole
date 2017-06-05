///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1998-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////

#include <MSGUI/MSPixmap.H>
#include <MSGUI/MSFontObject.H>
#include <MSGUI/MSToggleMenuItem.H>
#include <MSTypes/MSMessageLog.H>

MSToggleMenuItem::MSToggleMenuItem(MSMenu *owner_) : 
MSMenuItem(owner_) { init(); }

MSToggleMenuItem::MSToggleMenuItem(MSMenu *owner_,const char *label_,char mnemonic_,int tag_) : 
MSMenuItem(owner_,label_,mnemonic_,tag_) { init(); }

MSToggleMenuItem::MSToggleMenuItem(MSMenu *owner_,const MSString& label_,char mnemonic_,int tag_) : 
MSMenuItem(owner_,label_,mnemonic_,tag_) { init(); }

MSToggleMenuItem::MSToggleMenuItem(MSMenu *owner_,const MSPixmap &pixmap_,const MSPixmap &insensitivePixmap_,int tag_) :
MSMenuItem(owner_,pixmap_,insensitivePixmap_,tag_)
{ init(); }

MSToggleMenuItem::MSToggleMenuItem(MSMenu *menu_,const MSString &label_,const MSPixmap &pixmap_,const MSPixmap &armedPixmap_,
		       const MSPixmap &insensitivePixmap_,char mnemonic_,int tag_):
  MSMenuItem(menu_,label_,pixmap_,insensitivePixmap_,mnemonic_,tag_)
{
  init();
  initArmedPixmap(armedPixmap_);
}

MSToggleMenuItem::MSToggleMenuItem(MSMenu *menu_,const char* label_,const MSPixmap &pixmap_,const MSPixmap &armedPixmap_,
		       const MSPixmap &insensitivePixmap_,char mnemonic_,int tag_):
  MSMenuItem(menu_,label_,pixmap_,insensitivePixmap_,mnemonic_,tag_)
{
  init();
  initArmedPixmap(armedPixmap_);
}

MSToggleMenuItem::~MSToggleMenuItem(void) 
{
   if (_armedPixmap!=0)            delete _armedPixmap;
}

void MSToggleMenuItem::init(void)
{ 
  _selectColor=selectShadowColor();
  XGCValues gcvalues;
  unsigned long valueMask=GCForeground;
  gcvalues.foreground=selectColor();   
  _selectMSGC.setGCValues(server(), MSTrue,&gcvalues,valueMask);
  _armedPixmap=0;
  showDisarmState(MSTrue);
}

void MSToggleMenuItem::initArmedPixmap(const MSPixmap &armedPixmap_)
{
  if (armedPixmap_.server()==server())
    _armedPixmap=new MSPixmap(armedPixmap_);
  else
    {
      MSMessageLog::warningMessage("Warning : armedPixmap supplied is invalid, using default");
      _armedPixmap=createDefaultPixmap(armedPixmap_.width(),armedPixmap_.height(),
				  armedPixmap_.foreground(),armedPixmap_.background());
    }
}

void MSToggleMenuItem::armedPixmap(const MSPixmap &pixmap_)
{
  MSPixmap *tmp=_pixmap;
  if (pixmap_.server()==server())
  _armedPixmap=new MSPixmap(pixmap_);
  else
   {
     MSMessageLog::warningMessage("Warning :armedPixmap supplied for MSToggleMenuItem is invalid, using default");
     _armedPixmap=createDefaultPixmap(pixmap_.width(),pixmap_.height(),
				      pixmap_.foreground(),pixmap_.background());
   }
  if (tmp!=0) delete tmp;
  if (dynamic()==MSTrue) computeSize();
  else if (owner()->mapped()==MSTrue)
   {
     redraw();
     (armed()==MSTrue)?drawSunken():drawRaised();
   }
}

void MSToggleMenuItem::selectColor(const char *color_) 
{ selectColor(server()->pixel(color_)); }

void MSToggleMenuItem::selectColor(unsigned long pixel_)
{ 
  if (_selectColor!=pixel_)
   { 
     _selectColor=pixel_; 
     _selectMSGC.foreground(pixel_);
   }
}

void MSToggleMenuItem::computeSize(void)
{
  int oldW=width();
  int oldH=height();
  int pixmapW=pixmapWidth();
  int pixmapH=pixmapHeight();
  int labelW=labelWidth();
  int labelH=labelHeight();
  int symbolW=symbolWidth();
  int symbolH=symbolHeight();

  int widthOffset=(highlightThickness()+shadowThickness()+marginWidth())<<1;
  int heightOffset=(highlightThickness()+shadowThickness()+marginHeight())<<1;
  int width = widthOffset+indent();
  int height = heightOffset;
  if (showState()==ShowLabel)  // the label is drawn to the right of the symbol
    {
      width  += labelW;
      height +=  labelH>symbolH?labelH:symbolH;
    }
  else if ( showState() == ShowPixmap)  // the pixmap is drawn to the right of the symbol
    {
      width  += pixmapW;
      height +=pixmapH>symbolH?pixmapH:symbolH;
    }
  else
    {
      width += labelW ;
      height += pixmapH>labelH?pixmapH:labelH;
    }
  if(width==oldW&&height==oldH) redraw();
  else resize(width,height);
}


int MSToggleMenuItem::computeIndentation(void)
{ 
  // pixmap is displayed on the left hand side its with therefore is used to compute 
  // indentation
  if ( showState() == ShowBoth)  return pixmap()->width() + spacing();
//    return pixmapWidth() + spacing();
 
  else   
    return    symbolWidth() + spacing();
}

const MSPixmap *MSToggleMenuItem::currentPixmap(void) const 
{
  if (sensitive()==MSFalse) { return insensitivePixmap()!=0 ? insensitivePixmap():0;}
  
  
  if ( showState() ==ShowPixmap ) return pixmap()!=0 ? pixmap():0;
  
  // pixmap is displayed on the left hand side, its value reflects menuItems state
  else 
   {
     if (armed()==MSTrue)return  (armedPixmap()!=0)? armedPixmap():0; 
     else if (showDisarmState()==MSTrue) return pixmap()!=0 ? pixmap():0;
     return 0;
   }

}

int MSToggleMenuItem::symbolHeight(void) const
{  return 0; }
int MSToggleMenuItem::symbolWidth(void) const
{  return 0; }

 void MSToggleMenuItem::set(MSAttrValueList& avList_)
{
  MSMenuItem::set(avList_);
  MSIndexVector index;
  for (unsigned i=0;i<avList_.length();i++)
   {
     if (avList_[i].attribute()=="selectColor") selectColor(avList_[i].value()),index<<i;  
     else if (avList_[i].attribute()=="showDisarmState")
         showDisarmState(avList_[i].value().asBoolean()),index<<i;
   }
  avList_.remove(index);
}

MSAttrValueList& MSToggleMenuItem::get(MSAttrValueList& avList_)
{
  avList_<<MSAttrValue("selectColor",server()->colorName(selectColor()),MSAttrValue::Color);
  MSStringVector aStringVector("MSTrue\nMSFalse");
  avList_<<MSAttrValue("showDisarmState",
		       (showDisarmState()==MSTrue?"MSTrue":"MSFalse"),
		       aStringVector);

  return MSMenuItem::get(avList_);
}
