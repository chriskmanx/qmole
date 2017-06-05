///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////

#include <MSGUI/MSPixmap.H>
#include <MSGUI/MSFontObject.H>
#include <MSGUI/MSArrow.H>
#include <MSGUI/MSCascadeMenu.H>
#include <MSGUI/MSCascadeMenuItem.H>

MSCascadeMenuItem::MSCascadeMenuItem(MSMenu *owner_) : 
MSMenuItem(owner_) { init(); }
MSCascadeMenuItem::MSCascadeMenuItem(MSMenu *owner_,const char *label_,char mnemonic_,int tag_) : 
MSMenuItem(owner_,label_,mnemonic_,tag_) { init(); }
MSCascadeMenuItem::MSCascadeMenuItem(MSMenu *owner_,const MSString& label_,char mnemonic_,int tag_) : 
MSMenuItem(owner_,label_,mnemonic_,tag_) { init(); }
MSCascadeMenuItem::MSCascadeMenuItem(MSMenu *owner_,const MSPixmap &pixmap_,
				     const MSPixmap &insensitivePixmap_,int tag_) :
MSMenuItem(owner_,pixmap_,insensitivePixmap_,tag_) { init(); }

MSCascadeMenuItem::~MSCascadeMenuItem(void) 
{ 
  delete _arrow; 
  if (cascadeMenu()!=0) safeDestroy(cascadeMenu());
}

void MSCascadeMenuItem::init(void) 
{ 
  _cascade=MSTrue;
  _cascadeMenu=0;
  _arrow=new MSArrow(menu(),MSArrow::Right);
  arrow()->resize(fontObject()->textHeight()>>1,fontObject()->textHeight()>>1);
}

MSMenu* MSCascadeMenuItem::cascadedMenu(void)             { return (MSMenu*)_cascadeMenu; }
const MSMenu* MSCascadeMenuItem::cascadedMenu(void) const { return (MSMenu*)_cascadeMenu; }

void MSCascadeMenuItem::grab(void) 
{ if (cascadeMenu()!=0) cascadeMenu()->grabAndSelect(); }

void MSCascadeMenuItem::arm(void) 
{
  if (cascadeMenu()!=0) 
   {
     if(cascadeMenu()->mapped()==MSFalse) menushow();
     arrow()->select(MSTrue);
     int x=menu()->x_origin()+x_origin()+width()-4;
     int y=menu()->y_origin()+y_origin();     
     // If the cascade Menu has never been mapped, call map here so
     // it can calculate its size
     if (cascadeMenu()->firstMap()==MSFalse) 
     {
	cascadeMenu()->moveTo(server()->width(),server()->height());
	cascadeMenu()->map();
     }
     // Only adjust x if the left side of menu is within the screen
     if (x+cascadeMenu()->width()>server()->width())
      {
	int newx=server()->width()-cascadeMenu()->width();
	if (newx>0) x=newx;
	else x=0;
      }
     // Only adjust y if the top of menu is within the screen
     if (y+cascadeMenu()->height()>server()->height())
      {
	int newy=server()->height()-cascadeMenu()->height();
	if (newy>0) y=newy;
	else y=0;
      }
     cascadeMenu()->moveTo(x,y);
     cascadeMenu()->map();
     cascadeMenu()->raise();
   }
}

void MSCascadeMenuItem::disarm(void) 
{
  if (cascadeMenu()!=0) 
   {
     arrow()->select(MSFalse);
     cascadeMenu()->ungrab();
     cascadeMenu()->unmap();
   }
}

void MSCascadeMenuItem::cascadeMenu(MSCascadeMenu *menu_)
{
  if (cascadeMenu()!=menu_)
   {
     if (cascadeMenu()!=0) safeDestroy(cascadeMenu());
     _cascadeMenu=menu_;
   }
}

void MSCascadeMenuItem::menuDestroy(MSCascadeMenu *menu_)
{
  if (cascadeMenu()==menu_) _cascadeMenu=0;
}

void MSCascadeMenuItem::computePixmapDrawingCoord(const MSPixmap *pixmap_,int &srcX_,int &srcY_,
						  int &width_,int &height_,int &destX_,int &destY_)
{
     
   int heightOffset=highlightThickness()+shadowThickness()+marginHeight();
   int widthOffset=highlightThickness()+shadowThickness()+marginWidth();
   int myHeight=height()-heightOffset*2;
   int myWidth=width()-arrow()->width()*2-widthOffset*2;
   if (pixmap_->height()>myHeight)
   {
      height_=myHeight;
      srcY_=(pixmap_->height()-myHeight)/2;
      destY_=heightOffset+y_origin();
   }
   else
   {
      height_=pixmap_->height();
      srcY_=0;
      destY_=heightOffset+y_origin()+(myHeight-pixmap_->height())/2;
   }
   if (pixmap_->width()>myWidth)
   {
      width_=myWidth;
      srcX_=(pixmap_->width()-myWidth)/2;
      destX_=widthOffset+x_origin();
   }
   else
   {
      width_=pixmap_->width();
      srcX_=0;
      destX_=widthOffset+x_origin()+(myWidth-pixmap_->width())/2;
   }
}

void MSCascadeMenuItem::drawSymbol(void) 
{ 
  int offset=highlightThickness()+shadowThickness()+marginWidth();
  int x=x_origin()+width()+marginWidth()-offset-arrow()->width();
  arrow()->moveTo(x,y_origin()+(height()+marginHeight()-(arrow()->height())>>1));
  arrow()->draw(); 
}

void MSCascadeMenuItem::updateFont(Font oldfid_)
{
  MSMenuItem::updateFont(oldfid_);
  arrow()->resize(fontObject()->textHeight()>>1,fontObject()->textHeight()>>1);
  if (cascadeMenu()!=0&&cascadeMenu()->font()==oldfid_)
  {
     cascadeMenu()->font(font());
  }
}

void MSCascadeMenuItem::updateForeground(unsigned long oldfg_)
{
  MSMenuItem::updateForeground(oldfg_);
  if (cascadeMenu()!=0&&cascadeMenu()->foreground()==oldfg_)
  {
     cascadeMenu()->foreground(foreground());
  }
}

void MSCascadeMenuItem::updateBackground(unsigned long oldbg_)
{
  MSMenuItem::updateBackground(oldbg_);
  if (cascadeMenu()!=0&&cascadeMenu()->background()==oldbg_)
  {
     cascadeMenu()->background(background());
  }
}

void MSCascadeMenuItem::computeSize(void)
{
  int widthOffset=(highlightThickness()+shadowThickness()+marginWidth())<<1;
  int heightOffset=(highlightThickness()+shadowThickness()+marginHeight())<<1;
  if (pixmap()==0)
  {
     resize (fontObject()->textWidth(label(),label().length())+(arrow()->width()<<1)+widthOffset+indent(),
	     fontObject()->textHeight()+heightOffset);
  }
  else
  {
     resize (pixmap()->width()+(arrow()->width()<<1)+widthOffset,pixmap()->height()+heightOffset);
  }

}

void MSCascadeMenuItem::menushow()
{ activateCallback(MSWidgetCallback::menushow); }

MSAttrValueList& MSCascadeMenuItem::get(MSAttrValueList& avList_)
{
  avList_<<MSAttrValue("menushow","",MSAttrValue::Callback);
  return MSMenuItem::get(avList_);
}
