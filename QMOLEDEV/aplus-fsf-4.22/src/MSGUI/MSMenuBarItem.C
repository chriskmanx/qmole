///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////

#include <MSGUI/MSFontObject.H>
#include <MSGUI/MSPulldownMenu.H>
#include <MSGUI/MSMenuBar.H>
#include <MSGUI/MSMenuBarItem.H>

MSMenuBarItem::MSMenuBarItem(MSMenuBar *owner_) : 
MSMenuItem(owner_) { init(); }
MSMenuBarItem::MSMenuBarItem(MSMenuBar *owner_,const char *label_,char mnemonic_,int tag_) : 
MSMenuItem(owner_,label_,mnemonic_,tag_) { init(); }
MSMenuBarItem::MSMenuBarItem(MSMenuBar *owner_,const MSString& label_,char mnemonic_,int tag_) : 
MSMenuItem(owner_,label_,mnemonic_,tag_) { init(); }
MSMenuBarItem::MSMenuBarItem(MSMenuBar *owner_,const MSPixmap &pixmap_,
			     const MSPixmap &insensitivePixmap_,int tag_) :
MSMenuItem(owner_,pixmap_,insensitivePixmap_,tag_) { init(); }

MSMenuBarItem::~MSMenuBarItem(void) 
{
  if (pulldownMenu()!=0) safeDestroy(pulldownMenu()); 
}

void MSMenuBarItem::init(void) 
{ 
  _cascade=MSFalse;
  _pulldownMenu=0;
}

MSMenu* MSMenuBarItem::cascadedMenu(void)             { return (MSMenu*)_pulldownMenu; }
const MSMenu* MSMenuBarItem::cascadedMenu(void) const { return (MSMenu*)_pulldownMenu; }


void MSMenuBarItem::grab(void) 
{ if (pulldownMenu()!=0) pulldownMenu()->grabAndSelect(); }

void MSMenuBarItem::arm(void) 
{
  if (pulldownMenu()!=0) 
   {
     if (pulldownMenu()->mapped()==MSFalse) menushow();
     
     int rootx=0,rooty=0;
     owner()->rootXY(rootx,rooty);
     int x=rootx+x_origin();    
     int y=rooty+y_origin();
     if(((MSMenuBar*)owner())->orientation()==MSMenuBar::Horizontal)
      {
        y+=height();
      }
     else
      {
        x+=width();
      }
     // If the pulldown Menu has never been mapped, call map here so
     // it can calculate its size
     if (pulldownMenu()->firstMap()==MSFalse) 
     {
	pulldownMenu()->moveTo(server()->width(),server()->height());
	pulldownMenu()->map();
     }
     // Only adjust x if the left side of menu is within the screen
     if (x+pulldownMenu()->width()>server()->width())
      {
	int newx=server()->width()-pulldownMenu()->width();
	if (newx>0) x=newx;
	else x=0;
      }
     // Only pull up if the top of menu is within the screen
     if (y+pulldownMenu()->height()>server()->height())
      {
	int newy=rooty+y_origin()-pulldownMenu()->height();
	if (newy>0) y=newy;
      }
     
     pulldownMenu()->moveTo(x,y);
     pulldownMenu()->map();
     pulldownMenu()->raise();
   }
}

void MSMenuBarItem::disarm(void) 
{
  if (pulldownMenu()!=0) 
   {
     pulldownMenu()->ungrab();
     pulldownMenu()->unmap();
   }
}

void MSMenuBarItem::pulldownMenu(MSPulldownMenu *menu_)
{
  if (pulldownMenu()!=menu_)
   {
     if (pulldownMenu()!=0) safeDestroy(pulldownMenu());
     _pulldownMenu=menu_;
     if (pulldownMenu()!=0) _cascade=MSTrue;
     else _cascade=MSFalse;
   }
}

void MSMenuBarItem::menuDestroy(MSPulldownMenu *menu_)
{
  if (pulldownMenu()==menu_)
  {
     _pulldownMenu=0;
     _cascade=MSFalse;
  }
}

int MSMenuBarItem::computeYCoord(void)
{
   int offset=highlightThickness()+shadowThickness()+marginHeight();
   int diff=(height()-offset*2-fontObject()->textHeight())/2;
   if (diff<=0) return offset+y_origin();
   else return offset+diff+y_origin();
}

void MSMenuBarItem::updateFont(Font oldfid_)
{
  MSMenuItem::updateFont(oldfid_);
  if (pulldownMenu()!=0&&pulldownMenu()->font()==oldfid_)
  {
     pulldownMenu()->font(font());
  }
}

void MSMenuBarItem::updateForeground(unsigned long oldfg_)
{
  MSMenuItem::updateForeground(oldfg_);
  if (pulldownMenu()!=0&&pulldownMenu()->foreground()==oldfg_)
  {
     pulldownMenu()->foreground(foreground());
  }
}

void MSMenuBarItem::updateBackground(unsigned long oldbg_)
{
  MSMenuItem::updateBackground(oldbg_);
  if (pulldownMenu()!=0&&pulldownMenu()->background()==oldbg_)
  {
     pulldownMenu()->background(background());
  }
  //undraw shadow because MSWidgetOutput::updateBackground will draw it.
  if (selected()==MSFalse) undrawShadow();
}

void MSMenuBarItem::menushow()
{ activateCallback(MSWidgetCallback::menushow); }

MSAttrValueList& MSMenuBarItem::get(MSAttrValueList& avList_)
{
  avList_<<MSAttrValue("menushow","",MSAttrValue::Callback);
  return MSMenuItem::get(avList_);
}
