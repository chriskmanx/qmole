///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////

#include <MSGUI/MSMenuBar.H>

static const int MSMenuBarDefaultEntryBorder=2;

MSMenuBar::MSMenuBar(MSWidget *owner_) : MSMenu(owner_) 
{
   _entryBorder=MSMenuBarDefaultEntryBorder;
   _orientation=Horizontal;
}

MSMenuBar::~MSMenuBar(void) {}

void MSMenuBar::unmap(void)
{
  if (mapped()==MSTrue)
   {
     pulldownMenus();
     MSWidgetCommon::unmap();
     reset();
   }
}

void MSMenuBar::map(void)
{
  if (mapped()==MSFalse)
   {
     MSWidgetCommon::map();
     XFlush(display());
   }
}

MSMenuItem *MSMenuBar::nextRightItem(void)
{
   // This nextRightItem logic wraps, i.e. if it reaches the right most item
   // then try from the first item again until it either finds one or it returns
   // to the starting item.
   if (itemCount()>0)
   {
      int item=selectedItem();
      MSMenuItem *mi=menuItem(item);
      MSMenuItem *ni=0;
      if (mi==0||item<0)
	 item=-1;
      // Loop no more than the number of items
      for (int i=0; i<itemCount();i++)
      {
	 if (item+1==itemCount()) item=0;
	 else item++;
	 ni=menuItem(item);
	 if (ni==mi || ni->sensitive()==MSTrue)
	    return ni;
      }
   }
   return 0;
}

MSMenuItem *MSMenuBar::nextLeftItem(void)
{
   // This nextLeftItem logic wraps, i.e. if it reaches the left most item
   // then try from the right most item again until it either finds one or it returns
   // to the starting item.
   if (itemCount()>0)
   {
      int item=selectedItem();
      MSMenuItem *mi=menuItem(item);
      MSMenuItem *ni=0;
      if (mi==0||item<0)
	 item=1;;
      // Loop no more than the number of items
      for (int i=0; i<itemCount();i++)
      {
	 if (item-1<0) item=itemCount()-1;
	 else item--;
	 ni=menuItem(item);
	 if (ni==mi || ni->sensitive()==MSTrue)
	    return ni;
      }
   }
   return 0;
}

MSMenu *MSMenuBar::selectedMenu(void)
{
  MSMenu *menu=(MSMenu *)this;
  MSMenuItem *mi=activeMenuItem();
  while (mi!=0&&mi->cascade()==MSTrue&&mi->cascadedMenu()!=0)
   {
     menu=mi->cascadedMenu();
     mi=menu->activeMenuItem();
   }
  return menu;
}

const MSMenu *MSMenuBar::selectedMenu(void) const
{
  const MSMenu *menu=(MSMenu *)this;
  const MSMenuItem *mi=activeMenuItem();
  while (mi!=0&&mi->cascade()==MSTrue&&mi->cascadedMenu()!=0)
   {
     menu=mi->cascadedMenu();
     mi=menu->activeMenuItem();
   }
  return menu;
}

void MSMenuBar::done(void)
{
  menuBar(0);
  menuList().removeFromList(this);
  releaseGrab();
  pulldownMenus();
  reset();
}

void MSMenuBar::dropMenu(MSMenuItem *mi_)
{
  if (mi_!=0&&mi_->cascade()==MSTrue)
   {
     mi_->arm();
     mi_->grab();
   }
}

void MSMenuBar::moveToMenu(MSMenuItem *nextItem_)
{
  MSMenuItem *mi=menuItem(selectedItem());
  if (nextItem_!=0&&mi!=nextItem_)
   {
     if (mi!=0&&mi->cascade()==MSTrue) mi->disarm();
     undrawSelectedItem();
     selectedItem(nextItem_->item());
     drawSelectedItem();
     dropMenu(nextItem_);
   }
}

void MSMenuBar::up(void)
{
  if(orientation()==Horizontal)
   {
     dropMenu(menuItem(selectedItem()));
   }
  else
   {
     moveToMenu(nextLeftItem());
   }
}

void MSMenuBar::down(void)
{
  if(orientation()==Horizontal)
   {
     dropMenu(menuItem(selectedItem()));
   }
  else
   {
     moveToMenu(nextRightItem());
   } 
}

void MSMenuBar::left(void)
{
  moveToMenu(nextLeftItem()); 
}

void MSMenuBar::right(void)
{
  moveToMenu(nextRightItem());
}

void MSMenuBar::computeSize(void)
{
  if (firstMap()==MSTrue&&frozen()==MSFalse)
   {
     freeze();
     MSMenuItem *mi;
     int th=0,tw=0;
     int h=0,w=0;
     int offset=(entryBorder()+highlightThickness()+shadowThickness())<<1;
     int i,n=itemCount();

     if(orientation()==Horizontal)
      {
        for(i=0;i<n;i++)
         {
           mi=(MSMenuItem *)itemVector()(i);
           mi->naturalSize();
           th=mi->height();
           if (h<th) h=th;
           w+=mi->width();
         }
      }
     else
      {
        for(i=0;i<n;i++)
         {
           mi=(MSMenuItem *)itemVector()(i);
           mi->naturalSize();
           tw=mi->width();
           if (w<tw) w=tw;
           h+=mi->height();
         }
      }
     
     unfreeze();
     if (w+offset==width()&&h+offset==height()) configure();
     else resize(w+offset,h+offset);
   }
}

void MSMenuBar::placement(void)
{
  if (firstMap()==MSTrue&&frozen()==MSFalse)
   {
     freeze();
     MSMenuItem *mi;
     int offset=entryBorder()+highlightThickness()+shadowThickness();
     int y=offset;  
     int x=offset;
     int item=0;
     int i,n=itemCount();
     if(orientation()==Horizontal)
      {
        int h=height()-(offset<<1);
        for(i=0;i<n;i++)
         {
           mi=(MSMenuItem *)itemVector()(i);
           mi->height(h);
           setItem(mi,item++);
           if (mi->label()=="Help") 
            {
              mi->moveTo(width()-offset-mi->width(),y);
            }
           else
            { 
              mi->moveTo(x,y);
              x+=mi->width();
            }
         }
      }
     else
      {
        int w=width()-(offset<<1);
        for (i=0;i<n;i++)
         {
           mi=(MSMenuItem *)itemVector()(i);
           mi->width(w);
           setItem(mi,item++);
           mi->moveTo(x,y);
           y+=mi->height();
         }
      }
     unfreeze();
   }
}

void MSMenuBar::keyPress(const XEvent *pEvent_,KeySym keysym_,unsigned int state_,const char *pString_)
{
  MSKeyPress keyPress(keysym_, state_);
  if(keyTranslate(keyPress) == MSFalse)
   {
     menuBar(this);
     MSMenu::keyPress(pEvent_,keysym_,state_,pString_);
   }
}

void MSMenuBar::buttonPress(const XEvent *event_)
{
  if (event_->xbutton.button==Button1&&sensitive()==MSTrue)
   {
     MSBoolean proceed=(acceptFocus()==MSTrue)?traverseFocus(this):MSTrue;
     if (proceed==MSTrue)
      {
	if (findItem(event_->xbutton.x_root,event_->xbutton.y_root)!=0)
	 {
	   menuBar(this);
	   menuList().addToList(this);
	   MSMenu::buttonPress(event_);
	 }
      }
   }
}

void MSMenuBar::orientation(Orientation orientation_)
{
  if(orientation()!=orientation_)
   {
     _orientation=orientation_;
     computeSize();
   }
}

MSAttrValueList& MSMenuBar::get(MSAttrValueList& avList_)
{
  avList_<<MSAttrValue("orientation",orientation()==Horizontal?"Horizontal":"Vertical",
                       MSStringVector("Horizontal\nVertical"));
  return MSMenu::get(avList_);
}

void MSMenuBar::set(MSAttrValueList& avList_)
{
  MSMenu::set(avList_);
  MSIndexVector index;
  for (int i=0;i<avList_.length();i++)
   {
     if(avList_[i].attribute()=="orientation")
      {
        if(avList_[i].value()=="Horizontal") orientation(Horizontal);
        else orientation(Vertical);
        index<<i;
      }
   }
  avList_.remove(index);
  
}

// #########################################################
// default virtual methods - prevents gratuitous inlining
// #########################################################

void MSMenuBar::pointerLeave(void) {}
void MSMenuBar::selectNone(void) {}

MSMenuItem *MSMenuBar::nextUpItem(void)
{ return nextUpItem(); }

MSMenuItem *MSMenuBar::nextDownItem(void)
{ return nextRightItem(); }

