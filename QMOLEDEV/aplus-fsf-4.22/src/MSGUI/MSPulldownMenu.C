///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////

#include <MSGUI/MSMenuBarItem.H>
#include <MSGUI/MSPulldownMenu.H>

MSPulldownMenu::MSPulldownMenu(MSMenuBarItem *item_) : MSMenu(item_) 
{
  item()->pulldownMenu(this);
  background(item()->background());
  foreground(item()->foreground());
  font(item()->font());
}

MSPulldownMenu::~MSPulldownMenu(void)
{
  item()->menuDestroy(this);
}

void MSPulldownMenu::left(void)
{
  MSMenuItem *ni,*mi=menuItem(selectedItem());
  if ((ni=nextLeftItem())!=0&&(mi==0||(ni!=mi&&ni->item()<mi->item())))
  {
     undrawSelectedItem();
     selectedItem(ni->item());
     drawSelectedItem();
  }     
  else if (item()!=0) 
   {
     item()->menu()->grabAndSelect();
     item()->menu()->left();
   }
}

void MSPulldownMenu::right(void)
{
  MSMenuItem *ni, *mi=menuItem(selectedItem());
  if (mi!=0&&mi->cascade()==MSTrue)
   {
     mi->arm();
     mi->grab();
   }
  else if ((ni=nextRightItem())!=0&&(mi==0||(ni!=mi&&ni->item()>mi->item())))
  {
     undrawSelectedItem();
     selectedItem(ni->item());
     drawSelectedItem();
     if (ni->cascade()==MSTrue)
     {
	ni->arm();
	ni->grab();
     }     
  }
  else if (item()!=0) 
   {
     item()->menu()->grabAndSelect();
     item()->menu()->right();
   }
}

void MSPulldownMenu::done(void)
{
  ungrab();
  unmap();
  if (item()!=0) item()->menu()->done();
  reset();
}

void MSPulldownMenu::activate(void)
{
  if (activateCallback(MSWidgetCallback::activate)==MSFalse)
  {
     if (item()!=0) item()->activate();
  }
  done(); 
}
