///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////

#include <MSGUI/MSMenuBar.H>
#include <MSGUI/MSCascadeMenuItem.H>
#include <MSGUI/MSCascadeMenu.H>

MSCascadeMenu::MSCascadeMenu(MSCascadeMenuItem *item_) : MSMenu(item_) 
{
  item()->cascadeMenu(this);
  background(item()->background());
  foreground(item()->foreground());
  font(item()->font());
}

MSCascadeMenu::~MSCascadeMenu(void)
{
   item()->menuDestroy(this);
}

void MSCascadeMenu::left(void) 
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
     item()->disarm();
     item()->menu()->grabAndSelect();
   }
}

void MSCascadeMenu::right(void) 
{
  MSMenuItem *ni,*mi=menuItem(selectedItem());
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
  else if (menuBar()!=0) menuBar()->right();
}

void MSCascadeMenu::done(void)
{
  ungrab();
  unmap();
  if (item()!=0) item()->menu()->done();
  reset();
}

void MSCascadeMenu::activate(void)
{
  if (activateCallback(MSWidgetCallback::activate)==MSFalse)
  {
     if (item()!=0) item()->activate();
  }
  done(); 
}
