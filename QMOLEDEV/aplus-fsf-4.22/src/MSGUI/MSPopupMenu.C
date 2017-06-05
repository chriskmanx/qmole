///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////

#include <MSGUI/MSPopupMenu.H>

MSPopupMenu::MSPopupMenu(MSDisplayServer *server_) : MSMenu(server_) 
{ XDefineCursor(display(),window(),grabCursor()->cursor()); }

MSPopupMenu::~MSPopupMenu(void) {}

void MSPopupMenu::show(void)
{
  if (mapped()==MSFalse)
   {
     map();
     raise();
     grab();
   }
}

void MSPopupMenu::showAtPointer(void)
{
  int w,h;
  if (firstMap()==MSFalse) calculateNaturalSize(w,h);
  else
   {
     w=width();
     h=height();
   }
  int x,y;
  pointerXY(x,y);
  if (x+w>server()->width()) x=server()->width()-w;
  if (y+h>server()->height()) y=server()->height()-h;
  moveTo(x,y);
  map();
  raise();
  grab();
}

