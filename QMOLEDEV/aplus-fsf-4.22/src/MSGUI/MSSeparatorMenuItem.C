///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////

#include <MSGUI/MSSeparatorMenuItem.H>

MSSeparatorMenuItem::MSSeparatorMenuItem(MSMenu *owner_) : 
MSMenuItem(owner_) { init(); }
MSSeparatorMenuItem::~MSSeparatorMenuItem(void) {} 

void MSSeparatorMenuItem::init(void) 
{ 
  _highlightThickness=0;
  _shadowThickness=1;
  _marginHeight=0;
  _marginWidth=0;
  sensitive(MSFalse);  
}

void MSSeparatorMenuItem::computeSize(void) { height((marginHeight()+shadowThickness())<<1); }
void MSSeparatorMenuItem::drawSymbol(void) 
{ 
  int offset=shadowThickness();
  int x=x_origin()+marginWidth(),y=y_origin()+marginHeight();
  int Width=width()-(marginWidth()<<1);
  XFillRectangle(display(),owner()->window(),bottomShadowGC(),
		 x,y,Width,offset);     
  XBFillRectangle(display(),owner()->window(),topShadowGC(),
		  x,y+offset,Width,offset);     
}

