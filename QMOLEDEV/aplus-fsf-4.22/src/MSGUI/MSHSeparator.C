///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////

#include <MSGUI/MSHSeparator.H>

const int MSHSeparatorDefaultMarginWidth=0;
const int MSHSeparatorDefaultMarginHeight=2;

MSHSeparator::MSHSeparator(MSWidget *owner_)
:MSSeparator(owner_)
{
  _marginWidth=MSHSeparatorDefaultMarginWidth;
  _marginHeight=MSHSeparatorDefaultMarginHeight;
  resizeConstraints(At::MinimizeHeight|At::MaintainHeight);
}

void MSHSeparator::computeSize(void)
{
   height((marginHeight()+shadowThickness()+highlightThickness()+thickness())*2);
}

void MSHSeparator::redraw(void)
{
  if (frozen()==MSFalse&&mapped()==MSTrue)
  {
     int offset=highlightThickness()+shadowThickness();
     int x=marginWidth()+offset;
     int y=marginHeight()+offset;
     int Width=width()-((marginWidth()+offset)<<1);
     XFillRectangle(display(),window(),bottomShadowGC(),
		    x,y,Width,thickness());
     XBFillRectangle(display(),window(),topShadowGC(),
		     x,y+thickness(),Width,thickness());
  }
}

