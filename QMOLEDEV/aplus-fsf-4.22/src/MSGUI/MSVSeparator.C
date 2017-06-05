///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////

#include <MSGUI/MSVSeparator.H>

const int MSVSeparatorDefaultMarginWidth=2;
const int MSVSeparatorDefaultMarginHeight=0;

MSVSeparator::MSVSeparator(MSWidget *owner_)
:MSSeparator(owner_)
{
  _marginWidth=MSVSeparatorDefaultMarginWidth;
  _marginHeight=MSVSeparatorDefaultMarginHeight;
  resizeConstraints(At::MinimizeWidth|At::MaintainWidth);   
}

void MSVSeparator::computeSize(void)
{
   width((marginWidth()+shadowThickness()+highlightThickness()+thickness())*2);
}

void MSVSeparator::redraw(void)
{
  if (frozen()==MSFalse&&mapped()==MSTrue)
  {
     int offset=highlightThickness()+shadowThickness();
     int x=marginWidth()+offset;
     int y=marginHeight()+offset;
     int Height=height()-((marginHeight()+offset)<<1);
     XFillRectangle(display(),window(),bottomShadowGC(),
		    x,y,thickness(),Height);     
     XBFillRectangle(display(),window(),topShadowGC(),
		     x+thickness(),y,thickness(),Height);
  }
}
