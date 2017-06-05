///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////

#include <MSGUI/MSNumericArrayView.H>

MSNumericArrayView::MSNumericArrayView(MSWidget *owner_,const char *title_) : 
MSArrayView(owner_,title_),_format(MSFloat::Decimal2)
{}

MSNumericArrayView::MSNumericArrayView(MSWidget *owner_,const MSStringVector& title_) : 
MSArrayView(owner_,title_),_format(MSFloat::Decimal2)
{}

MSNumericArrayView::~MSNumericArrayView(void)
{}

void MSNumericArrayView::format(const MSFormat& format_)
{ 
  _format=format_; 
  updateFormat(); 
}

const char *MSNumericArrayView::formatValue(MSString &buffer_,double data_)
{
  MSFloat aFloat(data_);
  return aFloat.format(buffer_,format());
}

const char *MSNumericArrayView::formatValue(MSString &buffer_,int data_)
{ 
  buffer_=MSString(data_);
  return buffer_.string();
}

MSClipMode MSNumericArrayView::columnClipMode(unsigned) const
{ return MSClipStars; }

void MSNumericArrayView::updateFormat(void) {redraw();}
