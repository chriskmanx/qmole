///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////

#include <MSGUI/MSWidgetVector.H>

MSWidgetVector::MSWidgetVector(void) : MSUnsignedLongVector()
{}

MSWidgetVector::MSWidgetVector(const MSWidgetVector& aWidgetVector_) :
MSUnsignedLongVector(aWidgetVector_ )
{}

MSWidgetVector::MSWidgetVector(MSWidget *pWidget_) :
MSUnsignedLongVector(1,(unsigned long)pWidget_)
{}

MSWidgetVector::~MSWidgetVector(void)
{}

MSWidgetVector& MSWidgetVector::operator<<(MSWidget *pWidget_)
{ return append(pWidget_); }

MSWidgetVector& MSWidgetVector::operator<<(const MSWidgetVector& bWidgetVector_)
{ return append(bWidgetVector_); }

/***
MSWidgetVector& operator<<(MSWidgetVector& aWidgetVector_,MSWidget *pWidget_)
{ return aWidgetVector_.append(pWidget_); }

MSWidgetVector& operator<<(MSWidgetVector& aWidgetVector_,const MSWidgetVector& bWidgetVector_)
{ return aWidgetVector_.append(bWidgetVector_); }
***/
MSWidgetVector &MSWidgetVector::operator=(const MSWidgetVector& aWidgetVector_)
{
  MSUnsignedLongVector::operator=(aWidgetVector_);
  return *this;
}

MSWidgetVector& MSWidgetVector::append(MSWidget *pWidget_)
{
  MSUnsignedLongVector::append((unsigned long)pWidget_);
  return *this;
}

MSWidgetVector& MSWidgetVector::append(const MSWidgetVector& aWidgetVector_)
{
  MSUnsignedLongVector::append((MSUnsignedLongVector &)aWidgetVector_);
  return *this;
}


