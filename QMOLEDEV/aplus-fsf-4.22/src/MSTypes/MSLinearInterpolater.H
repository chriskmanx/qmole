#ifndef MSLinearInterpolaterHEADER
#define MSLinearInterpolaterHEADER

///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////


#include <MSTypes/MSCurveInterpolater.H>

#if defined(MS_NEED_IMPLEMENTATION_PRAGMA)
#pragma implementation("MSLinearInterpolater.C")
#endif

template<class X,class Y> 
class MSLinearInterpolater : public MSCurveInterpolater<X,Y> 
{
public:
  MSLinearInterpolater(MSCurve<X,Y> *curve_);
  MSLinearInterpolater(MSCurve<X,Y>& curve_);
  ~MSLinearInterpolater();
		      
  virtual Y interpolate(const X& xValue_) const;
  virtual MSTypeVector<Y> interpolate(const MSTypeVector<X>&) const;
};

#endif
