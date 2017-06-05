#ifndef MSConstantInterpolaterHEADER
#define MSConstantInterpolaterHEADER

///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////


#include <MSTypes/MSCurveInterpolater.H>

#if defined(MS_NEED_IMPLEMENTATION_PRAGMA)
#pragma implementation("MSConstantInterpolater.C")
#endif

template<class X,class Y> 
class MSConstantInterpolater : public MSCurveInterpolater<X,Y> 
{
public:
  MSConstantInterpolater(MSCurve<X,Y> *curve_);
  MSConstantInterpolater(MSCurve<X,Y>& curve_);
  ~MSConstantInterpolater();
		      
  virtual Y interpolate(const X& xValue_) const;
  virtual MSTypeVector<Y> interpolate(const MSTypeVector<X>&) const;
};

#endif
