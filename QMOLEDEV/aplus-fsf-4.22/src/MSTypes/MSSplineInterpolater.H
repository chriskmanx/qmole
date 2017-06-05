#ifndef MSSplineInterpolaterHEADER
#define MSSplineInterpolaterHEADER

///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////

#include <MSTypes/MSCurveInterpolater.H>

#if defined(MS_NEED_IMPLEMENTATION_PRAGMA)
#pragma implementation("MSSplineInterpolater.C")
#endif

// This class will do efficient cubic spline interpolation given a single x-value
// or a vector of x-values. The interpolation on a vector of x-values utilizes a
// sequential search and assumes that the values are in sort order.
template<class X,class Y> 
class MSSplineInterpolater : public MSCurveInterpolater<X,Y> 
{
public:
  MSSplineInterpolater(MSCurve<X,Y> *curve_);
  MSSplineInterpolater(MSCurve<X,Y>& curve_);
  virtual ~MSSplineInterpolater(); 

  virtual Y interpolate(const X& xValue_) const;                     
  virtual MSTypeVector<Y> interpolate(const MSTypeVector<X>&) const;
private:

  void initY2(void);   
  double *_y2;
};

#endif
