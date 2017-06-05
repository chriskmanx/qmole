#ifndef MSCFVInterpolaterHEADER
#define MSCFVInterpolaterHEADER

///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////


#include <MSTypes/MSCurveInterpolater.H>

#if defined(MS_NEED_IMPLEMENTATION_PRAGMA)
#pragma implementation("MSCFVInterpolater.C")
#endif

template<class X,class Y>
class MSCFVInterpolater : public MSCurveInterpolater<X,Y> 
{
public:
  MSCFVInterpolater(MSCurve<X,Y> *curve_,const X& xOrigin_);
  MSCFVInterpolater(MSCurve<X,Y>& curve_,const X& xOrigin_);
  ~MSCFVInterpolater();

  virtual Y interpolate(const X& xValue_) const;
  virtual MSTypeVector<Y> interpolate(const MSTypeVector<X>&) const;

  const X& xOrigin(void) const;

protected:
  X _xOrigin;
};

template<class X,class Y>
inline const X& MSCFVInterpolater<X,Y>::xOrigin(void) const
{ return _xOrigin; }

#endif
