#ifndef MSCFRInterpolaterHEADER
#define MSCFRInterpolaterHEADER

///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////


#include <MSTypes/MSCurveInterpolater.H>

#if defined(MS_NEED_IMPLEMENTATION_PRAGMA)
#pragma implementation("MSCFRInterpolater.C")
#endif

template<class X,class Y>
class MSCFRInterpolater : public MSCurveInterpolater<X,Y> 
{
public:
  MSCFRInterpolater(MSCurve<X,Y> *curve_,const X& xOrigin_);
  MSCFRInterpolater(MSCurve<X,Y>& curve_,const X& xOrigin_);
  ~MSCFRInterpolater();
		      
  virtual Y interpolate(const X& xValue_) const;
  virtual MSTypeVector<Y> interpolate(const MSTypeVector<X>&) const;

  const X& xOrigin(void) const;

protected:
  X _xOrigin;
};

template<class X,class Y>
inline const X& MSCFRInterpolater<X,Y>::xOrigin(void) const
{ return _xOrigin; }

#endif
