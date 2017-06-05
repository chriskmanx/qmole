#ifndef MSCurveTraceSetHEADER
#define MSCurveTraceSetHEADER

///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////


#include <MSTypes/MSCurve.H>
#include <MSGUI/MSTraceSet.H>

#ifndef MSDefinesHEADER
#include <MSTypes/MSDefines.H>
#endif

#if defined(MS_NEED_IMPLEMENTATION_PRAGMA)
#pragma implementation("MSCurveTraceSet.C")
#endif

template<class XX,class YY>
class MSCurveTraceSet : public MSTraceSet
{
public:
  MSCurveTraceSet(MSGraph *,const char *legends_,const MSSymbol& tag_=MSSymbol::nullSymbol());
  MSCurveTraceSet(MSGraph *,const MSStringVector&,const MSSymbol& tag_=MSSymbol::nullSymbol());
  MSCurveTraceSet(MSGraph *,MSCurve<XX,YY>&,const char *,const MSSymbol& tag_=MSSymbol::nullSymbol());
  MSCurveTraceSet(MSGraph *,MSCurve<XX,YY>&,const MSStringVector&,const MSSymbol& tag_=MSSymbol::nullSymbol());
  ~MSCurveTraceSet(void);

  void model(MSCurve<XX,YY>&);

  MSCurve<XX,YY>& viewModel(void)             { return curve(); }
  const MSCurve<XX,YY>& viewModel(void) const { return curve(); }

protected:
  const MSCurve<XX,YY>& curve(void) const;
  MSCurve<XX,YY>& curve(void);
  virtual int dataCount(void) const;
  virtual double x(int row_) const;
  virtual double y(int row_,int) const;
  virtual int numColumns(void) const;
  void curve(const MSCurve<XX,YY>& aCurve_);
  virtual void validate(int r_,int c_,double x_,double y_);

//  MSCurveTraceSet<XX,YY>& operator=(const MSCurve<XX,YY>& aCurve_) {curve(aCurve_); return *this;}
//  operator MSCurve<XX,YY>&() {return *((MSCurve<XX,YY> *)_model);} 
};

#endif
