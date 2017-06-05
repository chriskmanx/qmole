#ifndef MSFloatSPickHEADER
#define MSFloatSPickHEADER

///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////


#include <MSTypes/MSDefines.H>

#if defined(MS_TEMPLATE_BEFORE_SPECIALIZATION)
#include <MSTypes/MSBuiltinSPick.H>
#endif

template <class Type> class MSBuiltinSPick;
template <class Type> class MSBuiltinVector;

template <> class MSTypesExport MSBuiltinSPick<double>
{
protected:
  unsigned int _index; 
  MSBuiltinVector<double> *_pVector;

public:
  INLINELINKAGE MSBuiltinSPick (MSBuiltinVector<double> &, unsigned int);
  INLINELINKAGE MSBuiltinSPick (const MSBuiltinSPick<double> &);
  
  MSBuiltinSPick<double> & operator= (const MSBuiltinSPick<double> &);
  MSBuiltinSPick<double> & operator= (const double &);

  INLINELINKAGE MSBuiltinSPick<double> & operator++();        // prefix increment
  INLINELINKAGE MSBuiltinSPick<double> & operator--();        // prefix decrement
  INLINELINKAGE double operator++ (int);    // postfix increment
  INLINELINKAGE double operator-- (int);    // postfix decrement
  
  INLINELINKAGE MSBuiltinSPick<double> & operator+=  (const double &);
  INLINELINKAGE MSBuiltinSPick<double> & operator-=  (const double &);
  INLINELINKAGE MSBuiltinSPick<double> & operator*=  (const double &);
  INLINELINKAGE MSBuiltinSPick<double> & operator/=  (const double &);

  INLINELINKAGE operator double() const;
};

#ifndef MS_NO_INLINES
#include <MSTypes/MSFloatSPickInlines.C>
#endif  // MS_NO_INLINES

#endif  // MSFloatSPickHEADER
