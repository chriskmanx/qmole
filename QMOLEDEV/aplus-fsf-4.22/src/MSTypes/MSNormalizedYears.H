#ifndef MSNormalizedYearsHEADER
#define MSNormalizedYearsHEADER

///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////


#include <MSTypes/MSSymbol.H>

class MSDate;

class MSTypesExport MSNormalizedYears 
{
public:
  enum Basis { ActualActual,Actual365,AIBD30_360,Euro365 };
  
  // make directly from days or from (MSDate - MSDate)
  // uses absolute value of d
  // default was unitialized
  MSNormalizedYears() : _basis(ActualActual),_years(0.0) {}
  MSNormalizedYears(int i_, Basis b_=ActualActual) : _basis(b_) { makeFromDays(i_); }
  
  // make from analytics normalized years
  MSNormalizedYears(double d_, Basis b_=ActualActual) : _basis(b_),_years(d_) {}
  
  // absolute difference between dates
  MSNormalizedYears(const MSDate& d1_, const MSDate& d2_,Basis b_=ActualActual) : _basis(b_)
   { makeFromDates(d1_,d2_); }
  
  MSNormalizedYears(const MSNormalizedYears& n_) : _basis(n_._basis),_years(n_._years) {}

  virtual MSString asString(void) const;
  virtual MSString asDebugInfo(void) const;
  virtual MSString className(void) const;
  virtual const MSSymbol& type(void) const;
  static const MSSymbol& symbol(void);
  
  void set(int i_,Basis b_=ActualActual)    { _basis=b_; makeFromDays(i_); }
  void set(double d_,Basis b_=ActualActual) { _basis=b_; _years=d_; }
  void set(const MSDate& d1_,const MSDate& d2_, Basis b_=ActualActual)
   { _basis=b_; makeFromDates(d1_,d2_); }
  
  // We only add 0.49 so that 2 years is 365*2 days, not 365+366 days
  int days()            const { return int((_years*daysInYear())+0.49); }
  Basis basis()         const { return _basis; }
  double daysInYear() const;
  
  MSNormalizedYears operator+=(const MSNormalizedYears);
  MSNormalizedYears operator+ (const MSNormalizedYears) const;
  MSNormalizedYears operator-=(const MSNormalizedYears);
  MSNormalizedYears operator- (const MSNormalizedYears) const;
  MSNormalizedYears operator-() const { return MSNormalizedYears(-_years); }
  operator double() const { return _years; }
  
protected:
  virtual void makeFromDays(int);
  virtual void makeFromDates(const MSDate&, const MSDate&);
  
private:
  double           _years;
  Basis            _basis;
};


inline MSNormalizedYears MSNormalizedYears::operator+=(const MSNormalizedYears x)
{ _years+=x._years; return *this; }

inline MSNormalizedYears MSNormalizedYears::operator+(const MSNormalizedYears y) const
{ return MSNormalizedYears(_years+y._years); }

inline MSNormalizedYears MSNormalizedYears::operator-=(const MSNormalizedYears x)
{ _years-=x._years; return *this; }

inline MSNormalizedYears MSNormalizedYears::operator-(const MSNormalizedYears y) const
{ return MSNormalizedYears(_years-y._years); }

#endif
