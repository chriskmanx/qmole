///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////

#include <stdlib.h>
#include <stdio.h>
#include <math.h>

#include <MSTypes/MSDate.H>
#include <MSTypes/MSNormalizedYears.H>

static const double DaysInYear=365.25;

MSString MSNormalizedYears::asString(void) const
{ return MSString(_years); }

MSString MSNormalizedYears::asDebugInfo(void) const
{
  MSString result("MSNormalizedYears(@");
  result+=MSString((unsigned long)this).d2x().lowerCase();
  result+=",_years=";
  result+=MSString(_years);
  result+=",_basis=";
  result+=MSString((int)_basis);
  result+=",_type=";
  result+=type().symbolName();
  result+=")";
  return MSString(result);
}
  
MSString MSNormalizedYears::className(void) const
{ return MSString("MSNormalizedYears"); }
const MSSymbol& MSNormalizedYears::type(void) const
{ return symbol(); }

const MSSymbol& MSNormalizedYears::symbol(void)   
{
  static MSSymbol sym ("MSNormalizedYears");
  return sym;
}

void MSNormalizedYears::makeFromDates(const MSDate& d1_, const MSDate& d2_)
{ makeFromDays(d1_-d2_); }

void MSNormalizedYears::makeFromDays(int i_)
{
  // absolute value
  if (i_<0) i_=-i_;
  _years=double(i_)/daysInYear();
}

double MSNormalizedYears::daysInYear() const
{
  switch (_basis) 
   {
   case AIBD30_360:   return 360.0;
   case Actual365:
   case Euro365:      return 365.0;
   case ActualActual: return DaysInYear;
   }
  return 0;
}
