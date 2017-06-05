///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////

#include <MSTypes/MSHoliday.H>
#if HAVE_IOSTREAM
#include <iostream>
#else
#include <iostream.h>
#endif

//#######################################################
// MSHoliday
//#######################################################

MSHoliday::MSHoliday()
{}

MSHoliday::MSHoliday(const MSDate& date_,const MSResourceCode& resourceCode_,const MSString& description_) :
_resourceCode(resourceCode_),_description(description_),MSDate(date_)
{}

MSHoliday::MSHoliday(const MSHoliday& holiday_) :
_resourceCode(holiday_.resourceCode()),_description(holiday_.description()),MSDate(holiday_)
{}

MSHoliday& MSHoliday::operator=(const MSHoliday& holiday_)
{
  _resourceCode=holiday_.resourceCode();
  _description=holiday_.description();
  MSDate::operator=(holiday_);
  return *this;
}

ostream& operator<<(ostream& aStream_,const MSHoliday& aHoliday_)
{
  aStream_<<"Date: "<<(const MSDate&)aHoliday_;
  aStream_<<"\tResourceCode: "<<aHoliday_.resourceCode();
  aStream_<<"\tDescription: "<<aHoliday_.description();  
  return aStream_;
}

