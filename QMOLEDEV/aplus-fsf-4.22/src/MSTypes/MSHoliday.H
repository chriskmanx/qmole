#ifndef MSHolidayHEADER
#define MSHolidayHEADER

///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////


#include <MSTypes/MSString.H>
#include <MSTypes/MSDate.H>

#if HAVE_IOSTREAM
#include <iostream>
// #include <fstream>
using namespace std;
#else
class ostream;
#endif


typedef MSString MSResourceCode;

class MSTypesExport MSHoliday : public MSDate 
{
public:
  MSHoliday(void);
  MSHoliday(const MSHoliday&);  
  MSHoliday(const MSDate&,const MSResourceCode&,const MSString& description_="");

  const MSResourceCode& resourceCode(void) const;
  const MSString& description(void) const;  

  MSHoliday& operator=(const MSHoliday&);
  
  friend MSTypesExport ostream& operator<<(ostream&,const MSHoliday&);

private:
   MSResourceCode _resourceCode;
   MSString       _description;
};

// MSHoliday inlines
inline const MSResourceCode& MSHoliday::resourceCode(void) const
{ return _resourceCode; }

inline const MSString& MSHoliday::description(void) const
{ return _description; }

inline MSDate const& key(MSHoliday const& holiday_)
{ return (MSDate const&)holiday_; }

#endif
