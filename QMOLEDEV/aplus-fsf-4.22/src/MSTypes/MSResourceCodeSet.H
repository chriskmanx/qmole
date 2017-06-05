#ifndef MSResourceCodeSetHEADER
#define MSResourceCodeSetHEADER

///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////


#include <MSTypes/MSString.H>
#include <MSTypes/MSStringVector.H>
#include <MSTypes/MSHoliday.H>
#if HAVE_IOSTREAM
#include <iostream>
// #include <fstream>
using namespace std;
#else
class ostream;
#endif


class MSTypesExport MSResourceCodeSet 
{
public:
  MSResourceCodeSet();
  MSResourceCodeSet(const MSResourceCodeSet &);
  MSResourceCodeSet(const MSResourceCode &);
  ~MSResourceCodeSet();
  
  MSBoolean add(const MSResourceCode &);
  void removeAll(void);
  const MSResourceCode& elementAt(unsigned int ) const;
  MSResourceCodeSet & operator=(const MSResourceCodeSet &) ;

  MSBoolean isEmpty(void) const;
  unsigned int numberOfElements(void) const;
  friend MSTypesExport ostream& operator<<(ostream&,const MSResourceCodeSet&);
  MSString asDebugInfo(void) const;

private:
  MSStringVector  _stringVector;
};

#endif
