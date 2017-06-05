#ifndef MSRangeHEADER
#define MSRangeHEADER

///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////

#include <MSTypes/MSEnum.H>
#include <sys/types.h>      //for size_t

#if HAVE_IOSTREAM
#include <iostream>
// #include <fstream>
using namespace std;
#else
class ostream;
#endif


class MSTypesExport MSRange 
{
protected:
  unsigned _min;
  unsigned _max;
  unsigned _length;
public:
 
  MSRange(void) : _min(0),_max(0),_length(0) {}
  MSRange(const MSRange& aRange_) : _min(aRange_._min),_max(aRange_._max),_length(aRange_._length) {}
  MSRange(unsigned min_,unsigned max_) { range(min_,max_); }
  
  ~MSRange(void) {}

  unsigned min(void)           const {return _min;}
  unsigned max(void)           const {return _max;}
  unsigned length(void)        const {return _length;}
  unsigned firstIndexOf(void)  const {return _min;}
  unsigned lastIndexOf(void)   const {return _max;}
  MSBoolean isValid(void)      const {return MSBoolean(_length>0);}

  void range(unsigned min_,unsigned max_)
   {
     if (min_<=max_) _min=min_,_max=max_,_length=(max_-min_)+1;
     else            _min=0,_max=0,_length=0;
   }

  void min(unsigned min_) {range(min_,_max);}
  void max(unsigned max_) {range(_min,max_);}
  
  MSRange& operator=(const MSRange& aRange_)
   { 
     if (&aRange_!=this) _min=aRange_.min(),_max=aRange_.max(),_length=aRange_._length; 
     return *this; 
   }
  MSBoolean operator==(const MSRange& aRange_)
   { return MSBoolean(_min==aRange_.min()&&_max==aRange_.max()); }
  MSBoolean operator!=(const MSRange& aRange_)
   { return MSBoolean(_min!=aRange_.min()||_max!=aRange_.max()); }
  
  friend MSTypesExport ostream& operator<<(ostream&,const MSRange&);

};

#endif






