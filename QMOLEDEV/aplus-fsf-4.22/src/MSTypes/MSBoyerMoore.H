#ifndef MSBoyerMooreHEADER
#define MSBoyerMooreHEADER

///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////


#include <MSTypes/MSString.H>

class MSTypesExport MSBoyerMoore
{
private:
  static const unsigned DeltaTableSize;
  MSString  _sourceString;
  MSString  _searchPattern;
  unsigned *_deltaTable;

  unsigned deltaTableSize(void) const { return DeltaTableSize; }   
public:
  MSBoyerMoore(void);   
  MSBoyerMoore(const MSString&);   
  ~MSBoyerMoore(void);

  unsigned delta(char aChar_)         const { return _deltaTable[(unsigned)aChar_]; }
  const MSString& sourceString(void)  const { return _sourceString; }
  const MSString& searchPattern(void) const { return _searchPattern; }

  void searchPattern(const MSString& searchPattern_);
  void sourceString(const MSString& sourceString_);

  unsigned indexOf(unsigned startPos_,const char *searchPattern_=0);
};

#endif










