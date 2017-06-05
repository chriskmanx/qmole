///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////

#ifndef MSStringTestHEADER
#include <MSTypes/MSStringTest.H>
#endif

MSStringTest::MSStringTest(CFunction *cfunc):type(c)
{ data.cFn=cfunc; }

MSStringTest::~MSStringTest() {}

MSBoolean MSStringTest::test(int aInt_) const
{
  if (this->type==MSStringTest::c) return MSBoolean(data.cFn(aInt_)!=0);
#if defined(MS_C_LINKAGE_INCOMPATIBILITY)
  else if(this->type==MSStringTest::cpp) return MSBoolean(data.cppFn(aInt_)!=0);
#endif
  else return MSFalse;
}

