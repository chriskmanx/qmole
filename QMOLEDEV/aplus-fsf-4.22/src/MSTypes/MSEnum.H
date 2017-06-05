#ifndef MSEnumHEADER
#define MSEnumHEADER

///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////


#include <MSTypes/MSDefines.H>

enum MSBoolean    {MSFalse=0,MSTrue=1};

enum MSComparison {MSLessThan,MSGreaterThan,MSLessThanOrEqualTo,MSGreaterThanOrEqualTo,
                   MSEqualTo,MSNotEqualTo};

enum MSExplicitInit {MSInit};

enum MSAllocationFlag {MSRaw,MSConstructed};

enum MSEstimateType {MSBiased,MSUnbiased}; // for variance and standard deviation in vectors

typedef unsigned long Atom;
typedef unsigned long MSAtom;
const MSAtom MSNullAtom=0x0;
const Atom NullAtom=0;

#endif
