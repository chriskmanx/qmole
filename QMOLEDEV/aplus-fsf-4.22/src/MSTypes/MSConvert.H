#ifndef MSConvertHEADER
#define MSConvertHEADER

///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////

#include <MSTypes/MSDefines.H>
#include <MSTypes/MSError.H>

template <class Type> class MSTypeVector;
typedef MSTypeVector<int> MSIntVector;
typedef MSTypeVector<double> MSFloatVector;
class MSIndexVector;
class MSBinaryVector;

MSError::ErrorStatus msConvert(const MSIntVector&, MSFloatVector&);
MSError::ErrorStatus msConvert(const MSBinaryVector&, MSIndexVector&);

#ifndef  MS_NO_INLINES
#include <MSTypes/MSConvertInlines.C>
#endif  // MS_NO_INLINES

#endif  //MSConvertHEADER
