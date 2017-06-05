#ifndef AplusConvertHEADER
#define AplusConvertHEADER

///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved.
// See .../src/LICENSE for terms of distribution.
//
//
///////////////////////////////////////////////////////////////////////////////

#include <MSTypes/MSFloat.H>
#include <MSTypes/MSString.H>
#include <MSTypes/MSStringVector.H>
#include <MSTypes/MSFloatVector.H>
#include <MSTypes/MSIntVector.H>
#include <MSTypes/MSFloatMatrix.H>
#include <MSTypes/MSIntMatrix.H>
#include <MSTypes/MSUnsignedLongVector.H>
#include <MSTypes/MSUnsignedVector.H>
#include <a/fncdcls.h>
#include <MSIPC/MSA.H>


class AplusConvert
{
private:
  static MSA _convert;
  
public:

  AplusConvert(void) {}
  ~AplusConvert(void) {}
  
  static MSFloat        asMSFloat(A);
  static MSString       asMSString(A);

  // Vector Conversions

  static MSStringVector 	     asMSStringVector(A);
  static MSFloatVector  	     asMSFloatVector(A);
  static MSIntVector    	     asMSIntVector(A);
  static MSUnsignedLongVector        asMSUnsignedLongVector(A);
  static MSIndexVector               asMSIndexVector(A);
  static MSUnsignedVector            asMSUnsignedVector(A);

  // Matrix Conversions

  static MSFloatMatrix  asMSFloatMatrix(A);
  static MSIntMatrix    asMSIntMatrix(A);

  // A Conversions

  static A asA(const MSString &);
  static A asA(const MSFloat &);
  static A asA(const MSStringVector &);
  static A asA(const MSFloatVector &);
  static A asA(const MSUnsignedVector &);
  static A asA(const MSIndexVector &);
};

#endif  //AplusConvertHEADER
