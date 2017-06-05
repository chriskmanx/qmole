#ifndef MSMathDeclareHEADER
#define MSMathDeclareHEADER

///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////


#ifndef MSDefinesHEADER
#include <MSTypes/MSDefines.H>
#endif

#if !defined(MS_MATH_EXCEPTION_CONFLICT)
#include <math.h>

#if defined(MS_NEED_INLINE_SQRT_INT)
inline double sqrt(int l)
{ return sqrt((double)l); }
#endif

#else
/*********************************************************************/
// Obsolete workaround for exception conflict between <math.h> and
// RW stdlib library.  It doesn't seem to be needed anymore.
/*********************************************************************/

extern "C"  double sin(double);
extern "C"  double sinh(double);
extern "C"  double cos(double);
extern "C"  double cosh(double);
extern "C"  double tan(double); 
extern "C"  double tanh(double);
extern "C"  double asin(double);
extern "C"  double acos(double);
extern "C"  double atan(double);
extern "C"  double exp(double);
extern "C"  double log(double);
extern "C"  double sqrt(double);
extern "C"  double ceil(double);
extern "C"  double floor(double);
extern "C"  double fabs(double);
extern "C"  double rint(double);
extern "C"  double pow(double,double);
extern "C"  double fmod(double,double);

extern "C"  int finite(double);

#endif

#endif

