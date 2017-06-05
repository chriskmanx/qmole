#ifndef MSFormatFunctions_H
#define MSFormatFunctions_H

///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////

// Do not use MSFormatFunctionsHEADER since it's used in MSOV/MSFormatFunction.H

/*************************************************************************************/
// msFormatType - default calls member function format, specialized for all built in types
/*************************************************************************************/
#include <MSTypes/MSString.H>
#include <MSTypes/MSFloat.H>

class MSInt;
class MSMoney;
class MSRate;
class MSFormat;

template < class Type>
inline void msFormatType(const Type& aType, MSString& formatString_, const MSFormat& format_)
{ aType.format(formatString_, format_); }

inline void msFormatType(const double& aType, MSString& formatString_, const MSFormat& format_)
{ MSFloat(aType).format(formatString_, format_); }

inline void msFormatType(const float& aType, MSString& formatString_, const MSFormat&)
{ formatString_ = MSString(aType); }

inline void msFormatType(const int& aType, MSString& formatString_, const MSFormat&)
{ formatString_ = MSString(aType); }

inline void msFormatType(const long& aType, MSString& formatString_, const MSFormat&)
{ formatString_ = MSString(aType); }

inline void msFormatType(const short& aType, MSString& formatString_, const MSFormat&)
{ formatString_ = MSString(aType); }

inline void msFormatType(const char& aType, MSString& formatString_, const MSFormat&)
{ formatString_ = MSString(aType); }

inline void msFormatType(const unsigned int& aType, MSString& formatString_, const MSFormat&)
{ formatString_ = MSString(aType); }

inline void msFormatType(const unsigned long& aType, MSString& formatString_, const MSFormat&)
{ formatString_ = MSString(aType); }

inline void msFormatType(const unsigned char& aType, MSString& formatString_, const MSFormat&)
{ formatString_ = MSString(aType); }

inline void msFormatType(const unsigned short& aType, MSString& formatString_, const MSFormat&)
{ formatString_ = MSString(aType); }

inline void msFormatType(const MSString& aType, MSString& formatString_, const MSFormat&)
{ formatString_ = aType; }

inline void msFormatType(const MSSymbol& aType, MSString& formatString_, const MSFormat&)
{ formatString_ = aType.symbolName(); }

/*************************************************************************************/
// msIsNumericType - default returns MSFalse, specialized for built in and MStk numeric types
/*************************************************************************************/
template <class Type>
inline MSBoolean msIsNumericType(const Type&) 
{ return MSFalse;}

inline MSBoolean msIsNumericType(const MSFloat&) 
{ return MSTrue; }

inline MSBoolean msIsNumericType(const MSInt&) 
{ return MSTrue; }

inline MSBoolean msIsNumericType(const MSMoney&) 
{ return MSTrue; }

inline MSBoolean msIsNumericType(const MSRate&) 
{ return MSTrue; }

inline MSBoolean msIsNumericType(const short&) 
{ return MSTrue; }

inline MSBoolean msIsNumericType(const int&) 
{ return MSTrue; }

inline MSBoolean msIsNumericType(const float&) 
{ return MSTrue; }

inline MSBoolean msIsNumericType(const double&) 
{ return MSTrue; }

inline MSBoolean msIsNumericType(const long&) 
{ return MSTrue; }

inline MSBoolean msIsNumericType(const unsigned int&) 
{ return MSTrue; }

inline MSBoolean msIsNumericType(const unsigned long&) 
{ return MSTrue; }

inline MSBoolean msIsNumericType(const unsigned short&) 
{ return MSTrue; }


/*************************************************************************************/
// msToDoubleConversion - default doesn't perform any conversion, specialized 
/*************************************************************************************/
template <class Type>
inline double msToDoubleConversion (const Type&) 
{ return 0; }

inline double msToDoubleConversion (const MSFloat& aType_) 
{ return (double)aType_; }
   
inline double msToDoubleConversion (const MSInt& aType_) 
{ return (double)aType_; }

inline double msToDoubleConversion (const MSMoney& aType_) 
{ return (double)aType_; }

inline double msToDoubleConversion (const MSRate& aType_) 
{ return (double)aType_; }

inline double msToDoubleConversion (const short& aType_) 
{ return (double)aType_; }

inline double msToDoubleConversion (const int& aType_) 
{ return (double)aType_; }

inline double msToDoubleConversion (const double& aType_) 
{ return aType_; }

inline double msToDoubleConversion (const long& aType_) 
{ return (double)aType_; }

inline double msToDoubleConversion (const unsigned short& aType_) 
{ return (double)aType_; }

inline double msToDoubleConversion (const unsigned int& aType_) 
{ return (double)aType_; }

inline double msToDoubleConversion (const unsigned long& aType_) 
{ return (double)aType_; }

#endif
