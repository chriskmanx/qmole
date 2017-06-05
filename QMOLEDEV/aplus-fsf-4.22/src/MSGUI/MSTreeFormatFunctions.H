#ifndef MSTreeFormatFunctionsHEADER
#define MSTreeFormatFunctionsHEADER

///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1999-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////


/*************************************************************************************/
// msTreeFormatType - default calls member function format, specialize as necessary
// msTreeSetType    - default calls member function set   , specialize as necessary
/*************************************************************************************/
#include <MSTypes/MSString.H>

template <class T>
inline void msTreeFormatType(T& t_, MSString& buffer_)
{ t_.format(buffer_); }

template <class T>
inline MSError::ErrorStatus msTreeSetType(T& t_, const char *buffer_)
{ return t_.set(buffer_); }


#endif
