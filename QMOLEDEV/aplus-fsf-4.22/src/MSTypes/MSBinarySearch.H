#ifndef MSBinarySearchHEADER
#define MSBinarySearchHEADER

///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////


#include <MSTypes/MSBuiltinVector.H>
#include <MSTypes/MSTypeVector.H>
#include <MSTypes/MSIndexVector.H>

#if defined(MS_NEED_IMPLEMENTATION_PRAGMA)
#pragma implementation("MSBinarySearch.C")
#endif

template<class Type>
class MSBinarySearch
{
public:
  MSBinarySearch(const MSTypeVector<Type>& sourceVector_);
  ~MSBinarySearch(void) {}

  unsigned occurrencesOf(const Type& aKey_) const;
  unsigned indexOf(const Type& aKey_) const;
  unsigned firstIndexOf(const Type& aKey_) const;
  unsigned lastIndexOf(const Type& aKey_) const;
  MSIndexVector indicesOf(const Type& aKey_) const;

  const MSTypeVector<Type>& sourceVector(void) const;
  const MSIndexVector& indexVector(void) const;

private:
  const MSTypeVector<Type>& _sourceVector;
  MSIndexVector             _indexVector;
};

template<class Type>
inline const MSTypeVector<Type>& MSBinarySearch<Type>::sourceVector(void) const 
{ return _sourceVector; }

template<class Type>
inline const MSIndexVector& MSBinarySearch<Type>::indexVector(void) const 
{ return _indexVector; }

#endif








