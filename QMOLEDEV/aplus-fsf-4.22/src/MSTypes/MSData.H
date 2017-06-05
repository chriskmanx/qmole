#ifndef MSDataHEADER
#define MSDataHEADER

///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////


#include <MSTypes/MSDefines.H>

#ifdef MS_MULTI_THREAD
#include <MSTypes/MSMutex.H>
#endif

class MSTypesExport MSData
{
protected:
#ifdef MS_MULTI_THREAD    
  MSMutex   _mutex;      // ref count lock
#endif
  unsigned _size;
  unsigned _refCount;

public:
  MSData (unsigned int =0);
  ~MSData();

  INLINELINKAGE unsigned size() const;
  INLINELINKAGE unsigned refCount() const;

  static unsigned int computeSize (unsigned int);
};


//
// MSDataAlignment<Type> structure is a special way of calculating
// the appropriate alignment of MSData.  MSData is implicitly going
// to be followed by an array of data elements; however, depending
// on what type those elements actually are (or what types they
// contain inside), we may need to align the beginning of the array
// correctly - in the same way as it would be aligned automatically
// by the compiler.

// For example, if we have an array of double's following MSData,
// then that array has to start on an 8-byte boundary.  MSData itself
// does not guarantee that, since it does not know what type of
// elements are coming after it.  Therefore, if we want to align the
// array correctly, we cannot simply use sizeof(MSData) to see where
// MSData ends and where the array should start.  That's why we need
// an auxiliary structure - MSDataAlignment<Type>.
//
// MSDataAlignment<Type> structure can be used to find out how MSData 
// should be aligned when it's followed by an array of elements of
// type Type.  To get the *real* size of MSData (including possible
// alignment), we can just use the offsetof() macro to find out where
// _pElements starts:  offsetof(MSDataAlignment<Type>,_pElements).
// This can be done in subclasses of MSData (such as MSTypeData<Type>)
// to calculate the total number of bytes to be allocated considering
// alignment issues.
//  
template <class Type>
struct MSDataAlignment
{
  MSData _msData;
  Type   _pElements[1];
};

#ifndef MS_NO_INLINES
#include <MSTypes/MSDataInlines.C>
#endif  // MS_NO_INLINES

#endif  // MSDataHEADER
