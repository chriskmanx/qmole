#ifndef MSStandardOpsHEADER
#define MSStandardOpsHEADER

///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////


#include <string.h>

#ifndef MSDefinesHEADER
#include <MSTypes/MSDefines.H>
#endif

/*************************************************************************************/
// Global template functions from IBM collection classes.
// Redefined here with different names to remove conflict and dependancy with ccl.
/*************************************************************************************/
// compareElements - forwarding compare to msCompare using msElementForOps
// compare       - comparing two elements using ccl style
// elementForOps - default implementation, specialized by pointer classes.
/*************************************************************************************/

template <class Type>
inline long compareElements(const Type & aType_, const Type & bType_)
{
#if defined(MS_CONST_ELEMENT_FOR_OPS_BUG)
  return compare(elementForOps((Type&)aType_),elementForOps((Type&)bType_));
#else
  return compare(elementForOps(aType_),elementForOps(bType_));
#endif
}

#if !defined(MS_GLOBAL_COMPARE)
#define MS_GLOBAL_COMPARE 1

// ordering relation
template < class Element >
inline long compare (Element const& e1, Element const& e2)
{
  if      (e1 < e2) return -1;
  else if (e2 < e1) return 1;
  else              return 0;
}

inline long compare (int e1, int e2)                 { return e1 - e2;         }
inline long compare (char const* e1, char const* e2) { return strcmp (e1, e2); }

#endif // MS_GLOBAL_COMPARE

#if !defined(MS_ELEMENT_FOR_OPS)
#define MS_ELEMENT_FOR_OPS 1

#if !defined(MS_NO_NONCONST_ELEMENT_FOR_OPS)
template < class Element >
inline Element& elementForOps (Element& e)
{ return e; }
#endif

// this function is redundant for ANSI C++ compliant compilers
#if defined(MS_NEED_CONST_ELEMENT_FOR_OPS)
template < class Element >
inline Element const& elementForOps (Element const& e)
{ return e; }
#endif


#if defined(MS_SPECIALIZE_ELEMENT_FOR_OPS)
#define DECLARE_ELEMENT_FOR_OPS(Pointer,Type) \
  inline const Type & elementForOps (Pointer<Type> const & ptr_) { return *(Type*)ptr_; } \
  inline Type & elementForOps (Pointer<Type> & ptr_) { return *(Type*)ptr_; } 
#else
#define DECLARE_ELEMENT_FOR_OPS(Pointer,Type)
#endif

#if defined (MS_SPECIALIZE_ELEMENT_FOR_OPS_TYPE)
#define DECLARE_ELEMENT_FOR_OPS_TYPE(Type) \
  inline Type const & elementForOps (Type const& ptr_) { return ptr_; } \
  inline Type & elementForOps (Type& ptr_) { return ptr_; } 

DECLARE_ELEMENT_FOR_OPS_TYPE(int)
DECLARE_ELEMENT_FOR_OPS_TYPE(float)
DECLARE_ELEMENT_FOR_OPS_TYPE(double)
DECLARE_ELEMENT_FOR_OPS_TYPE(long)
DECLARE_ELEMENT_FOR_OPS_TYPE(short)
DECLARE_ELEMENT_FOR_OPS_TYPE(char)
DECLARE_ELEMENT_FOR_OPS_TYPE(unsigned int)
DECLARE_ELEMENT_FOR_OPS_TYPE(unsigned short)
DECLARE_ELEMENT_FOR_OPS_TYPE(unsigned long)
DECLARE_ELEMENT_FOR_OPS_TYPE(unsigned char)


#else
#define DECLARE_ELEMENT_FOR_OPS_TYPE(Type)
#endif


#endif // MS_ELEMENT_FOR_OPS

#ifndef forCursor
#define forCursor(c) for ((c).setToFirst (); (c).isValid (); (c).setToNext ())
#endif

#endif  //MSStandardOpsHEADER

