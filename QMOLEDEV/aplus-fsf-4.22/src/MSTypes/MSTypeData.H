#ifndef MSTypeDataHEADER
#define MSTypeDataHEADER

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

#ifdef MS_MULTI_THREAD
#include <MSTypes/MSMutex.H>
#endif


#if defined(MS_NEED_IMPLEMENTATION_PRAGMA)
#pragma implementation("MSTypeData.C")
#endif

#if HAVE_NEW
#include <new>
#else
#include <new.h>
#endif
#include <MSTypes/MSEnum.H>
#include <MSTypes/MSData.H>

#ifdef MS_OFFSETOF_IN_STDDEF
#include <stddef.h>
#endif

class MSString;

//
// Note that MS_COMPLEX_TEMPLATE_DEFAULT(x) will expand to: ' =x' or nothing,
// depending on the compiler's capabilities and/or flag settings (see MSDefines.H).
//
template <class Type, class Allocator MS_COMPLEX_TEMPLATE_DEFAULT(MSAllocator<Type>) >
class MSTypeData : public MSData
{
public:
  MSTypeData();
  MSTypeData (unsigned int);
  ~MSTypeData();
  
  MSString asDebugInfo() const;

  void *operator new (size_t, unsigned =0);
  void  operator delete (void *);
  
  INLINELINKAGE MSTypeData<Type,Allocator> *incrementCount();
  INLINELINKAGE void decrementCount (MSAllocationFlag =MSConstructed, unsigned int =0);
  INLINELINKAGE const Type *elements() const;
  INLINELINKAGE const Type *data() const;   // same as elements(); used for backward compatibility only

  INLINELINKAGE Type *elements();
  INLINELINKAGE Type *data();	// same as elements(); used for backward compatibility only
  
  INLINELINKAGE const Type& elementAt(unsigned index_) const;
  INLINELINKAGE Type& elementAt(unsigned index_);

  void set (unsigned int, const Type &, MSAllocationFlag =MSConstructed);

  static void constructElements(Type *, unsigned int, const Type&);
  static void destroyElements(Type *, unsigned int);

  static void fill (Type *, unsigned int, const Type &, MSAllocationFlag =MSConstructed);
  static void copy (const Type *, Type *, unsigned int, MSAllocationFlag =MSConstructed);
  static void copyBackward (const Type *, Type *, unsigned int);

  static MSTypeData<Type,Allocator> *allocateWithSize   (unsigned int, MSAllocationFlag =MSConstructed, unsigned int =0);
  static MSTypeData<Type,Allocator> *allocateWithLength (unsigned int, MSAllocationFlag =MSConstructed, unsigned int =0);
  void deallocate(MSAllocationFlag =MSConstructed, unsigned int =0);

protected:
  INLINELINKAGE static unsigned int dataOffset(void);
  //
  // ***** PLEASE NOTE *****
  //
  // This class cannot contain any data members of its own; the only data members that it can have
  // are those inherited from MSData base class.  The reason for this is that MSTypeData<Type> will
  // allocate an array of data elements **directly following** the end of MSData data members,
  // including the possible padding for alignment.
  //
};

template <class Type> INLINELINKAGE MSBoolean isConstructionNeeded(Type *);
template <class Type> INLINELINKAGE MSBoolean isDestructionNeeded(Type *);

#ifndef MS_NO_INLINES
#include <MSTypes/MSTypeDataInlines.C>
#endif  // MS_NO_INLINES

#endif  // MSTypeDataHEADER
