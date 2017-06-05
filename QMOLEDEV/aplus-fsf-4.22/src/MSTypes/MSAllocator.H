#ifndef MSAllocatorHEADER
#define MSAllocatorHEADER

///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////

#include <stddef.h>		// for size_t
#ifndef MSDefinesHEADER
#include <MSTypes/MSDefines.H>	// for INLINELINKAGE
#endif //MSDefinesHEADER

template <class Type>
class MSAllocator
{
public:
  INLINELINKAGE MSAllocator(void);
  INLINELINKAGE ~MSAllocator(void);

  INLINELINKAGE Type *allocate(size_t n_, Type *);
  INLINELINKAGE Type *allocate(size_t size_);
  INLINELINKAGE void deallocate(Type *p_);
  INLINELINKAGE void construct(Type *p_, const Type& value_);
  INLINELINKAGE void destroy(Type *p_);
};

template <class Type> INLINELINKAGE void msConstruct(Type *, const Type&);
template <class Type> INLINELINKAGE void msDestroy(Type *);

#ifndef MS_NO_INLINES
#ifndef MSAllocatorINLINES
#include <MSTypes/MSAllocatorInlines.C>
#endif //MSAllocatorINLINES
#endif //MS_NO_INLINES

#endif //MSAllocatorHEADER
