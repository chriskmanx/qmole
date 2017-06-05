#ifndef MSMergeSortHEADER
#define MSMergeSortHEADER

///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////


#include <sys/types.h> // look for size_t

#ifndef MSDefinesHEADER
#include <MSTypes/MSDefines.H>
#endif

#if defined(MS_NEED_IMPLEMENTATION_PRAGMA)
#pragma implementation("MSMergeSort.C")
#endif

template<class Type>
unsigned int msMergeSortUp(unsigned int n,Type *sp_,unsigned int *p_,unsigned int low_,unsigned int high_);

template<class Type>
unsigned int msMergeSortDown(unsigned int n,Type *sp_,unsigned int *p_,unsigned int low_,unsigned int high_);

#endif //MSMergeSortHEADER
