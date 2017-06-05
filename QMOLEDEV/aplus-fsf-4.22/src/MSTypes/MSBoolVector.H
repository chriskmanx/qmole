#ifndef MSBoolVectorHEADER
#define MSBoolVectorHEADER

///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////

#include <MSTypes/MSBool.H>
#include <MSTypes/MSTypeVector.H>

typedef MSTypeVector<MSBool> MSBoolVector;

inline MSBool msBadData(const MSBool&) { return MSBool(MSFalse); }

// We provide provide explicit "do_not_instantiate" pragmas here to avoid instantiation complications in
// MStk applications.
//
#if !defined(__MSTYPESLIBRARY_BUILD__) && defined(MS_DO_NOT_INSTANTIATE) && defined(MS_EDG_TEMPLATE_INSTANTIATION)
#pragma do_not_instantiate MSVectorModelAllocator<MSBool>
#pragma do_not_instantiate MSTypeData<MSBool, MSVectorModelAllocator<MSBool> >
#pragma do_not_instantiate MSBaseVector<MSBool, MSVectorModelAllocator<MSBool> >
#pragma do_not_instantiate MSBaseVectorOps<MSBool, MSVectorModelAllocator<MSBool> >
#pragma do_not_instantiate MSObjectVector<MSBool>
#pragma do_not_instantiate MSVectorElement<MSBool>
#pragma do_not_instantiate MSTypeVector<MSBool>
#pragma do_not_instantiate ostream& operator<<(ostream&, const MSBaseVector<MSBool, MSVectorModelAllocator<MSBool> >&)
#pragma do_not_instantiate unsigned int msMergeSortUp(unsigned int, MSBool *, unsigned int *,unsigned int, unsigned int)
#pragma do_not_instantiate unsigned int msMergeSortDown(unsigned int, MSBool *, unsigned int *,unsigned int, unsigned int)
#endif // !__MSTYPESLIBRARY_BUILD__ && defined(MS_DO_NOT_INSTANTIATE) && defined(MS_EDG_TEMPLATE_INSTANTIATION)

#endif  //MSBoolVectorHEADER
