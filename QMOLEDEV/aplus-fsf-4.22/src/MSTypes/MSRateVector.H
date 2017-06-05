#ifndef MSRateVectorHEADER
#define MSRateVectorHEADER

///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////

#include <MSTypes/MSRate.H>
#include <MSTypes/MSTypeVector.H>

typedef MSTypeVector<MSRate> MSRateVector;

inline MSRate msBadData(const MSRate&) { return MSRate(0.0); }

// We provide provide explicit "do_not_instantiate" pragmas here to avoid instantiation complications in
// MStk applications.
//
#if !defined(__MSTYPESLIBRARY_BUILD__) && defined(MS_DO_NOT_INSTANTIATE) && defined(MS_EDG_TEMPLATE_INSTANTIATION)
#pragma do_not_instantiate MSVectorModelAllocator<MSRate>
#pragma do_not_instantiate MSTypeData<MSRate, MSVectorModelAllocator<MSRate> >
#pragma do_not_instantiate MSBaseVector<MSRate, MSVectorModelAllocator<MSRate> >
#pragma do_not_instantiate MSBaseVectorOps<MSRate, MSVectorModelAllocator<MSRate> >
#pragma do_not_instantiate MSObjectVector<MSRate>
#pragma do_not_instantiate MSVectorElement<MSRate>
#pragma do_not_instantiate MSTypeVector<MSRate>
#pragma do_not_instantiate ostream& operator<<(ostream&, const MSBaseVector<MSRate, MSVectorModelAllocator<MSRate> >&)
#pragma do_not_instantiate unsigned int msMergeSortUp(unsigned int, MSRate *, unsigned int *,unsigned int, unsigned int)
#pragma do_not_instantiate unsigned int msMergeSortDown(unsigned int, MSRate *, unsigned int *,unsigned int, unsigned int)
#endif // !__MSTYPESLIBRARY_BUILD__ && defined(MS_DO_NOT_INSTANTIATE) && defined(MS_EDG_TEMPLATE_INSTANTIATION)

#endif  //MSRateVectorHEADER
