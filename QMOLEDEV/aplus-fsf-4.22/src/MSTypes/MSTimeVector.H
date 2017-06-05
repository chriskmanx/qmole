#ifndef MSTimeVectorHEADER
#define MSTimeVectorHEADER

///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////

#include <MSTypes/MSTime.H>
#include <MSTypes/MSTypeVector.H>

typedef MSTypeVector<MSTime> MSTimeVector;

inline MSTime msBadData(const MSTime&) { return MSTime::nullTime(); }

// We provide provide explicit "do_not_instantiate" pragmas here to avoid instantiation complications in
// MStk applications.
//
#if !defined(__MSTYPESLIBRARY_BUILD__) && defined(MS_DO_NOT_INSTANTIATE) && defined(MS_EDG_TEMPLATE_INSTANTIATION)
#pragma do_not_instantiate MSVectorModelAllocator<MSTime>
#pragma do_not_instantiate MSTypeData<MSTime, MSVectorModelAllocator<MSTime> >
#pragma do_not_instantiate MSBaseVector<MSTime, MSVectorModelAllocator<MSTime> >
#pragma do_not_instantiate MSBaseVectorOps<MSTime, MSVectorModelAllocator<MSTime> >
#pragma do_not_instantiate MSObjectVector<MSTime>
#pragma do_not_instantiate MSVectorElement<MSTime>
#pragma do_not_instantiate MSTypeVector<MSTime>
#pragma do_not_instantiate ostream& operator<<(ostream&, const MSBaseVector<MSTime, MSVectorModelAllocator<MSTime> >&)
#pragma do_not_instantiate unsigned int msMergeSortUp(unsigned int, MSTime *, unsigned int *,unsigned int, unsigned int)
#pragma do_not_instantiate unsigned int msMergeSortDown(unsigned int, MSTime *, unsigned int *,unsigned int, unsigned int)
#endif // !__MSTYPESLIBRARY_BUILD__ && defined(MS_DO_NOT_INSTANTIATE) && defined(MS_EDG_TEMPLATE_INSTANTIATION)

#if defined(MS_VC_NEED_TEMPLATE_EXPORT)
class MSTypesExport MSBaseVector<MSTime,MSVectorModelAllocator<MSTime> >;
class MSTypesExport MSBaseVectorOps<MSTime,MSVectorModelAllocator<MSTime> >;
class MSTypesExport MSObjectVector<MSTime>;
class MSTypesExport MSVectorElement<MSTime>;
class MSTypesExport MSTypeVector<MSTime>;
#endif

#endif  //MSTimeVectorHEADER
