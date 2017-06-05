#ifndef MSDateVectorHEADER
#define MSDateVectorHEADER

///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////

#include <MSTypes/MSDate.H>
#include <MSTypes/MSTypeVector.H>

typedef MSTypeVector<MSDate> MSDateVector;

inline MSDate msBadData(const MSDate&) { return MSDate::nullDate(); }

// We provide provide explicit "do_not_instantiate" pragmas here to avoid instantiation complications in
// MStk applications.
//
#if !defined(__MSTYPESLIBRARY_BUILD__) && defined(MS_DO_NOT_INSTANTIATE) && defined(MS_EDG_TEMPLATE_INSTANTIATION)
#pragma do_not_instantiate MSVectorModelAllocator<MSDate>
#pragma do_not_instantiate MSTypeData<MSDate, MSVectorModelAllocator<MSDate> >
#pragma do_not_instantiate MSBaseVector<MSDate, MSVectorModelAllocator<MSDate> >
#pragma do_not_instantiate MSBaseVectorOps<MSDate, MSVectorModelAllocator<MSDate> >
#pragma do_not_instantiate MSObjectVector<MSDate>
#pragma do_not_instantiate MSVectorElement<MSDate>
#pragma do_not_instantiate MSTypeVector<MSDate>
#pragma do_not_instantiate ostream& operator<<(ostream&, const MSBaseVector<MSDate, MSVectorModelAllocator<MSDate> >&)
#pragma do_not_instantiate unsigned int msMergeSortUp(unsigned int, MSDate *, unsigned int *,unsigned int, unsigned int)
#pragma do_not_instantiate unsigned int msMergeSortDown(unsigned int, MSDate *, unsigned int *,unsigned int, unsigned int)
#endif // !__MSTYPESLIBRARY_BUILD__ && defined(MS_DO_NOT_INSTANTIATE) && defined(MS_EDG_TEMPLATE_INSTANTIATION)

#if defined(MS_VC_NEED_TEMPLATE_EXPORT)
class MSTypesExport MSBaseVector<MSDate,MSVectorModelAllocator<MSDate> >;
class MSTypesExport MSBaseVectorOps<MSDate,MSVectorModelAllocator<MSDate> >;
class MSTypesExport MSObjectVector<MSDate>;
class MSTypesExport MSVectorElement<MSDate>;
class MSTypesExport MSTypeVector<MSDate>;
#endif

#endif  //MSDateVectorHEADER
