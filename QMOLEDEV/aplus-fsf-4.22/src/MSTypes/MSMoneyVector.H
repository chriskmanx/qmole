#ifndef MSMoneyVectorHEADER
#define MSMoneyVectorHEADER

///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////

#include <MSTypes/MSMoney.H>
#include <MSTypes/MSTypeVector.H>

typedef MSTypeVector<MSMoney> MSMoneyVector;

inline MSMoney msBadData(const MSMoney&) { return MSMoney(0.0); }

// We provide provide explicit "do_not_instantiate" pragmas here to avoid instantiation complications in
// MStk applications.
//
#if !defined(__MSTYPESLIBRARY_BUILD__) && defined(MS_DO_NOT_INSTANTIATE) && defined(MS_EDG_TEMPLATE_INSTANTIATION)
#pragma do_not_instantiate MSVectorModelAllocator<MSMoney>
#pragma do_not_instantiate MSTypeData<MSMoney, MSVectorModelAllocator<MSMoney> >
#pragma do_not_instantiate MSBaseVector<MSMoney, MSVectorModelAllocator<MSMoney> >
#pragma do_not_instantiate MSBaseVectorOps<MSMoney, MSVectorModelAllocator<MSMoney> >
#pragma do_not_instantiate MSObjectVector<MSMoney>
#pragma do_not_instantiate MSVectorElement<MSMoney>
#pragma do_not_instantiate MSTypeVector<MSMoney>
#pragma do_not_instantiate ostream& operator<<(ostream&, const MSBaseVector<MSMoney, MSVectorModelAllocator<MSMoney> >&)
#pragma do_not_instantiate unsigned int msMergeSortUp(unsigned int, MSMoney *, unsigned int *,unsigned int, unsigned int)
#pragma do_not_instantiate unsigned int msMergeSortDown(unsigned int, MSMoney *, unsigned int *,unsigned int, unsigned int)
#endif // !__MSTYPESLIBRARY_BUILD__ && defined(MS_DO_NOT_INSTANTIATE) && defined(MS_EDG_TEMPLATE_INSTANTIATION)

#endif  //MSMoneyVectorHEADER
