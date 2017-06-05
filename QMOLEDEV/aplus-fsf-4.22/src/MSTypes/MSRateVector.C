///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////

#include <MSTypes/MSRateVector.H>

#ifndef MSDefinesHEADER
#include <MSTypes/MSDefines.H>
#endif

#if defined(MSTK_MANUAL_INSTANTIATION)

#include <MSTypes/MSTypeVector.C>

#if defined(MS_XLC_TEMPLATE_INSTANTIATION)
#pragma define (MSTypeVector<MSRate>)
#endif

#if defined(MS_EDG_TEMPLATE_INSTANTIATION)
#pragma instantiate MSBaseVector<MSRate,MSVectorModelAllocator<MSRate> >
#pragma instantiate MSObjectVector<MSRate>
#pragma instantiate MSTypeVector<MSRate>
#if !defined(MS_TEMPLATE_MANUAL_FRIEND_BUG)
// The version of EDG used by SGI's dcc compiler has a bug related to access from friends
#pragma instantiate ostream & operator<< (ostream &, const MSBaseVector<MSRate,MSVectorModelAllocator<MSRate> > &)
#endif  //MS_TEMPLATE_MANUAL_FRIEND_BUG
#endif

#if defined(MS_VC_TEMPLATE_INSTANTIATION)
template MSBaseVector<MSRate,MSVectorModelAllocator<MSRate> >;
template MSBaseVectorOps<MSRate,MSVectorModelAllocator<MSRate> >;
template MSObjectVector<MSRate>;
template MSTypeVector<MSRate>;
// instantiate non-inline friend template functions
static int __dummy01__()  {  if (0) operator<<(cout, MSBaseVector<MSRate,MSVectorModelAllocator<MSRate> >()); return 0;  }
static int __dummyInt01__=__dummy01__();
#endif  // MS_VC_TEMPLATE_INSTANTIATION

#if defined(MS_STD_TEMPLATE_INSTANTIATION)
template class MSBaseVector<MSRate,MSVectorModelAllocator<MSRate> >;
template class MSBaseVectorOps<MSRate,MSVectorModelAllocator<MSRate> >;
template class MSObjectVector<MSRate>;
template class MSTypeVector<MSRate>;
template ostream& operator<<(ostream&,const MSBaseVector<MSRate,MSVectorModelAllocator<MSRate> >&);
#endif

#endif // MSTK_MANUAL_INSTANTIATION




