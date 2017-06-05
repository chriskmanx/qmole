///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////

#include <MSTypes/MSDateVector.H>

#if defined(MSTK_MANUAL_INSTANTIATION)

#include <MSTypes/MSTypeVector.C>

#if defined(MS_XLC_TEMPLATE_INSTANTIATION)
#pragma define (MSTypeVector<MSDate>)
#endif

#if defined(MS_EDG_TEMPLATE_INSTANTIATION)
#pragma instantiate MSBaseVector<MSDate,MSVectorModelAllocator<MSDate> >
#pragma instantiate MSObjectVector<MSDate>
#pragma instantiate MSTypeVector<MSDate>

#if !defined(MS_TEMPLATE_MANUAL_FRIEND_BUG)
// The version of EDG used by SGI's dcc compiler has a bug related to access from friends
#pragma instantiate ostream & operator<< (ostream &, const MSBaseVector<MSDate,MSVectorModelAllocator<MSDate> > &)
#endif  //MS_TEMPLATE_MANUAL_FRIEND_BUG

#endif //MS_EDG_TEMPLATE_INSTANTIATION

#if defined(MS_VC_TEMPLATE_INSTANTIATION)
template MSBaseVector<MSDate,MSVectorModelAllocator<MSDate> >;
template MSBaseVectorOps<MSDate,MSVectorModelAllocator<MSDate> >;
template MSObjectVector<MSDate>;
template MSVectorElement<MSDate>;
template MSTypeVector<MSDate>;
// instantiate non-inline friend template functions
static int __dummy01__()  {  if (0) operator<<(cout, MSBaseVector<MSDate,MSVectorModelAllocator<MSDate> >()); return 0;  }
static int __dummyInt01__=__dummy01__();
#endif  // MS_VC_TEMPLATE_INSTANTIATION

#if defined(MS_STD_TEMPLATE_INSTANTIATION)
template class MSBaseVector<MSDate,MSVectorModelAllocator<MSDate> >;
template class MSBaseVectorOps<MSDate,MSVectorModelAllocator<MSDate> >;
template class MSObjectVector<MSDate>;
template class MSVectorElement<MSDate>;
template class MSTypeVector<MSDate>;
template ostream& operator<<(ostream&,const MSBaseVector<MSDate,MSVectorModelAllocator<MSDate> >&);
#endif // MS_STD_TEMPLATE_INSTANTIATION

#endif // MSTK_MANUAL_INSTANTIATION




