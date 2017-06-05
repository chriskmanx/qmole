///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////

#include <MSTypes/MSTimeVector.H>

#if defined(MSTK_MANUAL_INSTANTIATION)

#include <MSTypes/MSTypeVector.C>

#if defined(MS_XLC_TEMPLATE_INSTANTIATION)
#pragma define (MSTypeVector<MSTime>)
#endif

#if defined(MS_EDG_TEMPLATE_INSTANTIATION)
#pragma instantiate MSBaseVector<MSTime,MSVectorModelAllocator<MSTime> >
#pragma instantiate MSObjectVector<MSTime>
#pragma instantiate MSTypeVector<MSTime>
#if !defined (MS_TEMPLATE_MANUAL_FRIEND_BUG)
// The version of EDG used by SGI's dcc compiler has a bug related to access from friends
#pragma instantiate ostream & operator<< (ostream &, const MSBaseVector<MSTime,MSVectorModelAllocator<MSTime> > &)
#endif  // MS_TEMPLATE_MANUAL_FRIEND_BUG

#endif  // MS_EDG_TEMPLATE_INSTANTIATION


#if defined(MS_VC_TEMPLATE_INSTANTIATION)
template MSBaseVector<MSTime,MSVectorModelAllocator<MSTime> >;
template MSBaseVectorOps<MSTime,MSVectorModelAllocator<MSTime> >;
template MSObjectVector<MSTime>;
template MSTypeVector<MSTime>;
// instantiate non-inline friend template functions
static int __dummy01__()  {  if (0) operator<<(cout, MSBaseVector<MSTime,MSVectorModelAllocator<MSTime> >()); return 0;  }
static int __dummyInt01__=__dummy01__();
#endif  //MS_VC_TEMPLATE_INSTANTIATION

#if defined(MS_STD_TEMPLATE_INSTANTIATION)
template class MSBaseVector<MSTime,MSVectorModelAllocator<MSTime> >;
template class MSBaseVectorOps<MSTime,MSVectorModelAllocator<MSTime> >;
template class MSObjectVector<MSTime>;
template class MSTypeVector<MSTime>;
template ostream& operator<<(ostream&,const MSBaseVector<MSTime,MSVectorModelAllocator<MSTime> >&);
#endif


#endif // MSTK_MANUAL_INSTANTIATION 





