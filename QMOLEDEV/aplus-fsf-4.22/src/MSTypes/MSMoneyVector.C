///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////

#include <MSTypes/MSMoneyVector.H>

#ifdef MSTK_MANUAL_INSTANTIATION

#include <MSTypes/MSTypeVector.C>

#if defined(MS_XLC_TEMPLATE_INSTANTIATION)
#pragma define (MSTypeVector<MSMoney>)
#endif

#if defined(MS_EDG_TEMPLATE_INSTANTIATION)
#pragma instantiate MSBaseVector<MSMoney,MSVectorModelAllocator<MSMoney> >
#pragma instantiate MSObjectVector<MSMoney>
#pragma instantiate MSTypeVector<MSMoney>
#if !defined(MS_TEMPLATE_MANUAL_FRIEND_BUG)
// The version of EDG used by SGI's dcc compiler has a bug related to access from friends
#pragma instantiate ostream & operator<< (ostream &, const MSBaseVector<MSMoney,MSVectorModelAllocator<MSMoney> > &)
#endif  //MS_TEMPLATE_MANUAL_FRIEND_BUG

#endif //MS_EDG_TEMPLATE_INSTANTIATION

#if defined(MS_VC_TEMPLATE_INSTANTIATION)
template MSBaseVector<MSMoney,MSVectorModelAllocator<MSMoney> >;
template MSBaseVectorOps<MSMoney,MSVectorModelAllocator<MSMoney> >;
template MSObjectVector<MSMoney>;
template MSTypeVector<MSMoney>;
// instantiate non-inline friend template functions
static int __dummy01__()  {  if (0) operator<<(cout, MSBaseVector<MSMoney,MSVectorModelAllocator<MSMoney> >()); return 0;  }
static int __dummyInt01__=__dummy01__();
#endif  //MS_VC_TEMPLATE_INSTANTIATION

#if defined(MS_STD_TEMPLATE_INSTANTIATION)
template class MSBaseVector<MSMoney,MSVectorModelAllocator<MSMoney> >;
template class MSBaseVectorOps<MSMoney,MSVectorModelAllocator<MSMoney> >;
template class MSObjectVector<MSMoney>;
template class MSTypeVector<MSMoney>;
template ostream& operator<<(ostream&,const MSBaseVector<MSMoney,MSVectorModelAllocator<MSMoney> >&);
#endif

#endif // MSTK_MANUAL_INSTANTIATION



