///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////

#include <MSTypes/MSBoolVector.H>

#if defined(MSTK_MANUAL_INSTANTIATION) 
#include <MSTypes/MSTypeVector.C>

#if defined(MS_XLC_TEMPLATE_INSTANTIATION)
#pragma define (MSTypeVector<MSBool>)
#endif

#if defined(MS_EDG_TEMPLATE_INSTANTIATION)
#pragma instantiate MSBaseVector<MSBool,MSVectorModelAllocator<MSBool> >
#pragma instantiate MSObjectVector<MSBool>
#pragma instantiate MSTypeVector<MSBool>
#if !defined(MS_TEMPLATE_MANUAL_FRIEND_BUG)
// The version of EDG used by SGI's dcc compiler has a bug related to access from friends
#pragma instantiate ostream & operator<< (ostream &, const MSBaseVector<MSBool,MSVectorModelAllocator<MSBool> > &)
#endif  //__sgi
#endif

#if defined(MS_VC_TEMPLATE_INSTANTIATION)
template MSBaseVector<MSBool,MSVectorModelAllocator<MSBool> >;
template MSBaseVectorOps<MSBool,MSVectorModelAllocator<MSBool> >;
template MSObjectVector<MSBool>;
template MSTypeVector<MSBool>;
// instantiate non-inline friend template functions
static int __dummy01__()  {  if (0) operator<<(cout, MSBaseVector<MSBool,MSVectorModelAllocator<MSBool> >()); return 0;  }
static int __dummyInt01__=__dummy01__();
#endif  //MS_VC_TEMPLATE_INSTANTIATION

#if defined(MS_STD_TEMPLATE_INSTANTIATION)
template class MSBaseVector<MSBool,MSVectorModelAllocator<MSBool> >;
template class MSBaseVectorOps<MSBool,MSVectorModelAllocator<MSBool> >;
template class MSObjectVector<MSBool>;
template class MSTypeVector<MSBool>;
template ostream& operator<<(ostream&,const MSBaseVector<MSBool,MSVectorModelAllocator<MSBool> >&);
#endif

#endif




