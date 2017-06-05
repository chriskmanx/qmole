///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////

#include <MSTypes/MSDefines.H>
#include <MSTypes/MSTypeData.H>
#include <MSTypes/MSAllocator.H>

#if defined(MSTK_MANUAL_INSTANTIATION)

#include <MSTypes/MSTypeData.C>

#if defined(MS_XLC_TEMPLATE_INSTANTIATION)
#pragma define (MSTypeData<long,MSAllocator<long> >)
#endif

#if defined(MS_EDG_TEMPLATE_INSTANTIATION)
#pragma instantiate MSTypeData<long,MSAllocator<long> >
#endif  //MS_EDG_TEMPLATE_INSTANTIATION

#if defined(MS_VC_TEMPLATE_INSTANTIATION)
template MSTypeData<long,MSAllocator<long> >;
#endif  //MS_VC_TEMPLATE_INSTANTIATION

#if defined(MS_STD_TEMPLATE_INSTANTIATION)
template class MSTypeData<long,MSAllocator<long> >;
#endif


#endif //MSTK_MANUAL_INSTANTIATION
