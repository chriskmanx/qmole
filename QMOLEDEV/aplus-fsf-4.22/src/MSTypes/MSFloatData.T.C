///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////

#ifndef MSTypeDataHEADER
#include <MSTypes/MSTypeData.H>
#endif

#ifndef MSAllocatorHEADER
#include <MSTypes/MSAllocator.H>
#endif //MSAllocatorHEADER

#if defined(MSTK_MANUAL_INSTANTIATION)
#include <MSTypes/MSTypeData.C>

#if defined(MS_XLC_TEMPLATE_INSTANTIATION)
#pragma define (MSTypeData<double,MSAllocator<double> >)
#endif

#if defined(MS_EDG_TEMPLATE_INSTANTIATION)
#pragma instantiate MSTypeData<double,MSAllocator<double> >
#endif  //MS_EDG_TEMPLATE_INSTANTIATION

#if defined(MS_VC_TEMPLATE_INSTANTIATION)
template MSTypeData<double,MSAllocator<double> >;
#endif  //MS_VC_TEMPLATE_INSTANTIATION

#if defined(MS_STD_TEMPLATE_INSTANTIATION)
template class MSTypeData<double,MSAllocator<double> >;
#endif

#endif //MSTK_MANUAL_INSTANTIATION

