///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////

#ifndef MSTypeDataHEADER
#include <MSTypes/MSTypeData.H>
#endif //MSTypeDataHEADER

#ifndef MSAllocatorHEADER
#include <MSTypes/MSAllocator.H>
#endif //MSAllocatorHEADER

#ifdef MSTK_MANUAL_INSTANTIATION
#include <MSTypes/MSTypeData.C>

#ifdef MS_XLC_TEMPLATE_INSTANTIATION
#pragma define (MSTypeData<char,MSAllocator<char> >)
#endif

#ifdef MS_EDG_TEMPLATE_INSTANTIATION
#pragma instantiate MSTypeData<char,MSAllocator<char> >
#endif

#ifdef MS_VC_TEMPLATE_INSTANTIATION
template MSTypeData<char,MSAllocator<char> >;
#endif  

#ifdef MS_STD_TEMPLATE_INSTANTIATION
template class MSTypeData<char,MSAllocator<char> >;
#endif

#endif //MSTK_MANUAL_INSTANTIATION
