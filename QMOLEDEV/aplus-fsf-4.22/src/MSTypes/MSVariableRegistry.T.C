///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////

#include <MSTypes/MSString.H>
#include <MSTypes/MSVariable.H>
#include <MSTypes/MSVariableRegistry.H>

#if defined(MSTK_MANUAL_INSTANTIATION)

#include <MSTypes/MSIHashKeySet.C>

#if defined(MS_XLC_TEMPLATE_INSTANTIATION)
#pragma define(MSIHashKeySet<MSVariable,MSString>)
#endif

#if defined(MS_EDG_TEMPLATE_INSTANTIATION)
#pragma instantiate MSIHashKeySet<MSVariable,MSString>
#endif

#if defined(MS_STD_TEMPLATE_INSTANTIATION)
template class MSIHashKeySet<MSVariable,MSString>;
#endif

#if defined(MS_VC_TEMPLATE_INSTANTIATION)
template MSIHashKeySet<MSVariable,MSString>;
#endif

#endif  //MSTK_MANUAL_INSTANTIATION
