#ifndef MSVaraibleRegistryHEADER
#define MSVaraibleRegistryHEADER

///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////


#include <MSTypes/MSVariable.H>
#include <MSTypes/MSIHashKeySet.H>

#if defined(MS_VC_NEED_TEMPLATE_EXPORT)
class MSTypesExport MSIHashKeySet<MSVariable,MSString>;
#endif

typedef MSIHashKeySet<MSVariable,MSString> MSVariableRegistry;
DECLARE_ELEMENT_FOR_OPS_TYPE(MSVariable)

// We provide provide explicit "do_not_instantiate" pragmas here to avoid instantiation complications in
// MStk applications.
//
#if !defined(__MSTYPESLIBRARY_BUILD__) && defined(MS_DO_NOT_INSTANTIATE) && defined(MS_EDG_TEMPLATE_INSTANTIATION)
#pragma do_not_instantiate MSIHashKeySet<MSVariable,MSString>
#pragma do_not_instantiate MSIHashKeySetNode<MSVariable>
#pragma do_not_instantiate MSIHashKeySetCursor<MSVariable,MSString>
#endif // !__MSTYPESLIBRARY_BUILD__ && defined(MS_DO_NOT_INSTANTIATE) && defined(MS_EDG_TEMPLATE_INSTANTIATION)

#endif
