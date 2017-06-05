///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////

#include <MSGUI/MSTrace.H>
#include <MSGUI/MSTraceSet.H>
#include <MSGUI/MSParagraph.H>
#include <MSGUI/MSTableColumn.H>
#include <MSGUI/MSPrintManager.H>
#include <MSGUI/MSPointerArray.H>

// Specializations for the MSGUI objects

#if defined(MSTK_MANUAL_INSTANTIATION)
#include <MSGUI/MSPointerArray.C>

#if defined(MS_EDG_TEMPLATE_INSTANTIATION)
#pragma instantiate MSPointerArray<MSTrace>
#pragma instantiate MSPointerArray<MSTraceSet>
#pragma instantiate MSPointerArray<MSParagraph>
#pragma instantiate MSPointerArray<MSPrintItem>
#pragma instantiate MSPointerArray<MSPrintManager>
#pragma instantiate MSPointerArray<MSTableColumn>
#endif

#if defined(MS_XLC_TEMPLATE_INSTANTIATION)
#pragma define (MSPointerArray<MSTrace>)
#pragma define (MSPointerArray<MSTraceSet>)
#pragma define (MSPointerArray<MSParagraph>)
#pragma define (MSPointerArray<MSPrintItem>)
#pragma define (MSPointerArray<MSPrintManager>)
#pragma define (MSPointerArray<MSTableColumn>)
#endif

#if defined(MS_STD_TEMPLATE_INSTANTIATION)
template class MSPointerArray<MSTrace>;
template class MSPointerArray<MSTraceSet>;
template class MSPointerArray<MSParagraph>;
template class MSPointerArray<MSPrintItem>;
template class MSPointerArray<MSPrintManager>;
template class MSPointerArray<MSTableColumn>;
#endif

#if defined(MS_VC_TEMPLATE_INSTANTIATION)
template MSPointerArray<MSTrace>;
template MSPointerArray<MSTraceSet>;
template MSPointerArray<MSParagraph>;
template MSPointerArray<MSPrintItem>;
template MSPointerArray<MSPrintManager>;
template MSPointerArray<MSTableColumn>;
#endif

#endif //MSTK_MANUAL_INSTANTIATION
