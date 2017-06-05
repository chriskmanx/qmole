///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////
// This file provides common instantiations for MSTreeListView template.

#include <MSTypes/MSString.H>
#include <MSGUI/MSPixmap.H>

// force local classes defined MSTreeListView.C to be compiled.
#if defined(MS_FORCE_INTERNAL_TEMPLATE_INSTANTIATION)
#include <MSGUI/MSTreeListView.H>
#include <MSGUI/MSTreeListView.C>
#endif

#ifndef MSTreeListViewHEADER
inline MSString const &key(const MSPixmap &pixmap_)
{
  return pixmap_.name();
}
#endif

#if defined(MSTK_MANUAL_INSTANTIATION)
#include <MSTypes/MSGenericVector.C>
#include <MSTypes/MSIHashKeySet.C>

#if defined(MS_EDG_TEMPLATE_INSTANTIATION)
#pragma instantiate MSIHashKeySet<MSPixmap,MSString>
#pragma instantiate MSGenericVector<MSPixmap>
#endif

#if defined(MS_STD_TEMPLATE_INSTANTIATION)
template class MSIHashKeySet<MSPixmap,MSString>;
template class MSGenericVector<MSPixmap>;
#endif

#if defined(MS_VC_TEMPLATE_INSTANTIATION)
template MSIHashKeySet<MSPixmap,MSString>;
template MSGenericVector<MSPixmap>;
#endif

#if defined(MS_XLC_TEMPLATE_INSTANTIATION)
#pragma define(MSGenericVector<MSPixmap>)
#pragma define(MSIHashKeySet<MSPixmap,MSString>)
#endif


#endif //MSTK_MANUAL_INSTANTIATION
