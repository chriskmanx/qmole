///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////

#ifndef MSTypeMatrixHEADER
#include <MSTypes/MSTypeMatrix.H>
#endif

#ifndef MSTypeDataHEADER
#include <MSTypes/MSTypeData.H>
#endif

#if defined(MSTK_MANUAL_INSTANTIATION)
#include <MSTypes/MSTypeMatrix.C>
#include <MSTypes/MSUnsignedVector.H>

#if defined(MS_XLC_TEMPLATE_INSTANTIATION)
#pragma define (MSTypeMatrix<unsigned>)
#endif

#if defined(MS_VC_TEMPLATE_INSTANTIATION)
template MSTypeMatrix<unsigned>;
#endif

#if defined(MS_EDG_TEMPLATE_INSTANTIATION)
#pragma instantiate MSTypeMatrix<unsigned>
#pragma instantiate MSMatrixSTypePick<unsigned>
#if !defined (MS_TEMPLATE_MANUAL_FRIEND_BUG)
// The version of EDG used by SGI's dcc compiler has a bug related to access from friends
#pragma instantiate ostream& operator<<(ostream&,const MSTypeMatrix<unsigned>&)
#pragma instantiate MSTypeMatrix<unsigned> operator-(const MSTypeMatrix<unsigned>&)
#pragma instantiate MSTypeMatrix<unsigned> operator+(const MSTypeMatrix<unsigned>&,unsigned)
#pragma instantiate MSTypeMatrix<unsigned> operator+(unsigned,const MSTypeMatrix<unsigned>&)
#pragma instantiate MSTypeMatrix<unsigned> operator+(const MSTypeMatrix<unsigned>&,const MSTypeMatrix<unsigned>&)
#pragma instantiate MSTypeMatrix<unsigned> operator-(const MSTypeMatrix<unsigned>&,unsigned)
#pragma instantiate MSTypeMatrix<unsigned> operator-(const MSTypeMatrix<unsigned>&,const MSTypeMatrix<unsigned>&)
#pragma instantiate MSTypeMatrix<unsigned> operator*(const MSTypeMatrix<unsigned>&,unsigned)
#pragma instantiate MSTypeMatrix<unsigned> operator*(unsigned,const MSTypeMatrix<unsigned>&)
#pragma instantiate MSTypeMatrix<unsigned> operator*(const MSTypeMatrix<unsigned>&,const MSTypeMatrix<unsigned>&)
#pragma instantiate MSTypeMatrix<unsigned> operator/(const MSTypeMatrix<unsigned>&,unsigned)
#pragma instantiate MSTypeMatrix<unsigned> operator/(const MSTypeMatrix<unsigned>&,const MSTypeMatrix<unsigned>&)
#pragma instantiate MSTypeMatrix<unsigned> operator+(const MSTypeMatrix<unsigned>&,const MSTypeVector<unsigned>&)
#pragma instantiate MSTypeMatrix<unsigned> operator-(const MSTypeMatrix<unsigned>&,const MSTypeVector<unsigned>&)
#pragma instantiate MSTypeMatrix<unsigned> operator*(const MSTypeMatrix<unsigned>&,const MSTypeVector<unsigned>&)
#pragma instantiate MSTypeMatrix<unsigned> operator/(const MSTypeMatrix<unsigned>&,const MSTypeVector<unsigned>&)
#pragma instantiate MSTypeMatrix<unsigned> operator+(const MSTypeVector<unsigned>&,const MSTypeMatrix<unsigned>&)
#pragma instantiate MSTypeMatrix<unsigned> operator*(const MSTypeVector<unsigned>&,const MSTypeMatrix<unsigned>&)
#pragma instantiate MSTypeMatrix<unsigned> multiply(const MSTypeMatrix<unsigned>&,const MSTypeMatrix<unsigned>&)
#pragma instantiate MSTypeMatrix<unsigned> stack(const MSTypeMatrix<unsigned>&,const MSTypeMatrix<unsigned>&)
#pragma instantiate MSTypeMatrix<unsigned> adjoin(const MSTypeMatrix<unsigned>&,const MSTypeMatrix<unsigned>&)
#endif  // !MS_TEMPLATE_MANUAL_FRIEND_BUG
#endif  // MS_EDG_TEMPLATE_INSTANTIATION

#if defined(MS_STD_TEMPLATE_INSTANTIATION)
template class MSTypeMatrix<unsigned>;
template class MSMatrixSTypePick<unsigned>;
template ostream& operator<<(ostream&,const MSTypeMatrix<unsigned>&);
template MSTypeMatrix<unsigned> operator-(const MSTypeMatrix<unsigned>&);
template MSTypeMatrix<unsigned> operator+(const MSTypeMatrix<unsigned>&,unsigned);
template MSTypeMatrix<unsigned> operator+(unsigned,const MSTypeMatrix<unsigned>&);
template MSTypeMatrix<unsigned> operator+(const MSTypeMatrix<unsigned>&,const MSTypeMatrix<unsigned>&);
template MSTypeMatrix<unsigned> operator-(const MSTypeMatrix<unsigned>&,unsigned);
template MSTypeMatrix<unsigned> operator-(const MSTypeMatrix<unsigned>&,const MSTypeMatrix<unsigned>&);
template MSTypeMatrix<unsigned> operator*(const MSTypeMatrix<unsigned>&,unsigned);
template MSTypeMatrix<unsigned> operator*(unsigned,const MSTypeMatrix<unsigned>&);
template MSTypeMatrix<unsigned> operator*(const MSTypeMatrix<unsigned>&,const MSTypeMatrix<unsigned>&);
template MSTypeMatrix<unsigned> operator/(const MSTypeMatrix<unsigned>&,unsigned);
template MSTypeMatrix<unsigned> operator/(const MSTypeMatrix<unsigned>&,const MSTypeMatrix<unsigned>&);
template MSTypeMatrix<unsigned> operator+(const MSTypeMatrix<unsigned>&,const MSTypeVector<unsigned>&);
template MSTypeMatrix<unsigned> operator-(const MSTypeMatrix<unsigned>&,const MSTypeVector<unsigned>&);
template MSTypeMatrix<unsigned> operator*(const MSTypeMatrix<unsigned>&,const MSTypeVector<unsigned>&);
template MSTypeMatrix<unsigned> operator/(const MSTypeMatrix<unsigned>&,const MSTypeVector<unsigned>&);
template MSTypeMatrix<unsigned> operator+(const MSTypeVector<unsigned>&,const MSTypeMatrix<unsigned>&);
template MSTypeMatrix<unsigned> operator*(const MSTypeVector<unsigned>&,const MSTypeMatrix<unsigned>&);
template MSTypeMatrix<unsigned> multiply(const MSTypeMatrix<unsigned>&,const MSTypeMatrix<unsigned>&);
template MSTypeMatrix<unsigned> stack(const MSTypeMatrix<unsigned>&,const MSTypeMatrix<unsigned>&);
template MSTypeMatrix<unsigned> adjoin(const MSTypeMatrix<unsigned>&,const MSTypeMatrix<unsigned>&);
#endif  // MS_STD_TEMPLATE_INSTANTIATION

#endif // MSTK_MANUAL_INSTANTIATION

