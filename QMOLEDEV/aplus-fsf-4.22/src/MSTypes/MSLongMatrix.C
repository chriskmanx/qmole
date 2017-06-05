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

#if defined (MSTK_MANUAL_INSTANTIATION)
#include <MSTypes/MSTypeMatrix.C>


#include <MSTypes/MSLongVector.H>

#if defined(MS_XLC_TEMPLATE_INSTANTIATION)
#pragma define (MSTypeMatrix<long>)
#endif

#if defined(MS_VC_TEMPLATE_INSTANTIATION)
template MSTypeMatrix<long>;
#endif

#if defined(MS_EDG_TEMPLATE_INSTANTIATION)
#pragma instantiate MSTypeMatrix<long>
#pragma instantiate MSMatrixSTypePick<long>
#if !defined(MS_TEMPLATE_MANUAL_FRIEND_BUG)
// The version of EDG used by SGI's dcc compiler has a bug related to access from friends
#pragma instantiate ostream& operator<<(ostream&,const MSTypeMatrix<long>&)
#pragma instantiate MSTypeMatrix<long> operator-(const MSTypeMatrix<long>&)
#pragma instantiate MSTypeMatrix<long> operator+(const MSTypeMatrix<long>&,long)
#pragma instantiate MSTypeMatrix<long> operator+(long,const MSTypeMatrix<long>&)
#pragma instantiate MSTypeMatrix<long> operator+(const MSTypeMatrix<long>&,const MSTypeMatrix<long>&)
#pragma instantiate MSTypeMatrix<long> operator-(const MSTypeMatrix<long>&,long)
#pragma instantiate MSTypeMatrix<long> operator-(const MSTypeMatrix<long>&,const MSTypeMatrix<long>&)
#pragma instantiate MSTypeMatrix<long> operator*(const MSTypeMatrix<long>&,long)
#pragma instantiate MSTypeMatrix<long> operator*(long,const MSTypeMatrix<long>&)
#pragma instantiate MSTypeMatrix<long> operator*(const MSTypeMatrix<long>&,const MSTypeMatrix<long>&)
#pragma instantiate MSTypeMatrix<long> operator/(const MSTypeMatrix<long>&,long)
#pragma instantiate MSTypeMatrix<long> operator/(const MSTypeMatrix<long>&,const MSTypeMatrix<long>&)
#pragma instantiate MSTypeMatrix<long> operator+(const MSTypeMatrix<long>&,const MSTypeVector<long>&)
#pragma instantiate MSTypeMatrix<long> operator-(const MSTypeMatrix<long>&,const MSTypeVector<long>&)
#pragma instantiate MSTypeMatrix<long> operator*(const MSTypeMatrix<long>&,const MSTypeVector<long>&)
#pragma instantiate MSTypeMatrix<long> operator/(const MSTypeMatrix<long>&,const MSTypeVector<long>&)
#pragma instantiate MSTypeMatrix<long> operator+(const MSTypeVector<long>&,const MSTypeMatrix<long>&)
#pragma instantiate MSTypeMatrix<long> operator*(const MSTypeVector<long>&,const MSTypeMatrix<long>&)
#pragma instantiate MSTypeMatrix<long> multiply(const MSTypeMatrix<long>&,const MSTypeMatrix<long>&)
#pragma instantiate MSTypeMatrix<long> stack(const MSTypeMatrix<long>&,const MSTypeMatrix<long>&)
#pragma instantiate MSTypeMatrix<long> adjoin(const MSTypeMatrix<long>&,const MSTypeMatrix<long>&)
#endif  //!MS_TEMPLATE_MANUAL_FRIEND_BUG
#endif  //MS_EDG_TEMPLATE_INSTANTIATION

#if defined(MS_STD_TEMPLATE_INSTANTIATION)
template class MSTypeMatrix<long>;
template class MSMatrixSTypePick<long>;
template MSTypeMatrix<long> operator-(const MSTypeMatrix<long>&);
template MSTypeMatrix<long> operator+(const MSTypeMatrix<long>&,long);
template MSTypeMatrix<long> operator+(long,const MSTypeMatrix<long>&);
template MSTypeMatrix<long> operator+(const MSTypeMatrix<long>&,const MSTypeMatrix<long>&);
template MSTypeMatrix<long> operator-(const MSTypeMatrix<long>&,long);
template MSTypeMatrix<long> operator-(const MSTypeMatrix<long>&,const MSTypeMatrix<long>&);
template MSTypeMatrix<long> operator*(const MSTypeMatrix<long>&,long);
template MSTypeMatrix<long> operator*(long,const MSTypeMatrix<long>&);
template MSTypeMatrix<long> operator*(const MSTypeMatrix<long>&,const MSTypeMatrix<long>&);
template MSTypeMatrix<long> operator/(const MSTypeMatrix<long>&,long);
template MSTypeMatrix<long> operator/(const MSTypeMatrix<long>&,const MSTypeMatrix<long>&);
template MSTypeMatrix<long> operator+(const MSTypeMatrix<long>&,const MSTypeVector<long>&);
template MSTypeMatrix<long> operator-(const MSTypeMatrix<long>&,const MSTypeVector<long>&);
template MSTypeMatrix<long> operator*(const MSTypeMatrix<long>&,const MSTypeVector<long>&);
template MSTypeMatrix<long> operator/(const MSTypeMatrix<long>&,const MSTypeVector<long>&);
template MSTypeMatrix<long> operator+(const MSTypeVector<long>&,const MSTypeMatrix<long>&);
template MSTypeMatrix<long> operator*(const MSTypeVector<long>&,const MSTypeMatrix<long>&);
template MSTypeMatrix<long> multiply(const MSTypeMatrix<long>&,const MSTypeMatrix<long>&);
template MSTypeMatrix<long> adjoin(const MSTypeMatrix<long>&,const MSTypeMatrix<long>&);
#endif  //MS_STD_TEMPLATE_INSTANTIATION


#endif // MSTK_MANUAL_INSTANTIATION

