///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////

#ifndef MSDefinesHEADER
#include <MSTypes/MSDefines.H>
#endif

#ifndef MSTypeMatrixHEADER
#include <MSTypes/MSTypeMatrix.H>
#endif

#ifndef MSTypeDataHEADER
#include <MSTypes/MSTypeData.H>
#endif

#if defined(MSTK_MANUAL_INSTANTIATION)

#include <MSTypes/MSTypeMatrix.C>
#include <MSTypes/MSUnsignedLongVector.H>

#if defined(MS_XLC_TEMPLATE_INSTANTIATION)
#pragma define (MSTypeMatrix<unsigned long>)
#endif

#if defined(MS_VC_TEMPLATE_INSTANTIATION)
template MSTypeMatrix<unsigned long>;
#endif

#if defined(MS_EDG_TEMPLATE_INSTANTIATION)
#pragma instantiate MSTypeMatrix<unsigned long>
#pragma instantiate MSMatrixSTypePick<unsigned long>
#if !defined (MS_TEMPLATE_MANUAL_FRIEND_BUG)
// The version of EDG used by SGI's dcc compiler has a bug related to access from friends
#pragma instantiate ostream& operator<<(ostream&,const MSTypeMatrix<unsigned long>&)
#pragma instantiate MSTypeMatrix<unsigned long> operator-(const MSTypeMatrix<unsigned long>&)
#pragma instantiate MSTypeMatrix<unsigned long> operator+(const MSTypeMatrix<unsigned long>&,unsigned long)
#pragma instantiate MSTypeMatrix<unsigned long> operator+(unsigned long,const MSTypeMatrix<unsigned long>&)
#pragma instantiate MSTypeMatrix<unsigned long> operator+(const MSTypeMatrix<unsigned long>&, \
							  const MSTypeMatrix<unsigned long>&)
#pragma instantiate MSTypeMatrix<unsigned long> operator-(const MSTypeMatrix<unsigned long>&,unsigned long)
#pragma instantiate MSTypeMatrix<unsigned long> operator-(const MSTypeMatrix<unsigned long>&, \
							  const MSTypeMatrix<unsigned long>&)
#pragma instantiate MSTypeMatrix<unsigned long> operator*(const MSTypeMatrix<unsigned long>&,unsigned long)
#pragma instantiate MSTypeMatrix<unsigned long> operator*(unsigned long,const MSTypeMatrix<unsigned long>&)
#pragma instantiate MSTypeMatrix<unsigned long> operator*(const MSTypeMatrix<unsigned long>&, \
							  const MSTypeMatrix<unsigned long>&)
#pragma instantiate MSTypeMatrix<unsigned long> operator/(const MSTypeMatrix<unsigned long>&,unsigned long)
#pragma instantiate MSTypeMatrix<unsigned long> operator/(const MSTypeMatrix<unsigned long>&, \
							  const MSTypeMatrix<unsigned long>&)
#pragma instantiate MSTypeMatrix<unsigned long> operator+(const MSTypeMatrix<unsigned long>&, \
							  const MSTypeVector<unsigned long>&)
#pragma instantiate MSTypeMatrix<unsigned long> operator-(const MSTypeMatrix<unsigned long>&, \
							  const MSTypeVector<unsigned long>&)
#pragma instantiate MSTypeMatrix<unsigned long> operator*(const MSTypeMatrix<unsigned long>&, \
							  const MSTypeVector<unsigned long>&)
#pragma instantiate MSTypeMatrix<unsigned long> operator/(const MSTypeMatrix<unsigned long>&, \
							  const MSTypeVector<unsigned long>&)
#pragma instantiate MSTypeMatrix<unsigned long> operator+(const MSTypeVector<unsigned long>&, \
							  const MSTypeMatrix<unsigned long>&)
#pragma instantiate MSTypeMatrix<unsigned long> operator*(const MSTypeVector<unsigned long>&, \
							  const MSTypeMatrix<unsigned long>&)
#pragma instantiate MSTypeMatrix<unsigned long> multiply(const MSTypeMatrix<unsigned long>&,const MSTypeMatrix<unsigned long>&)
#pragma instantiate MSTypeMatrix<unsigned long> stack(const MSTypeMatrix<unsigned long>&,const MSTypeMatrix<unsigned long>&)
#pragma instantiate MSTypeMatrix<unsigned long> adjoin(const MSTypeMatrix<unsigned long>&,const MSTypeMatrix<unsigned long>&)
#endif  // !MS_TEMPLATE_MANUAL_FRIEND_BUG
#endif  // MS_EDG_TEMPLATE_INSTANTIATION

#if defined(MS_STD_TEMPLATE_INSTANTIATION)
template class MSTypeMatrix<unsigned long>;
template class MSMatrixSTypePick<unsigned long>;
template ostream& operator<<(ostream&,const MSTypeMatrix<unsigned long>&);
template MSTypeMatrix<unsigned long> operator-(const MSTypeMatrix<unsigned long>&);
template MSTypeMatrix<unsigned long> operator+(const MSTypeMatrix<unsigned long>&,unsigned long);
template MSTypeMatrix<unsigned long> operator+(unsigned long,const MSTypeMatrix<unsigned long>&);
template MSTypeMatrix<unsigned long> operator+(const MSTypeMatrix<unsigned long>&,const MSTypeMatrix<unsigned long>&);
template MSTypeMatrix<unsigned long> operator-(const MSTypeMatrix<unsigned long>&,unsigned long);
template MSTypeMatrix<unsigned long> operator-(const MSTypeMatrix<unsigned long>&,const MSTypeMatrix<unsigned long>&);
template MSTypeMatrix<unsigned long> operator*(const MSTypeMatrix<unsigned long>&,unsigned long);
template MSTypeMatrix<unsigned long> operator*(unsigned long,const MSTypeMatrix<unsigned long>&);
template MSTypeMatrix<unsigned long> operator*(const MSTypeMatrix<unsigned long>&,const MSTypeMatrix<unsigned long>&);
template MSTypeMatrix<unsigned long> operator/(const MSTypeMatrix<unsigned long>&,unsigned long);
template MSTypeMatrix<unsigned long> operator/(const MSTypeMatrix<unsigned long>&,const MSTypeMatrix<unsigned long>&);
template MSTypeMatrix<unsigned long> operator+(const MSTypeMatrix<unsigned long>&,const MSTypeVector<unsigned long>&);
template MSTypeMatrix<unsigned long> operator-(const MSTypeMatrix<unsigned long>&,const MSTypeVector<unsigned long>&);
template MSTypeMatrix<unsigned long> operator*(const MSTypeMatrix<unsigned long>&,const MSTypeVector<unsigned long>&);
template MSTypeMatrix<unsigned long> operator/(const MSTypeMatrix<unsigned long>&,const MSTypeVector<unsigned long>&);
template MSTypeMatrix<unsigned long> operator+(const MSTypeVector<unsigned long>&,const MSTypeMatrix<unsigned long>&);
template MSTypeMatrix<unsigned long> operator*(const MSTypeVector<unsigned long>&,const MSTypeMatrix<unsigned long>&);
template MSTypeMatrix<unsigned long> multiply(const MSTypeMatrix<unsigned long>&,const MSTypeMatrix<unsigned long>&);
template MSTypeMatrix<unsigned long> stack(const MSTypeMatrix<unsigned long>&,const MSTypeMatrix<unsigned long>&);
template MSTypeMatrix<unsigned long> adjoin(const MSTypeMatrix<unsigned long>&,const MSTypeMatrix<unsigned long>&);
#endif // MS_STD_TEMPLATE_INSTANTIATION

#endif // MSTK_MANUAL_INSTANTIATION

