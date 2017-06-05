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

#ifdef MSTK_MANUAL_INSTANTIATION
#include <MSTypes/MSTypeMatrix.C>

#include <MSTypes/MSIntVector.H>
 
#if defined(MS_VC_TEMPLATE_INSTANTIATION)
class MSTypesExport MSTypeMatrix<int>;
template MSTypeMatrix<int>;
#endif

#if defined(MS_XLC_TEMPLATE_INSTANTIATION)
#pragma define (MSTypeMatrix<int>)
#endif

#if defined(MS_EDG_TEMPLATE_INSTANTIATION)
#pragma instantiate MSTypeMatrix<int>
#pragma instantiate MSMatrixSTypePick<int>
#if !defined(MS_TEMPLATE_MANUAL_FRIEND_BUG)
// The version of EDG used by SGI's dcc compiler has a bug related to access from friends
#pragma instantiate ostream& operator<<(ostream&,const MSTypeMatrix<int>&)
#pragma instantiate MSTypeMatrix<int> operator-(const MSTypeMatrix<int>&)
#pragma instantiate MSTypeMatrix<int> operator+(const MSTypeMatrix<int>&,int)
#pragma instantiate MSTypeMatrix<int> operator+(int,const MSTypeMatrix<int>&)
#pragma instantiate MSTypeMatrix<int> operator+(const MSTypeMatrix<int>&,const MSTypeMatrix<int>&)
#pragma instantiate MSTypeMatrix<int> operator-(const MSTypeMatrix<int>&,int)
#pragma instantiate MSTypeMatrix<int> operator-(const MSTypeMatrix<int>&,const MSTypeMatrix<int>&)
#pragma instantiate MSTypeMatrix<int> operator*(const MSTypeMatrix<int>&,int)
#pragma instantiate MSTypeMatrix<int> operator*(int,const MSTypeMatrix<int>&)
#pragma instantiate MSTypeMatrix<int> operator*(const MSTypeMatrix<int>&,const MSTypeMatrix<int>&)
#pragma instantiate MSTypeMatrix<int> operator/(const MSTypeMatrix<int>&,int)
#pragma instantiate MSTypeMatrix<int> operator/(const MSTypeMatrix<int>&,const MSTypeMatrix<int>&)
#pragma instantiate MSTypeMatrix<int> operator+(const MSTypeMatrix<int>&,const MSTypeVector<int>&)
#pragma instantiate MSTypeMatrix<int> operator-(const MSTypeMatrix<int>&,const MSTypeVector<int>&)
#pragma instantiate MSTypeMatrix<int> operator*(const MSTypeMatrix<int>&,const MSTypeVector<int>&)
#pragma instantiate MSTypeMatrix<int> operator/(const MSTypeMatrix<int>&,const MSTypeVector<int>&)
#pragma instantiate MSTypeMatrix<int> operator+(const MSTypeVector<int>&,const MSTypeMatrix<int>&)
#pragma instantiate MSTypeMatrix<int> operator*(const MSTypeVector<int>&,const MSTypeMatrix<int>&)
#pragma instantiate MSTypeMatrix<int> multiply(const MSTypeMatrix<int>&,const MSTypeMatrix<int>&)
#pragma instantiate MSTypeMatrix<int> stack(const MSTypeMatrix<int>&,const MSTypeMatrix<int>&)
#pragma instantiate MSTypeMatrix<int> adjoin(const MSTypeMatrix<int>&,const MSTypeMatrix<int>&)
#endif  //MS_TEMPLATE_MANUAL_FRIEND_BUG
#endif  //MS_EDG_TEMPLATE_INSTANTIATION

#if defined(MS_STD_TEMPLATE_INSTANTIATION)
template class MSTypeMatrix<int>;
template class MSMatrixSTypePick<int>;
template ostream& operator<<(ostream&,const MSTypeMatrix<int>&);
template MSTypeMatrix<int> operator-(const MSTypeMatrix<int>&);
template MSTypeMatrix<int> operator+(const MSTypeMatrix<int>&,int);
template MSTypeMatrix<int> operator+(int,const MSTypeMatrix<int>&);
template MSTypeMatrix<int> operator+(const MSTypeMatrix<int>&,const MSTypeMatrix<int>&);
template MSTypeMatrix<int> operator-(const MSTypeMatrix<int>&,int);
template MSTypeMatrix<int> operator-(const MSTypeMatrix<int>&,const MSTypeMatrix<int>&);
template MSTypeMatrix<int> operator*(const MSTypeMatrix<int>&,int);
template MSTypeMatrix<int> operator*(int,const MSTypeMatrix<int>&);
template MSTypeMatrix<int> operator*(const MSTypeMatrix<int>&,const MSTypeMatrix<int>&);
template MSTypeMatrix<int> operator/(const MSTypeMatrix<int>&,int);
template MSTypeMatrix<int> operator/(const MSTypeMatrix<int>&,const MSTypeMatrix<int>&);
template MSTypeMatrix<int> operator+(const MSTypeMatrix<int>&,const MSTypeVector<int>&);
template MSTypeMatrix<int> operator-(const MSTypeMatrix<int>&,const MSTypeVector<int>&);
template MSTypeMatrix<int> operator*(const MSTypeMatrix<int>&,const MSTypeVector<int>&);
template MSTypeMatrix<int> operator/(const MSTypeMatrix<int>&,const MSTypeVector<int>&);
template MSTypeMatrix<int> operator+(const MSTypeVector<int>&,const MSTypeMatrix<int>&);
template MSTypeMatrix<int> operator*(const MSTypeVector<int>&,const MSTypeMatrix<int>&);
template MSTypeMatrix<int> multiply(const MSTypeMatrix<int>&,const MSTypeMatrix<int>&);
template MSTypeMatrix<int> stack(const MSTypeMatrix<int>&,const MSTypeMatrix<int>&);
template MSTypeMatrix<int> adjoin(const MSTypeMatrix<int>&,const MSTypeMatrix<int>&);
#endif  //MS_STD_TEMPLATE_INSTANTIATION

#endif  //MSTK_MANUAL_INSTANTIATION


