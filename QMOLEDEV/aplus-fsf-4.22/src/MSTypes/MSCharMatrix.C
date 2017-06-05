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

#if HAVE_IOSTREAM
#include <iostream>
#else
#include <iostream.h>
#endif

#if defined(MSTK_MANUAL_INSTANTIATION)
#include <MSTypes/MSTypeMatrix.C>
#include <MSTypes/MSCharVector.H>

#if defined(MS_XLC_TEMPLATE_INSTANTIATION)
#pragma define (MSTypeMatrix<char>)
#endif

#if defined(MS_VC_TEMPLATE_INSTANTIATION)
template MSTypeMatrix<char>;
#endif

#if defined(MS_EDG_TEMPLATE_INSTANTIATION)

#pragma do_not_instantiate MSString MSTypeMatrix<char>::asString(void) const

#pragma instantiate MSTypeMatrix<char>
#pragma instantiate MSMatrixSTypePick<char>
#if !defined(MS_TEMPLATE_MANUAL_FRIEND_BUG)
// The version of EDG used by SGI's dcc compiler has a bug related to access from friends
#pragma instantiate MSTypeMatrix<char> operator-(const MSTypeMatrix<char>&)
#pragma instantiate MSTypeMatrix<char> operator+(const MSTypeMatrix<char>&,char)
#pragma instantiate MSTypeMatrix<char> operator+(char,const MSTypeMatrix<char>&)
#pragma instantiate MSTypeMatrix<char> operator+(const MSTypeMatrix<char>&,const MSTypeMatrix<char>&)
#pragma instantiate MSTypeMatrix<char> operator-(const MSTypeMatrix<char>&,char)
#pragma instantiate MSTypeMatrix<char> operator-(const MSTypeMatrix<char>&,const MSTypeMatrix<char>&)
#pragma instantiate MSTypeMatrix<char> operator*(const MSTypeMatrix<char>&,char)
#pragma instantiate MSTypeMatrix<char> operator*(char,const MSTypeMatrix<char>&)
#pragma instantiate MSTypeMatrix<char> operator*(const MSTypeMatrix<char>&,const MSTypeMatrix<char>&)
#pragma instantiate MSTypeMatrix<char> operator/(const MSTypeMatrix<char>&,char)
#pragma instantiate MSTypeMatrix<char> operator/(const MSTypeMatrix<char>&,const MSTypeMatrix<char>&)
#pragma instantiate MSTypeMatrix<char> operator+(const MSTypeMatrix<char>&,const MSTypeVector<char>&)
#pragma instantiate MSTypeMatrix<char> operator-(const MSTypeMatrix<char>&,const MSTypeVector<char>&)
#pragma instantiate MSTypeMatrix<char> operator*(const MSTypeMatrix<char>&,const MSTypeVector<char>&)
#pragma instantiate MSTypeMatrix<char> operator/(const MSTypeMatrix<char>&,const MSTypeVector<char>&)
#pragma instantiate MSTypeMatrix<char> operator+(const MSTypeVector<char>&,const MSTypeMatrix<char>&)
#pragma instantiate MSTypeMatrix<char> operator*(const MSTypeVector<char>&,const MSTypeMatrix<char>&)
#pragma instantiate MSTypeMatrix<char> multiply(const MSTypeMatrix<char>&,const MSTypeMatrix<char>&)
#pragma instantiate MSTypeMatrix<char> stack(const MSTypeMatrix<char>&,const MSTypeMatrix<char>&)
#pragma instantiate MSTypeMatrix<char> adjoin(const MSTypeMatrix<char>&,const MSTypeMatrix<char>&)
#endif  //!MS_TEMPLATE_MANUAL_FRIEND_BUG
#endif  //MS_EDG_TEMPLATE_INSTANTIATION

#if defined(MS_STD_TEMPLATE_INSTANTIATION)
template class MSTypeMatrix<char>;
template class MSMatrixSTypePick<char>;
template MSTypeMatrix<char> operator-(const MSTypeMatrix<char>&);
template MSTypeMatrix<char> operator+(const MSTypeMatrix<char>&,char);
template MSTypeMatrix<char> operator+(char,const MSTypeMatrix<char>&);
template MSTypeMatrix<char> operator+(const MSTypeMatrix<char>&,const MSTypeMatrix<char>&);
template MSTypeMatrix<char> operator-(const MSTypeMatrix<char>&,char);
template MSTypeMatrix<char> operator-(const MSTypeMatrix<char>&,const MSTypeMatrix<char>&);
template MSTypeMatrix<char> operator*(const MSTypeMatrix<char>&,char);
template MSTypeMatrix<char> operator*(char,const MSTypeMatrix<char>&);
template MSTypeMatrix<char> operator*(const MSTypeMatrix<char>&,const MSTypeMatrix<char>&);
template MSTypeMatrix<char> operator/(const MSTypeMatrix<char>&,char);
template MSTypeMatrix<char> operator/(const MSTypeMatrix<char>&,const MSTypeMatrix<char>&);
template MSTypeMatrix<char> operator+(const MSTypeMatrix<char>&,const MSTypeVector<char>&);
template MSTypeMatrix<char> operator-(const MSTypeMatrix<char>&,const MSTypeVector<char>&);
template MSTypeMatrix<char> operator*(const MSTypeMatrix<char>&,const MSTypeVector<char>&);
template MSTypeMatrix<char> operator/(const MSTypeMatrix<char>&,const MSTypeVector<char>&);
template MSTypeMatrix<char> operator+(const MSTypeVector<char>&,const MSTypeMatrix<char>&);
template MSTypeMatrix<char> operator*(const MSTypeVector<char>&,const MSTypeMatrix<char>&);
template MSTypeMatrix<char> multiply(const MSTypeMatrix<char>&,const MSTypeMatrix<char>&);
template MSTypeMatrix<char> stack(const MSTypeMatrix<char>&,const MSTypeMatrix<char>&);
template MSTypeMatrix<char> adjoin(const MSTypeMatrix<char>&,const MSTypeMatrix<char>&);
#endif  //MS_STD_TEMPLATE_INSTANTIATION

#endif  //MSTK_MANUAL_INSTANTIATION

//----------------------------------------------------------------------
// specializations for MSTypeMatrix<char>
//----------------------------------------------------------------------
// this method cannot be specialized with sunpro because a defintion
// exists and thus it will be automatically instantiated in the template
// database and lead to "multiply defined symbols".
#if !defined(MS_TEMPLATE_FUNCTION_SPECILIZATON_BUG)
MSString MSTypeMatrix<char>::asString(void) const
{
  MSString result;
  result+='(';
  result+=MSString(rows());
  result+=',';
  result+=MSString(columns());
  result+=") ";
  unsigned n=length();
  for (unsigned i=0;i<n;) result<<(int)data()[i++];
  return MSString(result);
}
#endif



ostream& operator<<(ostream& aStream_,const MSTypeMatrix<char>& aTypeMatrix_)
{
  unsigned r=aTypeMatrix_.rows();
  unsigned c=aTypeMatrix_.columns();
  for (unsigned i=0;i<r;i++) 
   {
     for (unsigned j=0;j<c;j++) aStream_<<aTypeMatrix_(i,j);
     aStream_<<endl;
   }
  return aStream_<<flush;
}


