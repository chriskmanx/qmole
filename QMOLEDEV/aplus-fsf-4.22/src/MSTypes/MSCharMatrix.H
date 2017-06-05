#ifndef MSCharMatrixHEADER
#define MSCharMatrixHEADER

///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////


#include <MSTypes/MSCharVector.H>

#ifndef MSTypeMatrixHEADER
#include <MSTypes/MSTypeMatrix.H>
#endif

typedef MSTypeMatrix<char> MSCharMatrix;

// We provide explicit "do_not_instantiate" pragmas here to avoid instantiation complications in
// MStk applications.
//
#if !defined(__MSTYPESLIBRARY_BUILD__)
#if defined(MS_EDG_TEMPLATE_INSTANTIATION)
#if defined(MS_DO_NOT_INSTANTIATE) 
//
// MStk templates are manually instantiated:  provide "do_not_instantiate" pragmas for the manually instantiated entities
//
#pragma do_not_instantiate MSAllocator<char>
#pragma do_not_instantiate MSTypeData<char, MSAllocator<char> >
#pragma do_not_instantiate MSTypeMatrix<char>
#pragma do_not_instantiate MSMatrixSTypePick<char>
#pragma do_not_instantiate MSTypeMatrix<char> operator-(const MSTypeMatrix<char>&)
#pragma do_not_instantiate MSTypeMatrix<char> operator+(const MSTypeMatrix<char>&,char)
#pragma do_not_instantiate MSTypeMatrix<char> operator+(char,const MSTypeMatrix<char>&)
#pragma do_not_instantiate MSTypeMatrix<char> operator+(const MSTypeMatrix<char>&,const MSTypeMatrix<char>&)
#pragma do_not_instantiate MSTypeMatrix<char> operator-(const MSTypeMatrix<char>&,char)
#pragma do_not_instantiate MSTypeMatrix<char> operator-(const MSTypeMatrix<char>&,const MSTypeMatrix<char>&)
#pragma do_not_instantiate MSTypeMatrix<char> operator*(const MSTypeMatrix<char>&,char)
#pragma do_not_instantiate MSTypeMatrix<char> operator*(char,const MSTypeMatrix<char>&)
#pragma do_not_instantiate MSTypeMatrix<char> operator*(const MSTypeMatrix<char>&,const MSTypeMatrix<char>&)
#pragma do_not_instantiate MSTypeMatrix<char> operator/(const MSTypeMatrix<char>&,char)
#pragma do_not_instantiate MSTypeMatrix<char> operator/(const MSTypeMatrix<char>&,const MSTypeMatrix<char>&)
#pragma do_not_instantiate MSTypeMatrix<char> operator+(const MSTypeMatrix<char>&,const MSTypeVector<char>&)
#pragma do_not_instantiate MSTypeMatrix<char> operator-(const MSTypeMatrix<char>&,const MSTypeVector<char>&)
#pragma do_not_instantiate MSTypeMatrix<char> operator*(const MSTypeMatrix<char>&,const MSTypeVector<char>&)
#pragma do_not_instantiate MSTypeMatrix<char> operator/(const MSTypeMatrix<char>&,const MSTypeVector<char>&)
#pragma do_not_instantiate MSTypeMatrix<char> operator+(const MSTypeVector<char>&,const MSTypeMatrix<char>&)
#pragma do_not_instantiate MSTypeMatrix<char> operator*(const MSTypeVector<char>&,const MSTypeMatrix<char>&)
#pragma do_not_instantiate MSTypeMatrix<char> multiply(const MSTypeMatrix<char>&,const MSTypeMatrix<char>&)
#pragma do_not_instantiate MSTypeMatrix<char> stack(const MSTypeMatrix<char>&,const MSTypeMatrix<char>&)
#pragma do_not_instantiate MSTypeMatrix<char> adjoin(const MSTypeMatrix<char>&,const MSTypeMatrix<char>&)
#else
//
// MStk templates are automatically instantiated:  provide "do_not_instantiate" pragmas for template function specializations
//
#pragma do_not_instantiate MSString MSTypeMatrix<char>::asString(void) const
#pragma do_not_instantiate ostream& operator<<(ostream&,const MSTypeMatrix<char>&)
#endif // MS_DO_NOT_INSTANTIATE
#endif // MS_EDG_TEMPLATE_INSTANTIATION
#endif // !__MSTYPESLIBRARY_BUILD__

#endif
