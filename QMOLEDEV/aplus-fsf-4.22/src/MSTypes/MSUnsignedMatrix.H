#ifndef MSUnsignedMatrixHEADER
#define MSUnsignedMatrixHEADER

///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////


#include <MSTypes/MSUnsignedVector.H>

#ifndef MSTypeMatrixHEADER
#include <MSTypes/MSTypeMatrix.H>
#endif

typedef MSTypeMatrix<unsigned> MSUnsignedMatrix;

// We provide provide explicit "do_not_instantiate" pragmas here to avoid instantiation complications in
// MStk applications.
//
#if !defined(__MSTYPESLIBRARY_BUILD__) && defined(MS_DO_NOT_INSTANTIATE) && defined(MS_EDG_TEMPLATE_INSTANTIATION)
#pragma do_not_instantiate MSAllocator<unsigned>
#pragma do_not_instantiate MSTypeData<unsigned, MSAllocator<unsigned> >
#pragma do_not_instantiate MSTypeMatrix<unsigned>
#pragma do_not_instantiate MSMatrixSTypePick<unsigned>
#pragma do_not_instantiate ostream& operator<<(ostream&,const MSTypeMatrix<unsigned>&)
#pragma do_not_instantiate MSTypeMatrix<unsigned> operator-(const MSTypeMatrix<unsigned>&)
#pragma do_not_instantiate MSTypeMatrix<unsigned> operator+(const MSTypeMatrix<unsigned>&,unsigned)
#pragma do_not_instantiate MSTypeMatrix<unsigned> operator+(unsigned,const MSTypeMatrix<unsigned>&)
#pragma do_not_instantiate MSTypeMatrix<unsigned> operator+(const MSTypeMatrix<unsigned>&,const MSTypeMatrix<unsigned>&)
#pragma do_not_instantiate MSTypeMatrix<unsigned> operator-(const MSTypeMatrix<unsigned>&,unsigned)
#pragma do_not_instantiate MSTypeMatrix<unsigned> operator-(const MSTypeMatrix<unsigned>&,const MSTypeMatrix<unsigned>&)
#pragma do_not_instantiate MSTypeMatrix<unsigned> operator*(const MSTypeMatrix<unsigned>&,unsigned)
#pragma do_not_instantiate MSTypeMatrix<unsigned> operator*(unsigned,const MSTypeMatrix<unsigned>&)
#pragma do_not_instantiate MSTypeMatrix<unsigned> operator*(const MSTypeMatrix<unsigned>&,const MSTypeMatrix<unsigned>&)
#pragma do_not_instantiate MSTypeMatrix<unsigned> operator/(const MSTypeMatrix<unsigned>&,unsigned)
#pragma do_not_instantiate MSTypeMatrix<unsigned> operator/(const MSTypeMatrix<unsigned>&,const MSTypeMatrix<unsigned>&)
#pragma do_not_instantiate MSTypeMatrix<unsigned> operator+(const MSTypeMatrix<unsigned>&,const MSTypeVector<unsigned>&)
#pragma do_not_instantiate MSTypeMatrix<unsigned> operator-(const MSTypeMatrix<unsigned>&,const MSTypeVector<unsigned>&)
#pragma do_not_instantiate MSTypeMatrix<unsigned> operator*(const MSTypeMatrix<unsigned>&,const MSTypeVector<unsigned>&)
#pragma do_not_instantiate MSTypeMatrix<unsigned> operator/(const MSTypeMatrix<unsigned>&,const MSTypeVector<unsigned>&)
#pragma do_not_instantiate MSTypeMatrix<unsigned> operator+(const MSTypeVector<unsigned>&,const MSTypeMatrix<unsigned>&)
#pragma do_not_instantiate MSTypeMatrix<unsigned> operator*(const MSTypeVector<unsigned>&,const MSTypeMatrix<unsigned>&)
#pragma do_not_instantiate MSTypeMatrix<unsigned> multiply(const MSTypeMatrix<unsigned>&,const MSTypeMatrix<unsigned>&)
#pragma do_not_instantiate MSTypeMatrix<unsigned> stack(const MSTypeMatrix<unsigned>&,const MSTypeMatrix<unsigned>&)
#pragma do_not_instantiate MSTypeMatrix<unsigned> adjoin(const MSTypeMatrix<unsigned>&,const MSTypeMatrix<unsigned>&)
#endif // !__MSTYPESLIBRARY_BUILD__ && defined(MS_DO_NOT_INSTANTIATE) && defined(MS_EDG_TEMPLATE_INSTANTIATION)

#endif
