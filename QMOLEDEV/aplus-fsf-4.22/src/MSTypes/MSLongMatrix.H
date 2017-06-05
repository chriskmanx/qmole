#ifndef MSLongMatrixHEADER
#define MSLongMatrixHEADER

///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////


#include <MSTypes/MSLongVector.H>

#ifndef MSTypeMatrixHEADER
#include <MSTypes/MSTypeMatrix.H>
#endif

typedef MSTypeMatrix<long> MSLongMatrix;

// We provide provide explicit "do_not_instantiate" pragmas here to avoid instantiation complications in
// MStk applications.
//
#if !defined(__MSTYPESLIBRARY_BUILD__) && defined(MS_DO_NOT_INSTANTIATE) && defined(MS_EDG_TEMPLATE_INSTANTIATION)
#pragma do_not_instantiate MSAllocator<long>
#pragma do_not_instantiate MSTypeData<long, MSAllocator<long> >
#pragma do_not_instantiate MSTypeMatrix<long>
#pragma do_not_instantiate MSMatrixSTypePick<long>
#pragma do_not_instantiate ostream& operator<<(ostream&,const MSTypeMatrix<long>&)
#pragma do_not_instantiate MSTypeMatrix<long> operator-(const MSTypeMatrix<long>&)
#pragma do_not_instantiate MSTypeMatrix<long> operator+(const MSTypeMatrix<long>&,long)
#pragma do_not_instantiate MSTypeMatrix<long> operator+(long,const MSTypeMatrix<long>&)
#pragma do_not_instantiate MSTypeMatrix<long> operator+(const MSTypeMatrix<long>&,const MSTypeMatrix<long>&)
#pragma do_not_instantiate MSTypeMatrix<long> operator-(const MSTypeMatrix<long>&,long)
#pragma do_not_instantiate MSTypeMatrix<long> operator-(const MSTypeMatrix<long>&,const MSTypeMatrix<long>&)
#pragma do_not_instantiate MSTypeMatrix<long> operator*(const MSTypeMatrix<long>&,long)
#pragma do_not_instantiate MSTypeMatrix<long> operator*(long,const MSTypeMatrix<long>&)
#pragma do_not_instantiate MSTypeMatrix<long> operator*(const MSTypeMatrix<long>&,const MSTypeMatrix<long>&)
#pragma do_not_instantiate MSTypeMatrix<long> operator/(const MSTypeMatrix<long>&,long)
#pragma do_not_instantiate MSTypeMatrix<long> operator/(const MSTypeMatrix<long>&,const MSTypeMatrix<long>&)
#pragma do_not_instantiate MSTypeMatrix<long> operator+(const MSTypeMatrix<long>&,const MSTypeVector<long>&)
#pragma do_not_instantiate MSTypeMatrix<long> operator-(const MSTypeMatrix<long>&,const MSTypeVector<long>&)
#pragma do_not_instantiate MSTypeMatrix<long> operator*(const MSTypeMatrix<long>&,const MSTypeVector<long>&)
#pragma do_not_instantiate MSTypeMatrix<long> operator/(const MSTypeMatrix<long>&,const MSTypeVector<long>&)
#pragma do_not_instantiate MSTypeMatrix<long> operator+(const MSTypeVector<long>&,const MSTypeMatrix<long>&)
#pragma do_not_instantiate MSTypeMatrix<long> operator*(const MSTypeVector<long>&,const MSTypeMatrix<long>&)
#pragma do_not_instantiate MSTypeMatrix<long> multiply(const MSTypeMatrix<long>&,const MSTypeMatrix<long>&)
#pragma do_not_instantiate MSTypeMatrix<long> stack(const MSTypeMatrix<long>&,const MSTypeMatrix<long>&)
#pragma do_not_instantiate MSTypeMatrix<long> adjoin(const MSTypeMatrix<long>&,const MSTypeMatrix<long>&)
#endif // !__MSTYPESLIBRARY_BUILD__ && defined(MS_DO_NOT_INSTANTIATE) && defined(MS_EDG_TEMPLATE_INSTANTIATION)

#endif
