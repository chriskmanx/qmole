#ifndef MSUnsignedLongMatrixHEADER
#define MSUnsignedLongMatrixHEADER

///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////


#include <MSTypes/MSUnsignedLongVector.H>

#ifndef MSTypeMatrixHEADER
#include <MSTypes/MSTypeMatrix.H>
#endif

typedef MSTypeMatrix<unsigned long> MSUnsignedLongMatrix;

// We provide provide explicit "do_not_instantiate" pragmas here to avoid instantiation complications in
// MStk applications.
//
#if !defined(__MSTYPESLIBRARY_BUILD__) && defined(MS_DO_NOT_INSTANTIATE) && defined(MS_EDG_TEMPLATE_INSTANTIATION)
#pragma do_not_instantiate MSAllocator<unsigned long>
#pragma do_not_instantiate MSTypeData<unsigned long, MSAllocator<unsigned long> >
#pragma do_not_instantiate MSTypeMatrix<unsigned long>
#pragma do_not_instantiate MSMatrixSTypePick<unsigned long>
#pragma do_not_instantiate ostream& operator<<(ostream&,const MSTypeMatrix<unsigned long>&)
#pragma do_not_instantiate MSTypeMatrix<unsigned long> operator-(const MSTypeMatrix<unsigned long>&)
#pragma do_not_instantiate MSTypeMatrix<unsigned long> operator+(const MSTypeMatrix<unsigned long>&,unsigned long)
#pragma do_not_instantiate MSTypeMatrix<unsigned long> operator+(unsigned long,const MSTypeMatrix<unsigned long>&)
#pragma do_not_instantiate MSTypeMatrix<unsigned long> operator+(const MSTypeMatrix<unsigned long>&,\
								 const MSTypeMatrix<unsigned long>&)
#pragma do_not_instantiate MSTypeMatrix<unsigned long> operator-(const MSTypeMatrix<unsigned long>&,unsigned long)
#pragma do_not_instantiate MSTypeMatrix<unsigned long> operator-(const MSTypeMatrix<unsigned long>&,\
								 const MSTypeMatrix<unsigned long>&)
#pragma do_not_instantiate MSTypeMatrix<unsigned long> operator*(const MSTypeMatrix<unsigned long>&,unsigned long)
#pragma do_not_instantiate MSTypeMatrix<unsigned long> operator*(unsigned long,const MSTypeMatrix<unsigned long>&)
#pragma do_not_instantiate MSTypeMatrix<unsigned long> operator*(const MSTypeMatrix<unsigned long>&,\
								 const MSTypeMatrix<unsigned long>&)
#pragma do_not_instantiate MSTypeMatrix<unsigned long> operator/(const MSTypeMatrix<unsigned long>&,unsigned long)
#pragma do_not_instantiate MSTypeMatrix<unsigned long> operator/(const MSTypeMatrix<unsigned long>&,\
								 const MSTypeMatrix<unsigned long>&)
#pragma do_not_instantiate MSTypeMatrix<unsigned long> operator+(const MSTypeMatrix<unsigned long>&,\
								 const MSTypeVector<unsigned long>&)
#pragma do_not_instantiate MSTypeMatrix<unsigned long> operator-(const MSTypeMatrix<unsigned long>&,\
								 const MSTypeVector<unsigned long>&)
#pragma do_not_instantiate MSTypeMatrix<unsigned long> operator*(const MSTypeMatrix<unsigned long>&,\
								 const MSTypeVector<unsigned long>&)
#pragma do_not_instantiate MSTypeMatrix<unsigned long> operator/(const MSTypeMatrix<unsigned long>&,\
								 const MSTypeVector<unsigned long>&)
#pragma do_not_instantiate MSTypeMatrix<unsigned long> operator+(const MSTypeVector<unsigned long>&,\
								 const MSTypeMatrix<unsigned long>&)
#pragma do_not_instantiate MSTypeMatrix<unsigned long> operator*(const MSTypeVector<unsigned long>&,\
								 const MSTypeMatrix<unsigned long>&)
#pragma do_not_instantiate MSTypeMatrix<unsigned long> multiply(const MSTypeMatrix<unsigned long>&,\
								const MSTypeMatrix<unsigned long>&)
#pragma do_not_instantiate MSTypeMatrix<unsigned long> stack(const MSTypeMatrix<unsigned long>&,\
							     const MSTypeMatrix<unsigned long>&)
#pragma do_not_instantiate MSTypeMatrix<unsigned long> adjoin(const MSTypeMatrix<unsigned long>&,\
							      const MSTypeMatrix<unsigned long>&)
#endif // !__MSTYPESLIBRARY_BUILD__ && defined(MS_DO_NOT_INSTANTIATE) && defined(MS_EDG_TEMPLATE_INSTANTIATION)

#endif
