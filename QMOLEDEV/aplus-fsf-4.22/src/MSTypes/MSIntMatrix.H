#ifndef MSIntMatrixHEADER
#define MSIntMatrixHEADER

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

#include <MSTypes/MSIntVector.H>

#ifndef MSTypeMatrixHEADER
#include <MSTypes/MSTypeMatrix.H>
#endif

typedef MSTypeMatrix<int> MSIntMatrix;

// We provide provide explicit "do_not_instantiate" pragmas here to avoid instantiation complications in
// MStk applications.
//
#if !defined(__MSTYPESLIBRARY_BUILD__) && defined(MS_DO_NOT_INSTANTIATE) && defined(MS_EDG_TEMPLATE_INSTANTIATION)
#pragma do_not_instantiate MSAllocator<int>
#pragma do_not_instantiate MSTypeData<int, MSAllocator<int> >
#pragma do_not_instantiate MSTypeMatrix<int>
#pragma do_not_instantiate MSMatrixSTypePick<int>
#pragma do_not_instantiate ostream& operator<<(ostream&,const MSTypeMatrix<int>&)
#pragma do_not_instantiate MSTypeMatrix<int> operator-(const MSTypeMatrix<int>&)
#pragma do_not_instantiate MSTypeMatrix<int> operator+(const MSTypeMatrix<int>&,int)
#pragma do_not_instantiate MSTypeMatrix<int> operator+(int,const MSTypeMatrix<int>&)
#pragma do_not_instantiate MSTypeMatrix<int> operator+(const MSTypeMatrix<int>&,const MSTypeMatrix<int>&)
#pragma do_not_instantiate MSTypeMatrix<int> operator-(const MSTypeMatrix<int>&,int)
#pragma do_not_instantiate MSTypeMatrix<int> operator-(const MSTypeMatrix<int>&,const MSTypeMatrix<int>&)
#pragma do_not_instantiate MSTypeMatrix<int> operator*(const MSTypeMatrix<int>&,int)
#pragma do_not_instantiate MSTypeMatrix<int> operator*(int,const MSTypeMatrix<int>&)
#pragma do_not_instantiate MSTypeMatrix<int> operator*(const MSTypeMatrix<int>&,const MSTypeMatrix<int>&)
#pragma do_not_instantiate MSTypeMatrix<int> operator/(const MSTypeMatrix<int>&,int)
#pragma do_not_instantiate MSTypeMatrix<int> operator/(const MSTypeMatrix<int>&,const MSTypeMatrix<int>&)
#pragma do_not_instantiate MSTypeMatrix<int> operator+(const MSTypeMatrix<int>&,const MSTypeVector<int>&)
#pragma do_not_instantiate MSTypeMatrix<int> operator-(const MSTypeMatrix<int>&,const MSTypeVector<int>&)
#pragma do_not_instantiate MSTypeMatrix<int> operator*(const MSTypeMatrix<int>&,const MSTypeVector<int>&)
#pragma do_not_instantiate MSTypeMatrix<int> operator/(const MSTypeMatrix<int>&,const MSTypeVector<int>&)
#pragma do_not_instantiate MSTypeMatrix<int> operator+(const MSTypeVector<int>&,const MSTypeMatrix<int>&)
#pragma do_not_instantiate MSTypeMatrix<int> operator*(const MSTypeVector<int>&,const MSTypeMatrix<int>&)
#pragma do_not_instantiate MSTypeMatrix<int> multiply(const MSTypeMatrix<int>&,const MSTypeMatrix<int>&)
#pragma do_not_instantiate MSTypeMatrix<int> stack(const MSTypeMatrix<int>&,const MSTypeMatrix<int>&)
#pragma do_not_instantiate MSTypeMatrix<int> adjoin(const MSTypeMatrix<int>&,const MSTypeMatrix<int>&)
#endif // !__MSTYPESLIBRARY_BUILD__ && defined(MS_DO_NOT_INSTANTIATE) && defined(MS_EDG_TEMPLATE_INSTANTIATION)

#if defined(MS_VC_NEED_TEMPLATE_EXPORT)
class MSTypesExport MSTypeMatrix<int>;
#endif

#endif
