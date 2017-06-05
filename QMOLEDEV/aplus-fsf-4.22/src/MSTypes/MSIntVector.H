#ifndef MSIntVectorHEADER
#define MSIntVectorHEADER

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

#if defined(MS_NEED_IMPLEMENTATION_PRAGMA)
#pragma implementation("MSIntVector.C")
#endif

#include <MSTypes/MSString.H>
#include <MSTypes/MSMergeSort.H>
#include <MSTypes/MSBuiltinTypeVector.H>

#if defined(MS_TEMPLATE_BEFORE_SPECIALIZATION)
#include <MSTypes/MSTypeVector.H>
#endif

template <class Type> class MSTypeVector;

typedef MSBaseVector<int, MSAllocator<int> >    BaseVectorInt;
typedef MSBuiltinVector<int> BuiltinVectorInt;

#ifndef MS_MSIntVector_INSTANTIATE
#ifdef  MS_DECLARE_TEMPLATE_SPECIALIZATION
template <> class MSBaseVector<int, MSAllocator<int> >;
template <> class MSBaseVectorOps<int, MSAllocator<int> >;
template <> class MSBuiltinVector<int>;
template <> class MSBuiltinVectorOps<int>;
template <> class MSBuiltinSPick<int>;
template <> MSBuiltinVector<int> operator- (const MSBuiltinVector<int>&);
template <> ostream& operator<<(ostream&,const MSBaseVector<int, MSAllocator<int> >&);
#endif
#endif

#if defined(MS_VC_NEED_TEMPLATE_EXPORT)
class MSTypesExport MSBaseVector<int, MSAllocator<int> >;
class MSTypesExport MSBaseVectorOps<int, MSAllocator<int> >;
class MSTypesExport MSBuiltinVector<int>;
class MSTypesExport MSBuiltinVectorOps<int>;
class MSTypesExport MSBuiltinSPick<int>;
//template <> MSBuiltinVector<int> operator- (const MSBuiltinVector<int>&);
//template <> ostream& operator<<(ostream&,const MSBaseVector<int, MSAllocator<int> >&);
#endif

#if !defined(MS_MSIntVector_INSTANTIATE) && defined(MS_STD_TEMPLATE_SPECIALIZATION)
template <> class MSTypeVector<int> : public BuiltinVectorInt
#else
template <> class MSTypesExport MSTypeVector<int> : public BuiltinVectorInt
#endif
{
public:
  MSTypeVector ();
  MSTypeVector (unsigned int);
  MSTypeVector (unsigned int, const int);
  MSTypeVector (const MSTypeVector<int> &);
  MSTypeVector (const BuiltinVectorInt &);
  MSTypeVector (const BaseVectorInt &);
  MSTypeVector (const char *);
  MSTypeVector (MSTypeData<int, MSAllocator<int> > *, unsigned int);
  MSTypeVector (const int *, unsigned int);

  virtual ~MSTypeVector();

  MSTypeVector<int> & operator= (const MSTypeVector<int> &);
  MSTypeVector<int> & operator= (const BuiltinVectorInt &);  
  MSTypeVector<int> & operator= (const BaseVectorInt &);
  MSTypeVector<int> & operator= (const int &);
  MSTypeVector<int> & operator= (const char *);

  virtual const MSSymbol & type() const;
  virtual MSString className() const;
  virtual MSModel *clone() const;
  virtual MSModel *create() const;

  static const MSSymbol & symbol();
};

typedef MSTypeVector<int> MSIntVector;

#ifndef MS_NO_INLINES
#include <MSTypes/MSIntVectorInlines.C>
#endif  // MS_NO_INLINES

// We provide provide explicit "do_not_instantiate" pragmas here to avoid instantiation complications in
// MStk applications.
//
#if !defined(__MSTYPESLIBRARY_BUILD__) && defined(MS_DO_NOT_INSTANTIATE) && defined(MS_EDG_TEMPLATE_INSTANTIATION)
#pragma do_not_instantiate MSAllocator<int>
#pragma do_not_instantiate MSTypeData<int, MSAllocator<int> >
#pragma do_not_instantiate MSBaseVector<int, MSAllocator<int> >
#pragma do_not_instantiate MSBaseVectorOps<int, MSAllocator<int> >
#pragma do_not_instantiate MSBuiltinVector<int>
#pragma do_not_instantiate MSBuiltinVectorOps<int>
#pragma do_not_instantiate MSBuiltinSPick<int>
#pragma do_not_instantiate ostream& operator<<(ostream&, const MSBaseVector<int, MSAllocator<int> >&)
#pragma do_not_instantiate unsigned int msMergeSortUp(unsigned int, int *, unsigned int *,unsigned int, unsigned int)
#pragma do_not_instantiate unsigned int msMergeSortDown(unsigned int, int *, unsigned int *,unsigned int, unsigned int)
#endif // !__MSTYPESLIBRARY_BUILD__ && defined(MS_DO_NOT_INSTANTIATE) && defined(MS_EDG_TEMPLATE_INSTANTIATION)

#endif // MSIntVectorHEADER
