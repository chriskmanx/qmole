#ifndef MSUnsignedVectorHEADER
#define MSUnsignedVectorHEADER

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
#pragma implementation("MSUnsignedVector.C")
#endif

#include <MSTypes/MSString.H>

#include <MSTypes/MSBuiltinTypeVector.H>

#if defined(MS_TEMPLATE_BEFORE_SPECIALIZATION)
#include <MSTypes/MSTypeVector.H>
#endif


template<class Type> class MSTypeVector;

typedef MSBaseVector<unsigned int,MSAllocator<unsigned int> >    BaseVectorUnsigned;
typedef MSBuiltinVector<unsigned int> BuiltinVectorUnsigned;

#ifndef MS_MSUnsignedVector_INSTANTIATE
#ifdef MS_DECLARE_TEMPLATE_SPECIALIZATION
template <> class MSBaseVector<unsigned int,MSAllocator<unsigned int> >;
template <> class MSBaseVectorOps<unsigned int,MSAllocator<unsigned int> >;
template <> class MSBuiltinVector<unsigned int>;
template <> class MSBuiltinVectorOps<unsigned int>;
template <> class MSBuiltinSPick<unsigned int>;
template <> MSBuiltinVector<unsigned int> operator- (const MSBuiltinVector<unsigned int>&);
template <> ostream& operator<<(ostream&,const MSBaseVector<unsigned int,MSAllocator<unsigned int> >&);
#endif
#endif

#if defined(MS_VC_NEED_TEMPLATE_EXPORT)
class MSTypesExport MSBaseVector<unsigned int,MSAllocator<unsigned int> >;
class MSTypesExport MSBaseVectorOps<unsigned int,MSAllocator<unsigned int> >;
class MSTypesExport MSBuiltinVector<unsigned int>;
class MSTypesExport MSBuiltinVectorOps<unsigned int>;
class MSTypesExport MSBuiltinSPick<unsigned int>;
#endif

#if !defined(MS_MSUnsignedVector_INSTANTIATE) && defined(MS_STD_TEMPLATE_SPECIALIZATION)
template <> class MSTypeVector<unsigned int> : public BuiltinVectorUnsigned
#else
template <> class MSTypesExport MSTypeVector<unsigned int> : public BuiltinVectorUnsigned
#endif
{
public:
  MSTypeVector ();
  MSTypeVector (unsigned int);
  MSTypeVector (unsigned int, const unsigned int);
  MSTypeVector (const MSTypeVector<unsigned int> &);
  MSTypeVector (const BuiltinVectorUnsigned &);
  MSTypeVector (const BaseVectorUnsigned &);
  MSTypeVector (const char *);
  MSTypeVector (MSTypeData<unsigned int,MSAllocator<unsigned int> > *, unsigned int);
  MSTypeVector (const unsigned int *, unsigned int);

  virtual ~MSTypeVector();

  MSTypeVector<unsigned int> & operator= (const MSTypeVector<unsigned int> &);
  MSTypeVector<unsigned int> & operator= (const BuiltinVectorUnsigned &);  
  MSTypeVector<unsigned int> & operator= (const BaseVectorUnsigned &);
  MSTypeVector<unsigned int> & operator= (const unsigned int &);
  MSTypeVector<unsigned int> & operator= (const char *);

  virtual const MSSymbol & type() const;
  virtual MSString className() const;
  virtual MSModel *clone() const;
  virtual MSModel *create() const;

  static const MSSymbol & symbol();
};

typedef MSTypeVector<unsigned int> MSUnsignedVector;

#ifndef MS_NO_INLINES
#include <MSTypes/MSUnsignedVectorInlines.C>
#endif  // MS_NO_INLINES

// We provide provide explicit "do_not_instantiate" pragmas here to avoid instantiation complications in
// MStk applications.
//
#if !defined(__MSTYPESLIBRARY_BUILD__) && defined(MS_DO_NOT_INSTANTIATE) && defined(MS_EDG_TEMPLATE_INSTANTIATION)
#pragma do_not_instantiate MSAllocator<unsigned int>
#pragma do_not_instantiate MSTypeData<unsigned int, MSAllocator<unsigned int> >
#pragma do_not_instantiate MSBaseVector<unsigned int, MSAllocator<unsigned int> >
#pragma do_not_instantiate MSBaseVectorOps<unsigned int, MSAllocator<unsigned int> >
#pragma do_not_instantiate MSBuiltinVector<unsigned int>
#pragma do_not_instantiate MSBuiltinVectorOps<unsigned int>
#pragma do_not_instantiate MSBuiltinSPick<unsigned int>
#pragma do_not_instantiate ostream& operator<<(ostream&, const MSBaseVector<unsigned int, MSAllocator<unsigned int> >&)
#endif // !__MSTYPESLIBRARY_BUILD__ && defined(MS_DO_NOT_INSTANTIATE) && defined(MS_EDG_TEMPLATE_INSTANTIATION)

#endif // MSUnsignedVectorHEADER
