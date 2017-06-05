#ifndef MSUnsignedLongVectorHEADER
#define MSUnsignedLongVectorHEADER

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
#pragma implementation("MSUnsignedLongVector.C")
#endif

#include <MSTypes/MSString.H>

#include <MSTypes/MSBuiltinTypeVector.H>

#if defined(MS_TEMPLATE_BEFORE_SPECIALIZATION)
#include <MSTypes/MSTypeVector.H>
#endif


template <class Type> class MSTypeVector;

typedef MSBaseVector<unsigned long, MSAllocator<unsigned long> >    BaseVectorUnsignedLong;
typedef MSBuiltinVector<unsigned long> BuiltinVectorUnsignedLong;

#ifndef MS_MSUnsignedLongVector_INSTANTIATE
#ifdef MS_DECLARE_TEMPLATE_SPECIALIZATION
template <> class MSBaseVector<unsigned long, MSAllocator<unsigned long> >;
template <> class MSBaseVectorOps<unsigned long, MSAllocator<unsigned long> >;
template <> class MSBuiltinVector<unsigned long>;
template <> class MSBuiltinVectorOps<unsigned long>;
template <> class MSBuiltinSPick<unsigned long>;
template <> MSBuiltinVector<unsigned long> operator- (const MSBuiltinVector<unsigned long>&);
template <> ostream& operator<<(ostream&,const MSBaseVector<unsigned long, MSAllocator<unsigned long> >&);
#endif
#endif

#if defined(MS_VC_NEED_TEMPLATE_EXPORT)
class MSTypesExport MSBaseVector<unsigned long, MSAllocator<unsigned long> >;
class MSTypesExport MSBaseVectorOps<unsigned long, MSAllocator<unsigned long> >;
class MSTypesExport MSBuiltinVector<unsigned long>;
class MSTypesExport MSBuiltinVectorOps<unsigned long>;
class MSTypesExport MSBuiltinSPick<unsigned long>;
//template <> MSBuiltinVector<unsigned long> operator- (const MSBuiltinVector<unsigned long>&);
//template <> ostream& operator<<(ostream&,const MSBaseVector<unsigned long, MSAllocator<unsigned long> >&);
#endif

#if !defined(MS_MSUnsignedLongVector_INSTANTIATE) && defined(MS_STD_TEMPLATE_SPECIALIZATION)
template <> class MSTypeVector<unsigned long> : public BuiltinVectorUnsignedLong
#else
template <> class MSTypesExport MSTypeVector<unsigned long> : public BuiltinVectorUnsignedLong
#endif
{
public:
  MSTypeVector ();
  MSTypeVector (unsigned int);
  MSTypeVector (unsigned int, const unsigned long &);
  MSTypeVector (const MSTypeVector<unsigned long> &);
  MSTypeVector (const BuiltinVectorUnsignedLong &);
  MSTypeVector (const BaseVectorUnsignedLong &);
  MSTypeVector (const char *);
  MSTypeVector (MSTypeData<unsigned long, MSAllocator<unsigned long> > *, unsigned int);
  MSTypeVector (const unsigned long *, unsigned int);

  virtual ~MSTypeVector();

  MSTypeVector<unsigned long> & operator= (const MSTypeVector<unsigned long> &);
  MSTypeVector<unsigned long> & operator= (const BuiltinVectorUnsignedLong &);  
  MSTypeVector<unsigned long> & operator= (const BaseVectorUnsignedLong &);
  MSTypeVector<unsigned long> & operator= (const unsigned long &);
  MSTypeVector<unsigned long> & operator= (const char *);

  virtual const MSSymbol & type() const;
  virtual MSString className() const;
  virtual MSModel *clone() const;
  virtual MSModel *create() const;

  static const MSSymbol & symbol();
};

typedef MSTypeVector<unsigned long> MSUnsignedLongVector;

#ifndef MS_NO_INLINES
#include <MSTypes/MSUnsignedLongVectorInlines.C>
#endif  // MS_NO_INLINES

// We provide provide explicit "do_not_instantiate" pragmas here to avoid instantiation complications in
// MStk applications.
//
#if !defined(__MSTYPESLIBRARY_BUILD__) && defined(MS_DO_NOT_INSTANTIATE) && defined(MS_EDG_TEMPLATE_INSTANTIATION)
#pragma do_not_instantiate MSAllocator<unsigned long>
#pragma do_not_instantiate MSTypeData<unsigned long, MSAllocator<unsigned long> >
#pragma do_not_instantiate MSBaseVector<unsigned long, MSAllocator<unsigned long> >
#pragma do_not_instantiate MSBaseVectorOps<unsigned long, MSAllocator<unsigned long> >
#pragma do_not_instantiate MSBuiltinVector<unsigned long>
#pragma do_not_instantiate MSBuiltinVectorOps<unsigned long>
#pragma do_not_instantiate MSBuiltinSPick<unsigned long>
#pragma do_not_instantiate ostream& operator<<(ostream&, const MSBaseVector<unsigned long, MSAllocator<unsigned long> >&)
#pragma do_not_instantiate unsigned int msMergeSortUp(unsigned int,unsigned long *,unsigned int *,unsigned int,unsigned int)
#pragma do_not_instantiate unsigned int msMergeSortDown(unsigned int,unsigned long *,unsigned int *,unsigned int,unsigned int)
#endif // !__MSTYPESLIBRARY_BUILD__ && defined(MS_DO_NOT_INSTANTIATE) && defined(MS_EDG_TEMPLATE_INSTANTIATION)

#endif // MSUnsignedLongVectorHEADER
