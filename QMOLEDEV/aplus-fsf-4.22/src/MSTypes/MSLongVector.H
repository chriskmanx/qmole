#ifndef MSLongVectorHEADER
#define MSLongVectorHEADER

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
#pragma implementation("MSLongVector.C")
#endif

#include <MSTypes/MSString.H>
#include <MSTypes/MSBuiltinTypeVector.H>

#if defined(MS_TEMPLATE_BEFORE_SPECIALIZATION)
#include <MSTypes/MSTypeVector.H>
#endif

template<class Type> class MSTypeVector;

typedef MSBaseVector<long,MSAllocator<long> >    BaseVectorLong;
typedef MSBuiltinVector<long> BuiltinVectorLong;

#ifndef MS_MSLongVector_INSTANTIATE
#ifdef MS_DECLARE_TEMPLATE_SPECIALIZATION
template <> class MSBaseVector<long,MSAllocator<long> >;
template <> class MSBaseVectorOps<long,MSAllocator<long> >;
template <> class MSBuiltinVector<long>;
template <> class MSBuiltinVectorOps<long>;
template <> class MSBuiltinSPick<long>;
template <> MSBuiltinVector<long> operator- (const MSBuiltinVector<long>&);
template <> ostream& operator<<(ostream&,const MSBaseVector<long,MSAllocator<long> >&);
#endif
#endif


#if defined(MS_VC_NEED_TEMPLATE_EXPORT)
class MSTypesExport MSBaseVector<long, MSAllocator<long> >;
class MSTypesExport MSBaseVectorOps<long, MSAllocator<long> >;
class MSTypesExport MSBuiltinVector<long>;
class MSTypesExport MSBuiltinVectorOps<long>;
#endif

#if !defined(MS_MSLongVector_INSTANTIATE) && defined(MS_STD_TEMPLATE_SPECIALIZATION)
template <> class MSTypeVector<long> : public BuiltinVectorLong
#else
template <> class MSTypesExport MSTypeVector<long> : public BuiltinVectorLong
#endif
{
public:
  MSTypeVector ();
  MSTypeVector (unsigned int);
  MSTypeVector (unsigned int, const long &);
  MSTypeVector (const MSTypeVector<long> &);
  MSTypeVector (const BuiltinVectorLong &);
  MSTypeVector (const BaseVectorLong &);
  MSTypeVector (const char *);
  MSTypeVector (MSTypeData<long,MSAllocator<long> > *, unsigned int);
  MSTypeVector (const long *, unsigned int);

  virtual ~MSTypeVector();

  MSTypeVector<long> & operator= (const MSTypeVector<long> &);
  MSTypeVector<long> & operator= (const BuiltinVectorLong &);  
  MSTypeVector<long> & operator= (const BaseVectorLong &);
  MSTypeVector<long> & operator= (const long &);
  MSTypeVector<long> & operator= (const char *);

  virtual const MSSymbol & type() const;
  virtual MSString className() const;
  virtual MSModel *clone() const;
  virtual MSModel *create() const;

  static const MSSymbol & symbol();
};

typedef MSTypeVector<long> MSLongVector;

#ifndef MS_NO_INLINES
#include <MSTypes/MSLongVectorInlines.C>
#endif  // MS_NO_INLINES

// We provide provide explicit "do_not_instantiate" pragmas here to avoid instantiation complications in
// MStk applications.
//
#if !defined(__MSTYPESLIBRARY_BUILD__) && defined(MS_DO_NOT_INSTANTIATE) && defined(MS_EDG_TEMPLATE_INSTANTIATION)
#pragma do_not_instantiate MSAllocator<long>
#pragma do_not_instantiate MSTypeData<long, MSAllocator<long> >
#pragma do_not_instantiate MSBaseVector<long, MSAllocator<long> >
#pragma do_not_instantiate MSBaseVectorOps<long, MSAllocator<long> >
#pragma do_not_instantiate MSBuiltinVector<long>
#pragma do_not_instantiate MSBuiltinVectorOps<long>
#pragma do_not_instantiate MSBuiltinSPick<long>
#pragma do_not_instantiate ostream& operator<<(ostream&, const MSBaseVector<long, MSAllocator<long> >&)
#pragma do_not_instantiate unsigned int msMergeSortUp(unsigned int, long *, unsigned int *,unsigned int, unsigned int)
#pragma do_not_instantiate unsigned int msMergeSortDown(unsigned int, long *, unsigned int *,unsigned int, unsigned int)
#endif // !__MSTYPESLIBRARY_BUILD__ && defined(MS_DO_NOT_INSTANTIATE) && defined(MS_EDG_TEMPLATE_INSTANTIATION)

#endif // MSLongVectorHEADER
