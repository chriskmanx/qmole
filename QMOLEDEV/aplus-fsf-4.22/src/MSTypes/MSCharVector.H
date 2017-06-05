#ifndef MSCharVectorHEADER
#define MSCharVectorHEADER

///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////

#include <MSTypes/MSDefines.H>

#if defined(MS_NEED_IMPLEMENTATION_PRAGMA)
#pragma implementation("MSCharVector.C")
#endif

#include <MSTypes/MSString.H>
#include <MSTypes/MSBuiltinTypeVector.H>

#if defined(MS_TEMPLATE_BEFORE_SPECIALIZATION)
#include <MSTypes/MSTypeVector.H>
#endif

template <class Type> class MSTypeVector;

typedef MSBaseVector<char,MSAllocator<char> >    BaseVectorChar;
typedef MSBuiltinVector<char> BuiltinVectorChar;

#ifndef MS_MSCharVector_INSTANTIATE
#ifdef MS_DECLARE_TEMPLATE_SPECIALIZATION
template <> class MSBaseVector<char,MSAllocator<char> >;
template <> class MSBaseVectorOps<char,MSAllocator<char> >;
template <> class MSBuiltinVector<char>;
template <> class MSBuiltinVectorOps<char>;
template <> class MSBuiltinSPick<char>;
template <> MSBuiltinVector<char> operator- (const MSBuiltinVector<char>&);
template <> ostream& operator<<(ostream&,const MSBaseVector<char,MSAllocator<char> >&);
#endif
#endif

#if defined(MS_VC_NEED_TEMPLATE_EXPORT)
class MSTypesExport MSBaseVector<char, MSAllocator<char> >;
class MSTypesExport MSBaseVectorOps<char, MSAllocator<char> >;
class MSTypesExport MSBuiltinVector<char>;
class MSTypesExport MSBuiltinVectorOps<char>;
class MSTypesExport MSBuiltinSPick<char>;
#endif

#if !defined(MS_MSCharVector_INSTANTIATE) && defined(MS_STD_TEMPLATE_SPECIALIZATION)
template <> class MSTypeVector<char> : public BuiltinVectorChar
#else
template <> class MSTypesExport MSTypeVector<char> : public BuiltinVectorChar
#endif
{
public:
  MSTypeVector ();
  MSTypeVector (unsigned int);
  MSTypeVector (unsigned int, const char);
  MSTypeVector (const MSTypeVector<char> &);
  MSTypeVector (const BuiltinVectorChar &);
  MSTypeVector (const BaseVectorChar &);
  MSTypeVector (const char *);
  MSTypeVector (MSTypeData<char,MSAllocator<char> > *, unsigned int);
  MSTypeVector (const char *, unsigned int);

  virtual ~MSTypeVector();

  MSTypeVector<char> & operator= (const MSTypeVector<char> &);
  MSTypeVector<char> & operator= (const BuiltinVectorChar &);  
  MSTypeVector<char> & operator= (const BaseVectorChar &);
  MSTypeVector<char> & operator= (const char);
  MSTypeVector<char> & operator= (const char *);

  virtual const MSSymbol & type() const;
  virtual MSString className() const;
  virtual MSModel *clone() const;
  virtual MSModel *create() const;

  static const MSSymbol & symbol();
};

typedef MSTypeVector<char> MSCharVector;

#ifndef MS_NO_INLINES
#include <MSTypes/MSCharVectorInlines.C>
#endif  // MS_NO_INLINES

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
#pragma do_not_instantiate MSBaseVector<char, MSAllocator<char> >
#pragma do_not_instantiate MSBaseVectorOps<char, MSAllocator<char> >
#pragma do_not_instantiate MSBuiltinVector<char>
#pragma do_not_instantiate MSBuiltinVectorOps<char>
#pragma do_not_instantiate MSBuiltinSPick<char>
#pragma do_not_instantiate ostream& operator<<(ostream&, const MSBaseVector<char, MSAllocator<char> >&)
#pragma do_not_instantiate unsigned int msMergeSortUp(unsigned int, char *, unsigned int *,unsigned int, unsigned int)
#pragma do_not_instantiate unsigned int msMergeSortDown(unsigned int, char *, unsigned int *,unsigned int, unsigned int)
#else
//
// MStk templates are automatically instantiated:  provide "do_not_instantiate" pragmas for template function specializations
//
#pragma do_not_instantiate MSString MSBaseVector<char,MSAllocator<char> >::asString(void) const
#pragma do_not_instantiate void MSBuiltinVectorOps<char>::print(const void *,unsigned int,ostream&) const
#pragma do_not_instantiate void whitespace(const char &,istrstream &)
#endif // MS_DO_NOT_INSTANTIATE
#endif // MS_EDG_TEMPLATE_INSTANTIATION
#endif // !__MSTYPESLIBRARY_BUILD__

#endif  // MSCharVectorHEADER
