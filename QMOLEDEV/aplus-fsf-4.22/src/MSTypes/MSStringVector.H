#ifndef MSStringVectorHEADER
#define MSStringVectorHEADER

///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////

#include <MSTypes/MSString.H>
#include <MSTypes/MSObjectTypeVector.H>

#ifndef MSDefinesHEADER
#include <MSTypes/MSDefines.H>
#endif

#if defined(MS_NEED_IMPLEMENTATION_PRAGMA)
#pragma implementation("MSStringVector.C")
#endif

#if defined(MS_TEMPLATE_BEFORE_SPECIALIZATION)
#include <MSTypes/MSTypeVector.H>
#endif

template <class Type> class MSTypeVector;

typedef MSBaseVector<MSString,MSVectorModelAllocator<MSString> >   BaseVectorString;
typedef MSObjectVector<MSString> ObjectVectorString;

#ifndef MS_MSStringVector_INSTANTIATE
#ifdef MS_DECLARE_TEMPLATE_SPECIALIZATION
template <> class MSBaseVector<MSString,MSVectorModelAllocator<MSString> >;
template <> class MSBaseVectorOps<MSString,MSVectorModelAllocator<MSString> >;
template <> class MSObjectVector<MSString>;
template <> class MSVectorElement<MSString>;
template <> ostream& operator<<(ostream&,const MSBaseVector<MSString,MSVectorModelAllocator<MSString> >&);
#endif
#endif

#if defined(MS_VC_NEED_TEMPLATE_EXPORT)
class MSTypesExport MSBaseVector<MSString, MSVectorModelAllocator<MSString> >;
class MSTypesExport MSBaseVectorOps<MSString, MSVectorModelAllocator<MSString> >;
class MSTypesExport MSObjectVector<MSString>;
class MSTypesExport MSObjectVector<MSString>;
class MSTypesExport MSVectorElement<MSString>;
#endif

#if !defined(MS_MSStringVector_INSTANTIATE) && defined(MS_STD_TEMPLATE_SPECIALIZATION)
template <> class MSTypeVector<MSString> : public ObjectVectorString
#else
template <> class MSTypesExport MSTypeVector<MSString> : public ObjectVectorString
#endif
{
public:
  MSTypeVector ();
  MSTypeVector (unsigned int);
  MSTypeVector (unsigned int, const MSString &);
  MSTypeVector (const MSTypeVector<MSString> &);
  MSTypeVector (const BaseVectorString &);
  MSTypeVector (const char *, const char ='\n');
  MSTypeVector (MSTypeData<MSString,MSVectorModelAllocator<MSString> > *, unsigned int);
  MSTypeVector (const MSString *, unsigned int);

  virtual ~MSTypeVector();

  MSTypeVector<MSString> & operator= (const MSTypeVector<MSString> &);
  MSTypeVector<MSString> & operator= (const BaseVectorString &);
  MSTypeVector<MSString> & operator= (const MSString &);
  MSTypeVector<MSString> & operator= (const char *);

  virtual       MSString asString () const;
  INLINELINKAGE MSString asString (const char) const;

  virtual const MSSymbol & type() const;
  virtual MSString className() const;
  virtual MSModel *clone() const;
  virtual MSModel *create() const;
  virtual MSError::ErrorStatus set (const char *);
  virtual MSError::ErrorStatus set (const char *, const char);

  static const MSSymbol & symbol();

  INLINELINKAGE MSError::ErrorStatus set (unsigned int, const MSString &);
  
  INLINELINKAGE MSTypeVector<MSString> & appendSingle (const MSString &);
  
                MSTypeVector<MSString> & append (const char *, const char ='\n');
  INLINELINKAGE MSTypeVector<MSString> & append (const MSString &, const char ='\n');
  INLINELINKAGE MSTypeVector<MSString> & append (const MSTypeVector<MSString> &);

  INLINELINKAGE MSTypeVector<MSString> & operator<<= (const MSString &);
  INLINELINKAGE MSTypeVector<MSString> & operator<<= (const char *);
  INLINELINKAGE MSTypeVector<MSString> & operator<<= (const MSTypeVector<MSString> &);
  INLINELINKAGE MSTypeVector<MSString> & operator<<  (const MSString &);
  INLINELINKAGE MSTypeVector<MSString> & operator<<  (const char *);
  INLINELINKAGE MSTypeVector<MSString> & operator<<  (const MSTypeVector<MSString> &);
};

inline MSString msBadData(const MSString&) { return MSString(""); }  

typedef MSTypeVector<MSString> MSStringVector;

#ifndef MS_NO_INLINES
#include <MSTypes/MSStringVectorInlines.C>
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
#pragma do_not_instantiate MSVectorModelAllocator<MSString>
#pragma do_not_instantiate MSTypeData<MSString, MSVectorModelAllocator<MSString> >
#pragma do_not_instantiate MSBaseVector<MSString, MSVectorModelAllocator<MSString> >
#pragma do_not_instantiate MSBaseVectorOps<MSString, MSVectorModelAllocator<MSString> >
#pragma do_not_instantiate MSObjectVector<MSString>
#pragma do_not_instantiate MSVectorElement<MSString>
#pragma do_not_instantiate ostream& operator<<(ostream&, const MSBaseVector<MSString, MSVectorModelAllocator<MSString> >&)
#pragma do_not_instantiate unsigned int msMergeSortUp(unsigned int, MSString *, unsigned int *,unsigned int, unsigned int)
#pragma do_not_instantiate unsigned int msMergeSortDown(unsigned int, MSString *, unsigned int *,unsigned int, unsigned int)
#else
//
// MStk templates are automatically instantiated:  provide "do_not_instantiate" pragmas for template function specializations
//
#pragma do_not_instantiate unsigned MSBaseVectorOps<MSString,MSVectorModelAllocator<MSString> >::numElements(const MSString&,\
													     const char) const
#pragma do_not_instantiate void MSBaseVectorOps<MSString,MSVectorModelAllocator<MSString> >::setFromMSString(void *,\
													     unsigned int,\
													     const MSString&,\
													     unsigned int,\
													     const char) const
#endif // MS_DO_NOT_INSTANTIATE
#endif // MS_EDG_TEMPLATE_INSTANTIATION
#endif // !__MSTYPESLIBRARY_BUILD__

#endif  // MSStringVectorHEADER
