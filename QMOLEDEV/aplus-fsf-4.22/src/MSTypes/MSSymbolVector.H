#ifndef MSSymbolVectorHEADER
#define MSSymbolVectorHEADER

///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////
#include <MSTypes/MSSymbol.H>
#include <MSTypes/MSBaseTypeVector.H>
#include <MSTypes/MSAllocator.H>

#ifndef MSDefinesHEADER
#include <MSTypes/MSDefines.H>
#endif

#if defined(MS_NEED_IMPLEMENTATION_PRAGMA)
#pragma implementation("MSSymbolVector.C")
#endif

#if defined(MS_TEMPLATE_BEFORE_SPECIALIZATION)
#include <MSTypes/MSTypeVector.H>
#endif

#if HAVE_IOSTREAM
#include <iostream>
// #include <fstream>
using namespace std;
#else
class istream;
#endif

template <class Type> class MSTypeVector;

#ifndef MS_MSSymbolVector_INSTANTIATE
#ifdef MS_DECLARE_TEMPLATE_SPECIALIZATION
template <> class MSBaseVector<MSSymbol,MSAllocator<MSSymbol> >;
template <> class MSBaseVectorOps<MSSymbol,MSAllocator<MSSymbol> >;
template <> ostream& operator<<(ostream&,const MSBaseVector<MSSymbol,MSAllocator<MSSymbol> >&);
#endif
#endif

#if defined(MS_VC_NEED_TEMPLATE_EXPORT)
class MSTypesExport MSBaseVector<MSSymbol, MSAllocator<MSSymbol> >;
class MSTypesExport MSBaseVectorOps<MSSymbol, MSAllocator<MSSymbol> >;
#endif

#if !defined(MS_MSSymbolVector_INSTANTIATE) && defined(MS_STD_TEMPLATE_SPECIALIZATION)
template <> class MSTypeVector<MSSymbol> : public MSBaseVector<MSSymbol,MSAllocator<MSSymbol> >
#else
template <> class MSTypesExport MSTypeVector<MSSymbol> : public MSBaseVector<MSSymbol,MSAllocator<MSSymbol> >
#endif
{
public:
  class MSTypesExport SPick
  {
  protected:
    MSTypeVector<MSSymbol> *_pVector;
    unsigned                _index;
    
  public:
    INLINELINKAGE SPick (MSTypeVector<MSSymbol> &, unsigned int);
    INLINELINKAGE SPick (const SPick &);

    SPick & operator= (const MSSymbol&);
    SPick & operator= (const SPick &);

    friend MSTypesExport istream & operator>> (istream &, SPick &);

    INLINELINKAGE operator MSSymbol() const;
  };

public:
  MSTypeVector ();
  MSTypeVector (unsigned int);
  MSTypeVector (unsigned int, const MSSymbol &);
  MSTypeVector (const MSTypeVector<MSSymbol> &);
  MSTypeVector (const MSBaseVector<MSSymbol,MSAllocator<MSSymbol> > &);
  MSTypeVector (const char *);
  MSTypeVector (MSTypeData<MSSymbol,MSAllocator<MSSymbol> > *, unsigned int);
  MSTypeVector (const MSSymbol *, unsigned int);

  virtual ~MSTypeVector();

  MSTypeVector<MSSymbol> & operator= (const MSTypeVector<MSSymbol> &);
  MSTypeVector<MSSymbol> & operator= (const MSBaseVector<MSSymbol,MSAllocator<MSSymbol> > &);
  MSTypeVector<MSSymbol> & operator= (const MSSymbol &);
  MSTypeVector<MSSymbol> & operator= (const char *);

  virtual const MSSymbol & type() const;
  virtual MSString className() const;
  virtual MSModel *clone() const;
  virtual MSModel *create() const;

  static const MSSymbol & symbol();

  INLINELINKAGE const MSSymbol & firstElement() const;
  INLINELINKAGE const MSSymbol & lastElement () const;
  INLINELINKAGE const MSSymbol & operator[] (unsigned int) const;
  INLINELINKAGE const MSSymbol & operator() (unsigned int) const;
  INLINELINKAGE const MSSymbol & elementAt  (unsigned int) const;

  INLINELINKAGE SPick operator[] (unsigned int);

  INLINELINKAGE MSTypeVector<MSSymbol> operator[] (const MSIndexVector &) const;
  INLINELINKAGE MSTypeVector<MSSymbol> operator[] (const MSBinaryVector &) const;
};

typedef MSTypeVector<MSSymbol> MSSymbolVector;

// MSSymbol specializations

inline MSString msAsMSF (const MSSymbol & value_)
{
  return value_.symbolName();
}


inline MSError::ErrorStatus msSetFromString (MSSymbol & value_, const char *pString_)
{
  value_ = MSSymbol(pString_);
  return MSError::MSSuccess;
}


inline MSError::ErrorStatus msSetFromMSF (MSSymbol & sym_, const char *pBuf)
{
  MSString str;
  MSError::ErrorStatus rc = str.setFromMSF (pBuf);
  sym_ = MSSymbol((const char *)str);
  return rc;
}

inline MSSymbol msBadData(const MSSymbol&)
{
  return MSSymbol::nullSymbol();
}

#ifndef MS_NO_INLINES
#include <MSTypes/MSSymbolVectorInlines.C>
#endif  // MS_NO_INLINES

// We provide provide explicit "do_not_instantiate" pragmas here to avoid instantiation complications in
// MStk applications.
//
#if !defined(__MSTYPESLIBRARY_BUILD__) && defined(MS_DO_NOT_INSTANTIATE) && defined(MS_EDG_TEMPLATE_INSTANTIATION)
#pragma do_not_instantiate MSAllocator<MSSymbol>
#pragma do_not_instantiate MSTypeData<MSSymbol, MSAllocator<MSSymbol> >
#pragma do_not_instantiate MSBaseVector<MSSymbol, MSAllocator<MSSymbol> >
#pragma do_not_instantiate MSBaseVectorOps<MSSymbol, MSAllocator<MSSymbol> >
#pragma do_not_instantiate ostream& operator<<(ostream&, const MSBaseVector<MSSymbol, MSAllocator<MSSymbol> >&)
#pragma do_not_instantiate unsigned int msMergeSortUp(unsigned int, MSSymbol *, unsigned int *,unsigned int, unsigned int)
#pragma do_not_instantiate unsigned int msMergeSortDown(unsigned int, MSSymbol *, unsigned int *,unsigned int, unsigned int)
#endif // !__MSTYPESLIBRARY_BUILD__ && defined(MS_DO_NOT_INSTANTIATE) && defined(MS_EDG_TEMPLATE_INSTANTIATION)

#endif  // MSSymbolVectorHEADER
