#ifndef MSTypeVectorHEADER
#define MSTypeVectorHEADER

///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////


#include <MSTypes/MSObjectTypeVector.H>

#ifndef MSDefinesHEADER
#include <MSTypes/MSDefines.H>
#endif

#if defined(MS_NEED_IMPLEMENTATION_PRAGMA)
#pragma implementation("MSTypeVector.C")
#endif

template <class Type>
class MSTypeVector : public MSObjectVector<Type>
{
protected:
  INLINELINKAGE static MSString name();  
  
public:
  MSTypeVector ();
  MSTypeVector (unsigned int);
  MSTypeVector (unsigned int, const Type &);
  MSTypeVector (const MSTypeVector<Type> &);
  MSTypeVector (const MSBaseVector<Type,MSVectorModelAllocator<Type> > &);
  MSTypeVector (const char *);
  MSTypeVector (MSTypeData<Type,MSVectorModelAllocator<Type> > *, unsigned int);
  MSTypeVector (const Type *, unsigned int);

  virtual ~MSTypeVector();

  MSTypeVector<Type> & operator= (const MSTypeVector<Type> &);
  MSTypeVector<Type> & operator= (const MSBaseVector<Type,MSVectorModelAllocator<Type> > &);
  MSTypeVector<Type> & operator= (const Type &);
  MSTypeVector<Type> & operator= (const char *);

  virtual MSString className() const;
  virtual const MSSymbol & type() const;
  virtual MSModel *clone() const;
  virtual MSModel *create() const;

  static const MSSymbol & symbol();
};

template <class Type> INLINELINKAGE MSString className (const Type &);

#ifndef MS_NO_INLINES
#include <MSTypes/MSTypeVectorInlines.C>
#endif  // MS_NO_INLINES

#endif // MSTypeVectorHEADER
