#ifndef MSObjectTypeVectorHEADER
#define MSObjectTypeVectorHEADER

///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////


#ifndef MSBaseTypeVectorHEADER
#include <MSTypes/MSBaseTypeVector.H>
#endif //MSBaseTypeVectorHEADER

#ifndef MSAllocatorHEADER
#include <MSTypes/MSAllocator.H>
#endif //MSAllocatorHEADER

#ifndef MSDefinesHEADER
#include <MSTypes/MSDefines.H>
#endif //MSDefinesHEADER

#if defined(MS_NEED_IMPLEMENTATION_PRAGMA)
#pragma implementation("MSObjectTypeVector.C")
#endif

template <class Type> class MSVectorElement;

template <class Type>
class MSVectorModelAllocator : public MSAllocator<Type>
{
public:
  INLINELINKAGE MSVectorModelAllocator(void);
  INLINELINKAGE ~MSVectorModelAllocator(void);

  INLINELINKAGE void construct(Type *p_, const Type& value_);
};


template <class Type>
class MSObjectVector : public MSBaseVector<Type,MSVectorModelAllocator<Type> >
{
friend class MSVectorElement<Type>;

public:
  MSObjectVector ();
  MSObjectVector (unsigned int);
  MSObjectVector (unsigned int, const Type &);
  MSObjectVector (const MSObjectVector<Type> &);
  MSObjectVector (const MSBaseVector<Type,MSVectorModelAllocator<Type> > &);
  MSObjectVector (const char *, const char =' ');
  MSObjectVector (MSTypeData<Type,MSVectorModelAllocator<Type> > *, unsigned int);
  MSObjectVector (const Type *, unsigned int);

  virtual ~MSObjectVector();

  MSObjectVector<Type> & operator= (const MSObjectVector<Type> &);
  MSObjectVector<Type> & operator= (const MSBaseVector<Type,MSVectorModelAllocator<Type> > &);
  MSObjectVector<Type> & operator= (const Type &);
  MSObjectVector<Type> & operator= (const char *);

  INLINELINKAGE       Type & firstElement();
  INLINELINKAGE       Type & lastElement ();
  INLINELINKAGE const Type & firstElement() const;
  INLINELINKAGE const Type & lastElement () const;

  INLINELINKAGE const Type & operator[] (unsigned int) const;
  INLINELINKAGE const Type & operator() (unsigned int) const;
  INLINELINKAGE const Type & elementAt  (unsigned int) const;

  INLINELINKAGE Type & operator[] (unsigned int);
  Type & elementAt  (unsigned int);

  INLINELINKAGE MSObjectVector<Type> operator[] (const MSIndexVector &) const;
  INLINELINKAGE MSObjectVector<Type> operator[] (const MSBinaryVector &) const;
};

template <class Type>
class MSVectorElement : public Type
{
protected:
  virtual void sendEvent (MSEvent &);
    
public:
  INLINELINKAGE MSVectorElement ();
  INLINELINKAGE MSVectorElement (const MSVectorElement<Type> &);
  INLINELINKAGE MSVectorElement (const Type &);
  ~MSVectorElement();

  INLINELINKAGE void vector (MSObjectVector<Type> *);
};
    
#ifndef MS_NO_INLINES
#include <MSTypes/MSObjectTypeVectorInlines.C>
#endif  // MS_NO_INLINES

#endif // MSObjectTypeVectorHEADER
