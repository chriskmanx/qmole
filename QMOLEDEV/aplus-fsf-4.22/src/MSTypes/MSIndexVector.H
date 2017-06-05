#ifndef MSIndexVectorHEADER
#define MSIndexVectorHEADER

///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////


#if HAVE_NEW
#include <new>
#else
#include <new.h>
#endif

#include <MSTypes/MSError.H>
#include <MSTypes/MSString.H>
#include <MSTypes/MSSymbol.H>
#include <MSTypes/MSVector.H>
#include <MSTypes/MSEvent.H>
#include <MSTypes/MSEventReceiver.H>
#include <MSTypes/MSEnum.H>

#include <MSTypes/MSData.H>
#include <MSTypes/MSBuiltinVectorImpl.H>
#include <MSTypes/MSMergeSort.H>

#ifndef MSDefinesHEADER
#include <MSTypes/MSDefines.H>
#endif

#if HAVE_IOSTREAM
#include <iostream>
#include <fstream>
using namespace std;
#else
class ostream;
#endif

#if HAVE_SSTREAM
#include <sstream>
#include <string>
#else
class istrstream;
class ostrstream;
#endif

class MSBinaryVector;

class MSTypesExport MSIndexVector : public MSVector
{
 public:
   class Data;
#ifdef MS_NESTED_CLASSES_ACCESS_BUG
public:
#else  
protected:
#endif
  class MSTypesExport Operations : public MSVectorImplOps, public MSBuiltinVectorImplOps
  {
  protected:
#ifdef MS_MULTI_THREAD
    static MSMutex _nullDataMutex;
#endif
    static Data& nullData(void);

  public:
    Operations();
    virtual ~Operations();
    
    virtual void * allocate         (unsigned int, unsigned int =0, MSAllocationFlag =MSRaw) const;
    virtual void * allocateWithSize (unsigned int, unsigned int =0, MSAllocationFlag =MSRaw) const;
    virtual void   deallocate       (void *, unsigned int =0, MSAllocationFlag =MSRaw) const ;

    virtual void incrementCount   (void *) const;
    virtual unsigned int refCount (const void *) const;

    virtual void set  (void *, unsigned int, const void *, MSAllocationFlag =MSConstructed) const;
    virtual void set  (void *, unsigned int, const void *, unsigned int, MSAllocationFlag =MSConstructed) const;
    virtual void fill (void *, unsigned int, unsigned int, const void *, MSAllocationFlag =MSConstructed) const;
    virtual void copy (const void *, void *, unsigned int, unsigned int =0, unsigned int =0,
		       MSAllocationFlag =MSConstructed) const;
    virtual void copyBackward (void *, unsigned int, unsigned int, unsigned int) const;
    virtual void destroy (void *, unsigned int, unsigned int) const;

    virtual int  isElementEqual     (const void *, unsigned int, const void *) const;
    virtual int  isElementLess      (const void *, unsigned int, const void *) const;
    virtual int  isElementLessEqual (const void *, unsigned int, const void *) const;
    virtual long compareElement     (const void *, unsigned int, const void *) const;

    virtual void *elementAt (const void *, unsigned int) const;
    virtual unsigned int size (const void *) const;
    virtual void swapElements (void *, unsigned int, unsigned int) const;

    virtual unsigned int gradeUp   (const void *, unsigned int, unsigned int *) const;
    virtual unsigned int gradeDown (const void *, unsigned int, unsigned int *) const;

    virtual void *badData() const;
    virtual void *defaultFiller() const;

    virtual MSString asString (const void *, unsigned int) const;
    virtual MSString asMSF    (const void *, unsigned int) const;

    virtual unsigned int elementLen (const void *, unsigned int) const;

    virtual MSError::ErrorStatus setFromString (void *, unsigned int, const char *) const;
    virtual MSError::ErrorStatus setFromMSF (void *, unsigned int, const char *) const;

    virtual void print (const void *, unsigned int, ostream &) const;

    virtual unsigned int numElements (const MSString &, const char) const;
    virtual void setFromMSString (void *, unsigned int, const MSString &, unsigned int&, const char) const;

    virtual void   setToNumber (void *, unsigned int, double) const;
    virtual double getAsNumber (const void *, unsigned int) const;

#if HAVE_SSTREAM
    virtual void readFromStream (void *, unsigned int, istringstream &) const;
    virtual void writeToStream  (const void *, unsigned int, ostringstream &) const;

    virtual void whitespace (istringstream &) const;
    virtual unsigned int stringLen (const char *) const;
#else
    virtual void readFromStream (void *, unsigned int, istrstream &) const;
    virtual void writeToStream  (const void *, unsigned int, ostrstream &) const;

    virtual void whitespace (istrstream &) const;
    virtual unsigned int stringLen (const char *) const;
#endif
  };

#ifdef MS_NESTED_CLASSES_ACCESS_BUG
public:
#endif
  class MSTypesExport SPick
  {
  protected:
    unsigned int   _index; 
    MSIndexVector *_pVector;

  public:
    INLINELINKAGE SPick (MSIndexVector &, unsigned int);
    INLINELINKAGE SPick (const SPick &);

    SPick & operator= (const SPick &);
    SPick & operator= (const unsigned int);

    INLINELINKAGE SPick & operator++();        // prefix increment
    INLINELINKAGE SPick & operator--();        // prefix decrement
           unsigned int   operator++ (int);	// postfix increment
           unsigned int   operator-- (int);	// postfix decrement
  
    INLINELINKAGE SPick & operator+=  (const unsigned int);
    INLINELINKAGE SPick & operator-=  (const unsigned int);
    INLINELINKAGE SPick & operator*=  (const unsigned int);
    INLINELINKAGE SPick & operator/=  (const unsigned int);
    INLINELINKAGE SPick & operator%=  (const unsigned int);
    INLINELINKAGE SPick & operator<<= (const unsigned int);
    INLINELINKAGE SPick & operator>>= (const unsigned int);
    INLINELINKAGE SPick & operator&=  (const unsigned int);
    INLINELINKAGE SPick & operator|=  (const unsigned int);
    INLINELINKAGE SPick & operator^=  (const unsigned int);
    
    INLINELINKAGE operator unsigned int() const;
  };

public:
  class MSTypesExport Data : public MSData
  {
  public:
    Data (unsigned int =0);
    ~Data();
  
    void *operator new (size_t, unsigned =0);

    MSString asDebugInfo() const;

    INLINELINKAGE Data * incrementCount();
    INLINELINKAGE void decrementCount();
  
    INLINELINKAGE const unsigned int * elements() const;
    INLINELINKAGE const unsigned int * data() const;   // same as elements(); used for backward compatibility only

    INLINELINKAGE unsigned int * elements();
    INLINELINKAGE unsigned int * data();	// same as elements(); used for backward compatibility only
  
    INLINELINKAGE const unsigned int & elementAt (unsigned index_) const;
    INLINELINKAGE       unsigned int & elementAt (unsigned index_);

    static void fill (unsigned int *, unsigned int, const unsigned int);
    static void copy (const unsigned int *src_, unsigned int *dst_, unsigned int length_);
    static void copyBackward (const unsigned int *, unsigned int *, unsigned int);
    
    static Data *allocateWithSize   (unsigned int);
    static Data *allocateWithLength (unsigned int);
    void deallocate();

  protected:
    unsigned int _pElements[1];
  };
  
protected:
  MSBuiltinVectorImpl *_pImpl;
#ifdef MS_MULTI_THREAD
  static MSMutex _operationsMutex;
#endif

  MSIndexVector (MSBuiltinVectorImpl *);
  virtual unsigned int getLength() const;
  INLINELINKAGE Data *vectorData() const;
  static Operations& ops(void);

public:
  MSIndexVector ();
  MSIndexVector (const MSIndexVector &);
  MSIndexVector (unsigned int);
  MSIndexVector (unsigned int, const unsigned int);
  MSIndexVector (const char *);
  MSIndexVector (Data *, unsigned int);
  MSIndexVector (const unsigned int *, unsigned int);

  virtual ~MSIndexVector();

  MSIndexVector & operator= (const MSIndexVector &);
  MSIndexVector & operator= (const unsigned int);

  virtual MSString asString() const;
  virtual MSString asMSF() const;
  virtual MSString asDebugInfo() const;
  virtual MSString className() const;
  virtual const MSSymbol & type() const;
  virtual MSModel *clone() const;
  virtual MSModel *create() const;

  virtual void assign  (const MSModel &);
  virtual long compare (const MSModel &) const;
  virtual MSError::ErrorStatus set (const char *);
  virtual MSError::ErrorStatus setFromMSF (const char *);

  static const MSSymbol & symbol();

  MSError::ErrorStatus set (unsigned int, const unsigned int);
  
  INLINELINKAGE unsigned int   size()   const;

  unsigned int   length() const;
  unsigned int * data() const;

  static const MSIndexVector & nullVector();  

  INLINELINKAGE unsigned int indexOf     (const unsigned int, unsigned int =0) const;
  INLINELINKAGE unsigned int lastIndexOf (const unsigned int, unsigned int =UINT_MAX-1) const;

  INLINELINKAGE unsigned int occurrencesOf (const unsigned int, unsigned int =0) const;

  INLINELINKAGE MSIndexVector  indicesOf (const MSIndexVector &) const;
  INLINELINKAGE MSBinaryVector memberOf (const MSIndexVector &) const;

  INLINELINKAGE MSBinaryVector uniqueElements() const;

  INLINELINKAGE MSIndexVector & replaceAt (unsigned int, const unsigned int);
   
  MSIndexVector & append    (const unsigned int);
  MSIndexVector & append    (const MSIndexVector &);
  MSIndexVector & insertAt  (unsigned int, const unsigned int);
  MSIndexVector & insertAt  (unsigned int, const MSIndexVector &);
  MSIndexVector & removeAt  (unsigned int);
  MSIndexVector & removeAt  (unsigned int, unsigned int);
  MSIndexVector & remove    (const MSIndexVector &);
  MSIndexVector & remove    (const MSBinaryVector &);
  MSIndexVector & removeAll ();

  MSIndexVector & select    (const MSIndexVector &);
  MSIndexVector & compress  (const MSBinaryVector &); 

  INLINELINKAGE MSIndexVector & selectUnique();

  MSIndexVector & reshape  (unsigned int);
  MSIndexVector & exchange (unsigned int, unsigned int);
  MSIndexVector & reverse  ();
  MSIndexVector & rotate   (int);
  MSIndexVector & take     (int, const unsigned int =0);
  MSIndexVector & drop     (int);

  static MSIndexVector reverse (const MSIndexVector &);
  static MSIndexVector rotate  (const MSIndexVector &, int);
  static MSIndexVector take    (const MSIndexVector &, int, const unsigned int =0);
  static MSIndexVector drop    (const MSIndexVector &, int);
  static MSIndexVector select  (const MSIndexVector &, const MSIndexVector &);
  static MSIndexVector compress(const MSIndexVector &, const MSBinaryVector &);

  INLINELINKAGE static MSIndexVector selectUnique (const MSIndexVector &);

  MSIndexVector & selectiveAssign (const MSIndexVector  &, const unsigned int);
  MSIndexVector & selectiveAssign (const MSIndexVector  &, const MSIndexVector &);
  MSIndexVector & selectiveAssign (const MSBinaryVector &, const unsigned int);
  MSIndexVector & selectiveAssign (const MSBinaryVector &, const MSIndexVector &);

  virtual MSIndexVector gradeUp() const;
  virtual MSIndexVector gradeDown() const;
  virtual void permute (const MSIndexVector &);
  INLINELINKAGE MSIndexVector & sortUp();
  INLINELINKAGE MSIndexVector & sortDown();

  INLINELINKAGE unsigned int firstElement() const;
  INLINELINKAGE unsigned int lastElement () const;
  INLINELINKAGE unsigned int operator[] (unsigned int) const;
  INLINELINKAGE unsigned int operator() (unsigned int) const;
  INLINELINKAGE const unsigned int & elementAt  (unsigned int) const;

  INLINELINKAGE SPick operator[] (unsigned int);

  INLINELINKAGE MSIndexVector operator[] (const MSIndexVector &) const;
  INLINELINKAGE MSIndexVector operator[] (const MSBinaryVector &) const;

  friend MSTypesExport ostream & operator<< (ostream &, const MSIndexVector &);

  INLINELINKAGE long compare (const MSIndexVector &) const;
  INLINELINKAGE MSBinaryVector binaryCompare (const MSIndexVector &, MSComparison) const;
  INLINELINKAGE MSBinaryVector binaryCompare (const unsigned int, MSComparison) const;

  INLINELINKAGE MSBoolean operator== (const MSIndexVector &) const;
  INLINELINKAGE MSBoolean operator!= (const MSIndexVector &) const;

  INLINELINKAGE friend MSTypesExport MSBoolean operator<  (const MSIndexVector &, const MSIndexVector &);
  INLINELINKAGE friend MSTypesExport MSBoolean operator>  (const MSIndexVector &, const MSIndexVector &);
  INLINELINKAGE friend MSTypesExport MSBoolean operator<= (const MSIndexVector &, const MSIndexVector &);
  INLINELINKAGE friend MSTypesExport MSBoolean operator>= (const MSIndexVector &, const MSIndexVector &);

  INLINELINKAGE friend MSTypesExport MSBoolean operator<  (const MSIndexVector &, const unsigned int);
  INLINELINKAGE friend MSTypesExport MSBoolean operator<  (const unsigned int,  const MSIndexVector &);
  INLINELINKAGE friend MSTypesExport MSBoolean operator>  (const MSIndexVector &, const unsigned int);
  INLINELINKAGE friend MSTypesExport MSBoolean operator>  (const unsigned int,  const MSIndexVector &);
  INLINELINKAGE friend MSTypesExport MSBoolean operator<= (const MSIndexVector &, const unsigned int);
  INLINELINKAGE friend MSTypesExport MSBoolean operator<= (const unsigned int,  const MSIndexVector &);
  INLINELINKAGE friend MSTypesExport MSBoolean operator>= (const MSIndexVector &, const unsigned int);
  INLINELINKAGE friend MSTypesExport MSBoolean operator>= (const unsigned int,  const MSIndexVector &);
  INLINELINKAGE friend MSTypesExport MSBoolean operator== (const MSIndexVector &, const unsigned int);
  INLINELINKAGE friend MSTypesExport MSBoolean operator== (const unsigned int,  const MSIndexVector &);
  INLINELINKAGE friend MSTypesExport MSBoolean operator!= (const MSIndexVector &, const unsigned int);
  INLINELINKAGE friend MSTypesExport MSBoolean operator!= (const unsigned int,  const MSIndexVector &);

  INLINELINKAGE MSIndexVector & operator<<= (const unsigned int);
  INLINELINKAGE MSIndexVector & operator<<= (const MSIndexVector &);
  INLINELINKAGE MSIndexVector & operator<<  (const unsigned int);
  INLINELINKAGE MSIndexVector & operator<<  (const MSIndexVector &);

  MSIndexVector & random (unsigned int =0);
  MSIndexVector & series (unsigned int, unsigned int =0);
  INLINELINKAGE MSIndexVector & series ();

  unsigned int min() const;
  unsigned int max() const;
  
  double sum() const;

  INLINELINKAGE double avg() const;
  INLINELINKAGE double mean() const;
  INLINELINKAGE double median() const;
  INLINELINKAGE double variance() const;
  INLINELINKAGE double stdDeviation() const;

  INLINELINKAGE friend MSTypesExport MSIndexVector movingAverage (const MSIndexVector &, unsigned int);

  // Arithmetic Operations
  
  friend MSTypesExport MSIndexVector operator- (const MSIndexVector &);
  
  MSIndexVector & operator++ ();      // prefix increment
  MSIndexVector & operator-- ();      // prefix decrement
  INLINELINKAGE MSIndexVector operator++ (int);   // postfix increment
  INLINELINKAGE MSIndexVector operator-- (int);   // postfix decrement

  friend MSTypesExport MSIndexVector operator+ (const MSIndexVector &, const MSIndexVector &);
  friend MSTypesExport MSIndexVector operator- (const MSIndexVector &, const MSIndexVector &);
  friend MSTypesExport MSIndexVector operator* (const MSIndexVector &, const MSIndexVector &);
  friend MSTypesExport MSIndexVector operator/ (const MSIndexVector &, const MSIndexVector &);

  friend MSTypesExport MSIndexVector operator+ (const MSIndexVector &, const unsigned int);
  friend MSTypesExport MSIndexVector operator- (const MSIndexVector &, const unsigned int);
  friend MSTypesExport MSIndexVector operator* (const MSIndexVector &, const unsigned int);
  friend MSTypesExport MSIndexVector operator/ (const MSIndexVector &, const unsigned int);

  INLINELINKAGE friend MSTypesExport MSIndexVector operator+ (const unsigned int,  const MSIndexVector &);
  INLINELINKAGE friend MSTypesExport MSIndexVector operator* (const unsigned int,  const MSIndexVector &);
  friend MSTypesExport MSIndexVector operator- (const unsigned int,  const MSIndexVector &);
  friend MSTypesExport MSIndexVector operator/ (const unsigned int,  const MSIndexVector &);

  MSIndexVector & operator+= (const MSIndexVector &);
  MSIndexVector & operator-= (const MSIndexVector &);
  MSIndexVector & operator*= (const MSIndexVector &);
  MSIndexVector & operator/= (const MSIndexVector &);

  MSIndexVector & operator+= (const unsigned int);
  MSIndexVector & operator-= (const unsigned int);
  MSIndexVector & operator*= (const unsigned int);
  MSIndexVector & operator/= (const unsigned int);
};

#ifndef MS_NO_INLINES
#include <MSTypes/MSIndexVectorInlines.C>
#endif  // MS_NO_INLINES

// We provide provide explicit "do_not_instantiate" pragmas here to avoid instantiation complications in
// MStk applications.
//
#if !defined(__MSTYPESLIBRARY_BUILD__) && defined(MS_DO_NOT_INSTANTIATE) && defined(MS_EDG_TEMPLATE_INSTANTIATION)
#pragma do_not_instantiate unsigned int msMergeSortUp(unsigned int,unsigned int *,unsigned int *,unsigned int,unsigned int)
#pragma do_not_instantiate unsigned int msMergeSortDown(unsigned int,unsigned int *,unsigned int *,unsigned int,unsigned int)
#endif // !__MSTYPESLIBRARY_BUILD__ && defined(MS_DO_NOT_INSTANTIATE) && defined(MS_EDG_TEMPLATE_INSTANTIATION)

#endif  // MSIndexVectorHEADER
