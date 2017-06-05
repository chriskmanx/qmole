#ifndef MSBinaryVectorHEADER
#define MSBinaryVectorHEADER

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
#include <MSTypes/MSVectorImpl.H>
#include <MSTypes/MSMergeSort.H>

#if HAVE_IOSTREAM
#include <iostream>
// #include <fstream>
using namespace std;
#else
class ostream;
#endif

class istrstream;
class ostrstream;
class MSIndexVector;

class MSTypesExport MSBinaryVector : public MSVector
{
 public:
  class Data;

#if defined(MS_NESTED_CLASSES_ACCESS_BUG)
public:
#else  
protected:
#endif  
  class MSTypesExport Operations : public MSVectorImplOps
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
  };

#if defined(MS_NESTED_CLASSES_ACCESS_BUG)
public:
#endif  
  class MSTypesExport SPick
  {
  protected:
    unsigned int    _index; 
    MSBinaryVector *_pVector;

  public:
    INLINELINKAGE SPick (MSBinaryVector &, unsigned int);
    INLINELINKAGE SPick (const SPick &);

    SPick & operator= (const SPick &);
    SPick & operator= (const unsigned char);

    INLINELINKAGE SPick & operator&=  (const unsigned char);
    INLINELINKAGE SPick & operator|=  (const unsigned char);
    INLINELINKAGE SPick & operator^=  (const unsigned char);

    INLINELINKAGE operator unsigned char() const;
  };

protected:

//*** data members ***//
  MSVectorImpl      *_pImpl;
#ifdef MS_MULTI_THREAD
  static MSMutex _operationsMutex;
#endif

  MSBinaryVector (MSVectorImpl *);	 
  virtual unsigned int getLength() const;
  static Operations& ops(void);

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
  
    INLINELINKAGE const unsigned char * elements() const;
    INLINELINKAGE const unsigned char * data() const;   // same as elements(); used for backward compatibility only

    INLINELINKAGE unsigned char * elements();
    INLINELINKAGE unsigned char * data();	// same as elements(); used for backward compatibility only
  
    INLINELINKAGE const unsigned char & elementAt (unsigned index_) const;
    INLINELINKAGE       unsigned char & elementAt (unsigned index_);

    static void fill (unsigned char *, unsigned int, const unsigned char);
    static void copy (const unsigned char *src_, unsigned char *dst_, unsigned int length_);
    static void copyBackward (const unsigned char *, unsigned char *, unsigned int);
    
    static Data *allocateWithSize   (unsigned int);
    static Data *allocateWithLength (unsigned int);
    void deallocate();

  protected:
    unsigned char _pElements[1];
  };
  
  MSBinaryVector ();
  MSBinaryVector (unsigned int, unsigned char =0);
  MSBinaryVector (const MSBinaryVector &);
  MSBinaryVector (const char *);
  MSBinaryVector (Data *, unsigned int);
  MSBinaryVector (const unsigned char *, unsigned int);

  virtual ~MSBinaryVector();

  MSBinaryVector & operator= (const MSBinaryVector &);
  MSBinaryVector & operator= (const unsigned char);

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

  INLINELINKAGE unsigned int    size()   const;

  unsigned int    length() const;
  unsigned char * data() const;

  INLINELINKAGE unsigned int indexOf     (const unsigned char, unsigned int =0) const;
  INLINELINKAGE unsigned int lastIndexOf (const unsigned char, unsigned int =UINT_MAX-1) const;

  INLINELINKAGE unsigned int occurrencesOf (const unsigned char, unsigned int =0) const;

  INLINELINKAGE MSIndexVector indicesOf (const MSBinaryVector &) const;
  INLINELINKAGE MSBinaryVector memberOf (const MSBinaryVector &) const;

  MSError::ErrorStatus set (unsigned int, const unsigned char);
  INLINELINKAGE MSBinaryVector & replaceAt (unsigned int, const unsigned char);

  MSBinaryVector & append    (const unsigned char);
  MSBinaryVector & append    (const MSBinaryVector &);
  MSBinaryVector & insertAt  (unsigned int, const unsigned char);
  MSBinaryVector & insertAt  (unsigned int, const MSBinaryVector &);
  MSBinaryVector & removeAt  (unsigned int);
  MSBinaryVector & removeAt  (unsigned int, unsigned int);
  MSBinaryVector & remove    (const MSIndexVector &);
  MSBinaryVector & remove    (const MSBinaryVector &);
  MSBinaryVector & removeAll ();

  MSBinaryVector & select    (const MSIndexVector &);
  MSBinaryVector & compress  (const MSBinaryVector &); 

  MSBinaryVector & reshape  (unsigned int);
  MSBinaryVector & exchange (unsigned int, unsigned int);
  MSBinaryVector & reverse  ();
  MSBinaryVector & rotate   (int);
  MSBinaryVector & take     (int, const unsigned char =0);
  MSBinaryVector & drop     (int);

  static MSBinaryVector reverse (const MSBinaryVector &);
  static MSBinaryVector rotate  (const MSBinaryVector &, int);
  static MSBinaryVector take    (const MSBinaryVector &, int, const unsigned char =0);
  static MSBinaryVector drop    (const MSBinaryVector &, int);
  static MSBinaryVector select  (const MSBinaryVector &, const MSIndexVector &);
  static MSBinaryVector compress(const MSBinaryVector &, const MSBinaryVector &);

  MSBinaryVector & selectiveAssign (const MSIndexVector  &, const unsigned char);
  MSBinaryVector & selectiveAssign (const MSIndexVector  &, const MSBinaryVector &);
  MSBinaryVector & selectiveAssign (const MSBinaryVector &, const unsigned char);
  MSBinaryVector & selectiveAssign (const MSBinaryVector &, const MSBinaryVector &);

  virtual void permute (const MSIndexVector &);

  INLINELINKAGE unsigned char firstElement() const;
  INLINELINKAGE unsigned char lastElement () const;
  INLINELINKAGE unsigned char operator[] (unsigned int) const;
  INLINELINKAGE unsigned char operator() (unsigned int) const;
  INLINELINKAGE const unsigned char & elementAt  (unsigned int) const;

  INLINELINKAGE SPick operator[] (unsigned int);

  INLINELINKAGE MSBinaryVector operator[] (const MSIndexVector &) const;
  INLINELINKAGE MSBinaryVector operator[] (const MSBinaryVector &) const;

  MSBinaryVector& random(void);	  // limit is always 2; nothing else makes sense

  friend MSTypesExport ostream & operator<< (ostream &, const MSBinaryVector &);

  INLINELINKAGE long compare (const MSBinaryVector &) const;
  INLINELINKAGE MSBinaryVector binaryCompare (const MSBinaryVector &, MSComparison) const;
  INLINELINKAGE MSBinaryVector binaryCompare (const unsigned char, MSComparison) const;

  INLINELINKAGE MSBoolean operator== (const MSBinaryVector &) const;
  INLINELINKAGE MSBoolean operator!= (const MSBinaryVector &) const;

  INLINELINKAGE friend MSTypesExport MSBoolean operator<  (const MSBinaryVector &, const MSBinaryVector &);
  INLINELINKAGE friend MSTypesExport MSBoolean operator>  (const MSBinaryVector &, const MSBinaryVector &);
  INLINELINKAGE friend MSTypesExport MSBoolean operator<= (const MSBinaryVector &, const MSBinaryVector &);
  INLINELINKAGE friend MSTypesExport MSBoolean operator>= (const MSBinaryVector &, const MSBinaryVector &);

  INLINELINKAGE friend MSTypesExport MSBoolean operator<  (const MSBinaryVector &, const unsigned char);
  INLINELINKAGE friend MSTypesExport MSBoolean operator<  (const unsigned char,  const MSBinaryVector &);
  INLINELINKAGE friend MSTypesExport MSBoolean operator>  (const MSBinaryVector &, const unsigned char);
  INLINELINKAGE friend MSTypesExport MSBoolean operator>  (const unsigned char,  const MSBinaryVector &);
  INLINELINKAGE friend MSTypesExport MSBoolean operator<= (const MSBinaryVector &, const unsigned char);
  INLINELINKAGE friend MSTypesExport MSBoolean operator<= (const unsigned char,  const MSBinaryVector &);
  INLINELINKAGE friend MSTypesExport MSBoolean operator>= (const MSBinaryVector &, const unsigned char);
  INLINELINKAGE friend MSTypesExport MSBoolean operator>= (const unsigned char,  const MSBinaryVector &);
  INLINELINKAGE friend MSTypesExport MSBoolean operator== (const MSBinaryVector &, const unsigned char);
  INLINELINKAGE friend MSTypesExport MSBoolean operator== (const unsigned char,  const MSBinaryVector &);
  INLINELINKAGE friend MSTypesExport MSBoolean operator!= (const MSBinaryVector &, const unsigned char);
  INLINELINKAGE friend MSTypesExport MSBoolean operator!= (const unsigned char,  const MSBinaryVector &);

  INLINELINKAGE MSBinaryVector & operator<<= (const unsigned char);
  INLINELINKAGE MSBinaryVector & operator<<= (const MSBinaryVector &);
  INLINELINKAGE MSBinaryVector & operator<<  (const unsigned char);
  INLINELINKAGE MSBinaryVector & operator<<  (const MSBinaryVector &);

  typedef void (*BitwiseOp)(unsigned char &, const unsigned char, const unsigned char);
  typedef unsigned char & (*BitwiseAssignOp)(unsigned char &, const unsigned char);

  static MSBinaryVector doBitwiseOp (const MSBinaryVector &, const MSBinaryVector &, BitwiseOp);
  static MSBinaryVector doBitwiseOp (const MSBinaryVector &, const unsigned char, BitwiseOp);

  MSBinaryVector & doBitwiseOp (const MSBinaryVector &, BitwiseAssignOp, BitwiseOp);
  MSBinaryVector & doBitwiseOp (const unsigned char, BitwiseAssignOp, BitwiseOp);

  INLINELINKAGE friend MSTypesExport MSBinaryVector operator& (const MSBinaryVector &, const MSBinaryVector &);
  INLINELINKAGE friend MSTypesExport MSBinaryVector operator& (const unsigned char, const MSBinaryVector &);
  INLINELINKAGE friend MSTypesExport MSBinaryVector operator& (const MSBinaryVector &, const unsigned char);

  INLINELINKAGE MSBinaryVector & operator&= (const MSBinaryVector &);
  INLINELINKAGE MSBinaryVector & operator&= (const unsigned char);

  INLINELINKAGE friend MSTypesExport MSBinaryVector operator| (const MSBinaryVector &, const MSBinaryVector &);
  INLINELINKAGE friend MSTypesExport MSBinaryVector operator| (const unsigned char, const MSBinaryVector &);
  INLINELINKAGE friend MSTypesExport MSBinaryVector operator| (const MSBinaryVector &, const unsigned char);

  INLINELINKAGE MSBinaryVector & operator|= (const MSBinaryVector &);
  INLINELINKAGE MSBinaryVector & operator|= (const unsigned char);

  INLINELINKAGE friend MSTypesExport MSBinaryVector operator^ (const MSBinaryVector &, const MSBinaryVector &);
  INLINELINKAGE friend MSTypesExport MSBinaryVector operator^ (const unsigned char, const MSBinaryVector &);
  INLINELINKAGE friend MSTypesExport MSBinaryVector operator^ (const MSBinaryVector &, const unsigned char);

  INLINELINKAGE MSBinaryVector & operator^= (const MSBinaryVector &);
  INLINELINKAGE MSBinaryVector & operator^= (const unsigned char);

                friend MSTypesExport MSBinaryVector operator! (const MSBinaryVector &);
  INLINELINKAGE friend MSTypesExport MSBinaryVector operator~ (const MSBinaryVector &);

  MSBinaryVector & complement();

  double sum() const;
};  

void bitwiseAND (unsigned char &, const unsigned char, const unsigned char);
void bitwiseOR  (unsigned char &, const unsigned char, const unsigned char);
void bitwiseXOR (unsigned char &, const unsigned char, const unsigned char);
unsigned char & bitwiseAND (unsigned char &, const unsigned char);
unsigned char & bitwiseOR  (unsigned char &, const unsigned char);
unsigned char & bitwiseXOR (unsigned char &, const unsigned char);

#ifndef MS_NO_INLINES
#include <MSTypes/MSBinaryVectorInlines.C>
#endif  // MS_NO_INLINES

// We provide provide explicit "do_not_instantiate" pragmas here to avoid instantiation complications in
// MStk applications.
//
#if !defined(__MSTYPESLIBRARY_BUILD__) && defined(MS_DO_NOT_INSTANTIATE) && defined(MS_EDG_TEMPLATE_INSTANTIATION)
#pragma do_not_instantiate unsigned int msMergeSortUp(unsigned int,unsigned char *,unsigned int *,unsigned int,unsigned int)
#pragma do_not_instantiate unsigned int msMergeSortDown(unsigned int,unsigned char *,unsigned int *,unsigned int,unsigned int)
#endif // !__MSTYPESLIBRARY_BUILD__ && defined(MS_DO_NOT_INSTANTIATE) && defined(MS_EDG_TEMPLATE_INSTANTIATION)

#endif  // MSBinaryVectorHEADER
