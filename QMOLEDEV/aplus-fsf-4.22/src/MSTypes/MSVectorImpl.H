#ifndef MSVectorImplHEADER
#define MSVectorImplHEADER

///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////


#include <MSTypes/MSString.H>
#include <MSTypes/MSError.H>
#include <MSTypes/MSEnum.H>

#ifndef MSDefinesHEADER
#include <MSTypes/MSDefines.H>
#endif

//*** forward declarations ***//
#if HAVE_IOSTREAM
#include <iostream>
// #include <fstream>
using namespace std;
#else
class ostream;
#endif

class MSIndexVector;
class MSBinaryVector;

class MSTypesExport MSVectorImplOps
{
public:
  MSVectorImplOps();
  virtual ~MSVectorImplOps();

  virtual void *allocate         (unsigned int, unsigned int =0, MSAllocationFlag =MSRaw) const =0;
  virtual void *allocateWithSize (unsigned int, unsigned int =0, MSAllocationFlag =MSRaw) const =0;
  virtual void  deallocate       (void *, unsigned int =0, MSAllocationFlag =MSRaw) const =0;
    
  virtual void incrementCount   (void *) const =0;
  virtual unsigned int refCount (const void *) const =0;
    
  virtual void set  (void *, unsigned int, const void *, MSAllocationFlag =MSConstructed) const =0;
  virtual void set  (void *, unsigned int, const void *, unsigned int, MSAllocationFlag =MSConstructed) const =0;
  virtual void fill (void *, unsigned int, unsigned int, const void *, MSAllocationFlag =MSConstructed) const =0;
  virtual void copy (const void *, void *, unsigned int, unsigned int =0, unsigned int =0,
		       MSAllocationFlag =MSConstructed) const =0;
  virtual void copyBackward (void *, unsigned int, unsigned int, unsigned int) const =0;
  virtual void destroy (void *, unsigned int, unsigned int) const =0;
    
  virtual int  isElementEqual     (const void *, unsigned int, const void *) const =0;
  virtual int  isElementLess      (const void *, unsigned int, const void *) const =0;
  virtual int  isElementLessEqual (const void *, unsigned int, const void *) const =0;
  virtual long compareElement     (const void *, unsigned int, const void *) const =0;
    
  virtual void * elementAt (const void *, unsigned int) const =0;
  virtual unsigned int size (const void *) const =0;
  virtual void swapElements (void *, unsigned int, unsigned int) const =0;

  virtual unsigned int gradeUp   (const void *, unsigned int, unsigned int *) const =0;
  virtual unsigned int gradeDown (const void *, unsigned int, unsigned int *) const =0;

  virtual void *badData() const =0;
  virtual void *defaultFiller() const =0;

  virtual MSString asString (const void *, unsigned int) const =0;
  virtual MSString asMSF    (const void *, unsigned int) const =0;

  virtual unsigned int elementLen (const void *, unsigned int) const =0;

  virtual MSError::ErrorStatus setFromString (void *, unsigned int, const char *) const =0;    
  virtual MSError::ErrorStatus setFromMSF (void *, unsigned int, const char *) const =0;

  virtual void print (const void *, unsigned int, ostream &) const =0;

  virtual unsigned int numElements (const MSString &, const char) const =0;
  virtual void setFromMSString (void *, unsigned int, const MSString &, unsigned int&, const char) const =0;
};

class MSTypesExport MSVectorImpl
{
public:
  MSVectorImpl (MSVectorImplOps *, unsigned int =0);
  MSVectorImpl (MSVectorImplOps *, unsigned int, void *);
  MSVectorImpl (const MSVectorImpl &);
  MSVectorImpl (MSVectorImplOps *, void *, unsigned int);

  virtual ~MSVectorImpl();  
  
  virtual MSVectorImpl *create (unsigned int =0, unsigned int =0) const;
  virtual MSVectorImpl *clone() const;	 

  void makeUniqueCopy();
  INLINELINKAGE void prepareToChange();
  
  void prepareToChangeWithoutCopy();
  void reallocateInPlace(unsigned int);

  INLINELINKAGE void *internalGet (unsigned int) const;  
  INLINELINKAGE void vectorIndexError  (unsigned int) const;
  
  MSVectorImpl & operator= (const MSVectorImpl &);

          MSString asString (const char =' ');
  virtual MSString asMSF();

  virtual MSError::ErrorStatus setFromString (const char *, const char =' ');
  virtual MSError::ErrorStatus setFromMSF    (const char *);

  void set (unsigned, void *);
  void setAll (const void *);

  void setSelected (const MSIndexVector &, const MSVectorImpl &);
  void setSelected (const MSIndexVector &, const void *);
  
  void setSelected (const MSBinaryVector &, const MSVectorImpl &);
  void setSelected (const MSBinaryVector &, const void *);  

  MSIndexVector setIndexSelected (const MSBinaryVector &, const MSVectorImpl &);
  MSIndexVector setIndexSelected (const MSBinaryVector &, const void *);    

  void print (ostream &) const;

  unsigned int indexOf (void *, unsigned int) const;
  unsigned int lastIndexOf (void *, unsigned int) const;

  unsigned int occurrencesOf (const void *, unsigned int) const;

  MSIndexVector  indicesOf (const MSVectorImpl &) const;
  MSBinaryVector memberOf (const MSVectorImpl &) const;

  MSError::ErrorStatus  append (void *);
  MSError::ErrorStatus  append (const MSVectorImpl &);
           unsigned int append (const char *, const char =' ');

  MSError::ErrorStatus  insertAt (unsigned int, void *);
  MSError::ErrorStatus  insertAt (unsigned int, const MSVectorImpl &);

  MSError::ErrorStatus removeAt (unsigned int, unsigned int =1);
  MSError::ErrorStatus remove   (const MSIndexVector &);
  MSError::ErrorStatus remove   (const MSBinaryVector &);
  MSError::ErrorStatus removeAll();
  MSError::ErrorStatus select   (const MSIndexVector &);
  MSError::ErrorStatus compress (const MSBinaryVector &);

  MSError::ErrorStatus reshape  (unsigned int);
  MSError::ErrorStatus exchange (unsigned int, unsigned int);
  MSError::ErrorStatus reverse  ();
  MSError::ErrorStatus rotate   (int);
  MSError::ErrorStatus take     (int, const void * =0);
  MSError::ErrorStatus drop     (int);

  void reverse  (const MSVectorImpl &);
  void rotate   (const MSVectorImpl &, int);
  void take     (const MSVectorImpl &, int, const void * =0);
  void drop     (const MSVectorImpl &, int);
  void select   (const MSVectorImpl &, const MSIndexVector &);
  void compress (const MSVectorImpl &, const MSBinaryVector &);

  MSBinaryVector unique();

  virtual MSIndexVector gradeUp  () const;
  virtual MSIndexVector gradeDown() const;
  INLINELINKAGE  void          permute  (const MSIndexVector &);

  unsigned int maxLength() const;

  INLINELINKAGE unsigned int length() const;
  INLINELINKAGE void *data() const;

  long           compare       (const MSVectorImpl &) const;
  MSBinaryVector binaryCompare (const MSVectorImpl &, MSComparison) const;
  MSBinaryVector binaryCompare (const void *, MSComparison) const;
  MSBoolean      scalarCompare (const void *, MSComparison) const;

protected:
  MSVectorImplOps   *_pOperations;
  void         *_pElements;
  unsigned int  _len;

  void *reallocate(unsigned int);
  void  reallocateAndCopy(unsigned int);
  
  void blockLeft (unsigned int, unsigned int, unsigned int);
  void blockRight (unsigned int, unsigned int, unsigned int);

  INLINELINKAGE void internalSet (unsigned int, const void *);
  INLINELINKAGE void internalSet (unsigned int, const MSVectorImpl &, unsigned int);
  void indexError (unsigned int) const;

  static unsigned int minSize();

  void mergeSortUp(unsigned int *, unsigned int *) const;
  void mergeSortDown(unsigned int *, unsigned int *) const;
};

#endif  // MSVectorImplHEADER
