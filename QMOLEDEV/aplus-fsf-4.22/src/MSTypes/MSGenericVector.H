#ifndef MSGenericVectorHEADER
#define MSGenericVectorHEADER

///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved.
// See .../src/LICENSE for terms of distribution
//
///////////////////////////////////////////////////////////////////////////////


#ifndef MSDefinesHEADER
#include <MSTypes/MSDefines.H>
#endif

#if defined(MS_NEED_IMPLEMENTATION_PRAGMA)
#pragma implementation("MSGenericVector.C")
#endif

#include <MSTypes/MSString.H>
#include <MSTypes/MSSymbol.H>

#include <MSTypes/MSData.H>
#include <MSTypes/MSIndexVector.H>  
#include <MSTypes/MSBinaryVector.H>
#include <MSTypes/MSVectorImpl.H>

template <class Type> class MSGenericData;

#if defined(MS_PREDECLARE_TEMPLATE_FRIENDS)
template<class Type> class MSGenericVector;
template<class Type> ostream& operator<< (ostream&, const MSGenericVector<Type>&);
template<class Type> INLINELINKAGE MSBoolean operator< (const MSGenericVector<Type>&, const MSGenericVector<Type>&);
template<class Type> INLINELINKAGE MSBoolean operator> (const MSGenericVector<Type>&, const MSGenericVector<Type>&);
template<class Type> INLINELINKAGE MSBoolean operator<=(const MSGenericVector<Type>&, const MSGenericVector<Type>&);
template<class Type> INLINELINKAGE MSBoolean operator>=(const MSGenericVector<Type>&, const MSGenericVector<Type>&);
template<class Type> INLINELINKAGE MSBoolean operator< (const MSGenericVector<Type>&, const Type&);
template<class Type> INLINELINKAGE MSBoolean operator< (const Type&, const MSGenericVector<Type>&);
template<class Type> INLINELINKAGE MSBoolean operator> (const MSGenericVector<Type>&, const Type&);
template<class Type> INLINELINKAGE MSBoolean operator> (const Type&, const MSGenericVector<Type>&);
template<class Type> INLINELINKAGE MSBoolean operator<=(const MSGenericVector<Type>&, const Type&);
template<class Type> INLINELINKAGE MSBoolean operator<=(const Type&, const MSGenericVector<Type>&);
template<class Type> INLINELINKAGE MSBoolean operator>=(const MSGenericVector<Type>&, const Type&);
template<class Type> INLINELINKAGE MSBoolean operator>=(const Type&, const MSGenericVector<Type>&);
template<class Type> INLINELINKAGE MSBoolean operator==(const MSGenericVector<Type>&, const Type&);
template<class Type> INLINELINKAGE MSBoolean operator==(const Type&, const MSGenericVector<Type>&);
template<class Type> INLINELINKAGE MSBoolean operator!=(const MSGenericVector<Type>&, const Type&);
template<class Type> INLINELINKAGE MSBoolean operator!=(const Type&, const MSGenericVector<Type>&);
template<class Type> INLINELINKAGE MSBoolean operator==(const MSGenericVector<Type>&, const MSGenericVector<Type>&);
template<class Type> INLINELINKAGE MSBoolean operator!=(const MSGenericVector<Type>&, const MSGenericVector<Type>&);
template<class Type> INLINELINKAGE MSGenericVector<Type>& operator<<(MSGenericVector<Type>&, const MSGenericVector<Type>&);
template<class Type> INLINELINKAGE MSGenericVector<Type>& operator<<(MSGenericVector<Type>&, const Type&);

#endif  //MS_PREDECLARE_TEMPLATE_FRIENDS

template <class Type>
class MSGenericVectorOps : public MSVectorImplOps
{
protected:
  typedef MSGenericData<Type> Data;

#ifdef MS_MULTI_THREAD
  static MSMutex _nullDataMutex;
#endif
  static Data& nullData(void);
    
  static unsigned mergeSortUp   (unsigned, Type *, unsigned *, unsigned, unsigned);
  static unsigned mergeSortDown (unsigned, Type *, unsigned *, unsigned, unsigned);

  INLINELINKAGE static unsigned indexCompareUp   (Type *, unsigned, unsigned);
  INLINELINKAGE static unsigned indexCompareDown (Type *, unsigned, unsigned);

public:
  MSGenericVectorOps();
  virtual ~MSGenericVectorOps();
    
  virtual void * allocate         (unsigned int, unsigned int =0, MSAllocationFlag =MSRaw) const;
  virtual void * allocateWithSize (unsigned int, unsigned int =0, MSAllocationFlag =MSRaw) const;
  virtual void   deallocate       (void *, unsigned int =0, MSAllocationFlag =MSRaw) const;

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

template <class Type>
class MSGenericData : public MSData
{
public:
  MSGenericData (unsigned int =0);
  ~MSGenericData();
  
  MSString asDebugInfo(void) const;

  void *operator new (size_t, unsigned =0);
  void  operator delete (void *);

  INLINELINKAGE MSGenericData<Type> * incrementCount();
  INLINELINKAGE void decrementCount (MSAllocationFlag =MSConstructed, unsigned int =0);
  
  INLINELINKAGE const Type * elements() const;
  INLINELINKAGE const Type * data() const;   // same as elements(); used for backward compatibility only

  INLINELINKAGE Type * elements();
  INLINELINKAGE Type * data();	// same as elements(); used for backward compatibility only
  
  INLINELINKAGE const Type & elementAt (unsigned index_) const;
  INLINELINKAGE       Type & elementAt (unsigned index_);

  void set (unsigned int, const Type &, MSAllocationFlag =MSConstructed);

  static void constructElements(Type *, unsigned int, const Type&);
  static void destroyElements(Type *, unsigned int);

  static void fill (Type *, unsigned int, const Type &, MSAllocationFlag =MSConstructed);
  static void copy (const Type *, Type *, unsigned int, MSAllocationFlag =MSConstructed);
  static void copyBackward (const Type *, Type *, unsigned int);

  static MSGenericData<Type> *allocateWithSize   (unsigned int, MSAllocationFlag =MSConstructed, unsigned int =0);
  static MSGenericData<Type> *allocateWithLength (unsigned int, MSAllocationFlag =MSConstructed, unsigned int =0);
         void  deallocate         (MSAllocationFlag =MSConstructed, unsigned int =0);

protected:
  INLINELINKAGE static unsigned int dataOffset(void);
  //
  // ***** PLEASE NOTE *****
  //
  // This class cannot contain any data members of its own; the only data members that it can have
  // are those inherited from MSData base class.  The reason for this is that MSGenericData<Type> will
  // allocate an array of data elements **directly following** the end of MSData data members,
  // including the possible padding for alignment.
  //
};
  
template <class Type>
class MSGenericVector
{
protected:
  typedef MSGenericVectorOps<Type> Operations;

  MSVectorImpl *_pImpl;
#ifdef MS_MULTI_THREAD
  static MSMutex _operationsMutex;
#endif

  MSGenericVector (MSVectorImpl *);
  static Operations& ops(void);

public:
  MSGenericVector ();
  MSGenericVector (const MSGenericVector<Type> &);
  MSGenericVector (unsigned int);
  MSGenericVector (unsigned int, const Type &);
  // special constructor used for return value optimization
  MSGenericVector (MSGenericData<Type> *, unsigned int);
  MSGenericVector (const Type *, unsigned int);

  ~MSGenericVector();

  MSGenericVector<Type> & operator= (const MSGenericVector<Type> &);
  MSGenericVector<Type> & operator= (const Type &);

  MSString asDebugInfo() const;

  MSError::ErrorStatus set (unsigned int, const Type &); 

  INLINELINKAGE unsigned int length() const;
  INLINELINKAGE unsigned int size() const;

  INLINELINKAGE Type * elements() const;

  INLINELINKAGE unsigned int indexOf     (const Type &, unsigned int =0) const;
  INLINELINKAGE unsigned int lastIndexOf (const Type &, unsigned int =UINT_MAX-1) const;

  INLINELINKAGE unsigned int occurrencesOf (const Type &, unsigned int =0) const;

  INLINELINKAGE MSIndexVector  indicesOf (const MSGenericVector<Type> &) const;
  INLINELINKAGE MSBinaryVector memberOf  (const MSGenericVector<Type> &) const;

  INLINELINKAGE MSBinaryVector uniqueElements() const;

  INLINELINKAGE MSGenericVector<Type> & replaceAt (unsigned int, const Type &);
   
  INLINELINKAGE MSGenericVector<Type> & append    (const Type &);
  INLINELINKAGE MSGenericVector<Type> & append    (const MSGenericVector<Type> &);
                MSGenericVector<Type> & insertAt  (unsigned int, const Type &);
                MSGenericVector<Type> & insertAt  (unsigned int, const MSGenericVector<Type> &);
  INLINELINKAGE MSGenericVector<Type> & removeAt  (unsigned int);
  INLINELINKAGE MSGenericVector<Type> & removeAt  (unsigned int, unsigned int);
  INLINELINKAGE MSGenericVector<Type> & remove    (const MSIndexVector &);
  INLINELINKAGE MSGenericVector<Type> & remove    (const MSBinaryVector &);
  INLINELINKAGE MSGenericVector<Type> & removeAll ();

  INLINELINKAGE MSGenericVector<Type> & select    (const MSIndexVector &);
  INLINELINKAGE MSGenericVector<Type> & compress  (const MSBinaryVector &); 

  INLINELINKAGE MSGenericVector<Type> & selectUnique();

  INLINELINKAGE MSGenericVector<Type> & reshape  (unsigned int);
  INLINELINKAGE MSGenericVector<Type> & exchange (unsigned int, unsigned int);
  INLINELINKAGE MSGenericVector<Type> & reverse  ();
  INLINELINKAGE MSGenericVector<Type> & rotate   (int);
  INLINELINKAGE MSGenericVector<Type> & take     (int);
  INLINELINKAGE MSGenericVector<Type> & take     (int, const Type &);
  INLINELINKAGE MSGenericVector<Type> & drop     (int);

  static MSGenericVector<Type> reverse (const MSGenericVector<Type> &);
  static MSGenericVector<Type> rotate  (const MSGenericVector<Type> &, int);
  static MSGenericVector<Type> take    (const MSGenericVector<Type> &, int);
  static MSGenericVector<Type> take    (const MSGenericVector<Type> &, int, const Type &);
  static MSGenericVector<Type> drop    (const MSGenericVector<Type> &, int);
  static MSGenericVector<Type> select  (const MSGenericVector<Type> &, const MSIndexVector &);
  static MSGenericVector<Type> compress(const MSGenericVector<Type> &, const MSBinaryVector &);

  INLINELINKAGE static MSGenericVector<Type> selectUnique (const MSGenericVector<Type> &);

  INLINELINKAGE MSGenericVector<Type>& selectiveAssign(const MSIndexVector&,  const Type&);
  INLINELINKAGE MSGenericVector<Type>& selectiveAssign(const MSIndexVector&,  const MSGenericVector<Type>&);
  INLINELINKAGE MSGenericVector<Type>& selectiveAssign(const MSBinaryVector&, const Type&);
  INLINELINKAGE MSGenericVector<Type>& selectiveAssign(const MSBinaryVector&, const MSGenericVector<Type>&);

  INLINELINKAGE MSIndexVector gradeUp() const;
  INLINELINKAGE MSIndexVector gradeDown() const;
  INLINELINKAGE void permute (const MSIndexVector &);
  INLINELINKAGE MSGenericVector<Type> & sortUp();
  INLINELINKAGE MSGenericVector<Type> & sortDown();

  INLINELINKAGE const Type & firstElement() const;
  INLINELINKAGE const Type & lastElement() const;
  INLINELINKAGE const Type & operator[] (unsigned int) const;
  INLINELINKAGE const Type & operator() (unsigned int) const;
  INLINELINKAGE const Type & elementAt  (unsigned int) const;

  INLINELINKAGE Type & firstElement();
  INLINELINKAGE Type & lastElement();
  INLINELINKAGE Type & operator[] (unsigned int);
                Type & elementAt  (unsigned int);

  INLINELINKAGE MSGenericVector<Type> operator[](const MSIndexVector&) const;
  INLINELINKAGE MSGenericVector<Type> operator[](const MSBinaryVector&) const;

  friend ostream& operator<< MS_FRIEND_TEMPLATEID  (ostream&, const MSGenericVector<Type>&);

  INLINELINKAGE long compare (const MSGenericVector<Type> &) const;
  INLINELINKAGE MSBinaryVector binaryCompare (const MSGenericVector<Type> &, MSComparison) const;
  INLINELINKAGE MSBinaryVector binaryCompare (const Type &, MSComparison) const;

  INLINELINKAGE2 friend MSBoolean operator<  MS_FRIEND_TEMPLATEID (const MSGenericVector<Type> &,
                                                                  const MSGenericVector<Type> &);
  INLINELINKAGE2 friend MSBoolean operator>  MS_FRIEND_TEMPLATEID (const MSGenericVector<Type> &,
                                                                  const MSGenericVector<Type> &);
  INLINELINKAGE2 friend MSBoolean operator<= MS_FRIEND_TEMPLATEID (const MSGenericVector<Type> &,
                                                                  const MSGenericVector<Type> &);
  INLINELINKAGE2 friend MSBoolean operator>= MS_FRIEND_TEMPLATEID (const MSGenericVector<Type> &, 
                                                                  const MSGenericVector<Type> &);

  INLINELINKAGE2 friend MSBoolean operator<  MS_FRIEND_TEMPLATEID (const MSGenericVector<Type> &, const Type &);
  INLINELINKAGE2 friend MSBoolean operator<  MS_FRIEND_TEMPLATEID (const Type &,  const MSGenericVector<Type> &);
  INLINELINKAGE2 friend MSBoolean operator>  MS_FRIEND_TEMPLATEID  (const MSGenericVector<Type> &, const Type &);
  INLINELINKAGE2 friend MSBoolean operator>  MS_FRIEND_TEMPLATEID (const Type &,  const MSGenericVector<Type> &);
  INLINELINKAGE2 friend MSBoolean operator<= MS_FRIEND_TEMPLATEID (const MSGenericVector<Type> &, const Type &);
  INLINELINKAGE2 friend MSBoolean operator<= MS_FRIEND_TEMPLATEID (const Type &,  const MSGenericVector<Type> &);
  INLINELINKAGE2 friend MSBoolean operator>= MS_FRIEND_TEMPLATEID (const MSGenericVector<Type> &, const Type &);
  INLINELINKAGE2 friend MSBoolean operator>= MS_FRIEND_TEMPLATEID (const Type &,  const MSGenericVector<Type> &);
  INLINELINKAGE2 friend MSBoolean operator== MS_FRIEND_TEMPLATEID (const MSGenericVector<Type> &, const Type &);
  INLINELINKAGE2 friend MSBoolean operator== MS_FRIEND_TEMPLATEID (const Type &,  const MSGenericVector<Type> &);
  INLINELINKAGE2 friend MSBoolean operator!= MS_FRIEND_TEMPLATEID (const MSGenericVector<Type> &, const Type &);
  INLINELINKAGE2 friend MSBoolean operator!= MS_FRIEND_TEMPLATEID (const Type &,  const MSGenericVector<Type> &);
  INLINELINKAGE2 friend MSBoolean operator== MS_FRIEND_TEMPLATEID (const MSGenericVector<Type> &, const MSGenericVector<Type> &);
  INLINELINKAGE2 friend MSBoolean operator!= MS_FRIEND_TEMPLATEID (const MSGenericVector<Type> &, const MSGenericVector<Type> &);
  INLINELINKAGE2 friend MSGenericVector<Type>& operator<< MS_FRIEND_TEMPLATEID (MSGenericVector<Type>&, const MSGenericVector<Type>&);
  INLINELINKAGE2 friend MSGenericVector<Type>& operator<< MS_FRIEND_TEMPLATEID (MSGenericVector<Type>&, const Type&);


  INLINELINKAGE MSGenericVector<Type> & operator<<= (const Type &);
  INLINELINKAGE MSGenericVector<Type> & operator<<= (const MSGenericVector<Type> &);
};

template <class Type> INLINELINKAGE MSBoolean isGenericConstructionNeeded(Type *);
template <class Type> INLINELINKAGE MSBoolean isGenericDestructionNeeded(Type *);

#ifndef MS_NO_INLINES
#include <MSTypes/MSGenericVectorInlines.C>
#endif  // MS_NO_INLINES

#if defined(MS_COMPILE_INSTANTIATE)
#include <MSTypes/MSGenericVector.C>
#endif


#endif  // MSGenericVectorHEADER
