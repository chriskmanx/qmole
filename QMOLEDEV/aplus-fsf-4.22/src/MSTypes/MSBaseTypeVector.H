
#ifndef MSBaseTypeVectorHEADER
#define MSBaseTypeVectorHEADER

///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved.
// See .../src/LICENSE for terms of distribution
//
///////////////////////////////////////////////////////////////////////////////


#include <MSTypes/MSError.H>
#include <MSTypes/MSString.H>
#include <MSTypes/MSSymbol.H>
#include <MSTypes/MSVector.H>
#include <MSTypes/MSEnum.H>

#include <MSTypes/MSTypeData.H>
#include <MSTypes/MSVectorImpl.H>

#if defined(MS_NEED_IMPLEMENTATION_PRAGMA)
#pragma implementation("MSBaseTypeVector.C")
#endif

class MSIndexVector;
class MSBinaryVector;
  
#if defined(MS_PREDECLARE_TEMPLATE_FRIENDS)
template <class Type, class Allocator> class MSBaseVector;
template <class Type, class Allocator> INLINELINKAGE MSBoolean operator<(const MSBaseVector<Type,Allocator>&,
									  const MSBaseVector<Type,Allocator>&);
template <class Type, class Allocator> INLINELINKAGE MSBoolean operator>(const MSBaseVector<Type,Allocator>&,
									  const MSBaseVector<Type,Allocator>&);
template <class Type, class Allocator> INLINELINKAGE MSBoolean operator<=(const MSBaseVector<Type,Allocator>&,
									  const MSBaseVector<Type,Allocator>&);
template <class Type, class Allocator> INLINELINKAGE MSBoolean operator>=(const MSBaseVector<Type,Allocator>&,
									  const MSBaseVector<Type,Allocator>&);
template <class Type, class Allocator> INLINELINKAGE MSBoolean operator<(const MSBaseVector<Type,Allocator>&,
									  const Type&);
template <class Type, class Allocator> INLINELINKAGE MSBoolean operator<(const Type&,
									  const MSBaseVector<Type,Allocator>&);
template <class Type, class Allocator> INLINELINKAGE MSBoolean operator>(const MSBaseVector<Type,Allocator>&,
									  const Type&);
template <class Type, class Allocator> INLINELINKAGE MSBoolean operator>(const Type&,
									  const MSBaseVector<Type,Allocator>&);
template <class Type, class Allocator> INLINELINKAGE MSBoolean operator<=(const MSBaseVector<Type,Allocator>&,
									  const Type&);
template <class Type, class Allocator> INLINELINKAGE MSBoolean operator<=(const Type&,
									  const MSBaseVector<Type,Allocator>&);
template <class Type, class Allocator> INLINELINKAGE MSBoolean operator>=(const MSBaseVector<Type,Allocator>&,
									  const Type&);
template <class Type, class Allocator> INLINELINKAGE MSBoolean operator>=(const Type&,
									  const MSBaseVector<Type,Allocator>&);
template <class Type, class Allocator> INLINELINKAGE MSBoolean operator==(const MSBaseVector<Type,Allocator>&,
									  const Type&);
template <class Type, class Allocator> INLINELINKAGE MSBoolean operator==(const Type&,
									  const MSBaseVector<Type,Allocator>&);
template <class Type, class Allocator> INLINELINKAGE MSBoolean operator!=(const MSBaseVector<Type,Allocator>&,
									  const Type&);
template <class Type, class Allocator> INLINELINKAGE MSBoolean operator!=(const Type&,
									  const MSBaseVector<Type,Allocator>&);

template <class Type, class Allocator> INLINELINKAGE MSBoolean operator==(const MSBaseVector<Type,Allocator>&,
									  const MSBaseVector<Type,Allocator>&);
template <class Type, class Allocator> INLINELINKAGE MSBoolean operator!=(const MSBaseVector<Type,Allocator>&,
									  const MSBaseVector<Type,Allocator>&);

template <class Type, class Allocator> INLINELINKAGE MSBaseVector<Type,Allocator>& operator<< ( MSBaseVector<Type,Allocator>&, const MSBaseVector<Type,Allocator>&);

template <class Type, class Allocator> INLINELINKAGE MSBaseVector<Type,Allocator>& operator<< ( MSBaseVector<Type,Allocator>&, const Type&);


#if defined(MS_TEMPLATE_MANUAL_FRIEND_BUG)
template <class Type, class Allocator> INLINELINKAGE ostream& operator<<(ostream&,
									  const MSBaseVector<Type,Allocator>&);
#else
template <class Type, class Allocator> ostream& operator<<(ostream&, const MSBaseVector<Type,Allocator>&);
#endif  //MS_MANUAL_FRIEND_ACCESS_BUG

#endif  //MS_PREDECLARE_TEMPLATE_FRIENDS


template <class Type, class Allocator>
class MSBaseVectorOps : public MSVectorImplOps
{
public:
  typedef MSTypeData<Type,Allocator> Data;

  MSBaseVectorOps();
  virtual ~MSBaseVectorOps();
    
  virtual void * allocate        (unsigned int, unsigned int =0, MSAllocationFlag =MSRaw) const;
  virtual void * allocateWithSize(unsigned int, unsigned int =0, MSAllocationFlag =MSRaw) const;
  virtual void   deallocate      (void *, unsigned int =0, MSAllocationFlag =MSRaw) const ;

  virtual void incrementCount  (void *) const;
  virtual unsigned int refCount(const void *) const;

  virtual void set (void *, unsigned int, const void *, MSAllocationFlag =MSConstructed) const;
  virtual void set (void *, unsigned int, const void *, unsigned int, MSAllocationFlag =MSConstructed) const;
  virtual void fill(void *, unsigned int, unsigned int, const void *, MSAllocationFlag =MSConstructed) const;
  virtual void copy(const void *, void *, unsigned int, unsigned int =0, unsigned int =0,
		       MSAllocationFlag =MSConstructed) const;
  virtual void copyBackward(void *, unsigned int, unsigned int, unsigned int) const;
  virtual void destroy(void *, unsigned int, unsigned int) const;

  virtual int  isElementEqual    (const void *, unsigned int, const void *) const;
  virtual int  isElementLess     (const void *, unsigned int, const void *) const;
  virtual int  isElementLessEqual(const void *, unsigned int, const void *) const;
  virtual long compareElement    (const void *, unsigned int, const void *) const;

  virtual void *elementAt(const void *, unsigned int) const;
  virtual unsigned int size(const void *) const;
  virtual void swapElements(void *, unsigned int, unsigned int) const;

  virtual unsigned int gradeUp  (const void *, unsigned int, unsigned int *) const;
  virtual unsigned int gradeDown(const void *, unsigned int, unsigned int *) const;

  virtual void *badData() const;
  virtual void *defaultFiller() const;

  virtual MSString asString(const void *, unsigned int) const;
  virtual MSString asMSF   (const void *, unsigned int) const;

  virtual unsigned int elementLen(const void *, unsigned int) const;

  virtual MSError::ErrorStatus setFromString(void *, unsigned int, const char *) const;
  virtual MSError::ErrorStatus setFromMSF(void *, unsigned int, const char *) const;

  virtual void print(const void *, unsigned int, ostream&) const;

  virtual unsigned int numElements(const MSString&, const char) const;
  virtual void setFromMSString(void *, unsigned int, const MSString&, unsigned int&, const char) const;

protected:
#ifdef MS_MULTI_THREAD
  static MSMutex _nullDataMutex;
#endif
  static Data& nullData(void);
};

template <class Type, class Allocator>
class MSBaseVector : public MSVector
{
protected:
  typedef MSBaseVectorOps<Type,Allocator> Operations;

  MSVectorImpl *_pImpl;
  MSBoolean _blocked;
#ifdef MS_MULTI_THREAD
  static MSMutex _operationsMutex;
#endif

  INLINELINKAGE MSBaseVector(MSVectorImpl *);
  INLINELINKAGE MSTypeData<Type,Allocator> *vectorData() const;
  virtual unsigned int getLength() const;
  static Operations& ops(void);
  
public:
  MSBaseVector();
  MSBaseVector(const MSBaseVector<Type,Allocator>&);
#if defined(MS_TEMPLATE_CONSTRUCTOR_OVERLOAD_BUG)
 // Visual C++ bug dealing with overloading resolution; only first argument is used
  MSBaseVector(unsigned int, int, int);
#else 
  MSBaseVector(unsigned int);
#endif  //MS_TEMPLATE_CONSTRUCTOR_OVERLOAD_BUG
  MSBaseVector(unsigned int, const Type&);
  MSBaseVector(const char *, const char=' ');
  MSBaseVector(MSTypeData<Type,Allocator> *, unsigned int);
  MSBaseVector(const Type *, unsigned int);
  
  virtual ~MSBaseVector();

  MSBaseVector<Type,Allocator>& operator=(const MSBaseVector<Type,Allocator>&);
  MSBaseVector<Type,Allocator>& operator=(const Type&);

/*** MSModel pure virtual functions overriden here ***/
  virtual MSString asString() const;
  virtual MSString asMSF() const;
  virtual MSString asDebugInfo() const;
  virtual MSString className() const;
  virtual const MSSymbol& type() const;
  virtual MSModel *clone() const;
  virtual MSModel *create() const;

  virtual void assign(const MSModel&);
  virtual long compare(const MSModel&) const;
  virtual MSError::ErrorStatus set(const char *);
  virtual MSError::ErrorStatus setFromMSF(const char *);

  MSError::ErrorStatus set(unsigned int, const Type&);
  MSError::ErrorStatus set(unsigned int, const char *);
  
  INLINELINKAGE unsigned int maxLength() const;
  INLINELINKAGE unsigned int size()   const;

  INLINELINKAGE unsigned int length() const;
  Type * data() const;
  Type * elements() const;

  INLINELINKAGE unsigned int indexOf(const Type&, unsigned int =0) const;
  INLINELINKAGE unsigned int lastIndexOf(const Type&, unsigned int =UINT_MAX-1) const;

  INLINELINKAGE unsigned int occurrencesOf(const Type&, unsigned int =0) const;

  // the following method is equivalent to dyadic iota in apl(indexOf on vectors)
  INLINELINKAGE MSIndexVector indicesOf(const MSBaseVector<Type,Allocator>&) const;
  // the following method is equivalent to membership in apl
  INLINELINKAGE MSBinaryVector memberOf(const MSBaseVector<Type,Allocator>&) const;

  INLINELINKAGE MSBinaryVector uniqueElements() const;

  INLINELINKAGE MSBaseVector<Type,Allocator>& replaceAt(unsigned int, const Type&);

  MSBaseVector<Type,Allocator>& removeAll();
  MSBaseVector<Type,Allocator>& append   (const Type&);
  MSBaseVector<Type,Allocator>& append   (const MSBaseVector<Type,Allocator>&);
  MSBaseVector<Type,Allocator>& insertAt (unsigned int, const Type&);
  MSBaseVector<Type,Allocator>& insertAt (unsigned int, const MSBaseVector<Type,Allocator>&);
  MSBaseVector<Type,Allocator>& removeAt (unsigned int);
  MSBaseVector<Type,Allocator>& removeAt (unsigned int, unsigned int);
  MSBaseVector<Type,Allocator>& remove   (const MSIndexVector&);
  MSBaseVector<Type,Allocator>& remove   (const MSBinaryVector&);

  MSBaseVector<Type,Allocator>& select   (const MSIndexVector&);
  MSBaseVector<Type,Allocator>& compress (const MSBinaryVector&);

  INLINELINKAGE MSBaseVector<Type,Allocator>& selectUnique();

  MSBaseVector<Type,Allocator>& reshape (unsigned int);
  MSBaseVector<Type,Allocator>& exchange(unsigned int, unsigned int);
  MSBaseVector<Type,Allocator>& reverse ();
  MSBaseVector<Type,Allocator>& rotate  (int);
  MSBaseVector<Type,Allocator>& drop    (int);
  MSBaseVector<Type,Allocator>& take    (int);
  MSBaseVector<Type,Allocator>& take    (int, const Type&);

  static MSBaseVector<Type,Allocator> reverse(const MSBaseVector<Type,Allocator>&);
  static MSBaseVector<Type,Allocator> rotate (const MSBaseVector<Type,Allocator>&, int);
  static MSBaseVector<Type,Allocator> take   (const MSBaseVector<Type,Allocator>&, int);
  static MSBaseVector<Type,Allocator> take   (const MSBaseVector<Type,Allocator>&, int, const Type&);
  static MSBaseVector<Type,Allocator> drop   (const MSBaseVector<Type,Allocator>&, int);
  static MSBaseVector<Type,Allocator> select (const MSBaseVector<Type,Allocator>&, const MSIndexVector&);
  static MSBaseVector<Type,Allocator> compress(const MSBaseVector<Type,Allocator>&, const MSBinaryVector&);

  INLINELINKAGE static MSBaseVector<Type,Allocator> selectUnique(const MSBaseVector<Type,Allocator>&);

  MSBaseVector<Type,Allocator>& selectiveAssign(const MSIndexVector &, const Type&);
  MSBaseVector<Type,Allocator>& selectiveAssign(const MSIndexVector &, const MSBaseVector<Type,Allocator>&);
  MSBaseVector<Type,Allocator>& selectiveAssign(const MSBinaryVector&, const Type&);
  MSBaseVector<Type,Allocator>& selectiveAssign(const MSBinaryVector&, const MSBaseVector<Type,Allocator>&);

  virtual MSIndexVector gradeUp() const;
  virtual MSIndexVector gradeDown() const;
  virtual void permute(const MSIndexVector&);
  INLINELINKAGE MSBaseVector<Type,Allocator>& sortUp();
  INLINELINKAGE MSBaseVector<Type,Allocator>& sortDown();

// The version of EDG used by SGI's dcc compiler has a bug related to access from friends during manual instantiation;
// therefore, for SGI, make operator<<() inline so that it does not need to be manually instantiated in the library.
#if defined(MS_TEMPLATE_MANUAL_FRIEND_BUG)
  INLINELINKAGE friend ostream& 
  operator<< MS_FRIEND_TEMPLATEID (ostream&,const MSBaseVector<Type,Allocator>&);
#else
  friend MSTypesExport ostream& 
  operator<<  MS_FRIEND_TEMPLATEID (ostream&, const MSBaseVector<Type,Allocator>&);
#endif  //MS_MANUAL_FRIEND_ACCESS_BUG

  INLINELINKAGE long compare(const MSBaseVector<Type,Allocator>&) const;
  INLINELINKAGE MSBinaryVector binaryCompare(const MSBaseVector<Type,Allocator>&, MSComparison) const;
  INLINELINKAGE MSBinaryVector binaryCompare(const Type&, MSComparison) const;

  INLINELINKAGE2 friend MSBoolean operator< MS_FRIEND_TEMPLATEID (const MSBaseVector<Type,Allocator>&, 
                                                                 const MSBaseVector<Type,Allocator>&);
  
  INLINELINKAGE2 friend MSBoolean operator> MS_FRIEND_TEMPLATEID (const MSBaseVector<Type,Allocator>&,
                                                                 const MSBaseVector<Type,Allocator>&);

  INLINELINKAGE2 friend MSBoolean operator<= MS_FRIEND_TEMPLATEID (const MSBaseVector<Type,Allocator>&,
                                                                  const MSBaseVector<Type,Allocator>&);
  INLINELINKAGE2 friend MSBoolean operator>= MS_FRIEND_TEMPLATEID (const MSBaseVector<Type,Allocator>&,
                                                                  const MSBaseVector<Type,Allocator>&);

  INLINELINKAGE2 friend MSBoolean operator< MS_FRIEND_TEMPLATEID (const MSBaseVector<Type,Allocator>&, const Type&);
  INLINELINKAGE2 friend MSBoolean operator< MS_FRIEND_TEMPLATEID (const Type&, const MSBaseVector<Type,Allocator>&);
  INLINELINKAGE2 friend MSBoolean operator> MS_FRIEND_TEMPLATEID (const MSBaseVector<Type,Allocator>&, const Type&);
  INLINELINKAGE2 friend MSBoolean operator> MS_FRIEND_TEMPLATEID (const Type&, const MSBaseVector<Type,Allocator>&);
  INLINELINKAGE2 friend MSBoolean operator<= MS_FRIEND_TEMPLATEID (const MSBaseVector<Type,Allocator>&, const Type&);
  INLINELINKAGE2 friend MSBoolean operator<= MS_FRIEND_TEMPLATEID (const Type&, const MSBaseVector<Type,Allocator>&);
  INLINELINKAGE2 friend MSBoolean operator>= MS_FRIEND_TEMPLATEID (const MSBaseVector<Type,Allocator>&, const Type&);
  INLINELINKAGE2 friend MSBoolean operator>= MS_FRIEND_TEMPLATEID (const Type&, const MSBaseVector<Type,Allocator>&);
  INLINELINKAGE2 friend MSBoolean operator== MS_FRIEND_TEMPLATEID (const MSBaseVector<Type,Allocator>&, const Type&);
  INLINELINKAGE2 friend MSBoolean operator== MS_FRIEND_TEMPLATEID (const Type&, const MSBaseVector<Type,Allocator>&);
  INLINELINKAGE2 friend MSBoolean operator!= MS_FRIEND_TEMPLATEID (const MSBaseVector<Type,Allocator>&, const Type&);
  INLINELINKAGE2 friend MSBoolean operator!= MS_FRIEND_TEMPLATEID (const Type&, const MSBaseVector<Type,Allocator>&);
  INLINELINKAGE2 friend MSBoolean operator== MS_FRIEND_TEMPLATEID (const MSBaseVector<Type,Allocator>&, const MSBaseVector<Type,Allocator>&);
  INLINELINKAGE2 friend MSBoolean operator!= MS_FRIEND_TEMPLATEID (const MSBaseVector<Type,Allocator>&, const MSBaseVector<Type,Allocator>&);

  INLINELINKAGE2 friend MSBaseVector<Type,Allocator>&  operator<< MS_FRIEND_TEMPLATEID (MSBaseVector<Type,Allocator>&, const MSBaseVector<Type,Allocator>&);
  INLINELINKAGE2 friend MSBaseVector<Type,Allocator>&  operator<< MS_FRIEND_TEMPLATEID (MSBaseVector<Type,Allocator>&, const Type&);


  INLINELINKAGE MSBaseVector<Type,Allocator>& operator<<=(const MSBaseVector<Type,Allocator>&);
  INLINELINKAGE MSBaseVector<Type,Allocator>& operator<<=(const Type&);
  
};

#ifndef MS_NO_INLINES
#include <MSTypes/MSBaseTypeVectorInlines.C>
#endif //MS_NO_INLINES

#endif // MSBaseTypeVectorHEADER
