#ifndef MSBuiltinTypeVectorHEADER
#define MSBuiltinTypeVectorHEADER

///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved.
// See .../src/LICENSE for terms of distribution
//
///////////////////////////////////////////////////////////////////////////////

#if HAVE_IOSTREAM
#include <iostream>
#endif
#if HAVE_SSTREAM
#include <sstream>
#include <string>
#else
#include <strstream.h>
#endif
#include <MSTypes/MSBaseTypeVector.H>
#include <MSTypes/MSBuiltinVectorImpl.H>
#include <MSTypes/MSAllocator.H>

#if defined(MS_NEED_IMPLEMENTATION_PRAGMA)
#pragma implementation("MSBuiltinTypeVector.C")
#endif

template <class Type> class MSBuiltinSPick;

#if defined(MS_PREDECLARE_TEMPLATE_FRIENDS)
template <class Type> class MSBuiltinVector;
template <class Type> INLINELINKAGE MSBuiltinVector<Type> movingAverage(const MSBuiltinVector<Type>&, unsigned int);
template <class Type> INLINELINKAGE MSBuiltinVector<Type> operator-(const MSBuiltinVector<Type>&);
template <class Type> INLINELINKAGE MSBuiltinVector<Type> operator+(const MSBuiltinVector<Type>&,
								    const MSBuiltinVector<Type>&);
template <class Type> INLINELINKAGE MSBuiltinVector<Type> operator-(const MSBuiltinVector<Type>&,
								    const MSBuiltinVector<Type>&);
template <class Type> INLINELINKAGE MSBuiltinVector<Type> operator*(const MSBuiltinVector<Type>&,
								    const MSBuiltinVector<Type>&);
template <class Type> INLINELINKAGE MSBuiltinVector<Type> operator/(const MSBuiltinVector<Type>&,
								    const MSBuiltinVector<Type>&);
template <class Type> INLINELINKAGE MSBuiltinVector<Type> operator+(const MSBuiltinVector<Type>&,
								    const Type&);
template <class Type> INLINELINKAGE MSBuiltinVector<Type> operator-(const MSBuiltinVector<Type>&, const Type&);
template <class Type> INLINELINKAGE MSBuiltinVector<Type> operator*(const MSBuiltinVector<Type>&, const Type&);
template <class Type> INLINELINKAGE MSBuiltinVector<Type> operator/(const MSBuiltinVector<Type>&, const Type&);
template <class Type> INLINELINKAGE MSBuiltinVector<Type> operator+(const Type&, const MSBuiltinVector<Type>&);
template <class Type> INLINELINKAGE MSBuiltinVector<Type> operator*(const Type&, const MSBuiltinVector<Type>&);
template <class Type> INLINELINKAGE MSBuiltinVector<Type> operator-(const Type&, const MSBuiltinVector<Type>&);
template <class Type> INLINELINKAGE MSBuiltinVector<Type> operator/(const Type&, const MSBuiltinVector<Type>&);
#endif  //MS_PREDECLARE_TEMPLATE_FRIENDS


template <class Type>
class MSBuiltinVectorOps : public MSBaseVectorOps<Type,MSAllocator<Type> >, public MSBuiltinVectorImplOps
{
public:
  MSBuiltinVectorOps();
  virtual ~MSBuiltinVectorOps();

  virtual void set (void *, unsigned int, const void *, MSAllocationFlag =MSConstructed) const;
  virtual void set (void *, unsigned int, const void *, unsigned int, MSAllocationFlag =MSConstructed) const;

  virtual void   setToNumber (void *, unsigned int, double) const;
  virtual double getAsNumber (const void *, unsigned int) const;

#if HAVE_SSTREAM
  virtual void readFromStream (void *, unsigned int, istringstream &) const;
  virtual void writeToStream  (const void *, unsigned int, ostringstream &) const;

  virtual void whitespace (istringstream &) const;
#else
  virtual void readFromStream (void *, unsigned int, istrstream &) const;
  virtual void writeToStream  (const void *, unsigned int, ostrstream &) const;

  virtual void whitespace (istrstream &) const;
#endif
  virtual unsigned int stringLen (const char *) const;

  virtual void print (const void *, unsigned int, ostream &) const;

  virtual void *defaultFiller() const;
};


template <class Type>
class MSBuiltinVector : public MSBaseVector<Type,MSAllocator<Type> >
{
protected:
  typedef MSBuiltinVectorOps<Type> Operations;
#ifdef MS_MULTI_THREAD
  static MSMutex _operationsMutex;
#endif
  static Operations& ops(void);

#ifdef MS_CLASS_ENUM_OVERLOADING_BUG
  enum MathOp2 {Plus,Minus,Divide,Times,Incr,Decr,Unary};
  typedef int MathOp;
#else
  enum MathOp {Plus,Minus,Divide,Times,Incr,Decr,Unary};
#endif
  
public:
  MSBuiltinVector ();
  MSBuiltinVector (unsigned int);
  MSBuiltinVector (unsigned int, const Type &);
  MSBuiltinVector (const MSBuiltinVector<Type> &);
  MSBuiltinVector (const MSBaseVector<Type,MSAllocator<Type> > &);
  MSBuiltinVector (const char *);
  MSBuiltinVector (MSBuiltinVectorImpl *);
  MSBuiltinVector (MSTypeData<Type,MSAllocator<Type> > *, unsigned int); 
  MSBuiltinVector (const Type *, unsigned int); 

  virtual ~MSBuiltinVector();

  MSBuiltinVector<Type> & operator= (const MSBuiltinVector<Type> &);
  MSBuiltinVector<Type> & operator= (const MSBaseVector<Type,MSAllocator<Type> > &);
  MSBuiltinVector<Type> & operator= (const Type &);

  INLINELINKAGE Type firstElement() const;
  INLINELINKAGE Type lastElement () const;
  INLINELINKAGE Type operator() (unsigned int) const;
  INLINELINKAGE Type operator[] (unsigned int) const;

  INLINELINKAGE const Type & elementAt  (unsigned int) const;

  INLINELINKAGE MSBuiltinSPick<Type> operator[] (unsigned int);

  INLINELINKAGE MSBuiltinVector<Type> operator[] (const MSIndexVector &) const;
  INLINELINKAGE MSBuiltinVector<Type> operator[] (const MSBinaryVector &) const;

  MSBuiltinVector<Type> & random (unsigned long =0);
  MSBuiltinVector<Type> & series (unsigned int, Type =0);
  INLINELINKAGE MSBuiltinVector<Type> & series ();

  Type   min() const;
  Type   max() const;
  double sum() const;

  INLINELINKAGE double avg() const;
  INLINELINKAGE double mean() const;
  INLINELINKAGE double median() const;
  INLINELINKAGE double variance(MSEstimateType =MSUnbiased) const;
  INLINELINKAGE double stdDeviation(MSEstimateType =MSUnbiased) const;

  INLINELINKAGE2 friend MSTypesExport MSBuiltinVector<Type> 
  movingAverage MS_FRIEND_TEMPLATEID (const MSBuiltinVector<Type> &, unsigned int);

  INLINELINKAGE2 friend MSTypesExport MSBuiltinVector<Type>
  operator- MS_FRIEND_TEMPLATEID (const MSBuiltinVector<Type> &);
  
  INLINELINKAGE MSBuiltinVector<Type> & operator++ ();      // prefix increment
  INLINELINKAGE MSBuiltinVector<Type> & operator-- ();      // prefix decrement
  INLINELINKAGE MSBuiltinVector<Type> operator++ (int);   // postfix increment
  INLINELINKAGE MSBuiltinVector<Type> operator-- (int);   // postfix decrement

  INLINELINKAGE2 friend MSTypesExport MSBuiltinVector<Type> operator+ MS_FRIEND_TEMPLATEID (const MSBuiltinVector<Type>&,
                                                                                           const MSBuiltinVector<Type>&);
  INLINELINKAGE2 friend MSTypesExport MSBuiltinVector<Type> operator- MS_FRIEND_TEMPLATEID (const MSBuiltinVector<Type>&,
                                                                                           const MSBuiltinVector<Type>&);
  INLINELINKAGE2 friend MSTypesExport MSBuiltinVector<Type> operator* MS_FRIEND_TEMPLATEID (const MSBuiltinVector<Type>&,
                                                                                           const MSBuiltinVector<Type>&);
  INLINELINKAGE2 friend MSTypesExport MSBuiltinVector<Type> operator/ MS_FRIEND_TEMPLATEID (const MSBuiltinVector<Type>&,
                                                                     const MSBuiltinVector<Type>&);

  INLINELINKAGE2 friend MSTypesExport MSBuiltinVector<Type> operator+ MS_FRIEND_TEMPLATEID(const MSBuiltinVector<Type>&,
                                                                                          const Type&);
  INLINELINKAGE2 friend MSTypesExport MSBuiltinVector<Type> operator- MS_FRIEND_TEMPLATEID (const MSBuiltinVector<Type>&,
                                                                                           const Type&);
  INLINELINKAGE2 friend MSTypesExport MSBuiltinVector<Type> operator* MS_FRIEND_TEMPLATEID (const MSBuiltinVector<Type>&,
                                                                                           const Type&);
  INLINELINKAGE2 friend MSTypesExport MSBuiltinVector<Type> operator/ MS_FRIEND_TEMPLATEID (const MSBuiltinVector<Type>&,
                                                                                           const Type&);

  INLINELINKAGE2 friend MSTypesExport MSBuiltinVector<Type> operator+ MS_FRIEND_TEMPLATEID (const Type&,
                                                                                           const MSBuiltinVector<Type>&);
  INLINELINKAGE2 friend MSTypesExport MSBuiltinVector<Type> operator* MS_FRIEND_TEMPLATEID (const Type&,
                                                                     const MSBuiltinVector<Type>&);
  INLINELINKAGE2 friend MSTypesExport MSBuiltinVector<Type> operator- MS_FRIEND_TEMPLATEID (const Type&,
                                                                                           const MSBuiltinVector<Type>&);
  INLINELINKAGE2 friend MSTypesExport MSBuiltinVector<Type> operator/ MS_FRIEND_TEMPLATEID (const Type&,
                                                                     const MSBuiltinVector<Type>&);

  INLINELINKAGE MSBuiltinVector<Type>& operator+=(const MSBuiltinVector<Type>&);
  INLINELINKAGE MSBuiltinVector<Type>& operator-=(const MSBuiltinVector<Type>&);
  INLINELINKAGE MSBuiltinVector<Type>& operator*=(const MSBuiltinVector<Type>&);
  INLINELINKAGE MSBuiltinVector<Type>& operator/=(const MSBuiltinVector<Type>&);

  INLINELINKAGE MSBuiltinVector<Type>& operator+=(const Type&);
  INLINELINKAGE MSBuiltinVector<Type>& operator-=(const Type&);
  INLINELINKAGE MSBuiltinVector<Type>& operator*=(const Type&);
  INLINELINKAGE MSBuiltinVector<Type>& operator/=(const Type&);

protected:
  // operations on the current vector 
  void doMath(const MSBuiltinVector<Type>&, MathOp);
  void doMath(const Type&, MathOp); 
  // static operations
  static MSBuiltinVectorImpl* doMath(const MSBuiltinVector<Type>&, const MSBuiltinVector<Type>&, MathOp);
  static MSBuiltinVectorImpl* doMath(const MSBuiltinVector<Type>&, const Type&, MathOp);
  static MSBuiltinVectorImpl* doMath(const Type&, const MSBuiltinVector<Type>&, MathOp);
}; 

#if HAVE_SSTREAM
template <class Type> void whitespace (const Type &, istringstream &);
#else
template <class Type> void whitespace (const Type &, istrstream &);
#endif
template <class Type> INLINELINKAGE unsigned int stringLen (const Type &, const char *);

#ifndef  MS_NO_INLINES
#include <MSTypes/MSBuiltinTypeVectorInlines.C>
#endif  // MS_NO_INLINES

#endif  // MSBuiltinTypeVectorHEADER
