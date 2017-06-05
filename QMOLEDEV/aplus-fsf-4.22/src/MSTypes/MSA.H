#ifndef MSAHEADER 
#define MSAHEADER 

///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////


#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#if defined(__FreeBSD__) || defined(__NetBSD__) || defined(__APPLE__)
#include <stdlib.h>
#else
#include <malloc.h>
#endif
#include <MSTypes/MSDefines.H>
#include <MSTypes/MSEnum.H>
#include <MSTypes/MSInt.H>
#include <MSTypes/MSFloat.H>
#include <MSTypes/MSString.H>
// Due to Lucid's template specialization bug,
// inside MStk MSFloatVector.H must be included BEFORE all the other
// built-in vectors.
#include <MSTypes/MSFloatVector.H>
#include <MSTypes/MSCharVector.H>
#include <MSTypes/MSIntVector.H>
#include <MSTypes/MSIndexVector.H>
#include <MSTypes/MSLongVector.H>
#include <MSTypes/MSUnsignedLongVector.H>
#include <MSTypes/MSSymbolVector.H>
// Note: MSStringVector must be included after the specialized type vectors.
#include <MSTypes/MSStringVector.H>

#include <MSTypes/MSFloatMatrix.H>
#include <MSTypes/MSIntMatrix.H>
#include <MSTypes/MSLongMatrix.H>
#include <MSTypes/MSUnsignedLongMatrix.H>
#include <MSTypes/MSCharMatrix.H>


class MSMMap;
template<class Type, class Allocator> class MSTypeData;

class MSTypesExport MSA
{
public:
  enum {MAXRANK=9};
  enum AType {INTEGERTYPE=0L,FLOATTYPE=1L,CHARACTERTYPE=2L,STRINGTYPE=3L,ETYPE=4L,UTYPE=8L,XTYPE=UTYPE};
  enum MSAplusType {MSAInt,MSAFloat,MSAChar,MSASymbol,MSABox,MSAInvalidType,MSANull};
  struct a
  {
    long c,t,r,n,d[MAXRANK],i,p[1];
  }; 
  enum {AHEADER=sizeof(struct a)-sizeof(long)};

protected:
  MSA::a *_aStructPtr;
  
public:
  MSA(void);
  MSA(long,long,long,long *);
  MSA(a *,MSBoolean iced_=MSFalse);
  MSA(const MSA&);
  MSA(char);
  MSA(unsigned);
  MSA(long);
  MSA(double);
  MSA(const MSInt&);
  MSA(const MSFloat&);
  MSA(const MSString&);
  MSA(const MSSymbol&,MSBoolean encloseSymbols_=MSTrue);
  MSA(const MSIntVector&);
  MSA(const MSFloatVector&);
  MSA(const MSSymbolVector&,MSBoolean encloseSymbols_=MSFalse);
  MSA(const MSStringVector&);
  MSA(const MSIntMatrix&);
  MSA(const MSFloatMatrix&);
  MSA(const MSCharMatrix&);
  MSA(const MSMMap&, MSBoolean bImport_=MSTrue);
  ~MSA(void);

  MSA& operator=(const MSA&);

  void aStructPtr(MSA::a *aStructPtr_);
  inline MSA::a *aStructPtr(void) const;

  MSBoolean isNullMSA(void) const;
  inline unsigned numberOfElements(void) const;

  MSA simplePick( unsigned) const;
  MSA simplePick(const MSIndexVector&) const;
  MSA complexPick(const MSA&) const;
  MSA iterativePick(const MSIndexVector&) const;

  void simpleSpecify(const MSIndexVector& Index_, const MSA& BaObject_);
  void iterativeSpecify(const MSIndexVector& Index_, const MSA& BaObject_);

  long depth(void) const;
  inline unsigned rank(void) const;
  MSIntVector shape(void) const;
  MSAplusType aPlusType(void) const;

  MSString asDebugInfo(void) const;
 
  char asChar(void) const;
  long asLong(void) const;
  unsigned long asUnsignedLong(void) const;
  double asDouble(void) const;
  MSInt asMSInt(void) const;
  MSString asMSString(void) const;
  MSFloat asMSFloat(void) const;
  MSSymbol asMSSymbol(void) const;
  MSIntVector asMSIntVector(void) const;
  MSUnsignedLongVector asMSUnsignedLongVector(void) const;
  MSLongVector asMSLongVector(void) const;
  MSCharVector asMSCharVector(void) const;
  MSFloatVector asMSFloatVector(void) const;
  MSSymbolVector asMSSymbolVector(void) const;
  MSStringVector asMSStringVector(void) const;
  MSFloatMatrix asMSFloatMatrix(void) const;
  MSCharMatrix asMSCharMatrix(void) const;
  MSIntMatrix asMSIntMatrix(void) const;
  MSLongMatrix asMSLongMatrix(void) const;
  MSUnsignedLongMatrix asMSUnsignedLongMatrix(void) const;
  

  void beamOut(const char *fileName_, MSBoolean bExport_=MSTrue) const;
  MSA exportAObject(void) const;

  //This 2 api for use by MSAConnection and anyone else who wants to export 
  //an MSA object into a string without constructing another MSA first.
  int exportAObjectSizePass(long * hsz_, long *dsz_, int long_bytes=4) const ;
  int exportAObjectFillPass(char *dest_, long hsz_, char * trp_,long for_a_,int long_bytes=4) const;

  MSBoolean import(const MSMMap &aMap);
  MSBoolean import(void);

  static MSA importAObject(char *cvp,long cvlen,char *trp);

  static void dc(MSA::a *aobj);
  static void dec(MSA::a *);
  static MSA::a *ga(long t,long r,long n,long *d);
  static MSA::a *gc(long t,long r,long n,long *d,long *p);
  static MSA::a *gd(long t,MSA::a * aobj);
  static MSA::a *gf(double f);
  static MSA::a *gi(long i);
  static MSA::a *gm(long t_,long m_,long n_);
  static MSA::a *gv(long t,long n);
  static MSA::a *gs(long t);
  static MSA::a *ic(MSA::a *aobj);
  static long longAt(char *c);
  static long *ma(long i);
  static long *mab(long i);
  static void mf(long *p);
  static void mv(long *d,long *s,long n);
  static long si(char * s);
  static char *tmv(long t,long *d,long *s,long n);
  static void *balloc(int);
  static void bfree(char *);

  /* Endian Conversion routines */
  static void ndn16copy(char *from, char *to, int n);
  static void ndn32copy(char *from, char *to, int n);
  static void ndn64copy(char *from, char *to, int n);

  static inline void ndnicopy(long *from, long *to, int n)
  {
#ifdef MS_64BIT
    ndn64copy((char*)from,(char *)to,n); 
#else
    ndn32copy((char *)from,(char *)to,n); 
#endif
  }

  static inline void ndnfcopy(double *from, double *to, int n) 
  { ndn64copy((char*)from,(char*)to,n); }

  
  static inline long dataSize(long t, long x)  /* like A+'s Tt macro */
   {
#if defined(MS_64BIT)
     return ((x)<<((((t>>1)&1)+3)&3)) + (t==CHARACTERTYPE);
#else
     return ((x)<<(t+2&3)) + (t==CHARACTERTYPE);
#endif
   }
protected:
  inline unsigned lastAxis(void) const;
  unsigned allButLastAxis(void) const;

  static MSA::a * gpick(MSA::a *apick_, MSA::a *aobj_);
  static MSA::a * gp_num(MSA::a *apick_, MSA::a *aobj_);
  static MSA::a * gp_nested(MSA::a *apick_, MSA::a *aobj_);
  static MSA::a * gp_scalar(long idx_,MSA::a *aobj_);
  static long gpi_sym(long key_,MSA::a *aobj_);
  static long gpi_num(MSA::a *apick_, MSA::a *aobj_);
  static long gpu_fillivec(long *ivec_, MSA::a *aobj_);
  static long pcki(int i_,MSA::a *a_);

  static void fillExportBuffer(MSA::a * aobj_, char *hp_, long hsz_, char *trp_, long for_a_,int);
  static MSA::a * extractpass(char **hpp_, char **dpp_, char *endp_, long *erp_, char *trp_, int);
  static void fillpass(MSA::a * aobj_, char **hpp_, char **dpp_, char *trp_, long for_a_,int);
  static long sizepass(MSA::a * aobj_, long *hszp_, long *dszp_,int);

  static MSA::a *copyAStruct(MSA::a *);
};

inline unsigned MSA::lastAxis(void) const
{ return (_aStructPtr==0) ? 0: _aStructPtr->d[_aStructPtr->r-1]; }

inline MSA::a *MSA::aStructPtr(void) const
{ return _aStructPtr; }

inline unsigned MSA::numberOfElements(void) const
{ return (_aStructPtr==0) ? 0 : _aStructPtr->n;}

inline unsigned MSA::rank(void) const
{ return(_aStructPtr==0)  ? 0 : _aStructPtr->r; }

typedef MSA::a MSAStruct;

#endif
