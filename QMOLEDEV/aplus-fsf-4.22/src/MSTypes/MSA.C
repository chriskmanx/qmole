///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////

#include <MSTypes/MSA.H>
#include <MSTypes/MSMessageLog.H>
#include <MSTypes/MSException.H>
#include <MSTypes/MSMMap.H>
#if HAVE_FSTREAM
#include <fstream>
#else
#include <fstream.h>
#endif
#include <stdlib.h>
#include <sys/types.h>
#include <unistd.h>
#include <fcntl.h>
#include <string.h>
#include <netinet/in.h>

#if defined(MS_NEED_FSYNC_DECLARATION)
extern "C" int fsync(int);
#endif

#define CDRXRHOLEN 4
#define CDRRTLEN 1
#define CDRRLLEN 1
#define CDRRANKLEN 2
#define CDRRHOLEN 4
#define CDRFLAGSLEN 1
#define CDRLENLEN 3
#define CDRFLAGSMAGIC  0x80
#define CDRFLAGS64     0x01
#define CDRFLAGSLITTLE 0x02
#define MINIMPLEN 4

/* This are from A+, and are copied for simplification of "code-sharing". */
typedef long  I;
typedef double F;
typedef char  C;

typedef MSAStruct * A;

#define Tt32(t,x) ((x)<<(t+2&3))
#define Tt64(t,x) ((x)<<((((t>>1)&1)+3)&3))
#if (_MIPS_SZLONG == 64) || defined(__alpha) || defined(__ia64)
#define Tt(t,x) ((x)<<((((t>>1)&1)+3)&3))
#else
#define Tt(t,x) ((x)<<(t+2&3))
#endif

#define aplusMask 7
#define QA(a) (0==((I)(a)&aplusMask))
#define QS(a) (a&&(a->t==STRINGTYPE))

#define It  INTEGERTYPE
#define Ft  FLOATTYPE
#define Ct  CHARACTERTYPE
#define Et  ETYPE

/*
 * This is a 64BIT-safe export struct header.
 */
struct impexpHeader
{
  int   xrho;
  char  rt;
  char  rl;
  short rank;
  int   d[MSA::MAXRANK];
};

/* template helper function */
template <class FromType, class ToType >
static MSTypeData<ToType,MSAllocator<ToType> > *
msaConvertData(MSA::a *aStructPtr, FromType *, ToType *)
{
  MSTypeData<ToType,MSAllocator<ToType> > *d=0;
  if (aStructPtr!=0)
   {
     unsigned count_=(unsigned)aStructPtr->n;
     d = MSTypeData<ToType,MSAllocator<ToType> >::allocateWithLength(count_);
     ToType *dp=d->data();
     FromType *data=(FromType*)(aStructPtr->p);
     for (unsigned i=0;i<count_;i++) dp[i]=(ToType)data[i];
   }
  return d;
}



MSA::MSA(void) 
{ 
  _aStructPtr=0;
}

MSA::MSA(long t_,long r_,long n_,long *d_)
{
  _aStructPtr=0;
  if (t_!=STRINGTYPE&&t_!=MSAInvalidType&&t_!=MSANull)
   {
     aStructPtr(ga(t_,r_,n_,d_));
     if (t_==ETYPE)
      {
        for (long i=0;i<n_;i++) _aStructPtr->p[i]=0;
      }
   }
}

MSA::MSA(MSAStruct * aStructPtr_,MSBoolean iced_)
{
  _aStructPtr=0;
  if (QS(aStructPtr_))
   {
     // we don't want any "naked" symbols so we need to enclose at top level
     long d[MAXRANK]={ 0,0,0,0,0,0,0,0,0 };
     aStructPtr(ga(ETYPE,0,1,d));
     if (_aStructPtr!=0)
      {
        *_aStructPtr->p=(long) aStructPtr_;
      }
   }
  else 
   {
     aStructPtr(aStructPtr_);
   }
  if (_aStructPtr!=0&&iced_==MSFalse)
   {
     // ic the a object passed in, do not overwrite the
     // internal astruct data member as it may not be equal
     // to the one passed in, i.e. STRINGTYPE above.
     (void)ic(aStructPtr_);
   }
}

MSA::MSA(const MSA& aObject_)
{
  _aStructPtr=0;
  _aStructPtr=copyAStruct(aObject_._aStructPtr);
}

MSA::~MSA(void)
{ 
  if (_aStructPtr) MSA::dc(_aStructPtr); 
}

MSA::MSA(char msg_)
{
  _aStructPtr=0;
  long d[MAXRANK]={ 1,0,0,0,0,0,0,0,0 };
  aStructPtr(ga(CHARACTERTYPE,0,1,d));
  if (_aStructPtr)
   {
     char *data=(char *)_aStructPtr->p;
     data[0]=msg_;
   }
}

MSA::MSA(long msg_)
{
  _aStructPtr=0;
  long d[MAXRANK]={ 1,0,0,0,0,0,0,0,0 };
  aStructPtr(ga(INTEGERTYPE,0,1,d));
  if (_aStructPtr)
   {
     long *data=_aStructPtr->p;
     data[0]= (long) msg_;
   }
}

MSA::MSA(unsigned msg_)
{
  _aStructPtr=0;
  long d[MAXRANK]={ 1,0,0,0,0,0,0,0,0 };
  aStructPtr(ga(INTEGERTYPE,0,1,d));
  if (_aStructPtr)
   {
     long *data=_aStructPtr->p;
     data[0]= (long) msg_;
   }
}

MSA::MSA(double msg_)
{
  _aStructPtr=0;
  long d[MAXRANK]={ 1,0,0,0,0,0,0,0,0 };
  aStructPtr(ga(FLOATTYPE,0,1,d));
  if (_aStructPtr)
   {
     double *data=(double *)_aStructPtr->p;
     data[0]=(double)msg_;
   }
}

MSA::MSA(const MSInt& msg_)
{
  _aStructPtr=0;
  long d[MAXRANK]={ 1,0,0,0,0,0,0,0,0 };
  aStructPtr(ga(INTEGERTYPE,0,1,d));
  if (_aStructPtr)
   {
     long *data=_aStructPtr->p;
     data[0]=(long)((int)msg_);
   }
}

MSA::MSA(const MSFloat& msg_)
{
  _aStructPtr=0;
  long d[MAXRANK]={ 1,0,0,0,0,0,0,0,0 };
  aStructPtr(ga(FLOATTYPE,0,1,d));
  if (_aStructPtr)
   {
     double *data=(double *)_aStructPtr->p;
     data[0]=(double)msg_;
   }
}

MSA::MSA(const MSString& msg_)
{
  _aStructPtr=0;
  long d[MAXRANK]={ 0,0,0,0,0,0,0,0,0 };
  d[0]=msg_.length();
  aStructPtr(gc(CHARACTERTYPE,1,msg_.length(),d,(long *)msg_.string()));
}

MSA::MSA(const MSSymbol& msg_,MSBoolean encloseSymbols_)
{
  _aStructPtr=0;
  long d[MAXRANK]={ 0,0,0,0,0,0,0,0,0 };
  if (encloseSymbols_==MSTrue)
   {
     aStructPtr(ga(ETYPE,0,1,d));
     if (_aStructPtr) _aStructPtr->p[0] = si((char *) msg_.symbolName());
   }
  else _aStructPtr= (MSAStruct *) si((char *) msg_.symbolName());
}

MSA::MSA(const MSIntVector& msg_)
{
  _aStructPtr=0;
  long d[MAXRANK]={ 0,0,0,0,0,0,0,0,0 };
  long xrho=msg_.length();
  d[0]=xrho;
  int *dp=msg_.data();
  aStructPtr(ga(INTEGERTYPE,1,xrho,d));
  if (_aStructPtr)
   {
     long *data=_aStructPtr->p;
     for (long i=0; i<xrho; i++) data[i]= dp[i];
   }
}

MSA::MSA(const MSFloatVector& msg_)
{
  _aStructPtr=0;
  long d[MAXRANK]={ 0,0,0,0,0,0,0,0,0 };
  long xrho=msg_.length();
  d[0]=xrho;
  double *dp=msg_.data();
  aStructPtr(ga(FLOATTYPE,1,xrho,d));
  if (_aStructPtr)
   {
     double *data=(double *)_aStructPtr->p;
     for (long i=0; i<xrho; i++) data[i]= dp[i];
   }
}

MSA::MSA(const MSSymbolVector& msg_,MSBoolean encloseSymbols_)
{
  _aStructPtr=0;
  long d[MAXRANK]={ 0,0,0,0,0,0,0,0,0 };
  long xrho=msg_.length();
  d[0]=xrho;
  aStructPtr(ga(ETYPE,1,xrho,d));
  if (_aStructPtr) 
   {
     for (long i=0; i<xrho; i++)
      {
        if (encloseSymbols_==MSTrue)
         {
           d[0]=0;
           _aStructPtr->p[i]=(long) ga(ETYPE,0,1,d);
           if (_aStructPtr->p[i])
         ((MSAStruct *) _aStructPtr->p[i])->p[0]=si((char *) msg_.elementAt(i).symbolName());
         }
        else
         {
           _aStructPtr->p[i]=si((char *) msg_.elementAt(i).symbolName());
         }
      }
   }
}


MSA::MSA(const MSStringVector& msg_)
{
  _aStructPtr=0;
  long d[MAXRANK]={ 0,0,0,0,0,0,0,0,0 };
  long xrho=msg_.length();
  d[0]=xrho;
  aStructPtr(ga(ETYPE,1,xrho,d));
  if (_aStructPtr)
   {
     for (long i=0; i<xrho; i++)
      {
        d[0]=msg_.elementAt(i).length();
        _aStructPtr->p[i]=(long) gc(CHARACTERTYPE,1,msg_.elementAt(i).length(),d,(long *)msg_.elementAt(i).string());
      }
   }
}

MSA::MSA(const MSCharMatrix& msg_)
{
  _aStructPtr=0;
  long d[MAXRANK]={ 0,0,0,0,0,0,0,0,0 };
  d[0]=msg_.rows();
  d[1]=msg_.columns();
  long xrho=msg_.length();
  char *dp=msg_.data();
  aStructPtr(ga(CHARACTERTYPE,2,xrho,d));
  if (_aStructPtr)
   {
     char *data=(char *)_aStructPtr->p;
     for (long i=0; i<xrho; i++) data[i]= dp[i];
   }
}

MSA::MSA(const MSIntMatrix& msg_)
{
  _aStructPtr=0;
  long d[MAXRANK]={ 0,0,0,0,0,0,0,0,0 };
  d[0]=msg_.rows();
  d[1]=msg_.columns();
  long xrho=msg_.length();
  int *dp=msg_.data();
  aStructPtr(ga(INTEGERTYPE,2,xrho,d));
  if (_aStructPtr)
   {
     long *data=_aStructPtr->p;
     for (long i=0; i<xrho; i++) data[i]= dp[i];
   }
}

MSA::MSA(const MSFloatMatrix& msg_)
{
  _aStructPtr=0;
  long d[MAXRANK]={ 0,0,0,0,0,0,0,0,0 };
  d[0]=msg_.rows();
  d[1]=msg_.columns();
  long xrho=msg_.length();
  double *dp=msg_.data();
  aStructPtr(ga(FLOATTYPE,2,xrho,d));
  if (_aStructPtr)
   {
     double *data=(double *)_aStructPtr->p;
     for (long i=0; i<xrho; i++) data[i]= dp[i];
   }
}

MSA::MSA(const MSMMap& aMap_, MSBoolean import_)
{
  _aStructPtr=0;
  if(aMap_.isValid()==MSTrue)
   {
     if(import_==MSTrue) import(aMap_);
     else
      {
	_aStructPtr=copyAStruct(aMap_.aplusData());
      } 
   }
}


MSString MSA::asDebugInfo(void) const
{
  MSString result("MSA(@");
  result+=MSString((unsigned long)this).d2x().lowerCase();
  result+=",_rank=";
  result+=MSString(rank());
  result+=",_numberOfelements=";
  result+=MSString(numberOfElements());
  result+=",_shape=";
  result+=MSString(shape().asString());
  // we don't want shape().asDebugInfo since
  // we really only want to see the vector data 
  // here.
  result+=",_type=";
  result+=MSString((int)aPlusType());
  result+=",_depth=";
  result+=MSString(depth());
  result+=")";
  return result;
}

void MSA::aStructPtr(MSAStruct *aStructPtr_)
{ 
  if (0!=_aStructPtr) dc(_aStructPtr); 
  _aStructPtr=aStructPtr_; 
}

MSBoolean MSA::isNullMSA(void) const
{
  if (0!=aStructPtr()) return MSFalse;
  else return MSTrue;
}

unsigned MSA::allButLastAxis(void) const
{ 
  unsigned dims(1); 
  if (_aStructPtr)
   {
     MSIntVector s=shape();
     long r=rank();
     if (r==0) return 0; 
     if (r==1) return(s(0)?s(0):1);
     for (long i=0;i<r-1;i++) dims=dims*_aStructPtr->d[i]; 
   }
  return dims; 
}

void MSA::simpleSpecify(const MSIndexVector& location_,const MSA& aObjectB_)
{
  unsigned index,product;
  if (_aStructPtr&&aObjectB_._aStructPtr&&_aStructPtr->t==ETYPE&& _aStructPtr->n!=0&&!(_aStructPtr->n==0&&_aStructPtr->r>0)) 
   {
     long r=rank();
     MSIntVector s=shape();
     if (_aStructPtr->c > 1)
      {
        aStructPtr(gc(_aStructPtr->t,
                      _aStructPtr->r,_aStructPtr->n,
                      _aStructPtr->d,_aStructPtr->p));
      }
     if (r==0) 
      {
	dc((MSAStruct *)_aStructPtr->p[0]);
	_aStructPtr->p[0]=(long) ic(aObjectB_._aStructPtr);
	return;
      }
     index=location_(0);
     if ((r==1)&&index) 
      { 
	dc((MSAStruct *)_aStructPtr->p[index]);
	_aStructPtr->p[index]=(long) ic(aObjectB_._aStructPtr);
	return; 
      }
     else
      {
        index=location_(r-1);
        product=1;
        for (long i=r-2; i>=0; i--) 
         {
           product=product*(unsigned) s(i+1);
           index=index+product*(unsigned) location_(i);
         }
	dc((MSAStruct *)_aStructPtr->p[index]);
        _aStructPtr->p[index]=(long) ic(aObjectB_._aStructPtr);	
      }
   }
}


void MSA::iterativeSpecify(const MSIndexVector& location_,const MSA& aObjectB_)
{
  unsigned length=location_.length();
  MSAStruct **index;
  index=&_aStructPtr;
  for (unsigned i=0;i<length;i++)
   {
     if ((*index)==0||(*index)->t!=ETYPE||
         (*index)->n==0||((*index)->n==0&&(*index)->r>0))  return;
     if ((*index)->c!=1)
      {
        MSAStruct *tmp=gc((*index)->t,(*index)->r,(*index)->n,
                          (*index)->d,(*index)->p);
        dc(*index);
        *index=tmp;
      }
     if ((*index)->r==0) index=(MSAStruct **) &((*index)->p[0]);
     if ((*index)->r >= 1) index=(MSAStruct **) &((*index)->p[location_(i)]);
   }
  dc(*index);
  *index=ic(aObjectB_._aStructPtr);
}

MSA MSA::simplePick(unsigned pickIndex_) const
{
  if ( _aStructPtr==0||_aStructPtr->t!=ETYPE||
       _aStructPtr->n==0||(_aStructPtr->n==0&&_aStructPtr->r>0))
return MSA();
  long r=rank();
  if (r==0&&((MSAStruct *) _aStructPtr->p[0])->t!=STRINGTYPE) return MSA((MSAStruct *) _aStructPtr->p[0]);
  else if (r>=1) return MSA((MSAStruct *) _aStructPtr->p[pickIndex_]);
  return MSA();
}

MSA MSA::simplePick(const MSIndexVector& pickIndex_) const
{
  unsigned I,product;
  if (_aStructPtr==0||_aStructPtr->t!=ETYPE||
      _aStructPtr->n==0||(_aStructPtr->n==0&&_aStructPtr->r>0)) return MSA();
  long r=rank();
  MSIntVector s=shape();
  if (r==0&&((MSAStruct *) _aStructPtr->p[0])->t!=STRINGTYPE) return MSA((MSAStruct *) _aStructPtr->p[0]);
  if (r==1&&pickIndex_.length()==1)
   {
     I=pickIndex_(0);
     return MSA((MSAStruct *) _aStructPtr->p[I]);
   }
  if (r>1&&pickIndex_.length()==r)
   {
     I=pickIndex_(r-1);
     product=1;
     for (long i=r-2; i>=0; i--)
      {
	product=product*(unsigned) s(i+1);
	I=I+product*(unsigned) pickIndex_(i);
      }
     return MSA((MSAStruct *) _aStructPtr->p[I]);
   }
  return MSA();
}

MSA MSA::complexPick(const MSA& pickIndex_) const
{
  if (_aStructPtr) return MSA(gpick(pickIndex_._aStructPtr,_aStructPtr),MSTrue);
  else return MSA();
}


MSA MSA::iterativePick(const MSIndexVector& pickIndex_) const
{
  unsigned length=pickIndex_.length();
  MSAStruct * const *index;
  index=&(_aStructPtr);
  for (unsigned i=0;i<length;i++)
   {
     if (*index==0||(*index)->t!=ETYPE||
         (*index)->n==0||((*index)->n==0&&(*index)->r>0)) return MSA();
     if ((*index)->r==0) index=(MSAStruct **) &((*index)->p[0]);
     else 
      {
        if ((*index)->r >= 1) index=(MSAStruct **) &((*index)->p[pickIndex_(i)]);
      }
   }
  return MSA(*index);
}

MSIntVector MSA::shape(void) const
{ 
  if (_aStructPtr==0) return MSIntVector();
  MSIntVector aShape(_aStructPtr->r); 
  for (long i=0; i<_aStructPtr->r; i++) aShape.replaceAt(i,_aStructPtr->d[i]);
  return aShape; 
}

static long dp(MSAStruct *aobj_)
{
  long k=0;
  long depth=0;
  if (aobj_==0) return(depth);
  long t=aobj_->t;
  if (t==MSA::STRINGTYPE) return(-1);
  if ((t==MSA::INTEGERTYPE)||(t==MSA::FLOATTYPE)||(t==MSA::CHARACTERTYPE)) return(0);
  else 
   {
     long i=0;
     long xrho=(aobj_->n);
     for (; i < xrho; ++i) 
      {
        if ((k = 1 + (dp((MSAStruct *)aobj_->p[i]))) > depth) depth = k;
      }
   }
  return depth;
}

long MSA::depth(void) const
{
  long depth=0;
  if (_aStructPtr!=0)
   {
     long t=_aStructPtr->t;
     if (t==STRINGTYPE) depth=-1;
     else if (t==ETYPE &&_aStructPtr->n!=0) depth=dp(_aStructPtr);
   }
  return depth;
}

MSA& MSA::operator=(const MSA& aObject_)
{
  if (this!=&aObject_)
    {
      if (_aStructPtr!=0)
	{
	  dc(_aStructPtr);
	}

      _aStructPtr=copyAStruct(aObject_._aStructPtr);
    }

  return *this;
}

char MSA::asChar(void) const
{
  if (_aStructPtr&&_aStructPtr->t==CHARACTERTYPE)
   {
     return (char)(((char *)_aStructPtr->p)[0]);
   }
  return char('\0');
}

long MSA::asLong(void) const
{
  long d=0;
  if (_aStructPtr)
   {
     if (_aStructPtr->t==INTEGERTYPE) return ((long)*((long *)_aStructPtr->p));
     if (_aStructPtr->t==CHARACTERTYPE) return ((long)*((char *)_aStructPtr->p));
   }
  return d;
}

unsigned long MSA::asUnsignedLong(void) const
{
  unsigned long d=0;
  if (_aStructPtr)
   {
     if (_aStructPtr->t==INTEGERTYPE) return ((unsigned long) *((long *)_aStructPtr->p));
     if (_aStructPtr->t==CHARACTERTYPE) return ((unsigned long) *((char *)_aStructPtr->p));
   }
  return d;
}

double MSA::asDouble(void) const
{
  double d=0;
  if (_aStructPtr)
   {
     if (_aStructPtr->t==FLOATTYPE) return ((double)*((double *)_aStructPtr->p));
     if (_aStructPtr->t==INTEGERTYPE) return ((double)*((long *)_aStructPtr->p));
     if (_aStructPtr->t==CHARACTERTYPE) return ((double)*((char *)_aStructPtr->p));
   }
  return d;
}

MSInt MSA::asMSInt(void) const
{
  if (_aStructPtr)
   {
     if (_aStructPtr->t==INTEGERTYPE) return MSInt((int)*(long *)_aStructPtr->p);
     if (_aStructPtr->t==CHARACTERTYPE) return MSInt((int)*(char *)_aStructPtr->p);
   }
  return MSInt();
}

MSFloat MSA::asMSFloat(void) const
{
  if (_aStructPtr)
   {
     if (_aStructPtr->t==FLOATTYPE) return MSFloat((double)*((double *)_aStructPtr->p));
     if (_aStructPtr->t==INTEGERTYPE) return MSFloat((double)*((long *)_aStructPtr->p));
   }
  return MSFloat();
}

MSSymbol MSA::asMSSymbol(void) const
{
  if (_aStructPtr)
   {
     if (_aStructPtr->t==STRINGTYPE)
      {
        return MSSymbol((char *)(_aStructPtr->p));
      }
     if (_aStructPtr->t==ETYPE&& _aStructPtr->n!=0 &&((MSAStruct *) *_aStructPtr->p)->t==STRINGTYPE)
      {
        return MSSymbol((const char *)((MSAStruct *) *_aStructPtr->p)->p);
      }
     if ( _aStructPtr->t==ETYPE&& _aStructPtr->n!=0 &&
          ((MSAStruct *) *_aStructPtr->p)->t==ETYPE&& 
          ((MSAStruct *) *_aStructPtr->p)->n!=0&&
          ((MSAStruct *) *(((MSAStruct *) _aStructPtr->p))->p)->t==STRINGTYPE)
      {
        return MSSymbol((const char *) 
                        ((MSAStruct *) *(((MSAStruct *) _aStructPtr->p))->p)->p);
      }
   }
  return MSSymbol();
}

MSString MSA::asMSString(void) const
{
  if (_aStructPtr)
   {
     if (_aStructPtr->t==STRINGTYPE)
      {
        return MSString((char *)(_aStructPtr->p),_aStructPtr->n);
      }
     if (_aStructPtr->t==CHARACTERTYPE)
      {
        return MSString((char *)(_aStructPtr->p),_aStructPtr->n);
      }
     if (_aStructPtr->t==ETYPE&& _aStructPtr->n!=0&&((MSAStruct *) *_aStructPtr->p)->t==STRINGTYPE)
      {
        return MSString((const char *)((MSAStruct *) *_aStructPtr->p)->p,((MSAStruct *) *_aStructPtr->p)->n);
      }
     if ( _aStructPtr->t==ETYPE&& _aStructPtr->n!=0 &&
          ((MSAStruct *) *_aStructPtr->p)->t==ETYPE&& 
          ((MSAStruct *) *_aStructPtr->p)->n!=0&&
          ((MSAStruct *) *(((MSAStruct *) _aStructPtr->p))->p)->t==STRINGTYPE)
      {
        return MSString((const char *) 
                        ((MSAStruct *) *(((MSAStruct *) _aStructPtr->p))->p)->p,((MSAStruct *) *(((MSAStruct *) _aStructPtr->p))->p)->n);
      }
   }
  return MSString();
}

MSIntMatrix MSA::asMSIntMatrix(void) const
{ 
  if (_aStructPtr&&_aStructPtr->t==INTEGERTYPE&&_aStructPtr->r>=2)
   {
     MSTypeData<int,MSAllocator<int> > *d=msaConvertData(_aStructPtr,(long*)0,(int*)0);
     return MSIntMatrix(d,allButLastAxis(),lastAxis());
   }
  else return MSIntMatrix();
}

MSLongMatrix MSA::asMSLongMatrix(void) const
{
  if (_aStructPtr&&_aStructPtr->t==INTEGERTYPE&&_aStructPtr->r>=2)
   {
     MSTypeData<long,MSAllocator<long> > *d=msaConvertData(_aStructPtr,(long*)0,(long*)0);
     return MSLongMatrix(d,allButLastAxis(),lastAxis());
   }
  else return MSLongMatrix();
}

MSUnsignedLongMatrix MSA::asMSUnsignedLongMatrix(void) const
{
  if (_aStructPtr&&_aStructPtr->t==INTEGERTYPE&&_aStructPtr->r>=2)
   {
     MSTypeData<unsigned long,MSAllocator<unsigned long> > *d=
       msaConvertData(_aStructPtr,(long*)0,(unsigned long*)0);
     return MSUnsignedLongMatrix(d,allButLastAxis(),lastAxis());
   }
  else return MSUnsignedLongMatrix();
}

MSCharMatrix MSA::asMSCharMatrix(void) const
{
  if (_aStructPtr&&_aStructPtr->t==CHARACTERTYPE&&_aStructPtr->r>=2)
   {
     MSTypeData<char,MSAllocator<char> > *d=msaConvertData(_aStructPtr,(long *)0,(char*)0);
     return MSCharMatrix(d,allButLastAxis(),lastAxis());
   }
  else return MSCharMatrix();
}

MSFloatMatrix MSA::asMSFloatMatrix(void) const
{
  MSTypeData<double,MSAllocator<double> > *d=0;
  if (_aStructPtr&&_aStructPtr->r>=2)
    {
      if(_aStructPtr->t==FLOATTYPE) d= msaConvertData(_aStructPtr,(double*)0,(double*)0);
      else if(_aStructPtr->t==INTEGERTYPE) d=msaConvertData(_aStructPtr,(long*)0,(double*)0);
    }
  if(d!=0) return MSFloatMatrix(d,allButLastAxis(),lastAxis());
  else return MSFloatMatrix();
}

MSIntVector MSA::asMSIntVector(void) const
{
  MSTypeData<int,MSAllocator<int> > *d=0;
  unsigned n=0;
  if (_aStructPtr)
   {
     n=_aStructPtr->n;
     if(_aStructPtr->t==INTEGERTYPE) d=msaConvertData(_aStructPtr,(long*)0,(int*)0);
     else if(_aStructPtr->t==CHARACTERTYPE) d=msaConvertData(_aStructPtr,(char*)0,(int*)0);
   }
  if(d!=0) return MSIntVector(d,n);
  else return MSIntVector();
}

MSUnsignedLongVector MSA::asMSUnsignedLongVector(void) const
{
  if (_aStructPtr&&_aStructPtr->t==INTEGERTYPE)
   {
     unsigned n=_aStructPtr->n;
     MSTypeData<unsigned long,MSAllocator<unsigned long> > *d=msaConvertData(_aStructPtr,(long*)0,(unsigned long*)0);
     return MSUnsignedLongVector(d,n);
   }
  else return MSUnsignedLongVector();
}

MSLongVector MSA::asMSLongVector(void) const
{
  if (_aStructPtr&&_aStructPtr->t==INTEGERTYPE)
   {
     unsigned n=_aStructPtr->n;
     MSTypeData<long,MSAllocator<long> > *d=msaConvertData(_aStructPtr,(long*)0,(long*)0);
     return MSLongVector(d,n);
   }
  else return MSLongVector();
}

MSCharVector MSA::asMSCharVector(void) const
{
  if (_aStructPtr&&_aStructPtr->t==CHARACTERTYPE) 
   {
     unsigned n=_aStructPtr->n;
     MSTypeData<char,MSAllocator<char> > *d=msaConvertData(_aStructPtr,(char*)0,(char*)0);
     return MSCharVector(d,n);
   }
  else return MSCharVector();
}

MSFloatVector MSA::asMSFloatVector(void) const
{
  MSTypeData<double,MSAllocator<double> > *d=0;
  unsigned n=0;
  if (_aStructPtr)
   {
     n=_aStructPtr->n;
     if(_aStructPtr->t==FLOATTYPE) d=msaConvertData(_aStructPtr,(double*)0,(double*)0);
     else if(_aStructPtr->t==INTEGERTYPE) d=msaConvertData(_aStructPtr,(long*)0,(double*)0);
   }
  if (d!=0) return MSFloatVector(d,n);
  else return MSFloatVector();
}

MSStringVector MSA::asMSStringVector(void) const
{
  if (_aStructPtr)
   {
     unsigned n=_aStructPtr->n;
     MSStringVector d;
     if (_aStructPtr->t==CHARACTERTYPE) 
      {
        if (_aStructPtr->r==0||_aStructPtr->r==1)
         {
//turn a char vector or 1 char into an MSStringvector of one string
           d<<MSString((char *) _aStructPtr->p,_aStructPtr->n);
           return d;
         }
        else 
         {
           if (_aStructPtr->r >1)
            {
              unsigned rows=allButLastAxis(); 
              // loop over number of rows - will
              // handle multiple dimensions by treating them
              // as two dimensional arrays
              unsigned cols=lastAxis();
              for (unsigned i=0;i<rows;i++)  d<<MSString(((char *) _aStructPtr->p)+i*cols,cols);
              return d;
            }
         }
      }
     else 
      {
        if (_aStructPtr->t==ETYPE)
         {
           for (unsigned i=0;i<n;i++) 
            {
              if (((MSAStruct *) (_aStructPtr->p[i]))->t==CHARACTERTYPE)
	       {
                 d<<MSString((char *) ((MSAStruct *) (_aStructPtr->p[i]))->p,((MSAStruct *) (_aStructPtr->p[i]))->n);
	       }
              else
	       {
                 return MSStringVector(); // whole vector is illegal,not all CHARACTERTYPE
	       }
            }
           return d;
         }
      }
   }
  return MSStringVector();
}

MSSymbolVector MSA::asMSSymbolVector(void) const
{
  if (_aStructPtr)
   {
     unsigned n=_aStructPtr->n;
     MSSymbolVector d;
     if (_aStructPtr->t==STRINGTYPE)
      {
        d<<MSSymbol((char *) (_aStructPtr->p));
        return d;
      }
     else 
      {
        if (_aStructPtr->t==ETYPE)
         {
           for (unsigned i=0;i<n;i++)
            {
              if (((MSAStruct *) (_aStructPtr->p[i]))->t==STRINGTYPE)
               {
                 d<<MSSymbol((const char *)((MSAStruct *) (_aStructPtr->p[i]))->p);
               }
              else return MSSymbolVector(); // whole vector is illegal,not all CHARACTERTYPE
            }
           return d;
         }
      }
   }
  return MSSymbolVector();
}

MSA::MSAplusType MSA::aPlusType(void) const
{
  if (_aStructPtr!=0)
   {
     switch(_aStructPtr->t)
      {
      case INTEGERTYPE:       return MSAInt;
      case FLOATTYPE:         return MSAFloat;
      case CHARACTERTYPE:     return MSAChar;
      case STRINGTYPE:        return MSAInvalidType;
      case ETYPE:
      {
        if (_aStructPtr->n==0&&_aStructPtr->r>0) return MSANull;
        if (((MSAStruct *)_aStructPtr->p[0]) &&
            ((MSAStruct *)_aStructPtr->p[0])->t==STRINGTYPE)
         {
           // this is a symbol with rank==0 or a symbol vector
           return MSASymbol;
         }
        if (((MSAStruct *)_aStructPtr->p[0]) &&
            (((MSAStruct *)_aStructPtr->p[0])->t==INTEGERTYPE ||
             ((MSAStruct *)_aStructPtr->p[0])->t==FLOATTYPE ||
             ((MSAStruct *)_aStructPtr->p[0])->t==CHARACTERTYPE ||
             ((MSAStruct *)_aStructPtr->p[0])->t==ETYPE)) return MSABox;
        break;
      }
      }
     return MSAInvalidType;
   }
  return MSANull;
}


long *MSA::mab(long i_)
{
  return (long *) MSA::balloc(i_);
}

long *MSA::ma(long i_)
{
  return (long *) MSA::balloc(i_*sizeof(long));
}

void MSA::mf(long *p_)
{
  (void) MSA::bfree((char *)p_);
}

MSAStruct *MSA::ic(MSAStruct * aobj_)
{
  MSAStruct *r=aobj_;;
  
  if(aobj_ && QA(aobj_))
   {
      if(aobj_->c==0) r=gc(aobj_->t,aobj_->r,aobj_->n,aobj_->d,aobj_->p);
      else { ++r->c; }
   }
  return r;
}

void MSA::dc(MSAStruct * aobj_)
{
  if(aobj_ && QA(aobj_))
   {
    if(aobj_->c==0) return;
    if((--aobj_->c)==0) MSA::dec(aobj_);
   }
}

void MSA::dec(MSAStruct * aobj_)
{
  if (aobj_==0) return;
  aobj_->c=-1;
  if(aobj_->t==ETYPE)
   {
     long i=0,_i=(aobj_->n);
     for(;i<_i;++i)
      {
        MSA::dc((MSAStruct *)(aobj_->p[i]));
      }
   }
  mf((long *)aobj_);
}

void MSA::mv(long *d_,long *s_,long n_)
{
  long i=0,_i=(n_);
  for(;i<_i;++i)
   {
     *d_++=*s_++;
   }
}

char *MSA::tmv(long t_,long *d_,long *s_,long n_)
{
  switch(t_) 
   {
   case INTEGERTYPE:
   {
     long i=0,_i=(n_);
     for(;i<_i;++i)
      {
        *d_++=*s_++;
      }
   }
   return(char*)d_ ;

   case ETYPE:
   { 
     MSAStruct **aobj=(MSAStruct **)d_;
     MSAStruct **b=(MSAStruct **)s_;
   {
     long i=0,_i=(n_);
     for(;i<_i;++i)
      {
        *aobj++=MSA::ic( (MSAStruct *) *b++);
      }
   }
   return (char*) aobj ;
   };

   case FLOATTYPE:
   {
     double *aobj=(double*)d_;
     double *b=(double*)s_;
   {
     long i=0,_i=(n_);
     for(;i<_i;++i){*aobj++=*b++;}
   }
   return (char*) aobj;
   };

   case CHARACTERTYPE:
   {
     char *aobj=(char*)d_;
     char *b=(char*)s_;
   {
     long i=0,_i=(n_);
     for(;i<_i;++i)
      {
        *aobj++=*b++;
      }
   }
   return (char*) aobj;
   };

   case STRINGTYPE:
   {
     char *aobj=(char*)d_;
     char *b=(char*)s_;
   {
     long i=0,_i=(n_);
     for(;i<_i;++i)
      {
        *aobj++=*b++;
      }
   }
   return (char*) aobj;
   };

   }
  return 0;
}

MSAStruct *MSA::gm(long t_,long m_,long n_)
{	
  MSAStruct * z=(MSAStruct *)mab(AHEADER+dataSize(t_,m_*n_));
  z->c=1,z->t=t_,z->r=2,z->n=m_*n_;
  *z->d=m_;
  z->d[1]=n_;
  if (t_==CHARACTERTYPE) ((char*)z->p)[m_*n_]=0;
  return z;
}	

MSAStruct *MSA::gv(long t_,long n_)
{
  MSAStruct * z=(MSAStruct *)mab(AHEADER+dataSize(t_,n_));
  z->c=1,z->t=t_,z->r=1,z->n=n_;
  *z->d=n_;
  if (t_==CHARACTERTYPE) ((char*)z->p)[n_]=0;
  return z;
}

MSAStruct *MSA::gd(long t_,MSAStruct * aobj_)
{
  MSAStruct * z=(MSAStruct *)mab(AHEADER+dataSize(t_,aobj_->n));
  z->c=1,z->t=t_,z->r=aobj_->r,z->n=aobj_->n;
  mv(z->d,aobj_->d,aobj_->r);
  if (t_==CHARACTERTYPE) ((char*)z->p)[aobj_->n]=0;
  return z;
}

MSAStruct *MSA::ga(long t_,long r_,long n_,long *d_)
{
  MSAStruct * z=(MSAStruct *)mab(AHEADER+dataSize(t_,n_));
  z->c=1,z->t=t_,z->r=r_,z->n=n_;
  mv(z->d,d_,r_);
  if (t_==CHARACTERTYPE) ((char*)z->p)[n_]=0;
  return z;
}

MSAStruct *MSA::gc(long t_,long r_,long n_,long *d_,long *p_)
{
  MSAStruct * z=(MSAStruct *)mab(AHEADER+dataSize(t_,n_));
  z->c=1,z->t=t_,z->r=r_,z->n=n_;
  memset((char *) z->d,0,MAXRANK*sizeof(long));
  mv(z->d,d_,r_);
  tmv(t_,z->p,p_,n_);
  if (t_==CHARACTERTYPE) ((char*)z->p)[n_]=0;
  return z;
}

MSAStruct *MSA::gi(long i_)
{
  MSAStruct * z=(MSAStruct *)mab(AHEADER+dataSize(INTEGERTYPE,1));
  z->c=1,z->t=INTEGERTYPE,z->r=0,z->n=1;
  *z->p=i_;
  return z;
}

MSAStruct *MSA::gf(double f_)
{
  MSAStruct * z=(MSAStruct *)mab(AHEADER+dataSize(FLOATTYPE,1));
  z->c=1,z->t=FLOATTYPE,z->r=0,z->n=1;
  *(double*)z->p=f_;
  return z;
}

MSAStruct *MSA::gs(long t_)
{
  MSAStruct * z=(MSAStruct *)mab(AHEADER+dataSize(t_,1));
  z->c=1,z->t=t_,z->r=0,z->n=1; 
  if(t_==CHARACTERTYPE)((char *)z->p)[1]=0;
  return z;
}

long MSA::si(char * s_)
{
  long n=strlen(s_);
  MSAStruct * z;
  z=gv(CHARACTERTYPE,n);
  strcpy((char *)z->p,s_);
  z->t=STRINGTYPE;
  return (long) z;
}

long MSA::longAt(char *c_)
{
  int r;
  memcpy((char *) &r,c_,4);
  r=ntohl(r);
  return r;
}

/* Description of exportAObject() sizepass() and  fillpass()
 * -------------------------------------------------------
 * This is to convert from A format data to Msvp format data.
 * The A object is converted into a character vector suitable
 * for transmission through msvp.  The return is a 1 or 2 element
 * enclosed vector.  The first element is a scaler integer
 * completion code.  The second element,if present,is a character
 * vector.  The second element is only present when the first
 * element is the scalar integer zero.
 */

/* importAObject() and extractpass()
 *
 * In a single traversal of the object,it should be possible to
 * convert from the CDR format to A objects.  The header of the
 * CDR message should indicate that it is of dense form.  Pointer
 * form messages are rejected.  The CDRDLEN field is used to
 * index into the message and locate the data that will fill
 * the a objects described by the descriptors.  Each descriptor
 * is handled in the extract pass function.  If the extract pass
 * is called for a general array descriptor,then it will be called
 * recursively for each element of the general array.  The conversion
 * from APL2 types to A types is as follows: B1,B4 and B8 are
 * converted to INTEGERTYPE.  I2 and I4 are converted to INTEGERTYPE.  E4 and E8 are
 * converted to FLOATTYPE.  E16 is rejected.  E4 and E8 are assumed to be in
 * IEEE floating point format.  J8,J16 and J32 are rejected.  C1
 * is converted to CHARACTERTYPE.  C4 is rejected.  A8 is rejected.  Pn is
 * rejected.  Zn is rejected.  X0 is ignored.  G0 is converted into ETYPE.
 */

/* 
   Export generates either I-8 or I-4 integers in 64-bit mode, and I-4 only
   in 32-bit mode.
   Import will read I-2, I-4, or I-8 (in 64 bit mode only).
*/
long MSA::sizepass(MSAStruct * a,long *hszp,long *dszp, int long_bytes) 
{
  register I t;
  
  if (QS(a))
    {
      /* turn symbol into character vector */
      //      S s = XS(a);      
      *hszp += (I)(CDRXRHOLEN+CDRRTLEN+CDRRLLEN+CDRRANKLEN+CDRRHOLEN);
      *dszp += strlen((char*)a->p);
      return (I)0;
    }
  else if (a && QA(a))
    {
      *hszp += (I)(CDRXRHOLEN+CDRRTLEN+CDRRLLEN+CDRRANKLEN) + (a->r * (I)CDRRHOLEN);
      if ((t = a->t) == Et)
	{
	  I rc, *p, n, i;
	  
	  if ((n = a->n) == (I)0)
	    {
	      /*descriptor length of prototype */
	      *hszp += (I)(2*(CDRXRHOLEN+CDRRTLEN+CDRRLLEN+CDRRANKLEN)+CDRRHOLEN);
	      return (I)0;
	    }
	  p = a->p;
	  for (i = (I)0; i < n; i++)
	    {
	      if (rc = sizepass((A)p[i], hszp, dszp, long_bytes))
		return rc;
	    }
	  return (I)0;
	}
      switch (t)
	{
	case It: {
#ifdef MS_64BIT
	  /* we going to bail out of the export here if we don't like the
	     size of the values stuffed in 4 byte longs */
	  I j;
	  /* check for data overflow now */
	  if(long_bytes < sizeof(I)) {
	    for(j=0; j < a->n; j++)
	      if(a->p[j] > INT_MAX || a->p[j] < INT_MIN) return(54);
	  }
#endif
	  *dszp += long_bytes * a->n; 
	  return (I)0;
	}
	case Ft: *dszp += (I)sizeof(F) * a->n; return (I)0;
	case Ct: *dszp += a->n; return (I)0;
	}
      return (I)54;
    }
  else
    {
      return (I)55;
    }
}

void MSA::fillpass(MSAStruct * a,char **hpp,char **dpp,char *trp,long for_a,int long_bytes)
{
  register char *hp, *dp;
  I xrho;
  register I t, dlen, hlen;
  register char rt, rl;
  
  impexpHeader h;
  
  hp = *hpp;
  dp = *dpp;
  
  if (QS(a))
    {
      /* turn symbol into character vector */
      //      S s = XS(a);
      //      xrho = strlen(s->n);
      xrho=strlen((char *) a->p);

      h.xrho = htonl(xrho);
      h.rt   = (for_a ? 'S' : 'C');
      h.rl   = 1;
      h.rank = htons((short)1);
      h.d[0] = htonl(xrho);

      hlen = CDRXRHOLEN+CDRRTLEN+CDRRLLEN+CDRRANKLEN+CDRRHOLEN;
      memcpy(hp,(char*)&h,hlen); hp+=hlen;

      /* CDRDSECT */
      if (trp != (char *)(0))
	{
	  char *src, *end_dp;
	  
	  for (src = (char*)a->p, end_dp = dp + xrho;
	       dp != end_dp;
	       *dp++ = trp[(unsigned)(*src++ & 0xff)]);
	}
      else
	{
	  memcpy(dp,(char*)a->p, (I)xrho); 
	  dp += xrho;
	}
      
      *hpp = hp;
      *dpp = dp;
      return;
    }
  
  xrho = a->n;
  t = a->t;
  switch (t)
    {
      /* changed the rl value for type It */
    case It: rt = 'I'; rl = long_bytes; dlen = (I)rl * xrho; break;
    case Ft: rt = 'E'; rl = sizeof(F); dlen = (I)rl * xrho; break;
    case Ct: rt = 'C'; rl = sizeof(C); dlen = (I)rl * xrho; break;
    case Et: rt = 'G'; rl = 0; break;
    }

  h.xrho = htonl(xrho);
  h.rt   = rt;
  h.rl   = rl;
  h.rank = htons((short)a->r);
#if defined(MS_64BIT) || defined(MS_LITTLE_ENDIAN)
  /* CDRRHO */
  for(int i=0; i< a->r; i++) {
    h.d[i] = htonl(a->d[i]);
  }
  hlen = CDRXRHOLEN+CDRRTLEN+CDRRLLEN+CDRRANKLEN+a->r*CDRRHOLEN;
  memcpy(hp,(char*)&h,hlen); hp+=hlen;
#else  
  /* 32-bit big endian optimization for eficiency so that not to copy a->d twice.*/
  hlen = CDRXRHOLEN+CDRRTLEN+CDRRLLEN+CDRRANKLEN;
  memcpy(hp,(char*)&h,hlen); hp+=hlen;
   
  hlen = (I)CDRRHOLEN * a->r;
  memcpy(hp,(char *)(a->d), hlen); hp += hlen;
#endif
  if (t == Et)
    {
      if (xrho == (I)0)
	{
	  h.xrho = htonl(1);
	  h.rt   = 'G';
	  h.rl   = 0;
	  h.rank = 0;
	  hlen = CDRXRHOLEN+CDRRTLEN+CDRRLLEN+CDRRANKLEN;
	  memcpy(hp,(char*)&h,hlen); hp+=hlen;
	  
	  h.xrho = 0;
	  h.rt   = 'I';
	  h.rl   = (char)long_bytes;
	  h.rank = htons(1);
	  h.d[0] = htonl(xrho);
	  hlen = CDRXRHOLEN+CDRRTLEN+CDRRLLEN+CDRRANKLEN +CDRRHOLEN;
	  memcpy(hp,(char*)&h,hlen); hp+=hlen;
	}
      *hpp = hp;
      *dpp = dp;
      for (int i = 0; i < xrho; i++)
	{
	  fillpass((A)a->p[i], hpp, dpp, trp, for_a,long_bytes);
	}
      return;
    }
  
  if ((t == Ct) && (trp != (char *)(0)))
    {
      char *src, *end_dp;
      
      for (src = (char *)(a->p), end_dp = dp + xrho;
	   dp != end_dp;
	   *dp++ = trp[(unsigned)(*src++ & 0xff)]);
    }
#ifdef MS_64BIT
   else if(t == It && (long_bytes < sizeof(I))) {
    /* 32 bit backward compatibility mode */
#ifndef MS_LITTLE_ENDIAN
      hlen = sizeof(I)-long_bytes;
#else
      hlen = 0;
#endif
    for(int i=0; i < xrho; i++) {
      memcpy(dp,(char *)(a->p+i)+hlen, long_bytes);
      dp += long_bytes;
    }
  }
#endif
  else
    {
      memcpy(dp,(char *)(a->p), (I)dlen);
      dp += dlen;
    }
  *hpp = hp;
  *dpp = dp;
  return;
}

/* Endian conversion routines */
void MSA::ndn16copy(char *from, char *to, int n)
{
  for(;n--;from+=2,to+=2)
  {
    to[1]=from[0];to[0]=from[1];
  }
}

void MSA::ndn32copy(char *from, char *to, int n)
{
  for(;n--;from+=4,to+=4)
  {
    to[3]=from[0];to[2]=from[1];to[1]=from[2];to[0]=from[3];
  }
}

void MSA::ndn64copy(char *from, char *to, int n)
{
  for(;n--;from+=8,to+=8)
  {
    to[7]=from[0];to[6]=from[1];to[5]=from[2];to[4]=from[3];
    to[3]=from[4];to[2]=from[5];to[1]=from[6];to[0]=from[7];
  }
}
/* The extract pass function takes a pointer the end of the CDR
 * format buffer and pointers to pointers into the header and
 * data sections of the  CDR buffer.  It returns an A object that
 * is the importation of its part of the CDR format buffer.  If
 * the extraction fails, a zero pointer is returned.  The extract
 * pass handles general arrays by calling itself recursively.  If
 * One of these calls returns zero then extract pass frees its A
 * object and returns zero to its caller.
 */
MSAStruct *MSA::extractpass(char **hpp,char **dpp,char *endp,long *erp,char *trp, int swap)
{
  register char *hp, *dp;
  I xrho=0, rt, rl, rank, hlen;
  register I i;
  register A a;
  I d[MAXRANK];
  impexpHeader h;
  
  /* process descriptor into ga call */
  hp = *hpp;
  if ((endp - hp) < 8)
    {
      *erp = (I)1;
      return (A)0;
    }
  
  hlen = CDRXRHOLEN+CDRRTLEN+CDRRLLEN+CDRRANKLEN;
  memcpy((char*)&h,hp,hlen); hp+=hlen;

  xrho = ntohl(h.xrho);
  rt   = h.rt;
  rl   = h.rl;
  rank = ntohs(h.rank); 
  if (rank > MAXRANK)
    {
      *erp = (I)1;
      return (A)0;
    }
  if ((endp - hp) < (CDRRHOLEN * rank))
    {
      *erp = (I)1;
      return (A)0;
    }
#if defined(MS_64BIT) || defined(MS_LITTLE_ENDIAN)
  for(i=0; i < rank; i++) {
    d[i] =ntohl((int)(*(int *)hp)); 
    hp+=CDRRHOLEN;
  }
  for(i=rank; i < MAXRANK; i++)    d[i] = 0;
#else
  /* this is more efficient in this case */
  memcpy((char *)(d), hp, 4 * rank); hp += 4 * rank;
  memset((char *)(d) + (4 * rank),0,4 * (MAXRANK - rank));
#endif

  *hpp = hp;
  if ((rt == 'P')
      || (rt == 'J')
      || (rt == 'Z')
      || ((rt == 'C') && (rl == 4))
      || ((rt == 'E') && (rl == 16)))
    {
      /* nonce error */
      *erp = (I)1;
      return (A)0;
    }

  if (dpp == (char **)(0))
    {
      /* prototype */
      if ((rt == 'G') && (rl== 0))
	{
	  for (i = 0; i < xrho; i++)
	    {
	      (void)extractpass(hpp, dpp, endp, erp, trp,swap);
	      if (*erp != (I)0) break;
	    }
	  return (A)0;
	}
      if (((rt == 'X') && (rl == 0))
	  || ((rt == 'I') && ((rl == 2) || (rl == 4)
#ifdef MS_64BIT
			      || (rl == 8)
#endif
			      ))
	  || ((rt == 'B') && ((rl == 1) || (rl == 4) || (rl == 8)))
	  || ((rt == 'E') && ((rl == 4) || (rl == 8)))
	  || ((rt == 'C') && (rl == 1))
	  || ((rt == 'S') && (rl == 1)))
	{
	  return (A)0;
	}
      *erp = 1;
      return (A)0;
    }
  if ((rt == 'G') && (rl == 0))
    {
      register A *ap;
      
      if (xrho == 0)
	{
	  /* do the prototype */
	  (void)extractpass(hpp, (char **)(0), endp, erp, trp, swap);
	  if (*erp != (I)0)
	    {
	      return (A)0;
	    }
	}
      a = ga(Et, rank, xrho, d);
      ap = (A *)(a->p);
      memset((char *)ap,0,sizeof(I) * xrho);
      for (i = 0; i < xrho; i++)
	{
	  if ((ap[i] = extractpass(hpp, dpp, endp, erp, trp, swap)) == (A)0)
	    {
	      if (*erp != 0)
		{
		  dc(a);
		  return (A)0;
		}
	      /* type X0 filler */
	      i--;
	    }
	}
      return a;
    }
  dp = *dpp;
#ifdef MS_64BIT
  if ((rt == 'I') && (rl == 8))
    {
      register C *ap;
      
      if ((endp - dp) < (rl * xrho))
	{
	  *erp = 1;
	  return (A)0;
	}
      a = ga(It, rank, xrho, d);
      ap = (C *)(a->p);
      if(swap) ndn64copy(dp, ap, xrho);
      else memcpy(ap,dp,8 * xrho); 
      dp += 8 * xrho;
      *dpp = dp;
      return a;
    }

  if ((rt == 'I') && (rl == 4))
    {
      register I *ap;
      int itmp;
      
      if ((endp - dp) < (4 * xrho))
	{
	  *erp = 1;
	  return (A)0;
	}
      a = ga(It, rank, xrho, d);
      ap = (I *)(a->p);
      for (i = 0; i < xrho; i++)
	{
	  if(swap) ndn32copy(dp, (char *)&itmp, 1);
	  else memcpy((char*)&itmp, dp, 4);      
	  ap[i] = itmp;
	  dp += 4;
	}
      *dpp = dp;
      return a;
    }
#else
  if ((rt == 'I') && (rl == 4))
    {
      register C *ap;
      
      if ((endp - dp) < (4 * xrho))
	{
	  *erp = 1;
	  return (A)0;
	}
      a = ga(It, rank, xrho, d);
      ap = (C *)(a->p);
      if(swap) ndn32copy(dp, ap, xrho);
      else memcpy(ap, dp, 4 * xrho);      
      dp += 4 * xrho;
      *dpp = dp;
      return a;
    }
#endif
  if ((rt == 'I') && (rl == 2))
    {
      register I *ap;
      short stmp;
      
      if ((endp - dp) < (2 * xrho))
	{
	  *erp = 1;
	  return (A)0;
	}
      a = ga(It, rank, xrho, d);
      ap = (I *)(a->p);
      for (i = 0; i < xrho; i++)
	{
	  if(swap) ndn16copy(dp, (char *)&stmp, 1);
	  else memcpy((char*)&stmp, dp, 2);      
	  ap[i] = stmp;
	  dp += 2;
	}
      *dpp = dp;
      return a;
    }
  if ((rt == 'E') && (rl == 4))
    {
      register F *ap;
      float ftmp;
      
      if ((endp - dp) < (rl * xrho))
	{
	  *erp = 1;
	  return (A)0;
	}
      a = ga(Ft, rank, xrho, d);
      ap = (F *)(a->p);
      for (i = 0; i < xrho; i++)
	{
	  if(swap) ndn32copy(dp, (char *)&ftmp, 1);
	  else memcpy((char*)&ftmp, dp, 4);      
	  ap[i] = ftmp;
	  dp += 4;
	}
      *dpp = dp;
      return a;
    }
  if ((rt == 'E') && (rl == 8))
    {
      register C *ap;
      
      if ((endp - dp) < (rl * xrho))
	{
	  *erp = 1;
	  return (A)0;
	}
      a = ga(Ft, rank, xrho, d);
      ap = (C *)(a->p);
      if(swap) ndn64copy(dp, ap, xrho);
      else memcpy(ap, dp, 8 * xrho);      
      dp += 8 * xrho;
      *dpp = dp;
      return a;
    }
  if ((rt == 'C') && (rl == 1))
    {
      register C *ap;
      
      if ((endp - dp) < xrho)
	{
	  *erp = 1;
	  return (A)0;
	}
      a = ga(Ct, rank, xrho, d);
      ap = (C *)(a->p);
      if (trp != (char *)(0))
	{
	  for (i = 0; i < xrho; i++)
	    {
	      ap[i] = trp[(unsigned)(*dp++ & 0xff)];
	    }
	}
      else
	{
	  memcpy(ap, dp, xrho); dp += xrho;
	}
      *dpp = dp;
      return a;
    }
  if ((rt == 'S') && (rl == 1))
    {
      register C *ap;
      register I s;
      
      if ((endp - dp) < xrho)
	{
	  *erp = 1;
	  return (A)0;
	}
      a = ga(Ct, 1, xrho, d);
      ap = (C *)(a->p);
      if (trp != (char *)(0))
	{
	  for (i = 0; i < xrho; i++)
	    {
	      ap[i] = trp[(unsigned)(*dp++ & 0xff)];
	    }
	}
      else
	{
	  memcpy(ap, dp, xrho); dp += xrho;
	}
      *dpp = dp;
      s = si(ap);
      dc(a);
      return (A)s;
    }
  if ((rt == 'B') && (rl == 8))
    {
      register I *ap;
      
      if ((endp - dp) < xrho)
	{
	  *erp = 1;
	  return (A)0;
	}
      a = ga(It, rank, xrho, d);
      ap = (I *)(a->p);
      for (i = 0; i < xrho; i++)
	{
	  ap[i] = *dp;
	  dp += 1;
	}
      *dpp = dp;
      return a;
    }
  if ((rt == 'B') && (rl == 4))
    {
      register I *ap;
      I m;
      
      if ((endp - dp) < (xrho + 1)/2)
	{
	  *erp = 1;
	  return (A)0;
	}
      a = ga(It, rank, xrho, d);
      ap = (I *)(a->p);
      for (m = 0, i = 0; i < xrho; i++)
	{
	  m = rl * (1 - (i % 2));
	  ap[i] = (*dp >> m) & 0x0f;
	  dp += (m == 0);
	}
      /* skip partially used byte */
      if (m != 0) dp++;
      *dpp = dp;
      return a;
    }
  if ((rt == 'B') && (rl == 1))
    {
      register I *ap;
      I m;
      
      if ((endp - dp) < (xrho + 7)/8)
	{
	  *erp = 1;
	  return (A)0;
	}
      a = ga(It, rank, xrho, d);
      ap = (I *)(a->p);
      for (m = 0, i = 0; i < xrho; i++)
	{
	  m = 7 - (i % 8);
	  ap[i] = (*dp >> m) & 0x01;
	  dp += (m == 0);
	}
      /* skip partially used byte */
      if (m != 0) dp++;
      *dpp = dp;
      return a;
    }
  if ((rt == 'X') && (rl == 0))
    {
      if ((endp - dp) < xrho)
	{
	  *erp = 1;
	  return (A)0;
	}
      dp += xrho;
      *dpp = dp;
      return (A)0;
    }
  *erp = 1;
  return (A)0;
}

void
MSA::fillExportBuffer(MSAStruct * aobj,char *hp,long hsz,char *trp,long for_a,int long_bytes)
{
  char *dp=hp+hsz;
  char flag = CDRFLAGSMAGIC;
  /* CDRFLAGS */
#ifdef MS_64BIT
  if(long_bytes == 8) flag |= CDRFLAGS64;
#endif
#ifdef MS_LITTLE_ENDIAN
  flag |= CDRFLAGSLITTLE;
#endif
  *hp = flag; 
  hp += CDRFLAGSLEN;
  /* CDRDLEN */
  int sz=htonl((int)hsz);
  memcpy(hp,((char *)(&sz))+1, CDRLENLEN); 
  hp += CDRLENLEN;
  fillpass(aobj, &hp, &dp, trp, for_a, long_bytes);
}


/* ExportAObject
 *
 * Creates a Msvp format char vector out of an A object.
 *
 * aobj is the A object to be abused.  trp is a translation vector, which 
 * must have length of 256, or be NULL.  for_a is a boolean flag, if 0 A
 * symbols are translated to character strings, 1 uses an extention to
 * Msvp format.
 *
 * Result is a pointer to the character vector created.  *plen is set to
 * the length of that vector, which may have embedded nulls.  In case of
 * error, the result is NULL, and *plen is an error code.
 *
 */

MSA MSA::exportAObject(void) const
{
  /* char *hp,*dp; */

  long hsz=4;
  long dsz=0;
  long sz;

  if ((sz=sizepass(_aStructPtr,&hsz,&dsz,4))!=0)
   {
     MSMessageLog::errorMessage("MSA Error: Attempt to export an object that is not an A Object.\n"); // this should throw an exception.
     return MSA();
   }

  sz=hsz+dsz;
  MSA result(gv(CHARACTERTYPE,sz),MSTrue);
  fillExportBuffer(_aStructPtr,(char *) (((MSAStruct *)result._aStructPtr)->p),hsz,(char *) 0,1,4);
  return result;
}

/* ImportAObject
 *
 * Creates an A object out of Msvp format char vector
 *
 * cvp points to the beginning of the msvp vector.  cvlen is the length
 * of the vector.  trp is a translation vector, which must have length
 * of 256, or be NULL.
 *
 * Result is A object created, or (A)0 in case of error.
 *
 */

MSA MSA::importAObject(char *cvp,long cvlen,char *trp)
{
  MSA result;  /* will be (A)0 if an error is detected */
  register char *endp;
  char *hp, *dp;
  I hsz, rc=0;
  I swap =0;
  unsigned char flag;

  if (MINIMPLEN>cvlen) return MSA();
  hp = cvp;
  endp = cvp + cvlen;
  flag = *hp;
  if ((flag&0xfC) != CDRFLAGSMAGIC) return MSA();  /* 0xfC == 11111100 */

  /* Do not accept 64 bit header if we are NOT 64 bit!. */
#if !defined(MS_64BIT)
  if (flag&CDRFLAGS64) return MSA();
#endif 
  /* Determine if we need to swap data bytes */
#if defined(MS_LITTLE_ENDIAN)
  if ((flag&CDRFLAGSLITTLE)==0) swap=1;
#else
  if (flag&CDRFLAGSLITTLE) swap=1;
#endif

  hp += CDRFLAGSLEN;
  hsz = 0;
  int sz=0;
  memcpy(((char *)(&sz))+1,hp,CDRLENLEN);
  hsz=ntohl(sz);
  hp += CDRLENLEN;
  dp = cvp + hsz;
  MSAStruct *aPtr=extractpass(&hp,&dp,endp,&rc,trp,swap);
  if (aPtr!=0) return MSA(aPtr,MSTrue);
  return MSA();
}

int MSA::exportAObjectSizePass(long * hsz_, long *dsz_, int long_bytes) const 
{
  *hsz_=4;
  *dsz_=0;
  return sizepass(_aStructPtr,hsz_,dsz_,long_bytes);
}

int MSA::exportAObjectFillPass(char *dest_, long hsz_, char * trp_,long for_a_,int long_bytes) const
{
  fillExportBuffer(_aStructPtr,dest_,hsz_,trp_,for_a_,long_bytes);
  return 0;
}




void MSA::beamOut(const char *fileName_,MSBoolean bExport_) const
{
  int fd;

  if (fileName_!=0)
   {
     MSA data;
     MSAStruct *pAStruct;
     if(bExport_==MSTrue)
      {
        data=exportAObject();
        pAStruct=data.aStructPtr();
      }
     else 
      {
	pAStruct=aStructPtr();
	if(pAStruct->t==ETYPE) 
	 {
	   MSMessageLog::errorMessage("MSA Error: Tried to beamOut a nested array without exporting.\n");
	   return;
	 }
      }
  
     fd = open(fileName_, 1);
     if (fd >= 0) {
        (void)lseek(fd,0,SEEK_SET);
        long t;
        if (pAStruct->c!=0) pAStruct->c=0;
        pAStruct->i=pAStruct->r?*pAStruct->d:1;
	long len=AHEADER+Tt(pAStruct->t,pAStruct->n);
        char *pString=(char *)pAStruct;
        do
         {
           t=write(fd,pString,len);
         }
        while (pString+=t,t!=-1&&(len-=t));
        fsync(fd);
        (void)close(fd);
      }
   }
}

MSBoolean MSA::import(void)
{
  MSA d;
  d=importAObject((char *)(_aStructPtr)->p,(_aStructPtr)->n,(char *)0);
  *this=d;
  return isNullMSA();
}

MSBoolean MSA::import(const MSMMap &aMap_)
{
  *this=importAObject((char *)(aMap_.aplusData())->p,(aMap_.aplusData())->n,(char *)0);
  return isNullMSA();
}

void *MSA::balloc(int size_)
{
  void *p;
  if (size_ <= 0)
   {
     return (void *)(0);
   }
  if ((p = (void *)malloc((unsigned)size_))==(void *)(0))
   {
     MSTKTHROWEXCEPTION(MSOutOfMemory("MSA: OUT OF MEMORY\n"));
   }
  return p;
}

void MSA::bfree(char *p_)
{
  if (p_!=(char *)(0))
   {
     free(p_);
   }
  return;
}

/* pcki checks for index error,pck does not */
long MSA::pcki(int i_,MSAStruct *a_)
{
  long z;
  long t=a_->t;
  if(!a_->r)return 0;
  if((unsigned)i_>=a_->d[0])return 0;
  return t==ETYPE&&(z=(long) a_->p[i_],!((!(0==((long)(z)&7))||((MSAStruct *)z)->t>ETYPE)&&!(0&&(3==((long)(z)&7)))))?(long) MSA::ic((MSAStruct *)z):(long) MSA::gc(t,0,1,0,(long *) ((char *)a_->p+((i_)<<(t+2&3))));
}

long MSA::gpu_fillivec(long *ivec_,MSAStruct *aobj_)
{
  long i; double f,*fvec=(double *)aobj_->p;
  for (i=0;i<aobj_->n;++i)
   {
     if (fvec[i]!=(f=rint(fvec[i]))) return 1;
     ivec_[i]=(int)f;
   }
  return 0;
}


long MSA::gpi_num(MSAStruct *apick_,MSAStruct *aobj_)
{
  long idx,i,ivec[9],*ip;
  if (ETYPE!= aobj_->t) { return(0); };
  if (2 <= apick_->r||apick_->n!=aobj_->r) { return(0); };
  if (FLOATTYPE==apick_->t)
   {
     if (gpu_fillivec(ivec,apick_)) { return(0); };
     ip=ivec;
   }
  else ip=apick_->p;
  
  if (aobj_->d[0]<=(unsigned)(idx=ip[0])) { return(0); };
  for (i=1;i<apick_->n;++i)
   {
     if (aobj_->d[i]<=(unsigned)(ip[i])) { return(0); };
     idx*=aobj_->d[i]; idx+=ip[i];
   }
  return idx;
}


long MSA::gpi_sym(long key_,MSAStruct *aobj_)
{
  long i,tt;
  MSAStruct *t0,*t1;
  if (ETYPE!= aobj_->t||2!=aobj_->n) {return(-1);}
  t0=(MSAStruct *)aobj_->p[0]; t1=(MSAStruct *)aobj_->p[1];
  if ((!(0==((long)(t0)&7)))||(!(0==((long)(t1)&7)))||ETYPE!=t1->t||t0->n!=t1->n) {return (-1);}
  for (i=0;i<t0->n;++i)
   {
     tt=t0->p[i];
     if (!(STRINGTYPE==((MSAStruct *) tt)->t)) {return(-1);}
     if (!strcmp((const char *) ((MSAStruct *) key_)->p,(const char *) ((MSAStruct *) tt)->p)) return i;
   }
  return(-1);
}


MSAStruct *MSA::gp_scalar(long idx_,MSAStruct *aobj_)
{
  MSAStruct * z,* zz; char  *cvec;
  if (1!=aobj_->r) { return(0); };
  if ((unsigned)idx_>=aobj_->n) { return(0); };
  if (STRINGTYPE==aobj_->t) { return(0); };
  switch (aobj_->t)
   {
   case INTEGERTYPE:return MSA::gi(aobj_->p[idx_]);
   case FLOATTYPE:return MSA::gf(((double *)aobj_->p)[idx_]);
   case CHARACTERTYPE:z=MSA::gs(CHARACTERTYPE);cvec=(char *)z->p;cvec[0]=((char *)aobj_->p)[idx_];cvec[1]='\0';return z;
   case ETYPE:return (MSAStruct *)pcki(idx_,aobj_);
   default:z=(MSAStruct *)MSA::gs(ETYPE);zz=MSA::gs(ETYPE);z->p[0]=(long)zz;zz->p[0]=aobj_->p[idx_];return z;
   }
}

MSAStruct *MSA::gp_nested(MSAStruct *apick_,MSAStruct *aobj_)
{
  long idx,i,issymbol=0;
  MSAStruct * atarg,* ares=aobj_;
  if (2 <= apick_->r) { return(0); };
  if (0==apick_->n) return (MSAStruct *)MSA::ic(ares);
  for (i=0;i<apick_->n;++i)
   {
     atarg=(MSAStruct *)apick_->p[i];
     if ( (!(0==((long)(ares)&7)))||ETYPE!= ares->t) { return(0); };
     if (STRINGTYPE==atarg->t)
      {
        if (-1==issymbol) { return(0); };
        issymbol=1;
        idx=gpi_sym((long)atarg,ares);
        ares=(MSAStruct *)ares->p[1];
      }
     else
      {
        if (1==issymbol) { return(0); };
        issymbol=-1;
        if (INTEGERTYPE==atarg->t||FLOATTYPE==atarg->t)
         {
          idx=gpi_num(atarg,ares); 
         }
        else if (ETYPE==atarg->t)
         {
           if (0==atarg->n&&1==ares->n) { ares=(MSAStruct *)*ares->p; continue;} 
           if ((STRINGTYPE==atarg->t)||1!=atarg->n) { return(0); };
           atarg=(MSAStruct *)*atarg->p;
           if (!(STRINGTYPE==atarg->t)) { return(0); };
           idx=gpi_sym((long)atarg,ares);
           ares=(MSAStruct *)ares->p[1];
         }
        else { return(0); };
      }
     ares=(MSAStruct *)ares->p[idx];
   }
  if (!(0==((long)(ares)&7))) { return(0); };
  return ((!(0==((long)(ares)&7))||((MSAStruct *)ares)->t>ETYPE)&&!(0&&(3==((long)(ares)&7))))?MSA::gc(ETYPE,0,1,0,(long *) &ares):(MSAStruct *)MSA::ic(ares);
}

MSAStruct *MSA::gp_num(MSAStruct *apick_,MSAStruct *aobj_)
{
  long i,*ivec=(long*)0,*ip;
  MSAStruct * ares=aobj_;
  if (2 <= apick_->r) { return(0); };
  if (0==apick_->n) return (MSAStruct *)MSA::ic(ares);
  if (FLOATTYPE==apick_->t)
   {
     ivec=(long *) MSA::balloc(apick_->n * sizeof(long));
     if (gpu_fillivec(ivec,apick_)) { MSA::bfree((char *) ivec); { return(0); }; }
     ip=ivec;
   }
  else ip=apick_->p;
  for (i=0;i<apick_->n;++i)
   {
     if ( (!(0==((long)(ares)&7)))||ETYPE!= ares->t) {MSA::bfree((char *) ivec);{ return(0); };}
     if ( 2 <= ares->r) {MSA::bfree((char *) ivec);{ return(0); };}
     if ((unsigned)(ip[i])>=ares->n) {MSA::bfree((char *) ivec);{ return(0); };}
     ares=(MSAStruct *)ares->p[ip[i]];
   }
  MSA::bfree((char *) ivec);
  if (!(0==((long)(ares)&7))) { return(0); };
  return ((!(0==((long)(ares)&7))||((MSAStruct *)ares)->t>ETYPE)&&!(0&&(3==((long)(ares)&7))))?MSA::gc(ETYPE,0,1,0,(long *) &ares):(MSAStruct *)MSA::ic(ares);
}


MSAStruct *MSA::gpick(MSAStruct *apick_,MSAStruct *aobj_)
{
  long idx;
  MSAStruct * z;
  switch(apick_->t)
   {
  case INTEGERTYPE: case FLOATTYPE: 
    if (1==apick_->n)
     {
       if (1!=aobj_->r) { return(0); };
       if (INTEGERTYPE==apick_->t) z=(MSAStruct *)pcki(*apick_->p,aobj_);
       else if (gpu_fillivec(&idx,apick_)) { return(0); }
       else z=(MSAStruct *)pcki(idx,aobj_);
     }
    else z=gp_num(apick_,aobj_); break;
   case ETYPE: z=gp_nested(apick_,aobj_); break;
   default: { return(0); }
   }
  return z;
}


MSAStruct* MSA::copyAStruct(MSAStruct *aStructPtr_)
{
  MSAStruct * aStructPtr=0;
  if (aStructPtr_) 
   {
     long xrho=aStructPtr_->n;
     aStructPtr = ga(aStructPtr_->t,aStructPtr_->r,aStructPtr_->n,aStructPtr_->d);
     if (aStructPtr)
       {
	 aStructPtr->i=aStructPtr_->i;
	 switch(aStructPtr_->t)
	  {
	  case INTEGERTYPE:
	    memcpy(aStructPtr->p,(char *) (aStructPtr_->p),(int)(sizeof(long)) *xrho);
	    break;
	  case FLOATTYPE:
	    memcpy(aStructPtr->p,(char *) (aStructPtr_->p),(int)(sizeof(double)) *xrho);
	    break;
	  case CHARACTERTYPE:
	    memcpy(aStructPtr->p,(char *) (aStructPtr_->p),(int)(sizeof(char)) *xrho);
	    break;
	  case STRINGTYPE:
	    // we do't want any "naked" symbols so we need to enclose at top level
	    {
	      long d[MAXRANK]={ 0,0,0,0,0,0,0,0,0 };
	      MSAStruct *tmpaStructPtr; 
	      memcpy(aStructPtr->p,(char *) (aStructPtr_->p),(int)(sizeof(char)) *xrho);
	      tmpaStructPtr = aStructPtr;
	      aStructPtr=ga(ETYPE,0,1,d);
	      if (aStructPtr!=0) *aStructPtr->p=(long) tmpaStructPtr;
	    }
	    break;
	  case ETYPE:
	    if (aStructPtr_->n!=0&&!(aStructPtr_->n==0&&aStructPtr_->r>0))
	     {
	       for (long i=0; i<xrho; i++)
		 {
		   aStructPtr->p[i]=(long) ic((MSAStruct *)aStructPtr_->p[i]);
		 }
	     }
	    break;
	  default:
	    break;
	  }
       }
   }
  return aStructPtr;
}
