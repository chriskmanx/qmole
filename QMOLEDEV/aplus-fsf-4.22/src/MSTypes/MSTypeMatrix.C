#ifndef MSTypeMatrixIMPLEMENTATION
#define MSTypeMatrixIMPLEMENTATION

///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////


#if HAVE_SSTREAM
#include <sstream>
#else
#include <strstream.h>
#endif
#include <MSTypes/MSRandom.H>
#include <MSTypes/MSUtil.H>
#include <MSTypes/MSBinaryVector.H>
#include <MSTypes/MSIndexVector.H>
#include <MSTypes/MSBinaryMatrix.H>
#include <MSTypes/MSTypes_MSF.H>
#include <MSTypes/MSGlobalInlines.H>

template<class Type> Type MSTypeMatrix<Type>::_badData=0;

template<class Type>
const MSSymbol& MSTypeMatrix<Type>::type(void) const 
{ return symbol(); }

template<class Type>
MSString MSTypeMatrix<Type>::asString(void) const
{
  MSString result;
  result+='(';
  result+=MSString(rows());
  result+=',';
  result+=MSString(columns());
  result+=") ";
  unsigned n=count();
  for (unsigned i=0;i<n;) 
   {
     result+=MSString(data()[i++]);
     if (i<n) result+=" ";
   }
  return MSString(result);
}

template<class Type>
MSString MSTypeMatrix<Type>::asMSF(void) const
{
#if HAVE_SSTREAM
  static string buf;
  static ostringstream oss(buf,ios::out);
#else
  static char buf[64];
  static ostrstream oss(buf,64,ios::out);
#endif
  oss.precision(8);
  MSString result;
  
  if (_count>0)
   {
     result+=MSMSF_US;
     result+=MSString(_rows);
     result+=MSMSF_US;
     result+=MSString(_columns);

     for (unsigned i=0;i<_count;i++)
      {
	oss.seekp(ios::beg);
	oss<<MSMSF_US<<data()[i]<<ends;
#if HAVE_SSTREAM
	result+=MSString(buf.data());
#else
	result+=buf;
#endif
      }
   }
  return result;
}

template<class Type>
MSString MSTypeMatrix<Type>::asDebugInfo(void) const
{
  MSString result("MSTypeMatrix<TYPE>(@");
  result+=MSString((unsigned long)this).d2x().lowerCase();
  result+=",_rows=";
  result+=MSString(rows());
  result+=",_columns=";
  result+=MSString(columns());
  result+=",_count=";
  result+=MSString(length());
  result+=",_size=";
  result+=MSString(size());
  result+=",_data=";
  result+=pData()->asDebugInfo();
  result+=",_type=";
  result+=type().symbolName();
  result+=")";
  return MSString(result);
}

template<class Type>
MSModel *MSTypeMatrix<Type>::clone(void) const
{ return new MSTypeMatrix<Type>(*this); }

template<class Type>
MSModel *MSTypeMatrix<Type>::create(void) const
{ return new MSTypeMatrix<Type>(); }

template<class Type>
void MSTypeMatrix<Type>::assign(const MSModel& aModel_)
{ *this=(MSTypeMatrix<Type>&)aModel_; }

template<class Type>
const MSSymbol& MSTypeMatrix<Type>::symbol(void)
{
  static MSSymbol sym("MSTypeMatrix<" + msClassName(Type()) + ">");
  return sym;  
}

template<class Type>
MSString MSTypeMatrix<Type>::className(void) const
{
  return  MSString("MSTypeMatrix<" + msClassName(Type()) + ">");
}

template<class Type>
long MSTypeMatrix<Type>::compare(const MSModel& aModel_) const
{ return compare((MSTypeMatrix<Type>&)aModel_); }

template<class Type>
void MSTypeMatrix<Type>::blockLeft(unsigned target_,unsigned moveCount_)
{ 
  Type *dp=data();
  for (unsigned i=target_;moveCount_>0;i++,moveCount_--) dp[i]=dp[i+1]; 
}

template<class Type>
void MSTypeMatrix<Type>::blockRight(unsigned target_,unsigned moveCount_)
{ 
  Type *dp=data();
  for (unsigned i=target_+moveCount_-1;moveCount_>0;i--,moveCount_--) dp[i]=dp[i-1]; 
}

template<class Type>
void MSTypeMatrix<Type>::allocData(unsigned length_)
{
  _count=length_;
  if (length()>0)
   {
     _pData=MSTypeData<Type,MSAllocator<Type> >::allocateWithLength(length());     
   }
  else
   {
     _pData=0;
   }
}

template<class Type>
void MSTypeMatrix<Type>::makeUniqueCopy(void)
{
  if (_pData!=0)
   {
     MSTypeData<Type,MSAllocator<Type> > *dst=MSTypeData<Type,MSAllocator<Type> >::allocateWithSize(size());
     MSTypeData<Type,MSAllocator<Type> >::copy(_pData->elements(),dst->elements(),length()); //src,dst,len
     _pData->decrementCount();
     _pData=dst;
   }
}

template<class Type>
void MSTypeMatrix<Type>::prepareToChange(void)
{
  if (_pData!=0)
   {
     if (_pData->refCount()>1) makeUniqueCopy();
   }
  else allocData(length());
}

template<class Type>
void MSTypeMatrix<Type>::freeData(void)
{ decrementCount(),_pData=0; }

// make sure there are at least size_+1 elements in the array
template<class Type>
void MSTypeMatrix<Type>::reserve(unsigned size_)
{
  unsigned n=size_+1;
  if (size()<n)
   {
     MSTypeData<Type,MSAllocator<Type> > *newData=MSTypeData<Type,MSAllocator<Type> >::allocateWithLength(n);     
     Type *dp=newData->elements();
     Type *sp=data();
     MSTypeData<Type,MSAllocator<Type> >::copy(sp,dp,size());
     freeData();
     _pData=newData;
   }
}

//-------------------------------------------------------------------------------------------------
// MSTypeMatrix<Type>
//-------------------------------------------------------------------------------------------------
template<class Type>
ostream& operator<<(ostream& aStream_,const MSTypeMatrix<Type>& aTypeMatrix_)
{
  unsigned r=aTypeMatrix_.rows();
  unsigned c=aTypeMatrix_.columns();
  for (unsigned i=0;i<r;i++) 
   {
     for (unsigned j=0;j<c;j++)
      {
	aStream_<<aTypeMatrix_(i,j)<<" ";
      }
     aStream_<<endl;
   }
  return aStream_<<flush;
}

template<class Type>
MSTypeMatrix<Type>::MSTypeMatrix(void) 
:MSMatrix()
{ _pData=0;_blocked=MSFalse; }

template<class Type>
MSTypeMatrix<Type>::MSTypeMatrix(unsigned rows_,unsigned cols_)
:MSMatrix(rows_,cols_)
{
  allocData(length());
  _blocked=MSFalse;
}

template<class Type>
MSTypeMatrix<Type>::MSTypeMatrix(unsigned rows_,unsigned cols_,Type fill_) 
:MSMatrix(rows_,cols_)
{
  if (length()>0)
   {
     unsigned n=length();
     allocData(n);
     Type *dp=data();
     while (n--) *dp++=fill_;     
   }
  else _pData=0;
  _blocked=MSFalse;
}

template<class Type>
MSTypeMatrix<Type>::MSTypeMatrix(const MSTypeMatrix<Type>& aTypeMatrix_) 
:MSMatrix(aTypeMatrix_.rows(),aTypeMatrix_.columns())
{
  _pData=aTypeMatrix_.pData();
  _blocked=MSFalse;
  (void)incrementCount();
}

// special private constructor used to avoid temporary object creation
// on return by value - see ARM page 267
template<class Type>
MSTypeMatrix<Type>::MSTypeMatrix(MSTypeData<Type,MSAllocator<Type> > *data_,unsigned rows_,unsigned cols_) 
:MSMatrix(rows_,cols_)
{
  _pData=data_;
  _blocked=MSFalse;
}


template <class Type>
MSTypeMatrix<Type>::MSTypeMatrix(const Type *pElements_, unsigned rows_, unsigned cols_)
: MSMatrix(rows_,cols_)
{
  _pData = MSTypeData<Type,MSAllocator<Type> >::allocateWithLength(_count, MSRaw);
  MSTypeData<Type,MSAllocator<Type> >::copy(pElements_, _pData->elements(), _count, MSRaw);
}


template<class Type>
MSTypeMatrix<Type>::~MSTypeMatrix(void)
{ freeData(); }

template<class Type>
MSError::ErrorStatus MSTypeMatrix<Type>::set(const char *pString_)
{
  freeData();
  if (pString_!=0)
   {
#if defined(MS_NO_ISTRSTREAM_CONSTCHAR_CONSTRUCTOR)
     // Visual C++ does not define a constructor that takes a const char *,
     // therefore we have to do the following cast.
     istrstream ist((char *)(void *)pString_,strlen(pString_));
#else
#if HAVE_SSTREAM
     istringstream ist(pString_);
#else
     istrstream ist(pString_,strlen(pString_));
#endif
#endif	
     char c='\0';

     // extract '('
     while (c!='('&&ist) ist>>c;
     ist>>_rows; 
     while (c!=','&&ist) ist>>c;
     ist>>_columns;
     while (c!=')'&&ist) ist>>c;
     if (!ist) 
      {
	_rows=_columns=_count=0;
	return MSError::MSFailure;
      }
     _count=rows()*columns();
     if (length()>0)
       _pData=MSTypeData<Type,MSAllocator<Type> >::allocateWithLength(length());
     unsigned n=length();
     Type *dp=data();
     for (unsigned i=0;i<n;i++) ist>>dp[i];

     if (!ist) return MSError::MSFailure;
   }
  changed();
  return MSError::MSSuccess;
}

template<class Type>
MSError::ErrorStatus MSTypeMatrix<Type>::setFromMSF(const char *pString_)
{
  MSError::ErrorStatus code;
  unsigned i;
  unsigned startpos;
  unsigned value[2];
  const char *pcurrent;
  char *pstring;
  
  if ((pString_!=0 &&(*pString_==MSMSF_US)&&(strlen(pString_)>sizeof(MSMSF_US))))
   {
     code=MSError::MSSuccess;
     freeData();
     _rows=0,_columns=0,_count=0;
     
     MSString decode(pString_);
     decode.decodeMSF();
     unsigned slen=decode.length();
     startpos=sizeof (MSMSF_US);
     pcurrent=(const char *)decode.string()+startpos;
     
// first get the size of the matrix....its the first two elements in 
// the string
     
     for (i=0;i<2;i++)
      {
	value[i]=0;
	if (isdigit(*pcurrent)) value[i]=strtoul(pcurrent,&pstring,10);
	if (*pstring!='\0')
	 {
	   startpos=decode.indexOf (MSMSF_US,startpos);
           startpos+=sizeof(MSMSF_US);
           if (startpos<slen) pcurrent=(const char *) decode.string()+startpos;
           else
	    {
              value[i]=0;
              break;
	    }
	 }
	else
	 {
	   value[i]=0;
	   break;
	 }
      }
     
// set the matrix size and put the rest of the data in the matrix, note
// the pnumber should always be pointing to the next valid value for the
// matrix. If this is not true the MSF string is improperly formated
     
     if ((value[0]!=0)&&(value[1]!=0))
      {
	_rows=value[0];
	_columns=value[1];
	_count=rows()*columns();
	_pData=MSTypeData<Type,MSAllocator<Type> >::allocateWithLength(length());	
	if (_pData!=0)
	 {
	   for (i=0;i<_count;i++)
	    {
	      if (startpos<slen)
	       {
		 if (MSMSF_US!=decode(startpos)) // can be next char in not MxN case
		  {
		    code=set(i,(const char *)decode.string()+startpos);
		    if (code==MSError::MSSuccess)
		     {
		       startpos=decode.indexOf(MSMSF_US,startpos);
		       startpos+=sizeof(MSMSF_US);
		     }
		    else 
		     {
		       code=MSError::BadMSFString;
		       break;
		     }
		  }
		 else set(i,Type(0)); // make sure fills in on not MxN case 
	       }
	      else
	       {
		 code=MSError::BadMSFString;
		 break;
	       }
	    }
	 }
	else code=MSError::MSFailure;
      }
     else code=MSError::BadMSFString;
   }
  else code=MSError::BadMSFString;
  
  if (code!=MSError::MSSuccess) removeAll();
  else changed();
  return code;
}

template<class Type>
MSError::ErrorStatus MSTypeMatrix<Type>::set(unsigned index_,const char *pString_)
{
  Type aType;
  return msSetFromString(aType,pString_)==MSError::MSSuccess?set(index_,aType):MSError::MSFailure;
}

template<class Type>
MSError::ErrorStatus MSTypeMatrix<Type>::set(unsigned index_,Type aValue_)
{ 
  if (index_<length())
   {
     prepareToChange();
     _pData->elements()[index_]=aValue_;
     if (doChanged()==MSTrue) changed(index_);
     return MSError::MSSuccess;
   }
  return MSError::MSFailure; 
}

template<class Type>
unsigned MSTypeMatrix<Type>::indexOf(Type aValue_,unsigned startPos_) const
{ 
  unsigned n=length();
  for (unsigned i=startPos_;i<n;i++) if (elementAt(i)==aValue_) return i;
  return n;
}

template<class Type>
unsigned MSTypeMatrix<Type>::lastIndexOf(Type aValue_,unsigned startPos_) const
{ 
  if (length()>0)
   {
     unsigned i;
     if (startPos_>=length()) i=length()-1;
     else i=startPos_;
     for (;i!=0;i--) if (elementAt(i)==aValue_) return i;
     if (i==0) if (elementAt(i)==aValue_) return i;
   }
  return length();
}

// Assign a MSTypeMatrix to this MSTypeMatrix
// aTypeMatrix=bTypeMatrix
template<class Type>
MSTypeMatrix<Type>& MSTypeMatrix<Type>::operator=(const MSTypeMatrix<Type>& aTypeMatrix_)
{
  if (this!=&aTypeMatrix_)
   {
     freeData();
     _count=aTypeMatrix_.length();
     _rows=aTypeMatrix_.rows();
     _columns=aTypeMatrix_.columns();
     _pData=aTypeMatrix_.pData();
     (void)incrementCount();
     changed();
   }
  return *this;
}

// Assign a unsigned index selected element of a MSTypeMatrix to all elements of this MSTypeMatrix.
// aTypeMatrix=bTypeMatrix[index]
template<class Type>
MSTypeMatrix<Type>& MSTypeMatrix<Type>::operator=(const MSMatrixSTypePick<Type>& aPick_)
{
  prepareToChange();
  unsigned n=length();
  Type s=aPick_.value();
  Type *dp=data();
  while (n--) *dp++=s;
  changed();
  return *this;
}

// Assign a Type to all elements of this MSTypeMatrix.
// aTypeMatrix=aType
template<class Type>
MSTypeMatrix<Type>& MSTypeMatrix<Type>::operator=(Type scalar_)
{
  prepareToChange();
  unsigned n=length();
  Type s=scalar_;
  Type *dp=data();
  while (n--) *dp++=s;
  changed();
  return *this;
}

template<class Type>
MSTypeMatrix<Type>& MSTypeMatrix<Type>::series(Type offset_) 
{
  prepareToChange();
  Type j=offset_;
  unsigned n=length();
  Type *dp=data();
  while (n--) *dp++=j++;
  changed();
  return *this;
}


//-------------------------------------------------------------------------------------------------
// relational operator functions
//-------------------------------------------------------------------------------------------------
template<class Type>
long MSTypeMatrix<Type>::compare(const MSTypeMatrix<Type>& aTypeMatrix_) const
{
  unsigned n=MSUtil::min(length(),aTypeMatrix_.length());
  for (unsigned i=0;i<n;i++)
   {
     if (elementAt(i)!=aTypeMatrix_.elementAt(i))
      {
	if (elementAt(i)<aTypeMatrix_.elementAt(i)) return -1;
	else return 1;
      }
   }
  if (length()==aTypeMatrix_.length()) return 0;
  if (length()<aTypeMatrix_.length()) return -1;
  else return 1;
}

template<class Type>
MSBinaryMatrix MSTypeMatrix<Type>::binaryCompare(const MSTypeMatrix<Type>& aTypeMatrix_,MSComparison aComparison_) const
{
  assert(rows()==aTypeMatrix_.rows()&&columns()==aTypeMatrix_.columns());
  unsigned aSize=size();
  unsigned n=length();
  MSTypeData<unsigned char,MSAllocator<unsigned char> > *d =
    MSTypeData<unsigned char,MSAllocator<unsigned char> >::allocateWithSize(aSize);
  const Type *ap=data();
  const Type *bp=aTypeMatrix_.data();
  unsigned char *dp=d->elements();
  unsigned i;
  switch (aComparison_)
   {
   case MSLessThan:
     for (i=0;i<n;i++) dp[i]=(ap[i]<bp[i]); break;
   case MSLessThanOrEqualTo:
     for (i=0;i<n;i++) dp[i]=(ap[i]<=bp[i]); break;
   case MSEqualTo:
     for (i=0;i<n;i++) dp[i]=(ap[i]==bp[i]); break;
   case MSNotEqualTo:
     for (i=0;i<n;i++) dp[i]=(ap[i]!=bp[i]); break;
   case MSGreaterThan:
     for (i=0;i<n;i++) dp[i]=(ap[i]>bp[i]); break;
   case MSGreaterThanOrEqualTo:
     for (i=0;i<n;i++) dp[i]=(ap[i]>=bp[i]); break;
   }

  return MSBinaryMatrix(d,rows(),columns());  
}

template<class Type>
MSBinaryMatrix MSTypeMatrix<Type>::binaryCompare(Type aType_,MSComparison aComparison_) const
{
  unsigned aSize=size();
  unsigned n=length();
  MSTypeData<unsigned char,MSAllocator<unsigned char> > *d =
    MSTypeData<unsigned char,MSAllocator<unsigned char> >::allocateWithSize(aSize);
  const Type *ap=data();
  unsigned char *dp=d->elements();
  unsigned i;
  switch (aComparison_)
   {
   case MSLessThan:
     for (i=0;i<n;i++) dp[i]=(ap[i]<aType_); break;
   case MSLessThanOrEqualTo:
     for (i=0;i<n;i++) dp[i]=(ap[i]<=aType_); break;
   case MSEqualTo:
     for (i=0;i<n;i++) dp[i]=(ap[i]==aType_); break;
   case MSNotEqualTo:
     for (i=0;i<n;i++) dp[i]=(ap[i]!=aType_); break;
   case MSGreaterThan:
     for (i=0;i<n;i++) dp[i]=(ap[i]>aType_); break;
   case MSGreaterThanOrEqualTo:
     for (i=0;i<n;i++) dp[i]=(ap[i]>=aType_); break;
   }

  return MSBinaryMatrix(d,rows(),columns());  
}

template<class Type>
MSBoolean MSTypeMatrix<Type>::scalarCompare(Type aType_,MSComparison aComparison_) const
{
  unsigned n=length();
  if (n>0)
   {
     const Type *ap=data();
     unsigned i;
     switch (aComparison_)
      {
      case MSLessThan:
	for (i=0;i<n;i++) if (ap[i]>=aType_) return MSFalse; break;
      case MSLessThanOrEqualTo:
	for (i=0;i<n;i++) if (ap[i]>aType_) return MSFalse; break;
      case MSEqualTo:
	for (i=0;i<n;i++) if (ap[i]!=aType_) return MSFalse; break;
      case MSNotEqualTo:
	for (i=0;i<n;i++) if (ap[i]==aType_) return MSFalse; break;
      case MSGreaterThan:
	for (i=0;i<n;i++) if (ap[i]<=aType_) return MSFalse; break;
      case MSGreaterThanOrEqualTo:
	for (i=0;i<n;i++) if (ap[i]<aType_) return MSFalse; break;
      default:
	return MSFalse;
      }
     return MSTrue;
   }
  return aComparison_==MSNotEqualTo?MSTrue:MSFalse;
}

//-------------------------------------------------------------------------------------------------
// unary operator functions
//-------------------------------------------------------------------------------------------------
template<class Type>
MSTypeMatrix<Type> operator-(const MSTypeMatrix<Type>& aTypeMatrix_)
{
  unsigned aSize=aTypeMatrix_.size();
  unsigned n=aTypeMatrix_.length();
  MSTypeData<Type,MSAllocator<Type> > *d=MSTypeData<Type,MSAllocator<Type> >::allocateWithSize(aSize);
  Type *sp=aTypeMatrix_.data();
  Type *dp=d->elements();
  while (n--) *dp++=-*sp++;
  return MSTypeMatrix<Type>(d,aTypeMatrix_.rows(),aTypeMatrix_.columns());
}


//-------------------------------------------------------------------------------------------------
// Prefix/Postfix increment/decrement
//-------------------------------------------------------------------------------------------------

// Prefix - add/subtract one, then return result
template<class Type>
MSTypeMatrix<Type>& MSTypeMatrix<Type>::operator++()
{
  if (length()>0)
   {
     prepareToChange();
     unsigned n=length();
     Type *sp=data();
     for (unsigned i=0;i<n;i++) ++sp[i];
     changed();
     return *this;
   }
  return *this;
}

template<class Type>
MSTypeMatrix<Type>& MSTypeMatrix<Type>::operator--()
{
  if (length()>0)
   {
     prepareToChange();
     unsigned n=length();
     Type *sp=data();
     for (unsigned i=0;i<n;i++) --sp[i];
     changed();
     return *this;
   }
  return *this;
}

// Postfix - add/subtract one, then return the initial value
template<class Type>
MSTypeMatrix<Type> MSTypeMatrix<Type>::operator++(int)
{
  if (length()>0)
   {
     MSTypeMatrix<Type> r(*this);
     prepareToChange();
     unsigned n=length();
     Type *sp=data();
     for (unsigned i=0;i<n;i++) ++sp[i];
     changed();
     return r;
   }
  return MSTypeMatrix<Type>();  
}

template<class Type>
MSTypeMatrix<Type> MSTypeMatrix<Type>::operator--(int)
{
  if (length()>0)
   {
     MSTypeMatrix<Type> r(*this);
     prepareToChange();
     unsigned n=length();
     Type *sp=data();
     for (unsigned i=0;i<n;i++) --sp[i];
     changed();
     return r;
   }
  return MSTypeMatrix<Type>();  
}

template<class Type>
MSMatrixSTypePick<Type>& MSMatrixSTypePick<Type>::operator++(int)
{
  Type value=pMatrix()->elementAt(_index)+1;
  _pMatrix->set(_index,value);
  return *this;
}

template<class Type>
MSMatrixSTypePick<Type>& MSMatrixSTypePick<Type>::operator--(int)
{
  Type value=pMatrix()->elementAt(_index)-1;
  _pMatrix->set(_index,value);
  return *this;
}

//-------------------------------------------------------------------------------------------------
// arithmetic operator +
//-------------------------------------------------------------------------------------------------
template<class Type>
MSTypeMatrix<Type> operator+(const MSTypeMatrix<Type>& aTypeMatrix_,Type aType_)
{
  MSTypeData<Type,MSAllocator<Type> > *d=0;
  unsigned n=aTypeMatrix_.length();
  unsigned aSize=aTypeMatrix_.size();
  if (n>0)
   {
     d=MSTypeData<Type,MSAllocator<Type> >::allocateWithSize(aSize);
     Type *dp=d->elements();
     const Type *ap=aTypeMatrix_.data();
     for (unsigned i=0;i<n;i++) *dp++=*ap++ + aType_;
   }
  return MSTypeMatrix<Type>(d,aTypeMatrix_.rows(),aTypeMatrix_.columns());
}

template<class Type>
MSTypeMatrix<Type> operator+(Type aType_,const MSTypeMatrix<Type>& aTypeMatrix_)
{ return aTypeMatrix_+aType_; }

template<class Type>
MSTypeMatrix<Type> operator+(const MSTypeMatrix<Type>& aTypeMatrix_,const MSTypeMatrix<Type>& bTypeMatrix_)
{
  assert(aTypeMatrix_.rows()==bTypeMatrix_.rows()&&aTypeMatrix_.columns()==bTypeMatrix_.columns());
  MSTypeData<Type,MSAllocator<Type> > *d=0;
  unsigned n=aTypeMatrix_.length();
  unsigned aSize=aTypeMatrix_.size();
  if (n>0)
   {
     d=MSTypeData<Type,MSAllocator<Type> >::allocateWithSize(aSize);
     Type *dp=d->elements();
     const Type *ap=aTypeMatrix_.data();
     const Type *bp=bTypeMatrix_.data();
     for (unsigned i=0;i<n;i++) *dp++=*ap++ + *bp++;
   }
  return MSTypeMatrix<Type>(d,aTypeMatrix_.rows(),aTypeMatrix_.columns());
}

// apl operation aMatrix_ (x @1 0) aVector_
template<class Type>
MSTypeMatrix<Type> operator+(const MSTypeMatrix<Type>& aMatrix_,const MSTypeVector<Type>& aVector_)
{
  if (aMatrix_.rows()!=aVector_.length()) 
   {
     aMatrix_.error("(x @1 0) Mismatch.");
     return MSTypeMatrix<Type>();
   }
  MSTypeData<Type,MSAllocator<Type> > *d=0;
  unsigned n=aMatrix_.length();
  unsigned aSize=aMatrix_.size();
  unsigned rows=aMatrix_.rows(),cols=aMatrix_.columns();
  if (n>0)
   {
     d=MSTypeData<Type,MSAllocator<Type> >::allocateWithSize(aSize);
     Type *dp=d->elements();
     const Type *mp=aMatrix_.data();
     const Type *vp=aVector_.data();
     unsigned j;
     for (unsigned i=0;i<rows;i++,vp++)
      {
	for (j=0;j<cols;j++)
	 {
	   *dp++=*mp++ + *vp;
	 }
      }
   }
  return MSTypeMatrix<Type>(d,rows,cols);
}

// apl operation aVector_ (x @1 0) aMatrix_
template<class Type>
MSTypeMatrix<Type> operator+(const MSTypeVector<Type>& aVector_,const MSTypeMatrix<Type>& aMatrix_)
{ return aMatrix_+aVector_; }

//-------------------------------------------------------------------------------------------------
// arithmetic assignment + operator
//-------------------------------------------------------------------------------------------------
template<class Type>
MSTypeMatrix<Type>& MSTypeMatrix<Type>::operator+=(Type aScalar_)
{
  prepareToChange();
  unsigned n=length();
  Type c=aScalar_;
  Type *dp=data();
  while (n--) *dp++ += c;
  changed();
  return *this;
}

template<class Type>
MSMatrixSTypePick<Type>& MSMatrixSTypePick<Type>::operator+=(Type aScalar_)
{
  Type value=pMatrix()->elementAt(_index)+aScalar_;
  _pMatrix->set(_index,value);
  return *this;
}

//-------------------------------------------------------------------------------------------------
// arithmetic operator -
//-------------------------------------------------------------------------------------------------
template<class Type>
MSTypeMatrix<Type> operator-(const MSTypeMatrix<Type>& aTypeMatrix_,Type aType_)
{
  MSTypeData<Type,MSAllocator<Type> > *d=0;
  unsigned n=aTypeMatrix_.length();
  unsigned aSize=aTypeMatrix_.size();
  if (n>0)
   {
     d=MSTypeData<Type,MSAllocator<Type> >::allocateWithSize(aSize);
     Type *dp=d->elements();
     const Type *ap=aTypeMatrix_.data();
     for (unsigned i=0;i<n;i++) *dp++=*ap++ - aType_;
   }
  return MSTypeMatrix<Type>(d,aTypeMatrix_.rows(),aTypeMatrix_.columns());
}

template<class Type>
MSTypeMatrix<Type> operator-(const MSTypeMatrix<Type>& aTypeMatrix_,const MSTypeMatrix<Type>& bTypeMatrix_)
{
  assert(aTypeMatrix_.rows()==bTypeMatrix_.rows()&&aTypeMatrix_.columns()==bTypeMatrix_.columns());
  MSTypeData<Type,MSAllocator<Type> > *d=0;
  unsigned n=aTypeMatrix_.length();
  unsigned aSize=aTypeMatrix_.size();
  if (n>0)
   {
     d=MSTypeData<Type,MSAllocator<Type> >::allocateWithSize(aSize);
     Type *dp=d->elements();
     const Type *ap=aTypeMatrix_.data();
     const Type *bp=bTypeMatrix_.data();
     for (unsigned i=0;i<n;i++) *dp++=*ap++ - *bp++;
   }
  return MSTypeMatrix<Type>(d,aTypeMatrix_.rows(),aTypeMatrix_.columns());
}

// apl operation aMatrix_ (x @1 0) aVector_
template<class Type>
MSTypeMatrix<Type> operator-(const MSTypeMatrix<Type>& aMatrix_,const MSTypeVector<Type>& aVector_)
{
  if (aMatrix_.rows()!=aVector_.length()) 
   {
     aMatrix_.error("(x @1 0) Mismatch.");
     return MSTypeMatrix<Type>();
   }
  MSTypeData<Type,MSAllocator<Type> > *d=0;
  unsigned n=aMatrix_.length();
  unsigned aSize=aMatrix_.size();
  unsigned rows=aMatrix_.rows(),cols=aMatrix_.columns();
  if (n>0)
   {
     d=MSTypeData<Type,MSAllocator<Type> >::allocateWithSize(aSize);
     Type *dp=d->elements();
     const Type *mp=aMatrix_.data();
     const Type *vp=aVector_.data();
     unsigned j;
     for (unsigned i=0;i<rows;i++,vp++)
      {
	for (j=0;j<cols;j++)
	 {
	   *dp++=*mp++ - *vp;
	 }
      }
   }
  return MSTypeMatrix<Type>(d,rows,cols);
}

//-------------------------------------------------------------------------------------------------
// arithmetic assignment - operator
//-------------------------------------------------------------------------------------------------
template<class Type>
MSTypeMatrix<Type>& MSTypeMatrix<Type>::operator-=(Type aScalar_)
{
  prepareToChange();
  unsigned n=length();
  Type c=aScalar_;
  Type *dp=data();
  while (n--) *dp++ -= c;
  changed();
  return *this;
}

template<class Type>
MSMatrixSTypePick<Type>& MSMatrixSTypePick<Type>::operator-=(Type aScalar_)
{
  Type value=pMatrix()->elementAt(_index)-aScalar_;
  _pMatrix->set(_index,value);
  return *this;
}

//-------------------------------------------------------------------------------------------------
// arithmetic operator *
//-------------------------------------------------------------------------------------------------
template<class Type>
MSTypeMatrix<Type> operator*(const MSTypeMatrix<Type>& aTypeMatrix_,Type aType_)
{
  MSTypeData<Type,MSAllocator<Type> > *d=0;
  unsigned n=aTypeMatrix_.length();
  unsigned aSize=aTypeMatrix_.size();
  if (n>0)
   {
     d=MSTypeData<Type,MSAllocator<Type> >::allocateWithSize(aSize);
     Type *dp=d->elements();
     const Type *ap=aTypeMatrix_.data();
     for (unsigned i=0;i<n;i++) *dp++=*ap++ * aType_;
   }
  return MSTypeMatrix<Type>(d,aTypeMatrix_.rows(),aTypeMatrix_.columns());
}

template<class Type>
MSTypeMatrix<Type> operator*(Type aType_,const MSTypeMatrix<Type>& aTypeMatrix_)
{ return aTypeMatrix_*aType_; }

template<class Type>
MSTypeMatrix<Type> operator*(const MSTypeMatrix<Type>& aTypeMatrix_,const MSTypeMatrix<Type>& bTypeMatrix_)
{
  assert(aTypeMatrix_.rows()==bTypeMatrix_.rows()&&aTypeMatrix_.columns()==bTypeMatrix_.columns());
  MSTypeData<Type,MSAllocator<Type> > *d=0;
  unsigned n=aTypeMatrix_.length();
  unsigned aSize=aTypeMatrix_.size();
  if (n>0)
   {
     d=MSTypeData<Type,MSAllocator<Type> >::allocateWithSize(aSize);
     Type *dp=d->elements();
     const Type *ap=aTypeMatrix_.data();
     const Type *bp=bTypeMatrix_.data();
     for (unsigned i=0;i<n;i++) *dp++=*ap++ * *bp++;
   }
  return MSTypeMatrix<Type>(d,aTypeMatrix_.rows(),aTypeMatrix_.columns());
}

// apl operation aMatrix_ (x @1 0) aVector_
template<class Type>
MSTypeMatrix<Type> operator*(const MSTypeMatrix<Type>& aMatrix_,const MSTypeVector<Type>& aVector_)
{
  if (aMatrix_.rows()!=aVector_.length()) 
   {
     aMatrix_.error("(x @1 0) Mismatch.");
     return MSTypeMatrix<Type>();
   }
  MSTypeData<Type,MSAllocator<Type> > *d=0;
  unsigned n=aMatrix_.length();
  unsigned aSize=aMatrix_.size();
  unsigned rows=aMatrix_.rows(),cols=aMatrix_.columns();
  if (n>0)
   {
     d=MSTypeData<Type,MSAllocator<Type> >::allocateWithSize(aSize);
     Type *dp=d->elements();
     const Type *mp=aMatrix_.data();
     const Type *vp=aVector_.data();
     unsigned j;
     for (unsigned i=0;i<rows;i++,vp++)
      {
	for (j=0;j<cols;j++)
	 {
	   *dp++=*mp++ * *vp;
	 }
      }
   }
  return MSTypeMatrix<Type>(d,rows,cols);
}

// apl operation aVector_ (x @1 0) aMatrix_
template<class Type>
MSTypeMatrix<Type> operator*(const MSTypeVector<Type>& aVector_,const MSTypeMatrix<Type>& aMatrix_)
{ return aMatrix_*aVector_; }

//-------------------------------------------------------------------------------------------------
// arithmetic assignment * operator
//-------------------------------------------------------------------------------------------------
template<class Type>
MSTypeMatrix<Type>& MSTypeMatrix<Type>::operator*=(Type aScalar_)
{
  prepareToChange();
  unsigned n=length();
  Type c=aScalar_;
  Type *dp=data();
  while (n--) *dp++ *= c;
  changed();
  return *this;
}

template<class Type>
MSMatrixSTypePick<Type>& MSMatrixSTypePick<Type>::operator*=(Type aScalar_)
{
  Type value=pMatrix()->elementAt(_index)*aScalar_;
  _pMatrix->set(_index,value);
  return *this;
}

//-------------------------------------------------------------------------------------------------
// arithmetic operator /
//-------------------------------------------------------------------------------------------------
template<class Type>
MSTypeMatrix<Type> operator/(const MSTypeMatrix<Type>& aTypeMatrix_,Type aType_)
{
  MSTypeData<Type,MSAllocator<Type> > *d=0;
  unsigned n=aTypeMatrix_.length();
  unsigned aSize=aTypeMatrix_.size();
  if (n>0)
   {
     d=MSTypeData<Type,MSAllocator<Type> >::allocateWithSize(aSize);
     Type *dp=d->elements();
     const Type *ap=aTypeMatrix_.data();
     for (unsigned i=0;i<n;i++) *dp++=*ap++ / aType_;
   }
  return MSTypeMatrix<Type>(d,aTypeMatrix_.rows(),aTypeMatrix_.columns());
}

template<class Type>
MSTypeMatrix<Type> operator/(const MSTypeMatrix<Type>& aTypeMatrix_,const MSTypeMatrix<Type>& bTypeMatrix_)
{
  assert(aTypeMatrix_.rows()==bTypeMatrix_.rows()&&aTypeMatrix_.columns()==bTypeMatrix_.columns());
  MSTypeData<Type,MSAllocator<Type> > *d=0;
  unsigned n=aTypeMatrix_.length();
  unsigned aSize=aTypeMatrix_.size();
  if (n>0)
   {
     d=MSTypeData<Type,MSAllocator<Type> >::allocateWithSize(aSize);
     Type *dp=d->elements();
     const Type *ap=aTypeMatrix_.data();
     const Type *bp=bTypeMatrix_.data();
     for (unsigned i=0;i<n;i++) *dp++=*ap++ / *bp++;
   }
  return MSTypeMatrix<Type>(d,aTypeMatrix_.rows(),aTypeMatrix_.columns());
}

// apl operation aMatrix_ (x @1 0) aVector_
template<class Type>
MSTypeMatrix<Type> operator/(const MSTypeMatrix<Type>& aMatrix_,const MSTypeVector<Type>& aVector_)
{
  if (aMatrix_.rows()!=aVector_.length()) 
   {
     aMatrix_.error("(x @1 0) Mismatch.");
     return MSTypeMatrix<Type>();
   }
  MSTypeData<Type,MSAllocator<Type> > *d=0;
  unsigned n=aMatrix_.length();
  unsigned aSize=aMatrix_.size();
  unsigned rows=aMatrix_.rows(),cols=aMatrix_.columns();
  if (n>0)
   {
     d=MSTypeData<Type,MSAllocator<Type> >::allocateWithSize(aSize);
     Type *dp=d->elements();
     const Type *mp=aMatrix_.data();
     const Type *vp=aVector_.data();
     unsigned j;
     for (unsigned i=0;i<rows;i++,vp++)
      {
	for (j=0;j<cols;j++)
	 {
	   *dp++=*mp++ / *vp;
	 }
      }
   }
  return MSTypeMatrix<Type>(d,rows,cols);
}

//-------------------------------------------------------------------------------------------------
// arithmetic assignment / operator
//-------------------------------------------------------------------------------------------------
template<class Type>
MSTypeMatrix<Type>& MSTypeMatrix<Type>::operator/=(Type aScalar_)
{
  prepareToChange();
  unsigned n=length();
  Type c=aScalar_;
  Type *dp=data();
  while (n--) *dp++ /= c;
  changed();
  return *this;
}

template<class Type>
MSMatrixSTypePick<Type>& MSMatrixSTypePick<Type>::operator/=(Type aScalar_)
{
  Type value=pMatrix()->elementAt(_index)/aScalar_;
  _pMatrix->set(_index,value);
  return *this;
}

//-------------------------------------------------------------------------------------------------
// stack, adjoin
//-------------------------------------------------------------------------------------------------

template<class Type>
MSTypeMatrix<Type> stack(const MSTypeMatrix<Type>& aTypeMatrix_,const MSTypeMatrix<Type>& bTypeMatrix_)
{
  if (aTypeMatrix_.columns()!=bTypeMatrix_.columns()) 
   {
     aTypeMatrix_.error("nonconformant MSTypeMatrix stack operands.");
     return MSTypeMatrix<Type>();
   }

  MSTypeData<Type,MSAllocator<Type> > *d=0;
  unsigned newLength=(aTypeMatrix_.rows()+bTypeMatrix_.rows())*aTypeMatrix_.columns();
  if (newLength>0)
   { 
     d=MSTypeData<Type,MSAllocator<Type> >::allocateWithLength(newLength);     
     Type *dp=d->elements();
     Type *mp=aTypeMatrix_.data();
     Type *row=aTypeMatrix_.data()+aTypeMatrix_.columns();
     if (mp!=0)
      {
	do
	 {
	   while (mp<row) *dp++=*mp++;
	 }
	while ((row+=aTypeMatrix_.columns())<=aTypeMatrix_.data()+aTypeMatrix_.length());
      }
     mp=bTypeMatrix_.data();
     row=bTypeMatrix_.data()+bTypeMatrix_.columns();
     if (mp!=0)
      {
	do
	 {
	   while (mp<row) *dp++=*mp++;
	 }
	while ((row+=bTypeMatrix_.columns())<=bTypeMatrix_.data()+bTypeMatrix_.length());
      }
   }
  return MSTypeMatrix<Type>(d,aTypeMatrix_.rows()+bTypeMatrix_.rows(),aTypeMatrix_.columns());
}

// inner product
template <class Type>
MSTypeMatrix<Type> multiply(const MSTypeMatrix<Type>& aTypeMatrix_,const MSTypeMatrix<Type>& bTypeMatrix_)
{
  if (aTypeMatrix_.columns()!=bTypeMatrix_.rows())
   {
     aTypeMatrix_.error("nonconformant MSTypeMatrix multiply operands.");
     return MSTypeMatrix<Type>();
   }
  unsigned len=aTypeMatrix_.rows()*bTypeMatrix_.columns();
  MSTypeData<Type,MSAllocator<Type> > *d=MSTypeData<Type,MSAllocator<Type> >::allocateWithLength(len);

  Type *aData=aTypeMatrix_.data(), *bData=bTypeMatrix_.data();
  Type *dp=d->elements();
  Type *row=aData;
  Type *ap=aData;
  Type *bp;
  unsigned column;
  unsigned aColumns=aTypeMatrix_.columns(), bColumns=bTypeMatrix_.columns();
  unsigned aCount=aTypeMatrix_.length();

  if (ap!=0)
   {
     while ((row+=aColumns)<=aData+aCount)
      {
	for (column=0;column<bColumns;column++)
	 {
	   bp=bData+column;
	   *dp=0;
	   while (ap<row)
	    {
	      *dp+=*ap++**bp;
	      bp+=bColumns;
	    }
	   // next d element
	   dp++;
	   // reset A row
	   ap-=aColumns;
	   // next B column
	 }
	// next A row
	ap+=aColumns;
	// reset B column
      } 
   }
  else  for (unsigned i=0;i<len;i++) *dp++=0; 

  return MSTypeMatrix<Type>(d,aTypeMatrix_.rows(),bColumns);
}

template<class Type>
MSTypeMatrix<Type>& MSTypeMatrix<Type>::stack(const MSTypeMatrix<Type>& aTypeMatrix_)
{
  if (aTypeMatrix_.columns()!=columns()) 
   {
     error("nonconformant stack operands.");
     return *this;
   }
  unsigned newLength=(rows()+aTypeMatrix_.rows())*columns();
  MSTypeData<Type,MSAllocator<Type> > *d=0;
  if (newLength>0)
   {
     d=MSTypeData<Type,MSAllocator<Type> >::allocateWithLength(newLength);     
     Type *dp=d->elements();
     Type *mp=data();
     Type *row=data()+columns();
     if (mp!=0)
      {
	do
	 {
	   while (mp<row) *dp++=*mp++;
	 }
	while ((row+=columns())<=data()+length());
      }
     mp=aTypeMatrix_.data();
     row=aTypeMatrix_.data()+aTypeMatrix_.columns();
     if (mp!=0)
      {
        do
         {
	   while (mp<row) *dp++=*mp++;
         }
        while ((row+=aTypeMatrix_.columns())<=aTypeMatrix_.data()+aTypeMatrix_.length());
      }
   }
  
  unsigned oldLength=length();
  freeData();
  _pData=d,_rows+=aTypeMatrix_.rows(),_count=newLength;
  if (receiverList()!=0&&aTypeMatrix_.length()>0)
   {
     MSIndexVector iv;
     iv.series(aTypeMatrix_.length(),oldLength);
     changed(iv);
   }
  return *this;
}

template<class Type>
MSTypeMatrix<Type> adjoin(const MSTypeMatrix<Type>& aTypeMatrix_,const MSTypeMatrix<Type>& bTypeMatrix_)
{
  if (aTypeMatrix_.rows()!=bTypeMatrix_.rows())
   {
     aTypeMatrix_.error("nonconformant MSTypeMatrix adjoin operands.");
     return MSTypeMatrix<Type>();
   }
  MSTypeData<Type,MSAllocator<Type> > *d=0;
  unsigned newLength=aTypeMatrix_.rows()*(aTypeMatrix_.columns()+bTypeMatrix_.columns());
  if (newLength>0)
   {
     d=MSTypeData<Type,MSAllocator<Type> >::allocateWithLength(newLength);     
     Type *dp=d->elements();
     Type *mp=aTypeMatrix_.data();
     Type *row=aTypeMatrix_.data()+aTypeMatrix_.columns();
     if (mp!=0)
      {
	do 
	 {
	   while (mp<row) *dp++=*mp++;
	   dp+=bTypeMatrix_.columns();
	 }
	while ((row+=aTypeMatrix_.columns())<=aTypeMatrix_.data()+aTypeMatrix_.length());
      }
     dp=d->elements()+aTypeMatrix_.columns();
     mp=bTypeMatrix_.data();
     row=bTypeMatrix_.data()+bTypeMatrix_.columns();
     if (mp!=0)
      {
	do
	 {
	   while (mp<row) *dp++=*mp++;
	   dp+=aTypeMatrix_.columns();
	 }
	while ((row+=bTypeMatrix_.columns())<=bTypeMatrix_.data()+bTypeMatrix_.length());
      }
   }
  return MSTypeMatrix<Type>(d,aTypeMatrix_.rows(),aTypeMatrix_.columns()+bTypeMatrix_.columns());
}

template<class Type>
MSTypeMatrix<Type>& MSTypeMatrix<Type>::adjoin(const MSTypeMatrix<Type>& aTypeMatrix_)
{
  if (rows()!=aTypeMatrix_.rows())
   {
     aTypeMatrix_.error("nonconformant MSTypeMatrix adjoin operands.");
     return *this;
   }
  MSTypeData<Type,MSAllocator<Type> > *d=0;
  unsigned newLength=rows()*(columns()+aTypeMatrix_.columns());
  if (newLength>0)
   {
     d=MSTypeData<Type,MSAllocator<Type> >::allocateWithLength(newLength);     
     Type *dp=d->elements();
     Type *mp=data();
     Type *row=data()+columns();
     if (mp!=0)
      {
	do 
	 {
	   while (mp<row) *dp++=*mp++;
	   dp+=aTypeMatrix_.columns();
	 }
	while ((row+=aTypeMatrix_.columns())<=aTypeMatrix_.data()+aTypeMatrix_.length());
      }
     dp=d->elements()+columns();
     mp=aTypeMatrix_.data();
     row=aTypeMatrix_.data()+aTypeMatrix_.columns();
     if (mp!=0)
      {
	do
	 {
	   while (mp<row) *dp++=*mp++;
	   dp+=columns();
	 }
	while ((row+=aTypeMatrix_.columns())<=aTypeMatrix_.data()+aTypeMatrix_.length());
      }
   }

  freeData();
  _pData=d,_columns+=aTypeMatrix_.columns(),_count=newLength;
  if (receiverList()!=0&&aTypeMatrix_.length()>0) changed();
  return *this;
}

template<class Type>
MSTypeMatrix<Type>& MSTypeMatrix<Type>::random(unsigned long limit_)
{
  int n=length();
  if (n>0)
   {
     prepareToChange();
     if (limit_==0) limit_=length();
     Type *dp=data();
     MSRandom rand;
     while (n--) *dp++=(Type)rand(limit_);
     changed();
   }
  return *this;
}

template<class Type>
Type MSTypeMatrix<Type>::min(void) const 
{
  Type min;
  unsigned n=length();
  if (n>0)
   {
     Type *dp=data();
     min=dp[0];
     for (unsigned i=1;i<n;i++) if (dp[i]<min) min=dp[i];
   }
  else min=0;
  return min;
}

template<class Type>
Type MSTypeMatrix<Type>::max(void) const 
{
  Type max;
  unsigned n=length();
  if (n>0)
   {
     Type *dp=data();
     max=dp[0];
     for (unsigned i=1;i<n;i++) if (dp[i]>max) max=dp[i];
   }
  else max=0;
  return max;
}

template<class Type>
double MSTypeMatrix<Type>::sum(void) const 
{
  double sum=0.0;
  Type *dp=data();
  unsigned n=length();
  while (n--) sum+=*dp++;
  return sum;
}

template<class Type>
void MSTypeMatrix<Type>::error(const char* msg_) const
{ (*_matrixErrorHandler)(msg_); }

//------------------------------------------------------------------------------------------
// matrix manipulation methods
//-------------------------------------------------------------------------------------------------
template<class Type>
MSTypeMatrix<Type>& MSTypeMatrix<Type>::appendColumn(const MSTypeVector<Type>& aTypeVector_)
{
  if (rows()==0||aTypeVector_.length()!=rows())
   {
     error("MSTypeMatrix length error.");
     return *this;
   }
  unsigned newLength=rows()*(columns()+1);
  MSTypeData<Type,MSAllocator<Type> > *d=MSTypeData<Type,MSAllocator<Type> >::allocateWithLength(newLength);  
  Type *dp=d->elements();
  Type *mp=data();
  Type *vp=aTypeVector_.data();
  unsigned i,j;
  for (j=0;j<rows();j++)
   {
     for (i=0;i<columns();i++) *dp++=*mp++;
     *dp++=*vp++;
   }
  freeData();
  _pData=d;
  _columns++;
  _count=newLength;
  changed();
  return *this;
}

template<class Type>
MSTypeMatrix<Type>& MSTypeMatrix<Type>::appendColumns(unsigned cols_,Type fill_)
{
  if (rows()==0)
   {
     error("MSTypeMatrix length error.");
     return *this;
   }
  unsigned newLength=rows()*(columns()+cols_);
  MSTypeData<Type,MSAllocator<Type> > *d=MSTypeData<Type,MSAllocator<Type> >::allocateWithLength(newLength);  
  Type *dp=d->elements();
  Type *mp=data();
  unsigned i,j;
  for (j=0;j<rows();j++)
   {
     for (i=0;i<columns();i++) *dp++=*mp++;
     for (i=0;i<cols_;i++) *dp++=fill_;
   }
  freeData();
  _pData=d;
  _columns+=cols_;
  _count=newLength;
  changed();  
  return *this;
}

template<class Type>
MSTypeMatrix<Type>& MSTypeMatrix<Type>::appendRow(const MSTypeVector<Type>& aTypeVector_)
{
  if (columns()==0||aTypeVector_.length()!=columns())
   {
     error("MSTypeMatrix length error.");
     return *this;
   }
  unsigned newLength=(rows()+1)*columns();
  MSTypeData<Type,MSAllocator<Type> > *d=MSTypeData<Type,MSAllocator<Type> >::allocateWithLength(newLength);  
  Type *dp=d->elements();
  Type *mp=data();
  Type *vp=aTypeVector_.data();
  unsigned i;
  for (i=0;i<length();i++) *dp++=*mp++;
  for (i=0;i<columns();i++) *dp++=*vp++;
  freeData();
  unsigned oldLength=length();
  _pData=d;
  _rows++;
  _count=newLength;
  if (receiverList()!=0)
   {
     MSIndexVector iv;
     iv.series(columns(),oldLength);
     changed(iv);
   }
  return *this;
}

template<class Type>
MSTypeMatrix<Type>& MSTypeMatrix<Type>::appendRows(unsigned rows_, Type fill_)
{
  if (columns()==0)
   {
     error("MSTypeMatrix length error.");
     return *this;
   }
  unsigned newLength=(rows()+rows_)*columns();
  MSTypeData<Type,MSAllocator<Type> > *d=MSTypeData<Type,MSAllocator<Type> >::allocateWithLength(newLength);  
  Type *dp=d->elements();
  Type *mp=data();
  unsigned i,appendLength=columns()*rows_;
  for (i=0;i<length();i++) *dp++=*mp++;
  for (i=0;i<appendLength;i++) *dp++=fill_;
  freeData();
  unsigned oldLength=length();
  _pData=d;
  _rows+=rows_;
  _count=newLength;
  if (receiverList()!=0)
   {
     MSIndexVector iv;
     iv.series(appendLength,oldLength);
     changed(iv);
   }
  return *this;
}

template<class Type>
MSTypeMatrix<Type>& MSTypeMatrix<Type>::exchangeColumns(unsigned aColumn_,unsigned bColumn_)
{
  if (aColumn_+1>columns()||bColumn_+1>columns()||aColumn_==bColumn_) return *this;

  prepareToChange();

  Type *aPtr=data()+aColumn_;
  Type *bPtr=data()+bColumn_;
  register Type tVal;

  for (unsigned i=0;i<rows();i++)
   {
     tVal=*aPtr;
     *aPtr=*bPtr;
     *bPtr=tVal;
     aPtr+=columns(),bPtr+=columns();
   }
  changed();
  return *this;
}

template<class Type>
MSTypeMatrix<Type>& MSTypeMatrix<Type>::exchangeRows(unsigned aRow_,unsigned bRow_)
{
  if (aRow_+1>rows()||bRow_+1>rows()||aRow_==bRow_) return *this;

  prepareToChange();

  Type *aPtr=data()+aRow_*columns();
  Type *bPtr=data()+bRow_*columns();
  register Type tVal;

  for (unsigned i=0;i<columns();i++)
   {
     tVal=*aPtr;
     *aPtr++=*bPtr;
     *bPtr++=tVal;
   }
  changed();  
  return *this;
}

template<class Type>
MSTypeMatrix<Type>& MSTypeMatrix<Type>::insertColumnBefore(unsigned col_,Type fill_)
{
  if (col_+1>columns()) return *this;
  
  unsigned newLength=rows()*(columns()+1);
  MSTypeData<Type,MSAllocator<Type> > *d=MSTypeData<Type,MSAllocator<Type> >::allocateWithLength(newLength);  
  Type *dp=d->elements();
  Type *mp=data();
  unsigned i,j;
  
  for (j=0;j<rows();j++)
   {
     for (i=0;i<columns()+1;i++) *dp++=(i==col_)?fill_:*mp++;
   }
  freeData();
  _pData=d;
  _columns++;
  _count=newLength;
  changed();  
  return *this;
}

template<class Type>
MSTypeMatrix<Type>& MSTypeMatrix<Type>::insertColumnBefore(unsigned col_,const MSTypeVector<Type>& aTypeVector_)
{
  if (col_+1>columns()) return *this;
  if (aTypeVector_.length()!=rows())
   {
     error("MSTypeMatrix length error.");
     return *this;
   }
  
  unsigned newLength=rows()*(columns()+1);
  MSTypeData<Type,MSAllocator<Type> > *d=MSTypeData<Type,MSAllocator<Type> >::allocateWithLength(newLength);  
  Type *dp=d->elements();
  Type *mp=data();
  Type *vp=aTypeVector_.data();
  unsigned i,j;
  
  for (j=0;j<rows();j++)
   {
     for (i=0;i<columns()+1;i++) *dp++=(i==col_)?*vp++:*mp++;
   }
  freeData();
  _pData=d;
  _columns++;
  _count=newLength;
  changed();  
  return *this;
}

template<class Type>
MSTypeMatrix<Type>& MSTypeMatrix<Type>::insertColumnAfter(unsigned col_,Type fill_)
{
  if (col_+1>columns()) return *this;

  unsigned newLength=rows()*(columns()+1);
  MSTypeData<Type,MSAllocator<Type> > *d=MSTypeData<Type,MSAllocator<Type> >::allocateWithLength(newLength);  
  Type *dp=d->elements();
  Type *mp=data();
  unsigned i,j;
  
  for (j=0;j<rows();j++)
   {
     for (i=0;i<columns()+1;i++) *dp++=(i==col_+1)?fill_:*mp++;
   }
  freeData();
  _pData=d;
  _columns++;
  _count=newLength;
  changed();  
  return *this;
}

template<class Type>
MSTypeMatrix<Type>& MSTypeMatrix<Type>::insertColumnAfter(unsigned col_,const MSTypeVector<Type>& aTypeVector_)
{
  if (col_+1>columns()) return *this;
  if (aTypeVector_.length()!=rows())
   {
     error("MSTypeMatrix length error.");
     return *this;
   }

  unsigned newLength=rows()*(columns()+1);
  MSTypeData<Type,MSAllocator<Type> > *d=MSTypeData<Type,MSAllocator<Type> >::allocateWithLength(newLength);  
  Type *dp=d->elements();
  Type *mp=data();
  Type *vp=aTypeVector_.data();
  unsigned i,j;
  
  for (j=0;j<rows();j++)
   {
     for (i=0;i<columns()+1;i++) *dp++=(i==col_+1)?*vp++:*mp++;
   }
  freeData();
  _pData=d;
  _columns++;
  _count=newLength;
  changed();  
  return *this;
}

template<class Type>
MSTypeMatrix<Type>& MSTypeMatrix<Type>::insertRowBefore(unsigned row_,Type fill_)
{
  if (row_+1>rows()) return *this;

  unsigned newLength=(rows()+1)*columns();
  MSTypeData<Type,MSAllocator<Type> > *d=MSTypeData<Type,MSAllocator<Type> >::allocateWithLength(newLength);  
  Type *dp=d->elements();
  Type *mp=data();
  unsigned i,j;
  
  for (j=0;j<rows()+1;j++)
   {
     for (i=0;i<columns();i++) *dp++=(j==row_)?fill_:*mp++;
   }
  freeData();
  _pData=d;
  _rows++;
  _count=newLength;
  changed();  
  return *this;
}

template<class Type>
MSTypeMatrix<Type>& MSTypeMatrix<Type>::insertRowBefore(unsigned row_,const MSTypeVector<Type>& aTypeVector_)
{
  if (row_+1>rows()) return *this;
  if (aTypeVector_.length()!=columns())
   {
     error("MSTypeMatrix length error.");
     return *this;
   }

  unsigned newLength=(rows()+1)*columns();
  MSTypeData<Type,MSAllocator<Type> > *d=MSTypeData<Type,MSAllocator<Type> >::allocateWithLength(newLength);  
  Type *dp=d->elements();
  Type *mp=data();
  Type *vp=aTypeVector_.data();
  unsigned i,j;
  
  for (j=0;j<rows()+1;j++)
   {
     for (i=0;i<columns();i++) *dp++=(j==row_)?*vp++:*mp++;
   }
  freeData();
  _pData=d;
  _rows++;
  _count=newLength;
  changed();  
  return *this;
}

template<class Type>
MSTypeMatrix<Type>& MSTypeMatrix<Type>::insertRowAfter(unsigned row_,Type fill_)
{
  if (row_+1>rows()) return *this;

  unsigned newLength=(rows()+1)*columns();
  MSTypeData<Type,MSAllocator<Type> > *d=MSTypeData<Type,MSAllocator<Type> >::allocateWithLength(newLength);  
  Type *dp=d->elements();
  Type *mp=data();
  unsigned i,j;
  
  for (j=0;j<rows()+1;j++)
   {
     for (i=0;i<columns();i++) *dp++=(j==row_+1)?fill_:*mp++;
   }
  freeData();
  _pData=d;
  _rows++;
  _count=newLength;
  changed();  
  return *this;
}

template<class Type>
MSTypeMatrix<Type>& MSTypeMatrix<Type>::insertRowAfter(unsigned row_,const MSTypeVector<Type>& aTypeVector_)
{
  if (row_>rows()) return *this;
  if (aTypeVector_.length()!=columns())
   {
     error("MSTypeMatrix length error.");
     return *this;
   }

  unsigned newLength=(rows()+1)*columns();
  MSTypeData<Type,MSAllocator<Type> > *d=MSTypeData<Type,MSAllocator<Type> >::allocateWithLength(newLength);  
  Type *dp=d->elements();
  Type *mp=data();
  Type *vp=aTypeVector_.data();
  unsigned i,j;
  
  for (j=0;j<rows()+1;j++)
   {
     for (i=0;i<columns();i++) *dp++=(j==row_+1)?*vp++:*mp++;
   }
  freeData();
  _pData=d;
  _rows++;
  _count=newLength;
  changed();  
  return *this;
}

template<class Type>
MSTypeMatrix<Type>& MSTypeMatrix<Type>::reshape(unsigned rows_,unsigned cols_)
{
  MSTypeData<Type,MSAllocator<Type> > *d=0;
  unsigned newLength=rows_*cols_;
  if (newLength>0)
   {
     d=MSTypeData<Type,MSAllocator<Type> >::allocateWithLength(newLength);     
     Type *dp=d->elements();
     Type *mp=data();
     Type *end=data()+length();
     if (mp==0) for (unsigned i=0;i<newLength;i++) *dp++=0;
     else if (length()>newLength) for (unsigned i=0;i<newLength;i++) *dp++=*mp++;
     else 
      {
	for (unsigned i=0;i<newLength;i++) 
	 { 
	   *dp++=*mp++;
	   if (mp==end) mp=data();
	 }
      }
   }
  freeData();
  _pData=d;
  _rows=rows_;
  _columns=cols_;
  _count=newLength;
  changed();  
  return *this;
}

template<class Type>
MSTypeMatrix<Type>& MSTypeMatrix<Type>::removeAll(void)
{
  freeData();
  _rows=0,_columns=0,_count=0;
  changed();  
  return *this;
}

template<class Type>
MSTypeMatrix<Type>& MSTypeMatrix<Type>::removeAllRows(void)
{
  freeData();
  _rows=0,_count=0;
  changed();  
  return *this;
}

template<class Type>
MSTypeMatrix<Type>& MSTypeMatrix<Type>::removeAllColumns(void)
{
  freeData();
  _columns=0,_count=0;
  changed();  
  return *this;
}

template<class Type>
MSTypeMatrix<Type>& MSTypeMatrix<Type>::assignColumn(unsigned col_,Type scalar_)
{
  if (col_+1>columns()) return *this;
  prepareToChange();
  Type *mp=data(); 
  unsigned i,j;
  if (receiverList()!=0)
   {
     MSIndexVector iv(rows());
     for (i=0,j=col_;i<rows();i++,j+=columns()) mp[j]=scalar_,iv.set(i,j);     
     changed(iv);
   }
  else for (i=0,j=col_;i<rows();i++,j+=columns()) mp[j]=scalar_;
  return *this;
}

template<class Type>
MSTypeMatrix<Type>& MSTypeMatrix<Type>::assignRow(unsigned row_,Type scalar_)
{
  if (row_+1>rows()) return *this;
  prepareToChange();
  Type *mp=data(); 
  unsigned i,j;
  for (i=row_*columns(),j=0;j<columns();i++,j++) mp[i]=scalar_;
  if (receiverList()!=0)
   {
     MSIndexVector iv;
     changed(iv.series(columns(),row_*columns()));
   }
  return *this;
}

template<class Type>
MSTypeMatrix<Type>& MSTypeMatrix<Type>::assignColumn(unsigned col_, const MSTypeVector<Type>& aTypeVector_)
{
  if (col_+1>columns()) return *this;
  if (aTypeVector_.length()!=rows())
   {
     error("MSTypeMatrix length error.");
     return *this;
   }
  prepareToChange();
  Type *mp;
  unsigned i;
  for (i=0,mp=data()+col_;i<rows();i++,mp+=columns()) *mp=aTypeVector_(i);
  changed();
  return *this;
}

template<class Type>
MSTypeMatrix<Type>& MSTypeMatrix<Type>::assignRow(unsigned row_, const MSTypeVector<Type>& aTypeVector_)
{
  if (row_+1>rows()) return *this;
  if (aTypeVector_.length()!=columns())
   {
     error("MSTypeMatrix length error.");
     return *this;
   }
  prepareToChange();
  Type *mp;
  unsigned i;
  for (i=0,mp=data()+row_*columns();i<columns();i++,mp++) *mp=aTypeVector_(i);
  if (receiverList()!=0)
   {
     MSIndexVector iv;
     changed(iv.series(columns(),row_*columns()));
   }
  return *this;
}

template<class Type>
MSTypeMatrix<Type>& MSTypeMatrix<Type>::removeRow(unsigned row_)
{
  if (row_+1>rows()) return *this;
  if (data()==0) return *this;
  
  unsigned newLength=(rows()-1)*columns();
  MSTypeData<Type,MSAllocator<Type> > *d=MSTypeData<Type,MSAllocator<Type> >::allocateWithLength(newLength);  
  Type *dp=d->elements();
  Type *mp=data();
  unsigned i,j;
  for (i=0;i<rows();i++)
   {
     if (i==row_) mp+=columns();
     else for (j=0;j<columns();j++) *dp++=*mp++;
   }
  freeData();
  _pData=d;
  _rows-=1;
  _count=newLength;
  changed();
  return *this;
}

template<class Type>
MSTypeMatrix<Type>& MSTypeMatrix<Type>::removeColumn(unsigned col_)
{
  if (col_+1>columns()) return *this;
  if (data()==0) return *this;

  unsigned newLength=rows()*(columns()-1);
  MSTypeData<Type,MSAllocator<Type> > *d=MSTypeData<Type,MSAllocator<Type> >::allocateWithLength(newLength);  
  Type *dp=d->elements();
  Type *mp=data();
  unsigned i,j;
  for (i=0;i<rows();i++)
   {
     for (j=0;j<columns();j++)
      {
	if (j==col_) mp++;
        else *dp++=*mp++;
      }
   }
  freeData();
  _pData=d;
  _columns-=1;
  _count=newLength;
  changed();  
  return *this;
}

template<class Type>
MSTypeMatrix<Type>& MSTypeMatrix<Type>::compressRows(const MSBinaryVector& aBinaryVector_ )
{
  if (data()==0) return *this;
  if (aBinaryVector_.length()!=rows())
   {
     error("MSTypeMatrix length error.");
     return *this;
   }
  
  unsigned newLength=(unsigned)(aBinaryVector_.sum()*columns());
  MSTypeData<Type,MSAllocator<Type> > *d=MSTypeData<Type,MSAllocator<Type> >::allocateWithLength(newLength);  
  Type *dp=d->elements();
  Type *mp=data();
  unsigned i,j;
  for (i=0;i<rows();i++)
   {
     if (aBinaryVector_(i)==0) mp+=columns();
     else for (j=0;j<columns();j++) *dp++=*mp++;
   }
  freeData();
  _pData=d;
  _rows=(unsigned int)aBinaryVector_.sum();
  _count=newLength;
  changed();  
  return *this;
}

template<class Type>
MSTypeMatrix<Type>& MSTypeMatrix<Type>::compressColumns(const MSBinaryVector& aBinaryVector_ )
{
  if (data()==0) return *this;
  if (aBinaryVector_.length()!=columns())
   {
     error("MSTypeMatrix length error.");
     return *this;
   }
  
  unsigned newLength=(unsigned)(rows()*aBinaryVector_.sum());
  MSTypeData<Type,MSAllocator<Type> > *d=MSTypeData<Type,MSAllocator<Type> >::allocateWithLength(newLength);  
  Type *dp=d->elements();
  Type *mp=data();
  unsigned i,j;

  for (i=0;i<rows();i++)
   {
     for (j=0;j<columns();j++)
      {
	if (aBinaryVector_(j)==0) mp++;
        else *dp++=*mp++;
      }
   }
  freeData();
  _pData=d;
  _columns=(unsigned int)aBinaryVector_.sum();
  _count=newLength;
  changed();  
  return *this;
}

template<class Type>
MSTypeMatrix<Type>& MSTypeMatrix<Type>::reverseRows(void)
{
  if (data()==0) return *this;
  prepareToChange();
  unsigned i,j;
  Type *upperRow=data();
  Type *lowerRow=data()+rows()*columns()-columns();
  register Type tVal;
  unsigned n=unsigned(rows()/2);
  for (i=0;i<n;i++)
   {
     for (j=0;j<columns();j++)
      {
	tVal=*(upperRow+j);
	*(upperRow+j)=*(lowerRow+j);
	*(lowerRow+j)=tVal;
      }
     upperRow+=columns();
     lowerRow-=columns();
   }
  changed();  
  return *this;
}

template<class Type>
MSTypeMatrix<Type>& MSTypeMatrix<Type>::reverseColumns(void)
{
  if (data()==0) return *this;
  prepareToChange();
  unsigned i,j;
  Type *leftColumn=data();
  Type *rightColumn=data()+columns()-1;
  register Type tVal;
  unsigned n=unsigned(columns()/2);
  for (i=0;i<rows();i++)
   {
     for (j=0;j<n;j++)
      {
	tVal=*(leftColumn+j);
	*(leftColumn+j)=*(rightColumn-j);
	*(rightColumn-j)=tVal;
      }
     leftColumn+=columns();
     rightColumn+=columns();
   }
  changed();  
  return *this;
}

template<class Type>
MSTypeMatrix<Type>& MSTypeMatrix<Type>::transpose(void)
{
  if (data()==0) return *this;
  unsigned aSize=size();
  MSTypeData<Type,MSAllocator<Type> > *d=MSTypeData<Type,MSAllocator<Type> >::allocateWithSize(aSize);
  Type *dp=d->elements();
  Type *mp=data();
  unsigned i,j;
  for (i=0;i<columns();i++)
   {
     for (j=0;j<rows();j++,mp+=columns())
      {
        *dp++=*(mp+i);
      }
     mp=data();
   }
  freeData();
  _pData=d;
  i=columns();
  _columns=_rows;
  _rows=i;
  changed();  
  return *this;
}

template<class Type>
MSTypeMatrix<Type>& MSTypeMatrix<Type>::rotateRows(int position_)
{
  int rowPosition=MSUtil::abs(position_);
  if (rowPosition>0&&rowPosition!=rows())
   {
     unsigned i;
     unsigned aSize=size();
     MSTypeData<Type,MSAllocator<Type> > *d=MSTypeData<Type,MSAllocator<Type> >::allocateWithSize(aSize);
     if (rowPosition>rows()) rowPosition%=rows();
     if (position_<0) rowPosition=rows()-rowPosition;
     unsigned start=rowPosition*columns();
     Type *mp=data()+start;
     Type *dp=d->elements();
     for (i=start;i<length();i++) *dp++=*mp++;
     mp=data();
     for (i=0;i<start;i++) *dp++=*mp++;
     freeData();
     _pData=d;
     changed();     
   }
  return *this;
}

template<class Type>
MSTypeMatrix<Type>& MSTypeMatrix<Type>::rotateColumns(int position_)
{
  int columnPosition=MSUtil::abs(position_);
  if (columnPosition>0&&columnPosition!=columns())
   {
     unsigned i,j;
     unsigned aSize=size();
     MSTypeData<Type,MSAllocator<Type> > *d=MSTypeData<Type,MSAllocator<Type> >::allocateWithSize(aSize);
     if (columnPosition>columns()) columnPosition%=columns();
     if (position_<0) columnPosition=columns()-columnPosition;
     Type *mp=data()+columnPosition;
     Type *dp=d->elements();
     for (j=0;j<rows();j++)
      {
        for (i=columnPosition;i<columns();i++) *dp++=*mp++;
	mp-=columns();
        for (i=0;i<columnPosition;i++) *dp++=*mp++;
	mp+=columns();
      }
     freeData();     
     _pData=d;
     changed();     
   }
  return *this;
}

template<class Type>
MSTypeMatrix<Type>& MSTypeMatrix<Type>::takeRows(int numberOfRows_)
{
  int numberOfRows=MSUtil::abs(numberOfRows_);
  if (numberOfRows>0&&numberOfRows!=rows())
   {
     unsigned i;
     unsigned newLength=numberOfRows*columns();
     MSTypeData<Type,MSAllocator<Type> > *d=MSTypeData<Type,MSAllocator<Type> >::allocateWithLength(newLength);
     Type *mp=data();
     Type *dp=d->elements();
     if (numberOfRows>rows())
      {
	if (numberOfRows_>0) for (i=0;i<newLength;i++) *dp++=i<length()?*mp++:0;
	else for (i=0;i<newLength;i++) *dp++=i<newLength-length()?0:*mp++;
      }
     else 
      {
	if (numberOfRows_>0) for (i=0;i<newLength;i++) *dp++=*mp++;
	else 
         {
	   mp+=length()-newLength;
	   for (i=0;i<newLength;i++) *dp++=*mp++;
	 }
      }
     freeData();     
     _pData=d;
     _rows=numberOfRows;
     _count=newLength;
     changed();     
   }
  return *this;
}

template<class Type>
MSTypeMatrix<Type>& MSTypeMatrix<Type>::takeColumns(int numberOfColumns_)
{
  int numberOfColumns=MSUtil::abs(numberOfColumns_);
  if (numberOfColumns>0&&numberOfColumns!=columns())
   {
     unsigned i,j;
     unsigned newLength=numberOfColumns*rows();
     MSTypeData<Type,MSAllocator<Type> > *d=MSTypeData<Type,MSAllocator<Type> >::allocateWithLength(newLength);     
     Type *mp=data();
     Type *dp=d->elements();
     if (numberOfColumns>columns())
      {
	if (numberOfColumns_>0)
	 {
	   for (i=0;i<rows();i++)
	    {
	      for (j=0;j<numberOfColumns;j++) *dp++=j<columns()?*mp++:0;
	    } 
	 }
	else 
	 {
	   for (i=0;i<rows();i++)
	    {
	      for (j=0;j<numberOfColumns;j++) *dp++=j<numberOfColumns-columns()?0:*mp++;
	    } 
	 }
      }
     else 
      {
	if (numberOfColumns_>0)
	 {
	   for (i=0;i<rows();i++)
	    {
	      for (j=0;j<numberOfColumns;j++)
	       {
		 *dp++=*mp++;
	       }
	      mp+=columns()-numberOfColumns;
	    }
	 }
	else 
         {
	   for (i=0;i<rows();i++)
	    {
	      mp+=columns()-numberOfColumns;
	      for (j=0;j<numberOfColumns;j++)
	       {
		 *dp++=*mp++;
	       }
	    }
	 }
      }
     freeData();     
     _pData=d;
     _columns=numberOfColumns;
     _count=newLength;
     changed();     
   }
  return *this;
}

template<class Type>
MSTypeMatrix<Type>& MSTypeMatrix<Type>::dropRows(int numberOfRows_)
{
  int numberOfRows=MSUtil::abs(numberOfRows_);
  if (numberOfRows>0)
   {
     if (numberOfRows<rows())
      {
	unsigned i;
	unsigned nRows=rows()-numberOfRows;
	unsigned newLength=nRows*columns();
	MSTypeData<Type,MSAllocator<Type> > *d=MSTypeData<Type,MSAllocator<Type> >::allocateWithLength(newLength);	
	Type *mp=data();
	Type *dp=d->elements();
	if (numberOfRows_>0) mp+=numberOfRows*columns();
	for (i=0;i<newLength;i++) *dp++=*mp++;
	freeData();     
	_pData=d;
	_rows=nRows;
	_count=newLength;
      }
     else 
      {
	freeData();     
	_rows=0,_columns=0,_count=0;
      }
     changed();     
   }
  return *this;
}

template<class Type>
MSTypeMatrix<Type>& MSTypeMatrix<Type>::dropColumns(int numberOfColumns_)
{
  int numberOfColumns=MSUtil::abs(numberOfColumns_);
  if (numberOfColumns>0)
   {
     if (numberOfColumns<columns())
      {
	unsigned i,j;
	unsigned cols=columns()-numberOfColumns;
	unsigned newLength=cols*rows();
	MSTypeData<Type,MSAllocator<Type> > *d=MSTypeData<Type,MSAllocator<Type> >::allocateWithLength(newLength);	
	Type *mp=data();
	Type *dp=d->elements();
	if (numberOfColumns_>0)
	 {
	   for (i=0;i<rows();i++)
	    {
	      mp+=numberOfColumns;
	      for (j=0;j<cols;j++) *dp++=*mp++;
	    }
	 }
	else 
	 {
	   for (i=0;i<rows();i++)
	    {
	      for (j=0;j<cols;j++) *dp++=*mp++;
	      mp+=numberOfColumns;
	    }
	 }
	freeData();     
	_pData=d;
	_columns=cols;
	_count=newLength;
      }
     else 
      {
	freeData();     
	_rows=0,_columns=0,_count=0;
      }
     changed();     
   }
  return *this;
}

template<class Type>
MSTypeVector<Type> MSTypeMatrix<Type>::rowAt(unsigned row_) const
{
  if (row_+1>rows()||columns()==0) return MSTypeVector<Type>();
  MSTypeData<Type,MSAllocator<Type> > *d=MSTypeData<Type,MSAllocator<Type> >::allocateWithLength(columns());  
  Type *mp;
  Type *dp=d->elements();
  unsigned i;
  for (i=0,mp=data()+row_*columns();i<columns();i++,mp++) *dp++=*mp;

  return MSTypeVector<Type>(d,columns());
}

template<class Type>
MSTypeVector<Type> MSTypeMatrix<Type>::columnAt(unsigned column_) const
{
  if (column_+1>columns()||rows()==0) return MSTypeVector<Type>();

  MSTypeData<Type,MSAllocator<Type> > *d=MSTypeData<Type,MSAllocator<Type> >::allocateWithLength(rows());  
  Type *mp;
  Type *dp=d->elements();
  unsigned i;
  for (i=0,mp=data()+column_;i<rows();i++,mp+=columns()) *dp++=*mp;

  return MSTypeVector<Type>(d,rows());
}



#endif
