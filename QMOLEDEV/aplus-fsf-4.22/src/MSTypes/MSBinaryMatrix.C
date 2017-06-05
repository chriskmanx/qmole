///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////

#if HAVE_IOSTREAM
#include <iostream>
#else
#include <iostream.h>
#endif
#if HAVE_SSTREAM
#include <sstream>
#include <iosfwd>
#else
#include <strstream.h>
#endif

#include <MSTypes/MSBinaryVector.H>
#include <MSTypes/MSIndexVector.H>

#include <MSTypes/MSRandom.H>
#include <MSTypes/MSUtil.H>
#include <MSTypes/MSTypes_MSF.H>
#include <MSTypes/MSBinaryMatrix.H>

unsigned char MSBinaryMatrix::_badData=0;

const MSSymbol& MSBinaryMatrix::type(void) const 
{ return symbol(); }

MSString MSBinaryMatrix::className(void) const
{ return MSString("MSBinaryMatrix"); }

MSString MSBinaryMatrix::asString(void) const
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

MSString MSBinaryMatrix::asMSF(void) const
{
  MSString result;
  
  if (_count>0)
   {
     result+=MSMSF_US;
     result+=MSString(_rows);
     result+=MSMSF_US;
     result+=MSString(_columns);
     for (unsigned i=0;i<_count;i++) 
      {
	result+=MSMSF_US;
        result+=MSString((unsigned)data()[i]);
      }
   }
  return result;
}

MSString MSBinaryMatrix::asDebugInfo(void) const
{
  MSString result("MSBinaryMatrix(@");
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

MSModel *MSBinaryMatrix::clone(void) const
{ return new MSBinaryMatrix(*this); }

MSModel *MSBinaryMatrix::create(void) const
{ return new MSBinaryMatrix(); }

void MSBinaryMatrix::assign(const MSModel& aModel_)
{ *this=(MSBinaryMatrix&)aModel_; }

long MSBinaryMatrix::compare(const MSModel& aModel_) const
{ return ::compare(*this,(MSBinaryMatrix&)aModel_); }

const MSSymbol& MSBinaryMatrix::symbol(void)   
{
  static MSSymbol sym ("MSBinaryMatrix");
  return sym;
}

void MSBinaryMatrix::blockLeft(unsigned target_,unsigned moveCount_)
{ 
  unsigned char *dp=data();
  for (unsigned i=target_;moveCount_>0;i++,moveCount_--) dp[i]=dp[i+1]; 
}

void MSBinaryMatrix::blockRight(unsigned target_,unsigned moveCount_)
{ 
  unsigned char *dp=data();
  for (unsigned i=target_+moveCount_-1;moveCount_>0;i--,moveCount_--) dp[i]=dp[i-1]; 
}

void MSBinaryMatrix::allocData(unsigned length_)
{
  _count=length_;
  if (length()>0)
   {
     _pData=MSTypeData<unsigned char,MSAllocator<unsigned char> >::allocateWithLength(length());     
   }
  else
   {
     _pData=0;
   }
}

void MSBinaryMatrix::makeUniqueCopy(void)
{
  if (_pData!=0)
   {
     MSTypeData<unsigned char,MSAllocator<unsigned char> > *dst=MSTypeData<unsigned char,MSAllocator<unsigned char> >::allocateWithSize(size());
     MSTypeData<unsigned char,MSAllocator<unsigned char> >::copy(_pData->elements(),dst->elements(),length()); //src,dst,len
     _pData->decrementCount();
     _pData=dst;
   }
}

void MSBinaryMatrix::prepareToChange(void)
{
  if (_pData!=0)
   {
     if (_pData->refCount()>1) makeUniqueCopy();
   }
  else allocData(length());
}

void MSBinaryMatrix::freeData(void)
{ decrementCount(),_pData=0; }

// make sure there are at least size_+1 elements in the array
void MSBinaryMatrix::reserve(unsigned size_)
{
  unsigned n=size_+1;
  if (size()<n)
   {
     MSTypeData<unsigned char,MSAllocator<unsigned char> > *newData=MSTypeData<unsigned char,MSAllocator<unsigned char> >::allocateWithLength(n);     
     unsigned char *dp=newData->elements();
     unsigned char *sp=data();
     MSTypeData<unsigned char,MSAllocator<unsigned char> >::copy(sp,dp,size());
     freeData();
     _pData=newData;
   }
}

//-------------------------------------------------------------------------------------------------
// MSBinaryMatrix
//-------------------------------------------------------------------------------------------------
ostream& operator<<(ostream& aStream_,const MSBinaryMatrix& aBinaryMatrix_)
{
  unsigned r=aBinaryMatrix_.rows();
  unsigned c=aBinaryMatrix_.columns();
  for (unsigned i=0;i<r;i++) 
   {
     for (unsigned j=0;j<c;j++)
      {
	aStream_<<(unsigned)aBinaryMatrix_(i,j)<<" ";
      }
     aStream_<<endl;
   }
  return aStream_<<flush;
}

MSBinaryMatrix::MSBinaryMatrix(void) 
:MSMatrix()
{ _pData=0;_blocked=MSFalse; }

MSBinaryMatrix::MSBinaryMatrix(unsigned rows_,unsigned cols_)
:MSMatrix(rows_,cols_)
{
  allocData(length());
  _blocked=MSFalse;
}

MSBinaryMatrix::MSBinaryMatrix(unsigned rows_,unsigned cols_,unsigned char fill_) 
:MSMatrix(rows_,cols_)
{
  if (length()>0)
   {
     unsigned n=length();
     unsigned char fill=fill_>0?1:0;
     allocData(n);
     unsigned char *dp=data();
     while (n--) *dp++=fill;
   }
  else _pData=0;
  _blocked=MSFalse;
}

MSBinaryMatrix::MSBinaryMatrix(const MSBinaryMatrix& aBinaryMatrix_) 
:MSMatrix(aBinaryMatrix_.rows(),aBinaryMatrix_.columns())
{
  _pData=aBinaryMatrix_.pData();
  _blocked=MSFalse;
  (void)incrementCount();
}

// special private constructor used to avoid temporary object creation
// on return by value - see ARM page 267
MSBinaryMatrix::MSBinaryMatrix(MSTypeData<unsigned char,MSAllocator<unsigned char> > *data_,unsigned rows_,unsigned cols_) 
:MSMatrix(rows_,cols_)
{
  _pData=data_;
  _blocked=MSFalse;
}


MSBinaryMatrix::MSBinaryMatrix(const unsigned char *pElements_, unsigned rows_, unsigned cols_)
: MSMatrix(rows_,cols_)
{
  _pData = MSTypeData<unsigned char,MSAllocator<unsigned char> >::allocateWithLength(_count, MSRaw);
  MSTypeData<unsigned char,MSAllocator<unsigned char> >::copy(pElements_, _pData->elements(), _count, MSRaw);
}


MSBinaryMatrix::~MSBinaryMatrix(void)
{ freeData(); }

MSError::ErrorStatus MSBinaryMatrix::set(const char *pString_)
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
       _pData=MSTypeData<unsigned char,MSAllocator<unsigned char> >::allocateWithLength(length());

     unsigned n=length();
     unsigned char *dp=data();
     for (unsigned i=0;i<n;i++) ist>>dp[i];

     if (!ist) return MSError::MSFailure;
   }
  changed();
  return MSError::MSSuccess;
}

MSError::ErrorStatus MSBinaryMatrix::setFromMSF(const char *pString_)
{
  MSError::ErrorStatus code;
  unsigned int i;
  unsigned int startpos;
  unsigned int value[2];
  const char *pcurrent;
  char *pstring;
  
  if ((pString_!=0&&(*pString_==MSMSF_US)&&(strlen(pString_)>sizeof(MSMSF_US))))
   {
     code=MSError::MSSuccess;
     freeData();
     _rows=0,_columns=0,_count=0;
     
     MSString decode(pString_);
     decode.decodeMSF();
     unsigned int slen=decode.length();
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
	   startpos=decode.indexOf (MSMSF_US, startpos);
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
	_pData=MSTypeData<unsigned char,MSAllocator<unsigned char> >::allocateWithLength(length());	
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
		 else set(i,(unsigned char)0); // make sure fills in on not MxN case 
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
  return code;
}

MSError::ErrorStatus MSBinaryMatrix::set(unsigned index_,unsigned char aValue_)
{ 
  if (index_<length())
   {
     prepareToChange();
     _pData->elements()[index_]=(aValue_==0?0:1);     
     if (doChanged()==MSTrue) changed(index_);
     return MSError::MSSuccess;
   }
  return MSError::MSFailure; 
}

MSError::ErrorStatus MSBinaryMatrix::set(unsigned index_,const char *pString_) 
{
  char *cp=0;
  unsigned long lnum=strtoul(pString_,&cp,10); // Base 10
  if (cp==pString_) return MSError::MSFailure;
  else return set(index_,(unsigned char)(lnum==0?0:1));
}

unsigned MSBinaryMatrix::indexOf(unsigned char aValue_,unsigned startPos_) const
{ 
  unsigned n=length();
  for (unsigned i=startPos_;i<n;i++) if (elementAt(i)==aValue_) return i;
  return n;
}

unsigned MSBinaryMatrix::lastIndexOf(unsigned char aValue_,unsigned startPos_) const
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

// Assign a MSBinaryMatrix to this MSBinaryMatrix
// aBinaryMatrix=bBinaryMatrix
MSBinaryMatrix& MSBinaryMatrix::operator=(const MSBinaryMatrix& aBinaryMatrix_)
{
  if (this!=&aBinaryMatrix_)
   {
     freeData();
     _count=aBinaryMatrix_.length();
     _rows=aBinaryMatrix_.rows();
     _columns=aBinaryMatrix_.columns();
     _pData=aBinaryMatrix_.pData();
     (void)incrementCount();
     changed();
   }
  return *this;
}

// Assign a unsigned index selected element of a MSBinaryMatrix to all elements of this MSBinaryMatrix.
// aBinaryMatrix=bBinaryMatrix[index]
MSBinaryMatrix& MSBinaryMatrix::operator=(const MSBinaryMatrixSTypePick& aPick_)
{
  prepareToChange();
  unsigned n=length();
  unsigned char s=aPick_.value();
  unsigned char *dp=data();
  while (n--) *dp++=s;
  changed();
  return *this;
}

// Assign a unsigned char to all elements of this MSBinaryMatrix.
// aBinaryMatrix=aBinaryValue
MSBinaryMatrix& MSBinaryMatrix::operator=(unsigned char scalar_)
{
  prepareToChange();
  unsigned n=length();
  unsigned char s=scalar_==0?0:1;
  unsigned char *dp=data();
  while (n--) *dp++=s;
  changed();
  return *this;
}

//------------------------------------------------------------------------------------------
// relational operator functions
//------------------------------------------------------------------------------------------
long MSBinaryMatrix::compare(const MSBinaryMatrix& aBinaryMatrix_) const
{
  unsigned n=MSUtil::min(length(),aBinaryMatrix_.length());
  for (unsigned i=0;i<n;i++)
   {
     if (elementAt(i)!=aBinaryMatrix_.elementAt(i))
      {
	if (elementAt(i)<aBinaryMatrix_.elementAt(i)) return -1;
	else return 1;
      }
   }
  if (length()==aBinaryMatrix_.length()) return 0;
  if (length()<aBinaryMatrix_.length()) return -1;
  else return 1;
}

MSBinaryMatrix MSBinaryMatrix::binaryCompare(const MSBinaryMatrix& aBinaryMatrix_,MSComparison aComparison_) const
{
  assert(rows()==aBinaryMatrix_.rows()&&columns()==aBinaryMatrix_.columns());
  unsigned aSize=size();
  unsigned n=length();
  MSTypeData<unsigned char,MSAllocator<unsigned char> > *d=MSTypeData<unsigned char,MSAllocator<unsigned char> >::allocateWithSize(aSize);
  const unsigned char *ap=data();
  const unsigned char *bp=aBinaryMatrix_.data();
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

MSBinaryMatrix MSBinaryMatrix::binaryCompare(unsigned char aBinaryValue_,MSComparison aComparison_) const
{
  unsigned aSize=size();
  unsigned n=length();
  MSTypeData<unsigned char,MSAllocator<unsigned char> > *d=MSTypeData<unsigned char,MSAllocator<unsigned char> >::allocateWithSize(aSize);
  const unsigned char *ap=data();
  unsigned char *dp=d->elements();
  unsigned i;
  switch (aComparison_)
   {
   case MSLessThan:
     for (i=0;i<n;i++) dp[i]=(ap[i]<aBinaryValue_); break;
   case MSLessThanOrEqualTo:
     for (i=0;i<n;i++) dp[i]=(ap[i]<=aBinaryValue_); break;
   case MSEqualTo:
     for (i=0;i<n;i++) dp[i]=(ap[i]==aBinaryValue_); break;
   case MSNotEqualTo:
     for (i=0;i<n;i++) dp[i]=(ap[i]!=aBinaryValue_); break;
   case MSGreaterThan:
     for (i=0;i<n;i++) dp[i]=(ap[i]>aBinaryValue_); break;
   case MSGreaterThanOrEqualTo:
     for (i=0;i<n;i++) dp[i]=(ap[i]>=aBinaryValue_); break;
   }
  return MSBinaryMatrix(d,rows(),columns());
}

MSBoolean MSBinaryMatrix::scalarCompare(unsigned char aBinaryValue_,MSComparison aComparison_) const
{
  unsigned n=length();
  if (n>0)
   {
     const unsigned char *ap=data();
     unsigned i;
     switch (aComparison_)
      {
      case MSLessThan:
	for (i=0;i<n;i++) if (ap[i]>=aBinaryValue_) return MSFalse; break;
      case MSLessThanOrEqualTo:
	for (i=0;i<n;i++) if (ap[i]>aBinaryValue_) return MSFalse; break;
      case MSEqualTo:
	for (i=0;i<n;i++) if (ap[i]!=aBinaryValue_) return MSFalse; break;
      case MSNotEqualTo:
	for (i=0;i<n;i++) if (ap[i]==aBinaryValue_) return MSFalse; break;
      case MSGreaterThan:
	for (i=0;i<n;i++) if (ap[i]<=aBinaryValue_) return MSFalse; break;
      case MSGreaterThanOrEqualTo:
	for (i=0;i<n;i++) if (ap[i]<aBinaryValue_) return MSFalse; break;
      default:
	return MSFalse;
      }
     return MSTrue;
   }
  return aComparison_==MSNotEqualTo?MSTrue:MSFalse;
}

//------------------------------------------------------------------------------------------
// unary operator functions
//------------------------------------------------------------------------------------------

// unary operator ! - negation
MSBinaryMatrix operator!(const MSBinaryMatrix& aBinaryMatrix_)
{
  MSTypeData<unsigned char,MSAllocator<unsigned char> > *d=0;
  unsigned n=aBinaryMatrix_.length();
  unsigned aSize=aBinaryMatrix_.size();
  if (n>0)
   {
     d=MSTypeData<unsigned char,MSAllocator<unsigned char> >::allocateWithSize(aSize);
     unsigned char *dp=d->elements();
     const unsigned char *sp=aBinaryMatrix_.data();
     for (unsigned i=0;i<n;i++) *dp++=!*sp++;
   }
  return MSBinaryMatrix(d,aBinaryMatrix_.rows(),aBinaryMatrix_.columns());
}

// unary operator ~ - 1's complement
MSBinaryMatrix operator~(const MSBinaryMatrix& aBinaryMatrix_)
{
  MSTypeData<unsigned char,MSAllocator<unsigned char> > *d=0;
  unsigned n=aBinaryMatrix_.length();
  unsigned aSize=aBinaryMatrix_.size();
  if (n>0)
   {
     d=MSTypeData<unsigned char,MSAllocator<unsigned char> >::allocateWithSize(aSize);
     unsigned char *dp=d->elements();
     const unsigned char *sp=aBinaryMatrix_.data();
     for (unsigned i=0;i<n;i++) *dp++=!*sp++;
   }
  return MSBinaryMatrix(d,aBinaryMatrix_.rows(),aBinaryMatrix_.columns());
}

//------------------------------------------------------------------------------------------
// arithmetic & operator functions
//------------------------------------------------------------------------------------------
MSBinaryMatrix operator&(const MSBinaryMatrix& aBinaryMatrix_,unsigned char aBinaryValue_)
{
  MSTypeData<unsigned char,MSAllocator<unsigned char> > *d=0;
  unsigned n=aBinaryMatrix_.length();
  unsigned aSize=aBinaryMatrix_.size();
  if (n>0)
   {
     d=MSTypeData<unsigned char,MSAllocator<unsigned char> >::allocateWithSize(aSize);
     unsigned char *dp=d->elements();
     const unsigned char *ap=aBinaryMatrix_.data();
     for (unsigned i=0;i<n;i++) *dp++=*ap++ & aBinaryValue_;
   }
  return MSBinaryMatrix(d,aBinaryMatrix_.rows(),aBinaryMatrix_.columns());
}

MSBinaryMatrix operator&(unsigned char aBinaryValue_,const MSBinaryMatrix& aBinaryMatrix_)
{ return aBinaryMatrix_ & aBinaryValue_; }

MSBinaryMatrix operator&(const MSBinaryMatrix& aBinaryMatrix_,const MSBinaryMatrix& bBinaryMatrix_)
{
  assert(aBinaryMatrix_.rows()==bBinaryMatrix_.rows()&&aBinaryMatrix_.columns()==bBinaryMatrix_.columns());
  MSTypeData<unsigned char,MSAllocator<unsigned char> > *d=0;
  unsigned n=aBinaryMatrix_.length();
  unsigned aSize=aBinaryMatrix_.size();
  if (n>0)
   {
     d=MSTypeData<unsigned char,MSAllocator<unsigned char> >::allocateWithSize(aSize);
     unsigned char *dp=d->elements();
     const unsigned char *ap=aBinaryMatrix_.data();
     const unsigned char *bp=aBinaryMatrix_.data();
     for (unsigned i=0;i<n;i++) *dp++=*ap++ & *bp++;
   }
  return MSBinaryMatrix(d,aBinaryMatrix_.rows(),aBinaryMatrix_.columns());
}

//------------------------------------------------------------------------------------------
// arithmetic assignment & operator functions
//------------------------------------------------------------------------------------------
MSBinaryMatrix& MSBinaryMatrix::operator&=(const MSBinaryMatrix& aBinaryMatrix_)
{
  prepareToChange();
  unsigned n=length();
  assert(n==aBinaryMatrix_.length());
  if (n>0)
   {     
     unsigned char *dp=data();
     const unsigned char *bp=aBinaryMatrix_.data();
     while (n--) *dp++ &= *bp++;
     changed();
   }
  return *this;
}

MSBinaryMatrix& MSBinaryMatrix::operator&=(unsigned char aBinaryValue_)
{
  prepareToChange();
  unsigned n=length();
  if (n>0)
   {
     unsigned char c=aBinaryValue_==0?0:1;
     unsigned char *dp=data();
     while (n--) *dp++ &= c;
     changed();
   }
  return *this;
}

//------------------------------------------------------------------------------------------
// arithmetic | operator functions
//------------------------------------------------------------------------------------------
MSBinaryMatrix operator|(const MSBinaryMatrix& aBinaryMatrix_,unsigned char aBinaryValue_)
{
  MSTypeData<unsigned char,MSAllocator<unsigned char> > *d=0;
  unsigned n=aBinaryMatrix_.length();
  unsigned aSize=aBinaryMatrix_.size();
  if (n>0)
   {
     d=MSTypeData<unsigned char,MSAllocator<unsigned char> >::allocateWithSize(aSize);
     unsigned char *dp=d->elements();
     const unsigned char *ap=aBinaryMatrix_.data();
     for (unsigned i=0;i<n;i++) *dp++=*ap++ | aBinaryValue_;
   }
  return MSBinaryMatrix(d,aBinaryMatrix_.rows(),aBinaryMatrix_.columns());
}

MSBinaryMatrix operator|(unsigned char aBinaryValue_,const MSBinaryMatrix& aBinaryMatrix_)
{ return aBinaryMatrix_ | aBinaryValue_; }

MSBinaryMatrix operator|(const MSBinaryMatrix& aBinaryMatrix_,const MSBinaryMatrix& bBinaryMatrix_)
{
  assert(aBinaryMatrix_.rows()==bBinaryMatrix_.rows()&&aBinaryMatrix_.columns()==bBinaryMatrix_.columns());
  MSTypeData<unsigned char,MSAllocator<unsigned char> > *d=0;
  unsigned n=aBinaryMatrix_.length();
  unsigned aSize=aBinaryMatrix_.size();
  if (n>0)
   {
     d=MSTypeData<unsigned char,MSAllocator<unsigned char> >::allocateWithSize(aSize);
     unsigned char *dp=d->elements();
     const unsigned char *ap=aBinaryMatrix_.data();
     const unsigned char *bp=aBinaryMatrix_.data();
     for (unsigned i=0;i<n;i++) *dp++=*ap++ | *bp++;
   }
  return MSBinaryMatrix(d,aBinaryMatrix_.rows(),aBinaryMatrix_.columns());
}

//------------------------------------------------------------------------------------------
// arithmetic assignment | operator functions
//------------------------------------------------------------------------------------------
MSBinaryMatrix& MSBinaryMatrix::operator|=(const MSBinaryMatrix& aBinaryMatrix_)
{
  prepareToChange();
  unsigned n=length();
  assert(n==aBinaryMatrix_.length());
  if (n>0)
   {     
     unsigned char *dp=data();
     const unsigned char *bp=aBinaryMatrix_.data();
     while (n--) *dp++ |= *bp++;
     changed();
   }
  return *this;
}

MSBinaryMatrix& MSBinaryMatrix::operator|=(unsigned char aBinaryValue_)
{
  prepareToChange();
  unsigned n=length();
  if (n>0)
   {
     unsigned char c=aBinaryValue_==0?0:1;
     unsigned char *dp=data();
     while (n--) *dp++ |= c;
     changed();
   }
  return *this;
}

//------------------------------------------------------------------------------------------
// arithmetic ^ operator functions
//------------------------------------------------------------------------------------------
MSBinaryMatrix operator^(const MSBinaryMatrix& aBinaryMatrix_,unsigned char aBinaryValue_)
{
  MSTypeData<unsigned char,MSAllocator<unsigned char> > *d=0;
  unsigned n=aBinaryMatrix_.length();
  unsigned aSize=aBinaryMatrix_.size();
  if (n>0)
   {
     d=MSTypeData<unsigned char,MSAllocator<unsigned char> >::allocateWithSize(aSize);
     unsigned char *dp=d->elements();
     const unsigned char *ap=aBinaryMatrix_.data();
     for (unsigned i=0;i<n;i++) *dp++=*ap++ ^ aBinaryValue_;
   }
  return MSBinaryMatrix(d,aBinaryMatrix_.rows(),aBinaryMatrix_.columns());
}

MSBinaryMatrix operator^(unsigned char aBinaryValue_,const MSBinaryMatrix& aBinaryMatrix_)
{ return aBinaryMatrix_ ^ aBinaryValue_; }

MSBinaryMatrix operator^(const MSBinaryMatrix& aBinaryMatrix_,const MSBinaryMatrix& bBinaryMatrix_)
{
  assert(aBinaryMatrix_.rows()==bBinaryMatrix_.rows()&&aBinaryMatrix_.columns()==bBinaryMatrix_.columns());
  MSTypeData<unsigned char,MSAllocator<unsigned char> > *d=0;
  unsigned n=aBinaryMatrix_.length();
  unsigned aSize=aBinaryMatrix_.size();
  if (n>0)
   {
     d=MSTypeData<unsigned char,MSAllocator<unsigned char> >::allocateWithSize(aSize);
     unsigned char *dp=d->elements();
     const unsigned char *ap=aBinaryMatrix_.data();
     const unsigned char *bp=aBinaryMatrix_.data();
     for (unsigned i=0;i<n;i++) *dp++=*ap++ ^ *bp++;
   }
  return MSBinaryMatrix(d,aBinaryMatrix_.rows(),aBinaryMatrix_.columns());
}

//------------------------------------------------------------------------------------------
// arithmetic assignment ^ operator functions
//------------------------------------------------------------------------------------------
MSBinaryMatrix& MSBinaryMatrix::operator^=(const MSBinaryMatrix& aBinaryMatrix_)
{
  prepareToChange();
  unsigned n=length();
  assert(n==aBinaryMatrix_.length());
  if (n>0)
   {     
     unsigned char *dp=data();
     const unsigned char *bp=aBinaryMatrix_.data();
     while (n--) *dp++ ^= *bp++;
     changed();
   }
  return *this;
}

MSBinaryMatrix& MSBinaryMatrix::operator^=(unsigned char aBinaryValue_)
{
  prepareToChange();
  unsigned n=length();
  if (n>0)
   {
     unsigned char c=aBinaryValue_==0?0:1;
     unsigned char *dp=data();
     while (n--) *dp++ ^= c;
     changed();
   }
  return *this;
}

//-------------------------------------------------------------------------------------------------
// stack, adjoin
//-------------------------------------------------------------------------------------------------
MSBinaryMatrix stack(const MSBinaryMatrix& aBinaryMatrix_,const MSBinaryMatrix& bBinaryMatrix_)
{
  if (aBinaryMatrix_.columns()!=bBinaryMatrix_.columns()) 
   {
     aBinaryMatrix_.error("nonconformant MSBinaryMatrix stack operands.");
     return MSBinaryMatrix();
   }

  MSTypeData<unsigned char,MSAllocator<unsigned char> > *d=0;
  unsigned newLength=(aBinaryMatrix_.rows()+bBinaryMatrix_.rows())*aBinaryMatrix_.columns();
  if (newLength>0)
   { 
     d=MSTypeData<unsigned char,MSAllocator<unsigned char> >::allocateWithLength(newLength);
     unsigned char *dp=d->elements();
     unsigned char *mp=aBinaryMatrix_.data();
     unsigned char *row=aBinaryMatrix_.data()+aBinaryMatrix_.columns();
     if (mp>0)
      {
	do
	 {
	   while (mp<row) *dp++=*mp++;
	 }
	while ((row+=aBinaryMatrix_.columns())<=aBinaryMatrix_.data()+aBinaryMatrix_.length());
      }
     mp=bBinaryMatrix_.data();
     row=bBinaryMatrix_.data()+bBinaryMatrix_.columns();
     if (mp>0)
      {
	do
	 {
	   while (mp<row) *dp++=*mp++;
	 }
	while ((row+=bBinaryMatrix_.columns())<=bBinaryMatrix_.data()+bBinaryMatrix_.length());
      }
   }
  return MSBinaryMatrix(d,aBinaryMatrix_.rows()+bBinaryMatrix_.rows(),aBinaryMatrix_.columns());
}

MSBinaryMatrix& MSBinaryMatrix::stack(const MSBinaryMatrix& aBinaryMatrix_)
{
  if (aBinaryMatrix_.columns()!=columns()) 
   {
     error("nonconformant MSBinaryMatrix stack operands.");
     return *this;
   }
  unsigned newLength=(rows()+aBinaryMatrix_.rows())*columns();
  MSTypeData<unsigned char,MSAllocator<unsigned char> > *d=0;
  if (newLength>0)
   {
     d=MSTypeData<unsigned char,MSAllocator<unsigned char> >::allocateWithLength(newLength);     
     unsigned char *dp=d->elements();
     unsigned char *mp=data();
     unsigned char *row=data()+columns();
     if (mp>0)
      {
	do
	 {
	   while (mp<row) *dp++=*mp++;
	 }
	while ((row+=columns())<=data()+length());
      }
     mp=aBinaryMatrix_.data();
     row=aBinaryMatrix_.data()+aBinaryMatrix_.columns();
     if (mp>0)
      {
        do
         {
	   while (mp<row) *dp++=*mp++;
         }
        while ((row+=aBinaryMatrix_.columns())<=aBinaryMatrix_.data()+aBinaryMatrix_.length());
      }
   }
  
  unsigned oldLength=length();
  freeData();
  _pData=d,_rows+=aBinaryMatrix_.rows(),_count=newLength;

  if (receiverList()!=0&&aBinaryMatrix_.length()>0)
   {
     MSIndexVector iv;
     iv.series(aBinaryMatrix_.length(),oldLength);
     changed(iv);
   }
  return *this;
}

MSBinaryMatrix adjoin(const MSBinaryMatrix& aBinaryMatrix_,const MSBinaryMatrix& bBinaryMatrix_)
{
  if (aBinaryMatrix_.rows()!=bBinaryMatrix_.rows())
   {
     aBinaryMatrix_.error("nonconformant MSBinaryMatrix adjoin operands.");
     return MSBinaryMatrix();
   }
  MSTypeData<unsigned char,MSAllocator<unsigned char> > *d=0;
  unsigned newLength=aBinaryMatrix_.rows()*(aBinaryMatrix_.columns()+bBinaryMatrix_.columns());
  if (newLength>0)
   {
     d=MSTypeData<unsigned char,MSAllocator<unsigned char> >::allocateWithLength(newLength);     
     unsigned char *dp=d->elements();
     unsigned char *mp=aBinaryMatrix_.data();
     unsigned char *row=aBinaryMatrix_.data()+aBinaryMatrix_.columns();
     if (mp>0)
      {
	do 
	 {
	   while (mp<row) *dp++=*mp++;
	   dp+=bBinaryMatrix_.columns();
	 }
	while ((row+=aBinaryMatrix_.columns())<=aBinaryMatrix_.data()+aBinaryMatrix_.length());
      }
     dp=d->elements()+aBinaryMatrix_.columns();
     mp=bBinaryMatrix_.data();
     row=bBinaryMatrix_.data()+bBinaryMatrix_.columns();
     if (mp>0)
      {
	do
	 {
	   while (mp<row) *dp++=*mp++;
	   dp+=aBinaryMatrix_.columns();
	 }
	while ((row+=bBinaryMatrix_.columns())<=bBinaryMatrix_.data()+bBinaryMatrix_.length());
      }
   }
  return MSBinaryMatrix(d,aBinaryMatrix_.rows(),aBinaryMatrix_.columns()+bBinaryMatrix_.columns());
}

MSBinaryMatrix& MSBinaryMatrix::adjoin(const MSBinaryMatrix& aBinaryMatrix_)
{
  if (rows()!=aBinaryMatrix_.rows())
   {
     aBinaryMatrix_.error("nonconformant MSBinaryMatrix adjoin operands.");
     return *this;
   }
  MSTypeData<unsigned char,MSAllocator<unsigned char> > *d=0;
  unsigned newLength=rows()*(columns()+aBinaryMatrix_.columns());
  if (newLength>0)
   {
     d=MSTypeData<unsigned char,MSAllocator<unsigned char> >::allocateWithLength(newLength);     
     unsigned char *dp=d->elements();
     unsigned char *mp=data();
     unsigned char *row=data()+columns();
     if (mp>0)
      {
	do 
	 {
	   while (mp<row) *dp++=*mp++;
	   dp+=aBinaryMatrix_.columns();
	 }
	while ((row+=aBinaryMatrix_.columns())<=aBinaryMatrix_.data()+aBinaryMatrix_.length());
      }
     dp=d->elements()+columns();
     mp=aBinaryMatrix_.data();
     row=aBinaryMatrix_.data()+aBinaryMatrix_.columns();
     if (mp>0)
      {
	do
	 {
	   while (mp<row) *dp++=*mp++;
	   dp+=columns();
	 }
	while ((row+=aBinaryMatrix_.columns())<=aBinaryMatrix_.data()+aBinaryMatrix_.length());
      }
   }

  freeData();
  _pData=d,_columns+=aBinaryMatrix_.columns(),_count=newLength;
  if (receiverList()!=0&&aBinaryMatrix_.length()>0) changed();
  return *this;
}

MSBinaryMatrix& MSBinaryMatrix::random(void)
{
  int n=length();
  if (n>0)
   {
     prepareToChange();
     unsigned char *dp=data();
     MSRandom rand;
     while (n--) *dp++=(unsigned char)rand(2);  // limit is always 2; nothing else makes sense
     changed();
   }
  return *this;
}

unsigned long MSBinaryMatrix::sum(void) const 
{
  unsigned long sum=0;
  unsigned char *dp=data();
  unsigned n=length();
  while (n--) sum+=*dp++;
  return sum;
}

void MSBinaryMatrix::error(const char* msg_) const
{ (*_matrixErrorHandler)(msg_); }

//------------------------------------------------------------------------------------------
// matrix manipulation methods
//-------------------------------------------------------------------------------------------------
MSBinaryMatrix& MSBinaryMatrix::appendColumn(const MSBinaryVector& aBinaryVector_)
{
  if (rows()==0||aBinaryVector_.length()!=rows())
   {
     error("MSBinaryMatrix length error.");
     return *this;
   }
  unsigned newLength=rows()*(columns()+1);
  MSTypeData<unsigned char,MSAllocator<unsigned char> > *d=MSTypeData<unsigned char,MSAllocator<unsigned char> >::allocateWithLength(newLength);  
  unsigned char *dp=d->elements();
  unsigned char *mp=data();
  unsigned char *vp=aBinaryVector_.data();
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

MSBinaryMatrix& MSBinaryMatrix::appendColumns(unsigned cols_,unsigned char fill_)
{
  if (rows()==0)
   {
     error("MSBinaryMatrix length error.");
     return *this;
   }
  unsigned newLength=rows()*(columns()+cols_);
  MSTypeData<unsigned char,MSAllocator<unsigned char> > *d=MSTypeData<unsigned char,MSAllocator<unsigned char> >::allocateWithLength(newLength);  
  unsigned char *dp=d->elements();
  unsigned char *mp=data();
  unsigned i,j;
  unsigned char fill=fill_>0?1:0;
  for (j=0;j<rows();j++)
   {
     for (i=0;i<columns();i++) *dp++=*mp++;
     for (i=0;i<cols_;i++) *dp++=fill;
   }
  freeData();
  _pData=d;
  _columns+=cols_;
  _count=newLength;
  changed();  
  return *this;
}

MSBinaryMatrix& MSBinaryMatrix::appendRow(const MSBinaryVector& aBinaryVector_)
{
  if (columns()==0||aBinaryVector_.length()!=columns())
   {
     error("MSBinaryMatrix length error.");
     return *this;
   }
  unsigned newLength=(rows()+1)*columns();
  MSTypeData<unsigned char,MSAllocator<unsigned char> > *d=MSTypeData<unsigned char,MSAllocator<unsigned char> >::allocateWithLength(newLength);  
  unsigned char *dp=d->elements();
  unsigned char *mp=data();
  unsigned char *vp=aBinaryVector_.data();
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

MSBinaryMatrix& MSBinaryMatrix::appendRows(unsigned rows_, unsigned char fill_)
{
  if (columns()==0)
   {
     error("MSBinaryMatrix length error.");
     return *this;
   }
  unsigned newLength=(rows()+rows_)*columns();
  MSTypeData<unsigned char,MSAllocator<unsigned char> > *d=MSTypeData<unsigned char,MSAllocator<unsigned char> >::allocateWithLength(newLength);  
  unsigned char *dp=d->elements();
  unsigned char *mp=data();
  unsigned i,appendLength=columns()*rows_;
  unsigned char fill=fill_>0?1:0;
  for (i=0;i<length();i++) *dp++=*mp++;
  for (i=0;i<appendLength;i++) *dp++=fill;
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

MSBinaryMatrix& MSBinaryMatrix::exchangeColumns(unsigned aColumn_,unsigned bColumn_)
{
  if (aColumn_+1>columns()||bColumn_+1>columns()||aColumn_==bColumn_) return *this;

  prepareToChange();

  unsigned char *aPtr=data()+aColumn_;
  unsigned char *bPtr=data()+bColumn_;
  register unsigned char tVal;

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

MSBinaryMatrix& MSBinaryMatrix::exchangeRows(unsigned aRow_,unsigned bRow_)
{
  if (aRow_+1>rows()||bRow_+1>rows()||aRow_==bRow_) return *this;

  prepareToChange();

  unsigned char *aPtr=data()+aRow_*columns();
  unsigned char *bPtr=data()+bRow_*columns();
  register unsigned char tVal;

  for (unsigned i=0;i<columns();i++)
   {
     tVal=*aPtr;
     *aPtr++=*bPtr;
     *bPtr++=tVal;
   }
  changed();  
  return *this;
}

MSBinaryMatrix& MSBinaryMatrix::insertColumnBefore(unsigned col_,unsigned char fill_)
{
  if (col_+1>columns()) return *this;
  
  unsigned newLength=rows()*(columns()+1);
  MSTypeData<unsigned char,MSAllocator<unsigned char> > *d=MSTypeData<unsigned char,MSAllocator<unsigned char> >::allocateWithLength(newLength);
  unsigned char *dp=d->elements();
  unsigned char *mp=data();
  unsigned i,j;
  unsigned char fill=fill_>0?1:0;
  
  for (j=0;j<rows();j++)
   {
     for (i=0;i<columns()+1;i++) *dp++=(i==col_)?fill:*mp++;
   }
  freeData();
  _pData=d;
  _columns++;
  _count=newLength;
  changed();  
  return *this;
}

MSBinaryMatrix& MSBinaryMatrix::insertColumnBefore(unsigned col_,const MSBinaryVector& aBinaryVector_)
{
  if (col_+1>columns()) return *this;
  if (aBinaryVector_.length()!=rows())
   {
     error("MSBinaryMatrix length error.");
     return *this;
   }
  
  unsigned newLength=rows()*(columns()+1);
  MSTypeData<unsigned char,MSAllocator<unsigned char> > *d=MSTypeData<unsigned char,MSAllocator<unsigned char> >::allocateWithLength(newLength);
  unsigned char *dp=d->elements();
  unsigned char *mp=data();
  unsigned char *vp=aBinaryVector_.data();
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

MSBinaryMatrix& MSBinaryMatrix::insertColumnAfter(unsigned col_,unsigned char fill_)
{
  if (col_+1>columns()) return *this;

  unsigned newLength=rows()*(columns()+1);
  MSTypeData<unsigned char,MSAllocator<unsigned char> > *d=MSTypeData<unsigned char,MSAllocator<unsigned char> >::allocateWithLength(newLength);
  unsigned char *dp=d->elements();
  unsigned char *mp=data();
  unsigned i,j;
  unsigned char fill=fill_>0?1:0;
  
  for (j=0;j<rows();j++)
   {
     for (i=0;i<columns()+1;i++) *dp++=(i==col_+1)?fill:*mp++;
   }
  freeData();
  _pData=d;
  _columns++;
  _count=newLength;
  changed();  
  return *this;
}

MSBinaryMatrix& MSBinaryMatrix::insertColumnAfter(unsigned col_,const MSBinaryVector& aBinaryVector_)
{
  if (col_+1>columns()) return *this;
  if (aBinaryVector_.length()!=rows())
   {
     error("MSBinaryMatrix length error.");
     return *this;
   }

  unsigned newLength=rows()*(columns()+1);
  MSTypeData<unsigned char,MSAllocator<unsigned char> > *d=MSTypeData<unsigned char,MSAllocator<unsigned char> >::allocateWithLength(newLength);  
  unsigned char *dp=d->elements();
  unsigned char *mp=data();
  unsigned char *vp=aBinaryVector_.data();
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

MSBinaryMatrix& MSBinaryMatrix::insertRowBefore(unsigned row_,unsigned char fill_)
{
  if (row_+1>rows()) return *this;

  unsigned newLength=(rows()+1)*columns();
  MSTypeData<unsigned char,MSAllocator<unsigned char> > *d=MSTypeData<unsigned char,MSAllocator<unsigned char> >::allocateWithLength(newLength);  
  unsigned char *dp=d->elements();
  unsigned char *mp=data();
  unsigned i,j;
  unsigned char fill=fill_>0?1:0;
  
  for (j=0;j<rows()+1;j++)
   {
     for (i=0;i<columns();i++) *dp++=(j==row_)?fill:*mp++;
   }
  freeData();
  _pData=d;
  _rows++;
  _count=newLength;
  changed();  
  return *this;
}

MSBinaryMatrix& MSBinaryMatrix::insertRowBefore(unsigned row_,const MSBinaryVector& aBinaryVector_)
{
  if (row_+1>rows()) return *this;
  if (aBinaryVector_.length()!=columns())
   {
     error("MSBinaryMatrix length error.");
     return *this;
   }

  unsigned newLength=(rows()+1)*columns();
  MSTypeData<unsigned char,MSAllocator<unsigned char> > *d=MSTypeData<unsigned char,MSAllocator<unsigned char> >::allocateWithLength(newLength);  
  unsigned char *dp=d->elements();
  unsigned char *mp=data();
  unsigned char *vp=aBinaryVector_.data();
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

MSBinaryMatrix& MSBinaryMatrix::insertRowAfter(unsigned row_,unsigned char fill_)
{
  if (row_+1>rows()) return *this;

  unsigned newLength=(rows()+1)*columns();
  MSTypeData<unsigned char,MSAllocator<unsigned char> > *d=MSTypeData<unsigned char,MSAllocator<unsigned char> >::allocateWithLength(newLength);  
  unsigned char *dp=d->elements();
  unsigned char *mp=data();
  unsigned i,j;
  unsigned char fill=fill_>0?1:0;
  
  for (j=0;j<rows()+1;j++)
   {
     for (i=0;i<columns();i++) *dp++=(j==row_+1)?fill:*mp++;
   }
  freeData();
  _pData=d;
  _rows++;
  _count=newLength;
  changed();  
  return *this;
}

MSBinaryMatrix& MSBinaryMatrix::insertRowAfter(unsigned row_,const MSBinaryVector& aBinaryVector_)
{
  if (row_>rows()) return *this;
  if (aBinaryVector_.length()!=columns())
   {
     error("MSBinaryMatrix length error.");
     return *this;
   }

  unsigned newLength=(rows()+1)*columns();
  MSTypeData<unsigned char,MSAllocator<unsigned char> > *d=MSTypeData<unsigned char,MSAllocator<unsigned char> >::allocateWithLength(newLength);  
  unsigned char *dp=d->elements();
  unsigned char *mp=data();
  unsigned char *vp=aBinaryVector_.data();
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

MSBinaryMatrix& MSBinaryMatrix::reshape(unsigned rows_,unsigned cols_)
{
  MSTypeData<unsigned char,MSAllocator<unsigned char> > *d=0;
  unsigned newLength=rows_*cols_;
  if (newLength>0)
   {
     d=MSTypeData<unsigned char,MSAllocator<unsigned char> >::allocateWithLength(newLength);     
     unsigned char *dp=d->elements();
     unsigned char *mp=data();
     unsigned char *end=data()+length();
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

MSBinaryMatrix& MSBinaryMatrix::removeAll(void)
{
  freeData();
  _rows=0,_columns=0,_count=0;
  changed();  
  return *this;
}

MSBinaryMatrix& MSBinaryMatrix::removeAllRows(void)
{
  freeData();
  _rows=0,_count=0;
  changed();  
  return *this;
}

MSBinaryMatrix& MSBinaryMatrix::removeAllColumns(void)
{
  freeData();
  _columns=0,_count=0;
  changed();  
  return *this;
}

MSBinaryMatrix& MSBinaryMatrix::assignColumn(unsigned col_,unsigned char scalar_)
{
  if (col_+1>columns()) return *this;
  prepareToChange();
  unsigned char *mp=data(); 
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

MSBinaryMatrix& MSBinaryMatrix::assignRow(unsigned row_,unsigned char scalar_)
{
  if (row_+1>rows()) return *this;
  prepareToChange();
  unsigned char *mp=data(); 
  unsigned i,j;
  for (i=row_*columns(),j=0;j<columns();i++,j++) mp[i]=scalar_;
  if (receiverList()!=0)
   {
     MSIndexVector iv;
     changed(iv.series(columns(),row_*columns()));
   }
  return *this;
}

MSBinaryMatrix& MSBinaryMatrix::assignColumn(unsigned col_, const MSBinaryVector& aBinaryVector_)
{
  if (col_+1>columns()) return *this;
  if (aBinaryVector_.length()!=rows())
   {
     error("MSBinaryMatrix length error.");
     return *this;
   }
  prepareToChange();
  unsigned char *mp;
  unsigned i;
  for (i=0,mp=data()+col_;i<rows();i++,mp+=columns()) *mp=aBinaryVector_(i);
  changed();
  return *this;
}

MSBinaryMatrix& MSBinaryMatrix::assignRow(unsigned row_, const MSBinaryVector& aBinaryVector_)
{
  if (row_+1>rows()) return *this;
  if (aBinaryVector_.length()!=columns())
   {
     error("MSBinaryMatrix length error.");
     return *this;
   }
  prepareToChange();
  unsigned char *mp;
  unsigned i;
  for (i=0,mp=data()+row_*columns();i<columns();i++,mp++) *mp=aBinaryVector_(i);
  if (receiverList()!=0)
   {
     MSIndexVector iv;
     changed(iv.series(columns(),row_*columns()));
   }
  return *this;
}

MSBinaryMatrix& MSBinaryMatrix::removeRow(unsigned row_)
{
  if (row_+1>rows()) return *this;
  if (data()==0) return *this;
  
  unsigned newLength=(rows()-1)*columns();
  MSTypeData<unsigned char,MSAllocator<unsigned char> > *d=MSTypeData<unsigned char,MSAllocator<unsigned char> >::allocateWithLength(newLength);
  unsigned char *dp=d->elements();
  unsigned char *mp=data();
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

MSBinaryMatrix& MSBinaryMatrix::removeColumn(unsigned col_)
{
  if (col_+1>columns()) return *this;
  if (data()==0) return *this;

  unsigned newLength=rows()*(columns()-1);
  MSTypeData<unsigned char,MSAllocator<unsigned char> > *d=MSTypeData<unsigned char,MSAllocator<unsigned char> >::allocateWithLength(newLength);  
  unsigned char *dp=d->elements();
  unsigned char *mp=data();
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

MSBinaryMatrix& MSBinaryMatrix::compressRows(const MSBinaryVector& aBinaryVector_ )
{
  if (data()==0) return *this;
  if (aBinaryVector_.length()!=rows())
   {
     error("MSBinaryMatrix length error.");
     return *this;
   }
  
  unsigned newLength=(unsigned)(aBinaryVector_.sum()*columns());
  MSTypeData<unsigned char,MSAllocator<unsigned char> > *d=MSTypeData<unsigned char,MSAllocator<unsigned char> >::allocateWithLength(newLength);  
  unsigned char *dp=d->elements();
  unsigned char *mp=data();
  unsigned i,j;
  for (i=0;i<rows();i++)
   {
     if (aBinaryVector_(i)==0) mp+=columns();
     else for (j=0;j<columns();j++) *dp++=*mp++;
   }
  freeData();
  _pData=d;
  _rows=(unsigned)aBinaryVector_.sum();
  _count=newLength;
  changed();  
  return *this;
}

MSBinaryMatrix& MSBinaryMatrix::compressColumns(const MSBinaryVector& aBinaryVector_ )
{
  if (data()==0) return *this;
  if (aBinaryVector_.length()!=columns())
   {
     error("MSBinaryMatrix length error.");
     return *this;
   }
  
  unsigned newLength=(unsigned)(rows()*aBinaryVector_.sum());
  MSTypeData<unsigned char,MSAllocator<unsigned char> > *d=MSTypeData<unsigned char,MSAllocator<unsigned char> >::allocateWithLength(newLength);  
  unsigned char *dp=d->elements();
  unsigned char *mp=data();
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
  _columns=(unsigned)aBinaryVector_.sum();
  _count=newLength;
  changed();  
  return *this;
}

MSBinaryMatrix& MSBinaryMatrix::reverseRows(void)
{
  if (data()==0) return *this;
  prepareToChange();
  unsigned i,j;
  unsigned char *upperRow=data();
  unsigned char *lowerRow=data()+rows()*columns()-columns();
  register unsigned char tVal;
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

MSBinaryMatrix& MSBinaryMatrix::reverseColumns(void)
{
  if (data()==0) return *this;
  prepareToChange();
  unsigned i,j;
  unsigned char *leftColumn=data();
  unsigned char *rightColumn=data()+columns()-1;
  register unsigned char tVal;
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

MSBinaryMatrix& MSBinaryMatrix::transpose(void)
{
  if (data()==0) return *this;
  unsigned aSize=size();
  MSTypeData<unsigned char,MSAllocator<unsigned char> > *d=MSTypeData<unsigned char,MSAllocator<unsigned char> >::allocateWithSize(aSize);
  unsigned char *dp=d->elements();
  unsigned char *mp=data();
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

MSBinaryMatrix& MSBinaryMatrix::rotateRows(int position_)
{
  int rowPosition=MSUtil::abs(position_);
  if (rowPosition>0&&rowPosition!=rows())
   {
     unsigned i;
     unsigned aSize=size();
     MSTypeData<unsigned char,MSAllocator<unsigned char> > *d=MSTypeData<unsigned char,MSAllocator<unsigned char> >::allocateWithSize(aSize);
     if (rowPosition>rows()) rowPosition%=rows();
     if (position_<0) rowPosition=rows()-rowPosition;
     unsigned start=rowPosition*columns();
     unsigned char *mp=data()+start;
     unsigned char *dp=d->elements();
     for (i=start;i<length();i++) *dp++=*mp++;
     mp=data();
     for (i=0;i<start;i++) *dp++=*mp++;
     freeData();
     _pData=d;
     changed();     
   }
  return *this;
}

MSBinaryMatrix& MSBinaryMatrix::rotateColumns(int position_)
{
  int columnPosition=MSUtil::abs(position_);
  if (columnPosition>0&&columnPosition!=columns())
   {
     unsigned i,j;
     unsigned aSize=size();
     MSTypeData<unsigned char,MSAllocator<unsigned char> > *d=MSTypeData<unsigned char,MSAllocator<unsigned char> >::allocateWithSize(aSize);
     if (columnPosition>columns()) columnPosition%=columns();
     if (position_<0) columnPosition=columns()-columnPosition;
     unsigned char *mp=data()+columnPosition;
     unsigned char *dp=d->elements();
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

MSBinaryMatrix& MSBinaryMatrix::takeRows(int numberOfRows_)
{
  int numberOfRows=MSUtil::abs(numberOfRows_);
  if (numberOfRows>0&&numberOfRows!=rows())
   {
     unsigned i;
     unsigned newLength=numberOfRows*columns();
     MSTypeData<unsigned char,MSAllocator<unsigned char> > *d=MSTypeData<unsigned char,MSAllocator<unsigned char> >::allocateWithLength(newLength);     
     unsigned char *mp=data();
     unsigned char *dp=d->elements();
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

MSBinaryMatrix& MSBinaryMatrix::takeColumns(int numberOfColumns_)
{
  int numberOfColumns=MSUtil::abs(numberOfColumns_);
  if (numberOfColumns>0&&numberOfColumns!=columns())
   {
     unsigned i,j;
     unsigned newLength=numberOfColumns*rows();
     MSTypeData<unsigned char,MSAllocator<unsigned char> > *d=MSTypeData<unsigned char,MSAllocator<unsigned char> >::allocateWithLength(newLength);     
     unsigned char *mp=data();
     unsigned char *dp=d->elements();
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

MSBinaryMatrix& MSBinaryMatrix::dropRows(int numberOfRows_)
{
  int numberOfRows=MSUtil::abs(numberOfRows_);
  if (numberOfRows>0)
   {
     if (numberOfRows<rows())
      {
	unsigned i;
	unsigned nRows=rows()-numberOfRows;
	unsigned newLength=nRows*columns();
	MSTypeData<unsigned char,MSAllocator<unsigned char> > *d=MSTypeData<unsigned char,MSAllocator<unsigned char> >::allocateWithLength(newLength);	
	unsigned char *mp=data();
	unsigned char *dp=d->elements();
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

MSBinaryMatrix& MSBinaryMatrix::dropColumns(int numberOfColumns_)
{
  int numberOfColumns=MSUtil::abs(numberOfColumns_);
  if (numberOfColumns>0)
   {
     if (numberOfColumns<columns())
      {
	unsigned i,j;
	unsigned cols=columns()-numberOfColumns;
	unsigned newLength=cols*rows();
	MSTypeData<unsigned char,MSAllocator<unsigned char> > *d =
	  MSTypeData<unsigned char,MSAllocator<unsigned char> >::allocateWithLength(newLength);	
	unsigned char *mp=data();
	unsigned char *dp=d->elements();
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

MSBinaryVector MSBinaryMatrix::rowAt(unsigned row_) const
{
  if (row_+1>rows()||columns()==0) return MSBinaryVector();
  MSBinaryVector::Data *d = MSBinaryVector::Data::allocateWithLength (columns());
  unsigned char *mp;
  unsigned char *dp=d->elements();
  unsigned i;
  for (i=0,mp=data()+row_*columns();i<columns();i++,mp++) *dp++=*mp;
  return MSBinaryVector(d,columns());
}

MSBinaryVector MSBinaryMatrix::columnAt(unsigned column_) const
{
  if (column_+1>columns()||rows()==0) return MSBinaryVector();
  MSBinaryVector::Data *d = MSBinaryVector::Data::allocateWithLength (rows());
  unsigned char *mp;
  unsigned char *dp=d->elements();
  unsigned i;
  for (i=0,mp=data()+column_;i<rows();i++,mp+=columns()) *dp++=*mp;
  return MSBinaryVector(d,rows());
}
