///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////
#define MS_MSFloatMatrix_INSTANTIATE

#include <math.h>
#include <float.h>
#if HAVE_SSTREAM
#include <sstream>
#else
#include <strstream.h>
#endif
#include <MSTypes/MSRandom.H>
#include <MSTypes/MSUtil.H>
#include <MSTypes/MSFloatMatrix.H>
#include <MSTypes/MSFloatVector.H>
#include <MSTypes/MSBinaryVector.H>
#include <MSTypes/MSIndexVector.H>
#include <MSTypes/MSTypes_MSF.H>

double MSTypeMatrix<double>::_badData(0.0);

const MSSymbol& MSTypeMatrix<double>::type(void) const 
{ return symbol(); }

MSString MSTypeMatrix<double>::asString(void) const
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

MSString MSTypeMatrix<double>::asMSF(void) const
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

MSString MSTypeMatrix<double>::asDebugInfo(void) const
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

MSString MSTypeMatrix<double>::className(void) const
{ return MSString("MSTypeMatrix<double>"); }

MSModel *MSTypeMatrix<double>::clone(void) const
{ return new MSTypeMatrix<double>(*this); }

MSModel *MSTypeMatrix<double>::create(void) const
{ return new MSTypeMatrix<double>(); }

void MSTypeMatrix<double>::assign(const MSModel& aModel_)
{ *this=(MSTypeMatrix<double>&)aModel_; }

long MSTypeMatrix<double>::compare(const MSModel& aModel_) const
{ return compare((MSTypeMatrix<double>&)aModel_); }

const MSSymbol& MSTypeMatrix<double>::symbol(void)   
{
  static MSSymbol sym ("MSTypeMatrix<double>");
  return sym;
}

void MSTypeMatrix<double>::blockLeft(unsigned target_,unsigned moveCount_)
{ 
  double *dp=data();
  for (unsigned i=target_;moveCount_>0;i++,moveCount_--) dp[i]=dp[i+1]; 
}

void MSTypeMatrix<double>::blockRight(unsigned target_,unsigned moveCount_)
{ 
  double *dp=data();
  for (unsigned i=target_+moveCount_-1;moveCount_>0;i--,moveCount_--) dp[i]=dp[i-1]; 
}

void MSTypeMatrix<double>::allocData(unsigned length_)
{
  _count=length_;
  if (length()>0)
   {
     _pData = MSTypeData<double,MSAllocator<double> >::allocateWithLength (length());
   }
  else
   {
     _pData=0;
   }
}

void MSTypeMatrix<double>::makeUniqueCopy(void)
{
  if (_pData!=0)
   {
     MSTypeData<double,MSAllocator<double> > *dst=MSTypeData<double,MSAllocator<double> >::allocateWithSize(size());
     MSTypeData<double,MSAllocator<double> >::copy(_pData->elements(),dst->elements(),length()); //src,dst,len
     _pData->decrementCount();
     _pData=dst;
   }
}

void MSTypeMatrix<double>::prepareToChange(void)
{
  if (_pData!=0)
   {
     if (_pData->refCount()>1) makeUniqueCopy();
   }
  else allocData(length());
}

void MSTypeMatrix<double>::freeData(void)
{ decrementCount(),_pData=0; }

// make sure there are at least size_+1 elements in the array
void MSTypeMatrix<double>::reserve(unsigned size_)
{
  unsigned n=size_+1;
  if (size()<n)
   {
     MSTypeData<double,MSAllocator<double> > *newData=MSTypeData<double,MSAllocator<double> >::allocateWithLength(n);
     double *dp=newData->elements();
     double *sp=data();
     MSTypeData<double,MSAllocator<double> >::copy(sp,dp,size());
     freeData();
     _pData=newData;
   }
}

//-------------------------------------------------------------------------------------------------
// MSTypeMatrix<double>
//-------------------------------------------------------------------------------------------------
ostream& operator<<(ostream& aStream_,const MSTypeMatrix<double>& aTypeMatrix_)
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

MSTypeMatrix<double>::MSTypeMatrix(void) 
:MSMatrix()
{ _pData=0;_blocked=MSFalse; }

MSTypeMatrix<double>::MSTypeMatrix(unsigned rows_,unsigned cols_)
:MSMatrix(rows_,cols_)
{
  allocData(length());
  _blocked=MSFalse;
}

MSTypeMatrix<double>::MSTypeMatrix(unsigned rows_,unsigned cols_,double fill_) 
:MSMatrix(rows_,cols_)
{
  if (length()>0)
   {
     unsigned n=length();
     allocData(n);
     double *dp=data();
     while (n--) *dp++=fill_;     
   }
  else _pData=0;
  _blocked=MSFalse;
}

MSTypeMatrix<double>::MSTypeMatrix(const MSTypeMatrix<double>& aTypeMatrix_) 
:MSMatrix(aTypeMatrix_.rows(),aTypeMatrix_.columns())
{
  _pData=aTypeMatrix_.pData();
  _blocked=MSFalse;
  (void)incrementCount();
}

// special private constructor used to avoid temporary object creation
// on return by value - see ARM page 267
MSTypeMatrix<double>::MSTypeMatrix(MSTypeData<double,MSAllocator<double> > *data_,unsigned rows_,unsigned cols_) 
:MSMatrix(rows_,cols_)
{
  _pData=data_;
  _blocked=MSFalse;
}


MSTypeMatrix<double>::MSTypeMatrix(const double *pElements_, unsigned rows_, unsigned cols_)
: MSMatrix(rows_,cols_)
{
  _pData = MSTypeData<double,MSAllocator<double> >::allocateWithLength(_count, MSRaw);
  MSTypeData<double,MSAllocator<double> >::copy(pElements_, _pData->elements(), _count, MSRaw);
}


MSTypeMatrix<double>::~MSTypeMatrix(void)
{ freeData(); }

MSError::ErrorStatus MSTypeMatrix<double>::set(unsigned index_,double aValue_)
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

MSError::ErrorStatus MSTypeMatrix<double>::set(unsigned index_,const char *pString_) 
{
  char *cp=0;
  double dnum=strtod(pString_,&cp); // Base 10
  if (cp==pString_) return MSError::MSFailure;
  else return set(index_,dnum);
}

MSError::ErrorStatus MSTypeMatrix<double>::set(const char *pString_)
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
     unsigned n=length();
     if (n>0)
       _pData = MSTypeData<double,MSAllocator<double> >::allocateWithLength (n);
     
     double *dp=data();
     for (unsigned i=0;i<n;i++) ist>>dp[i];

     if (!ist) return MSError::MSFailure;
   }
  changed();
  return MSError::MSSuccess;
}

MSError::ErrorStatus MSTypeMatrix<double>::setFromMSF(const char *pString_)
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
	_pData=MSTypeData<double,MSAllocator<double> >::allocateWithLength(length());
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
		 else set(i,double(0)); // make sure fills in on not MxN case 
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

unsigned MSTypeMatrix<double>::indexOf(double aValue_,unsigned startPos_) const
{ 
  unsigned n=length();
  for (unsigned i=startPos_;i<n;i++) if (elementAt(i)==aValue_) return i;
  return n;
}

unsigned MSTypeMatrix<double>::lastIndexOf(double aValue_,unsigned startPos_) const
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
MSTypeMatrix<double>& MSTypeMatrix<double>::operator=(const MSTypeMatrix<double>& aTypeMatrix_)
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
MSTypeMatrix<double>& MSTypeMatrix<double>::operator=(const MSMatrixSTypePick<double>& aPick_)
{
  prepareToChange();
  unsigned n=length();
  double s=aPick_.value();
  double *dp=data();
  while (n--) *dp++=s;
  changed();
  return *this;
}

// Assign a double to all elements of this MSTypeMatrix.
// aTypeMatrix=aScalar
MSTypeMatrix<double>& MSTypeMatrix<double>::operator=(double scalar_)
{
  prepareToChange();
  unsigned n=length();
  double s=scalar_;
  double *dp=data();
  while (n--) *dp++=s;
  changed();
  return *this;
}

MSTypeMatrix<double>& MSTypeMatrix<double>::series(double offset_) 
{
  prepareToChange();
  double j=offset_;
  unsigned n=length();
  double *dp=data();
  while (n--) *dp++=j++;
  changed();
  return *this;
}


//-------------------------------------------------------------------------------------------------
// relational operator functions
//-------------------------------------------------------------------------------------------------
long MSTypeMatrix<double>::compare(const MSTypeMatrix<double>& aTypeMatrix_) const
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

MSBinaryMatrix MSTypeMatrix<double>::binaryCompare(const MSTypeMatrix<double>& aTypeMatrix_,MSComparison aComparison_) const
{
  assert(rows()==aTypeMatrix_.rows()&&columns()==aTypeMatrix_.columns());
  unsigned aSize=size();
  unsigned n=length();
  MSTypeData<unsigned char,MSAllocator<unsigned char> > *d=MSTypeData<unsigned char,MSAllocator<unsigned char> >::allocateWithSize(aSize);
  const double *ap=data();
  const double *bp=aTypeMatrix_.data();
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

MSBinaryMatrix MSTypeMatrix<double>::binaryCompare(double aScalar_,MSComparison aComparison_) const
{
  unsigned aSize=size();
  unsigned n=length();
  MSTypeData<unsigned char,MSAllocator<unsigned char> > *d=MSTypeData<unsigned char,MSAllocator<unsigned char> >::allocateWithSize(aSize);
  const double *ap=data();
  unsigned char *dp=d->elements();
  unsigned i;
  switch (aComparison_)
   {
   case MSLessThan:
     for (i=0;i<n;i++) dp[i]=(ap[i]<aScalar_); break;
   case MSLessThanOrEqualTo:
     for (i=0;i<n;i++) dp[i]=(ap[i]<=aScalar_); break;
   case MSEqualTo:
     for (i=0;i<n;i++) dp[i]=(ap[i]==aScalar_); break;
   case MSNotEqualTo:
     for (i=0;i<n;i++) dp[i]=(ap[i]!=aScalar_); break;
   case MSGreaterThan:
     for (i=0;i<n;i++) dp[i]=(ap[i]>aScalar_); break;
   case MSGreaterThanOrEqualTo:
     for (i=0;i<n;i++) dp[i]=(ap[i]>=aScalar_); break;
   }
  return MSBinaryMatrix(d,rows(),columns());
}

MSBoolean MSTypeMatrix<double>::scalarCompare(double aScalar_,MSComparison aComparison_) const
{
  unsigned n=length();
  if (n>0)
   {
     const double *ap=data();
     unsigned i;
     switch (aComparison_)
      {
      case MSLessThan:
	for (i=0;i<n;i++) if (ap[i]>=aScalar_) return MSFalse; break;
      case MSLessThanOrEqualTo:
	for (i=0;i<n;i++) if (ap[i]>aScalar_) return MSFalse; break;
      case MSEqualTo:
	for (i=0;i<n;i++) if (ap[i]!=aScalar_) return MSFalse; break;
      case MSNotEqualTo:
	for (i=0;i<n;i++) if (ap[i]==aScalar_) return MSFalse; break;
      case MSGreaterThan:
	for (i=0;i<n;i++) if (ap[i]<=aScalar_) return MSFalse; break;
      case MSGreaterThanOrEqualTo:
	for (i=0;i<n;i++) if (ap[i]<aScalar_) return MSFalse; break;
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
MSTypeMatrix<double> operator-(const MSTypeMatrix<double>& aTypeMatrix_)
{
  unsigned aSize=aTypeMatrix_.size();
  unsigned n=aTypeMatrix_.length();
  MSTypeData<double,MSAllocator<double> > *d=MSTypeData<double,MSAllocator<double> >::allocateWithSize(aSize);
  double *sp=aTypeMatrix_.data();
  double *dp=d->elements();
  while (n--) *dp++=-*sp++;

  return MSTypeMatrix<double>(d,aTypeMatrix_.rows(),aTypeMatrix_.columns());
}


//-------------------------------------------------------------------------------------------------
// Prefix/Postfix increment/decrement
//-------------------------------------------------------------------------------------------------

// Prefix - add/subtract one, then return result
MSTypeMatrix<double>& MSTypeMatrix<double>::operator++()
{
  if (length()>0)
   {
     prepareToChange();
     unsigned n=length();
     double *sp=data();
     for (unsigned i=0;i<n;i++) ++sp[i];
     changed();
     return *this;
   }
  return *this;
}

MSTypeMatrix<double>& MSTypeMatrix<double>::operator--()
{
  if (length()>0)
   {
     prepareToChange();
     unsigned n=length();
     double *sp=data();
     for (unsigned i=0;i<n;i++) --sp[i];
     changed();
     return *this;
   }
  return *this;
}

// Postfix - add/subtract one, then return the initial value
MSTypeMatrix<double> MSTypeMatrix<double>::operator++(int)
{
  if (length()>0)
   {
     MSTypeMatrix<double> r(*this);
     prepareToChange();
     unsigned n=length();
     double *sp=data();
     for (unsigned i=0;i<n;i++) ++sp[i];
     changed();
     return r;
   }
  return MSTypeMatrix<double>();  
}

MSTypeMatrix<double> MSTypeMatrix<double>::operator--(int)
{
  if (length()>0)
   {
     MSTypeMatrix<double> r(*this);
     prepareToChange();
     unsigned n=length();
     double *sp=data();
     for (unsigned i=0;i<n;i++) --sp[i];
     changed();
     return r;
   }
  return MSTypeMatrix<double>();  
}

MSMatrixSTypePick<double>& MSMatrixSTypePick<double>::operator++(int)
{
  double value=pMatrix()->elementAt(_index)+1;
  _pMatrix->set(_index,value);
  return *this;
}

MSMatrixSTypePick<double>& MSMatrixSTypePick<double>::operator--(int)
{
  double value=pMatrix()->elementAt(_index)-1;
  _pMatrix->set(_index,value);
  return *this;
}

//-------------------------------------------------------------------------------------------------
// arithmetic operator +
//-------------------------------------------------------------------------------------------------
MSTypeMatrix<double> operator+(const MSTypeMatrix<double>& aTypeMatrix_,double aScalar_)
{
  MSTypeData<double,MSAllocator<double> > *d=0;
  unsigned n=aTypeMatrix_.length();
  unsigned aSize=aTypeMatrix_.size();
  if (n>0)
   {
     d=MSTypeData<double,MSAllocator<double> >::allocateWithSize(aSize);
     double *dp=d->elements();
     const double *ap=aTypeMatrix_.data();
     for (unsigned i=0;i<n;i++) *dp++=*ap++ + aScalar_;
   }
  return MSTypeMatrix<double>(d,aTypeMatrix_.rows(),aTypeMatrix_.columns());
}

MSTypeMatrix<double> operator+(double aScalar_,const MSTypeMatrix<double>& aTypeMatrix_)
{ return aTypeMatrix_+aScalar_; }

MSTypeMatrix<double> operator+(const MSTypeMatrix<double>& aTypeMatrix_,const MSTypeMatrix<double>& bTypeMatrix_)
{
  assert(aTypeMatrix_.rows()==bTypeMatrix_.rows()&&aTypeMatrix_.columns()==bTypeMatrix_.columns());
  MSTypeData<double,MSAllocator<double> > *d=0;
  unsigned n=aTypeMatrix_.length();
  unsigned aSize=aTypeMatrix_.size();
  if (n>0)
   {
     d=MSTypeData<double,MSAllocator<double> >::allocateWithSize(aSize);
     double *dp=d->elements();
     const double *ap=aTypeMatrix_.data();
     const double *bp=bTypeMatrix_.data();
     for (unsigned i=0;i<n;i++) *dp++=*ap++ + *bp++;
   }
  return MSTypeMatrix<double>(d,aTypeMatrix_.rows(),aTypeMatrix_.columns());
}

// apl operation aMatrix_ (x @1 0) aVector_
MSTypeMatrix<double> operator+(const MSTypeMatrix<double>& aMatrix_,const MSTypeVector<double>& aVector_)
{
  if (aMatrix_.rows()!=aVector_.length()) 
   {
     aMatrix_.error("(x @1 0) Mismatch.");
     return MSTypeMatrix<double>();
   }
  MSTypeData<double,MSAllocator<double> > *d=0;
  unsigned n=aMatrix_.length();
  unsigned aSize=aMatrix_.size();
  unsigned rows=aMatrix_.rows(),cols=aMatrix_.columns();
  if (n>0)
   {
     d=MSTypeData<double,MSAllocator<double> >::allocateWithSize(aSize);
     double *dp=d->elements();
     const double *mp=aMatrix_.data();
     const double *vp=aVector_.data();
     unsigned j;
     for (unsigned i=0;i<rows;i++,vp++)
      {
	for (j=0;j<cols;j++)
	 {
	   *dp++=*mp++ + *vp;
	 }
      }
   }
  return MSTypeMatrix<double>(d,rows,cols);
}

// apl operation aVector_ (x @1 0) aMatrix_
MSTypeMatrix<double> operator+(const MSTypeVector<double>& aVector_,const MSTypeMatrix<double>& aMatrix_)
{ return aMatrix_+aVector_; }

//-------------------------------------------------------------------------------------------------
// arithmetic assignment + operator
//-------------------------------------------------------------------------------------------------
MSTypeMatrix<double>& MSTypeMatrix<double>::operator+=(double aScalar_)
{
  prepareToChange();
  unsigned n=length();
  double c=aScalar_;
  double *dp=data();
  while (n--) *dp++ += c;
  changed();
  return *this;
}

MSMatrixSTypePick<double>& MSMatrixSTypePick<double>::operator+=(double aScalar_)
{
  double value=pMatrix()->elementAt(_index)+aScalar_;
  _pMatrix->set(_index,value);
  return *this;
}

//-------------------------------------------------------------------------------------------------
// arithmetic operator -
//-------------------------------------------------------------------------------------------------
MSTypeMatrix<double> operator-(const MSTypeMatrix<double>& aTypeMatrix_,double aScalar_)
{
  MSTypeData<double,MSAllocator<double> > *d=0;
  unsigned n=aTypeMatrix_.length();
  unsigned aSize=aTypeMatrix_.size();
  if (n>0)
   {
     d=MSTypeData<double,MSAllocator<double> >::allocateWithSize(aSize);
     double *dp=d->elements();
     const double *ap=aTypeMatrix_.data();
     for (unsigned i=0;i<n;i++) *dp++=*ap++ - aScalar_;
   }
  return MSTypeMatrix<double>(d,aTypeMatrix_.rows(),aTypeMatrix_.columns());
}

MSTypeMatrix<double> operator-(const MSTypeMatrix<double>& aTypeMatrix_,const MSTypeMatrix<double>& bTypeMatrix_)
{
  assert(aTypeMatrix_.rows()==bTypeMatrix_.rows()&&aTypeMatrix_.columns()==bTypeMatrix_.columns());
  MSTypeData<double,MSAllocator<double> > *d=0;
  unsigned n=aTypeMatrix_.length();
  unsigned aSize=aTypeMatrix_.size();
  if (n>0)
   {
     d=MSTypeData<double,MSAllocator<double> >::allocateWithSize(aSize);
     double *dp=d->elements();
     const double *ap=aTypeMatrix_.data();
     const double *bp=bTypeMatrix_.data();
     for (unsigned i=0;i<n;i++) *dp++=*ap++ - *bp++;
   }
  return MSTypeMatrix<double>(d,aTypeMatrix_.rows(),aTypeMatrix_.columns());
}

// apl operation aMatrix_ (x @1 0) aVector_
MSTypeMatrix<double> operator-(const MSTypeMatrix<double>& aMatrix_,const MSTypeVector<double>& aVector_)
{
  if (aMatrix_.rows()!=aVector_.length()) 
   {
     aMatrix_.error("(x @1 0) Mismatch.");
     return MSTypeMatrix<double>();
   }
  MSTypeData<double,MSAllocator<double> > *d=0;
  unsigned n=aMatrix_.length();
  unsigned aSize=aMatrix_.size();
  unsigned rows=aMatrix_.rows(),cols=aMatrix_.columns();
  if (n>0)
   {
     d=MSTypeData<double,MSAllocator<double> >::allocateWithSize(aSize);
     double *dp=d->elements();
     const double *mp=aMatrix_.data();
     const double *vp=aVector_.data();
     unsigned j;
     for (unsigned i=0;i<rows;i++,vp++)
      {
	for (j=0;j<cols;j++)
	 {
	   *dp++=*mp++ - *vp;
	 }
      }
   }
  return MSTypeMatrix<double>(d,rows,cols);
}

//-------------------------------------------------------------------------------------------------
// arithmetic assignment - operator
//-------------------------------------------------------------------------------------------------
MSTypeMatrix<double>& MSTypeMatrix<double>::operator-=(double aScalar_)
{
  prepareToChange();
  unsigned n=length();
  double c=aScalar_;
  double *dp=data();
  while (n--) *dp++ -= c;
  changed();
  return *this;
}

MSMatrixSTypePick<double>& MSMatrixSTypePick<double>::operator-=(double aScalar_)
{
  double value=pMatrix()->elementAt(_index)-aScalar_;
  _pMatrix->set(_index,value);
  return *this;
}

//-------------------------------------------------------------------------------------------------
// arithmetic operator *
//-------------------------------------------------------------------------------------------------
MSTypeMatrix<double> operator*(const MSTypeMatrix<double>& aTypeMatrix_,double aScalar_)
{
  MSTypeData<double,MSAllocator<double> > *d=0;
  unsigned n=aTypeMatrix_.length();
  unsigned aSize=aTypeMatrix_.size();
  if (n>0)
   {
     d=MSTypeData<double,MSAllocator<double> >::allocateWithSize(aSize);
     double *dp=d->elements();
     const double *ap=aTypeMatrix_.data();
     for (unsigned i=0;i<n;i++) *dp++=*ap++ * aScalar_;
   }
  return MSTypeMatrix<double>(d,aTypeMatrix_.rows(),aTypeMatrix_.columns());
}

MSTypeMatrix<double> operator*(double aScalar_,const MSTypeMatrix<double>& aTypeMatrix_)
{ return aTypeMatrix_*aScalar_; }

MSTypeMatrix<double> operator*(const MSTypeMatrix<double>& aTypeMatrix_,const MSTypeMatrix<double>& bTypeMatrix_)
{
  assert(aTypeMatrix_.rows()==bTypeMatrix_.rows()&&aTypeMatrix_.columns()==bTypeMatrix_.columns());
  MSTypeData<double,MSAllocator<double> > *d=0;
  unsigned n=aTypeMatrix_.length();
  unsigned aSize=aTypeMatrix_.size();
  if (n>0)
   {
     d=MSTypeData<double,MSAllocator<double> >::allocateWithSize(aSize);
     double *dp=d->elements();
     const double *ap=aTypeMatrix_.data();
     const double *bp=bTypeMatrix_.data();
     for (unsigned i=0;i<n;i++) *dp++=*ap++ * *bp++;
   }
  return MSTypeMatrix<double>(d,aTypeMatrix_.rows(),aTypeMatrix_.columns());
}

// apl operation aTypeMatrix_ (x @1 0) aTypeVector_
MSTypeMatrix<double> operator*(const MSTypeMatrix<double>& aMatrix_,const MSTypeVector<double>& aVector_)
{
  if (aMatrix_.rows()!=aVector_.length()) 
   {
     aMatrix_.error("(x @1 0) Mismatch.");
     return MSTypeMatrix<double>();
   }
  MSTypeData<double,MSAllocator<double> > *d=0;
  unsigned n=aMatrix_.length();
  unsigned aSize=aMatrix_.size();
  unsigned rows=aMatrix_.rows(),cols=aMatrix_.columns();
  if (n>0)
   {
     d=MSTypeData<double,MSAllocator<double> >::allocateWithSize(aSize);
     double *dp=d->elements();
     const double *mp=aMatrix_.data();
     const double *vp=aVector_.data();
     unsigned j;
     for (unsigned i=0;i<rows;i++,vp++)
      {
	for (j=0;j<cols;j++)
	 {
	   *dp++=*mp++ * *vp;
	 }
      }
   }
  return MSTypeMatrix<double>(d,rows,cols);
}

// apl operation aVector_ (x @1 0) aMatrix_
MSTypeMatrix<double> operator*(const MSTypeVector<double>& aVector_,const MSTypeMatrix<double>& aMatrix_)
{ return aMatrix_+aVector_; }

//-------------------------------------------------------------------------------------------------
// arithmetic assignment * operator
//-------------------------------------------------------------------------------------------------
MSTypeMatrix<double>& MSTypeMatrix<double>::operator*=(double aScalar_)
{
  prepareToChange();
  unsigned n=length();
  double c=aScalar_;
  double *dp=data();
  while (n--) *dp++ *= c;
  changed();
  return *this;
}

MSMatrixSTypePick<double>& MSMatrixSTypePick<double>::operator*=(double aScalar_)
{
  double value=pMatrix()->elementAt(_index)*aScalar_;
  _pMatrix->set(_index,value);
  return *this;
}

//-------------------------------------------------------------------------------------------------
// arithmetic operator /
//-------------------------------------------------------------------------------------------------
MSTypeMatrix<double> operator/(const MSTypeMatrix<double>& aTypeMatrix_,double aScalar_)
{
  MSTypeData<double,MSAllocator<double> > *d=0;
  unsigned n=aTypeMatrix_.length();
  unsigned aSize=aTypeMatrix_.size();
  if (n>0)
   {
     d=MSTypeData<double,MSAllocator<double> >::allocateWithSize(aSize);
     double *dp=d->elements();
     const double *ap=aTypeMatrix_.data();
     for (unsigned i=0;i<n;i++) *dp++=*ap++ / aScalar_;
   }
  return MSTypeMatrix<double>(d,aTypeMatrix_.rows(),aTypeMatrix_.columns());
}

MSTypeMatrix<double> operator/(const MSTypeMatrix<double>& aTypeMatrix_,const MSTypeMatrix<double>& bTypeMatrix_)
{
  assert(aTypeMatrix_.rows()==bTypeMatrix_.rows()&&aTypeMatrix_.columns()==bTypeMatrix_.columns());
  MSTypeData<double,MSAllocator<double> > *d=0;
  unsigned n=aTypeMatrix_.length();
  unsigned aSize=aTypeMatrix_.size();
  if (n>0)
   {
     d=MSTypeData<double,MSAllocator<double> >::allocateWithSize(aSize);
     double *dp=d->elements();
     const double *ap=aTypeMatrix_.data();
     const double *bp=bTypeMatrix_.data();
     for (unsigned i=0;i<n;i++) *dp++=*ap++ / *bp++;
   }
  return MSTypeMatrix<double>(d,aTypeMatrix_.rows(),aTypeMatrix_.columns());
}

// apl operation aMatrix_ (x @1 0) aVector_
MSTypeMatrix<double> operator/(const MSTypeMatrix<double>& aMatrix_,const MSTypeVector<double>& aVector_)
{
  if (aMatrix_.rows()!=aVector_.length()) 
   {
     aMatrix_.error("(x @1 0) Mismatch.");
     return MSTypeMatrix<double>();
   }
  MSTypeData<double,MSAllocator<double> > *d=0;
  unsigned n=aMatrix_.length();
  unsigned aSize=aMatrix_.size();
  unsigned rows=aMatrix_.rows(),cols=aMatrix_.columns();
  if (n>0)
   {
     d=MSTypeData<double,MSAllocator<double> >::allocateWithSize(aSize);
     double *dp=d->elements();
     const double *mp=aMatrix_.data();
     const double *vp=aVector_.data();
     unsigned j;
     for (unsigned i=0;i<rows;i++,vp++)
      {
	for (j=0;j<cols;j++)
	 {
	   *dp++=*mp++ / *vp;
	 }
      }
   }
  return MSTypeMatrix<double>(d,rows,cols);
}

//-------------------------------------------------------------------------------------------------
// arithmetic assignment / operator
//-------------------------------------------------------------------------------------------------
MSTypeMatrix<double>& MSTypeMatrix<double>::operator/=(double aScalar_)
{
  prepareToChange();
  unsigned n=length();
  double c=aScalar_;
  double *dp=data();
  while (n--) *dp++ /= c;
  changed();
  return *this;
}

MSMatrixSTypePick<double>& MSMatrixSTypePick<double>::operator/=(double aScalar_)
{
  double value=pMatrix()->elementAt(_index)/aScalar_;
  _pMatrix->set(_index,value);
  return *this;
}

//-------------------------------------------------------------------------------------------------
// multiply, stack, adjoin
//-------------------------------------------------------------------------------------------------

// inner product
MSTypeMatrix<double> multiply(const MSTypeMatrix<double>& aTypeMatrix_,const MSTypeMatrix<double>& bTypeMatrix_)
{
  if (aTypeMatrix_.columns()!=bTypeMatrix_.rows())
   {
     aTypeMatrix_.error("nonconformant MSTypeMatrix multiply operands.");
     return MSTypeMatrix<double>();
   }
  unsigned len=aTypeMatrix_.rows()*bTypeMatrix_.columns();
  MSTypeData<double,MSAllocator<double> > *d=MSTypeData<double,MSAllocator<double> >::allocateWithLength(len);

  double *dp=d->elements();
  double *row=aTypeMatrix_.data();
  double *ap=aTypeMatrix_.data();
  double *bp;
  unsigned column;

  if (ap!=0)
   {
     while ((row+=aTypeMatrix_.columns())<=aTypeMatrix_.data()+aTypeMatrix_.length())
      {
	for (column=0;column<bTypeMatrix_.columns();column++)
	 {
	   bp=bTypeMatrix_.data()+column;
	   *dp=0;
	   while (ap<row)
	    {
	      *dp+=*ap++**bp;
	      bp+=bTypeMatrix_.columns();
	    }
	   // next d element
	   dp++;
	   // reset A row
	   ap-=aTypeMatrix_.columns();
	   // next B column
	 }
	// next A row
	ap+=aTypeMatrix_.columns();
	// reset B column
      } 
   }
  else  for (unsigned i=0;i<len;i++) *dp++=0; 

  return MSTypeMatrix<double>(d,aTypeMatrix_.rows(),bTypeMatrix_.columns());
}

MSTypeMatrix<double> stack(const MSTypeMatrix<double>& aTypeMatrix_,const MSTypeMatrix<double>& bTypeMatrix_)
{
  if (aTypeMatrix_.columns()!=bTypeMatrix_.columns()) 
   {
     aTypeMatrix_.error("nonconformant MSTypeMatrix stack operands.");
     return MSTypeMatrix<double>();
   }

  MSTypeData<double,MSAllocator<double> > *d=0;
  unsigned newLength=(aTypeMatrix_.rows()+bTypeMatrix_.rows())*aTypeMatrix_.columns();
  if (newLength>0)
   { 
     d=MSTypeData<double,MSAllocator<double> >::allocateWithLength(newLength);     
     double *dp=d->elements();
     double *mp=aTypeMatrix_.data();
     double *row=aTypeMatrix_.data()+aTypeMatrix_.columns();
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

  return MSTypeMatrix<double>(d,aTypeMatrix_.rows()+bTypeMatrix_.rows(),aTypeMatrix_.columns());
}

MSTypeMatrix<double>& MSTypeMatrix<double>::stack(const MSTypeMatrix<double>& aTypeMatrix_)
{
  if (aTypeMatrix_.columns()!=columns()) 
   {
     error("nonconformant stack operands.");
     return *this;
   }
  unsigned newLength=(rows()+aTypeMatrix_.rows())*columns();
  MSTypeData<double,MSAllocator<double> > *d=0;
  if (newLength>0)
   {
     d=MSTypeData<double,MSAllocator<double> >::allocateWithLength(newLength);     
     double *dp=d->elements();
     double *mp=data();
     double *row=data()+columns();
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

MSTypeMatrix<double> adjoin(const MSTypeMatrix<double>& aTypeMatrix_,const MSTypeMatrix<double>& bTypeMatrix_)
{
  if (aTypeMatrix_.rows()!=bTypeMatrix_.rows())
   {
     aTypeMatrix_.error("nonconformant MSTypeMatrix adjoin operands.");
     return MSTypeMatrix<double>();
   }
  MSTypeData<double,MSAllocator<double> > *d=0;
  unsigned newLength=aTypeMatrix_.rows()*(aTypeMatrix_.columns()+bTypeMatrix_.columns());
  if (newLength>0)
   {
     d=MSTypeData<double,MSAllocator<double> >::allocateWithLength(newLength);     
     double *dp=d->elements();
     double *mp=aTypeMatrix_.data();
     double *row=aTypeMatrix_.data()+aTypeMatrix_.columns();
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

  return MSTypeMatrix<double>(d,aTypeMatrix_.rows(),aTypeMatrix_.columns()+bTypeMatrix_.columns());
}

MSTypeMatrix<double>& MSTypeMatrix<double>::adjoin(const MSTypeMatrix<double>& aTypeMatrix_)
{
  if (rows()!=aTypeMatrix_.rows())
   {
     aTypeMatrix_.error("nonconformant MSTypeMatrix adjoin operands.");
     return *this;
   }
  MSTypeData<double,MSAllocator<double> > *d=0;
  unsigned newLength=rows()*(columns()+aTypeMatrix_.columns());
  if (newLength>0)
   {
     d=MSTypeData<double,MSAllocator<double> >::allocateWithLength(newLength);     
     double *dp=d->elements();
     double *mp=data();
     double *row=data()+columns();
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

MSTypeMatrix<double>& MSTypeMatrix<double>::random(unsigned long limit_)
{
  int n=length();
  if (n>0)
   {
     prepareToChange();
     if (limit_==0) limit_=length();
     double *dp=data();
     MSRandom rand;
     while (n--) *dp++=(double)rand(limit_);
     changed();
   }
  return *this;
}

double MSTypeMatrix<double>::min(void) const 
{
  double min;
  unsigned n=length();
  if (n>0)
   {
     double *dp=data();
     min=dp[0];
     for (unsigned i=1;i<n;i++) if (dp[i]<min) min=dp[i];
   }
  else min=0.0;
  return min;
}

double MSTypeMatrix<double>::max(void) const 
{
  double max;
  unsigned n=length();
  if (n>0)
   {
     double *dp=data();
     max=dp[0];
     for (unsigned i=1;i<n;i++) if (dp[i]>max) max=dp[i];
   }
  else max=0.0;
  return max;
}


double MSTypeMatrix<double>::sum(void) const 
{
  double sum=0.0;
  double *dp=data();
  unsigned n=length();
  while (n--) sum+=*dp++;
  return sum;
}

void MSTypeMatrix<double>::error(const char* msg_) const
{ (*_matrixErrorHandler)(msg_); }

//------------------------------------------------------------------------------------------
// matrix manipulation methods
//-------------------------------------------------------------------------------------------------
MSTypeMatrix<double>& MSTypeMatrix<double>::appendColumn(const MSTypeVector<double>& aTypeVector_)
{
  if (rows()==0||aTypeVector_.length()!=rows())
   {
     error("MSTypeMatrix length error.");
     return *this;
   }
  unsigned newLength=rows()*(columns()+1);
  MSTypeData<double,MSAllocator<double> > *d=MSTypeData<double,MSAllocator<double> >::allocateWithLength(newLength);
  double *dp=d->elements();
  double *mp=data();
  double *vp=aTypeVector_.data();
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

MSTypeMatrix<double>& MSTypeMatrix<double>::appendColumns(unsigned cols_,double fill_)
{
  if (rows()==0)
   {
     error("MSTypeMatrix length error.");
     return *this;
   }
  unsigned newLength=rows()*(columns()+cols_);
  MSTypeData<double,MSAllocator<double> > *d=MSTypeData<double,MSAllocator<double> >::allocateWithLength(newLength);  
  double *dp=d->elements();
  double *mp=data();
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

MSTypeMatrix<double>& MSTypeMatrix<double>::appendRow(const MSTypeVector<double>& aTypeVector_)
{
  if (columns()==0||aTypeVector_.length()!=columns())
   {
     error("MSTypeMatrix length error.");
     return *this;
   }
  unsigned newLength=(rows()+1)*columns();
  MSTypeData<double,MSAllocator<double> > *d=MSTypeData<double,MSAllocator<double> >::allocateWithLength(newLength);
  double *dp=d->elements();
  double *mp=data();
  double *vp=aTypeVector_.data();
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

MSTypeMatrix<double>& MSTypeMatrix<double>::appendRows(unsigned rows_, double fill_)
{
  if (columns()==0)
   {
     error("MSTypeMatrix length error.");
     return *this;
   }
  unsigned newLength=(rows()+rows_)*columns();
  MSTypeData<double,MSAllocator<double> > *d=MSTypeData<double,MSAllocator<double> >::allocateWithLength(newLength);
  double *dp=d->elements();
  double *mp=data();
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

MSTypeMatrix<double>& MSTypeMatrix<double>::exchangeColumns(unsigned aColumn_,unsigned bColumn_)
{
  if (aColumn_+1>columns()||bColumn_+1>columns()||aColumn_==bColumn_) return *this;

  prepareToChange();

  double *aPtr=data()+aColumn_;
  double *bPtr=data()+bColumn_;
  register double tVal;

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

MSTypeMatrix<double>& MSTypeMatrix<double>::exchangeRows(unsigned aRow_,unsigned bRow_)
{
  if (aRow_+1>rows()||bRow_+1>rows()||aRow_==bRow_) return *this;

  prepareToChange();

  double *aPtr=data()+aRow_*columns();
  double *bPtr=data()+bRow_*columns();
  register double tVal;

  for (unsigned i=0;i<columns();i++)
   {
     tVal=*aPtr;
     *aPtr++=*bPtr;
     *bPtr++=tVal;
   }
  changed();  
  return *this;
}

MSTypeMatrix<double>& MSTypeMatrix<double>::insertColumnBefore(unsigned col_,double fill_)
{
  if (col_+1>columns()) return *this;
  
  unsigned newLength=rows()*(columns()+1);
  MSTypeData<double,MSAllocator<double> > *d=MSTypeData<double,MSAllocator<double> >::allocateWithLength(newLength);
  double *dp=d->elements();
  double *mp=data();
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

MSTypeMatrix<double>& MSTypeMatrix<double>::insertColumnBefore(unsigned col_,const MSTypeVector<double>& aTypeVector_)
{
  if (col_+1>columns()) return *this;
  if (aTypeVector_.length()!=rows())
   {
     error("MSTypeMatrix length error.");
     return *this;
   }
  
  unsigned newLength=rows()*(columns()+1);
  MSTypeData<double,MSAllocator<double> > *d=MSTypeData<double,MSAllocator<double> >::allocateWithLength(newLength);  
  double *dp=d->elements();
  double *mp=data();
  double *vp=aTypeVector_.data();
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

MSTypeMatrix<double>& MSTypeMatrix<double>::insertColumnAfter(unsigned col_,double fill_)
{
  if (col_+1>columns()) return *this;

  unsigned newLength=rows()*(columns()+1);
  MSTypeData<double,MSAllocator<double> > *d=MSTypeData<double,MSAllocator<double> >::allocateWithLength(newLength);
  double *dp=d->elements();
  double *mp=data();
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

MSTypeMatrix<double>& MSTypeMatrix<double>::insertColumnAfter(unsigned col_,const MSTypeVector<double>& aTypeVector_)
{
  if (col_+1>columns()) return *this;
  if (aTypeVector_.length()!=rows())
   {
     error("MSTypeMatrix length error.");
     return *this;
   }

  unsigned newLength=rows()*(columns()+1);
  MSTypeData<double,MSAllocator<double> > *d=MSTypeData<double,MSAllocator<double> >::allocateWithLength(newLength);  
  double *dp=d->elements();
  double *mp=data();
  double *vp=aTypeVector_.data();
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

MSTypeMatrix<double>& MSTypeMatrix<double>::insertRowBefore(unsigned row_,double fill_)
{
  if (row_+1>rows()) return *this;

  unsigned newLength=(rows()+1)*columns();
  MSTypeData<double,MSAllocator<double> > *d=MSTypeData<double,MSAllocator<double> >::allocateWithLength(newLength);  
  double *dp=d->elements();
  double *mp=data();
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

MSTypeMatrix<double>& MSTypeMatrix<double>::insertRowBefore(unsigned row_,const MSTypeVector<double>& aTypeVector_)
{
  if (row_+1>rows()) return *this;
  if (aTypeVector_.length()!=columns())
   {
     error("MSTypeMatrix length error.");
     return *this;
   }

  unsigned newLength=(rows()+1)*columns();
  MSTypeData<double,MSAllocator<double> > *d=MSTypeData<double,MSAllocator<double> >::allocateWithLength(newLength);
  double *dp=d->elements();
  double *mp=data();
  double *vp=aTypeVector_.data();
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

MSTypeMatrix<double>& MSTypeMatrix<double>::insertRowAfter(unsigned row_,double fill_)
{
  if (row_+1>rows()) return *this;

  unsigned newLength=(rows()+1)*columns();
  MSTypeData<double,MSAllocator<double> > *d=MSTypeData<double,MSAllocator<double> >::allocateWithLength(newLength);  
  double *dp=d->elements();
  double *mp=data();
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

MSTypeMatrix<double>& MSTypeMatrix<double>::insertRowAfter(unsigned row_,const MSTypeVector<double>& aTypeVector_)
{
  if (row_>rows()) return *this;
  if (aTypeVector_.length()!=columns())
   {
     error("MSTypeMatrix length error.");
     return *this;
   }

  unsigned newLength=(rows()+1)*columns();
  MSTypeData<double,MSAllocator<double> > *d=MSTypeData<double,MSAllocator<double> >::allocateWithLength(newLength);  
  double *dp=d->elements();
  double *mp=data();
  double *vp=aTypeVector_.data();
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

MSTypeMatrix<double>& MSTypeMatrix<double>::reshape(unsigned rows_,unsigned cols_)
{
  MSTypeData<double,MSAllocator<double> > *d=0;
  unsigned newLength=rows_*cols_;
  if (newLength>0)
   {
     d=MSTypeData<double,MSAllocator<double> >::allocateWithLength(newLength);
     double *dp=d->elements();
     double *mp=data();
     double *end=data()+length();
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

MSTypeMatrix<double>& MSTypeMatrix<double>::removeAll(void)
{
  freeData();
  _rows=0,_columns=0,_count=0;
  changed();  
  return *this;
}

MSTypeMatrix<double>& MSTypeMatrix<double>::removeAllRows(void)
{
  freeData();
  _rows=0,_count=0;
  changed();  
  return *this;
}

MSTypeMatrix<double>& MSTypeMatrix<double>::removeAllColumns(void)
{
  freeData();
  _columns=0,_count=0;
  changed();  
  return *this;
}

MSTypeMatrix<double>& MSTypeMatrix<double>::assignColumn(unsigned col_,double scalar_)
{
  if (col_+1>columns()) return *this;
  prepareToChange();
  double *mp=data(); 
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

MSTypeMatrix<double>& MSTypeMatrix<double>::assignRow(unsigned row_,double scalar_)
{
  if (row_+1>rows()) return *this;
  prepareToChange();
  double *mp=data(); 
  unsigned i,j;
  for (i=row_*columns(),j=0;j<columns();i++,j++) mp[i]=scalar_;
  if (receiverList()!=0)
   {
     MSIndexVector iv;
     changed(iv.series(columns(),row_*columns()));
   }
  return *this;
}

MSTypeMatrix<double>& MSTypeMatrix<double>::assignColumn(unsigned col_, const MSTypeVector<double>& aTypeVector_)
{
  if (col_+1>columns()) return *this;
  if (aTypeVector_.length()!=rows())
   {
     error("MSTypeMatrix length error.");
     return *this;
   }
  prepareToChange();
  double *mp;
  unsigned i;
  for (i=0,mp=data()+col_;i<rows();i++,mp+=columns()) *mp=aTypeVector_(i);
  changed();
  return *this;
}

MSTypeMatrix<double>& MSTypeMatrix<double>::assignRow(unsigned row_, const MSTypeVector<double>& aTypeVector_)
{
  if (row_+1>rows()) return *this;
  if (aTypeVector_.length()!=columns())
   {
     error("MSTypeMatrix length error.");
     return *this;
   }
  prepareToChange();
  double *mp;
  unsigned i;
  for (i=0,mp=data()+row_*columns();i<columns();i++,mp++) *mp=aTypeVector_(i);
  if (receiverList()!=0)
   {
     MSIndexVector iv;
     changed(iv.series(columns(),row_*columns()));
   }
  return *this;
}

MSTypeMatrix<double>& MSTypeMatrix<double>::removeRow(unsigned row_)
{
  if (row_+1>rows()) return *this;
  if (data()==0) return *this;
  
  unsigned newLength=(rows()-1)*columns();
  MSTypeData<double,MSAllocator<double> > *d=MSTypeData<double,MSAllocator<double> >::allocateWithLength(newLength);  
  double *dp=d->elements();
  double *mp=data();
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

MSTypeMatrix<double>& MSTypeMatrix<double>::removeColumn(unsigned col_)
{
  if (col_+1>columns()) return *this;
  if (data()==0) return *this;

  unsigned newLength=rows()*(columns()-1);
  MSTypeData<double,MSAllocator<double> > *d=MSTypeData<double,MSAllocator<double> >::allocateWithLength(newLength);  
  double *dp=d->elements();
  double *mp=data();
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

MSTypeMatrix<double>& MSTypeMatrix<double>::compressRows(const MSBinaryVector& aBinaryVector_ )
{
  if (data()==0) return *this;
  if (aBinaryVector_.length()!=rows())
   {
     error("MSTypeMatrix length error.");
     return *this;
   }
  
  unsigned newLength=(unsigned)(aBinaryVector_.sum()*columns());
  MSTypeData<double,MSAllocator<double> > *d=MSTypeData<double,MSAllocator<double> >::allocateWithLength(newLength);
  double *dp=d->elements();
  double *mp=data();
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

MSTypeMatrix<double>& MSTypeMatrix<double>::compressColumns(const MSBinaryVector& aBinaryVector_ )
{
  if (data()==0) return *this;
  if (aBinaryVector_.length()!=columns())
   {
     error("MSTypeMatrix length error.");
     return *this;
   }
  
  unsigned newLength=(unsigned)(rows()*aBinaryVector_.sum());
  MSTypeData<double,MSAllocator<double> > *d=MSTypeData<double,MSAllocator<double> >::allocateWithLength(newLength);  
  double *dp=d->elements();
  double *mp=data();
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

MSTypeMatrix<double>& MSTypeMatrix<double>::reverseRows(void)
{
  if (data()==0) return *this;
  prepareToChange();
  unsigned i,j;
  double *upperRow=data();
  double *lowerRow=data()+rows()*columns()-columns();
  register double tVal;
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

MSTypeMatrix<double>& MSTypeMatrix<double>::reverseColumns(void)
{
  if (data()==0) return *this;
  prepareToChange();
  unsigned i,j;
  double *leftColumn=data();
  double *rightColumn=data()+columns()-1;
  register double tVal;
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

MSTypeMatrix<double>& MSTypeMatrix<double>::transpose(void)
{
  if (data()==0) return *this;
  unsigned aSize=size();
  MSTypeData<double,MSAllocator<double> > *d=MSTypeData<double,MSAllocator<double> >::allocateWithSize(aSize);
  double *dp=d->elements();
  double *mp=data();
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

MSTypeMatrix<double>& MSTypeMatrix<double>::rotateRows(int position_)
{
  int rowPosition=MSUtil::abs(position_);
  if (rowPosition>0&&rowPosition!=rows())
   {
     unsigned i;
     unsigned aSize=size();
     MSTypeData<double,MSAllocator<double> > *d=MSTypeData<double,MSAllocator<double> >::allocateWithSize(aSize);
     if (rowPosition>rows()) rowPosition%=rows();
     if (position_<0) rowPosition=rows()-rowPosition;
     unsigned start=rowPosition*columns();
     double *mp=data()+start;
     double *dp=d->elements();
     for (i=start;i<length();i++) *dp++=*mp++;
     mp=data();
     for (i=0;i<start;i++) *dp++=*mp++;
     freeData();
     _pData=d;
     changed();     
   }
  return *this;
}

MSTypeMatrix<double>& MSTypeMatrix<double>::rotateColumns(int position_)
{
  int columnPosition=MSUtil::abs(position_);
  if (columnPosition>0&&columnPosition!=columns())
   {
     unsigned i,j;
     unsigned aSize=size();
     MSTypeData<double,MSAllocator<double> > *d=MSTypeData<double,MSAllocator<double> >::allocateWithSize(aSize);
     if (columnPosition>columns()) columnPosition%=columns();
     if (position_<0) columnPosition=columns()-columnPosition;
     double *mp=data()+columnPosition;
     double *dp=d->elements();
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

MSTypeMatrix<double>& MSTypeMatrix<double>::takeRows(int numberOfRows_)
{
  int numberOfRows=MSUtil::abs(numberOfRows_);
  if (numberOfRows>0&&numberOfRows!=rows())
   {
     unsigned i;
     unsigned newLength=numberOfRows*columns();
     MSTypeData<double,MSAllocator<double> > *d=MSTypeData<double,MSAllocator<double> >::allocateWithLength(newLength);     
     double *mp=data();
     double *dp=d->elements();
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

MSTypeMatrix<double>& MSTypeMatrix<double>::takeColumns(int numberOfColumns_)
{
  int numberOfColumns=MSUtil::abs(numberOfColumns_);
  if (numberOfColumns>0&&numberOfColumns!=columns())
   {
     unsigned i,j;
     unsigned newLength=numberOfColumns*rows();
     MSTypeData<double,MSAllocator<double> > *d=MSTypeData<double,MSAllocator<double> >::allocateWithLength(newLength);     
     double *mp=data();
     double *dp=d->elements();
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

MSTypeMatrix<double>& MSTypeMatrix<double>::dropRows(int numberOfRows_)
{
  int numberOfRows=MSUtil::abs(numberOfRows_);
  if (numberOfRows>0)
   {
     if (numberOfRows<rows())
      {
	unsigned i;
	unsigned nRows=rows()-numberOfRows;
	unsigned newLength=nRows*columns();
	MSTypeData<double,MSAllocator<double> > *d=MSTypeData<double,MSAllocator<double> >::allocateWithLength(newLength);
	double *mp=data();
	double *dp=d->elements();
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

MSTypeMatrix<double>& MSTypeMatrix<double>::dropColumns(int numberOfColumns_)
{
  int numberOfColumns=MSUtil::abs(numberOfColumns_);
  if (numberOfColumns>0)
   {
     if (numberOfColumns<columns())
      {
	unsigned i,j;
	unsigned cols=columns()-numberOfColumns;
	unsigned newLength=cols*rows();
	MSTypeData<double,MSAllocator<double> > *d=MSTypeData<double,MSAllocator<double> >::allocateWithLength(newLength);	
	double *mp=data();
	double *dp=d->elements();
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

MSTypeVector<double> MSTypeMatrix<double>::rowAt(unsigned row_) const
{
  if (row_+1>rows()||columns()==0) return MSTypeVector<double>();

  MSTypeData<double,MSAllocator<double> > *d=MSTypeData<double,MSAllocator<double> >::allocateWithLength(columns());  
  double *mp;
  double *dp=d->elements();
  unsigned i;
  for (i=0,mp=data()+row_*columns();i<columns();i++,mp++) *dp++=*mp;

  return MSTypeVector<double> (d,columns());  
}

MSTypeVector<double> MSTypeMatrix<double>::columnAt(unsigned column_) const
{
  if (column_+1>columns()||rows()==0) return MSTypeVector<double>();

  MSTypeData<double,MSAllocator<double> > *d=MSTypeData<double,MSAllocator<double> >::allocateWithLength(rows());  
  double *mp;
  double *dp=d->elements();
  unsigned i;
  for (i=0,mp=data()+column_;i<rows();i++,mp+=columns()) *dp++=*mp;

  return MSTypeVector<double>(d,rows());
}

//------------------------------------------------------------
// Math Functions
//------------------------------------------------------------
MSTypeMatrix<double>& MSTypeMatrix<double>::sin(void)
{ return allElementsDo(::sin); }
MSTypeMatrix<double>& MSTypeMatrix<double>::sinh(void)
{ return allElementsDo(::sinh); }
MSTypeMatrix<double>& MSTypeMatrix<double>::cos(void)
{ return allElementsDo(::cos); }
MSTypeMatrix<double>& MSTypeMatrix<double>::cosh(void)
{ return allElementsDo(::cosh); }
MSTypeMatrix<double>& MSTypeMatrix<double>::tan(void)
{ return allElementsDo(::tan); }
MSTypeMatrix<double>& MSTypeMatrix<double>::tanh(void)
{ return allElementsDo(::tanh); }
MSTypeMatrix<double>& MSTypeMatrix<double>::asin(void)
{ return allElementsDo(::asin); }
MSTypeMatrix<double>& MSTypeMatrix<double>::acos(void)
{ return allElementsDo(::acos); }
MSTypeMatrix<double>& MSTypeMatrix<double>::atan(void)
{ return allElementsDo(::atan); }
MSTypeMatrix<double>& MSTypeMatrix<double>::exp(void)
{ return allElementsDo(::exp); }
MSTypeMatrix<double>& MSTypeMatrix<double>::log(void)
{ return allElementsDo(::log); }
MSTypeMatrix<double>& MSTypeMatrix<double>::sqrt(void)
{ return allElementsDo(::sqrt); }
MSTypeMatrix<double>& MSTypeMatrix<double>::ceil(void)
{ return allElementsDo(::ceil); }
MSTypeMatrix<double>& MSTypeMatrix<double>::floor(void)
{ return allElementsDo(::floor); }
MSTypeMatrix<double>& MSTypeMatrix<double>::abs(void)
{ return allElementsDo(::fabs); }

MSTypeMatrix<double>& MSTypeMatrix<double>::pow(double exponent_)
{ return allElementsDo(_pow,(void *)&exponent_); }

MSTypeMatrix<double>& MSTypeMatrix<double>::allElementsDo(MathFunction aFunction_)
{
  prepareToChange();
  double *dp=data();
  double *end=data()+length();
  while (dp<end) { *dp=aFunction_(*dp); dp++; }
  changed();
  return *this;
}

MSTypeMatrix<double>& MSTypeMatrix<double>::allElementsDo(ElementWiseFunction aFunction_,void *clientData_)
{
  prepareToChange();
  double *dp=data();
  double *end=data()+length();
  while (dp<end) { *dp=aFunction_(*dp,clientData_); dp++; }
  changed();
  return *this;
}

MSTypeMatrix<double> allElementsDo(const MSTypeMatrix<double>& aFloatMatrix_,MathFunction aFunction_)
{
  MSTypeData<double,MSAllocator<double> > *d=MSTypeData<double,MSAllocator<double> >::allocateWithSize(aFloatMatrix_.size());
  const double *sp=aFloatMatrix_.data();
  double *dp=d->elements();
  double *end=d->elements()+aFloatMatrix_.length();
  while (dp<end) *dp++=aFunction_(*sp++);
  return MSTypeMatrix<double>(d,aFloatMatrix_.rows(),aFloatMatrix_.columns());
}

MSTypeMatrix<double> allElementsDo(const MSTypeMatrix<double>& aTypeMatrix_,
				   ElementWiseFunction aFunction_,void *clientData_)
{
  unsigned n=aTypeMatrix_.count();
  unsigned aSize=aTypeMatrix_.size();
  MSTypeData<double,MSAllocator<double> > *d=MSTypeData<double,MSAllocator<double> >::allocateWithSize(aSize);
  const double *sp=aTypeMatrix_.data();
  double *dp=d->elements();
  for (unsigned i=0;i<n;i++) *dp++=aFunction_(*sp++,clientData_);
  return MSTypeMatrix<double>(d,aTypeMatrix_.rows(),aTypeMatrix_.columns());
}

MSTypeMatrix<double> sin(const MSTypeMatrix<double>& aFloatMatrix_)
{return allElementsDo(aFloatMatrix_,::sin);}
MSTypeMatrix<double> sinh(const MSTypeMatrix<double>& aFloatMatrix_)
{return allElementsDo(aFloatMatrix_,::sinh);}
MSTypeMatrix<double> cos(const MSTypeMatrix<double>& aFloatMatrix_)
{return allElementsDo(aFloatMatrix_,::cos);}
MSTypeMatrix<double> cosh(const MSTypeMatrix<double>& aFloatMatrix_)
{return allElementsDo(aFloatMatrix_,::cosh);}
MSTypeMatrix<double> tan(const MSTypeMatrix<double>& aFloatMatrix_)
{return allElementsDo(aFloatMatrix_,::tan);}
MSTypeMatrix<double> tanh(const MSTypeMatrix<double>& aFloatMatrix_)
{return allElementsDo(aFloatMatrix_,::tanh);}
MSTypeMatrix<double> asin(const MSTypeMatrix<double>& aFloatMatrix_)
{return allElementsDo(aFloatMatrix_,::asin);}
MSTypeMatrix<double> acos(const MSTypeMatrix<double>& aFloatMatrix_)
{return allElementsDo(aFloatMatrix_,::acos);}
MSTypeMatrix<double> atan(const MSTypeMatrix<double>& aFloatMatrix_)
{return allElementsDo(aFloatMatrix_,::atan);}
MSTypeMatrix<double> exp(const MSTypeMatrix<double>& aFloatMatrix_)
{return allElementsDo(aFloatMatrix_,::exp);}
MSTypeMatrix<double> log(const MSTypeMatrix<double>& aFloatMatrix_)
{return allElementsDo(aFloatMatrix_,::log);}
MSTypeMatrix<double> sqrt(const MSTypeMatrix<double>& aFloatMatrix_)
{return allElementsDo(aFloatMatrix_,::sqrt);}
MSTypeMatrix<double> ceil(const MSTypeMatrix<double>& aFloatMatrix_)
{return allElementsDo(aFloatMatrix_,::ceil);}
MSTypeMatrix<double> floor(const MSTypeMatrix<double>& aFloatMatrix_)
{return allElementsDo(aFloatMatrix_,::floor);}
MSTypeMatrix<double> abs(const MSTypeMatrix<double>& aFloatMatrix_)
{return allElementsDo(aFloatMatrix_,::fabs);}

MSTypeMatrix<double> pow(const MSTypeMatrix<double>& aFloatMatrix_,double exponent_)
{return allElementsDo(aFloatMatrix_,MSTypeMatrix<double>::_pow,(void *)&exponent_);}

// ElementWiseFunction that returns x**y
double MSTypeMatrix<double>::_pow(double x_,void *y_)
{return ::pow(x_,*(double *)y_);}
