#ifndef MSBuiltinTypeVectorIMPLEMENTATION
#define MSBuiltinTypeVectorIMPLEMENTATION

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
#include <string>
#else
#include <strstream.h>
#endif
#include <MSTypes/MSRandom.H>
#include <MSTypes/MSBuiltinTypeVector.H>

#ifdef MS_NO_INLINES
#include <MSTypes/MSBuiltinTypeVectorInlines.C>
#endif // MS_NO_INLINES

#include <MSTypes/MSBaseTypeVector.C>

#ifdef MS_MULTI_THREAD
template <class Type> MSMutex MSBuiltinVector<Type>::_operationsMutex;
#endif

template <class Type>
MSBuiltinVector<Type>::MSBuiltinVector() : MSBaseVector<Type,MSAllocator<Type> > ((MSVectorImpl *)0)
{
  this->_pImpl = new MSBuiltinVectorImpl(&ops(),&ops());
}


template <class Type>
MSBuiltinVector<Type>::MSBuiltinVector (unsigned int length_) : MSBaseVector<Type,MSAllocator<Type> > ((MSVectorImpl *)0)
{
  this->_pImpl = new MSBuiltinVectorImpl(&ops(),&ops(),length_);
}


template <class Type>
MSBuiltinVector<Type>::MSBuiltinVector (unsigned int length_, const Type & filler_)
  : MSBaseVector<Type,MSAllocator<Type> > ((MSVectorImpl *)0)
{
  this->_pImpl = new MSBuiltinVectorImpl(&ops(),&ops(),length_,(void *)&filler_);
}


template <class Type>
MSBuiltinVector<Type>::MSBuiltinVector (const MSBuiltinVector<Type> & vect_)
  : MSBaseVector<Type,MSAllocator<Type> > (new MSBuiltinVectorImpl (*(MSBuiltinVectorImpl *)vect_._pImpl))
{
}


template <class Type>
MSBuiltinVector<Type>::MSBuiltinVector (const MSBaseVector<Type,MSAllocator<Type> > & vect_)
  : MSBaseVector<Type,MSAllocator<Type> > (new MSBuiltinVectorImpl (*(MSBuiltinVectorImpl *)((MSBuiltinVector<Type> &)vect_)._pImpl))
{
}
  

template <class Type>
MSBuiltinVector<Type>::MSBuiltinVector (MSBuiltinVectorImpl *pImpl_) : MSBaseVector<Type,MSAllocator<Type> > (pImpl_)
{
}


template <class Type>
MSBuiltinVector<Type>::MSBuiltinVector (const char * pString_) : MSBaseVector<Type,MSAllocator<Type> > ((MSVectorImpl *)0)
{
  this->_pImpl = new MSBuiltinVectorImpl(&ops(),&ops());
  this->_pImpl->setFromString (pString_);
}


template <class Type>
MSBuiltinVector<Type>::MSBuiltinVector (MSTypeData<Type,MSAllocator<Type> > *pData_, unsigned int len_)
  : MSBaseVector<Type,MSAllocator<Type> > ((MSVectorImpl *)0)
{
  this->_pImpl = new MSBuiltinVectorImpl(&ops(),&ops(),pData_,len_);
}


template <class Type>
MSBuiltinVector<Type>::MSBuiltinVector (const Type *pElements_, unsigned int len_)
  : MSBaseVector<Type,MSAllocator<Type> > ((MSVectorImpl *)0)
{
  MSTypeData<Type,MSAllocator<Type> > *pData = MSTypeData<Type,MSAllocator<Type> >::allocateWithLength (len_);
  MSTypeData<Type,MSAllocator<Type> >::copy (pElements_, pData->elements(), len_);

  this->_pImpl = new MSBuiltinVectorImpl(&ops(),&ops(),pData,len_);
}


template <class Type>
MSBuiltinVector<Type>::~MSBuiltinVector()
{
}
  

template <class Type>
MSBuiltinVector<Type> & MSBuiltinVector<Type>::operator= (const MSBuiltinVector<Type> & vect_)
{
  return (MSBuiltinVector<Type> &) MSBaseVector<Type,MSAllocator<Type> >::operator= (vect_);
}


template <class Type>
MSBuiltinVector<Type> & MSBuiltinVector<Type>::operator= (const MSBaseVector<Type,MSAllocator<Type> > & vect_)
{
  return (*this = (MSBuiltinVector<Type> &)vect_);
}


template <class Type>
MSBuiltinVector<Type> & MSBuiltinVector<Type>::operator= (const Type & value_)
{
  return (MSBuiltinVector<Type> &) MSBaseVector<Type,MSAllocator<Type> >::operator= (value_);
}


template <class Type>
MSBuiltinVector<Type> & MSBuiltinVector<Type>::random (unsigned long limit_)
{
  unsigned int len=this->_pImpl->length();
  if (len>0)
    {
      this->_pImpl->prepareToChangeWithoutCopy();
      
      if (limit_==0)  limit_=len;

      MSRandom rand;
      Type *pData=this->data();

      while (len--)
	{
	  *pData++ = (Type)rand(limit_);
	}
  
      this->changed();
    }

  return *this;
}


template <class Type>
MSBuiltinVector<Type> & MSBuiltinVector<Type>::series (unsigned int length_, Type offset_)
{
  this->_pImpl->reallocateInPlace(length_);

  Type *pData=this->data();

  while (length_--)
    {
      *pData++ = offset_++;
    }
  
  this->changed();
  return *this;
}


template <class Type>
Type MSBuiltinVector<Type>::min() const
{
  unsigned int len=this->_pImpl->length();

  if (len==0)  return 0;

  Type *pData=this->data();
  Type min=pData[0];

  for (unsigned int i=1; i<len; i++)
    {
      if (pData[i]<min)
	{
	  min = pData[i];
	}
    }

  return min;
}


template <class Type>
Type MSBuiltinVector<Type>::max() const
{
  unsigned int len=this->_pImpl->length();

  if (len==0)  return 0;

  Type *pData=this->data();
  Type max=pData[0];

  for (unsigned int i=1; i<len; i++)
    {
      if (pData[i]>max)
	{
	  max = pData[i];
	}
    }

  return max;
}


template <class Type>
double MSBuiltinVector<Type>::sum() const
{
  double sum=0.0;
  unsigned int len=this->_pImpl->length();
  Type *pData=this->data();

  while (len--)
    {
      sum += (double)*pData++;
    }

  return sum;
}

// doMath functions

template <class Type>
void MSBuiltinVector<Type>::doMath(const Type& value_, MathOp op_)
{
  unsigned int len=this->_pImpl->length();
  if (len>0)
   {
     Type *pData=this->data();
     this->_pImpl->prepareToChangeWithoutCopy();
     if (pData==this->data())	// no reallocation was necessary
      {
        switch(op_)
         {
         case Plus:   while (len--) *pData++ += value_; break;
         case Minus:  while (len--) *pData++ -= value_; break;
         case Divide: while (len--) *pData++ /= value_; break;
         case Times:  while (len--) *pData++ *= value_; break;
         case Incr:   while (len--) ++(*pData++);       break;
         case Decr:   while (len--) --(*pData++);       break;
         default:                                       break;
         }
      }
     else	// if we had to reallocate
      {
        // ASSERTION:  pData is still valid
        Type *pNewData=this->data();
        switch(op_)
         {
         case Plus:   while (len--) *pNewData++ = *pData++ + value_;break;
         case Minus:  while (len--) *pNewData++ = *pData++ - value_;break;
         case Divide: while (len--) *pNewData++ = *pData++ / value_;break;
         case Times:  while (len--) *pNewData++ = *pData++ * value_;break;
         case Incr:   while (len--) *pNewData++ = *pData++ + 1;     break;
         case Decr:   while (len--) *pNewData++ = *pData++ - 1;     break;
         default:                                                   break;
         }
      }
     this->changed();
   }
}

template <class Type>
void MSBuiltinVector<Type>::doMath(const MSBuiltinVector<Type>& vect_, MathOp op_)
{
  unsigned int len=this->_pImpl->length();
  assert(len=vect_._pImpl->length());
  if (len>0)
   {
     Type *pData=this->data(), *pVectData=vect_.data();
     this->_pImpl->prepareToChangeWithoutCopy();
     if (pData==this->data())	// no reallocation was necessary
      {
        switch(op_)
         {
         case Plus:   while (len--) *pData++ += *pVectData++; break;
         case Minus:  while (len--) *pData++ -= *pVectData++; break;
         case Divide: while (len--) *pData++ /= *pVectData++; break;
         case Times:  while (len--) *pData++ *= *pVectData++; break;
         default:                                             break;
         }
      }
     else // if we had to reallocate
      {
         // ASSERTION:  pData is still valid
        Type *pNewData=this->data();
         switch(op_)
          {
          case Plus:   while (len--) *pNewData++ = *pData++ + *pVectData++; break;
          case Minus:  while (len--) *pNewData++ = *pData++ - *pVectData++; break;
          case Divide: while (len--) *pNewData++ = *pData++ / *pVectData++; break;
          case Times:  while (len--) *pNewData++ = *pData++ * *pVectData++; break;
          default:                                                          break;
          }
      }
     this->changed();
   }
}


template <class Type>
MSBuiltinVectorImpl* MSBuiltinVector<Type>::doMath(const MSBuiltinVector<Type>& vect1_,
                                                   const MSBuiltinVector<Type>& vect2_, MathOp op_)
{
  unsigned int len=vect1_._pImpl->length();
  assert (len==vect2_._pImpl->length());
  MSBuiltinVectorImpl *pResImpl = (MSBuiltinVectorImpl *)vect1_._pImpl->create(len, vect1_.vectorData()->size());
  Type *pData1=vect1_.data(), *pData2=vect2_.data(), *pResData=((MSTypeData<Type,MSAllocator<Type> > *)pResImpl->data())->elements();
  switch(op_)
   {
   case Plus:   while (len--) *pResData++ = *pData1++ + *pData2++;break;
   case Minus:  while (len--) *pResData++ = *pData1++ - *pData2++;break;
   case Divide: while (len--) *pResData++ = *pData1++ / *pData2++;break;
   case Times:  while (len--) *pResData++ = *pData1++ * *pData2++;break;
   default:                                                       break;
   }
  return pResImpl;
}

template <class Type>
MSBuiltinVectorImpl* MSBuiltinVector<Type>::doMath(const MSBuiltinVector<Type>& vect_,
                                                   const Type& value_, MathOp op_)
{
  unsigned int len=vect_._pImpl->length();
  MSBuiltinVectorImpl *pResImpl = (MSBuiltinVectorImpl *)vect_._pImpl->create(len,vect_.vectorData()->size());
  Type *pData=vect_.data(), *pResData=((MSTypeData<Type,MSAllocator<Type> > *)pResImpl->data())->elements();
  switch(op_)
   {
   case Plus:   while (len--) *pResData++ = *pData++ + value_;break;
   case Minus:  while (len--) *pResData++ = *pData++ - value_;break;
   case Divide: while (len--) *pResData++ = *pData++ / value_;break;
   case Times:  while (len--) *pResData++ = *pData++ * value_;break;
   case Unary:  while (len--) *pResData++ = -*pData++;        break;
   default:                                                   break;
   }
  return pResImpl;
}

template <class Type>
MSBuiltinVectorImpl* MSBuiltinVector<Type>::doMath(const Type& value_,
                                                   const MSBuiltinVector<Type>& vect_, MathOp op_)
{
  unsigned int len=vect_._pImpl->length();
  MSBuiltinVectorImpl *pResImpl = (MSBuiltinVectorImpl *)vect_._pImpl->create(len,vect_.vectorData()->size());
  Type *pData=vect_.data(), *pResData=((MSTypeData<Type,MSAllocator<Type> > *)pResImpl->data())->elements();
  switch(op_)
   {
   case Plus:   while (len--) *pResData++ = value_ + *pData++;break;
   case Minus:  while (len--) *pResData++ = value_ * *pData++;break;
   case Divide: while (len--) *pResData++ = value_ / *pData++;break;
   case Times:  while (len--) *pResData++ = value_ * *pData++;break;
   default:                                                   break;
   }
  return pResImpl;
}


template <class Type>
#if __GNUC__ < 3
MSBuiltinVector<Type>::Operations& MSBuiltinVector<Type>::ops(void)
#else
typename MSBuiltinVector<Type>::Operations& MSBuiltinVector<Type>::ops(void)
#endif
{
  MS_SAFE_STATIC_INIT(Operations,_operationsMutex);
}


template <class Type>
MSBuiltinVectorOps<Type>::MSBuiltinVectorOps()
  : MSBaseVectorOps<Type,MSAllocator<Type> >(), MSBuiltinVectorImplOps()
{
}


template <class Type>
MSBuiltinVectorOps<Type>::~MSBuiltinVectorOps()
{
}


template <class Type>
void MSBuiltinVectorOps<Type>::set (void *pElements_, unsigned int index_, const void *pValue_,
					     MSAllocationFlag) const
{
  ((MSTypeData<Type,MSAllocator<Type> > *)pElements_)->elements()[index_] = *(Type *)pValue_;
}


template <class Type>
void MSBuiltinVectorOps<Type>::set (void *pDest_, unsigned int destInd_,
					     const void *pSrc_, unsigned int srcInd_, MSAllocationFlag) const
{
  ((MSTypeData<Type,MSAllocator<Type> > *)pDest_)->elements()[destInd_] = ((MSTypeData<Type,MSAllocator<Type> > *)pSrc_)->elements()[srcInd_];
}


template <class Type>
void MSBuiltinVectorOps<Type>::setToNumber (void *pElements_, unsigned int index_, double number_) const
{
  ((MSTypeData<Type,MSAllocator<Type> > *)pElements_)->elements()[index_] = (Type)number_;  
}


template <class Type>
double MSBuiltinVectorOps<Type>::getAsNumber (const void *pElements_, unsigned int index_) const
{
  return (double)((MSTypeData<Type,MSAllocator<Type> > *)pElements_)->elements()[index_];
}

template <class Type>
#if HAVE_SSTREAM
void MSBuiltinVectorOps<Type>::readFromStream (void *pData_, unsigned int index_, istringstream & ist) const
#else
void MSBuiltinVectorOps<Type>::readFromStream (void *pData_, unsigned int index_, istrstream & ist) const
#endif
{
  ist >> ((MSTypeData<Type,MSAllocator<Type> > *)pData_)->elements()[index_];
}


template <class Type>
#if HAVE_SSTREAM
void MSBuiltinVectorOps<Type>::writeToStream (const void *pData_, unsigned int index_, ostringstream & ost) const
#else
void MSBuiltinVectorOps<Type>::writeToStream (const void *pData_, unsigned int index_, ostrstream & ost) const
#endif
{
  ost << ((MSTypeData<Type,MSAllocator<Type> > *)pData_)->elements()[index_];
}


template <class Type>
#if HAVE_SSTREAM
void MSBuiltinVectorOps<Type>::whitespace (istringstream & ist) const
#else
void MSBuiltinVectorOps<Type>::whitespace (istrstream & ist) const
#endif
{
  static Type t;
  ::whitespace (t, ist);
}


template <class Type>
unsigned int MSBuiltinVectorOps<Type>::stringLen (const char *pString_) const
{
  static Type t;
  return ::stringLen (t, pString_);
}


template <class Type>
void MSBuiltinVectorOps<Type>::print (const void *pData_, unsigned int index_, ostream & stream_) const
{
  ::msOutputItem( ((MSTypeData<Type,MSAllocator<Type> > *)pData_)->elements()[index_],stream_);
}


template <class Type>
void * MSBuiltinVectorOps<Type>::defaultFiller() const
{
  static Type filler =0;
  return (void *) &filler;
}


template <class Type>
#if HAVE_SSTREAM
void whitespace (const Type &, istringstream &)
#else
void whitespace (const Type &, istrstream &)
#endif
{
}

#endif  // MSBuiltinTypeVectorIMPLEMENTATION
