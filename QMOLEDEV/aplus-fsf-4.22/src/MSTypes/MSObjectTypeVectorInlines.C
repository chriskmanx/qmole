#ifndef MSObjectTypeVectorINLINES
#define MSObjectTypeVectorINLINES

///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////


#ifndef MS_NO_INLINES
#include <MSTypes/MSBaseTypeVectorInlines.C>
#endif  // MS_NO_INLINES

template <class Type>
INLINELINKAGE Type & MSObjectVector<Type>::firstElement()
{
  return elementAt (0);
}


template <class Type>
INLINELINKAGE Type & MSObjectVector<Type>::lastElement()
{
  return elementAt (this->_pImpl->length() -1);
}


template <class Type>
INLINELINKAGE const Type & MSObjectVector<Type>::firstElement() const
{
  return elementAt (0);
}


template <class Type>
INLINELINKAGE const Type & MSObjectVector<Type>::lastElement() const
{
  return elementAt (this->_pImpl->length() -1);
}


template <class Type>
INLINELINKAGE const Type & MSObjectVector<Type>::operator() (unsigned int index_) const
{
  return elementAt (index_);
}


template <class Type>
INLINELINKAGE const Type & MSObjectVector<Type>::operator[] (unsigned int index_) const
{
  return elementAt (index_);
}


template <class Type>
INLINELINKAGE const Type & MSObjectVector<Type>::elementAt (unsigned int index_) const
{
#if !defined(MS_NO_INDEX_ERROR)
  if (index_ >= this->_pImpl->length())
    {
      this->_pImpl->vectorIndexError (index_);
      return *(const Type *)this->ops().badData();
    }
#endif  //MSPRODUCTION_BUILD
  return this->data()[index_];
}


template <class Type>
INLINELINKAGE Type & MSObjectVector<Type>::operator[] (unsigned int index_)
{
  return elementAt (index_);
}


template <class Type>
INLINELINKAGE MSObjectVector<Type> MSObjectVector<Type>::operator[] (const MSIndexVector & iVect_) const
{
  return this->select (*this, iVect_);
}


template <class Type>
INLINELINKAGE MSObjectVector<Type> MSObjectVector<Type>::operator[] (const MSBinaryVector & bVect_) const
{
  return compress (*this, bVect_);
}


template <class Type>
INLINELINKAGE MSVectorElement<Type>::MSVectorElement() : Type()
{
}


template <class Type>
INLINELINKAGE MSVectorElement<Type>::MSVectorElement (const MSVectorElement<Type> & value_)
  : Type(value_)
{
}


template <class Type>
INLINELINKAGE MSVectorElement<Type>::MSVectorElement (const Type & value_)
  : Type(value_)
{
}


template <class Type>
INLINELINKAGE void MSVectorElement<Type>::vector (MSObjectVector<Type> *pVect_)
{
  this->_pReceiverList = (MSEventSender::List *)pVect_;
}


template <class Type> INLINELINKAGE MSVectorModelAllocator<Type>::MSVectorModelAllocator(void) {}
template <class Type> INLINELINKAGE MSVectorModelAllocator<Type>::~MSVectorModelAllocator(void) {}

template <class Type>
INLINELINKAGE void MSVectorModelAllocator<Type>::construct(Type *p_, const Type& value_)
{
  new (p_) MSVectorElement<Type>(value_);
}

#endif // MSObjectTypeVectorINLINES
