#ifndef MSObjectTypeVectorIMPLEMENTATION
#define MSObjectTypeVectorIMPLEMENTATION

///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////


#include <MSTypes/MSObjectTypeVector.H>

#ifdef MS_NO_INLINES
#include <MSTypes/MSObjectTypeVectorInlines.C>
#endif // MS_NO_INLINES

#include <MSTypes/MSBaseTypeVector.C>

template <class Type>
MSObjectVector<Type>::MSObjectVector() : MSBaseVector<Type,MSVectorModelAllocator<Type> >()
{
}


template <class Type>
MSObjectVector<Type>::MSObjectVector (unsigned int length_)
#if !defined(MS_TEMPLATE_CONSTRUCTOR_OVERLOAD_BUG)
: MSBaseVector<Type,MSVectorModelAllocator<Type> > (length_)
#else  // Visual C++ overloading resolution bug
: MSBaseVector<Type,MSVectorModelAllocator<Type> > (length_, 0, 0)
#endif  //MS_TEMPLATE_CONSTRUCTOR_OVERLOAD_BUG
{
}


template <class Type>
MSObjectVector<Type>::MSObjectVector (unsigned int length_, const Type & filler_)
  : MSBaseVector<Type,MSVectorModelAllocator<Type> > (length_, filler_)
{
}


template <class Type>
MSObjectVector<Type>::MSObjectVector (const MSObjectVector<Type> & vect_) : MSBaseVector<Type,MSVectorModelAllocator<Type> > (vect_)
{
}


template <class Type>
MSObjectVector<Type>::MSObjectVector (const MSBaseVector<Type,MSVectorModelAllocator<Type> > & vect_) : MSBaseVector<Type,MSVectorModelAllocator<Type> > (vect_)
{
}


template <class Type>
MSObjectVector<Type>::MSObjectVector (const char *pString_, const char delimiter_) : MSBaseVector<Type,MSVectorModelAllocator<Type> > (pString_, delimiter_)
{
}


template <class Type>
MSObjectVector<Type>::MSObjectVector (MSTypeData<Type,MSVectorModelAllocator<Type> > *pData_, unsigned int len_)
  : MSBaseVector<Type,MSVectorModelAllocator<Type> > (pData_, len_)
{
}


template <class Type>
MSObjectVector<Type>::MSObjectVector (const Type *pElements_, unsigned int len_)
  : MSBaseVector<Type,MSVectorModelAllocator<Type> > (pElements_, len_)
{
}


template <class Type>
MSObjectVector<Type>::~MSObjectVector()
{
}


template <class Type>
MSObjectVector<Type> & MSObjectVector<Type>::operator= (const MSObjectVector<Type> & vect_)
{
  return (MSObjectVector<Type> &) MSBaseVector<Type,MSVectorModelAllocator<Type> >::operator= (vect_);
}


template <class Type>
MSObjectVector<Type> & MSObjectVector<Type>::operator= (const MSBaseVector<Type,MSVectorModelAllocator<Type> > & vect_)
{
  return (*this = (MSObjectVector<Type> &)vect_);
}


template <class Type>
MSObjectVector<Type> & MSObjectVector<Type>::operator= (const Type & value_)
{
  return (MSObjectVector<Type> &) MSBaseVector<Type,MSVectorModelAllocator<Type> >::operator= (value_);
}


template <class Type>
MSObjectVector<Type> & MSObjectVector<Type>::operator= (const char *pString_)
{
  this->set (pString_);
  return *this;
}


template <class Type>
Type & MSObjectVector<Type>::elementAt (unsigned int index_)
{
#if !defined(MS_NO_INDEX_ERROR)
  if (index_ >= this->_pImpl->length())
    {
      this->_pImpl->vectorIndexError (index_);
      return *(MSVectorElement<Type> *)this->ops().badData();
    }
#endif  //MSPRODUCTION_BUILD
  if (this->vectorData()->refCount() > 1)
    this->_pImpl->makeUniqueCopy();
  
  MSVectorElement<Type> &element = (MSVectorElement<Type> &)this->vectorData()->elements()[index_];  
  if (this->doChanged()==MSTrue)
    element.vector (this);

  return element;
}


template <class Type>
MSVectorElement<Type>::~MSVectorElement()
{
  vector (0);
}


template <class Type>
void MSVectorElement<Type>::sendEvent (MSEvent &)
{
  MSObjectVector<Type> *pVect = (MSObjectVector<Type> *)this->_pReceiverList;  
  // ASSERTION:  pVect != 0
  if (pVect->_blocked == MSTrue)
    vector (0);
  else	// pVect->_blocked == MSFalse
    {
      unsigned int index = ((unsigned long)this - (unsigned long)pVect->data()) / sizeof(Type);
      pVect->changed (index);
    }
}

#endif  // MSObjectTypeVectorIMPLEMENTATION
