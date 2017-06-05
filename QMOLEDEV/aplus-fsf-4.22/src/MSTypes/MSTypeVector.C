#ifndef MSTypeVectorIMPLEMENTATION
#define MSTypeVectorIMPLEMENTATION

///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////


#include <MSTypes/MSTypeVector.H>

#ifdef MS_NO_INLINES
#include <MSTypes/MSTypeVectorInlines.C>
#endif // MS_NO_INLINES

#include <MSTypes/MSObjectTypeVector.C>
#include <MSTypes/MSTypeData.C>

template <class Type>
MSTypeVector<Type>::MSTypeVector() : MSObjectVector<Type>()
{
}


template <class Type>
MSTypeVector<Type>::MSTypeVector (unsigned int length_) : MSObjectVector<Type> (length_)
{
}


template <class Type>
MSTypeVector<Type>::MSTypeVector (unsigned int length_, const Type & filler_)
  : MSObjectVector<Type> (length_, filler_)
{
}


template <class Type>
MSTypeVector<Type>::MSTypeVector (const MSTypeVector<Type> & vect_) : MSObjectVector<Type> (vect_)
{
}


template <class Type>
MSTypeVector<Type>::MSTypeVector (const MSBaseVector<Type,MSVectorModelAllocator<Type> > & vect_) : MSObjectVector<Type> (vect_)
{
}


template <class Type>
MSTypeVector<Type>::MSTypeVector (const char *pString_) : MSObjectVector<Type> (pString_)
{
}


template <class Type>
MSTypeVector<Type>::MSTypeVector (MSTypeData<Type,MSVectorModelAllocator<Type> > *pData_, unsigned int len_)
  : MSObjectVector<Type> (pData_, len_)
{
}


template <class Type>
MSTypeVector<Type>::MSTypeVector (const Type *pElements_, unsigned int len_)
  : MSObjectVector<Type> (pElements_, len_)
{
}


template <class Type>
MSTypeVector<Type>::~MSTypeVector()
{
}


template <class Type>
MSTypeVector<Type> & MSTypeVector<Type>::operator= (const MSTypeVector<Type> & vect_)
{
  return (MSTypeVector<Type> &) MSObjectVector<Type>::operator= (vect_);
}


template <class Type>
MSTypeVector<Type> & MSTypeVector<Type>::operator= (const MSBaseVector<Type,MSVectorModelAllocator<Type> > & vect_)
{
  return (*this = (MSTypeVector<Type> &)vect_);
}


template <class Type>
MSTypeVector<Type> & MSTypeVector<Type>::operator= (const Type & value_)
{
  return (MSTypeVector<Type> &) MSObjectVector<Type>::operator= (value_);
}


template <class Type>
MSTypeVector<Type> & MSTypeVector<Type>::operator= (const char *pString_)
{
  return (MSTypeVector<Type> &) MSObjectVector<Type>::operator= (pString_);
}


template <class Type>
MSString MSTypeVector<Type>::className() const
{
  return name();
}


template <class Type>
const MSSymbol & MSTypeVector<Type>::type() const
{
  return symbol();
}


template <class Type>
MSModel * MSTypeVector<Type>::clone() const
{
  return new MSTypeVector<Type> (*this);
}


template <class Type>
MSModel * MSTypeVector<Type>::create() const
{
  return new MSTypeVector<Type>;
}


template <class Type>
const MSSymbol & MSTypeVector<Type>::symbol()
{
  static MSSymbol sym = MSSymbol(name());
  return sym;
}

#endif  // MSTypeVectorIMPLEMENTATION
