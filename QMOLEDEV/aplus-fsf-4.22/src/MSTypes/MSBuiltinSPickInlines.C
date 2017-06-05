#ifndef MSBuiltinSPickINLINES
#define MSBuiltinSPickINLINES

///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////


#include <MSTypes/MSBuiltinTypeVector.H>


template <class Type>
INLINELINKAGE MSBuiltinSPick<Type>::MSBuiltinSPick (MSBuiltinVector<Type> & aVector_, unsigned int index_)
  : _pVector(&aVector_), _index(index_)
{ 
}


template <class Type>
INLINELINKAGE MSBuiltinSPick<Type>::MSBuiltinSPick (const MSBuiltinSPick<Type> & aPick_)
  : _pVector(aPick_._pVector), _index(aPick_._index)
{
}

// prefix increment
template <class Type>
INLINELINKAGE MSBuiltinSPick<Type> & MSBuiltinSPick<Type>::operator++()
{
  _pVector->set (_index, (*_pVector)(_index) +1);
  return *this;
}


// prefix decrement
template <class Type>
INLINELINKAGE MSBuiltinSPick<Type> & MSBuiltinSPick<Type>::operator--()
{
  _pVector->set (_index, (*_pVector)(_index) -1);
  return *this;
}


// postfix increment
template <class Type>
INLINELINKAGE Type MSBuiltinSPick<Type>::operator++ (int)
{
  Type temp = (*_pVector)(_index);
  _pVector->set (_index, temp+1);
  return temp;
}


template <class Type>
INLINELINKAGE Type MSBuiltinSPick<Type>::operator-- (int)
{
  Type temp = (*_pVector)(_index);
  _pVector->set (_index, temp-1);
  return temp;
}


template <class Type>
INLINELINKAGE MSBuiltinSPick<Type> & MSBuiltinSPick<Type>::operator+=  (const Type & value_)
{
  _pVector->set (_index, (*_pVector)(_index) + value_);
  return *this;
}


template <class Type>
INLINELINKAGE MSBuiltinSPick<Type> & MSBuiltinSPick<Type>::operator-=  (const Type & value_)
{
  _pVector->set (_index, (*_pVector)(_index) - value_);
  return *this;
}


template <class Type>
INLINELINKAGE MSBuiltinSPick<Type> & MSBuiltinSPick<Type>::operator*=  (const Type & value_)
{
  _pVector->set (_index, (*_pVector)(_index) * value_);
  return *this;
}


template <class Type>
INLINELINKAGE MSBuiltinSPick<Type> & MSBuiltinSPick<Type>::operator/=  (const Type & value_)
{
  _pVector->set (_index, (*_pVector)(_index) / value_);
  return *this;
}


template <class Type>
INLINELINKAGE MSBuiltinSPick<Type>::operator Type() const
{
  return (*_pVector)(_index);
}


template <class Type>
INLINELINKAGE MSBuiltinSPick<Type> & MSBuiltinSPick<Type>::operator%=  (const Type & value_)
{
  _pVector->set (_index, (*_pVector)(_index) % value_);
  return *this;
}


template <class Type>
INLINELINKAGE MSBuiltinSPick<Type> & MSBuiltinSPick<Type>::operator<<= (const Type & value_)
{
  _pVector->set (_index, (*_pVector)(_index) << value_);
  return *this;
}


template <class Type>
INLINELINKAGE MSBuiltinSPick<Type> & MSBuiltinSPick<Type>::operator>>= (const Type & value_)
{
  _pVector->set (_index, (*_pVector)(_index) >> value_);
  return *this;
}


template <class Type>
INLINELINKAGE MSBuiltinSPick<Type> & MSBuiltinSPick<Type>::operator&=  (const Type & value_)
{
  _pVector->set (_index, (*_pVector)(_index) & value_);
  return *this;
}


template <class Type>
INLINELINKAGE MSBuiltinSPick<Type> & MSBuiltinSPick<Type>::operator|=  (const Type & value_)
{
  _pVector->set (_index, (*_pVector)(_index) | value_);
  return *this;
}


template <class Type>
INLINELINKAGE MSBuiltinSPick<Type> & MSBuiltinSPick<Type>::operator^=  (const Type & value_)
{
  _pVector->set (_index, (*_pVector)(_index) ^ value_);
  return *this;
}

#endif  // MSBuiltinSPickINLINES
