#ifndef MSBuiltinSPickIMPLEMENTATION
#define MSBuiltinSPickIMPLEMENTATION

///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////


#include <MSTypes/MSBuiltinSPick.H>

#ifdef MS_NO_INLINES
#include <MSTypes/MSBuiltinSPickInlines.C>
#endif // MS_NO_INLINES

template <class Type>
MSBuiltinSPick<Type> & MSBuiltinSPick<Type>::operator= (const MSBuiltinSPick<Type> & sPick_)
{
  _pVector->set (_index, (*sPick_._pVector)(sPick_._index));
  return *this;
}


template <class Type>
MSBuiltinSPick<Type> & MSBuiltinSPick<Type>::operator= (const Type & aScalar_)
{
  _pVector->set (_index, aScalar_);
  return *this;
}

#endif  //MSBuiltinSPickIMPLEMENTATION
