#ifndef MSFloatSPickINLINES
#define MSFloatSPickINLINES

///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////


#include <MSTypes/MSBuiltinTypeVector.H>

#ifndef MS_NO_INLINES
#include <MSTypes/MSBuiltinTypeVectorInlines.C>
#endif

INLINELINKAGE MSBuiltinSPick<double>::MSBuiltinSPick (MSBuiltinVector<double> & aVector_, unsigned int index_)
  : _pVector(&aVector_), _index(index_)
{ 
}


INLINELINKAGE MSBuiltinSPick<double>::MSBuiltinSPick (const MSBuiltinSPick<double> & aPick_)
  : _pVector(aPick_._pVector), _index(aPick_._index)
{
}


// prefix increment
INLINELINKAGE MSBuiltinSPick<double> & MSBuiltinSPick<double>::operator++()
{
  _pVector->set (_index, (*_pVector)(_index) +1);
  return *this;
}


// prefix decrement
INLINELINKAGE MSBuiltinSPick<double> & MSBuiltinSPick<double>::operator--()
{
  _pVector->set (_index, (*_pVector)(_index) -1);
  return *this;
}


// postfix increment
INLINELINKAGE double MSBuiltinSPick<double>::operator++ (int)
{
  double temp = (*_pVector)(_index);
  _pVector->set (_index, temp+1);
  return temp;
}


INLINELINKAGE double MSBuiltinSPick<double>::operator-- (int)
{
  double temp = (*_pVector)(_index);
  _pVector->set (_index, temp-1);
  return temp;
}


INLINELINKAGE MSBuiltinSPick<double> & MSBuiltinSPick<double>::operator+=  (const double & value_)
{
  _pVector->set (_index, (*_pVector)(_index) + value_);
  return *this;
}


INLINELINKAGE MSBuiltinSPick<double> & MSBuiltinSPick<double>::operator-=  (const double & value_)
{
  _pVector->set (_index, (*_pVector)(_index) - value_);
  return *this;
}


INLINELINKAGE MSBuiltinSPick<double> & MSBuiltinSPick<double>::operator*=  (const double & value_)
{
  _pVector->set (_index, (*_pVector)(_index) * value_);
  return *this;
}


INLINELINKAGE MSBuiltinSPick<double> & MSBuiltinSPick<double>::operator/=  (const double & value_)
{
  _pVector->set (_index, (*_pVector)(_index) / value_);
  return *this;
}


INLINELINKAGE MSBuiltinSPick<double>::operator double() const
{
  return (*_pVector)(_index);
}

#endif  // MSFloatSPickINLINES
