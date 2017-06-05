///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////

#include <MSTypes/MSFloatSPick.H>

#ifdef MS_NO_INLINES
#include <MSTypes/MSFloatSPickInlines.C>
#endif // MS_NO_INLINES

MSBuiltinSPick<double> & MSBuiltinSPick<double>::operator= (const MSBuiltinSPick<double> & sPick_)
{
  _pVector->set (_index, (*sPick_._pVector)(sPick_._index));
  return *this;
}


MSBuiltinSPick<double> & MSBuiltinSPick<double>::operator= (const double & aScalar_)
{
  _pVector->set (_index, aScalar_);
  return *this;
}
