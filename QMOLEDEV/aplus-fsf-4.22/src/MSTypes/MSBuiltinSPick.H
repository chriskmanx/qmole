#ifndef MSBuiltinSPickHEADER
#define MSBuiltinSPickHEADER

///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////


#include <MSTypes/MSDefines.H>

template <class Type> class MSBuiltinVector;

template <class Type>
class MSBuiltinSPick
{
protected:
  unsigned int _index; 
  MSBuiltinVector<Type> *_pVector;

public:
  INLINELINKAGE MSBuiltinSPick (MSBuiltinVector<Type> &, unsigned int);  
  INLINELINKAGE MSBuiltinSPick (const MSBuiltinSPick<Type> &);
  
  MSBuiltinSPick<Type> & operator= (const MSBuiltinSPick<Type> &);
  MSBuiltinSPick<Type> & operator= (const Type &);

  INLINELINKAGE MSBuiltinSPick<Type> & operator++();        // prefix increment
  INLINELINKAGE MSBuiltinSPick<Type> & operator--();        // prefix decrement
  INLINELINKAGE Type operator++ (int);    // postfix increment
  INLINELINKAGE Type operator-- (int);	// postfix decrement
  
  INLINELINKAGE MSBuiltinSPick<Type> & operator+=  (const Type &);
  INLINELINKAGE MSBuiltinSPick<Type> & operator-=  (const Type &);
  INLINELINKAGE MSBuiltinSPick<Type> & operator*=  (const Type &);
  INLINELINKAGE MSBuiltinSPick<Type> & operator/=  (const Type &);

  // these operators are for integral types only
  INLINELINKAGE MSBuiltinSPick<Type> & operator%=  (const Type &);
  INLINELINKAGE MSBuiltinSPick<Type> & operator<<= (const Type &);
  INLINELINKAGE MSBuiltinSPick<Type> & operator>>= (const Type &);
  INLINELINKAGE MSBuiltinSPick<Type> & operator&=  (const Type &);
  INLINELINKAGE MSBuiltinSPick<Type> & operator|=  (const Type &);
  INLINELINKAGE MSBuiltinSPick<Type> & operator^=  (const Type &);

  INLINELINKAGE operator Type() const;
};

#ifndef MS_NO_INLINES
#include <MSTypes/MSBuiltinSPickInlines.C>
#endif  // MS_NO_INLINES

#endif  // MSBuiltinSPickHEADER
