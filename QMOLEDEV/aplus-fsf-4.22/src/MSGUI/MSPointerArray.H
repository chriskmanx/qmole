#ifndef MSPointerArrayHEADER
#define MSPointerArrayHEADER

///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////


#include <MSTypes/MSEnum.H>

#ifndef MSDefinesHEADER
#include <MSTypes/MSDefines.H>
#endif

#if defined(MS_NEED_IMPLEMENTATION_PRAGMA)
#pragma implementation("MSPointerArray.C")
#endif

template<class Type>
class MSPointerArray
{
private:
  Type      **_array;
  unsigned    _size;
  unsigned    _count;
  MSBoolean   _frozen;

protected:  
  void reserve(unsigned);
  int size(void)         const {return _size;}
  MSBoolean frozen(void) const {return _frozen;}
 
public:
  MSPointerArray(void);
  ~MSPointerArray(void);

  inline int count(void) const;
  inline Type *array(unsigned) const;
  inline Type *array(unsigned);
  void freeze(void)            {_frozen=MSTrue;}
  void unfreeze(void)          {_frozen=MSFalse;}
  MSBoolean assign(Type *,unsigned);
  MSBoolean find(Type *);
  MSBoolean add(Type *);
  MSBoolean remove(Type *);
  MSBoolean removeAll(void);
  MSBoolean insert(Type *,unsigned);  
  MSBoolean exchange(unsigned,unsigned);
};

template<class Type>
inline int MSPointerArray<Type>::count(void) const {return _count;}
template<class Type>
inline Type *MSPointerArray<Type>::array(unsigned i_) const
{return (i_<size())?_array[i_]:0;}
template<class Type>
inline Type *MSPointerArray<Type>::array(unsigned i_)
{return (i_<size())?_array[i_]:0;}
#endif




