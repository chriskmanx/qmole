#ifndef MSAutoPointerHEADER
#define MSAutoPointerHEADER

///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////


#ifndef MSDefinesHEADER
#include <MSTypes/MSDefines.H>
#endif

#include <MSTypes/MSEnum.H>

#if defined(MS_NEED_IMPLEMENTATION_PRAGMA)
#pragma implementation("MSAutoPointer.C")
#endif

#if HAVE_IOSTREAM
#include <iostream>
#else
class ostream;
using namespace std;
#endif

#if defined(MS_PREDECLARE_TEMPLATE_FRIENDS) && !defined(MS_PREDECLARE_TEMPLATE_FRIENDS_BUG)
template <class Type> class MSAutoPointer;
template <class Type> ostream& operator<<(ostream&,const MSAutoPointer<Type>&);
#if !defined(MS_SPECIALIZE_ELEMENT_FOR_OPS)
template <class Type> inline Type& elementForOps(MSAutoPointer<Type>&);
template <class Type> inline const Type& elementForOps(const MSAutoPointer<Type>&);
#endif //MS_SPECIALIZE_ELEMENT_FOR_OPS
#endif //MS_PREDECLARE_TEMPLATE_FRIENDS

template<class Type>
class MSAutoPointer
{
public:
  MSAutoPointer(Type *pObject_,MSExplicitInit) : _pObject(pObject_) {}
  MSAutoPointer(void) : _pObject(0) {}
#if !defined (MS_NO_EXPLICIT_KEYWORD)
  explicit MSAutoPointer(Type *pObject_) : _pObject(pObject_) {}
#endif
  MSAutoPointer(const MSAutoPointer<Type>&);

  ~MSAutoPointer(void) { delete _pObject; }

  void operator=(const MSAutoPointer<Type>&);

  Type& operator*()  const { return *_pObject; }
  Type* operator->() const { return _pObject;  }
  operator Type*()   const { return _pObject;  }
  inline Type* release ();         
  inline void reset(Type* pType_); 
  
  friend ostream& operator<<(ostream&,const MSAutoPointer<Type>&);
#if !defined(MS_SPECIALIZE_ELEMENT_FOR_OPS)
  friend inline       Type & elementForOps (MSAutoPointer<Type> & ptr_)        { return *ptr_; }
  friend inline const Type & elementForOps (const MSAutoPointer<Type> & ptr_)  { return *ptr_; }
#endif

protected:
  Type *_pObject;
};

template<class Type>
inline MSAutoPointer<Type>::MSAutoPointer(const MSAutoPointer<Type>& aAutoPointer_)
{
  _pObject=aAutoPointer_._pObject;
  ((MSAutoPointer<Type>&)aAutoPointer_)._pObject=0;
}

template<class Type>
inline void MSAutoPointer<Type>::operator=(const MSAutoPointer<Type>& aAutoPointer_)
{
  delete _pObject;
  _pObject=aAutoPointer_._pObject;
  ((MSAutoPointer<Type>&) aAutoPointer_)._pObject=0;
}

template<class Type>
inline Type* MSAutoPointer<Type>::release()
{
  Type* tmp=_pObject;_pObject=0;return tmp;
}

template<class Type>
inline void MSAutoPointer<Type>::reset(Type* aType_)
{
  if(_pObject) delete _pObject;
  _pObject=aType_;
}


#endif

