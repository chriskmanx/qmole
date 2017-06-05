#ifndef MSPointerHEADER
#define MSPointerHEADER

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
#pragma implementation("MSPointer.C")
#endif

#if HAVE_IOSTREAM
#include <iostream>
// #include <fstream>
using namespace std;
#else
class ostream;
#endif


#if defined(MS_PREDECLARE_TEMPLATE_FRIENDS) && !defined(MS_PREDECLARE_TEMPLATE_FRIENDS_BUG)
template <class Type> class MSPointer;
template <class Type> ostream& operator<<(ostream&,const MSPointer<Type>&);
#if !defined(MS_SPECIALIZE_ELEMENT_FOR_OPS)
template <class Type> inline Type& elementForOps(MSPointer<Type>&);
template <class Type> inline const Type& elementForOps(const MSPointer<Type>&);
#endif
#endif //MS_PREDECLARE_TEMPLATE_FRIENDS

template<class Type>
class MSPointer
{
public:
  MSPointer(void) : _pObject(0) {}
  MSPointer(Type* pObject_,MSExplicitInit=MSInit) : _pObject(pObject_) {}
#if !defined (MS_NO_EXPLICIT_KEYWORD)
  explicit MSPointer(Type* pObject_) : _pObject(pObject_) {}
#endif
  Type& operator*()  const { return *_pObject; }
  Type* operator->() const { return _pObject; }
  operator Type*()   const { return _pObject; }

  friend ostream& operator<<(ostream&,const MSPointer<Type>&);

#if !defined(MS_SPECIALIZE_ELEMENT_FOR_OPS)
  friend inline       Type & elementForOps (MSPointer<Type> & ptr_)        { return *ptr_; }
  friend inline const Type & elementForOps (const MSPointer<Type> & ptr_)  { return *ptr_; }
#endif

protected:
  Type *_pObject;
};


#endif
