#ifndef MSManagedPointerHEADER
#define MSManagedPointerHEADER

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

#ifdef MS_MULTI_THREAD
#include <MSTypes/MSMutex.H>
#endif

#include <MSTypes/MSEnum.H>

#if defined(MS_NEED_IMPLEMENTATION_PRAGMA)
#pragma implementation("MSManagedPointer.C")
#endif

#if HAVE_IOSTREAM
#include <iostream>
// #include <fstream>
using namespace std;
#else
class ostream;
#endif


#if defined(MS_PREDECLARE_TEMPLATE_FRIENDS) && !defined(MS_PREDECLARE_TEMPLATE_FRIENDS_BUG)
template <class Type> class MSManagedPointer;
template <class Type> ostream& operator<<(ostream&,const MSManagedPointer<Type>&);
#if !defined(MS_SPECIALIZE_ELEMENT_FOR_OPS)
template <class Type> inline Type& elementForOps (MSManagedPointer<Type>&);
template <class Type> inline const Type& elementForOps(const MSManagedPointer<Type>&);
#endif //MS_SPECIALIZE_ELEMENT_FOR_OPS
#endif //MS_PREDECLARE_TEMPLATE_FRIENDS

template<class Type>
class MSManagedPointer
{
protected:
  struct Node
  {
    Type     *_pObject;
    unsigned  _refCount;

    Node(Type *pType_) : _pObject(pType_),_refCount(1) {}
    ~Node(void) { delete _pObject; }
#ifdef MS_MULTI_THREAD    
    MSMutex   _mutex;      
    void addRef()     { _mutex.acquire();  _refCount++; _mutex.release(); }
    unsigned remRef() { MSGuard g(_mutex); _refCount--; return _refCount; }  
#endif
  } *_node;

public:
  MSManagedPointer(void) : _node(0) {}
#if !defined (MS_NO_EXPLICIT_KEYWORD)
  explicit MSManagedPointer(Type *pType_) : _node(new Node(pType_)) {}
#endif
  MSManagedPointer(Type *pType_,MSExplicitInit) : _node(new Node(pType_)) {}
  MSManagedPointer(const MSManagedPointer<Type>& aType_)
  {
    _node=aType_._node;
#ifdef MS_MULTI_THREAD
       if (_node!=0) _node->addRef();
#else
       if (_node!=0) _node->_refCount++;
#endif
  }
  
  ~MSManagedPointer(void)
  {
#ifdef MS_MULTI_THREAD
    if (_node!=0&&_node->remRef()==0) delete _node;
#else
    if (_node!=0&&--_node->_refCount==0) delete _node;
#endif
  }

  MSManagedPointer<Type>& operator=(const MSManagedPointer<Type>&);

  Type& operator*()  const { return *_node->_pObject; }
  Type* operator->() const { return _node->_pObject; }
  operator Type*()   const { if (_node==0) return 0; else return _node->_pObject; }
  inline void  reset(Type* pType_=0);
  inline Type* release();
  inline void  unset();
  
  friend ostream& operator<< MS_FRIEND_TEMPLATEID(ostream&,const MSManagedPointer<Type>&);

#if !defined(MS_SPECIALIZE_ELEMENT_FOR_OPS)
  friend inline       Type & elementForOps (MSManagedPointer<Type> & ptr_)       { return *(Type*)ptr_; }
  friend inline const Type & elementForOps (const MSManagedPointer<Type> & ptr_) { return *(Type*)ptr_; }
#endif

};

template<class Type>
inline MSManagedPointer<Type>& MSManagedPointer<Type>::operator=(const MSManagedPointer<Type>& aType_)
{
  if (_node!=aType_._node)
   {
#ifdef MS_MULTI_THREAD
     if (_node&&_node->remRef()==0) delete _node;
#else
     if (_node&&--_node->_refCount==0) delete _node;
#endif
     _node=aType_._node;
#ifdef MS_MULTI_THREAD
     if (_node!=0) _node->addRef();
#else
     if (_node!=0) _node->_refCount++;
#endif
   }
  return *this;
}

template<class Type>
inline void MSManagedPointer<Type>::reset(Type* pType_)
{
  if ( !_node || pType_!=_node->_pObject )
   {
#ifdef MS_MULTI_THREAD
     if (_node&&_node->remRef()==0) delete _node;
#else
     if (_node&&--_node->_refCount==0) delete _node;
#endif
     _node=new Node(pType_);
   }
}

template<class Type>
inline Type* MSManagedPointer<Type>::release()
{
  // if this managed pointer is the only reference to the underlying object, release it
  // otherwise ignore.
  if (_node&&_node->_refCount==1)
   {
     Type* tmp=_node->_pObject;
     _node->_pObject=0;
     _node->_refCount=0;
     return tmp;
   }
  return 0;
}

template<class Type>
inline void MSManagedPointer<Type>::unset()
{
  if (_node)
   {
#ifdef MS_MULTI_THREAD
     if (_node->remRef()==0) delete _node;
#else
     if (--_node->_refCount==0) delete _node;
#endif
    _node=0;
   }
}



#endif
