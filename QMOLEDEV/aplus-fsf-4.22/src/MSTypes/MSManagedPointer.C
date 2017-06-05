#ifndef MSManagedPointerIMPLEMENTATION
#define MSManagedPointerIMPLEMENTATION

///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////


#if HAVE_IOSTREAM
#include <iostream>
#else
#include <iostream.h>
#endif

template<class Type>
ostream& operator<<(ostream& aStream_,const MSManagedPointer<Type>& aPointer_)
{ 
  aStream_<<(void *)aPointer_._node->_pObject<<"  ";
  return aStream_<<"Ref Count: "<<aPointer_._node->_refCount<<flush;
}

#endif
