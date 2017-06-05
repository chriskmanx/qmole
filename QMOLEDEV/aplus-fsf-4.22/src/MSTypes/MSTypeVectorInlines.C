#ifndef MSTypeVectorINLINES
#define MSTypeVectorINLINES

///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////


template <class Type>
INLINELINKAGE MSString MSTypeVector<Type>::name()
{
  Type aType;
//  return MSString("MSTypeVector<") + aType.className() + ">";
    return MSString("MSTypeVector<") + ::className(aType) + ">";
}


template <class Type>
INLINELINKAGE MSString className (const Type & t)
{
  return t.className();
}

#endif  // MSTypeVectorINLINES
