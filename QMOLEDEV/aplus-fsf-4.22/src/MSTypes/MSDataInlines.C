#ifndef MSDataINLINES
#define MSDataINLINES

///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////


INLINELINKAGE unsigned MSData::size() const
{
  return _size;
}


INLINELINKAGE unsigned MSData::refCount () const
{
  return _refCount;
}

#endif  // MSDataINLINES
