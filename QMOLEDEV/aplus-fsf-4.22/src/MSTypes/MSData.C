///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////

#include <MSTypes/MSData.H>

#ifdef MS_NO_INLINES
#include <MSTypes/MSDataInlines.C>
#endif // MS_NO_INLINES

MSData::MSData (unsigned int size_) : _size(size_), _refCount(1)
{
}


MSData::~MSData()
{
}


unsigned int MSData::computeSize (unsigned int len_)
{
  if (len_ > 0)
    {
      unsigned l=len_>>1,p=1;
      for (;l!=0;l>>=1,p++);
      return (1<<p);
    }
  else
    return 0;
} 
