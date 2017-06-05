#ifndef MSStringBufferINLINES
#define MSStringBufferINLINES

///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////

extern "C"
{
#include <stddef.h>
}

#ifndef MS_NO_INLINES
#define INLINELINKAGE inline
#else
#define INLINELINKAGE
#endif

/*---------------------------- Overflow Checking -----------------------------*/
INLINELINKAGE unsigned MSStringBuffer::checkAddition(unsigned addend1,unsigned addend2)
{ return(addend1<UINT_MAX-addend2)?addend1+addend2:overflow(); }
INLINELINKAGE unsigned MSStringBuffer::checkMultiplication(unsigned factor1,unsigned factor2)
{ return(factor1<UINT_MAX/factor2)?factor1*factor2:overflow(); }

/*--------------------------------- Private ----------------------------------*/
INLINELINKAGE void *MSStringBuffer::operator new(size_t,unsigned realSize)
{
  void *p=::new char[checkAddition(sizeof(MSStringBuffer),realSize)];return p;
}

/*---------------------------- Reference Counting ----------------------------*/
INLINELINKAGE MSStringBuffer *MSStringBuffer::addRef()
{
#ifdef MS_MULTI_THREAD
  _mutex.acquire();
#endif  
  refs++;
#ifdef MS_MULTI_THREAD
  _mutex.release();
#endif  
  return this;
}

INLINELINKAGE void MSStringBuffer::removeRef()
{
#ifdef MS_MULTI_THREAD
  _mutex.acquire();
  if (--refs==0) 
    { 
      _mutex.release();
      delete this; 
    }
  else
    {
      _mutex.release();
    }
#else
  if(--refs==0) delete this;
#endif  
}

/*-------------------------------- Accessors ---------------------------------*/
INLINELINKAGE unsigned MSStringBuffer::useCount() const
{ return refs;}
INLINELINKAGE unsigned MSStringBuffer::length() const
{ return len;}
INLINELINKAGE const char *MSStringBuffer::contents() const
{ return data;}
INLINELINKAGE char *MSStringBuffer::contents()
{ return data;}
#if 0
INLINELINKAGE MSStringBuffer *MSStringBuffer::fromContents(const char *p)
{ return(MSStringBuffer*)(p-offsetof(MSStringBuffer,data)); }
#endif

#endif



