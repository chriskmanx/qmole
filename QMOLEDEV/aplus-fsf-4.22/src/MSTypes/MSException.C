///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////

#include <MSTypes/MSException.H>
#include <string.h>
//
// MSException 
//

MSException::~MSException()
{}


const char* MSException::what() const
{ return ""; }

//
// MSException error
//

MSExceptionError& MSExceptionError::operator=(const MSExceptionError& anError_)
{
  if(this!=&anError_)
   {
     if(_pMessage!=0)
      {
        delete [] _pMessage;
        _pMessage=0;
      }
     init(anError_._pMessage);
   }
  return *this;
}

MSExceptionError::~MSExceptionError()
{
  if(_pMessage) delete [] _pMessage;
}


void MSExceptionError::init(const char* pMessage_)
{
   if(pMessage_ != 0)
   {
     _pMessage=new char[strlen(pMessage_)+1];
     strcpy(_pMessage,pMessage_);     
   }
}

const char* MSExceptionError::what() const
{
  return _pMessage;
}

// MStk exceptions

MSOutOfMemory::MSOutOfMemory(const char * what_)
    : MSExceptionError(what_)
{}

MSOutOfMemory::~MSOutOfMemory()
{}

MSCollectionError::MSCollectionError(const char * what_)
    : MSExceptionError(what_)
{}

MSCollectionError::~MSCollectionError()
{}
