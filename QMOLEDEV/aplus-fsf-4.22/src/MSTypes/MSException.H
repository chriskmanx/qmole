#ifndef MSExceptionHEADER
#define MSExceptionHEADER

///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////


#include <MSTypes/MSDefines.H>

/****************************************************************************************/
// MStk exceptions defines.
//
/****************************************************************************************/

//
// Class MSException defines a standard interface for exception handling. 
//

class MSTypesExport MSException
{ 
public:
  MSException () {}
  MSException (const MSException&) {}
  MSException& operator= (const MSException&) { return *this; }
  virtual ~MSException ();

  virtual const char * what () const;
};

//
// MSExceptionError provides a string constructor that allows to define exception message
//

class MSTypesExport MSExceptionError : public MSException
{ 
public:
  MSExceptionError (const MSExceptionError& anError_) : _pMessage(0) { init(anError_._pMessage); }
  MSExceptionError (const char* pMessage_)            : _pMessage(0) { init(pMessage_);          }

  MSExceptionError& operator=(const MSExceptionError&);
  virtual ~MSExceptionError ();

  virtual const char * what () const;

protected:
  MSExceptionError(void) {}
  
private:
  void init(const char* pMessage_);
  char* _pMessage;
};

// A few standard mstk Errors.

class MSTypesExport MSOutOfMemory : public MSExceptionError
{
public:
  MSOutOfMemory(const char * what_);
  ~MSOutOfMemory(); 
};

class MSTypesExport MSCollectionError : public MSExceptionError
{
public:
  MSCollectionError(const char * what_);
  ~MSCollectionError(); 
};


#endif
