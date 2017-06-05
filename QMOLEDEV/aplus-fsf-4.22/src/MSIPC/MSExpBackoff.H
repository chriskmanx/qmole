#ifndef MSExpBackoffHEADER
#define MSExpBackoffHEADER

///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////

#include <MSTypes/MSDefines.H>

class MSIPCExport MSExpBackoff
{
protected:
  unsigned _negative;
  unsigned _first;
  unsigned _current;
  unsigned _last;

public:
  MSExpBackoff(int first_=0,int last_=0);
  virtual ~MSExpBackoff(void);
  
  unsigned negative(void) const { return _negative; }
  unsigned first(void)    const { return _first; }
  unsigned current(void)  const { return _current; }
  unsigned last(void)     const { return _last; }

  virtual int backoff(void);
  int value(void) { return (_negative?-1*(int)(_current):(int)(_current)); }
  int reset(void)
   { _current=_first; return (_negative?-1*(int)(_current):(int)(_current)); }
};

#endif 

