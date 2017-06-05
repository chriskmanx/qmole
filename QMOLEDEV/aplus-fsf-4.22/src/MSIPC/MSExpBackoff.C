///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////

#include <MSIPC/MSExpBackoff.H>

MSExpBackoff::MSExpBackoff(int first_,int last_)
{
  if (first_<0)
   {
     _negative=1;
     first_*=-1;
     last_*=-1;
   }
  else _negative=0;
  if (last_<0) last_=0;

  _first=(unsigned)first_;
  _current=(unsigned)first_;
  _last=(unsigned)last_;
}

MSExpBackoff::~MSExpBackoff(void) {}

int MSExpBackoff::backoff(void)
{
  if (_current<_last)
   {
     _current*=2;
     if (_current>_last) _current=_last;
   }
  else if (_current>_last)
   {
     _current/=2;
     if (_current<_last) _current=_last;
   }
  return (_negative?-1*(int)(_current):(int)(_current));
}













