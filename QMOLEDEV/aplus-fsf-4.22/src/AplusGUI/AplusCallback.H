#ifndef __AplusCallback_H
#define __AplusCallback_H

///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved.
// See .../src/LICENSE for terms of distribution.
//
//
///////////////////////////////////////////////////////////////////////////////

#include <MSTypes/MSCallback.H>
#include "AplusCommon.H"
typedef void (*PFV)(void*);

class AplusCallback : public MSCallback
{
public:

  AplusCallback(AClientData *);
  ~AplusCallback(void);

  AClientData *ac() { return _ac; }
  virtual void process(void);

protected:
  AClientData *_ac;

};

class AplusStdinCallback : public MSCallback
{
public:
  AplusStdinCallback(PFV func_) { _func = func_;}
  ~AplusStdinCallback(void) {}

protected:
  virtual void process(void) {if (_func!=(PFV)0) (*_func)(0);}

private:
  PFV _func;
};

#endif
