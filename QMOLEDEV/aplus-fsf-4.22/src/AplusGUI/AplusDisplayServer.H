#ifndef AplusDisplayServerHEADER
#define AplusDisplayServerHEADER

///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved.
// See .../src/LICENSE for terms of distribution.
//
//
///////////////////////////////////////////////////////////////////////////////

#include <MSGUI/MSDisplayServer.H>
#include <a/fncdcls.h>

class AExitFunc
{
public:
  AExitFunc(A function_,A data_) { _function=(A)ic(function_);_data=(A)ic(data_); }
  ~AExitFunc(void) { dc(_function);dc(_data); }

  A function(void)   { return _function; }
  A data(void)       { return _data; }

private:
  A _function;
  A _data;
};


class AplusDisplayServer : public MSDisplayServer
{
public:
  AplusDisplayServer(void) : MSDisplayServer(), _exitFunc(0) {}
  AplusDisplayServer(const char *name_) : MSDisplayServer(name_), _exitFunc(0) {}
  ~AplusDisplayServer(void) { if (exitFunc()!=0) delete _exitFunc;}

  AExitFunc *exitFunc(void)   { return _exitFunc; }
  void exitFunc(AExitFunc *exitFunc_) 
   { if (exitFunc()!=0) delete _exitFunc; _exitFunc=exitFunc_; }

  // TED-related functions
  //
  Atom *workspaceAtoms(void) const;
  int workspaceNumber(const char *) const;

  int virtualScreen(Window);
  int virtualScreen(void);
  void virtualGeometry(int& x_,int& y_);

protected:
  AExitFunc *_exitFunc;
};

#endif // AplusDisplayServerHEADER
