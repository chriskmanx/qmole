#ifndef MSBusyHEADER
#define MSBusyHEADER

///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////

#include <MSTypes/MSDefines.H>

class MSShell;

class MSGUIExport MSShellBusy
{
public:
  MSShellBusy(MSShell *shell_);
  ~MSShellBusy(void);
private:
  MSShell *_shell;
};

class MSGUIExport MSApplicationBusy
{
public:
  MSApplicationBusy(void);
  ~MSApplicationBusy(void);
  static int count(void);
  
private:
  static int _count;
};

#endif
