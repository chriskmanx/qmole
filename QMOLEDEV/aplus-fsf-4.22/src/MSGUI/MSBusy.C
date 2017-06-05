///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////

#include <MSGUI/MSBusy.H>
#include <MSGUI/MSGUIEnum.H>

extern void changeBusyState(MSShell *,MSBoolean);
extern void changeBusyState(MSBoolean);

int MSApplicationBusy::_count=0;

MSShellBusy::MSShellBusy(MSShell *shell_) : _shell(shell_)
{ changeBusyState(_shell,MSTrue); }

MSShellBusy::~MSShellBusy(void)
{ changeBusyState(_shell,MSFalse); }

MSApplicationBusy::MSApplicationBusy(void)
{
  if (_count==0) changeBusyState(MSTrue);
  _count++;
}

MSApplicationBusy::~MSApplicationBusy(void)
{
  if (_count==1) changeBusyState(MSFalse);
  _count--;
}

int MSApplicationBusy::count(void)
{ return _count; }



