///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved.
// See .../src/LICENSE for terms of distribution.
//
//
///////////////////////////////////////////////////////////////////////////////
//
// pRaw_Listener
//
// This class is a sub-class of pA_Listener.
//    

#include <pRaw_Listener.H>
#include <pRaw_Connection.H>

// M:Callbacks

void pRaw_Listener::acceptNotify(int fd_,
				 const struct sockaddr *peername,
				 int peernamelen,
				 const struct sockaddr *sockname,
				 int socknamelen)
{
  ipcWarn(wrnlvl(),
	  "%t pRaw_Listener::acceptNotify: creating connection. fd=%d\n",
	  fd_);
  pRaw_Connection *ncR=new pRaw_Connection(name(),fd_,acbfunc(),
					   _attrs,_pA_attrs);
  ncR->initFromListener();

  /* next line is just to stop compiler from whining about unused names. */
  if(0)ipcWarn(0,"",peername,peernamelen,sockname,socknamelen);
}
