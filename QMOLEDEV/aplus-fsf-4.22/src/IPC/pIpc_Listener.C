///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved.
// See .../src/LICENSE for terms of distribution.
//
//
///////////////////////////////////////////////////////////////////////////////
//
// pIpc_Listener
//
// This class is a sub-class of pA_Listener.
//    

#include <pIpc_Listener.H>
#include <pIpc_Connection.H>

// M:Callbacks

void pIpc_Listener::acceptNotify(int fd_,
				 const struct sockaddr *peername,
				 int peernamelen,
				 const struct sockaddr *sockname,
				 int socknamelen)
{
  ipcWarn(wrnlvl(),
	  "%t pIpc_Listener::acceptNotify: creating connection. fd=%d\n",
	  fd_);
  pIpc_Connection *ncR=new pIpc_Connection(name(),fd_,acbfunc(),
					   _attrs,_pA_attrs);
  ncR->initFromListener();

  /* next line is just to stop compiler from whining about unused names. */
  if(0)ipcWarn(0,"",peername,peernamelen,sockname,socknamelen);
}
