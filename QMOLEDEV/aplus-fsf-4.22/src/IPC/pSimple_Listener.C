///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved.
// See .../src/LICENSE for terms of distribution.
//
//
///////////////////////////////////////////////////////////////////////////////
//
// pSimple_Listener
//
// This class is a sub-class of pA_Listener.
//    

#include <pSimple_Listener.H>
#include <pSimple_Connection.H>

// M:Callbacks

void pSimple_Listener::acceptNotify(int fd_,
				 const struct sockaddr *peername,
				 int peernamelen,
				 const struct sockaddr *sockname,
				 int socknamelen)
{
  ipcWarn(wrnlvl(),
	  "%t pSimple_Listener::acceptNotify: creating connection. fd=%d\n",
	  fd_);
  pSimple_Connection *ncS=new pSimple_Connection(name(),fd_,acbfunc(),
						 _attrs,_pA_attrs);
  ncS->initFromListener();

  /* next line is just to stop compiler from whining about unused names. */
  if(0)ipcWarn(0,"",peername,peernamelen,sockname,socknamelen);
}
