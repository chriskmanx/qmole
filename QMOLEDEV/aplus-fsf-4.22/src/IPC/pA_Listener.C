///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved.
// See .../src/LICENSE for terms of distribution.
//
//
///////////////////////////////////////////////////////////////////////////////
//
// pA_Listener
//
// This class is a sub-class of AipcListener.
//    

#include <pA_Listener.H>
#include <pA_Connection.H>

// Static members

pA_Listener::pA_Listener(C *serviceName_,A cbfunc_)
  : AipcListener(serviceName_,cbfunc_)
{
  ipcWarn(0,"%t pA_Listener::pA_Listener(1)\n");
  AipcHostPortProtocol ahpp(serviceName_);
  hostPort().set(ahpp.host(),ahpp.port());
}

// M:Attribute interface

A pA_Listener::setableAttrlist(void)
{
  A fromParent=AipcListener::setableAttrlist();
  A fromHere=_pA_attrs.setableAttrs();
  A z=gv(Et,fromParent->n+fromHere->n);
  int i,idx=0;
  for(i=0;i<fromParent->n;++i)z->p[idx++]=fromParent->p[i];
  for(i=0;i<fromHere->n;++i)z->p[idx++]=fromHere->p[i];
  dc(fromParent);
  return z;
}

A pA_Listener::getableAttrlist(void)
{
  int i,idx=0;
  A fromParent=AipcListener::getableAttrlist();
  A sattrs=_pA_attrs.setableAttrs();
  A nsattrs=_pA_attrs.nonsetableAttrs();
  A z=gv(Et,fromParent->n+sattrs->n+nsattrs->n);
  for (i=0;i<fromParent->n;++i)z->p[idx++]=fromParent->p[i];
  for (i=0;i<sattrs->n;++i)z->p[idx++]=sattrs->p[i];
  for (i=0;i<nsattrs->n;++i)z->p[idx++]=nsattrs->p[i];
  dc(fromParent);
  R z;
}

MSBoolean pA_Listener::setAttr(C *attr_,A aval_)
{
  ipcWarn(wrnlvl(),"%t pA_Listener::setAttr\n");
  int idx=_pA_attrs.setAttrIndex(attr_);
  I ival;
  switch(idx) {
    CS(0,BoolCHK(aval_);_pA_attrs.burstMode(ival?MSTrue:MSFalse));
  default: R AipcListener::setAttr(attr_,aval_);
  }
  R MSTrue;
}

A pA_Listener::getAttr(C *attr_)
{
  ipcWarn(wrnlvl(),"%t pA_Listener::getAttr\n");
  int idx=_pA_attrs.setAttrIndex(attr_);
  if(-1!=idx)
  {
    switch(idx) {
      CSR(0,R gi(_pA_attrs.burstMode()?1:0));
    default: R aplus_nl;
    }
  } else {
    int idx=_pA_attrs.nonsetAttrIndex(attr_);
    if(-1!=idx)
    {
      switch(idx) {
      default: R aplus_nl;
      }
    }
    else R AipcListener::getAttr(attr_);
  }
}

// M:Callbacks

void pA_Listener::acceptNotify(int fd_,
				const struct sockaddr *peername,
				int peernamelen,
				const struct sockaddr *sockname,
				int socknamelen)
{
  pA_Connection *ncA=new pA_Connection(name(),fd_,acbfunc(),_attrs,_pA_attrs);
  ipcWarn(wrnlvl(),
	  "%t pA_Listener::acceptNotify: created connection %d. fd=%d\n",
	  ncA->handle(),fd_);

  ncA->initFromListener();

  /* next line is just to stop compiler from whining about unused names. */
  if(0)ipcWarn(0,"",peername,peernamelen,sockname,socknamelen);
}
