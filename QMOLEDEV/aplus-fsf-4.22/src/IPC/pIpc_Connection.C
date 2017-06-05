///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved.
// See .../src/LICENSE for terms of distribution.
//
//
///////////////////////////////////////////////////////////////////////////////
//
// pIpc_Connection
//
// This class is subclassed from pA_Connection.  It contains the
// code for the read and send functions using the `ipc protocol.
// 
// The `A protocol is to send messages with two parts: the first part
// is a four-byte integer of the length of the second part, which is
// athe result of running sys.exp on an A-object.
// 

#include <pIpc_Connection.H>

A pIpc_Connection::readBurst(void)
{
  ipcWarn(wrnlvl(),"%t pIpc_Connection::readBurst\n");
  A d=readOne();
  R d?gvi(Et,1,d):(A)0;
}


A pIpc_Connection::readOne(void)
{
  ipcWarn(wrnlvl(),"%t pIpc_Connection::readOne\n");
  A d,d0,d1,z;
  C *c;
  MSBuffer *hb=headBuffer();
  MSBuffer *db=readBuffer();
  I s;

  if(16>(s=hb->put()-hb->get())){
    if(0>readTheBuffer(hb,16-s)) R(A)0;
    if(16>(s=hb->put()-hb->get())) R(A)0;
    c = hb->get();
    d=gv(Et,2);
    d->p[0]=(I)(d0=gv(It,5));
    d0->p[0]=longAt(c);
    d0->p[1]=longAt(c+4);
    d0->p[2]=shortAt(c+8);
    d0->p[3]=shortAt(c+10);
    d0->p[4]=longAt(c+12);
    d->p[1]=(I)(d1=gv(Ct,s=d0->p[4]));
    db->minofbuffer((C *)(d));
    db->get(db->minofbuffer());
    db->put((C *)(d1->p));
    db->maxofbuffer(db->put()+s);
  }
  if(0>readTheBuffer(db,db->maxofbuffer()-db->put())) R(A)0;
  if(db->put() == db->maxofbuffer())
  {
    z=(A)(db->minofbuffer());
    hb->reset();
    db->minofbuffer(0);
    db->clear();
    turnInReadOff();
    R z;
  }
  R(A)0;
}

int pIpc_Connection::send(const A &msg_)
{
  I t;
  A rhdr, rdat; short s;

  ipcWarn(wrnlvl(),"%t pIpc_Connection::send\n");
  if(isInReset()||readChannel()==0) return -1;
  if((msg_->t!=Et)||(msg_->n!=2)) R -1;

  rhdr=(A)(msg_->p[0]);
  if((rhdr->t!=It)||(2!=rhdr->n)) R -1;
  rdat=(A)(msg_->p[1]);
  if((rdat->t!=Ct)||(rdat->r!=1)) R -1;
  MSBuffer *bp=new MSBuffer(16+rdat->n);

  bp->stuff("MGMG",4);			/* magic cookie */
  t=todsec(); bp->stuff((char *)&t,4);	/* time of day */
  s=rhdr->p[0]; bp->stuff((char *)&s,2);	/* sequence number */
  s=rhdr->p[1]; bp->stuff((char *)&s,2);	/* subject */
  bp->stuff((char *)&rdat->n,4);		/* msg len */
  bp->stuff((C *)(rdat->p),rdat->n);
  sendTheBuffer(bp);
  if (MSFalse==isWritePause()) writeChannel()->enable();
  return doWrite(MSFalse);
}
