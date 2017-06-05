///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved.
// See .../src/LICENSE for terms of distribution.
//
//
///////////////////////////////////////////////////////////////////////////////
//
// pA_Connection
//
// This class is subclassed from AipcConnection.  It contains the
// code for the read and send functions using the `A protocol.
// 
// The `A protocol is to send messages with two parts: the first part
// is a four-byte integer of the length of the second part, which is
// athe result of running sys.exp on an A-object.
// 

#ifndef sun
#include <unistd.h>
#endif

#include <errno.h>
#include <pA_Connection.H>

#include <cxsys/impexp.h>

#include <MSIPC/MSTv.H>
#include <BufferUtilities.H>
#if HAVE_IOSTREAM
#include <iostream>
#else
#include <iostream.h>
#endif

MSFds pA_Connection::Syncfds;

void pA_Connection::doRead(void)
{
  ipcWarn(wrnlvl(),"%t pA_Connection::doRead\n");
  if(!ValidateHandle()||isInReset()) return;
  A d=(isBurstMode())?readBurst():readOne();
  if((A)0!=d)
  {
    readNotify(d);  // send address to conform to readNotify argument 
    dc(d);
  }
}

static inline void dcbuffer(MSBuffer *bp)
{
  dc((A)bp->minofbuffer());
  bp->minofbuffer(0);
}

static inline void mfbuffer(MSBuffer *bp)
{
  mf((I *)bp->minofbuffer());
  bp->minofbuffer(0);
}

A pA_Connection::getAobjFromBuffer(MSBuffer *bb)
{
  ipcWarn(wrnlvl(),"%t pA_Connection::getAobjFromBuffer\n");
  MSBuffer *hb=headBuffer();
  MSBuffer *db=readBuffer();
  A d,z;
  I s;
  
  if(4>(s=hb->put()-hb->get()))
  {
    if(0>bufftobuff(bb,hb,4-s)) R(A)0;
    if(4>(s=hb->put()-hb->get())) R(A)0;
    s=longAt(hb->get());
    if(0>=s)
    {
      static char fmt[]="\343 IPC warning: zero-length message found.  s=%d [%d]\n";
      Warn(fmt, s,handle());
      hb->reset();
      turnInReadOff();
      R(A)0;
    }
    
    // create read buffer, doubled as A-object char array
    d=gv(Ct,s);
    db->minofbuffer((C *)d); db->get(db->minofbuffer()); 
    db->put((C *)(d->p)); db->maxofbuffer(db->put()+s);
  }

  if(0>bufftobuff(bb,db,db->maxofbuffer()-db->put())) R(A)0;
  if(db->put()==db->maxofbuffer())
  {
    d=(A)db->minofbuffer();
    z=ImportAObject((C *)d->p, d->n, (C *)0);
    hb->reset();
    dcbuffer(db);
    db->clear();
    if((A)0==z){resetWithError("readImport");R(A)0;}
    return z;
  }
  R(A)0;
}

A pA_Connection::readBurst(void)
{
  ipcWarn(wrnlvl(),"%t pA_Connection::readBurst\n");
  MSBuffer bbuff;
  A d,z=(A)0;
  I slen=readFileLength(),slen1,n,s,count;
  if(-1==slen)R(A)0;
  if(0==slen)
  {
    static char fmt[]="\343 IPC warning: pA::ReadBurst: read event with no data [%d]\n";
    Warn(fmt, handle());
  }

  /* create buff to hold it.  Fill buffer */
  slen1=slen?slen:4;
  bbuff.minofbuffer(mab(slen1));
  bbuff.maxofbuffer(bbuff.minofbuffer()+slen1);
  bbuff.reset();
  if(0>(n=readTheBuffer(&bbuff,slen1))) {mfbuffer(&bbuff); R(A)0;}
  if(0==n&&0==slen) {turnInReadOff(); mfbuffer(&bbuff); R(A)0;}

  d=getAobjFromBuffer(&bbuff);
  if((A)0==d){mfbuffer(&bbuff); R(A)0;}

  // determine how many more complete A-objects lie in bbuff
  count=1;
  for(C *cp=bbuff.get();cp<bbuff.put();cp+=s)
  {
    s=longAt(cp);
    cp+=sizeof(long);
    if(s<=bbuff.put()-cp)++count;
  }

  // create result
  z=gv(Et,count);
  for(int i=0;i<count;++i)z->p[i]=(I)aplus_nl;
  int idx=0;
  z->p[idx++]=(I)d;

  // retrieve additional A-objects from bbuff, fill in z
  while(idx<count)
  {
    d=getAobjFromBuffer(&bbuff);
    if((A)0==d)break;
    z->p[idx++]=(I)d;
  }
  if(idx<count)
  {
    ipcWarn(wrnlvl(),"%t burst mode aborted.  Possible data loss.\n");
  }

  // run once more to clear out bbuff and move partial object into connection
  // buffers
  if(bbuff.get()==bbuff.put())turnInReadOff();
  else
  {
    d=getAobjFromBuffer(&bbuff);
    if((A)0!=d || bbuff.get()!=bbuff.put())
    {
      ipcWarn(wrnlvl(),"%t burst buffer not cleared: %d %d %d\n",
	    d,bbuff.get(),bbuff.put());
    }
  }

  // free bbuff;
  mfbuffer(&bbuff);

  return z;     
}

A pA_Connection::readOne(void)
{
  ipcWarn(wrnlvl(),"%t pA_Connection::readOne\n");
  MSBuffer *hb=headBuffer();
  MSBuffer *db=readBuffer();
  int s;
  A d,z=(A)0;

  if(4>(s=hb->put()-hb->get()))
  {
    if(0>readTheBuffer(hb,4-s)) R(A)0;
    if(4>(s=hb->put()-hb->get())) R(A)0;
    
    s=longAt(hb->get());
    if(0>=s)
    {
      static char fmt[]="\343 IPC warning: zero-length A-protocol message.  s=%d [%d]\n";
      Warn(fmt,	s,handle());
      hb->reset();
      turnInReadOff();
      R(A)0;
    }
    d=gv(Ct,s);
    db->minofbuffer((C *)d); db->get(db->minofbuffer()); 
    db->put((C *)(d->p)); db->maxofbuffer(db->put()+s);
  }
  if(0>readTheBuffer(db,db->maxofbuffer()-db->put())) R(A)0;
  if(db->put()==db->maxofbuffer())
  {
    d=(A)(db->minofbuffer());
    z=ImportAObject((C *)d->p, d->n, (C *)0);
    hb->reset();
    dcbuffer(db);
    db->clear();
    turnInReadOff();
    if((A)0==z){resetWithError("readImport");R(A)0;}
  }
  return z;
}

int pA_Connection::send(const A &msg_)
{
  ipcWarn(wrnlvl(),"%t pA_Connection::send\n");
  if(isInReset()||readChannel()==0) return -1;

  MSBuffer *sb=createBufferFromAobj(msg_);
  if(NULL==sb) return -1;
  sendTheBuffer(sb);
  if (MSFalse==isWritePause()) writeChannel()->enable();
  return doWrite(MSFalse);
}


// M:Attribute Interface

A pA_Connection::setableAttrlist(void)
{
  ipcWarn(wrnlvl(),"%t pA_Connection::setableAttrlist\n");
  A fromParent=AipcConnection::setableAttrlist();
  A fromHere=_pA_attrs.setableAttrs();
  A z=gv(Et,fromParent->n+fromHere->n);
  int i,idx=0;
  for(i=0;i<fromParent->n;++i)z->p[idx++]=fromParent->p[i];
  for(i=0;i<fromHere->n;++i)z->p[idx++]=fromHere->p[i];
  dc(fromParent);
  return z;
}

A pA_Connection::getableAttrlist(void)
{
  ipcWarn(wrnlvl(),"%t pA_Connection::getableAttrlist\n");
  int i,idx=0;
  A fromParent=AipcConnection::getableAttrlist();
  A sattrs=_pA_attrs.setableAttrs();
  A nsattrs=_pA_attrs.nonsetableAttrs();
  A z=gv(Et,fromParent->n+sattrs->n+nsattrs->n);
  for (i=0;i<fromParent->n;++i)z->p[idx++]=fromParent->p[i];
  for (i=0;i<sattrs->n;++i)z->p[idx++]=sattrs->p[i];
  for (i=0;i<nsattrs->n;++i)z->p[idx++]=nsattrs->p[i];
  dc(fromParent);
  return z;
}

MSBoolean pA_Connection::setAttr(C *attr_,A aval_)
{
  ipcWarn(wrnlvl(),"%t pA_Connection::setAttr\n");
  int idx=_pA_attrs.setAttrIndex(attr_);
  I ival;
  switch(idx) {
    CSBOOL(0,aval_,turnBurstModeOn,turnBurstModeOff);
  default: return AipcConnection::setAttr(attr_,aval_);
  }
  return MSTrue;
}

A pA_Connection::getAttr(C *attr_)
{
  ipcWarn(wrnlvl(),"%t pA_Connection::getAttr\n");
  int idx=_pA_attrs.setAttrIndex(attr_);
  if(-1!=idx)
  {
    switch(idx) {
      CSR(0,R gi(isBurstMode()?1:0));
    default: return aplus_nl;
    }
  } else {
    int idx=_pA_attrs.nonsetAttrIndex(attr_);
    if(-1!=idx)
    {
      switch(idx) {
      default: return aplus_nl;
      }
    }
    else return AipcConnection::getAttr(attr_);
  }
}


// M:Syncronous Send and Read

#define LOCALCHANENBL(fds,chan) if(chan)Syncfds.fdsset(fds,chan->fd())

#define MAXBUF 256
static C errorMessage[MAXBUF];
static C errorSymbol[MAXBUF];

static A syncErrorResult(const C *sym_, const C *str_)
{
  return gvi(Et,3,gsym("error"),gsym(sym_),gsv(0,str_));
}

static I syncFillError(const C *sym, const C *fmt, ...) 
{
  va_list ap;
  
  strcpy(errorSymbol,sym);
  va_start(ap, fmt);
  (void)vsprintf(errorMessage, fmt, ap);
  va_end(ap);
  return -1;
}

/* syncDoWrite
 * returns -1 on error, 0 if not finished, 1 if finished
 * Note that, on a return of 0, some messages may have been sent, if 
 * several were queued up.
 */
I pA_Connection::syncDoWrite(void)
{
  I c,n=0;
  MSNodeItem *hp=writeList();
  MSNodeItem *np;
  MSBuffer *bp;
  MSBoolean notdone=MSTrue;
  
  ipcWarn(wrnlvl(), "%t pA_Connection::syncDoWrite\n");
  /* write queue to write channel */ 

  while(notdone&&(hp!=(np=hp->next())))
  {
    bp=(MSBuffer *)np->data();
    c=bp->put()-bp->get();
    while(c>0 && 0<(n=bp->write(fd(),c))) c-=n;
    if(bp->get() == bp->put())
    {
      delete bp;delete np;turnInWriteOff();
    }
    else 
    {
      notdone=MSFalse;turnInWriteOn();
    }
    if (0 > n)
      return syncFillError("buffwrite","buffwrite returned error %d",n);
  }
  if(hp == hp->next()) return 1;
  else return 0;
}


I pA_Connection::syncWriteLoop(struct timeval *pgameover)
{
  I result;
  int rc;
  struct timeval timeleft, *tvp;

  ipcWarn(wrnlvl(), "%t pA_Connection::syncWriteLoop\n");

  /* make arguments for select() */
  Syncfds.fdszero(Syncfds.w());
  Syncfds.fdszero(Syncfds.wa());
  LOCALCHANENBL(Syncfds.w(),writeChannel());

  if (pgameover != (struct timeval *)0) 
  {
    tvp=&timeleft;
    tvdiff(pgameover,tod(),tvp);
    if (0>tvp->tv_sec) tvp->tv_sec=tvp->tv_usec=0;
  } 
  else tvp=NULL;

  for(;;) {
    Syncfds.fdscopy(Syncfds.w(),Syncfds.wa());
    if (((rc = select(Syncfds.size(), NULL, Syncfds.wa(), NULL, tvp)) < 0)) 
    {
      if (-1==rc && EINTR==errno)
	result=syncFillError("interrupt","select() received an interrupt");
      else
	result=syncFillError("select",
			     "select() returned error code %d.  errno=%d",
			     rc, errno);
      break;
    }

    if (rc) 
    {
      if (Syncfds.fdsisset(Syncfds.wa(), fd())) 
      {
	if (result=syncDoWrite()) break;
      }
      else 
      {
	result=syncFillError("fdsisset","unexpected event broke select()");
	break;
      }
    }
    
    /* check for timeout and reset timeleft */
    if (NULL != tvp) 
    {
      tvdiff(pgameover,tod(),tvp);
      if (0>tvp->tv_sec) tvp->tv_sec=tvp->tv_usec=0;
      if (0==tvp->tv_sec && 0==tvp->tv_usec) {
	result=syncFillError("timeout","Syncwrite loop timed out");
	break;
      }
    }
  } /* end forever loop */

  return result;
}  
  
I pA_Connection::syncDoRead(A *pdataobj)
{
  I result;
  MSBuffer *hb=headBuffer();
  MSBuffer *db=readBuffer();
  ipcWarn(wrnlvl(),"%t pA_Connection::syncDoRead\n");
  
  *pdataobj=readOne();
  if(*pdataobj==(A)0) 
  {
    if(isInReset()) 
      result=syncFillError("reset","Reset occurred.  No message read.");
    else result=0;
  } 
  else result=1;

  return result;
}

A pA_Connection::syncReadLoop(struct timeval *pgameover)
{
  A result=(A)0, dataobj;
  int rc;
  struct timeval timeleft, *tvp;
  
  ipcWarn(wrnlvl(), "%t pA_Connection::syncReadLoop\n");
  /* make arguments for select() */
  Syncfds.fdszero(Syncfds.r());
  Syncfds.fdszero(Syncfds.ra());
  LOCALCHANENBL(Syncfds.r(),readChannel());
  
  if (pgameover != (struct timeval *)0) 
  {
    tvp=&timeleft;
    tvdiff(pgameover,tod(),tvp);
    if (0>tvp->tv_sec) tvp->tv_sec=tvp->tv_usec=0;
  } else tvp=NULL;
  
  for(;;) 
  {
    Syncfds.fdscopy(Syncfds.r(),Syncfds.ra());

    if (((rc = select(Syncfds.size(), Syncfds.ra(), NULL, NULL, tvp)) < 0)) 
    {
      if (EINTR==errno)
	syncFillError("interrupt","select() received an interrupt");
      else
	syncFillError("select", 
		      "select() returned error code %d.  errno=%d",
		      rc, errno);
      break;
    }
    
    if (rc) 
    {
      if (Syncfds.fdsisset(Syncfds.ra(), fd()))
      {
	rc=syncDoRead(&dataobj);
	if (0<rc) result=dataobj;
	if (rc) break;
      }
      else {
	syncFillError("fdsisset","unexpected event broke select()");
	break;
      }
    }
    
    /* check for timeout and reset timeleft */
    if (NULL != tvp) 
    {
      tvdiff(pgameover,tod(),tvp);
      if (0>tvp->tv_sec) tvp->tv_sec=tvp->tv_usec=0;
      if (0==tvp->tv_sec && 0==tvp->tv_usec) 
      {
	syncFillError("timeout","Syncread loop timed out");
	break;
      }
    }
    
  } /* end for(;;) loop */
  
  return result;
}  

A pA_Connection::syncSendCover(A msg_, A aTimeout)
{
  struct timeval gameover, *tvp;
  I rc;

  ipcWarn(wrnlvl(),"%t pA_Connection::syncSend\n");

  tvp = atotv(aTimeout, &gameover);
  if(writeChannel()==0) return syncErrorResult("nochan","channel is null");
  
  /* put stuff on queue */
  MSBuffer *sb=createBufferFromAobj(msg_);
  if(NULL==sb) return syncErrorResult("export","Export routine failed.");
  sendTheBuffer(sb);

  /* while loop on select() until timeout or write queue empty */
  rc=syncWriteLoop(tvp);
  if(0>rc) return syncErrorResult(errorSymbol,errorMessage);
  else return gvi(Et,3,gsym("OK"),gi(rc),writeQueueStatus());
}

A pA_Connection::syncReadCover(A aTimeout)
{
  struct timeval gameover, *tvp;
  A dataobj;
  ipcWarn(wrnlvl(),"%t pA_Connection::SyncRead\n");

  tvp = atotv(aTimeout, &gameover);
  if(readChannel()==0) return syncErrorResult("nochan","channel is null");

  /* while loop on select() until timeout or complete message received */
  dataobj=syncReadLoop(tvp);
  if (dataobj) return gvi(Et,3,gsym("OK"),dataobj,aplus_nl);
  else return syncErrorResult(errorSymbol, errorMessage);
}

