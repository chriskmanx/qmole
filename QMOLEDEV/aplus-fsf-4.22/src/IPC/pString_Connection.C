///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved.
// See .../src/LICENSE for terms of distribution.
//
//
///////////////////////////////////////////////////////////////////////////////
//
// pString_Connection
//
// This class is subclassed from pA_Connection.  It contains the
// code for the read and send functions using the `string protocol.
// 
// The `string protocol is to send messages with two parts: the first
// part is a four-byte integer of the length of the second part, which
// is the contents of the message, as a char string.
// 

#include <netinet/in.h>
//#if defined(__edgfe) || defined( __sgi) || defined(_AIX) || defined(SOLARIS_CSET)
#include <errno.h>
//#endif
#include <pString_Connection.H>

#include <BufferUtilities.H>

static inline void mfbuffer(MSBuffer *bp)
{
  mf((I *)bp->minofbuffer());
  bp->minofbuffer(0);
}

A pString_Connection::getAobjFromBuffer(MSBuffer *bb)
{
  ipcWarn(wrnlvl(),"%t pString_Connection::getAobjFromBuffer\n");
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
      Warn(fmt,	s,handle());
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
    z=(A)db->minofbuffer();
    hb->reset();
    db->minofbuffer(0);
    db->clear();
    return z;
  }
  R(A)0;
}

A pString_Connection::readBurst(void)
{
  ipcWarn(wrnlvl(),"%t pString_Connection::readBurst\n");
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

A pString_Connection::readOne(void)
{
  ipcWarn(wrnlvl(),"%t pString_Connection::readOne\n");
  MSBuffer *hb=headBuffer();
  MSBuffer *db=readBuffer();
  int slen;
  A d,z=(A)0;

  if(4>(slen=hb->put()-hb->get()))
  {
    if(0>readTheBuffer(hb,4-slen)) return (A)0;
    if(4>(slen=hb->put()-hb->get())) return (A)0;
    
    slen=longAt(hb->get());
    if(0>=slen)
    {
      static char fmt[]="\343 IPC warning: zero-length string message.  slen=%d [%d]\n";
      Warn(fmt,	slen,handle());
      hb->reset();
      turnInReadOff();
      return (A)0;
    }
    d=gv(Ct,slen);
    db->minofbuffer((C *)d); db->get(db->minofbuffer()); 
    db->put((C *)(d->p)); db->maxofbuffer(db->put()+slen);
  }
  if(0>readTheBuffer(db,db->maxofbuffer()-db->put())) return (A)0;
  if(db->put()==db->maxofbuffer())
  {
    z=(A)db->minofbuffer();
    hb->reset();
    db->minofbuffer(0);
    db->clear();
    return z;
  }
  return (A)0;
}

int pString_Connection::send(const A &msg_)
{
  ipcWarn(wrnlvl(),"%t pString_Connection::send\n");
  if(isInReset()||readChannel()==0) return -1;
  if(Ct!=msg_->t) return -1;

  MSBuffer *sb=new MSBuffer(msg_->n+sizeof(int));
  if(NULL==sb) return -1;
  int msgLen=htonl((int)msg_->n);
  sb->stuff((char *)(&msgLen),sizeof(int));
  sb->stuff((const char *)msg_->p, msg_->n);
  sendTheBuffer(sb);
  if (MSFalse==isWritePause()) writeChannel()->enable();
  return doWrite(MSFalse);
}


// M:Syncronous Send and Read

#define LOCALCHANENBL(fds,chan) if(chan)Syncfds.fdsset(fds,chan->fd())

#define MAXBUF 256
static C errorMessage[MAXBUF];
static C errorSymbol[MAXBUF];
static const int HeaderLength=4;

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
I pString_Connection::syncDoWrite(void)
{
  I c,n=0;
  MSNodeItem *hp=writeList();
  MSNodeItem *np;
  MSBuffer *bp;
  MSBoolean notdone=MSTrue;
  
  ipcWarn(wrnlvl(), "%t pString_Connection::syncDoWrite\n");
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


I pString_Connection::syncWriteLoop(struct timeval *pgameover)
{
  I result;
  int rc;
  struct timeval timeleft, *tvp;

  ipcWarn(wrnlvl(), "%t pString_Connection::syncWriteLoop\n");

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
  
I pString_Connection::syncDoRead(A *pdataobj)
{
  I result;
  MSBuffer *hb=headBuffer();
  MSBuffer *db=readBuffer();
  ipcWarn(wrnlvl(),"%t pString_Connection::syncDoRead\n");
  
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

A pString_Connection::syncReadLoop(struct timeval *pgameover)
{
  A result=(A)0, dataobj;
  int rc;
  struct timeval timeleft, *tvp;
  
  ipcWarn(wrnlvl(), "%t pString_Connection::syncReadLoop\n");
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

A pString_Connection::syncSendCover(A msg_, A aTimeout)
{
  struct timeval gameover, *tvp;
  I rc;

  ipcWarn(wrnlvl(),"%t pString_Connection::syncSend\n");

  tvp = atotv(aTimeout, &gameover);
  if(writeChannel()==0) return syncErrorResult("nochan","channel is null");
  
  /* put stuff on queue */
  int dataSize     = msg_->n;
  int bufferSize   = HeaderLength  +dataSize;
  int temp         = htonl(dataSize);

  MSBuffer *sb=new MSBuffer( bufferSize);
  if(NULL==sb) return syncErrorResult("Buffer","new MSBuffer routine failed.");
  sb->stuff((char *)(&temp), HeaderLength);
  sb->stuff((const char *)msg_->p, dataSize);
  sendTheBuffer(sb);

  /* while loop on select() until timeout or write queue empty */
  rc=syncWriteLoop(tvp);
  if(0>rc) return syncErrorResult(errorSymbol,errorMessage);
  else return gvi(Et,3,gsym("OK"),gi(rc),writeQueueStatus());
}

A pString_Connection::syncReadCover(A aTimeout)
{
  struct timeval gameover, *tvp;
  A dataobj;
  ipcWarn(wrnlvl(),"%t pString_Connection::SyncRead\n");

  tvp = atotv(aTimeout, &gameover);
  if(readChannel()==0) return syncErrorResult("nochan","channel is null");

  /* while loop on select() until timeout or complete message received */
  dataobj=syncReadLoop(tvp);
  if (dataobj) return gvi(Et,3,gsym("OK"),dataobj,aplus_nl);
  else return syncErrorResult(errorSymbol, errorMessage);
}
