///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////

#include <MSTypes/MSNodeList.H>
#include <MSTypes/MSString.H>
#include <MSIPC/MSFds.H>
#include <MSIPC/MSChannel.H>
#include <MSTypes/MSMessageLog.H>
#include <errno.h>
#include <unistd.h>
#include <sys/stat.h>

#ifdef MS_NEED_CTL
// We probably should get the following macros from ipc_const.h,
// but for ease of compilation, we'll hard code them for now.
#define IPC_READ_MASK           0x1
#define IPC_WRITE_MASK          0x2
#define IPC_EXCEPT_MASK         0x4
extern "C" 
{
int ipc_set_external(int,int,void(*)(int,int,void*),void*);
int ipc_del_external(int);
int ipc_dsa_external(int,int);
int ipc_ena_external(int,int);
}
#endif

MSNodeItem *MSChannel::_pCtlChannelList=0;
MSFds *MSChannel::_pFds=0;
MSNodeItem *MSChannel::_pChannelList=0;

MSChannel::MSChannel(const char *name_,int fd_,int pri_,Type type_,MSCallback *pCallback_)
{
  init();
  _pNode=new MSNodeItem((void *)this);
  _name=(name_==0)?"<UNKNOWN>":name_;
  _pCallback=pCallback_;
  
#if !defined(MS_WINSOCK)
  //This check is not valid in windows.
  if ((fd_<0)||(fd_>=_pFds->size()))
   {
     MSMessageLog::warningMessage("MSChannel: %s: warning: bad fd==%d\n",name_,fd_);
   }
#endif
  _fd=fd_;
  _type=type_;
  switch (type_)
   {
     case Read:
       _efds=_pFds->r();
       _afds=_pFds->ra();
       break;
     case Write:
       _efds=_pFds->w();
       _afds=_pFds->wa();
       break;
     case Exception:
     default:
       _efds=_pFds->x();

       _afds=_pFds->xa();
       break;
   }
  _pFds->fdsclr(_efds,_fd);
  _pFds->fdsclr(_afds,_fd);
  priority(pri_);
  _pCtlObject=0;
#ifdef MS_NEED_CTL
  MSNodeItem *hp=_pCtlChannelList;
  MSNodeItem *tnp;
  for (tnp=hp->next();tnp!=hp;tnp=tnp->next())
   {
     CtlObject *co=(CtlObject *) tnp->data();
     if (co->fd()==_fd)
      {
	_pCtlObject=co;
	break;
      }
   }
  if (_pCtlObject==0)
   {
     _pCtlObject=new CtlObject(_fd);
     MSNodeItem *item=new MSNodeItem(_pCtlObject);
     item->insert(hp->next());
     _pCtlObject->node(item);
   }
  switch (_type)
   {
   case Read:
     _pCtlObject->readChannel(this);
     ipc_set_external(_fd,_pCtlObject->mask(),ctlHandler,_pCtlObject);
     break;
   case Write:
     _pCtlObject->writeChannel(this);
     ipc_set_external(_fd,_pCtlObject->mask(),ctlHandler,_pCtlObject);
     break;
   case Exception:
     _pCtlObject->exceptionChannel(this);
     ipc_set_external(_fd,_pCtlObject->mask(),ctlHandler,_pCtlObject);
     break;
   }
#endif
}

MSChannel::~MSChannel(void)
{
#ifdef MS_NEED_CTL
  if (_type==Read) 
   {
     _pCtlObject->readChannel(0);
     _pCtlObject->mask(_pCtlObject->mask()&~IPC_READ_MASK);
     ipc_dsa_external(_fd,IPC_READ_MASK);
   }
  else if (_type==Write) 
   {
     _pCtlObject->writeChannel(0);
     _pCtlObject->mask(_pCtlObject->mask()&~IPC_WRITE_MASK);
     ipc_dsa_external(_fd,IPC_WRITE_MASK);
   }
  else if (_type==Exception) 
   {
     _pCtlObject->exceptionChannel(0);
     _pCtlObject->mask(_pCtlObject->mask()&~IPC_EXCEPT_MASK);
     ipc_dsa_external(_fd,IPC_EXCEPT_MASK);
   }
  if (_pCtlObject->readChannel()==0 &&
      _pCtlObject->writeChannel()==0 &&
      _pCtlObject->exceptionChannel()==0)
   {
     ipc_del_external(_fd);
     delete _pCtlObject->node();
     delete _pCtlObject;
   }
#endif 
  _pFds->fdsclr(_afds,_fd);
  _pFds->fdsclr(_efds,_fd);
  if (_pCallback!=0) delete _pCallback;
  delete _pNode;
}

void MSChannel::init(void)
{
  if (_pFds==0) _pFds=new MSFds;
  if (_pChannelList==0) _pChannelList=new MSNodeItem;
#ifdef MS_NEED_CTL
  if (_pCtlChannelList==0) _pCtlChannelList=new MSNodeItem;
#endif  
}

MSFds *MSChannel::fds(void)
{ return _pFds; }
fd_set *MSChannel::afds(void)
{ return _afds; }
fd_set *MSChannel::efds(void)
{ return _efds; }
MSCallback *MSChannel::callback(void)
{ return _pCallback; }

int MSChannel::priority(void) const
{ return _priority; }
int MSChannel::fd(void) const
{ return _fd; }
const char *MSChannel::name(void) const
{ return _name.string(); }
MSChannel::Type MSChannel::type(void) const
{ return _type; }  

MSBoolean MSChannel::enabled(void)
{ return (_pFds->fdsisset(_efds,_fd))?MSTrue:MSFalse; }
MSBoolean MSChannel::disabled(void)
{ return (_pFds->fdsisset(_efds,_fd))?MSFalse:MSTrue; }

void MSChannel::callback(MSCallback *pCallback_)
{
  if (_pCallback!=pCallback_)
   {
     if (_pCallback!=0) delete _pCallback;
     _pCallback=pCallback_;
   }
}

void MSChannel::priority(int pri_)
{
  MSNodeItem *hp=_pChannelList;
  MSNodeItem *tnp;
  
  _pNode->remove();
  _priority=pri_;
  for (tnp=hp->next();tnp!=hp;tnp=tnp->next())
   {
     MSChannel *pChannel=(MSChannel *) tnp->data();
     if (priority()>=pChannel->priority()) break;
   }
  _pNode->insert(tnp);
}

void MSChannel::fdsfresh(int fd_)
{
  init();
  _pFds->fdsfresh(fd_);
}

void MSChannel::fdscopy(void)
{
  init();
  _pFds->fdscopy();
}

void MSChannel::fdszero(void)
{
  init();
  _pFds->fdszero();
}

void MSChannel::process(void)
{ if (_pCallback!=0) _pCallback->process(); }

int MSChannel::select(int size_, fd_set *r_, fd_set *w_, fd_set *e_, struct timeval *t_)
{
#if defined(MS_USE_MSSELECT)
	return MSXselect(size_,r_,w_,e_,t_);
#else
	return ::select(size_,r_,w_,e_,t_);
#endif
}

int MSChannel::select(struct timeval *pTimeout_)
{ return select(_pFds->size(),_pFds->ra(),_pFds->wa(),_pFds->xa(),pTimeout_); }

// return codes:
// rc==0 timeout
// rc>0 select fired on given file descriptor
// rc<0 check errno,
// rc==-1 && errno==EIO - select poped incorrectly (fdisset(fd) failed)
int MSChannel::select(int fd_,MSChannel::Type type_,struct timeval *pTimeout_)
{
  int rc=0;
  int size=_pFds->size();
  fd_set fdSet;
  
  _pFds->fdszero(&fdSet);
  _pFds->fdsset(&fdSet,fd_);
  switch (type_)
   {
   case Read:
     rc=select(size,&fdSet,0,0,pTimeout_); break;
  case Write:
     rc=select(size,0,&fdSet,0,pTimeout_); break;
   case Exception:
     rc=select(size,0,0,&fdSet,pTimeout_); break;
   }
  if (rc>0&&!_pFds->fdsisset(&fdSet,fd_))
   {
     rc=-1;
     errno=EIO;
   }
  return rc;
}

// Notes for MSChannel::processChannels()  [maus: 1/96]
// 
// _pChannelList is a pointer to the head node of a double-linked,
// circular list.  A large part of the complexity of this routine
// relates to the workings of circular lists.  In particular, it
// is worth remembering that header nodes can be considered the start
// or end of the list, and that the insert() function actually
// serves a number of functions, including splitting a list into
// two pieces!
// 
// "work" and "done" are temporary header nodes.  wp and dp point to
// them.  They will be inserted into the Channel list to mark the
// top and bottom of a section of the list to be detached from the
// main list, looped over, and then rejoined to the main list.  This
// section is called the "work list".  When it is created, wp marks
// the start of the work list, and dp marks the end.  (But remember
// that the list is circular!)
// 
// np is a pointer to the "current node"--the node currently being
// examined or processed.
// 
// When the work list is looped over, a function called process() is
// called, which may have side-effects, including changes to the
// Channel list, and removal or deletion of the node being
// processed.
// 
// The complexity of processChannels reflects this.
// 
// The basic structure of the program is this:
// 
// 1. Find the first node of the Channel list to process.  Put wp
// before that node.
// 
// 2. Find the next node *not* to process.  Put dp before that node.
// 
// 3. Remove the sub-list wp->...->dp from the Channel list.  Call
// this the work list.  All items in the work list have equal
// priority.
// 
// 4. Loop over the work list.  For each node, move wp ahead of np,
// and process the node.  This may result in the node's destruction.
// It may also change the Channel list.  Keeping wp ahead of np
// allows us to loop using wp->next() instead of np->next(), which
// protects up from np's possible deletion.
// 
// At the end of the loop, wp is at the end of the list, and is
// removed.  Thanks to the circularity of the list, dp still acts as
// a head node.
// 
// 5. Re-insert the work list into the Channel list, maintaining the
// ordering by priority.

MSBoolean MSChannel::processChannels(void)
{
  if (MSChannel::_pChannelList!=0)
   {
     MSNodeItem  work; // work queue marker
     MSNodeItem  done; // done queue marker
     MSNodeItem *hp=_pChannelList;
     MSNodeItem *wp=work.address();
     MSNodeItem *dp=done.address();
     MSNodeItem *np;
     MSChannel  *pChannel;
     int pri;

     // Find the first node to process.  Store its priority as pri.
     for (np=hp->next();np!=hp;np=np->next())
      {
        pChannel=(MSChannel *) np->data();
        if (_pFds->fdsisset(pChannel->_afds,pChannel->_fd))
         {
           pri=pChannel->_priority;
           break;
         }
      }
     if (np==hp) return MSFalse; // nothing found to do
  
     // insert wp before np, to mark the start of the work list.
     wp->insert(np);
     // search forward for first node with a different priority
     while ((np=np->next())!=hp)
      {
        MSChannel *pPriChannel=(MSChannel *) np->data();
        if (pPriChannel->_priority!=pri) break;
      }

     // "Pinch off" the list between wp and np.  This will become the work
     // list. Note that the insert() call is a misnomer, and, in the
     // obscure physics of circular lists, will actually remove all
     // nodes between wp and np (including wp, but not including np)
     // from the Channel list.
     wp->insert(np);
     
     // Place dp behind wp.  The work list now has two head nodes.
     // wp will be used to control the loop over the work list.
     dp->insert(wp);
	 
     // process work list
     while ((np=wp->next())!=dp)
      {
        wp->toad();	// this moves wp ahead of np
        pChannel=(MSChannel *) np->data();
        if (_pFds->fdsisset(pChannel->_afds,pChannel->_fd))
         {
           _pFds->fdsclr(pChannel->_afds,pChannel->_fd);
           pChannel->process();
         }
      }

     // wp is now at the end of the list, behind dp.  remove it.
     wp->remove();

     // Now we are going to put the work list back into the Channel
     // list, which may have changed out from under us.  The Channel
     // list is sorted by priority, so the first step is to loop thru
     // the list until we find the first node past the point we want
     // to insert the work list.
     for (np=hp->next();np!=hp;np=np->next())
      {
        MSChannel *pPriChannel=(MSChannel *) np->data();
        if (pri>=pPriChannel->_priority) break;
      }
     // splice the work list into the Channel list in front of np.
     dp->insert(np);

     // take out dp, which has done its job.
     dp->remove();
     return MSTrue;
   }
  return MSFalse;
}

MSBoolean MSChannel::removeBadFds()
{
  MSNodeItem *hp=_pChannelList;
  MSNodeItem *np;
  MSChannel  *pChannel;
  MSBoolean ret=MSFalse;
  struct timeval timeout;
  int rc=0;
 
  // zero time value to ensure unblocking select
  timeout.tv_sec=(unsigned long)0;
  timeout.tv_usec=(unsigned long)0;
  
  // run unblocking select on all enabled channels and remove ones with bad fd
  for (np=hp->next();np!=hp;np=np->next())
   {
     pChannel=(MSChannel *) np->data();
     if (pChannel->enabled()==MSTrue)
      {
        rc = select(pChannel->fd(), pChannel->type(), &timeout);
        if (rc==-1&&errno==EBADF)
         {
           // found bad fd
           pChannel->disable();
           MSNodeItem* tmp = np->prev();
           np->remove();                   // remove from the list
           np=tmp;
         }
        else
         {
           // fds availible, set to return true to continue select
           ret=MSTrue;
         }
      }
   }
  return ret;
}

#ifdef MS_NEED_CTL
void MSChannel::ctlHandler(int,int reason_,void *client_)
{
  CtlObject *obj=(CtlObject *) client_;
  MSChannel *pChannel=0;
  if (reason_==IPC_READ_MASK) pChannel=obj->readChannel();
  else if (reason_==IPC_WRITE_MASK) pChannel=obj->writeChannel();
  else if (reason_==IPC_EXCEPT_MASK) pChannel=obj->exceptionChannel();
  if (pChannel!=0) pChannel->process();
  else
   {
     MSMessageLog::warningMessage("MSChannel: Getting called from IPC with no channel to Handle\n");
   }
}
#else
void MSChannel::ctlHandler(int,int,void *) {}
#endif

void MSChannel::disable(void)     
{ 
   _pFds->fdsclr(_efds,_fd); 
   
#ifdef MS_NEED_CTL
   switch (_type)
   {
     case Read:
      _pCtlObject->mask(_pCtlObject->mask()&~IPC_READ_MASK);
      ipc_dsa_external(_fd,IPC_READ_MASK);
      break;
     case Write:
      _pCtlObject->mask(_pCtlObject->mask()&~IPC_WRITE_MASK);
      ipc_dsa_external(_fd,IPC_WRITE_MASK);
       break;
     case Exception:
      _pCtlObject->mask(_pCtlObject->mask()&~IPC_EXCEPT_MASK);
      ipc_dsa_external(_fd,IPC_EXCEPT_MASK);
      break;
   }
#endif
}

void MSChannel::enable(void)      
{ 
   _pFds->fdsset(_efds,_fd); 
#ifdef MS_NEED_CTL
   switch (_type)
   {
     case Read:
      _pCtlObject->mask(_pCtlObject->mask()|IPC_READ_MASK);
      ipc_ena_external(_fd,IPC_READ_MASK);
      break;
     case Write:
      _pCtlObject->mask(_pCtlObject->mask()|IPC_WRITE_MASK);
      ipc_ena_external(_fd,IPC_WRITE_MASK);
       break;
     case Exception:
      _pCtlObject->mask(_pCtlObject->mask()|IPC_EXCEPT_MASK);
      ipc_ena_external(_fd,IPC_EXCEPT_MASK);
      break;
   }
#endif
}

// MSChannel::CtlObject implementation

MSChannel::CtlObject::CtlObject(int fd_) :
_fd(fd_),
_mask(0),
_readChannel(0),
_writeChannel(0),
_exceptionChannel(0)
{}

MSChannel::CtlObject::~CtlObject(void)
{}
    
void MSChannel::CtlObject::readChannel(MSChannel *read_)
{_readChannel=read_;}
void MSChannel::CtlObject::writeChannel(MSChannel *write_)
{_writeChannel=write_;}
void MSChannel::CtlObject::exceptionChannel(MSChannel *except_)
{_exceptionChannel=except_;}
void MSChannel::CtlObject::mask(int mask_)
{_mask=mask_;}
void MSChannel::CtlObject::node(MSNodeItem *node_)
{_node=node_;}

MSChannel *MSChannel::CtlObject::readChannel(void)
{return _readChannel;}
MSChannel *MSChannel::CtlObject::writeChannel(void)
{return _writeChannel;}
MSChannel *MSChannel::CtlObject::exceptionChannel(void)
{return _exceptionChannel;}
int MSChannel::CtlObject::mask(void)
{return _mask;}
int MSChannel::CtlObject::fd(void)
{return _fd;}
MSNodeItem *MSChannel::CtlObject::node(void)
{return _node;}
