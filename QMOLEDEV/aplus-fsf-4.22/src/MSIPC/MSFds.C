///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////

#include <sys/types.h>
#include <MSIPC/MSFds.H>
#include <MSTypes/MSMessageLog.H>

#include <stdlib.h>
#include <memory.h>

#if defined (MS_NO_GETRLIMIT)
#include <unistd.h>
#else
#include <sys/resource.h>
#endif

#if defined(MS_HOWMANY_IN_SYS_PARAM)
#include <sys/param.h>
#endif

// On glibc 2 for Linux the "fds_bits" item in fd_set structure
// is no longer called that to disable direct access. However they
// provide a macro to get access to it.
#if defined(__FDS_BITS)
#define MS_FDSBITS(set) __FDS_BITS(set)
#else
#define MS_FDSBITS(set) ((set)->fds_bits)
#endif

MSFds::MSFds(void)
{
  init();
}

MSFds::~MSFds(void)
{
}

void MSFds::init(void)
{
  int i;
#if !defined(MS_NO_GETRLIMIT)
  struct rlimit rl;
  if (getrlimit(RLIMIT_NOFILE,&rl)<0)
   {
     MSMessageLog::infoMessage("MSFds::init: getrlimit check failed\n");
     _size=FD_SETSIZE;
   }
  else _size=rl.rlim_cur;
#elif !defined(MS_WINSOCK)
  if ((i=getdtablesize())<0)
   {
     MSMessageLog::infoMessage("MSFds::init: warning: getdtablesize()\n");
     _size=FD_SETSIZE;
   }
  else _size=i;
#else
   _size= FD_SETSIZE;
#endif

  // make sure that _size is not bigger than FD_SETSIZE or
  // we'll be overwriting extra memory when accessing fd_set.
  if(_size>FD_SETSIZE) _size=FD_SETSIZE;

#ifdef MS_WINSOCK
  _howmany=_size;
  _sizeof=sizeof(fd_set);
  for (i=0;i<_howmany;i++) _all.fd_array[i]=i;
  _all.fd_count=_howmany;
#else 
  _howmany=howmany(_size,NFDBITS);
  _sizeof=_howmany*sizeof(fd_mask);
  
  for (i=0;i<_howmany;i++) MS_FDSBITS(&_all)[i]=(fd_mask)(~0);
#endif

  memset((void *)(&_none),0,sizeof(_none));
  memset((void *)(&_r),0,sizeof(_r));
  memset((void *)(&_ra),0,sizeof(_ra));
  memset((void *)(&_w),0,sizeof(_w));
  memset((void *)(&_wa),0,sizeof(_wa));    
  memset((void *)(&_x),0,sizeof(_x));
  memset((void *)(&_xa),0,sizeof(_xa));   
}

void MSFds::fdscopy(fd_set *p1,fd_set *p2)
{
  if (p2!=(fd_set *)(0))
   {
     if (p1==(fd_set *)(0)) memset((char *)p2,0,sizeOf());
     else memcpy((char *)p2,(char *)p1,sizeOf());
   }
}

fd_set *MSFds::fdsdup(register fd_set *p)
{
  register fd_set *p2=(fd_set *)malloc(sizeOf());
  fdscopy(p,p2);
  return p2;
}

void MSFds::fdsfree(register fd_set *p)
{ if (p!=(fd_set *)(0)) free((char *)p); }

void MSFds::fdsfresh(register int fd)
{
  fdsclr(r(),fd);
  fdsclr(ra(),fd);
  fdsclr(w(),fd);
  fdsclr(wa(),fd);
  fdsclr(x(),fd);
  fdsclr(xa(),fd);
}

void MSFds::fdscopy(void)
{
  fdscopy(r(),ra());
  fdscopy(w(),wa());
  fdscopy(x(),xa());
}

void MSFds::fdszero(void)
{
  fdszero(ra());
  fdszero(wa());
  fdszero(xa());
}

void MSFds::fdszero(register fd_set *p)
{ if (p!=(fd_set *)(0)) memset((char *)p,0,sizeOf()); }

#if !defined(MS_WINSOCK)

int MSFds::fdsanyset(register fd_set *p)
{
  if (p!=(fd_set *)(0))
   {
     for (int i=0;i<howMany();i++)
      {
	if (MS_FDSBITS(p)[i]!=(fd_mask)(0)) return 1;
      }
   }
  return 0;
}

void MSFds::fdsclr(register fd_set *p,register int fd)
{ if ((p!=(fd_set *)(0))&&(fd>=0)&&(fd<size())) FD_CLR(fd,p); }

int MSFds::fdsisset(register fd_set *p,register int fd)
{ return ((p!=(fd_set *)(0))&&(fd>=0)&&(fd<size()))?!(!(FD_ISSET(fd,p))):0; }

void MSFds::fdsset(register fd_set *p,register int fd)
{ if ((p!=(fd_set *)(0))&&(fd>=0)&&(fd<size())) FD_SET(fd,p); }

void MSFds::fdsand(fd_set *p1,fd_set *p2,fd_set *r)
{
  if ((p1!=(fd_set *)(0))&&(p2!=(fd_set *)(0)))
   {
     int hmany=howMany();
     for (int i=0;i<hmany;i++) MS_FDSBITS(r)[i]=MS_FDSBITS(p1)[i]&MS_FDSBITS(p2)[i];
   }
  else fdszero(r);
}

void MSFds::fdsor(fd_set *p1,fd_set *p2,fd_set *r)
{
  if (p1!=(fd_set *)(0))
   {
     if (p2!=(fd_set *)(0))
      {
	int hmany=howMany();
	for (int i=0;i<hmany;i++) MS_FDSBITS(r)[i]=MS_FDSBITS(p1)[i]|MS_FDSBITS(p2)[i];
      }
     else fdscopy(p1,r);
   }
  else if (p2!=(fd_set *)(0)) fdscopy(p2,r);
  else fdszero(r);
}

#else

void MSFds::fdsclr(register fd_set *p,register int fd)
{ if ((p!=(fd_set *)(0))&&(fd>=0)/*&&(fd<size())*/) FD_CLR(fd,p); }

int MSFds::fdsanyset(register fd_set *p)
{
  if (p!=(fd_set *)(0)) return p->fd_count;
  return 0;
}

void MSFds::fdsset(register fd_set *p,register int fd)
{  
   if ((p!=(fd_set *)(0))&&(fd>=0)/*&&(fd<size())*/) 
   {
     if(!FD_ISSET(fd,p)) FD_SET(fd,p);
   }
}

void MSFds::fdsand(fd_set *p1,fd_set *p2,fd_set *r)
{
  if ((p1!=(fd_set *)(0))&&(p2!=(fd_set *)(0)))
   {
     fdszero(r);
     //inefficent but how often if any it happens??
     int i;
     for(i=0;i<p1->fd_count;i++) r->fd_array[i]=p1->fd_array[i];
     r->fd_count=p1->fd_count;
     for(i=0;i<p2->fd_count;i++) fdsset(r,p2->fd_array[i]);
   }
  else fdszero(r);
}

void MSFds::fdsor(fd_set *p1,fd_set *p2,fd_set *r)
{
  if (p1!=(fd_set *)(0))
   {
     if (p2!=(fd_set *)(0))
      {
        fdszero(r);
        //inefficent but how often if any it happens??
        int i;
        for(i=0;i<p1->fd_count;i++) r->fd_array[i]=p1->fd_array[i];
        r->fd_count=p1->fd_count;
        for(i=0;i<p2->fd_count;i++)
        {
            if(fdsisset(r,p2->fd_array[i])) fdsclr(r,p2->fd_array[i]);
            else fdsset(r,p2->fd_array[i]);
        }
      } 
     else fdscopy(p1,r);
   }
  else if (p2!=(fd_set *)(0)) fdscopy(p2,r);
  else fdszero(r);
}

int MSFds::fdsisset(register fd_set *p,register int fd)
{ return ((p!=(fd_set *)(0))&&(fd>=0)/*&&(fd<size())*/)?!(!(FD_ISSET(fd,p))):0; }

#endif


